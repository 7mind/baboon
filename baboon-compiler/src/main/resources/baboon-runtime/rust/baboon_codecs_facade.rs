use crate::any_opaque::{AnyMeta, AnyOpaque, BaboonCodecError};
use crate::baboon_runtime::BaboonCodecContext;
use std::any::TypeId;
use std::collections::HashMap;
use std::io::{Cursor, Read};
use std::sync::{Arc, Mutex, OnceLock};

// --- Type-erased codec interfaces ---
//
// C#/Scala's facade stores codecs polymorphically via `IBaboonGenerated`/`BaboonGenerated`
// supertype; downstream callers downcast as needed. Rust has no inheritance, so we work
// with trait objects: each codec implements `BaboonAnyBinCodec` or `BaboonAnyJsonCodec`
// to carry an `IBaboonGenerated`-shaped opaque value (`Box<dyn BaboonGenerated>` / `Box<dyn Any>`).
//
// `BaboonGenerated` in this runtime has *static* methods only — it's a marker trait without
// instance methods. So the dynamic-dispatch carrier here is `Box<dyn BaboonGeneratedDyn>`,
// a sibling object-safe trait that an emitter can implement for each concrete generated type.
// Codec generators (PR 4.2+) will emit the `BaboonGeneratedDyn` impls and codec wrappers.

pub trait BaboonGeneratedDyn: std::any::Any + Send + Sync {
    fn baboon_domain_version_dyn(&self) -> &str;
    fn baboon_domain_identifier_dyn(&self) -> &str;
    fn baboon_type_identifier_dyn(&self) -> &str;
    /// Mirrors C#'s `IBaboonGenerated.BaboonSameInVersions()`/Scala's `baboonSameInVersions`.
    /// Codegen invariant: the slice is never empty (`first()` is called for `domainVersionMinCompat`
    /// in `BaboonTypeMeta::from`); a violation panics with a fail-fast message.
    fn baboon_same_in_versions_dyn(&self) -> Vec<String>;
    fn as_any(&self) -> &dyn std::any::Any;
    /// Consume the box and recover an `Any` for downcasting via `Box::downcast`. Mirrors
    /// the C# `if (current is TTo result)` pattern used in `Convert<TFrom, TTo>`.
    fn into_any(self: Box<Self>) -> Box<dyn std::any::Any>;
    /// Sentinel hook for ADT-aware encode paths (mirrors C#'s
    /// `value is IBaboonAdtMemberMeta && declaredType is Interface`). Default `None`; the codec
    /// generator overrides for ADT branch types so the encoder can substitute the ADT type id
    /// when the static-declared type is the trait. Without a static-type signal in Rust, the
    /// `encode_to_*_with_declared_trait(..., is_adt_trait: true)` variants gate this lookup.
    fn as_adt_member_meta_dyn(&self) -> Option<&dyn BaboonAdtMemberMetaDyn> {
        None
    }
    /// Type-erased static `TypeId` for this concrete value (mirrors C#'s `current.GetType()`
    /// used in conversion matching). Generator emits `std::any::TypeId::of::<Self>()`. Default
    /// uses `Any::type_id` via `as_any`.
    fn baboon_type_id_dyn(&self) -> TypeId {
        self.as_any().type_id()
    }
}

/// Sibling trait carrying ADT-member identity. Generator emits this for ADT branch types.
/// Returned via `BaboonGeneratedDyn::as_adt_member_meta_dyn`. Mirrors C#'s
/// `IBaboonAdtMemberMeta.BaboonAdtTypeIdentifier`/`BaboonAdtType`.
pub trait BaboonAdtMemberMetaDyn: BaboonGeneratedDyn {
    fn baboon_adt_type_identifier_dyn(&self) -> String;
    /// Type-erased `TypeId` of the ADT root type (mirrors C#'s `IBaboonAdtMemberMeta.BaboonAdtType()`).
    fn baboon_adt_type_id_dyn(&self) -> TypeId;
}

pub trait BaboonAnyBinCodec: Send + Sync {
    fn type_identifier(&self) -> &str;
    fn encode_dyn(
        &self,
        ctx: &BaboonCodecContext,
        writer: &mut dyn std::io::Write,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<(), BaboonCodecError>;
    fn decode_dyn(
        &self,
        ctx: &BaboonCodecContext,
        reader: &mut dyn std::io::Read,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError>;
}

pub trait BaboonAnyJsonCodec: Send + Sync {
    fn type_identifier(&self) -> &str;
    fn encode_json_dyn(
        &self,
        ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<serde_json::Value, BaboonCodecError>;
    fn decode_json_dyn(
        &self,
        ctx: &BaboonCodecContext,
        wire: &serde_json::Value,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError>;
}

// --- Conversion protocol ---
//
// Mirrors C#'s `IConversion` + `AbstractBaboonConversions.{FindConversions, Convert}` surface.
// `BaboonAnyConversion` is the per-conversion descriptor; `BaboonAnyConversions` is the table
// of conversions for a domain version that knows how to look up matching conversions for a
// runtime value and apply them. Codec generators in PR 4.2+ provide concrete impls.

pub trait BaboonAnyConversion: Send + Sync {
    /// Type-erased TypeId of the *source* type (mirrors C#'s `IConversion.TypeFrom()`).
    fn type_from(&self) -> TypeId;
    /// Source-version string. Used for diagnostics; mirrors C#'s `IConversion.VersionFrom()`.
    fn version_from(&self) -> &str;
    /// Target-version string (e.g. "2.0.0"). Used by `convert` to walk version order.
    fn version_to(&self) -> &str;
    /// Type-id (Baboon-domain-level type identifier; not the Rust `TypeId`). Used for ADT
    /// matching and diagnostics. Mirrors C#'s `IConversion.TypeId()`.
    fn type_id(&self) -> &str;

    /// Parsed semver of `version_to()`. Default impl parses; concrete impls may override.
    /// Returns `Result` rather than panicking — preserves PR-04-D02 typed-error discipline.
    fn version_to_parsed(&self) -> Result<BaboonVersion, BaboonCodecError> {
        BaboonVersion::parse(self.version_to())
    }
}

pub trait BaboonAnyConversions: Send + Sync {
    /// Mirrors C#'s `AbstractBaboonConversions.FindConversions(value)`. Returns the list of
    /// conversions registered for the runtime type of `value` (or, for ADT branches, the ADT
    /// root type plus the branch type). Order is implementation-defined; `convert` selects
    /// the highest-`version_to()` candidate.
    fn find_conversions(&self, value: &dyn BaboonGeneratedDyn) -> Vec<Arc<dyn BaboonAnyConversion>>;

    /// Apply `conversion` to `value`. Mirrors C#'s `AbstractBaboonConversions.Convert`.
    /// The conversion implementation is responsible for type-checking `value` against
    /// `conversion.type_from()`; this method dispatches and propagates failures via the
    /// typed `BaboonCodecError` channel.
    fn convert(
        &self,
        value: Box<dyn BaboonGeneratedDyn>,
        conversion: &dyn BaboonAnyConversion,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError>;
}

pub trait BaboonAnyMeta: Send + Sync {
    /// Returns versions in which `type_id` is structurally identical (mirrors Scala's
    /// `BaboonMeta.sameInVersions`). Used for the `getCodecMaxCompat` resolution path.
    fn same_in_versions(&self, type_id: &str) -> Vec<String>;
}

// --- Domain version key ---

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BaboonDomainVersion {
    pub domain_identifier: String,
    pub domain_version: String,
}

impl BaboonDomainVersion {
    pub fn new<D: Into<String>, V: Into<String>>(domain_identifier: D, domain_version: V) -> Self {
        BaboonDomainVersion { domain_identifier: domain_identifier.into(), domain_version: domain_version.into() }
    }

    pub fn version(&self) -> Result<BaboonVersion, BaboonCodecError> {
        BaboonVersion::parse(&self.domain_version)
    }
}

impl std::fmt::Display for BaboonDomainVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}@{}", self.domain_identifier, self.domain_version)
    }
}

// --- Semantic version ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BaboonVersion {
    pub major: i32,
    pub minor: i32,
    pub patch: i32,
}

impl BaboonVersion {
    pub fn parse(s: &str) -> Result<Self, BaboonCodecError> {
        let chunks: Vec<&str> = s.split('.').collect();
        if chunks.len() < 3 {
            return Err(BaboonCodecError::decoder_failure(format!(
                "Expected to have version in format x.y.z, got {}",
                s
            )));
        }
        let parse = |part: &str, slot: &str| -> Result<i32, BaboonCodecError> {
            part.trim().parse::<i32>().map_err(|_| {
                BaboonCodecError::decoder_failure(format!(
                    "Expected to have version in format x.y.z, got {}. Invalid {} value.",
                    s, slot
                ))
            })
        };
        Ok(BaboonVersion {
            major: parse(chunks[0], "major")?,
            minor: parse(chunks[1], "minor")?,
            patch: parse(chunks[2], "patch")?,
        })
    }
}

impl std::fmt::Display for BaboonVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

// --- BaboonTypeMeta (synthetic; used for facade lookup) ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaboonTypeMeta {
    pub meta_version: u8,
    pub domain_identifier: String,
    pub domain_version: String,
    pub domain_version_min_compat: String,
    pub type_identifier: String,
}

impl BaboonTypeMeta {
    pub const META_VERSION_1: u8 = 1;
    pub const META_VERSION: u8 = Self::META_VERSION_1;

    pub fn new<D: Into<String>, V: Into<String>, MC: Into<String>, T: Into<String>>(
        meta_version: u8,
        domain_identifier: D,
        domain_version: V,
        domain_version_min_compat: MC,
        type_identifier: T,
    ) -> Self {
        BaboonTypeMeta {
            meta_version,
            domain_identifier: domain_identifier.into(),
            domain_version: domain_version.into(),
            domain_version_min_compat: domain_version_min_compat.into(),
            type_identifier: type_identifier.into(),
        }
    }

    pub fn version_ref(&self) -> BaboonDomainVersion {
        BaboonDomainVersion::new(&self.domain_identifier, &self.domain_version)
    }

    pub fn version_min_compat(&self) -> Option<BaboonDomainVersion> {
        if self.domain_version_min_compat.is_empty() || self.domain_version_min_compat == self.domain_version {
            None
        } else {
            Some(BaboonDomainVersion::new(&self.domain_identifier, &self.domain_version_min_compat))
        }
    }
}

// --- BaboonTypeMeta wire codec ---
//
// Mirrors C#'s static `BaboonTypeMetaCodec` (BaboonTypeMeta.cs:126-214) and Scala's
// `BaboonTypeMetaCodec` (BaboonRuntimeShared.scala:156-214). Used by the facade encode/decode
// entry points to write/read the meta prefix that precedes the payload in both binary and
// JSON envelopes. Lives here (alongside `BaboonTypeMeta`) rather than in `any_opaque.rs`
// because it's part of the facade-level wire surface, not the AnyMeta protocol.

mod baboon_type_meta_codec {
    use super::{BaboonTypeMeta, BaboonCodecError};
    use crate::baboon_runtime::bin_tools;
    use std::io::{Read, Write};

    pub const META_VERSION_KEY: &str = "$mv";
    pub const DOMAIN_IDENTIFIER_KEY: &str = "$d";
    pub const DOMAIN_VERSION_KEY: &str = "$v";
    pub const DOMAIN_VERSION_MIN_COMPAT_KEY: &str = "$uv";
    pub const TYPE_IDENTIFIER_KEY: &str = "$t";

    /// Wire format: `[meta-version:u8][domain:string][version:string][has-min-compat:u8][min-compat?:string][type-id:string]`.
    /// Mirrors C# BaboonTypeMetaCodec.WriteBin / Scala writeBin exactly.
    pub fn write_bin(meta: &BaboonTypeMeta, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_byte(writer, BaboonTypeMeta::META_VERSION)?;
        bin_tools::write_string(writer, &meta.domain_identifier)?;
        bin_tools::write_string(writer, &meta.domain_version)?;
        if meta.domain_version == meta.domain_version_min_compat {
            bin_tools::write_byte(writer, 0)?;
        } else {
            bin_tools::write_byte(writer, 1)?;
            bin_tools::write_string(writer, &meta.domain_version_min_compat)?;
        }
        bin_tools::write_string(writer, &meta.type_identifier)
    }

    /// Reads the wire-format prefix. Returns `Ok(None)` when the leading meta-version byte
    /// does not match `META_VERSION_1` (forward-compat: future meta versions). Mirrors C#
    /// `ReadMeta(BinaryReader)` (BaboonTypeMeta.cs:169) returning nullable.
    pub fn read_bin<R: Read>(reader: &mut R) -> Result<Option<BaboonTypeMeta>, BaboonCodecError> {
        let meta_version = bin_tools::read_byte(reader).map_err(|e| {
            BaboonCodecError::decoder_failure_from_box(
                "BaboonTypeMetaCodec.read_bin: failed to read meta-version byte",
                e,
            )
        })?;
        if meta_version != BaboonTypeMeta::META_VERSION_1 {
            return Ok(None);
        }
        let domain_identifier = bin_tools::read_string(reader).map_err(|e| {
            BaboonCodecError::decoder_failure_from_box(
                "BaboonTypeMetaCodec.read_bin: failed to read domain identifier",
                e,
            )
        })?;
        let domain_version = bin_tools::read_string(reader).map_err(|e| {
            BaboonCodecError::decoder_failure_from_box(
                "BaboonTypeMetaCodec.read_bin: failed to read domain version",
                e,
            )
        })?;
        let has_min_compat = bin_tools::read_byte(reader).map_err(|e| {
            BaboonCodecError::decoder_failure_from_box(
                "BaboonTypeMetaCodec.read_bin: failed to read has-min-compat byte",
                e,
            )
        })?;
        let domain_version_min_compat = if has_min_compat == 1 {
            bin_tools::read_string(reader).map_err(|e| {
                BaboonCodecError::decoder_failure_from_box(
                    "BaboonTypeMetaCodec.read_bin: failed to read min-compat",
                    e,
                )
            })?
        } else {
            domain_version.clone()
        };
        let type_identifier = bin_tools::read_string(reader).map_err(|e| {
            BaboonCodecError::decoder_failure_from_box(
                "BaboonTypeMetaCodec.read_bin: failed to read type identifier",
                e,
            )
        })?;
        Ok(Some(BaboonTypeMeta::new(
            BaboonTypeMeta::META_VERSION,
            domain_identifier,
            domain_version,
            domain_version_min_compat,
            type_identifier,
        )))
    }

    /// JSON envelope reader. Mirrors C# `ReadMeta(JToken)` (BaboonTypeMeta.cs:189) and Scala
    /// `readMeta(json)` (BaboonRuntimeShared.scala:197). Returns `Ok(None)` when the input is
    /// not an object, when `$mv` is present but not "1", or when any of `$d`/`$v`/`$t` is missing.
    pub fn read_meta_json(
        json: &serde_json::Value,
    ) -> Result<Option<BaboonTypeMeta>, BaboonCodecError> {
        let obj = match json.as_object() {
            Some(o) => o,
            None => return Ok(None),
        };
        if let Some(mv) = obj.get(META_VERSION_KEY) {
            // MFACADE-PR-3: accept $mv as either a JSON number or a string (back-compat
            // with M28-vintage fixtures); both must equal META_VERSION_1.
            let mv_byte: Option<u8> = match mv {
                serde_json::Value::Number(n) => n.as_u64().and_then(|x| u8::try_from(x).ok()),
                serde_json::Value::String(s) => s.parse::<u8>().ok(),
                _ => None,
            };
            match mv_byte {
                Some(v) if v == BaboonTypeMeta::META_VERSION_1 => {}
                _ => return Ok(None),
            }
        }
        let d = match obj.get(DOMAIN_IDENTIFIER_KEY).and_then(|v| v.as_str()) {
            Some(s) => s,
            None => return Ok(None),
        };
        let v = match obj.get(DOMAIN_VERSION_KEY).and_then(|v| v.as_str()) {
            Some(s) => s,
            None => return Ok(None),
        };
        let t = match obj.get(TYPE_IDENTIFIER_KEY).and_then(|v| v.as_str()) {
            Some(s) => s,
            None => return Ok(None),
        };
        let uv = obj
            .get(DOMAIN_VERSION_MIN_COMPAT_KEY)
            .and_then(|v| v.as_str())
            .unwrap_or(v);
        Ok(Some(BaboonTypeMeta::new(
            BaboonTypeMeta::META_VERSION,
            d,
            v,
            uv,
            t,
        )))
    }
}

// --- Lazy<T> primitive ---
//
// Mirrors Scala/C# lazy-init semantics for codec construction. The codec registry stores
// thunks (constructor closures) wrapped in `LazyCodec` so codecs are instantiated on first
// access only. A registered codec table can call into lots of types' registrations during
// construction; lazy is what avoids paying that cost up front.

pub struct LazyCodec<T> {
    cell: OnceLock<Arc<T>>,
    init: Mutex<Option<Box<dyn FnOnce() -> T + Send>>>,
}

impl<T> LazyCodec<T> {
    pub fn new<F: FnOnce() -> T + Send + 'static>(init: F) -> Self {
        LazyCodec { cell: OnceLock::new(), init: Mutex::new(Some(Box::new(init))) }
    }

    pub fn from_value(value: T) -> Self {
        let cell = OnceLock::new();
        let _ = cell.set(Arc::new(value));
        LazyCodec { cell, init: Mutex::new(None) }
    }

    pub fn get(&self) -> Arc<T> {
        if let Some(v) = self.cell.get() {
            return Arc::clone(v);
        }
        let value = {
            let mut guard = self.init.lock().expect("LazyCodec mutex poisoned");
            match guard.take() {
                Some(thunk) => thunk(),
                None => {
                    drop(guard);
                    return Arc::clone(self.cell.get().expect("LazyCodec missing both init and value"));
                }
            }
        };
        let arc = Arc::new(value);
        match self.cell.set(Arc::clone(&arc)) {
            Ok(()) => arc,
            Err(_) => Arc::clone(self.cell.get().expect("LazyCodec set failed but cell empty")),
        }
    }
}

// --- Codec registry collections ---

pub trait AbstractBaboonCodecs: Send + Sync {
    fn try_find_bin(&self, _id: &str) -> Option<Arc<dyn BaboonAnyBinCodec>> {
        None
    }
    fn try_find_json(&self, _id: &str) -> Option<Arc<dyn BaboonAnyJsonCodec>> {
        None
    }
}

pub struct AbstractBaboonJsonCodecsImpl {
    codecs: HashMap<String, LazyCodec<Arc<dyn BaboonAnyJsonCodec>>>,
}

impl Default for AbstractBaboonJsonCodecsImpl {
    fn default() -> Self {
        Self::new()
    }
}

impl AbstractBaboonJsonCodecsImpl {
    pub fn new() -> Self {
        AbstractBaboonJsonCodecsImpl { codecs: HashMap::new() }
    }

    pub fn register<F: FnOnce() -> Arc<dyn BaboonAnyJsonCodec> + Send + 'static>(
        &mut self,
        id: &str,
        thunk: F,
    ) {
        self.codecs.insert(id.to_string(), LazyCodec::new(thunk));
    }

    pub fn try_find(&self, id: &str) -> Option<Arc<dyn BaboonAnyJsonCodec>> {
        self.codecs.get(id).map(|lazy| (*lazy.get()).clone())
    }
}

pub struct AbstractBaboonUebaCodecsImpl {
    codecs: HashMap<String, LazyCodec<Arc<dyn BaboonAnyBinCodec>>>,
}

impl Default for AbstractBaboonUebaCodecsImpl {
    fn default() -> Self {
        Self::new()
    }
}

impl AbstractBaboonUebaCodecsImpl {
    pub fn new() -> Self {
        AbstractBaboonUebaCodecsImpl { codecs: HashMap::new() }
    }

    pub fn register<F: FnOnce() -> Arc<dyn BaboonAnyBinCodec> + Send + 'static>(
        &mut self,
        id: &str,
        thunk: F,
    ) {
        self.codecs.insert(id.to_string(), LazyCodec::new(thunk));
    }

    pub fn try_find(&self, id: &str) -> Option<Arc<dyn BaboonAnyBinCodec>> {
        self.codecs.get(id).map(|lazy| (*lazy.get()).clone())
    }
}

// --- BaboonCodecsFacade ---

pub struct BaboonCodecsFacade {
    versions_codecs_json: Mutex<HashMap<BaboonDomainVersion, Arc<LazyCodec<Arc<AbstractBaboonJsonCodecsImpl>>>>>,
    versions_codecs_bin: Mutex<HashMap<BaboonDomainVersion, Arc<LazyCodec<Arc<AbstractBaboonUebaCodecsImpl>>>>>,
    versions_meta: Mutex<HashMap<BaboonDomainVersion, Arc<LazyCodec<Arc<dyn BaboonAnyMeta>>>>>,
    versions_conversions: Mutex<HashMap<BaboonDomainVersion, Arc<LazyCodec<Arc<dyn BaboonAnyConversions>>>>>,
    domain_versions: Mutex<HashMap<String, Vec<BaboonDomainVersion>>>,
}

impl std::fmt::Debug for BaboonCodecsFacade {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Cheap structural summary; avoids leaking codec internals + holding mutex during fmt.
        let domains = match self.domain_versions.try_lock() {
            Ok(g) => g.len(),
            Err(_) => 0,
        };
        write!(f, "BaboonCodecsFacade {{ domains: {} }}", domains)
    }
}

const CONTENT_JSON_KEY: &str = "$c";

impl Default for BaboonCodecsFacade {
    fn default() -> Self {
        Self::new()
    }
}

impl BaboonCodecsFacade {
    pub fn new() -> Self {
        BaboonCodecsFacade {
            versions_codecs_json: Mutex::new(HashMap::new()),
            versions_codecs_bin: Mutex::new(HashMap::new()),
            versions_meta: Mutex::new(HashMap::new()),
            versions_conversions: Mutex::new(HashMap::new()),
            domain_versions: Mutex::new(HashMap::new()),
        }
    }

    pub fn latest(&self, domain: &str) -> Result<BaboonVersion, BaboonCodecError> {
        let versions = self.domain_versions.lock().expect("domain_versions mutex poisoned");
        match versions.get(domain) {
            Some(vs) if !vs.is_empty() => vs[vs.len() - 1].version(),
            _ => Err(BaboonCodecError::codec_not_found(format!(
                "No registered version for {} domain found.",
                domain
            ))),
        }
    }

    pub fn register(
        &self,
        domain_version: BaboonDomainVersion,
        codecs_json: impl FnOnce() -> Arc<AbstractBaboonJsonCodecsImpl> + Send + 'static,
        codecs_bin: impl FnOnce() -> Arc<AbstractBaboonUebaCodecsImpl> + Send + 'static,
    ) -> BaboonDomainVersion {
        self.register_version(&domain_version);
        self.versions_codecs_json
            .lock()
            .expect("mutex poisoned")
            .insert(domain_version.clone(), Arc::new(LazyCodec::new(codecs_json)));
        self.versions_codecs_bin
            .lock()
            .expect("mutex poisoned")
            .insert(domain_version.clone(), Arc::new(LazyCodec::new(codecs_bin)));
        domain_version
    }

    pub fn register_with_meta(
        &self,
        domain_version: BaboonDomainVersion,
        codecs_json: impl FnOnce() -> Arc<AbstractBaboonJsonCodecsImpl> + Send + 'static,
        codecs_bin: impl FnOnce() -> Arc<AbstractBaboonUebaCodecsImpl> + Send + 'static,
        meta: impl FnOnce() -> Arc<dyn BaboonAnyMeta> + Send + 'static,
    ) -> BaboonDomainVersion {
        let dv = self.register(domain_version, codecs_json, codecs_bin);
        self.versions_meta
            .lock()
            .expect("mutex poisoned")
            .insert(dv.clone(), Arc::new(LazyCodec::new(meta)));
        dv
    }

    pub fn register_full(
        &self,
        domain_version: BaboonDomainVersion,
        codecs_json: impl FnOnce() -> Arc<AbstractBaboonJsonCodecsImpl> + Send + 'static,
        codecs_bin: impl FnOnce() -> Arc<AbstractBaboonUebaCodecsImpl> + Send + 'static,
        conversions: impl FnOnce() -> Arc<dyn BaboonAnyConversions> + Send + 'static,
        meta: impl FnOnce() -> Arc<dyn BaboonAnyMeta> + Send + 'static,
    ) -> BaboonDomainVersion {
        let dv = self.register(domain_version, codecs_json, codecs_bin);
        self.versions_conversions
            .lock()
            .expect("mutex poisoned")
            .insert(dv.clone(), Arc::new(LazyCodec::new(conversions)));
        self.versions_meta
            .lock()
            .expect("mutex poisoned")
            .insert(dv.clone(), Arc::new(LazyCodec::new(meta)));
        dv
    }

    pub fn register_meta(
        &self,
        domain_version: BaboonDomainVersion,
        meta: impl FnOnce() -> Arc<dyn BaboonAnyMeta> + Send + 'static,
    ) -> BaboonDomainVersion {
        self.register_version(&domain_version);
        self.versions_meta
            .lock()
            .expect("mutex poisoned")
            .insert(domain_version.clone(), Arc::new(LazyCodec::new(meta)));
        domain_version
    }

    pub fn register_conversions(
        &self,
        domain_version: BaboonDomainVersion,
        conversions: impl FnOnce() -> Arc<dyn BaboonAnyConversions> + Send + 'static,
    ) -> BaboonDomainVersion {
        self.register_version(&domain_version);
        self.versions_conversions
            .lock()
            .expect("mutex poisoned")
            .insert(domain_version.clone(), Arc::new(LazyCodec::new(conversions)));
        domain_version
    }

    pub fn verify(&self) -> Result<(), BaboonCodecError> {
        let versions = self.domain_versions.lock().expect("mutex poisoned");
        if versions.is_empty() {
            return Err(BaboonCodecError::codec_not_found(
                "Baboon codecs must have at least one domain registered.".to_string(),
            ));
        }
        let conversions = self.versions_conversions.lock().expect("mutex poisoned");
        let meta = self.versions_meta.lock().expect("mutex poisoned");
        for vs in versions.values() {
            for dv in vs {
                if !conversions.contains_key(dv) {
                    return Err(BaboonCodecError::conversion_not_found(format!(
                        "Baboon codecs must have conversion for {} registered.",
                        dv
                    )));
                }
                if !meta.contains_key(dv) {
                    return Err(BaboonCodecError::codec_not_found(format!(
                        "Baboon codecs must have codecs for {} registered.",
                        dv
                    )));
                }
            }
        }
        Ok(())
    }

    fn register_version(&self, domain_version: &BaboonDomainVersion) {
        let mut versions = self.domain_versions.lock().expect("mutex poisoned");
        let entry = versions.entry(domain_version.domain_identifier.clone()).or_default();
        if !entry.iter().any(|v| v == domain_version) {
            entry.push(domain_version.clone());
            // Sort by parsed semver so getCodec can read first/last as min/max.
            entry.sort_by_key(|dv| dv.version().ok());
        }
    }

    // --- Cross-format helpers ---

    /// Cross-format helper: decode an `AnyOpaqueJson` payload via the registered JSON codec, then
    /// re-encode it via the registered UEBA codec. The wire `meta` may omit components that the
    /// field's static declaration already pins down (variants B/C/D1/D2/D3). The codec generator
    /// passes the static fallbacks; runtime `meta.X` takes precedence over `static_x` to preserve
    /// override semantics. Without the static fallback, only variant A would work — see PR-06-D01.
    pub fn json_to_ueba_bytes(
        &self,
        meta: &AnyMeta,
        json: &serde_json::Value,
        static_domain: Option<&str>,
        static_version: Option<&str>,
        static_typeid: Option<&str>,
    ) -> Result<Vec<u8>, BaboonCodecError> {
        let type_meta =
            self.build_synthetic_type_meta(meta, static_domain, static_version, static_typeid)?;
        let json_codec = self.get_json_codec(&type_meta, false)?;
        let bin_codec = self.get_bin_codec(&type_meta, false)?;
        let typed = json_codec.decode_json_dyn(&BaboonCodecContext::Compact, json).map_err(|e| {
            BaboonCodecError::decoder_failure(format!(
                "json_to_ueba_bytes: cannot decode JSON payload of type [{}.{}] of version '{}': {}",
                type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
            ))
        })?;
        let mut buf = Vec::new();
        bin_codec
            .encode_dyn(&BaboonCodecContext::Compact, &mut buf, typed.as_ref())
            .map_err(|e| {
                BaboonCodecError::encoder_failure(format!(
                    "json_to_ueba_bytes: cannot encode UEBA payload of type [{}.{}] of version '{}': {}",
                    type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
                ))
            })?;
        Ok(buf)
    }

    /// Symmetric to `json_to_ueba_bytes`. See its documentation for the static-fallback contract.
    pub fn ueba_to_json(
        &self,
        meta: &AnyMeta,
        bytes: &[u8],
        static_domain: Option<&str>,
        static_version: Option<&str>,
        static_typeid: Option<&str>,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        let type_meta =
            self.build_synthetic_type_meta(meta, static_domain, static_version, static_typeid)?;
        let bin_codec = self.get_bin_codec(&type_meta, false)?;
        let json_codec = self.get_json_codec(&type_meta, false)?;
        let mut cursor = Cursor::new(bytes);
        let typed = bin_codec.decode_dyn(&BaboonCodecContext::Compact, &mut cursor).map_err(|e| {
            BaboonCodecError::decoder_failure(format!(
                "ueba_to_json: cannot decode UEBA payload of type [{}.{}] of version '{}': {}",
                type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
            ))
        })?;
        let json = json_codec
            .encode_json_dyn(&BaboonCodecContext::Compact, typed.as_ref())
            .map_err(|e| {
                BaboonCodecError::encoder_failure(format!(
                    "ueba_to_json: cannot encode JSON payload of type [{}.{}] of version '{}': {}",
                    type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
                ))
            })?;
        Ok(json)
    }

    pub fn decode_any(&self, opaque: &AnyOpaque) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let type_meta = self.build_synthetic_type_meta(opaque.meta(), None, None, None)?;
        match opaque {
            AnyOpaque::Ueba(u) => {
                let codec = self.get_bin_codec(&type_meta, false)?;
                let mut cursor = Cursor::new(&u.bytes);
                codec.decode_dyn(&BaboonCodecContext::Compact, &mut cursor).map_err(|e| {
                    BaboonCodecError::decoder_failure(format!(
                        "decode_any: cannot decode UEBA payload of type [{}.{}] of version '{}': {}",
                        type_meta.domain_identifier,
                        type_meta.type_identifier,
                        type_meta.domain_version,
                        e
                    ))
                })
            }
            AnyOpaque::Json(j) => {
                let codec = self.get_json_codec(&type_meta, false)?;
                codec.decode_json_dyn(&BaboonCodecContext::Compact, &j.json).map_err(|e| {
                    BaboonCodecError::decoder_failure(format!(
                        "decode_any: cannot decode JSON payload of type [{}.{}] of version '{}': {}",
                        type_meta.domain_identifier,
                        type_meta.type_identifier,
                        type_meta.domain_version,
                        e
                    ))
                })
            }
        }
    }

    /// Synthesise a `BaboonTypeMeta` from an `AnyMeta` plus optional static fallbacks.
    /// `meta.X` takes precedence over `static_x` (override semantics — wire wins).
    /// `decode_any` calls this with all-`None` statics so its limitation to variant A is preserved.
    fn build_synthetic_type_meta(
        &self,
        meta: &AnyMeta,
        static_domain: Option<&str>,
        static_version: Option<&str>,
        static_typeid: Option<&str>,
    ) -> Result<BaboonTypeMeta, BaboonCodecError> {
        let domain = meta.domain.as_deref().or(static_domain);
        let version = meta.version.as_deref().or(static_version);
        let typeid = meta.typeid.as_deref().or(static_typeid);
        match (domain, version, typeid) {
            (Some(d), Some(v), Some(t)) => Ok(BaboonTypeMeta::new(
                BaboonTypeMeta::META_VERSION,
                d,
                v,
                v,
                t,
            )),
            _ => {
                let mut missing = Vec::new();
                if domain.is_none() {
                    missing.push("domain");
                }
                if version.is_none() {
                    missing.push("version");
                }
                if typeid.is_none() {
                    missing.push("typeid");
                }
                Err(BaboonCodecError::decoder_failure(format!(
                    "AnyMeta requires domain/version/typeid for facade resolution; got kind 0x{:x} which lacks: {}",
                    meta.kind,
                    missing.join(", ")
                )))
            }
        }
    }

    fn get_bin_codec(
        &self,
        type_meta: &BaboonTypeMeta,
        exact: bool,
    ) -> Result<Arc<dyn BaboonAnyBinCodec>, BaboonCodecError> {
        let resolved = self.resolve_version(type_meta, exact)?;
        let table = {
            let codecs = self.versions_codecs_bin.lock().expect("mutex poisoned");
            codecs
                .get(&resolved)
                .cloned()
                .ok_or_else(|| {
                    BaboonCodecError::codec_not_found(format!(
                        "No codecs registered for domain version '{}'.",
                        resolved
                    ))
                })?
        };
        let table = table.get();
        table.try_find(&type_meta.type_identifier).ok_or_else(|| {
            BaboonCodecError::codec_not_found(format!(
                "No codec found for type [{}.{}] of version '{}'.",
                resolved.domain_identifier, type_meta.type_identifier, resolved.domain_version
            ))
        })
    }

    fn get_json_codec(
        &self,
        type_meta: &BaboonTypeMeta,
        exact: bool,
    ) -> Result<Arc<dyn BaboonAnyJsonCodec>, BaboonCodecError> {
        let resolved = self.resolve_version(type_meta, exact)?;
        let table = {
            let codecs = self.versions_codecs_json.lock().expect("mutex poisoned");
            codecs
                .get(&resolved)
                .cloned()
                .ok_or_else(|| {
                    BaboonCodecError::codec_not_found(format!(
                        "No codecs registered for domain version '{}'.",
                        resolved
                    ))
                })?
        };
        let table = table.get();
        table.try_find(&type_meta.type_identifier).ok_or_else(|| {
            BaboonCodecError::codec_not_found(format!(
                "No codec found for type [{}.{}] of version '{}'.",
                resolved.domain_identifier, type_meta.type_identifier, resolved.domain_version
            ))
        })
    }

    /// Resolve which `BaboonDomainVersion` to look up the codec under, given a model's
    /// declared version and the registered min/max. Mirrors Scala/C# `getCodec` resolution
    /// arms; the PR-07-D02 single-version-domain fix is baked in (the `!exact` arm at max).
    fn resolve_version(
        &self,
        type_meta: &BaboonTypeMeta,
        exact: bool,
    ) -> Result<BaboonDomainVersion, BaboonCodecError> {
        let versions = {
            let map = self.domain_versions.lock().expect("mutex poisoned");
            map.get(&type_meta.domain_identifier).cloned()
        };
        let versions = versions.filter(|v| !v.is_empty()).ok_or_else(|| {
            BaboonCodecError::codec_not_found(format!(
                "Unknown domain {}.",
                type_meta.domain_identifier
            ))
        })?;

        let min_version = &versions[0];
        let max_version = &versions[versions.len() - 1];

        let lookup_version = type_meta.version_ref();
        let lookup_v = lookup_version.version()?;
        let max_v = max_version.version()?;
        let min_v = min_version.version()?;

        let model_version = match type_meta.version_min_compat() {
            Some(min_compat) if lookup_v > max_v => min_compat,
            _ => lookup_version,
        };
        let model_v = model_version.version()?;

        if exact && model_v == max_v {
            return Ok(model_version);
        }
        // PR-07-D02: non-exact lookup at the latest registered version routes to exact lookup.
        // Without this arm a single-version domain (min == max == model) falls through every
        // other arm and yields "Unsupported domain version" because the strictly-less-than
        // bound on the next arm excludes equality.
        if !exact && model_v == max_v {
            return Ok(model_version);
        }
        if model_v >= min_v && model_v < max_v {
            return self.resolve_max_compat(&model_version, max_version, &type_meta.type_identifier);
        }
        if model_v < min_v {
            return self.resolve_max_compat(min_version, max_version, &type_meta.type_identifier);
        }
        Err(BaboonCodecError::codec_not_found(format!(
            "Unsupported domain version '{}'.",
            model_version
        )))
    }

    fn resolve_max_compat(
        &self,
        model_version: &BaboonDomainVersion,
        max_version: &BaboonDomainVersion,
        type_identifier: &str,
    ) -> Result<BaboonDomainVersion, BaboonCodecError> {
        let meta_lazy = {
            let map = self.versions_meta.lock().expect("mutex poisoned");
            map.get(model_version).cloned().ok_or_else(|| {
                BaboonCodecError::codec_not_found(format!(
                    "Unknown domain version '{}'.",
                    model_version
                ))
            })?
        };
        let meta = meta_lazy.get();
        let same_versions = meta.same_in_versions(type_identifier);
        let max_v = max_version.version()?;
        let mut best_same: Option<&String> = None;
        for sv in same_versions.iter().rev() {
            if sv == &max_version.domain_version {
                best_same = Some(sv);
                break;
            }
            if let Ok(parsed) = BaboonVersion::parse(sv) {
                if parsed <= max_v {
                    best_same = Some(sv);
                    break;
                }
            }
        }
        let best_same = best_same.ok_or_else(|| {
            BaboonCodecError::codec_not_found(format!(
                "No max compat codec found for type [{}.{}] of version '{}'.",
                model_version.domain_identifier, type_identifier, model_version.domain_version
            ))
        })?;
        Ok(BaboonDomainVersion::new(&model_version.domain_identifier, best_same))
    }

    // --- Bin/JSON encode entry points ---

    /// Synthesize a `BaboonTypeMeta` from a runtime value (mirrors Scala
    /// `BaboonTypeMeta.from`/C# `BaboonTypeMeta.From`).
    ///
    /// `is_adt_trait` is the runtime stand-in for Scala/C#'s static `ClassTag`/`Type`
    /// `IsInterface` test: when the call site's *declared* (static) type is an ADT trait,
    /// pass `true` to use the ADT's type-identifier instead of the branch's. The default
    /// `encode_to_*` entry points pass `false` (declared type is the branch); generator-emitted
    /// trait-typed callers pass `true`.
    ///
    /// Codegen invariant (mirrors Scala BaboonRuntimeShared.scala:138 / C# BaboonTypeMeta.cs:110):
    /// `baboon_same_in_versions_dyn()` is non-empty. Index `[0]` here panics on violation.
    fn type_meta_from(&self, value: &dyn BaboonGeneratedDyn, is_adt_trait: bool) -> BaboonTypeMeta {
        let type_identifier = if is_adt_trait {
            match value.as_adt_member_meta_dyn() {
                Some(adt) => adt.baboon_adt_type_identifier_dyn(),
                None => value.baboon_type_identifier_dyn().to_string(),
            }
        } else {
            value.baboon_type_identifier_dyn().to_string()
        };
        let same_in = value.baboon_same_in_versions_dyn();
        // PR-08-D02 fail-fast invariant: codegen must always emit a non-empty same-in-versions
        // list. A panic here surfaces a generator regression (not a recoverable user error).
        assert!(
            !same_in.is_empty(),
            "BaboonGeneratedDyn::baboon_same_in_versions_dyn returned empty; codegen invariant violated for [{}.{}]",
            value.baboon_domain_identifier_dyn(),
            value.baboon_type_identifier_dyn()
        );
        let min_compat = same_in[0].clone();
        BaboonTypeMeta::new(
            BaboonTypeMeta::META_VERSION,
            value.baboon_domain_identifier_dyn(),
            value.baboon_domain_version_dyn(),
            min_compat,
            type_identifier,
        )
    }

    pub fn encode_to_bin_with_override(
        &self,
        ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
        type_meta_override: Option<&BaboonTypeMeta>,
    ) -> Result<Vec<u8>, BaboonCodecError> {
        self.encode_to_bin_with_declared_trait(ctx, value, type_meta_override, false)
    }

    /// Trait-aware encode entry point. Pass `is_adt_trait = true` when the static-declared
    /// type at the call site is an ADT trait (mirrors C#'s `typeof(T) is Interface`). The
    /// codec generator decides which to call based on the declared type. Other entry points
    /// default to `false`.
    pub fn encode_to_bin_with_declared_trait(
        &self,
        ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
        type_meta_override: Option<&BaboonTypeMeta>,
        is_adt_trait: bool,
    ) -> Result<Vec<u8>, BaboonCodecError> {
        let type_meta = self.type_meta_from(value, is_adt_trait);
        let codec = self.get_bin_codec(&type_meta, true)?;
        let effective = type_meta_override.unwrap_or(&type_meta);
        let mut buf = Vec::new();
        baboon_type_meta_codec::write_bin(effective, &mut buf).map_err(|e| {
            BaboonCodecError::encoder_failure(format!(
                "Cannot write type meta prefix for type [{}.{}] of version '{}': {}",
                type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
            ))
        })?;
        codec.encode_dyn(ctx, &mut buf, value).map_err(|e| {
            BaboonCodecError::encoder_failure(format!(
                "Exception while trying to encode to binary form type [{}.{}] of version '{}': {}",
                type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
            ))
        })?;
        Ok(buf)
    }

    pub fn encode_to_bin(
        &self,
        ctx: &BaboonCodecContext,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<Vec<u8>, BaboonCodecError> {
        self.encode_to_bin_with_override(ctx, value, None)
    }

    pub fn encode_to_json_with_override(
        &self,
        value: &dyn BaboonGeneratedDyn,
        type_meta_override: Option<&BaboonTypeMeta>,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        self.encode_to_json_with_declared_trait(value, type_meta_override, false)
    }

    pub fn encode_to_json_with_declared_trait(
        &self,
        value: &dyn BaboonGeneratedDyn,
        type_meta_override: Option<&BaboonTypeMeta>,
        is_adt_trait: bool,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        let type_meta = self.type_meta_from(value, is_adt_trait);
        let codec = self.get_json_codec(&type_meta, true)?;
        let content = codec
            .encode_json_dyn(&BaboonCodecContext::Compact, value)
            .map_err(|e| {
                BaboonCodecError::encoder_failure(format!(
                    "Cannot encode to json form type [{}] of version '{}': {}",
                    value.baboon_type_identifier_dyn(),
                    value.baboon_domain_version_dyn(),
                    e
                ))
            })?;
        let effective = type_meta_override.unwrap_or(&type_meta);
        let mut envelope = serde_json::Map::new();
        // MFACADE-PR-3: always emit `$mv` as a JSON number so envelopes are
        // self-identifying without out-of-band knowledge (proposal §10.6 (a)).
        envelope.insert(
            "$mv".to_string(),
            serde_json::Value::Number(serde_json::Number::from(BaboonTypeMeta::META_VERSION)),
        );
        envelope.insert(
            "$d".to_string(),
            serde_json::Value::String(effective.domain_identifier.clone()),
        );
        envelope.insert(
            "$v".to_string(),
            serde_json::Value::String(effective.domain_version.clone()),
        );
        envelope.insert(
            "$t".to_string(),
            serde_json::Value::String(effective.type_identifier.clone()),
        );
        if effective.domain_version != effective.domain_version_min_compat
            && !effective.domain_version_min_compat.is_empty()
        {
            envelope.insert(
                "$uv".to_string(),
                serde_json::Value::String(effective.domain_version_min_compat.clone()),
            );
        }
        envelope.insert(CONTENT_JSON_KEY.to_string(), content);
        Ok(serde_json::Value::Object(envelope))
    }

    pub fn encode_to_json(
        &self,
        value: &dyn BaboonGeneratedDyn,
    ) -> Result<serde_json::Value, BaboonCodecError> {
        self.encode_to_json_with_override(value, None)
    }

    // --- Bin/JSON decode entry points ---

    /// Reads a `BaboonTypeMeta` prefix from `reader`, then dispatches to the matching codec.
    /// Mirrors C# `DecodeFromBin(BinaryReader)` (BaboonCodecsFacade.cs:420). The codec lookup
    /// uses `exact = false` so the resolver may route through min-compat version negotiation.
    pub fn decode_from_bin<R: Read>(
        &self,
        reader: &mut R,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let type_meta = baboon_type_meta_codec::read_bin(reader)?
            .ok_or_else(|| BaboonCodecError::decoder_failure("Cannot decode binary type meta"))?;
        let codec = self.get_bin_codec(&type_meta, false)?;
        codec.decode_dyn(&BaboonCodecContext::Compact, reader).map_err(|e| {
            BaboonCodecError::decoder_failure(format!(
                "Can not decode BIN form type [{}.{}] of version '{}': {}",
                type_meta.domain_identifier, type_meta.type_identifier, type_meta.domain_version, e
            ))
        })
    }

    /// Byte-slice convenience wrapper. Mirrors C#'s `DecodeFromBin(byte[])` (line 451).
    pub fn decode_from_bin_bytes(
        &self,
        bytes: &[u8],
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let mut cursor = Cursor::new(bytes);
        self.decode_from_bin(&mut cursor)
    }

    /// Mirrors C# `DecodeFromJson(JToken)` (BaboonCodecsFacade.cs:498). Returns
    /// `Ok(None)` when `value` is not a meta envelope (no readable type meta or no `$c`
    /// content key) — i.e. the user passed a non-Baboon JSON object. `Err` is reserved for
    /// codec-resolution and decode failures.
    pub fn decode_from_json(
        &self,
        value: &serde_json::Value,
    ) -> Result<Option<Box<dyn BaboonGeneratedDyn>>, BaboonCodecError> {
        let maybe_meta = baboon_type_meta_codec::read_meta_json(value)?;
        let type_meta = match maybe_meta {
            Some(m) => m,
            None => return Ok(None),
        };
        let content = match value.as_object().and_then(|o| o.get(CONTENT_JSON_KEY)) {
            Some(c) => c,
            None => return Ok(None),
        };
        let codec = self.get_json_codec(&type_meta, false)?;
        let decoded = codec
            .decode_json_dyn(&BaboonCodecContext::Compact, content)
            .map_err(|e| {
                BaboonCodecError::decoder_failure(format!(
                    "Can not decode JSON form type [{}.{}] of version '{}': {}",
                    type_meta.domain_identifier,
                    type_meta.type_identifier,
                    type_meta.domain_version,
                    e
                ))
            })?;
        Ok(Some(decoded))
    }

    /// String convenience wrapper. Mirrors C# `DecodeFromJson(string)` (line 533).
    pub fn decode_from_json_str(
        &self,
        s: &str,
    ) -> Result<Option<Box<dyn BaboonGeneratedDyn>>, BaboonCodecError> {
        let parsed: serde_json::Value = serde_json::from_str(s).map_err(|e| {
            BaboonCodecError::decoder_failure(format!("Cannot parse JSON: {}", e))
        })?;
        self.decode_from_json(&parsed)
    }

    // --- Cross-version conversion ---

    /// Mirrors C# `Convert<TFrom, TTo>` (BaboonCodecsFacade.cs:548). Walks the registered
    /// version list from the value's current version to the latest, applying each step's
    /// best-matching conversion. Returns the type-erased converted value; use
    /// `convert_typed::<TTo>` for a downcast wrapper.
    pub fn convert(
        &self,
        value: Box<dyn BaboonGeneratedDyn>,
    ) -> Result<Box<dyn BaboonGeneratedDyn>, BaboonCodecError> {
        let domain_id = value.baboon_domain_identifier_dyn().to_string();
        let dv_from =
            BaboonDomainVersion::new(domain_id.clone(), value.baboon_domain_version_dyn());

        let versions = {
            let map = self.domain_versions.lock().expect("domain_versions mutex poisoned");
            map.get(&domain_id).cloned()
        };
        let versions = versions.filter(|v| !v.is_empty()).ok_or_else(|| {
            BaboonCodecError::converter_failure(format!("Unknown domain '{}'.", domain_id))
        })?;

        if !versions.iter().any(|v| v == &dv_from) {
            return Err(BaboonCodecError::converter_failure(format!(
                "Unknown domain version' {}'.",
                dv_from
            )));
        }

        let mut current = value;
        for to_version in &versions {
            let current_v = BaboonDomainVersion::new(
                domain_id.clone(),
                current.baboon_domain_version_dyn(),
            );
            // Skip versions that the current value is already at-or-past. Mirrors C#'s
            // `current.BaboonDomainVersion() == toVersion.DomainVersion || ... >= toVersion.Version`.
            if current_v.domain_version == to_version.domain_version
                || current_v.version()? >= to_version.version()?
            {
                continue;
            }

            let conv_lazy = {
                let map = self.versions_conversions.lock().expect("versions_conversions mutex poisoned");
                map.get(to_version).cloned()
            };
            let conv_lazy = conv_lazy.ok_or_else(|| {
                BaboonCodecError::converter_failure(format!(
                    "Can not find version '{}' conversions.",
                    to_version
                ))
            })?;
            let conversions = conv_lazy.get();

            let candidates = conversions.find_conversions(&*current);
            let current_type_id = current.baboon_type_id_dyn();
            let current_adt_type_id = current
                .as_adt_member_meta_dyn()
                .map(|m| m.baboon_adt_type_id_dyn());

            let mut best: Option<Arc<dyn BaboonAnyConversion>> = None;
            let mut best_v: Option<BaboonVersion> = None;
            for c in candidates {
                let matches = c.type_from() == current_type_id
                    || current_adt_type_id == Some(c.type_from());
                if !matches {
                    continue;
                }
                let v = c.version_to_parsed()?;
                if best_v.map(|b| v > b).unwrap_or(true) {
                    best_v = Some(v);
                    best = Some(c);
                }
            }
            let conversion = best.ok_or_else(|| {
                BaboonCodecError::converter_failure(format!(
                    "Can not find version '{}' type [{}] conversions.",
                    to_version,
                    current.baboon_type_identifier_dyn()
                ))
            })?;
            let from_version = current.baboon_domain_version_dyn().to_string();
            current = conversions.convert(current, conversion.as_ref()).map_err(|e| {
                BaboonCodecError::converter_failure(format!(
                    "Exception while converting type of version '{}' to version '{}': {}",
                    from_version, to_version, e
                ))
            })?;
        }
        Ok(current)
    }

    /// Typed wrapper around `convert`. Mirrors C#'s `Convert<TFrom, TTo>` final-cast tail.
    /// `TTo` must be `'static` so its `TypeId` is known; the downcast goes through
    /// `Box<dyn Any>` via `BaboonGeneratedDyn::into_any`.
    pub fn convert_typed<TTo: BaboonGeneratedDyn + 'static>(
        &self,
        value: Box<dyn BaboonGeneratedDyn>,
    ) -> Result<Box<TTo>, BaboonCodecError> {
        // Identity short-circuit: already the target type.
        if (*value).type_id() == TypeId::of::<TTo>() {
            let any: Box<dyn std::any::Any> = value.into_any();
            return any.downcast::<TTo>().map_err(|_| {
                BaboonCodecError::converter_failure(
                    "Internal: TypeId-matched value failed downcast (impossible)",
                )
            });
        }
        let converted = self.convert(value)?;
        let converted_type_name = converted.baboon_type_identifier_dyn().to_string();
        if (*converted).type_id() == TypeId::of::<TTo>() {
            let any: Box<dyn std::any::Any> = converted.into_any();
            any.downcast::<TTo>().map_err(|_| {
                BaboonCodecError::converter_failure(
                    "Internal: TypeId-matched value failed downcast (impossible)",
                )
            })
        } else {
            Err(BaboonCodecError::converter_failure(format!(
                "Expected to have target type at the end, but got [{}].",
                converted_type_name
            )))
        }
    }
}

