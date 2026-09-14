// BaboonTypeMeta — the top-level codec envelope (docs/spec/codec-envelope.md) and its wire codec.
// Split out of `baboon_codecs_facade.rs`, which re-exports both symbols, to keep each embedded
// runtime file under the JVM's 64KB string-constant limit.

use crate::any_opaque::BaboonCodecError;
use crate::baboon_codecs_facade::BaboonDomainVersion;

// --- BaboonTypeMeta (synthetic; used for facade lookup) ---

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BaboonTypeMeta {
    pub meta_version: u8,
    pub domain_identifier: String,
    pub domain_version: String,
    pub domain_version_min_compat: String,
    pub type_identifier: String,
    /// Oldest domain version whose JSON codec can decode the payload under the json-additive
    /// contract (tolerant key lookup; fields unknown to that version are dropped). Always
    /// <= domain_version_min_compat. Published as `$rv` when it differs from the (effective)
    /// minCompat; the binary v1 envelope does not carry it. `new` defaults it to minCompat.
    pub domain_version_readable_min: String,
}

impl BaboonTypeMeta {
    pub const META_VERSION_1: u8 = 1;
    pub const META_VERSION: u8 = Self::META_VERSION_1;
    /// Tier key of the JSON envelope's readable-min bound in `baboon_min_reader_versions_dyn`.
    pub const JSON_READABLE_TIER: &'static str = "json-additive";
    /// Tier keys of the UEBA prefix bounds in `baboon_min_reader_versions_dyn`, per index mode.
    pub const UEBA_PREFIX_COMPACT_TIER: &'static str = "prefix-compact";
    pub const UEBA_PREFIX_ANY_MODE_TIER: &'static str = "prefix-any-mode";

    pub fn new<D: Into<String>, V: Into<String>, MC: Into<String>, T: Into<String>>(
        meta_version: u8,
        domain_identifier: D,
        domain_version: V,
        domain_version_min_compat: MC,
        type_identifier: T,
    ) -> Self {
        let domain_version_min_compat: String = domain_version_min_compat.into();
        BaboonTypeMeta {
            meta_version,
            domain_identifier: domain_identifier.into(),
            domain_version: domain_version.into(),
            domain_version_readable_min: domain_version_min_compat.clone(),
            domain_version_min_compat,
            type_identifier: type_identifier.into(),
        }
    }

    pub fn with_readable_min<R: Into<String>>(mut self, readable_min: R) -> Self {
        self.domain_version_readable_min = readable_min.into();
        self
    }

    pub fn version_readable_min(&self) -> Option<BaboonDomainVersion> {
        if self.domain_version_readable_min.is_empty() {
            return self.version_min_compat();
        }
        if self.domain_version_readable_min == self.domain_version {
            None
        } else {
            Some(BaboonDomainVersion::new(&self.domain_identifier, &self.domain_version_readable_min))
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

pub mod baboon_type_meta_codec {
    use super::{BaboonTypeMeta, BaboonCodecError};
    use crate::baboon_runtime::bin_tools;
    use std::io::{Read, Write};

    pub const META_VERSION_KEY: &str = "$mv";
    pub const DOMAIN_IDENTIFIER_KEY: &str = "$d";
    pub const DOMAIN_VERSION_KEY: &str = "$v";
    pub const DOMAIN_VERSION_MIN_COMPAT_KEY: &str = "$uv";
    pub const DOMAIN_VERSION_READABLE_KEY: &str = "$rv";
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
        // codec-envelope.md §2.1: only 0x00 (elided) and 0x01 (present) are legal; anything else is rejected
        if has_min_compat != 0 && has_min_compat != 1 {
            return Ok(None);
        }
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

    /// JSON envelope writer. Mirrors C# `BaboonTypeMetaCodec.WriteJson` (BaboonTypeMeta.cs:156)
    /// and Scala `writeMeta(json)` — returns the `{$mv,$d,$v,$t,$uv?}` object without `$c`.
    /// Callers (e.g. `BaboonCodecsFacade::encode_to_json`) are expected to append the content
    /// payload under the `$c` key. Symmetric to `read_meta_json` so the rust runtime exposes
    /// the same shape of public envelope API as the peer backends; closes
    /// `[MFACADE-PR-3-D13]`.
    pub fn write_meta_json(meta: &BaboonTypeMeta) -> serde_json::Map<String, serde_json::Value> {
        let mut envelope = serde_json::Map::new();
        // MFACADE-PR-3: always emit `$mv` as a JSON number so envelopes are
        // self-identifying without out-of-band knowledge (proposal §10.6 (a)).
        envelope.insert(
            META_VERSION_KEY.to_string(),
            serde_json::Value::Number(serde_json::Number::from(BaboonTypeMeta::META_VERSION)),
        );
        envelope.insert(
            DOMAIN_IDENTIFIER_KEY.to_string(),
            serde_json::Value::String(meta.domain_identifier.clone()),
        );
        envelope.insert(
            DOMAIN_VERSION_KEY.to_string(),
            serde_json::Value::String(meta.domain_version.clone()),
        );
        envelope.insert(
            TYPE_IDENTIFIER_KEY.to_string(),
            serde_json::Value::String(meta.type_identifier.clone()),
        );
        if meta.domain_version != meta.domain_version_min_compat
            && !meta.domain_version_min_compat.is_empty()
        {
            envelope.insert(
                DOMAIN_VERSION_MIN_COMPAT_KEY.to_string(),
                serde_json::Value::String(meta.domain_version_min_compat.clone()),
            );
        }
        // `$rv` is elided when it equals the effective `$uv`: unchanged types emit no new bytes
        if !meta.domain_version_readable_min.is_empty()
            && meta.domain_version_readable_min != meta.domain_version_min_compat
        {
            envelope.insert(
                DOMAIN_VERSION_READABLE_KEY.to_string(),
                serde_json::Value::String(meta.domain_version_readable_min.clone()),
            );
        }
        envelope
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
        let rv = obj
            .get(DOMAIN_VERSION_READABLE_KEY)
            .and_then(|v| v.as_str())
            .unwrap_or(uv);
        Ok(Some(
            BaboonTypeMeta::new(BaboonTypeMeta::META_VERSION, d, v, uv, t).with_readable_min(rv),
        ))
    }
}
