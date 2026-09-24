use crate::baboon_runtime::bin_tools;
use serde::de::{self, MapAccess, Visitor};
use serde::ser::{SerializeMap, Serializer};
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::HashSet;
use std::fmt;
use std::io::{Read, Write};

// --- Error type ---
//
// Mirrors Scala's `BaboonCodecException` ADT and C#'s `BaboonCodecException` hierarchy.
// Unlike `Box<dyn std::error::Error>` (which the lower-level `bin_tools` module uses for
// trait fallibles), the facade-side error type carries enough structure for callers to
// match on the failure category. All variants accept an optional `cause` for chaining.

#[derive(Debug)]
pub enum BaboonCodecError {
    EncoderFailure {
        message: String,
        cause: Option<Box<dyn std::error::Error + Send + Sync>>,
    },
    DecoderFailure {
        message: String,
        cause: Option<Box<dyn std::error::Error + Send + Sync>>,
    },
    ConverterFailure {
        message: String,
        cause: Option<Box<dyn std::error::Error + Send + Sync>>,
    },
    CodecNotFound {
        message: String,
    },
    ConversionNotFound {
        message: String,
    },
}

impl BaboonCodecError {
    pub fn encoder_failure<S: Into<String>>(message: S) -> Self {
        BaboonCodecError::EncoderFailure { message: message.into(), cause: None }
    }

    pub fn encoder_failure_with<S: Into<String>, E: std::error::Error + Send + Sync + 'static>(
        message: S,
        cause: E,
    ) -> Self {
        BaboonCodecError::EncoderFailure { message: message.into(), cause: Some(Box::new(cause)) }
    }

    pub fn decoder_failure<S: Into<String>>(message: S) -> Self {
        BaboonCodecError::DecoderFailure { message: message.into(), cause: None }
    }

    pub fn decoder_failure_with<S: Into<String>, E: std::error::Error + Send + Sync + 'static>(
        message: S,
        cause: E,
    ) -> Self {
        BaboonCodecError::DecoderFailure { message: message.into(), cause: Some(Box::new(cause)) }
    }

    /// Convert from `bin_tools`'s `Box<dyn std::error::Error>` (non-Send/Sync) by stringifying.
    /// The structured error chain is sacrificed for the type bound; the message preserves enough
    /// context. Mirrors PR-04 / PR-05 lessons on error-channel discipline.
    pub fn decoder_failure_from_box<S: Into<String>>(
        message: S,
        cause: Box<dyn std::error::Error>,
    ) -> Self {
        BaboonCodecError::DecoderFailure {
            message: format!("{}: {}", message.into(), cause),
            cause: None,
        }
    }

    pub fn encoder_failure_from_box<S: Into<String>>(
        message: S,
        cause: Box<dyn std::error::Error>,
    ) -> Self {
        BaboonCodecError::EncoderFailure {
            message: format!("{}: {}", message.into(), cause),
            cause: None,
        }
    }

    pub fn converter_failure<S: Into<String>>(message: S) -> Self {
        BaboonCodecError::ConverterFailure { message: message.into(), cause: None }
    }

    pub fn converter_failure_with<S: Into<String>, E: std::error::Error + Send + Sync + 'static>(
        message: S,
        cause: E,
    ) -> Self {
        BaboonCodecError::ConverterFailure { message: message.into(), cause: Some(Box::new(cause)) }
    }

    pub fn codec_not_found<S: Into<String>>(message: S) -> Self {
        BaboonCodecError::CodecNotFound { message: message.into() }
    }

    pub fn conversion_not_found<S: Into<String>>(message: S) -> Self {
        BaboonCodecError::ConversionNotFound { message: message.into() }
    }
}

impl fmt::Display for BaboonCodecError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BaboonCodecError::EncoderFailure { message, .. } => write!(f, "EncoderFailure: {}", message),
            BaboonCodecError::DecoderFailure { message, .. } => write!(f, "DecoderFailure: {}", message),
            BaboonCodecError::ConverterFailure { message, .. } => write!(f, "ConverterFailure: {}", message),
            BaboonCodecError::CodecNotFound { message } => write!(f, "CodecNotFound: {}", message),
            BaboonCodecError::ConversionNotFound { message } => write!(f, "ConversionNotFound: {}", message),
        }
    }
}

pub mod any_field_codec {
    pub fn encode_any_field(
        ctx: &crate::baboon_runtime::BaboonCodecContext,
        writer: &mut dyn std::io::Write,
        expected_kind: u8,
        static_domain: Option<&str>,
        static_version: Option<&str>,
        static_typeid: Option<&str>,
        value: &crate::any_opaque::AnyOpaque,
    ) -> std::io::Result<()> {
        if value.meta().kind != expected_kind {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "any: meta-kind 0x{:02x} does not match field-declared 0x{:02x}",
                    value.meta().kind, expected_kind
                ),
            ));
        }
        let any_blob: std::borrow::Cow<'_, [u8]> = match value {
            crate::any_opaque::AnyOpaque::Ueba(u) => std::borrow::Cow::Borrowed(&u.bytes),
            crate::any_opaque::AnyOpaque::Json(j) => {
                let f = ctx.facade().ok_or_else(|| std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "Cannot encode AnyOpaque::Json into UEBA without a facade reference. Construct the codec context via BaboonCodecContext::with_facade(use_indices, facade), or supply AnyOpaque::Ueba directly."
                ))?;
                std::borrow::Cow::Owned(f.json_to_ueba_bytes(ctx, &j.meta, &j.json, static_domain, static_version, static_typeid)
                    .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{}", e)))?)
            }
        };
        // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
        let mut any_meta_buf: Vec<u8> = Vec::new();
        crate::any_opaque::any_meta_codec::write_bin(value.meta(), &mut any_meta_buf)?;
        let any_total_length: i32 = (4 + any_meta_buf.len() + any_blob.len()) as i32;
        crate::baboon_runtime::bin_tools::write_i32(writer, any_total_length)?;
        crate::baboon_runtime::bin_tools::write_i32(writer, any_meta_buf.len() as i32)?;
        writer.write_all(&any_meta_buf)?;
        writer.write_all(&any_blob)?;
        Ok(())
    }

    pub fn decode_any_field(
        wire: &mut dyn std::io::Read,
        expected_kind: u8,
    ) -> Result<crate::any_opaque::AnyOpaqueUeba, Box<dyn std::error::Error>> {
        let any_total_length_i = crate::baboon_runtime::bin_tools::read_i32(wire)?;
        if any_total_length_i < 0 {
            return Err(format!(
                "any: negative total-length {}", any_total_length_i
            ).into());
        }
        let any_total_length = any_total_length_i as usize;
        let any_meta_length_i = crate::baboon_runtime::bin_tools::read_i32(wire)?;
        if any_meta_length_i < 0 {
            return Err(format!(
                "any: negative meta-length {}", any_meta_length_i
            ).into());
        }
        let any_meta_length = any_meta_length_i as usize;
        if any_total_length < 4 + any_meta_length {
            return Err(format!(
                "any: total-length {} smaller than 4 + meta-length {}",
                any_total_length, any_meta_length
            ).into());
        }
        let (any_meta, any_bytes_read) = crate::any_opaque::any_meta_codec::read_bin_with_length(wire)
            .map_err(|e| -> Box<dyn std::error::Error> { Box::new(e) })?;
        if any_bytes_read > any_meta_length {
            return Err(format!(
                "any: meta-bytes-read {} exceeded meta-length window {}",
                any_bytes_read, any_meta_length
            ).into());
        }
        if any_bytes_read < any_meta_length {
            // Forward-compat: skip future meta-extension bytes within the meta-length window.
            let mut any_skip = vec![0u8; any_meta_length - any_bytes_read];
            wire.read_exact(&mut any_skip)?;
        }
        if any_meta.kind != expected_kind {
            return Err(format!(
                "any: wire kind 0x{:02x} does not match field-declared 0x{:02x}",
                any_meta.kind, expected_kind
            ).into());
        }
        let any_blob_len = any_total_length - 4 - any_meta_length;
        let mut any_blob = vec![0u8; any_blob_len];
        wire.read_exact(&mut any_blob)?;
        Ok(crate::any_opaque::AnyOpaqueUeba::new(any_meta, any_blob))
    }

    /// Reads an `any` field from its JSON envelope. Always yields the JSON branch — a JSON
    /// payload is already in JSON form, and no backend's `any` decoder needs a facade. The
    /// declared kind byte is checked here because only the field site knows it.
    pub fn any_from_json(
        wire: &serde_json::Value,
        expected_kind: u8,
    ) -> Result<crate::any_opaque::AnyOpaqueJson, crate::any_opaque::BaboonCodecError> {
        let meta = crate::any_opaque::any_meta_codec::read_json(wire)?;
        if meta.kind != expected_kind {
            return Err(crate::any_opaque::BaboonCodecError::decoder_failure(format!(
                "any: wire kind 0x{:02x} does not match field-declared 0x{:02x}",
                meta.kind, expected_kind
            )));
        }
        let content = wire
            .as_object()
            .and_then(|o| o.get(crate::any_opaque::any_meta_codec::ANY_CONTENT_KEY))
            .ok_or_else(|| {
                crate::any_opaque::BaboonCodecError::decoder_failure(format!(
                    "any: JSON envelope missing '{}' content key",
                    crate::any_opaque::any_meta_codec::ANY_CONTENT_KEY
                ))
            })?;
        Ok(crate::any_opaque::AnyOpaqueJson::new(meta, content.clone()))
    }

    /// Writes an `any` field as its JSON envelope, transcoding a UEBA-form payload when needed.
    ///
    /// The field site is the only place that knows the declared kind byte and the static
    /// fallbacks the facade needs to resolve a wire `meta` that deliberately omits what the
    /// declaration already pins down, so the generated JSON encoder calls through here rather
    /// than relying on `Serialize for AnyOpaque` — which gets no codec context and therefore no
    /// facade.
    pub fn any_to_json(
        ctx: &crate::baboon_runtime::BaboonCodecContext,
        expected_kind: u8,
        static_domain: Option<&str>,
        static_version: Option<&str>,
        static_typeid: Option<&str>,
        value: &crate::any_opaque::AnyOpaque,
    ) -> Result<serde_json::Value, crate::any_opaque::BaboonCodecError> {
        if value.meta().kind != expected_kind {
            return Err(crate::any_opaque::BaboonCodecError::encoder_failure(format!(
                "any: meta-kind 0x{:02x} does not match field-declared 0x{:02x}",
                value.meta().kind, expected_kind
            )));
        }
        let inner = match value {
            crate::any_opaque::AnyOpaque::Json(j) => j.json.clone(),
            crate::any_opaque::AnyOpaque::Ueba(u) => {
                let f = ctx.facade().ok_or_else(|| {
                    crate::any_opaque::BaboonCodecError::encoder_failure(
                        "Cannot encode AnyOpaque::Ueba into JSON without a facade reference. Construct the codec context via BaboonCodecContext::with_facade(use_indices, facade), or supply AnyOpaque::Json directly.",
                    )
                })?;
                f.ueba_to_json(ctx, &u.meta, &u.bytes, static_domain, static_version, static_typeid)?
            }
        };
        let mut envelope = crate::any_opaque::any_meta_codec::write_json(value.meta());
        match envelope.as_object_mut() {
            Some(map) => {
                map.insert(crate::any_opaque::any_meta_codec::ANY_CONTENT_KEY.to_string(), inner);
            }
            None => {
                return Err(crate::any_opaque::BaboonCodecError::encoder_failure(
                    "any: AnyMeta envelope must be a JSON object",
                ))
            }
        }
        Ok(envelope)
    }
}

impl std::error::Error for BaboonCodecError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            BaboonCodecError::EncoderFailure { cause: Some(c), .. }
            | BaboonCodecError::DecoderFailure { cause: Some(c), .. }
            | BaboonCodecError::ConverterFailure { cause: Some(c), .. } => {
                Some(c.as_ref() as &(dyn std::error::Error + 'static))
            }
            _ => None,
        }
    }
}

// --- AnyMeta ---

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AnyMeta {
    pub kind: u8,
    pub domain: Option<String>,
    pub version: Option<String>,
    pub typeid: Option<String>,
}

impl AnyMeta {
    /// Construct an `AnyMeta` enforcing the four locked invariants:
    /// 1. domain presence == bit 2 of kind (0x04)
    /// 2. version presence == bit 1 of kind (0x02)
    /// 3. typeid presence == bit 0 of kind (0x01)
    /// 4. kind ∈ VALID_KINDS (rejects reserved bytes 0x04/0x05)
    ///
    /// Returns `Result` rather than panicking — Rust idiom; lets callers decide. Mirrors
    /// PR-04-D02 (Scala/C# `Either`-channel discipline for user-facing codec parse).
    pub fn new(
        kind: u8,
        domain: Option<String>,
        version: Option<String>,
        typeid: Option<String>,
    ) -> Result<Self, BaboonCodecError> {
        let domain_bit_set = (kind & any_meta_codec::DOMAIN_BIT) != 0;
        if domain_bit_set != domain.is_some() {
            return Err(BaboonCodecError::decoder_failure(format!(
                "AnyMeta: domain presence ({}) does not match kind 0x{:x} bit 2",
                domain.is_some(),
                kind
            )));
        }
        let version_bit_set = (kind & any_meta_codec::VERSION_BIT) != 0;
        if version_bit_set != version.is_some() {
            return Err(BaboonCodecError::decoder_failure(format!(
                "AnyMeta: version presence ({}) does not match kind 0x{:x} bit 1",
                version.is_some(),
                kind
            )));
        }
        let typeid_bit_set = (kind & any_meta_codec::TYPEID_BIT) != 0;
        if typeid_bit_set != typeid.is_some() {
            return Err(BaboonCodecError::decoder_failure(format!(
                "AnyMeta: typeid presence ({}) does not match kind 0x{:x} bit 0",
                typeid.is_some(),
                kind
            )));
        }
        if !any_meta_codec::is_valid_kind(kind) {
            return Err(BaboonCodecError::decoder_failure(format!(
                "AnyMeta: reserved or invalid meta-kind byte: 0x{:02x}",
                kind
            )));
        }
        Ok(AnyMeta { kind, domain, version, typeid })
    }
}

// --- AnyOpaque ---
//
// Sealed ADT mirroring Scala's `AnyOpaque`/C#'s `AnyOpaque`. Rust-idiomatic enum.
//
// Auto-derived `PartialEq` is content-wise:
//  - `Vec<u8>` PartialEq compares contents byte-by-byte (unlike Scala/C# Array reference identity).
//  - `serde_json::Value` PartialEq is structural (verified — equality recurses into objects/arrays).
// So no manual override is needed (mirrors PR-05-D08 lesson — non-issue in Rust).

#[derive(Debug, Clone, PartialEq)]
pub enum AnyOpaque {
    Ueba(AnyOpaqueUeba),
    Json(AnyOpaqueJson),
}

impl AnyOpaque {
    pub fn meta(&self) -> &AnyMeta {
        match self {
            AnyOpaque::Ueba(u) => &u.meta,
            AnyOpaque::Json(j) => &j.meta,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnyOpaqueUeba {
    pub meta: AnyMeta,
    pub bytes: Vec<u8>,
}

impl AnyOpaqueUeba {
    pub fn new(meta: AnyMeta, bytes: Vec<u8>) -> Self {
        AnyOpaqueUeba { meta, bytes }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AnyOpaqueJson {
    pub meta: AnyMeta,
    pub json: serde_json::Value,
}

impl AnyOpaqueJson {
    pub fn new(meta: AnyMeta, json: serde_json::Value) -> Self {
        AnyOpaqueJson { meta, json }
    }
}

// --- Custom serde Serialize/Deserialize for AnyOpaque ---
//
// Rust's serde derive would emit `{"Ueba": {...}}` / `{"Json": {...}}` — NOT the locked
// `{"$ak", "$ad"?, "$av"?, "$at"?, "$c"}` envelope. Custom impls below produce/consume
// the envelope shape required by the cross-language wire spec.
//
// Behavior:
//   Serialize  AnyOpaque::Json(j)  → emits the envelope with $c = j.json.
//   Serialize  AnyOpaque::Ueba(_)  → returns an error. The Ueba branch's bytes can only
//                                    be cross-converted to JSON via the `BaboonCodecsFacade`
//                                    (which has codec access). Consistent with PR 3.1/3.2/3.3
//                                    fail-fast: codec generators route Ueba → JSON via
//                                    `facade.ueba_to_json(...)` *before* serde, so this
//                                    branch should not be reached in generator-emitted code.
//   Deserialize → AnyOpaque::Json(j)  always (matches Scala/C# decoder, which always
//                                      returns the native branch from the input format).

impl Serialize for AnyOpaque {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            AnyOpaque::Ueba(_) => Err(serde::ser::Error::custom(
                "AnyOpaque::Ueba cannot be JSON-serialized directly; use BaboonCodecsFacade::ueba_to_json to convert first",
            )),
            AnyOpaque::Json(j) => {
                let meta = &j.meta;
                let mut count = 2usize; // $ak, $c
                if meta.domain.is_some() {
                    count += 1;
                }
                if meta.version.is_some() {
                    count += 1;
                }
                if meta.typeid.is_some() {
                    count += 1;
                }
                let mut map = serializer.serialize_map(Some(count))?;
                map.serialize_entry(any_meta_codec::ANY_KIND_KEY, &(meta.kind as u32))?;
                if let Some(d) = &meta.domain {
                    map.serialize_entry(any_meta_codec::ANY_DOMAIN_KEY, d)?;
                }
                if let Some(v) = &meta.version {
                    map.serialize_entry(any_meta_codec::ANY_VERSION_KEY, v)?;
                }
                if let Some(t) = &meta.typeid {
                    map.serialize_entry(any_meta_codec::ANY_TYPEID_KEY, t)?;
                }
                map.serialize_entry(any_meta_codec::ANY_CONTENT_KEY, &j.json)?;
                map.end()
            }
        }
    }
}

impl<'de> Deserialize<'de> for AnyOpaque {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct AnyOpaqueVisitor;

        impl<'de> Visitor<'de> for AnyOpaqueVisitor {
            type Value = AnyOpaque;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "an AnyOpaque envelope object with $ak/$c keys")
            }

            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<AnyOpaque, M::Error> {
                let mut kind: Option<u8> = None;
                let mut domain: Option<String> = None;
                let mut version: Option<String> = None;
                let mut typeid: Option<String> = None;
                let mut content: Option<serde_json::Value> = None;

                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        k if k == any_meta_codec::ANY_KIND_KEY => {
                            let v: u32 = map.next_value()?;
                            if v > 0xFF {
                                return Err(de::Error::custom(format!(
                                    "AnyOpaque deserialize: '$ak' value out of u8 range: {}",
                                    v
                                )));
                            }
                            kind = Some(v as u8);
                        }
                        k if k == any_meta_codec::ANY_DOMAIN_KEY => {
                            domain = Some(map.next_value()?);
                        }
                        k if k == any_meta_codec::ANY_VERSION_KEY => {
                            version = Some(map.next_value()?);
                        }
                        k if k == any_meta_codec::ANY_TYPEID_KEY => {
                            typeid = Some(map.next_value()?);
                        }
                        k if k == any_meta_codec::ANY_CONTENT_KEY => {
                            content = Some(map.next_value()?);
                        }
                        _ => {
                            // forward-compat: ignore unknown keys
                            let _: serde::de::IgnoredAny = map.next_value()?;
                        }
                    }
                }

                let kind = kind.ok_or_else(|| {
                    de::Error::custom(format!(
                        "AnyOpaque deserialize: missing '{}' field",
                        any_meta_codec::ANY_KIND_KEY
                    ))
                })?;
                let content = content.ok_or_else(|| {
                    de::Error::custom(format!(
                        "AnyOpaque deserialize: missing '{}' field",
                        any_meta_codec::ANY_CONTENT_KEY
                    ))
                })?;

                let meta = AnyMeta::new(kind, domain, version, typeid)
                    .map_err(|e| de::Error::custom(format!("AnyOpaque deserialize: {}", e)))?;
                Ok(AnyOpaque::Json(AnyOpaqueJson::new(meta, content)))
            }
        }

        deserializer.deserialize_map(AnyOpaqueVisitor)
    }
}

// --- AnyMetaCodec helpers ---

pub mod any_meta_codec {
    use super::*;

    pub const DOMAIN_BIT: u8 = 0x04;
    pub const VERSION_BIT: u8 = 0x02;
    pub const TYPEID_BIT: u8 = 0x01;

    pub const ANY_KIND_KEY: &str = "$ak";
    pub const ANY_DOMAIN_KEY: &str = "$ad";
    pub const ANY_VERSION_KEY: &str = "$av";
    pub const ANY_TYPEID_KEY: &str = "$at";
    pub const ANY_CONTENT_KEY: &str = "$c";

    pub const VALID_KINDS: &[u8] = &[0x00, 0x01, 0x02, 0x03, 0x06, 0x07];

    pub fn is_valid_kind(k: u8) -> bool {
        VALID_KINDS.contains(&k)
    }

    pub fn valid_kinds_set() -> HashSet<u8> {
        VALID_KINDS.iter().copied().collect()
    }

    /// Write the kind byte followed by domain/version/typeid strings (in that fixed order)
    /// for whichever of those bits are set in the kind.
    pub fn write_bin(meta: &AnyMeta, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_byte(writer, meta.kind)?;
        if let Some(d) = &meta.domain {
            bin_tools::write_string(writer, d)?;
        }
        if let Some(v) = &meta.version {
            bin_tools::write_string(writer, v)?;
        }
        if let Some(t) = &meta.typeid {
            bin_tools::write_string(writer, t)?;
        }
        Ok(())
    }

    /// Read meta from the wire; trusts the format. Mirrors Scala's `readBin` discipline.
    /// Errors propagate as `BaboonCodecError::DecoderFailure` (typed for the user-facing
    /// facade; inner I/O errors are wrapped via `decoder_failure_with`).
    pub fn read_bin<R: Read>(reader: &mut R) -> Result<AnyMeta, BaboonCodecError> {
        let kind = bin_tools::read_byte(reader).map_err(|e| {
            BaboonCodecError::decoder_failure_from_box("AnyMetaCodec.read_bin: failed to read kind byte", e)
        })?;
        let domain = if (kind & DOMAIN_BIT) != 0 {
            Some(bin_tools::read_string(reader).map_err(|e| {
                BaboonCodecError::decoder_failure_from_box("AnyMetaCodec.read_bin: failed to read domain", e)
            })?)
        } else {
            None
        };
        let version = if (kind & VERSION_BIT) != 0 {
            Some(bin_tools::read_string(reader).map_err(|e| {
                BaboonCodecError::decoder_failure_from_box("AnyMetaCodec.read_bin: failed to read version", e)
            })?)
        } else {
            None
        };
        let typeid = if (kind & TYPEID_BIT) != 0 {
            Some(bin_tools::read_string(reader).map_err(|e| {
                BaboonCodecError::decoder_failure_from_box("AnyMetaCodec.read_bin: failed to read typeid", e)
            })?)
        } else {
            None
        };
        AnyMeta::new(kind, domain, version, typeid)
    }

    /// Counting wrapper: tracks bytes consumed during a meta read so the calling codec can
    /// skip any extra meta-extension bytes left in the on-wire `meta-length` window. Mirrors
    /// PR-05-D01 (Scala's `CountingInputStream`/C#'s `BaseStream.Position` diff).
    struct CountingReader<'a, R: Read + ?Sized> {
        inner: &'a mut R,
        count: usize,
    }

    impl<'a, R: Read + ?Sized> Read for CountingReader<'a, R> {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            let n = self.inner.read(buf)?;
            self.count += n;
            Ok(n)
        }
    }

    /// Reads meta and returns (meta, bytes_read). Forward-compat: callers that know the
    /// on-wire `meta-length` window skip any trailing future-extension bytes.
    /// `?Sized` allows passing `&mut dyn Read` from codec generators.
    pub fn read_bin_with_length<R: Read + ?Sized>(reader: &mut R) -> Result<(AnyMeta, usize), BaboonCodecError> {
        let mut counting = CountingReader { inner: reader, count: 0 };
        let meta = read_bin(&mut counting)?;
        Ok((meta, counting.count))
    }

    /// Always returns a JSON object. The JSON encoder envelope build relies on this invariant:
    /// it adds a `$c` content key onto the returned object.
    pub fn write_json(meta: &AnyMeta) -> serde_json::Value {
        let mut map = serde_json::Map::new();
        map.insert(
            ANY_KIND_KEY.to_string(),
            serde_json::Value::Number(serde_json::Number::from(meta.kind as u64)),
        );
        if let Some(d) = &meta.domain {
            map.insert(ANY_DOMAIN_KEY.to_string(), serde_json::Value::String(d.clone()));
        }
        if let Some(v) = &meta.version {
            map.insert(ANY_VERSION_KEY.to_string(), serde_json::Value::String(v.clone()));
        }
        if let Some(t) = &meta.typeid {
            map.insert(ANY_TYPEID_KEY.to_string(), serde_json::Value::String(t.clone()));
        }
        serde_json::Value::Object(map)
    }

    /// Read meta from a JSON value. Returns `Result` mirroring Scala/C# user-facing JSON parse:
    /// binary path trusts the wire and surfaces I/O errors; JSON parse threads typed errors.
    pub fn read_json(json: &serde_json::Value) -> Result<AnyMeta, BaboonCodecError> {
        let obj = json.as_object().ok_or_else(|| {
            BaboonCodecError::decoder_failure(format!(
                "AnyMetaCodec.read_json: expected object, got {}",
                json_type_name(json)
            ))
        })?;

        let kind_token = obj.get(ANY_KIND_KEY).ok_or_else(|| {
            BaboonCodecError::decoder_failure(format!(
                "AnyMetaCodec.read_json: missing or non-numeric '{}' field",
                ANY_KIND_KEY
            ))
        })?;
        let kind_u64 = kind_token.as_u64().ok_or_else(|| {
            BaboonCodecError::decoder_failure(format!(
                "AnyMetaCodec.read_json: missing or non-numeric '{}' field",
                ANY_KIND_KEY
            ))
        })?;
        if kind_u64 > 0xFF {
            return Err(BaboonCodecError::decoder_failure(format!(
                "AnyMetaCodec.read_json: '{}' value out of u8 range: {}",
                ANY_KIND_KEY, kind_u64
            )));
        }
        let kind = kind_u64 as u8;

        let domain = read_opt_string(obj, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain")?;
        let version = read_opt_string(obj, ANY_VERSION_KEY, kind, VERSION_BIT, "version")?;
        let typeid = read_opt_string(obj, ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid")?;

        AnyMeta::new(kind, domain, version, typeid)
    }

    fn read_opt_string(
        obj: &serde_json::Map<String, serde_json::Value>,
        key: &str,
        kind: u8,
        bit: u8,
        name: &str,
    ) -> Result<Option<String>, BaboonCodecError> {
        let present = (kind & bit) != 0;
        let value = obj.get(key).and_then(|v| v.as_str()).map(|s| s.to_string());
        match (present, value) {
            (true, Some(v)) => Ok(Some(v)),
            (false, None) => Ok(None),
            (true, None) => Err(BaboonCodecError::decoder_failure(format!(
                "AnyMetaCodec.read_json: kind 0x{:x} requires '{}' ({}) but it is missing",
                kind, key, name
            ))),
            (false, Some(_)) => Err(BaboonCodecError::decoder_failure(format!(
                "AnyMetaCodec.read_json: kind 0x{:x} forbids '{}' ({}) but it is present",
                kind, key, name
            ))),
        }
    }

    fn json_type_name(v: &serde_json::Value) -> &'static str {
        match v {
            serde_json::Value::Null => "null",
            serde_json::Value::Bool(_) => "boolean",
            serde_json::Value::Number(_) => "number",
            serde_json::Value::String(_) => "string",
            serde_json::Value::Array(_) => "array",
            serde_json::Value::Object(_) => "object",
        }
    }
}
