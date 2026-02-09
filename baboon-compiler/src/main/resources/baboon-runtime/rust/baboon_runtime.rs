use std::io::{Read, Write};

// --- Codec Context ---

#[derive(Clone, Debug, PartialEq)]
pub enum BaboonCodecContext {
    Default,
    Indexed,
    Compact,
}

impl Default for BaboonCodecContext {
    fn default() -> Self {
        BaboonCodecContext::Default
    }
}

impl BaboonCodecContext {
    pub fn use_indices(&self) -> bool {
        matches!(self, BaboonCodecContext::Indexed)
    }
}

// --- Index support ---

#[derive(Clone, Debug, PartialEq)]
pub struct BaboonIndexEntry {
    pub offset: u32,
    pub length: u32,
}

pub trait BaboonBinCodecIndexed {
    fn index_elements_count(ctx: &BaboonCodecContext) -> u16;

    fn read_index(
        ctx: &BaboonCodecContext,
        reader: &mut dyn Read,
    ) -> Result<(u8, Vec<BaboonIndexEntry>), Box<dyn std::error::Error>> {
        let header = bin_tools::read_byte(reader)?;
        let is_indexed = (header & 0x01) != 0;
        let mut result = Vec::new();
        let mut prev_offset: u32 = 0;
        let mut prev_len: u32 = 0;
        if is_indexed {
            let mut left = Self::index_elements_count(ctx) as usize;
            while left > 0 {
                let offset = bin_tools::read_i32(reader)? as u32;
                let len = bin_tools::read_i32(reader)? as u32;
                assert!(len > 0, "Length must be positive");
                assert!(
                    offset >= prev_offset + prev_len,
                    "Offset violation: {} not >= {}",
                    offset,
                    prev_offset + prev_len
                );
                result.push(BaboonIndexEntry { offset, length: len });
                left -= 1;
                prev_offset = offset;
                prev_len = len;
            }
        }
        Ok((header, result))
    }
}

// --- Binary codec traits ---

pub trait BaboonBinEncode {
    fn encode_ueba(
        &self,
        ctx: &BaboonCodecContext,
        writer: &mut dyn Write,
    ) -> std::io::Result<()>;
}

pub trait BaboonBinDecode: Sized {
    fn decode_ueba(
        ctx: &BaboonCodecContext,
        reader: &mut dyn Read,
    ) -> Result<Self, Box<dyn std::error::Error>>;
}

// --- BaboonBinEncode/BaboonBinDecode impls for scalar types ---

macro_rules! impl_bin_codec_copy {
    ($t:ty, $write_fn:ident, $read_fn:ident) => {
        impl BaboonBinEncode for $t {
            fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
                bin_tools::$write_fn(writer, *self)
            }
        }
        impl BaboonBinDecode for $t {
            fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
                bin_tools::$read_fn(reader)
            }
        }
    };
}

impl_bin_codec_copy!(bool, write_bool, read_bool);
impl_bin_codec_copy!(i8, write_i8, read_i8);
impl_bin_codec_copy!(i16, write_i16, read_i16);
impl_bin_codec_copy!(i32, write_i32, read_i32);
impl_bin_codec_copy!(i64, write_i64, read_i64);
impl_bin_codec_copy!(u8, write_u8, read_u8);
impl_bin_codec_copy!(u16, write_u16, read_u16);
impl_bin_codec_copy!(u32, write_u32, read_u32);
impl_bin_codec_copy!(u64, write_u64, read_u64);
impl_bin_codec_copy!(f32, write_f32, read_f32);
impl_bin_codec_copy!(f64, write_f64, read_f64);

impl BaboonBinEncode for String {
    fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_string(writer, self)
    }
}
impl BaboonBinDecode for String {
    fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
        bin_tools::read_string(reader)
    }
}

impl BaboonBinEncode for Vec<u8> {
    fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_bytes(writer, self)
    }
}
impl BaboonBinDecode for Vec<u8> {
    fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
        bin_tools::read_bytes(reader)
    }
}

impl BaboonBinEncode for rust_decimal::Decimal {
    fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_decimal(writer, self)
    }
}
impl BaboonBinDecode for rust_decimal::Decimal {
    fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
        bin_tools::read_decimal(reader)
    }
}

impl BaboonBinEncode for uuid::Uuid {
    fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_uuid(writer, self)
    }
}
impl BaboonBinDecode for uuid::Uuid {
    fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
        bin_tools::read_uuid(reader)
    }
}

impl BaboonBinEncode for chrono::DateTime<chrono::Utc> {
    fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_timestamp(writer, self)
    }
}
impl BaboonBinDecode for chrono::DateTime<chrono::Utc> {
    fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
        bin_tools::read_timestamp_utc(reader)
    }
}

impl BaboonBinEncode for chrono::DateTime<chrono::FixedOffset> {
    fn encode_ueba(&self, _ctx: &BaboonCodecContext, writer: &mut dyn Write) -> std::io::Result<()> {
        bin_tools::write_timestamp(writer, self)
    }
}
impl BaboonBinDecode for chrono::DateTime<chrono::FixedOffset> {
    fn decode_ueba(_ctx: &BaboonCodecContext, reader: &mut dyn Read) -> Result<Self, Box<dyn std::error::Error>> {
        bin_tools::read_timestamp_offset(reader)
    }
}

// --- Metadata traits ---

pub trait BaboonGenerated {
    fn baboon_domain_version() -> &'static str;
    fn baboon_domain_identifier() -> &'static str;
    fn baboon_type_identifier() -> &'static str;
}

pub trait BaboonGeneratedLatest: BaboonGenerated {}
pub trait BaboonAdtMemberMeta: BaboonGenerated {}

pub trait BaboonMeta {
    fn same_in_versions(type_id: &str) -> &[&str];
}

// --- Conversion traits ---

pub trait AbstractConversion<From, To> {
    fn convert(&self, from: &From) -> To;
}

pub trait AbstractBaboonConversions {
    fn versions_from(&self) -> &[&str];
    fn version_to(&self) -> &str;
}

// --- Serde helpers for bytes (hex encoding) ---

pub mod hex_bytes {
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
        let hex_string: String = bytes.iter().map(|b| format!("{:02X}", b)).collect();
        serializer.serialize_str(&hex_string)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<u8>, D::Error> {
        let s = String::deserialize(deserializer)?;
        hex_decode(&s).map_err(serde::de::Error::custom)
    }

    pub fn hex_decode(s: &str) -> Result<Vec<u8>, String> {
        if s.len() % 2 != 0 {
            return Err(format!("Hex string has odd length: {}", s.len()));
        }
        (0..s.len())
            .step_by(2)
            .map(|i| {
                u8::from_str_radix(&s[i..i + 2], 16)
                    .map_err(|e| format!("Invalid hex at position {}: {}", i, e))
            })
            .collect()
    }
}

pub mod opt_hex_bytes {
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(
        value: &Option<Vec<u8>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        match value {
            Some(bytes) => super::hex_bytes::serialize(bytes, serializer),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<Vec<u8>>, D::Error> {
        use serde::Deserialize;
        let opt: Option<String> = Option::deserialize(deserializer)?;
        match opt {
            Some(s) => {
                let bytes = super::hex_bytes::hex_decode(&s).map_err(serde::de::Error::custom)?;
                Ok(Some(bytes))
            }
            None => Ok(None),
        }
    }
}

// --- Serde helpers for Decimal (as JSON number) ---

pub mod decimal_as_number {
    use rust_decimal::Decimal;
    use serde::{Deserialize, Deserializer, Serializer};
    use std::str::FromStr;

    pub fn serialize<S: Serializer>(value: &Decimal, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::Serialize;
        let s = value.normalize().to_string();
        let n: serde_json::Number =
            serde_json::Number::from_str(&s).unwrap_or_else(|_| serde_json::Number::from(0));
        n.serialize(serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Decimal, D::Error> {
        let value = serde_json::Value::deserialize(deserializer)?;
        match &value {
            serde_json::Value::Number(n) => {
                Decimal::from_str(&n.to_string()).map(|d| d.normalize()).map_err(serde::de::Error::custom)
            }
            serde_json::Value::String(s) => {
                Decimal::from_str(s).map(|d| d.normalize()).map_err(serde::de::Error::custom)
            }
            _ => Err(serde::de::Error::custom(format!(
                "Expected number or string for Decimal, got: {}",
                value
            ))),
        }
    }
}

pub mod opt_decimal_as_number {
    use rust_decimal::Decimal;
    use serde::{Deserializer, Serializer};

    pub fn serialize<S: Serializer>(
        value: &Option<Decimal>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        match value {
            Some(d) => super::decimal_as_number::serialize(d, serializer),
            None => serializer.serialize_none(),
        }
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Option<Decimal>, D::Error> {
        use serde::Deserialize;
        use std::str::FromStr;
        let opt: Option<serde_json::Value> = Option::deserialize(deserializer)?;
        match opt {
            Some(serde_json::Value::Null) => Ok(None),
            Some(serde_json::Value::Number(n)) => {
                let d = rust_decimal::Decimal::from_str(&n.to_string())
                    .map(|d| d.normalize())
                    .map_err(serde::de::Error::custom)?;
                Ok(Some(d))
            }
            Some(serde_json::Value::String(s)) => {
                let d = rust_decimal::Decimal::from_str(&s)
                    .map(|d| d.normalize())
                    .map_err(serde::de::Error::custom)?;
                Ok(Some(d))
            }
            Some(other) => Err(serde::de::Error::custom(format!(
                "Expected number, string, or null for Option<Decimal>, got: {}",
                other
            ))),
            None => Ok(None),
        }
    }
}

// --- Serde helpers for timestamps ---

pub mod tsu_serde {
    use chrono::{DateTime, Utc};
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(
        value: &DateTime<Utc>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let s = super::time_formats::format_tsu(value);
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<DateTime<Utc>, D::Error> {
        let s = String::deserialize(deserializer)?;
        super::time_formats::parse_tsu(&s).map_err(serde::de::Error::custom)
    }
}

pub mod tso_serde {
    use chrono::{DateTime, FixedOffset};
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(
        value: &DateTime<FixedOffset>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let s = super::time_formats::format_tso(value);
        serializer.serialize_str(&s)
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<DateTime<FixedOffset>, D::Error> {
        let s = String::deserialize(deserializer)?;
        super::time_formats::parse_tso(&s).map_err(serde::de::Error::custom)
    }
}

// --- Time format helpers ---

pub mod time_formats {
    use chrono::{DateTime, FixedOffset, NaiveDateTime, TimeZone, Utc};

    pub fn format_tsu(dt: &DateTime<Utc>) -> String {
        // Match the Scala/C# format: yyyy-MM-dd'T'HH:mm:ss.SSS'Z'
        dt.format("%Y-%m-%dT%H:%M:%S%.3fZ").to_string()
    }

    pub fn format_tso(dt: &DateTime<FixedOffset>) -> String {
        // Match the Scala/C# format: yyyy-MM-dd'T'HH:mm:ss.SSSxxx
        dt.format("%Y-%m-%dT%H:%M:%S%.3f%:z").to_string()
    }

    pub fn parse_tsu(s: &str) -> Result<DateTime<Utc>, String> {
        // Try multiple formats
        if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
            return Ok(dt.with_timezone(&Utc));
        }
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S%.fZ") {
            return Ok(Utc.from_utc_datetime(&dt));
        }
        if let Ok(dt) = NaiveDateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%SZ") {
            return Ok(Utc.from_utc_datetime(&dt));
        }
        Err(format!("Failed to parse TSU timestamp: {}", s))
    }

    pub fn parse_tso(s: &str) -> Result<DateTime<FixedOffset>, String> {
        if let Ok(dt) = DateTime::parse_from_rfc3339(s) {
            return Ok(dt);
        }
        if let Ok(dt) = DateTime::parse_from_str(s, "%Y-%m-%dT%H:%M:%S%.f%:z") {
            return Ok(dt);
        }
        Err(format!("Failed to parse TSO timestamp: {}", s))
    }
}

// --- Binary encoding/decoding tools ---

pub mod bin_tools {
    use chrono::{DateTime, FixedOffset, NaiveDateTime, Offset, TimeZone, Utc};
    use rust_decimal::Decimal;
    use std::io::{Read, Write};
    use uuid::Uuid;

    // --- Writers ---

    pub fn write_bool(writer: &mut dyn Write, value: bool) -> std::io::Result<()> {
        writer.write_all(&[if value { 1 } else { 0 }])
    }

    pub fn write_byte(writer: &mut dyn Write, value: u8) -> std::io::Result<()> {
        writer.write_all(&[value])
    }

    pub fn write_i8(writer: &mut dyn Write, value: i8) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_i16(writer: &mut dyn Write, value: i16) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_i32(writer: &mut dyn Write, value: i32) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_i64(writer: &mut dyn Write, value: i64) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_u8(writer: &mut dyn Write, value: u8) -> std::io::Result<()> {
        writer.write_all(&[value])
    }

    pub fn write_u16(writer: &mut dyn Write, value: u16) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_u32(writer: &mut dyn Write, value: u32) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_u64(writer: &mut dyn Write, value: u64) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_f32(writer: &mut dyn Write, value: f32) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_f64(writer: &mut dyn Write, value: f64) -> std::io::Result<()> {
        writer.write_all(&value.to_le_bytes())
    }

    pub fn write_decimal(writer: &mut dyn Write, value: &Decimal) -> std::io::Result<()> {
        // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32)
        // flags: sign in bit 31, scale in bits 16-23
        let normalized = value.normalize();
        let scale = normalized.scale();
        let is_negative = normalized.is_sign_negative();
        let mantissa = normalized.mantissa().unsigned_abs();

        let lo = (mantissa & 0xFFFFFFFF) as i32;
        let mid = ((mantissa >> 32) & 0xFFFFFFFF) as i32;
        let hi = ((mantissa >> 64) & 0xFFFFFFFF) as i32;

        let sign: u32 = if is_negative { 0x80000000 } else { 0 };
        let flags = (sign | ((scale as u32) << 16)) as i32;

        write_i32(writer, lo)?;
        write_i32(writer, mid)?;
        write_i32(writer, hi)?;
        write_i32(writer, flags)
    }

    pub fn write_string(writer: &mut dyn Write, value: &str) -> std::io::Result<()> {
        let bytes = value.as_bytes();
        // VLQ (7-bit variable-length) encoding for length, matching C#/Scala format
        let mut len = bytes.len() as u32;
        loop {
            let mut current_byte = (len & 0x7F) as u8;
            len >>= 7;
            if len != 0 {
                current_byte |= 0x80;
            }
            writer.write_all(&[current_byte])?;
            if len == 0 {
                break;
            }
        }
        writer.write_all(bytes)
    }

    pub fn write_bytes(writer: &mut dyn Write, value: &[u8]) -> std::io::Result<()> {
        write_i32(writer, value.len() as i32)?;
        writer.write_all(value)
    }

    pub fn write_uuid(writer: &mut dyn Write, value: &Uuid) -> std::io::Result<()> {
        // .NET GUID mixed-endian format: reverse bytes 0-3, 4-5, 6-7
        let bytes = value.as_bytes();
        let mut guid_bytes = *bytes;
        guid_bytes.swap(0, 3);
        guid_bytes.swap(1, 2);
        guid_bytes.swap(4, 5);
        guid_bytes.swap(6, 7);
        writer.write_all(&guid_bytes)
    }

    const DOTNET_EPOCH_OFFSET_MS: i64 = 62135596800000;

    pub fn write_timestamp<Tz: chrono::TimeZone>(
        writer: &mut dyn Write,
        value: &DateTime<Tz>,
    ) -> std::io::Result<()> {
        // .NET DateTimeOffset format: localTicksMs (i64) + offsetMs (i64) + kind (u8)
        let epoch_ms = value.timestamp_millis();
        let dotnet_utc_ticks_ms = epoch_ms + DOTNET_EPOCH_OFFSET_MS;
        let offset_seconds = value.offset().fix().local_minus_utc();
        let offset_ms = offset_seconds as i64 * 1000;
        let dotnet_local_ticks_ms = dotnet_utc_ticks_ms + offset_ms;
        let kind: u8 = if offset_seconds == 0 { 1 } else { 0 };

        write_i64(writer, dotnet_local_ticks_ms)?;
        write_i64(writer, offset_ms)?;
        write_byte(writer, kind)
    }

    // --- Readers ---

    pub fn read_bool(reader: &mut dyn Read) -> Result<bool, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 1];
        reader.read_exact(&mut buf)?;
        Ok(buf[0] != 0)
    }

    pub fn read_byte(reader: &mut dyn Read) -> Result<u8, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 1];
        reader.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    pub fn read_i8(reader: &mut dyn Read) -> Result<i8, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 1];
        reader.read_exact(&mut buf)?;
        Ok(i8::from_le_bytes(buf))
    }

    pub fn read_i16(reader: &mut dyn Read) -> Result<i16, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf)?;
        Ok(i16::from_le_bytes(buf))
    }

    pub fn read_i32(reader: &mut dyn Read) -> Result<i32, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf)?;
        Ok(i32::from_le_bytes(buf))
    }

    pub fn read_i64(reader: &mut dyn Read) -> Result<i64, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 8];
        reader.read_exact(&mut buf)?;
        Ok(i64::from_le_bytes(buf))
    }

    pub fn read_u8(reader: &mut dyn Read) -> Result<u8, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 1];
        reader.read_exact(&mut buf)?;
        Ok(buf[0])
    }

    pub fn read_u16(reader: &mut dyn Read) -> Result<u16, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 2];
        reader.read_exact(&mut buf)?;
        Ok(u16::from_le_bytes(buf))
    }

    pub fn read_u32(reader: &mut dyn Read) -> Result<u32, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf)?;
        Ok(u32::from_le_bytes(buf))
    }

    pub fn read_u64(reader: &mut dyn Read) -> Result<u64, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 8];
        reader.read_exact(&mut buf)?;
        Ok(u64::from_le_bytes(buf))
    }

    pub fn read_f32(reader: &mut dyn Read) -> Result<f32, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 4];
        reader.read_exact(&mut buf)?;
        Ok(f32::from_le_bytes(buf))
    }

    pub fn read_f64(reader: &mut dyn Read) -> Result<f64, Box<dyn std::error::Error>> {
        let mut buf = [0u8; 8];
        reader.read_exact(&mut buf)?;
        Ok(f64::from_le_bytes(buf))
    }

    pub fn read_decimal(reader: &mut dyn Read) -> Result<Decimal, Box<dyn std::error::Error>> {
        // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32)
        let lo = read_i32(reader)? as u32;
        let mid = read_i32(reader)? as u32;
        let hi = read_i32(reader)? as u32;
        let flags = read_i32(reader)? as u32;

        let scale = ((flags >> 16) & 0xFF) as u32;
        let is_negative = (flags & 0x80000000) != 0;

        let mantissa = (lo as u128) | ((mid as u128) << 32) | ((hi as u128) << 64);
        let mantissa_i128 = if is_negative {
            -(mantissa as i128)
        } else {
            mantissa as i128
        };

        Ok(Decimal::from_i128_with_scale(mantissa_i128, scale))
    }

    pub fn read_string(reader: &mut dyn Read) -> Result<String, Box<dyn std::error::Error>> {
        // VLQ (7-bit variable-length) decoding for length, matching C#/Scala format
        let mut length: u32 = 0;
        let mut shift: u32 = 0;
        loop {
            let mut buf = [0u8; 1];
            reader.read_exact(&mut buf)?;
            let byte_read = buf[0];
            length |= ((byte_read & 0x7F) as u32) << shift;
            shift += 7;
            if (byte_read & 0x80) == 0 {
                break;
            }
        }
        let mut buf = vec![0u8; length as usize];
        reader.read_exact(&mut buf)?;
        Ok(String::from_utf8(buf)?)
    }

    pub fn read_bytes(reader: &mut dyn Read) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
        let len = read_i32(reader)? as usize;
        let mut buf = vec![0u8; len];
        reader.read_exact(&mut buf)?;
        Ok(buf)
    }

    pub fn read_uuid(reader: &mut dyn Read) -> Result<Uuid, Box<dyn std::error::Error>> {
        // .NET GUID mixed-endian format: reverse bytes 0-3, 4-5, 6-7
        let mut buf = [0u8; 16];
        reader.read_exact(&mut buf)?;
        buf.swap(0, 3);
        buf.swap(1, 2);
        buf.swap(4, 5);
        buf.swap(6, 7);
        Ok(Uuid::from_bytes(buf))
    }

    pub fn read_timestamp_utc(
        reader: &mut dyn Read,
    ) -> Result<DateTime<Utc>, Box<dyn std::error::Error>> {
        // .NET DateTimeOffset format: localTicksMs (i64) + offsetMs (i64) + kind (u8)
        let dotnet_local_ticks_ms = read_i64(reader)?;
        let offset_ms = read_i64(reader)?;
        let _kind = read_byte(reader)?;

        let dotnet_utc_ticks_ms = dotnet_local_ticks_ms - offset_ms;
        let epoch_ms = dotnet_utc_ticks_ms - DOTNET_EPOCH_OFFSET_MS;

        let secs = epoch_ms / 1000;
        let nanos = ((epoch_ms % 1000) * 1_000_000) as u32;
        let dt = NaiveDateTime::from_timestamp_opt(secs, nanos)
            .ok_or("Invalid timestamp")?;
        Ok(Utc.from_utc_datetime(&dt))
    }

    pub fn read_timestamp_offset(
        reader: &mut dyn Read,
    ) -> Result<DateTime<FixedOffset>, Box<dyn std::error::Error>> {
        // .NET DateTimeOffset format: localTicksMs (i64) + offsetMs (i64) + kind (u8)
        let dotnet_local_ticks_ms = read_i64(reader)?;
        let offset_ms = read_i64(reader)?;
        let _kind = read_byte(reader)?;

        let dotnet_utc_ticks_ms = dotnet_local_ticks_ms - offset_ms;
        let epoch_ms = dotnet_utc_ticks_ms - DOTNET_EPOCH_OFFSET_MS;
        let offset_seconds = (offset_ms / 1000) as i32;

        let offset = FixedOffset::east_opt(offset_seconds)
            .ok_or("Invalid offset")?;
        let secs = epoch_ms / 1000;
        let nanos = ((epoch_ms % 1000) * 1_000_000) as u32;
        let dt = NaiveDateTime::from_timestamp_opt(secs, nanos)
            .ok_or("Invalid timestamp")?;
        Ok(offset.from_utc_datetime(&dt))
    }
}
