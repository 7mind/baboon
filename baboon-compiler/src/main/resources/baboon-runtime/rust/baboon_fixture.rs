use chrono::{DateTime, FixedOffset, NaiveDateTime, TimeZone, Utc};
use rand::Rng;
use rust_decimal::Decimal;
use std::collections::{BTreeMap, BTreeSet};
use uuid::Uuid;

pub struct BaboonRandom {
    rng: rand::rngs::ThreadRng,
}

impl BaboonRandom {
    pub fn new() -> Self {
        BaboonRandom {
            rng: rand::thread_rng(),
        }
    }

    pub fn next_i08(&mut self) -> i8 {
        self.rng.gen()
    }

    pub fn next_i16(&mut self) -> i16 {
        self.rng.gen()
    }

    pub fn next_i32(&mut self) -> i32 {
        self.rng.gen()
    }

    pub fn next_i64(&mut self) -> i64 {
        self.rng.gen()
    }

    pub fn next_u08(&mut self) -> u8 {
        self.rng.gen()
    }

    pub fn next_u16(&mut self) -> u16 {
        self.rng.gen()
    }

    pub fn next_u32(&mut self) -> u32 {
        self.rng.gen()
    }

    pub fn next_u64(&mut self) -> u64 {
        self.rng.gen()
    }

    pub fn next_f32(&mut self) -> f32 {
        self.rng.gen_range(-1000.0f32..1000.0f32)
    }

    pub fn next_f64(&mut self) -> f64 {
        self.rng.gen_range(-1000.0f64..1000.0f64)
    }

    pub fn next_f128(&mut self) -> Decimal {
        // Use integer values to avoid precision issues in round-trips
        let value: i64 = self.rng.gen_range(-999999..999999);
        Decimal::new(value, 2).normalize()
    }

    pub fn next_string(&mut self) -> String {
        let len = self.rng.gen_range(1..20);
        (0..len)
            .map(|_| {
                let idx = self.rng.gen_range(0..26);
                (b'a' + idx) as char
            })
            .collect()
    }

    pub fn next_bytes(&mut self) -> Vec<u8> {
        let len = self.rng.gen_range(1..20);
        (0..len).map(|_| self.rng.gen()).collect()
    }

    pub fn next_uid(&mut self) -> Uuid {
        Uuid::new_v4()
    }

    pub fn next_tsu(&mut self) -> DateTime<Utc> {
        // Generate a random timestamp with millisecond precision (matching other languages)
        let secs = self.rng.gen_range(0i64..2000000000i64);
        let millis = self.rng.gen_range(0u32..1000u32);
        let dt = NaiveDateTime::from_timestamp_opt(secs, millis * 1_000_000)
            .unwrap_or_else(|| NaiveDateTime::from_timestamp_opt(0, 0).unwrap());
        Utc.from_utc_datetime(&dt)
    }

    pub fn next_tso(&mut self) -> DateTime<FixedOffset> {
        let secs = self.rng.gen_range(0i64..2000000000i64);
        let millis = self.rng.gen_range(0u32..1000u32);
        let offset_hours = self.rng.gen_range(-12i32..=12i32);
        let offset = FixedOffset::east_opt(offset_hours * 3600).unwrap();
        let dt = NaiveDateTime::from_timestamp_opt(secs, millis * 1_000_000)
            .unwrap_or_else(|| NaiveDateTime::from_timestamp_opt(0, 0).unwrap());
        offset.from_utc_datetime(&dt)
    }

    pub fn next_bit(&mut self) -> bool {
        self.rng.gen()
    }

    pub fn next_usize(&mut self, max: usize) -> usize {
        self.rng.gen_range(0..max)
    }

    pub fn mk_list<T>(&mut self, mut gen: impl FnMut() -> T) -> Vec<T> {
        let count = self.rng.gen_range(0..5);
        (0..count).map(|_| gen()).collect()
    }

    pub fn mk_set<T: Ord>(&mut self, mut gen: impl FnMut() -> T) -> BTreeSet<T> {
        let count = self.rng.gen_range(0..5);
        (0..count).map(|_| gen()).collect()
    }

    pub fn mk_map<K: Ord, V>(&mut self, mut gen_key: impl FnMut() -> K, mut gen_val: impl FnMut() -> V) -> BTreeMap<K, V> {
        let count = self.rng.gen_range(0..5);
        (0..count).map(|_| (gen_key(), gen_val())).collect()
    }

    pub fn mk_option<T>(&mut self, mut gen: impl FnMut() -> T) -> Option<T> {
        if self.rng.gen_bool(0.5) {
            Some(gen())
        } else {
            None
        }
    }

    pub fn mk_enum<T: Clone>(&mut self, variants: &[T]) -> T {
        let idx = self.rng.gen_range(0..variants.len());
        variants[idx].clone()
    }
}

impl Default for BaboonRandom {
    fn default() -> Self {
        Self::new()
    }
}
