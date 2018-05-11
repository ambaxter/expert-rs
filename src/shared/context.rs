use ::runtime::memory::SymbolId;
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ordered_float::NotNaN;
use runtime::memory::StringCache;

pub trait AlphaContext {
    fn get_string_cache(&self) -> &StringCache;
}

pub trait BetaContext {
    fn get_bool(&self, sym: SymbolId) -> &bool;
    fn get_i8(&self, sym: SymbolId) -> i8;
    fn get_i16(&self, sym: SymbolId) -> i16;
    fn get_i32(&self, sym: SymbolId) -> i32;
    fn get_i64(&self, sym: SymbolId) -> i64;
    fn get_i128(&self, sym: SymbolId) -> i128;
    fn get_u8(&self, sym: SymbolId) -> u8;
    fn get_u16(&self, sym: SymbolId) -> u16;
    fn get_u32(&self, sym: SymbolId) -> u32;
    fn get_u64(&self, sym: SymbolId) -> u64;
    fn get_u128(&self, sym: SymbolId) -> u128;
    fn get_f32(&self, sym: SymbolId) -> NotNaN<f32>;
    fn get_f64(&self, sym: SymbolId) -> NotNaN<f64>;
    fn get_d128(&self, sym: SymbolId) -> OrdVar<d128>;
    fn get_str(&self, sym: SymbolId) -> &str;
    fn get_time(&self, sym: SymbolId) -> &NaiveTime;
    fn get_date(&self, sym: SymbolId) -> &Date<Utc>;
    fn get_datetime(&self, sym: SymbolId) -> &DateTime<Utc>;
    fn get_string_cache(&self) -> &StringCache;
}