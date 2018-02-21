use ::runtime::memory::SymbolId;
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};

pub trait LocalContext {
    fn get_boolean(&self, sym: SymbolId) -> &bool;
    fn get_number(&self, sym: SymbolId) -> &OrdVar<d128>;
    fn got_str(&self, sym: SymbolId) -> &str;
    fn get_time(&self, sym: SymbolId) -> &NaiveTime;
    fn get_date(&self, sym: SymbolId) -> &Date<Utc>;
    fn get_datetime(&self, sym: SymbolId) -> &DateTime<Utc>;
}