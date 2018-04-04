use ::shared::nodes::beta::{
    TestRepr, SLimit, SDynLimit, DLimit, DDynLimit,
    BoolTest,
    I8Test, I16Test, I32Test, I64Test,
    U8Test, U16Test, U32Test, U64Test,
    F32Test, F64Test, D128Test,
    StrTest,
    TimeTest, DateTest, DateTimeTest,
    SDynTests, DDynTests
};
use std::marker;
use ord_subset::OrdVar;
use ordered_float::NotNaN;
use decimal::d128;
use chrono::{Utc, NaiveTime, Date, DateTime};
use std::borrow::Cow;
use shared::nodes::tests::{Truth, EqTest, OrdTest, BetweenTest, StrArrayTest};

pub fn dyn<S: AsRef<str>>(limit: S) -> SDynLimit<S> {
    SDynLimit{limit}
}

pub trait AString: AsRef<str> {}

impl<'a> AString for &'a str {}
impl AString for String {}
impl<'a> AString for Cow<'a, str> {}

pub trait IntoEqTest<S: AsRef<str>> {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S>;
}

pub trait IntoOrdTest<S: AsRef<str>> {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S>;
}

pub trait IntoBtwnTest<S: AsRef<str>> {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S>;
}

pub trait IntoStrTest<S: AsRef<str>> {
    fn into_str_test(self, field: S, test: StrArrayTest) -> TestRepr<S>;
}

// Single values

// Eq testing
macro_rules! into_eq_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoEqTest<S> for $id {
                fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Eq(Truth::Is, test, SLimit::St(self)))
                }
            }
        )*
    };
}

into_eq_tests!(
    bool => [BOOL, BoolTest],
    i8 => [I8, I8Test],
    i16 => [I16, I16Test],
    i32 => [I32, I32Test],
    i64 => [I64, I64Test],
    u8 => [U8, U8Test],
    u16 => [U16, U16Test],
    u32 => [U32, U32Test],
    u64 => [U64, U64Test],
    OrdVar<d128> => [D128, D128Test],
    NaiveTime => [TIME, TimeTest],
    Date<Utc> => [DATE, DateTest],
    DateTime<Utc> => [DATETIME, DateTimeTest]
    );

macro_rules! float_into_approx_eq_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoEqTest<S> for $id {
                fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::ApproxEq(Truth::Is, test.into(), SLimit::St(self.into())))
                }
            }
        )*
    };
}

float_into_approx_eq_tests!(
    f32 => [F32, F32Test],
    f64 => [F64, F64Test]
);

macro_rules! nn_float_into_approx_eq_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoEqTest<S> for $id {
                fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::ApproxEq(Truth::Is, test.into(), SLimit::St(self)))
                }
            }
        )*
    };
}

nn_float_into_approx_eq_tests!(
    NotNaN<f32> => [F32, F32Test],
    NotNaN<f64> => [F64, F64Test]
);


impl<S: AString> IntoEqTest<S> for S {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::Eq(Truth::Is, test, SLimit::St(self)))
    }
}

impl<S: AsRef<str>> IntoEqTest<S> for d128 {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
        TestRepr::D128(field, D128Test::Eq(Truth::Is, test, SLimit::St(self.into())))
    }
}

impl<S: AsRef<str>> IntoEqTest<S> for SDynLimit<S> {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
        TestRepr::SDYN(field, Truth::Is, SDynTests::Eq(test), self)
    }
}

// Ord testing

macro_rules! into_ord_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoOrdTest<S> for $id {
                fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Ord(Truth::Is,test, SLimit::St(self)))
                }
            }
        )*
    };
}

into_ord_tests!(
    i8 => [I8, I8Test],
    i16 => [I16, I16Test],
    i32 => [I32, I32Test],
    i64 => [I64, I64Test],
    u8 => [U8, U8Test],
    u16 => [U16, U16Test],
    u32 => [U32, U32Test],
    u64 => [U64, U64Test],
    NotNaN<f32> => [F32, F32Test],
    NotNaN<f64> => [F64, F64Test],
    OrdVar<d128> => [D128, D128Test],
    NaiveTime => [TIME, TimeTest],
    Date<Utc> => [DATE, DateTest],
    DateTime<Utc> => [DATETIME, DateTimeTest]
    );

macro_rules! float_into_ord_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoOrdTest<S> for $id {
                fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Ord(Truth::Is, test, SLimit::St(self.into())))
                }
            }
        )*
    };
}

float_into_ord_tests!(
    f32 => [F32, F32Test],
    f64 => [F64, F64Test],
    d128 => [D128, D128Test]
);

impl<S: AString> IntoOrdTest<S> for S {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::Ord(Truth::Is, test, SLimit::St(self)))
    }
}

impl<S: AsRef<str>> IntoOrdTest<S> for SDynLimit<S> {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
        TestRepr::SDYN(field, Truth::Is,SDynTests::Ord(test), self)
    }
}

// Double values

// Between testing

macro_rules! into_btwn_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoBtwnTest<S> for ($id, $id) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Btwn(Truth::Is, test, DLimit::St(self.0, self.1)))
                }
            }

            impl<S: AsRef<str>> IntoBtwnTest<S> for (SDynLimit<S>, $id) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Btwn(Truth::Is, test, DLimit::DynSt(self.0.limit, self.1)))
                }
            }

            impl<S: AsRef<str>> IntoBtwnTest<S> for ($id, SDynLimit<S>) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Btwn(Truth::Is, test, DLimit::StDyn(self.0, self.1.limit)))
                }
            }
        )*
    };
}

into_btwn_tests!(
    i8 => [I8, I8Test],
    i16 => [I16, I16Test],
    i32 => [I32, I32Test],
    i64 => [I64, I64Test],
    u8 => [U8, U8Test],
    u16 => [U16, U16Test],
    u32 => [U32, U32Test],
    u64 => [U64, U64Test],
    NotNaN<f32> => [F32, F32Test],
    NotNaN<f64> => [F64, F64Test],
    OrdVar<d128> => [D128, D128Test],
    NaiveTime => [TIME, TimeTest],
    Date<Utc> => [DATE, DateTest],
    DateTime<Utc> => [DATETIME, DateTimeTest]
    );

macro_rules! float_into_btwn_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: AsRef<str>> IntoBtwnTest<S> for ($id, $id) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Btwn(Truth::Is, test, DLimit::St(self.0.into(), self.1.into())))
                }
            }

            impl<S: AsRef<str>> IntoBtwnTest<S> for (SDynLimit<S>, $id) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Btwn(Truth::Is, test, DLimit::DynSt(self.0.limit, self.1.into())))
                }
            }

            impl<S: AsRef<str>> IntoBtwnTest<S> for ($id, SDynLimit<S>) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::Btwn(Truth::Is, test, DLimit::StDyn(self.0.into(), self.1.limit)))
                }
            }
        )*
    };
}

float_into_btwn_tests!(
    f32 => [F32, F32Test],
    f64 => [F64, F64Test],
    d128 => [D128, D128Test]
);

impl<S: AString> IntoBtwnTest<S> for (S, S) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::Btwn(Truth::Is,test, DLimit::St(self.0, self.1)))
    }
}

impl<S: AString> IntoBtwnTest<S> for (SDynLimit<S>, S) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::Btwn(Truth::Is,test, DLimit::DynSt(self.0.limit, self.1)))
    }
}

impl<S: AString> IntoBtwnTest<S> for (S, SDynLimit<S>) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::Btwn(Truth::Is, test, DLimit::StDyn(self.0, self.1.limit)))
    }
}

impl<S: AsRef<str>> IntoBtwnTest<S> for (SDynLimit<S>, SDynLimit<S>) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        let limit = DDynLimit{l: self.0.limit, r: self.1.limit};
        TestRepr::DDYN(field, Truth::Is, DDynTests::Btwn(test), limit)
    }
}

impl<S: AString> IntoStrTest<S> for S {
    fn into_str_test(self, field: S, test: StrArrayTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::Str(Truth::Is,test, SLimit::St(self.into())))
    }
}

pub trait CompileStage1 {
    type Output;

    fn compile_stage1(&self) -> Self::Output;

    fn compile_slice_stage1(t: &[Self]) -> Vec<Self::Output> where Self: marker::Sized {
        t.iter().map(|c| c.compile_stage1()).collect()
    }
}