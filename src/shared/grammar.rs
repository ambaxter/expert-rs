use ::shared::tests::{
    EqTest, OrdTest, BetweenTest, TestRepr, SLimit, SDynLimit, DLimit, DDynLimit,
    BoolTest, NumberTest, StrTest, TimeTest, DateTest, DateTimeTest, SDynTests, DDynTests
};
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{Utc, NaiveTime, Date, DateTime};

pub trait AString: Clone + Into<String> + AsRef<str> {}

impl<'a> AString for &'a str {}
impl AString for String {}

pub trait IntoEqTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S>;
}

pub trait IntoOrdTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S>;
}

pub trait IntoStrTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_str_test(self, field: S, test: OrdTest) -> TestRepr<S>;
}

pub trait IntoBtwnTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S>;
}

// Single values

// Eq testing
macro_rules! into_eq_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: Clone + Into<String> + AsRef<str>> IntoEqTest<S> for $id {
                fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::EQ(test, SLimit::St(self)))
                }
            }
        )*
    };
}

into_eq_tests!(
    bool => [BOOL, BoolTest],
    OrdVar<d128> => [NUMBER, NumberTest],
    NaiveTime => [TIME, TimeTest],
    Date<Utc> => [DATE, DateTest],
    DateTime<Utc> => [DATETIME, DateTimeTest]
    );

impl<S: AString> IntoEqTest<S> for S {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::EQ(test, SLimit::St(self)))
    }
}

impl<S: Clone + Into<String> + AsRef<str>> IntoEqTest<S> for d128 {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
        TestRepr::NUMBER(field, NumberTest::EQ(test, SLimit::St(self.into())))
    }
}

impl<S: Clone + Into<String> + AsRef<str>> IntoEqTest<S> for SDynLimit<S> {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S> {
        TestRepr::SDYN(field, SDynTests::EQ(test), self)
    }
}

// Ord testing

macro_rules! into_ord_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: Clone + Into<String> + AsRef<str>> IntoOrdTest<S> for $id {
                fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::ORD(test, SLimit::St(self)))
                }
            }
        )*
    };
}

into_ord_tests!(
    OrdVar<d128> => [NUMBER, NumberTest],
    NaiveTime => [TIME, TimeTest],
    Date<Utc> => [DATE, DateTest],
    DateTime<Utc> => [DATETIME, DateTimeTest]
    );

impl<S: AString> IntoOrdTest<S> for S {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::ORD(test, SLimit::St(self)))
    }
}

impl<S: Clone + Into<String> + AsRef<str>> IntoOrdTest<S> for d128 {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
        TestRepr::NUMBER(field, NumberTest::ORD(test, SLimit::St(self.into())))
    }
}

impl<S: Clone + Into<String> + AsRef<str>> IntoOrdTest<S> for SDynLimit<S> {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S> {
        TestRepr::SDYN(field, SDynTests::ORD(test), self)
    }
}

// Double values

// Between testing

macro_rules! into_btwn_tests {
    ($($id:ty => [$sub:ident, $test:ident]),+) => {
        $(
            impl<S: Clone + Into<String> + AsRef<str>> IntoBtwnTest<S> for ($id, $id) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::BTWN(test, DLimit::St(self.0, self.1)))
                }
            }

            impl<S: Clone + Into<String> + AsRef<str>> IntoBtwnTest<S> for (SDynLimit<S>, $id) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::BTWN(test, DLimit::LocalSt(self.0.limit, self.1)))
                }
            }

            impl<S: Clone + Into<String> + AsRef<str>> IntoBtwnTest<S> for ($id, SDynLimit<S>) {
                fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
                    TestRepr::$sub(field, $test::BTWN(test, DLimit::StLocal(self.0, self.1.limit)))
                }
            }
        )*
    };
}

into_btwn_tests!(
    OrdVar<d128> => [NUMBER, NumberTest],
    NaiveTime => [TIME, TimeTest],
    Date<Utc> => [DATE, DateTest],
    DateTime<Utc> => [DATETIME, DateTimeTest]
    );

impl<S: AString> IntoBtwnTest<S> for (S, S) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::BTWN(test, DLimit::St(self.0, self.1)))
    }
}

impl<S: AString> IntoBtwnTest<S> for (SDynLimit<S>, S) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::BTWN(test, DLimit::LocalSt(self.0.limit, self.1)))
    }
}

impl<S: AString> IntoBtwnTest<S> for (S, SDynLimit<S>) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::STR(field, StrTest::BTWN(test, DLimit::StLocal(self.0, self.1.limit)))
    }
}

impl<S: AString> IntoBtwnTest<S> for (d128, d128) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::NUMBER(field, NumberTest::BTWN(test, DLimit::St(self.0.into(), self.1.into())))
    }
}

impl<S: AString> IntoBtwnTest<S> for (SDynLimit<S>, d128) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::NUMBER(field, NumberTest::BTWN(test, DLimit::LocalSt(self.0.limit, self.1.into())))
    }
}

impl<S: AString> IntoBtwnTest<S> for (d128, SDynLimit<S>) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        TestRepr::NUMBER(field, NumberTest::BTWN(test, DLimit::StLocal(self.0.into(), self.1.limit)))
    }
}

impl<S: Clone + Into<String> + AsRef<str>> IntoBtwnTest<S> for (SDynLimit<S>, SDynLimit<S>) {
    fn into_btwn_test(self, field: S, test: BetweenTest) -> TestRepr<S> {
        let limit = DDynLimit{l: self.0.limit, r: self.1.limit};
        TestRepr::DDYN(field, DDynTests::BTWN(test), limit)
    }
}