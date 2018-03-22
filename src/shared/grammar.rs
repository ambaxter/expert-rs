use ::shared::tests::{
    EqTest, OrdTest, BetweenTest, TestRepr, SLimit, SDynLimit, DLimit, DDynLimit,
    BoolTest, NumberTest, StrTest, TimeTest, DateTest, DateTimeTest, SDynTests, DDynTests
};
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{Utc, NaiveTime, Date, DateTime};


pub trait IntoEqTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_eq_test(self, field: S, test: EqTest) -> TestRepr<S>;
}

pub trait IntoOrdTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S>;

}

pub trait IntoStrTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_ord_test(self, field: S, test: OrdTest) -> TestRepr<S>;
}

pub trait IntoBtwnTest<S: Clone + Into<String> + AsRef<str>> {
    fn into_btwn_test(self, field: S, test: OrdTest) -> TestRepr<S>;
}

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

pub trait AString: Clone + Into<String> + AsRef<str> {}

impl<'a> AString for &'a str {}
impl AString for String {}

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