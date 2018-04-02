use ordered_float::NotNaN;
use float_cmp::{Ulps, ApproxEqUlps};

pub trait STest<T: ?Sized>{
    fn test(&self, val: &T, to: &T) -> bool;
}

pub trait DTest<T: ?Sized>{
    fn test(&self, val: &T, from: &T, to: &T) -> bool;
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum OrdTest {
    Lt,
    Le,
    Gt,
    Ge,
}

impl<T> STest<T> for OrdTest
    where T: Ord + ?Sized {
    fn test(&self, val: &T, to: &T) -> bool {
        use self::OrdTest::*;
        match self {
            &Lt => val < to,
            &Le => val <= to,
            &Gt => val > to,
            &Ge => val >= to,
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BetweenTest {
    GtLt,
    GeLt,
    GtLe,
    GeLe,
    // Not
    LtGt,
    LeGt,
    LtGe,
    LeGe
}

impl<T> DTest<T> for BetweenTest
    where T: Ord + ?Sized{
    fn test(&self, val: &T, from: &T, to: &T) -> bool {
        use self::BetweenTest::*;
        match self {
            &GtLt => val > from && val < to,
            &GeLt => val >= from && val < to,
            &GtLe => val > from && val <= to,
            &GeLe => val >= from && val <= to,
            &LtGt => val < from || val > to,
            &LeGt => val <= from || val > to,
            &LtGe => val < from || val >= to,
            &LeGe => val <= from || val >= to,
        }
    }
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum EqTest {
    Eq,
    Ne
}

impl<T> STest<T> for EqTest
    where T: Eq + ?Sized {
    fn test(&self, val: &T, to: &T) -> bool {
        use self::EqTest::*;
        match self {
            &Eq => val == to,
            &Ne => val != to,
        }
    }
}


impl Into<ApproxEqTest> for EqTest {
    fn into(self) -> ApproxEqTest {
        match self {
            EqTest::Eq => ApproxEqTest::Eq,
            EqTest::Ne => ApproxEqTest::Ne
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum ApproxEqTest {
    Eq,
    Ne
}

// TODO: I wish I could make this more generic. Revisit once impl specialization lands?
impl STest<NotNaN<f32>> for ApproxEqTest {
    fn test(&self, val: &NotNaN<f32>, to: &NotNaN<f32>) -> bool {
        use self::ApproxEqTest::*;
        match self {
            &Eq => val.as_ref().approx_eq_ulps(to.as_ref(), 2),
            &Ne => val.as_ref().approx_ne_ulps(to.as_ref(), 2),
        }
    }
}

impl STest<NotNaN<f64>> for ApproxEqTest {
    fn test(&self, val: &NotNaN<f64>, to: &NotNaN<f64>) -> bool {
        use self::ApproxEqTest::*;
        match self {
            &Eq => val.as_ref().approx_eq_ulps(to.as_ref(), 2),
            &Ne => val.as_ref().approx_ne_ulps(to.as_ref(), 2),
        }
    }
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrArrayTest {
    Contains,
    StartsWith,
    EndsWith,
    //Not Versions
    NotContains,
    NotStartsWith,
    NotEndsWith,
}

impl<T> STest<T> for StrArrayTest
    where T: AsRef<str> + ?Sized {
    fn test(&self, val: &T, to: &T) -> bool {
        use self::StrArrayTest::*;
        match self {
            &Contains => val.as_ref().contains(to.as_ref()),
            &StartsWith => val.as_ref().starts_with(to.as_ref()),
            &EndsWith => val.as_ref().ends_with(to.as_ref()),
            &NotContains => !val.as_ref().contains(to.as_ref()),
            &NotStartsWith => !val.as_ref().starts_with(to.as_ref()),
            &NotEndsWith => !val.as_ref().ends_with(to.as_ref())
        }
    }
}
