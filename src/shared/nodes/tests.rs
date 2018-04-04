/*!
Base test implementations and traits
*/

use ordered_float::NotNaN;
use float_cmp::{Ulps, ApproxEqUlps};

/// Updates a test's configuration to apply a not
pub trait ApplyNot {
    fn apply_not(&mut self);
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum Truth {
    Not,
    Is
}

impl Truth {
    pub fn as_bool(&self) -> bool {
        use self::Truth::*;
        match self {
            &Not => false,
            &Is => true
        }
    }
}

/// Compare a value against a single parameter
pub trait STest<T: ?Sized>{
    fn test(&self, val: &T, to: &T) -> bool;
}

/// Compare a value against two parameters
pub trait DTest<T: ?Sized>{
    fn test(&self, val: &T, from: &T, to: &T) -> bool;
}

/// Single value ordinal test
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum OrdTest {
    /// val < to
    Lt,
    /// val <= to
    Le,
    /// val > to
    Gt,
    ///
    /// val >= to
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

impl ApplyNot for OrdTest {
    fn apply_not(&mut self) {
        use self::OrdTest::*;
        let new = match *self {
            Lt => Ge,
            Le => Gt,
            Gt => Le,
            Ge => Lt,
        };
        *self = new;
    }
}

/// Multiple value ordinal test
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BetweenTest {
    /// val > from && val < to
    GtLt,
    /// val >= from && val < to
    GeLt,
    /// val > from && val <= to
    GtLe,
    /// val >= from && val <= to
    GeLe,
    // Not
    /// !(val > from && val < to)
    NotGtLt,
    /// !(val >= from && val < to)
    NotGeLt,
    /// !(val > from && val <= to)
    NotGtLe,
    /// !(val >= from && val <= to)
    NotGeLe,
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
            &NotGtLt => !(val > from && val < to),
            &NotGeLt => !(val >= from && val < to),
            &NotGtLe => !(val > from && val <= to),
            &NotGeLe => !(val >= from && val <= to),
        }
    }
}

impl ApplyNot for BetweenTest {
    fn apply_not(&mut self) {
        use self::BetweenTest::*;
        let new = match *self {
            GtLt => NotGtLt,
            GeLt => NotGeLt,
            GtLe => NotGtLe,
            GeLe => NotGeLe,
            NotGtLt => GtLt,
            NotGeLt => GeLt,
            NotGtLe => GtLe,
            NotGeLe => GeLe,
        };
        *self = new;
    }
}

/// Single value equivalence test
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum EqTest {
    /// val == to
    Eq,
    /// val != to
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

impl ApplyNot for EqTest {
    fn apply_not(&mut self) {
        use self::EqTest::*;
        let new = match *self {
            Eq => Ne,
            Ne => Eq,
        };
        *self = new;
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

/// Single value approximate equivalence test for floats (default to 2 ULPs)
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum ApproxEqTest {
    /// val ~= to
    Eq,
    /// val !~= to
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

impl ApplyNot for ApproxEqTest {
    fn apply_not(&mut self) {
        use self::ApproxEqTest::*;
        let new = match *self {
            Eq => Ne,
            Ne => Eq,
        };
        *self = new;
    }
}

/// &str tests
#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrArrayTest {
    /// val.contains(to)
    Contains,
    /// val.starts_with(to)
    StartsWith,
    /// val.ends_with(to)
    EndsWith,
    /// to.contains(val)
    ContainedBy,
    /// to.starts_with(val)
    StartedBy,
    /// to.ends_with(val)
    EndedBy,
    //Not Versions
    /// !val.contains(to)
    NotContains,
    /// !val.starts_with(to)
    NotStartsWith,
    /// !val.ends_with(to)
    NotEndsWith,
    /// !to.contains(val)
    NotContainedBy,
    /// !to.starts_with(val)
    NotStartedBy,
    /// !to.ends_with(val)
    NotEndedBy,
}

impl<T> STest<T> for StrArrayTest
    where T: AsRef<str> + ?Sized {
    fn test(&self, val: &T, to: &T) -> bool {
        use self::StrArrayTest::*;
        match self {
            &Contains => val.as_ref().contains(to.as_ref()),
            &StartsWith => val.as_ref().starts_with(to.as_ref()),
            &EndsWith => val.as_ref().ends_with(to.as_ref()),
            &ContainedBy => to.as_ref().contains(val.as_ref()),
            &StartedBy => to.as_ref().starts_with(val.as_ref()),
            &EndedBy => to.as_ref().ends_with(val.as_ref()),
            &NotContains => !val.as_ref().contains(to.as_ref()),
            &NotStartsWith => !val.as_ref().starts_with(to.as_ref()),
            &NotEndsWith => !val.as_ref().ends_with(to.as_ref()),
            &NotContainedBy => to.as_ref().contains(val.as_ref()),
            &NotStartedBy => to.as_ref().starts_with(val.as_ref()),
            &NotEndedBy => to.as_ref().ends_with(val.as_ref()),
        }
    }
}

impl ApplyNot for StrArrayTest {
    fn apply_not(&mut self) {
        use self::StrArrayTest::*;
        let new = match *self {
            Contains => NotContains,
            StartsWith => NotStartsWith,
            EndsWith => NotEndsWith,
            ContainedBy => NotContainedBy,
            StartedBy => NotStartedBy,
            EndedBy => NotEndedBy,
            NotContains => Contains,
            NotStartsWith => StartsWith,
            NotEndsWith => EndsWith,
            NotContainedBy => ContainedBy,
            NotStartedBy => StartedBy,
            NotEndedBy => EndedBy
        };
        *self = new;
    }
}