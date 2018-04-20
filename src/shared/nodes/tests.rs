/*!
Base test implementations and traits
*/

use ordered_float::NotNaN;
use float_cmp::ApproxEqUlps;

/// Updates a test's configuration to apply a not
pub trait ApplyNot {
    fn apply_not(&mut self);
}

// Don't try to make this Truth<T> again. This ends up making the Repl -> Node function massive
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum Truth {
    Not,
    Is
}

impl Truth {
    pub fn is_not(&self) -> bool {
        use self::Truth::*;
        match self {
            &Not => true,
            &Is => false
        }
    }
}

impl ApplyNot for Truth {
    fn apply_not(&mut self) {
        use self::Truth::*;
        *self = match *self {
            Not => Is,
            Is => Not
        };
    }
}

/// Compare a value against a single parameter
pub trait STest<T: ?Sized>{
    fn test(&self, val: &T, to: &T) -> bool;
}

impl<'a, F, T: ? Sized> STest<T> for (Truth, &'a F)
    where F: STest<T> {
    fn test(&self, val: &T, to: &T) -> bool {
        self.0.is_not() ^ self.1.test(val, to)
    }
}

/// Compare a value against two parameters
pub trait DTest<T: ?Sized>{
    fn test(&self, val: &T, from: &T, to: &T) -> bool;
}

impl<'a, F, T: ? Sized> DTest<T> for (Truth, &'a F)
    where F: DTest<T> {
    fn test(&self, val: &T, from: &T, to: &T) -> bool {
        self.0.is_not() ^ self.1.test(val, from, to)
    }
}


/// Single value ordinal test
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
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

/// Multiple value ordinal test
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum BetweenTest {
    /// val > from && val < to
    GtLt,
    /// val >= from && val < to
    GeLt,
    /// val > from && val <= to
    GtLe,
    /// val >= from && val <= to
    GeLe,
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
        }
    }
}

/// Single value equivalence test
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
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

impl From<EqTest> for ApproxEqTest  {
    fn from(eq: EqTest) -> ApproxEqTest {
        match eq {
            EqTest::Eq => ApproxEqTest::Eq,
            EqTest::Ne => ApproxEqTest::Ne
        }
    }
}

/// Single value approximate equivalence test for floats (default to 2 ULPs)
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
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

/// &str tests
#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
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
        }
    }
}



#[cfg(test)]
mod tests {

    use ordered_float::NotNaN;
    use shared::nodes::tests::*;

    #[test]
    fn eq_tests() {
        assert_eq!(true, EqTest::Eq.test(&5, &5));
        assert_eq!(false, EqTest::Eq.test(&4, &5));

        assert_eq!(false, EqTest::Ne.test(&5, &5));
        assert_eq!(true, EqTest::Ne.test(&4, &5));
    }

    #[test]
    fn truth_tests() {
        assert_eq!(true, (Truth::Is, &EqTest::Eq).test(&5, &5));
        assert_eq!(false, (Truth::Not, &EqTest::Eq).test(&5, &5));
        assert_eq!(true, (Truth::Not, &EqTest::Eq).test(&4, &5));
        assert_eq!(false, (Truth::Is, &EqTest::Eq).test(&4, &5));
    }

    #[test]
    fn ord_tests() {
        assert_eq!(false, OrdTest::Lt.test(&5, &5));
        assert_eq!(true, OrdTest::Lt.test(&4, &5));

        assert_eq!(false, OrdTest::Le.test(&6, &5));
        assert_eq!(true, OrdTest::Le.test(&5, &5));
        assert_eq!(true, OrdTest::Le.test(&4, &5));

        assert_eq!(false, OrdTest::Gt.test(&5, &5));
        assert_eq!(true, OrdTest::Gt.test(&5, &4));

        assert_eq!(false, OrdTest::Ge.test(&5, &6));
        assert_eq!(true, OrdTest::Ge.test(&5, &5));
        assert_eq!(true, OrdTest::Ge.test(&5, &4));

    }

    #[test]
    fn approx_eq_tests() {
        let five = NotNaN::new(5.0f32).unwrap();
        let four = NotNaN::new(4.0f32).unwrap();

        assert_eq!(true, ApproxEqTest::Eq.test(&five, &five));
        assert_eq!(false, ApproxEqTest::Eq.test(&four, &five));

        assert_eq!(false, ApproxEqTest::Ne.test(&five, &five));
        assert_eq!(true, ApproxEqTest::Ne.test(&four, &five));
    }

    #[test]
    fn str_array_tests() {
        assert_eq!(true, StrArrayTest::Contains.test("abcd", "bc"));
        assert_eq!(true, StrArrayTest::StartsWith.test("abcd", "ab"));
        assert_eq!(true, StrArrayTest::EndsWith.test("abcd", "cd"));

        assert_eq!(true, StrArrayTest::ContainedBy.test("bc" , "abcd"));
        assert_eq!(true, StrArrayTest::StartedBy.test( "ab", "abcd"));
        assert_eq!(true, StrArrayTest::EndedBy.test("cd", "abcd"));
    }

}