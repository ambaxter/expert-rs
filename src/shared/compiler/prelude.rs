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
use shared::fact::Fact;
use runtime::memory::StringCache;
use shared::nodes::beta::{IsAlpha, BetaNode};
use errors::CompileError;
use enum_index;
use enum_index::EnumIndex;
use std::cmp::Ordering;
use shared::nodes::tests::ApplyNot;
use std::mem;
use runtime::memory::SymbolId;
use shared::fact::Getter;
use shared::nodes::alpha::AlphaNode;
use shared::nodes::beta::CollectRequired;
use std::collections::HashSet;

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

pub trait DrainWhere<T, F>
    where F: FnMut(&T) -> bool {
    fn drain_where(&mut self, f: F) -> Self;
}

impl<T, F> DrainWhere<T, F> for Vec<T>
    where F: FnMut(&T) -> bool {
    fn drain_where(&mut self, mut f: F) -> Self {
        let mut i = 0;
        let mut v = Vec::new();
        while i != self.len() {
            if f(&mut self[i]) {
                v.push(self.remove(i));
            } else {
                i += 1;
            }
        }
        v
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug, EnumIndex)]
pub enum Stage1Node<T: Fact> {
    T(BetaNode<T>),
    Any(Vec<Stage1Node<T>>),
    NotAny(Vec<Stage1Node<T>>),
    All(Vec<Stage1Node<T>>),
    NotAll(Vec<Stage1Node<T>>)
}

// https://stackoverflow.com/questions/36557412/change-enum-variant-while-moving-the-field-to-the-new-variant
impl<T: Fact> ApplyNot for Stage1Node<T> {
    fn apply_not(&mut self) {
        use self::Stage1Node::*;
        let interim = unsafe { mem::zeroed() };
        let prev = mem::replace(self, interim);
        let next = match prev {
            T(mut node) => {
                node.apply_not();
                T(node)
            },
            Any(nodes) => NotAny(nodes),
            NotAny(nodes) => Any(nodes),
            All(nodes) => NotAll(nodes),
            NotAll(nodes) => All(nodes),
        };
        let interim = mem::replace(self, next);
        mem::forget(interim);   // Important! interim was never initialized
    }
}

impl<T: Fact> Ord for Stage1Node<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        use self::Stage1Node::*;
        match (self, other) {
            (T(ref n1), T(ref n2)) => n1.cmp(n2),
            (Any(ref v1), Any(ref v2)) => v1.cmp(v2),
            (NotAny(ref v1), NotAny(ref v2)) => v1.cmp(v2),
            (All(ref v1), All(ref v2)) => v1.cmp(v2),
            (NotAll(ref v1), NotAll(ref v2)) => v1.cmp(v2),
            _ => self.enum_index().cmp(&other.enum_index())
        }
    }
}

impl<T: Fact> PartialOrd for Stage1Node<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Fact> Stage1Node<T> {

    pub fn is_test(&self) -> bool {
        use self::Stage1Node::*;
        match *self {
            T(_) => true,
            _ => false
        }
    }

    pub fn is_alpha(&self) -> bool {
        use self::Stage1Node::*;
        match *self {
            T(ref test) => test.is_alpha(),
            _ => false
        }
    }

    pub fn get_alpha(self) -> AlphaNode<T> {
        use self::Stage1Node::*;
        match self {
            T(test) => test.into(),
            _ => unreachable!("get_alpha on non alpha node")
        }
    }

    pub fn is_any(&self) -> bool {
        use self::Stage1Node::*;
        match *self {
            Any(_) => true,
            _ => false
        }
    }

    pub fn is_all(&self) -> bool {
        use self::Stage1Node::*;
        match *self {
            All(_) => true,
            _ => false
        }
    }

    fn is_singleton(&self) -> bool {
        use self::Stage1Node::*;
        match *self {
            Any(ref nodes) => nodes.len() == 1,
            NotAny(ref nodes) => nodes.len() == 1,
            All(ref nodes) => nodes.len() == 1,
            NotAll(ref nodes) => nodes.len() == 1,
            _ => false
        }
    }

    fn extract_singleton(self) -> Self {
        use self::Stage1Node::*;
        debug_assert!(self.is_singleton());
        match self {
            Any(mut nodes) => nodes.pop().unwrap(),
            NotAny(mut nodes) => {
                let mut node = nodes.pop().unwrap();
                node.apply_not();
                node
            },
            All(mut nodes) => nodes.pop().unwrap(),
            NotAll(mut nodes) => {
                let mut node = nodes.pop().unwrap();
                node.apply_not();
                node
            },
            _ => unreachable!("extract_singleton on a BetaNode"),
        }
    }

    fn simplify(&mut self) {
        use self::Stage1Node::*;
        match *self {
            Any(ref mut nodes) => while Self::simplify_any(nodes) {},
            NotAny(ref mut nodes) => while Self::simplify_any(nodes) {},
            All(ref mut nodes) => while Self::simplify_all(nodes) {},
            NotAll(ref mut nodes) => while Self::simplify_all(nodes) {},
            _ => {}
        }
    }

    /*
     * A while ago I was having a bad day and this quote made me laugh.
     * Let it be immortalized here forever with these merge functions as it
     * was posted around the same time I wrote the initial version of this algorithm.
     * They are inextricably bound in my mind
     *
     * This tank ain't big enough for both of us.
     * It's just the perfect size for a fish of our both's combined volume! And that fish is me!
     * - /u/GregTheMad - https://www.reddit.com/r/WTF/comments/82o8jv/this_tank_aint_big_enough_for_both_of_us/dvboaqw/
    */
    fn give(&mut self, to: &mut Vec<Self>) {
        use self::Stage1Node::*;
        match *self {
            Any(ref mut from) => to.append(from),
            All(ref mut from) => to.append(from),
            _ => unreachable!("give from invalid node")
        }
    }

    fn merge(&mut self, from_node: Self) {
        use self::Stage1Node::*;
        match(self, from_node) {
            (&mut Any(ref mut to), Any(ref mut from)) => to.append(from),
            _ => unreachable!("merge on invalid node combination")
        }
    }

    fn simplify_any(any: &mut Vec<Self>) -> bool {
        for node in any.iter_mut() {
            node.simplify();
        }
        let mut continue_simplify = false;
        // Extract singletons
        if any.iter().filter(|n| n.is_singleton()).count() > 0 {
            continue_simplify = true;
            for mut o in any.drain_where(|n| n.is_singleton()) {
                any.push(o.extract_singleton());
            }
        }
        // Merge any nodes
        if any.iter().filter(|n| n.is_any()).count() > 0 {
            continue_simplify = true;
            for mut o in any.drain_where(|n| n.is_any()) {
                o.give(any);
            }

        }
        continue_simplify
    }

    fn simplify_all(all: &mut Vec<Self>) -> bool {
        for node in all.iter_mut() {
            node.simplify();
        }
        let mut continue_simplify = false;
        // Extract singletons
        if all.iter().filter(|n| n.is_singleton()).count() > 0 {
            continue_simplify = true;
            for mut o in all.drain_where(|n| n.is_singleton()) {
                all.push(o.extract_singleton());
            }
        }
        // Merge all nodes
        if all.iter().filter(|n| n.is_all()).count() > 0 {
            continue_simplify = true;
            for mut o in all.drain_where(|n| n.is_all()) {
                o.give(all);
            }
        }
        // If there are multiple any nodes, merge into 1
        if all.iter().filter(|n| n.is_any()).count() > 1 {
            let mut cum_any = all.drain_where(|n| n.is_any());
            let mut parent = cum_any.pop().unwrap();
            for mut o in cum_any {
                parent.merge(o);
            }
            parent.simplify();
            all.push(parent);
        }
        continue_simplify
    }

    fn dedup(&mut self) {
        use self::Stage1Node::*;
        match *self {
            Any(ref mut nodes) => Self::dedup_vec(nodes),
            NotAny(ref mut nodes) => Self::dedup_vec(nodes),
            All(ref mut nodes) => Self::dedup_vec(nodes),
            NotAll(ref mut nodes) => Self::dedup_vec(nodes),
            _ => {}
        }
    }

    fn dedup_vec(nodes: &mut Vec<Self>) {
        for node in nodes.iter_mut() {
            node.dedup();
        }
        nodes.sort();
        nodes.dedup();
    }

    pub fn clean(mut self) -> Self {
        self.simplify();
        self.dedup();

        // TODO - add a walker to determine if there are any singletons, then do this until there are none
        self.simplify();
        self.dedup();
        self
    }

    pub fn collect_alpha(&mut self) -> Vec<AlphaNode<T>> {
        use self::Stage1Node::*;
        match *self {
            All(ref mut v) => {
                v.drain_where(|n| n.is_alpha())
                    .into_iter()
                    .map(|n| n.get_alpha()).collect()
            },
            _ => unreachable!("collect_alpha on non-all node")
        }
    }
}

impl<T: Fact> CollectRequired for Stage1Node<T> {
    fn collect_required(&self, symbols: &mut HashSet<SymbolId>) {
        use self::Stage1Node::*;
        match *self {
            T(ref node) => node.collect_required(symbols),
            Any(ref nodes) => nodes.iter().for_each(|n| n.collect_required(symbols)),
            NotAny(ref nodes) => nodes.iter().for_each(|n| n.collect_required(symbols)),
            All(ref nodes) => nodes.iter().for_each(|n| n.collect_required(symbols)),
            NotAll(ref nodes) => nodes.iter().for_each(|n| n.collect_required(symbols)),
        }
    }
}

pub trait Stage1Compile<T: Fact> {

    fn stage1_compile(&self, cache: &mut StringCache) -> Result<Stage1Node<T>, CompileError>;

    fn stage1_compile_slice(t: &[Self], cache: &mut StringCache) -> Result<Vec<Stage1Node<T>>, CompileError>
        where Self: marker::Sized {
        t.iter().map(|c| c.stage1_compile(cache)).collect()
    }
}


#[derive(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug, EnumIndex)]
pub enum DeclareNode<S, G> {
    Var(S),
    Field(S, G)
}

impl<S, G> DeclareNode<S, G> {
    pub fn is_variable(&self) -> bool {
        use self::DeclareNode::*;
        match *self {
            Var(_) => true,
            _ => false,
        }
    }

    pub fn is_field(&self) -> bool {
        use self::DeclareNode::*;
        match *self {
            Field(..) => true,
            _ => false,
        }
    }
}

impl<S> DeclareNode<S, S> where S: AsRef<str> {
    pub fn compile<T: Fact>(&self, cache: &mut StringCache) -> Result<DeclareNode<SymbolId, Getter<T>>, CompileError> {
        use self::DeclareNode::*;
        match *self {
            Var(ref s) => Ok(Var(cache.get_or_intern(s.as_ref()))),
            Field(ref s, ref g) => Ok(Field(
                cache.get_or_intern(s.as_ref()),
                T::getter(g.as_ref()).ok_or_else(|| CompileError::MissingGetter { getter: g.as_ref().to_owned() })?
            )),
        }
    }
}

pub fn var<S: AsRef<str>>(s: S) -> DeclareNode<S, S> {
    DeclareNode::Var(s)
}

pub fn field<S: AsRef<str>>(s: S, g: S) -> DeclareNode<S, S> {
    DeclareNode::Field(s, g)
}