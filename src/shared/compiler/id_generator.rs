use std::fmt;
use serial::SerialGen;

macro_rules! id_generator {
    ( $( $t:ident => $g:ident => $f:ident),+) => {
        $(
            #[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
            pub struct $t{index: usize}

            impl $t {
                pub fn index(&self) -> usize {self.index}
            }

            impl fmt::Debug for $t {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self.index)
                }
            }

            impl From<usize> for $t {
                fn from( index: usize) -> $t {
                    $t{index}
                }
            }

            pub type $g = SerialGen<usize, $t>;

        )*

        #[derive(Debug)]
        pub struct IdGenerator {
            $(
                pub $f : $g,
            )*
        }

        impl IdGenerator {
            pub fn new() -> IdGenerator {
                IdGenerator {
                    $(
                        $f: Default::default(),
                    )*
                }
            }
        }

        impl Default for IdGenerator {
            fn default() -> Self {
                IdGenerator::new()
            }
        }
    };
}

id_generator!(
    RuleId => RuleIdGen => rule_ids,
    StatementId => StatementIdGen => statement_ids,
    ConditionId => ConditionIdGen => condition_ids,
    HashEqId => HashEqIdGen => hasheq_ids,
    AlphaId => AlphaIdGen => alpha_ids,
    BetaId => BetaIdGen => beta_ids
    );

