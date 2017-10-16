macro_rules! index_id {
    ( $( $t:ident),+) => {
        $(
            impl $t {
                pub fn index(&self) -> usize {self.index}
            }

            impl Debug for $t {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self.index)
                }
            }

            impl Into<$t> for usize {
                fn into(self) -> $t {
                    $t{index: self}
                }
            }

        )*
    };
}

macro_rules! into_dest {
    ($dest: ident; $($id:ident => $sub:ident),+) => {
        $(
            impl Into<$dest> for $id {
                fn into(self) -> $dest {
                    $dest::$sub(self)
                }
            }
        )*
    };
}