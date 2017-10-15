macro_rules! index_id {
    ( $( $t:ident),+) => {
        $(
            impl $t {
                pub fn index(&self) -> usize {self.id}
            }

            impl Debug for $t {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "{:?}", self.id)
                }
            }

            impl Into<$t> for usize {
                fn into(self) -> $t {
                    $t{id: self}
                }
            }
        )*
    };
}