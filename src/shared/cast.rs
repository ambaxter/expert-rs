use num::{NumCast, ToPrimitive};
use ordered_float::NotNaN;
use num::Float;

pub trait ToNotNaNPrimitive : ToPrimitive {
    #[inline]
    fn to_nn_f32(&self) -> Option<NotNaN<f32>> {
        self.to_f32().map(|f| f.into())
    }

    #[inline]
    fn to_nn_f64(&self) -> Option<NotNaN<f64>> {
        self.to_f64().map(|f| f.into())
    }
}


/// An interface for casting between machine scalars.
pub trait NotNaNCast: Sized + ToNotNaNPrimitive {
    type Output;
    /// Creates a number from another value that can be converted into
    /// a primitive via the `ToPrimitive` trait.
    fn from<T: ToNotNaNPrimitive>(n: T) -> Option<Self::Output>;
}

#[inline]
pub fn cast<T: NotNaNCast, U: NotNaNCast>(n: T) -> Option<U::Output>  {
    <U as NotNaNCast>::from(n)
}

macro_rules! impl_nn_cast {
    ($T:ty, $O:ty, $conv:ident) => (
        impl ToNotNaNPrimitive for $T {}

        impl NotNaNCast for $T {
            type Output = $O;
            #[inline]
            #[allow(deprecated)]
            fn from<N: ToNotNaNPrimitive>(n: N) -> Option<Self::Output> {
                // `$conv` could be generated using `concat_idents!`, but that
                // macro seems to be broken at the moment
                n.$conv()
            }
        }
    )
}

impl_nn_cast!(u8, u8, to_u8);
impl_nn_cast!(u16, u16, to_u16);
impl_nn_cast!(u32, u32, to_u32);
impl_nn_cast!(u64, u64, to_u64);
impl_nn_cast!(usize, usize, to_usize);
impl_nn_cast!(i8, i8, to_i8);
impl_nn_cast!(i16, i16, to_i16);
impl_nn_cast!(i32, i32, to_i32);
impl_nn_cast!(i64, i64, to_i64);
impl_nn_cast!(isize, isize, to_isize);
impl_nn_cast!(f32, NotNaN<f32>, to_nn_f32);
impl_nn_cast!(f64, NotNaN<f64>, to_nn_f64);
