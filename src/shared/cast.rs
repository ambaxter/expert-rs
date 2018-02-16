use num::{NumCast, Float, ToPrimitive};
use ordered_float::NotNaN;
use std::ops::Deref;

// Since I can't implement ToPrimitive for NotNaN, we need to unpack NotNaN (and everything else)
pub trait UnpackPrimitive {
    type Output: ToNNPrimitive;

    fn unpack(&self) -> Self::Output;
}

macro_rules! impl_unpack_prim {
    ($T:ty) => {
        impl UnpackPrimitive for $T {
            type Output = $T;

            #[inline]
            fn unpack(&self) -> Self::Output {
                *self
            }
        }
    }
}

macro_rules! impl_unpack_nnprim {
    ($T:ty) => {
        impl<T> UnpackPrimitive for $T
            where T: Float + ToNNPrimitive {
            type Output = T;

            #[inline]
            fn unpack(&self) -> Self::Output {
                *self.deref()
            }
        }
    }
}

impl_unpack_prim!(i8);
impl_unpack_prim!(i16);
impl_unpack_prim!(i32);
impl_unpack_prim!(i64);
impl_unpack_prim!(isize);
impl_unpack_prim!(u8);
impl_unpack_prim!(u16);
impl_unpack_prim!(u32);
impl_unpack_prim!(u64);
impl_unpack_prim!(usize);
impl_unpack_nnprim!(NotNaN<T>);

pub trait ToNNPrimitive: ToPrimitive {
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
pub trait NNCast: Sized + ToNNPrimitive {
    type Output;
    /// Creates a number from another value that can be converted into
    /// a primitive via the `ToPrimitive` trait.
    fn from<T: ToNNPrimitive>(n: T) -> Option<Self::Output>;
}

#[inline]
pub fn cast<T: ToNNPrimitive, U: NNCast>(n: T) -> Option<U::Output> {
    <U as NNCast>::from(n)
}

macro_rules! impl_nn_cast {
    ($T:ty, $O:ty, $conv:ident) => (
        impl ToNNPrimitive for $T {}

        impl NNCast for $T {
            type Output = $O;
            #[inline]
            #[allow(deprecated)]
            fn from<N: ToNNPrimitive>(n: N) -> Option<Self::Output> {
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


#[cfg(test)]
mod tests {

    use ::shared::cast::{NNCast, cast};

    #[test]
    fn test() {
        assert_eq!(Some(2u8), cast(2u16));
    }

}