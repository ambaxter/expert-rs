use num::{One, Zero, Unsigned};
use num::zero;
use std::marker::PhantomData;

pub struct SerialGen<T: Unsigned  + Zero + One + Copy + Into<R>, R> {
    r: PhantomData<R>,
    serial: T
}

impl<T: Unsigned  + Zero + One + Copy + Into<R>, R> SerialGen<T, R>{
    pub fn new(serial: T) -> SerialGen<T, R> {
        SerialGen{r: PhantomData, serial}
    }

    pub fn next(&mut self) -> R {
        let prev = self.serial;
        self.serial = prev + T::one();
        prev.into()
    }

    pub fn check(&self) -> T {
        self.serial
    }
}


impl<T: Unsigned  + Zero + One + Copy + Into<R>, R> Default for SerialGen<T, R> {
    fn default() -> Self {
        SerialGen::new(T::zero())
    }
}

