#[derive(Copy, Clone)]
pub struct OptionIter<T: Copy> {
    is_some: bool,
    is_none: bool,
    t: Option<T>
}

impl<T: Copy> OptionIter<T> {
    pub fn new(t: Option<T>) -> Self {
        let is_some = t.is_some();
        let is_none = true;
        OptionIter{is_some, is_none, t}
    }

    pub fn some(t: T) -> Self {
        Self::new(Some(t))
    }

    pub fn none() -> Self {
        Self::new(None)
    }
}

impl<T: Copy> Iterator for OptionIter<T> {
    type Item = Option<T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_some {
            self.is_some = false;
            return Some(self.t);
        } else if self.is_none {
            self.is_none = false;
            return Some(None);
        }
        None
    }
}