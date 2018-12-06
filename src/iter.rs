#[derive(Copy, Clone)]
pub struct OptionIter<T: Copy> {
    is_some: bool,
    is_none: bool,
    t: Option<T>,
}

impl<T: Copy> OptionIter<T> {
    pub fn new(t: Option<T>) -> Self {
        let is_some = t.is_some();
        let is_none = true;
        OptionIter {
            is_some,
            is_none,
            t,
        }
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

/* Thanks bluss! - owe you alcohol of choice

pub fn product<I, F>(mut iterators: Vec<I>, mut cb: F)
    where I: Iterator + Clone,
          F: FnMut(&[I::Item])
{
    inner(&mut Vec::with_capacity(iterators.len()),
          &iterators.clone(), &mut iterators, &mut cb)
}

fn inner<I, F>(cur: &mut Vec<I::Item>,
               orig: &[I], iters: &mut [I], cb: &mut F)
    where I: Iterator + Clone,
          F: FnMut(&[I::Item])
{
    if let Some((front, rest)) = iters.split_first_mut() {
        for elt in &mut *front {
            cur.push(elt);
            inner(cur, &orig[1..], rest, cb);
            cur.pop();
        }
        if !cur.is_empty() {
            *front = orig[0].clone();
        }
    } else {
        cb(cur.as_slice())
    }
}


fn main() {
    let iter = vec![0..3, 1..2, 0..5, 0..3];

    product(iter, |elems| println!("{:?}", elems));
}
*/
