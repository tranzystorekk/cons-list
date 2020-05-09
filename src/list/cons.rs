use crate::list::core::List;

macro_rules! head_method_body {
    ($myself:ident) => {
        match $myself {
            Cons::Cons(head, _) => Some(head),
            _ => None,
        }
    };
}

macro_rules! tail_method_body {
    ($myself:ident) => {
        match $myself {
            Cons::Cons(_, tail) if !tail.is_empty() => Some(tail),
            _ => None,
        }
    };
}

pub enum Cons<T> {
    Cons(T, List<T>),
    Nil,
}

impl<T> Cons<T> {
    pub fn is_cons(&self) -> bool {
        matches!(self, Cons::Cons(_, _))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Cons::Nil)
    }

    pub fn head(self) -> Option<T> {
        head_method_body!(self)
    }

    pub fn as_head(&self) -> Option<&T> {
        head_method_body!(self)
    }

    pub fn as_mut_head(&mut self) -> Option<&mut T> {
        head_method_body!(self)
    }

    pub fn tail(self) -> Option<List<T>> {
        tail_method_body!(self)
    }

    pub fn as_tail(&self) -> Option<&List<T>> {
        tail_method_body!(self)
    }

    pub fn as_mut_tail(&mut self) -> Option<&mut List<T>> {
        tail_method_body!(self)
    }
}
