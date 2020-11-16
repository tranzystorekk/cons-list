use crate::list::List;

use std::ops::Deref;

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

#[macro_export]
macro_rules! head_matches {
    ($cons:expr, $($head:pat)|+ $( if $guard:expr )?) => {
        matches!(
            $cons,
            $( $crate::Cons::Cons($head, _) )|+
            $( if $guard )?
        )
    };
}

pub type LCons<T> = Cons<T, List<T>>;

pub enum Cons<T, L> {
    Cons(T, L),
    Nil,
}

impl<T> Cons<T, List<T>> {
    pub fn head(self) -> Option<T> {
        head_method_body!(self)
    }

    pub fn as_head(&self) -> Option<&T> {
        head_method_body!(self)
    }

    pub fn as_mut_head(&mut self) -> Option<&mut T> {
        head_method_body!(self)
    }

    /// Converts `&Cons<T, List<T>>` to `Option<&T::Target>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![Box::new(1), Box::new(2)];
    /// let cons = list.cons();
    ///
    /// assert!(matches!(cons.as_deref_head(), Some(&1)));
    /// ```
    pub fn as_deref_head(&self) -> Option<&T::Target>
    where
        T: Deref,
    {
        match self {
            Cons::Cons(ref head, _) => Some(head.deref()),
            _ => None,
        }
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

    pub fn as_ref(&self) -> Cons<&T, &List<T>> {
        match self {
            Cons::Cons(ref head, ref tail) => Cons::Cons(head, tail),
            _ => Cons::Nil,
        }
    }

    /// Converts from `&Cons<T, List<T>>` to `Cons<&T::Target, &List<T>>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::{head_matches, linked_list};
    ///
    /// let list = linked_list![Box::new(1), Box::new(2)];
    /// let cons = list.cons();
    ///
    /// assert!(head_matches!(cons.as_deref(), &1));
    /// ```
    pub fn as_deref(&self) -> Cons<&T::Target, &List<T>>
    where
        T: Deref,
    {
        match self {
            Cons::Cons(ref head, ref tail) => Cons::Cons(head.deref(), tail),
            _ => Cons::Nil,
        }
    }
}

impl<T, L> Cons<T, L> {
    pub fn is_cons(&self) -> bool {
        matches!(self, Cons::Cons(_, _))
    }

    pub fn is_nil(&self) -> bool {
        matches!(self, Cons::Nil)
    }
}
