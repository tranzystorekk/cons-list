//! A cons enum.

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

/// A macro shortcut for checking if the head portion of a `Cons` matches a given pattern.
///
/// # Examples
///
/// ```
/// use cons_list::{head_matches, linked_list};
///
/// let list = linked_list![Some(1), Some(2), None, None, Some(5), Some(6)];
/// let cons = list.cons();
///
/// assert!(head_matches!(cons, Some(1)));
/// ```
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

pub(crate) type LCons<T> = Cons<T, List<T>>;

/// An enum that allows Cons-like operations and pattern matching on a `List`.
///
/// Obtained via `List::cons()`.
pub enum Cons<T, L> {
    /// A pair of a head element and a tail list (everything excluding the head).
    Cons(T, L),
    /// A result of consing an empty list.
    Nil,
}

impl<T> Cons<T, List<T>> {
    /// Converts from `Cons<T, List<T>>` to `Option<T>`, discarding the tail.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![1, 2];
    /// let cons = list.cons();
    ///
    /// assert!(matches!(cons.head(), Some(1)));
    /// ```
    pub fn head(self) -> Option<T> {
        head_method_body!(self)
    }

    /// Converts from `&Cons<T, List<T>>` to `Option<&T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![1, 2];
    /// let cons = list.cons();
    ///
    /// assert!(matches!(cons.as_head(), Some(&1)));
    /// ```
    pub fn as_head(&self) -> Option<&T> {
        head_method_body!(self)
    }

    /// Converts from `&mut Cons<T, List<T>>` to `Option<&mut T>`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::{head_matches, linked_list};
    ///
    /// let list = linked_list![1, 2];
    /// let mut cons = list.cons();
    ///
    /// if let Some(x) = cons.as_mut_head() {
    ///     *x += 10;
    /// }
    ///
    /// assert!(head_matches!(cons, 11));
    /// ```
    pub fn as_mut_head(&mut self) -> Option<&mut T> {
        head_method_body!(self)
    }

    /// Converts from `&Cons<T, List<T>>` to `Option<&T::Target>`.
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

    /// Converts from `Cons<T, List<T>>` to `Option<List<T>>`, discarding the head.
    ///
    /// Note: discards tail and returns `None` if the tail is empty.
    pub fn tail(self) -> Option<List<T>> {
        tail_method_body!(self)
    }

    /// Converts from `&Cons<T, List<T>>` to `Option<&List<T>>`.
    ///
    /// Note: returns `None` if the tail is empty.
    pub fn as_tail(&self) -> Option<&List<T>> {
        tail_method_body!(self)
    }

    /// Converts from `&mut Cons<T, List<T>>` to `Option<&mut List<T>>`.
    ///
    /// Note: returns `None` if the tail is empty.
    pub fn as_mut_tail(&mut self) -> Option<&mut List<T>> {
        tail_method_body!(self)
    }

    /// Converts from `&Cons<T, List<T>>` to `Cons<&T, &List<T>>`.
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
    /// Returns `true` if this cons is a `Cons`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![1, 2];
    ///
    /// assert!(list.cons().is_cons());
    /// ```
    pub const fn is_cons(&self) -> bool {
        matches!(self, Cons::Cons(_, _))
    }

    /// Returns `true` if this cons is a `Nil`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::List;
    ///
    /// let list: List<i32> = List::new();
    ///
    /// assert!(list.cons().is_nil());
    /// ```
    pub const fn is_nil(&self) -> bool {
        matches!(self, Cons::Nil)
    }
}
