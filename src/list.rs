//! A single-linked list.

use crate::cons::{Cons, LCons};

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::{FromIterator, FusedIterator};

/// Creates a `List` containing the arguments.
///
/// For `linked_list![x; n]`, the given element is cloned to fill the list.
///
/// For `linked_list![x, y, z, ...]`, the elements are pushed into the list
/// ([Clone](std::clone::Clone) not required).
///
/// # Examples
///
/// This macro is similar in syntax to the [`vec!`] macro:
/// ```
/// use cons_list::linked_list;
///
/// let list_one = linked_list![1, 2, 3, 4, 5];
/// let list_two = linked_list!["foo"; 10];
/// ```
#[macro_export]
macro_rules! linked_list {
    () => {
        $crate::List::new()
    };
    ($el:expr; $n:expr) => {{
        let e = $el;
        let size = $n;
        let mut result = $crate::List::new();

        if size > 0 {
            for _ in 0..(size - 1) {
                result.push_front(e.clone());
            }
            result.push_front(e);
        }

        result
    }};
    ($($x:expr),+ $(,)?) => {
        $crate::List::from([$($x),+])
    };
}

struct Node<T> {
    value: T,
    next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

/// A single-linked list.
///
/// This list internally keeps only its head,
/// so operations like [`List::push_back()`], [`List::last()`] or [`List::len()`]
/// will execute in *O*(n).
#[derive(Default)]
pub struct List<T> {
    head: Link<T>,
}

/// A reference iterator over a `List`.
pub struct Iter<'a, T> {
    current_node: Option<&'a Node<T>>,
}

/// A mutable reference iterator over a `List`.
pub struct IterMut<'a, T> {
    current_node: Option<&'a mut Node<T>>,
}

/// An iterator that takes ownership of a `List`.
#[derive(Clone)]
pub struct IntoIter<T> {
    list: List<T>,
}

/// An iterator that removes from a `List` elements specified by a predicate.
pub struct DrainFilter<'a, T, F: FnMut(&mut T) -> bool> {
    owner: &'a mut Link<T>,
    pred: F,
}

impl<T> List<T> {
    /// Creates an empty `List`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::List;
    ///
    /// let list: List<u32> = List::new();
    /// ```
    pub const fn new() -> Self {
        Self { head: None }
    }

    /// Creates a list from a cons pair `head, tail`.
    ///
    /// Allocates memory for the `head` element within the `tail` list.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::{linked_list, List};
    ///
    /// let head = 1;
    /// let tail = linked_list![2, 3, 4, 5];
    ///
    /// let list = List::from_cons(head, tail);
    ///
    /// assert_eq!(linked_list![1, 2, 3, 4, 5], list);
    /// ```
    pub fn from_cons(head: T, mut tail: List<T>) -> Self {
        let head = Node {
            value: head,
            next: tail.head.take(),
        };

        Self {
            head: Some(Box::new(head)),
        }
    }

    /// Returns `true` if the `List` is empty.
    ///
    /// Complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::List;
    ///
    /// let mut list = List::new();
    /// assert!(list.is_empty());
    ///
    /// list.push_front("foo");
    /// assert!(!list.is_empty());
    /// ```
    pub const fn is_empty(&self) -> bool {
        self.head.is_none()
    }

    /// Adds an element to the front of the `List`.
    ///
    /// Complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![];
    ///
    /// list.push_front(2);
    /// list.push_front(1);
    ///
    /// assert_eq!(linked_list![1, 2], list);
    /// ```
    pub fn push_front(&mut self, elem: T) {
        let new_node = Node {
            value: elem,
            next: self.head.take(),
        };

        self.head = Some(Box::new(new_node));
    }

    /// Removes the first element from the `List` and returns it,
    /// or [`None`] if the `List` is empty.
    ///
    /// Complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3];
    ///
    /// assert_eq!(list.pop_front(), Some(1));
    /// assert_eq!(list.pop_front(), Some(2));
    /// assert_eq!(list.pop_front(), Some(3));
    /// assert_eq!(list.pop_front(), None);
    /// ```
    pub fn pop_front(&mut self) -> Option<T> {
        self.pop_node().map(|node| node.value)
    }

    /// Removes all elements from the `List`.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![5, 6, 7, 8, 9, 10];
    ///
    /// assert!(!list.is_empty());
    ///
    /// list.clear();
    /// assert!(list.is_empty());
    /// ```
    pub fn clear(&mut self) {
        while self.pop_node().is_some() {}
    }

    /// Provides a reference to the first element,
    /// or [`None`] if the `List` is empty.
    ///
    /// Complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![1, 2, 3];
    ///
    /// assert_eq!(Some(&1), list.head());
    /// ```
    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }

    /// Provides a mutable reference to the first element,
    /// or [`None`] if the `List` is empty.
    ///
    /// Complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3];
    ///
    /// if let Some(head) = list.head_mut() {
    ///     *head += 10;
    /// }
    ///
    /// assert_eq!(linked_list![11, 2, 3], list);
    /// ```
    pub fn head_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.value)
    }

    /// Provides a reference to the last element,
    /// or [`None`] if the `List` is empty.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![1, 2, 3];
    ///
    /// assert_eq!(Some(&3), list.last());
    /// ```
    pub fn last(&self) -> Option<&T> {
        self.iter().last()
    }

    /// Provides a mutable reference to the last element,
    /// or [`None`] if the `List` is empty.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3];
    ///
    /// if let Some(last) = list.last_mut() {
    ///     *last += 10;
    /// }
    ///
    /// assert_eq!(linked_list![1, 2, 13], list);
    /// ```
    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.iter_mut().last()
    }

    /// Adds an element to the back of the `List`.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![];
    ///
    /// list.push_back(1);
    /// list.push_back(2);
    ///
    /// assert_eq!(linked_list![1, 2], list);
    /// ```
    pub fn push_back(&mut self, elem: T) {
        let new_node = Node {
            value: elem,
            next: None,
        };
        let new_packed = Some(Box::new(new_node));

        let owner = unsafe { self.empty_owner() };

        *owner = new_packed;
    }

    /// Inserts an element at the given position.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Panics
    ///
    /// Panics if `at > len`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3, 5, 6];
    ///
    /// list.insert(3, 4);
    ///
    /// assert_eq!(linked_list![1, 2, 3, 4, 5, 6], list);
    /// ```
    pub fn insert(&mut self, at: usize, value: T) {
        let nth = unsafe { self.nth_owner(at) };

        let new_node = Node {
            value,
            next: nth.take(),
        };

        *nth = Some(Box::new(new_node));
    }

    /// Moves all elements from `other` to the back of the `List`.
    ///
    /// After this operation, `other` becomes empty.
    ///
    /// Complexity: *O*(n)
    ///
    /// Memory complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3];
    /// let mut second = linked_list![4, 5, 6];
    ///
    /// list.append(&mut second);
    ///
    /// assert!(second.is_empty());
    /// assert_eq!(linked_list![1, 2, 3, 4, 5, 6], list);
    /// ```
    pub fn append(&mut self, other: &mut List<T>) {
        if other.is_empty() {
            return;
        }

        let other_head = other.head.take();

        let owner = unsafe { self.empty_owner() };

        *owner = other_head;
    }

    /// Moves all elements from `other` to the front of the `List`.
    ///
    /// After this operation, `other` becomes empty.
    ///
    /// Complexity: *O*(k) where k is the size of `other`.
    ///
    /// Memory complexity: *O*(1)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![4, 5, 6];
    /// let mut second = linked_list![1, 2, 3];
    ///
    /// list.prepend(&mut second);
    ///
    /// assert!(second.is_empty());
    /// assert_eq!(linked_list![1, 2, 3, 4, 5, 6], list);
    /// ```
    pub fn prepend(&mut self, other: &mut List<T>) {
        let other_owner = unsafe { other.last_owner() };

        if let Some(node) = other_owner.as_deref_mut() {
            node.next = self.head.take();
            self.head = other.head.take();
        }
    }

    /// Returns the length of the `List`.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3];
    ///
    /// assert_eq!(list.len(), 3);
    ///
    /// list.pop_front();
    /// assert_eq!(list.len(), 2);
    /// ```
    pub fn len(&self) -> usize {
        self.iter().count()
    }

    /// Converts from `List<T>` to `Cons<T, List<T>>`.
    ///
    /// See [`Cons`] for more information.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::{linked_list, Cons};
    ///
    /// let list = linked_list![1, 2, 3, 4, 5];
    /// let cons = list.cons();
    ///
    /// assert_eq!(Some(&1), cons.as_head());
    /// assert_eq!(Some(&linked_list![2, 3, 4, 5,]), cons.as_tail());
    /// ```
    pub fn cons(mut self) -> LCons<T> {
        match self.head.take() {
            Some(node) => {
                let tail = Self { head: node.next };
                Cons::Cons(node.value, tail)
            }
            _ => Cons::Nil,
        }
    }

    /// Returns a forward iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list![1, 2, 3, 4];
    ///
    /// let mut iter = list.iter();
    /// assert_eq!(iter.next(), Some(&1));
    /// assert_eq!(iter.next(), Some(&2));
    /// assert_eq!(iter.next(), Some(&3));
    /// assert_eq!(iter.next(), Some(&4));
    /// assert_eq!(iter.next(), None);
    /// ```
    pub fn iter(&self) -> Iter<'_, T> {
        Iter::new(self)
    }

    /// Returns a forward iterator with mutable references.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3, 4];
    ///
    /// for el in list.iter_mut() {
    ///     *el += 10;
    /// }
    ///
    /// assert_eq!(linked_list![11, 12, 13, 14], list);
    /// ```
    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut::new(self)
    }

    /// Returns `true` if the `List` contains an element equal to the given value.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let list = linked_list!["aaa", "bbb", "ccc", "ddd"];
    ///
    /// assert!(list.contains(&"ccc"));
    /// assert!(!list.contains(&"abc"));
    /// ```
    pub fn contains(&self, x: &T) -> bool
    where
        T: PartialEq,
    {
        self.iter().any(|el| el == x)
    }

    /// Returns an iterator that uses the predicate to determine whether
    /// an element should be removed.
    ///
    /// If the predicate evaluates to `true`, an element is removed and yielded.
    ///
    /// Note that the elements are removed regardless of whether you consume the iterator or not.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![5, 6, 7, 8, 9];
    ///
    /// {
    ///     let mut drain = list.drain_filter(|&mut el| el % 4 > 1);
    ///     assert_eq!(drain.next(), Some(6));
    ///     assert_eq!(drain.next(), Some(7));
    ///     assert_eq!(drain.next(), None);
    /// }
    ///
    /// assert_eq!(linked_list![5, 8, 9], list);
    /// ```
    pub fn drain_filter<F: FnMut(&mut T) -> bool>(&mut self, pred: F) -> DrainFilter<'_, T, F> {
        DrainFilter::new(self, pred)
    }

    /// Remove and discard from the `List` all elements for which the predicate evaluates to `true`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3, 4, 5, 6, 7, 8, 9];
    ///
    /// list.remove_if(|el| *el % 3 == 0);
    ///
    /// assert_eq!(linked_list![1, 2, 4, 5, 7, 8], list);
    /// ```
    pub fn remove_if<F: FnMut(&mut T) -> bool>(&mut self, pred: F) {
        let _ = self.drain_filter(pred);
    }

    /// Reverses the order of elements in the `List`, in place.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3, 4, 5];
    ///
    /// list.reverse();
    ///
    /// assert_eq!(linked_list![5, 4, 3, 2, 1], list);
    /// ```
    pub fn reverse(&mut self) {
        let head_node = self.head.as_deref();
        if let Some(Node { next: None, .. }) | None = head_node {
            return;
        }

        let mut cur = self.head.take();
        while let Some(mut node) = cur {
            cur = std::mem::replace(&mut node.next, self.head.take());
            self.head = Some(node);
        }
    }

    /// Removes the last element from the `List` and returns it,
    /// or [`None`] if the `List` is empty.
    ///
    /// Complexity: *O*(n)
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3];
    ///
    /// assert_eq!(list.pop_back(), Some(3));
    /// assert_eq!(list.pop_back(), Some(2));
    /// assert_eq!(list.pop_back(), Some(1));
    /// assert_eq!(list.pop_back(), None);
    /// ```
    pub fn pop_back(&mut self) -> Option<T> {
        unsafe { self.pop_back_impl() }
    }

    unsafe fn pop_back_impl(&mut self) -> Option<T> {
        self.last_owner().take().map(|node| node.value)
    }

    /// Removes and returns the element at the given index.
    ///
    /// # Panics
    ///
    /// Panics if `at >= len`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3, 4, 5];
    ///
    /// assert_eq!(list.remove(3), 4);
    /// assert_eq!(linked_list![1, 2, 3, 5], list);
    /// ```
    pub fn remove(&mut self, at: usize) -> T {
        let owner = unsafe { self.nth_owner(at) };

        let mut node = owner.take().expect("illegal access past list bounds");
        *owner = node.next.take();

        node.value
    }

    /// Splits the `List` at the given index.
    /// Returns everything after the index (inclusive).
    ///
    /// # Panics
    ///
    /// Panics if `at > len`.
    ///
    /// # Examples
    ///
    /// ```
    /// use cons_list::linked_list;
    ///
    /// let mut list = linked_list![1, 2, 3, 4, 5, 6];
    /// let split = list.split_off(4);
    ///
    /// assert_eq!(linked_list![1, 2, 3, 4], list);
    /// assert_eq!(linked_list![5, 6], split);
    /// ```
    pub fn split_off(&mut self, at: usize) -> Self {
        unsafe { self.split_off_impl(at) }
    }

    unsafe fn split_off_impl(&mut self, at: usize) -> Self {
        Self {
            head: self.nth_owner(at).take(),
        }
    }

    unsafe fn nth_owner(&mut self, n: usize) -> &mut Link<T> {
        let mut cur: *mut _ = &mut self.head;

        for _ in 0..n {
            let node = (*cur)
                .as_deref_mut()
                .expect("illegal access past list bounds");
            cur = &mut node.next;
        }

        &mut *cur
    }

    unsafe fn last_owner(&mut self) -> &mut Link<T> {
        let mut cur: *mut _ = &mut self.head;

        while let Some(node) = (*cur).as_deref_mut() {
            if node.next.is_none() {
                break;
            }

            cur = &mut node.next;
        }

        &mut *cur
    }

    unsafe fn empty_owner(&mut self) -> &mut Link<T> {
        let mut cur: *mut _ = &mut self.head;

        while let Some(node) = (*cur).as_deref_mut() {
            cur = &mut node.next;
        }

        &mut *cur
    }

    unsafe fn extend_from_iter<I: Iterator<Item = T>>(&mut self, iter: I) {
        let mut owner = self.empty_owner();

        for value in iter {
            let new_node = Node { value, next: None };

            let node_in_place = owner.insert(Box::new(new_node));
            owner = &mut node_in_place.next;
        }
    }

    fn pop_node(&mut self) -> Link<T> {
        self.head.take().map(|mut node| {
            self.head = node.next.take();
            node
        })
    }
}

impl<T> Extend<T> for List<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        unsafe {
            self.extend_from_iter(iter.into_iter());
        }
    }
}

impl<'a, T: 'a + Copy> Extend<&'a T> for List<T> {
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        unsafe {
            self.extend_from_iter(iter.into_iter().copied());
        }
    }
}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut result = List::new();
        result.extend(iter);

        result
    }
}

impl<T: Clone> Clone for List<T> {
    fn clone(&self) -> Self {
        self.iter().cloned().collect()
    }

    fn clone_from(&mut self, source: &Self) {
        let mut owner: *mut _ = &mut self.head;
        let mut iter_other = source.iter();

        // clone into existing nodes in-place
        while let Some((node, elem)) =
            unsafe { Iterator::zip((*owner).iter_mut(), iter_other.by_ref()).next() }
        {
            node.value.clone_from(elem);
            owner = &mut node.next;
        }

        // allocate missing nodes
        for value in iter_other.cloned() {
            let new_node = Node { value, next: None };

            let node_in_place = unsafe { (*owner).insert(Box::new(new_node)) };
            owner = &mut node_in_place.next;
        }

        // drop unneeded nodes
        let _ = Self {
            head: unsafe { (*owner).take() },
        };
    }
}

impl<T> From<Cons<T, List<T>>> for List<T> {
    fn from(cons: Cons<T, List<T>>) -> Self {
        match cons {
            Cons::Cons(head, tail) => Self::from_cons(head, tail),
            Cons::Nil => Self::new(),
        }
    }
}

impl<T, const N: usize> From<[T; N]> for List<T> {
    fn from(array: [T; N]) -> Self {
        array.into_iter().collect()
    }
}

impl<T: PartialEq> PartialEq for List<T> {
    fn eq(&self, other: &Self) -> bool {
        self.iter().eq(other)
    }

    #[allow(clippy::partialeq_ne_impl)]
    fn ne(&self, other: &Self) -> bool {
        self.iter().ne(other)
    }
}

impl<T: Eq> Eq for List<T> {}

impl<T: PartialOrd> PartialOrd for List<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.iter().partial_cmp(other)
    }
}

impl<T: Ord> Ord for List<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.iter().cmp(other)
    }
}

impl<T: Hash> Hash for List<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for el in self {
            el.hash(state);
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for List<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self).finish()
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        while self.pop_node().is_some() {}
    }
}

impl<T> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { list: self }
    }
}

impl<'a, T> IntoIterator for &'a List<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut List<T> {
    type Item = &'a mut T;
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<'a, T> Iter<'a, T> {
    fn new(list: &'a List<T>) -> Self {
        Self {
            current_node: list.head.as_deref(),
        }
    }
}

impl<'a, T> IterMut<'a, T> {
    fn new(list: &'a mut List<T>) -> Self {
        Self {
            current_node: list.head.as_deref_mut(),
        }
    }
}

impl<'a, T, F: FnMut(&mut T) -> bool> DrainFilter<'a, T, F> {
    fn new(list: &'a mut List<T>, pred: F) -> Self {
        Self {
            owner: &mut list.head,
            pred,
        }
    }

    fn next_node(&mut self) -> Link<T> {
        unsafe { self.move_to_next_drained() };

        self.owner.take().map(|mut node| {
            *self.owner = node.next.take();
            node
        })
    }

    unsafe fn move_to_next_drained(&mut self) {
        let mut ptr: *mut _ = self.owner;
        while let Some(node) = (*ptr).as_deref_mut() {
            if (self.pred)(&mut node.value) {
                break;
            }

            self.owner = &mut node.next;
            ptr = self.owner;
        }
    }
}

impl<T, F: FnMut(&mut T) -> bool> Drop for DrainFilter<'_, T, F> {
    fn drop(&mut self) {
        while self.next_node().is_some() {}
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.current_node.map(|node| {
            self.current_node = node.next.as_deref();

            &node.value
        })
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.current_node.take().map(|node| {
            self.current_node = node.next.as_deref_mut();

            &mut node.value
        })
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.list.pop_front()
    }
}

impl<T, F: FnMut(&mut T) -> bool> Iterator for DrainFilter<'_, T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_node().map(|node| node.value)
    }
}

/// This [`Clone`] impl in fact behaves like [`Copy`].
/// Implicit copying is disabled to avoid ambiguous usage bugs.
impl<'a, T> Clone for Iter<'a, T> {
    fn clone(&self) -> Self {
        Self {
            current_node: self.current_node,
        }
    }
}

impl<T: fmt::Debug> fmt::Debug for IntoIter<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("IntoIter").field(&self.list).finish()
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for Iter<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Iter")
            .field(&DebugIter(self.clone()))
            .finish()
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for IterMut<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = Iter {
            current_node: self.current_node.as_deref(),
        };

        f.debug_tuple("IterMut").field(&DebugIter(iter)).finish()
    }
}

impl<'a, T: fmt::Debug, F: FnMut(&mut T) -> bool> fmt::Debug for DrainFilter<'a, T, F> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let iter = Iter {
            current_node: self.owner.as_deref(),
        };

        f.debug_tuple("DrainFilter")
            .field(&DebugIter(iter))
            .finish()
    }
}

struct DebugIter<'a, T>(Iter<'a, T>);

impl<'a, T: fmt::Debug> fmt::Debug for DebugIter<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.0.clone()).finish()
    }
}

impl<T> FusedIterator for Iter<'_, T> {}
impl<T> FusedIterator for IterMut<'_, T> {}
impl<T> FusedIterator for IntoIter<T> {}
impl<T, F: FnMut(&mut T) -> bool> FusedIterator for DrainFilter<'_, T, F> {}
