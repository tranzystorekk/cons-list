use crate::cons::{Cons, LCons};

use std::fmt;
use std::iter::FusedIterator;

#[macro_export]
macro_rules! linked_list {
    () => {
        $crate::List::new()
    };
    ($el:expr; $n:expr) => {
        {
            let e = $el;
            let size = $n;
            let mut result = $crate::List::new();
            for _ in 0..size {
                result.push_front(e.clone());
            }

            result
        }
    };
    ($($x:expr),+ $(,)?) => {
        {
            let mut result = $crate::List::new();
            let arr = [$($x),+];
            for el in arr.iter().cloned().rev() {
                result.push_front(el);
            }

            result
        }
    };
}

struct Node<T> {
    value: T,
    next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

#[derive(Default)]
pub struct List<T> {
    head: Link<T>,
}

pub struct Iter<'a, T> {
    current_node: Option<&'a Node<T>>,
}

pub struct IterMut<'a, T> {
    current_node: Option<&'a mut Node<T>>,
}

pub struct IntoIter<T> {
    list: List<T>,
}

pub struct DrainFilter<'a, T, F: FnMut(&mut T) -> bool> {
    owner: &'a mut Link<T>,
    pred: F,
}

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn from_cons(head: T, mut tail: List<T>) -> Self {
        let head = Node {
            value: head,
            next: tail.head.take(),
        };

        List {
            head: Some(Box::new(head)),
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.head, None)
    }

    pub fn push_front(&mut self, elem: T) {
        let new_node = Node {
            value: elem,
            next: self.head.take(),
        };

        self.head = Some(Box::new(new_node));
    }

    pub fn pop(&mut self) -> Option<T> {
        self.pop_node().map(|node| node.value)
    }

    pub fn clear(&mut self) {
        while self.pop_node().is_some() {}
    }

    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }

    pub fn head_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|node| &mut node.value)
    }

    pub fn last(&self) -> Option<&T> {
        self.iter().last()
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        self.iter_mut().last()
    }

    pub fn push_back(&mut self, elem: T) {
        let new_node = Node {
            value: elem,
            next: None,
        };
        let new_packed = Some(Box::new(new_node));

        match self.last_node_mut() {
            Some(node) => node.next = new_packed,
            _ => self.head = new_packed,
        }
    }

    pub fn append(&mut self, mut other: List<T>) {
        if other.is_empty() {
            return;
        }

        let other_head = other.head.take();

        match self.last_node_mut() {
            Some(node) => node.next = other_head,
            _ => self.head = other_head,
        }
    }

    pub fn prepend(&mut self, mut other: List<T>) {
        if let Some(node) = other.last_node_mut() {
            node.next = self.head.take();
            self.head = other.head.take();
        }
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn cons(mut self) -> LCons<T> {
        match self.head.take() {
            Some(node) => {
                let tail = List { head: node.next };
                Cons::Cons(node.value, tail)
            }
            _ => Cons::Nil,
        }
    }

    pub fn iter(&self) -> Iter<'_, T> {
        Iter::from(self)
    }

    pub fn iter_mut(&mut self) -> IterMut<'_, T> {
        IterMut::from(self)
    }

    pub fn drain_filter<F: FnMut(&mut T) -> bool>(&mut self, pred: F) -> DrainFilter<'_, T, F> {
        DrainFilter::from(self, pred)
    }

    pub fn reverse(&mut self) {
        let head_node = self.head.as_deref();
        if matches!(head_node, Some(&Node { next: None, .. }) | None) {
            return;
        }

        let mut cur = self.head.take();
        while let Some(mut node) = cur {
            cur = std::mem::replace(&mut node.next, self.head.take());
            self.head.replace(node);
        }
    }

    pub fn remove(&mut self, at: usize) -> T {
        unsafe { self.remove_impl(at) }
    }

    unsafe fn remove_impl(&mut self, mut at: usize) -> T {
        let mut cur: *mut _ = &mut self.head;

        while let Some(node) = (*cur).as_deref_mut() {
            if at == 0 {
                break;
            }

            cur = &mut node.next;
            at -= 1;
        }

        let mut node = (*cur).take().expect("remove called past list bounds");
        *cur = node.next.take();

        node.value
    }

    fn pop_node(&mut self) -> Link<T> {
        self.head.take().map(|mut node| {
            self.head = node.next.take();
            node
        })
    }

    fn last_node_mut(&mut self) -> Option<&mut Node<T>> {
        let mut cur = self.head.as_deref_mut();

        while let Some(node) = cur.take() {
            if node.next.is_none() {
                return Some(node);
            }

            cur = node.next.as_deref_mut();
        }

        None
    }
}

impl<T: PartialEq> List<T> {
    pub fn contains(&self, x: &T) -> bool {
        self.iter().any(|el| el == x)
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
    pub fn from(list: &'a List<T>) -> Self {
        Iter {
            current_node: list.head.as_deref(),
        }
    }
}

impl<'a, T> IterMut<'a, T> {
    pub fn from(list: &'a mut List<T>) -> Self {
        IterMut {
            current_node: list.head.as_deref_mut(),
        }
    }
}

impl<'a, T, F: FnMut(&mut T) -> bool> DrainFilter<'a, T, F> {
    pub fn from(list: &'a mut List<T>, pred: F) -> Self {
        Self {
            owner: &mut list.head,
            pred,
        }
    }

    fn next_node(&mut self) -> Link<T> {
        while let Some(node) = unsafe { self.deref_trans() } {
            if (self.pred)(&mut node.value) {
                break;
            }

            self.owner = &mut node.next;
        }

        self.owner.take().map(|mut node| {
            *self.owner = node.next.take();
            node
        })
    }

    unsafe fn deref_trans<'b>(&mut self) -> Option<&'b mut Node<T>> {
        std::mem::transmute(self.owner.as_deref_mut())
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
        self.list.pop()
    }
}

impl<T, F: FnMut(&mut T) -> bool> Iterator for DrainFilter<'_, T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_node().map(|node| node.value)
    }
}

impl<T> FusedIterator for Iter<'_, T> {}
impl<T> FusedIterator for IterMut<'_, T> {}
impl<T> FusedIterator for IntoIter<T> {}
impl<T, F: FnMut(&mut T) -> bool> FusedIterator for DrainFilter<'_, T, F> {}
