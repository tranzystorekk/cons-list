use crate::list::cons::Cons;

use std::iter::FusedIterator;

struct Node<T> {
    value: T,
    next: Link<T>,
}

type Link<T> = Option<Box<Node<T>>>;

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

impl<T> List<T> {
    pub fn new() -> Self {
        List { head: None }
    }

    pub fn from_cons(head: T, mut tail: List<T>) -> Self {
        let head = Node {
            value: head,
            next: tail.head.take(),
        };

        List { head: Some(Box::new(head)) }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self.head, None)
    }

    pub fn push(&mut self, elem: T) {
        let new_node = Node {
            value: elem,
            next: self.head.take(),
        };

        self.head = Some(Box::new(new_node));
    }

    pub fn pop(&mut self) -> Option<T> {
        self.pop_node().map(|node| node.value)
    }

    pub fn len(&self) -> usize {
        self.iter().count()
    }

    pub fn head(&self) -> Option<&T> {
        self.head.as_ref().map(|node| &node.value)
    }

    pub fn cons(mut self) -> Cons<T> {
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

    fn pop_node(&mut self) -> Link<T> {
        self.head.take().map(|mut node| {
            self.head = node.next.take();
            node
        })
    }
}

impl<T> Drop for List<T> {
    fn drop(&mut self) {
        while let Some(_) = self.pop_node() {}
    }
}

impl<T> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter { list: self }
    }
}

impl<'a, T> Iter<'a, T> {
    pub fn from(list: &'a List<T>) -> Self {
        Iter {
            current_node: list.head.as_ref().map(|node| &**node),
        }
    }
}

impl<'a, T> IterMut<'a, T> {
    pub fn from(list: &'a mut List<T>) -> Self {
        IterMut {
            current_node: list.head.as_mut().map(|node| &mut **node),
        }
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.current_node.map(|node| {
            self.current_node = node.next.as_ref().map(|node| &**node);

            &node.value
        })
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    fn next(&mut self) -> Option<Self::Item> {
        self.current_node.take().map(|node| {
            self.current_node = node.next.as_mut().map(|node| &mut **node);

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

impl<'a, T> FusedIterator for Iter<'a, T> {}
impl<T> FusedIterator for IntoIter<T> {}
