use crate::list::core::List;

pub enum Cons<T> {
    Cons(T, List<T>),
    Nil,
}

impl<T> Cons<T> {
    pub fn is_nil(&self) -> bool {
        matches!(self, Cons::Nil)
    }

    pub fn head(self) -> Option<T> {
        match self {
            Cons::Cons(head, _) => Some(head),
            _ => None,
        }
    }

    pub fn tail(self) -> Option<List<T>> {
        match self {
            Cons::Cons(_, tail) if !tail.is_empty() => Some(tail),
            _ => None,
        }
    }
}
