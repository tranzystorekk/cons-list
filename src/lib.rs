//! A more straightforward, single-linked list implementation.
//!
//! This crate was heavily inspired
//! by the [Too many linked lists](https://rust-unofficial.github.io/too-many-lists/) book.
//! It's meant more as a reference with an API based on [`std::collections::LinkedList`]
//! than a fully performant solution.
//!
//! This crate also provides the [`Cons`] enum,
//! a concept known in some functional programming languages.

pub mod cons;
pub mod list;

pub use cons::Cons;
pub use list::List;

#[cfg(test)]
mod tests;
