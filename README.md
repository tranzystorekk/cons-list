# Linked list

## About

This is a linked list implementation in Rust with some bells and whistles made for training purposes:

* A `Cons` struct that resembles the `head | tail` consing/pattern matching in languages such as Lisp or Haskell
* A `linked_list!` macro for quickly creating a pre-filled list
* Iterators

Inspired by the [Too many linked lists](https://rust-unofficial.github.io/too-many-lists/) book.

## `unsafe` code

Internal implementation uses raw pointers for jumping around nodes
to work around Rust's strict lifetime constraints.
