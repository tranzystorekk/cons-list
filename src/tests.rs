use crate::{linked_list, head_matches};
use crate::Cons;
use crate::List;

#[test]
fn new_list_is_empty() {
    let l: List<()> = List::new();

    assert!(l.is_empty());
}

#[test]
fn list_head_can_be_peeked() {
    let l = linked_list![1, 2, 3];

    assert_eq!(l.peek(), Some(&1));
}

#[test]
fn list_last_can_be_peeked() {
    let l = linked_list![1, 2, 3];

    assert_eq!(l.peek_last(), Some(&3));
}

#[test]
fn list_head_can_be_mutably_peeked() {
    let mut l = linked_list![1, 2, 3];

    l.peek_mut().into_iter().for_each(|v| *v += 5);

    assert_eq!(l.peek(), Some(&6));
}

#[test]
fn list_last_can_be_mutably_peeked() {
    let mut l = linked_list![1, 2, 3];

    l.peek_last_mut().into_iter().for_each(|v| *v += 5);

    assert_eq!(l.peek_last(), Some(&8));
}

#[test]
fn head_and_tail_can_be_consed_together() {
    let l = linked_list![3, 2, 1];

    let consed = List::from_cons(4, l);

    let collected: Vec<i32> = consed.into_iter().collect();

    assert_eq!(collected, vec![4, 3, 2, 1]);
}

#[test]
fn empty_list_is_zero_len() {
    let l: List<()> = List::new();

    assert_eq!(l.len(), 0);
}

#[test]
fn list_len_works_correctly() {
    let l = linked_list![1, 2, 3];

    assert_eq!(l.len(), 3);
}

#[test]
fn items_are_popped_in_lifo_order() {
    let mut l = linked_list![1, 2, 3];

    assert_eq!(l.pop(), Some(1));
    assert_eq!(l.pop(), Some(2));
    assert_eq!(l.pop(), Some(3));
    assert_eq!(l.pop(), None);
}

#[test]
fn list_can_be_consed() {
    let l = linked_list![1];

    assert!(matches!(l.cons(), Cons::Cons(1, _)));
}

#[test]
fn tail_can_be_consed() {
    let l = linked_list![1, 2, 3];

    let tail = l
        .cons()
        .tail()
        .map(|list| list.into_iter().collect::<Vec<i32>>());

    assert_eq!(tail, Some(vec![2, 3]));
}

#[test]
fn no_tail_when_consing_one_element_list() {
    let l = linked_list![1];

    assert!(matches!(l.cons().tail(), None));
}

#[test]
fn list_can_be_iterated() {
    let l = linked_list![1, 2, 3];

    let collected: Vec<i32> = l.iter().copied().collect();

    assert_eq!(collected, vec![1, 2, 3]);
}

#[test]
fn list_can_be_mutably_iterated() {
    let mut l = linked_list![1, 2, 3];

    l.iter_mut().for_each(|el| *el *= 2);

    let collected: Vec<i32> = l.iter().copied().collect();

    assert_eq!(collected, vec![2, 4, 6]);
}

#[test]
fn list_can_be_transformed_into_iterator() {
    let l = linked_list![1, 2, 3];

    let collected: Vec<i32> = l.into_iter().collect();

    assert_eq!(collected, vec![1, 2, 3]);
}

#[test]
fn cons_head_matches() {
    let l = linked_list![Some(1), None, Some(3)];

    assert!(head_matches!(l.cons(), Some(1)));
}
