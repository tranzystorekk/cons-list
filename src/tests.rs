use crate::Cons;
use crate::List;
use crate::{head_matches, linked_list};

#[test]
fn new_list_is_empty() {
    let l: List<()> = List::new();

    assert!(l.is_empty());
}

#[test]
fn list_head_can_be_peeked() {
    let l = linked_list![1, 2, 3];

    assert_eq!(l.head(), Some(&1));
}

#[test]
fn list_last_can_be_peeked() {
    let l = linked_list![1, 2, 3];

    assert_eq!(l.last(), Some(&3));
}

#[test]
fn list_head_can_be_mutably_peeked() {
    let mut l = linked_list![1, 2, 3];

    l.head_mut().into_iter().for_each(|v| *v += 5);

    assert_eq!(l.head(), Some(&6));
}

#[test]
fn list_last_can_be_mutably_peeked() {
    let mut l = linked_list![1, 2, 3];

    l.last_mut().into_iter().for_each(|v| *v += 5);

    assert_eq!(l.last(), Some(&8));
}

#[test]
fn list_can_be_appended_to() {
    let mut l = linked_list!(1, 2, 3);

    l.append(4);

    assert_eq!(l.pop(), Some(1));
    assert_eq!(l.pop(), Some(2));
    assert_eq!(l.pop(), Some(3));
    assert_eq!(l.pop(), Some(4));
    assert_eq!(l.pop(), None);
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
fn list_can_be_looped_over() {
    let l = linked_list![1, 2, 3];

    let mut x = [1, 2, 3].iter();

    for el in &l {
        assert_eq!(el, x.next().unwrap());
    }
}

#[test]
fn list_can_be_mutably_looped_over() {
    let mut l = linked_list![1, 2, 3];

    for el in &mut l {
        *el *= 2;
    }

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

#[test]
fn contains_finds_existing_elements() {
    let l = linked_list![1, 2, 3, 4];

    assert!(l.contains(&3));
}

#[test]
fn contains_fails_to_find_non_existing_elements() {
    let l = linked_list![1, 2, 3, 4];

    assert!(!l.contains(&10));
}

#[test]
fn reverse_works_on_regular_lists() {
    let mut l = linked_list![1, 2, 3, 4, 5];

    l.reverse();

    let expected = [5, 4, 3, 2, 1].iter();
    assert!(l.iter().eq(expected));
}

#[test]
fn reverse_works_on_one_element_lists() {
    let mut l = linked_list![1];

    l.reverse();

    let expexted = [1].iter();
    assert!(l.iter().eq(expexted));
}

#[test]
fn reverse_works_on_empty_lists() {
    let mut l: List<i32> = linked_list![];

    l.reverse();

    assert!(l.is_empty());
}
