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
fn list_can_be_pushed_back_to() {
    let mut l = linked_list!(1, 2, 3);

    l.push_back(4);

    let expected = [1, 2, 3, 4];
    itertools::assert_equal(&expected, &l);
}

#[test]
fn head_and_tail_can_be_consed_together() {
    let l = linked_list![3, 2, 1];

    let consed = List::from_cons(4, l);

    let expected = [4, 3, 2, 1];
    itertools::assert_equal(&expected, &consed);
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

    let expected = [1, 2, 3];
    itertools::assert_equal(&expected, &l);
}

#[test]
fn list_can_be_mutably_iterated() {
    let mut l = linked_list![1, 2, 3];

    l.iter_mut().for_each(|el| *el *= 2);

    let expected = [2, 4, 6];
    itertools::assert_equal(&expected, &l);
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

    let expected = [2, 4, 6];
    itertools::assert_equal(&expected, &l);
}

#[test]
fn list_can_be_transformed_into_iterator() {
    let l = linked_list![1, 2, 3];

    let expected = [1, 2, 3].iter().copied();
    itertools::assert_equal(expected, l);
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

    let expected = [5, 4, 3, 2, 1];
    itertools::assert_equal(&expected, &l);
}

#[test]
fn reverse_works_on_one_element_lists() {
    let mut l = linked_list![1];

    l.reverse();

    let expected = [1];
    itertools::assert_equal(&expected, &l);
}

#[test]
fn reverse_works_on_empty_lists() {
    let mut l: List<i32> = linked_list![];

    l.reverse();

    assert!(l.is_empty());
}

#[test]
fn list_can_be_appended_to_another() {
    let mut first = linked_list![1, 2, 3, 4];
    let second = linked_list![5, 6, 7, 8, 9, 10];

    first.append(second);

    let expected = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    itertools::assert_equal(&expected, &first);
}

#[test]
fn list_can_be_prepended_to_another() {
    let mut first = linked_list![1, 2, 3, 4, 5];
    let second = linked_list![7, 8, 9];

    first.prepend(second);

    let expected = [7, 8, 9, 1, 2, 3, 4, 5];
    itertools::assert_equal(&expected, &first);
}

#[test]
fn drain_filter_works() {
    let mut l = linked_list![1, 2, 3, 4, 5, 6];

    let drain_odd = l.drain_filter(|&mut n| n % 2 != 0);
    let expected_drained = [1, 3, 5].iter().copied();
    itertools::assert_equal(expected_drained, drain_odd);

    let expected_remaining = [2, 4, 6];
    itertools::assert_equal(&expected_remaining, &l);
}

#[test]
fn drain_filter_works_when_empty() {
    let mut l = linked_list![1, 2, 3, 4, 5, 6];

    {
        let mut drain_greater_than_ten = l.drain_filter(|&mut n| n > 10);
        let next_elem = drain_greater_than_ten.next();
        assert!(
            next_elem.is_none(),
            "DrainFilter(x > 10) is not an empty iterator: next() returned {:?}",
            next_elem
        );
    }

    let expected = [1, 2, 3, 4, 5, 6];
    itertools::assert_equal(&expected, &l);
}

#[test]
fn drain_filter_drains_even_if_not_consumed() {
    let mut l = linked_list!["aaa", "dEf", "ghi", "XYZ"];

    let _ = l.drain_filter(|s| s.chars().all(char::is_lowercase));

    let expected = ["dEf", "XYZ"];
    itertools::assert_equal(&expected, &l);
}
