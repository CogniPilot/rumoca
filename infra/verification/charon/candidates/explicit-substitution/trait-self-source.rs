pub fn increment_borrowed(value: Option<&mut u32>) -> Option<&mut u32> {
    let value = value?;
    *value += 1;
    Some(value)
}

#[test]
fn optional_mutable_borrow_preserves_none_and_updates_some() {
    assert_eq!(increment_borrowed(None), None);
    let mut value = 6;
    assert_eq!(increment_borrowed(Some(&mut value)), Some(&mut 7));
    assert_eq!(value, 7);
}
