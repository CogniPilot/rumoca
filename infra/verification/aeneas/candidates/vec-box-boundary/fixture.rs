pub fn move_values<T>(values: Vec<T>) -> Box<[T]> {
    values.into_boxed_slice()
}

pub fn from_values<T>(values: Vec<T>) -> Box<[T]> {
    Box::from(values)
}

#[cfg(test)]
mod tests {
    use super::{from_values, move_values};

    #[test]
    fn ordered_values_and_spare_capacity() {
        let mut values = Vec::with_capacity(16);
        values.extend([7, 0, 42]);
        assert!(values.capacity() > values.len());
        assert_eq!(&*move_values(values), &[7, 0, 42]);
        assert_eq!(&*from_values(vec![7, 0, 42]), &[7, 0, 42]);
    }

    #[test]
    fn empty_and_zero_sized_elements() {
        assert!(move_values::<u32>(Vec::new()).is_empty());
        assert!(from_values::<u32>(Vec::new()).is_empty());
        assert_eq!(move_values(vec![(); 4]).len(), 4);
        assert_eq!(from_values(vec![(); 4]).len(), 4);
    }

    #[test]
    fn move_only_elements_need_no_clone() {
        #[derive(Debug, PartialEq)]
        struct MoveOnly(u32);
        assert_eq!(
            &*move_values(vec![MoveOnly(8), MoveOnly(3)]),
            &[MoveOnly(8), MoveOnly(3)]
        );
        assert_eq!(
            &*from_values(vec![MoveOnly(8), MoveOnly(3)]),
            &[MoveOnly(8), MoveOnly(3)]
        );
    }
}
