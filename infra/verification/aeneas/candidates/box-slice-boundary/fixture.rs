pub fn box_values<T: Clone>(values: &[T]) -> Box<[T]> {
    Box::from(values)
}

pub fn into_values<T: Clone>(values: &[T]) -> Box<[T]> {
    values.into()
}

pub fn u32_values(values: &[u32]) -> Box<[u32]> {
    into_values(values)
}

pub struct Flip(pub bool);

impl Clone for Flip {
    fn clone(&self) -> Self {
        Self(!self.0)
    }
}

pub fn flip_values(values: &[Flip]) -> Box<[Flip]> {
    box_values(values)
}

#[cfg(test)]
mod tests {
    use super::{Flip, box_values, flip_values, into_values, u32_values};

    #[test]
    fn dimensions_preserve_order_and_input() {
        let values = [7, 0, u32::MAX, 42];
        assert_eq!(&*u32_values(&values), &values);
        assert_eq!(&*box_values(&values), &values);
        assert_eq!(values, [7, 0, u32::MAX, 42]);
    }

    #[test]
    fn cloning_is_not_unconditional_copying() {
        let values = [Flip(true), Flip(false), Flip(false)];
        let cloned = flip_values(&values);
        assert_eq!(cloned.len(), 3);
        assert!(!cloned[0].0);
        assert!(cloned[1].0);
        assert!(cloned[2].0);
        assert!(values[0].0);
        assert!(!values[1].0);
    }

    #[test]
    fn empty_and_zero_sized_slices() {
        assert!(box_values::<u32>(&[]).is_empty());
        assert!(into_values::<u32>(&[]).is_empty());
        assert_eq!(box_values(&[(); 5]).len(), 5);
    }

    #[test]
    fn failing_clone_is_not_dropped() {
        struct Refuses;
        impl Clone for Refuses {
            fn clone(&self) -> Self {
                panic!("clone refused")
            }
        }
        assert!(box_values::<Refuses>(&[]).is_empty());
        assert!(std::panic::catch_unwind(|| box_values(&[Refuses])).is_err());
    }
}
