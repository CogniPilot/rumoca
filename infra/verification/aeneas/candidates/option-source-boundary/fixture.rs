pub fn copy_value<T: Copy>(value: Option<&T>) -> Option<T> {
    value.copied()
}

pub fn flatten_value<T>(value: Option<Option<T>>) -> Option<T> {
    value.flatten()
}

pub fn copy_flatten<T: Copy>(value: Option<&Option<T>>) -> Option<T> {
    value.copied().flatten()
}

#[cfg(test)]
mod tests {
    use super::{copy_flatten, copy_value, flatten_value};

    #[test]
    fn copy_preserves_payload_and_absence() {
        let value = (17_u32, 23_u32);
        assert_eq!(copy_value(Some(&value)), Some(value));
        assert_eq!(copy_value::<(u32, u32)>(None), None);
    }

    #[test]
    fn flatten_distinguishes_all_three_cases() {
        let value = (17_u32, 23_u32);
        assert_eq!(flatten_value(Some(Some(value))), Some(value));
        assert_eq!(flatten_value::<(u32, u32)>(Some(None)), None);
        assert_eq!(flatten_value::<(u32, u32)>(None), None);
    }

    #[test]
    fn copy_flatten_preserves_payload_and_absence() {
        let value = Some((17_u32, 23_u32));
        let missing: Option<(u32, u32)> = None;
        assert_eq!(copy_flatten(Some(&value)), value);
        assert_eq!(copy_flatten(Some(&missing)), None);
        assert_eq!(copy_flatten::<(u32, u32)>(None), None);
    }

    #[test]
    fn false_is_a_payload_not_absence() {
        assert_eq!(copy_value(Some(&false)), Some(false));
        assert_eq!(flatten_value(Some(Some(false))), Some(false));
        assert_eq!(copy_flatten(Some(&Some(false))), Some(false));
    }
}
