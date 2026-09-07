extern crate two_region_iterator;

pub fn first(x: &u8, y: &u8) -> u8 {
    two_region_iterator::consume_two_regions(x, y)
}

pub fn second(x: &u8, y: &u8) -> u8 {
    let mut iterator = two_region_iterator::two_regions_capture_second(x, y);
    match iterator.next() {
        Some(value) => value,
        None => 0,
    }
}

pub fn reference_item(xs: &[u8], y: &u8) -> u8 {
    two_region_iterator::consume_ref_item(xs, y)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dependency_captures_select_the_correct_input() {
        let x = 7;
        let y = 42;
        assert_eq!(first(&x, &y), x);
        assert_eq!(second(&x, &y), y);
        assert_eq!(reference_item(&[13, 19], &y), 13);
        assert_eq!(reference_item(&[], &y), 0);
    }
}
