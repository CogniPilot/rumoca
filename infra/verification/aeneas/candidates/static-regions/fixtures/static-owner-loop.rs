const TABLE: [&u32; 2] = [&11, &29];

pub fn selected(index: usize) -> &'static u32 {
    TABLE[index]
}

pub fn ordinary_selected(value: &u32) -> &u32 {
    value
}

pub fn accumulate_static(count: u32, index: usize) -> u32 {
    let mut total = 0u32;
    let mut iteration = 0;
    while iteration < count {
        if iteration % 2 == 0 {
            total = total.wrapping_add(*selected(index));
        }
        iteration += 1;
    }
    total
}

pub fn accumulate_ordinary(count: u32, value: &u32) -> u32 {
    let mut total = 0u32;
    let mut iteration = 0;
    while iteration < count {
        if iteration % 2 == 0 {
            total = total.wrapping_add(*ordinary_selected(value));
        }
        iteration += 1;
    }
    total
}

pub fn retain_static(count: u32, index: usize) -> &'static u32 {
    let mut chosen = selected(0);
    let mut iteration = 0;
    while iteration < count {
        chosen = selected(index);
        iteration += 1;
    }
    chosen
}

pub fn retain_ordinary<'a>(count: u32, first: &'a u32, other: &'a u32) -> &'a u32 {
    let mut chosen = ordinary_selected(first);
    let mut iteration = 0;
    while iteration < count {
        chosen = ordinary_selected(other);
        iteration += 1;
    }
    chosen
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn zero_one_and_repeated_issuance_preserve_selected_data() {
        for count in 0u32..8 {
            for (index, value) in TABLE.into_iter().enumerate() {
                let expected = count.div_ceil(2) * value;
                assert_eq!(accumulate_static(count, index), expected);
                assert_eq!(accumulate_ordinary(count, value), expected);
            }
        }
    }

    #[test]
    fn untaken_branch_does_not_evaluate_invalid_index() {
        assert_eq!(accumulate_static(0, 2), 0);
    }

    #[test]
    fn live_return_reference_keeps_its_selected_referent() {
        for count in 0..4 {
            let expected = if count == 0 { TABLE[0] } else { TABLE[1] };
            assert!(std::ptr::eq(retain_static(count, 1), expected));
            assert!(std::ptr::eq(
                retain_ordinary(count, TABLE[0], TABLE[1]),
                expected
            ));
        }
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn taken_branch_preserves_index_failure() {
        let _ = accumulate_static(1, 2);
    }
}
