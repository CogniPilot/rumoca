const FIRST: [&u32; 2] = [&11, &29];
const SECOND: [&u32; 2] = [&43, &71];

pub fn static_input(values: [&'static u32; 2], index: usize) -> &'static u32 {
    values[index]
}

pub fn ordinary_input<'a>(values: [&'a u32; 2], index: usize) -> &'a u32 {
    values[index]
}

pub fn returned_global() -> [&'static u32; 2] {
    FIRST
}

pub fn call_return(index: usize) -> &'static u32 {
    returned_global()[index]
}

pub fn branch_globals(first: bool, index: usize) -> &'static u32 {
    let values = if first { FIRST } else { SECOND };
    values[index]
}

pub fn loop_global(count: usize) -> &'static u32 {
    let mut value = FIRST[0];
    let mut iteration = 0;
    while iteration < count {
        value = SECOND[iteration % 2];
        iteration += 1;
    }
    value
}

pub fn loop_input<'a>(values: [&'a u32; 2], count: usize) -> &'a u32 {
    let mut value = values[0];
    let mut iteration = 0;
    while iteration < count {
        value = values[iteration % 2];
        iteration += 1;
    }
    value
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn input_call_and_branch_preserve_selection() {
        for index in 0..2 {
            assert_eq!(static_input(FIRST, index), FIRST[index]);
            assert_eq!(ordinary_input(FIRST, index), FIRST[index]);
            assert_eq!(call_return(index), FIRST[index]);
            assert_eq!(branch_globals(true, index), FIRST[index]);
            assert_eq!(branch_globals(false, index), SECOND[index]);
        }
    }

    #[test]
    fn loop_preserves_zero_and_repeated_iteration_values() {
        assert_eq!(*loop_global(0), 11);
        assert_eq!(*loop_input(FIRST, 0), 11);
        for count in 1..8 {
            assert_eq!(loop_global(count), SECOND[(count - 1) % 2]);
            assert_eq!(loop_input(FIRST, count), FIRST[(count - 1) % 2]);
        }
    }

    #[test]
    #[should_panic(expected = "index out of bounds")]
    fn returned_global_still_checks_bounds() {
        let _ = call_return(2);
    }
}
