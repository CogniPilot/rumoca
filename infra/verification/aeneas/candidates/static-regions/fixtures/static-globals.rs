const NUMS: [u32; 2] = [11, 29];
const NUMBER: &u32 = &11;
const REFS: [&u32; 2] = [&11, &29];
const NAME: &str = "functions";
const NAMES: [&str; 2] = ["functions", "variables"];

pub fn copy_nums() -> [u32; 2] {
    NUMS
}

pub fn copy_number() -> &'static u32 {
    NUMBER
}

pub fn copy_refs() -> [&'static u32; 2] {
    REFS
}

pub fn copy_name() -> &'static str {
    NAME
}

pub fn copy_names() -> [&'static str; 2] {
    NAMES
}

pub fn select_num(index: usize) -> u32 {
    NUMS[index]
}

pub fn select_ref(index: usize) -> u32 {
    *REFS[index]
}

pub fn select_ref_borrow(index: usize) -> &'static u32 {
    REFS[index]
}

pub fn select_name(index: usize) -> &'static str {
    NAMES[index]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn copies_preserve_all_values() {
        assert_eq!(copy_nums(), [11, 29]);
        assert_eq!(*copy_number(), 11);
        assert_eq!(copy_refs().map(|value| *value), [11, 29]);
        assert_eq!(copy_name(), "functions");
        assert_eq!(copy_names(), ["functions", "variables"]);
    }

    #[test]
    fn indexed_reads_preserve_each_value() {
        for (index, expected) in [11, 29].into_iter().enumerate() {
            assert_eq!(select_num(index), expected);
            assert_eq!(select_ref(index), expected);
            assert_eq!(*select_ref_borrow(index), expected);
        }
        assert_eq!(select_name(0), "functions");
        assert_eq!(select_name(1), "variables");
    }

    #[test]
    #[should_panic]
    fn numeric_array_index_refuses_out_of_bounds() {
        let _value = select_num(2);
    }

    #[test]
    #[should_panic]
    fn reference_array_index_refuses_out_of_bounds() {
        let _value = select_ref(2);
    }

    #[test]
    #[should_panic]
    fn string_array_index_refuses_out_of_bounds() {
        let _value = select_name(2);
    }
}
