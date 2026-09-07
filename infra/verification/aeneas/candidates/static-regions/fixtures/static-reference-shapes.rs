#[derive(Clone, Copy, Debug, PartialEq)]
pub struct RefPair {
    pub first: &'static u32,
    pub second: &'static u32,
}

const TUPLE: (&u32, &u32) = (&11, &29);
const PAIR: RefPair = RefPair { first: &11, second: &29 };
const EMPTY: [&u32; 0] = [];

pub fn copy_tuple() -> (&'static u32, &'static u32) {
    TUPLE
}

pub fn copy_pair() -> RefPair {
    PAIR
}

pub fn select_tuple() -> &'static u32 {
    TUPLE.1
}

pub fn select_pair() -> &'static u32 {
    PAIR.second
}

pub fn copy_empty() -> [&'static u32; 0] {
    EMPTY
}

pub fn copy_input_refs(items: [&u32; 2]) -> ([&u32; 2], [&u32; 2]) {
    (items, items)
}

pub fn select_owned_ref(items: [&u32; 2], index: usize) -> &u32 {
    items[index]
}

pub fn select_input_ref<'a>(items: &[&'a u32; 2], index: usize) -> &'a u32 {
    items[index]
}

pub fn select_local_ref<'a>(first: &'a u32, second: &'a u32, index: usize) -> &'a u32 {
    let items = [first, second];
    items[index]
}

pub fn select_local_value(first: u32, second: u32, index: usize) -> u32 {
    let items = [&first, &second];
    *items[index]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn globals_preserve_fields() {
        assert_eq!(copy_tuple(), (&11, &29));
        assert_eq!(copy_pair(), RefPair { first: &11, second: &29 });
        assert_eq!(*select_tuple(), 29);
        assert_eq!(*select_pair(), 29);
        assert!(copy_empty().is_empty());
    }

    #[test]
    fn copies_preserve_both_arrays() {
        let items = [&7, &13];
        assert_eq!(copy_input_refs(items), (items, items));
    }

    #[test]
    fn indices_preserve_each_value() {
        let first = 7;
        let second = 13;
        let items = [&first, &second];
        for index in 0..2 {
            assert_eq!(select_owned_ref(items, index), items[index]);
            assert_eq!(select_input_ref(&items, index), items[index]);
            assert_eq!(select_local_ref(&first, &second, index), items[index]);
            assert_eq!(select_local_value(first, second, index), *items[index]);
        }
    }

    #[test]
    #[should_panic]
    fn owned_index_refuses_out_of_bounds() {
        let _value = select_owned_ref([&7, &13], 2);
    }

    #[test]
    #[should_panic]
    fn borrowed_index_refuses_out_of_bounds() {
        let _value = select_input_ref(&[&7, &13], 2);
    }

    #[test]
    #[should_panic]
    fn local_ref_index_refuses_out_of_bounds() {
        let _value = select_local_ref(&7, &13, 2);
    }

    #[test]
    #[should_panic]
    fn local_value_index_refuses_out_of_bounds() {
        let _value = select_local_value(7, 13, 2);
    }
}
