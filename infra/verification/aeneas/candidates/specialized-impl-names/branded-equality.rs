use std::marker::PhantomData;

#[derive(PartialEq, Eq)]
pub struct Id<'brand> {
    raw: u32,
    brand: PhantomData<&'brand mut &'brand ()>,
}

pub fn same<'brand>(left: &Id<'brand>, right: &Id<'brand>) -> bool {
    left == right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn equality_compares_raw_values_and_brand_has_no_runtime_storage() {
        let first = Id {
            raw: 7,
            brand: PhantomData,
        };
        let equal = Id {
            raw: 7,
            brand: PhantomData,
        };
        let different = Id {
            raw: 8,
            brand: PhantomData,
        };
        assert!(same(&first, &equal));
        assert!(!same(&first, &different));
        assert_eq!(std::mem::size_of::<Id<'_>>(), std::mem::size_of::<u32>());
    }
}
