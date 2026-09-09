use std::marker::PhantomData;

pub struct Id<'brand> {
    raw: u32,
    brand: PhantomData<&'brand mut &'brand ()>,
}

pub trait Matches<Rhs> {
    fn matches(&self, rhs: &Rhs) -> bool;
}

impl<'brand> Matches<Id<'brand>> for u32 {
    fn matches(&self, rhs: &Id<'brand>) -> bool {
        *self == rhs.raw
    }
}

impl<'brand> Matches<u32> for Id<'brand> {
    fn matches(&self, rhs: &u32) -> bool {
        self.raw == *rhs
    }
}

pub fn branded_rhs(value: u32, id: &Id<'_>) -> bool {
    value.matches(id)
}

pub fn branded_self(id: &Id<'_>, value: u32) -> bool {
    id.matches(&value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn both_trait_argument_positions_preserve_the_compared_value() {
        let id = Id {
            raw: 7,
            brand: PhantomData,
        };
        assert!(branded_rhs(7, &id));
        assert!(!branded_rhs(8, &id));
        assert!(branded_self(&id, 7));
        assert!(!branded_self(&id, 8));
        assert_eq!(std::mem::size_of::<Id<'_>>(), std::mem::size_of::<u32>());
    }
}
