use std::marker::PhantomData;

pub struct Id<'brand> {
    raw: u32,
    brand: PhantomData<&'brand mut &'brand ()>,
}

pub trait Compared<Rhs> {
    fn matches(&self, rhs: &Rhs) -> bool;

    fn differs(&self, rhs: &Rhs) -> bool {
        !self.matches(rhs)
    }
}

impl<'brand> Compared<u32> for Id<'brand> {
    fn matches(&self, rhs: &u32) -> bool {
        self.raw == *rhs
    }
}

impl<'brand> Compared<Id<'brand>> for u32 {
    fn matches(&self, rhs: &Id<'brand>) -> bool {
        *self == rhs.raw
    }
}

pub fn default_on_self(id: &Id<'_>, value: u32) -> bool {
    id.differs(&value)
}

pub fn default_on_rhs(value: u32, id: &Id<'_>) -> bool {
    value.differs(id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn default_method_preserves_both_argument_positions() {
        let id = Id {
            raw: 7,
            brand: PhantomData,
        };
        assert!(!default_on_self(&id, 7));
        assert!(default_on_self(&id, 8));
        assert!(!default_on_rhs(7, &id));
        assert!(default_on_rhs(8, &id));
    }
}
