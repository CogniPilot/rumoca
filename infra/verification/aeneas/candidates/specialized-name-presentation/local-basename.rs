use std::marker::PhantomData;

pub struct Id<'brand> {
    raw: u32,
    brand: PhantomData<&'brand mut &'brand ()>,
}

pub struct Wrap<T>(pub T);

pub fn wrap<'brand>(raw: u32) -> Wrap<Id<'brand>> {
    Wrap(Id {
        raw,
        brand: PhantomData,
    })
}

pub fn read(raw: u32) -> u32 {
    wrap(raw).0.raw
}

#[cfg(test)]
mod tests {
    #[test]
    fn unnamed_specialized_temporary_preserves_its_payload() {
        assert_eq!(super::read(0), 0);
        assert_eq!(super::read(7), 7);
        assert_eq!(super::read(u32::MAX), u32::MAX);
    }
}
