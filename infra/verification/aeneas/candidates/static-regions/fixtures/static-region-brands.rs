use core::marker::PhantomData;

pub struct Branded<'a> {
    pub value: u32,
    pub brand: PhantomData<&'a mut &'a ()>,
}

pub struct StaticPhantom {
    pub value: u32,
    pub brand: PhantomData<&'static ()>,
}

pub struct MixedShared<'a> {
    pub value: &'a u32,
    pub brand: PhantomData<&'static ()>,
}

pub struct MixedMut<'a> {
    pub value: &'a mut u32,
    pub brand: PhantomData<&'static ()>,
}

pub fn branded_ordinary<'a>(value: Branded<'a>) -> u32 {
    value.value
}

pub fn phantom_only(value: StaticPhantom) -> u32 {
    value.value
}

pub fn mixed_shared<'a>(value: MixedShared<'a>) -> &'a u32 {
    value.value
}

pub fn mixed_mut<'a>(value: MixedMut<'a>) -> &'a mut u32 {
    value.value
}

pub fn mixed_mut_write(value: MixedMut<'_>, replacement: u32) {
    *value.value = replacement;
}

pub fn mixed_mut_identity<'a>(value: MixedMut<'a>) -> MixedMut<'a> {
    value
}

pub fn unit_return() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn phantom_fields_preserve_data() {
        assert_eq!(
            branded_ordinary(Branded {
                value: 11,
                brand: PhantomData
            }),
            11
        );
        assert_eq!(
            phantom_only(StaticPhantom {
                value: 29,
                brand: PhantomData
            }),
            29
        );
        unit_return();
    }

    #[test]
    fn phantom_fields_preserve_borrow_identity() {
        let value = 43;
        let result = mixed_shared(MixedShared {
            value: &value,
            brand: PhantomData,
        });
        assert!(core::ptr::eq(result, &value));
        let mut mutable = 71;
        let returned = mixed_mut_identity(MixedMut {
            value: &mut mutable,
            brand: PhantomData,
        });
        let result = mixed_mut(returned);
        *result = 89;
        assert_eq!(mutable, 89);
        mixed_mut_write(
            MixedMut {
                value: &mut mutable,
                brand: PhantomData,
            },
            97,
        );
        assert_eq!(mutable, 97);
    }
}
