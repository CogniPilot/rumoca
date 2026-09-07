use core::marker::PhantomData;

pub struct Inner<'a> {
    pub value: &'a u32,
}

pub struct Outer<'a> {
    pub inner: Inner<'a>,
}

pub struct Split<'a, 'brand> {
    pub inner: Outer<'a>,
    pub brand: PhantomData<&'brand ()>,
}

pub struct Closed {
    pub value: &'static u32,
}

pub struct ClosedOuter {
    pub inner: Closed,
}

pub fn closed_static(value: ClosedOuter) -> &'static u32 {
    value.inner.value
}

pub fn generic_static(value: Outer<'static>) -> &'static u32 {
    value.inner.value
}

pub fn generic_phantom_static<'a>(value: Split<'a, 'static>) -> &'a u32 {
    value.inner.inner.value
}

pub fn generic_stored_static<'brand>(value: Split<'static, 'brand>) -> &'static u32 {
    value.inner.inner.value
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_lifetime_substitution_preserves_referents() {
        static STATIC_VALUE: u32 = 11;
        let local_value = 29;
        assert!(core::ptr::eq(
            closed_static(ClosedOuter {
                inner: Closed {
                    value: &STATIC_VALUE
                }
            }),
            &STATIC_VALUE,
        ));
        assert!(core::ptr::eq(
            generic_static(Outer {
                inner: Inner {
                    value: &STATIC_VALUE
                }
            }),
            &STATIC_VALUE,
        ));
        assert!(core::ptr::eq(
            generic_phantom_static(Split {
                inner: Outer {
                    inner: Inner {
                        value: &local_value
                    }
                },
                brand: PhantomData,
            }),
            &local_value,
        ));
        assert!(core::ptr::eq(
            generic_stored_static(Split {
                inner: Outer {
                    inner: Inner {
                        value: &STATIC_VALUE
                    }
                },
                brand: PhantomData,
            }),
            &STATIC_VALUE,
        ));
    }
}
