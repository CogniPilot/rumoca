use std::marker::PhantomData;

pub struct BrandedView<'brand, 'data> {
    brand: PhantomData<&'brand mut &'brand ()>,
    value: &'data u32,
}

impl<'brand, 'data> BrandedView<'brand, 'data> {
    pub fn get(&self) -> &'data u32 {
        self.value
    }
}

pub fn phantom_capture<'brand, 'data>(
    brand: PhantomData<&'brand mut &'brand ()>,
    value: &'data u32,
) -> impl Fn() -> &'data u32 + use<'brand, 'data> {
    let view = BrandedView { brand, value };
    move || view.get()
}

pub fn mixed_bound_free<'data>(
    value: &'data u32,
) -> impl for<'arg> Fn(&'arg u32) -> (&'arg u32, &'data u32) {
    move |argument| (argument, value)
}

pub fn consume_once<'data>(value: &'data u32) -> impl FnOnce() -> &'data u32 {
    let owned = Box::new(value);
    move || *owned
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn phantom_field_has_no_runtime_storage() {
        assert_eq!(
            std::mem::size_of::<BrandedView<'_, '_>>(),
            std::mem::size_of::<&u32>()
        );
        let value = 7;
        let get = phantom_capture(PhantomData, &value);
        assert!(std::ptr::eq(get(), &value));
    }

    #[test]
    fn bound_argument_and_free_capture_remain_distinct() {
        let value = 7;
        let get = mixed_bound_free(&value);
        {
            let argument = 42;
            let (first, second) = get(&argument);
            assert!(std::ptr::eq(first, &argument));
            assert!(std::ptr::eq(second, &value));
        }
        let argument = 13;
        assert!(std::ptr::eq(get(&argument).0, &argument));
        assert!(std::ptr::eq(consume_once(&value)(), &value));
    }
}
