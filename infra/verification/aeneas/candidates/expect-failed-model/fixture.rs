use std::marker::PhantomData;

pub struct VariableId<'a> {
    value: u32,
    brand: PhantomData<&'a mut &'a ()>,
}

pub struct VariableView<'a> {
    value: &'a u32,
    brand: PhantomData<&'a mut &'a ()>,
}

pub fn expect_id(value: Option<VariableId<'_>>) -> VariableId<'_> {
    value.expect("required id")
}

pub fn expect_view(value: Option<VariableView<'_>>) -> VariableView<'_> {
    value.expect("required view")
}

pub fn id_value(value: VariableId<'_>) -> u32 {
    value.value
}

pub fn view_value(value: VariableView<'_>) -> u32 {
    *value.value
}

pub fn expect_plain(value: Option<u32>) -> u32 {
    value.expect("required value")
}

pub fn propagate(value: Result<u32, u32>) -> Result<u32, u32> {
    let output = value?;
    Ok(output + 1)
}

#[cfg(test)]
mod tests {
    use super::{
        VariableId, VariableView, expect_id, expect_plain, expect_view, id_value, propagate,
        view_value,
    };
    use std::marker::PhantomData;

    #[test]
    fn preserves_present_values_and_try_dispositions() {
        let value = 17;
        assert_eq!(expect_plain(Some(value)), value);
        assert_eq!(
            id_value(expect_id(Some(VariableId {
                value,
                brand: PhantomData,
            }))),
            value
        );
        assert_eq!(
            view_value(expect_view(Some(VariableView {
                value: &value,
                brand: PhantomData,
            }))),
            value
        );
        assert_eq!(propagate(Ok(17)), Ok(18));
        assert_eq!(propagate(Err(23)), Err(23));
    }

    #[test]
    #[should_panic(expected = "required id")]
    fn missing_id_panics() {
        expect_id(None);
    }

    #[test]
    #[should_panic(expected = "required view")]
    fn missing_view_panics() {
        expect_view(None);
    }

    #[test]
    #[should_panic(expected = "required value")]
    fn plain_absence_panics() {
        expect_plain(None);
    }
}
