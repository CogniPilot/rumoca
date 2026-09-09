use std::marker::PhantomData;

pub struct Tag<const N: usize>;
pub struct WrappedTag<const N: usize>(pub Tag<N>);
pub struct PairTag<const N: usize>(pub Tag<N>, pub u32);
pub struct NominalTag<const N: usize> {
    pub value: u32,
}
pub struct WrappedValue<T>(pub T);

pub fn tag_value<const N: usize>(_tag: Tag<N>) -> usize {
    N
}

pub fn wrapped_value<const N: usize>(_tag: WrappedTag<N>) -> usize {
    N
}

pub fn pair_value<const N: usize>(_tag: PairTag<N>) -> usize {
    N
}

pub fn nominal_value<const N: usize>(_tag: NominalTag<N>) -> usize {
    N
}

pub fn read_tag<const N: usize>() -> usize {
    tag_value(Tag::<N>)
}

pub fn read_wrapped<const N: usize>() -> usize {
    wrapped_value(WrappedTag(Tag::<N>))
}

pub fn read_pair<const N: usize>(value: u32) -> usize {
    pair_value(PairTag(Tag::<N>, value))
}

pub fn read_nominal<const N: usize>(tag: NominalTag<N>) -> usize {
    nominal_value(tag)
}

pub fn phantom_value<T>(_tag: PhantomData<T>, value: u32) -> u32 {
    value
}

pub fn branded_value<'brand>(brand: PhantomData<&'brand mut &'brand ()>, value: u32) -> u32 {
    phantom_value(brand, value)
}

pub fn identity<T>(value: T) -> T {
    value
}

pub fn unwrap_value<T>(value: WrappedValue<T>) -> T {
    value.0
}

pub fn array_length<const N: usize>(_values: [u32; N]) -> usize {
    N
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn erased_marker_parameters_still_select_the_result() {
        assert_eq!(read_tag::<7>(), 7);
        assert_eq!(read_tag::<8>(), 8);
        assert_eq!(read_wrapped::<7>(), 7);
        assert_eq!(read_pair::<8>(123), 8);
        assert_eq!(read_nominal(NominalTag::<7> { value: 123 }), 7);
        assert_eq!(branded_value(PhantomData, 123), 123);
        assert_eq!(identity(123_u32), 123);
        assert_eq!(unwrap_value(WrappedValue(123_u32)), 123);
        assert_eq!(array_length([1, 2, 3]), 3);
        assert_eq!(std::mem::size_of::<Tag<7>>(), 0);
        assert_eq!(std::mem::size_of::<WrappedTag<7>>(), 0);
        assert_eq!(std::mem::size_of::<PairTag<7>>(), 4);
    }
}
