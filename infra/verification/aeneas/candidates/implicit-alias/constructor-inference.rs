pub struct Tag<const N: usize> {
    pub value: u32,
}

pub fn tag_value<const N: usize>(_tag: Tag<N>) -> usize {
    N
}

pub fn read_tag<const N: usize>(value: u32) -> usize {
    tag_value(Tag::<N> { value })
}

#[cfg(test)]
mod tests {
    #[test]
    fn nominal_constructor_carries_its_const_parameter() {
        assert_eq!(super::read_tag::<7>(123), 7);
        assert_eq!(super::read_tag::<8>(123), 8);
    }
}
