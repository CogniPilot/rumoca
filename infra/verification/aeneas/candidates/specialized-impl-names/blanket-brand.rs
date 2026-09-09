#[path = "branded-trait.rs"]
pub mod branded_trait;

pub trait Keep<Rhs> {
    fn keep(self, rhs: &Rhs) -> Self;
}

impl<T, Rhs> Keep<Rhs> for T {
    fn keep(self, _rhs: &Rhs) -> Self {
        self
    }
}

pub fn keep_with_brand(id: &branded_trait::Id<'_>, value: u32) -> u32 {
    value.keep(id)
}

#[cfg(test)]
mod tests {
    use super::Keep;

    #[test]
    fn blanket_keeps_the_receiver_not_the_argument() {
        assert_eq!(7u32.keep(&8u32), 7);
        assert_eq!(8u32.keep(&7u32), 8);
    }
}
