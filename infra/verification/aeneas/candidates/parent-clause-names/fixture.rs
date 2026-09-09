#![feature(register_tool)]
#![register_tool(charon)]

pub trait Value {
    fn value(&self) -> u32;
}

impl Value for u32 {
    fn value(&self) -> u32 {
        *self
    }
}

impl Value for bool {
    fn value(&self) -> u32 {
        if *self { 1 } else { 0 }
    }
}

pub trait Pair {
    type Left: Value;
    type Right: Value;
    fn left(&self) -> &Self::Left;
    fn right(&self) -> &Self::Right;
    #[charon::rename("ValueInst")]
    fn reserved_first(&self) -> u32;
    #[charon::rename("ValueInst1")]
    fn reserved_second(&self) -> u32;
}

pub struct Values {
    pub left: u32,
    pub right: bool,
}

impl Pair for Values {
    type Left = u32;
    type Right = bool;

    fn left(&self) -> &u32 {
        &self.left
    }

    fn right(&self) -> &bool {
        &self.right
    }

    fn reserved_first(&self) -> u32 {
        101
    }

    fn reserved_second(&self) -> u32 {
        202
    }
}

pub fn observe<P: Pair>(pair: &P) -> (u32, u32, u32, u32) {
    (
        pair.left().value(),
        pair.right().value(),
        pair.reserved_first(),
        pair.reserved_second(),
    )
}

pub fn concrete(left: u32, right: bool) -> (u32, u32, u32, u32) {
    observe(&Values { left, right })
}

pub trait Single: Value {}
impl Single for u32 {}

pub fn single<T: Single>(value: &T) -> u32 {
    value.value()
}

#[cfg(test)]
mod tests {
    use super::{concrete, single};

    #[test]
    fn distinct_constraints_preserve_both_dispatches() {
        assert_eq!(concrete(17, true), (17, 1, 101, 202));
        assert_eq!(concrete(u32::MAX, false), (u32::MAX, 0, 101, 202));
        assert_eq!(concrete(0, true), (0, 1, 101, 202));
    }

    #[test]
    fn unrelated_trait_keeps_its_own_field_scope() {
        assert_eq!(single(&23_u32), 23);
    }
}
