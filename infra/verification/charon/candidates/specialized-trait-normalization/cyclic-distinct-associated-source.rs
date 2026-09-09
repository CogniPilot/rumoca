pub trait Family: Sized {
    type Item;
    type Canonical: CanonicalFamily<Self::Item>;
    type Tag;
}

pub trait CanonicalFamily<T>: Sized {
    type Member: Family<Item = T, Canonical = Self>;
}

pub struct First;
pub struct Second;
pub struct Token;

impl Family for First {
    type Item = u32;
    type Canonical = Token;
    type Tag = u8;
}

impl Family for Second {
    type Item = u32;
    type Canonical = Token;
    type Tag = u16;
}

impl CanonicalFamily<u32> for Token {
    type Member = First;
}

pub fn member_tag() -> <<Token as CanonicalFamily<u32>>::Member as Family>::Tag {
    1_u8
}

pub fn other_tag() -> <Second as Family>::Tag {
    2_u16
}

#[test]
fn associated_types_of_distinct_implementors_stay_distinct() {
    let first: u8 = member_tag();
    let second: u16 = other_tag();
    assert_eq!(u16::from(first) + second, 3);
}
