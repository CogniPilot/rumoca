pub fn use_helper(value: &u8) -> u8 {
    transport_dep::helper(value)
}

pub fn use_closure(value: &u8) -> u8 {
    transport_dep::make(value)()
}

pub fn takes_dep_closure(value: &u8) -> impl Fn() -> u8 + '_ {
    transport_dep::make(value)
}
