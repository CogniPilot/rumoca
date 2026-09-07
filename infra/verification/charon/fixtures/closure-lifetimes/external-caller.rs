extern crate simpler_closures;

pub fn named(value: &u32) -> &u32 {
    simpler_closures::named()(value)
}

pub fn higher_ranked(value: &u32) -> &u32 {
    simpler_closures::higher_ranked()(value)
}
