use std::sync::Once;

pub mod s2_analyzer;
pub mod s3_optimizer;
pub mod s4_generator;

#[macro_use]
extern crate macro_rules_attribute;

static INIT: Once = Once::new();

pub fn init_logger() {
    INIT.call_once(|| {
        env_logger::init();
    });
}
