#[cfg(all(test, feature = "host-engine", not(target_arch = "wasm32")))]
mod tests;
