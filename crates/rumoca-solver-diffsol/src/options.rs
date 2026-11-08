//! Concrete options owned by the Diffsol numerical adapter.

/// Method implemented by this adapter. No method selector lives in common ME.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum DiffsolMethod {
    #[default]
    Bdf,
}

/// Backend configuration; common tolerances and nominals remain host-owned.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct DiffsolOptions {
    pub method: DiffsolMethod,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn concrete_default_keeps_the_existing_bdf_method() {
        assert_eq!(DiffsolOptions::default().method, DiffsolMethod::Bdf);
    }
}
