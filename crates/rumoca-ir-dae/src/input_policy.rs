//! The contract a consumer declares for an input before its first runtime write.

/// The caller's contract for an input before its first runtime write.
///
/// A lowering names the policy once; the reference evaluator and the Solve
/// lowering read the same value when they select an input's initial
/// expression.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum InputInitializationPolicy {
    /// A runtime driver supplies every input before the first evaluation.
    #[default]
    RequireRuntimeDriver,
    /// A host owns subsequent writes; use the checked declaration's start.
    HostDrivenStart,
    /// FMI packaging issues a bounds-aware language default before host writes.
    FmiExportDefaults,
}
