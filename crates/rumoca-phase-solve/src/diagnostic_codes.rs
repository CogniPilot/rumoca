//! Stable Solve-lowering diagnostic codes.

/// A checked DAE expression has semantics not yet representable in Solve IR.
pub const EL001_UNSUPPORTED_EXPRESSION: &str = "EL001";
/// A checked DAE cannot be lowered to a computable Solve problem or a Solve
/// constructor contract was violated.
pub const EL005_INVALID_SOLVE_CONTRACT: &str = "EL005";

pub const SOLVE_LOWER_DIAGNOSTIC_CODES: &[&str] =
    &[EL001_UNSUPPORTED_EXPRESSION, EL005_INVALID_SOLVE_CONTRACT];
