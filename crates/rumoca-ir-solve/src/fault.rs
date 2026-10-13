//! Fault classification of one Solve operation (SPEC_0040 SOLVE-C102).
//!
//! Whether an operation may be dropped when nothing reads its results is a
//! property of the operation, not of the backend that asks. Every pass that
//! prunes, reorders, speculates or folds reads this one classification.

/// How an operation can fail at run time.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FaultKind {
    /// Checked Integer arithmetic whose result can leave the Integer domain,
    /// or divide by zero.
    IntegerArithmetic,
    /// A Real to Integer conversion whose result can leave the Integer domain.
    IntegerConversion,
    /// A computed subscript that can leave the extent it indexes.
    IndexBounds,
    /// A numeric solve that can be singular.
    SingularSystem,
    /// A Real builtin evaluated off its domain (`sqrt`, `log`, `asin`, ...):
    /// the value is a NaN, not an error, but its derivative is undefined. The
    /// typed program never raises it; the DAE classification reports it for a
    /// selection that must stay differentiable.
    RealDomain,
}

/// What evaluating an operation can do besides produce its results.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FaultClass {
    /// A pure computation that cannot fail: when nothing reads its results it
    /// may be dropped, and it may be evaluated speculatively.
    CannotFault,
    /// A pure computation that can fail in the stated way: it always runs, so
    /// the fault it would raise is never removed.
    MayFault(FaultKind),
    /// An operation with an effect (a call, an assertion, a store, a write of
    /// assigned state, a foreign body) or a loop: it always runs.
    HasEffect,
}

impl FaultClass {
    /// Whether the operation may be dropped when nothing reads its results.
    #[must_use]
    pub const fn is_prunable(self) -> bool {
        matches!(self, Self::CannotFault)
    }

    /// The class of a sequence of operations evaluated together: the first
    /// effect, else the first fault, else `CannotFault`.
    #[must_use]
    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::HasEffect, _) | (_, Self::HasEffect) => Self::HasEffect,
            (Self::MayFault(kind), _) | (Self::CannotFault, Self::MayFault(kind)) => {
                Self::MayFault(kind)
            }
            (Self::CannotFault, Self::CannotFault) => Self::CannotFault,
        }
    }
}
