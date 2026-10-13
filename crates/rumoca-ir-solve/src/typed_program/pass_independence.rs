//! The vocabulary of independent passes (SPEC_0040 SOLVE-C104).
//!
//! A loop has independent passes when no pass reads a value another pass
//! writes and no value is carried from one pass to the next except an array
//! whose passes write disjoint, binder-indexed elements. The program proves
//! this once, when it is constructed, and states how far a backend may go with
//! it: the exactness of the operations of one pass decides whether passes may
//! be evaluated in lanes.

use super::SolveBinaryOperator;

/// Whether evaluating several passes of a loop together computes, for every
/// pass, the bits that evaluating the passes one after the other computes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LaneExactness {
    /// Every operation of a pass is pure, cannot fault and is one of the
    /// operations whose result for a pass is a function of that pass's
    /// operands alone, under the lane forms the SOLVE-C104 table states
    /// (IEEE 754 Binary64 `+ - * /`, `sqrt`, negation, absolute value, the
    /// comparisons, selection, the extrema of `real_extremum`, exact Integer
    /// arithmetic proven inside its domain, element access proven inside its
    /// extent). Passes may be evaluated in any order or in lanes.
    Exact,
    /// Some operation of a pass depends on pass order: it can fault (the first
    /// fault in pass order is the one reported), has an effect or records an
    /// observation, is a reduction whose association order is fixed by the
    /// scalar path, calls the host's math library, or is a loop of its own.
    /// That operation keeps pass order.
    PassOrdered,
}

impl LaneExactness {
    /// The exactness of operations evaluated together.
    #[must_use]
    pub const fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::Exact, Self::Exact) => Self::Exact,
            _ => Self::PassOrdered,
        }
    }
}

/// The independent-passes property of one loop (SOLVE-C104).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IndependentPasses {
    exactness: LaneExactness,
}

impl IndependentPasses {
    pub(super) const fn new(exactness: LaneExactness) -> Self {
        Self { exactness }
    }

    /// The exactness of the operations of one pass.
    #[must_use]
    pub const fn exactness(self) -> LaneExactness {
        self.exactness
    }

    /// Whether a backend may evaluate passes in lanes: every operation of a
    /// pass is lane-exact.
    #[must_use]
    pub const fn admits_lanes(self) -> bool {
        matches!(self.exactness, LaneExactness::Exact)
    }
}

/// What an Integer register holds across the passes of one loop.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SolveIndexForm {
    /// The same value on every pass, when it is known.
    Invariant(Option<i64>),
    /// `scale * binder + offset` for the binder at `ordinal` of the loop's
    /// domain, `scale` not zero.
    Binder {
        ordinal: usize,
        scale: i64,
        offset: i64,
    },
    /// Any other value: it may differ from pass to pass in an unstated way.
    Varying,
}

impl SolveIndexForm {
    /// The form of a binder itself.
    #[must_use]
    pub const fn binder(ordinal: usize) -> Self {
        Self::Binder {
            ordinal,
            scale: 1,
            offset: 0,
        }
    }

    /// The form of `-self`.
    #[must_use]
    pub fn negate(self) -> Self {
        match self {
            Self::Invariant(value) => Self::Invariant(value.and_then(i64::checked_neg)),
            Self::Binder {
                ordinal,
                scale,
                offset,
            } => match (scale.checked_neg(), offset.checked_neg()) {
                (Some(scale), Some(offset)) => Self::Binder {
                    ordinal,
                    scale,
                    offset,
                },
                _ => Self::Varying,
            },
            Self::Varying => Self::Varying,
        }
    }

    /// The form of `lhs <operator> rhs` for the operators that keep a form
    /// affine. Any other operator of invariant operands is invariant, and any
    /// other operator of a varying operand is varying.
    #[must_use]
    pub fn binary(operator: SolveBinaryOperator, lhs: Self, rhs: Self) -> Self {
        use SolveBinaryOperator as B;
        match (operator, lhs, rhs) {
            (B::Add, ..) => Self::sum(lhs, rhs),
            (B::Subtract, ..) => Self::sum(lhs, rhs.negate()),
            (B::Multiply, ..) => Self::product(lhs, rhs),
            (_, Self::Invariant(_), Self::Invariant(_)) => Self::Invariant(None),
            _ => Self::Varying,
        }
    }

    fn sum(lhs: Self, rhs: Self) -> Self {
        match (lhs, rhs) {
            (Self::Invariant(a), Self::Invariant(b)) => {
                Self::Invariant(a.zip(b).and_then(|(a, b)| a.checked_add(b)))
            }
            (Self::Binder { .. }, Self::Invariant(None))
            | (Self::Invariant(None), Self::Binder { .. }) => Self::Varying,
            (
                Self::Binder {
                    ordinal,
                    scale,
                    offset,
                },
                Self::Invariant(Some(constant)),
            )
            | (
                Self::Invariant(Some(constant)),
                Self::Binder {
                    ordinal,
                    scale,
                    offset,
                },
            ) => offset
                .checked_add(constant)
                .map_or(Self::Varying, |offset| Self::Binder {
                    ordinal,
                    scale,
                    offset,
                }),
            (
                Self::Binder {
                    ordinal: first,
                    scale: a,
                    offset: x,
                },
                Self::Binder {
                    ordinal: second,
                    scale: b,
                    offset: y,
                },
            ) if first == second => match (a.checked_add(b), x.checked_add(y)) {
                (Some(0), Some(offset)) => Self::Invariant(Some(offset)),
                (Some(scale), Some(offset)) => Self::Binder {
                    ordinal: first,
                    scale,
                    offset,
                },
                _ => Self::Varying,
            },
            _ => Self::Varying,
        }
    }

    fn product(lhs: Self, rhs: Self) -> Self {
        match (lhs, rhs) {
            (Self::Invariant(a), Self::Invariant(b)) => {
                Self::Invariant(a.zip(b).and_then(|(a, b)| a.checked_mul(b)))
            }
            (
                Self::Binder {
                    ordinal,
                    scale,
                    offset,
                },
                Self::Invariant(Some(factor)),
            )
            | (
                Self::Invariant(Some(factor)),
                Self::Binder {
                    ordinal,
                    scale,
                    offset,
                },
            ) => match (scale.checked_mul(factor), offset.checked_mul(factor)) {
                (Some(0), Some(offset)) => Self::Invariant(Some(offset)),
                (Some(scale), Some(offset)) => Self::Binder {
                    ordinal,
                    scale,
                    offset,
                },
                _ => Self::Varying,
            },
            _ => Self::Varying,
        }
    }

    /// The form of a value that is `first` on some passes and `second` on the
    /// others.
    #[must_use]
    pub fn merge(first: Self, second: Self) -> Self {
        if first == second {
            first
        } else if matches!((first, second), (Self::Invariant(_), Self::Invariant(_))) {
            Self::Invariant(None)
        } else {
            Self::Varying
        }
    }
}
