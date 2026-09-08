//! The closed result of typecheck's expression value-type inference.
//!
//! This is the type-axis counterpart of `ExpressionShape`: three states, both
//! non-success arms carrying a required reason, discharging SPEC_0008's rule
//! that inference which can fail to determine required data "must return a
//! distinct unknown/error result". `Known` carries a canonical resolved
//! [`TypeId`], so enumeration identity survives the carrier; no flat
//! scalar-domain enum is minted here, because a flat `Enumeration` variant
//! cannot tell `Colors` from `Sizes`.
//!
//! Width, storage format and target arithmetic profile are deliberately absent:
//! they are a target-build selection, not a Modelica type, and the crate graph
//! (`rumoca-phase-typecheck` depends on neither `rumoca-ir-solve`,
//! `rumoca-ir-galec` nor `rumoca-phase-codegen`) makes naming one a compile
//! error rather than a convention.

use rumoca_core::TypeId;

/// A [`TypeId`] that is known to name a type.
///
/// The field is private to this module and [`ResolvedTypeId::new`] is the only
/// producer, so a `ResolvedTypeId` holding `TypeId::UNKNOWN` cannot be built
/// anywhere in the crate. That is what makes `ExpressionType::Known(UNKNOWN)` -
/// a fourth state meaning "resolved to the unresolved sentinel" -
/// unrepresentable rather than merely unminted: no caller outside this module
/// can name the value the `Known` arm requires.
///
/// Without it, an unresolved identity reaches `format_type_name`, which has no
/// entry for the sentinel and falls back to a Rust `Debug` rendering, so a user
/// is shown `TypeId(4294967295)` where a Modelica type name belongs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ResolvedTypeId(TypeId);

impl ResolvedTypeId {
    /// The sole producer. `None` exactly when the identity is the unresolved
    /// sentinel, which name and type resolution own.
    fn new(type_id: TypeId) -> Option<Self> {
        (!type_id.is_unknown()).then_some(Self(type_id))
    }

    /// The underlying identity, which is never the unresolved sentinel.
    pub(crate) fn get(self) -> TypeId {
        self.0
    }
}

/// Where two value types were composed, and therefore which MLS rule decided
/// the composition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ValueCompositionContext {
    /// MLS §10.4: the arguments of an array constructor. The constructor's type
    /// is the maximally expanded type of *every* argument, and every argument
    /// must be type compatible (§6.7).
    ArrayConstructor,
    /// MLS §10.4.3: the `start`, `step` and `end` bounds of a range. The vector
    /// is Real when any bound is Real, so the element type is a function of all
    /// three bounds, not of `start` alone.
    RangeBounds,
    /// MLS §3.6.5: the branch values of an if-expression.
    /// `check_if_expression_branch_types` already reports incompatibility here.
    ConditionalBranches,
    /// MLS §6.7: the two operands of a binary operator.
    /// `require_numeric_expression` / `require_boolean_expression` /
    /// `require_addition_expressions` already report operand legality here.
    BinaryOperands,
}

/// A composition whose incompatibility **this phase reports itself**, because
/// no earlier checker inspects those operand types.
///
/// The delegated contexts have no representation here, so a message renderer
/// taking this type cannot have an arm for them: the one-fact-one-issuer filter
/// is carried by the type rather than by an early return at the call site.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ReportedComposition {
    ArrayConstructor,
    RangeBounds,
}

/// How two composed value types fail MLS §6.7.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum IncompatibilityKind {
    /// Two types that are not compatible under the numeric promotion rule.
    ValueTypes,
    /// Two enumeration expressions with different enumeration identities.
    /// Identity, never spelling, decides this: two enumerations declaring the
    /// same literal names in the same order are still two enumerations unless
    /// they are the same type.
    EnumerationIdentity,
}

/// A form that carries several values at once and therefore has no single
/// value type. These are legal Modelica in an output-expression-list and
/// illegal in a value position; the equation-level checker decides which.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MultiValueForm {
    /// `(a, b)` on the left of an equation (MLS §8.3.1, §10.6.13).
    OutputExpressionList,
    /// A call to a function declaring more than one output (MLS §12.4.1).
    MultiOutputCall,
}

/// Why an expression has no value type of its own.
///
/// Every variant is a definite ill-typedness or a definite absence of a single
/// value type. None of them means "not inferable yet"; that is `Unknown`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum TypeErrorReason {
    /// MLS §6.7: two composed positions are not type compatible.
    Incompatible {
        context: ValueCompositionContext,
        kind: IncompatibilityKind,
        left: ResolvedTypeId,
        right: ResolvedTypeId,
    },
    /// SPEC_0022 ARR-006 / MLS §10.4: `{}` has no element type.
    ///
    /// This is a definite ill-formedness, not an abstention, so it stays
    /// `Invalid`: no consumer may derive a type from it. This phase has no
    /// checker that reports ARR-006 yet, and a new rejection needs its own
    /// acceptance contract, so nothing renders it.
    EmptyArrayConstructor,
    /// The expression carries several values, so it has no single value type.
    MultiValue(MultiValueForm),
}

/// A type error this phase reports itself, carrying exactly the data its
/// message needs and nothing that could fail to render.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ReportedTypeError {
    pub(crate) composition: ReportedComposition,
    pub(crate) kind: IncompatibilityKind,
    pub(crate) left: ResolvedTypeId,
    pub(crate) right: ResolvedTypeId,
}

impl TypeErrorReason {
    /// `Some` exactly when this phase, rather than another checker, issues the
    /// diagnostic for this error.
    ///
    /// Re-proving a fact an earlier checker already issued is prohibited
    /// (SPEC_0033 §2a), so a delegated context yields `None` here and the
    /// composing node stays silent.
    pub(crate) fn reported(&self) -> Option<ReportedTypeError> {
        let Self::Incompatible {
            context,
            kind,
            left,
            right,
        } = self
        else {
            return None;
        };
        let composition = match context {
            ValueCompositionContext::ArrayConstructor => ReportedComposition::ArrayConstructor,
            ValueCompositionContext::RangeBounds => ReportedComposition::RangeBounds,
            ValueCompositionContext::ConditionalBranches
            | ValueCompositionContext::BinaryOperands => return None,
        };
        Some(ReportedTypeError {
            composition,
            kind: *kind,
            left: *left,
            right: *right,
        })
    }
}

/// The value type of an expression, as issued by `infer_expression_type`.
///
/// `infer_expression_type` is the sole minter. Consumers accept this closed
/// result; none of them may substitute a plausible type for a non-`Known` arm,
/// and none of them re-derives the fact.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum ExpressionType {
    /// Inferred, with full type identity including enumeration identity.
    Known(ResolvedTypeId),
    /// Not inferable here; another owner (name resolution, type resolution)
    /// reports the underlying problem. The reason is required.
    Unknown(&'static str),
    /// Definitely has no value type. The reason names the defect, and
    /// [`TypeErrorReason::reported`] names whether this phase issues it.
    Invalid(TypeErrorReason),
}

impl ExpressionType {
    /// The reason recorded when an identity turns out to be unresolved.
    const UNRESOLVED: &'static str =
        "expression type identity is unresolved; name and type resolution own it";

    /// The sole constructor of the `Known` arm outside this module.
    ///
    /// An unresolved identity becomes `Unknown`, never `Known`. Because
    /// [`ResolvedTypeId`]'s field is private, this is not merely the
    /// conventional way to build a `Known`: it is the only reachable one.
    pub(crate) fn known(type_id: TypeId) -> Self {
        match ResolvedTypeId::new(type_id) {
            Some(resolved) => Self::Known(resolved),
            None => Self::Unknown(Self::UNRESOLVED),
        }
    }

    /// The value identity this expression carries, if it carries one.
    ///
    /// The `Unknown` and `Invalid` arms yield no identity **by construction**:
    /// this projection cannot invent, default or widen a `TypeId`, so an
    /// ill-typed expression can never satisfy a downstream compatibility
    /// predicate. The `Some` case is always a resolved identity, so no consumer
    /// needs to re-check for the unresolved sentinel. Consumers that must
    /// distinguish abstention from ill-typedness match on the enum instead.
    pub(crate) fn value_identity(&self) -> Option<TypeId> {
        match self {
            Self::Known(resolved) => Some(resolved.get()),
            Self::Unknown(reason) => {
                debug_assert!(!reason.is_empty(), "an abstention must carry a reason");
                None
            }
            Self::Invalid(_) => None,
        }
    }
}
