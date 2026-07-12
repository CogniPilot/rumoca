//! SPEC_0008-shaped diagnostics for the DAE → GALEC projection.
//!
//! Error codes use the stable `ET0xx` range (GALEC **T**arget projection).
//! Variants carry the best available source [`Span`] from the canonical DAE
//! (variable declaration spans, clock expression spans, function declaration
//! spans); constructs without a source anchor report no span rather than a
//! fabricated one.
//!
//! Per SPEC_0034 GAL-025, rejections that reflect the current scope of the
//! Rumoca projection (continuous states, external functions, runtime events)
//! say "not yet supported by the Rumoca GALEC projection" — never
//! "unsupported by eFMI", because eFMI itself expects discretized models.

use rumoca_core::Span;

/// Errors produced by the DAE → GALEC projection, with stable `ET0xx` codes.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum GalecTargetError {
    /// GAL-025: continuous dynamics are a projection-scope rejection.
    #[error(
        "model has continuous dynamics ({states} continuous state(s), \
         {equations} continuous equation(s)); continuous states are not yet \
         supported by the Rumoca GALEC projection [ET001]"
    )]
    ContinuousDynamics { states: usize, equations: usize },

    /// GAL-025: external functions are a projection-scope rejection.
    #[error(
        "function `{function}` is declared external (language `{language}`); \
         external functions are not yet supported by the Rumoca GALEC \
         projection [ET002]"
    )]
    ExternalFunction {
        function: String,
        language: String,
        span: Span,
    },

    /// GAL-025: runtime event handling is a projection-scope rejection.
    /// Clocked relations that only evaluate at ticks are admissible and do
    /// not raise this error (see `admissibility`).
    #[error(
        "model requires runtime event handling ({scheduled_time_events} \
         scheduled time event(s), {event_actions} event action(s)); runtime \
         events are not yet supported by the Rumoca GALEC projection [ET003]"
    )]
    RuntimeEvents {
        scheduled_time_events: usize,
        event_actions: usize,
    },

    /// GAL-016: GALEC blocks have exactly one static base period; dynamic
    /// (runtime-triggered) clocks are not yet supported by the Rumoca GALEC
    /// projection (counter-encoding over one base period is a future option).
    #[error(
        "model has {count} runtime-triggered clock condition(s); dynamic \
         clocks are not yet supported by the Rumoca GALEC projection \
         (GALEC blocks are driven by exactly one fixed-period clock) [ET004]"
    )]
    DynamicClock { count: usize },

    /// GAL-016: exactly one fixed-period clock schedule is required.
    #[error(
        "model declares {count} periodic clock schedule(s); a GALEC block is \
         driven by exactly one fixed-period clock [ET005]"
    )]
    ClockCountNotOne { count: usize },

    /// The single clock schedule must have a finite, strictly positive
    /// period.
    #[error(
        "clock period {period_seconds} s is not a finite, strictly positive \
         sample period [ET006]"
    )]
    InvalidClockPeriod { period_seconds: f64, span: Span },

    /// MLS §4.7: partial models are incomplete by declaration.
    #[error("partial models cannot be projected to a GALEC block [ET007]")]
    PartialModel,

    /// Only complete simulation-model class types project to a block.
    #[error(
        "root class type `{class_type}` cannot be projected to a GALEC block \
         (expected `model`, `block`, or `class`) [ET008]"
    )]
    UnsupportedClassType { class_type: &'static str },

    /// GAL-020: manifest dimensions are literal integers >= 1;
    /// structurally-parametric (unresolved or empty) array sizes are
    /// rejected.
    #[error(
        "variable `{variable}` dimension {dimension} has size {size}; GALEC \
         array dimensions must be literal integers >= 1 \
         (structurally-parametric array sizes are rejected) [ET009]"
    )]
    NonPositiveDimension {
        variable: String,
        /// 1-based dimension position.
        dimension: usize,
        size: i64,
        span: Span,
    },

    /// The GAL-020 classification table has no row for this variable.
    #[error(
        "variable `{variable}` (causality `{causality}`, partition \
         `{partition}`, origin `{origin}`) does not match any GALEC variable \
         class [ET010]"
    )]
    UnclassifiableVariable {
        variable: String,
        causality: &'static str,
        partition: &'static str,
        origin: &'static str,
        span: Span,
    },

    /// SPEC_0008 / S8: a scalar type is never inferred from start-value
    /// literals or defaulted; when neither the DAE partition contract nor
    /// the caller-provided type provenance determines it, projection fails.
    #[error(
        "cannot determine the GALEC scalar type of `{variable}` (partition \
         `{partition}`): the DAE partition does not fix a scalar type and no \
         type provenance was supplied; types are never inferred from start \
         values [ET011]"
    )]
    UnresolvedScalarType {
        variable: String,
        partition: &'static str,
        span: Span,
    },

    /// GAL-015: the name cannot be carried into GALEC (plain or quoted).
    #[error("`{variable}` cannot be represented as a GALEC name: {reason} [ET012]")]
    UnrepresentableName {
        variable: String,
        reason: &'static str,
    },

    /// A manifest attribute expression is outside the constant-evaluable
    /// subset (literals, unary +/-/not, basic arithmetic, references to
    /// parameter/constant defaults).
    #[error(
        "`{attribute}` of `{variable}` is not evaluable to a constant: \
         {reason} [ET013]"
    )]
    AttributeNotEvaluable {
        variable: String,
        attribute: &'static str,
        reason: String,
        span: Option<Span>,
    },

    /// The evaluated attribute value does not fit the variable's declared
    /// scalar type (types come from the DAE, never from the value).
    #[error(
        "`{attribute}` of `{variable}` evaluates to a {found} value, but the \
         variable's scalar type is {expected} [ET014]"
    )]
    AttributeTypeMismatch {
        variable: String,
        attribute: &'static str,
        expected: &'static str,
        found: &'static str,
        span: Option<Span>,
    },

    /// Default expressions of parameters/constants reference each other in a
    /// cycle.
    #[error("start expressions form a dependency cycle through `{through}` [ET015]")]
    StartDependencyCycle { through: String },

    /// Typed manifest-model construction rejected projection output.
    #[error("manifest construction failed: {source} [ET016]")]
    Manifest {
        #[from]
        source: crate::manifest_context::EfmiError,
    },

    /// GAL-007: a DAE construct outside the currently lowerable subset.
    /// `feature` is the stable feature id of the `unsupported-feature:`
    /// namespace; rejections that reflect projection scope use the GAL-025
    /// wording in `detail`.
    #[error(
        "{detail}; not yet supported by the Rumoca GALEC projection \
         [unsupported-feature:{feature}] [ET017]"
    )]
    UnsupportedFeature {
        feature: String,
        detail: String,
        span: Option<Span>,
    },

    /// A bug in the projection itself: lowering produced output that fails
    /// GALEC/manifest post-validation (GAL-004), or a canonical-DAE
    /// invariant the projection relies on did not hold.
    #[error("internal GALEC projection error (please report): {detail} [ET018]")]
    LoweringInternal { detail: String },

    /// An expression references a variable that exists in no DAE partition.
    #[error(
        "expression references `{name}`, which is not a variable of any DAE \
         partition [ET019]"
    )]
    UnknownVariableReference { name: String, span: Option<Span> },

    /// GALEC has no implicit conversions (trap T5); operand/target types
    /// that cannot be reconciled with an explicit widening cast fail.
    #[error(
        "type mismatch in {context}: expected {expected}, found {found} \
         (GALEC has no implicit conversions) [ET020]"
    )]
    LoweringTypeMismatch {
        context: String,
        expected: &'static str,
        found: &'static str,
        span: Option<Span>,
    },

    /// GAL-024/GAL-015: two distinct GALEC names mangle to the same C
    /// identifier; the embedded C export refuses to rename either apart
    /// (SPEC_0008: no silent defaults).
    #[error(
        "GALEC names `{first}` and `{second}` both mangle to the C \
         identifier `{c_name}`; the embedded C export never renames \
         silently — rename one variable in the model [ET022]"
    )]
    CNameCollision {
        first: String,
        second: String,
        c_name: String,
    },

    /// GAL-007: the embedded C export met a GALEC construct outside the
    /// shape the current lowering emits; rejected loudly, never dropped.
    #[error("the embedded C export does not support {construct}: {detail} [ET023]")]
    CExportUnsupported {
        construct: &'static str,
        detail: String,
    },

    /// GAL-025: initial equations are a projection-scope rejection. Startup
    /// is built from manifest `start` values (plus the dependent-parameter
    /// recomputation) only, so admitting a non-empty initialization
    /// partition would silently ignore the model's initial equations.
    #[error(
        "model has {equations} scalar initial equation(s) \
         ({structured_families} structured initial-equation famil(y/ies)); \
         initial equations are not yet supported by the Rumoca GALEC \
         projection (Startup initializes from `start` values only) [ET021]"
    )]
    InitialEquations {
        equations: usize,
        structured_families: usize,
    },
}

impl GalecTargetError {
    /// Stable diagnostic code (SPEC_0008).
    #[must_use]
    pub const fn code(&self) -> &'static str {
        match self {
            Self::ContinuousDynamics { .. } => "ET001",
            Self::ExternalFunction { .. } => "ET002",
            Self::RuntimeEvents { .. } => "ET003",
            Self::DynamicClock { .. } => "ET004",
            Self::ClockCountNotOne { .. } => "ET005",
            Self::InvalidClockPeriod { .. } => "ET006",
            Self::PartialModel => "ET007",
            Self::UnsupportedClassType { .. } => "ET008",
            Self::NonPositiveDimension { .. } => "ET009",
            Self::UnclassifiableVariable { .. } => "ET010",
            Self::UnresolvedScalarType { .. } => "ET011",
            Self::UnrepresentableName { .. } => "ET012",
            Self::AttributeNotEvaluable { .. } => "ET013",
            Self::AttributeTypeMismatch { .. } => "ET014",
            Self::StartDependencyCycle { .. } => "ET015",
            Self::Manifest { .. } => "ET016",
            Self::UnsupportedFeature { .. } => "ET017",
            Self::LoweringInternal { .. } => "ET018",
            Self::UnknownVariableReference { .. } => "ET019",
            Self::LoweringTypeMismatch { .. } => "ET020",
            Self::InitialEquations { .. } => "ET021",
            Self::CNameCollision { .. } => "ET022",
            Self::CExportUnsupported { .. } => "ET023",
        }
    }

    /// Best available source span, when the rejected construct has one.
    #[must_use]
    pub fn span(&self) -> Option<Span> {
        match self {
            Self::ExternalFunction { span, .. }
            | Self::InvalidClockPeriod { span, .. }
            | Self::NonPositiveDimension { span, .. }
            | Self::UnclassifiableVariable { span, .. }
            | Self::UnresolvedScalarType { span, .. } => (!span.is_dummy()).then_some(*span),
            Self::AttributeNotEvaluable { span, .. }
            | Self::AttributeTypeMismatch { span, .. }
            | Self::UnsupportedFeature { span, .. }
            | Self::UnknownVariableReference { span, .. }
            | Self::LoweringTypeMismatch { span, .. } => span.filter(|span| !span.is_dummy()),
            Self::ContinuousDynamics { .. }
            | Self::RuntimeEvents { .. }
            | Self::DynamicClock { .. }
            | Self::ClockCountNotOne { .. }
            | Self::PartialModel
            | Self::UnsupportedClassType { .. }
            | Self::UnrepresentableName { .. }
            | Self::StartDependencyCycle { .. }
            | Self::Manifest { .. }
            | Self::LoweringInternal { .. }
            | Self::InitialEquations { .. }
            | Self::CNameCollision { .. }
            | Self::CExportUnsupported { .. } => None,
        }
    }
}
