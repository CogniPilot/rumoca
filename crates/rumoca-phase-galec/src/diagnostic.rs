//! SPEC_0008-shaped diagnostics for the DAE → GALEC projection.
//!
//! Error codes use the stable `EGT0xx` range (**G**ALEC **T**arget projection).
//! Variants carry the best available source [`Span`] from the canonical DAE
//! (variable declaration spans, clock expression spans, function declaration
//! spans); constructs without a source anchor report no span rather than a
//! fabricated one.
//!
//! Per SPEC_0034 GAL-025, rejections that reflect the current scope of the
//! Rumoca projection (continuous states, external functions, runtime events)
//! say "not yet supported by the Rumoca GALEC projection" — never
//! "unsupported by eFMI", because eFMI itself expects discretized models.

use std::fmt;

use rumoca_core::{Diagnostic, PhaseError, PrimaryLabel, Span};

/// Complete ordered refusal set produced by one GALEC target projection.
///
/// Target orchestration carries this aggregate as an error source so adapters
/// can recover every phase-local diagnostic without parsing display text.
#[derive(Debug)]
pub struct GalecTargetErrors(Box<[GalecTargetError]>);

impl GalecTargetErrors {
    pub fn iter(&self) -> std::slice::Iter<'_, GalecTargetError> {
        self.0.iter()
    }
}

impl From<Vec<GalecTargetError>> for GalecTargetErrors {
    fn from(errors: Vec<GalecTargetError>) -> Self {
        Self(errors.into_boxed_slice())
    }
}

impl fmt::Display for GalecTargetErrors {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (ordinal, error) in self.0.iter().enumerate() {
            if ordinal != 0 {
                formatter.write_str("; ")?;
            }
            write!(formatter, "{error}")?;
        }
        Ok(())
    }
}

impl std::error::Error for GalecTargetErrors {}

/// Errors produced by the DAE → GALEC projection, with stable `EGT0xx` codes.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum GalecTargetError {
    /// GAL-025: continuous dynamics are a projection-scope rejection.
    #[error(
        "model has continuous dynamics ({states} continuous state(s), \
         {equations} continuous equation(s)); continuous states are not yet \
         supported by the Rumoca GALEC projection \
         [unsupported-feature:continuous-dynamics]"
    )]
    ContinuousDynamics {
        states: usize,
        equations: usize,
        span: Option<Span>,
    },

    /// GAL-025: external functions are a projection-scope rejection.
    #[error(
        "function `{function}` is declared external (language `{language}`); \
         external functions are not yet supported by the Rumoca GALEC \
         projection"
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
         events are not yet supported by the Rumoca GALEC projection"
    )]
    RuntimeEvents {
        scheduled_time_events: usize,
        event_actions: usize,
    },

    /// GAL-016: GALEC blocks have one static base period; dynamic
    /// (runtime-triggered) clocks are not yet supported by the Rumoca GALEC
    /// projection.
    #[error(
        "model has {count} runtime-triggered clock condition(s); dynamic \
         clocks are not yet supported by the Rumoca GALEC projection \
         (GALEC blocks are driven by one fixed base period)"
    )]
    DynamicClock { count: usize },

    /// GAL-016: at least one fixed-period clock schedule is required.
    #[error(
        "model declares no fixed-period clock schedule; a GALEC block requires \
         at least one fixed clock to establish its base period"
    )]
    NoPeriodicClock,

    /// The single clock schedule must have a finite, strictly positive
    /// period.
    #[error(
        "clock period {period_seconds} s is not a finite, strictly positive \
         sample period"
    )]
    InvalidClockPeriod { period_seconds: f64, span: Span },

    /// MLS §4.7: partial models are incomplete by declaration.
    #[error("partial models cannot be projected to a GALEC block")]
    PartialModel,

    /// Only complete simulation-model class types project to a block.
    #[error(
        "root class type `{class_type}` cannot be projected to a GALEC block \
         (expected `model`, `block`, or `class`)"
    )]
    UnsupportedClassType { class_type: &'static str },

    /// GAL-020: manifest dimensions are literal integers >= 1;
    /// structurally-parametric (unresolved or empty) array sizes are
    /// rejected.
    #[error(
        "variable `{variable}` dimension {dimension} has size {size}; GALEC \
         array dimensions must be literal integers >= 1 \
         (structurally-parametric array sizes are rejected)"
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
         class"
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
         values"
    )]
    UnresolvedScalarType {
        variable: String,
        partition: &'static str,
        span: Span,
    },

    /// GAL-015: the name cannot be carried into GALEC (plain or quoted).
    #[error("`{variable}` cannot be represented as a GALEC name: {reason}")]
    UnrepresentableName {
        variable: String,
        reason: &'static str,
    },

    /// A manifest attribute expression is outside the constant-evaluable
    /// subset (literals, unary +/-/not, basic arithmetic, references to
    /// parameter/constant defaults).
    #[error(
        "`{attribute}` of `{variable}` is not evaluable to a constant: \
         {reason}"
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
         variable's scalar type is {expected}"
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
    #[error("start expressions form a dependency cycle through `{through}`")]
    StartDependencyCycle { through: String },

    /// GAL-017: `Startup` may call builtins only, so a dependent parameter
    /// bound to a Modelica function call has to be folded to a value. The
    /// fold needs every input frozen when the code is generated; `reason`
    /// says which requirement this binding does not meet.
    #[error(
        "dependent parameter `{variable}` is bound to a call to `{function}`, which \
         Startup may not make, and the call could not be folded while the code was \
         generated: {reason}"
    )]
    DependentParameterNotFoldable {
        variable: String,
        function: String,
        reason: String,
        span: Option<Span>,
    },

    /// GAL-007: a DAE construct outside the currently lowerable subset.
    /// `feature` is the stable feature id of the `unsupported-feature:`
    /// namespace; rejections that reflect projection scope use the GAL-025
    /// wording in `detail`.
    #[error(
        "{detail}; not yet supported by the Rumoca GALEC projection \
         [unsupported-feature:{feature}]"
    )]
    UnsupportedFeature {
        feature: String,
        detail: String,
        span: Option<Span>,
    },

    /// A bug in the projection itself: lowering produced output that fails
    /// GALEC/manifest post-validation (GAL-004), or a canonical-DAE
    /// invariant the projection relies on did not hold.
    #[error("internal GALEC projection error (please report): {detail}")]
    LoweringInternal { detail: String },

    /// An expression references a variable that exists in no DAE partition.
    #[error(
        "expression references `{name}`, which is not a variable of any DAE \
         partition"
    )]
    UnknownVariableReference { name: String, span: Option<Span> },

    /// GALEC has no implicit conversions (trap T5); operand/target types
    /// that cannot be reconciled with an explicit widening cast fail.
    #[error(
        "type mismatch in {context}: expected {expected}, found {found} \
         (GALEC has no implicit conversions)"
    )]
    LoweringTypeMismatch {
        context: String,
        expected: &'static str,
        found: &'static str,
        span: Option<Span>,
    },

    /// GAL-025: initial equations are a projection-scope rejection. Startup
    /// is built from manifest `start` values (plus the dependent-parameter
    /// recomputation) only, so admitting a non-empty initialization
    /// partition would silently ignore the model's initial equations.
    #[error(
        "model has {equations} scalar initial equation(s) \
         ({structured_families} structured initial-equation famil(y/ies)); \
         initial equations are not yet supported by the Rumoca GALEC \
         projection (Startup initializes from `start` values only)"
    )]
    InitialEquations {
        equations: usize,
        structured_families: usize,
    },

    /// MLS §8.6: an `initial algorithm` that determines a discrete-time
    /// variable is an initialization-partition definition of that variable's
    /// initial value. GALEC Startup initializes from `start` attributes only,
    /// so admitting one would silently start the block from the declared
    /// `start` instead of the determined value.
    #[error(
        "model determines {definitions} discrete initial value(s) in an initial section; algorithm-determined initial values are not yet supported by the Rumoca GALEC projection (Startup initializes from `start` values only)"
    )]
    InitialDiscreteValues { definitions: usize },

    /// A current-tick dependency named a variable identity outside the exact
    /// checked DAE view being projected. Continuing would silently delete the
    /// dependency edge and could invent an executable order.
    #[error(
        "internal GALEC projection error: current-tick dependency references foreign DAE variable #{variable_index}"
    )]
    ForeignCausalRead { variable_index: u32, span: Span },

    /// A complete scalar-definition family cannot be addressed by the DAE's
    /// u32 scalar identity domain. The family is refused before traversal.
    #[error(
        "internal GALEC projection error: causal definition family for `{variable}` has {scalar_count} scalars, exceeding the u32 identity domain"
    )]
    CausalScalarDefinitionOverflow {
        variable: String,
        scalar_count: usize,
        span: Span,
    },

    /// Structural analysis claimed a complete scalar-definition family but
    /// supplied no definition for one scalar. No partial family may authorize
    /// a same-tick schedule.
    #[error(
        "internal GALEC projection error: causal definition family for `{variable}` is missing scalar {missing_scalar} of {scalar_count}"
    )]
    IncompleteCausalScalarDefinitions {
        variable: String,
        missing_scalar: u32,
        scalar_count: u32,
        span: Span,
    },
}

impl GalecTargetError {
    /// Stable diagnostic code (SPEC_0008).
    #[must_use]
    pub const fn code(&self) -> &'static str {
        match self {
            Self::ContinuousDynamics { .. } => "EGT001",
            Self::ExternalFunction { .. } => "EGT002",
            Self::RuntimeEvents { .. } => "EGT003",
            Self::DynamicClock { .. } => "EGT004",
            Self::NoPeriodicClock => "EGT005",
            Self::InvalidClockPeriod { .. } => "EGT006",
            Self::PartialModel => "EGT007",
            Self::UnsupportedClassType { .. } => "EGT008",
            Self::NonPositiveDimension { .. } => "EGT009",
            Self::UnclassifiableVariable { .. } => "EGT010",
            Self::UnresolvedScalarType { .. } => "EGT011",
            Self::UnrepresentableName { .. } => "EGT012",
            Self::AttributeNotEvaluable { .. } => "EGT013",
            Self::AttributeTypeMismatch { .. } => "EGT014",
            Self::StartDependencyCycle { .. } => "EGT015",
            Self::UnsupportedFeature { .. } => "EGT017",
            Self::LoweringInternal { .. } => "EGT018",
            Self::UnknownVariableReference { .. } => "EGT019",
            Self::LoweringTypeMismatch { .. } => "EGT020",
            Self::InitialEquations { .. } => "EGT021",
            Self::InitialDiscreteValues { .. } => "EGT022",
            Self::DependentParameterNotFoldable { .. } => "EGT023",
            Self::ForeignCausalRead { .. } => "EGT024",
            Self::CausalScalarDefinitionOverflow { .. } => "EGT025",
            Self::IncompleteCausalScalarDefinitions { .. } => "EGT026",
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
            Self::ForeignCausalRead { span, .. }
            | Self::CausalScalarDefinitionOverflow { span, .. }
            | Self::IncompleteCausalScalarDefinitions { span, .. } => {
                (!span.is_dummy()).then_some(*span)
            }
            Self::ContinuousDynamics { span, .. }
            | Self::AttributeNotEvaluable { span, .. }
            | Self::AttributeTypeMismatch { span, .. }
            | Self::UnsupportedFeature { span, .. }
            | Self::UnknownVariableReference { span, .. }
            | Self::DependentParameterNotFoldable { span, .. }
            | Self::LoweringTypeMismatch { span, .. } => span.filter(|span| !span.is_dummy()),
            Self::RuntimeEvents { .. }
            | Self::DynamicClock { .. }
            | Self::NoPeriodicClock
            | Self::PartialModel
            | Self::UnsupportedClassType { .. }
            | Self::UnrepresentableName { .. }
            | Self::StartDependencyCycle { .. }
            | Self::LoweringInternal { .. }
            | Self::InitialEquations { .. }
            | Self::InitialDiscreteValues { .. } => None,
        }
    }

    fn primary_label(&self, span: Span) -> PrimaryLabel {
        let message = match self {
            Self::UnsupportedFeature { feature, .. } => {
                format!("unsupported GALEC projection feature `{feature}`")
            }
            Self::ContinuousDynamics { .. } => {
                "unsupported GALEC projection feature `continuous-dynamics`".to_owned()
            }
            _ => "GALEC projection rejected this construct".to_owned(),
        };
        PrimaryLabel::new(span).with_message(message)
    }
}

impl PhaseError for GalecTargetError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self.span() {
            Some(span) => {
                Diagnostic::error(self.code(), self.to_string(), self.primary_label(span))
            }
            None => Diagnostic::global_error(self.code(), self.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::SourceId;

    #[test]
    fn phase_diagnostic_preserves_code_and_source_span() {
        let span = Span::from_offsets(SourceId::from_source_name("galec-target.mo"), 7, 11);
        let diagnostic = GalecTargetError::UnsupportedFeature {
            feature: "array-projection".to_owned(),
            detail: "projection is not representable".to_owned(),
            span: Some(span),
        }
        .to_diagnostic();

        assert_eq!(diagnostic.code.as_deref(), Some("EGT017"));
        assert_eq!(
            diagnostic
                .labels
                .iter()
                .find(|label| label.primary)
                .map(|label| label.span),
            Some(span)
        );
    }

    #[test]
    fn phase_diagnostic_supports_model_level_rejections() {
        let diagnostic = GalecTargetError::ContinuousDynamics {
            states: 1,
            equations: 1,
            span: None,
        }
        .to_diagnostic();
        assert_eq!(diagnostic.code.as_deref(), Some("EGT001"));
        assert!(!diagnostic.labels.iter().any(|label| label.primary));
    }
}
