use rumoca_core::{ProvenanceSpan, Span};
use serde::ser::SerializeStruct;
use serde::{Deserialize, Serialize};

use crate::DaeConstructionError;

/// Typed generation classification attached to a source-derived DAE object.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DaeGeneration {
    SyntheticResidual,
    BindingEquation,
    ConnectionEquation,
    FlowBalanceEquation,
    AlgorithmEquation,
    DiscreteUpdate,
    ConditionLowering,
    PreValueLowering,
    ClockLowering,
    DelayLowering,
    TerminalLowering,
    EventActionLowering,
    InitializationEquation,
    DefaultStart,
    ArrayEquationProjection,
    RecordEquationProjection,
    FunctionLoopLowering,
    FunctionConditionLowering,
    FunctionAggregateLowering,
    DerivedParameterLowering,
    IndexReduction,
    AliasElimination,
}

impl std::fmt::Display for DaeGeneration {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(match self {
            Self::BindingEquation => "binding equation",
            Self::SyntheticResidual => "synthetic residual",
            Self::ConnectionEquation => "connection equation",
            Self::FlowBalanceEquation => "flow-balance equation",
            Self::AlgorithmEquation => "algorithm equation",
            Self::DiscreteUpdate => "discrete update",
            Self::ConditionLowering => "condition lowering",
            Self::PreValueLowering => "pre-value lowering",
            Self::ClockLowering => "clock lowering",
            Self::DelayLowering => "delay lowering",
            Self::TerminalLowering => "terminal lowering",
            Self::EventActionLowering => "event-action lowering",
            Self::InitializationEquation => "initialization equation",
            Self::DefaultStart => "language-defined default start",
            Self::ArrayEquationProjection => "array-equation projection",
            Self::RecordEquationProjection => "record-equation projection",
            Self::FunctionLoopLowering => "function-loop lowering",
            Self::FunctionConditionLowering => "function-condition lowering",
            Self::FunctionAggregateLowering => "function-aggregate lowering",
            Self::DerivedParameterLowering => "derived-parameter lowering",
            Self::IndexReduction => "index reduction",
            Self::AliasElimination => "alias elimination",
        })
    }
}

/// Typed classification of a DAE object as source-written or generated.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DaeProvenanceOrigin {
    Source,
    Generated(DaeGeneration),
}

impl std::fmt::Display for DaeProvenanceOrigin {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Source => formatter.write_str("source DAE object"),
            Self::Generated(generation) => write!(formatter, "generated DAE object ({generation})"),
        }
    }
}

/// Source traceability attached to a DAE object.
///
/// Construction enforces a typed source/generated classification and a
/// non-dummy, source-backed owner span. It cannot prove that the caller selected
/// the semantically correct owner.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DaeProvenance {
    origin: DaeProvenanceOrigin,
    span: ProvenanceSpan,
}

impl DaeProvenance {
    /// Classify an object as source-written using its source-backed span.
    pub fn source(span: Span) -> Result<Self, DaeConstructionError> {
        Self::try_new(DaeProvenanceOrigin::Source, span)
    }

    /// Classify an object as generated using a source-backed owner span.
    ///
    /// The caller remains responsible for choosing the semantically correct
    /// owner; construction only rejects a dummy span.
    pub fn generated(
        generation: DaeGeneration,
        owner_span: Span,
    ) -> Result<Self, DaeConstructionError> {
        Self::try_new(DaeProvenanceOrigin::Generated(generation), owner_span)
    }

    /// Return the typed source/generated origin.
    pub const fn origin(self) -> DaeProvenanceOrigin {
        self.origin
    }

    /// Return the caller-supplied, non-dummy owner span.
    pub fn span(self) -> Span {
        self.span.span()
    }

    pub(crate) fn try_new(
        origin: DaeProvenanceOrigin,
        span: Span,
    ) -> Result<Self, DaeConstructionError> {
        let checked_span = ProvenanceSpan::new(span, "DAE object").map_err(|_| {
            DaeConstructionError::MissingProvenance {
                origin,
                attempted_span: span,
            }
        })?;
        Ok(Self {
            origin,
            span: checked_span,
        })
    }
}

impl Serialize for DaeProvenance {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut state = serializer.serialize_struct("DaeProvenance", 2)?;
        state.serialize_field("origin", &self.origin)?;
        state.serialize_field("span", &self.span.span())?;
        state.end()
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceId, Span};

    use super::{DaeGeneration, DaeProvenance, DaeProvenanceOrigin};
    use crate::DaeConstructionError;

    fn source_span() -> Span {
        Span::from_offsets(SourceId::from_source_name("provenance.mo"), 12, 20)
    }

    #[test]
    fn source_provenance_preserves_the_owner_span() {
        let provenance = DaeProvenance::source(source_span()).expect("test span is source-backed");

        assert_eq!(provenance.origin(), DaeProvenanceOrigin::Source);
        assert_eq!(provenance.span(), source_span());
    }

    #[test]
    fn generated_provenance_is_typed() {
        let provenance = DaeProvenance::generated(DaeGeneration::ConnectionEquation, source_span())
            .expect("test span is source-backed");

        assert_eq!(
            provenance.origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::ConnectionEquation)
        );
    }

    #[test]
    fn dummy_span_cannot_enter_checked_provenance() {
        assert_eq!(
            DaeProvenance::generated(DaeGeneration::PreValueLowering, Span::DUMMY),
            Err(DaeConstructionError::MissingProvenance {
                origin: DaeProvenanceOrigin::Generated(DaeGeneration::PreValueLowering),
                attempted_span: Span::DUMMY,
            })
        );
    }
}
