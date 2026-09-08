use rumoca_core::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveProgramConstructionError {
    MissingProvenance,
    WireMismatch,
    IdentityOverflow { provenance: Span },
    ProfileMismatch { provenance: Span },
    WritableConstant { provenance: Span },
    UnknownSlot { provenance: Span },
    UnknownRegister { provenance: Span },
    ReadOnlyStore { provenance: Span },
    TypeMismatch { provenance: Span },
    UnprovedIntegerRange { provenance: Span },
    InvalidAggregate { provenance: Span },
    InvalidProjection { provenance: Span },
    UnknownCallOwner { provenance: Span },
    InvalidCallInterface { provenance: Span },
    EmptyCallOutput { provenance: Span },
    InvalidCallOutput { provenance: Span },
    InvalidRegion { provenance: Span },
    InvalidMap { provenance: Span },
    InvalidFold { provenance: Span },
    InvalidTensorAlgebra { provenance: Span },
    MissingReductionContract { provenance: Span },
    EmptyFirstProductDomain { provenance: Span },
    IncompleteCallOutput { provenance: Span },
    DuplicateCallIdentity { provenance: Span },
    UninitializedSlot { provenance: Span },
}

impl SolveProgramConstructionError {
    #[must_use]
    pub const fn source_span(&self) -> Option<Span> {
        match self {
            Self::MissingProvenance | Self::WireMismatch => None,
            Self::IdentityOverflow { provenance }
            | Self::ProfileMismatch { provenance }
            | Self::WritableConstant { provenance }
            | Self::UnknownSlot { provenance }
            | Self::UnknownRegister { provenance }
            | Self::ReadOnlyStore { provenance }
            | Self::TypeMismatch { provenance }
            | Self::UnprovedIntegerRange { provenance }
            | Self::InvalidAggregate { provenance }
            | Self::InvalidProjection { provenance }
            | Self::UnknownCallOwner { provenance }
            | Self::InvalidCallInterface { provenance }
            | Self::EmptyCallOutput { provenance }
            | Self::InvalidCallOutput { provenance }
            | Self::InvalidRegion { provenance }
            | Self::InvalidMap { provenance }
            | Self::InvalidFold { provenance }
            | Self::InvalidTensorAlgebra { provenance }
            | Self::MissingReductionContract { provenance }
            | Self::EmptyFirstProductDomain { provenance }
            | Self::IncompleteCallOutput { provenance }
            | Self::DuplicateCallIdentity { provenance }
            | Self::UninitializedSlot { provenance } => Some(*provenance),
        }
    }
}

impl std::fmt::Display for SolveProgramConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = match self {
            Self::MissingProvenance => "typed program owner is missing exact provenance",
            Self::WireMismatch => "typed program wire does not replay through checked construction",
            Self::IdentityOverflow { .. } => "typed program identity capacity exceeded",
            Self::ProfileMismatch { .. } => "value type does not belong to the program profile",
            Self::WritableConstant { .. } => "constant storage cannot be writable",
            Self::UnknownSlot { .. } => "slot is not owned by this program",
            Self::UnknownRegister { .. } => "register is not owned by this program",
            Self::ReadOnlyStore { .. } => "store targets read-only storage",
            Self::TypeMismatch { .. } => "typed program operand or result type mismatch",
            Self::UnprovedIntegerRange { .. } => {
                "integer operation has no construction-issued result-range proof"
            }
            Self::InvalidAggregate { .. } => "typed aggregate shape or element contract is invalid",
            Self::InvalidProjection { .. } => "typed aggregate projection is invalid",
            Self::UnknownCallOwner { .. } => "pure-call owner was not issued by this table",
            Self::InvalidCallInterface { .. } => "pure-call argument or slot interface is invalid",
            Self::EmptyCallOutput { .. } => "pure-call owner has no value or assertion output",
            Self::InvalidCallOutput { .. } => "pure-call output kind does not match its type",
            Self::InvalidRegion { .. } => "typed structured region interface is invalid",
            Self::InvalidMap { .. } => "typed compact map interface or domain is invalid",
            Self::InvalidFold { .. } => "typed compact fold interface or domain is invalid",
            Self::InvalidTensorAlgebra { .. } => {
                "typed tensor-algebra rank, shape, or element contract is invalid"
            }
            Self::MissingReductionContract { .. } => {
                "typed reduction has no construction-issued arithmetic contract"
            }
            Self::EmptyFirstProductDomain { .. } => {
                "first-product matrix reduction requires a nonempty inner domain"
            }
            Self::IncompleteCallOutput { .. } => {
                "pure-call body does not define every output exactly once"
            }
            Self::DuplicateCallIdentity { .. } => "pure-call semantic identity was already issued",
            Self::UninitializedSlot { .. } => {
                "slot load is not dominated by an external or stored definition"
            }
        };
        formatter.write_str(message)
    }
}

impl std::error::Error for SolveProgramConstructionError {}
