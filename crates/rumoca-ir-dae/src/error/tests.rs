use std::error::Error as _;

use rumoca_core::{SourceId, Span};

use super::*;
use crate::DaeGeneration;

struct Case {
    error: DaeConstructionError,
    message: &'static str,
    span: Option<Span>,
}

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("dae-error-tests.mo"), 2, 5)
}

fn assert_cases(cases: impl IntoIterator<Item = Case>) {
    for case in cases {
        assert_eq!(case.error.to_string(), case.message);
        assert_eq!(case.error.source_span(), case.span);
    }
}

#[test]
fn provenance_and_foundation_messages_are_exact() {
    let at = span();
    assert_cases([
        Case {
            error: DaeConstructionError::MissingProvenance {
                origin: DaeProvenanceOrigin::Source,
                attempted_span: at,
            },
            message: "missing source provenance for source DAE object",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::MissingProvenance {
                origin: DaeProvenanceOrigin::Generated(DaeGeneration::ConnectionEquation),
                attempted_span: at,
            },
            message: "missing source provenance for generated DAE object (connection equation)",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::UnknownSource { span: at },
            message: "DAE provenance references an unknown source: Span { source: SourceId(1350506341627150748), start: BytePos(2), end: BytePos(5) }",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidSourceRange {
                span: at,
                source_len: 9,
            },
            message: "DAE provenance range Span { source: SourceId(1350506341627150748), start: BytePos(2), end: BytePos(5) } is invalid for source length 9",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidEffectiveTypeId {
                type_id: TypeId::new(3),
                span: at,
            },
            message: "invalid effective Flat type identity TypeId(3)",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidDomain {
                source: StructuredIndexDomainError::ScalarCountOverflow,
                span: at,
            },
            message: "invalid structured DAE domain: structured domain scalar count overflows usize",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidClockLattice {
                source: ClockLatticeErrorKind::NonPositivePeriod,
                span: at,
            },
            message: "invalid exact DAE clock value: clock interval must be strictly positive (MLS §16.3 interval > 0)",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::CapacityExceeded {
                arena: "expression arena",
                attempted_index: 4_294_967_296,
                span: at,
            },
            message: "expression arena exceeded its u32 identity capacity at 4294967296",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::UnknownId {
                kind: "expression",
                index: 12,
                span: at,
            },
            message: "unknown expression identity 12",
            span: Some(at),
        },
    ]);
}

#[test]
fn expression_and_numeric_messages_are_exact() {
    let at = span();
    assert_cases([
        Case {
            error: DaeConstructionError::TypeMismatch {
                expected: ScalarType::Real,
                found: ScalarType::Integer,
                span: at,
            },
            message: "expression type mismatch: expected Real, found Integer",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::ShapeMismatch { span: at },
            message: "expression shape mismatch",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::ExpectedScalar { span: at },
            message: "expected a scalar expression",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::ExpectedNumeric {
                found: ScalarType::String,
                span: at,
            },
            message: "expected a numeric expression, found String",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::ExpectedPrimitiveRelation { span: at },
            message: "expected a primitive relational expression",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidArity {
                expected: 2,
                found: 3,
                span: at,
            },
            message: "invalid expression arity: expected 2, found 3",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::EmptyArray { span: at },
            message: "empty array needs an explicit type",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::ZeroRangeStep { span: at },
            message: "range step cannot be zero",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::RangeExtentOverflow { span: at },
            message: "range extent exceeds the DAE u32 domain",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidArrayExtent { span: at },
            message: "array extent must be a nonnegative literal Integer",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidPositiveParameter { span: at },
            message: "expected a finite, strictly-positive parameter expression",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::NonStaticDiscontinuity {
                operator: "floor",
                span: at,
            },
            message: "discontinuous builtin `floor` requires statically computable operands until it has a checked event owner",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::UndefinedBuiltinDomain {
                operator: "sqrt",
                span: at,
            },
            message: "builtin `sqrt` operands are outside the defined numeric domain",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidSubscript { span: at },
            message: "invalid array subscript",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidEnumerationOrdinal {
                ordinal: 0,
                span: at,
            },
            message: "invalid one-based enumeration ordinal 0",
            span: Some(at),
        },
    ]);
}

#[test]
fn ownership_messages_are_exact() {
    let at = span();
    assert_cases([
        Case {
            error: DaeConstructionError::InvalidBinderScope {
                expected_domain: Some(4),
                found_domain: 7,
                span: at,
            },
            message: "domain binder from 7 cannot be used in domain 4",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidBinderScope {
                expected_domain: None,
                found_domain: 7,
                span: at,
            },
            message: "domain binder from 7 escaped its structured owner",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(4),
                found_function: 7,
                span: at,
            },
            message: "parameter from function 7 cannot be used in function 4",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidFunctionScope {
                expected_function: None,
                found_function: 7,
                span: at,
            },
            message: "parameter from function 7 escaped its function owner",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidFunctionValueRead {
                value: 2,
                expected_definition: Some(3),
                found_definition: 5,
                span: at,
            },
            message: "function value 2 reads definition 5, expected Some(3)",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidFunctionValueRead {
                value: 2,
                expected_definition: None,
                found_definition: 5,
                span: at,
            },
            message: "function value 2 reads definition 5, expected None",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidFunctionCoordinate {
                coordinate: "time",
                span: at,
            },
            message: "model coordinate `time` cannot be captured by a pure function",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::InvalidVariableRole {
                name: VarName::new("plant.x"),
                span: at,
            },
            message: "variable `plant.x` has the wrong DAE coordinate role",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::MissingClockOwnership {
                variable: 3,
                clock: 9,
                span: at,
            },
            message: "variable identity 3 is not owned by clock identity 9",
            span: Some(at),
        },
    ]);
}

#[test]
fn definition_messages_are_exact() {
    let at = span();
    assert_cases([
        Case {
            error: DaeConstructionError::DuplicateDefinition {
                kind: "state",
                index: 8,
                span: at,
            },
            message: "duplicate state definition for identity 8",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::DuplicateKey {
                kind: "variable",
                key: "plant.x".to_owned(),
                span: at,
            },
            message: "duplicate variable key `plant.x`",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::UnissuedDiscreteDependency {
                target: 6,
                dependency: 7,
                span: at,
            },
            message: "B.1c target identity 6 reads not-yet-issued current discrete value 7",
            span: Some(at),
        },
        Case {
            error: DaeConstructionError::IncompleteDefinition {
                kind: "parameter",
                index: 11,
                span: at,
            },
            message: "missing parameter definition for identity 11",
            span: Some(at),
        },
    ]);
}

#[test]
fn source_free_messages_are_exact_and_unspanned() {
    assert_cases([
        Case {
            error: DaeConstructionError::DuplicateTopology {
                kind: "B.1c topology",
                span: None,
            },
            message: "duplicate B.1c topology construction",
            span: None,
        },
        Case {
            error: DaeConstructionError::InvalidSchemaVersion {
                expected: 11,
                found: 10,
            },
            message: "unsupported DAE schema version 10; expected 11",
            span: None,
        },
        Case {
            error: DaeConstructionError::MalformedWire {
                column: "expressions",
            },
            message: "malformed DAE wire column `expressions`",
            span: None,
        },
    ]);
}

#[test]
fn structured_and_clock_causes_are_chained() {
    let domain = DaeConstructionError::InvalidDomain {
        source: StructuredIndexDomainError::ScalarCountOverflow,
        span: span(),
    };
    assert_eq!(
        domain.source().map(ToString::to_string),
        Some("structured domain scalar count overflows usize".to_owned())
    );

    let clock = DaeConstructionError::InvalidClockLattice {
        source: ClockLatticeErrorKind::NonPositivePeriod,
        span: span(),
    };
    assert_eq!(
        clock.source().map(ToString::to_string),
        Some("clock interval must be strictly positive (MLS §16.3 interval > 0)".to_owned())
    );
    assert!(
        DaeConstructionError::ExpectedScalar { span: span() }
            .source()
            .is_none()
    );
}
