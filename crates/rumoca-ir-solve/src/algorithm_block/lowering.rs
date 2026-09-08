//! Scalar declaration/literal facts shared by Algorithm Block construction.

use rumoca_ir_galec::ast::ScalarType;
use rumoca_ir_galec::package::{
    AlgorithmCodeArithmeticProfile, AlgorithmCodeDeclarationClass, AlgorithmCodeEvaluatedLiteral,
    AlgorithmCodeEvaluatedScalar, AlgorithmCodeEvaluatedStart, AlgorithmCodeExpressionKind,
    AlgorithmCodeIntegerFormat, AlgorithmCodeRealFormat, ExpressionSubject, SemanticProvenance,
};

use crate::{
    SolveArithmeticProfile, SolveIntegerDomain, SolveProgramConstructionError, SolveRealFormat,
    SolveScalarType, SolveSlotAccess, SolveTypeConstructionError, SolveValue, SolveValueType,
};

use super::root::{
    SolveAlgorithmBlockConstructionError, SolveAlgorithmBlockStorageClass,
    SolveAlgorithmMethodKind, SolveDeclarationStartValue, UnsupportedTensorInitializationPlan,
};

pub(super) const fn solve_arithmetic_profile(
    source: AlgorithmCodeArithmeticProfile,
) -> SolveArithmeticProfile {
    let real = match source.source_real() {
        AlgorithmCodeRealFormat::Binary32 => SolveRealFormat::Binary32,
        AlgorithmCodeRealFormat::Binary64 => SolveRealFormat::Binary64,
    };
    let integers = match source.source_integer() {
        AlgorithmCodeIntegerFormat::I8 => SolveIntegerDomain::I8,
        AlgorithmCodeIntegerFormat::I16 => SolveIntegerDomain::I16,
        AlgorithmCodeIntegerFormat::I32 => SolveIntegerDomain::I32,
        AlgorithmCodeIntegerFormat::I64 => SolveIntegerDomain::FULL,
    };
    SolveArithmeticProfile::construct(real, integers, source.real_matrix_multiply())
}

pub(super) fn solve_scalar_literal(
    subject: ExpressionSubject<'_, '_>,
    arithmetic: SolveArithmeticProfile,
) -> Result<SolveValue, SolveAlgorithmBlockConstructionError> {
    if !subject.value().extents().is_empty() {
        return Err(
            SolveAlgorithmBlockConstructionError::UnsupportedExpressionShape {
                provenance: subject.provenance(),
            },
        );
    }
    let value = match subject.kind() {
        AlgorithmCodeExpressionKind::Boolean(value) => SolveValue::boolean(value),
        AlgorithmCodeExpressionKind::Integer(value) => SolveValue::integer(arithmetic, value)
            .map_err(|source| SolveAlgorithmBlockConstructionError::Literal {
                source,
                provenance: subject.provenance(),
            })?,
        AlgorithmCodeExpressionKind::RealBits(bits) => {
            SolveValue::real(arithmetic, f64::from_bits(bits))
        }
        kind => {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedExpression {
                    kind,
                    provenance: subject.provenance(),
                },
            );
        }
    };
    let source_type = solve_value_type(subject.value(), arithmetic).map_err(|source| {
        SolveAlgorithmBlockConstructionError::ValueType {
            source,
            provenance: subject.provenance(),
        }
    })?;
    if value.value_type() != &source_type {
        return Err(SolveAlgorithmBlockConstructionError::LiteralTypeMismatch {
            provenance: subject.provenance(),
        });
    }
    Ok(value)
}

pub(super) const fn has_external_initialization_authority(
    class: AlgorithmCodeDeclarationClass,
) -> bool {
    match class {
        AlgorithmCodeDeclarationClass::Input | AlgorithmCodeDeclarationClass::TunableParameter => {
            true
        }
        AlgorithmCodeDeclarationClass::Output
        | AlgorithmCodeDeclarationClass::DependentParameter
        | AlgorithmCodeDeclarationClass::Constant
        | AlgorithmCodeDeclarationClass::PersistentState
        | AlgorithmCodeDeclarationClass::CompartmentDependentParameter
        | AlgorithmCodeDeclarationClass::CompartmentConstant
        | AlgorithmCodeDeclarationClass::CompartmentPersistentState
        | AlgorithmCodeDeclarationClass::MethodLocal
        | AlgorithmCodeDeclarationClass::FunctionInput
        | AlgorithmCodeDeclarationClass::FunctionOutput
        | AlgorithmCodeDeclarationClass::FunctionLocal => false,
    }
}

pub(super) fn evaluated_start_matches_expression(
    evaluated: AlgorithmCodeEvaluatedStart,
    expression: AlgorithmCodeEvaluatedLiteral,
) -> bool {
    match (evaluated, expression) {
        (
            AlgorithmCodeEvaluatedStart::Scalar(expected),
            AlgorithmCodeEvaluatedLiteral::Scalar(actual),
        )
        | (
            AlgorithmCodeEvaluatedStart::UniformTensorFill(expected),
            AlgorithmCodeEvaluatedLiteral::UniformTensorFill(actual),
        ) => expected == actual,
        (AlgorithmCodeEvaluatedStart::Missing, _)
        | (AlgorithmCodeEvaluatedStart::UnsupportedScalarExpression, _)
        | (AlgorithmCodeEvaluatedStart::UnsupportedNonUniformTensor, _)
        | (AlgorithmCodeEvaluatedStart::UnsupportedSymbolicTensor, _)
        | (AlgorithmCodeEvaluatedStart::Scalar(_), _)
        | (AlgorithmCodeEvaluatedStart::UniformTensorFill(_), _) => false,
    }
}

pub(super) fn solve_declaration_start(
    evaluated: AlgorithmCodeEvaluatedStart,
    target: &SolveValueType,
    arithmetic: SolveArithmeticProfile,
    provenance: SemanticProvenance,
) -> Result<SolveDeclarationStartValue, SolveAlgorithmBlockConstructionError> {
    let (scalar, aggregate) = match evaluated {
        AlgorithmCodeEvaluatedStart::Scalar(scalar) => (scalar, false),
        AlgorithmCodeEvaluatedStart::UniformTensorFill(scalar) => (scalar, true),
        AlgorithmCodeEvaluatedStart::UnsupportedNonUniformTensor => {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedTensorInitializationPlan {
                    plan: UnsupportedTensorInitializationPlan::NonUniformLiteral,
                    provenance,
                },
            );
        }
        AlgorithmCodeEvaluatedStart::UnsupportedSymbolicTensor => {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedTensorInitializationPlan {
                    plan: UnsupportedTensorInitializationPlan::Symbolic,
                    provenance,
                },
            );
        }
        AlgorithmCodeEvaluatedStart::Missing
        | AlgorithmCodeEvaluatedStart::UnsupportedScalarExpression => {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedEvaluatedDeclarationStart {
                    provenance,
                },
            );
        }
    };
    if aggregate == target.dimensions().is_empty() {
        return Err(SolveAlgorithmBlockConstructionError::LiteralTypeMismatch { provenance });
    }
    let value = match scalar {
        AlgorithmCodeEvaluatedScalar::RealBits(bits) => {
            SolveValue::real(arithmetic, f64::from_bits(bits))
        }
        AlgorithmCodeEvaluatedScalar::Integer(value) => {
            SolveValue::integer(arithmetic, value).map_err(|source| {
                SolveAlgorithmBlockConstructionError::Literal { source, provenance }
            })?
        }
        AlgorithmCodeEvaluatedScalar::Boolean(value) => SolveValue::boolean(value),
    };
    let value = coerce_start_scalar(value, target, arithmetic, provenance)?;
    Ok(if aggregate {
        SolveDeclarationStartValue::UniformTensorFill(value)
    } else {
        SolveDeclarationStartValue::Scalar(value)
    })
}

pub(super) fn solve_uniform_tensor_literal(
    subject: ExpressionSubject<'_, '_>,
    arithmetic: SolveArithmeticProfile,
) -> Result<SolveDeclarationStartValue, SolveAlgorithmBlockConstructionError> {
    let evaluated = match subject.evaluated_literal() {
        AlgorithmCodeEvaluatedLiteral::UniformTensorFill(scalar) => {
            AlgorithmCodeEvaluatedStart::UniformTensorFill(scalar)
        }
        AlgorithmCodeEvaluatedLiteral::NonUniformTensor => {
            AlgorithmCodeEvaluatedStart::UnsupportedNonUniformTensor
        }
        AlgorithmCodeEvaluatedLiteral::Symbolic | AlgorithmCodeEvaluatedLiteral::Scalar(_) => {
            AlgorithmCodeEvaluatedStart::UnsupportedSymbolicTensor
        }
    };
    let value_type = solve_value_type(subject.value(), arithmetic).map_err(|source| {
        SolveAlgorithmBlockConstructionError::ValueType {
            source,
            provenance: subject.provenance(),
        }
    })?;
    solve_declaration_start(evaluated, &value_type, arithmetic, subject.provenance())
}

fn coerce_start_scalar(
    value: SolveValue,
    target: &SolveValueType,
    arithmetic: SolveArithmeticProfile,
    provenance: SemanticProvenance,
) -> Result<SolveValue, SolveAlgorithmBlockConstructionError> {
    if value.value_type().element_type() == target.element_type() {
        return Ok(value);
    }
    if matches!(target.element_type(), SolveScalarType::Real { .. })
        && let crate::SolveValueKind::Integer(integer) = value.kind()
    {
        return Ok(SolveValue::real(arithmetic, integer as f64));
    }
    Err(SolveAlgorithmBlockConstructionError::LiteralTypeMismatch { provenance })
}

pub(super) fn program_span(
    provenance: SemanticProvenance,
) -> Result<rumoca_core::Span, SolveProgramConstructionError> {
    match provenance {
        SemanticProvenance::Exact(span) | SemanticProvenance::NearestStatement(span)
            if !span.is_dummy() =>
        {
            Ok(span)
        }
        SemanticProvenance::Exact(_)
        | SemanticProvenance::NearestStatement(_)
        | SemanticProvenance::Generated(_) => Err(SolveProgramConstructionError::MissingProvenance),
    }
}

pub(super) fn solve_declaration_storage(
    class: AlgorithmCodeDeclarationClass,
    provenance: SemanticProvenance,
) -> Result<(SolveAlgorithmBlockStorageClass, SolveSlotAccess), SolveAlgorithmBlockConstructionError>
{
    let storage = match class {
        AlgorithmCodeDeclarationClass::Input => (
            SolveAlgorithmBlockStorageClass::Input,
            SolveSlotAccess::ReadOnly,
        ),
        AlgorithmCodeDeclarationClass::Output => (
            SolveAlgorithmBlockStorageClass::Output,
            SolveSlotAccess::ReadWrite,
        ),
        AlgorithmCodeDeclarationClass::TunableParameter => (
            SolveAlgorithmBlockStorageClass::TunableParameter,
            SolveSlotAccess::ReadOnly,
        ),
        AlgorithmCodeDeclarationClass::DependentParameter => (
            SolveAlgorithmBlockStorageClass::CalculatedParameter,
            SolveSlotAccess::ReadWrite,
        ),
        AlgorithmCodeDeclarationClass::Constant => (
            SolveAlgorithmBlockStorageClass::Constant,
            SolveSlotAccess::ReadOnly,
        ),
        AlgorithmCodeDeclarationClass::PersistentState => (
            SolveAlgorithmBlockStorageClass::PersistentState,
            SolveSlotAccess::ReadWrite,
        ),
        AlgorithmCodeDeclarationClass::CompartmentDependentParameter
        | AlgorithmCodeDeclarationClass::CompartmentConstant
        | AlgorithmCodeDeclarationClass::CompartmentPersistentState
        | AlgorithmCodeDeclarationClass::MethodLocal
        | AlgorithmCodeDeclarationClass::FunctionInput
        | AlgorithmCodeDeclarationClass::FunctionOutput
        | AlgorithmCodeDeclarationClass::FunctionLocal => {
            return Err(
                SolveAlgorithmBlockConstructionError::UnsupportedDeclarationClass {
                    class,
                    provenance,
                },
            );
        }
    };
    Ok(storage)
}

pub(super) fn solve_value_type(
    value: rumoca_ir_galec::package::ExactValueShape<'_>,
    arithmetic: SolveArithmeticProfile,
) -> Result<SolveValueType, SolveTypeConstructionError> {
    let scalar = match value.scalar() {
        ScalarType::Real => SolveScalarType::real(arithmetic),
        ScalarType::Integer => SolveScalarType::integer(arithmetic),
        ScalarType::Boolean => SolveScalarType::Boolean,
    };
    if value.extents().is_empty() {
        return Ok(SolveValueType::scalar(scalar));
    }
    SolveValueType::tensor(scalar, value.extents().to_vec())
}

pub(super) const fn method_index(kind: SolveAlgorithmMethodKind) -> usize {
    match kind {
        SolveAlgorithmMethodKind::Startup => 0,
        SolveAlgorithmMethodKind::Recalibrate => 1,
        SolveAlgorithmMethodKind::DoStep => 2,
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::RealMatrixMultiplySemantics;
    use rumoca_ir_galec::package::{
        AlgorithmCodeArithmeticProfile, AlgorithmCodeIntegerFormat, AlgorithmCodeRealFormat,
        GeneratedSubjectKind, SemanticProvenance,
    };

    use super::{program_span, solve_arithmetic_profile};

    #[test]
    fn package_source_specializations_totally_determine_solve_arithmetic() {
        let matrix = RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero;
        for (source_real, solve_real) in [
            (
                AlgorithmCodeRealFormat::Binary32,
                crate::SolveRealFormat::Binary32,
            ),
            (
                AlgorithmCodeRealFormat::Binary64,
                crate::SolveRealFormat::Binary64,
            ),
        ] {
            for (source_integer, solve_integers) in [
                (
                    AlgorithmCodeIntegerFormat::I8,
                    crate::SolveIntegerDomain::I8,
                ),
                (
                    AlgorithmCodeIntegerFormat::I16,
                    crate::SolveIntegerDomain::I16,
                ),
                (
                    AlgorithmCodeIntegerFormat::I32,
                    crate::SolveIntegerDomain::I32,
                ),
                (
                    AlgorithmCodeIntegerFormat::I64,
                    crate::SolveIntegerDomain::FULL,
                ),
            ] {
                assert_eq!(
                    solve_arithmetic_profile(AlgorithmCodeArithmeticProfile::construct(
                        source_real,
                        source_integer,
                        matrix,
                    )),
                    crate::SolveArithmeticProfile::construct(solve_real, solve_integers, matrix),
                );
            }
        }
    }

    #[test]
    fn source_free_subjects_cannot_acquire_typed_program_provenance() {
        assert_eq!(
            program_span(SemanticProvenance::Generated(
                GeneratedSubjectKind::Expression,
            )),
            Err(crate::SolveProgramConstructionError::MissingProvenance)
        );
    }
}
