//! Compact pointwise tangents over one arithmetic-profile-checked aggregate shape.

use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder};

pub(super) struct BroadcastTangent<T> {
    pub(super) aggregate: Directional<T>,
    pub(super) scalar: Directional<T>,
    pub(super) tangents: [T; 2],
    pub(super) primal: T,
    pub(super) operator: SolveBinaryOperator,
    pub(super) scalar_on_lhs: bool,
}

pub(super) fn binary_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    arguments: BroadcastTangent<ProgramRegister<'program>>,
    value_type: &SolveValueType,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let domain = pointwise_domain(value_type, builder.arithmetic, provenance)?;
    let has_aggregate_tangent = arguments.aggregate.tangent.is_some();
    let has_scalar_tangent = arguments.scalar.tangent.is_some();
    let mut inputs = vec![
        arguments.aggregate.primal,
        arguments.tangents[0],
        arguments.scalar.primal,
        arguments.tangents[1],
    ];
    if arguments.operator == SolveBinaryOperator::Power {
        inputs.push(arguments.primal);
    }
    builder.map(
        domain,
        &inputs,
        SolveValueType::scalar(value_type.element_type()),
        provenance,
        |builder, captures, binders, output| {
            let coordinates = binders
                .iter()
                .map(|binder| builder.load(*binder, provenance))
                .collect::<Result<Vec<_>, _>>()?;
            let aggregate = builder.load(captures[0], provenance)?;
            let aggregate = builder.project_element_dynamic(aggregate, &coordinates, provenance)?;
            let aggregate_tangent = builder.load(captures[1], provenance)?;
            let aggregate_tangent =
                builder.project_element_dynamic(aggregate_tangent, &coordinates, provenance)?;
            let scalar = builder.load(captures[2], provenance)?;
            let scalar_tangent = builder.load(captures[3], provenance)?;
            let aggregate = Directional {
                primal: aggregate,
                tangent: has_aggregate_tangent.then_some(aggregate_tangent),
            };
            let scalar = Directional {
                primal: scalar,
                tangent: has_scalar_tangent.then_some(scalar_tangent),
            };
            let (values, tangents) = if arguments.scalar_on_lhs {
                ([scalar, aggregate], [scalar_tangent, aggregate_tangent])
            } else {
                ([aggregate, scalar], [aggregate_tangent, scalar_tangent])
            };
            let zero = builder.constant(SolveValue::real(builder.arithmetic, 0.0), provenance)?;
            let tangent = match arguments.operator {
                SolveBinaryOperator::Divide => partials::quotient_tangent(
                    builder,
                    [values[0].primal, values[1].primal],
                    tangents,
                    zero,
                    provenance,
                )?,
                SolveBinaryOperator::Power => {
                    let primal = builder.load(captures[4], provenance)?;
                    let primal =
                        builder.project_element_dynamic(primal, &coordinates, provenance)?;
                    partials::power_tangent(builder, values, primal, tangents, zero, provenance)?
                }
                _ => {
                    return Err(SolveProgramConstructionError::InvalidTensorAlgebra { provenance });
                }
            };
            builder.store(output, tangent, provenance)
        },
    )
}

pub(super) fn logarithm_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    operand: ProgramRegister<'program>,
    tangent: ProgramRegister<'program>,
    base_ten: bool,
    value_type: &SolveValueType,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let domain = pointwise_domain(value_type, builder.arithmetic, provenance)?;
    builder.map(
        domain,
        &[operand, tangent],
        SolveValueType::scalar(value_type.element_type()),
        provenance,
        |builder, captures, binders, output| {
            let coordinates = binders
                .iter()
                .map(|binder| builder.load(*binder, provenance))
                .collect::<Result<Vec<_>, _>>()?;
            let operand = builder.load(captures[0], provenance)?;
            let operand = builder.project_element_dynamic(operand, &coordinates, provenance)?;
            let tangent = builder.load(captures[1], provenance)?;
            let tangent = builder.project_element_dynamic(tangent, &coordinates, provenance)?;
            let zero = builder.constant(SolveValue::real(builder.arithmetic, 0.0), provenance)?;
            let tangent =
                partials::logarithm_tangent(builder, operand, tangent, base_ten, zero, provenance)?;
            builder.store(output, tangent, provenance)
        },
    )
}

pub(super) fn pointwise_domain(
    value_type: &SolveValueType,
    arithmetic: SolveArithmeticProfile,
    provenance: Span,
) -> Result<StructuredIndexDomain, SolveProgramConstructionError> {
    let domain = StructuredIndexDomain {
        binders: value_type
            .dimensions()
            .iter()
            .enumerate()
            .map(|(id, extent)| StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id,
                display_name: format!("axis{id}"),
                lower: 1,
                upper: i64::from(*extent),
                step: 1,
            })
            .collect(),
    };
    require_map_domain(&domain, arithmetic, provenance)?;
    Ok(domain)
}
