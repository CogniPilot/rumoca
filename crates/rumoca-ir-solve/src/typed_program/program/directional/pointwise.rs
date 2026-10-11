//! Compact pointwise tangents over one arithmetic-profile-checked aggregate shape.

use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder};

pub(super) fn quotient_tangent<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    aggregate: Directional<ProgramRegister<'program>>,
    scalar: Directional<ProgramRegister<'program>>,
    tangents: [ProgramRegister<'program>; 2],
    scalar_on_lhs: bool,
    value_type: &SolveValueType,
    provenance: Span,
) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
    let domain = pointwise_domain(value_type, builder.arithmetic, provenance)?;
    builder.map(
        domain,
        &[aggregate.primal, tangents[0], scalar.primal, tangents[1]],
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
            let (primals, tangents) = if scalar_on_lhs {
                ([scalar, aggregate], [scalar_tangent, aggregate_tangent])
            } else {
                ([aggregate, scalar], [aggregate_tangent, scalar_tangent])
            };
            let zero = builder.constant(SolveValue::real(builder.arithmetic, 0.0), provenance)?;
            let tangent = partials::quotient_tangent(builder, primals, tangents, zero, provenance)?;
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
