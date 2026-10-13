//! Adapter of `rumoca_core::modelica_integer_power` to the existing checked
//! fold vocabulary (SPEC_0040 SOLVE-C86). No exponent enters a Real register.

use super::*;

impl<'program> ExpressionLowerer<'_, 'program, '_> {
    pub(super) fn integer_exponent_power(
        &mut self,
        base: solve::ProgramRegister<'program>,
        exponent: solve::ProgramRegister<'program>,
        at: rumoca_core::Span,
    ) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
        let base_shape = self.builder.value_type_of(base, at)?.dimensions().to_vec();
        let exponent_shape = self
            .builder
            .value_type_of(exponent, at)?
            .dimensions()
            .to_vec();
        if exponent_shape.is_empty() && base_shape.is_empty() {
            return scalar_exponent_power(self.builder, base, exponent, at);
        }
        if !base_shape.is_empty() && !exponent_shape.is_empty() && base_shape != exponent_shape {
            return Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra {
                provenance: at,
            });
        }
        let shape = if exponent_shape.is_empty() {
            &base_shape
        } else {
            &exponent_shape
        };
        let domain = rumoca_core::StructuredIndexDomain {
            binders: shape
                .iter()
                .enumerate()
                .map(|(id, extent)| rumoca_core::StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id,
                    display_name: format!("power_axis_{id}"),
                    lower: 1,
                    upper: i64::from(*extent),
                    step: 1,
                })
                .collect(),
        };
        self.builder.map(
            domain,
            &[base, exponent],
            solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile())),
            at,
            |builder, captures, binders, output| {
                let base = builder.load(captures[0], at)?;
                let exponent = builder.load(captures[1], at)?;
                let indices = binders
                    .iter()
                    .map(|slot| builder.load(*slot, at))
                    .collect::<Result<Vec<_>, _>>()?;
                let base = if base_shape.is_empty() {
                    base
                } else {
                    builder.project_element_dynamic(base, &indices, at)?
                };
                let exponent = if exponent_shape.is_empty() {
                    exponent
                } else {
                    builder.project_element_dynamic(exponent, &indices, at)?
                };
                let result = scalar_exponent_power(builder, base, exponent, at)?;
                builder.store(output, result, at)
            },
        )
    }
}

fn scalar_exponent_power<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    base: solve::ProgramRegister<'program>,
    exponent: solve::ProgramRegister<'program>,
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    let shape = builder.value_type_of(base, at)?.dimensions().to_vec();
    let one = builder.constant(solve::SolveValue::real(arithmetic_profile(), 1.0), at)?;
    let one = if shape.is_empty() {
        one
    } else {
        builder.fill(one, shape, at)?
    };
    let zero = integer_constant(builder, 0, at)?;
    let negative = builder.compare(solve::SolveCompareOperator::Less, exponent, zero, at)?;
    let reciprocal = builder.binary(solve::SolveBinaryOperator::Divide, one, base, at)?;
    let base = builder.select(negative, reciprocal, base, at)?;
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "integer_power_bit".into(),
            lower: 1,
            upper: i64::from(i64::BITS),
            step: 1,
        }],
    };
    let values = builder.fold_while(
        domain,
        &[base, one, exponent],
        &[],
        at,
        |builder, carried, _, _, outputs| {
            let exponent = builder.load(carried[2], at)?;
            let zero = integer_constant(builder, 0, at)?;
            let active =
                builder.compare(solve::SolveCompareOperator::NotEqual, exponent, zero, at)?;
            builder.store(outputs[0], active, at)
        },
        |builder, carried, _, _, outputs| {
            let base = builder.load(carried[0], at)?;
            let result = builder.load(carried[1], at)?;
            let exponent = builder.load(carried[2], at)?;
            let two = integer_constant(builder, 2, at)?;
            let zero = integer_constant(builder, 0, at)?;
            let remainder = builder.binary(
                solve::SolveBinaryOperator::IntegerRemainder,
                exponent,
                two,
                at,
            )?;
            let odd =
                builder.compare(solve::SolveCompareOperator::NotEqual, remainder, zero, at)?;
            let product = builder.binary(solve::SolveBinaryOperator::Multiply, result, base, at)?;
            let result = builder.select(odd, product, result, at)?;
            let base = builder.binary(solve::SolveBinaryOperator::Multiply, base, base, at)?;
            let exponent = builder.binary(
                solve::SolveBinaryOperator::IntegerQuotient,
                exponent,
                two,
                at,
            )?;
            builder.store(outputs[0], base, at)?;
            builder.store(outputs[1], result, at)?;
            builder.store(outputs[2], exponent, at)
        },
    )?;
    values
        .get(1)
        .copied()
        .ok_or(solve::SolveProgramConstructionError::InvalidFold { provenance: at })
}

fn integer_constant<'program>(
    builder: &mut solve::TypedProgramBuilder<'program>,
    value: i64,
    at: rumoca_core::Span,
) -> Result<solve::ProgramRegister<'program>, solve::SolveProgramConstructionError> {
    let value = solve::SolveValue::integer(arithmetic_profile(), value)
        .map_err(|_| solve::SolveProgramConstructionError::ProfileMismatch { provenance: at })?;
    builder.constant(value, at)
}
