//! EXPR-042 selects scalar Boolean values through the existing conditional owner.

use super::*;

pub(super) fn is_scalar_boolean_operation<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    operator: &OpBinary,
    lhs: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<bool, dae::DaeConstructionError> {
    if !matches!(operator, OpBinary::And | OpBinary::Or) {
        return Ok(false);
    }
    let lhs_type =
        construction.expressions(|expressions| expressions.value_type(lhs, provenance))?;
    // The checked And/Or vocabulary requires equal shapes. A valid RHS of a
    // scalar Boolean LHS must therefore also be scalar Boolean; validate its
    // actual issued type inside the selected arm rather than guessing rank.
    Ok(lhs_type == dae::ValueType::scalar(dae::ScalarType::Boolean))
}

pub(super) fn lower_logical_value<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    operator: &OpBinary,
    lhs: dae::ExprId<'dae>,
    rhs: &Expression,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let rhs_on_true = matches!(operator, OpBinary::And);
    model_conditionals::lower_value_conditional(
        construction,
        lhs,
        provenance,
        symbols.function_body.is_some(),
        |construction| {
            lower_logical_arm(
                construction,
                symbols,
                binders,
                rhs,
                rhs_on_true,
                true,
                provenance,
            )
        },
        |construction| {
            lower_logical_arm(
                construction,
                symbols,
                binders,
                rhs,
                !rhs_on_true,
                false,
                provenance,
            )
        },
    )
}

fn lower_logical_arm<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    symbols: LoweringSymbols<'_, 'dae>,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
    rhs: &Expression,
    selected: bool,
    constant: bool,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if !selected {
        return construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Boolean(constant))
        });
    }
    let rhs = lower_expression_scoped(construction, symbols, binders, rhs, None)?;
    require_scalar_boolean_operand(construction, rhs, provenance)
}

fn require_scalar_boolean_operand<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    rhs: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    // Both operands retain the checked binary vocabulary's type and shape
    // requirements, including for an unselected source operand.
    let ty = construction.expressions(|expressions| expressions.value_type(rhs, provenance))?;
    if !ty.dimensions().is_empty() {
        return Err(dae::DaeConstructionError::ShapeMismatch {
            span: provenance.span(),
        });
    }
    if ty != dae::ValueType::scalar(dae::ScalarType::Boolean) {
        return Err(dae::DaeConstructionError::TypeMismatch {
            expected: dae::ScalarType::Boolean,
            found: ty.scalar_type(),
            span: provenance.span(),
        });
    }
    Ok(rhs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn boolean_tensor_shape_one_keeps_the_checked_binary_owner() {
        let mut sources = SourceMap::new();
        let source = sources.add("BooleanPolicy.mo", "true and true");
        let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 13)).unwrap();
        dae::Dae::construct(sources, |construction| {
            for operator in [OpBinary::And, OpBinary::Or] {
                assert_tensor_operator(construction, operator, provenance)?;
            }
            Ok(())
        })
        .unwrap();
    }

    fn assert_tensor_operator<'dae>(
        construction: &mut dae::DaeConstruction<'dae>,
        operator: OpBinary,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        let (scalar, tensor) = construction.expressions(|expressions| {
            let scalar = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Boolean(true))?;
            let tensor = expressions.at(provenance).array([scalar])?;
            Ok((scalar, tensor))
        })?;
        assert!(is_scalar_boolean_operation(
            construction,
            &operator,
            scalar,
            provenance
        )?);
        assert!(!is_scalar_boolean_operation(
            construction,
            &operator,
            tensor,
            provenance
        )?);
        let binary = construction.expressions(|expressions| {
            expressions
                .at(provenance)
                .binary(binary_operator(&operator), tensor, tensor)
        })?;
        let ty =
            construction.expressions(|expressions| expressions.value_type(binary, provenance))?;
        assert_eq!(ty, dae::ValueType::array(dae::ScalarType::Boolean, [1]));
        for (lhs, rhs) in [(scalar, tensor), (tensor, scalar)] {
            let error = construction
                .expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .binary(binary_operator(&operator), lhs, rhs)
                })
                .expect_err("Boolean and/or has no scalar/tensor broadcast");
            assert!(matches!(
                error,
                dae::DaeConstructionError::ShapeMismatch { .. }
            ));
        }
        let error = require_scalar_boolean_operand(construction, tensor, provenance)
            .expect_err("the scalar-LHS selected RHS path rejects rank one");
        assert!(matches!(
            error,
            dae::DaeConstructionError::ShapeMismatch { .. }
        ));

        Ok(())
    }
}
