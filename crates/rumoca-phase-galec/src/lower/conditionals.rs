//! Conditional lowering for [`ExpressionLowerer`].
//!
//! Kept apart from the scalar and aggregate dispatch so the materialization
//! rules for `if` branches, which decide whether a branch becomes a shared
//! prefix statement or an inline expression, read as one unit.

use super::*;

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    pub(super) fn lower_conditional_at(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        if self.materialize_function_values {
            self.conditional_depth += 1;
            let result = self.lower_materialized_conditional(operands, indices, scalar_type, span);
            self.conditional_depth -= 1;
            return result;
        }
        self.lower_conditional_branches(operands, indices, scalar_type, span)
    }

    fn lower_materialized_conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let name = gast::Name::ident(format!(
            "rumoca_{}_conditional_{}",
            self.temporary_namespace, self.temporary_counter
        ));
        self.temporary_counter += 1;
        self.temporary_locals.push(gast::VariableDeclaration {
            ty: gast::TypeRef::Primitive(scalar_type),
            name: name.clone(),
            dimensions: Vec::new(),
            range: gast::RangeAttributes::default(),
            span,
        });
        let conditional = MaterializedConditional {
            operands,
            indices,
            scalar_type,
            target: &name,
            activation_operands: conditional_activation_operands(operands),
            span,
        };
        let statements = self.lower_materialized_conditional_branch(&conditional, 0)?;
        self.pending_prefix_statements.extend(statements);
        Ok(gast::Expression::Ref(gast::Reference::local(name)))
    }

    fn lower_materialized_conditional_branch(
        &mut self,
        conditional: &MaterializedConditional<'_, 'dae>,
        ordinal: usize,
    ) -> Result<Vec<gast::Spanned<gast::Statement>>, GalecTargetError> {
        self.conditional_activation_path
            .push(ConditionalActivationKey {
                kind: ConditionalActivationKind::ConditionalScalar,
                operands: conditional.activation_operands.clone(),
                branch: u32::try_from(ordinal / 2).map_err(|_| {
                    GalecTargetError::LoweringInternal {
                        detail: "conditional branch exceeds the activation-key capacity".to_owned(),
                    }
                })?,
            });
        if ordinal + 1 == conditional.operands.len() {
            let start = self.pending_prefix_statements.len();
            let value = self.lower_at(
                conditional
                    .operands
                    .get(ordinal)
                    .expect("checked conditional fallback"),
                conditional.indices,
            );
            self.conditional_activation_path.pop();
            let value = value?;
            let mut body = self.pending_prefix_statements.split_off(start);
            body.push(gast::Spanned::new(
                gast::Statement::Assignment {
                    target: gast::Reference::local(conditional.target.clone()),
                    value: coerce(value, conditional.scalar_type, conditional.span)?,
                },
                conditional.span,
            ));
            return Ok(body);
        }

        let condition_start = self.pending_prefix_statements.len();
        let condition = self.lower(
            conditional
                .operands
                .get(ordinal)
                .expect("checked conditional branch condition"),
        );
        let condition = match condition {
            Ok(condition) => condition,
            Err(error) => {
                self.conditional_activation_path.pop();
                return Err(error);
            }
        };
        if let Err(error) = require_boolean(&condition, conditional.span) {
            self.conditional_activation_path.pop();
            return Err(error);
        }
        let mut statements = self.pending_prefix_statements.split_off(condition_start);

        let value_start = self.pending_prefix_statements.len();
        let value = self.lower_at(
            conditional
                .operands
                .get(ordinal + 1)
                .expect("checked conditional branch value"),
            conditional.indices,
        );
        self.conditional_activation_path.pop();
        let value = value?;
        let mut body = self.pending_prefix_statements.split_off(value_start);
        body.push(gast::Spanned::new(
            gast::Statement::Assignment {
                target: gast::Reference::local(conditional.target.clone()),
                value: coerce(value, conditional.scalar_type, conditional.span)?,
            },
            conditional.span,
        ));
        let else_body = self.lower_materialized_conditional_branch(conditional, ordinal + 2)?;
        statements.push(gast::Spanned::new(
            gast::Statement::If(gast::IfStatement {
                branches: vec![gast::IfBranch {
                    condition: gast::Condition::Expression(condition.expression),
                    body,
                    span: conditional.span,
                }],
                else_body: Some(else_body),
            }),
            conditional.span,
        ));
        Ok(statements)
    }

    fn lower_conditional_branches(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        indices: &[gast::Expression],
        scalar_type: gast::ScalarType,
        span: Span,
    ) -> Result<gast::Expression, GalecTargetError> {
        let mut branches = Vec::new();
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let condition =
                self.lower(operands.get(ordinal).expect("checked condition operand"))?;
            require_boolean(&condition, span)?;
            let value = self.lower_at(
                operands.get(ordinal + 1).expect("checked value operand"),
                indices,
            )?;
            branches.push((condition.expression, coerce(value, scalar_type, span)?));
        }
        let fallback = self.lower_at(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional fallback"),
            indices,
        )?;
        Ok(gast::Expression::If(gast::IfExpression::new(
            branches,
            coerce(fallback, scalar_type, span)?,
        )))
    }
}
