use rumoca_core::{DefId, Expression, FallibleExpressionRewriter, Reference, Span, Subscript};
use rustc_hash::FxHashSet;

use crate::{Context, FlattenError};

/// Materialize non-local constants in every function parameter shape.
///
/// Shape identity is semantic after Resolve: a reference is either one of the
/// function's own declarations or an exact external target. Rendered names are
/// retained only for diagnostics.
pub(super) fn materialize_function_shape_constants(
    function: &mut rumoca_core::Function,
    ctx: &Context,
) -> Result<(), FlattenError> {
    let local_def_ids = function
        .inputs
        .iter()
        .chain(function.outputs.iter())
        .chain(function.locals.iter())
        .filter_map(|parameter| parameter.def_id)
        .collect();
    let mut materializer = FunctionShapeConstantMaterializer {
        ctx,
        local_def_ids,
        expansion_stack: Vec::new(),
    };

    for parameter in function
        .inputs
        .iter_mut()
        .chain(function.outputs.iter_mut())
        .chain(function.locals.iter_mut())
    {
        let mut dimensions = parameter.dimensions().to_vec();
        for (axis, subscript) in parameter.shape_expr.iter_mut().enumerate() {
            materializer.materialize_subscript(subscript)?;
            if let Subscript::Index { value, .. } = subscript {
                let dimension = dimensions
                    .get_mut(axis)
                    .expect("function shape expressions and dimensions are constructed together");
                *dimension = *value;
            }
        }
        parameter.effective_type = rumoca_core::EffectiveType::new(
            parameter.effective_type.nominal_type(),
            parameter.effective_type.canonical_type(),
            dimensions,
        )
        .map_err(|error| {
            FlattenError::missing_resolved_class_metadata(
                &parameter.name,
                format!("materialized function shape: {error}"),
                parameter.span,
            )
        })?;
    }
    Ok(())
}

struct FunctionShapeConstantMaterializer<'a> {
    ctx: &'a Context,
    local_def_ids: FxHashSet<DefId>,
    expansion_stack: Vec<DefId>,
}

impl FunctionShapeConstantMaterializer<'_> {
    fn materialize_subscript(&mut self, subscript: &mut Subscript) -> Result<(), FlattenError> {
        if let Subscript::Expr { expr, span } = subscript {
            let rewritten = self.rewrite_expression(expr)?;
            if let Expression::Literal {
                value: rumoca_core::Literal::Integer(value),
                span,
            } = rewritten
            {
                *subscript = Subscript::Index { value, span };
            } else {
                **expr = rewritten;
                debug_assert_eq!(expr.span(), Some(*span));
            }
        }
        Ok(())
    }

    fn replacement_for(
        &self,
        target: DefId,
        rendered_name: &str,
        span: Span,
    ) -> Result<Expression, FlattenError> {
        self.ctx
            .constant_values_by_def_id
            .get(&target)
            .cloned()
            .ok_or_else(|| FlattenError::UnresolvedFlatReference {
                name: rendered_name.to_owned(),
                span,
            })
    }

    fn cycle_error(&self, target: DefId, rendered_name: &str, span: Span) -> FlattenError {
        let cycle_start = self
            .expansion_stack
            .iter()
            .position(|candidate| *candidate == target)
            .expect("cycle target must already be on the exact-identity stack");
        let cycle = self.expansion_stack[cycle_start..]
            .iter()
            .chain(std::iter::once(&target))
            .map(|def_id| {
                self.ctx
                    .target_def_names
                    .get(def_id)
                    .map(String::as_str)
                    .unwrap_or("<missing resolved declaration name>")
            })
            .collect::<Vec<_>>()
            .join(" -> ");
        FlattenError::cyclic_constant_binding(rendered_name, cycle, span)
    }
}

impl FallibleExpressionRewriter for FunctionShapeConstantMaterializer<'_> {
    type Error = FlattenError;

    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Expression, Self::Error> {
        let target = name
            .target_def_id()
            .ok_or_else(|| FlattenError::UnresolvedFlatReference {
                name: name.as_str().to_owned(),
                span,
            })?;
        let rewritten_subscripts = self.rewrite_subscripts(subscripts)?;

        if self.local_def_ids.contains(&target) {
            return Ok(Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span,
            });
        }
        if self.expansion_stack.contains(&target) {
            return Err(self.cycle_error(target, name.as_str(), span));
        }

        let replacement = self.replacement_for(target, name.as_str(), span)?;
        self.expansion_stack.push(target);
        let materialized = self.rewrite_expression(&replacement);
        let popped = self
            .expansion_stack
            .pop()
            .expect("constant expansion pushes before recursive rewriting");
        debug_assert_eq!(popped, target);
        let materialized = materialized?;

        if rewritten_subscripts.is_empty() {
            Ok(materialized.with_span(span))
        } else {
            Ok(Expression::Index {
                base: Box::new(materialized),
                subscripts: rewritten_subscripts,
                span,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BytePos, ComponentRefPart, ComponentReference, Function, Literal, SourceId};

    const SOURCE_NAME: &str = "<exact_function_shape_constants>";
    const SOURCE: &str = "Left.nState Right.nState";

    #[test]
    fn same_leaf_constants_materialize_by_exact_target_with_use_provenance() {
        let left_id = DefId::new(10);
        let right_id = DefId::new(20);
        let left_span = span_of("Left.nState");
        let right_span = span_of("Right.nState");
        let declaration_span = Span::new(
            SourceId::from_source_name(SOURCE_NAME),
            BytePos(1),
            BytePos(2),
        );
        let mut ctx = Context::new();
        ctx.constant_values_by_def_id.insert(
            left_id,
            Expression::Literal {
                value: Literal::Integer(4),
                span: declaration_span,
            },
        );
        ctx.constant_values_by_def_id.insert(
            right_id,
            Expression::Literal {
                value: Literal::Integer(9),
                span: declaration_span,
            },
        );

        let mut left = function_with_shape("Left.makeState", left_id, left_span);
        let mut right = function_with_shape("Right.makeState", right_id, right_span);
        materialize_function_shape_constants(&mut left, &ctx).expect("left shape materializes");
        materialize_function_shape_constants(&mut right, &ctx).expect("right shape materializes");

        assert_shape(&left, 4, left_span);
        assert_shape(&right, 9, right_span);
    }

    fn function_with_shape(name: &str, target: DefId, span: Span) -> Function {
        let reference = Reference::from_component_reference(
            ComponentReference::construct(
                false,
                span,
                vec![ComponentRefPart {
                    ident: "nState".to_string(),
                    span,
                    subs: Vec::new(),
                    def_id: target,
                }],
            )
            .expect("shape constant reference carries exact declaration identity"),
        );
        let shape = Expression::VarRef {
            name: reference,
            subscripts: Vec::new(),
            span,
        };
        let output = crate::test_support::integer_param("state", vec![0], span)
            .with_shape_expr(vec![Subscript::expr(Box::new(shape), span)]);
        let mut function = Function::new(name, span);
        function.add_output(output);
        function
    }

    fn assert_shape(function: &Function, extent: i64, span: Span) {
        let output = function.outputs.first().expect("function has output");
        assert_eq!(output.dimensions(), [extent]);
        assert!(matches!(
            output.shape_expr.as_slice(),
            [Subscript::Index {
                value,
                span: actual_span
            }] if *value == extent && *actual_span == span
        ));
    }

    fn span_of(needle: &str) -> Span {
        let start = SOURCE.find(needle).expect("fixture contains occurrence");
        Span::from_offsets(
            SourceId::from_source_name(SOURCE_NAME),
            start,
            start + needle.len(),
        )
    }
}
