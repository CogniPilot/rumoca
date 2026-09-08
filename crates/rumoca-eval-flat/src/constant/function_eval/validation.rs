//! Structural validation for function IR before interpretation.

use rumoca_core::{
    ComponentRefPart, ComponentReference, Function, FunctionParam, Span, Statement, StatementBlock,
    Subscript,
};

use super::super::EvalContext;
use super::super::errors::EvalError;
use super::super::expr_eval::validate_semantic_expression_in_context;

pub(super) fn validate_function_semantic_ir(
    func: &Function,
    ctx: &EvalContext,
    fallback_span: Span,
) -> Result<(), EvalError> {
    validate_declarations(func, ctx)?;
    FunctionSemanticValidator {
        function: func,
        ctx,
        fallback_span,
        loop_binders: Vec::new(),
        loop_depth: 0,
    }
    .validate_statements(&func.body)
}

fn validate_declarations(func: &Function, ctx: &EvalContext) -> Result<(), EvalError> {
    let mut names = rustc_hash::FxHashSet::default();
    let mut identities = rustc_hash::FxHashSet::default();
    for param in func.inputs.iter().chain(&func.outputs).chain(&func.locals) {
        let Some(def_id) = param.def_id else {
            return Err(EvalError::InvalidSemanticIr {
                reason: format!("function declaration `{}` has no DefId", param.name),
                span: param.span,
            });
        };
        if !names.insert(param.name.as_str()) {
            return Err(EvalError::InvalidSemanticIr {
                reason: format!(
                    "function declaration name `{}` is issued more than once",
                    param.name
                ),
                span: param.span,
            });
        }
        if !identities.insert(def_id) {
            return Err(EvalError::InvalidSemanticIr {
                reason: format!("function declaration DefId {def_id} is issued more than once"),
                span: param.span,
            });
        }
        for expression in [&param.default, &param.min, &param.max]
            .into_iter()
            .flatten()
        {
            validate_semantic_expression_in_context(expression, ctx, param.span)?;
        }
        validate_subscripts(&param.shape_expr, ctx, param.span)?;
    }
    Ok(())
}

struct FunctionSemanticValidator<'a> {
    function: &'a Function,
    ctx: &'a EvalContext,
    fallback_span: Span,
    loop_binders: Vec<String>,
    loop_depth: usize,
}

impl FunctionSemanticValidator<'_> {
    fn validate_statements(&mut self, statements: &[Statement]) -> Result<(), EvalError> {
        for statement in statements {
            self.validate_statement(statement)?;
        }
        Ok(())
    }

    fn validate_statement(&mut self, statement: &Statement) -> Result<(), EvalError> {
        let span = statement.source_span().unwrap_or(self.fallback_span);
        match statement {
            Statement::Empty { .. } => self.invalid(
                "empty statement is not valid in a semantic function body",
                span,
            ),
            Statement::Assignment { comp, value, .. } => {
                self.validate_writable_target(comp, span)?;
                self.validate_expression(value, span)
            }
            Statement::Return { .. } => Ok(()),
            Statement::Break { .. } => self.validate_break(span),
            Statement::For {
                indices, equations, ..
            } => self.validate_for(indices, equations, span),
            Statement::While { block, .. } => self.validate_loop_block(block, span),
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.validate_if(cond_blocks, else_block, span),
            Statement::When { .. } => {
                self.invalid("when statement is not permitted in a function body", span)
            }
            Statement::FunctionCall { .. } => self.validate_function_call(statement, span),
            Statement::Reinit { .. } => {
                self.invalid("reinit statement is not permitted in a function body", span)
            }
            Statement::Assert {
                condition,
                message,
                level,
                ..
            } => self.validate_assert(condition, message, level.as_deref(), span),
        }
    }

    fn validate_expression(
        &self,
        expression: &rumoca_core::Expression,
        span: Span,
    ) -> Result<(), EvalError> {
        validate_semantic_expression_in_context(expression, self.ctx, span)
    }

    fn validate_break(&self, span: Span) -> Result<(), EvalError> {
        if self.loop_depth == 0 {
            return self.invalid("break statement is outside a for or while loop", span);
        }
        Ok(())
    }

    fn validate_if(
        &mut self,
        blocks: &[StatementBlock],
        else_block: &Option<Vec<Statement>>,
        span: Span,
    ) -> Result<(), EvalError> {
        for block in blocks {
            self.validate_statement_block(block, span)?;
        }
        if let Some(statements) = else_block {
            self.validate_statements(statements)?;
        }
        Ok(())
    }

    fn validate_for(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        body: &[Statement],
        span: Span,
    ) -> Result<(), EvalError> {
        if indices.is_empty() {
            return self.invalid("for statement has no loop index", span);
        }
        let old_binder_count = self.loop_binders.len();
        for index in indices {
            self.validate_expression(&index.range, span)?;
            self.loop_binders.push(index.ident.clone());
        }
        self.loop_depth += 1;
        let result = self.validate_statements(body);
        self.loop_depth -= 1;
        self.loop_binders.truncate(old_binder_count);
        result
    }

    fn validate_loop_block(&mut self, block: &StatementBlock, span: Span) -> Result<(), EvalError> {
        self.validate_expression(&block.cond, span)?;
        self.loop_depth += 1;
        let result = self.validate_statements(&block.stmts);
        self.loop_depth -= 1;
        result
    }

    fn validate_statement_block(
        &mut self,
        block: &StatementBlock,
        span: Span,
    ) -> Result<(), EvalError> {
        self.validate_expression(&block.cond, span)?;
        self.validate_statements(&block.stmts)
    }

    fn validate_function_call(
        &mut self,
        statement: &Statement,
        span: Span,
    ) -> Result<(), EvalError> {
        let Statement::FunctionCall {
            comp,
            args,
            outputs,
            ..
        } = statement
        else {
            return self.invalid(
                "function-call validator received a non-call statement",
                span,
            );
        };
        let name = comp.as_str();
        let result_arity = self.validate_static_call_shape(statement, comp, span)?;
        if let Some(result_arity) = result_arity
            && outputs.len() > result_arity
        {
            return self.invalid(
                format!(
                    "call to `{}` has {} receiver slots for {result_arity} result(s)",
                    name,
                    outputs.len()
                ),
                span,
            );
        }
        for argument in args {
            self.validate_expression(argument, span)?;
        }
        for output in outputs.iter().flatten() {
            self.validate_writable_target(output, span)?;
        }
        Ok(())
    }

    /// Validate statically selected call contracts before control-flow
    /// interpretation and return their declared result arity.
    fn validate_static_call_shape(
        &self,
        statement: &Statement,
        occurrence: &rumoca_core::Reference,
        span: Span,
    ) -> Result<Option<usize>, EvalError> {
        let name = occurrence.as_str();
        if name == "String" {
            return Err(EvalError::InvalidSemanticIr {
                reason: "predefined String conversion must use Expression::StringConversion"
                    .to_string(),
                span,
            });
        }
        let target =
            crate::constant::resolve_context_function_occurrence(occurrence, self.ctx, span)?;
        let result_arity = super::function_result_arity(target.function());
        drop(crate::constant::checked_statement_call_plan(
            target, statement, span,
        )?);
        Ok(Some(result_arity))
    }

    fn validate_assert(
        &self,
        condition: &rumoca_core::Expression,
        message: &rumoca_core::Expression,
        level: Option<&rumoca_core::Expression>,
        span: Span,
    ) -> Result<(), EvalError> {
        self.validate_expression(condition, span)?;
        self.validate_expression(message, span)?;
        if let Some(level) = level {
            self.validate_expression(level, span)?;
        }
        Ok(())
    }

    fn validate_writable_target(
        &self,
        reference: &ComponentReference,
        span: Span,
    ) -> Result<(), EvalError> {
        self.validate_writable_target_parts(reference.parts(), span)
    }

    fn validate_writable_target_parts(
        &self,
        parts: &[ComponentRefPart],
        span: Span,
    ) -> Result<(), EvalError> {
        let Some(root) = parts.first() else {
            return self.invalid("assignment target has no component-reference parts", span);
        };
        if self.loop_binders.iter().any(|binder| binder == &root.ident) {
            return self.invalid(
                format!("cannot assign to read-only loop index `{}`", root.ident),
                span,
            );
        }
        if self
            .function
            .inputs
            .iter()
            .any(|param| parameter_matches_root(param, root))
        {
            return self.invalid(
                format!("cannot assign to function input `{}`", root.ident),
                span,
            );
        }
        let writable = self
            .function
            .outputs
            .iter()
            .chain(&self.function.locals)
            .any(|param| parameter_matches_root(param, root));
        if !writable {
            return self.invalid(
                format!(
                    "assignment target `{}` is not a function output or local",
                    root.ident
                ),
                span,
            );
        }
        validate_component_parts(parts, self.ctx, span)
    }

    fn invalid<T>(&self, reason: impl Into<String>, span: Span) -> Result<T, EvalError> {
        Err(EvalError::InvalidSemanticIr {
            reason: reason.into(),
            span,
        })
    }
}

fn parameter_matches_root(param: &FunctionParam, root: &ComponentRefPart) -> bool {
    param.def_id == Some(root.def_id)
}

fn validate_component_parts(
    parts: &[ComponentRefPart],
    ctx: &EvalContext,
    fallback_span: Span,
) -> Result<(), EvalError> {
    for part in parts {
        validate_subscripts(&part.subs, ctx, fallback_span)?;
    }
    Ok(())
}

fn validate_subscripts(
    subscripts: &[Subscript],
    ctx: &EvalContext,
    fallback_span: Span,
) -> Result<(), EvalError> {
    for subscript in subscripts {
        if let Subscript::Expr { expr, .. } = subscript {
            validate_semantic_expression_in_context(expr, ctx, fallback_span)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    // Fixture-only function exposure identities in this module use the 20_000 range.

    #[test]
    fn empty_assignment_target_parts_return_typed_invalid_ir() {
        let function = Function::new(
            "test.emptyTarget",
            rumoca_core::DefId::new(20_001),
            Span::DUMMY,
        );
        let context = EvalContext::structural_preidentity();
        let validator = FunctionSemanticValidator {
            function: &function,
            ctx: &context,
            fallback_span: Span::DUMMY,
            loop_binders: Vec::new(),
            loop_depth: 0,
        };

        assert!(matches!(
            validator.validate_writable_target_parts(&[], Span::DUMMY),
            Err(EvalError::InvalidSemanticIr { .. })
        ));
    }
}
