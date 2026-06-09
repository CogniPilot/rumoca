use indexmap::IndexMap;
use rumoca_core::{ExpressionRewriter, Literal, OpBinary};
use rumoca_ir_dae as dae;

use crate::lower::LowerError;

mod projection_selection;
use projection_selection::*;

use super::super::helpers::is_record_constructor_signature;
use super::{
    is_add, is_mul, is_sub, scalar_key_expr, split_subtraction, sub_with_span, variable_dims,
};

#[derive(Debug, Clone)]
struct ProjectedFunctionOutput {
    output_name: String,
    selector: String,
    expr: rumoca_core::Expression,
}

struct FunctionProjectionAnalysis<'a> {
    dae_model: &'a dae::Dae,
    structural_bindings: &'a IndexMap<String, f64>,
}

#[derive(Clone, Default)]
struct FunctionProjectionScope {
    full: IndexMap<String, rumoca_core::Expression>,
    scalars: IndexMap<String, Vec<rumoca_core::Expression>>,
    dims: IndexMap<String, Vec<i64>>,
}

struct ProjectionValueCtx<'a> {
    dims: &'a [i64],
    flat_index: usize,
    scope: &'a FunctionProjectionScope,
    depth: usize,
    span: rumoca_core::Span,
}

struct MatrixVectorProductDims<'a> {
    lhs_dims: &'a [i64],
    rhs_dims: &'a [i64],
    rows: i64,
    cols: i64,
}

struct ProjectionAssignmentTarget {
    base: String,
    indices: Option<Vec<i64>>,
    span: rumoca_core::Span,
}

struct IndexedAssignment<'a> {
    target: &'a str,
    indices: &'a [i64],
    value: &'a rumoca_core::Expression,
    span: rumoca_core::Span,
    depth: usize,
}

struct IfStatementProjection<'a> {
    cond_blocks: &'a [rumoca_core::StatementBlock],
    else_block: &'a Option<Vec<rumoca_core::Statement>>,
    span: rumoca_core::Span,
    depth: usize,
}

struct ScalarSelectionCtx<'a> {
    name: &'a str,
    subscripts: &'a [rumoca_core::Subscript],
    dims: &'a [i64],
    values: &'a [rumoca_core::Expression],
    span: rumoca_core::Span,
    depth: usize,
}

pub(super) fn function_projected_residuals(
    residual: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let Some((lhs, rhs)) = split_subtraction(residual) else {
        return Ok(None);
    };
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    if let Some((call, field)) = function_field_access(rhs)
        && let Some(call_outputs) = analysis.function_call_outputs(call, 0)?
    {
        let Some(target_base) = plain_var_ref_name(lhs) else {
            return Ok(None);
        };
        return Ok(Some(projected_field_residuals_for_target(
            target_base,
            field,
            call_outputs,
            true,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        )));
    }
    if let Some((call, field)) = function_field_access(lhs)
        && let Some(call_outputs) = analysis.function_call_outputs(call, 0)?
    {
        let Some(target_base) = plain_var_ref_name(rhs) else {
            return Ok(None);
        };
        return Ok(Some(projected_field_residuals_for_target(
            target_base,
            field,
            call_outputs,
            false,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        )));
    }
    if let Some(call_outputs) = analysis.function_call_outputs(rhs, 0)? {
        let Some(target_base) = plain_var_ref_name(lhs) else {
            return Ok(None);
        };
        return Ok(Some(projected_residuals_for_target(
            target_base,
            call_outputs,
            true,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        )?));
    }
    if let Some(call_outputs) = analysis.function_call_outputs(lhs, 0)? {
        let Some(target_base) = plain_var_ref_name(rhs) else {
            return Ok(None);
        };
        return Ok(Some(projected_residuals_for_target(
            target_base,
            call_outputs,
            false,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        )?));
    }
    Ok(None)
}

fn function_field_access(
    expr: &rumoca_core::Expression,
) -> Option<(&rumoca_core::Expression, &str)> {
    let rumoca_core::Expression::FieldAccess { base, field, .. } = expr else {
        return None;
    };
    matches!(base.as_ref(), rumoca_core::Expression::FunctionCall { .. })
        .then_some((base.as_ref(), field.as_str()))
}

pub(super) fn function_call_projected_scalars(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    let outputs = FunctionProjectionAnalysis::new(dae_model, structural_bindings)
        .function_call_outputs(expr, 0)?;
    Ok(outputs.map(|outputs| outputs.into_iter().map(|output| output.expr).collect()))
}

fn projected_field_residuals_for_target(
    target_base: &rumoca_core::Reference,
    field: &str,
    outputs: Vec<ProjectedFunctionOutput>,
    target_minus_call: bool,
    span: rumoca_core::Span,
) -> Vec<rumoca_core::Expression> {
    outputs
        .into_iter()
        .filter_map(|output| {
            let target = projected_field_target_name(target_base.as_str(), field, &output)?;
            let target_expr = scalar_key_expr(target);
            Some(if target_minus_call {
                sub_with_span(target_expr, output.expr, span)
            } else {
                sub_with_span(output.expr, target_expr, span)
            })
        })
        .collect()
}

fn projected_residuals_for_target(
    target_base: &rumoca_core::Reference,
    outputs: Vec<ProjectedFunctionOutput>,
    target_minus_call: bool,
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    outputs
        .into_iter()
        .map(|output| {
            let target = projected_target_name(target_base.as_str(), &output, span)?;
            let target_expr = scalar_key_expr(target);
            Ok(if target_minus_call {
                sub_with_span(target_expr, output.expr, span)
            } else {
                sub_with_span(output.expr, target_expr, span)
            })
        })
        .collect()
}

fn projected_field_target_name(
    base: &str,
    field: &str,
    output: &ProjectedFunctionOutput,
) -> Option<String> {
    let field_prefix = format!("{}.{}", output.output_name, field);
    let suffix = output.selector.strip_prefix(&field_prefix)?;
    Some(format!("{base}{suffix}"))
}

fn projected_target_name(
    base: &str,
    output: &ProjectedFunctionOutput,
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    let suffix = output
        .selector
        .strip_prefix(output.output_name.as_str())
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "projected output selector `{}` does not start with output name `{}`",
                output.selector, output.output_name
            ),
            span,
        })?;
    Ok(format!("{base}{suffix}"))
}

fn plain_var_ref_name(expr: &rumoca_core::Expression) -> Option<&rumoca_core::Reference> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => Some(name),
        _ => None,
    }
}

fn project_scalar_outputs(
    output: &rumoca_core::FunctionParam,
    values: &[rumoca_core::Expression],
) -> Vec<ProjectedFunctionOutput> {
    project_target_scalar_outputs(&output.name, &output.dims, values.to_vec())
}

fn project_target_scalar_outputs(
    target: &str,
    dims: &[i64],
    values: Vec<rumoca_core::Expression>,
) -> Vec<ProjectedFunctionOutput> {
    values
        .into_iter()
        .enumerate()
        .map(|(idx, expr)| ProjectedFunctionOutput {
            output_name: target.to_string(),
            selector: dae::scalar_name_text_for_flat_index(target, dims, idx),
            expr,
        })
        .collect()
}

fn function_outputs_dims(output_count: usize) -> Vec<i64> {
    if output_count == 1 {
        Vec::new()
    } else {
        vec![output_count as i64]
    }
}

impl<'a> FunctionProjectionAnalysis<'a> {
    fn new(dae_model: &'a dae::Dae, structural_bindings: &'a IndexMap<String, f64>) -> Self {
        Self {
            dae_model,
            structural_bindings,
        }
    }

    fn function_call_outputs(
        &self,
        expr: &rumoca_core::Expression,
        depth: usize,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        if depth > super::super::MAX_FUNCTION_INLINE_DEPTH {
            return Ok(None);
        }
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = expr
        else {
            return Ok(None);
        };
        if self.is_record_constructor_call(name, *is_constructor) {
            return Ok(None);
        }
        let Some(function) = self.dae_model.symbols.functions.get(name.var_name()) else {
            return Ok(None);
        };
        if !function.pure || function.external.is_some() {
            return Ok(None);
        }
        let Some(mut scope) = self.bind_inputs(function, args, depth + 1)? else {
            return Ok(None);
        };
        let mut projected = Vec::new();
        for statement in &function.body {
            self.apply_statement(function, statement, &mut scope, &mut projected, depth + 1)?;
        }
        if !projected.is_empty() {
            return Ok(Some(projected));
        }
        self.projected_outputs_from_scope(function, &scope, depth + 1)
    }

    fn bind_inputs(
        &self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        depth: usize,
    ) -> Result<Option<FunctionProjectionScope>, LowerError> {
        let (named, positional) =
            super::super::function_calls::split_named_and_positional_call_args(
                function.name.as_str(),
                args,
            )?;
        let mut scope = FunctionProjectionScope::default();
        let mut positional_idx = 0usize;
        for input in &function.inputs {
            let actual = named.get(input.name.as_str()).copied().or_else(|| {
                super::super::function_calls::next_positional_function_input_arg(
                    input,
                    &positional,
                    &mut positional_idx,
                )
            });
            let Some(actual) = actual.or(input.default.as_ref()) else {
                return Ok(None);
            };
            let actual = actual.clone();
            let actual = self.substitute(&actual, &scope);
            scope.full.insert(input.name.clone(), actual.clone());
            let dims = self
                .expr_dims(&actual, &scope, depth + 1)
                .filter(|dims| !dims.is_empty())
                .unwrap_or_else(|| input.dims.clone());
            if !dims.is_empty() {
                self.insert_input_scalar_projection(input, &actual, dims, &mut scope, depth + 1)?;
            }
        }
        Ok(Some(scope))
    }

    fn insert_input_scalar_projection(
        &self,
        input: &rumoca_core::FunctionParam,
        actual: &rumoca_core::Expression,
        dims: Vec<i64>,
        scope: &mut FunctionProjectionScope,
        depth: usize,
    ) -> Result<(), LowerError> {
        let Some(scalars) = self.project_value_scalars(actual, &dims, scope, depth)? else {
            if actual.contains_der() {
                return Err(LowerError::Unsupported {
                    reason: format!(
                        "function input `{}` derivative expression could not be projected",
                        input.name
                    ),
                });
            }
            return Ok(());
        };
        scope.scalars.insert(input.name.clone(), scalars);
        scope.dims.insert(input.name.clone(), dims);
        Ok(())
    }

    fn apply_statement(
        &self,
        function: &rumoca_core::Function,
        statement: &rumoca_core::Statement,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
    ) -> Result<(), LowerError> {
        match statement {
            rumoca_core::Statement::Assignment { .. } => {
                self.apply_assignment(function, statement, scope, projected, depth)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                span,
            } => self.apply_if_statement(
                function,
                IfStatementProjection {
                    cond_blocks,
                    else_block,
                    span: *span,
                    depth,
                },
                scope,
                projected,
            ),
            statement if is_ignorable_projection_statement(statement) => Ok(()),
            _ => Err(LowerError::Unsupported {
                reason: format!(
                    "function `{}` contains a statement that cannot be projected",
                    function.name
                ),
            }),
        }
    }

    fn apply_assignment(
        &self,
        function: &rumoca_core::Function,
        statement: &rumoca_core::Statement,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
    ) -> Result<(), LowerError> {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return Err(LowerError::ContractViolation {
                reason: "non-assignment statement reached assignment projection".to_string(),
                span: function.span,
            });
        };
        let target = projection_assignment_target(comp)?;
        let value = self.scalar_assignment_value(value, scope, depth + 1)?;
        if let Some(indices) = target.indices.as_deref() {
            self.apply_indexed_assignment(
                function,
                IndexedAssignment {
                    target: &target.base,
                    indices,
                    value: &value,
                    span: target.span,
                    depth: depth + 1,
                },
                scope,
            )?;
            return Ok(());
        }
        let target = target.base;
        scope.full.insert(target.clone(), value.clone());
        if let Some(record_outputs) =
            self.record_constructor_outputs(&target, &value, scope, depth + 1)?
        {
            if is_function_output_target(function, &target) {
                projected.extend(record_outputs);
            }
            return Ok(());
        }
        let dims = self
            .expr_dims(&value, scope, depth + 1)
            .filter(|dims| !dims.is_empty())
            .or_else(|| declared_dims(function, &target))
            .unwrap_or_default();
        if !dims.is_empty() {
            let scalars = self
                .project_value_scalars(&value, &dims, scope, depth + 1)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: format!(
                        "function `{}` assignment to `{target}` could not be projected",
                        function.name
                    ),
                })?;
            scope.scalars.insert(target.clone(), scalars.clone());
            scope.dims.insert(target.clone(), dims.clone());
            if is_function_output_target(function, &target) {
                projected.extend(project_target_scalar_outputs(&target, &dims, scalars));
            }
        }
        Ok(())
    }

    fn apply_indexed_assignment(
        &self,
        function: &rumoca_core::Function,
        assignment: IndexedAssignment<'_>,
        scope: &mut FunctionProjectionScope,
    ) -> Result<(), LowerError> {
        let target = assignment.target;
        let span = assignment.span;
        if self
            .expr_dims(assignment.value, scope, assignment.depth + 1)
            .is_some_and(|dims| !dims.is_empty())
        {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "indexed assignment to scalar element `{target}` received an array value"
                ),
            }
            .with_fallback_span(span));
        }
        let dims = scope
            .dims
            .get(target)
            .cloned()
            .or_else(|| declared_dims(function, target))
            .ok_or_else(|| guarded_assignment_without_base(target, span))?;
        let flat_index = flat_index_from_indices(&dims, assignment.indices).ok_or_else(|| {
            LowerError::ContractViolation {
                reason: format!(
                    "indexed assignment to `{target}` uses out-of-bounds index {:?} for dimensions {dims:?}",
                    assignment.indices
                ),
                span,
            }
        })?;
        let mut values = scope
            .scalars
            .get(target)
            .cloned()
            .ok_or_else(|| guarded_assignment_without_base(target, span))?;
        let Some(slot) = values.get_mut(flat_index) else {
            return Err(LowerError::ContractViolation {
                reason: format!(
                    "indexed assignment to `{target}` flat index {flat_index} is missing from scalar projection"
                ),
                span,
            });
        };
        *slot = self.substitute(assignment.value, scope).with_span(span);
        scope.scalars.insert(target.to_string(), values);
        scope.dims.insert(target.to_string(), dims);
        Ok(())
    }

    fn scalar_assignment_value(
        &self,
        value: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<rumoca_core::Expression, LowerError> {
        if let Some((name, subscripts, span)) = indexed_var_selection(value)
            && let Some(values) = scope.scalars.get(name.as_str())
        {
            let dims =
                scope
                    .dims
                    .get(name.as_str())
                    .ok_or_else(|| LowerError::ContractViolation {
                        reason: format!(
                            "projected scalar selection for `{}` has values but no dimensions",
                            name.as_str()
                        ),
                        span,
                    })?;
            return projected_scalar_selection(
                ScalarSelectionCtx {
                    name: name.as_str(),
                    subscripts,
                    dims,
                    values,
                    span,
                    depth,
                },
                self,
                scope,
            );
        }
        let value = self.substitute(value, scope);
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = &value
        else {
            return Ok(value);
        };
        if subscripts.is_empty() {
            return Ok(value);
        }
        let Some(values) = scope.scalars.get(name.as_str()) else {
            return Ok(value);
        };
        let dims = scope
            .dims
            .get(name.as_str())
            .ok_or_else(|| LowerError::ContractViolation {
                reason: format!(
                    "projected scalar selection for `{}` has values but no dimensions",
                    name.as_str()
                ),
                span: *span,
            })?;
        projected_scalar_selection(
            ScalarSelectionCtx {
                name: name.as_str(),
                subscripts,
                dims,
                values,
                span: *span,
                depth,
            },
            self,
            scope,
        )
    }

    fn apply_if_statement(
        &self,
        function: &rumoca_core::Function,
        if_statement: IfStatementProjection<'_>,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
    ) -> Result<(), LowerError> {
        if let Some(selected) = self.compile_time_statement_if_selection(
            if_statement.cond_blocks,
            if_statement.else_block,
            scope,
        ) {
            for statement in selected {
                self.apply_statement(
                    function,
                    statement,
                    scope,
                    projected,
                    if_statement.depth + 1,
                )?;
            }
            return Ok(());
        }

        let entry_scope = scope.clone();
        let mut branch_conditions = Vec::with_capacity(if_statement.cond_blocks.len());
        let mut branch_scopes = Vec::with_capacity(if_statement.cond_blocks.len());
        for block in if_statement.cond_blocks {
            let condition = self.substitute(&block.cond, &entry_scope);
            let mut branch_scope = entry_scope.clone();
            let mut branch_projected = Vec::new();
            for statement in &block.stmts {
                self.apply_statement(
                    function,
                    statement,
                    &mut branch_scope,
                    &mut branch_projected,
                    if_statement.depth + 1,
                )?;
            }
            branch_conditions.push(condition);
            branch_scopes.push(branch_scope);
        }

        let mut else_scope = entry_scope.clone();
        if let Some(statements) = if_statement.else_block {
            let mut else_projected = Vec::new();
            for statement in statements {
                self.apply_statement(
                    function,
                    statement,
                    &mut else_scope,
                    &mut else_projected,
                    if_statement.depth + 1,
                )?;
            }
        }

        *scope = self.merged_if_scope(
            &entry_scope,
            &branch_conditions,
            &branch_scopes,
            &else_scope,
            if_statement.span,
        )?;
        Ok(())
    }

    fn compile_time_statement_if_selection<'stmt>(
        &self,
        cond_blocks: &'stmt [rumoca_core::StatementBlock],
        else_block: &'stmt Option<Vec<rumoca_core::Statement>>,
        scope: &FunctionProjectionScope,
    ) -> Option<&'stmt [rumoca_core::Statement]> {
        for block in cond_blocks {
            let condition = self.substitute(&block.cond, scope);
            let value = self.compile_time_scalar(&condition)?;
            if value != 0.0 {
                return Some(&block.stmts);
            }
        }
        Some(else_block.as_deref().unwrap_or(&[]))
    }

    fn merged_if_scope(
        &self,
        entry_scope: &FunctionProjectionScope,
        branch_conditions: &[rumoca_core::Expression],
        branch_scopes: &[FunctionProjectionScope],
        else_scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<FunctionProjectionScope, LowerError> {
        let mut merged = entry_scope.clone();
        for name in projection_scope_names(entry_scope, branch_scopes, else_scope) {
            if projection_scope_has_scalars(&name, entry_scope, branch_scopes, else_scope) {
                let values = self.merged_if_scalar_values(
                    &name,
                    entry_scope,
                    branch_conditions,
                    branch_scopes,
                    else_scope,
                    span,
                )?;
                let dims = merged_projection_dims(&name, entry_scope, branch_scopes, else_scope)
                    .unwrap_or_else(|| function_outputs_dims(values.len()));
                merged.scalars.insert(name.clone(), values);
                merged.dims.insert(name.clone(), dims);
            }
            if projection_scope_has_full(&name, entry_scope, branch_scopes, else_scope) {
                let value = merged_if_full_value(
                    &name,
                    entry_scope,
                    branch_conditions,
                    branch_scopes,
                    else_scope,
                    span,
                )?;
                merged.full.insert(name, value);
            }
        }
        Ok(merged)
    }

    fn merged_if_scalar_values(
        &self,
        name: &str,
        entry_scope: &FunctionProjectionScope,
        branch_conditions: &[rumoca_core::Expression],
        branch_scopes: &[FunctionProjectionScope],
        else_scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Vec<rumoca_core::Expression>, LowerError> {
        let base_values = else_scope
            .scalars
            .get(name)
            .or_else(|| entry_scope.scalars.get(name))
            .ok_or_else(|| guarded_assignment_without_base(name, span))?;
        let mut merged = base_values.clone();
        for (condition, branch_scope) in branch_conditions.iter().zip(branch_scopes.iter()).rev() {
            let Some(branch_values) = branch_scope.scalars.get(name) else {
                continue;
            };
            if branch_values.len() != merged.len() {
                return Err(LowerError::ContractViolation {
                    reason: format!(
                        "if-statement assignment to `{name}` has mismatched scalar widths: branch {}, else/current {}",
                        branch_values.len(),
                        merged.len()
                    ),
                    span,
                });
            }
            merged = branch_values
                .iter()
                .cloned()
                .zip(merged)
                .map(|(branch, fallback)| rumoca_core::Expression::If {
                    branches: vec![(condition.clone(), branch)],
                    else_branch: Box::new(fallback),
                    span,
                })
                .collect();
        }
        Ok(merged)
    }

    fn projected_outputs_from_scope(
        &self,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        let mut outputs = Vec::new();
        for output in &function.outputs {
            if let Some(values) = scope.scalars.get(output.name.as_str()) {
                outputs.extend(project_scalar_outputs(output, values));
                continue;
            }
            if let Some(expr) = scope.full.get(output.name.as_str()) {
                outputs.extend(self.project_output_expr(output, expr, scope, depth + 1)?);
            }
        }
        Ok((!outputs.is_empty()).then_some(outputs))
    }

    fn project_output_expr(
        &self,
        output: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
        if output.dims.is_empty() {
            return Ok(vec![ProjectedFunctionOutput {
                output_name: output.name.clone(),
                selector: output.name.clone(),
                expr: expr.clone(),
            }]);
        }
        let values = self
            .project_value_scalars(expr, &output.dims, scope, depth)?
            .ok_or_else(|| LowerError::Unsupported {
                reason: format!("function output `{}` could not be projected", output.name),
            })?;
        Ok(project_scalar_outputs(output, &values))
    }

    fn is_record_constructor_call(
        &self,
        name: &rumoca_core::Reference,
        is_constructor: bool,
    ) -> bool {
        is_constructor
            || self
                .dae_model
                .symbols
                .functions
                .get(name.var_name())
                .is_some_and(|function| is_record_constructor_signature(name.as_str(), function))
    }

    fn record_constructor_outputs(
        &self,
        target: &str,
        value: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = value
        else {
            return Ok(None);
        };
        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(None);
        }
        let Some(constructor) = self.dae_model.symbols.functions.get(name.var_name()) else {
            return Ok(None);
        };
        let (named, positional) =
            super::super::function_calls::split_named_and_positional_call_args(
                name.as_str(),
                args,
            )?;
        let mut positional_idx = 0usize;
        let mut outputs = Vec::new();
        for input in &constructor.inputs {
            let actual = named.get(input.name.as_str()).copied().or_else(|| {
                let actual = positional.get(positional_idx).copied();
                positional_idx += usize::from(actual.is_some());
                actual
            });
            let Some(actual) = actual.or(input.default.as_ref()) else {
                return Ok(None);
            };
            let actual = actual.clone();
            let actual = self.substitute(&actual, scope);
            let Some(scalars) =
                self.optional_constructor_input_scalars(&actual, &input.dims, scope, depth + 1)?
            else {
                continue;
            };
            outputs.extend(scalars.into_iter().enumerate().map(|(idx, expr)| {
                let field = format!("{target}.{}", input.name);
                ProjectedFunctionOutput {
                    output_name: target.to_string(),
                    selector: dae::scalar_name_text_for_flat_index(&field, &input.dims, idx),
                    expr,
                }
            }));
        }
        Ok(Some(outputs))
    }

    fn optional_constructor_input_scalars(
        &self,
        actual: &rumoca_core::Expression,
        dims: &[i64],
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
        match self.project_value_scalars(actual, dims, scope, depth)? {
            Some(scalars) => Ok(Some(scalars)),
            None if actual.contains_der() => Err(LowerError::Unsupported {
                reason: "record constructor derivative input could not be projected".to_string(),
            }),
            None => Ok(None),
        }
    }

    fn substitute(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
    ) -> rumoca_core::Expression {
        let mut substituter = FunctionScopeSubstituter { scope };
        substituter.rewrite_expression(expr)
    }

    fn project_value_scalars(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
        let count = scalar_count_for_dims(dims).ok_or_else(|| LowerError::ContractViolation {
            reason: format!("invalid projected value dimensions `{dims:?}`"),
            span: expr.span().unwrap_or(rumoca_core::Span::DUMMY),
        })?;
        if let Some(values) = self.project_function_call_scalars_once(expr, count, scope, depth)? {
            return Ok(Some(values));
        }
        (0..count)
            .map(|idx| self.project_value(expr, dims, idx, scope, depth))
            .collect::<Result<Option<Vec<_>>, _>>()
    }

    fn project_function_call_scalars_once(
        &self,
        expr: &rumoca_core::Expression,
        count: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
        let substituted = self.substitute(expr, scope);
        let rumoca_core::Expression::FunctionCall {
            is_constructor: false,
            ..
        } = substituted
        else {
            return Ok(None);
        };
        let Some(outputs) = self.function_call_outputs(&substituted, depth + 1)? else {
            return Ok(None);
        };
        Ok((outputs.len() == count).then(|| {
            outputs
                .into_iter()
                .map(|output| output.expr)
                .collect::<Vec<_>>()
        }))
    }

    fn project_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if dims.is_empty() {
            return Ok(Some(self.substitute(expr, scope)));
        }
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() => {
                if let Some(values) = scope.scalars.get(name.as_str())
                    && let Some(value) = values.get(flat_index)
                {
                    return Ok(Some(value.clone().with_span(*span)));
                }
                let indices = required_flat_index_to_subscripts(dims, flat_index, *span)?;
                let name = self.reference_with_dae_component_ref(name);
                Ok(Some(rumoca_core::Expression::VarRef {
                    name: project_reference_indices(&name, &indices, *span)?,
                    subscripts: Vec::new(),
                    span: *span,
                }))
            }
            rumoca_core::Expression::Array { elements, span, .. } => {
                let Some(element) = flatten_array_elements(elements).get(flat_index).cloned()
                else {
                    return Err(LowerError::ContractViolation {
                        reason: format!(
                            "flat index {flat_index} is out of bounds for array expression"
                        ),
                        span: *span,
                    });
                };
                Ok(Some(self.substitute(&element, scope)))
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => {
                let ctx = ProjectionValueCtx {
                    dims,
                    flat_index,
                    scope,
                    depth,
                    span: *span,
                };
                self.project_if_value(branches, else_branch, &ctx)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args,
                span,
            } if args.len() == 1 => {
                let arg = self
                    .project_value(&args[0], dims, flat_index, scope, depth)?
                    .ok_or_else(|| LowerError::Unsupported {
                        reason: "derivative argument could not be projected".to_string(),
                    })?;
                Ok(Some(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Der,
                    args: vec![arg],
                    span: *span,
                }))
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args,
                ..
            } if args.len() == 1 => {
                self.project_transpose_value(&args[0], dims, flat_index, scope, depth)
            }
            rumoca_core::Expression::FunctionCall {
                is_constructor: false,
                ..
            } => self.project_function_call_value(expr, flat_index, scope, depth),
            rumoca_core::Expression::Binary { op, lhs, rhs, span }
                if is_mul(op) || is_add(op) || is_sub(op) || is_div(op) =>
            {
                let ctx = ProjectionValueCtx {
                    dims,
                    flat_index,
                    scope,
                    depth,
                    span: *span,
                };
                self.project_binary_value(op, lhs, rhs, &ctx)
            }
            other => self.project_indexed_value(other, dims, flat_index, scope),
        }
    }

    fn project_binary_value(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if is_mul(op)
            && let Some(expr) = self.project_tensor_product(lhs, rhs, ctx)?
        {
            return Ok(Some(expr));
        }
        self.project_binary_elementwise(op.clone(), lhs, rhs, ctx)
    }

    fn project_if_value(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if let Some(selected) = self.compile_time_if_selection(branches, else_branch, ctx.scope) {
            return self.project_value(
                selected,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth + 1,
            );
        }
        let projected_branches = branches
            .iter()
            .map(|(condition, branch)| {
                Ok((
                    self.substitute(condition, ctx.scope),
                    self.project_value(branch, ctx.dims, ctx.flat_index, ctx.scope, ctx.depth + 1)?
                        .ok_or_else(|| LowerError::Unsupported {
                            reason: "if branch could not be projected".to_string(),
                        })?,
                ))
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let projected_else = self
            .project_value(
                else_branch,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth + 1,
            )?
            .ok_or_else(|| LowerError::Unsupported {
                reason: "if else branch could not be projected".to_string(),
            })?;
        Ok(Some(rumoca_core::Expression::If {
            branches: projected_branches,
            else_branch: Box::new(projected_else),
            span: ctx.span,
        }))
    }

    fn project_transpose_value(
        &self,
        arg: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let Some(input_dims) = self.expr_dims(arg, scope, depth) else {
            return Ok(None);
        };
        let [input_rows, input_cols] = input_dims.as_slice() else {
            return Ok(None);
        };
        let output_dims = [*input_cols, *input_rows];
        if dims != output_dims {
            return Ok(None);
        }
        let output_rows =
            usize::try_from(*input_rows).map_err(|_| LowerError::ContractViolation {
                reason: format!("transpose input has invalid row dimension `{input_rows}`"),
                span: arg.span().unwrap_or(rumoca_core::Span::DUMMY),
            })?;
        let input_cols =
            usize::try_from(*input_cols).map_err(|_| LowerError::ContractViolation {
                reason: format!("transpose input has invalid column dimension `{input_cols}`"),
                span: arg.span().unwrap_or(rumoca_core::Span::DUMMY),
            })?;
        if output_rows == 0 || input_cols == 0 {
            return Ok(None);
        }
        let output_row = flat_index / output_rows;
        let output_col = flat_index % output_rows;
        let input_index = output_col * input_cols + output_row;
        self.project_value(arg, &input_dims, input_index, scope, depth)
    }

    fn project_function_call_value(
        &self,
        expr: &rumoca_core::Expression,
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let call = self.substitute(expr, scope);
        Ok(self
            .function_call_outputs(&call, depth + 1)?
            .and_then(|outputs| outputs.get(flat_index).map(|output| output.expr.clone())))
    }

    fn project_indexed_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let span = expr.span().unwrap_or(rumoca_core::Span::DUMMY);
        let indices = required_flat_index_to_subscripts(dims, flat_index, span)?;
        Ok(Some(rumoca_core::Expression::Index {
            base: Box::new(self.substitute(expr, scope)),
            subscripts: indices
                .into_iter()
                .map(|idx| rumoca_core::Subscript::generated_index(idx as i64, span))
                .collect(),
            span,
        }))
    }

    fn project_binary_elementwise(
        &self,
        op: OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let lhs_dims = self
            .expr_dims(lhs, ctx.scope, ctx.depth)
            .unwrap_or_default();
        let rhs_dims = self
            .expr_dims(rhs, ctx.scope, ctx.depth)
            .unwrap_or_default();
        let lhs_expr = if lhs_dims.is_empty() {
            self.substitute(lhs, ctx.scope)
        } else {
            self.project_value(lhs, ctx.dims, ctx.flat_index, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "binary lhs could not be projected".to_string(),
                })?
        };
        let rhs_expr = if rhs_dims.is_empty() {
            self.substitute(rhs, ctx.scope)
        } else {
            self.project_value(rhs, ctx.dims, ctx.flat_index, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "binary rhs could not be projected".to_string(),
                })?
        };
        Ok(Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs_expr),
            rhs: Box::new(rhs_expr),
            span: ctx.span,
        }))
    }

    fn project_tensor_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let Some(lhs_dims) = self.expr_dims(lhs, ctx.scope, ctx.depth) else {
            return Ok(None);
        };
        let Some(rhs_dims) = self.expr_dims(rhs, ctx.scope, ctx.depth) else {
            return Ok(None);
        };
        match (lhs_dims.as_slice(), rhs_dims.as_slice(), ctx.dims) {
            ([rows, cols], [n], [_]) if cols == n => self.project_matrix_vector_product(
                lhs,
                rhs,
                MatrixVectorProductDims {
                    lhs_dims: &lhs_dims,
                    rhs_dims: &rhs_dims,
                    rows: *rows,
                    cols: *cols,
                },
                ctx,
            ),
            ([n], [rows, cols], [_]) if n == rows => {
                self.project_vector_matrix_product(lhs, rhs, &rhs_dims, ctx, *rows, *cols)
            }
            ([rows, inner_lhs], [inner_rhs, cols], [out_rows, out_cols])
                if inner_lhs == inner_rhs && rows == out_rows && cols == out_cols =>
            {
                self.project_matrix_matrix_product(lhs, rhs, &lhs_dims, &rhs_dims, ctx, *cols)
            }
            _ => Ok(None),
        }
    }

    fn project_matrix_vector_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        product_dims: MatrixVectorProductDims<'_>,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let rows = valid_product_dim(product_dims.rows, ctx.span, "matrix-vector rows")?;
        let cols = valid_product_dim(product_dims.cols, ctx.span, "matrix-vector columns")?;
        if ctx.flat_index >= rows {
            return Ok(None);
        }
        let row = ctx.flat_index;
        let mut terms = Vec::with_capacity(cols);
        for col in 0..cols {
            let lhs_idx = row * cols + col;
            let lhs_term = self
                .project_value(lhs, product_dims.lhs_dims, lhs_idx, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "matrix-vector lhs could not be projected".to_string(),
                })?;
            let rhs_term = self
                .project_value(rhs, product_dims.rhs_dims, col, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "matrix-vector rhs could not be projected".to_string(),
                })?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Ok(Some(sum_expressions(terms, ctx.span)))
    }

    fn project_vector_matrix_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        rhs_dims: &[i64],
        ctx: &ProjectionValueCtx<'_>,
        rows: i64,
        cols: i64,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let rows = valid_product_dim(rows, ctx.span, "vector-matrix rows")?;
        let cols = valid_product_dim(cols, ctx.span, "vector-matrix columns")?;
        if ctx.flat_index >= cols {
            return Ok(None);
        }
        let col = ctx.flat_index;
        let mut terms = Vec::with_capacity(rows);
        for row in 0..rows {
            let lhs_term = self
                .project_value(lhs, &[rows as i64], row, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "vector-matrix lhs could not be projected".to_string(),
                })?;
            let rhs_idx = row * cols + col;
            let rhs_term = self
                .project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "vector-matrix rhs could not be projected".to_string(),
                })?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Ok(Some(sum_expressions(terms, ctx.span)))
    }

    fn project_matrix_matrix_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        lhs_dims: &[i64],
        rhs_dims: &[i64],
        ctx: &ProjectionValueCtx<'_>,
        cols: i64,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let inner = valid_product_dim(lhs_dims[1], ctx.span, "matrix-matrix inner dimension")?;
        let cols = valid_product_dim(cols, ctx.span, "matrix-matrix columns")?;
        let row = ctx.flat_index / cols;
        let col = ctx.flat_index % cols;
        let mut terms = Vec::with_capacity(inner);
        for inner_idx in 0..inner {
            let lhs_idx = row * inner + inner_idx;
            let rhs_idx = inner_idx * cols + col;
            let lhs_term = self
                .project_value(lhs, lhs_dims, lhs_idx, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "matrix-matrix lhs could not be projected".to_string(),
                })?;
            let rhs_term = self
                .project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth)?
                .ok_or_else(|| LowerError::Unsupported {
                    reason: "matrix-matrix rhs could not be projected".to_string(),
                })?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Ok(Some(sum_expressions(terms, ctx.span)))
    }

    fn expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<Vec<i64>> {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => {
                if let Some(dims) = scope.dims.get(name.as_str()) {
                    return Some(dims.clone());
                }
                if let Some(values) = scope.scalars.get(name.as_str()) {
                    return Some(match values.len() {
                        0 | 1 => Vec::new(),
                        len => vec![len as i64],
                    });
                }
                variable_dims(self.dae_model, name.as_str())
                    .map(|dims| dims.into_iter().map(|dim| dim as i64).collect::<Vec<_>>())
            }
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                ..
            } => array_expression_dims(elements, *is_matrix),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args,
                ..
            } => args
                .first()
                .and_then(|arg| self.expr_dims(arg, scope, depth)),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args,
                ..
            } => {
                let dims = args
                    .first()
                    .and_then(|arg| self.expr_dims(arg, scope, depth))?;
                let [rows, cols] = dims.as_slice() else {
                    return None;
                };
                Some(vec![*cols, *rows])
            }
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor: false,
                ..
            } => self.function_call_expr_dims(name, expr, depth),
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_mul(op) => {
                let lhs_dims = self.expr_dims(lhs, scope, depth).unwrap_or_default();
                let rhs_dims = self.expr_dims(rhs, scope, depth).unwrap_or_default();
                match (lhs_dims.as_slice(), rhs_dims.as_slice()) {
                    ([], dims) if !dims.is_empty() => Some(dims.to_vec()),
                    (dims, []) if !dims.is_empty() => Some(dims.to_vec()),
                    ([lhs_rows, lhs_cols], [rhs_rows, rhs_cols]) if lhs_cols == rhs_rows => {
                        Some(vec![*lhs_rows, *rhs_cols])
                    }
                    ([rows, cols], [n]) if cols == n => Some(vec![*rows]),
                    ([n], [rows, cols]) if n == rows => Some(vec![*cols]),
                    ([n], [m]) if n == m => Some(Vec::new()),
                    _ => None,
                }
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_add(op) || is_sub(op) => {
                let lhs_dims = self.expr_dims(lhs, scope, depth).unwrap_or_default();
                let rhs_dims = self.expr_dims(rhs, scope, depth).unwrap_or_default();
                elementwise_binary_dims(&lhs_dims, &rhs_dims)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } if is_div(op) => {
                let lhs_dims = self.expr_dims(lhs, scope, depth).unwrap_or_default();
                let rhs_dims = self.expr_dims(rhs, scope, depth).unwrap_or_default();
                elementwise_binary_dims(&lhs_dims, &rhs_dims)
            }
            _ => None,
        }
    }

    fn function_call_expr_dims(
        &self,
        name: &rumoca_core::Reference,
        expr: &rumoca_core::Expression,
        depth: usize,
    ) -> Option<Vec<i64>> {
        self.declared_function_output_dims(name).or_else(|| {
            self.function_call_outputs(expr, depth + 1)
                .ok()
                .flatten()
                .map(|outputs| function_outputs_dims(outputs.len()))
        })
    }

    fn declared_function_output_dims(&self, name: &rumoca_core::Reference) -> Option<Vec<i64>> {
        let function = self.dae_model.symbols.functions.get(name.var_name())?;
        let [output] = function.outputs.as_slice() else {
            return None;
        };
        (!output.dims.is_empty() && output.dims.iter().all(|dim| *dim > 0))
            .then(|| output.dims.clone())
    }

    fn reference_with_dae_component_ref(
        &self,
        name: &rumoca_core::Reference,
    ) -> rumoca_core::Reference {
        if name.component_ref().is_some() {
            return name.clone();
        }
        self.dae_variable_component_ref(name.var_name())
            .map(|component_ref| {
                rumoca_core::Reference::with_component_reference(name.as_str(), component_ref)
            })
            .unwrap_or_else(|| name.clone())
    }

    fn dae_variable_component_ref(
        &self,
        name: &rumoca_core::VarName,
    ) -> Option<rumoca_core::ComponentReference> {
        self.dae_model
            .variables
            .states
            .get(name)
            .or_else(|| self.dae_model.variables.algebraics.get(name))
            .or_else(|| self.dae_model.variables.outputs.get(name))
            .or_else(|| self.dae_model.variables.parameters.get(name))
            .or_else(|| self.dae_model.variables.inputs.get(name))
            .or_else(|| self.dae_model.variables.discrete_reals.get(name))
            .or_else(|| self.dae_model.variables.discrete_valued.get(name))
            .or_else(|| self.dae_model.variables.constants.get(name))
            .and_then(|var| var.component_ref.clone())
    }

    fn compile_time_if_selection<'expr>(
        &self,
        branches: &'expr [(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &'expr rumoca_core::Expression,
        scope: &FunctionProjectionScope,
    ) -> Option<&'expr rumoca_core::Expression> {
        for (condition, branch) in branches {
            let condition = self.substitute(condition, scope);
            let value = self.compile_time_scalar(&condition)?;
            if value != 0.0 {
                return Some(branch);
            }
        }
        Some(else_branch)
    }

    fn compile_time_scalar(&self, expr: &rumoca_core::Expression) -> Option<f64> {
        match expr {
            rumoca_core::Expression::Literal { value, .. } => literal_to_f64(value),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                let key = compile_time_var_key(name, subscripts)?;
                self.structural_bindings.get(key.as_str()).copied()
            }
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                let value = self.compile_time_scalar(rhs)?;
                match op {
                    rumoca_core::OpUnary::Plus
                    | rumoca_core::OpUnary::DotPlus
                    | rumoca_core::OpUnary::Empty => Some(value),
                    rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Some(-value),
                    rumoca_core::OpUnary::Not => Some(f64::from(value == 0.0)),
                }
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                let lhs = self.compile_time_scalar(lhs)?;
                let rhs = self.compile_time_scalar(rhs)?;
                compile_time_binary(op, lhs, rhs)
            }
            _ => None,
        }
    }
}

fn literal_to_f64(value: &Literal) -> Option<f64> {
    match value {
        Literal::Real(value) => Some(*value),
        Literal::Integer(value) => Some(*value as f64),
        Literal::Boolean(value) => Some(f64::from(*value)),
        _ => None,
    }
}

fn compile_time_var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, .. } if *value > 0 => Some(*value as usize),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;
    Some(dae::format_subscript_key(name.as_str(), &indices))
}

fn compile_time_binary(op: &OpBinary, lhs: f64, rhs: f64) -> Option<f64> {
    match op {
        OpBinary::Add | OpBinary::AddElem => Some(lhs + rhs),
        OpBinary::Sub | OpBinary::SubElem => Some(lhs - rhs),
        OpBinary::Mul | OpBinary::MulElem => Some(lhs * rhs),
        OpBinary::Div | OpBinary::DivElem => Some(lhs / rhs),
        OpBinary::Exp | OpBinary::ExpElem => Some(lhs.powf(rhs)),
        OpBinary::Lt => Some(f64::from(lhs < rhs)),
        OpBinary::Le => Some(f64::from(lhs <= rhs)),
        OpBinary::Gt => Some(f64::from(lhs > rhs)),
        OpBinary::Ge => Some(f64::from(lhs >= rhs)),
        OpBinary::Eq => Some(f64::from((lhs - rhs).abs() < f64::EPSILON)),
        OpBinary::Neq => Some(f64::from((lhs - rhs).abs() >= f64::EPSILON)),
        OpBinary::And => Some(f64::from(lhs != 0.0 && rhs != 0.0)),
        OpBinary::Or => Some(f64::from(lhs != 0.0 || rhs != 0.0)),
        OpBinary::Assign | OpBinary::Empty => None,
    }
}

fn declared_dims(function: &rumoca_core::Function, name: &str) -> Option<Vec<i64>> {
    function
        .outputs
        .iter()
        .chain(function.locals.iter())
        .chain(function.inputs.iter())
        .find(|param| param.name == name && !param.dims.is_empty())
        .map(|param| param.dims.clone())
}

fn project_reference_indices(
    name: &rumoca_core::Reference,
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, LowerError> {
    if indices.is_empty() {
        return Ok(name.clone());
    }
    let component_ref = name
        .component_ref()
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "array projection for `{}` lost structured component-reference metadata",
                name.as_str()
            ),
            span,
        })?;
    let mut component_ref = component_ref.clone();
    let last = component_ref
        .parts
        .last_mut()
        .ok_or_else(|| LowerError::ContractViolation {
            reason: format!(
                "array projection for `{}` has an empty component reference",
                name.as_str()
            ),
            span,
        })?;
    last.subs.extend(
        indices
            .iter()
            .copied()
            .map(|index| rumoca_core::Subscript::generated_index(index as i64, span)),
    );
    Ok(rumoca_core::Reference::from_component_reference(
        component_ref,
    ))
}

fn is_ignorable_projection_statement(statement: &rumoca_core::Statement) -> bool {
    match statement {
        rumoca_core::Statement::Empty { .. } => true,
        rumoca_core::Statement::FunctionCall { comp, .. } => {
            comp.to_var_name().as_str() == "assert"
        }
        _ => false,
    }
}

fn projection_assignment_target(
    component_ref: &rumoca_core::ComponentReference,
) -> Result<ProjectionAssignmentTarget, LowerError> {
    let span = component_ref.span;
    let mut base_ref = component_ref.clone();
    let last = base_ref
        .parts
        .last_mut()
        .ok_or_else(|| LowerError::ContractViolation {
            reason: "function assignment target has an empty component reference".to_string(),
            span,
        })?;
    if last.subs.is_empty() {
        return Ok(ProjectionAssignmentTarget {
            base: component_ref.to_var_name().as_str().to_string(),
            indices: None,
            span,
        });
    }
    let indices = last
        .subs
        .iter()
        .map(|subscript| match subscript {
            rumoca_core::Subscript::Index { value, span } if *value > 0 => Ok(*value),
            _ => Err(LowerError::Unsupported {
                reason: "dynamic function assignment target subscripts cannot be projected"
                    .to_string(),
            }
            .with_fallback_span(subscript.span())),
        })
        .collect::<Result<Vec<_>, _>>()?;
    last.subs.clear();
    Ok(ProjectionAssignmentTarget {
        base: rumoca_core::Reference::from_component_reference(base_ref)
            .as_str()
            .to_string(),
        indices: Some(indices),
        span,
    })
}

struct FunctionScopeSubstituter<'a> {
    scope: &'a FunctionProjectionScope,
}

impl ExpressionRewriter for FunctionScopeSubstituter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return self.walk_expression(expr);
        };
        if !subscripts.is_empty() {
            return self.walk_expression(expr);
        }
        if let Some(expr) = self.scope.full.get(name.as_str()) {
            return expr.clone().with_span(*span);
        }
        if let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str())
            && let Some(values) = self.scope.scalars.get(scalar.base)
        {
            let dims = vec![values.len() as i64];
            if let Some(idx) = flat_index_from_indices(&dims, &scalar.indices)
                && let Some(expr) = values.get(idx)
            {
                return expr.clone().with_span(*span);
            }
        }
        self.walk_expression(expr)
    }
}

fn scalar_count_for_dims(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return Some(1);
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        let dim = usize::try_from(*dim).ok()?;
        acc.checked_mul(dim)
    })
}

fn required_flat_index_to_subscripts(
    dims: &[i64],
    flat_index: usize,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    dae::flat_index_to_subscripts(dims, flat_index).ok_or_else(|| LowerError::ContractViolation {
        reason: format!("flat index {flat_index} is out of bounds for dimensions `{dims:?}`"),
        span,
    })
}

fn valid_product_dim(
    dim: i64,
    span: rumoca_core::Span,
    context: &str,
) -> Result<usize, LowerError> {
    usize::try_from(dim).map_err(|_| LowerError::ContractViolation {
        reason: format!("{context} has invalid dimension `{dim}`"),
        span,
    })
}

fn array_expression_dims(
    elements: &[rumoca_core::Expression],
    is_matrix: bool,
) -> Option<Vec<i64>> {
    if !is_matrix {
        return Some(vec![elements.len() as i64]);
    }
    let cols = match elements.first()? {
        rumoca_core::Expression::Array { elements, .. } => elements.len(),
        _ => return None,
    };
    Some(vec![elements.len() as i64, cols as i64])
}

fn elementwise_binary_dims(lhs_dims: &[i64], rhs_dims: &[i64]) -> Option<Vec<i64>> {
    match (lhs_dims, rhs_dims) {
        ([], []) => Some(Vec::new()),
        (dims, []) | ([], dims) if !dims.is_empty() => Some(dims.to_vec()),
        (lhs, rhs) if lhs == rhs => Some(lhs.to_vec()),
        _ => None,
    }
}

fn is_div(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Div | OpBinary::DivElem)
}

fn flatten_array_elements(elements: &[rumoca_core::Expression]) -> Vec<rumoca_core::Expression> {
    let mut flattened = Vec::new();
    for element in elements {
        match element {
            rumoca_core::Expression::Array { elements: row, .. } => {
                flattened.extend(row.iter().cloned());
            }
            _ => flattened.push(element.clone()),
        }
    }
    flattened
}

fn flat_index_from_indices(dims: &[i64], indices: &[i64]) -> Option<usize> {
    if dims.len() != indices.len() || dims.is_empty() {
        return None;
    }
    let mut flat = 0usize;
    let mut stride = 1usize;
    for (&dim, &index) in dims.iter().rev().zip(indices.iter().rev()) {
        if dim <= 0 || index <= 0 || index > dim {
            return None;
        }
        flat += (index as usize - 1) * stride;
        stride = stride.checked_mul(dim as usize)?;
    }
    Some(flat)
}

fn sum_expressions(
    mut terms: Vec<rumoca_core::Expression>,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    if terms.is_empty() {
        return rumoca_core::Expression::Literal {
            value: Literal::Real(0.0),
            span,
        };
    }
    let first = terms.remove(0);
    terms
        .into_iter()
        .fold(first, |lhs, rhs| rumoca_core::Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span,
        })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn array(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
        rumoca_core::Expression::Array {
            elements,
            is_matrix,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn component_reference(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::Reference {
        rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
            local: false,
            span: rumoca_core::Span::DUMMY,
            parts,
            def_id: None,
        })
    }

    #[test]
    fn flatten_array_elements_flattens_matrix_rows() {
        let row1 = array(vec![real(1.0), real(2.0)], false);
        let row2 = array(vec![real(3.0), real(4.0)], false);

        let flattened = flatten_array_elements(&[row1, row2]);
        let values = flattened
            .iter()
            .map(|expr| match expr {
                rumoca_core::Expression::Literal {
                    value: Literal::Real(value),
                    ..
                } => *value,
                other => panic!("expected scalar literal, got {other:?}"),
            })
            .collect::<Vec<_>>();

        assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
    }

    #[test]
    fn scoped_single_scalar_value_has_scalar_dimensions() {
        let dae_model = dae::Dae::default();
        let structural_bindings = IndexMap::new();
        let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
        let mut scope = FunctionProjectionScope::default();
        scope
            .scalars
            .insert("tau_inv".to_string(), vec![real(17.0)]);
        let expr = rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("tau_inv"),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };

        assert_eq!(analysis.expr_dims(&expr, &scope, 0), Some(Vec::new()));
    }

    #[test]
    fn project_reference_indices_preserves_indexed_component_parts() {
        let reference = component_reference(vec![
            rumoca_core::ComponentRefPart {
                ident: "vehicle".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: Vec::new(),
            },
            rumoca_core::ComponentRefPart {
                ident: "motor".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: vec![rumoca_core::Subscript::generated_index(
                    1,
                    rumoca_core::Span::DUMMY,
                )],
            },
            rumoca_core::ComponentRefPart {
                ident: "history".to_string(),
                span: rumoca_core::Span::DUMMY,
                subs: Vec::new(),
            },
        ]);

        let projected = project_reference_indices(&reference, &[2], rumoca_core::Span::DUMMY)
            .expect("structured reference projection should succeed");

        let component_ref = projected
            .component_ref()
            .expect("projected reference should preserve component-reference structure");
        assert_eq!(projected.as_str(), "vehicle.motor[1].history[2]");
        assert_eq!(component_ref.parts[1].ident, "motor");
        assert_eq!(component_ref.parts[1].subs.len(), 1);
        assert_eq!(component_ref.parts[2].ident, "history");
        assert_eq!(component_ref.parts[2].subs.len(), 1);
    }
}
