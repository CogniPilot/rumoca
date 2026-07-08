// SPEC_0021 file-size exception: function derivative projection still combines
// dependency discovery, call rewriting, and projection row generation.
// split plan: move discovery, rewriting, and row generation into focused modules.

use std::cell::RefCell;

use indexmap::IndexMap;
use rumoca_core::{ExpressionRewriter, Literal, NAMED_FUNCTION_ARG_PREFIX, OpBinary};
use rumoca_ir_dae as dae;

use crate::lower::helpers::is_stream_passthrough_intrinsic;
use crate::lower::{LowerError, unsupported_at};
use crate::projection_suffix::parse_output_projection_suffix;

#[path = "function_projection/compile_time.rs"]
mod compile_time;
#[path = "function_projection/dimension_helpers.rs"]
mod dimension_helpers;
#[path = "function_projection/dimension_inference.rs"]
mod dimension_inference;
#[path = "function_projection/entrypoints.rs"]
mod entrypoints;
#[path = "function_projection/inline_budget.rs"]
mod inline_budget;
#[path = "function_projection/loop_projection.rs"]
mod loop_projection;
#[path = "function_projection/projection_helpers.rs"]
mod projection_helpers;
mod projection_selection;
#[path = "function_projection/selected_output.rs"]
mod selected_output;
#[path = "function_projection/target_projection.rs"]
mod target_projection;
#[cfg(test)]
#[path = "function_projection/tests.rs"]
mod tests;
use dimension_helpers::{
    FunctionScopeSubstituter, append_projected_outputs, array_expression_dims,
    assignment_projection_dims, binary_mul_dims, constructor_input_projection_dims,
    copy_projection_dims, declared_param_dims, elementwise_binary_dims,
    exact_declared_function_output_dims, flat_index_from_indices, flatten_array_elements,
    formal_actual_projection_dims, is_ignorable_projection_statement, is_same_plain_var_ref,
    named_actual_span, named_argument_spans, projected_declared_output_dims,
    projected_field_output_dims, projection_assignment_target, required_flat_index_to_subscripts,
    reserve_projection_capacity, scalar_count_for_dims, selector_dims_from_indices,
    single_field_path, sum_expressions, valid_product_dim,
};
pub(in crate::lower) use entrypoints::function_projected_residuals_with_owner;
use entrypoints::{
    checked_generated_subscript_from_usize, checked_projection_offset, checked_usize_dims_to_i64,
    checked_usize_to_i64, function_outputs_dims, project_target_scalar_outputs,
    required_merged_projection_dims, variable_dims_i64,
};
pub(in crate::lower) use entrypoints::{
    function_call_projected_output_groups_with_owner, function_call_projected_scalars_with_owner,
    project_array_like_scalar_with_owner, project_array_like_scalars_with_owner,
};
use inline_budget::{
    CachedProjectionOutcome, CallOutputsCacheEntry, exceeds_projection_node_budget,
    projection_budget_exceeded,
};
use loop_projection::ForProjectionCtx;
use projection_helpers::{
    ArrayProjectionValueCtx, IfStatementProjection, IndexedAssignment, MatrixVectorProductDims,
    ProjectionAssignmentTarget, ProjectionValueCtx, ScalarSelectionCtx, array_element_scalar_width,
    inherited_projection_source_span, inherited_projection_span, matrix_column_child_flat_index,
    matrix_column_operand_count, matrix_elements_are_row_literals,
    outputs_contain_unresolved_function_scope_refs, projection_actual_with_span,
    projection_arg_or_context_span, projection_value_ctx,
};
use projection_selection::*;
use selected_output::selected_function_output_call;
use target_projection::{project_reference_field_path_and_indices, projected_target_binding_key};

use super::super::helpers::{
    field_access_binding_key, format_i64_dims, is_record_constructor_signature,
};
use super::{
    dae_variable_ref_expr, is_add, is_div, is_mul, is_sub, split_subtraction, sub_with_span,
    variable_by_name,
};

const MAX_STATIC_WHILE_PROJECTION_ITERATIONS: usize = 1024;

type ConstructorInputScalars = (Vec<i64>, Vec<rumoca_core::Expression>);

#[derive(Debug, Clone)]
struct ProjectedFunctionOutput {
    field_path: Vec<String>,
    selector_indices: Vec<usize>,
    expr: rumoca_core::Expression,
}

struct FunctionProjectionAnalysis<'a> {
    dae_model: &'a dae::Dae,
    structural_bindings: &'a IndexMap<String, f64>,
    /// Memoized `function_call_outputs` results. Per-element projection
    /// re-projects the same substituted call once per scalar element, so
    /// nested array-valued calls are exponential without this cache.
    call_outputs_cache: RefCell<Vec<CallOutputsCacheEntry>>,
}

#[derive(Clone, Default)]
struct FunctionProjectionScope {
    full: IndexMap<String, rumoca_core::Expression>,
    scalars: IndexMap<String, Vec<rumoca_core::Expression>>,
    dims: IndexMap<String, Vec<i64>>,
}

#[derive(Clone, Copy)]
struct FunctionCallStatementProjection<'a> {
    function: &'a rumoca_core::Function,
    comp: &'a rumoca_core::ComponentReference,
    args: &'a [rumoca_core::Expression],
    output_targets: &'a [rumoca_core::ComponentReference],
    span: rumoca_core::Span,
    depth: usize,
}

fn merge_vectorized_scalar_dims(
    dims: &mut Option<Vec<i64>>,
    candidate: &[i64],
    name: &str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if candidate == [1] && dims.as_ref().is_some_and(|dims| dims.as_slice() != [1]) {
        return Ok(());
    }
    if dims
        .as_ref()
        .is_some_and(|dims| dims.as_slice() == [1] && candidate != [1])
    {
        *dims = Some(candidate.to_vec());
        return Ok(());
    }
    match dims {
        Some(existing) if existing.as_slice() != candidate => Err(LowerError::contract_violation(
            format!(
                "vectorized scalar `{name}` has dimensions {}, expected {}",
                format_i64_dims(candidate),
                format_i64_dims(existing)
            ),
            span,
        )),
        Some(_) => Ok(()),
        None => {
            *dims = Some(candidate.to_vec());
            Ok(())
        }
    }
}

fn is_elementwise_binary_projection_op(op: &OpBinary) -> bool {
    is_add(op)
        || is_sub(op)
        || is_mul(op)
        || is_div(op)
        || op.is_relational()
        || matches!(
            op,
            OpBinary::And | OpBinary::Or | OpBinary::Exp | OpBinary::ExpElem
        )
}

fn is_elementwise_builtin_projection(function: &rumoca_core::BuiltinFunction) -> bool {
    matches!(
        function,
        rumoca_core::BuiltinFunction::Abs
            | rumoca_core::BuiltinFunction::Sign
            | rumoca_core::BuiltinFunction::Sqrt
            | rumoca_core::BuiltinFunction::Div
            | rumoca_core::BuiltinFunction::Mod
            | rumoca_core::BuiltinFunction::Rem
            | rumoca_core::BuiltinFunction::Floor
            | rumoca_core::BuiltinFunction::Ceil
            | rumoca_core::BuiltinFunction::Min
            | rumoca_core::BuiltinFunction::Max
            | rumoca_core::BuiltinFunction::Sin
            | rumoca_core::BuiltinFunction::Cos
            | rumoca_core::BuiltinFunction::Tan
            | rumoca_core::BuiltinFunction::Asin
            | rumoca_core::BuiltinFunction::Acos
            | rumoca_core::BuiltinFunction::Atan
            | rumoca_core::BuiltinFunction::Atan2
            | rumoca_core::BuiltinFunction::Sinh
            | rumoca_core::BuiltinFunction::Cosh
            | rumoca_core::BuiltinFunction::Tanh
            | rumoca_core::BuiltinFunction::Exp
            | rumoca_core::BuiltinFunction::Log
            | rumoca_core::BuiltinFunction::Log10
            | rumoca_core::BuiltinFunction::Integer
            | rumoca_core::BuiltinFunction::NoEvent
            | rumoca_core::BuiltinFunction::Smooth
            | rumoca_core::BuiltinFunction::Homotopy
    )
}

fn projected_child_flat_index(child_dims: &[i64], outer_flat_index: usize) -> usize {
    if child_dims == [1] {
        0
    } else {
        outer_flat_index
    }
}

fn function_call_declared_output_count(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
) -> Option<usize> {
    let rumoca_core::Expression::FunctionCall {
        name,
        is_constructor: false,
        ..
    } = expr
    else {
        return None;
    };
    dae_model
        .symbols
        .functions
        .get(name.var_name())
        .map(|function| function.outputs.len())
}

fn checked_shape_dimension(value: f64, span: rumoca_core::Span) -> Result<i64, LowerError> {
    let rounded = value.round();
    if !value.is_finite() || (rounded - value).abs() > 1e-9 {
        return Err(unsupported_at(
            format!("function parameter shape dimension must be an integer, got `{value}`"),
            span,
        ));
    }
    if rounded < 0.0 || rounded >= i64::MAX as f64 {
        return Err(unsupported_at(
            format!("function parameter shape dimension `{value}` is out of range"),
            span,
        ));
    }
    Ok(rounded as i64)
}

impl<'a> FunctionProjectionAnalysis<'a> {
    fn new(dae_model: &'a dae::Dae, structural_bindings: &'a IndexMap<String, f64>) -> Self {
        Self {
            dae_model,
            structural_bindings,
            call_outputs_cache: RefCell::new(Vec::new()),
        }
    }

    fn projected_field_residuals_for_target(
        &self,
        target_base: &rumoca_core::Reference,
        field: &str,
        outputs: Vec<ProjectedFunctionOutput>,
        target_minus_call: bool,
        span: rumoca_core::Span,
    ) -> Result<Vec<rumoca_core::Expression>, LowerError> {
        let mut residuals =
            projection_vec_with_capacity(outputs.len(), "projected field residual count", span)?;
        for output in outputs {
            let Some((selected_field, remaining_fields)) = output.field_path.split_first() else {
                continue;
            };
            if selected_field != field {
                continue;
            }
            let target_expr =
                self.projected_target_expr(target_base, remaining_fields, &output, span)?;
            residuals.push(if target_minus_call {
                sub_with_span(target_expr, output.expr, span)
            } else {
                sub_with_span(output.expr, target_expr, span)
            });
        }
        Ok(residuals)
    }

    fn projected_residuals_for_target(
        &self,
        target_base: &rumoca_core::Reference,
        outputs: Vec<ProjectedFunctionOutput>,
        target_minus_call: bool,
        span: rumoca_core::Span,
    ) -> Result<Vec<rumoca_core::Expression>, LowerError> {
        let mut residuals =
            projection_vec_with_capacity(outputs.len(), "projected residual count", span)?;
        for output in outputs {
            let target_expr =
                self.projected_target_expr(target_base, &output.field_path, &output, span)?;
            residuals.push(if target_minus_call {
                sub_with_span(target_expr, output.expr, span)
            } else {
                sub_with_span(output.expr, target_expr, span)
            });
        }
        Ok(residuals)
    }

    fn projected_target_expr(
        &self,
        target_base: &rumoca_core::Reference,
        field_path: &[String],
        output: &ProjectedFunctionOutput,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, LowerError> {
        let key = projected_target_binding_key(
            target_base.as_str(),
            field_path,
            &output.selector_indices,
        );
        if let Some(variable) = variable_by_name(self.dae_model, &key) {
            return dae_variable_ref_expr(&key, variable, span, Vec::new());
        }
        let base_key = projected_target_binding_key(target_base.as_str(), field_path, &[]);
        if let Some(variable) = variable_by_name(self.dae_model, &base_key) {
            let mut subscripts = projection_vec_with_capacity(
                output.selector_indices.len(),
                "projected target variable subscript count",
                span,
            )?;
            for index in &output.selector_indices {
                subscripts.push(checked_generated_subscript_from_usize(
                    *index,
                    span,
                    "projected target variable subscript",
                )?);
            }
            return dae_variable_ref_expr(&base_key, variable, span, subscripts);
        }
        let name = project_reference_field_path_and_indices(
            target_base,
            field_path,
            &output.selector_indices,
            span,
        )?;
        Ok(rumoca_core::Expression::VarRef {
            name,
            subscripts: Vec::new(),
            span,
        })
    }

    fn function_call_outputs_with_owner(
        &self,
        expr: &rumoca_core::Expression,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        self.function_call_outputs_with_projection_scope(expr, depth, owner_span, None)
    }

    fn function_call_outputs_with_projection_scope(
        &self,
        expr: &rumoca_core::Expression,
        depth: usize,
        owner_span: rumoca_core::Span,
        caller_scope: Option<&FunctionProjectionScope>,
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
        if caller_scope.is_none()
            && let Some(outcome) = self.cached_call_outputs(expr, depth)
        {
            return outcome.into_result();
        }
        let call_span = inherited_projection_source_span(expr.span(), owner_span);
        let outcome = match self.uncached_function_call_outputs(
            function,
            args,
            depth,
            call_span,
            caller_scope,
        ) {
            Ok(outputs) => CachedProjectionOutcome::Outputs(outputs),
            Err(err) => match err.projection_budget_exceeded_parts() {
                Some((function, span)) => CachedProjectionOutcome::BudgetExceeded {
                    function: function.to_string(),
                    span,
                },
                None => return Err(err),
            },
        };
        if caller_scope.is_none() {
            self.record_call_outputs_outcome(expr, depth, outcome.clone());
        }
        outcome.into_result()
    }

    fn uncached_function_call_outputs(
        &self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        depth: usize,
        owner_span: rumoca_core::Span,
        caller_scope: Option<&FunctionProjectionScope>,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        let function_span = inherited_projection_span(function.span, owner_span);
        let Some(mut scope) = self.bind_inputs_with_projection_scope(
            function,
            args,
            depth + 1,
            function_span,
            caller_scope,
        )?
        else {
            return Ok(None);
        };
        if scope
            .scalars
            .values()
            .flatten()
            .chain(scope.full.values())
            .any(exceeds_projection_node_budget)
        {
            return Err(projection_budget_exceeded(function));
        }
        self.initialize_projected_declared_arrays(function, &mut scope, depth + 1, function_span)?;
        let mut projected = projection_vec_with_capacity(
            function.body.len(),
            "projected function body output count",
            function_span,
        )?;
        for statement in &function.body {
            self.apply_statement(
                function,
                statement,
                &mut scope,
                &mut projected,
                depth + 1,
                function_span,
            )?;
        }
        let outputs =
            self.complete_projected_outputs(function, &scope, projected, depth + 1, function_span)?;
        let outputs = outputs
            .map(|outputs| self.resolve_projected_scalar_field_outputs(outputs, function_span))
            .transpose()?;
        if let Some(outputs) = &outputs
            && outputs
                .iter()
                .any(|output| exceeds_projection_node_budget(&output.expr))
        {
            return Err(projection_budget_exceeded(function));
        }
        if outputs_contain_unresolved_function_scope_refs(function, outputs.as_deref()) {
            return Ok(None);
        }
        Ok(outputs)
    }

    fn complete_projected_outputs(
        &self,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
        mut projected: Vec<ProjectedFunctionOutput>,
        depth: usize,
        function_span: rumoca_core::Span,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        let Some(scope_outputs) =
            self.projected_outputs_from_scope(function, scope, depth, function_span)?
        else {
            return Ok((!projected.is_empty()).then_some(projected));
        };
        if projected.is_empty() {
            return Ok(Some(scope_outputs));
        }
        if function.outputs.len() <= 1 {
            return Ok(Some(projected));
        }
        for output_param in &function.outputs {
            if projected
                .iter()
                .any(|output| output.field_path.first() == Some(&output_param.name))
            {
                continue;
            }
            for output in scope_outputs
                .iter()
                .filter(|output| output.field_path.first() == Some(&output_param.name))
            {
                projected.push(output.clone());
            }
        }
        Ok(Some(projected))
    }

    #[cfg(test)]
    fn bind_inputs(
        &self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<FunctionProjectionScope>, LowerError> {
        self.bind_inputs_with_projection_scope(function, args, depth, owner_span, None)
    }

    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    fn bind_inputs_with_projection_scope(
        &self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        depth: usize,
        owner_span: rumoca_core::Span,
        caller_scope: Option<&FunctionProjectionScope>,
    ) -> Result<Option<FunctionProjectionScope>, LowerError> {
        let (named, positional) =
            super::super::function_calls::split_named_and_positional_call_args(
                function.name.as_str(),
                args,
            )?;
        let named_spans = named_argument_spans(args, owner_span)?;
        let mut scope = FunctionProjectionScope::default();
        let mut positional_idx = 0usize;
        let used_inputs = super::super::function_calls::referenced_function_input_names(function);
        for (input_idx, input) in function.inputs.iter().enumerate() {
            let input_span = inherited_projection_span(input.span, owner_span);
            if !used_inputs.contains(&input.name)
                && let Some((prefix, field)) = split_flattened_projection_input_name(&input.name)
            {
                let flattened_group_has_used_sibling = function.inputs.iter().any(|candidate| {
                    flattened_projection_input_has_prefix(&candidate.name, prefix)
                        && used_inputs.contains(&candidate.name)
                });
                if flattened_group_has_used_sibling && !named.contains_key(input.name.as_str()) {
                    let later_flattened_sibling_is_used =
                        function.inputs.iter().skip(input_idx + 1).any(|next| {
                            flattened_projection_input_has_prefix(&next.name, prefix)
                                && used_inputs.contains(&next.name)
                        });
                    if !later_flattened_sibling_is_used
                        || positional.get(positional_idx).is_some_and(|arg| {
                            super::super::function_calls::is_flattened_record_field_actual(
                                arg, field,
                            )
                        })
                    {
                        positional_idx += usize::from(positional_idx < positional.len());
                    }
                    continue;
                }
            }
            let mut consume_flattened_positional = false;
            let mut project_flattened_positional_field = false;
            let actual = if let Some(actual) = named.get(input.name.as_str()).copied() {
                Some((
                    actual.clone(),
                    inherited_projection_span(
                        named_actual_span(&named_spans, input, actual),
                        input_span,
                    ),
                    true,
                ))
            } else if let Some((prefix, field)) = split_flattened_projection_input_name(&input.name)
                && let Some(actual) = positional.get(positional_idx).cloned()
                && (self.flattened_projection_actual_has_field(actual, field, &scope)
                    || caller_scope.is_some_and(|caller_scope| {
                        self.flattened_projection_actual_has_field(actual, field, caller_scope)
                    })
                    || (positional.len().saturating_sub(positional_idx) == 1
                        && flattened_projection_input_is_group_start(
                            &function.inputs,
                            input_idx,
                            prefix,
                        )
                        && flattened_projection_group_has_prefix(&function.inputs, prefix)))
            {
                project_flattened_positional_field = true;
                consume_flattened_positional = !function
                    .inputs
                    .iter()
                    .skip(input_idx + 1)
                    .any(|next| flattened_projection_input_has_prefix(&next.name, prefix));
                let actual = self
                    .flattened_projection_actual_field_value(actual, field, caller_scope)
                    .unwrap_or_else(|| rumoca_core::Expression::FieldAccess {
                        base: Box::new(actual.clone()),
                        field: field.to_string(),
                        span: actual.span().unwrap_or(input_span),
                    });
                let actual_span = actual.span().unwrap_or(input_span);
                Some((actual, actual_span, true))
            } else {
                super::super::function_calls::next_positional_function_input_arg(
                    input,
                    &positional,
                    &mut positional_idx,
                )
                .map(|actual| (actual.clone(), actual.span().unwrap_or(input_span), true))
            };
            let Some((actual, actual_span, actual_is_call_arg)) = actual.or_else(|| {
                input
                    .default
                    .as_ref()
                    .map(|actual| (actual.clone(), actual.span().unwrap_or(input_span), false))
            }) else {
                return Ok(None);
            };
            if consume_flattened_positional {
                positional_idx += 1;
            }
            if actual_is_call_arg && let Some(caller_scope) = caller_scope {
                let caller_actual = self.substitute(&actual, caller_scope)?;
                let caller_dims = self.expr_dims_with_owner(
                    &caller_actual,
                    caller_scope,
                    depth + 1,
                    actual_span,
                )?;
                let dims = formal_actual_projection_dims(
                    input,
                    caller_dims,
                    format!("function `{}` input `{}`", function.name, input.name),
                    caller_actual.span().unwrap_or(actual_span),
                )?;
                if let Some(dims) = dims.filter(|dims| !dims.is_empty()) {
                    let scalars = self
                        .project_value_scalars(
                            &caller_actual,
                            &dims,
                            caller_scope,
                            depth + 1,
                            actual_span,
                        )?
                        .ok_or_else(|| {
                            unsupported_at(
                                format!(
                                    "function `{}` input `{}` could not be projected from caller scope",
                                    function.name, input.name
                                ),
                                actual_span,
                            )
                        })?;
                    scope.full.insert(input.name.clone(), caller_actual);
                    scope.scalars.insert(input.name.clone(), scalars);
                    scope.dims.insert(input.name.clone(), dims);
                    continue;
                }
                scope.full.insert(input.name.clone(), caller_actual);
                continue;
            }
            let actual = self.substitute(&actual, &scope)?;
            let actual_dims = self.expr_dims_with_owner(&actual, &scope, depth + 1, actual_span)?;
            let actual = if project_flattened_positional_field {
                self.project_value_scalars(&actual, &[], &scope, depth + 1, actual_span)?
                    .and_then(|values| values.into_iter().next())
                    .unwrap_or(actual)
            } else {
                actual
            };
            scope.full.insert(input.name.clone(), actual.clone());
            let dims = formal_actual_projection_dims(
                input,
                actual_dims,
                format!("function `{}` input `{}`", function.name, input.name),
                actual.span().unwrap_or(actual_span),
            )?;
            if let Some(dims) = dims.filter(|dims| !dims.is_empty()) {
                self.insert_input_scalar_projection(
                    input,
                    &actual,
                    dims,
                    &mut scope,
                    depth + 1,
                    actual_span,
                )?;
            }
        }
        Ok(Some(scope))
    }

    fn initialize_projected_declared_arrays(
        &self,
        function: &rumoca_core::Function,
        scope: &mut FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        for param in function.outputs.iter().chain(function.locals.iter()) {
            if self.initialize_declared_default(function, param, scope, depth + 1, owner_span)? {
                continue;
            }
            if param.dims.is_empty() || scope.scalars.contains_key(param.name.as_str()) {
                continue;
            }
            let param_span = inherited_projection_span(param.span, owner_span);
            let dims = self.function_param_projection_dims(param, scope, param_span)?;
            let count =
                scalar_count_for_dims(&dims, "function declared array dimensions", param_span)?;
            scope.dims.insert(param.name.clone(), dims);
            let mut scalars = projection_vec_with_capacity(
                count,
                "projected declared array scalar count",
                param_span,
            )?;
            for _ in 0..count {
                scalars.push(rumoca_core::Expression::Empty { span: param_span });
            }
            scope.scalars.insert(param.name.clone(), scalars);
        }
        Ok(())
    }

    #[allow(clippy::excessive_nesting)]
    fn initialize_declared_default(
        &self,
        function: &rumoca_core::Function,
        param: &rumoca_core::FunctionParam,
        scope: &mut FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<bool, LowerError> {
        let Some(default) = &param.default else {
            return Ok(false);
        };
        let param_span = inherited_projection_span(param.span, owner_span);
        let default_span = inherited_projection_source_span(default.span(), param_span);
        let value = self.substitute(default, scope)?;
        scope.full.insert(param.name.clone(), value.clone());
        if param.dims.is_empty() {
            let default_dims = self.expr_dims_with_owner(&value, scope, depth + 1, default_span)?;
            let projection_dims = match default_dims {
                Some(default_dims) if !default_dims.is_empty() => Some(default_dims),
                _ => self.vectorized_scalar_expr_dims(default, function, scope)?,
            };
            if let Some(default_dims) = projection_dims.as_deref().filter(|dims| !dims.is_empty()) {
                let scalars = self
                    .project_value_scalars(&value, default_dims, scope, depth + 1, default_span)
                    .map_err(|err| err.with_fallback_span(default_span))?
                    .ok_or_else(|| {
                        unsupported_at(
                            format!(
                                "declaration binding for `{}` could not be projected",
                                param.name
                            ),
                            default_span,
                        )
                    })?;
                scope.dims.insert(param.name.clone(), default_dims.to_vec());
                scope.scalars.insert(param.name.clone(), scalars);
            }
            return Ok(true);
        }
        let dims = self.function_param_projection_dims(param, scope, param_span)?;
        let scalars = self
            .project_value_scalars(&value, &dims, scope, depth + 1, default_span)
            .map_err(|err| err.with_fallback_span(default_span))?
            .ok_or_else(|| {
                unsupported_at(
                    format!(
                        "declaration binding for `{}` could not be projected",
                        param.name
                    ),
                    default_span,
                )
            })?;
        scope.dims.insert(param.name.clone(), dims);
        scope.scalars.insert(param.name.clone(), scalars);
        Ok(true)
    }

    fn function_param_projection_dims(
        &self,
        param: &rumoca_core::FunctionParam,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Vec<i64>, LowerError> {
        if param.shape_expr.is_empty() {
            return copy_projection_dims(
                &param.dims,
                "projected declared array dimension count",
                span,
            );
        }
        let mut dims = projection_vec_with_capacity(
            param.shape_expr.len(),
            "projected declared shape expression count",
            span,
        )?;
        for shape in &param.shape_expr {
            let dim = self.shape_expr_dimension(shape, scope, span)?;
            dims.push(dim);
        }
        Ok(dims)
    }

    fn shape_expr_dimension(
        &self,
        shape: &rumoca_core::Subscript,
        scope: &FunctionProjectionScope,
        owner_span: rumoca_core::Span,
    ) -> Result<i64, LowerError> {
        match shape {
            rumoca_core::Subscript::Index { value, .. } => Ok(*value),
            rumoca_core::Subscript::Expr { expr, span } => {
                let value = self
                    .compile_time_scalar_in_scope(expr, scope)?
                    .ok_or_else(|| {
                        unsupported_at("function parameter shape is not compile-time bound", *span)
                    })?;
                checked_shape_dimension(value, *span)
            }
            rumoca_core::Subscript::Colon { span } => Err(unsupported_at(
                "function parameter shape cannot use colon during projection",
                inherited_projection_span(*span, owner_span),
            )),
        }
    }

    fn declared_dims_in_scope(
        &self,
        function: &rumoca_core::Function,
        name: &str,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        Ok(self
            .declared_param_dims_in_scope(function, name, scope)?
            .filter(|dims| !dims.is_empty()))
    }

    fn declared_param_dims_in_scope(
        &self,
        function: &rumoca_core::Function,
        name: &str,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let Some(param) = function
            .outputs
            .iter()
            .chain(function.locals.iter())
            .chain(function.inputs.iter())
            .find(|param| param.name == name)
        else {
            return Ok(None);
        };
        let dims = self.function_param_projection_dims(param, scope, param.span)?;
        Ok(Some(dims))
    }

    fn flattened_projection_actual_has_field(
        &self,
        expr: &rumoca_core::Expression,
        field: &str,
        scope: &FunctionProjectionScope,
    ) -> bool {
        match expr {
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } => {
                self.is_record_constructor_call(name, *is_constructor)
                    || self
                        .dae_model
                        .symbols
                        .functions
                        .get(name.var_name())
                        .is_some_and(|function| {
                            matches!(
                                function.outputs.as_slice(),
                                [output]
                                    if output.type_class == Some(rumoca_core::ClassType::Record)
                            )
                        })
            }
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() => {
                let field_key = format!("{}.{field}", name.as_str());
                let flattened_key = format!("{}_{field}", name.as_str());
                scope.full.contains_key(&field_key)
                    || scope.scalars.contains_key(&field_key)
                    || scope.dims.contains_key(&field_key)
                    || scope.full.contains_key(&flattened_key)
                    || scope.scalars.contains_key(&flattened_key)
                    || scope.dims.contains_key(&flattened_key)
            }
            _ => false,
        }
    }

    fn flattened_projection_actual_field_value(
        &self,
        expr: &rumoca_core::Expression,
        field: &str,
        caller_scope: Option<&FunctionProjectionScope>,
    ) -> Option<rumoca_core::Expression> {
        let caller_scope = caller_scope?;
        let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        let field_key = format!("{}.{field}", name.as_str());
        let flattened_key = format!("{}_{field}", name.as_str());
        self.projected_scope_value_for_key(caller_scope, &field_key, *span)
            .or_else(|| self.projected_scope_value_for_key(caller_scope, &flattened_key, *span))
    }

    fn projected_scope_value_for_key(
        &self,
        scope: &FunctionProjectionScope,
        key: &str,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        if let Some(value) = scope.full.get(key) {
            return Some(value.clone().with_span(span));
        }
        let values = scope.scalars.get(key)?;
        match values.as_slice() {
            [value] => Some(value.clone().with_span(span)),
            _ => Some(rumoca_core::Expression::Array {
                elements: values.clone(),
                is_matrix: false,
                span,
            }),
        }
    }

    fn insert_input_scalar_projection(
        &self,
        input: &rumoca_core::FunctionParam,
        actual: &rumoca_core::Expression,
        dims: Vec<i64>,
        scope: &mut FunctionProjectionScope,
        depth: usize,
        actual_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let span = actual.span().unwrap_or(actual_span);
        let Some(scalars) = self
            .project_value_scalars(actual, &dims, scope, depth, span)
            .map_err(|err| err.with_fallback_span(span))?
        else {
            if actual.contains_der() {
                return Err(unsupported_at(
                    format!(
                        "function input `{}` derivative expression could not be projected",
                        input.name
                    ),
                    span,
                ));
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
        owner_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        match statement {
            rumoca_core::Statement::Assignment { .. } => {
                self.apply_assignment(function, statement, scope, projected, depth, owner_span)
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
                    span: inherited_projection_span(*span, owner_span),
                    depth,
                },
                scope,
                projected,
            ),
            rumoca_core::Statement::For {
                indices,
                equations,
                span,
            } => self.apply_for_statement(
                function,
                indices,
                equations,
                scope,
                projected,
                ForProjectionCtx {
                    span: inherited_projection_span(*span, owner_span),
                    depth,
                    index_depth: 0,
                },
            ),
            rumoca_core::Statement::While { block, span } => self.apply_static_while_statement(
                function,
                block,
                scope,
                projected,
                depth,
                inherited_projection_span(*span, owner_span),
            ),
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } if !outputs.is_empty() => self.apply_function_call_statement(
                FunctionCallStatementProjection {
                    function,
                    comp,
                    args,
                    output_targets: outputs,
                    span: inherited_projection_span(*span, owner_span),
                    depth,
                },
                scope,
                projected,
            ),
            statement if is_ignorable_projection_statement(statement) => Ok(()),
            _ => Err(unsupported_at(
                format!(
                    "function `{}` contains a statement that cannot be projected",
                    function.name
                ),
                inherited_projection_source_span(statement.source_span(), owner_span),
            )),
        }
    }

    fn apply_function_call_statement(
        &self,
        request: FunctionCallStatementProjection<'_>,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
    ) -> Result<(), LowerError> {
        let call = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::from_component_reference(request.comp.clone()),
            args: request.args.to_vec(),
            is_constructor: false,
            span: request.span,
        };
        let outputs = match self.function_call_outputs_with_projection_scope(
            &call,
            request.depth + 1,
            request.span,
            Some(scope),
        ) {
            Ok(outputs) => outputs,
            Err(err) if err.is_projection_budget_exceeded() => {
                return self.apply_budgeted_function_call_statement(request, scope, projected);
            }
            Err(err) => return Err(err),
        };
        let Some(outputs) = outputs else {
            return Err(unsupported_at(
                format!(
                    "function `{}` call statement to `{}` could not be projected",
                    request.function.name,
                    request.comp.to_var_name()
                ),
                request.span,
            ));
        };
        let callee_name = request.comp.to_var_name();
        let Some(callee) = self.dae_model.symbols.functions.get(&callee_name) else {
            return Err(LowerError::MissingFunction {
                name: callee_name.to_string(),
            });
        };
        if callee.outputs.len() != request.output_targets.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "function call statement to `{callee_name}` has {} outputs for {} targets",
                    callee.outputs.len(),
                    request.output_targets.len()
                ),
                request.span,
            ));
        }
        for (idx, (target, output_param)) in request
            .output_targets
            .iter()
            .zip(callee.outputs.iter())
            .enumerate()
        {
            let selected = self.selected_projected_statement_outputs(
                &outputs,
                callee,
                output_param,
                idx,
                request.span,
            )?;
            self.apply_projected_function_statement_output(
                request.function,
                output_param,
                target,
                selected,
                scope,
                projected,
                request.depth + 1,
                request.span,
            )?;
        }
        Ok(())
    }

    fn selected_projected_statement_outputs(
        &self,
        outputs: &[ProjectedFunctionOutput],
        callee: &rumoca_core::Function,
        output_param: &rumoca_core::FunctionParam,
        output_idx: usize,
        span: rumoca_core::Span,
    ) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
        if callee.outputs.len() == 1 {
            return Ok(outputs.to_vec());
        }
        let mut selected = projection_vec_with_capacity(
            outputs.len(),
            "selected projected function statement output count",
            span,
        )?;
        for output in outputs {
            let Some((head, tail)) = output.field_path.split_first() else {
                return Err(LowerError::contract_violation(
                    format!(
                        "multi-output function `{}` projected output {} for `{}` has no output selector",
                        callee.name,
                        output_idx + 1,
                        output_param.name
                    ),
                    span,
                ));
            };
            if head == &output_param.name {
                selected.push(ProjectedFunctionOutput {
                    field_path: tail.to_vec(),
                    selector_indices: output.selector_indices.clone(),
                    expr: output.expr.clone(),
                });
            }
        }
        Ok(selected)
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_projected_function_statement_output(
        &self,
        function: &rumoca_core::Function,
        output_param: &rumoca_core::FunctionParam,
        target: &rumoca_core::ComponentReference,
        selected: Vec<ProjectedFunctionOutput>,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if selected.is_empty() {
            return self.apply_empty_projected_function_statement_output(
                function,
                output_param,
                target,
                scope,
                span,
            );
        }
        if selected.iter().any(|output| !output.field_path.is_empty()) {
            return self.apply_projected_function_statement_output_as_assignments(
                function, target, selected, scope, projected, depth, span,
            );
        }
        let target = self.substitute_component_reference(target, scope)?;
        let assignment_target = projection_assignment_target(&target)?;
        if let Some(indices) = assignment_target.indices.as_deref() {
            if selected.len() != 1 {
                return Err(unsupported_at(
                    "indexed function call statement target cannot receive array output",
                    assignment_target.span,
                ));
            }
            self.apply_indexed_assignment(
                function,
                IndexedAssignment {
                    target: &assignment_target.base,
                    indices,
                    value: &selected[0].expr,
                    span: assignment_target.span,
                    depth,
                },
                scope,
            )?;
            return Ok(());
        }
        let selector_indices = selected
            .iter()
            .map(|output| output.selector_indices.as_slice())
            .collect::<Vec<_>>();
        let projected_dims =
            selector_dims_from_indices(&selector_indices, span)?.ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "function call statement output `{}` has non-dense selector projection",
                        output_param.name
                    ),
                    span,
                )
            })?;
        let value_dims = Some(projected_dims.clone());
        let dims = assignment_projection_dims(
            function.name.as_str(),
            &assignment_target.base,
            self.declared_param_dims_in_scope(function, &assignment_target.base, scope)?,
            value_dims,
            span,
        )?
        .unwrap_or(projected_dims);
        let scalars = selected
            .into_iter()
            .map(|output| output.expr)
            .collect::<Vec<_>>();
        scope
            .scalars
            .insert(assignment_target.base.clone(), scalars.clone());
        scope
            .dims
            .insert(assignment_target.base.clone(), dims.clone());
        scope.full.insert(
            assignment_target.base.clone(),
            rumoca_core::Expression::Array {
                elements: scalars.clone(),
                is_matrix: false,
                span,
            },
        );
        if let Some(output_param) = function
            .outputs
            .iter()
            .find(|output| output.name == assignment_target.base)
        {
            let outputs = self.tag_function_output_projection(
                function,
                output_param,
                project_target_scalar_outputs(&dims, scalars, span)?,
                span,
            )?;
            append_projected_outputs(
                projected,
                outputs,
                "function call statement projected output count",
                span,
            )?;
        }
        Ok(())
    }

    fn apply_empty_projected_function_statement_output(
        &self,
        function: &rumoca_core::Function,
        output_param: &rumoca_core::FunctionParam,
        target: &rumoca_core::ComponentReference,
        scope: &mut FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let target = self.substitute_component_reference(target, scope)?;
        let assignment_target = projection_assignment_target(&target)?;
        if assignment_target.indices.is_some() {
            return Err(unsupported_at(
                "indexed function call statement target cannot receive empty array output",
                assignment_target.span,
            ));
        }
        let Some(declared) =
            self.declared_param_dims_in_scope(function, &assignment_target.base, scope)?
        else {
            return Err(unsupported_at(
                format!(
                    "function call statement output `{}` projected no scalar values for `{}`",
                    output_param.name, assignment_target.base
                ),
                span,
            ));
        };
        if scalar_count_for_dims(
            &declared,
            "empty function statement output dimensions",
            span,
        )? != 0
        {
            return Err(unsupported_at(
                format!(
                    "function call statement output `{}` projected no scalar values for non-empty target `{}`",
                    output_param.name, assignment_target.base
                ),
                span,
            ));
        }
        let scalars = Vec::new();
        scope
            .scalars
            .insert(assignment_target.base.clone(), scalars.clone());
        scope.dims.insert(assignment_target.base.clone(), declared);
        scope.full.insert(
            assignment_target.base,
            rumoca_core::Expression::Array {
                elements: scalars,
                is_matrix: false,
                span,
            },
        );
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_projected_function_statement_output_as_assignments(
        &self,
        function: &rumoca_core::Function,
        target: &rumoca_core::ComponentReference,
        selected: Vec<ProjectedFunctionOutput>,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if selected.len() != 1 {
            return Err(unsupported_at(
                "record-valued function call statement output cannot be projected as an array assignment",
                span,
            ));
        }
        let assignment = rumoca_core::Statement::Assignment {
            comp: target.clone(),
            value: selected
                .into_iter()
                .next()
                .expect("selected len checked")
                .expr,
            span,
        };
        self.apply_assignment(function, &assignment, scope, projected, depth, span)
    }

    fn apply_budgeted_function_call_statement(
        &self,
        request: FunctionCallStatementProjection<'_>,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
    ) -> Result<(), LowerError> {
        let callee_name = request.comp.to_var_name();
        let Some(callee) = self.dae_model.symbols.functions.get(&callee_name) else {
            return Err(LowerError::MissingFunction {
                name: callee_name.to_string(),
            });
        };
        if callee.outputs.len() != request.output_targets.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "budgeted function call statement to `{callee_name}` has {} outputs for {} targets",
                    callee.outputs.len(),
                    request.output_targets.len()
                ),
                request.span,
            ));
        }
        for (target, output) in request.output_targets.iter().zip(callee.outputs.iter()) {
            let selected_call = rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_var_name(rumoca_core::VarName::new(format!(
                    "{callee_name}.{}",
                    output.name
                ))),
                args: request.args.to_vec(),
                is_constructor: false,
                span: request.span,
            };
            let assignment = rumoca_core::Statement::Assignment {
                comp: target.clone(),
                value: selected_call,
                span: request.span,
            };
            self.apply_assignment(
                request.function,
                &assignment,
                scope,
                projected,
                request.depth + 1,
                request.span,
            )?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_lines)]
    fn apply_assignment(
        &self,
        function: &rumoca_core::Function,
        statement: &rumoca_core::Statement,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return Err(LowerError::contract_violation(
                "non-assignment statement reached assignment projection",
                owner_span,
            ));
        };
        let original_value = value;
        let assignment_span = inherited_projection_source_span(statement.source_span(), owner_span);
        let comp = self.substitute_component_reference(comp, scope)?;
        let target = projection_assignment_target(&comp)?;
        let value = self.scalar_assignment_value(value, scope, depth + 1)?;
        if exceeds_projection_node_budget(&value) {
            return Err(projection_budget_exceeded(function));
        }
        if let Some(indices) = target.indices.as_deref() {
            let indexed_span = inherited_projection_span(target.span, assignment_span);
            self.apply_indexed_assignment(
                function,
                IndexedAssignment {
                    target: &target.base,
                    indices,
                    value: &value,
                    span: indexed_span,
                    depth: depth + 1,
                },
                scope,
            )?;
            return Ok(());
        }
        let target_span = inherited_projection_span(target.span, assignment_span);
        let target = target.base;
        if let Some(record_outputs) = self.record_constructor_outputs(&value, scope, depth + 1)? {
            if let Some(output_param) = function.outputs.iter().find(|output| output.name == target)
            {
                let record_outputs = self.tag_function_output_projection(
                    function,
                    output_param,
                    record_outputs,
                    target_span,
                )?;
                append_projected_outputs(
                    projected,
                    record_outputs,
                    "record constructor assignment output count",
                    target_span,
                )?;
            }
            scope.full.insert(target, value);
            return Ok(());
        }
        if function
            .outputs
            .iter()
            .any(|output| output.name == target && Self::function_output_is_record_like(output))
        {
            scope.full.insert(target, value);
            return Ok(());
        }
        let value_span = value.span().unwrap_or(target_span);
        let value_dims = self.expr_dims_with_owner(&value, scope, depth + 1, value_span)?;
        let vectorized_scalar_assignment = self.vectorized_scalar_assignment_dims(
            function,
            &target,
            original_value,
            value_dims.as_deref(),
            scope,
        )?;
        let dims = if let Some(dims) = vectorized_scalar_assignment {
            Some(dims)
        } else {
            let declared = self.declared_param_dims_in_scope(function, &target, scope)?;
            assignment_projection_dims(
                function.name.as_str(),
                &target,
                declared,
                value_dims,
                value_span,
            )?
        };
        if let Some(dims) = dims.filter(|dims| !dims.is_empty()) {
            let scalars = self
                .project_value_scalars(&value, &dims, scope, depth + 1, value_span)
                .map_err(|err| err.with_fallback_span(value_span))?
                .ok_or_else(|| {
                    unsupported_at(
                        format!(
                            "function `{}` assignment to `{target}` could not be projected",
                            function.name
                        ),
                        value_span,
                    )
                })?;
            if scalars.iter().any(exceeds_projection_node_budget) {
                return Err(projection_budget_exceeded(function));
            }
            scope.scalars.insert(target.clone(), scalars.clone());
            scope.dims.insert(
                target.clone(),
                copy_projection_dims(&dims, "scalar assignment dimension count", target_span)?,
            );
            if let Some(output_param) = function.outputs.iter().find(|output| output.name == target)
            {
                let outputs = self.tag_function_output_projection(
                    function,
                    output_param,
                    project_target_scalar_outputs(&dims, scalars, target_span)?,
                    target_span,
                )?;
                append_projected_outputs(
                    projected,
                    outputs,
                    "scalar assignment projected output count",
                    target_span,
                )?;
            }
        }
        scope.full.insert(target, value);
        Ok(())
    }

    fn vectorized_scalar_assignment_dims(
        &self,
        function: &rumoca_core::Function,
        target: &str,
        original_value: &rumoca_core::Expression,
        value_dims: Option<&[i64]>,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        if self
            .declared_dims_in_scope(function, target, scope)?
            .is_some_and(|dims| !dims.is_empty())
        {
            return Ok(None);
        }
        let Some(value_dims) = value_dims else {
            return Ok(None);
        };
        if value_dims.is_empty()
            || !self.expr_depends_on_vectorized_scalar(original_value, function, scope)?
        {
            return Ok(None);
        }
        Ok(Some(value_dims.to_vec()))
    }

    #[allow(clippy::excessive_nesting)]
    fn expr_depends_on_vectorized_scalar(
        &self,
        expr: &rumoca_core::Expression,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
    ) -> Result<bool, LowerError> {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => Ok(scope
                .dims
                .get(name.as_str())
                .is_some_and(|dims| !dims.is_empty())
                && declared_param_dims(function, name.as_str())?
                    .is_some_and(|dims| dims.is_empty())),
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.expr_depends_on_vectorized_scalar(rhs, function, scope)
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => Ok(self
                .expr_depends_on_vectorized_scalar(lhs, function, scope)?
                || self.expr_depends_on_vectorized_scalar(rhs, function, scope)?),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch) in branches {
                    if self.expr_depends_on_vectorized_scalar(condition, function, scope)?
                        || self.expr_depends_on_vectorized_scalar(branch, function, scope)?
                    {
                        return Ok(true);
                    }
                }
                self.expr_depends_on_vectorized_scalar(else_branch, function, scope)
            }
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                elements.iter().try_fold(false, |found, element| {
                    Ok(
                        found
                            || self.expr_depends_on_vectorized_scalar(element, function, scope)?,
                    )
                })
            }
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => Ok(
                self.expr_depends_on_vectorized_scalar(start, function, scope)?
                    || step
                        .as_deref()
                        .map(|step| self.expr_depends_on_vectorized_scalar(step, function, scope))
                        .transpose()?
                        .unwrap_or(false)
                    || self.expr_depends_on_vectorized_scalar(end, function, scope)?,
            ),
            rumoca_core::Expression::BuiltinCall { args, .. }
            | rumoca_core::Expression::FunctionCall { args, .. } => {
                args.iter().try_fold(false, |found, arg| {
                    Ok(found || self.expr_depends_on_vectorized_scalar(arg, function, scope)?)
                })
            }
            rumoca_core::Expression::FieldAccess { base, .. } => {
                self.expr_depends_on_vectorized_scalar(base, function, scope)
            }
            _ => Ok(false),
        }
    }

    fn vectorized_scalar_expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
    ) -> Result<Option<Vec<i64>>, LowerError> {
        let mut dims = None;
        self.collect_vectorized_scalar_expr_dims(expr, function, scope, &mut dims)?;
        Ok(dims)
    }

    fn collect_vectorized_scalar_expr_dims(
        &self,
        expr: &rumoca_core::Expression,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
        dims: &mut Option<Vec<i64>>,
    ) -> Result<(), LowerError> {
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() => {
                if let Some(candidate) = scope.dims.get(name.as_str())
                    && !candidate.is_empty()
                    && declared_param_dims(function, name.as_str())?
                        .is_some_and(|declared| declared.is_empty())
                {
                    merge_vectorized_scalar_dims(dims, candidate, name.as_str(), *span)?;
                }
                Ok(())
            }
            rumoca_core::Expression::Unary { rhs, .. } => {
                self.collect_vectorized_scalar_expr_dims(rhs, function, scope, dims)
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => {
                self.collect_vectorized_scalar_expr_dims(lhs, function, scope, dims)?;
                self.collect_vectorized_scalar_expr_dims(rhs, function, scope, dims)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch) in branches {
                    self.collect_vectorized_scalar_expr_dims(condition, function, scope, dims)?;
                    self.collect_vectorized_scalar_expr_dims(branch, function, scope, dims)?;
                }
                self.collect_vectorized_scalar_expr_dims(else_branch, function, scope, dims)
            }
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                for element in elements {
                    self.collect_vectorized_scalar_expr_dims(element, function, scope, dims)?;
                }
                Ok(())
            }
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => {
                self.collect_vectorized_scalar_expr_dims(start, function, scope, dims)?;
                if let Some(step) = step {
                    self.collect_vectorized_scalar_expr_dims(step, function, scope, dims)?;
                }
                self.collect_vectorized_scalar_expr_dims(end, function, scope, dims)
            }
            rumoca_core::Expression::BuiltinCall { args, .. }
            | rumoca_core::Expression::FunctionCall { args, .. } => {
                for arg in args {
                    self.collect_vectorized_scalar_expr_dims(arg, function, scope, dims)?;
                }
                Ok(())
            }
            rumoca_core::Expression::FieldAccess { base, .. } => {
                self.collect_vectorized_scalar_expr_dims(base, function, scope, dims)
            }
            _ => Ok(()),
        }
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
            .expr_dims_with_owner(assignment.value, scope, assignment.depth + 1, span)?
            .is_some_and(|dims| !dims.is_empty())
        {
            return Err(unsupported_at(
                format!("indexed assignment to scalar element `{target}` received an array value"),
                span,
            ));
        }
        let dims = if let Some(dims) = scope.dims.get(target) {
            copy_projection_dims(dims, "indexed assignment scope dimension count", span)?
        } else {
            self.declared_dims_in_scope(function, target, scope)?
                .ok_or_else(|| guarded_assignment_without_base(target, span))?
        };
        let flat_index = flat_index_from_indices(
            &dims,
            assignment.indices,
            span,
            "indexed assignment flat index",
        )?
        .ok_or_else(|| {
            let dims = format_i64_dims(&dims);
            let indices = format_i64_dims(assignment.indices);
            LowerError::contract_violation(
                format!(
                    "indexed assignment to `{target}` uses out-of-bounds index {indices} for dimensions {dims}"
                ),
                span,
            )
        })?;
        let mut values = scope
            .scalars
            .get(target)
            .cloned()
            .ok_or_else(|| guarded_assignment_without_base(target, span))?;
        let Some(slot) = values.get_mut(flat_index) else {
            return Err(LowerError::contract_violation(
                format!(
                    "indexed assignment to `{target}` flat index {flat_index} is missing from scalar projection"
                ),
                span,
            ));
        };
        let slot_value = self.substitute(assignment.value, scope)?.with_span(span);
        if exceeds_projection_node_budget(&slot_value) {
            return Err(projection_budget_exceeded(function));
        }
        *slot = slot_value;
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
            let dims = scope.dims.get(name.as_str()).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "projected scalar selection for `{}` has values but no dimensions",
                        name.as_str()
                    ),
                    span,
                )
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
        let value = self.substitute(value, scope)?;
        if matches!(
            value,
            rumoca_core::Expression::FunctionCall {
                is_constructor: false,
                ..
            }
        ) {
            let span = value
                .span()
                .unwrap_or_else(rumoca_core::Span::source_free_serde_default);
            match self.function_call_outputs_with_projection_scope(
                &value,
                depth + 1,
                span,
                Some(scope),
            ) {
                Ok(Some(outputs)) if outputs.len() == 1 => {
                    let output = only_projected_scalar_assignment_output(outputs, span)?;
                    return Ok(output.expr.with_span(span));
                }
                Ok(_) | Err(LowerError::ProjectionBudgetExceeded { .. }) => {}
                Err(err) => return Err(err),
            }
        }
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
        let dims = scope.dims.get(name.as_str()).ok_or_else(|| {
            LowerError::contract_violation(
                format!(
                    "projected scalar selection for `{}` has values but no dimensions",
                    name.as_str()
                ),
                *span,
            )
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

    fn apply_static_while_statement(
        &self,
        function: &rumoca_core::Function,
        block: &rumoca_core::StatementBlock,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        for _ in 0..MAX_STATIC_WHILE_PROJECTION_ITERATIONS {
            let condition = self.substitute(&block.cond, scope)?;
            let Some(value) = self.compile_time_scalar_in_scope(&condition, scope)? else {
                return Err(unsupported_at(
                    format!(
                        "function `{}` contains a while statement with a non-static condition",
                        function.name
                    ),
                    span,
                ));
            };
            if value == 0.0 {
                return Ok(());
            }
            for statement in &block.stmts {
                self.apply_statement(function, statement, scope, projected, depth + 1, span)?;
            }
        }
        Err(projection_budget_exceeded(function).with_fallback_span(span))
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
        )? {
            for statement in selected {
                self.apply_statement(
                    function,
                    statement,
                    scope,
                    projected,
                    if_statement.depth + 1,
                    if_statement.span,
                )?;
            }
            return Ok(());
        }

        let entry_scope = scope.clone();
        let mut branch_conditions = projection_vec_with_capacity(
            if_statement.cond_blocks.len(),
            "if projection branch condition count",
            if_statement.span,
        )?;
        let mut branch_scopes = projection_vec_with_capacity(
            if_statement.cond_blocks.len(),
            "if projection branch scope count",
            if_statement.span,
        )?;
        for block in if_statement.cond_blocks {
            let condition = self.substitute(&block.cond, &entry_scope)?;
            let mut branch_scope = entry_scope.clone();
            let mut branch_projected = projection_vec_with_capacity(
                block.stmts.len(),
                "if projection branch projected output count",
                if_statement.span,
            )?;
            for statement in &block.stmts {
                self.apply_statement(
                    function,
                    statement,
                    &mut branch_scope,
                    &mut branch_projected,
                    if_statement.depth + 1,
                    if_statement.span,
                )?;
            }
            branch_conditions.push(condition);
            branch_scopes.push(branch_scope);
        }

        let mut else_scope = entry_scope.clone();
        if let Some(statements) = if_statement.else_block {
            let mut else_projected = projection_vec_with_capacity(
                statements.len(),
                "if projection else projected output count",
                if_statement.span,
            )?;
            for statement in statements {
                self.apply_statement(
                    function,
                    statement,
                    &mut else_scope,
                    &mut else_projected,
                    if_statement.depth + 1,
                    if_statement.span,
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
    ) -> Result<Option<&'stmt [rumoca_core::Statement]>, LowerError> {
        for block in cond_blocks {
            let condition = self.substitute(&block.cond, scope)?;
            let Some(value) = self.compile_time_scalar_in_scope(&condition, scope)? else {
                return Ok(None);
            };
            if value != 0.0 {
                return Ok(Some(&block.stmts));
            }
        }
        Ok(Some(else_block.as_deref().unwrap_or(&[])))
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
        for name in projection_scope_names(entry_scope, branch_scopes, else_scope, span)? {
            if !entry_scope.scalars.contains_key(&name)
                && !entry_scope.full.contains_key(&name)
                && !entry_scope.dims.contains_key(&name)
                && !else_scope.scalars.contains_key(&name)
                && !else_scope.full.contains_key(&name)
                && !else_scope.dims.contains_key(&name)
            {
                continue;
            }
            if projection_scope_has_scalars(&name, entry_scope, branch_scopes, else_scope) {
                let values = self.merged_if_scalar_values(
                    &name,
                    entry_scope,
                    branch_conditions,
                    branch_scopes,
                    else_scope,
                    span,
                )?;
                let dims = required_merged_projection_dims(
                    &name,
                    entry_scope,
                    branch_scopes,
                    else_scope,
                    span,
                )?;
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
        let mut merged = projection_vec_with_capacity(
            base_values.len(),
            "if projection scalar value count",
            span,
        )?;
        for value in base_values {
            merged.push(value.clone());
        }
        for (condition, branch_scope) in branch_conditions.iter().zip(branch_scopes.iter()).rev() {
            let Some(branch_values) = branch_scope.scalars.get(name) else {
                continue;
            };
            if branch_values.len() != merged.len() {
                return Err(LowerError::contract_violation(
                    format!(
                        "if-statement assignment to `{name}` has mismatched scalar widths: branch {}, else/current {}",
                        branch_values.len(),
                        merged.len()
                    ),
                    span,
                ));
            }
            let mut next_merged = projection_vec_with_capacity(
                branch_values.len(),
                "if projection merged scalar value count",
                span,
            )?;
            for (branch, fallback) in branch_values.iter().cloned().zip(merged) {
                next_merged.push(rumoca_core::Expression::If {
                    branches: single_projection_branch(condition.clone(), branch, span)?,
                    else_branch: Box::new(fallback),
                    span,
                });
            }
            merged = next_merged;
        }
        Ok(merged)
    }

    #[allow(clippy::excessive_nesting)]
    fn projected_outputs_from_scope(
        &self,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        let function_span = inherited_projection_span(function.span, owner_span);
        let mut outputs = projection_vec_with_capacity(
            function.outputs.len(),
            "projected function output count",
            function_span,
        )?;
        for output in &function.outputs {
            let output_span = inherited_projection_span(output.span, function_span);
            let output_dims = self.function_param_projection_dims(output, scope, output_span)?;
            if let Some(values) = scope.scalars.get(output.name.as_str()) {
                let dims = if output_dims.is_empty() {
                    match scope.dims.get(output.name.as_str()) {
                        Some(dims) => dims.clone(),
                        None => function_outputs_dims(values.len(), output_span)?,
                    }
                } else {
                    output_dims
                };
                let projected = self.tag_function_output_projection(
                    function,
                    output,
                    project_target_scalar_outputs(&dims, values.clone(), output_span)?,
                    output_span,
                )?;
                append_projected_outputs(
                    &mut outputs,
                    projected,
                    "projected scalar output count",
                    output_span,
                )?;
                continue;
            }
            if let Some(expr) = scope.full.get(output.name.as_str()) {
                let projected = self.project_output_expr(
                    function,
                    output,
                    expr,
                    scope,
                    depth + 1,
                    output_span,
                    &output_dims,
                )?;
                let projected =
                    self.tag_function_output_projection(function, output, projected, output_span)?;
                append_projected_outputs(
                    &mut outputs,
                    projected,
                    "projected full output count",
                    output_span,
                )?;
            }
        }
        Ok((!outputs.is_empty()).then_some(outputs))
    }

    #[allow(clippy::excessive_nesting, clippy::too_many_arguments)]
    fn project_output_expr(
        &self,
        function: &rumoca_core::Function,
        output: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        output_span: rumoca_core::Span,
        output_dims: &[i64],
    ) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
        if output_dims.is_empty() {
            if Self::function_output_is_record_like(output) {
                let expr = self.substitute(expr, scope)?;
                if let Some(projected) = self.project_record_like_output_expr(
                    output,
                    &expr,
                    scope,
                    depth + 1,
                    output_span,
                )? {
                    return Ok(projected);
                }
                let mut projected = projection_vec_with_capacity(
                    1,
                    "record function output projection count",
                    output_span,
                )?;
                projected.push(ProjectedFunctionOutput {
                    field_path: Vec::new(),
                    selector_indices: Vec::new(),
                    expr,
                });
                return Ok(projected);
            }
            let expr_span = inherited_projection_source_span(expr.span(), output_span);
            if let Some(expr_dims) = self.expr_dims_with_owner(expr, scope, depth + 1, expr_span)?
                && !expr_dims.is_empty()
            {
                let values = self
                    .project_value_scalars(expr, &expr_dims, scope, depth + 1, expr_span)
                    .map_err(|err| err.with_fallback_span(expr_span))?
                    .ok_or_else(|| {
                        unsupported_at(
                            format!("function output `{}` could not be projected", output.name),
                            expr_span,
                        )
                    })?;
                return project_target_scalar_outputs(&expr_dims, values, output_span);
            }
            if let Some(expr_dims) = self.vectorized_scalar_expr_dims(expr, function, scope)? {
                let values = self
                    .project_value_scalars(expr, &expr_dims, scope, depth + 1, expr_span)
                    .map_err(|err| err.with_fallback_span(expr_span))?
                    .ok_or_else(|| {
                        unsupported_at(
                            format!("function output `{}` could not be projected", output.name),
                            expr_span,
                        )
                    })?;
                return project_target_scalar_outputs(&expr_dims, values, output_span);
            }
            let expr = expr.clone();
            let expr = if matches!(
                expr,
                rumoca_core::Expression::FunctionCall {
                    is_constructor: false,
                    ..
                }
            ) {
                match self.function_call_outputs_with_projection_scope(
                    &expr,
                    depth + 1,
                    output_span,
                    Some(scope),
                )? {
                    Some(outputs) if outputs.len() == 1 => outputs
                        .into_iter()
                        .next()
                        .map(|output| output.expr)
                        .unwrap_or(expr),
                    _ => expr,
                }
            } else {
                expr
            };
            let expr =
                self.normalize_projected_scalar_output_expr(&expr, scope, depth + 1, output_span)?;
            let mut projected = projection_vec_with_capacity(
                1,
                "scalar function output projection count",
                output_span,
            )?;
            projected.push(ProjectedFunctionOutput {
                field_path: Vec::new(),
                selector_indices: Vec::new(),
                expr: expr.clone(),
            });
            return Ok(projected);
        }
        let expr_span = inherited_projection_source_span(expr.span(), output_span);
        let values = self
            .project_value_scalars(expr, output_dims, scope, depth, expr_span)
            .map_err(|err| err.with_fallback_span(expr_span))?
            .ok_or_else(|| {
                unsupported_at(
                    format!("function output `{}` could not be projected", output.name),
                    expr_span,
                )
            })?;
        project_target_scalar_outputs(output_dims, values, output_span)
    }

    fn tag_function_output_projection(
        &self,
        function: &rumoca_core::Function,
        output: &rumoca_core::FunctionParam,
        projected: Vec<ProjectedFunctionOutput>,
        span: rumoca_core::Span,
    ) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
        if function.outputs.len() <= 1 {
            return Ok(projected);
        }
        let prefix = single_field_path(&output.name, span)?;
        let mut tagged = projection_vec_with_capacity(
            projected.len(),
            "tagged projected function output count",
            span,
        )?;
        for mut output in projected {
            let mut field_path = prefix.clone();
            field_path.append(&mut output.field_path);
            output.field_path = field_path;
            tagged.push(output);
        }
        Ok(tagged)
    }

    fn project_record_like_output_expr(
        &self,
        output: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        output_span: rumoca_core::Span,
    ) -> Result<Option<Vec<ProjectedFunctionOutput>>, LowerError> {
        let constructor_name = rumoca_core::VarName::new(&output.type_name);
        let Some(constructor) = self.dae_model.symbols.functions.get(&constructor_name) else {
            return Ok(None);
        };
        if !is_record_constructor_signature(&output.type_name, constructor)
            && !constructor.is_constructor
        {
            return Ok(None);
        }
        let mut projected = projection_vec_with_capacity(
            constructor.inputs.len(),
            "record-like function output field count",
            output_span,
        )?;
        for input in &constructor.inputs {
            let input_span = inherited_projection_span(input.span, output_span);
            let Some(field_expr) =
                self.project_record_field_value(expr, &input.name, scope, input_span)?
            else {
                return Ok(None);
            };
            let Some((projection_dims, scalars)) = self.optional_constructor_input_scalars(
                &field_expr,
                input,
                scope,
                depth,
                input_span,
            )?
            else {
                return Ok(None);
            };
            reserve_projection_capacity(
                &mut projected,
                scalars.len(),
                "record-like function output scalar count",
                input_span,
            )?;
            for (idx, expr) in scalars.into_iter().enumerate() {
                projected.push(ProjectedFunctionOutput {
                    field_path: single_field_path(&input.name, input_span)?,
                    selector_indices: required_flat_index_to_subscripts(
                        &projection_dims,
                        idx,
                        input_span,
                    )?,
                    expr,
                });
            }
        }
        Ok(Some(projected))
    }

    fn function_output_is_record_like(output: &rumoca_core::FunctionParam) -> bool {
        !output.type_name.is_empty()
            && !rumoca_core::qualified_type_name_matches(&output.type_name, "Real")
            && !rumoca_core::qualified_type_name_matches(&output.type_name, "Integer")
            && !rumoca_core::qualified_type_name_matches(&output.type_name, "Boolean")
            && !rumoca_core::qualified_type_name_matches(&output.type_name, "String")
    }

    fn normalize_projected_scalar_output_expr(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, LowerError> {
        match expr {
            rumoca_core::Expression::Binary {
                op,
                lhs,
                rhs,
                span: expr_span,
            } => {
                let span = inherited_projection_span(*expr_span, span);
                let lhs = self.normalize_projected_scalar_output_expr(lhs, scope, depth, span)?;
                let rhs = self.normalize_projected_scalar_output_expr(rhs, scope, depth, span)?;
                let op = self.projected_binary_op(op, &lhs, &rhs, scope, depth, span)?;
                Ok(rumoca_core::Expression::Binary {
                    op,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    span,
                })
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span: expr_span,
            } => {
                let span = inherited_projection_span(*expr_span, span);
                let mut normalized_branches = projection_vec_with_capacity(
                    branches.len(),
                    "projected scalar output if branch count",
                    span,
                )?;
                for (condition, value) in branches {
                    normalized_branches.push((
                        self.substitute(condition, scope)?,
                        self.normalize_projected_scalar_output_expr(value, scope, depth, span)?,
                    ));
                }
                let normalized_else =
                    self.normalize_projected_scalar_output_expr(else_branch, scope, depth, span)?;
                Ok(rumoca_core::Expression::If {
                    branches: normalized_branches,
                    else_branch: Box::new(normalized_else),
                    span,
                })
            }
            rumoca_core::Expression::VarRef { .. } => {
                let substituted = self.substitute(expr, scope)?;
                if substituted == *expr {
                    Ok(expr.clone())
                } else {
                    self.normalize_projected_scalar_output_expr(
                        &substituted,
                        scope,
                        depth + 1,
                        span,
                    )
                }
            }
            rumoca_core::Expression::FieldAccess {
                base,
                field,
                span: expr_span,
            } if matches!(base.as_ref(), rumoca_core::Expression::FunctionCall { .. }) => {
                let span = inherited_projection_span(*expr_span, span);
                let base = self.substitute(base, scope)?;
                let field_expr = rumoca_core::Expression::FieldAccess {
                    base: Box::new(base),
                    field: field.clone(),
                    span,
                };
                if let Some(mut outputs) = self.function_call_projected_scalars_with_scope(
                    &field_expr,
                    scope,
                    depth,
                    span,
                )? && outputs.len() == 1
                {
                    return Ok(outputs.remove(0));
                }
                Ok(field_expr)
            }
            _ => Ok(expr.clone()),
        }
    }

    fn function_call_projected_scalars_with_scope(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
        let Some((call, field)) = entrypoints::function_field_access(expr) else {
            return Ok(None);
        };
        let Some(outputs) =
            self.function_call_outputs_with_projection_scope(call, depth + 1, span, Some(scope))?
        else {
            return Ok(None);
        };
        let mut selected = projection_vec_with_capacity(
            outputs.len(),
            "projected function field scalar count",
            span,
        )?;
        for output in outputs {
            if let Some(expr) = self.project_output_field_value(output, field, scope, span)? {
                selected.push(expr);
            }
        }
        Ok((!selected.is_empty()).then_some(selected))
    }

    #[allow(clippy::excessive_nesting)]
    fn resolve_projected_scalar_field_outputs(
        &self,
        outputs: Vec<ProjectedFunctionOutput>,
        span: rumoca_core::Span,
    ) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
        let scope = FunctionProjectionScope::default();
        let mut resolved = projection_vec_with_capacity(
            outputs.len(),
            "resolved projected scalar field output count",
            span,
        )?;
        for mut output in outputs {
            if let rumoca_core::Expression::FieldAccess {
                base,
                field,
                span: expr_span,
            } = &output.expr
                && matches!(base.as_ref(), rumoca_core::Expression::FunctionCall { .. })
            {
                let field_expr = rumoca_core::Expression::FieldAccess {
                    base: base.clone(),
                    field: field.clone(),
                    span: inherited_projection_span(*expr_span, span),
                };
                if let Some(mut values) =
                    self.function_call_projected_scalars_with_scope(&field_expr, &scope, 0, span)?
                    && values.len() == 1
                {
                    output.expr = values.remove(0);
                }
            }
            resolved.push(output);
        }
        Ok(resolved)
    }

    fn projected_binary_op(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::OpBinary, LowerError> {
        if !is_div(op) {
            return Ok(op.clone());
        }
        let lhs_dims = self.expr_dims_with_owner(lhs, scope, depth, span)?;
        let rhs_dims = self.expr_dims_with_owner(rhs, scope, depth, span)?;
        if lhs_dims.as_ref().is_some_and(|dims| !dims.is_empty())
            || rhs_dims.as_ref().is_some_and(|dims| !dims.is_empty())
        {
            Ok(rumoca_core::OpBinary::DivElem)
        } else {
            Ok(op.clone())
        }
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
        let constructor_span = inherited_projection_source_span(value.span(), constructor.span);
        let named_spans = named_argument_spans(args, constructor_span)?;
        let mut positional_idx = 0usize;
        let mut outputs = projection_vec_with_capacity(
            constructor.inputs.len(),
            "record constructor projected output count",
            constructor_span,
        )?;
        for input in &constructor.inputs {
            let input_span = inherited_projection_span(input.span, constructor_span);
            let actual = if let Some(actual) = named.get(input.name.as_str()).copied() {
                Some((
                    actual,
                    inherited_projection_span(
                        named_actual_span(&named_spans, input, actual),
                        input_span,
                    ),
                ))
            } else {
                let actual = positional.get(positional_idx).copied();
                positional_idx += usize::from(actual.is_some());
                match actual {
                    Some(actual) => Some(projection_actual_with_span(actual, input_span)?),
                    None => None,
                }
            };
            let actual = match actual {
                Some(actual) => Some(actual),
                None => input
                    .default
                    .as_ref()
                    .map(|actual| projection_actual_with_span(actual, input_span))
                    .transpose()?,
            };
            let Some((actual, actual_span)) = actual else {
                return Ok(None);
            };
            let actual = actual.clone();
            let actual = self.substitute(&actual, scope)?;
            let Some((projection_dims, scalars)) = self.optional_constructor_input_scalars(
                &actual,
                input,
                scope,
                depth + 1,
                actual_span,
            )?
            else {
                continue;
            };
            reserve_projection_capacity(
                &mut outputs,
                scalars.len(),
                "record constructor projected field output count",
                input_span,
            )?;
            for (idx, expr) in scalars.into_iter().enumerate() {
                outputs.push(ProjectedFunctionOutput {
                    field_path: single_field_path(&input.name, input_span)?,
                    selector_indices: required_flat_index_to_subscripts(
                        &projection_dims,
                        idx,
                        input_span,
                    )?,
                    expr,
                });
            }
        }
        Ok(Some(outputs))
    }

    fn optional_constructor_input_scalars(
        &self,
        actual: &rumoca_core::Expression,
        input: &rumoca_core::FunctionParam,
        scope: &FunctionProjectionScope,
        depth: usize,
        actual_span: rumoca_core::Span,
    ) -> Result<Option<ConstructorInputScalars>, LowerError> {
        let span = projection_arg_or_context_span(actual, actual_span)?;
        let Some(mut dims) = constructor_input_projection_dims(
            input,
            self.expr_dims_with_owner(actual, scope, depth + 1, span)?,
            span,
        )?
        else {
            return Ok(None);
        };
        if (input.dims.as_slice() == [0]
            || scalar_count_for_dims(&dims, "record constructor input dimensions", span)? == 0)
            && let rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } = actual
            && let Some(selected) = self.compile_time_if_selection(branches, else_branch, scope)?
            && let Some(selected_dims) =
                self.expr_dims_with_owner(selected, scope, depth + 1, span)?
            && !selected_dims.is_empty()
        {
            dims = selected_dims;
        }
        match self
            .project_value_scalars(actual, &dims, scope, depth, span)
            .map_err(|err| err.with_fallback_span(span))?
        {
            Some(scalars) => Ok(Some((dims, scalars))),
            None if actual.contains_der() => Err(unsupported_at(
                "record constructor derivative input could not be projected",
                span,
            )),
            None => Ok(None),
        }
    }

    fn substitute(
        &self,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
    ) -> Result<rumoca_core::Expression, LowerError> {
        let mut substituter = FunctionScopeSubstituter {
            scope,
            error: None,
            stack: Vec::new(),
        };
        let expr = substituter.rewrite_expression(expr);
        if let Some(error) = substituter.error {
            return Err(error);
        }
        Ok(expr)
    }

    fn project_value_scalars(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
        let span = inherited_projection_source_span(expr.span(), owner_span);
        let count = scalar_count_for_dims(dims, "projected value dimensions", span)?;
        if let Some(values) =
            self.project_function_call_scalars_once(expr, dims, count, scope, depth, span)?
        {
            return Ok(Some(values));
        }
        (0..count)
            .map(|idx| self.project_value(expr, dims, idx, scope, depth, span))
            .collect::<Result<Option<Vec<_>>, _>>()
    }

    fn project_function_call_scalars_once(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        count: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
        let mut substituted = self.substitute(expr, scope)?;
        if substituted.span().is_none() {
            substituted = substituted.with_span(owner_span);
        }
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } = &substituted
        else {
            return Ok(None);
        };
        if is_stream_passthrough_intrinsic(name.as_str()) {
            let Some(arg) = args.first() else {
                return Ok(None);
            };
            return self.project_value_scalars(arg, dims, scope, depth + 1, owner_span);
        }
        let outputs = match self.function_call_outputs_with_projection_scope(
            &substituted,
            depth + 1,
            owner_span,
            Some(scope),
        ) {
            Ok(outputs) => outputs,
            Err(err) if err.is_projection_budget_exceeded() => return Ok(None),
            Err(err) => return Err(err),
        };
        let Some(outputs) = outputs else {
            return Ok(None);
        };
        if outputs.len() != count {
            return Ok(None);
        }
        let mut scalars = projection_vec_with_capacity(
            outputs.len(),
            "projected function call scalar output count",
            owner_span,
        )?;
        for output in outputs {
            scalars.push(output.expr);
        }
        Ok(Some(scalars))
    }

    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    fn project_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let owner_span = inherited_projection_source_span(expr.span(), owner_span);
        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr {
            let span = inherited_projection_span(*span, owner_span);
            if let rumoca_core::Expression::FunctionCall { .. } = base.as_ref() {
                let mut call = self.substitute(base, scope)?;
                if call.span().is_none() {
                    call = call.with_span(span);
                }
                if let Some(outputs) = self.function_call_outputs_with_projection_scope(
                    &call,
                    depth + 1,
                    span,
                    Some(scope),
                )? {
                    for output in outputs {
                        if let Some(expr) =
                            self.project_output_field_value(output, field, scope, span)?
                        {
                            return Ok(Some(expr.with_span(span)));
                        }
                    }
                }
            }
            if let rumoca_core::Expression::FunctionCall { args, .. } = base.as_ref() {
                for arg in args {
                    if let Some((name, value)) =
                        super::super::function_calls::decode_named_function_arg(arg)
                        && name == field
                    {
                        return Ok(Some(self.substitute(value, scope)?.with_span(span)));
                    }
                }
            }
        }
        if dims.is_empty() {
            return Ok(Some(self.substitute(expr, scope)?));
        }
        match expr {
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() => {
                let span = inherited_projection_span(*span, owner_span);
                if let Some(values) = scope.scalars.get(name.as_str()) {
                    let value = assigned_projected_scalar_value(
                        name.as_str(),
                        dims,
                        values,
                        flat_index,
                        span,
                    )?;
                    return Ok(Some(value.clone().with_span(span)));
                }
                if let Some(value) = scope.full.get(name.as_str())
                    && !is_same_plain_var_ref(value, name.as_str())
                {
                    if let Some(projected) =
                        self.project_value(value, dims, flat_index, scope, depth + 1, span)?
                    {
                        return Ok(Some(projected.with_span(span)));
                    }
                    return Ok(Some(self.substitute(value, scope)?.with_span(span)));
                }
                let indices = required_flat_index_to_subscripts(dims, flat_index, span)?;
                let name = self.reference_with_dae_component_ref(name);
                Ok(Some(rumoca_core::Expression::VarRef {
                    name: project_reference_field_path_and_indices(&name, &[], &indices, span)?,
                    subscripts: Vec::new(),
                    span,
                }))
            }
            rumoca_core::Expression::Array {
                elements,
                is_matrix,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
                self.project_array_expression_value(elements, *is_matrix, &ctx)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
                self.project_if_value(branches, else_branch, &ctx)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Der,
                args,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                self.project_derivative_value(args, span, dims, flat_index, scope, depth)
            }
            rumoca_core::Expression::Unary { op, rhs, span } => {
                let span = inherited_projection_span(*span, owner_span);
                let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
                self.project_unary_value(op, rhs, &ctx)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Transpose,
                args,
                ..
            } if args.len() == 1 => {
                self.project_transpose_value(&args[0], dims, flat_index, scope, depth, owner_span)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Identity,
                args,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                self.project_identity_value(args, dims, flat_index, scope, span)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill,
                args,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                self.project_fill_value(args, scope, span)
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Cat,
                args,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                self.project_cat_value(args, dims, flat_index, scope, depth, span)
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } if is_elementwise_builtin_projection(function) => {
                let span = inherited_projection_span(*span, owner_span);
                let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
                self.project_builtin_call_value(function, args, &ctx)
            }
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                span,
            } if is_stream_passthrough_intrinsic(name.as_str()) => {
                let span = inherited_projection_span(*span, owner_span);
                match args.first() {
                    Some(arg) => self.project_value(arg, dims, flat_index, scope, depth + 1, span),
                    None => Ok(None),
                }
            }
            rumoca_core::Expression::FunctionCall {
                is_constructor: false,
                ..
            } => self.project_function_call_value(expr, dims, flat_index, scope, depth, owner_span),
            rumoca_core::Expression::Binary { op, lhs, rhs, span }
                if is_elementwise_binary_projection_op(op) =>
            {
                let span = inherited_projection_span(*span, owner_span);
                let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
                self.project_binary_value(op, lhs, rhs, &ctx)
            }
            other => self.project_indexed_value(other, dims, flat_index, scope, owner_span),
        }
    }

    fn project_cat_value(
        &self,
        args: &[rumoca_core::Expression],
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let Some(dim_expr) = args.first() else {
            return Ok(None);
        };
        let Some(dim_value) = self.compile_time_scalar_in_scope(dim_expr, scope)? else {
            return Ok(None);
        };
        if (dim_value - 1.0).abs() > f64::EPSILON {
            return Ok(None);
        }
        let mut offset = 0usize;
        for operand in &args[1..] {
            let Some(operand_dims) = self.expr_dims_with_owner(operand, scope, depth + 1, span)?
            else {
                return Ok(None);
            };
            if operand_dims.is_empty() || operand_dims.len() != dims.len() {
                return Ok(None);
            }
            if operand_dims.len() > 1 && operand_dims[1..] != dims[1..] {
                return Ok(None);
            }
            let operand_count =
                scalar_count_for_dims(&operand_dims, "cat operand scalar count", span)?;
            if flat_index < offset + operand_count {
                return self.project_value(
                    operand,
                    &operand_dims,
                    flat_index - offset,
                    scope,
                    depth + 1,
                    span,
                );
            }
            offset += operand_count;
        }
        Ok(None)
    }

    fn project_array_expression_value(
        &self,
        elements: &[rumoca_core::Expression],
        is_matrix: bool,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let child_dims = self.array_child_dims(elements, ctx.scope, ctx.depth, ctx.span)?;
        let array_ctx = ArrayProjectionValueCtx {
            elements,
            child_dims: &child_dims,
            projection: ctx,
        };
        if is_matrix && let Some(expr) = self.project_matrix_array_expression_value(&array_ctx)? {
            return Ok(Some(expr));
        }
        if let Some(expr) = self.project_array_sequence_value(
            elements,
            ctx.flat_index,
            &child_dims,
            ctx.scope,
            ctx.depth,
            ctx.span,
        )? {
            return Ok(Some(expr));
        }
        let flattened = flatten_array_elements(elements, ctx.span)?;
        let Some(element) = flattened.get(ctx.flat_index).cloned() else {
            return Err(LowerError::contract_violation(
                format!(
                    "flat index {} is out of bounds for array expression",
                    ctx.flat_index
                ),
                ctx.span,
            ));
        };
        Ok(Some(self.substitute(&element, ctx.scope)?))
    }

    fn array_child_dims(
        &self,
        elements: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Vec<Option<Vec<i64>>>, LowerError> {
        elements
            .iter()
            .map(|element| self.expr_dims_with_owner(element, scope, depth + 1, span))
            .collect()
    }

    fn project_array_sequence_value(
        &self,
        elements: &[rumoca_core::Expression],
        flat_index: usize,
        child_dims: &[Option<Vec<i64>>],
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let mut offset = 0usize;
        for (element, dims) in elements.iter().zip(child_dims) {
            let width = array_element_scalar_width(dims.as_deref(), span)?;
            let end = offset.checked_add(width).ok_or_else(|| {
                LowerError::contract_violation(
                    "array expression scalar offset overflows host index range",
                    span,
                )
            })?;
            if flat_index < end {
                return self.project_array_child_value(
                    element,
                    dims.as_deref(),
                    flat_index - offset,
                    scope,
                    depth + 1,
                    span,
                );
            }
            offset = end;
        }
        Ok(None)
    }

    fn project_matrix_array_expression_value(
        &self,
        ctx: &ArrayProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if matrix_elements_are_row_literals(ctx.elements) {
            return self.project_array_sequence_value(
                ctx.elements,
                ctx.projection.flat_index,
                ctx.child_dims,
                ctx.projection.scope,
                ctx.projection.depth,
                ctx.projection.span,
            );
        }
        let [rows, cols] = ctx.projection.dims else {
            return Ok(None);
        };
        let rows = valid_product_dim(*rows, ctx.projection.span, "matrix expression row count")?;
        let cols = valid_product_dim(*cols, ctx.projection.span, "matrix expression column count")?;
        if rows == 0 || cols == 0 {
            return Ok(None);
        }
        let row = ctx.projection.flat_index / cols;
        let col = ctx.projection.flat_index % cols;
        if row >= rows {
            return Ok(None);
        }
        self.project_matrix_column_value(ctx, row, col, rows)
    }

    fn project_matrix_column_value(
        &self,
        ctx: &ArrayProjectionValueCtx<'_>,
        row: usize,
        col: usize,
        rows: usize,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let mut offset = 0usize;
        for (element, dims) in ctx.elements.iter().zip(ctx.child_dims) {
            let Some(cols) =
                matrix_column_operand_count(dims.as_deref(), rows, ctx.projection.span)?
            else {
                return Ok(None);
            };
            let end = offset.checked_add(cols).ok_or_else(|| {
                LowerError::contract_violation(
                    "matrix expression column offset overflows host index range",
                    ctx.projection.span,
                )
            })?;
            if col < end {
                let child_flat_index = matrix_column_child_flat_index(
                    dims.as_deref(),
                    row,
                    col - offset,
                    ctx.projection.span,
                )?;
                return self.project_array_child_value(
                    element,
                    dims.as_deref(),
                    child_flat_index,
                    ctx.projection.scope,
                    ctx.projection.depth + 1,
                    ctx.projection.span,
                );
            }
            offset = end;
        }
        Ok(None)
    }

    fn project_array_child_value(
        &self,
        element: &rumoca_core::Expression,
        dims: Option<&[i64]>,
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        match dims {
            Some(dims) if !dims.is_empty() => {
                self.project_value(element, dims, flat_index, scope, depth, span)
            }
            _ if flat_index == 0 => Ok(Some(self.substitute(element, scope)?)),
            _ => Err(LowerError::contract_violation(
                format!("flat index {flat_index} is out of bounds for scalar array element"),
                span,
            )),
        }
    }

    fn project_derivative_value(
        &self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let [arg] = args else {
            return Err(LowerError::contract_violation(
                format!(
                    "derivative projection expected one argument, got {}",
                    args.len()
                ),
                span,
            ));
        };
        let arg = self
            .project_value(arg, dims, flat_index, scope, depth, span)?
            .ok_or_else(|| unsupported_at("derivative argument could not be projected", span))?;
        Ok(Some(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Der,
            args: vec![arg],
            span,
        }))
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
        if let Some(selected) = self.compile_time_if_selection(branches, else_branch, ctx.scope)? {
            return self.project_value(
                selected,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth + 1,
                ctx.span,
            );
        }
        let mut projected_branches =
            projection_vec_with_capacity(branches.len(), "projected if branch count", ctx.span)?;
        for (condition, branch) in branches {
            let condition = self.project_lane_or_substitute(condition, ctx)?;
            let branch = self
                .project_value(
                    branch,
                    ctx.dims,
                    ctx.flat_index,
                    ctx.scope,
                    ctx.depth + 1,
                    ctx.span,
                )?
                .ok_or_else(|| unsupported_at("if branch could not be projected", ctx.span))?;
            projected_branches.push((condition, branch));
        }
        let projected_else = self
            .project_value(
                else_branch,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth + 1,
                ctx.span,
            )?
            .ok_or_else(|| unsupported_at("if else branch could not be projected", ctx.span))?;
        Ok(Some(rumoca_core::Expression::If {
            branches: projected_branches,
            else_branch: Box::new(projected_else),
            span: ctx.span,
        }))
    }

    fn project_builtin_call_value(
        &self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if matches!(function, rumoca_core::BuiltinFunction::Homotopy) {
            let Some(actual) = args.first() else {
                return Ok(None);
            };
            return self.project_lane_or_substitute(actual, ctx).map(Some);
        }
        let mut projected_args =
            projection_vec_with_capacity(args.len(), "projected builtin argument count", ctx.span)?;
        for arg in args {
            projected_args.push(self.project_lane_or_substitute(arg, ctx)?);
        }
        Ok(Some(rumoca_core::Expression::BuiltinCall {
            function: *function,
            args: projected_args,
            span: ctx.span,
        }))
    }

    fn project_fill_value(
        &self,
        args: &[rumoca_core::Expression],
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let Some(value) = args.first() else {
            return Ok(None);
        };
        Ok(Some(self.substitute(value, scope)?.with_span(span)))
    }

    fn project_lane_or_substitute(
        &self,
        expr: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<rumoca_core::Expression, LowerError> {
        let dims = match self.expr_dims_with_owner(expr, ctx.scope, ctx.depth + 1, ctx.span)? {
            Some(dims) if !dims.is_empty() => Some(dims),
            _ => self.scope_projected_expr_dims(expr, ctx.scope, ctx.span)?,
        };
        if let Some(dims) = dims.filter(|dims| !dims.is_empty()) {
            let flat_index = if dims == ctx.dims {
                ctx.flat_index
            } else if scalar_count_for_dims(&dims, "projected child dimensions", ctx.span)? == 1 {
                0
            } else {
                return self.substitute(expr, ctx.scope);
            };
            return self
                .project_value(expr, &dims, flat_index, ctx.scope, ctx.depth + 1, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("expression argument could not be projected", ctx.span)
                });
        }
        self.substitute(expr, ctx.scope)
    }

    fn project_unary_value(
        &self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let rhs_dims = self
            .expr_dims_with_owner(rhs, ctx.scope, ctx.depth, ctx.span)?
            .ok_or_else(|| {
                unsupported_at(
                    "unary array expression has unknown operand dimensions",
                    ctx.span,
                )
            })?;
        let rhs = if rhs_dims.is_empty() {
            self.substitute(rhs, ctx.scope)?
        } else if rhs_dims == ctx.dims {
            self.project_value(
                rhs,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth + 1,
                ctx.span,
            )?
            .ok_or_else(|| unsupported_at("unary array operand could not be projected", ctx.span))?
        } else {
            return Err(unsupported_at(
                format!(
                    "unary array expression has operand dimensions {}, expected {}",
                    format_i64_dims(&rhs_dims),
                    format_i64_dims(ctx.dims)
                ),
                ctx.span,
            ));
        };
        Ok(Some(rumoca_core::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(rhs),
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
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let span = inherited_projection_source_span(arg.span(), owner_span);
        let Some(input_dims) = self.expr_dims_with_owner(arg, scope, depth, span)? else {
            return Ok(None);
        };
        let [input_rows, input_cols] = input_dims.as_slice() else {
            return Ok(None);
        };
        let output_dims = [*input_cols, *input_rows];
        if dims != output_dims {
            return Ok(None);
        }
        let output_rows = usize::try_from(*input_rows).map_err(|_| {
            LowerError::contract_violation(
                format!("transpose input has invalid row dimension `{input_rows}`"),
                span,
            )
        })?;
        let input_cols = usize::try_from(*input_cols).map_err(|_| {
            LowerError::contract_violation(
                format!("transpose input has invalid column dimension `{input_cols}`"),
                span,
            )
        })?;
        if output_rows == 0 || input_cols == 0 {
            return Ok(None);
        }
        let output_row = flat_index / output_rows;
        let output_col = flat_index % output_rows;
        let input_index = output_col * input_cols + output_row;
        self.project_value(arg, &input_dims, input_index, scope, depth, span)
    }

    fn project_function_call_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if let Some(indexed_call) =
            self.indexed_selected_output_call(expr, dims, flat_index, owner_span)?
        {
            return self.project_function_call_value(
                &indexed_call,
                &[],
                0,
                scope,
                depth + 1,
                owner_span,
            );
        }
        let outputs = match self.function_call_outputs_with_projection_scope(
            expr,
            depth + 1,
            owner_span,
            Some(scope),
        ) {
            Ok(outputs) => outputs,
            Err(err) if err.is_projection_budget_exceeded() => None,
            Err(err) => return Err(err),
        };
        if let Some(outputs) = outputs {
            let span = inherited_projection_source_span(expr.span(), owner_span);
            let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
            if let [output] = outputs.as_slice() {
                return self
                    .project_lane_or_substitute(&output.expr, &ctx)
                    .map(Some);
            }
            if function_call_declared_output_count(expr, self.dae_model)
                .is_some_and(|count| count > 1)
                && let Some(output) = outputs.first()
            {
                return self
                    .project_lane_or_substitute(&output.expr, &ctx)
                    .map(Some);
            }
            return outputs
                .get(flat_index)
                .map(|output| {
                    self.project_lane_or_substitute(&output.expr, &ctx)
                        .map(Some)
                })
                .unwrap_or(Ok(None));
        }
        let span = inherited_projection_source_span(expr.span(), owner_span);
        let mut call =
            self.project_function_call_with_lane_args(expr, dims, flat_index, scope, depth, span)?;
        if call.span().is_none() {
            call = call.with_span(owner_span);
        }
        let outputs = match self.function_call_outputs_with_owner(&call, depth + 1, owner_span) {
            Ok(outputs) => outputs,
            Err(err) if err.is_projection_budget_exceeded() => return Ok(Some(call)),
            Err(err) => return Err(err),
        };
        let Some(outputs) = outputs else {
            return Ok(Some(call));
        };
        if let [output] = outputs.as_slice() {
            return Ok(Some(output.expr.clone()));
        }
        if function_call_declared_output_count(&call, self.dae_model).is_some_and(|count| count > 1)
        {
            return Ok(outputs.first().map(|output| output.expr.clone()));
        }
        Ok(outputs.get(flat_index).map(|output| output.expr.clone()))
    }

    fn indexed_selected_output_call(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } = expr
        else {
            return Ok(None);
        };
        if dims.is_empty()
            || self
                .dae_model
                .symbols
                .functions
                .contains_key(name.var_name())
        {
            return Ok(None);
        }
        rumoca_core::find_map_top_level_splits_rev(name.as_str(), |base_name, suffix| {
            let function = self
                .dae_model
                .symbols
                .functions
                .get(&rumoca_core::VarName::new(base_name))?;
            let projection_suffix = parse_output_projection_suffix(suffix)?;
            if !projection_suffix.indices.is_empty() || projection_suffix.output_field.is_some() {
                return None;
            }
            let output = function
                .outputs
                .iter()
                .find(|output| output.name == projection_suffix.output_name)?;
            if output.dims.is_empty() || output.dims.as_slice() != dims {
                return None;
            }
            let selector = dae::scalar_name_text_for_flat_index(
                output.name.as_str(),
                &output.dims,
                flat_index,
            );
            Some((base_name.to_string(), selector))
        })
        .map(|(base_name, selector)| {
            Ok(Some(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new(format!("{base_name}.{selector}")).into(),
                args: args.clone(),
                is_constructor: false,
                span,
            }))
        })
        .unwrap_or(Ok(None))
    }

    fn project_function_call_with_lane_args(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        span: rumoca_core::Span,
    ) -> Result<rumoca_core::Expression, LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = expr
        else {
            return self.substitute(expr, scope);
        };
        let ctx = projection_value_ctx(dims, flat_index, scope, depth, span);
        let mut projected_args =
            projection_vec_with_capacity(args.len(), "projected function argument count", span)?;
        for arg in args {
            projected_args.push(self.project_lane_or_substitute(arg, &ctx)?);
        }
        Ok(rumoca_core::Expression::FunctionCall {
            name: name.clone(),
            args: projected_args,
            is_constructor: *is_constructor,
            span,
        })
    }

    fn project_output_field_value(
        &self,
        output: ProjectedFunctionOutput,
        field: &str,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let Some((head, tail)) = output.field_path.split_first() else {
            return self.project_record_field_value(&output.expr, field, scope, span);
        };
        if head != field {
            return Ok(None);
        }
        if tail.is_empty() {
            return Ok(Some(self.substitute(&output.expr, scope)?));
        }
        Ok(None)
    }

    #[allow(clippy::excessive_nesting)]
    fn project_record_field_value(
        &self,
        value: &rumoca_core::Expression,
        field: &str,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        match value {
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                if let Some(selected) =
                    self.compile_time_if_selection(branches, else_branch, scope)?
                {
                    return self.project_record_field_value(selected, field, scope, span);
                }
                let mut projected_branches = projection_vec_with_capacity(
                    branches.len(),
                    "projected record field if branch count",
                    span,
                )?;
                for (condition, branch) in branches {
                    let Some(projected_branch) =
                        self.project_record_field_value(branch, field, scope, span)?
                    else {
                        return Ok(None);
                    };
                    projected_branches.push((self.substitute(condition, scope)?, projected_branch));
                }
                let Some(projected_else) =
                    self.project_record_field_value(else_branch, field, scope, span)?
                else {
                    return Ok(None);
                };
                Ok(Some(rumoca_core::Expression::If {
                    branches: projected_branches,
                    else_branch: Box::new(projected_else),
                    span,
                }))
            }
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                for arg in args {
                    if let Some((name, actual)) =
                        super::super::function_calls::decode_named_function_arg(arg)
                        && name == field
                    {
                        return Ok(Some(self.substitute(actual, scope)?));
                    }
                }
                let Some(constructor) = self.dae_model.symbols.functions.get(name.var_name())
                else {
                    return Ok(None);
                };
                if !is_record_constructor_signature(name.as_str(), constructor)
                    && !constructor.is_constructor
                {
                    return Ok(None);
                }
                let (_named, positional) =
                    super::super::function_calls::split_named_and_positional_call_args(
                        name.as_str(),
                        args,
                    )?;
                let Some(index) = constructor
                    .inputs
                    .iter()
                    .position(|input| input.name == field)
                else {
                    return Ok(None);
                };
                if let Some(actual) = positional.get(index) {
                    return Ok(Some(self.substitute(actual, scope)?));
                }
                Ok(None)
            }
            _ => Ok(None),
        }
    }

    fn project_indexed_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let span = inherited_projection_source_span(expr.span(), owner_span);
        let indices = required_flat_index_to_subscripts(dims, flat_index, span)?;
        let mut subscripts = projection_vec_with_capacity(
            indices.len(),
            "projected expression subscript count",
            span,
        )?;
        for idx in indices {
            subscripts.push(checked_generated_subscript_from_usize(
                idx,
                span,
                "projected expression index subscript",
            )?);
        }
        Ok(Some(rumoca_core::Expression::Index {
            base: Box::new(self.substitute(expr, scope)?),
            subscripts,
            span,
        }))
    }

    fn project_identity_value(
        &self,
        args: &[rumoca_core::Expression],
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        if dims.len() != 2 || dims[0] != dims[1] {
            return Ok(None);
        }
        let Some(dim_expr) = args.first() else {
            return Ok(None);
        };
        let Some(dim_value) = self.compile_time_scalar_in_scope(dim_expr, scope)? else {
            return Ok(None);
        };
        let dim = i64::try_from(dim_value as i128).map_err(|_| {
            LowerError::contract_violation("identity dimension is outside host range", span)
        })?;
        if dim <= 0 || dim != dims[0] {
            return Ok(None);
        }
        let dim = usize::try_from(dim).map_err(|_| {
            LowerError::contract_violation("identity dimension is outside host range", span)
        })?;
        let row = flat_index / dim;
        let col = flat_index % dim;
        Ok(Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(if row == col { 1.0 } else { 0.0 }),
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
        let lhs_dims = self.known_expr_dims(lhs, ctx.scope, ctx.depth, "binary lhs", ctx.span)?;
        let rhs_dims = self.known_expr_dims(rhs, ctx.scope, ctx.depth, "binary rhs", ctx.span)?;
        let lhs_expr = if lhs_dims.is_empty() {
            self.project_lane_or_substitute(lhs, ctx)?
        } else {
            let flat_index = projected_child_flat_index(&lhs_dims, ctx.flat_index);
            self.project_value(lhs, &lhs_dims, flat_index, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| unsupported_at("binary lhs could not be projected", ctx.span))?
        };
        let rhs_expr = if rhs_dims.is_empty() {
            self.project_lane_or_substitute(rhs, ctx)?
        } else {
            let flat_index = projected_child_flat_index(&rhs_dims, ctx.flat_index);
            self.project_value(rhs, &rhs_dims, flat_index, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| unsupported_at("binary rhs could not be projected", ctx.span))?
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
        let Some(lhs_dims) = self.expr_dims_with_owner(lhs, ctx.scope, ctx.depth, ctx.span)? else {
            return Ok(None);
        };
        let Some(rhs_dims) = self.expr_dims_with_owner(rhs, ctx.scope, ctx.depth, ctx.span)? else {
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
        let mut terms =
            projection_vec_with_capacity(cols, "matrix-vector product term count", ctx.span)?;
        for col in 0..cols {
            let lhs_idx = checked_projection_offset(
                row,
                cols,
                col,
                "matrix-vector lhs flat index",
                ctx.span,
            )?;
            let lhs_term = self
                .project_value(
                    lhs,
                    product_dims.lhs_dims,
                    lhs_idx,
                    ctx.scope,
                    ctx.depth,
                    ctx.span,
                )?
                .ok_or_else(|| {
                    unsupported_at("matrix-vector lhs could not be projected", ctx.span)
                })?;
            let rhs_term = self
                .project_value(
                    rhs,
                    product_dims.rhs_dims,
                    col,
                    ctx.scope,
                    ctx.depth,
                    ctx.span,
                )?
                .ok_or_else(|| {
                    unsupported_at("matrix-vector rhs could not be projected", ctx.span)
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
        let lhs_dims = [checked_usize_to_i64(rows, "vector-matrix rows", ctx.span)?];
        let mut terms =
            projection_vec_with_capacity(rows, "vector-matrix product term count", ctx.span)?;
        for row in 0..rows {
            let lhs_term = self
                .project_value(lhs, &lhs_dims, row, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("vector-matrix lhs could not be projected", ctx.span)
                })?;
            let rhs_idx = checked_projection_offset(
                row,
                cols,
                col,
                "vector-matrix rhs flat index",
                ctx.span,
            )?;
            let rhs_term = self
                .project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("vector-matrix rhs could not be projected", ctx.span)
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
        if cols == 0 {
            return Ok(None);
        }
        let row = ctx.flat_index / cols;
        let col = ctx.flat_index % cols;
        let mut terms =
            projection_vec_with_capacity(inner, "matrix-matrix product term count", ctx.span)?;
        for inner_idx in 0..inner {
            let lhs_idx = checked_projection_offset(
                row,
                inner,
                inner_idx,
                "matrix-matrix lhs flat index",
                ctx.span,
            )?;
            let rhs_idx = checked_projection_offset(
                inner_idx,
                cols,
                col,
                "matrix-matrix rhs flat index",
                ctx.span,
            )?;
            let lhs_term = self
                .project_value(lhs, lhs_dims, lhs_idx, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("matrix-matrix lhs could not be projected", ctx.span)
                })?;
            let rhs_term = self
                .project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth, ctx.span)?
                .ok_or_else(|| {
                    unsupported_at("matrix-matrix rhs could not be projected", ctx.span)
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
}

fn split_flattened_projection_input_name(name: &str) -> Option<(&str, &str)> {
    let (prefix, field) = name.split_once('_')?;
    (!prefix.is_empty() && !field.is_empty()).then_some((prefix, field))
}

fn flattened_projection_input_has_prefix(name: &str, prefix: &str) -> bool {
    split_flattened_projection_input_name(name).is_some_and(|(candidate, _)| candidate == prefix)
}

fn flattened_projection_group_has_prefix(
    inputs: &[rumoca_core::FunctionParam],
    prefix: &str,
) -> bool {
    inputs
        .iter()
        .filter(|input| flattened_projection_input_has_prefix(&input.name, prefix))
        .take(2)
        .count()
        >= 2
}

fn flattened_projection_input_is_group_start(
    inputs: &[rumoca_core::FunctionParam],
    input_idx: usize,
    prefix: &str,
) -> bool {
    !inputs
        .iter()
        .take(input_idx)
        .any(|input| flattened_projection_input_has_prefix(&input.name, prefix))
}

fn only_projected_scalar_assignment_output(
    mut outputs: Vec<ProjectedFunctionOutput>,
    span: rumoca_core::Span,
) -> Result<ProjectedFunctionOutput, LowerError> {
    outputs.pop().ok_or_else(|| {
        LowerError::contract_violation(
            "projected scalar function assignment produced no output",
            span,
        )
    })
}
