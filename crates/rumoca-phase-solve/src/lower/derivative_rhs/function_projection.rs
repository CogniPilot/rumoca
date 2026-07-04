// SPEC_0021 file-size exception: function derivative projection still combines
// dependency discovery, call rewriting, and projection row generation.
// split plan: move discovery, rewriting, and row generation into focused modules.

use std::cell::RefCell;

use indexmap::IndexMap;
use rumoca_core::{ExpressionRewriter, Literal, NAMED_FUNCTION_ARG_PREFIX, OpBinary};
use rumoca_ir_dae as dae;

use crate::lower::helpers::is_stream_passthrough_intrinsic;
use crate::lower::{LowerError, unsupported_at};

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
    copy_projection_dims, declared_dims, elementwise_binary_dims,
    exact_declared_function_output_dims, flat_index_from_indices, flatten_array_elements,
    formal_actual_projection_dims, is_ignorable_projection_statement, is_same_plain_var_ref,
    named_actual_span, named_argument_spans, projected_declared_output_dims,
    projected_field_output_dims, projection_assignment_target, required_flat_index_to_subscripts,
    reserve_projection_capacity, scalar_count_for_dims, single_field_path, sum_expressions,
    valid_product_dim,
};
pub(super) use entrypoints::function_projected_residuals_with_owner;
use entrypoints::{
    checked_generated_subscript_from_usize, checked_projection_offset, checked_usize_dims_to_i64,
    checked_usize_to_i64, function_outputs_dims, project_scalar_outputs,
    project_target_scalar_outputs, required_merged_projection_dims, variable_dims_i64,
};
pub(in crate::lower) use entrypoints::{
    function_call_projected_scalars_with_owner, project_array_like_scalars_with_owner,
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
        let outputs = if projected.is_empty() {
            self.projected_outputs_from_scope(function, &scope, depth + 1, function_span)?
        } else {
            Some(projected)
        };
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
                ))
            } else if let Some((prefix, field)) = split_flattened_projection_input_name(&input.name)
                && let Some(actual) = positional.get(positional_idx).cloned()
                && (flattened_projection_group_has_prefix(&function.inputs, prefix)
                    || self.flattened_projection_actual_has_field(actual, field, &scope)
                    || caller_scope.is_some_and(|caller_scope| {
                        self.flattened_projection_actual_has_field(actual, field, caller_scope)
                    }))
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
                Some((actual, actual_span))
            } else {
                super::super::function_calls::next_positional_function_input_arg(
                    input,
                    &positional,
                    &mut positional_idx,
                )
                .map(|actual| (actual.clone(), actual.span().unwrap_or(input_span)))
            };
            let Some((actual, actual_span)) = actual.or_else(|| {
                input
                    .default
                    .as_ref()
                    .map(|actual| (actual.clone(), actual.span().unwrap_or(input_span)))
            }) else {
                return Ok(None);
            };
            if consume_flattened_positional {
                positional_idx += 1;
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
            if self.initialize_declared_default(param, scope, depth + 1, owner_span)? {
                continue;
            }
            if param.dims.is_empty() || scope.scalars.contains_key(param.name.as_str()) {
                continue;
            }
            let param_span = inherited_projection_span(param.span, owner_span);
            let count = scalar_count_for_dims(
                &param.dims,
                "function declared array dimensions",
                param_span,
            )?;
            let dims = copy_projection_dims(
                &param.dims,
                "projected declared array dimension count",
                param_span,
            )?;
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

    fn initialize_declared_default(
        &self,
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
            return Ok(true);
        }
        let dims = copy_projection_dims(
            &param.dims,
            "projected declared array dimension count",
            param_span,
        )?;
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
                name, subscripts, ..
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
        let value_span = value.span().unwrap_or(target_span);
        if let Some(record_outputs) = self.record_constructor_outputs(&value, scope, depth + 1)? {
            if is_function_output_target(function, &target) {
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
        let dims = assignment_projection_dims(
            function,
            &target,
            self.expr_dims_with_owner(&value, scope, depth + 1, value_span)?,
            value_span,
        )?;
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
            if is_function_output_target(function, &target) {
                let outputs = project_target_scalar_outputs(&dims, scalars, target_span)?;
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
            declared_dims(function, target)?
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
            let Some(value) = self.compile_time_scalar(&condition) else {
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
            if let Some(values) = scope.scalars.get(output.name.as_str()) {
                let projected = project_scalar_outputs(output, values, output_span)?;
                append_projected_outputs(
                    &mut outputs,
                    projected,
                    "projected scalar output count",
                    output_span,
                )?;
                continue;
            }
            if let Some(expr) = scope.full.get(output.name.as_str()) {
                let projected =
                    self.project_output_expr(output, expr, scope, depth + 1, output_span)?;
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

    fn project_output_expr(
        &self,
        output: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
        output_span: rumoca_core::Span,
    ) -> Result<Vec<ProjectedFunctionOutput>, LowerError> {
        if output.dims.is_empty() {
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
            .project_value_scalars(expr, &output.dims, scope, depth, expr_span)
            .map_err(|err| err.with_fallback_span(expr_span))?
            .ok_or_else(|| {
                unsupported_at(
                    format!("function output `{}` could not be projected", output.name),
                    expr_span,
                )
            })?;
        project_scalar_outputs(output, &values, output_span)
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
        if scalar_count_for_dims(&dims, "record constructor input dimensions", span)? == 0
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
        let Some(outputs) = self.function_call_outputs_with_projection_scope(
            &substituted,
            depth + 1,
            owner_span,
            Some(scope),
        )?
        else {
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
                function: rumoca_core::BuiltinFunction::Cat,
                args,
                span,
            } => {
                let span = inherited_projection_span(*span, owner_span);
                self.project_cat_value(args, dims, flat_index, scope, depth, span)
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
            } => self.project_function_call_value(expr, flat_index, scope, depth, owner_span),
            rumoca_core::Expression::Binary { op, lhs, rhs, span }
                if is_mul(op) || is_add(op) || is_sub(op) || is_div(op) =>
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
            let condition = self.substitute(condition, ctx.scope)?;
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
        if rhs_dims != ctx.dims {
            return Err(unsupported_at(
                format!(
                    "unary array expression has operand dimensions {}, expected {}",
                    format_i64_dims(&rhs_dims),
                    format_i64_dims(ctx.dims)
                ),
                ctx.span,
            ));
        }
        let rhs = self
            .project_value(
                rhs,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth + 1,
                ctx.span,
            )?
            .ok_or_else(|| {
                unsupported_at("unary array operand could not be projected", ctx.span)
            })?;
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
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
        owner_span: rumoca_core::Span,
    ) -> Result<Option<rumoca_core::Expression>, LowerError> {
        let mut call = self.substitute(expr, scope)?;
        if call.span().is_none() {
            call = call.with_span(owner_span);
        }
        Ok(self
            .function_call_outputs_with_owner(&call, depth + 1, owner_span)?
            .and_then(|outputs| outputs.get(flat_index).map(|output| output.expr.clone())))
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
            self.substitute(lhs, ctx.scope)?
        } else {
            self.project_value(
                lhs,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth,
                ctx.span,
            )?
            .ok_or_else(|| unsupported_at("binary lhs could not be projected", ctx.span))?
        };
        let rhs_expr = if rhs_dims.is_empty() {
            self.substitute(rhs, ctx.scope)?
        } else {
            self.project_value(
                rhs,
                ctx.dims,
                ctx.flat_index,
                ctx.scope,
                ctx.depth,
                ctx.span,
            )?
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
