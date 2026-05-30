use indexmap::IndexMap;
use rumoca_core::{ExpressionRewriter, Literal, OpBinary};
use rumoca_ir_dae as dae;

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

#[derive(Default)]
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

pub(super) fn function_projected_residuals(
    residual: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    structural_bindings: &IndexMap<String, f64>,
) -> Option<Vec<rumoca_core::Expression>> {
    let (lhs, rhs) = split_subtraction(residual)?;
    let analysis = FunctionProjectionAnalysis::new(dae_model, structural_bindings);
    if let Some((call, field)) = function_field_access(rhs)
        && let Some(call_outputs) = analysis.function_call_outputs(call, 0)
    {
        let target_base = plain_var_ref_name(lhs)?;
        return Some(projected_field_residuals_for_target(
            target_base,
            field,
            call_outputs,
            true,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        ));
    }
    if let Some((call, field)) = function_field_access(lhs)
        && let Some(call_outputs) = analysis.function_call_outputs(call, 0)
    {
        let target_base = plain_var_ref_name(rhs)?;
        return Some(projected_field_residuals_for_target(
            target_base,
            field,
            call_outputs,
            false,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        ));
    }
    if let Some(call_outputs) = analysis.function_call_outputs(rhs, 0) {
        let target_base = plain_var_ref_name(lhs)?;
        return Some(projected_residuals_for_target(
            target_base,
            call_outputs,
            true,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        ));
    }
    if let Some(call_outputs) = analysis.function_call_outputs(lhs, 0) {
        let target_base = plain_var_ref_name(rhs)?;
        return Some(projected_residuals_for_target(
            target_base,
            call_outputs,
            false,
            residual.span().unwrap_or(rumoca_core::Span::DUMMY),
        ));
    }
    None
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
) -> Option<Vec<rumoca_core::Expression>> {
    FunctionProjectionAnalysis::new(dae_model, structural_bindings)
        .function_call_outputs(expr, 0)
        .map(|outputs| outputs.into_iter().map(|output| output.expr).collect())
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
) -> Vec<rumoca_core::Expression> {
    outputs
        .into_iter()
        .map(|output| {
            let target = projected_target_name(target_base.as_str(), &output);
            let target_expr = scalar_key_expr(target);
            if target_minus_call {
                sub_with_span(target_expr, output.expr, span)
            } else {
                sub_with_span(output.expr, target_expr, span)
            }
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

fn projected_target_name(base: &str, output: &ProjectedFunctionOutput) -> String {
    let suffix = output
        .selector
        .strip_prefix(output.output_name.as_str())
        .unwrap_or_default();
    format!("{base}{suffix}")
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
    values
        .iter()
        .cloned()
        .enumerate()
        .map(|(idx, expr)| ProjectedFunctionOutput {
            output_name: output.name.clone(),
            selector: dae::scalar_name_text_for_flat_index(&output.name, &output.dims, idx),
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
    ) -> Option<Vec<ProjectedFunctionOutput>> {
        if depth > super::super::MAX_FUNCTION_INLINE_DEPTH {
            return None;
        }
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = expr
        else {
            return None;
        };
        if self.is_record_constructor_call(name, *is_constructor) {
            return None;
        }
        let function = self.dae_model.symbols.functions.get(name.var_name())?;
        if !function.pure || function.external.is_some() {
            return None;
        }
        let mut scope = self.bind_inputs(function, args, depth + 1)?;
        let mut projected = Vec::new();
        for statement in &function.body {
            self.apply_assignment(function, statement, &mut scope, &mut projected, depth + 1)?;
        }
        if !projected.is_empty() {
            return Some(projected);
        }
        self.projected_outputs_from_scope(function, &scope, depth + 1)
    }

    fn bind_inputs(
        &self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        depth: usize,
    ) -> Option<FunctionProjectionScope> {
        let (named, positional) =
            super::super::function_calls::split_named_and_positional_call_args(
                function.name.as_str(),
                args,
            )
            .ok()?;
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
            let actual = actual.or(input.default.as_ref())?.clone();
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
        Some(scope)
    }

    fn insert_input_scalar_projection(
        &self,
        input: &rumoca_core::FunctionParam,
        actual: &rumoca_core::Expression,
        dims: Vec<i64>,
        scope: &mut FunctionProjectionScope,
        depth: usize,
    ) -> Option<()> {
        let Some(scalars) = self.project_value_scalars(actual, &dims, scope, depth) else {
            return (!actual.contains_der()).then_some(());
        };
        scope.scalars.insert(input.name.clone(), scalars);
        scope.dims.insert(input.name.clone(), dims);
        Some(())
    }

    fn apply_assignment(
        &self,
        function: &rumoca_core::Function,
        statement: &rumoca_core::Statement,
        scope: &mut FunctionProjectionScope,
        projected: &mut Vec<ProjectedFunctionOutput>,
        depth: usize,
    ) -> Option<()> {
        let rumoca_core::Statement::Assignment { comp, value, .. } = statement else {
            return is_ignorable_projection_statement(statement).then_some(());
        };
        let target = comp.to_var_name().as_str().to_string();
        let value = self.substitute(value, scope);
        scope.full.insert(target.clone(), value.clone());
        if let Some(record_outputs) =
            self.record_constructor_outputs(&target, &value, scope, depth + 1)
        {
            projected.extend(record_outputs);
            return Some(());
        }
        let dims = self
            .expr_dims(&value, scope, depth + 1)
            .filter(|dims| !dims.is_empty())
            .or_else(|| declared_dims(function, &target))
            .unwrap_or_default();
        if !dims.is_empty() {
            let scalars = self.project_value_scalars(&value, &dims, scope, depth + 1)?;
            scope.scalars.insert(target.clone(), scalars.clone());
            scope.dims.insert(target.clone(), dims.clone());
            projected.extend(scalars.into_iter().enumerate().map(|(idx, expr)| {
                ProjectedFunctionOutput {
                    output_name: target.clone(),
                    selector: dae::scalar_name_text_for_flat_index(&target, &dims, idx),
                    expr,
                }
            }));
        }
        Some(())
    }

    fn projected_outputs_from_scope(
        &self,
        function: &rumoca_core::Function,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<Vec<ProjectedFunctionOutput>> {
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
        (!outputs.is_empty()).then_some(outputs)
    }

    fn project_output_expr(
        &self,
        output: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<Vec<ProjectedFunctionOutput>> {
        if output.dims.is_empty() {
            return Some(vec![ProjectedFunctionOutput {
                output_name: output.name.clone(),
                selector: output.name.clone(),
                expr: expr.clone(),
            }]);
        }
        let values = self.project_value_scalars(expr, &output.dims, scope, depth)?;
        Some(project_scalar_outputs(output, &values))
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
    ) -> Option<Vec<ProjectedFunctionOutput>> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = value
        else {
            return None;
        };
        if !self.is_record_constructor_call(name, *is_constructor) {
            return None;
        }
        let constructor = self.dae_model.symbols.functions.get(name.var_name())?;
        let (named, positional) =
            super::super::function_calls::split_named_and_positional_call_args(name.as_str(), args)
                .ok()?;
        let mut positional_idx = 0usize;
        let mut outputs = Vec::new();
        for input in &constructor.inputs {
            let actual = named.get(input.name.as_str()).copied().or_else(|| {
                let actual = positional.get(positional_idx).copied();
                positional_idx += usize::from(actual.is_some());
                actual
            });
            let actual = actual.or(input.default.as_ref())?.clone();
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
        Some(outputs)
    }

    fn optional_constructor_input_scalars(
        &self,
        actual: &rumoca_core::Expression,
        dims: &[i64],
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<Option<Vec<rumoca_core::Expression>>> {
        match self.project_value_scalars(actual, dims, scope, depth) {
            Some(scalars) => Some(Some(scalars)),
            None if actual.contains_der() => None,
            None => Some(None),
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
    ) -> Option<Vec<rumoca_core::Expression>> {
        let count = scalar_count_for_dims(dims)?;
        if let Some(values) = self.project_function_call_scalars_once(expr, count, scope, depth) {
            return Some(values);
        }
        (0..count)
            .map(|idx| self.project_value(expr, dims, idx, scope, depth))
            .collect()
    }

    fn project_function_call_scalars_once(
        &self,
        expr: &rumoca_core::Expression,
        count: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<Vec<rumoca_core::Expression>> {
        let substituted = self.substitute(expr, scope);
        let rumoca_core::Expression::FunctionCall {
            is_constructor: false,
            ..
        } = substituted
        else {
            return None;
        };
        let outputs = self.function_call_outputs(&substituted, depth + 1)?;
        (outputs.len() == count).then(|| {
            outputs
                .into_iter()
                .map(|output| output.expr)
                .collect::<Vec<_>>()
        })
    }

    fn project_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<rumoca_core::Expression> {
        if dims.is_empty() {
            return Some(self.substitute(expr, scope));
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
                    return Some(value.clone().with_span(*span));
                }
                let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
                Some(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::new(dae::format_subscript_key(
                        name.as_str(),
                        &indices,
                    )),
                    subscripts: Vec::new(),
                    span: *span,
                })
            }
            rumoca_core::Expression::Array { elements, .. } => flatten_array_elements(elements)
                .get(flat_index)
                .map(|element| self.substitute(element, scope)),
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
                let arg = self.project_value(&args[0], dims, flat_index, scope, depth)?;
                Some(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Der,
                    args: vec![arg],
                    span: *span,
                })
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
            rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_mul(op) => {
                let ctx = ProjectionValueCtx {
                    dims,
                    flat_index,
                    scope,
                    depth,
                    span: *span,
                };
                if let Some(expr) = self.project_tensor_product(lhs, rhs, &ctx) {
                    return Some(expr);
                }
                self.project_binary_elementwise(op.clone(), lhs, rhs, &ctx)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_add(op) || is_sub(op) => {
                let ctx = ProjectionValueCtx {
                    dims,
                    flat_index,
                    scope,
                    depth,
                    span: *span,
                };
                self.project_binary_elementwise(op.clone(), lhs, rhs, &ctx)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, span } if is_div(op) => {
                let ctx = ProjectionValueCtx {
                    dims,
                    flat_index,
                    scope,
                    depth,
                    span: *span,
                };
                self.project_binary_elementwise(op.clone(), lhs, rhs, &ctx)
            }
            other => self.project_indexed_value(other, dims, flat_index, scope),
        }
    }

    fn project_if_value(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Option<rumoca_core::Expression> {
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
                Some((
                    self.substitute(condition, ctx.scope),
                    self.project_value(branch, ctx.dims, ctx.flat_index, ctx.scope, ctx.depth + 1)?,
                ))
            })
            .collect::<Option<Vec<_>>>()?;
        let projected_else = self.project_value(
            else_branch,
            ctx.dims,
            ctx.flat_index,
            ctx.scope,
            ctx.depth + 1,
        )?;
        Some(rumoca_core::Expression::If {
            branches: projected_branches,
            else_branch: Box::new(projected_else),
            span: ctx.span,
        })
    }

    fn project_transpose_value(
        &self,
        arg: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<rumoca_core::Expression> {
        let input_dims = self.expr_dims(arg, scope, depth)?;
        let [input_rows, input_cols] = input_dims.as_slice() else {
            return None;
        };
        let output_dims = [*input_cols, *input_rows];
        if dims != output_dims {
            return None;
        }
        let output_row = flat_index / usize::try_from(*input_rows).ok()?;
        let output_col = flat_index % usize::try_from(*input_rows).ok()?;
        let input_cols = usize::try_from(*input_cols).ok()?;
        let input_index = output_col * input_cols + output_row;
        self.project_value(arg, &input_dims, input_index, scope, depth)
    }

    fn project_function_call_value(
        &self,
        expr: &rumoca_core::Expression,
        flat_index: usize,
        scope: &FunctionProjectionScope,
        depth: usize,
    ) -> Option<rumoca_core::Expression> {
        let call = self.substitute(expr, scope);
        self.function_call_outputs(&call, depth + 1)
            .and_then(|outputs| outputs.get(flat_index).map(|output| output.expr.clone()))
    }

    fn project_indexed_value(
        &self,
        expr: &rumoca_core::Expression,
        dims: &[i64],
        flat_index: usize,
        scope: &FunctionProjectionScope,
    ) -> Option<rumoca_core::Expression> {
        let span = expr.span().unwrap_or(rumoca_core::Span::DUMMY);
        let indices = dae::flat_index_to_subscripts(dims, flat_index)?;
        Some(rumoca_core::Expression::Index {
            base: Box::new(self.substitute(expr, scope)),
            subscripts: indices
                .into_iter()
                .map(|idx| rumoca_core::Subscript::generated_index(idx as i64, span))
                .collect(),
            span,
        })
    }

    fn project_binary_elementwise(
        &self,
        op: OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Option<rumoca_core::Expression> {
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
        };
        let rhs_expr = if rhs_dims.is_empty() {
            self.substitute(rhs, ctx.scope)
        } else {
            self.project_value(rhs, ctx.dims, ctx.flat_index, ctx.scope, ctx.depth)?
        };
        Some(rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs_expr),
            rhs: Box::new(rhs_expr),
            span: ctx.span,
        })
    }

    fn project_tensor_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Option<rumoca_core::Expression> {
        let lhs_dims = self.expr_dims(lhs, ctx.scope, ctx.depth)?;
        let rhs_dims = self.expr_dims(rhs, ctx.scope, ctx.depth)?;
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
            _ => None,
        }
    }

    fn project_matrix_vector_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        product_dims: MatrixVectorProductDims<'_>,
        ctx: &ProjectionValueCtx<'_>,
    ) -> Option<rumoca_core::Expression> {
        let rows = usize::try_from(product_dims.rows).ok()?;
        let cols = usize::try_from(product_dims.cols).ok()?;
        if ctx.flat_index >= rows {
            return None;
        }
        let row = ctx.flat_index;
        let mut terms = Vec::with_capacity(cols);
        for col in 0..cols {
            let lhs_idx = row * cols + col;
            let lhs_term =
                self.project_value(lhs, product_dims.lhs_dims, lhs_idx, ctx.scope, ctx.depth)?;
            let rhs_term =
                self.project_value(rhs, product_dims.rhs_dims, col, ctx.scope, ctx.depth)?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Some(sum_expressions(terms, ctx.span))
    }

    fn project_vector_matrix_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        rhs_dims: &[i64],
        ctx: &ProjectionValueCtx<'_>,
        rows: i64,
        cols: i64,
    ) -> Option<rumoca_core::Expression> {
        let rows = usize::try_from(rows).ok()?;
        let cols = usize::try_from(cols).ok()?;
        if ctx.flat_index >= cols {
            return None;
        }
        let col = ctx.flat_index;
        let mut terms = Vec::with_capacity(rows);
        for row in 0..rows {
            let lhs_term = self.project_value(lhs, &[rows as i64], row, ctx.scope, ctx.depth)?;
            let rhs_idx = row * cols + col;
            let rhs_term = self.project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth)?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Some(sum_expressions(terms, ctx.span))
    }

    fn project_matrix_matrix_product(
        &self,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        lhs_dims: &[i64],
        rhs_dims: &[i64],
        ctx: &ProjectionValueCtx<'_>,
        cols: i64,
    ) -> Option<rumoca_core::Expression> {
        let inner = usize::try_from(lhs_dims[1]).ok()?;
        let cols = usize::try_from(cols).ok()?;
        let row = ctx.flat_index / cols;
        let col = ctx.flat_index % cols;
        let mut terms = Vec::with_capacity(inner);
        for inner_idx in 0..inner {
            let lhs_idx = row * inner + inner_idx;
            let rhs_idx = inner_idx * cols + col;
            let lhs_term = self.project_value(lhs, lhs_dims, lhs_idx, ctx.scope, ctx.depth)?;
            let rhs_term = self.project_value(rhs, rhs_dims, rhs_idx, ctx.scope, ctx.depth)?;
            terms.push(rumoca_core::Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(lhs_term),
                rhs: Box::new(rhs_term),
                span: ctx.span,
            });
        }
        Some(sum_expressions(terms, ctx.span))
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
                    return Some(vec![values.len() as i64]);
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

fn is_ignorable_projection_statement(statement: &rumoca_core::Statement) -> bool {
    match statement {
        rumoca_core::Statement::Empty { .. } => true,
        rumoca_core::Statement::FunctionCall { comp, .. } => {
            comp.to_var_name().as_str() == "assert"
        }
        _ => false,
    }
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
        (*dim > 0).then_some(acc.checked_mul(*dim as usize)?)
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
}
