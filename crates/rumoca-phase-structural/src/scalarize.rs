use std::collections::HashMap;

use rumoca_core::ExpressionRewriter;
use rumoca_ir_dae as dae;

use crate::projection_maps::{
    FunctionOutputProjectionMap, RecordFieldProjectionMap, build_component_index_projection_map,
    build_function_output_projection_map, build_record_field_projection_map,
    checked_projection_subscript, output_scalar_count,
};

mod function_maps;
mod index_projection;
mod projection;
mod shape;
use function_maps::{
    ConstructorInputMap, build_constructor_input_map, build_dynamic_function_output_map,
    build_function_output_dims_map, project_wrapped_dynamic_function_output,
};
#[cfg(test)]
use projection::is_complex_field_scalar_name;
use projection::{
    ScalarProjectionContext, lower_scalar_linear_algebra_exprs, project_rhs_for_scalar_target,
    scalarized_equation_lhs,
};
use shape::*;

type Dae = dae::Dae;
type Equation = dae::Equation;
type Expression = rumoca_core::Expression;
type Literal = rumoca_core::Literal;
type OpBinary = rumoca_core::OpBinary;
type OpUnary = rumoca_core::OpUnary;
type Reference = rumoca_core::Reference;
type Span = rumoca_core::Span;
type Subscript = rumoca_core::Subscript;
type VarName = rumoca_core::VarName;
type StructuralError = crate::StructuralError;
type ScalarizedEquations = (Vec<Equation>, Vec<(usize, usize)>);

struct StaticComprehensionSubstituter {
    values: HashMap<String, i64>,
}

impl ExpressionRewriter for StaticComprehensionSubstituter {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty()
            && let Some(value) = self.values.get(name.as_str())
        {
            return Expression::Literal {
                value: Literal::Integer(*value),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }

    fn walk_array_comprehension_expression(
        &mut self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
        span: Span,
    ) -> Expression {
        let rewritten_indices = self.rewrite_comprehension_indices(indices);
        let mut nested_values = self.values.clone();
        for index in indices {
            nested_values.remove(&index.name);
        }
        let mut nested = Self {
            values: nested_values,
        };
        Expression::ArrayComprehension {
            expr: Box::new(nested.rewrite_expression(expr)),
            indices: rewritten_indices,
            filter: filter.map(|filter| Box::new(nested.rewrite_expression(filter))),
            span,
        }
    }
}

fn constructor_inputs_for_call<'a>(
    name: &Reference,
    constructors: &'a ConstructorInputMap,
) -> Option<&'a [rumoca_core::FunctionParam]> {
    let instance_id = name.resolved_function()?.instance_id;
    constructors.get(&instance_id).map(Vec::as_slice)
}

fn bind_constructor_fields(
    name: &Reference,
    args: &[Expression],
    span: Span,
    constructors: &ConstructorInputMap,
) -> Result<Vec<Expression>, StructuralError> {
    let inputs = constructor_inputs_for_call(name, constructors).ok_or_else(|| {
        structural_contract_violation(
            format!(
                "record constructor `{}` lacks a resolved constructor signature",
                name.as_str()
            ),
            span,
        )
    })?;
    crate::function_arguments::bind_function_arguments(inputs, args).ok_or_else(|| {
        structural_contract_violation(
            format!(
                "record constructor `{}` arguments do not fill its declared input slots",
                name.as_str()
            ),
            span,
        )
    })
}

/// Build output variable names in solver-vector order (states, algebraics, outputs).
///
/// Array variables are expanded with their DAE shape metadata, so vectors use
/// `name[1]`, `name[2]`, etc. and matrices use `name[1,1]`, `name[1,2]`, etc.
pub fn build_output_names(dae: &Dae) -> Result<Vec<String>, StructuralError> {
    let mut names = Vec::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
    {
        let sz = output_scalar_count(&var.dims, var.source_span)?;
        if sz <= 1 {
            names.push(name.as_str().to_string());
        } else {
            for i in 1..=sz {
                names.push(format_scalar_ref(
                    name.as_str(),
                    &var.dims,
                    var.source_span,
                    i,
                )?);
            }
        }
    }
    Ok(names)
}

/// Collect variable dimensions from all variable categories.
pub fn build_var_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut map = HashMap::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.discrete_reals.iter())
        .chain(dae.variables.discrete_valued.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
    {
        if !var.dims.is_empty() {
            map.insert(name.as_str().to_string(), var.dims.clone());
        }
    }
    map
}

pub fn build_var_spans_map(dae: &Dae) -> HashMap<String, Span> {
    let mut map = HashMap::new();
    for (name, var) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.discrete_reals.iter())
        .chain(dae.variables.discrete_valued.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
    {
        if !var.source_span.is_dummy() {
            map.insert(name.as_str().to_string(), var.source_span);
        }
    }
    map
}

fn build_structural_int_map(
    dae: &Dae,
    var_dims: &HashMap<String, Vec<i64>>,
) -> HashMap<String, i64> {
    let mut values = HashMap::new();
    for _ in 0..dae
        .variables
        .parameters
        .len()
        .max(dae.variables.constants.len())
        .max(1)
    {
        let before = values.len();
        for (name, var) in dae
            .variables
            .constants
            .iter()
            .chain(dae.variables.parameters.iter())
        {
            if !dae.variables.constants.contains_key(name) && var.is_tunable {
                continue;
            }
            let Some(start) = var.start.as_ref() else {
                continue;
            };
            if let Some(value) = eval_structural_int_expr(start, &values, var_dims) {
                values.insert(name.as_str().to_string(), value);
            }
        }
        if values.len() == before {
            break;
        }
    }
    values
}

fn eval_structural_int_expr(
    expr: &Expression,
    values: &HashMap<String, i64>,
    var_dims: &HashMap<String, Vec<i64>>,
) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => values.get(name.as_str()).copied(),
        Expression::Unary { op, rhs, .. } => {
            let value = eval_structural_int_expr(rhs, values, var_dims)?;
            match op {
                OpUnary::Plus | OpUnary::DotPlus => Some(value),
                OpUnary::Minus | OpUnary::DotMinus => Some(-value),
                _ => None,
            }
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_structural_int_expr(lhs, values, var_dims)?;
            let rhs = eval_structural_int_expr(rhs, values, var_dims)?;
            match op {
                OpBinary::Add | OpBinary::AddElem => lhs.checked_add(rhs),
                OpBinary::Sub | OpBinary::SubElem => lhs.checked_sub(rhs),
                OpBinary::Mul | OpBinary::MulElem => lhs.checked_mul(rhs),
                OpBinary::Div | OpBinary::DivElem if rhs != 0 && lhs % rhs == 0 => Some(lhs / rhs),
                _ => None,
            }
        }
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args,
            ..
        } => {
            let Expression::VarRef {
                name, subscripts, ..
            } = args.first()?
            else {
                return None;
            };
            if !subscripts.is_empty() {
                return Some(1);
            }
            let dims = var_dims.get(name.as_str())?;
            let Some(dim_expr) = args.get(1) else {
                return (dims.len() == 1).then_some(dims[0]);
            };
            let dim = eval_structural_int_expr(dim_expr, values, var_dims)?;
            dims.get(usize::try_from(dim).ok()?.checked_sub(1)?)
                .copied()
        }
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Min,
            args,
            ..
        } => args
            .iter()
            .map(|arg| eval_structural_int_expr(arg, values, var_dims))
            .collect::<Option<Vec<_>>>()
            .and_then(|values| values.into_iter().min()),
        Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Max,
            args,
            ..
        } => args
            .iter()
            .map(|arg| eval_structural_int_expr(arg, values, var_dims))
            .collect::<Option<Vec<_>>>()
            .and_then(|values| values.into_iter().max()),
        _ => None,
    }
}

/// Build a map from a Complex-record base name to concrete `base.re` / `base.im`
/// scalar variable names present in the DAE.
pub fn build_complex_field_map(dae: &Dae) -> HashMap<String, [Option<String>; 2]> {
    let mut map: HashMap<String, [Option<String>; 2]> = HashMap::new();
    for (name, _) in dae
        .variables
        .states
        .iter()
        .chain(dae.variables.algebraics.iter())
        .chain(dae.variables.outputs.iter())
        .chain(dae.variables.parameters.iter())
        .chain(dae.variables.constants.iter())
        .chain(dae.variables.inputs.iter())
    {
        let raw = name.as_str();
        let Some((base, field)) = name.scope_split() else {
            continue;
        };
        let slot = match field {
            "re" => 0,
            "im" => 1,
            _ => continue,
        };
        map.entry(base.to_string()).or_insert([None, None])[slot] = Some(raw.to_string());
    }
    map
}
fn integer_literal_value(expr: &Expression) -> Option<i64> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(*value),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } if value.is_finite() && value.fract() == 0.0 => Some(*value as i64),
        _ => None,
    }
}

fn project_subscripted_dims(
    dims: &[i64],
    subscripts: &[Subscript],
    linear_index: usize,
    span: rumoca_core::Span,
    structural_values: &HashMap<String, i64>,
) -> Result<Option<Vec<Subscript>>, StructuralError> {
    if linear_index == 0 {
        return Ok(None);
    }
    // MLS §10.6: array equations are equivalent to scalar equations over each
    // selected element. Preserve written fixed subscripts and replace only
    // projected slice/trailing dimensions with scalar indices.
    let Some(projected_dims) = apply_subscripts_to_dims(dims, subscripts, structural_values) else {
        return Ok(None);
    };
    let scalar_count = output_scalar_count(&projected_dims, span)?;
    if scalar_count == 0
        || linear_index > scalar_count
        || (scalar_count == 1 && projected_dims.is_empty())
    {
        return Ok(None);
    }

    let projection = linear_subscripts_for_dims_with_span(&projected_dims, linear_index, span)?;
    let mut projection_iter = projection.into_iter();
    let mut projected_subscripts = Vec::new();
    let mut dim_idx = 0usize;

    for subscript in subscripts {
        if dim_idx >= dims.len() {
            projected_subscripts.push(subscript.clone());
            continue;
        }
        match subscript {
            Subscript::Expr { expr, .. }
                if range_subscript_indices(expr, structural_values).is_some() =>
            {
                let Subscript::Index {
                    value: projected_index,
                    ..
                } = projection_iter.next().ok_or_else(|| {
                    structural_contract_violation(
                        format!(
                            "scalarization projection for dimensions {projected_dims:?} ended \
                             before subscript projection completed"
                        ),
                        span,
                    )
                })?
                else {
                    return Ok(None);
                };
                let Some(selected_index) = usize::try_from(projected_index)
                    .ok()
                    .and_then(|index| index.checked_sub(1))
                else {
                    return Ok(None);
                };
                let Some(selected) = range_subscript_indices(expr, structural_values)
                    .and_then(|indices| indices.get(selected_index).copied())
                else {
                    return Ok(None);
                };
                projected_subscripts.push(positive_generated_index_subscript(
                    selected,
                    span,
                    "structural range-selected subscript",
                )?);
                dim_idx += 1;
            }
            Subscript::Index { value, .. } => {
                projected_subscripts.push(positive_generated_index_subscript(
                    *value,
                    span,
                    "structural fixed subscript",
                )?);
                dim_idx += 1;
            }
            Subscript::Expr { expr: _, .. } => {
                projected_subscripts.push(subscript.clone());
                dim_idx += 1;
            }
            Subscript::Colon { .. } => {
                let Some(projected) = projection_iter.next() else {
                    return Ok(None);
                };
                projected_subscripts.push(projected);
                dim_idx += 1;
            }
        }
    }

    while dim_idx < dims.len() {
        let Some(projected) = projection_iter.next() else {
            return Ok(None);
        };
        projected_subscripts.push(projected);
        dim_idx += 1;
    }

    Ok(Some(projected_subscripts))
}

fn format_scalar_ref(
    name: &str,
    dims: &[i64],
    span: rumoca_core::Span,
    linear_index: usize,
) -> Result<String, StructuralError> {
    if dims.is_empty() || output_scalar_count(dims, span)? <= 1 {
        return Ok(name.to_string());
    }
    Ok(dae::scalar_name_text_for_flat_index(
        name,
        dims,
        linear_index.saturating_sub(1),
    ))
}

fn binary_expr(op: OpBinary, lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn projection_source_span(
    name: &Reference,
    fallback: &Expression,
    var_spans: &HashMap<String, Span>,
    context_span: Option<Span>,
) -> Result<Span, StructuralError> {
    fallback
        .span()
        .filter(|span| !span.is_dummy())
        .or_else(|| {
            name.component_ref()
                .map(|component_ref| component_ref.span)
                .filter(|span| !span.is_dummy())
        })
        .or_else(|| var_spans.get(name.as_str()).copied())
        .or_else(|| context_span.filter(|span| !span.is_dummy()))
        .ok_or_else(|| StructuralError::UnspannedContractViolation {
            reason: format!(
                "cannot project `{}` without a source span on the expression, component reference, DAE variable, or enclosing expression context",
                name.as_str()
            ),
        })
}

fn add_expr_with_span(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    binary_expr(OpBinary::Add, lhs, rhs, span)
}

#[cfg(test)]
fn scalarize_test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_structural_scalarize_tests_source_1.mo"),
        1,
        2,
    )
}

#[cfg(test)]
fn sub_expr(lhs: Expression, rhs: Expression) -> Expression {
    sub_expr_with_span(lhs, rhs, scalarize_test_span())
}

fn sub_expr_with_span(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    binary_expr(OpBinary::Sub, lhs, rhs, span)
}

#[cfg(test)]
fn mul_expr(lhs: Expression, rhs: Expression) -> Expression {
    mul_expr_with_span(lhs, rhs, scalarize_test_span())
}

fn mul_expr_with_span(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    binary_expr(OpBinary::Mul, lhs, rhs, span)
}

#[cfg(test)]
fn sum_terms(terms: impl IntoIterator<Item = Expression>) -> Expression {
    sum_terms_with_span(terms, scalarize_test_span())
}

fn sum_terms_with_span(terms: impl IntoIterator<Item = Expression>, span: Span) -> Expression {
    let mut iter = terms.into_iter();
    let Some(first) = iter.next() else {
        return Expression::Literal {
            value: Literal::Real(0.0),
            span,
        };
    };
    iter.fold(first, |lhs, rhs| add_expr_with_span(lhs, rhs, span))
}

pub fn index_into_expr(
    expr: &Expression,
    i: usize,
    var_dims: &HashMap<String, Vec<i64>>,
    complex_fields: &HashMap<String, [Option<String>; 2]>,
    component_index_map: &HashMap<String, HashMap<usize, String>>,
    function_output_index_map: &FunctionOutputProjectionMap,
) -> Result<Expression, StructuralError> {
    let structural_values = HashMap::new();
    let var_spans = HashMap::new();
    let dynamic_function_output_map = HashMap::new();
    let function_output_dims_map = HashMap::new();
    let record_field_projection_map = HashMap::new();
    let constructor_input_map = HashMap::new();
    index_projection::IndexProjectionContext {
        i,
        context_span: expr.span().filter(|span| !span.is_dummy()),
        var_dims,
        var_spans: &var_spans,
        structural_values: &structural_values,
        complex_fields,
        component_index_map,
        function_output_index_map,
        function_output_dims_map: &function_output_dims_map,
        dynamic_function_output_map: &dynamic_function_output_map,
        record_field_projection_map: &record_field_projection_map,
        constructor_input_map: &constructor_input_map,
        expected_dims: None,
        allow_dynamic_function_projection: true,
    }
    .project(expr)
}

pub struct ExpressionScalarizationContext {
    var_dims: HashMap<String, Vec<i64>>,
    var_spans: HashMap<String, Span>,
    structural_values: HashMap<String, i64>,
    complex_fields: HashMap<String, [Option<String>; 2]>,
    component_index_map: HashMap<String, HashMap<usize, String>>,
    function_output_index_map: FunctionOutputProjectionMap,
    function_output_dims_map: HashMap<rumoca_core::FunctionInstanceId, Vec<i64>>,
    dynamic_function_output_map: HashMap<rumoca_core::FunctionInstanceId, String>,
    record_field_projection_map: RecordFieldProjectionMap,
    constructor_input_map: ConstructorInputMap,
}

pub fn build_expression_scalarization_context(
    dae: &Dae,
) -> Result<ExpressionScalarizationContext, StructuralError> {
    let var_dims = build_var_dims_map(dae);
    let var_spans = build_var_spans_map(dae);
    Ok(ExpressionScalarizationContext {
        structural_values: build_structural_int_map(dae, &var_dims),
        var_spans,
        var_dims,
        complex_fields: build_complex_field_map(dae),
        component_index_map: build_component_index_projection_map(dae),
        function_output_index_map: build_function_output_projection_map(dae)?,
        function_output_dims_map: build_function_output_dims_map(dae)?,
        dynamic_function_output_map: build_dynamic_function_output_map(dae)?,
        record_field_projection_map: build_record_field_projection_map(dae)?,
        constructor_input_map: build_constructor_input_map(dae)?,
    })
}

pub fn scalarize_expression_rows(
    expr: &Expression,
    output_len: usize,
    ctx: &ExpressionScalarizationContext,
) -> Result<Vec<Expression>, StructuralError> {
    if output_len <= 1 {
        return Ok(vec![expr.clone()]);
    }

    (1..=output_len)
        .map(|index| {
            index_projection::IndexProjectionContext {
                i: index,
                context_span: expr.span().filter(|span| !span.is_dummy()),
                var_dims: &ctx.var_dims,
                var_spans: &ctx.var_spans,
                structural_values: &ctx.structural_values,
                complex_fields: &ctx.complex_fields,
                component_index_map: &ctx.component_index_map,
                function_output_index_map: &ctx.function_output_index_map,
                function_output_dims_map: &ctx.function_output_dims_map,
                dynamic_function_output_map: &ctx.dynamic_function_output_map,
                record_field_projection_map: &ctx.record_field_projection_map,
                constructor_input_map: &ctx.constructor_input_map,
                expected_dims: None,
                allow_dynamic_function_projection: true,
            }
            .project(expr)
        })
        .collect::<Result<Vec<_>, _>>()
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScalarizedLhsTarget {
    name: String,
    expr: Expression,
    array_selector: Option<usize>,
    field_selector: Option<usize>,
}

impl ScalarizedLhsTarget {
    fn from_projected_var_ref(
        name: &Reference,
        subscripts: Vec<Subscript>,
        index: usize,
        span: Span,
    ) -> Option<Self> {
        Some(Self {
            name: scalarization_var_ref_name(name, &subscripts)?,
            expr: var_ref_expr_with_span(name, &subscripts, span),
            array_selector: Some(index),
            field_selector: None,
        })
    }

    fn from_flattened_name(
        lhs: &str,
        target: &str,
        span: Span,
    ) -> Result<Option<Self>, StructuralError> {
        let Some((array_selector, field_selector)) = parse_scalar_target_projection(lhs, target)
        else {
            return Ok(None);
        };
        Ok(Some(Self {
            name: target.to_string(),
            expr: target_var_ref_expr_with_span(target, span)?,
            array_selector,
            field_selector,
        }))
    }
}

pub fn scalar_targets_for_lhs(
    lhs: &str,
    span: rumoca_core::Span,
    scalar_names: &[String],
    var_dims: &HashMap<String, Vec<i64>>,
) -> Result<Vec<ScalarizedLhsTarget>, StructuralError> {
    if let Some(dims) = var_dims.get(lhs) {
        let scalar_count = output_scalar_count(dims, span)?;
        if scalar_count > 1 {
            let lhs_name = Reference::new(lhs);
            return (1..=scalar_count)
                .filter_map(|index| {
                    let subscripts = match linear_subscripts_for_dims_with_span(dims, index, span) {
                        Ok(subscripts) => subscripts,
                        Err(err) => return Some(Err(err)),
                    };
                    ScalarizedLhsTarget::from_projected_var_ref(&lhs_name, subscripts, index, span)
                        .map(Ok)
                })
                .collect();
        }
    }

    let dotted_prefix = format!("{lhs}.");
    let indexed_prefix = format!("{lhs}[");
    let mut targets = Vec::new();
    for target in scalar_names.iter().filter(|name| {
        let raw = name.as_str();
        raw == lhs || raw.starts_with(&dotted_prefix) || raw.starts_with(&indexed_prefix)
    }) {
        if let Some(target) = ScalarizedLhsTarget::from_flattened_name(lhs, target, span)? {
            targets.push(target);
        }
    }
    Ok(targets)
}

pub fn scalarization_subscript_text(sub: &Subscript) -> Option<String> {
    match sub {
        Subscript::Index { value: i, .. } => Some(i.to_string()),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: Literal::Integer(i),
                ..
            } => Some(i.to_string()),
            Expression::Literal {
                value: Literal::Real(v),
                ..
            } if v.is_finite() && v.fract() == 0.0 => Some((*v as i64).to_string()),
            _ => None,
        },
        _ => None,
    }
}

pub trait ScalarizationReferenceName {
    fn scalarization_name(&self) -> &str;
}

impl ScalarizationReferenceName for VarName {
    fn scalarization_name(&self) -> &str {
        self.as_str()
    }
}

impl ScalarizationReferenceName for Reference {
    fn scalarization_name(&self) -> &str {
        self.as_str()
    }
}

pub fn scalarization_var_ref_name(
    name: &impl ScalarizationReferenceName,
    subscripts: &[Subscript],
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.scalarization_name().to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(scalarization_subscript_text(sub)?);
    }
    Some(format!(
        "{}[{}]",
        name.scalarization_name(),
        indices.join(",")
    ))
}

pub fn residual_lhs_target_name(expr: &Expression) -> Option<String> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    {
        return scalarization_var_ref_name(name, subscripts);
    }
    None
}

fn residual_lhs_var_ref(expr: &Expression) -> Option<(&Reference, &[Subscript])> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        ..
    } = expr
    else {
        return None;
    };
    if let Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    {
        return Some((name, subscripts));
    }
    None
}

fn residual_lhs_scalar_targets(
    expr: &Expression,
    span: rumoca_core::Span,
    var_dims: &HashMap<String, Vec<i64>>,
    structural_values: &HashMap<String, i64>,
) -> Result<Vec<ScalarizedLhsTarget>, StructuralError> {
    let Some((name, subscripts)) = residual_lhs_var_ref(expr) else {
        return Ok(Vec::new());
    };
    let Some(dims) = var_dims.get(name.as_str()) else {
        return Ok(Vec::new());
    };
    if subscripts.is_empty() {
        return Ok(Vec::new());
    }

    let Some(projected_dims) = apply_subscripts_to_dims(dims, subscripts, structural_values) else {
        return Ok(Vec::new());
    };
    let scalar_count = output_scalar_count(&projected_dims, span)?;

    let mut targets = Vec::new();
    for idx in 1..=scalar_count {
        let Some(projected_subscripts) =
            project_subscripted_dims(dims, subscripts, idx, span, structural_values)?
        else {
            continue;
        };
        if let Some(target) =
            ScalarizedLhsTarget::from_projected_var_ref(name, projected_subscripts, idx, span)
        {
            targets.push(target);
        }
    }
    Ok(targets)
}

pub fn parse_one_based_index(text: &str) -> Option<usize> {
    let idx = text.trim().parse::<usize>().ok()?;
    (idx > 0).then_some(idx)
}

pub fn parse_complex_field_selector(fragment: &str) -> Option<(usize, Option<usize>)> {
    let (field_name, array_selector) =
        if let Some(scalar) = rumoca_core::parse_scalar_name(fragment) {
            let index = match scalar.indices.as_slice() {
                [index] => usize::try_from(*index).ok().filter(|index| *index > 0)?,
                _ => return None,
            };
            (scalar.base.trim(), Some(index))
        } else {
            (fragment.trim(), None)
        };

    let field_selector = match field_name {
        "re" => 1,
        "im" => 2,
        _ => return None,
    };
    Some((field_selector, array_selector))
}

fn scalar_target_index_for_base(base: &str, target: &str) -> Option<usize> {
    let scalar = rumoca_core::parse_scalar_name(target)?;
    if scalar.base != base {
        return None;
    }
    match scalar.indices.as_slice() {
        [index] => usize::try_from(*index).ok().filter(|index| *index > 0),
        _ => None,
    }
}

pub fn parse_scalar_target_projection(
    lhs: &str,
    target: &str,
) -> Option<(Option<usize>, Option<usize>)> {
    if target == lhs {
        return Some((None, None));
    }

    if let Some(index) = scalar_target_index_for_base(lhs, target) {
        return Some((Some(index), None));
    }

    let target_path = rumoca_core::ComponentPath::from_flat_path(target);
    let (field_part, base_parts) = target_path.parts().split_last()?;
    let base_path = rumoca_core::ComponentPath::from_parts(base_parts.iter().cloned());
    let base_part = base_path.as_str();
    if base_part == lhs {
        let (field_selector, array_selector) = parse_complex_field_selector(field_part)?;
        return Some((array_selector, Some(field_selector)));
    }
    if let Some(index) = scalar_target_index_for_base(lhs, base_part) {
        let (field_selector, nested_array_selector) = parse_complex_field_selector(field_part)?;
        return Some((nested_array_selector.or(Some(index)), Some(field_selector)));
    }

    None
}

pub fn target_var_ref_expr_with_span(
    target: &str,
    span: Span,
) -> Result<Expression, StructuralError> {
    if let Some(scalar) = rumoca_core::parse_scalar_name(target) {
        let Some(indices) = scalar
            .indices
            .into_iter()
            .map(|index| usize::try_from(index).ok().filter(|index| *index > 0))
            .collect::<Option<Vec<_>>>()
        else {
            return Err(structural_contract_violation(
                format!("scalar target `{target}` contains a non-positive index"),
                span,
            ));
        };
        let subscripts = indices
            .into_iter()
            .map(|index| generated_index_subscript(index, span, "scalar target subscript"))
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(Expression::VarRef {
            name: rumoca_core::Reference::new(scalar.base.to_string()),
            subscripts,
            span,
        });
    }

    Ok(Expression::VarRef {
        name: rumoca_core::Reference::new(target.to_string()),
        subscripts: Vec::new(),
        span,
    })
}

fn var_ref_expr_with_span(name: &Reference, subscripts: &[Subscript], span: Span) -> Expression {
    Expression::VarRef {
        name: name.clone(),
        subscripts: subscripts.to_vec(),
        span,
    }
}

fn scalar_lhs_targets_for_equation(
    residual_lhs_targets: Vec<ScalarizedLhsTarget>,
    scalarization_target: Option<&str>,
    lhs_reference: Option<&Reference>,
    span: Span,
    scalar_names: &[String],
    var_dims: &HashMap<String, Vec<i64>>,
    var_spans: &HashMap<String, Span>,
) -> Result<(Vec<ScalarizedLhsTarget>, bool), StructuralError> {
    if !residual_lhs_targets.is_empty() {
        return Ok((residual_lhs_targets, true));
    }

    let Some(lhs) = scalarization_target else {
        return Ok((Vec::new(), false));
    };

    let target_span = scalar_lhs_target_owner_span(lhs, lhs_reference, span, var_spans)?;
    Ok((
        scalar_targets_for_lhs(lhs, target_span, scalar_names, var_dims)?,
        false,
    ))
}

fn scalar_lhs_target_owner_span(
    lhs: &str,
    lhs_reference: Option<&Reference>,
    equation_span: Span,
    var_spans: &HashMap<String, Span>,
) -> Result<Span, StructuralError> {
    if !equation_span.is_dummy() {
        return Ok(equation_span);
    }
    if let Some(span) = lhs_reference.and_then(Reference::span) {
        return Ok(span);
    }
    if let Some(span) = var_spans.get(lhs).copied().filter(|span| !span.is_dummy()) {
        return Ok(span);
    }
    if let Some(scalar) = rumoca_core::parse_scalar_name(lhs)
        && let Some(span) = var_spans
            .get(scalar.base)
            .copied()
            .filter(|span| !span.is_dummy())
    {
        return Ok(span);
    }
    Err(StructuralError::UnspannedContractViolation {
        reason: format!(
            "cannot scalarize target `{lhs}` without source provenance on the equation, LHS reference, or DAE variable"
        ),
    })
}

/// Expand array equations (scalar_count > 1) into individual scalar equations.
///
/// After this pass every element of `dae.continuous.equations` has `scalar_count == 1`,
/// which is required by solvers that expect one equation per unknown.
pub fn scalarize_equations(dae: &mut Dae) -> Result<(), StructuralError> {
    let var_dims = build_var_dims_map(dae);
    let var_spans = build_var_spans_map(dae);
    let structural_values = build_structural_int_map(dae, &var_dims);
    let complex_fields = build_complex_field_map(dae);
    let component_index_map = build_component_index_projection_map(dae);
    let function_output_index_map = build_function_output_projection_map(dae)?;
    let function_output_dims_map = build_function_output_dims_map(dae)?;
    let dynamic_function_output_map = build_dynamic_function_output_map(dae)?;
    let record_field_projection_map = build_record_field_projection_map(dae)?;
    let constructor_input_map = build_constructor_input_map(dae)?;
    let projection = ScalarProjectionContext {
        context_span: None,
        var_dims: &var_dims,
        var_spans: &var_spans,
        structural_values: &structural_values,
        complex_fields: &complex_fields,
        component_index_map: &component_index_map,
        function_output_index_map: &function_output_index_map,
        function_output_dims_map: &function_output_dims_map,
        dynamic_function_output_map: &dynamic_function_output_map,
        record_field_projection_map: &record_field_projection_map,
        constructor_input_map: &constructor_input_map,
        expected_dims: None,
    };
    let (expanded, spans) = scalarize_continuous_equations(
        dae,
        &projection,
        &var_dims,
        &var_spans,
        &structural_values,
    )?;
    dae.continuous.equations = expanded;
    rumoca_ir_dae::remap_structured_families_after_expansion(
        &mut dae.continuous.structured_equations,
        &spans,
    );

    lower_event_scalar_linear_algebra(dae, &projection)?;
    Ok(())
}

fn scalarize_continuous_equations(
    dae: &Dae,
    projection: &ScalarProjectionContext<'_>,
    var_dims: &HashMap<String, Vec<i64>>,
    var_spans: &HashMap<String, Span>,
    structural_values: &HashMap<String, i64>,
) -> Result<ScalarizedEquations, StructuralError> {
    let scalar_names = build_output_names(dae)?;
    let mut expanded = Vec::new();
    // One `(new_start, new_len)` span per input equation, so structured families
    // can be re-pointed at their (post-expansion) row blocks below.
    let mut spans: Vec<(usize, usize)> = Vec::with_capacity(dae.continuous.equations.len());
    for eq in &dae.continuous.equations {
        let new_start = expanded.len();
        let scalarization_target = eq
            .lhs
            .as_ref()
            .map(|lhs| lhs.as_str().to_string())
            .or_else(|| residual_lhs_target_name(&eq.rhs));
        let expected_dims = scalarization_target
            .as_deref()
            .and_then(|target| var_dims.get(target))
            .map(Vec::as_slice);
        let eq_projection = projection
            .with_context_span(eq.span)
            .with_expected_dims(expected_dims);
        let residual_lhs_targets =
            residual_lhs_scalar_targets(&eq.rhs, eq.span, var_dims, structural_values)?;
        let (lhs_targets, has_residual_lhs_targets) = scalar_lhs_targets_for_equation(
            residual_lhs_targets,
            scalarization_target.as_deref(),
            eq.lhs.as_ref(),
            eq.span,
            &scalar_names,
            var_dims,
            var_spans,
        )?;
        let rhs_shape_count = shape_scalar_count(eq_projection.expression_shape(&eq.rhs));
        let scalar_count = if has_residual_lhs_targets {
            lhs_targets.len().max(1)
        } else if let Some(rhs_count) = rhs_shape_count.filter(|count| *count > 1) {
            // MLS §10.6 / SPEC_0019: array equations represent one scalar
            // equation per array element. Prefer the expression IR shape over
            // stale scalar_count metadata for residuals such as
            // `J * der(omega) - M_body`.
            rhs_count.max(lhs_targets.len())
        } else {
            eq.scalar_count.max(lhs_targets.len()).max(1)
        };
        if scalar_count <= 1 {
            let mut lowered = eq.clone();
            let rhs = if projection
                .expression_shape(&lowered.rhs)
                .is_singleton_array()
            {
                eq_projection.project_index(&lowered.rhs, 1)?
            } else {
                lowered.rhs.clone()
            };
            lowered.rhs = eq_projection.lower_scalar_linear_algebra(&rhs)?;
            expanded.push(lowered);
        } else {
            for i in 1..=scalar_count {
                let target = lhs_targets.get(i - 1);
                expanded.push(Equation {
                    lhs: scalarized_equation_lhs(eq, target, i, eq.span)?,
                    rhs: project_rhs_for_scalar_target(
                        &eq.rhs,
                        i,
                        scalar_count,
                        scalarization_target.as_deref(),
                        target,
                        eq.span,
                        &eq_projection,
                    )?,
                    span: eq.span,
                    origin: eq.origin.clone(),
                    scalar_count: 1,
                });
            }
        }
        spans.push((new_start, expanded.len() - new_start));
    }
    Ok((expanded, spans))
}

fn lower_event_scalar_linear_algebra(
    dae: &mut Dae,
    projection: &ScalarProjectionContext<'_>,
) -> Result<(), StructuralError> {
    lower_scalar_linear_algebra_exprs(&mut dae.conditions.relations, projection)?;
    lower_scalar_linear_algebra_exprs(&mut dae.events.synthetic_root_conditions, projection)?;
    lower_scalar_linear_algebra_exprs(&mut dae.clocks.triggered_conditions, projection)?;
    lower_scalar_linear_algebra_exprs(&mut dae.clocks.constructor_exprs, projection)
}

#[cfg(test)]
mod tests;
