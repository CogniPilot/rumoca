//! Constant-fold parameter/state `start` expressions to typed literals.
//!
//! Many Modelica parameters have `start` values that reference other parameters
//! (e.g., `G_T = G_T_ref` where `G_T_ref = 300.15`). Template backends (Julia MTK,
//! SymPy, etc.) need concrete values. This pass iteratively evaluates start
//! expressions using a fixed-point approach, replacing evaluable expressions
//! with literal values.

use crate::errors::ToDaeError;
use rumoca_core::ExpressionVisitor;
use rumoca_core::{
    BuiltinFunction, Expression, Literal, OpBinary, OpUnary, Reference, Span, Subscript, VarName,
    apply_scalar_binary_math, apply_scalar_unary_math,
};
use rumoca_eval_dae::constant::{ConstValue, eval_const_expr_with};
use rumoca_ir_dae::{Dae, DaeVariableMutVisitor, DaeVariablePartition, DaeVisitor, Variable};
use std::collections::HashMap;

/// Evaluate all parameter/state/constant start expressions to typed literals
/// where possible. Modifies the DAE in place.
pub(crate) fn fold_start_values_to_literals(dae: &mut Dae) -> Result<(), ToDaeError> {
    // Phase 1: build a name→value map from constants, enum ordinals, and
    // parameter start expressions (fixed-point iteration).
    let values = collect_foldable_start_values(dae);

    // Set of parameter names: a parameter start expression that references
    // another parameter stays symbolic (see below), matching the bare-alias
    // preservation that already exists for top-level VarRef starts.
    let param_names: std::collections::HashSet<String> = dae
        .variables
        .parameters
        .keys()
        .map(|name| name.as_str().to_string())
        .collect();

    // Parameters whose translation-time values were consumed by a fold.
    // MLS 3.7 §4.5.2: using a parameter's value during translation turns it
    // into an evaluated parameter, and "it must be ensured that the parameter
    // cannot be assigned a different value after translation". Folding a
    // start expression bakes the referenced parameters' values into the
    // literal, so those parameters are pinned non-tunable below.
    let mut consumed_params: std::collections::HashSet<String> = std::collections::HashSet::new();

    // Phase 2: rewrite start expressions to literals where we found values.
    // Also clear self-referencing defaults (start = VarRef(self_name)).
    let mut rewrite_error = Ok(());
    let rewrite = |var: &mut Variable, is_parameter: bool| {
        if rewrite_error.is_err() {
            return;
        }
        if let Some(ref start) = var.start {
            // Check for self-reference: start = VarRef(own_name)
            if let Expression::VarRef {
                name, subscripts, ..
            } = start
                && subscripts.is_empty()
                && name.as_str() == var.name.as_str()
            {
                var.start = None;
                return;
            }
            // Preserve top-level VarRef starts symbolically.
            // - Enum literal refs (MLS §4.8.5) carry identity: codegen for
            //   FMI 3.0, embedded C, and Julia MTK needs the literal name,
            //   not its ordinal.
            // - Parameter refs preserve the dependency so downstream
            //   overrides (FMI parameter tweaks, structural parameters)
            //   flow through to dependents instead of being locked at
            //   compile time. Topo-sorting (sort_parameters_by_start_deps)
            //   already orders the chain for forward-eval templates.
            if let Expression::VarRef { subscripts, .. } = start
                && subscripts.is_empty()
            {
                return;
            }
            // MLS 3.7 §4.5 makes the translation-vs-initialization split for
            // evaluable parameters tool dependent; keeping the expression
            // (`massRatio = exp(dv/(Isp*g0))`) defers evaluation to
            // initialization so a runtime override of a base parameter
            // (`Isp`) still propagates to its dependents. The parameter
            // chain is already topo-ordered for forward evaluation.
            // String-dependent expressions (MSL TransformerData selects by
            // winding-connection letter) still fold: string evaluation is
            // translation-time only in the solve runtime, and the consumed
            // parameters are pinned non-tunable per §4.5.2.
            if is_parameter
                && start_references_parameter(start, &param_names)
                && !expr_depends_on_string_value(start, &values)
            {
                return;
            }
            let Some(val) = values.get(var.name.as_str()).cloned() else {
                return;
            };
            let mut refs: Vec<VarName> = Vec::new();
            start.collect_var_refs(&mut refs);
            consumed_params.extend(
                refs.iter()
                    .map(|name| name.as_str().to_string())
                    .filter(|name| param_names.contains(name)),
            );
            let span = match folded_start_span(var, start) {
                Ok(span) => span,
                Err(err) => {
                    rewrite_error = Err(err);
                    return;
                }
            };
            var.start = Some(Expression::Literal {
                value: val.into_literal(),
                span,
            });
        }
    };

    StartRewriter { rewrite }.visit_variables_mut(&mut dae.variables);
    rewrite_error?;

    // Evaluated-parameter lock (MLS 3.7 §4.5.2): parameters whose values
    // were folded into literals above must reject post-translation overrides.
    for (name, var) in dae.variables.parameters.iter_mut() {
        if consumed_params.contains(name.as_str()) {
            var.is_tunable = false;
        }
    }
    Ok(())
}

/// Prepare FMI modelDescription metadata: XML numeric attributes have no
/// expression language, so every serialized start/min/max/nominal value must be
/// a finite numeric value or value list under default parameter values.
pub(crate) fn fold_fmi_start_values_to_literals(dae: &mut Dae) -> Result<(), ToDaeError> {
    let dims = collect_variable_dims(dae);
    let values = collect_fmi_metadata_values(dae, &dims);
    let mut rewrite_error = Ok(());
    let rewrite = |var: &mut Variable, _is_parameter: bool| {
        if rewrite_error.is_err() {
            return;
        }
        if let Some(expr) = var.start.as_ref() {
            match fmi_numeric_attribute_literal_expression(
                var,
                FmiNumericAttribute::Start,
                expr,
                &values,
                &dims,
            ) {
                Ok(expr) => var.start = Some(expr),
                Err(err) => {
                    rewrite_error = Err(err);
                    return;
                }
            }
        }
        if let Some(expr) = var.min.as_ref() {
            match fmi_numeric_attribute_literal_expression(
                var,
                FmiNumericAttribute::Min,
                expr,
                &values,
                &dims,
            ) {
                Ok(expr) => var.min = Some(expr),
                Err(err) => {
                    rewrite_error = Err(err);
                    return;
                }
            }
        }
        if let Some(expr) = var.max.as_ref() {
            match fmi_numeric_attribute_literal_expression(
                var,
                FmiNumericAttribute::Max,
                expr,
                &values,
                &dims,
            ) {
                Ok(expr) => var.max = Some(expr),
                Err(err) => {
                    rewrite_error = Err(err);
                    return;
                }
            }
        }
        if let Some(expr) = var.nominal.as_ref() {
            match fmi_numeric_attribute_literal_expression(
                var,
                FmiNumericAttribute::Nominal,
                expr,
                &values,
                &dims,
            ) {
                Ok(expr) => var.nominal = Some(expr),
                Err(err) => rewrite_error = Err(err),
            }
        }
    };

    StartRewriter { rewrite }.visit_variables_mut(&mut dae.variables);
    rewrite_error
}

fn collect_foldable_start_values(dae: &Dae) -> HashMap<String, ConstValue> {
    let mut values: HashMap<String, ConstValue> = HashMap::new();

    for (name, ordinal) in &dae.symbols.enum_literal_ordinals {
        values.insert(name.clone(), ConstValue::Real(*ordinal as f64));
    }

    let mut bindings = Vec::new();
    StartBindingCollector {
        bindings: &mut bindings,
    }
    .visit_dae(dae);

    let max_passes = bindings.len().max(1) * 2;
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, expr) in &bindings {
            if values.contains_key(name.as_str()) {
                continue;
            }
            if let Some(value) = eval_start_const_expr(expr, &values)
                && value.is_finite()
            {
                values.insert(name.to_string(), value);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    values
}

fn folded_start_span(var: &Variable, start: &Expression) -> Result<Span, ToDaeError> {
    var.start_attribute_span()
        .or_else(|| start.span())
        .filter(|span| !span.is_dummy())
        .ok_or_else(|| {
            ToDaeError::runtime_metadata_violation(format!(
                "folded start literal for `{}` is missing source provenance",
                var.name.as_str()
            ))
        })
}

#[derive(Clone, Copy)]
enum FmiNumericAttribute {
    Start,
    Min,
    Max,
    Nominal,
}

impl FmiNumericAttribute {
    fn name(self) -> &'static str {
        match self {
            Self::Start => "start",
            Self::Min => "min",
            Self::Max => "max",
            Self::Nominal => "nominal",
        }
    }

    fn span(self, var: &Variable, expr: &Expression) -> Result<Span, ToDaeError> {
        let attr_span = match self {
            Self::Start => var.start_attribute_span(),
            Self::Min => var.min_attribute_span(),
            Self::Max => var.max_attribute_span(),
            Self::Nominal => var.nominal_attribute_span(),
        };
        attr_span
            .or_else(|| expr.span())
            .filter(|span| !span.is_dummy())
            .ok_or_else(|| {
                ToDaeError::runtime_metadata_violation(format!(
                    "folded FMI modelDescription {} literal for `{}` is missing source provenance",
                    self.name(),
                    var.name.as_str()
                ))
            })
    }
}

#[derive(Clone, Debug, PartialEq)]
enum FmiConstValue {
    Real(f64),
    Bool(bool),
    String(String),
    Array(Vec<FmiConstValue>),
}

impl FmiConstValue {
    fn as_real(&self) -> Option<f64> {
        match self {
            Self::Real(value) => Some(*value),
            Self::Bool(_) | Self::String(_) | Self::Array(_) => None,
        }
    }

    fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(value) => Some(*value),
            Self::Real(value) => value.is_finite().then_some(value.abs() > 1.0e-12),
            Self::String(_) | Self::Array(_) => None,
        }
    }

    fn as_index(&self) -> Option<i64> {
        let value = self.as_real()?;
        if value.is_finite()
            && value >= 1.0
            && value <= i64::MAX as f64
            && value.fract().abs() <= 1.0e-12
        {
            Some(value as i64)
        } else {
            None
        }
    }

    fn is_finite(&self) -> bool {
        match self {
            Self::Real(value) => value.is_finite(),
            Self::Bool(_) | Self::String(_) => true,
            Self::Array(elements) => elements.iter().all(Self::is_finite),
        }
    }

    fn flatten_numeric(&self, out: &mut Vec<f64>) -> bool {
        match self {
            Self::Real(value) if value.is_finite() => {
                out.push(*value);
                true
            }
            Self::Array(elements) => elements.iter().all(|element| element.flatten_numeric(out)),
            Self::Real(_) | Self::Bool(_) | Self::String(_) => false,
        }
    }
}

struct FmiMetadataEnv<'a> {
    values: &'a HashMap<String, FmiConstValue>,
    dims: &'a HashMap<String, Vec<i64>>,
}

fn collect_variable_dims(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut dims = HashMap::new();
    VariableDimsCollector { dims: &mut dims }.visit_dae(dae);
    dims
}

struct VariableDimsCollector<'a> {
    dims: &'a mut HashMap<String, Vec<i64>>,
}

impl DaeVisitor for VariableDimsCollector<'_> {
    fn visit_variable(
        &mut self,
        _partition: DaeVariablePartition,
        name: &VarName,
        variable: &Variable,
    ) {
        self.dims
            .insert(name.as_str().to_string(), variable.dims.clone());
        if let Some(scalar) = rumoca_core::parse_scalar_name(name.as_str()) {
            self.dims
                .entry(scalar.base.to_string())
                .or_insert_with(Vec::new);
        }
    }
}

fn collect_fmi_metadata_values(
    dae: &Dae,
    dims: &HashMap<String, Vec<i64>>,
) -> HashMap<String, FmiConstValue> {
    let mut values: HashMap<String, FmiConstValue> = HashMap::new();

    for (name, ordinal) in &dae.symbols.enum_literal_ordinals {
        values.insert(name.clone(), FmiConstValue::Real(*ordinal as f64));
    }

    let mut bindings = Vec::new();
    StartBindingCollector {
        bindings: &mut bindings,
    }
    .visit_dae(dae);

    let max_passes = bindings.len().max(1) * 2;
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, expr) in &bindings {
            if values.contains_key(name.as_str()) {
                continue;
            }
            let evaluated = {
                let env = FmiMetadataEnv {
                    values: &values,
                    dims,
                };
                eval_fmi_const_expr(expr, &env)
            };
            if let Some(value) = evaluated
                && value.is_finite()
            {
                values.insert(name.to_string(), value);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    values
}

fn fmi_numeric_attribute_literal_expression(
    var: &Variable,
    attr: FmiNumericAttribute,
    expr: &Expression,
    values: &HashMap<String, FmiConstValue>,
    dims: &HashMap<String, Vec<i64>>,
) -> Result<Expression, ToDaeError> {
    let span = attr.span(var, expr)?;
    let env = FmiMetadataEnv { values, dims };
    let value = eval_fmi_const_expr(expr, &env)
        .ok_or_else(|| fmi_metadata_not_serializable_error(var, attr, expr))?;
    fmi_numeric_value_to_expression(var, value, span)
        .ok_or_else(|| fmi_metadata_not_serializable_error(var, attr, expr))
}

fn fmi_numeric_value_to_expression(
    var: &Variable,
    value: FmiConstValue,
    span: Span,
) -> Option<Expression> {
    if let Some(value) = value.as_real() {
        return value.is_finite().then_some(Expression::Literal {
            value: Literal::Real(value),
            span,
        });
    }

    let mut values = Vec::new();
    if !value.flatten_numeric(&mut values) {
        return None;
    }
    let var_size = var.try_size().ok()?;
    if var_size == 1 {
        return (values.len() == 1).then_some(Expression::Literal {
            value: Literal::Real(values[0]),
            span,
        });
    }
    if values.len() != var_size {
        return None;
    }
    let elements = values
        .into_iter()
        .map(|value| Expression::Literal {
            value: Literal::Real(value),
            span,
        })
        .collect();
    Some(Expression::Array {
        elements,
        is_matrix: false,
        span,
    })
}

fn fmi_metadata_not_serializable_error(
    var: &Variable,
    attr: FmiNumericAttribute,
    expr: &Expression,
) -> ToDaeError {
    let detail = format!(
        "FMI modelDescription {} for `{}` must serialize to finite numeric XML under default parameters; unevaluable {} expression: {expr:?}",
        attr.name(),
        var.name.as_str(),
        attr.name()
    );
    match attr.span(var, expr) {
        Ok(span) => ToDaeError::runtime_metadata_violation_at(detail, span),
        Err(_) => ToDaeError::runtime_metadata_violation(detail),
    }
}

fn eval_fmi_const_expr(expr: &Expression, env: &FmiMetadataEnv<'_>) -> Option<FmiConstValue> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Some(FmiConstValue::Real(*value as f64)),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Some(FmiConstValue::Real(*value)),
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Some(FmiConstValue::Bool(*value)),
        Expression::Literal {
            value: Literal::String(value),
            ..
        } => Some(FmiConstValue::String(value.clone())),
        Expression::VarRef {
            name, subscripts, ..
        } => eval_fmi_var_ref(name, subscripts, env),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_value = eval_fmi_const_expr(base, env)?;
            apply_fmi_subscripts(base_value, subscripts, None, env)
        }
        Expression::Unary { op, rhs, .. } => {
            let rhs = eval_fmi_const_expr(rhs, env)?;
            eval_fmi_unary(op, rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_fmi_const_expr(lhs, env)?;
            let rhs = eval_fmi_const_expr(rhs, env)?;
            eval_fmi_binary(op, lhs, rhs)
        }
        Expression::BuiltinCall { function, args, .. } => eval_fmi_builtin(*function, args, env),
        Expression::FunctionCall { name, args, .. } => {
            eval_fmi_named_function(name.last_segment(), args, env)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, then_expr) in branches {
                if eval_fmi_const_expr(condition, env)?.as_bool()? {
                    return eval_fmi_const_expr(then_expr, env);
                }
            }
            eval_fmi_const_expr(else_branch, env)
        }
        Expression::Array { elements, .. } => {
            let mut values = Vec::with_capacity(elements.len());
            for element in elements {
                values.push(eval_fmi_const_expr(element, env)?);
            }
            Some(FmiConstValue::Array(values))
        }
        Expression::Range {
            start, step, end, ..
        } => eval_fmi_range(start, step.as_deref(), end, env),
        _ => None,
    }
}

fn eval_fmi_var_ref(
    name: &Reference,
    subscripts: &[Subscript],
    env: &FmiMetadataEnv<'_>,
) -> Option<FmiConstValue> {
    let name_text = name.as_str();
    if let Some(value) = env.values.get(name_text)
        && subscripts.is_empty()
    {
        return Some(value.clone());
    }

    let mut embedded_indices = Vec::new();
    let base_name = if let Some(scalar) = rumoca_core::parse_scalar_name(name_text) {
        embedded_indices.extend(scalar.indices);
        scalar.base
    } else {
        name_text
    };
    let value = env.values.get(base_name).cloned().or_else(|| {
        rumoca_ir_dae::component_base_name(name_text)
            .and_then(|base| env.values.get(&base).cloned())
    })?;

    let mut indices = embedded_indices;
    indices.extend(eval_fmi_subscript_indices(subscripts, env)?);
    let dims = env.dims.get(base_name).map(Vec::as_slice);
    apply_fmi_indices(value, &indices, dims)
}

fn eval_fmi_subscript_indices(
    subscripts: &[Subscript],
    env: &FmiMetadataEnv<'_>,
) -> Option<Vec<i64>> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        match subscript {
            Subscript::Index { value, .. } => indices.push(*value),
            Subscript::Expr { expr, .. } => {
                indices.push(eval_fmi_const_expr(expr, env)?.as_index()?)
            }
            Subscript::Colon { .. } => {
                if subscripts
                    .iter()
                    .all(|item| matches!(item, Subscript::Colon { .. }))
                {
                    return Some(Vec::new());
                }
                return None;
            }
        }
    }
    Some(indices)
}

fn apply_fmi_subscripts(
    value: FmiConstValue,
    subscripts: &[Subscript],
    dims: Option<&[i64]>,
    env: &FmiMetadataEnv<'_>,
) -> Option<FmiConstValue> {
    let indices = eval_fmi_subscript_indices(subscripts, env)?;
    apply_fmi_indices(value, &indices, dims)
}

fn apply_fmi_indices(
    value: FmiConstValue,
    indices: &[i64],
    dims: Option<&[i64]>,
) -> Option<FmiConstValue> {
    if indices.is_empty() {
        return Some(value);
    }
    let FmiConstValue::Array(elements) = value else {
        return None;
    };
    let offset = if let Some(dims) = dims {
        flat_offset_for_indices(indices, dims)?
    } else if indices.len() == 1 {
        usize::try_from(indices[0].checked_sub(1)?).ok()?
    } else {
        return None;
    };
    flattened_fmi_values(&elements).get(offset).cloned()
}

fn flattened_fmi_values(elements: &[FmiConstValue]) -> Vec<FmiConstValue> {
    let mut flat = Vec::new();
    flatten_fmi_values_into(elements, &mut flat);
    flat
}

fn flatten_fmi_values_into(elements: &[FmiConstValue], out: &mut Vec<FmiConstValue>) {
    for element in elements {
        match element {
            FmiConstValue::Array(nested) => flatten_fmi_values_into(nested, out),
            value => out.push(value.clone()),
        }
    }
}

fn flat_offset_for_indices(indices: &[i64], dims: &[i64]) -> Option<usize> {
    if indices.len() != dims.len() {
        return None;
    }
    let mut offset = 0usize;
    for (index, dim) in indices.iter().zip(dims) {
        let dim = usize::try_from(*dim).ok()?;
        if dim == 0 {
            return None;
        }
        let index = usize::try_from(index.checked_sub(1)?).ok()?;
        if index >= dim {
            return None;
        }
        offset = offset.checked_mul(dim)?.checked_add(index)?;
    }
    Some(offset)
}

fn eval_fmi_unary(op: &OpUnary, rhs: FmiConstValue) -> Option<FmiConstValue> {
    match op {
        OpUnary::Minus | OpUnary::DotMinus => Some(FmiConstValue::Real(-rhs.as_real()?)),
        OpUnary::Plus | OpUnary::DotPlus => Some(rhs),
        OpUnary::Not => Some(FmiConstValue::Bool(!rhs.as_bool()?)),
        _ => None,
    }
}

fn eval_fmi_binary(op: &OpBinary, lhs: FmiConstValue, rhs: FmiConstValue) -> Option<FmiConstValue> {
    match op {
        OpBinary::Eq => Some(FmiConstValue::Bool(fmi_values_equal(&lhs, &rhs)?)),
        OpBinary::Neq => Some(FmiConstValue::Bool(!fmi_values_equal(&lhs, &rhs)?)),
        OpBinary::Add | OpBinary::AddElem => {
            Some(FmiConstValue::Real(lhs.as_real()? + rhs.as_real()?))
        }
        OpBinary::Sub | OpBinary::SubElem => {
            Some(FmiConstValue::Real(lhs.as_real()? - rhs.as_real()?))
        }
        OpBinary::Mul | OpBinary::MulElem => {
            Some(FmiConstValue::Real(lhs.as_real()? * rhs.as_real()?))
        }
        OpBinary::Div | OpBinary::DivElem => {
            let rhs = rhs.as_real()?;
            (rhs.abs() > f64::EPSILON).then_some(FmiConstValue::Real(lhs.as_real()? / rhs))
        }
        OpBinary::Exp | OpBinary::ExpElem => {
            Some(FmiConstValue::Real(lhs.as_real()?.powf(rhs.as_real()?)))
        }
        OpBinary::Lt => Some(FmiConstValue::Bool(lhs.as_real()? < rhs.as_real()?)),
        OpBinary::Le => Some(FmiConstValue::Bool(lhs.as_real()? <= rhs.as_real()?)),
        OpBinary::Gt => Some(FmiConstValue::Bool(lhs.as_real()? > rhs.as_real()?)),
        OpBinary::Ge => Some(FmiConstValue::Bool(lhs.as_real()? >= rhs.as_real()?)),
        OpBinary::And => Some(FmiConstValue::Bool(lhs.as_bool()? && rhs.as_bool()?)),
        OpBinary::Or => Some(FmiConstValue::Bool(lhs.as_bool()? || rhs.as_bool()?)),
        _ => None,
    }
}

fn fmi_values_equal(lhs: &FmiConstValue, rhs: &FmiConstValue) -> Option<bool> {
    match (lhs, rhs) {
        (FmiConstValue::Real(lhs), FmiConstValue::Real(rhs)) => {
            Some((lhs - rhs).abs() <= 1.0e-12 * (1.0 + lhs.abs().max(rhs.abs())))
        }
        (FmiConstValue::Bool(lhs), FmiConstValue::Bool(rhs)) => Some(lhs == rhs),
        (FmiConstValue::String(lhs), FmiConstValue::String(rhs)) => Some(lhs == rhs),
        _ => None,
    }
}

fn eval_fmi_builtin(
    function: BuiltinFunction,
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> Option<FmiConstValue> {
    let scalar_arg = |index: usize| eval_fmi_const_expr(args.get(index)?, env)?.as_real();

    match function {
        BuiltinFunction::NoEvent => eval_fmi_const_expr(args.first()?, env),
        BuiltinFunction::Smooth => args
            .get(1)
            .and_then(|expr| eval_fmi_const_expr(expr, env))
            .or_else(|| args.first().and_then(|expr| eval_fmi_const_expr(expr, env))),
        BuiltinFunction::Homotopy | BuiltinFunction::Delay => {
            eval_fmi_const_expr(args.first()?, env)
        }
        BuiltinFunction::Integer => scalar_arg(0).map(f64::floor).map(FmiConstValue::Real),
        BuiltinFunction::Zeros => fmi_array_count(args, 0, env)
            .map(|count| FmiConstValue::Array(vec![FmiConstValue::Real(0.0); count])),
        BuiltinFunction::Ones => fmi_array_count(args, 0, env)
            .map(|count| FmiConstValue::Array(vec![FmiConstValue::Real(1.0); count])),
        BuiltinFunction::Fill => {
            let value = eval_fmi_const_expr(args.first()?, env)?;
            let count = fmi_array_count(args, 1, env)?;
            Some(FmiConstValue::Array(vec![value; count]))
        }
        BuiltinFunction::Linspace => eval_fmi_linspace(args, env),
        BuiltinFunction::Scalar => {
            let value = eval_fmi_const_expr(args.first()?, env)?;
            let mut flat = Vec::new();
            flatten_fmi_values_into(std::slice::from_ref(&value), &mut flat);
            (flat.len() == 1).then(|| flat.remove(0))
        }
        BuiltinFunction::Vector | BuiltinFunction::Matrix => {
            eval_fmi_const_expr(args.first()?, env)
        }
        BuiltinFunction::Sum => {
            let mut values = Vec::new();
            eval_fmi_const_expr(args.first()?, env)?
                .flatten_numeric(&mut values)
                .then_some(())?;
            Some(FmiConstValue::Real(values.iter().sum()))
        }
        BuiltinFunction::Product => {
            let mut values = Vec::new();
            eval_fmi_const_expr(args.first()?, env)?
                .flatten_numeric(&mut values)
                .then_some(())?;
            Some(FmiConstValue::Real(values.iter().product()))
        }
        _ => scalar_arg(0)
            .and_then(|lhs| {
                apply_scalar_unary_math(function, lhs).or_else(|| {
                    scalar_arg(1).and_then(|rhs| apply_scalar_binary_math(function, lhs, rhs))
                })
            })
            .map(FmiConstValue::Real),
    }
}

fn eval_fmi_named_function(
    short_name: &str,
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> Option<FmiConstValue> {
    if short_name == "substring" {
        return eval_fmi_substring(args, env).map(FmiConstValue::String);
    }
    if short_name == "ln" {
        return eval_fmi_builtin(BuiltinFunction::Log, args, env);
    }
    let function = BuiltinFunction::from_name(short_name)?;
    eval_fmi_builtin(function, args, env)
}

fn eval_fmi_substring(args: &[Expression], env: &FmiMetadataEnv<'_>) -> Option<String> {
    let FmiConstValue::String(input) = eval_fmi_const_expr(args.first()?, env)? else {
        return None;
    };
    let start = eval_fmi_const_expr(args.get(1)?, env)?.as_index()? as usize;
    let stop = eval_fmi_const_expr(args.get(2)?, env)?.as_index()? as usize;
    if stop < start {
        return Some(String::new());
    }
    let skip = start.checked_sub(1)?;
    Some(input.chars().skip(skip).take(stop - start + 1).collect())
}

fn fmi_array_count(
    args: &[Expression],
    first_dim_arg: usize,
    env: &FmiMetadataEnv<'_>,
) -> Option<usize> {
    if args.len() <= first_dim_arg {
        return None;
    }
    args.get(first_dim_arg..)?
        .iter()
        .try_fold(1usize, |count, expr| {
            let dim = usize::try_from(eval_fmi_const_expr(expr, env)?.as_index()?).ok()?;
            count.checked_mul(dim)
        })
}

fn eval_fmi_linspace(args: &[Expression], env: &FmiMetadataEnv<'_>) -> Option<FmiConstValue> {
    let start = eval_fmi_const_expr(args.first()?, env)?.as_real()?;
    let end = eval_fmi_const_expr(args.get(1)?, env)?.as_real()?;
    let count = usize::try_from(eval_fmi_const_expr(args.get(2)?, env)?.as_index()?).ok()?;
    if count == 0 {
        return None;
    }
    let values = if count == 1 {
        vec![FmiConstValue::Real(start)]
    } else {
        let step = (end - start) / ((count - 1) as f64);
        (0..count)
            .map(|index| FmiConstValue::Real(start + step * (index as f64)))
            .collect()
    };
    Some(FmiConstValue::Array(values))
}

fn eval_fmi_range(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    env: &FmiMetadataEnv<'_>,
) -> Option<FmiConstValue> {
    let start = eval_fmi_const_expr(start, env)?.as_real()?;
    let step = match step {
        Some(step) => eval_fmi_const_expr(step, env)?.as_real()?,
        None => 1.0,
    };
    let end = eval_fmi_const_expr(end, env)?.as_real()?;
    if !start.is_finite() || !step.is_finite() || !end.is_finite() || step.abs() <= f64::EPSILON {
        return None;
    }
    let mut values = Vec::new();
    let mut current = start;
    let forward = step > 0.0;
    while (forward && current <= end + 1.0e-12) || (!forward && current >= end - 1.0e-12) {
        values.push(FmiConstValue::Real(current));
        if values.len() > 100_000 {
            return None;
        }
        current += step;
    }
    Some(FmiConstValue::Array(values))
}

/// True when the expression contains a String literal or references a name
/// whose folded constant value is a String. Such expressions are evaluable
/// only at translation time (the solve runtime has no string operations), so
/// they must keep folding to literals.
fn expr_depends_on_string_value(expr: &Expression, values: &HashMap<String, ConstValue>) -> bool {
    let mut refs: Vec<VarName> = Vec::new();
    expr.collect_var_refs(&mut refs);
    if refs
        .iter()
        .any(|name| matches!(values.get(name.as_str()), Some(ConstValue::String(_))))
    {
        return true;
    }
    struct StringLiteralFinder {
        found: bool,
    }
    impl ExpressionVisitor for StringLiteralFinder {
        fn visit_expression(&mut self, expr: &Expression) {
            if let Expression::Literal {
                value: rumoca_core::Literal::String(_),
                ..
            } = expr
            {
                self.found = true;
            }
            self.walk_expression(expr);
        }
    }
    let mut finder = StringLiteralFinder { found: false };
    finder.visit_expression(expr);
    finder.found
}

/// True when the start expression references at least one parameter by name.
fn start_references_parameter(
    expr: &Expression,
    param_names: &std::collections::HashSet<String>,
) -> bool {
    let mut refs: Vec<VarName> = Vec::new();
    expr.collect_var_refs(&mut refs);
    refs.iter().any(|name| param_names.contains(name.as_str()))
}

struct StartBindingCollector<'a> {
    bindings: &'a mut Vec<(VarName, Expression)>,
}

impl DaeVisitor for StartBindingCollector<'_> {
    fn visit_variable(
        &mut self,
        _partition: DaeVariablePartition,
        name: &VarName,
        variable: &Variable,
    ) {
        // MLS 3.7 §4.5: with `fixed = false` the variable is determined by
        // the initialization problem and `start` is only a guess value, so
        // it must not be treated as the variable's value when evaluating
        // other start expressions.
        if variable.fixed == Some(false) {
            return;
        }
        if let Some(expr) = &variable.start {
            self.bindings.push((name.clone(), expr.clone()));
        }
    }
}

struct StartRewriter<F> {
    rewrite: F,
}

impl<F> DaeVariableMutVisitor for StartRewriter<F>
where
    F: FnMut(&mut Variable, bool),
{
    fn visit_variable_mut(
        &mut self,
        partition: DaeVariablePartition,
        _name: &VarName,
        variable: &mut Variable,
    ) {
        (self.rewrite)(variable, partition == DaeVariablePartition::Parameter);
    }
}

fn eval_start_const_expr(
    expr: &Expression,
    env: &HashMap<String, ConstValue>,
) -> Option<ConstValue> {
    eval_const_expr_with(expr, &|name, subscripts| {
        if !subscripts.is_empty() {
            return None;
        }
        env.get(name.as_str()).cloned().or_else(|| {
            rumoca_ir_dae::component_base_name(name.as_str())
                .and_then(|base| env.get(&base).cloned())
        })
    })
}

// ---------------------------------------------------------------------------
// Topological sort of parameters by start-expression dependencies
// ---------------------------------------------------------------------------

/// Collect all parameter/constant names referenced in an expression.
fn collect_param_refs(
    expr: &Expression,
    param_names: &std::collections::HashSet<String>,
) -> Vec<String> {
    let mut collector = ParamRefCollector {
        param_names,
        refs: Vec::new(),
    };
    collector.visit_expression(expr);
    collector.refs
}

struct ParamRefCollector<'a> {
    param_names: &'a std::collections::HashSet<String>,
    refs: Vec<String>,
}

impl ExpressionVisitor for ParamRefCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        self.insert_ref(name.as_str());
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

impl ParamRefCollector<'_> {
    fn insert_ref(&mut self, name: &str) {
        if self.param_names.contains(name) && !self.refs.iter().any(|item| item == name) {
            self.refs.push(name.to_string());
            return;
        }

        // Also match base name for subscripted refs: "e[1]" -> "e".
        let base = var_base_name(name);
        if base != name
            && self.param_names.contains(base)
            && !self.refs.iter().any(|item| item == base)
        {
            self.refs.push(base.to_string());
        }
    }
}

/// Sort algebraic and output variable maps by equation dependency order.
///
/// For each variable in `dae.variables.algebraics` or `dae.variables.outputs`, finds its defining
/// equation in `dae.continuous.equations` and extracts which other algebraic/output variables
/// the equation references. Then topologically sorts so that variables are
/// evaluated after their dependencies.
pub(crate) fn sort_algebraics_by_equation_deps(dae: &mut Dae) -> Result<(), ToDaeError> {
    use std::collections::HashSet;

    // Collect all algebraic + output variable names
    let alg_names: HashSet<String> = dae
        .variables
        .algebraics
        .keys()
        .chain(dae.variables.outputs.keys())
        .map(|k| k.as_str().to_string())
        .collect();

    if alg_names.len() <= 1 {
        return Ok(());
    }

    // For each algebraic/output variable, find which other alg/output vars
    // its equation references
    let mut eq_deps: std::collections::HashMap<String, Vec<String>> =
        std::collections::HashMap::new();
    for alg_name in &alg_names {
        eq_deps.insert(alg_name.clone(), Vec::new());
    }

    for eq in &dae.continuous.equations {
        let refs = collect_param_refs(&eq.rhs, &alg_names);
        // This equation may define one of our algebraic vars.
        // Try to identify which variable this equation defines
        // by checking if it matches the pattern `0 = var - expr` or additive form.
        for alg_name in &alg_names {
            if equation_defines_var(&eq.rhs, alg_name) {
                let deps: Vec<String> = refs
                    .iter()
                    .filter(|r| r.as_str() != alg_name.as_str())
                    .cloned()
                    .collect();
                eq_deps.insert(alg_name.clone(), deps);
            }
        }
    }

    // Sort dae.variables.algebraics
    dae.variables.algebraics = topo_sort_by_eq_deps(&dae.variables.algebraics, &eq_deps)?;
    // Sort dae.variables.outputs
    dae.variables.outputs = topo_sort_by_eq_deps(&dae.variables.outputs, &eq_deps)?;
    Ok(())
}

/// Check if an equation's RHS defines a given variable (appears as LHS of subtraction
/// or as a term in an additive equation).
fn equation_defines_var(rhs: &Expression, var_name: &str) -> bool {
    match rhs {
        Expression::Binary {
            op,
            lhs,
            rhs: rhs_inner,
            ..
        } => {
            if matches!(op, rumoca_core::OpBinary::Sub) {
                // 0 = var - expr or 0 = expr - var
                if is_var_ref_named(lhs, var_name) || is_var_ref_named(rhs_inner, var_name) {
                    return true;
                }
            }
            if matches!(op, rumoca_core::OpBinary::Add) {
                // Check additive terms
                let terms = collect_additive_var_refs(rhs);
                if terms.iter().any(|t| t == var_name) {
                    return true;
                }
            }
            false
        }
        Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs: inner,
            ..
        } => equation_defines_var(inner, var_name),
        Expression::VarRef { name, .. } => name.as_str() == var_name,
        _ => false,
    }
}

/// Extract the base name from a possibly-subscripted variable name.
/// E.g., `"e[1]"` → `"e"`, `"q_err_w"` → `"q_err_w"`.
fn var_base_name(name: &str) -> &str {
    rumoca_core::strip_scalar_name_subscripts(name).unwrap_or(name)
}

fn is_var_ref_named(expr: &Expression, name: &str) -> bool {
    matches!(expr, Expression::VarRef { name: n, .. } if n.as_str() == name || var_base_name(n.as_str()) == name)
}

/// Collect all VarRef names from an additive expression tree.
fn collect_additive_var_refs(expr: &Expression) -> Vec<String> {
    match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        }
        | Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            let mut v = collect_additive_var_refs(lhs);
            v.extend(collect_additive_var_refs(rhs));
            v
        }
        Expression::Unary { rhs, .. } => collect_additive_var_refs(rhs),
        Expression::VarRef { name, .. } => vec![var_base_name(name.as_str()).to_string()],
        _ => vec![],
    }
}

fn topo_sort_by_eq_deps(
    map: &indexmap::IndexMap<VarName, Variable>,
    eq_deps: &std::collections::HashMap<String, Vec<String>>,
) -> Result<indexmap::IndexMap<VarName, Variable>, ToDaeError> {
    use std::collections::{HashSet, VecDeque};

    if map.len() <= 1 {
        return Ok(map.clone());
    }

    let entries: Vec<_> = map.iter().collect();
    let name_list: Vec<String> = entries
        .iter()
        .map(|(name, _)| name.as_str().to_string())
        .collect();
    let name_set: HashSet<&str> = name_list.iter().map(|s| s.as_str()).collect();

    // Build adjacency
    let mut deps_idx: Vec<HashSet<usize>> = Vec::with_capacity(map.len());
    for (idx, name) in name_list.iter().enumerate() {
        let dep_names = eq_deps.get(name).ok_or_else(|| {
            ToDaeError::runtime_contract_violation_at(
                format!("start-value dependency set missing for `{name}`"),
                entries[idx].1.source_span,
            )
        })?;
        let dep_indices: HashSet<usize> = dep_names
            .iter()
            .filter(|d| name_set.contains(d.as_str()))
            .filter_map(|d| name_list.iter().position(|n| n == d))
            .collect();
        deps_idx.push(dep_indices);
    }

    // Kahn's algorithm
    let n = map.len();
    let mut in_degree = vec![0usize; n];
    let mut dependents: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (i, dep_set) in deps_idx.iter().enumerate() {
        in_degree[i] = dep_set.len();
        for &d in dep_set {
            dependents[d].push(i);
        }
    }

    let mut queue: VecDeque<usize> = VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut order: Vec<usize> = Vec::with_capacity(n);
    while let Some(idx) = queue.pop_front() {
        order.push(idx);
        for &dep in &dependents[idx] {
            in_degree[dep] -= 1;
            if in_degree[dep] == 0 {
                queue.push_back(dep);
            }
        }
    }

    // Append cyclic entries in original order
    if order.len() < n {
        for i in 0..n {
            if !order.contains(&i) {
                order.push(i);
            }
        }
    }

    reorder_by_index_order(map, &order)
}

fn reorder_by_index_order(
    map: &indexmap::IndexMap<VarName, Variable>,
    order: &[usize],
) -> Result<indexmap::IndexMap<VarName, Variable>, ToDaeError> {
    let mut sorted = indexmap::IndexMap::with_capacity(map.len());
    for &idx in order {
        let Some((k, v)) = map.get_index(idx) else {
            return Err(ToDaeError::internal(format!(
                "topological order index {idx} out of range for {} variables",
                map.len()
            )));
        };
        sorted.insert(k.clone(), v.clone());
    }
    Ok(sorted)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{BuiltinFunction, Literal, OpBinary};

    fn test_span(start: usize, end: usize) -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fold_start_values_fixture.mo"),
            start,
            end,
        )
    }

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: vec![],
            span: test_span(1, 2),
        }
    }

    fn var_index(name: &str, index: i64) -> Expression {
        Expression::VarRef {
            name: VarName::new(name).into(),
            subscripts: vec![Subscript::Index {
                value: index,
                span: test_span(1, 2),
            }],
            span: test_span(1, 2),
        }
    }

    fn array(elements: Vec<Expression>) -> Expression {
        Expression::Array {
            elements,
            is_matrix: false,
            span: test_span(9, 10),
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal {
            value: Literal::Real(value),
            span: test_span(3, 4),
        }
    }

    fn integer(value: i64) -> Expression {
        Expression::Literal {
            value: Literal::Integer(value),
            span: test_span(5, 6),
        }
    }

    fn string(value: &str) -> Expression {
        Expression::Literal {
            value: Literal::String(value.to_string()),
            span: test_span(7, 8),
        }
    }

    fn parameter(name: &str, start: Expression) -> Variable {
        let mut var = Variable::new(
            VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.start = Some(start);
        var.is_tunable = false;
        var
    }

    fn state(name: &str, start: Expression) -> Variable {
        let mut var = Variable::new(
            VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        var.start = Some(start);
        var
    }

    fn assert_real(expr: &Expression, expected: f64) {
        match expr {
            Expression::Literal {
                value: Literal::Real(value),
                ..
            } => assert!(
                (*value - expected).abs() <= 1.0e-12,
                "expected {expected}, got {value}"
            ),
            other => panic!("expected real literal {expected}, got {other:?}"),
        }
    }

    fn assert_real_array(expr: &Expression, expected: &[f64]) {
        let Expression::Array { elements, .. } = expr else {
            panic!("expected real array {expected:?}, got {expr:?}");
        };
        assert_eq!(elements.len(), expected.len());
        for (element, expected) in elements.iter().zip(expected) {
            assert_real(element, *expected);
        }
    }

    #[test]
    fn var_base_name_strips_only_trailing_subscripts() {
        assert_eq!(var_base_name("e[1]"), "e");
        assert_eq!(var_base_name("e[1,2]"), "e");
        assert_eq!(var_base_name("q_err_w"), "q_err_w");
        assert_eq!(var_base_name("record[1].field"), "record[1].field");
        assert_eq!(var_base_name("record[1].field[2]"), "record[1].field");
        assert_eq!(var_base_name("record[index.re]"), "record[index.re]");
    }

    #[test]
    fn var_ref_dependency_match_uses_trailing_scalar_subscript_base() {
        assert!(is_var_ref_named(&var("e[1]"), "e"));
        assert!(is_var_ref_named(
            &var("record[1].field[2]"),
            "record[1].field"
        ));
        assert!(!is_var_ref_named(&var("record[1].field"), "record"));
        assert!(!is_var_ref_named(&var("record[index.re]"), "record"));
    }

    #[test]
    fn reorder_by_index_order_reports_invalid_topological_index() {
        let mut variables = indexmap::IndexMap::new();
        variables.insert(
            VarName::new("x"),
            Variable::new(
                VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );

        let err = reorder_by_index_order(&variables, &[1]).expect_err("invalid index");

        assert!(
            matches!(err, ToDaeError::Internal(message) if message.contains("topological order index 1 out of range for 1 variables"))
        );
    }

    #[test]
    fn folds_string_substring_condition_before_runtime_parameter_tail() {
        let mut dae = Dae::new();
        dae.variables.parameters.insert(
            VarName::new("transformer1.VectorGroup"),
            parameter("transformer1.VectorGroup", string("Dy01")),
        );
        dae.variables.parameters.insert(
            VarName::new("transformerData1.C1"),
            parameter(
                "transformerData1.C1",
                Expression::FunctionCall {
                    name: VarName::new("Modelica.Utilities.Strings.substring").into(),
                    args: vec![var("transformer1.VectorGroup"), integer(1), integer(1)],
                    is_constructor: false,
                    span: test_span(9, 10),
                },
            ),
        );
        dae.variables.parameters.insert(
            VarName::new("transformerData1.V1"),
            parameter("transformerData1.V1", real(100.0)),
        );
        dae.variables.parameters.insert(
            VarName::new("transformerData1.V1ph"),
            parameter(
                "transformerData1.V1ph",
                Expression::Binary {
                    op: OpBinary::Div,
                    lhs: Box::new(var("transformerData1.V1")),
                    rhs: Box::new(Expression::If {
                        branches: vec![(
                            Expression::Binary {
                                op: OpBinary::Eq,
                                lhs: Box::new(var("transformerData1.C1")),
                                rhs: Box::new(string("D")),
                                span: test_span(11, 12),
                            },
                            integer(1),
                        )],
                        else_branch: Box::new(Expression::BuiltinCall {
                            function: BuiltinFunction::Sqrt,
                            args: vec![integer(3)],
                            span: test_span(13, 14),
                        }),
                        span: test_span(15, 16),
                    }),
                    span: test_span(17, 18),
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        let c1 = &dae.variables.parameters[&VarName::new("transformerData1.C1")];
        assert!(matches!(
            c1.start,
            Some(Expression::Literal {
                value: Literal::String(ref value),
                ..
            }) if value == "D"
        ));

        let v1ph = &dae.variables.parameters[&VarName::new("transformerData1.V1ph")];
        match v1ph.start {
            Some(Expression::Literal {
                value: Literal::Real(value),
                ..
            }) => assert!((value - 100.0).abs() <= 1.0e-12),
            ref other => panic!("expected folded numeric V1ph start, got {other:?}"),
        }
    }

    #[test]
    fn preserves_numeric_parameter_start_depending_on_parameters() {
        let mut dae = Dae::new();
        dae.variables
            .parameters
            .insert(VarName::new("Isp"), parameter("Isp", real(300.0)));
        dae.variables
            .parameters
            .insert(VarName::new("g0"), parameter("g0", real(9.81)));
        dae.variables.parameters.insert(
            VarName::new("ve"),
            parameter(
                "ve",
                Expression::Binary {
                    op: OpBinary::Mul,
                    lhs: Box::new(var("Isp")),
                    rhs: Box::new(var("g0")),
                    span: test_span(19, 20),
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        // A runtime override of `Isp` must propagate to `ve`, so the computed
        // start keeps its expression instead of folding to 2943.0.
        let ve = &dae.variables.parameters[&VarName::new("ve")];
        assert!(
            matches!(ve.start, Some(Expression::Binary { .. })),
            "expected preserved expression start for ve, got {:?}",
            ve.start
        );
    }

    #[test]
    fn fmi_model_description_folds_parameter_dependent_starts() {
        let mut dae = Dae::new();
        dae.variables
            .parameters
            .insert(VarName::new("p"), parameter("p", real(2.0)));
        dae.variables
            .parameters
            .insert(VarName::new("q"), parameter("q", real(3.0)));
        dae.variables.parameters.insert(
            VarName::new("r"),
            parameter(
                "r",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(var("p")),
                    rhs: Box::new(var("q")),
                    span: test_span(21, 22),
                },
            ),
        );
        dae.variables.states.insert(
            VarName::new("x"),
            state(
                "x",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(var("p")),
                    rhs: Box::new(var("q")),
                    span: test_span(23, 24),
                },
            ),
        );
        dae.variables
            .states
            .insert(VarName::new("y"), state("y", var("r")));

        fold_fmi_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("FMI start folding should succeed: {err}"));

        for name in ["r", "x", "y"] {
            let start = dae
                .variables
                .parameters
                .get(&VarName::new(name))
                .or_else(|| dae.variables.states.get(&VarName::new(name)))
                .and_then(|var| var.start.as_ref())
                .unwrap_or_else(|| panic!("{name} should have a folded start"));
            match start {
                Expression::Literal {
                    value: Literal::Real(value),
                    ..
                } => assert!(
                    (*value - 5.0).abs() <= 1.0e-12,
                    "{name} should fold to 5.0, got {value}"
                ),
                other => panic!("{name} should fold to a real literal, got {other:?}"),
            }
        }
    }

    #[test]
    fn fmi_model_description_rejects_unserializable_start_expression() {
        let mut dae = Dae::new();
        dae.variables
            .states
            .insert(VarName::new("x"), state("x", var("unbound")));

        let err = fold_fmi_start_values_to_literals(&mut dae)
            .expect_err("FMI start folding should reject unresolved starts");

        assert!(
            matches!(err, ToDaeError::RuntimeMetadataViolationAt { detail, .. } if detail.contains("FMI modelDescription start for `x`"))
        );
    }

    #[test]
    fn fmi_model_description_folds_numeric_metadata_attributes() {
        let mut dae = Dae::new();
        dae.variables
            .parameters
            .insert(VarName::new("p"), parameter("p", real(2.0)));
        dae.variables
            .parameters
            .insert(VarName::new("q"), parameter("q", real(3.0)));
        dae.variables.parameters.insert(
            VarName::new("r"),
            parameter(
                "r",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(var("p")),
                    rhs: Box::new(var("q")),
                    span: test_span(24, 25),
                },
            ),
        );
        let mut x = state("x", var("r"));
        x.min = Some(Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(var("p")),
            rhs: Box::new(real(1.0)),
            span: test_span(26, 27),
        });
        x.max = Some(Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(var("q")),
            rhs: Box::new(real(5.0)),
            span: test_span(28, 29),
        });
        x.nominal = Some(var("r"));
        dae.variables.states.insert(VarName::new("x"), x);

        fold_fmi_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("FMI metadata folding should succeed: {err}"));

        let x = &dae.variables.states[&VarName::new("x")];
        assert_real(x.start.as_ref().expect("start"), 5.0);
        assert_real(x.min.as_ref().expect("min"), 1.0);
        assert_real(x.max.as_ref().expect("max"), 8.0);
        assert_real(x.nominal.as_ref().expect("nominal"), 5.0);
    }

    #[test]
    fn fmi_model_description_folds_array_alias_and_subscripted_starts() {
        let mut dae = Dae::new();
        let mut p = parameter("p", array(vec![real(1.0), real(2.0)]));
        p.dims = vec![2];
        dae.variables.parameters.insert(VarName::new("p"), p);
        let mut x = state("x", var("p"));
        x.dims = vec![2];
        dae.variables.states.insert(VarName::new("x"), x);
        dae.variables
            .states
            .insert(VarName::new("y"), state("y", var_index("p", 2)));

        fold_fmi_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("FMI array folding should succeed: {err}"));

        assert_real_array(
            dae.variables.states[&VarName::new("x")]
                .start
                .as_ref()
                .expect("x start"),
            &[1.0, 2.0],
        );
        assert_real(
            dae.variables.states[&VarName::new("y")]
                .start
                .as_ref()
                .expect("y start"),
            2.0,
        );
    }

    #[test]
    fn fmi_model_description_folds_array_constructors_and_ranges() {
        let mut dae = Dae::new();
        dae.variables
            .parameters
            .insert(VarName::new("p"), parameter("p", real(4.0)));
        let mut fill_var = state(
            "fillVar",
            Expression::BuiltinCall {
                function: BuiltinFunction::Fill,
                args: vec![
                    Expression::Binary {
                        op: OpBinary::Sub,
                        lhs: Box::new(var("p")),
                        rhs: Box::new(real(3.0)),
                        span: test_span(30, 31),
                    },
                    integer(3),
                ],
                span: test_span(32, 33),
            },
        );
        fill_var.dims = vec![3];
        dae.variables
            .states
            .insert(VarName::new("fillVar"), fill_var);
        let mut range_var = state(
            "rangeVar",
            Expression::Range {
                start: Box::new(integer(1)),
                step: None,
                end: Box::new(integer(3)),
                span: test_span(34, 35),
            },
        );
        range_var.dims = vec![3];
        dae.variables
            .states
            .insert(VarName::new("rangeVar"), range_var);

        fold_fmi_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("FMI constructor folding should succeed: {err}"));

        assert_real_array(
            dae.variables.states[&VarName::new("fillVar")]
                .start
                .as_ref()
                .expect("fillVar start"),
            &[1.0, 1.0, 1.0],
        );
        assert_real_array(
            dae.variables.states[&VarName::new("rangeVar")]
                .start
                .as_ref()
                .expect("rangeVar start"),
            &[1.0, 2.0, 3.0],
        );
    }

    #[test]
    fn folded_start_literal_preserves_start_attribute_span() {
        let mut dae = Dae::new();
        let span = test_span(10, 20);
        let mut parameter = parameter(
            "ratio",
            Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(real(2.0)),
                rhs: Box::new(real(3.0)),
                span: test_span(1, 8),
            },
        );
        parameter.start_span = Some(span);
        dae.variables
            .parameters
            .insert(VarName::new("ratio"), parameter);

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        assert_eq!(
            dae.variables.parameters[&VarName::new("ratio")]
                .start
                .as_ref()
                .and_then(Expression::span),
            Some(span)
        );
    }

    #[test]
    fn folded_start_literal_preserves_expression_span_without_attribute_span() {
        let mut dae = Dae::new();
        let span = test_span(30, 40);
        dae.variables.parameters.insert(
            VarName::new("ratio"),
            parameter(
                "ratio",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(real(2.0)),
                    rhs: Box::new(real(3.0)),
                    span,
                },
            ),
        );

        fold_start_values_to_literals(&mut dae)
            .unwrap_or_else(|err| panic!("start folding should succeed: {err}"));

        assert_eq!(
            dae.variables.parameters[&VarName::new("ratio")]
                .start
                .as_ref()
                .and_then(Expression::span),
            Some(span)
        );
    }

    #[test]
    fn folded_start_literal_rejects_missing_provenance() {
        let mut dae = Dae::new();
        dae.variables.parameters.insert(
            VarName::new("ratio"),
            parameter(
                "ratio",
                Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Box::new(Expression::Literal {
                        value: Literal::Real(2.0),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Real(3.0),
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
            ),
        );

        let err = fold_start_values_to_literals(&mut dae)
            .expect_err("unspanned folded start should fail");

        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }
}
