//! Constant-fold FMI modelDescription numeric metadata to typed literals.
//!
//! FMI XML attributes such as `start`, `min`, `max`, and `nominal` carry
//! concrete values, not Modelica expressions. This pass evaluates those
//! attributes under default parameter values for the XML metadata projection
//! only; it does not mutate the simulation DAE used by runtime codegen.

use crate::errors::ToDaeError;
use rumoca_core::{
    BuiltinFunction, DefId, Expression, Literal, OpBinary, OpUnary, Reference, Span, Subscript,
    VarName, apply_scalar_binary_math, apply_scalar_unary_math,
};
use rumoca_ir_dae::{
    Dae, DaeVariableMutVisitor, DaeVariablePartition, DaeVisitor, Variable, VariableOrigin,
};
use std::collections::HashMap;
use std::sync::Arc;

type FmiEvalResult<T> = Result<T, FmiEvalError>;

/// Prepare FMI modelDescription metadata: XML numeric attributes have no
/// expression language, so every serialized start/min/max/nominal value must be
/// a finite numeric value or value list under default parameter values.
pub(crate) fn fold_fmi_model_description_values_to_literals(
    dae: &mut Dae,
) -> Result<(), ToDaeError> {
    let dims = collect_variable_dims(dae)?;
    let runtime =
        rumoca_eval_dae::build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
            dae,
            &[],
            0.0,
            Arc::new(rumoca_eval_dae::EvalRuntimeState::new()),
        )
        .map_err(|err| {
            ToDaeError::runtime_metadata_violation(format!(
                "could not prepare default parameter evaluation for FMI modelDescription: {err}"
            ))
        })?;
    let values = collect_best_effort_fmi_metadata_values(dae, &dims, &runtime)?;
    rewrite_fmi_model_description_values(dae, &dims, &runtime, &values)
}

pub(crate) fn fold_fmi_model_description_values_from_source(
    target: &mut Dae,
    source: &Dae,
) -> Result<(), ToDaeError> {
    let dims = collect_variable_dims(source)?;
    let runtime =
        rumoca_eval_dae::build_partial_runtime_parameter_tail_env_with_declared_slots_and_runtime(
            source,
            &[],
            0.0,
            Arc::new(rumoca_eval_dae::EvalRuntimeState::new()),
        )
        .map_err(|err| {
            ToDaeError::runtime_metadata_violation(format!(
                "could not prepare default parameter evaluation for FMI modelDescription: {err}"
            ))
        })?;
    let values = collect_best_effort_fmi_metadata_values(source, &dims, &runtime)?;
    rewrite_fmi_model_description_values(target, &dims, &runtime, &values)
}

fn rewrite_fmi_model_description_values(
    target: &mut Dae,
    dims: &HashMap<FmiValueKey, Vec<i64>>,
    runtime: &rumoca_eval_dae::VarEnv<f64>,
    values: &HashMap<FmiValueKey, FmiConstValue>,
) -> Result<(), ToDaeError> {
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
                values,
                dims,
                runtime,
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
                values,
                dims,
                runtime,
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
                values,
                dims,
                runtime,
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
                values,
                dims,
                runtime,
            ) {
                Ok(expr) => var.nominal = Some(expr),
                Err(err) => rewrite_error = Err(err),
            }
        }
    };

    FmiMetadataRewriter { rewrite }.visit_variables_mut(&mut target.variables);
    rewrite_error
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum FmiValueKey {
    Def(DefId),
    GeneratedName(VarName),
    EnumLiteralName(VarName),
}

fn variable_key(name: &VarName, variable: &Variable) -> Result<FmiValueKey, ToDaeError> {
    if let Some(def_id) = variable
        .component_ref
        .as_ref()
        .and_then(|reference| reference.def_id)
    {
        return Ok(FmiValueKey::Def(def_id));
    }
    match variable.origin {
        VariableOrigin::Generated => Ok(FmiValueKey::GeneratedName(name.clone())),
        VariableOrigin::Source => Err(ToDaeError::runtime_contract_violation_at(
            format!("source DAE variable `{name}` lost DefId metadata before FMI metadata folding"),
            variable.source_span,
        )),
    }
}

fn reference_key(name: &Reference, env: &FmiMetadataEnv<'_>) -> FmiEvalResult<FmiValueKey> {
    if let Some(def_id) = name.target_def_id() {
        return Ok(FmiValueKey::Def(def_id));
    }
    if name.is_generated() {
        return Ok(FmiValueKey::GeneratedName(name.var_name().clone()));
    }
    if !name.has_structure() {
        let generated_key = FmiValueKey::GeneratedName(name.var_name().clone());
        if env.values.contains_key(&generated_key) || env.dims.contains_key(&generated_key) {
            return Ok(generated_key);
        }
        let enum_key = FmiValueKey::EnumLiteralName(name.var_name().clone());
        if env.values.contains_key(&enum_key) {
            return Ok(enum_key);
        }
        return Err(FmiEvalError::PendingDependency);
    }
    Err(FmiEvalError::MissingDefId)
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
    fn as_real(&self) -> FmiEvalResult<f64> {
        match self {
            Self::Real(value) => Ok(*value),
            Self::Bool(_) | Self::String(_) | Self::Array(_) => Err(FmiEvalError::NonNumeric),
        }
    }

    fn as_bool(&self) -> FmiEvalResult<bool> {
        match self {
            Self::Bool(value) => Ok(*value),
            Self::Real(_) | Self::String(_) | Self::Array(_) => Err(FmiEvalError::NonBoolean),
        }
    }

    fn as_index(&self) -> FmiEvalResult<i64> {
        let value = self.as_real()?;
        if value.is_finite()
            && value >= 1.0
            && value <= i64::MAX as f64
            && value.fract().abs() <= 1.0e-12
        {
            Ok(value as i64)
        } else {
            Err(FmiEvalError::InvalidIndex)
        }
    }

    fn is_finite(&self) -> bool {
        match self {
            Self::Real(value) => value.is_finite(),
            Self::Bool(_) | Self::String(_) => true,
            Self::Array(elements) => elements.iter().all(Self::is_finite),
        }
    }

    fn flatten_numeric(&self, out: &mut Vec<f64>) -> FmiEvalResult<()> {
        match self {
            Self::Real(value) if value.is_finite() => {
                out.push(*value);
                Ok(())
            }
            Self::Real(_) => Err(FmiEvalError::NonFinite),
            Self::Bool(value) => {
                out.push(if *value { 1.0 } else { 0.0 });
                Ok(())
            }
            Self::Array(elements) => {
                for element in elements {
                    element.flatten_numeric(out)?;
                }
                Ok(())
            }
            Self::String(_) => Err(FmiEvalError::NonNumeric),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum FmiEvalError {
    PendingDependency,
    Unsupported(&'static str),
    NonNumeric,
    NonBoolean,
    NonString,
    NonFinite,
    InvalidIndex,
    InvalidSubscript,
    OutOfBoundsSubscript,
    ShapeMismatch,
    DivisionByZero,
    Overflow,
    MissingDefId,
    FunctionEvaluation(String),
}

impl std::fmt::Display for FmiEvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PendingDependency => write!(f, "referenced value is not available"),
            Self::Unsupported(what) => write!(f, "unsupported expression: {what}"),
            Self::NonNumeric => write!(f, "value is not numeric"),
            Self::NonBoolean => write!(f, "condition is not Boolean"),
            Self::NonString => write!(f, "value is not a String"),
            Self::NonFinite => write!(f, "numeric value is not finite"),
            Self::InvalidIndex => write!(f, "subscript is not a positive integer"),
            Self::InvalidSubscript => write!(f, "unsupported subscript"),
            Self::OutOfBoundsSubscript => write!(f, "subscript is out of bounds"),
            Self::ShapeMismatch => write!(f, "value shape does not match variable shape"),
            Self::DivisionByZero => write!(f, "division by zero"),
            Self::Overflow => write!(f, "integer overflow while evaluating metadata"),
            Self::MissingDefId => write!(f, "structured reference is missing DefId metadata"),
            Self::FunctionEvaluation(detail) => {
                write!(f, "function evaluation failed: {detail}")
            }
        }
    }
}

struct FmiMetadataEnv<'a> {
    values: &'a HashMap<FmiValueKey, FmiConstValue>,
    dims: &'a HashMap<FmiValueKey, Vec<i64>>,
    runtime: &'a rumoca_eval_dae::VarEnv<f64>,
}

fn collect_variable_dims(dae: &Dae) -> Result<HashMap<FmiValueKey, Vec<i64>>, ToDaeError> {
    let mut dims = HashMap::new();
    let mut collector = VariableDimsCollector {
        dims: &mut dims,
        error: None,
    };
    collector.visit_dae(dae);
    if let Some(error) = collector.error {
        return Err(error);
    }
    Ok(dims)
}

struct VariableDimsCollector<'a> {
    dims: &'a mut HashMap<FmiValueKey, Vec<i64>>,
    error: Option<ToDaeError>,
}

impl DaeVisitor for VariableDimsCollector<'_> {
    fn visit_variable(
        &mut self,
        _partition: DaeVariablePartition,
        name: &VarName,
        variable: &Variable,
    ) {
        if self.error.is_some() {
            return;
        }
        match variable_key(name, variable) {
            Ok(key) => {
                self.dims.insert(key, variable.dims.clone());
            }
            Err(error) => self.error = Some(error),
        }
    }
}

fn collect_best_effort_fmi_metadata_values(
    dae: &Dae,
    dims: &HashMap<FmiValueKey, Vec<i64>>,
    runtime: &rumoca_eval_dae::VarEnv<f64>,
) -> Result<HashMap<FmiValueKey, FmiConstValue>, ToDaeError> {
    let mut values: HashMap<FmiValueKey, FmiConstValue> = HashMap::new();

    for (name, ordinal) in &dae.symbols.enum_literal_ordinals {
        values.insert(
            FmiValueKey::EnumLiteralName(VarName::new(name)),
            FmiConstValue::Real(*ordinal as f64),
        );
    }

    let mut bindings = Vec::new();
    let mut collector = FmiMetadataBindingCollector {
        bindings: &mut bindings,
        error: None,
    };
    collector.visit_dae(dae);
    if let Some(error) = collector.error {
        return Err(error);
    }

    let max_passes = bindings.len().max(1) * 2;
    for _ in 0..max_passes {
        let mut changed = false;
        for (key, expr) in &bindings {
            if values.contains_key(key) {
                continue;
            }
            let evaluated = {
                let env = FmiMetadataEnv {
                    values: &values,
                    dims,
                    runtime,
                };
                try_eval_fmi_const_expr(expr, &env)
            };
            let Ok(value) = evaluated else {
                continue;
            };
            if value.is_finite() {
                values.insert(key.clone(), value);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    Ok(values)
}

fn fmi_numeric_attribute_literal_expression(
    var: &Variable,
    attr: FmiNumericAttribute,
    expr: &Expression,
    values: &HashMap<FmiValueKey, FmiConstValue>,
    dims: &HashMap<FmiValueKey, Vec<i64>>,
    runtime: &rumoca_eval_dae::VarEnv<f64>,
) -> Result<Expression, ToDaeError> {
    let span = attr.span(var, expr)?;
    let env = FmiMetadataEnv {
        values,
        dims,
        runtime,
    };
    let value = try_eval_fmi_const_expr(expr, &env)
        .map_err(|err| fmi_metadata_not_serializable_error(var, attr, expr, err))?;
    fmi_numeric_value_to_expression(var, value, span)
        .map_err(|err| fmi_metadata_not_serializable_error(var, attr, expr, err))
}

fn fmi_numeric_value_to_expression(
    var: &Variable,
    value: FmiConstValue,
    span: Span,
) -> FmiEvalResult<Expression> {
    match &value {
        FmiConstValue::Real(value) => {
            if !value.is_finite() {
                return Err(FmiEvalError::NonFinite);
            }
            return Ok(Expression::Literal {
                value: Literal::Real(*value),
                span,
            });
        }
        FmiConstValue::Bool(value) => {
            return Ok(Expression::Literal {
                value: Literal::Real(if *value { 1.0 } else { 0.0 }),
                span,
            });
        }
        FmiConstValue::String(_) | FmiConstValue::Array(_) => {}
    }

    let mut values = Vec::new();
    value.flatten_numeric(&mut values)?;
    let var_size = var.try_size().map_err(|_| FmiEvalError::ShapeMismatch)?;
    if var_size == 1 {
        return match values.as_slice() {
            [value] => Ok(Expression::Literal {
                value: Literal::Real(*value),
                span,
            }),
            _ => Err(FmiEvalError::ShapeMismatch),
        };
    }
    if values.len() != var_size {
        return Err(FmiEvalError::ShapeMismatch);
    }
    let elements = values
        .into_iter()
        .map(|value| Expression::Literal {
            value: Literal::Real(value),
            span,
        })
        .collect();
    Ok(Expression::Array {
        elements,
        is_matrix: false,
        span,
    })
}

fn fmi_metadata_not_serializable_error(
    var: &Variable,
    attr: FmiNumericAttribute,
    expr: &Expression,
    err: FmiEvalError,
) -> ToDaeError {
    let detail = format!(
        "FMI modelDescription {} for `{}` must serialize to finite numeric XML under default parameters; {err}; unevaluable {} expression: {expr:?}",
        attr.name(),
        var.name.as_str(),
        attr.name()
    );
    match attr.span(var, expr) {
        Ok(span) => ToDaeError::runtime_metadata_violation_at(detail, span),
        Err(_) => ToDaeError::runtime_metadata_violation(detail),
    }
}

fn try_eval_fmi_const_expr(
    expr: &Expression,
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    match expr {
        Expression::Literal {
            value: Literal::Integer(value),
            ..
        } => Ok(FmiConstValue::Real(*value as f64)),
        Expression::Literal {
            value: Literal::Real(value),
            ..
        } => Ok(FmiConstValue::Real(*value)),
        Expression::Literal {
            value: Literal::Boolean(value),
            ..
        } => Ok(FmiConstValue::Bool(*value)),
        Expression::Literal {
            value: Literal::String(value),
            ..
        } => Ok(FmiConstValue::String(value.clone())),
        Expression::VarRef {
            name, subscripts, ..
        } => try_eval_fmi_var_ref(name, subscripts, env),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_value = try_eval_fmi_const_expr(base, env)?;
            apply_fmi_subscripts(base_value, subscripts, None, env)
        }
        Expression::Unary { op, rhs, .. } => {
            let rhs = try_eval_fmi_const_expr(rhs, env)?;
            eval_fmi_unary(op, rhs)
        }
        Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = try_eval_fmi_const_expr(lhs, env)?;
            let rhs = try_eval_fmi_const_expr(rhs, env)?;
            let has_array =
                matches!(&lhs, FmiConstValue::Array(_)) || matches!(&rhs, FmiConstValue::Array(_));
            match eval_fmi_binary(op, lhs, rhs) {
                Err(FmiEvalError::NonNumeric) if has_array => {
                    eval_fmi_runtime_numeric_expr(expr, env)
                }
                result => result,
            }
        }
        Expression::BuiltinCall { function, args, .. } => eval_fmi_builtin(*function, args, env),
        Expression::FunctionCall { name, args, .. } => eval_fmi_named_function(name, args, env),
        Expression::FieldAccess { .. } => eval_fmi_runtime_numeric_expr(expr, env),
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, then_expr) in branches {
                if try_eval_fmi_const_expr(condition, env)?.as_bool()? {
                    return try_eval_fmi_const_expr(then_expr, env);
                }
            }
            try_eval_fmi_const_expr(else_branch, env)
        }
        Expression::Array { elements, .. } => {
            let mut values = Vec::with_capacity(elements.len());
            for element in elements {
                values.push(try_eval_fmi_const_expr(element, env)?);
            }
            Ok(FmiConstValue::Array(values))
        }
        Expression::Range {
            start, step, end, ..
        } => eval_fmi_range(start, step.as_deref(), end, env),
        _ => Err(FmiEvalError::Unsupported("expression kind")),
    }
}

fn eval_fmi_runtime_numeric_expr(
    expr: &Expression,
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let values = rumoca_eval_dae::eval_array_values::<f64>(expr, env.runtime)
        .map_err(|err| FmiEvalError::FunctionEvaluation(err.to_string()))?;
    match values.as_slice() {
        [value] => Ok(FmiConstValue::Real(*value)),
        _ => Ok(FmiConstValue::Array(
            values.into_iter().map(FmiConstValue::Real).collect(),
        )),
    }
}

fn try_eval_fmi_var_ref(
    name: &Reference,
    subscripts: &[Subscript],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let key = reference_key(name, env)?;
    let value = env
        .values
        .get(&key)
        .cloned()
        .ok_or(FmiEvalError::PendingDependency)?;
    let combined_subscripts = combined_reference_subscripts(name, subscripts);
    if combined_subscripts.is_empty() {
        return Ok(value);
    }
    let indices = eval_fmi_subscript_indices(&combined_subscripts, env)?;
    let dims = env.dims.get(&key).map(Vec::as_slice);
    apply_fmi_indices(value, &indices, dims)
}

fn combined_reference_subscripts(
    name: &Reference,
    explicit_subscripts: &[Subscript],
) -> Vec<Subscript> {
    let mut subscripts = name
        .parts()
        .last()
        .map(|part| part.subs.clone())
        .unwrap_or_default();
    subscripts.extend(explicit_subscripts.iter().cloned());
    subscripts
}

fn eval_fmi_subscript_indices(
    subscripts: &[Subscript],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<Vec<i64>> {
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        match subscript {
            Subscript::Index { value, .. } => indices.push(*value),
            Subscript::Expr { expr, .. } => {
                indices.push(try_eval_fmi_const_expr(expr, env)?.as_index()?);
            }
            Subscript::Colon { .. } => {
                if subscripts
                    .iter()
                    .all(|item| matches!(item, Subscript::Colon { .. }))
                {
                    return Ok(Vec::new());
                }
                return Err(FmiEvalError::InvalidSubscript);
            }
        }
    }
    Ok(indices)
}

fn apply_fmi_subscripts(
    value: FmiConstValue,
    subscripts: &[Subscript],
    dims: Option<&[i64]>,
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let indices = eval_fmi_subscript_indices(subscripts, env)?;
    apply_fmi_indices(value, &indices, dims)
}

fn apply_fmi_indices(
    value: FmiConstValue,
    indices: &[i64],
    dims: Option<&[i64]>,
) -> FmiEvalResult<FmiConstValue> {
    if indices.is_empty() {
        return Ok(value);
    }
    let FmiConstValue::Array(elements) = value else {
        return Err(FmiEvalError::InvalidSubscript);
    };
    let offset = if let Some(dims) = dims {
        flat_offset_for_indices(indices, dims)?
    } else if indices.len() == 1 {
        let index = indices[0]
            .checked_sub(1)
            .ok_or(FmiEvalError::InvalidIndex)?;
        usize::try_from(index).map_err(|_| FmiEvalError::InvalidIndex)?
    } else {
        return Err(FmiEvalError::ShapeMismatch);
    };
    flattened_fmi_values(&elements)
        .get(offset)
        .cloned()
        .ok_or(FmiEvalError::OutOfBoundsSubscript)
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

fn flat_offset_for_indices(indices: &[i64], dims: &[i64]) -> FmiEvalResult<usize> {
    if indices.len() != dims.len() {
        return Err(FmiEvalError::ShapeMismatch);
    }
    let mut offset = 0usize;
    for (index, dim) in indices.iter().zip(dims) {
        let dim = usize::try_from(*dim).map_err(|_| FmiEvalError::ShapeMismatch)?;
        if dim == 0 {
            return Err(FmiEvalError::ShapeMismatch);
        }
        let index = index
            .checked_sub(1)
            .ok_or(FmiEvalError::InvalidIndex)
            .and_then(|value| usize::try_from(value).map_err(|_| FmiEvalError::InvalidIndex))?;
        if index >= dim {
            return Err(FmiEvalError::OutOfBoundsSubscript);
        }
        offset = offset
            .checked_mul(dim)
            .and_then(|offset| offset.checked_add(index))
            .ok_or(FmiEvalError::Overflow)?;
    }
    Ok(offset)
}

fn eval_fmi_unary(op: &OpUnary, rhs: FmiConstValue) -> FmiEvalResult<FmiConstValue> {
    match op {
        OpUnary::Minus | OpUnary::DotMinus => Ok(FmiConstValue::Real(-rhs.as_real()?)),
        OpUnary::Plus | OpUnary::DotPlus => Ok(rhs),
        OpUnary::Not => Ok(FmiConstValue::Bool(!rhs.as_bool()?)),
        _ => Err(FmiEvalError::Unsupported("unary operator")),
    }
}

fn eval_fmi_binary(
    op: &OpBinary,
    lhs: FmiConstValue,
    rhs: FmiConstValue,
) -> FmiEvalResult<FmiConstValue> {
    match op {
        OpBinary::Eq => Ok(FmiConstValue::Bool(fmi_values_equal(&lhs, &rhs)?)),
        OpBinary::Neq => Ok(FmiConstValue::Bool(!fmi_values_equal(&lhs, &rhs)?)),
        OpBinary::Add | OpBinary::AddElem => {
            Ok(FmiConstValue::Real(lhs.as_real()? + rhs.as_real()?))
        }
        OpBinary::Sub | OpBinary::SubElem => {
            Ok(FmiConstValue::Real(lhs.as_real()? - rhs.as_real()?))
        }
        OpBinary::Mul | OpBinary::MulElem => {
            Ok(FmiConstValue::Real(lhs.as_real()? * rhs.as_real()?))
        }
        OpBinary::Div | OpBinary::DivElem => {
            let rhs = rhs.as_real()?;
            if rhs.abs() <= f64::EPSILON {
                return Err(FmiEvalError::DivisionByZero);
            }
            Ok(FmiConstValue::Real(lhs.as_real()? / rhs))
        }
        OpBinary::Exp | OpBinary::ExpElem => {
            Ok(FmiConstValue::Real(lhs.as_real()?.powf(rhs.as_real()?)))
        }
        OpBinary::Lt => Ok(FmiConstValue::Bool(lhs.as_real()? < rhs.as_real()?)),
        OpBinary::Le => Ok(FmiConstValue::Bool(lhs.as_real()? <= rhs.as_real()?)),
        OpBinary::Gt => Ok(FmiConstValue::Bool(lhs.as_real()? > rhs.as_real()?)),
        OpBinary::Ge => Ok(FmiConstValue::Bool(lhs.as_real()? >= rhs.as_real()?)),
        OpBinary::And => Ok(FmiConstValue::Bool(lhs.as_bool()? && rhs.as_bool()?)),
        OpBinary::Or => Ok(FmiConstValue::Bool(lhs.as_bool()? || rhs.as_bool()?)),
        _ => Err(FmiEvalError::Unsupported("binary operator")),
    }
}

fn fmi_values_equal(lhs: &FmiConstValue, rhs: &FmiConstValue) -> FmiEvalResult<bool> {
    match (lhs, rhs) {
        (FmiConstValue::Real(lhs), FmiConstValue::Real(rhs)) => {
            Ok((lhs - rhs).abs() <= 1.0e-12 * (1.0 + lhs.abs().max(rhs.abs())))
        }
        (FmiConstValue::Bool(lhs), FmiConstValue::Bool(rhs)) => Ok(lhs == rhs),
        (FmiConstValue::String(lhs), FmiConstValue::String(rhs)) => Ok(lhs == rhs),
        (FmiConstValue::Array(lhs), FmiConstValue::Array(rhs)) if lhs.len() == rhs.len() => {
            lhs.iter().zip(rhs).try_fold(true, |same, (lhs, rhs)| {
                Ok(same && fmi_values_equal(lhs, rhs)?)
            })
        }
        _ => Err(FmiEvalError::Unsupported("equality for value types")),
    }
}

fn eval_fmi_builtin(
    function: BuiltinFunction,
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    match function {
        BuiltinFunction::NoEvent => eval_arg(args, 0, env),
        BuiltinFunction::Smooth => eval_arg(args, 1, env),
        BuiltinFunction::Homotopy => eval_arg(args, 0, env),
        BuiltinFunction::Delay => Err(FmiEvalError::Unsupported("delay")),
        BuiltinFunction::Integer => scalar_arg(args, 0, env)
            .map(f64::floor)
            .map(FmiConstValue::Real),
        BuiltinFunction::Zeros => {
            let count = fmi_array_count(args, 0, env)?;
            Ok(FmiConstValue::Array(vec![FmiConstValue::Real(0.0); count]))
        }
        BuiltinFunction::Ones => {
            let count = fmi_array_count(args, 0, env)?;
            Ok(FmiConstValue::Array(vec![FmiConstValue::Real(1.0); count]))
        }
        BuiltinFunction::Identity => eval_fmi_identity(args, env),
        BuiltinFunction::Fill => {
            let value = eval_arg(args, 0, env)?;
            let count = fmi_array_count(args, 1, env)?;
            Ok(FmiConstValue::Array(vec![value; count]))
        }
        BuiltinFunction::Linspace => eval_fmi_linspace(args, env),
        BuiltinFunction::Scalar => {
            let value = eval_arg(args, 0, env)?;
            let mut flat = Vec::new();
            flatten_fmi_values_into(std::slice::from_ref(&value), &mut flat);
            match flat.as_slice() {
                [value] => Ok(value.clone()),
                _ => Err(FmiEvalError::ShapeMismatch),
            }
        }
        BuiltinFunction::Vector | BuiltinFunction::Matrix => eval_arg(args, 0, env),
        BuiltinFunction::Sum => {
            let mut values = Vec::new();
            eval_arg(args, 0, env)?.flatten_numeric(&mut values)?;
            Ok(FmiConstValue::Real(values.iter().sum()))
        }
        BuiltinFunction::Product => {
            let mut values = Vec::new();
            eval_arg(args, 0, env)?.flatten_numeric(&mut values)?;
            Ok(FmiConstValue::Real(values.iter().product()))
        }
        _ => eval_fmi_math_builtin(function, args, env),
    }
}

fn eval_fmi_identity(
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let dimension = usize::try_from(eval_arg(args, 0, env)?.as_index()?)
        .map_err(|_| FmiEvalError::InvalidIndex)?;
    let count = dimension
        .checked_mul(dimension)
        .ok_or(FmiEvalError::Overflow)?;
    let mut values = vec![FmiConstValue::Real(0.0); count];
    for index in 0..dimension {
        let offset = index
            .checked_mul(dimension)
            .and_then(|offset| offset.checked_add(index))
            .ok_or(FmiEvalError::Overflow)?;
        values[offset] = FmiConstValue::Real(1.0);
    }
    Ok(FmiConstValue::Array(values))
}

fn eval_arg(
    args: &[Expression],
    index: usize,
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let arg = args
        .get(index)
        .ok_or(FmiEvalError::Unsupported("missing function argument"))?;
    try_eval_fmi_const_expr(arg, env)
}

fn scalar_arg(args: &[Expression], index: usize, env: &FmiMetadataEnv<'_>) -> FmiEvalResult<f64> {
    eval_arg(args, index, env)?.as_real()
}

fn eval_fmi_math_builtin(
    function: BuiltinFunction,
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let lhs = scalar_arg(args, 0, env)?;
    if let Some(value) = apply_scalar_unary_math(function, lhs) {
        return Ok(FmiConstValue::Real(value));
    }
    let rhs = scalar_arg(args, 1, env)?;
    apply_scalar_binary_math(function, lhs, rhs)
        .map(FmiConstValue::Real)
        .ok_or(FmiEvalError::Unsupported("builtin function"))
}

fn eval_fmi_named_function(
    name: &Reference,
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let short_name = name.last_segment();

    if let Ok(values) =
        rumoca_eval_dae::eval::eval_user_function_array_output_pub::<f64>(name, args, env.runtime)
    {
        return match values.as_slice() {
            [value] => Ok(FmiConstValue::Real(*value)),
            _ => Ok(FmiConstValue::Array(
                values.into_iter().map(FmiConstValue::Real).collect(),
            )),
        };
    }

    if short_name == "substring" {
        return eval_fmi_substring(args, env).map(FmiConstValue::String);
    }
    if short_name == "ln" {
        return eval_fmi_builtin(BuiltinFunction::Log, args, env);
    }
    if let Some(function) = BuiltinFunction::from_name(short_name) {
        return eval_fmi_builtin(function, args, env);
    }

    rumoca_eval_dae::eval_function_call_pub::<f64>(name.var_name(), args, env.runtime)
        .map(FmiConstValue::Real)
        .map_err(|err| FmiEvalError::FunctionEvaluation(err.to_string()))
}

fn eval_fmi_substring(args: &[Expression], env: &FmiMetadataEnv<'_>) -> FmiEvalResult<String> {
    let FmiConstValue::String(input) = eval_arg(args, 0, env)? else {
        return Err(FmiEvalError::NonString);
    };
    let start = eval_arg(args, 1, env)?.as_index()? as usize;
    let stop = eval_arg(args, 2, env)?.as_index()? as usize;
    if stop < start {
        return Ok(String::new());
    }
    let skip = start.checked_sub(1).ok_or(FmiEvalError::InvalidIndex)?;
    Ok(input.chars().skip(skip).take(stop - start + 1).collect())
}

fn fmi_array_count(
    args: &[Expression],
    first_dim_arg: usize,
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<usize> {
    if args.len() <= first_dim_arg {
        return Err(FmiEvalError::Unsupported("missing array dimension"));
    }
    args.get(first_dim_arg..)
        .ok_or(FmiEvalError::Unsupported("missing array dimension"))?
        .iter()
        .try_fold(1usize, |count, expr| {
            let dim = usize::try_from(try_eval_fmi_const_expr(expr, env)?.as_index()?)
                .map_err(|_| FmiEvalError::InvalidIndex)?;
            count.checked_mul(dim).ok_or(FmiEvalError::Overflow)
        })
}

fn eval_fmi_linspace(
    args: &[Expression],
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let start = scalar_arg(args, 0, env)?;
    let end = scalar_arg(args, 1, env)?;
    let count = usize::try_from(eval_arg(args, 2, env)?.as_index()?)
        .map_err(|_| FmiEvalError::InvalidIndex)?;
    if count == 0 {
        return Err(FmiEvalError::InvalidIndex);
    }
    let values = if count == 1 {
        vec![FmiConstValue::Real(start)]
    } else {
        let step = (end - start) / ((count - 1) as f64);
        (0..count)
            .map(|index| FmiConstValue::Real(start + step * (index as f64)))
            .collect()
    };
    Ok(FmiConstValue::Array(values))
}

fn eval_fmi_range(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    env: &FmiMetadataEnv<'_>,
) -> FmiEvalResult<FmiConstValue> {
    let start = try_eval_fmi_const_expr(start, env)?.as_real()?;
    let step = match step {
        Some(step) => try_eval_fmi_const_expr(step, env)?.as_real()?,
        None => 1.0,
    };
    let end = try_eval_fmi_const_expr(end, env)?.as_real()?;
    if !start.is_finite() || !step.is_finite() || !end.is_finite() {
        return Err(FmiEvalError::NonFinite);
    }
    if step.abs() <= f64::EPSILON {
        return Err(FmiEvalError::DivisionByZero);
    }
    let mut values = Vec::new();
    let mut current = start;
    let forward = step > 0.0;
    while (forward && current <= end + 1.0e-12) || (!forward && current >= end - 1.0e-12) {
        values.push(FmiConstValue::Real(current));
        if values.len() > 100_000 {
            return Err(FmiEvalError::Unsupported("range exceeds metadata limit"));
        }
        current += step;
    }
    Ok(FmiConstValue::Array(values))
}

struct FmiMetadataBindingCollector<'a> {
    bindings: &'a mut Vec<(FmiValueKey, Expression)>,
    error: Option<ToDaeError>,
}

impl DaeVisitor for FmiMetadataBindingCollector<'_> {
    fn visit_variable(
        &mut self,
        _partition: DaeVariablePartition,
        name: &VarName,
        variable: &Variable,
    ) {
        if self.error.is_some() {
            return;
        }
        if variable.fixed == Some(false) {
            return;
        }
        if let Some(expr) = &variable.start {
            match variable_key(name, variable) {
                Ok(key) => self.bindings.push((key, expr.clone())),
                Err(error) => self.error = Some(error),
            }
        }
    }
}

struct FmiMetadataRewriter<F> {
    rewrite: F,
}

impl<F> DaeVariableMutVisitor for FmiMetadataRewriter<F>
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
