use indexmap::IndexMap;
use rumoca_ir_dae as dae;

use super::size_binding_key;

pub(super) fn structural_bindings(dae_model: &dae::Dae) -> IndexMap<String, f64> {
    let mut bindings = enum_literal_bindings(&dae_model.symbols.enum_literal_ordinals);
    let shapes = variable_shapes(dae_model);
    insert_shape_bindings(&mut bindings, &shapes);
    insert_constant_variables(&mut bindings, dae_model, &shapes);
    insert_structural_parameters(&mut bindings, dae_model, &shapes);
    bindings
}

fn enum_literal_bindings(ordinals: &IndexMap<String, i64>) -> IndexMap<String, f64> {
    let mut bindings = IndexMap::new();
    for (name, ordinal) in ordinals {
        bindings.insert(name.clone(), *ordinal as f64);
        if let Some(alternate) = alternate_enum_literal_key(name) {
            bindings.insert(alternate, *ordinal as f64);
        }
    }
    bindings
}

fn variable_shapes(dae_model: &dae::Dae) -> IndexMap<String, Vec<i64>> {
    let mut shapes = IndexMap::new();
    for (name, var) in dae_model
        .variables
        .parameters
        .iter()
        .chain(dae_model.variables.constants.iter())
        .chain(dae_model.variables.inputs.iter())
        .chain(dae_model.variables.states.iter())
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
    {
        shapes.insert(name.as_str().to_string(), var.dims.clone());
    }
    shapes
}

fn insert_shape_bindings(
    bindings: &mut IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) {
    for (name, dims) in shapes {
        for (idx, dim) in dims.iter().enumerate() {
            bindings.insert(size_binding_key(name, idx + 1), *dim as f64);
        }
    }
}

fn insert_constant_variables(
    bindings: &mut IndexMap<String, f64>,
    dae_model: &dae::Dae,
    shapes: &IndexMap<String, Vec<i64>>,
) {
    for (name, var) in &dae_model.variables.constants {
        insert_variable_start_bindings(bindings, shapes, name.as_str(), var);
    }
}

fn insert_structural_parameters(
    bindings: &mut IndexMap<String, f64>,
    dae_model: &dae::Dae,
    shapes: &IndexMap<String, Vec<i64>>,
) {
    for _ in 0..dae_model.variables.parameters.len().max(1) {
        let before = bindings.len();
        for (name, var) in &dae_model.variables.parameters {
            if !var.is_tunable {
                insert_variable_start_bindings(bindings, shapes, name.as_str(), var);
            }
        }
        if bindings.len() == before {
            break;
        }
    }
}

fn insert_variable_start_bindings(
    bindings: &mut IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    name: &str,
    var: &dae::Variable,
) {
    let Some(start) = var.start.as_ref() else {
        return;
    };
    let Some(raw_values) = eval_values(start, bindings, shapes) else {
        return;
    };
    let values = expand_values_to_size(raw_values, var.size());
    insert_scalarized_bindings(bindings, name, &var.dims, &values);
}

fn insert_scalarized_bindings(
    bindings: &mut IndexMap<String, f64>,
    name: &str,
    dims: &[i64],
    values: &[f64],
) {
    let Some(first) = values.first().copied() else {
        return;
    };
    bindings.insert(name.to_string(), first);
    for (idx, value) in values.iter().copied().enumerate() {
        bindings.insert(dae::scalar_name_text_for_flat_index(name, dims, idx), value);
    }
}

fn eval_values(
    expr: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    match expr {
        rumoca_core::Expression::Literal { value: literal, .. } => {
            Some(vec![literal_to_f64(literal)?])
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let key = var_key(name, subscripts, bindings)?;
            bindings.get(key.as_str()).copied().map(|value| vec![value])
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => eval_unary(op, rhs, bindings, shapes),
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            eval_binary(op, lhs, rhs, bindings, shapes)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            eval_builtin(*function, args, bindings, shapes)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let mut values = Vec::new();
            for element in elements {
                values.extend(eval_values(element, bindings, shapes)?);
            }
            Some(values)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => eval_range(start, step.as_deref(), end, bindings, shapes),
        _ => None,
    }
}

fn eval_scalar(
    expr: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<f64> {
    let values = eval_values(expr, bindings, shapes)?;
    (values.len() == 1).then_some(values[0])
}

fn eval_unary(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    let values = eval_values(rhs, bindings, shapes)?;
    match op {
        rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Some(values),
        rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
            Some(values.into_iter().map(|value| -value).collect())
        }
        rumoca_core::OpUnary::Not | rumoca_core::OpUnary::Empty => None,
    }
}

fn eval_binary(
    op: &rumoca_core::OpBinary,
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    let lhs = eval_scalar(lhs, bindings, shapes)?;
    let rhs = eval_scalar(rhs, bindings, shapes)?;
    let value = match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => lhs + rhs,
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => lhs - rhs,
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => lhs * rhs,
        rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => lhs / rhs,
        rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => lhs.powf(rhs),
        _ => return None,
    };
    Some(vec![value])
}

fn eval_range(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    let start = eval_scalar(start, bindings, shapes)?;
    let end = eval_scalar(end, bindings, shapes)?;
    let step = step
        .map(|expr| eval_scalar(expr, bindings, shapes))
        .unwrap_or_else(|| Some(if end >= start { 1.0 } else { -1.0 }))?;
    if step.abs() <= f64::EPSILON {
        return None;
    }
    let mut values = Vec::new();
    let mut value = start;
    let tol = step.abs() * 1.0e-9 + 1.0e-12;
    for _ in 0..100_000 {
        if (step > 0.0 && value > end + tol) || (step < 0.0 && value < end - tol) {
            break;
        }
        values.push(value);
        value += step;
    }
    Some(values)
}

fn eval_builtin(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    use rumoca_core::BuiltinFunction as Builtin;
    match function {
        // MLS §10.3.1: size(A, i) is a structural property of the array type.
        Builtin::Size => eval_size(args, bindings, shapes),
        Builtin::Min => eval_min_max(args, bindings, shapes, f64::min),
        Builtin::Max => eval_min_max(args, bindings, shapes, f64::max),
        Builtin::NoEvent => eval_values(args.first()?, bindings, shapes),
        Builtin::Smooth => eval_values(args.get(1)?, bindings, shapes),
        Builtin::Homotopy => eval_values(args.first()?, bindings, shapes),
        Builtin::Abs => unary_builtin(args, bindings, shapes, f64::abs),
        Builtin::Sign => unary_builtin(args, bindings, shapes, f64::signum),
        Builtin::Sqrt => unary_builtin(args, bindings, shapes, f64::sqrt),
        Builtin::Floor | Builtin::Integer => unary_builtin(args, bindings, shapes, f64::floor),
        Builtin::Ceil => unary_builtin(args, bindings, shapes, f64::ceil),
        _ => None,
    }
}

fn eval_size(
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = args.first()?
    else {
        return Some(vec![
            eval_values(args.first()?, bindings, shapes)?.len() as f64
        ]);
    };
    if !subscripts.is_empty() {
        return Some(vec![1.0]);
    }
    let dims = shapes.get(name.as_str())?;
    let Some(dim_expr) = args.get(1) else {
        return Some(dims.iter().map(|dim| *dim as f64).collect());
    };
    let dim = eval_scalar(dim_expr, bindings, shapes)? as usize;
    dims.get(dim.checked_sub(1)?)
        .map(|value| vec![*value as f64])
}

fn eval_min_max(
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    op: fn(f64, f64) -> f64,
) -> Option<Vec<f64>> {
    let mut values = Vec::new();
    for arg in args {
        values.extend(eval_values(arg, bindings, shapes)?);
    }
    let first = *values.first()?;
    Some(vec![values.into_iter().fold(first, op)])
}

fn unary_builtin(
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    op: fn(f64) -> f64,
) -> Option<Vec<f64>> {
    Some(vec![op(eval_scalar(args.first()?, bindings, shapes)?)])
}

fn var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    bindings: &IndexMap<String, f64>,
) -> Option<String> {
    let name = name.as_str();
    if subscripts.is_empty() {
        return Some(name.to_string());
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let index = match subscript {
            rumoca_core::Subscript::Index { value: index, .. } => *index as usize,
            rumoca_core::Subscript::Expr { expr, .. } => {
                eval_scalar(expr, bindings, &IndexMap::new())? as usize
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        };
        indices.push(index);
    }
    Some(dae::format_subscript_key(name, &indices))
}

fn literal_to_f64(literal: &rumoca_core::Literal) -> Option<f64> {
    match literal {
        rumoca_core::Literal::Real(value) => Some(*value),
        rumoca_core::Literal::Integer(value) => Some(*value as f64),
        rumoca_core::Literal::Boolean(value) => Some(if *value { 1.0 } else { 0.0 }),
        rumoca_core::Literal::String(_) => None,
    }
}

fn alternate_enum_literal_key(raw: &str) -> Option<String> {
    let (prefix, literal) = rumoca_core::split_last_top_level(raw)?;
    if literal.len() >= 2 && literal.starts_with('\'') && literal.ends_with('\'') {
        return Some(format!("{prefix}.{}", &literal[1..literal.len() - 1]));
    }
    Some(format!("{prefix}.'{literal}'"))
}

fn expand_values_to_size(raw_values: Vec<f64>, size: usize) -> Vec<f64> {
    if raw_values.len() == size {
        return raw_values;
    }
    if raw_values.len() == 1 {
        return vec![raw_values[0]; size];
    }
    raw_values
}
