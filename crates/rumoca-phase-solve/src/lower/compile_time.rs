use indexmap::IndexMap;
use rumoca_ir_dae as dae;

use super::{LowerError, helpers::variable_size, size_binding_key};

pub(super) fn structural_bindings(
    dae_model: &dae::Dae,
) -> Result<IndexMap<String, f64>, LowerError> {
    let mut bindings = enum_literal_bindings(&dae_model.symbols.enum_literal_ordinals);
    let shapes = variable_shapes(dae_model);
    insert_shape_bindings(&mut bindings, &shapes);
    insert_constant_variables(&mut bindings, dae_model, &shapes)?;
    insert_structural_parameters(&mut bindings, dae_model, &shapes)?;
    Ok(bindings)
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
) -> Result<(), LowerError> {
    for (name, var) in &dae_model.variables.constants {
        insert_variable_start_bindings(bindings, shapes, name.as_str(), var)?;
    }
    Ok(())
}

fn insert_structural_parameters(
    bindings: &mut IndexMap<String, f64>,
    dae_model: &dae::Dae,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Result<(), LowerError> {
    for _ in 0..dae_model.variables.parameters.len().max(1) {
        let before = bindings.len();
        for (name, var) in &dae_model.variables.parameters {
            if !var.is_tunable {
                insert_variable_start_bindings(bindings, shapes, name.as_str(), var)?;
            }
        }
        if bindings.len() == before {
            break;
        }
    }
    Ok(())
}

fn insert_variable_start_bindings(
    bindings: &mut IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    name: &str,
    var: &dae::Variable,
) -> Result<(), LowerError> {
    let Some(start) = var.start.as_ref() else {
        return Ok(());
    };
    let Some(raw_values) = eval_values(start, bindings, shapes) else {
        return Ok(());
    };
    let values = expand_values_to_size(raw_values, variable_size(var)?, name, var.source_span)?;
    insert_scalarized_bindings(bindings, name, &var.dims, &values);
    Ok(())
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
        } => eval_var_ref_values(name, subscripts, bindings, shapes),
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

fn eval_var_ref_values(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
) -> Option<Vec<f64>> {
    if !subscripts.is_empty() {
        let key = var_key(name, subscripts, bindings)?;
        return bindings.get(key.as_str()).copied().map(|value| vec![value]);
    }
    let name = name.as_str();
    let Some(dims) = shapes.get(name).filter(|dims| !dims.is_empty()) else {
        return bindings.get(name).copied().map(|value| vec![value]);
    };
    let size = dims.iter().try_fold(1usize, |size, dim| {
        size.checked_mul(usize::try_from(*dim).ok()?)
    })?;
    (0..size)
        .map(|index| {
            let key = dae::scalar_name_text_for_flat_index(name, dims, index);
            bindings.get(key.as_str()).copied()
        })
        .collect()
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
    if !start.is_finite() || !end.is_finite() || !step.is_finite() || step.abs() <= f64::EPSILON {
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
        if !value.is_finite() {
            return None;
        }
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
    let dim = positive_usize_from_f64(eval_scalar(dim_expr, bindings, shapes)?)?;
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
            rumoca_core::Subscript::Index { value: index, .. } => positive_i64_to_usize(*index)?,
            rumoca_core::Subscript::Expr { expr, .. } => {
                positive_usize_from_f64(eval_scalar(expr, bindings, &IndexMap::new())?)?
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

fn positive_i64_to_usize(value: i64) -> Option<usize> {
    if value > 0 {
        usize::try_from(value).ok()
    } else {
        None
    }
}

fn positive_usize_from_f64(value: f64) -> Option<usize> {
    let rounded = value.round();
    if rounded.is_finite()
        && rounded > 0.0
        && (rounded - value).abs() < f64::EPSILON
        && rounded < usize::MAX as f64
    {
        // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
        return Some(rounded as usize);
    }
    None
}

fn alternate_enum_literal_key(raw: &str) -> Option<String> {
    let (prefix, literal) = crate::path_utils::scope_split(raw)?;
    if literal.len() >= 2 && literal.starts_with('\'') && literal.ends_with('\'') {
        return Some(format!("{prefix}.{}", &literal[1..literal.len() - 1]));
    }
    Some(format!("{prefix}.'{literal}'"))
}

fn expand_values_to_size(
    raw_values: Vec<f64>,
    size: usize,
    name: &str,
    span: rumoca_core::Span,
) -> Result<Vec<f64>, LowerError> {
    if raw_values.len() == size {
        return Ok(raw_values);
    }
    if raw_values.len() == 1 {
        let mut values = Vec::new();
        reserve_compile_time_start_capacity(&mut values, size, name, span)?;
        values.resize(size, raw_values[0]);
        return Ok(values);
    }
    Ok(raw_values)
}

fn reserve_compile_time_start_capacity(
    values: &mut Vec<f64>,
    capacity: usize,
    name: &str,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        LowerError::contract_violation(
            format!("compile-time start value capacity for `{name}` overflows"),
            span,
        )
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn compile_time_test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_compile_time_fixture.mo"),
            1,
            2,
        )
    }

    fn unspanned_compile_time_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: compile_time_test_span(),
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: compile_time_test_span(),
        }
    }

    #[test]
    fn structural_bindings_report_invalid_variable_shape_span() {
        let mut dae_model = dae::Dae::default();
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_compile_time_source_45.mo"),
            7,
            19,
        );
        dae_model.variables.constants.insert(
            rumoca_core::VarName::new("bad"),
            dae::Variable {
                name: rumoca_core::VarName::new("bad"),
                dims: vec![2, -1],
                source_span: span,
                start: Some(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span,
                }),
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let err = structural_bindings(&dae_model)
            .expect_err("invalid DAE variable shape should bubble from structural bindings");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(
            err.reason()
                .contains("DAE variable `bad` has negative dimension -1"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn expand_values_to_size_reports_capacity_overflow_with_source_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_compile_time_source_46.mo"),
            5,
            12,
        );
        let err = expand_values_to_size(vec![1.0], usize::MAX, "huge_structural", span)
            .expect_err("oversized structural start broadcast should fail before allocating");

        assert_eq!(err.source_span(), Some(span));
        assert!(matches!(err, LowerError::ContractViolation { .. }));
        assert!(
            err.reason()
                .contains("compile-time start value capacity for `huge_structural` overflows"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn expand_values_to_size_reports_capacity_overflow_without_fabricating_span() {
        let err = expand_values_to_size(
            vec![1.0],
            usize::MAX,
            "huge_structural",
            unspanned_compile_time_test_span(),
        )
        .expect_err("oversized structural start broadcast should fail before allocating");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(
            err.reason()
                .contains("compile-time start value capacity for `huge_structural` overflows"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn expand_values_to_size_preserves_scalar_broadcast_behavior() {
        let span = compile_time_test_span();
        assert_eq!(
            expand_values_to_size(vec![3.0], 4, "x", span)
                .expect("scalar structural start should broadcast"),
            vec![3.0, 3.0, 3.0, 3.0]
        );
        assert_eq!(
            expand_values_to_size(vec![1.0, 2.0], 4, "x", span)
                .expect("non-scalar structural starts are left unchanged"),
            vec![1.0, 2.0]
        );
    }

    #[test]
    fn structural_array_alias_preserves_each_source_component() {
        let span = compile_time_test_span();
        let mut dae_model = dae::Dae::default();
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("alias"),
            dae::Variable {
                name: rumoca_core::VarName::new("alias"),
                dims: vec![3],
                start: Some(var_ref("source")),
                is_tunable: false,
                ..dae::Variable::empty_with_span(span)
            },
        );
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("source"),
            dae::Variable {
                name: rumoca_core::VarName::new("source"),
                dims: vec![3],
                start: Some(rumoca_core::Expression::Array {
                    elements: vec![real(1.0), real(2.0), real(3.0)],
                    is_matrix: false,
                    span,
                }),
                is_tunable: false,
                ..dae::Variable::empty_with_span(span)
            },
        );

        let bindings = structural_bindings(&dae_model).expect("structural aliases should resolve");

        assert_eq!(bindings.get("alias[1]"), Some(&1.0));
        assert_eq!(bindings.get("alias[2]"), Some(&2.0));
        assert_eq!(bindings.get("alias[3]"), Some(&3.0));
    }

    #[test]
    fn eval_size_declines_unrepresentable_dimension_index() {
        let args = vec![var_ref("x"), real(usize::MAX as f64)];
        let shapes = IndexMap::from([("x".to_string(), vec![2, 3])]);

        assert_eq!(eval_size(&args, &IndexMap::new(), &shapes), None);
    }

    #[test]
    fn var_key_declines_unrepresentable_expression_subscript() {
        let subscript = rumoca_core::Subscript::expr(
            Box::new(real(usize::MAX as f64)),
            compile_time_test_span(),
        );

        assert_eq!(
            var_key(
                &rumoca_core::Reference::new("x"),
                &[subscript],
                &IndexMap::new()
            ),
            None
        );
    }
}
