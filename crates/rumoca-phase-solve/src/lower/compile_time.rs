use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use std::sync::Arc;

use super::{LowerError, helpers::variable_size, size_binding_key};

pub(super) fn structural_bindings(
    dae_model: &dae::Dae,
) -> Result<IndexMap<String, f64>, LowerError> {
    structural_bindings_with_eval_env(dae_model).map(|(bindings, _)| bindings)
}

pub(super) fn external_table_data(
    dae_model: &dae::Dae,
) -> Result<Vec<rumoca_core::ExternalTableData>, LowerError> {
    let (_, eval_env) = structural_bindings_with_eval_env(dae_model)?;
    Ok(rumoca_eval_dae::all_external_table_data_in_env(&eval_env))
}

pub(in crate::lower) fn eval_selected_function_output(
    dae_model: &dae::Dae,
    function_name: &rumoca_core::VarName,
    output_name: &str,
    indices: &[i64],
    args: &[rumoca_core::Expression],
) -> Option<f64> {
    let env = compile_time_eval_env(dae_model);
    rumoca_eval_dae::eval_selected_function_output_pub::<f64>(
        function_name,
        output_name,
        indices,
        args,
        &env,
    )
    .ok()
}

fn structural_bindings_with_eval_env(
    dae_model: &dae::Dae,
) -> Result<(IndexMap<String, f64>, rumoca_eval_dae::VarEnv<f64>), LowerError> {
    let mut eval_env = compile_time_eval_env(dae_model);
    let mut bindings = enum_literal_bindings(&dae_model.symbols.enum_literal_ordinals);
    let shapes = variable_shapes(dae_model);
    insert_shape_bindings(&mut bindings, &shapes);
    insert_constant_variables(&mut bindings, dae_model, &shapes, &mut eval_env)?;
    seed_compile_time_start_values(dae_model, &mut eval_env);
    insert_structural_parameters(&mut bindings, dae_model, &shapes, &mut eval_env)?;
    insert_static_initial_assignments(&mut bindings, dae_model);
    insert_external_table_handle_bindings(&mut bindings, dae_model, &shapes, &mut eval_env)?;
    Ok((bindings, eval_env))
}

fn compile_time_eval_env(dae_model: &dae::Dae) -> rumoca_eval_dae::VarEnv<f64> {
    let mut env = rumoca_eval_dae::VarEnv::new();
    env.runtime = Arc::new(rumoca_eval_dae::EvalRuntimeState::new());
    env.functions = Arc::new(rumoca_eval_dae::collect_user_functions(dae_model));
    env.dims = Arc::new(rumoca_eval_dae::collect_var_dims(dae_model));
    env.start_exprs = Arc::new(rumoca_eval_dae::collect_var_starts(dae_model));
    env.nonnumeric_names = Arc::new(
        dae_model
            .metadata
            .nonnumeric_variable_names
            .iter()
            .cloned()
            .collect(),
    );
    env.clock_intervals = Arc::new(dae_model.clocks.intervals.clone());
    env.enum_literal_ordinals = Arc::new(dae_model.symbols.enum_literal_ordinals.clone());
    for &(name, value) in rumoca_eval_dae::MODELICA_CONSTANTS {
        env.set(name, value);
    }
    for &(name, value) in rumoca_eval_dae::MODELICA_COMPLEX_CONSTANTS {
        env.set(name, value);
    }
    env
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
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Result<(), LowerError> {
    for (name, var) in &dae_model.variables.constants {
        insert_variable_start_bindings(bindings, shapes, eval_env, name.as_str(), var)?;
    }
    Ok(())
}

fn insert_structural_parameters(
    bindings: &mut IndexMap<String, f64>,
    dae_model: &dae::Dae,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Result<(), LowerError> {
    for _ in 0..dae_model.variables.parameters.len().max(1) {
        let before = bindings.len();
        for (name, var) in &dae_model.variables.parameters {
            if !var.is_tunable {
                insert_variable_start_bindings(bindings, shapes, eval_env, name.as_str(), var)?;
            }
        }
        if bindings.len() == before {
            break;
        }
    }
    Ok(())
}

fn insert_external_table_handle_bindings(
    bindings: &mut IndexMap<String, f64>,
    dae_model: &dae::Dae,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Result<(), LowerError> {
    seed_compile_time_start_values(dae_model, eval_env);
    for table_id_name in &dae_model.metadata.nonnumeric_variable_names {
        if !table_id_name.ends_with(".tableID") {
            continue;
        }
        let Some(prefix) = table_id_name.strip_suffix(".tableID") else {
            continue;
        };
        let constructor = external_table_record_constructor(prefix, &dae_model.variables)
            .or_else(|| {
                external_table_constructor_for_prefix(prefix, dae_model.variables.constants.iter())
                    .cloned()
            })
            .or_else(|| {
                external_table_constructor_for_prefix(prefix, dae_model.variables.parameters.iter())
                    .cloned()
            });
        let Some(constructor) = constructor else {
            continue;
        };
        let Some(values) = eval_values(&constructor, bindings, shapes, eval_env) else {
            continue;
        };
        let Some(table_id) = values.first().copied() else {
            continue;
        };
        bindings.insert(table_id_name.clone(), table_id);
        eval_env.set(table_id_name, table_id);
    }
    Ok(())
}

fn insert_static_initial_assignments(bindings: &mut IndexMap<String, f64>, dae_model: &dae::Dae) {
    for _ in 0..dae_model.initialization.equations.len().max(1) {
        let before = bindings.len();
        for equation in &dae_model.initialization.equations {
            let Some(lhs) = equation.lhs.as_ref() else {
                continue;
            };
            if !initial_assignment_target_is_structural(lhs.as_str(), &dae_model.variables) {
                continue;
            }
            let Ok(value) = eval_static_initial_numeric(&equation.rhs, bindings) else {
                continue;
            };
            bindings.insert(lhs.as_str().to_string(), value);
        }
        if bindings.len() == before {
            break;
        }
    }
}

fn initial_assignment_target_is_structural(name: &str, variables: &dae::DaeVariables) -> bool {
    let var_name = rumoca_core::VarName::new(name);
    [
        variables.parameters.get(&var_name),
        variables.constants.get(&var_name),
        variables.discrete_valued.get(&var_name),
        variables.discrete_reals.get(&var_name),
    ]
    .into_iter()
    .flatten()
    .any(|variable| !variable.is_tunable)
}

fn eval_static_initial_numeric(
    expr: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
) -> Result<f64, ()> {
    match expr {
        rumoca_core::Expression::Literal { value, .. } => match value {
            rumoca_core::Literal::Real(value) => Ok(*value),
            rumoca_core::Literal::Integer(value) => Ok(*value as f64),
            rumoca_core::Literal::Boolean(value) => Ok(if *value { 1.0 } else { 0.0 }),
            rumoca_core::Literal::String(_) => Err(()),
        },
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => bindings.get(name.as_str()).copied().ok_or(()),
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let value = eval_static_initial_numeric(rhs, bindings)?;
            match op {
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Ok(-value),
                rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus
                | rumoca_core::OpUnary::Empty => Ok(value),
                rumoca_core::OpUnary::Not => Ok(if value == 0.0 { 1.0 } else { 0.0 }),
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_static_initial_numeric(lhs, bindings)?;
            let rhs = eval_static_initial_numeric(rhs, bindings)?;
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Ok(lhs + rhs),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Ok(lhs - rhs),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Ok(lhs * rhs),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => Ok(lhs / rhs),
                rumoca_core::OpBinary::Eq => Ok(if (lhs - rhs).abs() < f64::EPSILON {
                    1.0
                } else {
                    0.0
                }),
                rumoca_core::OpBinary::Neq => Ok(if (lhs - rhs).abs() >= f64::EPSILON {
                    1.0
                } else {
                    0.0
                }),
                rumoca_core::OpBinary::Lt => Ok(if lhs < rhs { 1.0 } else { 0.0 }),
                rumoca_core::OpBinary::Le => Ok(if lhs <= rhs { 1.0 } else { 0.0 }),
                rumoca_core::OpBinary::Gt => Ok(if lhs > rhs { 1.0 } else { 0.0 }),
                rumoca_core::OpBinary::Ge => Ok(if lhs >= rhs { 1.0 } else { 0.0 }),
                rumoca_core::OpBinary::And => Ok(if lhs != 0.0 && rhs != 0.0 { 1.0 } else { 0.0 }),
                rumoca_core::OpBinary::Or => Ok(if lhs != 0.0 || rhs != 0.0 { 1.0 } else { 0.0 }),
                _ => Err(()),
            }
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                if eval_static_initial_numeric(condition, bindings)? != 0.0 {
                    return eval_static_initial_numeric(value, bindings);
                }
            }
            eval_static_initial_numeric(else_branch, bindings)
        }
        rumoca_core::Expression::FunctionCall { name, args, .. }
            if matches!(
                name.as_str(),
                "Modelica.Utilities.Strings.isEqual" | "Strings.isEqual" | "isEqual"
            ) =>
        {
            let left = args.first().ok_or(())?;
            let right = args.get(1).ok_or(())?;
            Ok(
                if eval_static_initial_string(left, bindings)?
                    == eval_static_initial_string(right, bindings)?
                {
                    1.0
                } else {
                    0.0
                },
            )
        }
        _ => Err(()),
    }
}

fn eval_static_initial_string(
    expr: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
) -> Result<String, ()> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value),
            ..
        } => Ok(value.clone()),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => {
            let [subscript] = subscripts.as_slice() else {
                return Err(());
            };
            let index = match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value > 0 => *value as usize,
                rumoca_core::Subscript::Expr { expr, .. } => {
                    let value = eval_static_initial_numeric(expr, bindings)?;
                    if value.fract().abs() > f64::EPSILON || value <= 0.0 {
                        return Err(());
                    }
                    value as usize
                }
                _ => return Err(()),
            };
            let rumoca_core::Expression::Array { elements, .. } = base.as_ref() else {
                return Err(());
            };
            eval_static_initial_string(elements.get(index - 1).ok_or(())?, bindings)
        }
        _ => Err(()),
    }
}

fn seed_compile_time_start_values(
    dae_model: &dae::Dae,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) {
    for _ in 0..dae_model.variables.parameters.len().max(1).clamp(1, 8) {
        let before = eval_env.vars.len();
        for (name, var) in dae_model
            .variables
            .constants
            .iter()
            .chain(dae_model.variables.parameters.iter())
        {
            seed_compile_time_start_value(name.as_str(), var, eval_env);
        }
        if eval_env.vars.len() == before {
            break;
        }
    }
}

fn seed_compile_time_start_value(
    name: &str,
    var: &dae::Variable,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) {
    let Some(start) = var.start.as_ref() else {
        return;
    };
    if rumoca_eval_dae::start_expr_is_nonnumeric(start, eval_env) {
        return;
    }
    if !var.dims.is_empty() {
        let values = if var.size() == 0 && var.dims.len() >= 2 {
            rumoca_eval_dae::eval_matrix_values::<f64>(start, eval_env)
                .ok()
                .flatten()
                .map(|matrix| matrix.into_iter().flatten().collect())
                .or_else(|| rumoca_eval_dae::eval_array_values::<f64>(start, eval_env).ok())
        } else {
            rumoca_eval_dae::eval_array_values::<f64>(start, eval_env).ok()
        };
        if let Some(values) = values {
            rumoca_eval_dae::set_array_entries(eval_env, name, &var.dims, &values);
        }
        return;
    }
    if let Ok(value) = rumoca_eval_dae::eval_expr::<f64>(start, eval_env) {
        eval_env.set(name, value);
    }
}

fn external_table_record_constructor(
    prefix: &str,
    variables: &dae::DaeVariables,
) -> Option<rumoca_core::Expression> {
    external_table_record_constructor_from_fields(
        "Modelica.Blocks.Types.ExternalCombiTimeTable",
        prefix,
        variables,
        &[
            "tableName",
            "fileName",
            "table",
            "startTime",
            "columns",
            "smoothness",
            "extrapolation",
            "shiftTime",
            "timeEvents",
            "verboseRead",
            "delimiter",
            "nHeaderLines",
        ],
    )
    .or_else(|| {
        external_table_record_constructor_from_fields(
            "Modelica.Blocks.Types.ExternalCombiTable1D",
            prefix,
            variables,
            &[
                "tableName",
                "fileName",
                "table",
                "columns",
                "smoothness",
                "extrapolation",
                "verboseRead",
                "delimiter",
                "nHeaderLines",
            ],
        )
    })
}

fn external_table_record_constructor_from_fields(
    constructor_name: &str,
    prefix: &str,
    variables: &dae::DaeVariables,
    fields: &[&str],
) -> Option<rumoca_core::Expression> {
    let args = fields
        .iter()
        .map(|field| external_table_record_field_ref(prefix, field, variables))
        .collect::<Option<Vec<_>>>()?;
    let span = args.first().and_then(rumoca_core::Expression::span)?;
    Some(rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::new(constructor_name),
        args,
        is_constructor: true,
        span,
    })
}

fn external_table_record_field_ref(
    prefix: &str,
    field: &str,
    variables: &dae::DaeVariables,
) -> Option<rumoca_core::Expression> {
    let name = format!("{prefix}.{field}");
    let var_name = rumoca_core::VarName::new(name.as_str());
    let span = variables
        .parameters
        .get(&var_name)
        .or_else(|| variables.constants.get(&var_name))
        .map(|var| var.source_span)?;
    Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(name.as_str()),
        subscripts: Vec::new(),
        span,
    })
}

fn external_table_constructor_for_prefix<'a>(
    prefix: &str,
    variables: impl Iterator<Item = (&'a rumoca_core::VarName, &'a dae::Variable)>,
) -> Option<&'a rumoca_core::Expression> {
    let prefix = format!("{prefix}.");
    variables
        .filter(|(name, var)| !var.is_tunable && name.as_str().starts_with(prefix.as_str()))
        .filter_map(|(_, var)| var.start.as_ref())
        .find_map(find_external_table_constructor)
}

fn find_external_table_constructor(
    expr: &rumoca_core::Expression,
) -> Option<&rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            if is_external_table_constructor(name) {
                return Some(expr);
            }
            args.iter().find_map(find_external_table_constructor)
        }
        rumoca_core::Expression::Unary { rhs, .. } => find_external_table_constructor(rhs),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            find_external_table_constructor(lhs).or_else(|| find_external_table_constructor(rhs))
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            args.iter().find_map(find_external_table_constructor)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .find_map(|(condition, value)| {
                find_external_table_constructor(condition)
                    .or_else(|| find_external_table_constructor(value))
            })
            .or_else(|| find_external_table_constructor(else_branch)),
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => find_external_table_constructor(base).or_else(|| {
            subscripts.iter().find_map(|subscript| match subscript {
                rumoca_core::Subscript::Expr { expr, .. } => find_external_table_constructor(expr),
                rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => None,
            })
        }),
        rumoca_core::Expression::FieldAccess { base, .. } => find_external_table_constructor(base),
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => find_external_table_constructor(start)
            .or_else(|| step.as_deref().and_then(find_external_table_constructor))
            .or_else(|| find_external_table_constructor(end)),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => find_external_table_constructor(expr)
            .or_else(|| {
                indices
                    .iter()
                    .find_map(|index| find_external_table_constructor(&index.range))
            })
            .or_else(|| filter.as_deref().and_then(find_external_table_constructor)),
        rumoca_core::Expression::Literal { .. }
        | rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Empty { .. } => None,
    }
}

fn is_external_table_constructor(name: &rumoca_core::Reference) -> bool {
    matches!(
        name.last_segment(),
        "ExternalCombiTimeTable" | "ExternalCombiTable1D"
    )
}

fn insert_variable_start_bindings(
    bindings: &mut IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
    name: &str,
    var: &dae::Variable,
) -> Result<(), LowerError> {
    let Some(start) = var.start.as_ref() else {
        return Ok(());
    };
    let Some(raw_values) = eval_values(start, bindings, shapes, eval_env) else {
        return Ok(());
    };
    let values = expand_values_to_size(raw_values, variable_size(var)?, name, var.source_span)?;
    insert_scalarized_bindings(bindings, name, &var.dims, &values);
    sync_bindings_to_eval_env(eval_env, bindings);
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
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    match expr {
        rumoca_core::Expression::Literal { value: literal, .. } => {
            Some(vec![literal_to_f64(literal)?])
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let key = var_key(name, subscripts, bindings, eval_env)?;
            bindings.get(key.as_str()).copied().map(|value| vec![value])
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            eval_unary(op, rhs, bindings, shapes, eval_env)
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            eval_binary(op, lhs, rhs, bindings, shapes, eval_env)
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            eval_builtin(*function, args, bindings, shapes, eval_env)
        }
        rumoca_core::Expression::FunctionCall { name, .. } if is_external_table_function(name) => {
            eval_external_table_function(expr, bindings, eval_env)
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let mut values = Vec::new();
            for element in elements {
                values.extend(eval_values(element, bindings, shapes, eval_env)?);
            }
            Some(values)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => eval_range(start, step.as_deref(), end, bindings, shapes, eval_env),
        _ => None,
    }
}

fn is_external_table_function(name: &rumoca_core::Reference) -> bool {
    matches!(
        name.last_segment(),
        "ExternalCombiTimeTable"
            | "ExternalCombiTable1D"
            | "getTimeTableTmin"
            | "getTimeTableTmax"
            | "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getNextTimeEvent"
            | "getTable1DAbscissaUmin"
            | "getTable1DAbscissaUmax"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue"
    )
}

fn eval_external_table_function(
    expr: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    sync_bindings_to_eval_env(eval_env, bindings);
    rumoca_eval_dae::eval_expr::<f64>(expr, eval_env)
        .ok()
        .map(|value| vec![value])
}

fn sync_bindings_to_eval_env(
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
    bindings: &IndexMap<String, f64>,
) {
    for (name, value) in bindings {
        eval_env.set(name, *value);
    }
}

fn eval_scalar(
    expr: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<f64> {
    let values = eval_values(expr, bindings, shapes, eval_env)?;
    (values.len() == 1).then_some(values[0])
}

fn eval_unary(
    op: &rumoca_core::OpUnary,
    rhs: &rumoca_core::Expression,
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    let values = eval_values(rhs, bindings, shapes, eval_env)?;
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
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    let lhs = eval_scalar(lhs, bindings, shapes, eval_env)?;
    let rhs = eval_scalar(rhs, bindings, shapes, eval_env)?;
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
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    let start = eval_scalar(start, bindings, shapes, eval_env)?;
    let end = eval_scalar(end, bindings, shapes, eval_env)?;
    let step = step
        .map(|expr| eval_scalar(expr, bindings, shapes, eval_env))
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
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    use rumoca_core::BuiltinFunction as Builtin;
    match function {
        // MLS §10.3.1: size(A, i) is a structural property of the array type.
        Builtin::Size => eval_size(args, bindings, shapes, eval_env),
        Builtin::Min => eval_min_max(args, bindings, shapes, eval_env, f64::min),
        Builtin::Max => eval_min_max(args, bindings, shapes, eval_env, f64::max),
        Builtin::NoEvent => eval_values(args.first()?, bindings, shapes, eval_env),
        Builtin::Smooth => eval_values(args.get(1)?, bindings, shapes, eval_env),
        Builtin::Homotopy => eval_values(args.first()?, bindings, shapes, eval_env),
        Builtin::Abs => unary_builtin(args, bindings, shapes, eval_env, f64::abs),
        Builtin::Sign => unary_builtin(args, bindings, shapes, eval_env, f64::signum),
        Builtin::Sqrt => unary_builtin(args, bindings, shapes, eval_env, f64::sqrt),
        Builtin::Floor | Builtin::Integer => {
            unary_builtin(args, bindings, shapes, eval_env, f64::floor)
        }
        Builtin::Ceil => unary_builtin(args, bindings, shapes, eval_env, f64::ceil),
        _ => None,
    }
}

fn eval_size(
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
) -> Option<Vec<f64>> {
    if let Some(dims) = literal_array_shape(args.first()?) {
        let Some(dim_expr) = args.get(1) else {
            return Some(dims.into_iter().map(|dim| dim as f64).collect());
        };
        let dim = positive_usize_from_f64(eval_scalar(dim_expr, bindings, shapes, eval_env)?)?;
        return dims
            .get(dim.checked_sub(1)?)
            .map(|value| vec![*value as f64]);
    }
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = args.first()?
    else {
        return Some(vec![
            eval_values(args.first()?, bindings, shapes, eval_env)?.len() as f64,
        ]);
    };
    if !subscripts.is_empty() {
        return Some(vec![1.0]);
    }
    let dims = shapes.get(name.as_str())?;
    let Some(dim_expr) = args.get(1) else {
        return Some(dims.iter().map(|dim| *dim as f64).collect());
    };
    let dim = positive_usize_from_f64(eval_scalar(dim_expr, bindings, shapes, eval_env)?)?;
    dims.get(dim.checked_sub(1)?)
        .map(|value| vec![*value as f64])
}

fn literal_array_shape(expr: &rumoca_core::Expression) -> Option<Vec<usize>> {
    match expr {
        rumoca_core::Expression::Array {
            elements,
            is_matrix: false,
            ..
        }
        | rumoca_core::Expression::Tuple { elements, .. } => Some(vec![elements.len()]),
        rumoca_core::Expression::Array {
            elements,
            is_matrix: true,
            ..
        } => {
            let first_row = elements.first()?;
            let rumoca_core::Expression::Array {
                elements: row_elements,
                ..
            } = first_row
            else {
                return Some(vec![elements.len()]);
            };
            Some(vec![elements.len(), row_elements.len()])
        }
        _ => None,
    }
}

fn eval_min_max(
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
    op: fn(f64, f64) -> f64,
) -> Option<Vec<f64>> {
    let mut values = Vec::new();
    for arg in args {
        values.extend(eval_values(arg, bindings, shapes, eval_env)?);
    }
    let first = *values.first()?;
    Some(vec![values.into_iter().fold(first, op)])
}

fn unary_builtin(
    args: &[rumoca_core::Expression],
    bindings: &IndexMap<String, f64>,
    shapes: &IndexMap<String, Vec<i64>>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
    op: fn(f64) -> f64,
) -> Option<Vec<f64>> {
    Some(vec![op(eval_scalar(
        args.first()?,
        bindings,
        shapes,
        eval_env,
    )?)])
}

fn var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    bindings: &IndexMap<String, f64>,
    eval_env: &mut rumoca_eval_dae::VarEnv<f64>,
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
                positive_usize_from_f64(eval_scalar(expr, bindings, &IndexMap::new(), eval_env)?)?
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

    fn integer(value: i64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            span: compile_time_test_span(),
        }
    }

    fn string(value: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String(value.to_string()),
            span: compile_time_test_span(),
        }
    }

    fn boolean(value: bool) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(value),
            span: compile_time_test_span(),
        }
    }

    fn array(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
        rumoca_core::Expression::Array {
            elements,
            is_matrix: false,
            span: compile_time_test_span(),
        }
    }

    fn matrix(elements: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
        rumoca_core::Expression::Array {
            elements,
            is_matrix: true,
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

    fn indexed_var(name: &str, index: usize) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                index as i64,
                compile_time_test_span(),
            )],
            span: compile_time_test_span(),
        }
    }

    fn binary(
        op: rumoca_core::OpBinary,
        lhs: rumoca_core::Expression,
        rhs: rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: compile_time_test_span(),
        }
    }

    fn builtin_call(
        function: rumoca_core::BuiltinFunction,
        args: Vec<rumoca_core::Expression>,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function,
            args,
            span: compile_time_test_span(),
        }
    }

    fn range(
        start: rumoca_core::Expression,
        end: rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::Range {
            start: Box::new(start),
            step: None,
            end: Box::new(end),
            span: compile_time_test_span(),
        }
    }

    fn comprehension(
        index: &str,
        values: rumoca_core::Expression,
        expr: rumoca_core::Expression,
    ) -> rumoca_core::Expression {
        rumoca_core::Expression::ArrayComprehension {
            expr: Box::new(expr),
            indices: vec![rumoca_core::ComprehensionIndex {
                name: index.to_string(),
                range: values,
            }],
            filter: None,
            span: compile_time_test_span(),
        }
    }

    fn insert_parameter_start(
        dae_model: &mut dae::Dae,
        name: &str,
        start: rumoca_core::Expression,
        is_tunable: bool,
        dims: &[i64],
    ) {
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                name: rumoca_core::VarName::new(name),
                dims: dims.to_vec(),
                start: Some(start),
                is_tunable,
                ..dae::Variable::empty_with_span(compile_time_test_span())
            },
        );
    }

    fn insert_external_time_table_record_fields(
        dae_model: &mut dae::Dae,
        prefix: &str,
        table: rumoca_core::Expression,
        table_dims: &[i64],
    ) {
        let field_specs = [
            ("tableName", string("NoName"), false, &[][..]),
            ("fileName", string("NoName"), false, &[][..]),
            ("table", table, true, table_dims),
            ("startTime", real(0.0), true, &[][..]),
            ("columns", array(vec![integer(2)]), false, &[1][..]),
            (
                "smoothness",
                var_ref("Modelica.Blocks.Types.Smoothness.ConstantSegments"),
                false,
                &[][..],
            ),
            (
                "extrapolation",
                var_ref("Modelica.Blocks.Types.Extrapolation.HoldLastPoint"),
                true,
                &[][..],
            ),
            ("shiftTime", real(0.0), true, &[][..]),
            (
                "timeEvents",
                var_ref("Modelica.Blocks.Types.TimeEvents.Always"),
                false,
                &[][..],
            ),
            ("verboseRead", boolean(false), false, &[][..]),
            ("delimiter", string(","), false, &[][..]),
            ("nHeaderLines", integer(0), false, &[][..]),
        ];
        for (field, start, is_tunable, dims) in field_specs {
            insert_parameter_start(
                dae_model,
                &format!("{prefix}.{field}"),
                start,
                is_tunable,
                dims,
            );
        }
    }

    fn insert_time_table_enum_ordinals(dae_model: &mut dae::Dae) {
        dae_model.symbols.enum_literal_ordinals.insert(
            "Modelica.Blocks.Types.Smoothness.ConstantSegments".to_string(),
            3,
        );
        dae_model.symbols.enum_literal_ordinals.insert(
            "Modelica.Blocks.Types.Extrapolation.HoldLastPoint".to_string(),
            1,
        );
        dae_model
            .symbols
            .enum_literal_ordinals
            .insert("Modelica.Blocks.Types.TimeEvents.Always".to_string(), 1);
    }

    #[test]
    fn structural_bindings_evaluate_external_table_constructor_starts() {
        let mut dae_model = dae::Dae::default();
        let table_expr = array(vec![
            array(vec![real(0.0), real(1.0)]),
            array(vec![real(1.0), real(2.0)]),
        ]);
        let constructor = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("ExternalCombiTimeTable"),
            args: vec![
                string("NoName"),
                string("NoName"),
                table_expr,
                real(0.0),
                array(vec![integer(2)]),
            ],
            is_constructor: false,
            span: compile_time_test_span(),
        };
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("tableID"),
            dae::Variable {
                name: rumoca_core::VarName::new("tableID"),
                start: Some(constructor),
                is_tunable: false,
                ..dae::Variable::empty_with_span(compile_time_test_span())
            },
        );

        let bindings = structural_bindings(&dae_model)
            .expect("external table constructors should produce structural table ids");

        assert!(
            bindings.get("tableID").is_some_and(|value| *value > 0.0),
            "expected numeric table id binding, got {bindings:?}"
        );
    }

    #[test]
    fn structural_bindings_derive_metadata_external_table_handle_from_parameter_start() {
        let mut dae_model = dae::Dae::default();
        let table_expr = array(vec![
            array(vec![real(0.0), real(1.0)]),
            array(vec![real(1.0), real(2.0)]),
        ]);
        let constructor = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("ExternalCombiTimeTable"),
            args: vec![
                string("NoName"),
                string("NoName"),
                table_expr,
                real(0.0),
                array(vec![integer(2)]),
            ],
            is_constructor: false,
            span: compile_time_test_span(),
        };
        let table_min = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("getTimeTableTmin"),
            args: vec![constructor],
            is_constructor: false,
            span: compile_time_test_span(),
        };
        dae_model.variables.parameters.insert(
            rumoca_core::VarName::new("block.table.t_minScaled"),
            dae::Variable {
                name: rumoca_core::VarName::new("block.table.t_minScaled"),
                start: Some(table_min),
                is_tunable: false,
                ..dae::Variable::empty_with_span(compile_time_test_span())
            },
        );
        dae_model
            .metadata
            .nonnumeric_variable_names
            .push("block.table.tableID".to_string());

        let bindings = structural_bindings(&dae_model)
            .expect("external table metadata handles should be derived from table users");

        assert!(
            bindings
                .get("block.table.tableID")
                .is_some_and(|value| *value > 0.0),
            "expected metadata table id binding, got {bindings:?}"
        );
    }

    #[test]
    fn structural_bindings_derive_external_time_table_handle_from_record_fields() {
        let mut dae_model = dae::Dae::default();
        insert_external_time_table_record_fields(
            &mut dae_model,
            "block.table",
            array(vec![
                array(vec![real(2.0), real(0.0)]),
                array(vec![real(4.0), real(1.0)]),
            ]),
            &[2, 2],
        );
        dae_model
            .metadata
            .nonnumeric_variable_names
            .push("block.table.tableID".to_string());
        insert_time_table_enum_ordinals(&mut dae_model);

        let bindings = structural_bindings(&dae_model)
            .expect("record fields should derive external time table handle");

        assert!(
            bindings
                .get("block.table.tableID")
                .is_some_and(|value| *value > 0.0),
            "expected record-field table id binding, got {bindings:?}"
        );
    }

    #[test]
    fn structural_bindings_materialize_boolean_table_dynamic_matrix() {
        let mut dae_model = dae::Dae::default();
        insert_parameter_start(
            &mut dae_model,
            "booleanTable.table",
            array(vec![
                real(2.0),
                real(4.0),
                real(6.0),
                real(6.5),
                real(7.0),
                real(9.0),
                real(11.0),
            ]),
            true,
            &[0],
        );
        insert_parameter_start(
            &mut dae_model,
            "booleanTable.n",
            builtin_call(
                rumoca_core::BuiltinFunction::Size,
                vec![var_ref("booleanTable.table"), integer(1)],
            ),
            false,
            &[],
        );
        insert_parameter_start(
            &mut dae_model,
            "booleanTable.startValue",
            boolean(false),
            false,
            &[],
        );
        let toggle_values = comprehension(
            "i",
            range(integer(1), var_ref("booleanTable.n")),
            builtin_call(
                rumoca_core::BuiltinFunction::Mod,
                vec![var_ref("i"), real(2.0)],
            ),
        );
        let table_matrix = rumoca_core::Expression::If {
            branches: vec![(
                binary(
                    rumoca_core::OpBinary::Gt,
                    var_ref("booleanTable.n"),
                    real(0.0),
                ),
                matrix(vec![
                    array(vec![indexed_var("booleanTable.table", 1), real(0.0)]),
                    array(vec![var_ref("booleanTable.table"), toggle_values]),
                ]),
            )],
            else_branch: Box::new(matrix(vec![array(vec![real(0.0), real(0.0)])])),
            span: compile_time_test_span(),
        };
        insert_external_time_table_record_fields(
            &mut dae_model,
            "booleanTable.combiTimeTable",
            table_matrix,
            &[0, 2],
        );
        dae_model
            .metadata
            .nonnumeric_variable_names
            .push("booleanTable.combiTimeTable.tableID".to_string());
        insert_time_table_enum_ordinals(&mut dae_model);

        let mut env = compile_time_eval_env(&dae_model);
        let mut bindings = enum_literal_bindings(&dae_model.symbols.enum_literal_ordinals);
        let shapes = variable_shapes(&dae_model);
        insert_shape_bindings(&mut bindings, &shapes);
        insert_constant_variables(&mut bindings, &dae_model, &shapes, &mut env)
            .expect("constants should seed");
        seed_compile_time_start_values(&dae_model, &mut env);
        insert_structural_parameters(&mut bindings, &dae_model, &shapes, &mut env)
            .expect("structural parameters should seed");
        insert_external_table_handle_bindings(&mut bindings, &dae_model, &shapes, &mut env)
            .expect("external table handle should seed");

        let table_id = *bindings
            .get("booleanTable.combiTimeTable.tableID")
            .expect("tableID binding");
        let table_data = rumoca_eval_dae::all_external_table_data_in_env(&env);
        assert_eq!(table_data.len(), 1, "expected one external table");
        assert_eq!(table_data[0].id as f64, table_id);
        assert!(
            table_data[0].data.len() > 1,
            "BooleanTable handle must not register an empty table: {table_data:?}"
        );
    }

    #[test]
    fn metadata_external_table_handle_overrides_default_empty_table_id() {
        let mut dae_model = dae::Dae::default();
        let default_empty_constructor = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::Reference::new("ExternalCombiTimeTable"),
            args: vec![
                string("NoName"),
                string("NoName"),
                matrix(Vec::new()),
                real(0.0),
                array(vec![integer(2)]),
            ],
            is_constructor: false,
            span: compile_time_test_span(),
        };
        insert_parameter_start(
            &mut dae_model,
            "block.table.tableID",
            default_empty_constructor,
            false,
            &[],
        );
        insert_external_time_table_record_fields(
            &mut dae_model,
            "block.table",
            matrix(vec![
                array(vec![real(0.0), real(1.0)]),
                array(vec![real(1.0), real(2.0)]),
            ]),
            &[2, 2],
        );
        dae_model
            .metadata
            .nonnumeric_variable_names
            .push("block.table.tableID".to_string());
        insert_time_table_enum_ordinals(&mut dae_model);

        let (bindings, env) = structural_bindings_with_eval_env(&dae_model)
            .expect("metadata table handle should override stale default tableID");
        let table_id = *bindings
            .get("block.table.tableID")
            .expect("tableID binding");
        let table_data = rumoca_eval_dae::all_external_table_data_in_env(&env);
        let selected = table_data
            .iter()
            .find(|table| table.id as f64 == table_id)
            .expect("selected table data should be registered");

        assert_eq!(selected.data.len(), 2, "selected table must be non-empty");
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
    fn eval_size_declines_unrepresentable_dimension_index() {
        let args = vec![var_ref("x"), real(usize::MAX as f64)];
        let shapes = IndexMap::from([("x".to_string(), vec![2, 3])]);
        let mut eval_env = rumoca_eval_dae::VarEnv::new();

        assert_eq!(
            eval_size(&args, &IndexMap::new(), &shapes, &mut eval_env),
            None
        );
    }

    #[test]
    fn eval_size_reads_string_literal_array_shape_without_numeric_values() {
        let args = vec![array(vec![string("water"), string("air")]), integer(1)];
        let mut eval_env = rumoca_eval_dae::VarEnv::new();

        assert_eq!(
            eval_size(&args, &IndexMap::new(), &IndexMap::new(), &mut eval_env),
            Some(vec![2.0])
        );
    }

    #[test]
    fn var_key_declines_unrepresentable_expression_subscript() {
        let subscript = rumoca_core::Subscript::expr(
            Box::new(real(usize::MAX as f64)),
            compile_time_test_span(),
        );
        let mut eval_env = rumoca_eval_dae::VarEnv::new();

        assert_eq!(
            var_key(
                &rumoca_core::Reference::new("x"),
                &[subscript],
                &IndexMap::new(),
                &mut eval_env
            ),
            None
        );
    }
}
