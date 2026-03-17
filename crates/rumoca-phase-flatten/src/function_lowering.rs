//! Record parameter lowering and array size insertion for flattened functions.
//!
//! This module handles post-collection passes that transform function signatures
//! and call sites:
//! - Decomposing record-typed parameters into scalar fields
//! - Rewriting FieldAccess expressions on record params to direct VarRef
//! - Inserting size arguments for variable-size array parameters

use rumoca_ir_flat as flat;
use std::collections::{HashMap, HashSet};

/// Known record types and their ordered scalar field names.
/// These records are decomposed into scalar fields for backends that
/// don't support record-typed function parameters natively.
const KNOWN_RECORD_FIELDS: &[(&str, &[&str])] = &[
    ("Complex", &["re", "im"]),
    ("Modelica.Units.SI.ComplexVoltage", &["re", "im"]),
    ("Modelica.Units.SI.ComplexCurrent", &["re", "im"]),
];

/// Get the field names for a known record type, checking both short and qualified names.
pub(super) fn record_fields(type_name: &str) -> Option<&'static [&'static str]> {
    for &(name, fields) in KNOWN_RECORD_FIELDS {
        if type_name == name || type_name.ends_with(&format!(".{name}")) {
            return Some(fields);
        }
    }
    None
}

/// Rewrite FieldAccess on decomposed record params to direct VarRef.
fn rewrite_field_access_in_statement(stmt: &mut flat::Statement, params: &HashSet<String>) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            rewrite_field_access_in_expr(value, params);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                rewrite_field_access_in_expr(&mut idx.range, params);
            }
            for inner in equations {
                rewrite_field_access_in_statement(inner, params);
            }
        }
        flat::Statement::While(block) => {
            rewrite_field_access_in_expr(&mut block.cond, params);
            for inner in &mut block.stmts {
                rewrite_field_access_in_statement(inner, params);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                rewrite_field_access_in_expr(&mut block.cond, params);
                for inner in &mut block.stmts {
                    rewrite_field_access_in_statement(inner, params);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    rewrite_field_access_in_statement(inner, params);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                rewrite_field_access_in_expr(arg, params);
            }
            for out in outputs {
                rewrite_field_access_in_expr(out, params);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            rewrite_field_access_in_expr(value, params);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            rewrite_field_access_in_expr(condition, params);
            rewrite_field_access_in_expr(message, params);
            if let Some(l) = level {
                rewrite_field_access_in_expr(l, params);
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
        flat::Statement::When(blocks) => {
            for block in blocks {
                rewrite_field_access_in_expr(&mut block.cond, params);
                for inner in &mut block.stmts {
                    rewrite_field_access_in_statement(inner, params);
                }
            }
        }
    }
}

fn rewrite_field_access_in_expr(expr: &mut flat::Expression, params: &HashSet<String>) {
    // Check if this is a FieldAccess on a decomposed record param
    if let flat::Expression::FieldAccess { base, field } = expr
        && let flat::Expression::VarRef {
            name, subscripts, ..
        } = base.as_ref()
        && subscripts.is_empty()
        && params.contains(name.as_str())
    {
        let new_name = format!("{}_{}", name.as_str(), field);
        *expr = flat::Expression::VarRef {
            name: flat::VarName::new(new_name),
            subscripts: vec![],
        };
        return;
    }

    // Recurse into sub-expressions
    match expr {
        flat::Expression::Binary { lhs, rhs, .. } => {
            rewrite_field_access_in_expr(lhs, params);
            rewrite_field_access_in_expr(rhs, params);
        }
        flat::Expression::Unary { rhs, .. } => {
            rewrite_field_access_in_expr(rhs, params);
        }
        flat::Expression::BuiltinCall { args, .. }
        | flat::Expression::FunctionCall { args, .. } => {
            for arg in args {
                rewrite_field_access_in_expr(arg, params);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                rewrite_field_access_in_expr(cond, params);
                rewrite_field_access_in_expr(then_expr, params);
            }
            rewrite_field_access_in_expr(else_branch, params);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                rewrite_field_access_in_expr(elem, params);
            }
        }
        flat::Expression::Range { start, step, end } => {
            rewrite_field_access_in_expr(start, params);
            if let Some(s) = step {
                rewrite_field_access_in_expr(s, params);
            }
            rewrite_field_access_in_expr(end, params);
        }
        flat::Expression::Index { base, .. } => {
            rewrite_field_access_in_expr(base, params);
        }
        flat::Expression::FieldAccess { base, .. } => {
            rewrite_field_access_in_expr(base, params);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            rewrite_field_access_in_expr(expr, params);
            for idx in indices {
                rewrite_field_access_in_expr(&mut idx.range, params);
            }
            if let Some(f) = filter {
                rewrite_field_access_in_expr(f, params);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

/// Rewrite FieldAccess expressions on record-typed params in a function body.
///
/// Converts `c.re` → `c_re` in the body without changing the function signature.
/// This is safe for all backends since it only changes expression structure.
pub(super) fn rewrite_record_field_access_in_body(func: &mut flat::Function) {
    let mut record_param_names: HashSet<String> = HashSet::new();
    for input in &func.inputs {
        if record_fields(&input.type_name).is_some() {
            record_param_names.insert(input.name.clone());
        }
    }
    if record_param_names.is_empty() {
        return;
    }
    for stmt in &mut func.body {
        rewrite_field_access_in_statement(stmt, &record_param_names);
    }
}

// =============================================================================
// Post-collection passes: record decomposition and array size insertion
// =============================================================================

/// Decompose record-typed function parameters and rewrite call sites.
///
/// For each function with a known record-typed input (e.g., `Complex`):
/// 1. Replace the record input with scalar field inputs in the signature.
/// 2. Rewrite FieldAccess in the body to VarRef.
/// 3. Walk all equations/functions and decompose call-site arguments.
pub(crate) fn lower_record_function_params(flat: &mut flat::Model) {
    let mut decomposition_map: HashMap<String, Vec<DecomposedParam>> = HashMap::new();

    for (func_name, func) in flat.functions.iter_mut() {
        let mut decomposed: Vec<DecomposedParam> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some(fields) = record_fields(&input.type_name) {
                decomposed.push(DecomposedParam {
                    original_index: idx,
                    param_name: input.name.clone(),
                    fields: fields.to_vec(),
                });
            }
        }
        if decomposed.is_empty() {
            continue;
        }

        // Rewrite FieldAccess in body
        let param_names: HashSet<String> =
            decomposed.iter().map(|d| d.param_name.clone()).collect();
        for stmt in &mut func.body {
            rewrite_field_access_in_statement(stmt, &param_names);
        }

        // Replace record inputs with scalar field inputs
        let old_inputs = std::mem::take(&mut func.inputs);
        for (idx, input) in old_inputs.into_iter().enumerate() {
            let Some(dp) = decomposed.iter().find(|d| d.original_index == idx) else {
                func.inputs.push(input);
                continue;
            };
            for field in &dp.fields {
                func.inputs.push(flat::FunctionParam::new(
                    format!("{}_{}", dp.param_name, field),
                    "Real",
                ));
            }
        }

        decomposition_map.insert(func_name.as_str().to_string(), decomposed);
    }

    if decomposition_map.is_empty() {
        return;
    }

    // Rewrite call sites in equations, variable bindings, and function bodies.
    for eq in &mut flat.equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map);
    }
    for eq in &mut flat.initial_equations {
        decompose_record_call_args_in_expr(&mut eq.residual, &decomposition_map);
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            decompose_record_call_args_in_expr(binding, &decomposition_map);
        }
        if let Some(ref mut start) = var.start {
            decompose_record_call_args_in_expr(start, &decomposition_map);
        }
    }
    for func in flat.functions.values_mut() {
        for stmt in &mut func.body {
            decompose_record_call_args_in_stmt(stmt, &decomposition_map);
        }
    }
}

struct DecomposedParam {
    original_index: usize,
    param_name: String,
    fields: Vec<&'static str>,
}

fn named_constructor_arg<'a>(
    ctor_args: &'a [flat::Expression],
    field: &str,
) -> Option<&'a flat::Expression> {
    for arg in ctor_args {
        if let flat::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

fn decompose_record_call_args_in_stmt(
    stmt: &mut flat::Statement,
    map: &HashMap<String, Vec<DecomposedParam>>,
) {
    match stmt {
        flat::Statement::Assignment { value, .. } => {
            decompose_record_call_args_in_expr(value, map);
        }
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                decompose_record_call_args_in_expr(&mut idx.range, map);
            }
            for inner in equations {
                decompose_record_call_args_in_stmt(inner, map);
            }
        }
        flat::Statement::While(block) => {
            decompose_record_call_args_in_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                decompose_record_call_args_in_stmt(inner, map);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                decompose_record_call_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_call_args_in_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    decompose_record_call_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args {
                decompose_record_call_args_in_expr(arg, map);
            }
            for out in outputs {
                decompose_record_call_args_in_expr(out, map);
            }
        }
        flat::Statement::Reinit { value, .. } => {
            decompose_record_call_args_in_expr(value, map);
        }
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            decompose_record_call_args_in_expr(condition, map);
            decompose_record_call_args_in_expr(message, map);
            if let Some(l) = level {
                decompose_record_call_args_in_expr(l, map);
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                decompose_record_call_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_call_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn decompose_record_call_args_in_expr(
    expr: &mut flat::Expression,
    map: &HashMap<String, Vec<DecomposedParam>>,
) {
    match expr {
        flat::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                decompose_record_call_args_in_expr(arg, map);
            }
            let Some(decomposed) = map.get(name.as_str()) else {
                return;
            };
            let old_args = std::mem::take(args);
            let mut old_idx = 0;
            for dp in decomposed {
                while old_idx < dp.original_index && old_idx < old_args.len() {
                    args.push(old_args[old_idx].clone());
                    old_idx += 1;
                }
                if old_idx < old_args.len() {
                    expand_record_arg(&old_args[old_idx], &dp.fields, args);
                    old_idx += 1;
                }
            }
            while old_idx < old_args.len() {
                args.push(old_args[old_idx].clone());
                old_idx += 1;
            }
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            decompose_record_call_args_in_expr(lhs, map);
            decompose_record_call_args_in_expr(rhs, map);
        }
        flat::Expression::Unary { rhs, .. } => {
            decompose_record_call_args_in_expr(rhs, map);
        }
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                decompose_record_call_args_in_expr(arg, map);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                decompose_record_call_args_in_expr(cond, map);
                decompose_record_call_args_in_expr(then_expr, map);
            }
            decompose_record_call_args_in_expr(else_branch, map);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for elem in elements {
                decompose_record_call_args_in_expr(elem, map);
            }
        }
        flat::Expression::Range { start, step, end } => {
            decompose_record_call_args_in_expr(start, map);
            if let Some(s) = step {
                decompose_record_call_args_in_expr(s, map);
            }
            decompose_record_call_args_in_expr(end, map);
        }
        flat::Expression::Index { base, .. } | flat::Expression::FieldAccess { base, .. } => {
            decompose_record_call_args_in_expr(base, map);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            decompose_record_call_args_in_expr(expr, map);
            for idx in indices {
                decompose_record_call_args_in_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                decompose_record_call_args_in_expr(f, map);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}

/// Expand a record argument into scalar field arguments.
fn expand_record_arg(arg: &flat::Expression, fields: &[&str], out: &mut Vec<flat::Expression>) {
    // Constructor call Complex(re, im) → extract positional/named args
    if let flat::Expression::FunctionCall {
        args: ctor_args,
        is_constructor: true,
        ..
    } = arg
    {
        let positional: Vec<&flat::Expression> = ctor_args
            .iter()
            .filter(|a| {
                !matches!(a, flat::Expression::FunctionCall { is_constructor: true, name, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
            })
            .collect();

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg(ctor_args, field);
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else {
                out.push(flat::Expression::Literal(flat::Literal::Real(0.0)));
            }
        }
        return;
    }

    // Variable reference → emit field VarRefs
    if let flat::Expression::VarRef { name, .. } = arg {
        for field in fields {
            out.push(flat::Expression::VarRef {
                name: flat::VarName::new(format!("{}.{}", name.as_str(), field)),
                subscripts: vec![],
            });
        }
        return;
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(flat::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.to_string(),
        });
    }
}

/// Insert size arguments for variable-size array parameters at call sites.
pub(crate) fn insert_array_size_args(flat: &mut flat::Model) {
    let array_param_map: HashMap<String, Vec<usize>> = flat
        .functions
        .iter()
        .filter_map(|(name, func)| {
            let indices: Vec<usize> = func
                .inputs
                .iter()
                .enumerate()
                .filter(|(_, p)| !p.dims.is_empty())
                .map(|(i, _)| i)
                .collect();
            if indices.is_empty() {
                None
            } else {
                Some((name.as_str().to_string(), indices))
            }
        })
        .collect();

    if array_param_map.is_empty() {
        return;
    }

    for eq in &mut flat.equations {
        insert_size_args_in_expr(&mut eq.residual, &array_param_map);
    }
    for eq in &mut flat.initial_equations {
        insert_size_args_in_expr(&mut eq.residual, &array_param_map);
    }
    for var in flat.variables.values_mut() {
        if let Some(ref mut binding) = var.binding {
            insert_size_args_in_expr(binding, &array_param_map);
        }
    }
    for func in flat.functions.values_mut() {
        for stmt in &mut func.body {
            insert_size_args_in_stmt(stmt, &array_param_map);
        }
    }
}

fn insert_size_args_in_stmt(stmt: &mut flat::Statement, map: &HashMap<String, Vec<usize>>) {
    match stmt {
        flat::Statement::Assignment { value, .. } => insert_size_args_in_expr(value, map),
        flat::Statement::For { indices, equations } => {
            for idx in indices {
                insert_size_args_in_expr(&mut idx.range, map);
            }
            for inner in equations {
                insert_size_args_in_stmt(inner, map);
            }
        }
        flat::Statement::While(block) => {
            insert_size_args_in_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                insert_size_args_in_stmt(inner, map);
            }
        }
        flat::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                insert_size_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_in_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    insert_size_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_in_expr(arg, map);
            }
            for out in outputs {
                insert_size_args_in_expr(out, map);
            }
        }
        flat::Statement::Reinit { value, .. } => insert_size_args_in_expr(value, map),
        flat::Statement::Assert {
            condition,
            message,
            level,
        } => {
            insert_size_args_in_expr(condition, map);
            insert_size_args_in_expr(message, map);
            if let Some(l) = level {
                insert_size_args_in_expr(l, map);
            }
        }
        flat::Statement::When(blocks) => {
            for block in blocks {
                insert_size_args_in_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_in_stmt(inner, map);
                }
            }
        }
        flat::Statement::Empty | flat::Statement::Return | flat::Statement::Break => {}
    }
}

fn insert_size_args_in_expr(expr: &mut flat::Expression, map: &HashMap<String, Vec<usize>>) {
    match expr {
        flat::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_in_expr(arg, map);
            }
            let Some(array_indices) = map.get(name.as_str()) else {
                return;
            };
            let mut new_args = std::mem::take(args);
            for &param_idx in array_indices.iter().rev() {
                if param_idx < new_args.len() {
                    let size_expr = flat::Expression::BuiltinCall {
                        function: flat::BuiltinFunction::Size,
                        args: vec![
                            new_args[param_idx].clone(),
                            flat::Expression::Literal(flat::Literal::Integer(1)),
                        ],
                    };
                    new_args.insert(param_idx + 1, size_expr);
                }
            }
            *args = new_args;
        }
        flat::Expression::Binary { lhs, rhs, .. } => {
            insert_size_args_in_expr(lhs, map);
            insert_size_args_in_expr(rhs, map);
        }
        flat::Expression::Unary { rhs, .. } => insert_size_args_in_expr(rhs, map),
        flat::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                insert_size_args_in_expr(arg, map);
            }
        }
        flat::Expression::If {
            branches,
            else_branch,
        } => {
            for (c, t) in branches {
                insert_size_args_in_expr(c, map);
                insert_size_args_in_expr(t, map);
            }
            insert_size_args_in_expr(else_branch, map);
        }
        flat::Expression::Array { elements, .. } | flat::Expression::Tuple { elements } => {
            for e in elements {
                insert_size_args_in_expr(e, map);
            }
        }
        flat::Expression::Range { start, step, end } => {
            insert_size_args_in_expr(start, map);
            if let Some(s) = step {
                insert_size_args_in_expr(s, map);
            }
            insert_size_args_in_expr(end, map);
        }
        flat::Expression::Index { base, .. } | flat::Expression::FieldAccess { base, .. } => {
            insert_size_args_in_expr(base, map);
        }
        flat::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            insert_size_args_in_expr(expr, map);
            for idx in indices {
                insert_size_args_in_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                insert_size_args_in_expr(f, map);
            }
        }
        flat::Expression::VarRef { .. }
        | flat::Expression::Literal(_)
        | flat::Expression::Empty => {}
    }
}
