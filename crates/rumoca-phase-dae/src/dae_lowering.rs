//! DAE-level lowering passes for code generation.
//!
//! This module contains record function parameter decomposition, array size
//! argument insertion, parameter dependency sorting, and vector equation
//! scalarization that operate on the DAE IR before code generation.

use indexmap::IndexSet;
use rumoca_ir_dae as dae;
use std::collections::{BTreeMap, HashMap, HashSet};

type Dae = dae::Dae;

// =============================================================================
// Record function parameter decomposition (DAE level)
// =============================================================================

/// Known record types and their field names for decomposition.
const KNOWN_RECORD_FIELDS: &[(&str, &[&str])] = &[
    ("Complex", &["re", "im"]),
    ("Modelica.Units.SI.ComplexVoltage", &["re", "im"]),
    ("Modelica.Units.SI.ComplexCurrent", &["re", "im"]),
];

fn dae_record_fields(type_name: &str) -> Option<&'static [&'static str]> {
    for &(name, fields) in KNOWN_RECORD_FIELDS {
        if type_name == name || type_name.ends_with(&format!(".{name}")) {
            return Some(fields);
        }
    }
    None
}

/// Decompose record-typed function params in the DAE. This rewrites function
/// signatures (replacing `c: Complex` with `c_re, c_im: Real`) and decomposes
/// call-site arguments throughout all DAE equations.
/// Decompose record-typed function params in the DAE for code generators.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn lower_record_function_params_dae(dae: &mut Dae) {
    // Identify functions with record params and rewrite their signatures.
    let mut decomp_map: HashMap<String, Vec<(usize, Vec<&'static str>)>> = HashMap::new();

    for (func_name, func) in dae.functions.iter_mut() {
        let mut decomposed: Vec<(usize, String, Vec<&'static str>)> = Vec::new();
        for (idx, input) in func.inputs.iter().enumerate() {
            if let Some(fields) = dae_record_fields(&input.type_name) {
                decomposed.push((idx, input.name.clone(), fields.to_vec()));
            }
        }
        if decomposed.is_empty() {
            continue;
        }

        // Replace record inputs with scalar field inputs
        let old_inputs = std::mem::take(&mut func.inputs);
        for (idx, input) in old_inputs.into_iter().enumerate() {
            let Some((_, param_name, fields)) = decomposed.iter().find(|(i, _, _)| *i == idx)
            else {
                func.inputs.push(input);
                continue;
            };
            for field in fields {
                func.inputs.push(dae::FunctionParam {
                    name: format!("{param_name}_{field}"),
                    type_name: "Real".to_string(),
                    dims: vec![],
                    default: None,
                    description: None,
                });
            }
        }

        let entry: Vec<(usize, Vec<&str>)> = decomposed
            .iter()
            .map(|(idx, _, fields)| (*idx, fields.clone()))
            .collect();
        decomp_map.insert(func_name.as_str().to_string(), entry);
    }

    if decomp_map.is_empty() {
        return;
    }

    // Rewrite call sites in all DAE equations and function bodies.
    for eq in &mut dae.f_x {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.f_z {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.f_m {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.f_c {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for eq in &mut dae.initial_equations {
        decompose_record_args_dae_expr(&mut eq.rhs, &decomp_map);
    }
    for func in dae.functions.values_mut() {
        for stmt in &mut func.body {
            decompose_record_args_dae_stmt(stmt, &decomp_map);
        }
    }
}

fn decompose_record_args_dae_stmt(
    stmt: &mut dae::Statement,
    map: &HashMap<String, Vec<(usize, Vec<&str>)>>,
) {
    match stmt {
        dae::Statement::Assignment { value, .. } => {
            decompose_record_args_dae_expr(value, map);
        }
        dae::Statement::For { indices, equations } => {
            for idx in indices {
                decompose_record_args_dae_expr(&mut idx.range, map);
            }
            for inner in equations {
                decompose_record_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::While(block) => {
            decompose_record_args_dae_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                decompose_record_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                decompose_record_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_args_dae_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    decompose_record_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args.iter_mut() {
                decompose_record_args_dae_expr(arg, map);
            }
            for out in outputs {
                decompose_record_args_dae_expr(out, map);
            }
        }
        dae::Statement::Reinit { value, .. } => {
            decompose_record_args_dae_expr(value, map);
        }
        dae::Statement::Assert {
            condition,
            message,
            level,
        } => {
            decompose_record_args_dae_expr(condition, map);
            decompose_record_args_dae_expr(message, map);
            if let Some(l) = level {
                decompose_record_args_dae_expr(l, map);
            }
        }
        dae::Statement::When(blocks) => {
            for block in blocks {
                decompose_record_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    decompose_record_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::Empty | dae::Statement::Return | dae::Statement::Break => {}
    }
}

fn decompose_record_args_dae_expr(
    expr: &mut dae::Expression,
    map: &HashMap<String, Vec<(usize, Vec<&str>)>>,
) {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                decompose_record_args_dae_expr(arg, map);
            }
            let Some(decomposed) = map.get(name.as_str()) else {
                return;
            };
            let old_args = std::mem::take(args);
            let mut old_idx = 0;
            for (param_idx, fields) in decomposed {
                while old_idx < *param_idx && old_idx < old_args.len() {
                    args.push(old_args[old_idx].clone());
                    old_idx += 1;
                }
                if old_idx < old_args.len() {
                    expand_dae_record_arg(&old_args[old_idx], fields, args);
                    old_idx += 1;
                }
            }
            while old_idx < old_args.len() {
                args.push(old_args[old_idx].clone());
                old_idx += 1;
            }
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            decompose_record_args_dae_expr(lhs, map);
            decompose_record_args_dae_expr(rhs, map);
        }
        dae::Expression::Unary { rhs, .. } => {
            decompose_record_args_dae_expr(rhs, map);
        }
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                decompose_record_args_dae_expr(arg, map);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (c, t) in branches {
                decompose_record_args_dae_expr(c, map);
                decompose_record_args_dae_expr(t, map);
            }
            decompose_record_args_dae_expr(else_branch, map);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for e in elements {
                decompose_record_args_dae_expr(e, map);
            }
        }
        dae::Expression::Range { start, step, end } => {
            decompose_record_args_dae_expr(start, map);
            if let Some(s) = step {
                decompose_record_args_dae_expr(s, map);
            }
            decompose_record_args_dae_expr(end, map);
        }
        dae::Expression::Index { base, .. } | dae::Expression::FieldAccess { base, .. } => {
            decompose_record_args_dae_expr(base, map);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            decompose_record_args_dae_expr(expr, map);
            for idx in indices {
                decompose_record_args_dae_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                decompose_record_args_dae_expr(f, map);
            }
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn named_constructor_arg_dae<'a>(
    ctor_args: &'a [dae::Expression],
    field: &str,
) -> Option<&'a dae::Expression> {
    for arg in ctor_args {
        if let dae::Expression::FunctionCall {
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

fn expand_dae_record_arg(arg: &dae::Expression, fields: &[&str], out: &mut Vec<dae::Expression>) {
    // Constructor call → extract positional/named args
    if let dae::Expression::FunctionCall {
        args: ctor_args,
        is_constructor: true,
        ..
    } = arg
    {
        let positional: Vec<&dae::Expression> = ctor_args
            .iter()
            .filter(|a| {
                !matches!(a, dae::Expression::FunctionCall { is_constructor: true, name, .. }
                if name.as_str().starts_with("__rumoca_named_arg__."))
            })
            .collect();

        for (i, field) in fields.iter().enumerate() {
            let named = named_constructor_arg_dae(ctor_args, field);
            if let Some(val) = named {
                out.push(val.clone());
            } else if i < positional.len() {
                out.push(positional[i].clone());
            } else {
                out.push(dae::Expression::Literal(dae::Literal::Real(0.0)));
            }
        }
        return;
    }

    // Variable reference → emit field VarRefs
    if let dae::Expression::VarRef { name, .. } = arg {
        for field in fields {
            out.push(dae::Expression::VarRef {
                name: dae::VarName::new(format!("{}.{}", name.as_str(), field)),
                subscripts: vec![],
            });
        }
        return;
    }

    // Check for FieldAccess on the record variable (e.g. `c.re` passed directly)
    if let dae::Expression::FieldAccess { .. } = arg {
        // Single field access on a record — just push the base.field expression
        // This handles cases like passing `c.re` where `c` was the original record param.
        out.push(arg.clone());
        // Pad remaining fields with 0.0
        for _ in 1..fields.len() {
            out.push(dae::Expression::Literal(dae::Literal::Real(0.0)));
        }
        return;
    }

    // Scalar expression passed to a record-typed parameter (e.g. Real expr passed
    // where Complex is expected) — treat as the first field, zero-fill the rest.
    // This avoids generating invalid C like `(expr).re`.
    if is_obviously_scalar(arg) {
        out.push(arg.clone());
        for _ in 1..fields.len() {
            out.push(dae::Expression::Literal(dae::Literal::Real(0.0)));
        }
        return;
    }

    // General expression → emit FieldAccess
    for field in fields {
        out.push(dae::Expression::FieldAccess {
            base: Box::new(arg.clone()),
            field: field.to_string(),
        });
    }
}

/// Returns true if the expression is obviously a scalar (not a record type).
fn is_obviously_scalar(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::Literal(_) => true,
        dae::Expression::Binary { .. } | dae::Expression::Unary { .. } => true,
        dae::Expression::BuiltinCall { function, .. } => {
            // Math builtins return scalars
            matches!(
                function,
                dae::BuiltinFunction::Abs
                    | dae::BuiltinFunction::Sign
                    | dae::BuiltinFunction::Sqrt
                    | dae::BuiltinFunction::Sin
                    | dae::BuiltinFunction::Cos
                    | dae::BuiltinFunction::Tan
                    | dae::BuiltinFunction::Asin
                    | dae::BuiltinFunction::Acos
                    | dae::BuiltinFunction::Atan
                    | dae::BuiltinFunction::Atan2
                    | dae::BuiltinFunction::Sinh
                    | dae::BuiltinFunction::Cosh
                    | dae::BuiltinFunction::Tanh
                    | dae::BuiltinFunction::Exp
                    | dae::BuiltinFunction::Log
                    | dae::BuiltinFunction::Log10
                    | dae::BuiltinFunction::Floor
                    | dae::BuiltinFunction::Ceil
                    | dae::BuiltinFunction::Min
                    | dae::BuiltinFunction::Max
                    | dae::BuiltinFunction::Sum
                    | dae::BuiltinFunction::Size
                    | dae::BuiltinFunction::Der
                    | dae::BuiltinFunction::Pre
                    | dae::BuiltinFunction::Mod
                    | dae::BuiltinFunction::Rem
                    | dae::BuiltinFunction::Div
            )
        }
        dae::Expression::If { else_branch, .. } => is_obviously_scalar(else_branch),
        // User-defined function calls return scalar double in generated C
        dae::Expression::FunctionCall { .. } => true,
        _ => false,
    }
}

/// Insert size arguments for variable-size array params at DAE call sites.
/// Must NOT be called before simulation — only before codegen rendering.
pub fn insert_array_size_args_dae(dae: &mut Dae) {
    let array_param_map: HashMap<String, Vec<usize>> = dae
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

    for eq in &mut dae.f_x {
        insert_size_args_dae_expr(&mut eq.rhs, &array_param_map);
    }
    for eq in &mut dae.f_z {
        insert_size_args_dae_expr(&mut eq.rhs, &array_param_map);
    }
    for eq in &mut dae.f_m {
        insert_size_args_dae_expr(&mut eq.rhs, &array_param_map);
    }
    for func in dae.functions.values_mut() {
        for stmt in &mut func.body {
            insert_size_args_dae_stmt(stmt, &array_param_map);
        }
    }
}

fn insert_size_args_dae_stmt(stmt: &mut dae::Statement, map: &HashMap<String, Vec<usize>>) {
    match stmt {
        dae::Statement::Assignment { value, .. } => insert_size_args_dae_expr(value, map),
        dae::Statement::For { indices, equations } => {
            for idx in indices {
                insert_size_args_dae_expr(&mut idx.range, map);
            }
            for inner in equations {
                insert_size_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::While(block) => {
            insert_size_args_dae_expr(&mut block.cond, map);
            for inner in &mut block.stmts {
                insert_size_args_dae_stmt(inner, map);
            }
        }
        dae::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                insert_size_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_dae_stmt(inner, map);
                }
            }
            if let Some(stmts) = else_block {
                for inner in stmts {
                    insert_size_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::FunctionCall { args, outputs, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_dae_expr(arg, map);
            }
            for out in outputs {
                insert_size_args_dae_expr(out, map);
            }
        }
        dae::Statement::Reinit { value, .. } => insert_size_args_dae_expr(value, map),
        dae::Statement::Assert {
            condition,
            message,
            level,
        } => {
            insert_size_args_dae_expr(condition, map);
            insert_size_args_dae_expr(message, map);
            if let Some(l) = level {
                insert_size_args_dae_expr(l, map);
            }
        }
        dae::Statement::When(blocks) => {
            for block in blocks {
                insert_size_args_dae_expr(&mut block.cond, map);
                for inner in &mut block.stmts {
                    insert_size_args_dae_stmt(inner, map);
                }
            }
        }
        dae::Statement::Empty | dae::Statement::Return | dae::Statement::Break => {}
    }
}

fn insert_size_args_dae_expr(expr: &mut dae::Expression, map: &HashMap<String, Vec<usize>>) {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            for arg in args.iter_mut() {
                insert_size_args_dae_expr(arg, map);
            }
            let Some(array_indices) = map.get(name.as_str()) else {
                return;
            };
            let mut new_args = std::mem::take(args);
            for &param_idx in array_indices.iter().rev() {
                if param_idx < new_args.len() {
                    // If the argument is an Array literal with known element count,
                    // use the literal count directly instead of size() which can't
                    // be rendered for compound literals in C.
                    let size_expr = array_size_expr_for_arg(&new_args[param_idx]);
                    new_args.insert(param_idx + 1, size_expr);
                }
            }
            *args = new_args;
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            insert_size_args_dae_expr(lhs, map);
            insert_size_args_dae_expr(rhs, map);
        }
        dae::Expression::Unary { rhs, .. } => insert_size_args_dae_expr(rhs, map),
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                insert_size_args_dae_expr(arg, map);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (c, t) in branches {
                insert_size_args_dae_expr(c, map);
                insert_size_args_dae_expr(t, map);
            }
            insert_size_args_dae_expr(else_branch, map);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for e in elements {
                insert_size_args_dae_expr(e, map);
            }
        }
        dae::Expression::Range { start, step, end } => {
            insert_size_args_dae_expr(start, map);
            if let Some(s) = step {
                insert_size_args_dae_expr(s, map);
            }
            insert_size_args_dae_expr(end, map);
        }
        dae::Expression::Index { base, .. } | dae::Expression::FieldAccess { base, .. } => {
            insert_size_args_dae_expr(base, map);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            insert_size_args_dae_expr(expr, map);
            for idx in indices {
                insert_size_args_dae_expr(&mut idx.range, map);
            }
            if let Some(f) = filter {
                insert_size_args_dae_expr(f, map);
            }
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn array_size_expr_for_arg(arg: &dae::Expression) -> dae::Expression {
    if let dae::Expression::Array { elements, .. } = arg {
        return dae::Expression::Literal(dae::Literal::Integer(elements.len() as i64));
    }

    dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Size,
        args: vec![
            arg.clone(),
            dae::Expression::Literal(dae::Literal::Integer(1)),
        ],
    }
}

// =============================================================================

/// Topologically sort parameters so that start-value dependencies are ordered.
///
/// If parameter A's `start` expression references parameter B, then B must
/// appear before A in the parameter map. This ensures code generators that
/// evaluate start values sequentially produce correct numeric results.
///
/// Falls back to the original order if cycles are detected.
pub(crate) fn sort_parameters_by_start_dependency(dae: &mut Dae) {
    let param_names: IndexSet<dae::VarName> = dae.parameters.keys().cloned().collect();
    if param_names.len() <= 1 {
        return;
    }

    // Build adjacency: param → set of params its start expression depends on
    let mut deps: HashMap<usize, Vec<usize>> = HashMap::new();
    for (idx, (_, var)) in dae.parameters.iter().enumerate() {
        let Some(ref start_expr) = var.start else {
            continue;
        };
        for ref_name in collect_expression_var_refs(start_expr) {
            let Some(dep_idx) = param_names.get_index_of(&ref_name) else {
                continue;
            };
            if dep_idx != idx {
                deps.entry(idx).or_default().push(dep_idx);
            }
        }
    }

    // If no dependencies exist, nothing to reorder.
    if deps.is_empty() {
        return;
    }

    // Kahn's algorithm for topological sort
    let n = param_names.len();
    let mut in_degree = vec![0usize; n];
    // Build forward edges: if node depends on dep, then dep → node
    let mut forward: Vec<Vec<usize>> = vec![Vec::new(); n];
    for (&node, predecessors) in &deps {
        for &pred in predecessors {
            forward[pred].push(node);
            in_degree[node] += 1;
        }
    }

    let mut queue: std::collections::VecDeque<usize> = std::collections::VecDeque::new();
    for (i, &deg) in in_degree.iter().enumerate() {
        if deg == 0 {
            queue.push_back(i);
        }
    }

    let mut sorted_indices = Vec::with_capacity(n);
    while let Some(node) = queue.pop_front() {
        sorted_indices.push(node);
        for &next in &forward[node] {
            in_degree[next] -= 1;
            if in_degree[next] == 0 {
                queue.push_back(next);
            }
        }
    }

    if sorted_indices.len() != n {
        // Cycle detected — keep original order (conservative fallback)
        return;
    }

    // Rebuild the parameters IndexMap in sorted order
    let old_params: Vec<(dae::VarName, dae::Variable)> = dae.parameters.drain(..).collect();
    for &idx in &sorted_indices {
        let (name, var) = old_params[idx].clone();
        dae.parameters.insert(name, var);
    }
}

/// Collect all VarRef names from an expression tree.
fn collect_expression_var_refs(expr: &dae::Expression) -> Vec<dae::VarName> {
    let mut refs = Vec::new();
    collect_var_refs_recursive(expr, &mut refs);
    refs
}

// =============================================================================
// Vector equation scalarization
// =============================================================================

/// Scalarize vector equations that reference "phantom" base names.
///
/// In Modelica, connector arrays like `plug_p.pin[3]` produce scalarized
/// variables (`sineVoltage.plug_p.pin[1].v`, `…pin[2].v`, `…pin[3].v`)
/// but some component-level equations reference the unsubscripted base name
/// (`sineVoltage.plug_p.pin.v`) as a vector.  These phantom base names do
/// not appear in any DAE variable map, so backends that render equations
/// directly (CasADi, SymPy, JAX) produce undefined identifiers.
///
/// This pass detects equations with `scalar_count > 1` whose expressions
/// contain such phantom VarRefs, and expands each into `scalar_count`
/// scalar equations — one per element — with every phantom VarRef replaced
/// by its indexed variant and every declared-array VarRef subscripted.
pub fn scalarize_phantom_vector_equations(dae: &mut Dae) {
    let known_names = build_known_var_name_set(dae);
    let phantom_map = build_phantom_expansion_map(&known_names);
    if phantom_map.is_empty() {
        return;
    }

    // Build array-dims lookup for declared array variables (those with dims)
    let array_dims = build_array_dims_map(dae);

    scalarize_equation_list(&mut dae.f_x, &known_names, &phantom_map, &array_dims);
    scalarize_equation_list(
        &mut dae.initial_equations,
        &known_names,
        &phantom_map,
        &array_dims,
    );
}

/// Build the set of all variable names known to the DAE.
fn build_known_var_name_set(dae: &Dae) -> HashSet<String> {
    let mut names = HashSet::new();
    for map in [
        &dae.states,
        &dae.algebraics,
        &dae.inputs,
        &dae.outputs,
        &dae.parameters,
        &dae.constants,
        &dae.discrete_reals,
        &dae.discrete_valued,
        &dae.derivative_aliases,
    ] {
        for name in map.keys() {
            names.insert(name.as_str().to_string());
        }
    }
    names
}

/// Build a map from stripped base name → sorted list of actual indexed names.
///
/// For example, if the DAE has `sineVoltage.plug_p.pin[1].v`,
/// `sineVoltage.plug_p.pin[2].v`, `sineVoltage.plug_p.pin[3].v`, the map
/// will contain `"sineVoltage.plug_p.pin.v" → ["…pin[1].v", "…pin[2].v", "…pin[3].v"]`.
///
/// Only base names that do NOT themselves appear in `known_names` are included
/// (i.e., only phantom base names that need expansion).
fn build_phantom_expansion_map(known_names: &HashSet<String>) -> HashMap<String, Vec<String>> {
    // Group all known names by their stripped base
    let mut base_to_indexed: HashMap<String, BTreeMap<String, ()>> = HashMap::new();
    for name in known_names {
        let base = super::path_utils::strip_all_subscripts(name);
        if base != *name {
            // This name has subscripts — record it under the base
            base_to_indexed
                .entry(base)
                .or_default()
                .insert(name.clone(), ());
        }
    }

    // Only keep entries where the base name itself is NOT a known variable
    let mut result = HashMap::new();
    for (base, indexed) in base_to_indexed {
        if !known_names.contains(&base) && indexed.len() > 1 {
            result.insert(base, indexed.into_keys().collect());
        }
    }
    result
}

/// Build a map from variable name → dims for declared array variables.
fn build_array_dims_map(dae: &Dae) -> HashMap<String, Vec<i64>> {
    let mut dims_map = HashMap::new();
    for map in [
        &dae.states,
        &dae.algebraics,
        &dae.inputs,
        &dae.outputs,
        &dae.parameters,
        &dae.constants,
        &dae.discrete_reals,
        &dae.discrete_valued,
        &dae.derivative_aliases,
    ] {
        for (name, var) in map {
            if !var.dims.is_empty() {
                dims_map.insert(name.as_str().to_string(), var.dims.clone());
            }
        }
    }
    dims_map
}

/// Check if an expression contains any phantom VarRef that needs expansion.
#[allow(clippy::only_used_in_recursion)]
fn expr_has_phantom_refs(
    expr: &dae::Expression,
    known_names: &HashSet<String>,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> bool {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            let n = name.as_str();
            if subscripts.is_empty() {
                // Phantom base name?
                if phantom_map.contains_key(n) {
                    return true;
                }
                // Declared array variable without subscripts?
                if array_dims.contains_key(n) {
                    return true;
                }
            }
            false
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expr_has_phantom_refs(lhs, known_names, phantom_map, array_dims)
                || expr_has_phantom_refs(rhs, known_names, phantom_map, array_dims)
        }
        dae::Expression::Unary { rhs, .. } => {
            expr_has_phantom_refs(rhs, known_names, phantom_map, array_dims)
        }
        dae::Expression::BuiltinCall { args, .. } | dae::Expression::FunctionCall { args, .. } => {
            args.iter()
                .any(|a| expr_has_phantom_refs(a, known_names, phantom_map, array_dims))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(c, v)| {
                expr_has_phantom_refs(c, known_names, phantom_map, array_dims)
                    || expr_has_phantom_refs(v, known_names, phantom_map, array_dims)
            }) || expr_has_phantom_refs(else_branch, known_names, phantom_map, array_dims)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|e| expr_has_phantom_refs(e, known_names, phantom_map, array_dims)),
        _ => false,
    }
}

/// Scalarize an expression at index `k` (0-based).
///
/// - Phantom VarRefs are replaced by the k-th indexed variant from `phantom_map`.
/// - Declared array VarRefs (with no subscripts) get subscript `[k+1]` (1-based).
/// - All other expressions are recursively processed.
fn scalarize_expr_at(
    expr: &dae::Expression,
    k: usize,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
) -> dae::Expression {
    match expr {
        dae::Expression::VarRef { name, subscripts } => {
            let n = name.as_str();
            if subscripts.is_empty() {
                // Phantom base name — replace with the k-th indexed variant
                if let Some(variants) = phantom_map.get(n)
                    && k < variants.len()
                {
                    return dae::Expression::VarRef {
                        name: dae::VarName::new(&variants[k]),
                        subscripts: vec![],
                    };
                }
                // Declared array variable — add subscript [k+1] (1-based)
                if array_dims.contains_key(n) {
                    return dae::Expression::VarRef {
                        name: name.clone(),
                        subscripts: vec![dae::Subscript::Index((k + 1) as i64)],
                    };
                }
            }
            // Scalar or already subscripted — leave unchanged
            expr.clone()
        }
        dae::Expression::Binary { op, lhs, rhs } => dae::Expression::Binary {
            op: op.clone(),
            lhs: Box::new(scalarize_expr_at(lhs, k, phantom_map, array_dims)),
            rhs: Box::new(scalarize_expr_at(rhs, k, phantom_map, array_dims)),
        },
        dae::Expression::Unary { op, rhs } => dae::Expression::Unary {
            op: op.clone(),
            rhs: Box::new(scalarize_expr_at(rhs, k, phantom_map, array_dims)),
        },
        dae::Expression::BuiltinCall { function, args } => dae::Expression::BuiltinCall {
            function: *function,
            args: args
                .iter()
                .map(|a| scalarize_expr_at(a, k, phantom_map, array_dims))
                .collect(),
        },
        dae::Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => dae::Expression::FunctionCall {
            name: name.clone(),
            args: args
                .iter()
                .map(|a| scalarize_expr_at(a, k, phantom_map, array_dims))
                .collect(),
            is_constructor: *is_constructor,
        },
        dae::Expression::If {
            branches,
            else_branch,
        } => dae::Expression::If {
            branches: branches
                .iter()
                .map(|(c, v)| {
                    (
                        scalarize_expr_at(c, k, phantom_map, array_dims),
                        scalarize_expr_at(v, k, phantom_map, array_dims),
                    )
                })
                .collect(),
            else_branch: Box::new(scalarize_expr_at(else_branch, k, phantom_map, array_dims)),
        },
        dae::Expression::Array { elements, .. } => {
            // An array literal in a vector equation context: extract element k
            if k < elements.len() {
                scalarize_expr_at(&elements[k], k, phantom_map, array_dims)
            } else {
                expr.clone()
            }
        }
        _ => expr.clone(),
    }
}

/// Process an equation list, expanding vector equations with phantom refs.
fn scalarize_equation_list(
    equations: &mut Vec<dae::Equation>,
    known_names: &HashSet<String>,
    phantom_map: &HashMap<String, Vec<String>>,
    array_dims: &HashMap<String, Vec<i64>>,
) {
    let mut new_equations = Vec::with_capacity(equations.len());
    for eq in equations.drain(..) {
        if eq.scalar_count > 1
            && expr_has_phantom_refs(&eq.rhs, known_names, phantom_map, array_dims)
        {
            // Expand into scalar_count individual equations
            for k in 0..eq.scalar_count {
                let scalar_rhs = scalarize_expr_at(&eq.rhs, k, phantom_map, array_dims);
                new_equations.push(dae::Equation::residual(
                    scalar_rhs,
                    eq.span,
                    format!("{} [scalarized {}]", eq.origin, k + 1),
                ));
            }
        } else {
            new_equations.push(eq);
        }
    }
    *equations = new_equations;
}

fn collect_var_refs_recursive(expr: &dae::Expression, refs: &mut Vec<dae::VarName>) {
    match expr {
        dae::Expression::VarRef { name, .. } => {
            refs.push(name.clone());
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            collect_var_refs_recursive(lhs, refs);
            collect_var_refs_recursive(rhs, refs);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_var_refs_recursive(rhs, refs);
        }
        dae::Expression::BuiltinCall { args, .. } => {
            for arg in args {
                collect_var_refs_recursive(arg, refs);
            }
        }
        dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_var_refs_recursive(arg, refs);
            }
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_var_refs_recursive(cond, refs);
                collect_var_refs_recursive(then_expr, refs);
            }
            collect_var_refs_recursive(else_branch, refs);
        }
        dae::Expression::Array { elements, .. } => {
            for elem in elements {
                collect_var_refs_recursive(elem, refs);
            }
        }
        dae::Expression::Tuple { elements } => {
            for elem in elements {
                collect_var_refs_recursive(elem, refs);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_var_refs_recursive(start, refs);
            if let Some(step_expr) = step {
                collect_var_refs_recursive(step_expr, refs);
            }
            collect_var_refs_recursive(end, refs);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_var_refs_recursive(expr, refs);
            for idx in indices {
                collect_var_refs_recursive(&idx.range, refs);
            }
            if let Some(f) = filter {
                collect_var_refs_recursive(f, refs);
            }
        }
        dae::Expression::Index { base, .. } => {
            collect_var_refs_recursive(base, refs);
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_var_refs_recursive(base, refs);
        }
        dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;

    /// Build a VarRef expression.
    fn var_ref(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    /// Build a binary subtraction: lhs - rhs.
    fn sub(lhs: dae::Expression, rhs: dae::Expression) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    /// Collect all VarRef names from an expression (for assertions).
    fn all_var_names(expr: &dae::Expression) -> Vec<String> {
        let mut names = Vec::new();
        collect_var_names_rec(expr, &mut names);
        names
    }

    fn collect_var_names_rec(expr: &dae::Expression, names: &mut Vec<String>) {
        match expr {
            dae::Expression::VarRef { name, subscripts } => {
                if subscripts.is_empty() {
                    names.push(name.as_str().to_string());
                } else {
                    // Format with subscripts for clarity
                    let subs: Vec<String> = subscripts
                        .iter()
                        .map(|s| match s {
                            dae::Subscript::Index(i) => format!("{i}"),
                            _ => "?".to_string(),
                        })
                        .collect();
                    names.push(format!("{}[{}]", name.as_str(), subs.join(",")));
                }
            }
            dae::Expression::Binary { lhs, rhs, .. } => {
                collect_var_names_rec(lhs, names);
                collect_var_names_rec(rhs, names);
            }
            dae::Expression::Unary { rhs, .. } => {
                collect_var_names_rec(rhs, names);
            }
            _ => {}
        }
    }

    #[test]
    fn test_scalarize_phantom_vector_equations() {
        // Set up a DAE that mimics the TransformerYY pattern:
        // - sineVoltage.v is a declared 3-element array variable
        // - sineVoltage.plug_p.pin[1].v, [2].v, [3].v are individual scalars
        // - sineVoltage.plug_n.pin[1].v, [2].v, [3].v are individual scalars
        // - Equation: sineVoltage.v - (sineVoltage.plug_p.pin.v - sineVoltage.plug_n.pin.v) = 0
        //   with scalar_count = 3

        let mut dae = Dae::new();

        // Declared array variable
        let mut sv_v = dae::Variable::new(dae::VarName::new("sineVoltage.v"));
        sv_v.dims = vec![3];
        dae.algebraics
            .insert(dae::VarName::new("sineVoltage.v"), sv_v);

        // Scalarized connector pin variables (no dims — they're individual scalars)
        for k in 1..=3 {
            let name_p = format!("sineVoltage.plug_p.pin[{k}].v");
            dae.algebraics.insert(
                dae::VarName::new(&name_p),
                dae::Variable::new(dae::VarName::new(&name_p)),
            );
            let name_n = format!("sineVoltage.plug_n.pin[{k}].v");
            dae.algebraics.insert(
                dae::VarName::new(&name_n),
                dae::Variable::new(dae::VarName::new(&name_n)),
            );
        }

        // Vector equation: sineVoltage.v - (sineVoltage.plug_p.pin.v - sineVoltage.plug_n.pin.v) = 0
        let eq_rhs = sub(
            var_ref("sineVoltage.v"),
            sub(
                var_ref("sineVoltage.plug_p.pin.v"),
                var_ref("sineVoltage.plug_n.pin.v"),
            ),
        );
        let eq = dae::Equation::residual_array(eq_rhs, Span::DUMMY, "test equation", 3);
        dae.f_x.push(eq);

        // Run scalarization
        scalarize_phantom_vector_equations(&mut dae);

        // Should now have 3 scalar equations instead of 1 vector equation
        assert_eq!(
            dae.f_x.len(),
            3,
            "expected 3 scalar equations, got {}",
            dae.f_x.len()
        );

        for (k, eq) in dae.f_x.iter().enumerate() {
            assert_eq!(eq.scalar_count, 1, "equation {k} should be scalar");

            let names = all_var_names(&eq.rhs);
            // Should NOT contain phantom base names
            assert!(
                !names.iter().any(|n| n == "sineVoltage.plug_p.pin.v"),
                "equation {k} still has phantom ref: {names:?}"
            );
            assert!(
                !names.iter().any(|n| n == "sineVoltage.plug_n.pin.v"),
                "equation {k} still has phantom ref: {names:?}"
            );
            // Should contain the indexed variants
            let expected_p = format!("sineVoltage.plug_p.pin[{}].v", k + 1);
            let expected_n = format!("sineVoltage.plug_n.pin[{}].v", k + 1);
            assert!(
                names.iter().any(|n| n == &expected_p),
                "equation {k} missing {expected_p}: {names:?}"
            );
            assert!(
                names.iter().any(|n| n == &expected_n),
                "equation {k} missing {expected_n}: {names:?}"
            );
            // sineVoltage.v should be subscripted with [k+1]
            let expected_sv = format!("sineVoltage.v[{}]", k + 1);
            assert!(
                names.iter().any(|n| n == &expected_sv),
                "equation {k} missing {expected_sv}: {names:?}"
            );
        }
    }

    #[test]
    fn test_scalarize_leaves_scalar_equations_unchanged() {
        let mut dae = Dae::new();

        // A simple scalar equation: x - y = 0
        dae.algebraics.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae.algebraics.insert(
            dae::VarName::new("y"),
            dae::Variable::new(dae::VarName::new("y")),
        );

        let eq = dae::Equation::residual(sub(var_ref("x"), var_ref("y")), Span::DUMMY, "scalar eq");
        dae.f_x.push(eq);

        scalarize_phantom_vector_equations(&mut dae);

        // Should remain 1 equation
        assert_eq!(dae.f_x.len(), 1);
        assert_eq!(dae.f_x[0].scalar_count, 1);
    }

    #[test]
    fn test_scalarize_ignores_vector_equations_without_phantom_refs() {
        let mut dae = Dae::new();

        // Both variables are declared arrays — no phantom refs
        let mut var_a = dae::Variable::new(dae::VarName::new("a"));
        var_a.dims = vec![3];
        dae.algebraics.insert(dae::VarName::new("a"), var_a);

        let mut var_b = dae::Variable::new(dae::VarName::new("b"));
        var_b.dims = vec![3];
        dae.algebraics.insert(dae::VarName::new("b"), var_b);

        let eq = dae::Equation::residual_array(
            sub(var_ref("a"), var_ref("b")),
            Span::DUMMY,
            "vector eq",
            3,
        );
        dae.f_x.push(eq);

        scalarize_phantom_vector_equations(&mut dae);

        // No phantom refs exist — both a and b are properly declared array vars.
        // The pass should leave the equation unchanged (vector ops are fine for backends).
        assert_eq!(dae.f_x.len(), 1);
        assert_eq!(dae.f_x[0].scalar_count, 3);
    }
}
