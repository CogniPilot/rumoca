use super::{
    IntegerEvalEnv, MAX_EXPR_EVAL_DEPTH, ResolveClassComponents, ast, eval_integer_binary,
    eval_integer_function_call, evaluate_component_condition_with_depth,
    try_eval_bool_expr_with_depth_and_locals, try_eval_bool_expr_with_local_values,
    try_eval_integer_expr_with_depth_and_locals,
};
use rustc_hash::FxHashMap;

const MAX_FUNCTION_LOOP_ITERATIONS: usize = 4096;

enum LocalValue {
    Integer(i64),
    Bool(bool),
}

#[derive(Default)]
struct MixedLocals {
    ints: FxHashMap<String, i64>,
    bools: FxHashMap<String, bool>,
}

#[derive(Copy, Clone)]
struct MixedCallerLocals<'a> {
    ints: Option<&'a FxHashMap<String, i64>>,
    bools: Option<&'a FxHashMap<String, bool>>,
}

pub(super) fn bind_function_inputs(
    function_def: &ast::ClassDef,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    caller_locals: Option<&FxHashMap<String, i64>>,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<()> {
    let inputs: Vec<_> = function_def
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.causality, rumoca_core::Causality::Input(_)))
        .collect();

    let mut positional_idx = 0usize;
    for arg in args {
        if matches!(arg, ast::Expression::NamedArgument { .. }) {
            continue;
        }
        if let Some((name, _)) = inputs.get(positional_idx) {
            let value = try_eval_integer_expr_with_depth_and_locals(
                arg,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth,
                caller_locals,
            )?;
            local_values.insert((*name).clone(), value);
        }
        positional_idx += 1;
    }

    for arg in args {
        if let ast::Expression::NamedArgument { name, value, .. } = arg
            && let Some((param_name, _)) = inputs
                .iter()
                .find(|(input_name, _)| input_name.as_str() == name.text.as_ref())
        {
            let input_value = try_eval_integer_expr_with_depth_and_locals(
                value,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth,
                caller_locals,
            )?;
            local_values.insert((*param_name).clone(), input_value);
        }
    }

    for (input_name, input_component) in inputs {
        if local_values.contains_key(input_name.as_str()) {
            continue;
        }
        if let Some(binding) = &input_component.binding
            && let Some(value) = try_eval_integer_expr_with_depth_and_locals(
                binding,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth,
                Some(local_values),
            )
        {
            local_values.insert(input_name.clone(), value);
            continue;
        }
        if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
            &input_component.start,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth,
            Some(local_values),
        ) {
            local_values.insert(input_name.clone(), value);
        }
    }

    Some(())
}

pub(super) fn initialize_function_locals(
    function_def: &ast::ClassDef,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) {
    for (name, component) in &function_def.components {
        if local_values.contains_key(name.as_str()) {
            continue;
        }
        if let Some(binding) = &component.binding
            && let Some(value) = try_eval_integer_expr_with_depth_and_locals(
                binding,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth,
                Some(local_values),
            )
        {
            local_values.insert(name.clone(), value);
            continue;
        }
        if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
            &component.start,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth,
            Some(local_values),
        ) {
            local_values.insert(name.clone(), value);
        }
    }
}

pub(super) fn find_function_output_name(function_def: &ast::ClassDef) -> Option<String> {
    function_def
        .components
        .iter()
        .find(|(_, comp)| matches!(comp.causality, rumoca_core::Causality::Output(_)))
        .map(|(name, _)| name.clone())
}

pub(super) fn find_scalar_function_output_name(function_def: &ast::ClassDef) -> Option<String> {
    function_def
        .components
        .iter()
        .find(|(_, comp)| {
            matches!(comp.causality, rumoca_core::Causality::Output(_))
                && comp.shape.is_empty()
                && comp.shape_expr.is_empty()
        })
        .map(|(name, _)| name.clone())
}

pub(super) fn interpret_function_statements(
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    for statement in statements {
        if interpret_function_statement(statement, env, depth, local_values)? {
            return Some(true);
        }
    }
    Some(false)
}

fn interpret_function_statement(
    statement: &ast::Statement,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    match statement {
        ast::Statement::Assignment { comp, value } => {
            let evaluated = try_eval_integer_expr_with_depth_and_locals(
                value,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
                Some(local_values),
            )?;
            local_values.insert(comp.to_string(), evaluated);
            Some(false)
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => interpret_function_if(cond_blocks, else_block.as_deref(), env, depth, local_values),
        ast::Statement::For { indices, equations } => {
            interpret_function_for(indices, equations, env, depth, local_values)
        }
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            handle_function_call_statement(comp, args, outputs, env, depth, local_values);
            Some(false)
        }
        ast::Statement::Return { .. } => Some(true),
        ast::Statement::Empty
        | ast::Statement::Break { .. }
        | ast::Statement::While(_)
        | ast::Statement::When(_)
        | ast::Statement::Reinit { .. }
        | ast::Statement::Assert { .. } => Some(false),
    }
}

pub(super) fn eval_user_defined_bool_function(
    function_def: &ast::ClassDef,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    caller_ints: Option<&FxHashMap<String, i64>>,
    caller_bools: Option<&FxHashMap<String, bool>>,
) -> Option<bool> {
    if !function_def.pure || function_def.external.is_some() || depth >= MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let mut locals = MixedLocals::default();
    bind_mixed_function_inputs(
        function_def,
        args,
        env,
        depth + 1,
        MixedCallerLocals {
            ints: caller_ints,
            bools: caller_bools,
        },
        &mut locals,
    )?;
    initialize_mixed_function_locals(function_def, env, depth + 1, &mut locals);
    let output_name = find_function_output_name(function_def)?;

    for algorithm in &function_def.algorithms {
        if interpret_bool_function_statements(algorithm, env, depth + 1, &mut locals)? {
            break;
        }
    }

    locals.bools.get(&output_name).copied()
}

fn bind_mixed_function_inputs(
    function_def: &ast::ClassDef,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    caller_locals: MixedCallerLocals<'_>,
    locals: &mut MixedLocals,
) -> Option<()> {
    let inputs: Vec<_> = function_def
        .components
        .iter()
        .filter(|(_, comp)| matches!(comp.causality, rumoca_core::Causality::Input(_)))
        .collect();

    let mut positional_idx = 0usize;
    for arg in args {
        if matches!(arg, ast::Expression::NamedArgument { .. }) {
            continue;
        }
        if let Some((name, _)) = inputs.get(positional_idx) {
            let value =
                eval_mixed_local_value(arg, env, depth, caller_locals.ints, caller_locals.bools)?;
            insert_local_value(name, value, locals);
        }
        positional_idx += 1;
    }

    for arg in args {
        if let ast::Expression::NamedArgument { name, value, .. } = arg
            && let Some((param_name, _)) = inputs
                .iter()
                .find(|(input_name, _)| input_name.as_str() == name.text.as_ref())
        {
            let input_value =
                eval_mixed_local_value(value, env, depth, caller_locals.ints, caller_locals.bools)?;
            insert_local_value(param_name, input_value, locals);
        }
    }

    for (input_name, input_component) in inputs {
        if locals.ints.contains_key(input_name.as_str())
            || locals.bools.contains_key(input_name.as_str())
        {
            continue;
        }
        if assign_component_default(input_name, input_component, env, depth, locals) {
            continue;
        }
    }

    Some(())
}

fn initialize_mixed_function_locals(
    function_def: &ast::ClassDef,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) {
    for (name, component) in &function_def.components {
        if locals.ints.contains_key(name.as_str()) || locals.bools.contains_key(name.as_str()) {
            continue;
        }
        assign_component_default(name, component, env, depth, locals);
    }
}

fn assign_component_default(
    name: &str,
    component: &ast::Component,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> bool {
    for expr in component
        .binding
        .iter()
        .chain(std::iter::once(&component.start))
    {
        if let Some(value) =
            eval_mixed_local_value(expr, env, depth, Some(&locals.ints), Some(&locals.bools))
        {
            insert_local_value(name, value, locals);
            return true;
        }
    }
    false
}

fn eval_mixed_local_value(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
    local_bools: Option<&FxHashMap<String, bool>>,
) -> Option<LocalValue> {
    if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
        expr,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        depth + 1,
        local_ints,
    ) {
        return Some(LocalValue::Integer(value));
    }
    try_eval_bool_expr_with_local_values(expr, env, depth + 1, local_ints, local_bools)
        .map(LocalValue::Bool)
}

fn insert_local_value(name: &str, value: LocalValue, locals: &mut MixedLocals) {
    match value {
        LocalValue::Integer(value) => {
            locals.bools.remove(name);
            locals.ints.insert(name.to_string(), value);
        }
        LocalValue::Bool(value) => {
            locals.ints.remove(name);
            locals.bools.insert(name.to_string(), value);
        }
    }
}

fn interpret_bool_function_statements(
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<bool> {
    for statement in statements {
        if interpret_bool_function_statement(statement, env, depth, locals)? {
            return Some(true);
        }
    }
    Some(false)
}

fn interpret_bool_function_statement(
    statement: &ast::Statement,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<bool> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    match statement {
        ast::Statement::Assignment { comp, value } => {
            let evaluated = eval_mixed_local_value(
                value,
                env,
                depth + 1,
                Some(&locals.ints),
                Some(&locals.bools),
            )?;
            insert_local_value(&comp.to_string(), evaluated, locals);
            Some(false)
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => interpret_bool_function_if(cond_blocks, else_block.as_deref(), env, depth, locals),
        ast::Statement::While(block) => interpret_bool_function_while(block, env, depth, locals),
        ast::Statement::Return { .. } => Some(true),
        ast::Statement::Empty | ast::Statement::Break { .. } => Some(false),
        ast::Statement::Assert { condition, .. } => try_eval_bool_expr_with_local_values(
            condition,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )
        .filter(|condition_holds| *condition_holds)
        .map(|_| false),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } if outputs.is_empty() && comp.to_string() == "assert" => args
            .first()
            .and_then(|condition| {
                try_eval_bool_expr_with_local_values(
                    condition,
                    env,
                    depth + 1,
                    Some(&locals.ints),
                    Some(&locals.bools),
                )
            })
            .filter(|condition_holds| *condition_holds)
            .map(|_| false),
        ast::Statement::For { .. }
        | ast::Statement::When(_)
        | ast::Statement::FunctionCall { .. }
        | ast::Statement::Reinit { .. } => None,
    }
}

fn interpret_bool_function_if(
    cond_blocks: &[rumoca_ir_ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<bool> {
    for block in cond_blocks {
        if try_eval_bool_expr_with_local_values(
            &block.cond,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )? {
            return interpret_bool_function_statements(&block.stmts, env, depth + 1, locals);
        }
    }
    if let Some(else_stmts) = else_block {
        return interpret_bool_function_statements(else_stmts, env, depth + 1, locals);
    }
    Some(false)
}

fn interpret_bool_function_while(
    block: &rumoca_ir_ast::StatementBlock,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<bool> {
    for _ in 0..MAX_FUNCTION_LOOP_ITERATIONS {
        if !try_eval_bool_expr_with_local_values(
            &block.cond,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )? {
            return Some(false);
        }
        if interpret_bool_function_statements(&block.stmts, env, depth + 1, locals)? {
            return Some(true);
        }
    }
    None
}

fn interpret_function_if(
    cond_blocks: &[rumoca_ir_ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    for block in cond_blocks {
        if try_eval_bool_expr_with_depth_and_locals(
            &block.cond,
            env,
            depth + 1,
            Some(local_values),
        )? {
            return interpret_function_statements(&block.stmts, env, depth + 1, local_values);
        }
    }
    if let Some(else_stmts) = else_block {
        return interpret_function_statements(else_stmts, env, depth + 1, local_values);
    }
    Some(false)
}

fn interpret_function_for(
    indices: &[rumoca_ir_ast::ForIndex],
    equations: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) -> Option<bool> {
    if indices.len() != 1 {
        return Some(false);
    }
    let index = &indices[0];
    let loop_name = index.ident.text.to_string();
    let values = evaluate_for_index_values(&index.range, env, depth + 1, Some(local_values))?;
    for value in values {
        local_values.insert(loop_name.clone(), value);
        if interpret_function_statements(equations, env, depth + 1, local_values)? {
            local_values.remove(&loop_name);
            return Some(true);
        }
    }
    local_values.remove(&loop_name);
    Some(false)
}

fn handle_function_call_statement(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    outputs: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_values: &mut FxHashMap<String, i64>,
) {
    let result = eval_integer_function_call(comp, args, env, depth + 1, Some(local_values));
    if let Some(value) = result
        && outputs.len() == 1
        && let ast::Expression::ComponentReference(output_ref) = &outputs[0]
    {
        local_values.insert(output_ref.to_string(), value);
    }
}

fn evaluate_for_index_values(
    range: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<Vec<i64>> {
    match range {
        ast::Expression::Range {
            start, step, end, ..
        } => {
            let start_value = try_eval_integer_expr_with_depth_and_locals(
                start,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
                local_ints,
            )?;
            let end_value = try_eval_integer_expr_with_depth_and_locals(
                end,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
                local_ints,
            )?;
            let step_value = if let Some(step_expr) = step {
                try_eval_integer_expr_with_depth_and_locals(
                    step_expr,
                    env.mod_env,
                    env.effective_components,
                    env.tree,
                    env.resolve_class_components,
                    depth + 1,
                    local_ints,
                )?
            } else {
                1
            };
            if step_value == 0 {
                return None;
            }

            let mut values = Vec::new();
            let mut current = start_value;
            if step_value > 0 {
                while current <= end_value {
                    values.push(current);
                    current += step_value;
                }
            } else {
                while current >= end_value {
                    values.push(current);
                    current += step_value;
                }
            }
            Some(values)
        }
        _ => {
            let end_value = try_eval_integer_expr_with_depth_and_locals(
                range,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
                local_ints,
            )?;
            (end_value >= 1).then(|| (1..=end_value).collect())
        }
    }
}

/// Evaluate array dimensions from shape_expr subscripts.
/// Returns None if any dimension cannot be evaluated.
///
/// MLS §10.1: Array dimensions can depend on parameters that are overridden
/// by modifications. We try evaluating shape_expr with the current mod_env first,
/// which handles cases like `Plug starpoints(m=mSystems)` where `mSystems=1`
/// overrides the default `m=3`. Falls back to pre-computed shape if shape_expr
/// evaluation fails.
pub fn evaluate_array_dimensions(
    shape: &[usize],
    shape_expr: &[rumoca_ir_ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
) -> Option<Vec<i64>> {
    // Prefer shape_expr because it reflects active modifications.
    // Fall back to precomputed shape only if expression evaluation fails.
    if !shape_expr.is_empty()
        && let Some(dims) = eval_shape_expr(
            shape_expr,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
        )
    {
        return Some(dims);
    }

    if !shape.is_empty() {
        return Some(shape.iter().map(|&d| d as i64).collect());
    }

    Some(vec![]) // Scalar
}

/// Try to evaluate shape_expr subscripts to concrete dimensions.
fn eval_shape_expr(
    shape_expr: &[rumoca_ir_ast::Subscript],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
) -> Option<Vec<i64>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for sub in shape_expr {
        match sub {
            rumoca_ir_ast::Subscript::Expression(expr) => {
                // Shape expressions may include structural `if` branches that are
                // valid at translation time for parameter-based dimensions (MLS §10.1).
                // Keep this behavior scoped to shape evaluation so general integer
                // expression evaluation remains unchanged.
                let dim = try_eval_integer_shape_expr(
                    expr,
                    mod_env,
                    effective_components,
                    tree,
                    resolve_class_components,
                )?;
                if dim < 0 {
                    return None;
                }
                dims.push(dim);
            }
            rumoca_ir_ast::Subscript::Range { .. } | rumoca_ir_ast::Subscript::Empty => {
                return None;
            }
        }
    }
    Some(dims)
}

pub fn try_eval_integer_shape_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
) -> Option<i64> {
    try_eval_integer_shape_expr_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        0,
    )
}

fn try_eval_integer_shape_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse = |e| {
        try_eval_integer_shape_expr_with_depth(
            e,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
        )
    };

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse::<i64>().ok(),
        ast::Expression::ComponentReference(comp_ref) => eval_integer_shape_component_ref(
            comp_ref,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth,
        ),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (cond, branch_expr) in branches {
                if !shape_condition_uses_static_components(cond, mod_env, effective_components) {
                    return None;
                }
                let condition_value = evaluate_component_condition_with_depth(
                    cond,
                    mod_env,
                    effective_components,
                    tree,
                    resolve_class_components,
                    depth + 1,
                )?;
                if condition_value {
                    return recurse(branch_expr);
                }
            }
            recurse(else_branch)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let l = recurse(lhs)?;
            let r = recurse(rhs)?;
            eval_integer_binary(op, l, r)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            let value = recurse(rhs)?;
            match op {
                rumoca_core::OpUnary::Minus => value.checked_neg(),
                rumoca_core::OpUnary::Plus => Some(value),
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::FunctionCall { .. } => try_eval_integer_expr_with_depth_and_locals(
            expr,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
            None,
        ),
        _ => None,
    }
}

fn eval_integer_shape_component_ref(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let recurse = |expr| {
        try_eval_integer_shape_expr_with_depth(
            expr,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
        )
    };

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    if let Some(mod_value) = mod_env.get(&param_path) {
        return recurse(&mod_value.value);
    }

    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        if let Some(component) = effective_components.get(name) {
            return recurse(super::component_expr_for_structural_eval(component)?);
        }
    }

    if let Some(component) = effective_components.get(dotted.as_str()) {
        return recurse(super::component_expr_for_structural_eval(component)?);
    }

    for candidate in super::enclosing_scope_candidates(dotted.as_str()) {
        let qn = ast::QualifiedName::from_dotted(&candidate);
        if let Some(mod_value) = mod_env.get(&qn) {
            return recurse(&mod_value.value);
        }
        if let Some(component) = effective_components.get(candidate.as_str()) {
            return recurse(super::component_expr_for_structural_eval(component)?);
        }
    }

    let env = super::IntegerEvalEnv {
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
    };
    if let Some(value) = super::eval_integer_class_redeclare_field_ref(comp_ref, env, depth, None) {
        return Some(value);
    }

    // Fall back to record-field resolution for paths like `data.n` where the
    // value lives in a record default/modification environment.
    super::eval_integer_record_field_ref(comp_ref, env, depth)
}

fn shape_condition_uses_static_components(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
) -> bool {
    match expr {
        ast::Expression::Terminal { .. } => true,
        ast::Expression::ComponentReference(comp_ref) => {
            shape_component_ref_is_static(comp_ref, mod_env, effective_components)
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            shape_condition_uses_static_components(lhs, mod_env, effective_components)
                && shape_condition_uses_static_components(rhs, mod_env, effective_components)
        }
        ast::Expression::Unary { rhs, .. } => {
            shape_condition_uses_static_components(rhs, mod_env, effective_components)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            shape_condition_uses_static_components(inner, mod_env, effective_components)
        }
        _ => false,
    }
}

fn shape_component_ref_is_static(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
) -> bool {
    if comp_ref.parts.is_empty() {
        return false;
    }

    let dotted = comp_ref
        .parts
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    let is_static_component = |component: &ast::Component| {
        component.is_structural
            || matches!(
                component.variability,
                rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
            )
    };

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    if mod_env.get(&param_path).is_some() {
        return true;
    }

    if let Some(component) = effective_components.get(dotted.as_str()) {
        return is_static_component(component);
    }

    if comp_ref.parts.len() == 1
        && let Some(component) = effective_components.get(comp_ref.parts[0].ident.text.as_ref())
    {
        return is_static_component(component);
    }

    // Unresolved refs in conditions are typically enum literals and are checked
    // by evaluate_component_condition_with_depth.
    true
}

/// Generate all array indices for multi-dimensional arrays.
/// For dims = `[2, 3]`, generates: `[[1,1], [1,2], [1,3], [2,1], [2,2], [2,3]]`.
/// Uses 1-based indexing per Modelica semantics (MLS §10.1).
pub fn generate_array_indices(dims: &[i64]) -> Vec<Vec<i64>> {
    if dims.is_empty() {
        return vec![]; // Scalar, no indices needed
    }

    let total: usize = dims.iter().map(|&d| d as usize).product();
    let mut result = Vec::with_capacity(total);

    // Generate all combinations using iterative approach
    let mut indices = vec![1i64; dims.len()];
    loop {
        result.push(indices.clone());

        // Increment indices from right to left (like counting)
        let mut i = dims.len();
        while i > 0 {
            i -= 1;
            indices[i] += 1;
            if indices[i] <= dims[i] {
                break;
            }
            // Carry over
            if i == 0 {
                return result; // All combinations generated
            }
            indices[i] = 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::super::{
        InstantiateEvalCtx, enum_values_equal, eval_integer_binary, evaluate_component_condition,
        try_eval_integer_expr,
    };
    use super::evaluate_array_dimensions;
    use rumoca_ir_ast as ast;
    use rumoca_ir_ast::AstIndexMap as IndexMap;

    fn no_op_resolve_class_components(
        _tree: &ast::ClassTree,
        class: &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component> {
        class.components.clone()
    }

    #[test]
    fn enum_values_equal_accepts_different_qualification_prefixes() {
        let a = "sensor_frame_a2.MultiBody.Types.ResolveInFrameA.frame_resolve";
        let b = "Modelica.Mechanics.MultiBody.Types.ResolveInFrameA.frame_resolve";
        assert!(enum_values_equal(a, b));
    }

    #[test]
    fn enum_values_equal_rejects_different_enum_types() {
        let a = "Modelica.Blocks.Types.SimpleController.PI";
        let b = "Modelica.Blocks.Types.Init.PI";
        assert!(!enum_values_equal(a, b));
    }

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text),
            ..rumoca_core::Token::default()
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("instantiate_function_eval_test.mo"),
            1,
            2,
        )
    }

    fn cref(path: &str) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: rumoca_core::ComponentPath::from_flat_path(path)
                .into_parts()
                .into_iter()
                .map(|part| ast::ComponentRefPart {
                    ident: token(&part),
                    subs: None,
                })
                .collect(),
            def_id: None,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn eq_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Eq,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn add_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn mul_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn lt_expr(lhs: ast::Expression, rhs: ast::Expression) -> ast::Expression {
        ast::Expression::Binary {
            op: rumoca_core::OpBinary::Lt,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn func_call(path: &str, args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: cref(path),
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn if_expr(
        branches: Vec<(ast::Expression, ast::Expression)>,
        else_branch: ast::Expression,
    ) -> ast::Expression {
        ast::Expression::If {
            branches,
            else_branch: Arc::new(else_branch),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn int_expr(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: token(&value.to_string()),

            span: rumoca_core::Span::DUMMY,
        }
    }

    fn bool_expr(value: bool) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token: token(if value { "true" } else { "false" }),

            span: rumoca_core::Span::DUMMY,
        }
    }

    fn input_int_component(name: &str) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            causality: rumoca_core::Causality::Input(token("input")),
            variability: rumoca_core::Variability::Parameter(token("parameter")),
            ..ast::Component::empty_with_span(test_span())
        }
    }

    fn output_bool_component(name: &str) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            causality: rumoca_core::Causality::Output(token("output")),
            start: bool_expr(false),
            ..ast::Component::empty_with_span(test_span())
        }
    }

    fn output_int_component(name: &str) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            causality: rumoca_core::Causality::Output(token("output")),
            start: int_expr(0),
            ..ast::Component::empty_with_span(test_span())
        }
    }

    fn local_int_component(name: &str, start: i64) -> ast::Component {
        ast::Component {
            name: name.to_string(),
            start: int_expr(start),
            ..ast::Component::empty_with_span(test_span())
        }
    }

    fn assignment(path: &str, value: ast::Expression) -> ast::Statement {
        ast::Statement::Assignment {
            comp: cref(path),
            value,
        }
    }

    fn msl_math_tree() -> ast::ClassTree {
        let modelica_id = rumoca_core::DefId::new(1);
        let math_id = rumoca_core::DefId::new(2);
        let function_id = rumoca_core::DefId::new(3);

        let function = msl_is_power_of_two_function(function_id);

        let mut math = ast::ClassDef {
            def_id: Some(math_id),
            name: token("Math"),
            class_type: rumoca_core::ClassType::Package,
            ..ast::ClassDef::default()
        };
        math.classes.insert("isPowerOf2".to_string(), function);

        let mut modelica = ast::ClassDef {
            def_id: Some(modelica_id),
            name: token("Modelica"),
            class_type: rumoca_core::ClassType::Package,
            ..ast::ClassDef::default()
        };
        modelica.classes.insert("Math".to_string(), math);

        let mut tree = ast::ClassTree::new();
        tree.definitions
            .classes
            .insert("Modelica".to_string(), modelica);
        tree.def_map.insert(modelica_id, "Modelica".to_string());
        tree.def_map.insert(math_id, "Modelica.Math".to_string());
        tree.def_map
            .insert(function_id, "Modelica.Math.isPowerOf2".to_string());
        tree.name_map.insert("Modelica".to_string(), modelica_id);
        tree.name_map.insert("Modelica.Math".to_string(), math_id);
        tree.name_map
            .insert("Modelica.Math.isPowerOf2".to_string(), function_id);
        tree
    }

    fn msl_is_power_of_two_function(function_id: rumoca_core::DefId) -> ast::ClassDef {
        let mut function = ast::ClassDef {
            def_id: Some(function_id),
            name: token("isPowerOf2"),
            class_type: rumoca_core::ClassType::Function,
            pure: true,
            ..ast::ClassDef::default()
        };
        function
            .components
            .insert("i".to_string(), input_int_component("i"));
        function
            .components
            .insert("result".to_string(), output_bool_component("result"));
        function
            .components
            .insert("target".to_string(), local_int_component("target", 0));
        function
            .components
            .insert("powOf2".to_string(), local_int_component("powOf2", 1));
        function.algorithms.push(msl_is_power_of_two_algorithm());
        function
    }

    fn msl_is_power_of_two_algorithm() -> Vec<ast::Statement> {
        vec![
            ast::Statement::FunctionCall {
                comp: cref("assert"),
                args: vec![
                    rumoca_ir_ast::Expression::Binary {
                        op: rumoca_core::OpBinary::Ge,
                        lhs: Arc::new(ast::Expression::ComponentReference(cref("i"))),
                        rhs: Arc::new(int_expr(1)),
                        span: rumoca_core::Span::DUMMY,
                    },
                    bool_expr(true),
                ],
                outputs: Vec::new(),
            },
            ast::Statement::If {
                cond_blocks: vec![ast::StatementBlock {
                    cond: eq_expr(
                        func_call(
                            "mod",
                            vec![ast::Expression::ComponentReference(cref("i")), int_expr(2)],
                        ),
                        int_expr(1),
                    ),
                    stmts: vec![assignment(
                        "result",
                        eq_expr(ast::Expression::ComponentReference(cref("i")), int_expr(1)),
                    )],
                }],
                else_block: Some(msl_is_power_of_two_even_branch()),
            },
        ]
    }

    fn msl_is_power_of_two_even_branch() -> Vec<ast::Statement> {
        vec![
            assignment(
                "target",
                func_call(
                    "div",
                    vec![ast::Expression::ComponentReference(cref("i")), int_expr(2)],
                ),
            ),
            assignment("powOf2", int_expr(1)),
            ast::Statement::While(ast::StatementBlock {
                cond: lt_expr(
                    ast::Expression::ComponentReference(cref("powOf2")),
                    ast::Expression::ComponentReference(cref("target")),
                ),
                stmts: vec![assignment(
                    "powOf2",
                    mul_expr(
                        ast::Expression::ComponentReference(cref("powOf2")),
                        int_expr(2),
                    ),
                )],
            }),
            assignment(
                "result",
                eq_expr(
                    ast::Expression::ComponentReference(cref("target")),
                    ast::Expression::ComponentReference(cref("powOf2")),
                ),
            ),
        ]
    }

    #[test]
    fn integer_div_operator_requires_exact_quotient() {
        assert_eq!(
            eval_integer_binary(&rumoca_core::OpBinary::Div, 8, 2),
            Some(4)
        );
        assert_eq!(eval_integer_binary(&rumoca_core::OpBinary::Div, 7, 2), None);
    }

    #[test]
    fn integer_div_builtin_remains_truncating() {
        let expr = ast::Expression::FunctionCall {
            comp: cref("div"),
            args: vec![int_expr(7), int_expr(2)],
            span: rumoca_core::Span::DUMMY,
        };
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: &no_op_resolve_class_components,
        };

        assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(3));
    }

    #[test]
    fn unqualified_unique_function_call_evaluates_without_def_id() {
        let tree = msl_math_tree();
        let ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: &no_op_resolve_class_components,
        };
        let expr = if_expr(
            vec![(
                ast::Expression::FunctionCall {
                    comp: cref("isPowerOf2"),
                    args: vec![int_expr(8)],
                    span: rumoca_core::Span::DUMMY,
                },
                int_expr(1),
            )],
            int_expr(0),
        );

        assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(1));
    }

    #[test]
    fn scalar_integer_eval_rejects_array_output_functions() {
        let function_id = rumoca_core::DefId::new(1);
        let mut function = ast::ClassDef {
            def_id: Some(function_id),
            name: token("arrayInteger"),
            class_type: rumoca_core::ClassType::Function,
            pure: true,
            ..ast::ClassDef::default()
        };
        let mut output = output_int_component("y");
        output.shape_expr = vec![ast::Subscript::Expression(int_expr(2))];
        function.components.insert("y".to_string(), output);
        function
            .algorithms
            .push(vec![assignment("y[1]", int_expr(1))]);

        let mut tree = ast::ClassTree::new();
        tree.definitions
            .classes
            .insert("arrayInteger".to_string(), function);
        tree.def_map.insert(function_id, "arrayInteger".to_string());
        tree.name_map
            .insert("arrayInteger".to_string(), function_id);

        let expr = func_call("arrayInteger", Vec::new());
        let ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: &no_op_resolve_class_components,
        };

        assert_eq!(try_eval_integer_expr(&ctx, &expr), None);
    }

    #[test]
    fn evaluate_component_condition_with_resolved_enum_ref() {
        let mut components = IndexMap::default();
        let mut model_structure = ast::Component {
            name: "modelStructure".to_string(),
            ..ast::Component::empty_with_span(test_span())
        };
        model_structure.start =
            ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb"));
        components.insert("modelStructure".to_string(), model_structure);

        let condition = eq_expr(
            ast::Expression::ComponentReference(cref("modelStructure")),
            ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
        );
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &components,
            resolve_class_components: &no_op_resolve_class_components,
        };
        let value = evaluate_component_condition(&ctx, &condition);

        assert_eq!(value, Some(true));
    }

    #[test]
    fn evaluate_component_condition_uses_declaration_binding() {
        let mut components = IndexMap::default();
        components.insert(
            "use_numberPort".to_string(),
            ast::Component {
                name: "use_numberPort".to_string(),
                type_name: ast::Name::from_string("Boolean"),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                binding: Some(bool_expr(true)),
                has_explicit_binding: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let condition = ast::Expression::ComponentReference(cref("use_numberPort"));
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &components,
            resolve_class_components: &no_op_resolve_class_components,
        };

        assert_eq!(evaluate_component_condition(&ctx, &condition), Some(true));
    }

    #[test]
    fn evaluate_component_condition_with_unresolved_enum_ref_is_unknown() {
        let mut components = IndexMap::default();
        let model_structure = ast::Component {
            name: "modelStructure".to_string(),
            ..ast::Component::empty_with_span(test_span())
        };
        components.insert("modelStructure".to_string(), model_structure);

        let condition = eq_expr(
            ast::Expression::ComponentReference(cref("modelStructure")),
            ast::Expression::ComponentReference(cref("Types.ModelStructure.a_vb")),
        );
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &components,
            resolve_class_components: &no_op_resolve_class_components,
        };
        let value = evaluate_component_condition(&ctx, &condition);

        assert_eq!(value, None);
    }

    #[test]
    fn evaluate_array_dimensions_supports_structural_if_shape_refs() {
        let mut components = IndexMap::default();
        components.insert(
            "useLumpedPressure".to_string(),
            ast::Component {
                name: "useLumpedPressure".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                start: bool_expr(false),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "nFMLumped".to_string(),
            ast::Component {
                name: "nFMLumped".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                start: int_expr(2),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "nFMDistributed".to_string(),
            ast::Component {
                name: "nFMDistributed".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                start: int_expr(1),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "nFM".to_string(),
            ast::Component {
                name: "nFM".to_string(),
                start: if_expr(
                    vec![(
                        ast::Expression::ComponentReference(cref("useLumpedPressure")),
                        ast::Expression::ComponentReference(cref("nFMLumped")),
                    )],
                    ast::Expression::ComponentReference(cref("nFMDistributed")),
                ),
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let dims = evaluate_array_dimensions(
            &[1],
            &[ast::Subscript::Expression(add_expr(
                ast::Expression::ComponentReference(cref("nFM")),
                int_expr(1),
            ))],
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
            &no_op_resolve_class_components,
        );

        assert_eq!(dims, Some(vec![2]));
    }

    #[test]
    fn evaluate_array_dimensions_rejects_runtime_if_shape_condition() {
        let mut components = IndexMap::default();
        components.insert(
            "runtimeSwitch".to_string(),
            ast::Component {
                name: "runtimeSwitch".to_string(),
                start: bool_expr(false),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "nA".to_string(),
            ast::Component {
                name: "nA".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                start: int_expr(2),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "nB".to_string(),
            ast::Component {
                name: "nB".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                start: int_expr(1),
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "n".to_string(),
            ast::Component {
                name: "n".to_string(),
                start: if_expr(
                    vec![(
                        ast::Expression::ComponentReference(cref("runtimeSwitch")),
                        ast::Expression::ComponentReference(cref("nA")),
                    )],
                    ast::Expression::ComponentReference(cref("nB")),
                ),
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let dims = evaluate_array_dimensions(
            &[1],
            &[ast::Subscript::Expression(add_expr(
                ast::Expression::ComponentReference(cref("n")),
                int_expr(1),
            ))],
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
            &no_op_resolve_class_components,
        );

        // Non-compile-time condition should keep dimension-expression evaluation
        // disabled and preserve the precomputed shape.
        assert_eq!(dims, Some(vec![1]));
    }

    #[test]
    fn try_eval_integer_expr_prefers_binding_over_start_for_component_refs() {
        let mut components = IndexMap::default();
        components.insert(
            "n".to_string(),
            ast::Component {
                name: "n".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                // Unresolvable start should not override explicit binding.
                start: ast::Expression::ComponentReference(cref("missing.scope.value")),
                binding: Some(int_expr(1)),
                has_explicit_binding: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &components,
            resolve_class_components: &no_op_resolve_class_components,
        };

        let value = try_eval_integer_expr(
            &ctx,
            &add_expr(ast::Expression::ComponentReference(cref("n")), int_expr(1)),
        );

        assert_eq!(value, Some(2));
    }

    #[test]
    fn evaluate_array_dimensions_prefers_binding_over_start_for_shape_refs() {
        let mut components = IndexMap::default();
        components.insert(
            "m".to_string(),
            ast::Component {
                name: "m".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                // Keep start unresolved and provide the structural value via binding.
                start: ast::Expression::ComponentReference(cref("missing.scope.value")),
                binding: Some(int_expr(1)),
                has_explicit_binding: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let dims = evaluate_array_dimensions(
            &[0],
            &[ast::Subscript::Expression(add_expr(
                ast::Expression::ComponentReference(cref("m")),
                int_expr(1),
            ))],
            &ast::ModificationEnvironment::new(),
            &components,
            &ast::ClassTree::new(),
            &no_op_resolve_class_components,
        );

        assert_eq!(dims, Some(vec![2]));
    }

    #[test]
    fn evaluate_array_dimensions_reads_record_field_from_class_modification() {
        let mut components = IndexMap::default();
        components.insert(
            "stackData".to_string(),
            ast::Component {
                name: "stackData".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                ..ast::Component::empty_with_span(test_span())
            },
        );

        let mut mod_env = ast::ModificationEnvironment::new();
        let stack_data_mod = ast::Expression::ClassModification {
            target: cref("StackData"),
            modifications: vec![
                ast::Expression::NamedArgument {
                    name: token("Ns"),
                    value: Arc::new(int_expr(3)),

                    span: rumoca_core::Span::DUMMY,
                },
                ast::Expression::NamedArgument {
                    name: token("Np"),
                    value: Arc::new(int_expr(2)),

                    span: rumoca_core::Span::DUMMY,
                },
            ],

            each_flags: vec![false, false],
            final_flags: vec![false, false],
            redeclare_flags: vec![false, false],
            span: rumoca_core::Span::DUMMY,
        };
        mod_env.add(
            ast::QualifiedName::from_ident("stackData"),
            ast::ModificationValue::simple(stack_data_mod),
        );

        let dims = evaluate_array_dimensions(
            &[1, 1],
            &[
                ast::Subscript::Expression(ast::Expression::ComponentReference(cref(
                    "stackData.Ns",
                ))),
                ast::Subscript::Expression(ast::Expression::ComponentReference(cref(
                    "stackData.Np",
                ))),
            ],
            &mod_env,
            &components,
            &ast::ClassTree::new(),
            &no_op_resolve_class_components,
        );

        assert_eq!(dims, Some(vec![3, 2]));
    }

    #[test]
    fn try_eval_integer_expr_resolves_enclosing_scope_component_ref() {
        let mut components = IndexMap::default();
        components.insert(
            "pipe2.nFM".to_string(),
            ast::Component {
                name: "pipe2.nFM".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                start: int_expr(1),
                binding: Some(int_expr(1)),
                has_explicit_binding: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &components,
            resolve_class_components: &no_op_resolve_class_components,
        };

        let value = try_eval_integer_expr(
            &ctx,
            &add_expr(
                ast::Expression::ComponentReference(cref("pipe2.flowModel.nFM")),
                int_expr(1),
            ),
        );

        assert_eq!(value, Some(2));
    }

    #[test]
    fn try_eval_integer_expr_evaluates_if_expressions() {
        let expr = if_expr(vec![(bool_expr(true), int_expr(2))], int_expr(1));
        let ctx = InstantiateEvalCtx {
            tree: &ast::ClassTree::new(),
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &IndexMap::default(),
            resolve_class_components: &no_op_resolve_class_components,
        };
        let value = try_eval_integer_expr(&ctx, &expr);

        assert_eq!(value, Some(2));
    }

    #[test]
    fn try_eval_integer_expr_evaluates_parameterized_if_expressions() {
        let mut components = IndexMap::default();
        components.insert(
            "ParDesired".to_string(),
            ast::Component {
                name: "ParDesired".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                binding: Some(int_expr(2)),
                has_explicit_binding: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );
        components.insert(
            "mSystems".to_string(),
            ast::Component {
                name: "mSystems".to_string(),
                variability: rumoca_core::Variability::Parameter(token("parameter")),
                binding: Some(int_expr(2)),
                has_explicit_binding: true,
                ..ast::Component::empty_with_span(test_span())
            },
        );
        let expr = if_expr(
            vec![(
                func_call(
                    "Modelica.Math.isPowerOf2",
                    vec![ast::Expression::ComponentReference(cref("ParDesired"))],
                ),
                ast::Expression::ComponentReference(cref("ParDesired")),
            )],
            ast::Expression::ComponentReference(cref("mSystems")),
        );
        let tree = msl_math_tree();
        let ctx = InstantiateEvalCtx {
            tree: &tree,
            mod_env: &ast::ModificationEnvironment::new(),
            effective_components: &components,
            resolve_class_components: &no_op_resolve_class_components,
        };

        assert_eq!(try_eval_integer_expr(&ctx, &expr), Some(2));
    }
}
