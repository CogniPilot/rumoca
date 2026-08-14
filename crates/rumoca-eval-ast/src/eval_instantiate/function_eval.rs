use super::{
    IntegerEvalEnv, MAX_EXPR_EVAL_DEPTH, ast, eval_integer_binary, eval_integer_function_call,
    evaluate_component_condition_with_depth, evaluate_enum_equality_with_depth,
    try_eval_bool_expr_with_local_values, try_eval_integer_expr_with_depth_and_locals,
};
use crate::ast_scalar::{self, AstScalarContext};
use crate::function_control::FunctionStmtFlow;
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

pub(super) fn eval_user_defined_integer_function(
    function_def: &ast::ClassDef,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    caller_locals: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
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
            ints: caller_locals,
            bools: None,
        },
        &mut locals,
    )?;
    initialize_mixed_function_locals(function_def, env, depth + 1, &mut locals);
    let output_name = find_scalar_function_output_name(function_def)?;
    interpret_function_algorithms(function_def, env, depth + 1, &mut locals)?;
    locals.ints.get(&output_name).copied()
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
    let output_name = find_scalar_function_output_name(function_def)?;
    interpret_function_algorithms(function_def, env, depth + 1, &mut locals)?;
    locals.bools.get(&output_name).copied()
}

fn find_scalar_function_output_name(function_def: &ast::ClassDef) -> Option<String> {
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

fn interpret_function_algorithms(
    function_def: &ast::ClassDef,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<()> {
    for algorithm in &function_def.algorithms {
        match interpret_function_statements(algorithm, env, depth + 1, locals)? {
            FunctionStmtFlow::Continue => {}
            FunctionStmtFlow::Return => return Some(()),
            FunctionStmtFlow::Break => return None,
        }
    }
    Some(())
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
        let (name, _) = inputs.get(positional_idx)?;
        let value =
            eval_mixed_local_value(arg, env, depth, caller_locals.ints, caller_locals.bools)?;
        insert_local_value(name, value, locals);
        positional_idx += 1;
    }

    for arg in args {
        if let ast::Expression::NamedArgument { name, value, .. } = arg {
            let (param_name, _) = inputs
                .iter()
                .find(|(input_name, _)| input_name.as_str() == name.text.as_ref())
                .copied()?;
            if locals.ints.contains_key(param_name.as_str())
                || locals.bools.contains_key(param_name.as_str())
            {
                return None;
            }
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
        return None;
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

/// Seed `name` from its declaration binding (MLS §12.4.1 default argument /
/// declaration assignment).
///
/// The `start` attribute is deliberately not consulted. The parser seeds every
/// `Real`/`Integer`/`Boolean` declaration with `0.0`/`0`/`false`, so reading it
/// would hand an unsupplied input or an unassigned local a value the function
/// never defined — a fabricated result rather than an undecided one
/// (SPEC_0008). Without a binding this returns `false` and the caller abandons
/// the fold.
fn assign_component_default(
    name: &str,
    component: &ast::Component,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> bool {
    let Some(binding) = component.binding.as_ref() else {
        return false;
    };
    let Some(value) =
        eval_mixed_local_value(binding, env, depth, Some(&locals.ints), Some(&locals.bools))
    else {
        return false;
    };
    insert_local_value(name, value, locals);
    true
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

fn interpret_function_statements(
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
    for statement in statements {
        let flow = interpret_function_statement(statement, env, depth, locals)?;
        if flow != FunctionStmtFlow::Continue {
            return Some(flow);
        }
    }
    Some(FunctionStmtFlow::Continue)
}

fn interpret_function_statement(
    statement: &ast::Statement,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
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
            Some(FunctionStmtFlow::Continue)
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => interpret_function_if(cond_blocks, else_block.as_deref(), env, depth, locals),
        ast::Statement::For { indices, equations } => {
            interpret_function_for(indices, equations, env, depth, locals)
        }
        ast::Statement::While(block) => interpret_function_while(block, env, depth, locals),
        ast::Statement::Break { .. } => Some(FunctionStmtFlow::Break),
        ast::Statement::Return { .. } => Some(FunctionStmtFlow::Return),
        ast::Statement::Empty => Some(FunctionStmtFlow::Continue),
        ast::Statement::Assert { condition, .. } => try_eval_bool_expr_with_local_values(
            condition,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )
        .filter(|condition_holds| *condition_holds)
        .map(|_| FunctionStmtFlow::Continue),
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
            .map(|_| FunctionStmtFlow::Continue),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => interpret_function_call(comp, args, outputs, env, depth, locals),
        ast::Statement::When(_) | ast::Statement::Reinit { .. } => None,
    }
}

fn interpret_function_if(
    cond_blocks: &[rumoca_ir_ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
    for block in cond_blocks {
        if try_eval_bool_expr_with_local_values(
            &block.cond,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )? {
            return interpret_function_statements(&block.stmts, env, depth + 1, locals);
        }
    }
    if let Some(else_stmts) = else_block {
        return interpret_function_statements(else_stmts, env, depth + 1, locals);
    }
    Some(FunctionStmtFlow::Continue)
}

fn interpret_function_for(
    indices: &[rumoca_ir_ast::ForIndex],
    statements: &[ast::Statement],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
    if indices.len() != 1 {
        return None;
    }
    let index = &indices[0];
    let loop_name = index.ident.text.to_string();
    let values = evaluate_for_index_values(&index.range, env, depth + 1, Some(&locals.ints))?;
    for value in values {
        locals.ints.insert(loop_name.clone(), value);
        match interpret_function_statements(statements, env, depth + 1, locals)? {
            FunctionStmtFlow::Continue => {}
            FunctionStmtFlow::Break => {
                locals.ints.remove(&loop_name);
                return Some(FunctionStmtFlow::Continue);
            }
            FunctionStmtFlow::Return => {
                locals.ints.remove(&loop_name);
                return Some(FunctionStmtFlow::Return);
            }
        }
    }
    locals.ints.remove(&loop_name);
    Some(FunctionStmtFlow::Continue)
}

fn interpret_function_while(
    block: &rumoca_ir_ast::StatementBlock,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
    for _ in 0..MAX_FUNCTION_LOOP_ITERATIONS {
        if !try_eval_bool_expr_with_local_values(
            &block.cond,
            env,
            depth + 1,
            Some(&locals.ints),
            Some(&locals.bools),
        )? {
            return Some(FunctionStmtFlow::Continue);
        }
        match interpret_function_statements(&block.stmts, env, depth + 1, locals)? {
            FunctionStmtFlow::Continue => {}
            FunctionStmtFlow::Break => return Some(FunctionStmtFlow::Continue),
            FunctionStmtFlow::Return => return Some(FunctionStmtFlow::Return),
        }
    }
    None
}

fn interpret_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    outputs: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &mut MixedLocals,
) -> Option<FunctionStmtFlow> {
    let [ast::Expression::ComponentReference(output_ref)] = outputs else {
        return None;
    };
    let value = eval_integer_function_call(comp, args, env, depth + 1, Some(&locals.ints))?;
    insert_local_value(&output_ref.to_string(), LocalValue::Integer(value), locals);
    Some(FunctionStmtFlow::Continue)
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
            collect_integer_range(start_value, step_value, end_value)
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
            (end_value >= 1 && end_value <= MAX_FUNCTION_LOOP_ITERATIONS as i64)
                .then(|| (1..=end_value).collect())
        }
    }
}

fn collect_integer_range(start: i64, step: i64, end: i64) -> Option<Vec<i64>> {
    let mut values = Vec::new();
    let mut current = start;
    while if step > 0 {
        current <= end
    } else {
        current >= end
    } {
        if values.len() == MAX_FUNCTION_LOOP_ITERATIONS {
            return None;
        }
        values.push(current);
        if current == end {
            break;
        }
        current = current.checked_add(step)?;
    }
    Some(values)
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
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
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
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
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
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
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
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let adapter = ShapeScalarAdapter {
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
    };
    ast_scalar::eval_integer(expr, &adapter, "", depth)
}

struct ShapeScalarAdapter<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a ast::AstIndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    resolve_class_components:
        fn(&ast::ClassTree, &ast::ClassDef) -> ast::AstIndexMap<String, ast::Component>,
}

impl AstScalarContext for ShapeScalarAdapter<'_> {
    fn expression_depth_limit(&self) -> Option<usize> {
        Some(MAX_EXPR_EVAL_DEPTH)
    }

    fn lookup_integer(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<i64> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        eval_integer_shape_component_ref(
            reference,
            self.mod_env,
            self.effective_components,
            self.tree,
            self.resolve_class_components,
            depth,
        )
    }

    fn lookup_boolean(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<bool> {
        evaluate_component_condition_with_depth(
            expr,
            self.mod_env,
            self.effective_components,
            self.tree,
            self.resolve_class_components,
            depth,
        )
    }

    fn call_integer(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        _scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        eval_integer_function_call(
            function,
            args,
            IntegerEvalEnv {
                mod_env: self.mod_env,
                effective_components: self.effective_components,
                tree: self.tree,
                resolve_class_components: self.resolve_class_components,
            },
            depth,
            None,
        )
    }

    fn enum_equal(
        &self,
        lhs: &ast::Expression,
        rhs: &ast::Expression,
        _scope: &str,
        depth: usize,
    ) -> Option<bool> {
        evaluate_enum_equality_with_depth(
            lhs,
            rhs,
            self.mod_env,
            self.effective_components,
            self.tree,
            self.resolve_class_components,
            depth,
        )
    }

    fn integer_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: i64,
        rhs: i64,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        eval_integer_binary(op, lhs, rhs)
    }

    fn boolean_expression_allowed(&self, expr: &ast::Expression) -> bool {
        shape_condition_uses_static_components(expr, self.mod_env, self.effective_components)
    }
}

fn eval_integer_shape_component_ref(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &ast::AstIndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> ast::AstIndexMap<String, ast::Component>,
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

#[cfg(test)]
mod tests;
