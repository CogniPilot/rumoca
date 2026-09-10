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
    Real(f64),
    /// A rank-1 Real vector. Higher ranks and non-Real element types stay
    /// outside this interpreter and fail closed.
    Reals(Vec<f64>),
}

#[derive(Default)]
struct MixedLocals {
    ints: FxHashMap<String, i64>,
    bools: FxHashMap<String, bool>,
    reals: FxHashMap<String, f64>,
    real_arrays: FxHashMap<String, Vec<f64>>,
}

impl MixedLocals {
    fn contains(&self, name: &str) -> bool {
        self.ints.contains_key(name)
            || self.bools.contains_key(name)
            || self.reals.contains_key(name)
            || self.real_arrays.contains_key(name)
    }
}

#[derive(Copy, Clone, Default)]
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
        let (name, component) = inputs.get(positional_idx)?;
        let value =
            eval_mixed_local_value(arg, env, depth, caller_locals.ints, caller_locals.bools)?;
        insert_checked_local_value(name, component, value, env, locals)?;
        positional_idx += 1;
    }

    for arg in args {
        if let ast::Expression::NamedArgument { name, value, .. } = arg {
            let (param_name, param_component) = inputs
                .iter()
                .find(|(input_name, _)| input_name.as_str() == name.text.as_ref())
                .copied()?;
            if locals.contains(param_name.as_str()) {
                return None;
            }
            let input_value =
                eval_mixed_local_value(value, env, depth, caller_locals.ints, caller_locals.bools)?;
            insert_checked_local_value(param_name, param_component, input_value, env, locals)?;
        }
    }

    for (input_name, input_component) in inputs {
        if locals.contains(input_name.as_str()) {
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
        if locals.contains(name.as_str()) {
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
    // The binding runs in the callee frame: MLS §12.4.4 lets a protected
    // `Integer n = size(v1, 1)` read the already-bound inputs.
    let Some(value) = eval_function_expr(binding, env, depth, locals) else {
        return false;
    };
    insert_checked_local_value(name, component, value, env, locals).is_some()
}

/// Insert a value into the typed frame only when it satisfies the formal's
/// checked declaration contract.
///
/// The scalar Integer/Bool lanes keep their long-standing behavior. The new
/// Real lanes are validated against the declaration before binding:
/// a Real scalar needs a declared-scalar, provably Real formal; a rank-1
/// Real vector needs a declared rank-1, provably Real formal whose extent —
/// when the declaration states one statically — matches the value's length
/// (a declared `[:]` accepts any length). Real-ness is proven only by the
/// type name's resolved identity equalling the tree's registered predefined
/// `Real` DefId; absent identity on either side — and every derived alias —
/// fails closed rather than binding a value the declaration never admitted.
fn insert_checked_local_value(
    name: &str,
    component: &ast::Component,
    value: LocalValue,
    env: IntegerEvalEnv<'_>,
    locals: &mut MixedLocals,
) -> Option<()> {
    match &value {
        LocalValue::Integer(_) | LocalValue::Bool(_) => {}
        LocalValue::Real(_) => {
            if !component_is_predefined_real(component, env) || declared_rank(component) != 0 {
                return None;
            }
        }
        LocalValue::Reals(values) => {
            if !component_is_predefined_real(component, env) || declared_rank(component) != 1 {
                return None;
            }
            match declared_vector_extent(component) {
                DeclaredExtent::Any => {}
                DeclaredExtent::Fixed(extent) if extent == values.len() => {}
                DeclaredExtent::Fixed(_) | DeclaredExtent::Unproved => return None,
            }
        }
    }
    insert_local_value(name, value, locals);
    Some(())
}

fn component_is_predefined_real(component: &ast::Component, env: IntegerEvalEnv<'_>) -> bool {
    // Identity only, never spelling: the declared type must carry the exact
    // DefId the tree registered for predefined `Real`. Absent identity on
    // either side fails closed — an unresolved or user-owned declaration
    // spelled `Real` must not acquire predefined semantics. Derived Real
    // aliases are a stated fail-closed coverage boundary of this slice.
    let predefined_real = env
        .tree
        .scope_tree
        .predefined_member(&rumoca_core::ComponentPath::from_flat_path("Real"));
    match (component.type_name.def_id, predefined_real) {
        (Some(declared), Some(real)) => declared == real,
        _ => false,
    }
}

fn declared_rank(component: &ast::Component) -> usize {
    component.shape.len().max(component.shape_expr.len())
}

enum DeclaredExtent {
    Any,
    Fixed(usize),
    Unproved,
}

fn declared_vector_extent(component: &ast::Component) -> DeclaredExtent {
    if let [extent] = component.shape.as_slice() {
        return DeclaredExtent::Fixed(*extent);
    }
    match component.shape_expr.as_slice() {
        // `[:]` parses as the colon Range subscript; Empty is the
        // no-subscript placeholder synthetic declarations carry. Both state
        // an unknown extent that any length satisfies (MLS §12.4.5).
        [ast::Subscript::Empty] | [ast::Subscript::Range { .. }] => DeclaredExtent::Any,
        [
            ast::Subscript::Expression(ast::Expression::Terminal {
                terminal_type: ast::TerminalType::UnsignedInteger,
                token,
                ..
            }),
        ] => token
            .text
            .parse::<usize>()
            .map_or(DeclaredExtent::Unproved, DeclaredExtent::Fixed),
        _ => DeclaredExtent::Unproved,
    }
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
    if let Some(value) =
        try_eval_bool_expr_with_local_values(expr, env, depth + 1, local_ints, local_bools)
    {
        return Some(LocalValue::Bool(value));
    }
    // Real scalars and rank-1 Real vector literals. Call arguments cannot
    // read the callee's locals, so the empty typed frame is the correct
    // scope; only Real-typed results are accepted from it, the scalar paths
    // above having already answered for int/bool.
    let empty = MixedLocals::default();
    match eval_function_expr(expr, env, depth + 1, &empty) {
        Some(value @ (LocalValue::Real(_) | LocalValue::Reals(_))) => Some(value),
        _ => None,
    }
}

fn insert_local_value(name: &str, value: LocalValue, locals: &mut MixedLocals) {
    locals.ints.remove(name);
    locals.bools.remove(name);
    locals.reals.remove(name);
    locals.real_arrays.remove(name);
    match value {
        LocalValue::Integer(value) => {
            locals.ints.insert(name.to_string(), value);
        }
        LocalValue::Bool(value) => {
            locals.bools.insert(name.to_string(), value);
        }
        LocalValue::Real(value) => {
            locals.reals.insert(name.to_string(), value);
        }
        LocalValue::Reals(values) => {
            locals.real_arrays.insert(name.to_string(), values);
        }
    }
}

/// Evaluate a function-body expression over the typed local frame.
///
/// This carries the value forms ordinary pure AST function interpretation
/// needs beyond the scalar int/bool paths: Real literals and arithmetic,
/// rank-1 Real vector literals, references to typed locals (including a
/// one-subscript indexed read of a local vector), `size` of a local vector,
/// `abs`, mixed-type comparisons, and if-expressions. The existing scalar
/// evaluators answer first; every unsupported form fails closed.
fn eval_function_expr(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &MixedLocals,
) -> Option<LocalValue> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    // The scalar paths receive `depth` unincremented: this dispatcher is not
    // a semantic recursion level, and adding one here shortened the shared
    // MAX_EXPR_EVAL_DEPTH budget enough to break recursive-function folds
    // that fit before it existed. Structural descent below still increments.
    if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
        expr,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        depth,
        Some(&locals.ints),
    ) {
        return Some(LocalValue::Integer(value));
    }
    if let Some(value) = try_eval_bool_expr_with_local_values(
        expr,
        env,
        depth,
        Some(&locals.ints),
        Some(&locals.bools),
    ) {
        return Some(LocalValue::Bool(value));
    }
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
            ..
        } => token
            .text
            .parse::<f64>()
            .ok()
            .filter(|value| value.is_finite())
            .map(LocalValue::Real),
        ast::Expression::Array {
            elements,
            is_matrix: false,
            ..
        } => {
            let mut values = Vec::with_capacity(elements.len());
            for element in elements {
                values.push(function_expr_real(element, env, depth + 1, locals)?);
            }
            Some(LocalValue::Reals(values))
        }
        ast::Expression::ComponentReference(reference) => {
            local_reference_value(reference, env, depth, locals)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            let value = function_expr_real(rhs, env, depth + 1, locals)?;
            match op {
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                    Some(LocalValue::Real(-value))
                }
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => {
                    Some(LocalValue::Real(value))
                }
                _ => None,
            }
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = function_expr_real(lhs, env, depth + 1, locals)?;
            let rhs = function_expr_real(rhs, env, depth + 1, locals)?;
            eval_real_binary(op, lhs, rhs)
        }
        ast::Expression::FunctionCall {
            comp,
            args,
            is_partial_application: false,
            ..
        } => eval_function_builtin_call(comp, args, env, depth, locals),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                match eval_function_expr(condition, env, depth + 1, locals)? {
                    LocalValue::Bool(true) => {
                        return eval_function_expr(value, env, depth + 1, locals);
                    }
                    LocalValue::Bool(false) => {}
                    _ => return None,
                }
            }
            eval_function_expr(else_branch, env, depth + 1, locals)
        }
        _ => None,
    }
}

/// A function-body condition through the typed frame: the scalar bool path
/// answers first inside `eval_function_expr`; a non-Bool result fails closed.
fn eval_function_condition(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &MixedLocals,
) -> Option<bool> {
    match eval_function_expr(expr, env, depth, locals)? {
        LocalValue::Bool(value) => Some(value),
        _ => None,
    }
}

fn function_expr_real(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &MixedLocals,
) -> Option<f64> {
    match eval_function_expr(expr, env, depth, locals)? {
        LocalValue::Real(value) => Some(value),
        // MLS §10.6.2: Integer operands promote in a Real expression.
        LocalValue::Integer(value) => Some(value as f64),
        _ => None,
    }
}

fn eval_real_binary(op: &rumoca_core::OpBinary, lhs: f64, rhs: f64) -> Option<LocalValue> {
    use rumoca_core::OpBinary;
    // A structural fold must never manufacture a value from an undefined
    // operation: a zero divisor is rejected before dividing, and any
    // non-finite arithmetic result (overflow, 0/0) refuses to fold instead
    // of flowing into a comparison as Inf/NaN — the same fail-closed
    // posture as the checked DAE numeric owner.
    let checked_real = |value: f64| value.is_finite().then_some(LocalValue::Real(value));
    match op {
        OpBinary::Add | OpBinary::AddElem => checked_real(lhs + rhs),
        OpBinary::Sub | OpBinary::SubElem => checked_real(lhs - rhs),
        OpBinary::Mul | OpBinary::MulElem => checked_real(lhs * rhs),
        OpBinary::Div | OpBinary::DivElem => {
            if rhs == 0.0 {
                return None;
            }
            checked_real(lhs / rhs)
        }
        OpBinary::Lt => Some(LocalValue::Bool(lhs < rhs)),
        OpBinary::Le => Some(LocalValue::Bool(lhs <= rhs)),
        OpBinary::Gt => Some(LocalValue::Bool(lhs > rhs)),
        OpBinary::Ge => Some(LocalValue::Bool(lhs >= rhs)),
        // Real equality inside a function body is legal MLS §8.5; both
        // operands are exact evaluated values here.
        OpBinary::Eq => Some(LocalValue::Bool(lhs == rhs)),
        OpBinary::Neq => Some(LocalValue::Bool(lhs != rhs)),
        _ => None,
    }
}

fn local_reference_value(
    reference: &ast::ComponentReference,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &MixedLocals,
) -> Option<LocalValue> {
    let [part] = reference.parts.as_slice() else {
        return None;
    };
    let name = part.ident.text.as_ref();
    let subs = part.subs.as_deref().unwrap_or(&[]);
    match subs {
        [] => {
            if let Some(value) = locals.reals.get(name) {
                return Some(LocalValue::Real(*value));
            }
            locals
                .real_arrays
                .get(name)
                .map(|values| LocalValue::Reals(values.clone()))
        }
        [ast::Subscript::Expression(index_expr)] => {
            let values = locals.real_arrays.get(name)?;
            let LocalValue::Integer(index) =
                eval_function_expr(index_expr, env, depth + 1, locals)?
            else {
                return None;
            };
            let index = usize::try_from(index).ok()?.checked_sub(1)?;
            values.get(index).copied().map(LocalValue::Real)
        }
        _ => None,
    }
}

fn eval_function_builtin_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &MixedLocals,
) -> Option<LocalValue> {
    let [part] = comp.parts.as_slice() else {
        return None;
    };
    if part.subs.as_ref().is_some_and(|subs| !subs.is_empty()) {
        return None;
    }
    // The predefined operation may answer only a call whose identity is the
    // tree's own checked predefined member for this spelling (or an
    // identity-free synthetic spelling). A resolved identity pointing at any
    // other declaration selected a user function, and its body must never
    // be replaced by the predefined operation.
    if let Some(selected) = part.def_id {
        let predefined =
            env.tree
                .scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_flat_path(
                    part.ident.text.as_ref(),
                ));
        if predefined != Some(selected) {
            return None;
        }
    }
    match part.ident.text.as_ref() {
        "size" => {
            let [ast::Expression::ComponentReference(array_ref), dimension] = args else {
                return None;
            };
            let [array_part] = array_ref.parts.as_slice() else {
                return None;
            };
            if array_part
                .subs
                .as_ref()
                .is_some_and(|subs| !subs.is_empty())
            {
                return None;
            }
            let values = locals.real_arrays.get(array_part.ident.text.as_ref())?;
            let LocalValue::Integer(1) = eval_function_expr(dimension, env, depth + 1, locals)?
            else {
                return None;
            };
            i64::try_from(values.len()).ok().map(LocalValue::Integer)
        }
        "abs" => {
            let [argument] = args else {
                return None;
            };
            function_expr_real(argument, env, depth + 1, locals)
                .map(|value| LocalValue::Real(value.abs()))
        }
        _ => None,
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
            let evaluated = eval_function_expr(value, env, depth + 1, locals)?;
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
        if eval_function_condition(&block.cond, env, depth + 1, locals)? {
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
        if !eval_function_condition(&block.cond, env, depth + 1, locals)? {
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
