use super::{
    IntegerEvalEnv, MAX_EXPR_EVAL_DEPTH, ast, call_targets_predefined, eval_integer_binary,
    eval_integer_function_call, evaluate_component_condition_with_depth,
    evaluate_enum_equality_with_depth, try_eval_bool_expr_with_local_values,
    try_eval_integer_expr_with_env_and_locals,
};
use crate::ast_scalar::{self, AstScalarContext};
use crate::function_budget::AstFunctionWorkBudget;
use crate::function_control::FunctionStmtFlow;
use rustc_hash::FxHashMap;

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

struct SavedMixedLocal {
    integer: Option<i64>,
    boolean: Option<bool>,
    real: Option<f64>,
    real_array: Option<Vec<f64>>,
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
    if !function_def.pure
        || function_def.external.is_some()
        || depth >= MAX_EXPR_EVAL_DEPTH
        || function_declarations_are_invalid(function_def)
    {
        return None;
    }

    let root_budget = AstFunctionWorkBudget::new();
    let env = env.with_work_budget(&root_budget);
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
    if !function_def.pure
        || function_def.external.is_some()
        || depth >= MAX_EXPR_EVAL_DEPTH
        || function_declarations_are_invalid(function_def)
    {
        return None;
    }

    let root_budget = AstFunctionWorkBudget::new();
    let env = env.with_work_budget(&root_budget);
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

fn function_declarations_are_invalid(function_def: &ast::ClassDef) -> bool {
    function_def.components.values().any(|component| {
        let binding_is_invalid = component
            .binding
            .as_ref()
            .is_some_and(|binding| ast::expression_required_value_violation(binding).is_some());
        component.has_explicit_binding != component.binding.is_some()
            || binding_is_invalid
            || component.shape_expr.iter().any(|subscript| {
                ast::declaration_subscript_required_value_violation(subscript).is_some()
            })
    })
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
    let inputs = crate::ast_call_binding::bind_ast_function_call(function_def, args)?;
    for input in inputs.iter().filter(|input| input.argument.is_some()) {
        let value = eval_mixed_local_value(
            input.argument?,
            env,
            depth,
            caller_locals.ints,
            caller_locals.bools,
        )?;
        insert_checked_local_value(input.name, input.component, value, env, locals)?;
    }

    for input in inputs {
        if locals.contains(input.name) {
            continue;
        }
        if assign_component_default(input.name, input.component, env, depth, locals) {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
        // `[:]` parses as the colon Range subscript and admits any actual
        // rank-one extent (MLS §12.4.5). Empty is parser recovery, not colon.
        [ast::Subscript::Range { .. }] => DeclaredExtent::Any,
        [ast::Subscript::Empty] => DeclaredExtent::Unproved,
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
    if let Some(value) = try_eval_integer_expr_with_env_and_locals(expr, env, depth + 1, local_ints)
    {
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

fn take_mixed_local(name: &str, locals: &mut MixedLocals) -> SavedMixedLocal {
    SavedMixedLocal {
        integer: locals.ints.remove(name),
        boolean: locals.bools.remove(name),
        real: locals.reals.remove(name),
        real_array: locals.real_arrays.remove(name),
    }
}

fn restore_mixed_local(name: &str, saved: SavedMixedLocal, locals: &mut MixedLocals) {
    locals.ints.remove(name);
    locals.bools.remove(name);
    locals.reals.remove(name);
    locals.real_arrays.remove(name);
    if let Some(value) = saved.integer {
        locals.ints.insert(name.to_string(), value);
    }
    if let Some(value) = saved.boolean {
        locals.bools.insert(name.to_string(), value);
    }
    if let Some(value) = saved.real {
        locals.reals.insert(name.to_string(), value);
    }
    if let Some(value) = saved.real_array {
        locals.real_arrays.insert(name.to_string(), value);
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
    if ast::expression_required_value_violation(expr).is_some() {
        return None;
    }
    eval_function_expr_inner(expr, env, depth, locals)
}

fn eval_function_expr_inner(
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
    if let Some(value) =
        try_eval_integer_expr_with_env_and_locals(expr, env, depth, Some(&locals.ints))
    {
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
                values.push(function_expr_real_inner(element, env, depth + 1, locals)?);
            }
            Some(LocalValue::Reals(values))
        }
        ast::Expression::ComponentReference(reference) => {
            local_reference_value(reference, env, depth, locals)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            let value = function_expr_real_inner(rhs, env, depth + 1, locals)?;
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
            let lhs = function_expr_real_inner(lhs, env, depth + 1, locals)?;
            let rhs = function_expr_real_inner(rhs, env, depth + 1, locals)?;
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
                match eval_function_expr_inner(condition, env, depth + 1, locals)? {
                    LocalValue::Bool(true) => {
                        return eval_function_expr_inner(value, env, depth + 1, locals);
                    }
                    LocalValue::Bool(false) => {}
                    _ => return None,
                }
            }
            eval_function_expr_inner(else_branch, env, depth + 1, locals)
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

fn function_expr_real_inner(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    locals: &MixedLocals,
) -> Option<f64> {
    match eval_function_expr_inner(expr, env, depth, locals)? {
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
                eval_function_expr_inner(index_expr, env, depth + 1, locals)?
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
    if !call_targets_predefined(comp, part.ident.text.as_ref(), env.tree) {
        return None;
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
            let LocalValue::Integer(1) =
                eval_function_expr_inner(dimension, env, depth + 1, locals)?
            else {
                return None;
            };
            i64::try_from(values.len()).ok().map(LocalValue::Integer)
        }
        "abs" => {
            let [argument] = args else {
                return None;
            };
            function_expr_real_inner(argument, env, depth + 1, locals)
                .map(|value| LocalValue::Real(value.abs()))
        }
        _ => None,
    }
}

mod interpreter;
use interpreter::interpret_function_statements;

mod shape;
pub use shape::{
    EvaluatedShapeDimension, evaluate_array_dimensions_with_index,
    try_eval_integer_shape_expr_with_index, try_eval_integer_shape_expr_with_proof,
};

#[cfg(test)]
mod tests;
