//! AST expression evaluation for the instantiate phase.
//!
//! This module handles evaluation of expressions during instantiation:
//! - Boolean conditions for conditional components (MLS §4.8)
//! - Integer expressions for array dimensions (MLS §10.1)
//! - Enum comparisons for parameter-based conditions
//! - StateSelect parsing from annotations

use crate::ast_scalar::{self, AstScalarContext};
use rumoca_core::{
    IntegerBinaryOperator, eval_integer_binary as eval_common_integer_binary,
    eval_integer_div_builtin,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rustc_hash::FxHashMap;
use std::borrow::Cow;

mod array_indices;
mod class_lookup;
mod component_params;
mod enum_literal;
mod function_eval;
mod scoped_condition;

pub use array_indices::{ArrayIndexTuples, array_index_tuples, generate_array_indices};
use class_lookup::{resolve_class_constant_binding, resolve_component_ref_from_record_defaults};
pub use component_params::{
    AstScalarKind, canonical_scalar_kind, component_allows_structural_evaluation,
    component_explicitly_disables_structural_evaluation, component_has_evaluate_annotation,
    eval_state_select_expr, eval_state_select_expr_with_source_scope, expr_to_string,
    extract_binding, extract_bool_params_with_mods, extract_int_params_with_mods,
    extract_real_params_with_mods, modification_environment_disables_structural_evaluation,
    parse_state_select, propagate_record_alias_integer_params,
    propagate_scoped_record_alias_integer_params, try_eval_string_expr,
    try_eval_structural_boolean,
};
pub(super) use component_params::{
    component_expr_for_structural_eval, component_ref_to_dotted_no_subscripts,
    enclosing_scope_candidates,
};
pub use function_eval::{
    EvaluatedShapeDimension, evaluate_array_dimensions_with_index,
    try_eval_integer_shape_expr_with_index, try_eval_integer_shape_expr_with_proof,
};
use scoped_condition::eval_scoped_string_condition_with_depth;

/// Maximum recursion depth for condition evaluation (prevents stack overflow)
const MAX_CONDITION_DEPTH: usize = 10;

/// Maximum recursion depth for expression evaluation.
const MAX_EXPR_EVAL_DEPTH: usize = 20;

/// Context for instantiation-phase AST expression evaluation.
pub struct InstantiateEvalCtx<'a> {
    pub tree: &'a ast::ClassTree,
    pub mod_env: &'a ast::ModificationEnvironment,
    pub effective_components: &'a IndexMap<String, ast::Component>,
    /// Resolve effective components of an arbitrary class (including inherited).
    /// Implementations must return the class's own components when inherited
    /// component expansion is unavailable.
    pub resolve_class_components:
        fn(&ast::ClassTree, &ast::ClassDef) -> IndexMap<String, ast::Component>,
}

#[derive(Copy, Clone)]
struct ConditionEvalEnv<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    resolve_class_components:
        fn(&ast::ClassTree, &ast::ClassDef) -> IndexMap<String, ast::Component>,
}

mod condition_eval;
use condition_eval::{
    InstantiateScalarAdapter, IntegerEvalEnv, enum_value_for_comparison_with_depth,
    enum_values_equal, evaluate_component_condition_with_depth, evaluate_enum_equality_with_depth,
    find_class_in_tree, get_enum_value_with_depth, local_reference_occurrence_allows,
    reference_root_is_outer, resolve_component_ref_expr, try_eval_bool_literal,
};
pub use condition_eval::{
    OuterValues, evaluate_component_condition, evaluate_component_condition_with_outer_values,
    expr_to_bool,
};

/// Fold a Real-valued parameter expression at instantiation time (MLS §4.4.5).
///
/// Returns `None` when the expression is not a Real parameter expression this
/// phase can decide, or when it folds to a non-finite value; nothing is invented
/// for the undecided case (SPEC_0008).
pub fn try_eval_real_expr(ctx: &InstantiateEvalCtx, expr: &ast::Expression) -> Option<f64> {
    try_eval_real_expr_with_known(ctx, expr, &FxHashMap::default())
}

/// [`try_eval_real_expr`], with references that already have a settled value
/// answered from `known` instead of from their declaration (MLS §7.2).
pub fn try_eval_real_expr_with_known(
    ctx: &InstantiateEvalCtx,
    expr: &ast::Expression,
    known: &FxHashMap<String, f64>,
) -> Option<f64> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    let adapter = InstantiateScalarAdapter {
        env: IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components: *resolve_class_components,
            work_budget: None,
        },
        local_ints: None,
        local_bools: None,
        local_reals: (!known.is_empty()).then_some(known),
    };
    ast_scalar::eval_real(expr, &adapter, "", 0).filter(|value| value.is_finite())
}

/// Try to evaluate an integer expression for array dimension expansion.
/// Used to expand array components during instantiation (MLS §10.1).
pub fn try_eval_integer_expr(ctx: &InstantiateEvalCtx, expr: &ast::Expression) -> Option<i64> {
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    try_eval_integer_expr_with_depth_and_locals(
        expr,
        mod_env,
        effective_components,
        tree,
        *resolve_class_components,
        0,
        None,
    )
}

fn try_eval_integer_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<i64> {
    try_eval_integer_expr_with_depth_and_locals(
        expr,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        depth,
        None,
    )
}

fn try_eval_integer_expr_with_depth_and_locals(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    try_eval_integer_expr_with_env_and_locals(
        expr,
        IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            work_budget: None,
        },
        depth,
        local_ints,
    )
}

fn try_eval_integer_expr_with_env_and_locals(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let adapter = InstantiateScalarAdapter {
        env,
        local_ints,
        local_bools: None,
        local_reals: None,
    };
    ast_scalar::eval_integer(expr, &adapter, "", depth)
}

pub(super) fn try_eval_bool_expr_with_local_values(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
    local_bools: Option<&FxHashMap<String, bool>>,
) -> Option<bool> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let adapter = InstantiateScalarAdapter {
        env,
        local_ints,
        local_bools,
        local_reals: None,
    };
    ast_scalar::eval_boolean(expr, &adapter, "", depth)
}

fn eval_bool_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
    local_bools: Option<&FxHashMap<String, bool>>,
) -> Option<bool> {
    let func_name = comp
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    let qualified_name = comp
        .root_def_id()
        .and_then(|did| env.tree.def_map.get(&did))
        .map(String::as_str);

    let function_def = lookup_function_definition(&func_name, qualified_name, env.tree)?;
    function_eval::eval_user_defined_bool_function(
        function_def,
        args,
        env,
        depth,
        local_ints,
        local_bools,
    )
}

fn eval_integer_component_ref(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    let recurse =
        |expr| try_eval_integer_expr_with_env_and_locals(expr, env, depth + 1, local_ints);

    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    if !local_reference_occurrence_allows(comp_ref, dotted.as_str(), env) {
        return None;
    }

    if let Some(local_values) = local_ints
        && let Some(value) = lookup_local_scalar(comp_ref, local_values)
    {
        return Some(value);
    }
    if reference_root_is_outer(comp_ref, env) {
        return None;
    }

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    if let Some(mod_value) = env.mod_env.get(&param_path) {
        return recurse(&mod_value.value);
    }

    if comp_ref.parts.len() == 1 {
        let param_name = comp_ref.parts[0].ident.text.as_ref();
        let sibling = env.effective_components.get(param_name)?;
        return recurse(component_expr_for_structural_eval(sibling)?);
    }
    if let Some(sibling) = env.effective_components.get(dotted.as_str()) {
        return recurse(component_expr_for_structural_eval(sibling)?);
    }

    for candidate in enclosing_scope_candidates(dotted.as_str()) {
        let qn = ast::QualifiedName::from_dotted(&candidate);
        if let Some(mod_value) = env.mod_env.get(&qn) {
            return recurse(&mod_value.value);
        }
        if let Some(sibling) = env.effective_components.get(candidate.as_str()) {
            return recurse(component_expr_for_structural_eval(sibling)?);
        }
    }

    if let Some(value) = eval_integer_class_redeclare_field_ref(comp_ref, env, depth, local_ints) {
        return Some(value);
    }

    if let Some(value) = eval_integer_class_constant_ref(comp_ref, env, depth, local_ints) {
        return Some(value);
    }

    // Resolve record field references from defaults/modifications in the
    // component's declared type scope (e.g., `data.mSystems`, `data.mBasic`).
    eval_integer_record_field_ref(comp_ref, env, depth)
}

fn eval_integer_class_constant_ref(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    if comp_ref.parts.len() < 2
        || comp_ref
            .parts
            .iter()
            .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }

    let field_name = comp_ref.parts.last()?.ident.text.as_ref();
    let class_path = comp_ref.parts[..comp_ref.parts.len() - 1]
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    let class = env.tree.get_class_by_qualified_name(&class_path)?;
    let effective_components = (env.resolve_class_components)(env.tree, class);
    let field_component = effective_components.get(field_name)?;
    if !matches!(
        field_component.variability,
        rumoca_core::Variability::Constant(_) | rumoca_core::Variability::Parameter(_)
    ) {
        return None;
    }
    let value_expr = component_expr_for_structural_eval(field_component)?;

    try_eval_integer_expr_with_env_and_locals(
        value_expr,
        IntegerEvalEnv {
            effective_components: &effective_components,
            ..env
        },
        depth + 1,
        local_ints,
    )
}

/// Evaluate `Pkg.field` references when `Pkg` is a class/package redeclare in mod_env.
///
/// MLS §7.3: class/package redeclare modifiers inside component scopes can forward to
/// enclosing overrides (`redeclare package Medium = Medium`). Dimension expressions like
/// `Medium.nC` must resolve against the effective redeclared package, not the local default.
fn eval_integer_class_redeclare_field_ref(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    if comp_ref.parts.len() != 2 || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }

    let root_name = comp_ref.parts[0].ident.text.as_ref();
    let field_name = comp_ref.parts[1].ident.text.as_ref();
    let root_mod = env
        .mod_env
        .get(&ast::QualifiedName::from_ident(root_name))?;

    let target_cref = match &root_mod.value {
        ast::Expression::ClassModification { target, .. } => target,
        ast::Expression::ComponentReference(cref) => cref,
        _ => return None,
    };

    // MLS §7.3: `redeclare package Medium = Medium` is a forwarding redeclare.
    // Do not collapse this to the local default package at template time;
    // keep shape evaluation unresolved so instantiation can apply enclosing
    // overrides (e.g., `Source s(redeclare package Medium = MediumCO2)`).
    let forwarding_self_redeclare = target_cref.parts.len() == 1
        && target_cref.parts[0].subs.is_none()
        && target_cref.parts[0].ident.text.as_ref() == root_name;
    if forwarding_self_redeclare {
        return None;
    }

    let target_class = resolve_class_from_cref(env.tree, target_cref)?;
    let effective_components = (env.resolve_class_components)(env.tree, target_class);
    let field_component = effective_components.get(field_name)?;
    let occurrence = format!("{root_name}.{field_name}");
    if !component_allows_structural_evaluation(&occurrence, field_component, &env.instantiate_ctx())
    {
        return None;
    }
    let value_expr = component_expr_for_structural_eval(field_component)?;

    try_eval_integer_expr_with_env_and_locals(
        value_expr,
        IntegerEvalEnv {
            effective_components: &effective_components,
            ..env
        },
        depth + 1,
        local_ints,
    )
}

fn resolve_class_from_cref<'a>(
    tree: &'a ast::ClassTree,
    cref: &ast::ComponentReference,
) -> Option<&'a ast::ClassDef> {
    if let Some(def_id) = cref.root_def_id()
        && let Some(class) = tree.get_class_by_def_id(def_id)
    {
        return Some(class);
    }

    let qualified = cref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    tree.get_class_by_qualified_name(&qualified)
        .or_else(|| find_class_in_tree(tree, &qualified))
}

/// Evaluate integer-valued record field references in component scope.
///
/// Supports direct record-field paths like `rec.n` where `rec` is a component
/// and `n` is a field parameter in the record type. Field expressions are
/// evaluated in declaration order so later fields can reference earlier ones
/// (e.g., `mBasic = integer(m / mSystems)`).
fn eval_integer_record_field_ref(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'_>,
    depth: usize,
) -> Option<i64> {
    if comp_ref.parts.len() != 2 || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }

    let root_name = comp_ref.parts[0].ident.text.as_ref();
    let field_name = comp_ref.parts[1].ident.text.as_ref();

    let root_comp = env.effective_components.get(root_name)?;
    let record_class = record_class_for_component(env.tree, root_comp)?;
    if record_class.class_type != rumoca_core::ClassType::Record {
        return None;
    }

    // MLS §7.2 record modification semantics: if the whole record is rebound
    // by reference, field values must come from the bound record, not from
    // stale defaults of the declared record type.
    if root_comp.modifications.get(field_name).is_none()
        && let Some(root_alias) = record_root_alias_from_mod_env(root_name, env.mod_env)
    {
        let mut alias_field_ref = root_alias.clone();
        alias_field_ref.parts.push(comp_ref.parts[1].clone());

        if alias_field_ref != *comp_ref
            && let Some(value) = eval_integer_component_ref(&alias_field_ref, env, depth + 1, None)
        {
            return Some(value);
        }

        return None;
    }

    // MLS §7.1: the record's fields include the inherited ones, so iterate its
    // effective components rather than only what the class declares itself.
    let record_fields = (env.resolve_class_components)(env.tree, record_class);
    let target_field = record_fields.get(field_name)?;
    let target_occurrence = format!("{root_name}.{field_name}");
    if !component_allows_structural_evaluation(
        &target_occurrence,
        target_field,
        &env.instantiate_ctx(),
    ) {
        return None;
    }

    // Prefer an explicit occurrence modification only after the selected
    // field has proved that it may participate in structural evaluation.
    match resolve_record_field_override_from_mod_env(root_name, field_name, env, depth) {
        RecordFieldOverride::Value(value) => return Some(value),
        RecordFieldOverride::Ambiguous => return None,
        RecordFieldOverride::NoMatch => {}
    }

    let field_env = IntegerEvalEnv {
        effective_components: &record_fields,
        ..env
    };
    let mut local_values: FxHashMap<String, i64> = FxHashMap::default();
    for (name, field_comp) in &record_fields {
        let occurrence = format!("{root_name}.{name}");
        if !component_allows_structural_evaluation(&occurrence, field_comp, &env.instantiate_ctx())
        {
            continue;
        }
        let field_mod = root_comp.modifications.get(name);
        let extends_override = record_extends_field_override(record_class, name);
        let field_expr = field_mod
            .or(extends_override)
            .or_else(|| component_expr_for_structural_eval(field_comp));

        if let Some(expr) = field_expr
            && let Some(value) = try_eval_integer_expr_with_env_and_locals(
                expr,
                field_env,
                depth + 1,
                Some(&local_values),
            )
        {
            local_values.insert(name.clone(), value);
        }

        if name == field_name {
            if let Some(value) = local_values.get(name) {
                return Some(*value);
            }
            let field_expr = field_expr?;
            return try_eval_integer_expr_with_env_and_locals(
                field_expr,
                field_env,
                depth + 1,
                Some(&local_values),
            );
        }
    }

    None
}

fn record_class_for_component<'a>(
    tree: &'a ast::ClassTree,
    component: &ast::Component,
) -> Option<&'a ast::ClassDef> {
    if let Some(type_def_id) = component.type_def_id
        && let Some(class) = tree.get_class_by_def_id(type_def_id)
    {
        return Some(class);
    }

    let type_name = component.type_name.to_string();
    find_class_in_tree(tree, &type_name).or_else(|| lookup_unique_class_suffix(tree, &type_name))
}

fn lookup_unique_class_suffix<'a>(
    tree: &'a ast::ClassTree,
    type_name: &str,
) -> Option<&'a ast::ClassDef> {
    let suffix = format!(".{type_name}");
    let mut matches = tree
        .name_map
        .keys()
        .filter(|qualified| qualified.as_str() == type_name || qualified.ends_with(&suffix))
        .filter_map(|qualified| tree.get_class_by_qualified_name(qualified));
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn record_extends_field_override<'a>(
    record_class: &'a ast::ClassDef,
    field_name: &str,
) -> Option<&'a ast::Expression> {
    let mut field_expr = None;

    for extend in &record_class.extends {
        for modification in &extend.modifications {
            if modification.redeclare {
                continue;
            }
            match &modification.expr {
                ast::Expression::Modification {
                    target,
                    value: Some(value),
                    ..
                } if target.parts.len() == 1
                    && target.parts[0].ident.text.as_ref() == field_name
                    && !matches!(value.as_ref(), ast::Expression::Empty { .. }) =>
                {
                    field_expr = Some(value.as_ref());
                }
                ast::Expression::NamedArgument { name, value, .. }
                    if name.text.as_ref() == field_name
                        && !matches!(value.as_ref(), ast::Expression::Empty { .. }) =>
                {
                    field_expr = Some(value.as_ref());
                }
                _ => {}
            }
        }
    }

    field_expr
}

fn record_root_alias_from_mod_env<'a>(
    root_name: &str,
    mod_env: &'a ast::ModificationEnvironment,
) -> Option<&'a ast::ComponentReference> {
    let root_mod = mod_env.get(&ast::QualifiedName::from_ident(root_name))?;
    let ast::Expression::ComponentReference(comp_ref) = &root_mod.value else {
        return None;
    };
    Some(comp_ref)
}

enum RecordFieldOverride {
    NoMatch,
    Value(i64),
    Ambiguous,
}

fn resolve_record_field_override_from_mod_env(
    root_name: &str,
    field_name: &str,
    env: IntegerEvalEnv<'_>,
    depth: usize,
) -> RecordFieldOverride {
    let dotted = format!("{root_name}.{field_name}");
    let mut saw_match = false;
    let mut resolved_value = None;

    for (qn, mod_value) in &env.mod_env.active {
        let qn_dotted = qn
            .parts
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(".");

        // MLS §7.2: field overrides may be stored either as dotted field keys
        // (`rec.n = ...`) or as a root class-modification (`rec(n = ...)`).
        let candidate_expr = if qn_dotted == dotted {
            Some(&mod_value.value)
        } else if qn_dotted == root_name {
            extract_field_override_from_class_modification(&mod_value.value, field_name)
        } else {
            None
        };

        let Some(expr) = candidate_expr else {
            continue;
        };

        saw_match = true;
        let Some(value) = try_eval_integer_expr_with_env_and_locals(expr, env, depth + 1, None)
        else {
            return RecordFieldOverride::Ambiguous;
        };

        if let Some(existing) = resolved_value
            && existing != value
        {
            return RecordFieldOverride::Ambiguous;
        }
        resolved_value = Some(value);
    }

    if let Some(value) = resolved_value {
        RecordFieldOverride::Value(value)
    } else if saw_match {
        RecordFieldOverride::Ambiguous
    } else {
        RecordFieldOverride::NoMatch
    }
}

fn extract_field_override_from_class_modification<'a>(
    expr: &'a ast::Expression,
    field_name: &str,
) -> Option<&'a ast::Expression> {
    let ast::Expression::ClassModification { modifications, .. } = expr else {
        return None;
    };

    let mut resolved = None;
    for modification in modifications {
        let candidate = match modification {
            ast::Expression::NamedArgument { name, value, .. }
                if name.text.as_ref() == field_name
                    && !matches!(value.as_ref(), ast::Expression::Empty { .. }) =>
            {
                Some(value.as_ref())
            }
            ast::Expression::Modification {
                target,
                value: Some(value),
                ..
            } if target.parts.len() == 1
                && target.parts[0].ident.text.as_ref() == field_name
                && !matches!(value.as_ref(), ast::Expression::Empty { .. }) =>
            {
                Some(value.as_ref())
            }
            _ => None,
        };

        if let Some(candidate_expr) = candidate
            && resolved.replace(candidate_expr).is_some()
        {
            return None;
        }
    }

    resolved
}

fn eval_integer_binary(op: &rumoca_core::OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    let operator = match op {
        rumoca_core::OpBinary::Add => IntegerBinaryOperator::Add,
        rumoca_core::OpBinary::Sub => IntegerBinaryOperator::Sub,
        rumoca_core::OpBinary::Mul => IntegerBinaryOperator::Mul,
        rumoca_core::OpBinary::Div => IntegerBinaryOperator::Div,
        _ => return None,
    };
    eval_common_integer_binary(operator, lhs, rhs)
}

/// Look up a subscript-free reference in a caller-supplied value map.
///
/// A subscripted reference names one element of an array and the map is keyed by
/// scalar path, so it is left unresolved rather than answered with the array's
/// entry (SPEC_0008).
fn lookup_local_scalar<T: Copy>(
    comp_ref: &ast::ComponentReference,
    local_values: &FxHashMap<String, T>,
) -> Option<T> {
    if comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    local_values.get(&dotted).copied()
}

/// Evaluate a function call to an integer value during instantiation.
///
/// Handles Modelica builtins (integer, mod, div, abs) and user-defined pure
/// functions by looking them up in the ast::ClassTree and evaluating with rumoca_eval_const.
fn eval_integer_function_call(
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    let func_name = comp
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    if let Some(function_def) = selected_user_function_for_call(comp, &func_name, env.tree) {
        return eval_user_defined_integer_function(function_def, args, env, depth, local_ints);
    }
    if !call_targets_predefined(comp, &func_name, env.tree) {
        return None;
    }

    let recurse = |e| try_eval_integer_expr_with_env_and_locals(e, env, depth + 1, local_ints);

    match (func_name.as_str(), args) {
        ("integer", [argument]) => {
            // Integral arguments are unchanged. Real arguments are evaluated by
            // the scalar adapter above so MLS floor semantics stay type-aware.
            let val = recurse(argument)?;
            return Some(val);
        }
        ("mod", [lhs, rhs]) => {
            // MLS §3.7.2: mod(x, y) = x - floor(x/y)*y
            let x = recurse(lhs)?;
            let y = recurse(rhs)?;
            return rumoca_core::eval_integer_mod_builtin(x, y);
        }
        ("div", [lhs, rhs]) => {
            // MLS §3.7.2: div(x, y) = truncate(x/y)
            let x = recurse(lhs)?;
            let y = recurse(rhs)?;
            return eval_integer_div_builtin(x, y);
        }
        ("abs", [argument]) => {
            return recurse(argument)?.checked_abs();
        }
        ("min", [lhs, rhs]) => {
            let x = recurse(lhs)?;
            let y = recurse(rhs)?;
            return Some(x.min(y));
        }
        ("max", [lhs, rhs]) => {
            let x = recurse(lhs)?;
            let y = recurse(rhs)?;
            return Some(x.max(y));
        }
        _ => {}
    }
    None
}

fn selected_user_function_for_call<'a>(
    comp: &ast::ComponentReference,
    rendered: &str,
    tree: &'a ast::ClassTree,
) -> Option<&'a ast::ClassDef> {
    if let Some(target) = comp.target_def_id() {
        return tree
            .get_class_by_def_id(target)
            .filter(|class| class.class_type == rumoca_core::ClassType::Function);
    }
    let qualified_name = comp
        .root_def_id()
        .and_then(|did| tree.def_map.get(&did))
        .map(String::as_str);
    lookup_function_definition(rendered, qualified_name, tree)
}

fn call_targets_predefined(
    comp: &ast::ComponentReference,
    rendered: &str,
    tree: &ast::ClassTree,
) -> bool {
    let [part] = comp.parts.as_slice() else {
        return false;
    };
    if part
        .subs
        .as_ref()
        .is_some_and(|subscripts| !subscripts.is_empty())
        || rendered != part.ident.text.as_ref()
        || selected_user_function_for_call(comp, rendered, tree).is_some()
    {
        return false;
    }
    match comp.target_def_id() {
        Some(target) => {
            tree.scope_tree
                .predefined_member(&rumoca_core::ComponentPath::from_flat_path(
                    part.ident.text.as_ref(),
                ))
                == Some(target)
        }
        None => true,
    }
}

fn lookup_function_definition<'a>(
    func_name: &str,
    qualified_name: Option<&str>,
    tree: &'a ast::ClassTree,
) -> Option<&'a ast::ClassDef> {
    if let Some(name) = qualified_name
        && let Some(class) = tree.get_class_by_qualified_name(name)
        && class.class_type == rumoca_core::ClassType::Function
    {
        return Some(class);
    }

    if let Some(class) = tree.get_class_by_qualified_name(func_name)
        && class.class_type == rumoca_core::ClassType::Function
    {
        return Some(class);
    }

    lookup_unique_short_function_name(func_name, tree)
}

fn lookup_unique_short_function_name<'a>(
    func_name: &str,
    tree: &'a ast::ClassTree,
) -> Option<&'a ast::ClassDef> {
    if func_name.contains('.') {
        return None;
    }

    let mut matches = tree
        .def_map
        .values()
        .filter(|qualified| {
            rumoca_core::ComponentPath::from_flat_path(qualified)
                .parts()
                .last()
                .is_some_and(|leaf| leaf == func_name)
        })
        .filter_map(|qualified| tree.get_class_by_qualified_name(qualified))
        .filter(|class| class.class_type == rumoca_core::ClassType::Function);
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn eval_user_defined_integer_function(
    function_def: &ast::ClassDef,
    args: &[ast::Expression],
    env: IntegerEvalEnv<'_>,
    depth: usize,
    caller_locals: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    function_eval::eval_user_defined_integer_function(function_def, args, env, depth, caller_locals)
}
