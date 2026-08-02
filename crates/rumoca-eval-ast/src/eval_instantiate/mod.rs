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
pub(super) use component_params::{
    component_expr_for_structural_eval, component_ref_to_dotted_no_subscripts,
    enclosing_scope_candidates,
};
pub use component_params::{
    eval_state_select_expr, eval_state_select_expr_with_source_scope, expr_to_string,
    extract_binding, extract_bool_params_with_mods, extract_int_params_with_mods,
    extract_real_params_with_mods, parse_state_select, propagate_record_alias_integer_params,
    propagate_scoped_record_alias_integer_params, try_eval_string_expr,
};
pub use function_eval::{evaluate_array_dimensions, try_eval_integer_shape_expr};
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

/// Look up a class by name in the class tree.
///
/// Uses O(1) lookup via the name_map (populated during resolve phase).
fn find_class_in_tree<'a>(tree: &'a ast::ClassTree, name: &str) -> Option<&'a ast::ClassDef> {
    if let Some(&def_id) = tree.name_map.get(name) {
        return tree.get_class_by_def_id(def_id);
    }

    if let Some(class) = tree.definitions.classes.get(name) {
        return Some(class);
    }

    None
}

/// Convert a boolean literal expression to its value.
pub fn expr_to_bool(expr: &ast::Expression) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match &*token.text {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Try to evaluate a boolean literal expression.
fn try_eval_bool_literal(expr: &ast::Expression) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Evaluate a conditional component's condition (MLS §4.8).
///
/// Tries to evaluate the condition expression to a boolean at instantiation time.
/// Returns:
/// - Some(true): condition is true, component should be instantiated
/// - Some(false): condition is false, component should be skipped
/// - None: condition cannot be evaluated from current parameter/modifier context,
///   and instantiation proceeds conservatively.
///
/// The condition is typically a simple boolean parameter reference like `useSupport`.
/// Disabled component paths are recorded in `overlay.disabled_components` so the
/// flatten phase can filter out connections and equations involving them.
pub fn evaluate_component_condition(
    ctx: &InstantiateEvalCtx,
    condition: &ast::Expression,
) -> Option<bool> {
    evaluate_component_condition_with_outer_values(ctx, condition, OuterValues::default())
}

/// Values reached through a class's `outer` references (MLS §5.4).
///
/// An `outer` element denotes the nearest enclosing `inner` element of the same
/// name, so a condition such as `world.enableAnimation and sphereDiameter > 0`
/// cannot be answered from the declaring class alone. The instantiate phase
/// resolves those references against the matching `inner` instance and passes the
/// values here, keyed by the dotted path as written in the condition.
#[derive(Clone, Copy, Default)]
pub struct OuterValues<'a> {
    pub bools: Option<&'a FxHashMap<String, bool>>,
    pub reals: Option<&'a FxHashMap<String, f64>>,
}

impl<'a> OuterValues<'a> {
    /// Borrow both maps, dropping the empty ones so lookups stay on the fast path.
    #[must_use]
    pub fn new(bools: &'a FxHashMap<String, bool>, reals: &'a FxHashMap<String, f64>) -> Self {
        Self {
            bools: (!bools.is_empty()).then_some(bools),
            reals: (!reals.is_empty()).then_some(reals),
        }
    }
}

/// Evaluate a conditional component's condition with pre-resolved reference values.
///
/// See [`OuterValues`] for what `outer_values` carries; everything else evaluates
/// exactly as in [`evaluate_component_condition`].
pub fn evaluate_component_condition_with_outer_values(
    ctx: &InstantiateEvalCtx,
    condition: &ast::Expression,
    outer_values: OuterValues<'_>,
) -> Option<bool> {
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
        },
        local_ints: None,
        local_bools: outer_values.bools,
        local_reals: outer_values.reals,
    };
    ast_scalar::eval_boolean(condition, &adapter, "", 0)
}

fn evaluate_component_condition_with_depth(
    condition: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<bool> {
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }
    let adapter = InstantiateScalarAdapter {
        env: IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
        },
        local_ints: None,
        local_bools: None,
        local_reals: None,
    };
    ast_scalar::eval_boolean(condition, &adapter, "", depth)
}

/// Evaluate a parameter reference in a condition.
fn eval_param_ref(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<bool> {
    // Build qualified path for multi-part references (e.g., smpmData.useDamperCage)
    let param_path = build_qualified_path(comp_ref);

    // Check modification environment for an override
    if let Some(mod_value) = mod_env.get(&param_path) {
        if let Some(val) = expr_to_bool(&mod_value.value) {
            return Some(val);
        }
        if let Some(source_scope) = mod_value.source_scope.as_ref() {
            let scope_prefix = source_scope.to_flat_string();
            if let Some(val) = eval_scoped_string_condition_with_depth(
                &mod_value.value,
                ConditionEvalEnv {
                    mod_env,
                    effective_components,
                    tree,
                    resolve_class_components,
                },
                Some(scope_prefix.as_str()),
                depth + 1,
            ) {
                return Some(val);
            }
        }
        // Recursively evaluate only if mod_env value is a ast::ComponentReference
        // (another parameter ref like smpmData.useDamperCage → false)
        if matches!(&mod_value.value, ast::Expression::ComponentReference(_))
            && let Some(val) = evaluate_component_condition_with_depth(
                &mod_value.value,
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
                depth + 1,
            )
        {
            return Some(val);
        }
        return None;
    }

    // Look up the parameter's declared value from effective components
    // (single-part only). MLS §4.9: a component without a binding has no value
    // here — its `start` attribute is an initial guess, not an answer — so an
    // undecidable condition stays `None`.
    if comp_ref.parts.len() == 1 {
        let param_name = comp_ref.parts[0].ident.text.as_ref();
        let sibling = effective_components.get(param_name)?;
        let value_expr = component_expr_for_structural_eval(sibling)?;
        // Try simple boolean extraction first
        if let Some(val) = expr_to_bool(value_expr) {
            return Some(val);
        }
        // Try recursive evaluation for expressions like controllerType == PI
        return evaluate_component_condition_with_depth(
            value_expr,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
        );
    }

    // MLS §5.3.2: qualified references to class-level constants
    // (`P.pT_explicit`) resolve through the class tree. MLS §7.1/§7.2: they may
    // instead name a field of a record component (`smpmData.useDamperCage`),
    // whose value is the record's modification or its declared field default —
    // the Integer path already reaches those, and a Boolean condition that reads
    // a record field needs the same reach.
    if let Some(binding) =
        resolve_class_redeclare_field_expr(comp_ref, mod_env, tree, resolve_class_components)
            .or_else(|| resolve_class_constant_binding(comp_ref, tree, resolve_class_components))
            .or_else(|| {
                resolve_component_ref_from_record_defaults(
                    comp_ref,
                    effective_components,
                    tree,
                    resolve_class_components,
                )
            })
    {
        if let Some(val) = expr_to_bool(&binding) {
            return Some(val);
        }
        return evaluate_component_condition_with_depth(
            &binding,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
        );
    }

    None
}

/// Build a ast::QualifiedName from a ast::ComponentReference's parts.
fn build_qualified_path(comp_ref: &ast::ComponentReference) -> ast::QualifiedName {
    if comp_ref.parts.len() == 1 {
        ast::QualifiedName::from_ident(&comp_ref.parts[0].ident.text)
    } else {
        let dotted = comp_ref
            .parts
            .iter()
            .map(|p| p.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join(".");
        ast::QualifiedName::from_dotted(&dotted)
    }
}

/// Evaluate an enum equality comparison like `controllerType == SimpleController.PI`.
///
/// Returns Some(true) if values are equal, Some(false) if not equal, None if cannot evaluate.
fn evaluate_enum_equality_with_depth(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    depth: usize,
) -> Option<bool> {
    // Prevent deep recursion
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    // Get values from both sides (could be enum literals or parameter references).
    // If a side stays as an unresolved component reference, keep comparison unknown.
    let lhs_val = enum_value_for_comparison_with_depth(
        lhs,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        None,
        depth,
    );
    let rhs_val = enum_value_for_comparison_with_depth(
        rhs,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        None,
        depth,
    );

    match (lhs_val, rhs_val) {
        (Some(l), Some(r)) => {
            // Compare enum values, handling qualified name differences
            // "Modelica.Blocks.Types.SimpleController.PI" should match "SimpleController.PI"
            Some(enum_values_equal(&l, &r))
        }
        _ => None,
    }
}

fn enum_value_for_comparison_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<String> {
    // MLS §4.8: a compile-time condition may only use values that are actually
    // known here. `get_enum_value_with_depth` already declines a reference it
    // could not resolve to a String literal or an enumeration literal, so no
    // further filtering of "looks unresolved" spellings is needed.
    get_enum_value_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        scope_prefix,
        depth,
    )
    .map(ResolvedValueText::into_text)
}

/// A value this phase resolved to a comparable spelling.
///
/// The two cases are kept apart because they are not interchangeable: an
/// enumeration literal is a value of its enumeration type (MLS §4.8.5.1) and
/// must never be rewritten into a `String` modifier, while a `String` literal
/// (MLS §4.9) may be. Collapsing them to one `String` is what let a rendered
/// reference be substituted where a string value was expected.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum ResolvedValueText {
    StringLiteral(String),
    EnumerationLiteral(String),
}

impl ResolvedValueText {
    fn into_text(self) -> String {
        match self {
            ResolvedValueText::StringLiteral(text)
            | ResolvedValueText::EnumerationLiteral(text) => text,
        }
    }

    pub(super) fn into_string_literal(self) -> Option<String> {
        match self {
            ResolvedValueText::StringLiteral(text) => Some(text),
            ResolvedValueText::EnumerationLiteral(_) => None,
        }
    }
}

/// Check if two enum values are equal, handling qualified enum spellings.
///
/// Enum values can be:
/// - Fully qualified: "Modelica.Blocks.Types.SimpleController.PI"
/// - Short form: "SimpleController.PI"
/// - Just the value: "PI" (rare but possible)
///
fn enum_values_equal(a: &str, b: &str) -> bool {
    rumoca_core::enum_values_equal(a, b)
}

/// Get an enum or string value from an expression.
///
/// Handles:
/// - String literals
/// - Enumeration literals (e.g., `SimpleController.PI`), recognized against the
///   class tree rather than by their spelling (MLS §4.8.5.1)
/// - References that resolve to one of the above
///
/// A reference this phase cannot resolve is *unknown*: it is never answered
/// with its own rendered name (SPEC_0008 — no invented values).
fn get_enum_value_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<ResolvedValueText> {
    // Prevent deep recursion
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    match expr {
        // String literal: "D", "Y", etc.
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
            ..
        } => {
            let s = token.text.trim_matches('"');
            Some(ResolvedValueText::StringLiteral(s.to_string()))
        }
        ast::Expression::ComponentReference(comp_ref) => resolve_component_ref_expr(
            comp_ref,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            scope_prefix,
        )
        .and_then(|(resolved_expr, next_scope)| {
            get_enum_value_with_depth(
                &resolved_expr,
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
                next_scope.as_deref(),
                depth + 1,
            )
        })
        .or_else(|| {
            enum_literal::enumeration_literal_path(comp_ref, tree)
                .map(ResolvedValueText::EnumerationLiteral)
        }),
        ast::Expression::Parenthesized { inner, .. } => get_enum_value_with_depth(
            inner,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            scope_prefix,
            depth + 1,
        ),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let env = ConditionEvalEnv {
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
            };
            eval_if_enum_value(branches, else_branch, env, scope_prefix, depth + 1)
        }
        _ => None,
    }
}

fn parent_dotted_scope(path: &str) -> Option<String> {
    let enclosing = rumoca_core::ComponentPath::from_flat_path(path).parent()?;
    (!enclosing.is_root()).then(|| enclosing.to_flat_string())
}

fn eval_if_enum_value(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<ResolvedValueText> {
    for (cond, branch_expr) in branches {
        match eval_scoped_string_condition_with_depth(cond, env, scope_prefix, depth) {
            Some(true) => {
                return get_enum_value_with_depth(
                    branch_expr,
                    env.mod_env,
                    env.effective_components,
                    env.tree,
                    env.resolve_class_components,
                    scope_prefix,
                    depth,
                );
            }
            Some(false) => continue,
            None => return None,
        }
    }
    get_enum_value_with_depth(
        else_branch,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        scope_prefix,
        depth,
    )
}

fn resolve_component_ref_expr(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
) -> Option<(ast::Expression, Option<String>)> {
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    let candidate_paths = candidate_paths_for_ref(comp_ref, dotted.as_str(), scope_prefix);

    lookup_exact_component_ref(candidate_paths.as_slice(), mod_env, effective_components)
        .or_else(|| {
            resolve_component_ref_from_record_defaults(
                comp_ref,
                effective_components,
                tree,
                resolve_class_components,
            )
            .map(|expr| (expr, parent_dotted_scope(&dotted)))
        })
        .or_else(|| {
            if comp_ref.parts.len() != 1 {
                return None;
            }
            let prefix = scope_prefix?;
            let scoped_expr = resolve_scoped_record_field_expr(
                prefix,
                dotted.as_str(),
                effective_components,
                tree,
            )?;
            Some((scoped_expr, Some(prefix.to_string())))
        })
        .or_else(|| {
            resolve_class_redeclare_field_expr(comp_ref, mod_env, tree, resolve_class_components)
                .map(|expr| (expr, None))
        })
        .or_else(|| {
            resolve_class_constant_binding(comp_ref, tree, resolve_class_components)
                .map(|expr| (expr, None))
        })
}

fn resolve_class_redeclare_field_expr(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    tree: &ast::ClassTree,
    resolve_class_components: fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
) -> Option<ast::Expression> {
    if comp_ref.parts.len() != 2
        || comp_ref
            .parts
            .iter()
            .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }

    let root_name = comp_ref.parts[0].ident.text.as_ref();
    let field_name = comp_ref.parts[1].ident.text.as_ref();
    let root_mod = mod_env.get(&ast::QualifiedName::from_ident(root_name))?;
    let target_cref = match &root_mod.value {
        ast::Expression::ClassModification { target, .. } => target,
        ast::Expression::ComponentReference(cref) => cref,
        _ => return None,
    };

    let forwarding_self_redeclare = target_cref.parts.len() == 1
        && target_cref.parts[0].subs.is_none()
        && target_cref.parts[0].ident.text.as_ref() == root_name;
    if forwarding_self_redeclare {
        return None;
    }

    let target_class = resolve_class_from_cref(tree, target_cref)?;
    let effective_components = resolve_class_components(tree, target_class);
    let field_component = effective_components.get(field_name)?;
    component_expr_for_structural_eval(field_component).cloned()
}

fn candidate_paths_for_ref(
    comp_ref: &ast::ComponentReference,
    dotted: &str,
    scope_prefix: Option<&str>,
) -> Vec<String> {
    let mut paths = Vec::with_capacity(2);
    if comp_ref.parts.len() == 1
        && let Some(prefix) = scope_prefix
    {
        paths.push(format!("{prefix}.{dotted}"));
    }
    paths.push(dotted.to_string());
    paths
}

fn lookup_exact_component_ref(
    candidate_paths: &[String],
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
) -> Option<(ast::Expression, Option<String>)> {
    for candidate in candidate_paths {
        if let Some(mod_value) = mod_env.get(&ast::QualifiedName::from_dotted(candidate))
            && !transparent_self_modifier(candidate, &mod_value.value)
        {
            return Some((
                mod_value.value.clone(),
                mod_value
                    .source_scope
                    .as_ref()
                    .map(ast::QualifiedName::to_flat_string)
                    .or_else(|| parent_dotted_scope(candidate)),
            ));
        }
        if let Some(comp) = effective_components.get(candidate.as_str()) {
            let expr = component_expr_for_structural_eval(comp)?;
            return Some((expr.clone(), parent_dotted_scope(candidate)));
        }
    }
    None
}

fn transparent_self_modifier(candidate: &str, value: &ast::Expression) -> bool {
    let ast::Expression::ComponentReference(comp_ref) = value else {
        return false;
    };
    if comp_ref.parts.len() != 1 || comp_ref.parts[0].subs.is_some() {
        return false;
    }
    let Some(name) = rumoca_core::ComponentPath::from_flat_path(candidate)
        .into_parts()
        .last()
        .cloned()
    else {
        return false;
    };
    comp_ref.parts[0].ident.text.as_ref() == name
}

fn resolve_scoped_record_field_expr(
    scope_prefix: &str,
    field_name: &str,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let scope_comp = effective_components.get(scope_prefix)?;
    if let Some(expr) = scope_comp.modifications.get(field_name) {
        return Some(expr.clone());
    }
    let type_def_id = scope_comp.type_def_id?;
    let class = tree.get_class_by_def_id(type_def_id)?;
    let field = class.components.get(field_name)?;
    component_expr_for_structural_eval(field).cloned()
}

#[derive(Copy, Clone)]
pub(super) struct IntegerEvalEnv<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    resolve_class_components:
        fn(&ast::ClassTree, &ast::ClassDef) -> IndexMap<String, ast::Component>,
}

struct InstantiateScalarAdapter<'a> {
    env: IntegerEvalEnv<'a>,
    local_ints: Option<&'a FxHashMap<String, i64>>,
    local_bools: Option<&'a FxHashMap<String, bool>>,
    local_reals: Option<&'a FxHashMap<String, f64>>,
}

/// Resolve a reference to the declaration-side expression that defines it.
///
/// MLS §4.4.4 / §7.2: a parameter's value is written either as an applied
/// modification or as the declaration binding, and MLS §5.3 makes an unqualified
/// name visible from the enclosing scopes as well. MLS §5.3.2 additionally makes a
/// qualified name denote a class-level constant (`Modelica.Constants.eps`). This
/// walks exactly those places and returns the expression found; a reference with
/// subscripts names one array element and is left unresolved rather than answered
/// with the whole array (SPEC_0008).
fn resolve_scalar_declaration_expr<'a>(
    comp_ref: &ast::ComponentReference,
    env: IntegerEvalEnv<'a>,
) -> Option<Cow<'a, ast::Expression>> {
    if comp_ref
        .parts
        .iter()
        .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    if let Some(mod_value) = env.mod_env.get(&param_path) {
        return Some(Cow::Borrowed(&mod_value.value));
    }

    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if let Some(component) = env.effective_components.get(dotted.as_str()) {
        return component_expr_for_structural_eval(component).map(Cow::Borrowed);
    }

    for candidate in enclosing_scope_candidates(dotted.as_str()) {
        let qualified = ast::QualifiedName::from_dotted(&candidate);
        if let Some(mod_value) = env.mod_env.get(&qualified) {
            return Some(Cow::Borrowed(&mod_value.value));
        }
        if let Some(component) = env.effective_components.get(candidate.as_str()) {
            return component_expr_for_structural_eval(component).map(Cow::Borrowed);
        }
    }

    // MLS §5.3.2: a qualified name may denote a constant declared by a class or
    // package (`Modelica.Constants.eps`) rather than a component of this scope.
    // MLS §7.1/§7.2: it may equally name a field of a record component, whose
    // value comes from the record's modification or its declared default. The
    // Boolean and Integer paths already resolve both; a Real parameter
    // expression that compares against one needs the same reach.
    resolve_class_redeclare_field_expr(
        comp_ref,
        env.mod_env,
        env.tree,
        env.resolve_class_components,
    )
    .or_else(|| resolve_class_constant_binding(comp_ref, env.tree, env.resolve_class_components))
    .or_else(|| {
        resolve_component_ref_from_record_defaults(
            comp_ref,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
        )
    })
    .map(Cow::Owned)
}

impl AstScalarContext for InstantiateScalarAdapter<'_> {
    fn expression_depth_limit(&self) -> Option<usize> {
        Some(MAX_EXPR_EVAL_DEPTH)
    }

    fn lookup_integer(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<i64> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        eval_integer_component_ref(reference, self.env, depth, self.local_ints)
    }

    /// Fold a Real-valued reference (MLS §4.4.5 parameter expression).
    ///
    /// A conditional component's condition may compare a Real parameter, as in
    /// `Parts.Body`'s `world.enableAnimation and animation and sphereDiameter > 0`.
    /// Values pre-resolved through an `outer` reference (MLS §5.4) win, otherwise
    /// the reference is followed to its declaration binding and folded there.
    fn lookup_real(&self, expr: &ast::Expression, scope: &str, depth: usize) -> Option<f64> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        if let Some(values) = self.local_reals
            && let Some(value) = lookup_local_scalar(reference, values)
        {
            return Some(value);
        }
        // Integer-valued parameter declarations and pure function calls are
        // promoted when they occur in a Real expression (MLS §10.6.2).  Ask
        // the Integer evaluator first: it only returns a value when the full
        // declaration expression is exact, while `/` inside the surrounding
        // Real expression remains Real division.
        if let Some(value) =
            eval_integer_component_ref(reference, self.env, depth, self.local_ints)
        {
            return Some(value as f64);
        }
        let declaration = resolve_scalar_declaration_expr(reference, self.env)?;
        ast_scalar::eval_real(declaration.as_ref(), self, scope, depth)
    }

    fn lookup_boolean(&self, expr: &ast::Expression, _scope: &str, depth: usize) -> Option<bool> {
        let ast::Expression::ComponentReference(reference) = expr else {
            return None;
        };
        self.local_bools
            .and_then(|values| lookup_local_scalar(reference, values))
            .or_else(|| {
                eval_param_ref(
                    reference,
                    self.env.mod_env,
                    self.env.effective_components,
                    self.env.tree,
                    self.env.resolve_class_components,
                    depth,
                )
            })
    }

    fn call_integer(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        if function.parts.len() == 1
            && function.parts[0].ident.text.as_ref() == "integer"
            && let [argument] = args
        {
            let value = ast_scalar::eval_real(argument, self, scope, depth)?;
            let value = rumoca_core::modelica_integer_value(value);
            if value.is_finite() && value >= i64::MIN as f64 && value < -(i64::MIN as f64) {
                return Some(value as i64);
            }
            return None;
        }
        eval_integer_function_call(function, args, self.env, depth, self.local_ints)
    }

    fn call_boolean(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        _scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<bool> {
        eval_bool_function_call(
            function,
            args,
            self.env,
            depth,
            self.local_ints,
            self.local_bools,
        )
    }

    fn call_real(
        &self,
        function: &ast::ComponentReference,
        args: &[ast::Expression],
        _scope: &str,
        depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<f64> {
        // A structurally evaluated Integer result is a valid Real operand by
        // MLS §10.6.2.  The Integer evaluator remains the certificate here: it
        // rejects functions whose result cannot be established exactly, so
        // this promotion cannot turn an undecidable Real call into a value.
        eval_integer_function_call(function, args, self.env, depth, self.local_ints)
            .map(|value| value as f64)
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
            self.env.mod_env,
            self.env.effective_components,
            self.env.tree,
            self.env.resolve_class_components,
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
}

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
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let adapter = InstantiateScalarAdapter {
        env: IntegerEvalEnv {
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
        },
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
    let recurse = |expr| {
        try_eval_integer_expr_with_depth_and_locals(
            expr,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
            local_ints,
        )
    };

    if let Some(local_values) = local_ints
        && let Some(value) = lookup_local_scalar(comp_ref, local_values)
    {
        return Some(value);
    }

    let mut param_path = ast::QualifiedName::new();
    for part in &comp_ref.parts {
        param_path.push(part.ident.text.to_string(), Vec::new());
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

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

    try_eval_integer_expr_with_depth_and_locals(
        value_expr,
        env.mod_env,
        &effective_components,
        env.tree,
        env.resolve_class_components,
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
    let value_expr = component_expr_for_structural_eval(field_component)?;

    try_eval_integer_expr_with_depth_and_locals(
        value_expr,
        env.mod_env,
        &effective_components,
        env.tree,
        env.resolve_class_components,
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

    // Prefer explicit outer modifications first. These can resolve field values
    // even when record-type metadata is unavailable in the local component map.
    match resolve_record_field_override_from_mod_env(root_name, field_name, env, depth) {
        RecordFieldOverride::Value(value) => return Some(value),
        // Multiple matching overrides (or unevaluable override expressions)
        // are ambiguous here. Keep expression unresolved instead of forcing
        // a potentially stale record default.
        RecordFieldOverride::Ambiguous => return None,
        RecordFieldOverride::NoMatch => {}
    }

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
    let mut local_values: FxHashMap<String, i64> = FxHashMap::default();
    for (name, field_comp) in &record_fields {
        let field_mod = root_comp.modifications.get(name);
        let extends_override = record_extends_field_override(record_class, name);
        let field_expr = field_mod
            .or(extends_override)
            .or_else(|| component_expr_for_structural_eval(field_comp));

        if let Some(expr) = field_expr
            && let Some(value) = try_eval_integer_expr_with_depth_and_locals(
                expr,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
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
            return try_eval_integer_expr_with_depth_and_locals(
                field_expr,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
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
                ast::Expression::Modification { target, value, .. }
                    if target.parts.len() == 1
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
        let Some(value) = try_eval_integer_expr_with_depth_and_locals(
            expr,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
            None,
        ) else {
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
            ast::Expression::Modification { target, value, .. }
                if target.parts.len() == 1
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
    // Build function name from parts
    let func_name = comp
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");

    // Also try the qualified name via def_id (resolve phase may have set this)
    let qualified_name = comp
        .root_def_id()
        .and_then(|did| env.tree.def_map.get(&did))
        .cloned();

    let recurse = |e| {
        try_eval_integer_expr_with_depth_and_locals(
            e,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
            local_ints,
        )
    };

    // Handle Modelica builtins that return integers
    match func_name.as_str() {
        "integer" => {
            // Integral arguments are unchanged. Real arguments are evaluated by
            // the scalar adapter above so MLS floor semantics stay type-aware.
            let val = recurse(args.first()?)?;
            return Some(val);
        }
        "mod" => {
            // MLS §3.7.2: mod(x, y) = x - floor(x/y)*y
            let x = recurse(args.first()?)?;
            let y = recurse(args.get(1)?)?;
            return if y != 0 {
                Some(((x % y) + y) % y)
            } else {
                None
            };
        }
        "div" => {
            // MLS §3.7.2: div(x, y) = truncate(x/y)
            let x = recurse(args.first()?)?;
            let y = recurse(args.get(1)?)?;
            return eval_integer_div_builtin(x, y);
        }
        "abs" => {
            let x = recurse(args.first()?)?;
            return Some(x.abs());
        }
        "min" => {
            let x = recurse(args.first()?)?;
            let y = recurse(args.get(1)?)?;
            return Some(x.min(y));
        }
        "max" => {
            let x = recurse(args.first()?)?;
            let y = recurse(args.get(1)?)?;
            return Some(x.max(y));
        }
        _ => {}
    }

    let function_def = lookup_function_definition(&func_name, qualified_name.as_deref(), env.tree)?;
    eval_user_defined_integer_function(function_def, args, env, depth, local_ints)
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
