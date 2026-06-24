//! AST expression evaluation for the instantiate phase.
//!
//! This module handles evaluation of expressions during instantiation:
//! - Boolean conditions for conditional components (MLS §4.8)
//! - Integer expressions for array dimensions (MLS §10.1)
//! - Enum comparisons for parameter-based conditions
//! - StateSelect parsing from annotations

use rumoca_core::{
    IntegerBinaryOperator, eval_integer_binary as eval_common_integer_binary,
    eval_integer_div_builtin,
};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use rustc_hash::FxHashMap;

pub type ResolveClassComponents<'a> =
    dyn Fn(&ast::ClassTree, &ast::ClassDef) -> IndexMap<String, ast::Component> + 'a;

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
    pub resolve_class_components: &'a ResolveClassComponents<'a>,
}

#[derive(Copy, Clone)]
struct ConditionEvalEnv<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    resolve_class_components: &'a ResolveClassComponents<'a>,
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
    let InstantiateEvalCtx {
        tree,
        mod_env,
        effective_components,
        resolve_class_components,
    } = ctx;
    evaluate_component_condition_with_depth(
        condition,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        0,
    )
}

fn evaluate_component_condition_with_depth(
    condition: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
    depth: usize,
) -> Option<bool> {
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    // Case 1: Direct boolean literal
    if let Some(val) = expr_to_bool(condition) {
        return Some(val);
    }

    // Case 2: ast::Component reference to a parameter
    if let ast::Expression::ComponentReference(comp_ref) = condition
        && let Some(val) = eval_param_ref(
            comp_ref,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth,
        )
    {
        return Some(val);
    }

    // Case 3: Unary 'not' operator
    if let ast::Expression::Unary { op, rhs, .. } = condition
        && matches!(op, rumoca_core::OpUnary::Not)
        && let Some(inner) = evaluate_component_condition_with_depth(
            rhs,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth + 1,
        )
    {
        return Some(!inner);
    }

    // Case 4: Parenthesized expressions
    if let ast::Expression::Parenthesized { inner, .. } = condition {
        return evaluate_component_condition_with_depth(
            inner,
            mod_env,
            effective_components,
            tree,
            resolve_class_components,
            depth,
        );
    }

    // Case 5: Binary operations
    if let ast::Expression::Binary { op, lhs, rhs, .. } = condition
        && let Some(val) = eval_binary_condition(
            op,
            lhs,
            rhs,
            ConditionEvalEnv {
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
            },
            depth,
        )
    {
        return Some(val);
    }

    None
}

/// Evaluate a parameter reference in a condition.
fn eval_param_ref(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
    depth: usize,
) -> Option<bool> {
    // Build qualified path for multi-part references (e.g., smpmData.useDamperCage)
    let param_path = build_qualified_path(comp_ref);

    // Check modification environment for an override
    if let Some(mod_value) = mod_env.get(&param_path) {
        if let Some(val) = expr_to_bool(&mod_value.value) {
            return Some(val);
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
    }

    // Type attributes may be evaluated while extracting fields of a structured
    // component. In MSL media records, expressions such as
    // `medium.preferredMediumStates` refer to the current structured component's
    // `preferredMediumStates` parameter even after the field is being processed.
    if comp_ref.parts.len() > 1
        && let Some(last) = comp_ref.parts.last()
    {
        let field_name = last.ident.text.as_ref();
        if let Some(sibling) = effective_components.get(field_name) {
            let value_expr = component_params::component_condition_value_expr(sibling);
            if let Some(val) = expr_to_bool(value_expr) {
                return Some(val);
            }
            if let Some(val) = evaluate_component_condition_with_depth(
                value_expr,
                mod_env,
                effective_components,
                tree,
                resolve_class_components,
                depth + 1,
            ) {
                return Some(val);
            }
        }
    }

    // Look up the parameter's default value from effective components (single-part only)
    if comp_ref.parts.len() == 1 {
        let param_name = comp_ref.parts[0].ident.text.as_ref();
        let sibling = effective_components.get(param_name)?;
        // Try simple boolean extraction first
        let value_expr = component_params::component_condition_value_expr(sibling);
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
    // (`P.pT_explicit`) resolve through the class tree.
    if let Some(binding) =
        resolve_class_redeclare_field_expr(comp_ref, mod_env, tree, resolve_class_components)
            .or_else(|| resolve_class_constant_binding(comp_ref, tree, resolve_class_components))
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

/// Evaluate a binary condition (or, and, eq, neq, relational).
///
/// MLS §4.8: Conditional component conditions must be evaluable at compile time.
/// Supports enum comparison, integer comparison, and relational operators.
fn eval_binary_condition(
    op: &rumoca_core::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    depth: usize,
) -> Option<bool> {
    let eval = |e| {
        evaluate_component_condition_with_depth(
            e,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
        )
    };
    let enum_eq = || {
        evaluate_enum_equality_with_depth(
            lhs,
            rhs,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
        )
    };
    let int_eval = |e| {
        try_eval_integer_expr_with_depth(
            e,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
        )
    };

    match op {
        rumoca_core::OpBinary::Or => {
            let (l, r) = (eval(lhs), eval(rhs));
            if l == Some(true) || r == Some(true) {
                return Some(true);
            }
            if l == Some(false) && r == Some(false) {
                return Some(false);
            }
        }
        rumoca_core::OpBinary::And => {
            let (l, r) = (eval(lhs), eval(rhs));
            if l == Some(false) || r == Some(false) {
                return Some(false);
            }
            if l == Some(true) && r == Some(true) {
                return Some(true);
            }
        }
        rumoca_core::OpBinary::Eq => {
            // Enum equality is more specific than integer equality.
            if let Some(val) = enum_eq() {
                return Some(val);
            }
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l == r);
            }
        }
        rumoca_core::OpBinary::Neq => {
            if let Some(val) = enum_eq() {
                return Some(!val);
            }
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l != r);
            }
        }
        rumoca_core::OpBinary::Lt => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l < r);
            }
        }
        rumoca_core::OpBinary::Le => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l <= r);
            }
        }
        rumoca_core::OpBinary::Gt => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l > r);
            }
        }
        rumoca_core::OpBinary::Ge => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l >= r);
            }
        }
        _ => {}
    }
    None
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
    resolve_class_components: &ResolveClassComponents<'_>,
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
    resolve_class_components: &ResolveClassComponents<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<String> {
    let value = get_enum_value_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
        scope_prefix,
        depth,
    )?;
    // MLS §4.8: compile-time conditions must only use values that are
    // actually known at this point; unresolved variable refs stay unknown.
    if let ast::Expression::ComponentReference(comp_ref) = expr
        && value == comp_ref.to_string()
        && unresolved_component_ref_looks_like_variable(
            comp_ref,
            mod_env,
            effective_components,
            scope_prefix,
        )
    {
        return None;
    }
    Some(value)
}

fn unresolved_component_ref_looks_like_variable(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
) -> bool {
    let Some(first_part) = comp_ref.parts.first() else {
        return false;
    };
    let first = first_part.ident.text.as_ref();

    if effective_components.contains_key(first)
        || mod_env
            .get(&ast::QualifiedName::from_ident(first))
            .is_some()
    {
        return true;
    }

    if let Some(prefix) = scope_prefix {
        let scoped = format!("{prefix}.{first}");
        if effective_components.contains_key(&scoped)
            || mod_env
                .get(&ast::QualifiedName::from_dotted(&scoped))
                .is_some()
        {
            return true;
        }
    }

    false
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

/// Get an enum value from an expression.
///
/// Handles:
/// - Enum literals (e.g., SimpleController.PI)
/// - Parameter references that resolve to enum values
fn get_enum_value_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<String> {
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
            Some(s.to_string())
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
        .or_else(|| component_ref_to_dotted_no_subscripts(comp_ref).map(|_| comp_ref.to_string())),
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
) -> Option<String> {
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
    resolve_class_components: &dyn Fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
    scope_prefix: Option<&str>,
) -> Option<(ast::Expression, Option<String>)> {
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    let candidate_paths = candidate_paths_for_ref(comp_ref, dotted.as_str(), scope_prefix);

    lookup_exact_component_ref(candidate_paths.as_slice(), mod_env, effective_components)
        .or_else(|| {
            resolve_component_ref_from_record_defaults(comp_ref, effective_components, tree)
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

/// Resolve a qualified reference like `P.pT_explicit` to the binding of a
/// class-level constant. Enclosing-scope constants are qualified to their
/// declaring class by the package-constant alias pass (MLS §5.3.2), so this
/// is the evaluation counterpart of that lexical lookup.
fn resolve_class_constant_binding(
    comp_ref: &ast::ComponentReference,
    tree: &ast::ClassTree,
    resolve_class_components: &dyn Fn(
        &ast::ClassTree,
        &ast::ClassDef,
    ) -> IndexMap<String, ast::Component>,
) -> Option<ast::Expression> {
    if comp_ref.parts.len() < 2
        || comp_ref
            .parts
            .iter()
            .any(|part| part.subs.as_ref().is_some_and(|subs| !subs.is_empty()))
    {
        return None;
    }
    let member = comp_ref.parts.last()?.ident.text.as_ref();
    let class_path = comp_ref.parts[..comp_ref.parts.len() - 1]
        .iter()
        .map(|part| part.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    let class = tree.get_class_by_qualified_name(&class_path)?;
    let effective_components = resolve_class_components(tree, class);
    let component = effective_components.get(member)?;
    if !matches!(component.variability, rumoca_core::Variability::Constant(_)) {
        return None;
    }
    component.binding.clone()
}

fn resolve_class_redeclare_field_expr(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    tree: &ast::ClassTree,
    resolve_class_components: &dyn Fn(
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

fn eval_scoped_string_condition_with_depth(
    condition: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    if let Some(val) = expr_to_bool(condition) {
        return Some(val);
    }

    let recurse = |expr: &ast::Expression| {
        eval_scoped_string_condition_with_depth(expr, env, scope_prefix, depth + 1)
    };

    match condition {
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => recurse(rhs).map(|v| !v),
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::Binary { op, lhs, rhs, .. } => eval_scoped_string_binary_condition(
            op,
            lhs,
            rhs,
            env,
            ScopedEvalState {
                scope_prefix,
                depth: depth + 1,
            },
        )
        .or_else(|| {
            evaluate_component_condition_with_depth(
                condition,
                env.mod_env,
                env.effective_components,
                env.tree,
                env.resolve_class_components,
                depth + 1,
            )
        }),
        _ => evaluate_component_condition_with_depth(
            condition,
            env.mod_env,
            env.effective_components,
            env.tree,
            env.resolve_class_components,
            depth + 1,
        ),
    }
}

struct ScopedEvalState<'a> {
    scope_prefix: Option<&'a str>,
    depth: usize,
}

fn eval_scoped_string_binary_condition(
    op: &rumoca_core::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    state: ScopedEvalState<'_>,
) -> Option<bool> {
    match op {
        rumoca_core::OpBinary::Or => eval_scoped_or(lhs, rhs, env, state.scope_prefix, state.depth),
        rumoca_core::OpBinary::And => {
            eval_scoped_and(lhs, rhs, env, state.scope_prefix, state.depth)
        }
        rumoca_core::OpBinary::Eq => {
            eval_scoped_enum_equality(lhs, rhs, env, state.scope_prefix, state.depth)
        }
        rumoca_core::OpBinary::Neq => {
            eval_scoped_enum_equality(lhs, rhs, env, state.scope_prefix, state.depth).map(|v| !v)
        }
        _ => None,
    }
}

fn eval_scoped_or(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let recurse = |expr| eval_scoped_string_condition_with_depth(expr, env, scope_prefix, depth);
    let (l, r) = (recurse(lhs), recurse(rhs));
    if l == Some(true) || r == Some(true) {
        return Some(true);
    }
    if l == Some(false) && r == Some(false) {
        return Some(false);
    }
    None
}

fn eval_scoped_and(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let recurse = |expr| eval_scoped_string_condition_with_depth(expr, env, scope_prefix, depth);
    let (l, r) = (recurse(lhs), recurse(rhs));
    if l == Some(false) || r == Some(false) {
        return Some(false);
    }
    if l == Some(true) && r == Some(true) {
        return Some(true);
    }
    None
}

fn eval_scoped_enum_equality(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    env: ConditionEvalEnv<'_>,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let lhs_val = enum_value_for_comparison_with_depth(
        lhs,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        scope_prefix,
        depth,
    )?;
    let rhs_val = enum_value_for_comparison_with_depth(
        rhs,
        env.mod_env,
        env.effective_components,
        env.tree,
        env.resolve_class_components,
        scope_prefix,
        depth,
    )?;
    Some(enum_values_equal(&lhs_val, &rhs_val))
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

fn resolve_component_ref_from_record_defaults(
    comp_ref: &ast::ComponentReference,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    if comp_ref.parts.len() < 2 || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let mut parts = comp_ref.parts.iter().map(|part| part.ident.text.as_ref());
    let first: &str = parts.next()?;
    let mut current = effective_components.get(first)?;
    let mut expr = None;

    for field_name in parts {
        if let Some(mod_expr) = current.modifications.get(field_name) {
            expr = Some(mod_expr.clone());
            break;
        }

        let type_def_id = current.type_def_id?;
        let class = tree.get_class_by_def_id(type_def_id)?;
        let field_comp = class.components.get(field_name)?;
        expr = component_expr_for_structural_eval(field_comp).cloned();
        current = field_comp;
    }

    expr
}

#[derive(Copy, Clone)]
pub(super) struct IntegerEvalEnv<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
    resolve_class_components: &'a ResolveClassComponents<'a>,
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
        resolve_class_components,
        0,
        None,
    )
}

fn try_eval_integer_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    resolve_class_components: &ResolveClassComponents<'_>,
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
    resolve_class_components: &ResolveClassComponents<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<i64> {
    if depth > MAX_EXPR_EVAL_DEPTH {
        return None;
    }
    let env = IntegerEvalEnv {
        mod_env,
        effective_components,
        tree,
        resolve_class_components,
    };

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

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse::<i64>().ok(),
        ast::Expression::ComponentReference(comp_ref) => {
            eval_integer_component_ref(comp_ref, env, depth, local_ints)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let l = recurse(lhs)?;
            let r = recurse(rhs)?;
            eval_integer_binary(op, l, r)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            let r = recurse(rhs)?;
            match op {
                rumoca_core::OpUnary::Minus => r.checked_neg(),
                rumoca_core::OpUnary::Plus => Some(r),
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, branch_expr) in branches {
                if try_eval_bool_expr_with_depth_and_locals(condition, env, depth + 1, local_ints)?
                {
                    return recurse(branch_expr);
                }
            }
            recurse(else_branch)
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            eval_integer_function_call(comp, args, env, depth, local_ints)
        }
        _ => None,
    }
}

pub(super) fn try_eval_bool_expr_with_depth_and_locals(
    expr: &ast::Expression,
    env: IntegerEvalEnv<'_>,
    depth: usize,
    local_ints: Option<&FxHashMap<String, i64>>,
) -> Option<bool> {
    try_eval_bool_expr_with_local_values(expr, env, depth, local_ints, None)
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

    let recurse =
        |e| try_eval_bool_expr_with_local_values(e, env, depth + 1, local_ints, local_bools);
    let int_eval = |e| {
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
        ast::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => Some(!recurse(rhs)?),
        ast::Expression::Parenthesized { inner, .. } => recurse(inner),
        ast::Expression::ComponentReference(comp_ref) => {
            local_bools.and_then(|values| lookup_local_bool(comp_ref, values))
        }
        ast::Expression::FunctionCall { comp, args, .. } => {
            eval_bool_function_call(comp, args, env, depth, local_ints, local_bools)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => match op {
            rumoca_core::OpBinary::And => Some(recurse(lhs)? && recurse(rhs)?),
            rumoca_core::OpBinary::Or => Some(recurse(lhs)? || recurse(rhs)?),
            rumoca_core::OpBinary::Eq => {
                if let (Some(lhs), Some(rhs)) = (int_eval(lhs), int_eval(rhs)) {
                    Some(lhs == rhs)
                } else {
                    Some(recurse(lhs)? == recurse(rhs)?)
                }
            }
            rumoca_core::OpBinary::Neq => {
                if let (Some(lhs), Some(rhs)) = (int_eval(lhs), int_eval(rhs)) {
                    Some(lhs != rhs)
                } else {
                    Some(recurse(lhs)? != recurse(rhs)?)
                }
            }
            rumoca_core::OpBinary::Lt => Some(int_eval(lhs)? < int_eval(rhs)?),
            rumoca_core::OpBinary::Le => Some(int_eval(lhs)? <= int_eval(rhs)?),
            rumoca_core::OpBinary::Gt => Some(int_eval(lhs)? > int_eval(rhs)?),
            rumoca_core::OpBinary::Ge => Some(int_eval(lhs)? >= int_eval(rhs)?),
            _ => None,
        },
        _ => None,
    }
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
        .def_id
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
        && let Some(value) = lookup_local_integer(comp_ref, local_values)
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
    if let Some(def_id) = cref.def_id
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

    let mut local_values: FxHashMap<String, i64> = FxHashMap::default();
    for (name, field_comp) in &record_class.components {
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

fn lookup_local_integer(
    comp_ref: &ast::ComponentReference,
    local_values: &FxHashMap<String, i64>,
) -> Option<i64> {
    if comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if let Some(value) = local_values.get(&dotted) {
        return Some(*value);
    }
    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        return local_values.get(name).copied();
    }
    None
}

fn lookup_local_bool(
    comp_ref: &ast::ComponentReference,
    local_values: &FxHashMap<String, bool>,
) -> Option<bool> {
    if comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    let dotted = comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.as_ref())
        .collect::<Vec<_>>()
        .join(".");
    if let Some(value) = local_values.get(&dotted) {
        return Some(*value);
    }
    if comp_ref.parts.len() == 1 {
        let name = comp_ref.parts[0].ident.text.as_ref();
        return local_values.get(name).copied();
    }
    None
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
        .def_id
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
            // MLS §3.7.2: integer(x) truncates Real to Integer
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
    if !function_def.pure || function_def.external.is_some() {
        return None;
    }
    if depth >= MAX_EXPR_EVAL_DEPTH {
        return None;
    }

    let mut local_values = FxHashMap::default();
    function_eval::bind_function_inputs(
        function_def,
        args,
        env,
        depth + 1,
        caller_locals,
        &mut local_values,
    )?;
    function_eval::initialize_function_locals(function_def, env, depth + 1, &mut local_values);
    let output_name = function_eval::find_scalar_function_output_name(function_def)?;

    for algorithm in &function_def.algorithms {
        if function_eval::interpret_function_statements(
            algorithm,
            env,
            depth + 1,
            &mut local_values,
        )? {
            break;
        }
    }

    local_values.get(&output_name).copied()
}

mod component_params;
mod function_eval;
pub(super) use component_params::{
    component_expr_for_structural_eval, component_ref_to_dotted_no_subscripts,
    enclosing_scope_candidates,
};
pub use component_params::{
    eval_state_select_expr, expr_to_string, extract_binding, extract_bool_params_with_mods,
    extract_int_params_with_mods, parse_state_select, propagate_record_alias_integer_params,
    try_eval_string_expr,
};
pub use function_eval::{
    evaluate_array_dimensions, generate_array_indices, try_eval_integer_shape_expr,
};
