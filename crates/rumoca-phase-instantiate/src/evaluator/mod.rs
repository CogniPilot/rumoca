//! ast::Expression evaluation for the instantiate phase.
//!
//! This module handles evaluation of expressions during instantiation:
//! - Boolean conditions for conditional components (MLS §4.8)
//! - Integer expressions for array dimensions (MLS §10.1)
//! - Enum comparisons for parameter-based conditions
//! - ast::StateSelect parsing from annotations

use super::inheritance::{find_class_in_tree, get_effective_components};
use indexmap::IndexMap;
use rumoca_core::{
    IntegerBinaryOperator, eval_integer_binary as eval_common_integer_binary,
    eval_integer_div_builtin,
};
use rumoca_ir_ast as ast;
use rustc_hash::FxHashMap;

/// Maximum recursion depth for condition evaluation (prevents stack overflow)
const MAX_CONDITION_DEPTH: usize = 10;

/// Maximum recursion depth for expression evaluation.
const MAX_EXPR_EVAL_DEPTH: usize = 20;

/// Convert a boolean literal expression to its value.
pub(crate) fn expr_to_bool(expr: &ast::Expression) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } => match &*token.text {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// Try to evaluate a boolean literal expression.
pub(crate) fn try_eval_bool_literal(expr: &ast::Expression) -> Option<bool> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
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
pub(crate) fn evaluate_component_condition(
    condition: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<bool> {
    evaluate_component_condition_with_depth(condition, mod_env, effective_components, tree, 0)
}

fn evaluate_component_condition_with_depth(
    condition: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
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
        && let Some(val) = eval_param_ref(comp_ref, mod_env, effective_components, tree, depth)
    {
        return Some(val);
    }

    // Case 3: Unary 'not' operator
    if let ast::Expression::Unary { op, rhs } = condition
        && matches!(op, rumoca_ir_ast::OpUnary::Not(_))
        && let Some(inner) = evaluate_component_condition_with_depth(
            rhs,
            mod_env,
            effective_components,
            tree,
            depth + 1,
        )
    {
        return Some(!inner);
    }

    // Case 4: Parenthesized expressions
    if let ast::Expression::Parenthesized { inner } = condition {
        return evaluate_component_condition_with_depth(
            inner,
            mod_env,
            effective_components,
            tree,
            depth,
        );
    }

    // Case 5: Binary operations
    if let ast::Expression::Binary { op, lhs, rhs } = condition
        && let Some(val) =
            eval_binary_condition(op, lhs, rhs, mod_env, effective_components, tree, depth)
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
                depth + 1,
            )
        {
            return Some(val);
        }
    }

    // Look up the parameter's default value from effective components (single-part only)
    if comp_ref.parts.len() == 1 {
        let param_name = comp_ref.parts[0].ident.text.as_ref();
        let sibling = effective_components.get(param_name)?;
        // Try simple boolean extraction first
        if let Some(val) = expr_to_bool(&sibling.start) {
            return Some(val);
        }
        // Try recursive evaluation for expressions like controllerType == PI
        return evaluate_component_condition_with_depth(
            &sibling.start,
            mod_env,
            effective_components,
            tree,
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
    op: &rumoca_ir_ast::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> Option<bool> {
    let eval = |e| {
        evaluate_component_condition_with_depth(e, mod_env, effective_components, tree, depth + 1)
    };
    let enum_eq = || {
        evaluate_enum_equality_with_depth(lhs, rhs, mod_env, effective_components, tree, depth + 1)
    };
    let int_eval =
        |e| try_eval_integer_expr_with_depth(e, mod_env, effective_components, tree, depth + 1);

    match op {
        ast::OpBinary::Or(_) => {
            let (l, r) = (eval(lhs), eval(rhs));
            if l == Some(true) || r == Some(true) {
                return Some(true);
            }
            if l == Some(false) && r == Some(false) {
                return Some(false);
            }
        }
        ast::OpBinary::And(_) => {
            let (l, r) = (eval(lhs), eval(rhs));
            if l == Some(false) || r == Some(false) {
                return Some(false);
            }
            if l == Some(true) && r == Some(true) {
                return Some(true);
            }
        }
        ast::OpBinary::Eq(_) => {
            // Try enum comparison first, then fall back to integer comparison
            if let Some(val) = enum_eq() {
                return Some(val);
            }
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l == r);
            }
        }
        ast::OpBinary::Neq(_) => {
            if let Some(val) = enum_eq() {
                return Some(!val);
            }
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l != r);
            }
        }
        ast::OpBinary::Lt(_) => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l < r);
            }
        }
        ast::OpBinary::Le(_) => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l <= r);
            }
        }
        ast::OpBinary::Gt(_) => {
            if let (Some(l), Some(r)) = (int_eval(lhs), int_eval(rhs)) {
                return Some(l > r);
            }
        }
        ast::OpBinary::Ge(_) => {
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
    depth: usize,
) -> Option<bool> {
    // Prevent deep recursion
    if depth > MAX_CONDITION_DEPTH {
        return None;
    }

    // Get values from both sides (could be enum literals or parameter references).
    // If a side stays as an unresolved component reference, keep comparison unknown.
    let lhs_val =
        enum_value_for_comparison_with_depth(lhs, mod_env, effective_components, tree, None, depth);
    let rhs_val =
        enum_value_for_comparison_with_depth(rhs, mod_env, effective_components, tree, None, depth);

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
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<String> {
    let value = get_enum_value_with_depth(
        expr,
        mod_env,
        effective_components,
        tree,
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

/// Check if two enum values are equal, handling qualified name differences.
///
/// Enum values can be:
/// - Fully qualified: "Modelica.Blocks.Types.SimpleController.PI"
/// - Short form: "SimpleController.PI"
/// - Just the value: "PI" (rare but possible)
///
/// We consider them equal if the shorter one is a suffix of the longer one.
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
        } => {
            let s = token.text.trim_matches('"');
            Some(s.to_string())
        }
        ast::Expression::ComponentReference(comp_ref) => {
            resolve_component_ref_expr(comp_ref, mod_env, effective_components, tree, scope_prefix)
                .and_then(|(resolved_expr, next_scope)| {
                    get_enum_value_with_depth(
                        &resolved_expr,
                        mod_env,
                        effective_components,
                        tree,
                        next_scope.as_deref(),
                        depth + 1,
                    )
                })
                .or_else(|| {
                    component_ref_to_dotted_no_subscripts(comp_ref).map(|_| comp_ref.to_string())
                })
        }
        ast::Expression::Parenthesized { inner } => get_enum_value_with_depth(
            inner,
            mod_env,
            effective_components,
            tree,
            scope_prefix,
            depth + 1,
        ),
        ast::Expression::If {
            branches,
            else_branch,
        } => eval_if_enum_value(
            branches,
            else_branch,
            mod_env,
            effective_components,
            tree,
            scope_prefix,
            depth + 1,
        ),
        _ => None,
    }
}

fn parent_dotted_scope(path: &str) -> Option<String> {
    path.rsplit_once('.').map(|(prefix, _)| prefix.to_string())
}

fn eval_if_enum_value(
    branches: &[(ast::Expression, ast::Expression)],
    else_branch: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<String> {
    for (cond, branch_expr) in branches {
        match eval_scoped_string_condition_with_depth(
            cond,
            mod_env,
            effective_components,
            tree,
            scope_prefix,
            depth,
        ) {
            Some(true) => {
                return get_enum_value_with_depth(
                    branch_expr,
                    mod_env,
                    effective_components,
                    tree,
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
        mod_env,
        effective_components,
        tree,
        scope_prefix,
        depth,
    )
}

fn resolve_component_ref_expr(
    comp_ref: &ast::ComponentReference,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    scope_prefix: Option<&str>,
) -> Option<(ast::Expression, Option<String>)> {
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    let candidate_paths = candidate_paths_for_ref(comp_ref, dotted.as_str(), scope_prefix);

    lookup_exact_component_ref(candidate_paths.as_slice(), mod_env, effective_components)
        .or_else(|| lookup_suffix_component_ref(dotted.as_str(), mod_env, effective_components))
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
        if let Some(mod_value) = mod_env.get(&ast::QualifiedName::from_dotted(candidate)) {
            return Some((mod_value.value.clone(), parent_dotted_scope(candidate)));
        }
        if let Some(comp) = effective_components.get(candidate.as_str()) {
            return Some((comp.start.clone(), parent_dotted_scope(candidate)));
        }
    }
    None
}

fn lookup_suffix_component_ref(
    dotted: &str,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
) -> Option<(ast::Expression, Option<String>)> {
    let dotted_suffix = format!(".{dotted}");
    unique_suffix_mod_value(dotted_suffix.as_str(), mod_env)
        .or_else(|| unique_suffix_component_value(dotted_suffix.as_str(), effective_components))
}

fn unique_suffix_mod_value(
    dotted_suffix: &str,
    mod_env: &ast::ModificationEnvironment,
) -> Option<(ast::Expression, Option<String>)> {
    let mut found = None;
    for (qn, mv) in &mod_env.active {
        let qn_dotted = qn
            .parts
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        if !qn_dotted.ends_with(dotted_suffix) {
            continue;
        }
        if found.is_some() {
            return None;
        }
        found = Some((mv.value.clone(), parent_dotted_scope(&qn_dotted)));
    }
    found
}

fn unique_suffix_component_value(
    dotted_suffix: &str,
    effective_components: &IndexMap<String, ast::Component>,
) -> Option<(ast::Expression, Option<String>)> {
    let mut found = None;
    for (name, comp) in effective_components {
        if !name.ends_with(dotted_suffix) {
            continue;
        }
        if found.is_some() {
            return None;
        }
        found = Some((comp.start.clone(), parent_dotted_scope(name)));
    }
    found
}

fn eval_scoped_string_condition_with_depth(
    condition: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
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
        eval_scoped_string_condition_with_depth(
            expr,
            mod_env,
            effective_components,
            tree,
            scope_prefix,
            depth + 1,
        )
    };

    match condition {
        ast::Expression::Unary {
            op: rumoca_ir_ast::OpUnary::Not(_),
            rhs,
        } => recurse(rhs).map(|v| !v),
        ast::Expression::Parenthesized { inner } => recurse(inner),
        ast::Expression::Binary { op, lhs, rhs } => eval_scoped_string_binary_condition(
            op,
            lhs,
            rhs,
            mod_env,
            effective_components,
            tree,
            ScopedEvalState {
                scope_prefix,
                depth: depth + 1,
            },
        )
        .or_else(|| {
            evaluate_component_condition_with_depth(
                condition,
                mod_env,
                effective_components,
                tree,
                depth + 1,
            )
        }),
        _ => evaluate_component_condition_with_depth(
            condition,
            mod_env,
            effective_components,
            tree,
            depth + 1,
        ),
    }
}

struct ScopedEvalState<'a> {
    scope_prefix: Option<&'a str>,
    depth: usize,
}

fn eval_scoped_string_binary_condition(
    op: &rumoca_ir_ast::OpBinary,
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    state: ScopedEvalState<'_>,
) -> Option<bool> {
    match op {
        rumoca_ir_ast::OpBinary::Or(_) => eval_scoped_or(
            lhs,
            rhs,
            mod_env,
            effective_components,
            tree,
            state.scope_prefix,
            state.depth,
        ),
        rumoca_ir_ast::OpBinary::And(_) => eval_scoped_and(
            lhs,
            rhs,
            mod_env,
            effective_components,
            tree,
            state.scope_prefix,
            state.depth,
        ),
        rumoca_ir_ast::OpBinary::Eq(_) => eval_scoped_enum_equality(
            lhs,
            rhs,
            mod_env,
            effective_components,
            tree,
            state.scope_prefix,
            state.depth,
        ),
        rumoca_ir_ast::OpBinary::Neq(_) => eval_scoped_enum_equality(
            lhs,
            rhs,
            mod_env,
            effective_components,
            tree,
            state.scope_prefix,
            state.depth,
        )
        .map(|v| !v),
        _ => None,
    }
}

fn eval_scoped_or(
    lhs: &ast::Expression,
    rhs: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let recurse = |expr| {
        eval_scoped_string_condition_with_depth(
            expr,
            mod_env,
            effective_components,
            tree,
            scope_prefix,
            depth,
        )
    };
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
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let recurse = |expr| {
        eval_scoped_string_condition_with_depth(
            expr,
            mod_env,
            effective_components,
            tree,
            scope_prefix,
            depth,
        )
    };
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
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    scope_prefix: Option<&str>,
    depth: usize,
) -> Option<bool> {
    let lhs_val = enum_value_for_comparison_with_depth(
        lhs,
        mod_env,
        effective_components,
        tree,
        scope_prefix,
        depth,
    )?;
    let rhs_val = enum_value_for_comparison_with_depth(
        rhs,
        mod_env,
        effective_components,
        tree,
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
    Some(field.start.clone())
}

fn resolve_component_ref_from_record_defaults(
    comp_ref: &ast::ComponentReference,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<ast::Expression> {
    let dotted = component_ref_to_dotted_no_subscripts(comp_ref)?;
    if !dotted.contains('.') {
        return None;
    }
    let mut parts = dotted.split('.');
    let first = parts.next()?;
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
        expr = Some(field_comp.start.clone());
        current = field_comp;
    }

    expr
}

/// Try to extract a string from an expression.
pub(crate) fn expr_to_string(expr: &ast::Expression) -> Option<String> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
        } => {
            // Remove surrounding quotes if present
            let s = token.text.trim_matches('"');
            Some(s.to_string())
        }
        _ => None,
    }
}

/// Try to evaluate an expression to a string/enum value without falling back to
/// unresolved component references.
pub(crate) fn try_eval_string_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<String> {
    let value = get_enum_value_with_depth(expr, mod_env, effective_components, tree, None, 0)?;
    if let ast::Expression::ComponentReference(comp_ref) = expr
        && value == comp_ref.to_string()
    {
        return None;
    }
    Some(value)
}

/// Parse a ast::StateSelect value from an expression.
pub(crate) fn parse_state_select(expr: &ast::Expression) -> ast::StateSelect {
    // ast::StateSelect is typically a qualified name like ast::StateSelect.prefer
    if let ast::Expression::ComponentReference(comp_ref) = expr
        && let Some(last) = comp_ref.parts.last()
    {
        match &*last.ident.text {
            "never" => return ast::StateSelect::Never,
            "avoid" => return ast::StateSelect::Avoid,
            "default" => return ast::StateSelect::Default,
            "prefer" => return ast::StateSelect::Prefer,
            "always" => return ast::StateSelect::Always,
            _ => {}
        }
    }
    ast::StateSelect::Default
}

/// Extract binding from declaration or modification.
///
/// MLS §7.2: Modifications from outer scopes override inner bindings.
/// A modification like `p(k = 5.0)` provides a binding value for component k.
pub(crate) fn extract_binding(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
) -> (Option<ast::Expression>, bool, Option<ast::QualifiedName>) {
    // Check mod_env for binding override (outer modification takes precedence)
    // The binding modification is stored under just the component name
    let binding_path = ast::QualifiedName::from_ident(&comp.name);
    if let Some(mod_value) = mod_env.get(&binding_path) {
        return (
            Some(mod_value.value.clone()),
            true,
            mod_value.source_scope.clone(),
        );
    }

    // Check if the component has an explicit binding from declaration
    // Use the dedicated `binding` field which preserves the binding even when
    // there's a separate start= modifier (e.g., `Real v(start=V0) = p.v - n.v`)
    if let Some(binding) = &comp.binding {
        return (Some(binding.clone()), false, None);
    }

    // Fallback: check start when has_explicit_binding is true
    // Handles code paths where binding may not be populated (e.g. modification-only declarations)
    if comp.has_explicit_binding
        && !comp.start_is_modification
        && !matches!(comp.start, ast::Expression::Empty)
    {
        return (Some(comp.start.clone()), false, None);
    }

    (None, false, None)
}

/// Extract boolean parameter values from components for conditional equation evaluation.
///
/// This enables proper handling of patterns like:
/// ```modelica
/// if use_numberPort then connect(numberPort, showNumber); else ... end if;
/// ```
///
/// Returns a map of component names to their boolean values.
/// Takes the modification environment to check for parameter overrides.
pub(crate) fn extract_bool_params_with_mods(
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
) -> FxHashMap<String, bool> {
    let mut bool_params = FxHashMap::default();

    for (name, comp) in effective_components {
        // Only look at boolean parameters
        if !matches!(comp.variability, rumoca_ir_ast::Variability::Parameter(_)) {
            continue;
        }

        // Check if it's a Boolean type
        let type_name = comp.type_name.to_string();
        if type_name != "Boolean" && !type_name.ends_with(".Boolean") {
            continue;
        }

        // Check modification environment first (outer modifications override inner values)
        let mod_path = ast::QualifiedName::from_ident(name);
        if let Some(mod_value) = mod_env.get(&mod_path)
            && let Some(value) = try_eval_bool_literal(&mod_value.value)
        {
            bool_params.insert(name.clone(), value);
            continue;
        }

        // Fall back to value from component's start/binding
        if let Some(value) = try_eval_bool_literal(&comp.start) {
            bool_params.insert(name.clone(), value);
        }
    }

    bool_params
}

/// Extract integer parameter values from components for for-loop range evaluation.
///
/// This enables proper handling of patterns like:
/// ```modelica
/// for k in 1:m loop connect(plug_p.pin[k], resistor[k].p); end for;
/// ```
///
/// Returns a map of component names to their integer values.
/// Takes the modification environment to check for parameter overrides.
pub(crate) fn extract_int_params_with_mods(
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
    tree: &ast::ClassTree,
) -> FxHashMap<String, i64> {
    let mut int_params = FxHashMap::default();

    for (name, comp) in effective_components {
        // Only look at parameters (including constants which might have integer values)
        if !matches!(
            comp.variability,
            rumoca_ir_ast::Variability::Parameter(_) | rumoca_ir_ast::Variability::Constant(_)
        ) {
            continue;
        }

        // Try to evaluate as an integer - we don't check the declared type,
        // just try to get an integer value. This handles both `Integer m = 3`
        // and type aliases like `Modelica.SIunits.Integer m = 3`.

        // Check modification environment first (outer modifications override inner values)
        let mod_path = ast::QualifiedName::from_ident(name);
        if let Some(mod_value) = mod_env.get(&mod_path)
            && let Some(value) =
                try_eval_integer_expr(&mod_value.value, mod_env, effective_components, tree)
        {
            int_params.insert(name.clone(), value);
            continue;
        }

        // Fall back to declaration value, preferring explicit binding over start.
        if let Some(value_expr) = component_expr_for_structural_eval(comp)
            && let Some(value) =
                try_eval_integer_expr(value_expr, mod_env, effective_components, tree)
        {
            int_params.insert(name.clone(), value);
        }
    }

    // Also add dotted keys from multi-part modifications in mod_env.
    // This handles record field references like cellData.nRC used in for-loop ranges.
    for (qn, mod_value) in &mod_env.active {
        if qn.parts.len() > 1 {
            let dotted_key: String = qn
                .parts
                .iter()
                .map(|(name, _)| name.as_str())
                .collect::<Vec<_>>()
                .join(".");
            if int_params.contains_key(&dotted_key) {
                continue;
            }
            if let Some(value) =
                try_eval_integer_expr(&mod_value.value, mod_env, effective_components, tree)
            {
                int_params.insert(dotted_key, value);
            }
        }
    }

    // If a record parameter is rebound by reference (e.g., `cellData = cellData2`),
    // propagate integer fields from the target record (`cellData2.nRC`) to the
    // bound name (`cellData.nRC`) so for-loop ranges use the overridden values.
    propagate_record_alias_integer_params(&mut int_params, mod_env);

    int_params
}

/// Return the declaration-side value expression for structural evaluation.
///
/// MLS §4.4.4 / §7.2: structural parameter values come from declaration bindings
/// (`x = expr`) and applied modifications; `start` is only an initialization
/// attribute for simulation variables and should be secondary for compile-time shape.
fn component_expr_for_structural_eval(comp: &ast::Component) -> Option<&ast::Expression> {
    if let Some(binding) = comp.binding.as_ref() {
        return Some(binding);
    }
    if comp.has_explicit_binding
        && !comp.start_is_modification
        && !matches!(comp.start, ast::Expression::Empty)
    {
        return Some(&comp.start);
    }
    (!matches!(comp.start, ast::Expression::Empty)).then_some(&comp.start)
}

/// Generate enclosing-scope lookup paths for a dotted reference.
///
/// Example: `pipe2.flowModel.nFM` -> [`pipe2.nFM`, `nFM`]
/// This models lexical scope climbing for nested component members.
fn enclosing_scope_candidates(dotted: &str) -> Vec<String> {
    let mut parts: Vec<&str> = dotted.split('.').collect();
    let mut candidates = Vec::new();
    while parts.len() > 1 {
        let remove_idx = parts.len() - 2;
        parts.remove(remove_idx);
        candidates.push(parts.join("."));
    }
    candidates
}

pub(crate) fn propagate_record_alias_integer_params(
    int_params: &mut FxHashMap<String, i64>,
    mod_env: &ast::ModificationEnvironment,
) {
    let aliases = collect_record_aliases(mod_env);
    if aliases.is_empty() {
        return;
    }

    // Resolve simple alias chains (a=b, b=c) without unbounded growth.
    const MAX_ALIAS_PROPAGATION_PASSES: usize = 8;
    for _ in 0..MAX_ALIAS_PROPAGATION_PASSES {
        let mut changed = false;

        for (alias_name, target_name) in &aliases {
            if alias_name == target_name {
                continue;
            }

            if let Some(value) = int_params.get(target_name).copied() {
                let prev = int_params.insert(alias_name.clone(), value);
                changed |= prev != Some(value);
            }

            let target_prefix = format!("{target_name}.");
            let alias_prefix = format!("{alias_name}.");
            let propagated: Vec<_> = int_params
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix(&target_prefix)
                        .map(|suffix| (format!("{alias_prefix}{suffix}"), *value))
                })
                .collect();

            for (key, value) in propagated {
                let prev = int_params.insert(key, value);
                changed |= prev != Some(value);
            }
        }

        if !changed {
            break;
        }
    }
}

fn collect_record_aliases(mod_env: &ast::ModificationEnvironment) -> Vec<(String, String)> {
    let mut aliases = Vec::new();
    for (target_qn, mod_value) in &mod_env.active {
        if target_qn.parts.len() != 1 {
            continue;
        }
        let Some(alias_name) = target_qn.first_name() else {
            continue;
        };
        let ast::Expression::ComponentReference(comp_ref) = &mod_value.value else {
            continue;
        };
        let Some(target_name) = component_ref_to_dotted_no_subscripts(comp_ref) else {
            continue;
        };
        aliases.push((alias_name.to_string(), target_name));
    }
    aliases
}

fn component_ref_to_dotted_no_subscripts(comp_ref: &ast::ComponentReference) -> Option<String> {
    if comp_ref.parts.is_empty() || comp_ref.parts.iter().any(|part| part.subs.is_some()) {
        return None;
    }
    Some(
        comp_ref
            .parts
            .iter()
            .map(|part| part.ident.text.as_ref())
            .collect::<Vec<_>>()
            .join("."),
    )
}

#[derive(Copy, Clone)]
struct IntegerEvalEnv<'a> {
    mod_env: &'a ast::ModificationEnvironment,
    effective_components: &'a IndexMap<String, ast::Component>,
    tree: &'a ast::ClassTree,
}

/// Try to evaluate an integer expression for array dimension expansion.
/// Used to expand array components during instantiation (MLS §10.1).
pub(crate) fn try_eval_integer_expr(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
) -> Option<i64> {
    try_eval_integer_expr_with_depth_and_locals(expr, mod_env, effective_components, tree, 0, None)
}

fn try_eval_integer_expr_with_depth(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    depth: usize,
) -> Option<i64> {
    try_eval_integer_expr_with_depth_and_locals(
        expr,
        mod_env,
        effective_components,
        tree,
        depth,
        None,
    )
}

fn try_eval_integer_expr_with_depth_and_locals(
    expr: &ast::Expression,
    mod_env: &ast::ModificationEnvironment,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
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
    };

    let recurse = |e| {
        try_eval_integer_expr_with_depth_and_locals(
            e,
            env.mod_env,
            env.effective_components,
            env.tree,
            depth + 1,
            local_ints,
        )
    };

    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse::<i64>().ok(),
        ast::Expression::ComponentReference(comp_ref) => {
            eval_integer_component_ref(comp_ref, env, depth, local_ints)
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            let l = recurse(lhs)?;
            let r = recurse(rhs)?;
            eval_integer_binary(op, l, r)
        }
        ast::Expression::Unary { op, rhs } => {
            let r = recurse(rhs)?;
            match op {
                rumoca_ir_ast::OpUnary::Minus(_) => Some(-r),
                rumoca_ir_ast::OpUnary::Plus(_) => Some(r),
                _ => None,
            }
        }
        ast::Expression::Parenthesized { inner } => recurse(inner),
        ast::Expression::FunctionCall { comp, args } => {
            eval_integer_function_call(comp, args, env, depth, local_ints)
        }
        _ => None,
    }
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

    let dotted_suffix = format!(".{dotted}");
    let mut mod_suffix_match = None;
    for (qn, mv) in &env.mod_env.active {
        let qn_dotted = qn
            .parts
            .iter()
            .map(|(name, _)| name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        if qn_dotted.ends_with(&dotted_suffix) && mod_suffix_match.replace(mv).is_some() {
            mod_suffix_match = None;
            break;
        }
    }
    if let Some(mv) = mod_suffix_match {
        return recurse(&mv.value);
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

    let mut comp_suffix_match: Option<&ast::Component> = None;
    for (name, comp) in env.effective_components {
        if name.ends_with(&dotted_suffix) && comp_suffix_match.replace(comp).is_some() {
            comp_suffix_match = None;
            break;
        }
    }
    if let Some(comp) = comp_suffix_match
        && let Some(value_expr) = component_expr_for_structural_eval(comp)
        && let Some(value) = recurse(value_expr)
    {
        return Some(value);
    }

    if let Some(value) = eval_integer_class_redeclare_field_ref(comp_ref, env, depth, local_ints) {
        return Some(value);
    }

    // Resolve record field references from defaults/modifications in the
    // component's declared type scope (e.g., `data.mSystems`, `data.mBasic`).
    eval_integer_record_field_ref(comp_ref, env, depth)
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
    let effective_components = get_effective_components(env.tree, target_class)
        .unwrap_or_else(|_| target_class.components.clone());
    let field_component = effective_components.get(field_name)?;
    let value_expr = component_expr_for_structural_eval(field_component)?;

    try_eval_integer_expr_with_depth_and_locals(
        value_expr,
        env.mod_env,
        &effective_components,
        env.tree,
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
    let type_def_id = root_comp.type_def_id?;
    let record_class = env.tree.get_class_by_def_id(type_def_id)?;
    if record_class.class_type != ast::ClassType::Record {
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
        let field_expr = field_mod.or(extends_override).unwrap_or(&field_comp.start);

        if let Some(value) = try_eval_integer_expr_with_depth_and_locals(
            field_expr,
            env.mod_env,
            env.effective_components,
            env.tree,
            depth + 1,
            Some(&local_values),
        ) {
            local_values.insert(name.clone(), value);
        }

        if name == field_name {
            if let Some(value) = local_values.get(name) {
                return Some(*value);
            }
            return try_eval_integer_expr_with_depth_and_locals(
                field_expr,
                env.mod_env,
                env.effective_components,
                env.tree,
                depth + 1,
                Some(&local_values),
            );
        }
    }

    None
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
                ast::Expression::Modification { target, value }
                    if target.parts.len() == 1
                        && target.parts[0].ident.text.as_ref() == field_name
                        && !matches!(value.as_ref(), ast::Expression::Empty) =>
                {
                    field_expr = Some(value.as_ref());
                }
                ast::Expression::NamedArgument { name, value }
                    if name.text.as_ref() == field_name
                        && !matches!(value.as_ref(), ast::Expression::Empty) =>
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
    let dotted_suffix = format!(".{dotted}");
    let root_suffix = format!(".{root_name}");
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
        let candidate_expr = if qn_dotted == dotted || qn_dotted.ends_with(&dotted_suffix) {
            Some(&mod_value.value)
        } else if qn_dotted == root_name || qn_dotted.ends_with(&root_suffix) {
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
            ast::Expression::NamedArgument { name, value }
                if name.text.as_ref() == field_name
                    && !matches!(value.as_ref(), ast::Expression::Empty) =>
            {
                Some(value.as_ref())
            }
            ast::Expression::Modification { target, value }
                if target.parts.len() == 1
                    && target.parts[0].ident.text.as_ref() == field_name
                    && !matches!(value.as_ref(), ast::Expression::Empty) =>
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

fn eval_integer_binary(op: &rumoca_ir_ast::OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    let operator = match op {
        rumoca_ir_ast::OpBinary::Add(_) => IntegerBinaryOperator::Add,
        rumoca_ir_ast::OpBinary::Sub(_) => IntegerBinaryOperator::Sub,
        rumoca_ir_ast::OpBinary::Mul(_) => IntegerBinaryOperator::Mul,
        rumoca_ir_ast::OpBinary::Div(_) => IntegerBinaryOperator::Div,
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

/// Evaluate a function call to an integer value during instantiation.
///
/// Handles Modelica builtins (integer, mod, div, abs) and user-defined pure
/// functions by looking them up in the ast::ClassTree and evaluating with rumoca_eval_flat.
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
        && class.class_type == ast::ClassType::Function
    {
        return Some(class);
    }

    if let Some(class) = tree.get_class_by_qualified_name(func_name)
        && class.class_type == ast::ClassType::Function
    {
        return Some(class);
    }

    let suffix = format!(".{func_name}");
    let mut matches = tree
        .name_map
        .keys()
        .filter(|qualified| qualified.ends_with(&suffix) || qualified.as_str() == func_name);
    let first = matches.next()?;
    if matches.next().is_some() {
        return None;
    }
    let class = tree.get_class_by_qualified_name(first)?;
    (class.class_type == ast::ClassType::Function).then_some(class)
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
    bind_function_inputs(
        function_def,
        args,
        env,
        depth + 1,
        caller_locals,
        &mut local_values,
    )?;
    initialize_function_locals(function_def, env, depth + 1, &mut local_values);
    let output_name = find_function_output_name(function_def)?;

    for algorithm in &function_def.algorithms {
        if interpret_function_statements(algorithm, env, depth + 1, &mut local_values)? {
            break;
        }
    }

    local_values.get(&output_name).copied()
}

mod function_eval_and_tests;
use function_eval_and_tests::{
    bind_function_inputs, find_function_output_name, initialize_function_locals,
    interpret_function_statements,
};
pub(crate) use function_eval_and_tests::{
    evaluate_array_dimensions, generate_array_indices, try_eval_integer_shape_expr,
};
