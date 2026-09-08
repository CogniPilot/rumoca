//! Locks the initialization projection planner to one fail-closed solve-lowering boundary.

use std::collections::BTreeSet;
use std::fs;

use syn::visit::{self, Visit};

use super::architecture_hardening_support::workspace_root;

const PROJECTION_PATH: &str = "crates/rumoca-phase-solve/src/lower/initial_projection.rs";
const LOWER_PATH: &str = "crates/rumoca-phase-solve/src/lower.rs";
const PARAMETERS_PATH: &str = "crates/rumoca-phase-solve/src/lower/initial_parameters.rs";
const PLANNER: &str = "plan_initialization_projection";
const MATCHER: &str = "match_component";
const ORDERER: &str = "ordered_dependents";
const OWNERSHIP: &str = "initialization_parameter_ownership";

#[test]
fn initialization_planner_boundary_is_fail_closed() {
    let (projection, lower, parameters) = production_sources();
    let violations = boundary_violations(&projection, &lower, &parameters);

    assert!(
        violations.is_empty(),
        "initialization planner boundary drifted: {violations:#?}"
    );
}

#[test]
fn mutations_detect_infallible_planner_and_matcher_apis() {
    let (projection, lower, parameters) = production_sources();
    let infallible_planner = replace_once(
        &projection,
        ") -> Result<InitialProjection, LowerError> {",
        ") -> InitialProjection {",
    );
    assert_violation(
        &infallible_planner,
        &lower,
        &parameters,
        "plan_initialization_projection-return-not-result",
    );

    let infallible_matcher = replace_once(
        &projection,
        ") -> Result<Vec<(usize, InitialUnknown)>, UnmatchedEntity> {",
        ") -> Vec<(usize, InitialUnknown)> {",
    );
    assert_violation(
        &infallible_matcher,
        &lower,
        &parameters,
        "match_component-return-not-result",
    );
}

#[test]
fn mutations_detect_missing_or_duplicated_planner_propagation() {
    let (projection, lower, parameters) = production_sources();
    let call = "initial_projection::plan_initialization_projection(&space, &row_incidence)?;";
    let without_try = replace_once(
        &lower,
        call,
        "initial_projection::plan_initialization_projection(&space, &row_incidence);",
    );
    assert_violation(
        &projection,
        &without_try,
        &parameters,
        "planner-try-call-count:0",
    );

    let duplicate = replace_once(
        &lower,
        "let plan = initial_projection::plan_initialization_projection(&space, &row_incidence)?;",
        "let _duplicate = initial_projection::plan_initialization_projection(&space, &row_incidence)?;\n    let plan = initial_projection::plan_initialization_projection(&space, &row_incidence)?;",
    );
    assert_violation(&projection, &duplicate, &parameters, "planner-call-count:2");
}

#[test]
fn mutations_detect_recovery_calls_inside_planner_and_matcher() {
    let (projection, lower, parameters) = production_sources();
    let planner_unwrap = replace_once(
        &projection,
        ") -> Result<InitialProjection, LowerError> {",
        ") -> Result<InitialProjection, LowerError> {\n    let _recovered = Option::<usize>::None.unwrap();",
    );
    assert_violation(
        &planner_unwrap,
        &lower,
        &parameters,
        "forbidden-recovery-call:plan_initialization_projection:unwrap",
    );

    let matcher_default = replace_once(
        &projection,
        ") -> Result<Vec<(usize, InitialUnknown)>, UnmatchedEntity> {",
        ") -> Result<Vec<(usize, InitialUnknown)>, UnmatchedEntity> {\n    let _recovered = Vec::<usize>::default();",
    );
    assert_violation(
        &matcher_default,
        &lower,
        &parameters,
        "forbidden-recovery-call:match_component:default",
    );

    let matcher_fallback = replace_once(
        &projection,
        ") -> Result<Vec<(usize, InitialUnknown)>, UnmatchedEntity> {",
        ") -> Result<Vec<(usize, InitialUnknown)>, UnmatchedEntity> {\n    let _recovered = Option::<usize>::None.or_else(|| Some(0));",
    );
    assert_violation(
        &matcher_fallback,
        &lower,
        &parameters,
        "forbidden-recovery-call:match_component:or_else",
    );
}

#[test]
fn mutations_detect_parameter_ordering_fallbacks() {
    let (projection, lower, parameters) = production_sources();
    let infallible_orderer = replace_once(
        &parameters,
        ") -> Result<Vec<u32>, LowerError> {",
        ") -> Vec<u32> {",
    );
    assert_violation(
        &projection,
        &lower,
        &infallible_orderer,
        "ordered_dependents-return-not-result",
    );

    let orderer_fallback = replace_once(
        &parameters,
        ") -> Result<Vec<u32>, LowerError> {",
        ") -> Result<Vec<u32>, LowerError> {\n    let _recovered = Option::<usize>::None.unwrap_or_default();",
    );
    assert_violation(
        &projection,
        &lower,
        &orderer_fallback,
        "forbidden-recovery-call:ordered_dependents:unwrap_or_default",
    );
}

fn production_sources() -> (String, String, String) {
    let root = workspace_root();
    (
        fs::read_to_string(root.join(PROJECTION_PATH)).expect("read initialization projection"),
        fs::read_to_string(root.join(LOWER_PATH)).expect("read solve lowering"),
        fs::read_to_string(root.join(PARAMETERS_PATH)).expect("read parameter ownership"),
    )
}

fn boundary_violations(projection: &str, lower: &str, parameters: &str) -> BTreeSet<String> {
    let projection = syn::parse_file(projection).expect("parse initialization projection");
    let lower = syn::parse_file(lower).expect("parse solve lowering");
    let parameters = syn::parse_file(parameters).expect("parse parameter ownership");
    let mut violations = BTreeSet::new();

    for (syntax, name) in [
        (&projection, PLANNER),
        (&projection, MATCHER),
        (&parameters, ORDERER),
        (&parameters, OWNERSHIP),
    ] {
        let Some(function) = item_function(syntax, name) else {
            violations.insert(format!("{name}-missing"));
            continue;
        };
        if !returns_result(function) {
            violations.insert(format!("{name}-return-not-result"));
        }
        let mut recovery = RecoveryCallVisitor {
            calls: BTreeSet::new(),
        };
        recovery.visit_block(&function.block);
        violations.extend(
            recovery
                .calls
                .into_iter()
                .map(|call| format!("forbidden-recovery-call:{name}:{call}")),
        );
    }

    let mut calls = PlannerCallVisitor { total: 0, tried: 0 };
    calls.visit_file(&lower);
    if calls.total != 1 {
        violations.insert(format!("planner-call-count:{}", calls.total));
    }
    if calls.tried != 1 {
        violations.insert(format!("planner-try-call-count:{}", calls.tried));
    }

    violations
}

fn item_function<'syntax>(syntax: &'syntax syn::File, name: &str) -> Option<&'syntax syn::ItemFn> {
    syntax.items.iter().find_map(|item| match item {
        syn::Item::Fn(function) if function.sig.ident == name => Some(function),
        _ => None,
    })
}

fn returns_result(function: &syn::ItemFn) -> bool {
    let syn::ReturnType::Type(_, output) = &function.sig.output else {
        return false;
    };
    let syn::Type::Path(output) = output.as_ref() else {
        return false;
    };
    let Some(segment) = output.path.segments.last() else {
        return false;
    };
    if segment.ident != "Result" {
        return false;
    }
    let syn::PathArguments::AngleBracketed(arguments) = &segment.arguments else {
        return false;
    };
    arguments
        .args
        .iter()
        .filter(|argument| matches!(argument, syn::GenericArgument::Type(_)))
        .count()
        == 2
}

struct PlannerCallVisitor {
    total: usize,
    tried: usize,
}

impl<'syntax> Visit<'syntax> for PlannerCallVisitor {
    fn visit_expr_call(&mut self, call: &'syntax syn::ExprCall) {
        if is_planner_call(call) {
            self.total += 1;
        }
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_try(&mut self, expression: &'syntax syn::ExprTry) {
        if let syn::Expr::Call(call) = peel_expression(&expression.expr)
            && is_planner_call(call)
        {
            self.tried += 1;
        }
        visit::visit_expr_try(self, expression);
    }
}

fn is_planner_call(call: &syn::ExprCall) -> bool {
    let syn::Expr::Path(path) = peel_expression(&call.func) else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == PLANNER)
}

fn peel_expression(expression: &syn::Expr) -> &syn::Expr {
    match expression {
        syn::Expr::Group(group) => peel_expression(&group.expr),
        syn::Expr::Paren(paren) => peel_expression(&paren.expr),
        expression => expression,
    }
}

struct RecoveryCallVisitor {
    calls: BTreeSet<String>,
}

impl<'syntax> Visit<'syntax> for RecoveryCallVisitor {
    fn visit_expr_method_call(&mut self, call: &'syntax syn::ExprMethodCall) {
        self.record(&call.method.to_string());
        visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_call(&mut self, call: &'syntax syn::ExprCall) {
        if let syn::Expr::Path(path) = peel_expression(&call.func)
            && let Some(segment) = path.path.segments.last()
        {
            self.record(&segment.ident.to_string());
        }
        visit::visit_expr_call(self, call);
    }
}

impl RecoveryCallVisitor {
    fn record(&mut self, name: &str) {
        if is_recovery_call(name) {
            self.calls.insert(name.to_owned());
        }
    }
}

fn is_recovery_call(name: &str) -> bool {
    name.starts_with("unwrap")
        || name.starts_with("expect")
        || name.contains("fallback")
        || matches!(
            name,
            "default"
                | "or"
                | "or_else"
                | "map_or"
                | "map_or_else"
                | "get_or_insert"
                | "get_or_insert_with"
                | "get_or_insert_default"
                | "ok"
        )
}

fn replace_once(source: &str, old: &str, new: &str) -> String {
    assert_eq!(
        source.match_indices(old).count(),
        1,
        "mutation anchor must occur exactly once: {old}"
    );
    source.replacen(old, new, 1)
}

fn assert_violation(projection: &str, lower: &str, parameters: &str, expected: &str) {
    let violations = boundary_violations(projection, lower, parameters);
    assert!(
        violations.contains(expected),
        "mutation must produce {expected:?}; found {violations:#?}"
    );
}
