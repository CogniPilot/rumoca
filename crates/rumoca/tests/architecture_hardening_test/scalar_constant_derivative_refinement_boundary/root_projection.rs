//! Syntax evidence for the unproved owner/metadata forwarding seam.
//! This does not prove those producers or the value-projection semantics.

use super::*;

const FACTS: &str = "SolveFacts";
const VALUES: &str = "project_solve_values_with_owner_premises";

pub(super) fn check(
    sources: &[(std::path::PathBuf, syn::File)],
    violations: &mut BTreeSet<String>,
) {
    let Some((_, checker)) = sources
        .iter()
        .find(|(path, _)| path.ends_with(Path::new(CHECKER_PATH)))
    else {
        violations.insert("root-projection-source-missing".to_owned());
        return;
    };
    check_forwarding(checker, violations);
    let aliases = resolved_type_aliases(sources, &[FACTS], violations);
    let mut literals = 0;
    for (_, file) in sources {
        let mut census = LiteralCensus::new([FACTS], &aliases);
        census.visit_file(file);
        literals += census.counts[FACTS];
        literals += self_literal_count_in_impls(file, FACTS, &aliases);
    }
    if literals != 1
        || !find_function(checker, VALUES).is_some_and(|function| {
            matches!(function.vis, syn::Visibility::Inherited)
                && explicit_struct_literal_count(&function.block, FACTS) == 1
        })
    {
        violations.insert("solve-facts-construction-census".to_owned());
    }
}

fn check_forwarding(file: &syn::File, violations: &mut BTreeSet<String>) {
    let Some(function) = find_function(file, "project_solve_facts") else {
        violations.insert("root-projection-entry-missing".to_owned());
        return;
    };
    if !matches!(function.vis, syn::Visibility::Inherited)
        || !takes_named_references(&function.sig.inputs, &[("model", "SolveModel")])
        || !return_contains(&function.sig.output, FACTS)
        || function.block.stmts.len() != 1
        || !tail_expression(&function.block).is_some_and(direct_same_root_call)
    {
        violations.insert("root-projection-premises-not-same-root".to_owned());
    }
}

fn direct_same_root_call(expression: &Expr) -> bool {
    let Expr::Call(call) = expression else {
        return false;
    };
    let Expr::Path(path) = call.func.as_ref() else {
        return false;
    };
    path.path.is_ident(VALUES)
        && call.args.len() == 3
        && matches!(&call.args[0], Expr::Path(path) if path.path.is_ident("model"))
        && direct_premise(&call.args[1], "project_solve_owner_census")
        && direct_premise(&call.args[2], "project_solve_metadata")
}

fn direct_premise(expression: &Expr, producer: &str) -> bool {
    let Expr::Call(call) = expression else {
        return false;
    };
    matches!(call.func.as_ref(), Expr::Path(path) if path.path.is_ident(producer))
        && call.args.len() == 1
        && matches!(&call.args[0], Expr::Path(path) if path.path.is_ident("model"))
}

#[test]
fn forwarding_rejects_foreign_substituted_and_deferred_premises() {
    let sources = Sources::read();
    assert!(
        boundary_violations(&sources).is_empty(),
        "baseline must pass"
    );
    for (old, new) in [
        (
            "project_solve_owner_census(model),",
            "project_solve_owner_census(other_model),",
        ),
        (
            "project_solve_metadata(model),",
            "project_solve_metadata(other_model),",
        ),
        (
            "project_solve_owner_census(model),",
            "[0; SOLVE_OWNER_WIDTH],",
        ),
        ("project_solve_metadata(model),", "cached_metadata,"),
        (
            "project_solve_values_with_owner_premises(\n        model,",
            "project_solve_values_with_owner_premises(\n        other_model,",
        ),
        (
            "fn project_solve_facts(model: &solve::SolveModel) -> SolveFacts {",
            "fn project_solve_facts(model: &solve::SolveModel) -> SolveFacts {\nlet model = other_model;",
        ),
        (
            "project_solve_owner_census(model),",
            "(|| project_solve_owner_census(model))(),",
        ),
    ] {
        let mut changed = sources.clone();
        changed.checker = replace_once(&changed.checker, old, new);
        assert_violation(&changed, "root-projection-premises-not-same-root");
    }
}

#[test]
fn fact_construction_rejects_second_literal_and_alias_mints() {
    let sources = Sources::read();
    assert!(
        boundary_violations(&sources).is_empty(),
        "baseline must pass"
    );
    for extra in [
        "fn second(original: SolveFacts) -> SolveFacts { SolveFacts { ..original } }",
        "type OtherFacts = SolveFacts; fn second(original: SolveFacts) -> OtherFacts { OtherFacts { ..original } }",
        "impl SolveFacts { fn second(self) -> Self { Self { ..self } } }",
    ] {
        let mut changed = sources.clone();
        changed.checker.push_str(extra);
        assert_violation(&changed, "solve-facts-construction-census");
    }
}
