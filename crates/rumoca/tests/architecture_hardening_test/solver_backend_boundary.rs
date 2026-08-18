//! Concrete numerical-plugin dependency boundary (SPEC_0041 §4).

use super::*;

/// Concrete solver backends consume only `rumoca-solver`'s opaque FMI ME
/// importer/host contract. Target encoders are banned from every dependency
/// table. Solve IR and its evaluator are permitted only in dev-dependencies
/// for construction fixtures.
#[test]
fn concrete_solver_backends_consume_the_me_contract_only() {
    let root = workspace_root();
    let offenders = ["rumoca-solver-diffsol", "rumoca-solver-rk45"]
        .iter()
        .flat_map(|crate_name| solver_backend_boundary_offenders(&root, crate_name))
        .collect::<Vec<_>>();

    assert!(
        offenders.is_empty(),
        "concrete solver backends consume only rumoca-solver's opaque FMI ME importer/host \
contract in production; phase, compiler IR/evaluator, facade, and rumoca-exec-* dependencies \
are forbidden outside the explicit Solve dev-fixture exception: {offenders:?}"
    );
}

fn solver_backend_boundary_offenders(root: &Path, crate_name: &str) -> Vec<String> {
    let cargo_toml = root.join(format!("crates/{crate_name}/Cargo.toml"));
    let content = fs::read_to_string(&cargo_toml).expect("read solver backend Cargo.toml");
    all_manifest_dependency_names(&content)
        .into_iter()
        .filter(|(section, dependency)| solver_backend_dep_is_banned(section, dependency))
        .map(|(section, dependency)| format!("{crate_name} {section} {dependency}"))
        .collect()
}

pub(super) fn solver_backend_dep_is_banned(section: &str, dependency: &str) -> bool {
    const BANNED_EXACT: &[&str] = &[
        "rumoca-compile",
        "rumoca-ir-ast",
        "rumoca-ir-flat",
        "rumoca-ir-dae",
        "rumoca-eval-ast",
        "rumoca-eval-flat",
        "rumoca-eval-dae",
        "rumoca-sim",
    ];

    let test_fixture_dependency = section.ends_with("dev-dependencies")
        && matches!(dependency, "rumoca-ir-solve" | "rumoca-eval-solve");
    (!test_fixture_dependency && matches!(dependency, "rumoca-ir-solve" | "rumoca-eval-solve"))
        || dependency.starts_with("rumoca-phase-")
        || dependency.starts_with("rumoca-exec-")
        || dependency == "rumoca-phase-codegen"
        || BANNED_EXACT.contains(&dependency)
}
