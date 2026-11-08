use super::*;

const DEP: &str = "within;\npackage Dep\n  connector Port\n    Real phi;\n    flow Real tau;\n  end Port;\n  function twice\n    input Real x;\n    output Real y;\n  algorithm\n    y := 2*x;\n  end twice;\nend Dep;\n";
const USER: &str = "within;\npackage UserLib\n  model M\n    Dep.Port port;\n    Real y;\n  equation\n    port.phi = time;\n    y = Dep.twice(port.phi);\n  end M;\nend UserLib;\n";

fn write_entry_roots(root: &std::path::Path) -> [std::path::PathBuf; 2] {
    let paths = [root.join("Dep"), root.join("UserLib")];
    for (path, source) in paths.iter().zip([DEP, USER]) {
        std::fs::create_dir_all(path).expect("create source root");
        std::fs::write(path.join("package.mo"), source).expect("write source root");
    }
    paths
}

fn load_entry_root(session: &mut Session, root: &std::path::Path, cache: &std::path::Path) {
    let report = session.load_source_root_tolerant_with_cache_dir_for_tests(
        &root.to_string_lossy(),
        SourceRootKind::External,
        root,
        None,
        Some(cache),
    );
    assert!(report.diagnostics.is_empty(), "{:?}", report.diagnostics);
}

fn assert_foreign_entry_compiles(session: &mut Session) {
    let (plan, _) = session
        .build_resolution_plan_for_strict_compile()
        .expect("build target plan");
    assert!(plan.completed().is_some(), "combined roots must resolve");
    let closure = session.reachable_model_closure_query(
        plan.tree(),
        ResolveBuildMode::StrictCompileRecovery,
        "UserLib.M",
    );
    for required in ["Dep.Port", "Dep.twice"] {
        assert!(
            closure
                .reachable_classes
                .iter()
                .any(|name| name == required),
            "loaded primary must retain {required}: {:?}",
            closure.reachable_classes
        );
    }
    let result = session
        .compile_model_dae_strict_reachable_uncached_with_recovery("UserLib.M")
        .expect("primary source-root model resolves foreign port and function");
    assert_eq!(
        result.dae.inspect(|view| view.continuous_equation_count()),
        3
    );
}

#[test]
fn source_root_primary_retains_foreign_dependencies_cold_and_warm() {
    let temp = tempfile::tempdir().expect("tempdir");
    let roots = write_entry_roots(temp.path());
    let cache = temp.path().join("cache");
    for reverse in [false, true] {
        let order = if reverse { [1, 0] } else { [0, 1] };
        for _ in 0..2 {
            let mut session = Session::default();
            for index in order {
                load_entry_root(&mut session, &roots[index], &cache);
            }
            assert_foreign_entry_compiles(&mut session);
            for root in &roots {
                let uri = root.join("package.mo").to_string_lossy().to_string();
                assert!(session.is_source_root_backed_document(&uri));
                assert!(
                    session
                        .get_document(&uri)
                        .expect("root document")
                        .content
                        .is_empty()
                );
            }
        }
    }
}

#[test]
fn source_root_primary_workspace_entry_keeps_foreign_resolution() {
    let temp = tempfile::tempdir().expect("tempdir");
    let roots = write_entry_roots(temp.path());
    let mut session = Session::default();
    load_entry_root(&mut session, &roots[0], &temp.path().join("cache"));
    session
        .add_document(&roots[1].join("package.mo").to_string_lossy(), USER)
        .expect("parse primary workspace document");
    assert_foreign_entry_compiles(&mut session);
}

#[test]
fn source_root_primary_missing_foreign_provider_still_refuses() {
    let temp = tempfile::tempdir().expect("tempdir");
    let roots = write_entry_roots(temp.path());
    let mut session = Session::default();
    load_entry_root(&mut session, &roots[1], &temp.path().join("cache"));
    assert!(
        session
            .compile_model_dae_strict_reachable_uncached_with_recovery("UserLib.M")
            .is_err(),
        "an absent foreign provider must not be synthesized or ignored"
    );
}
