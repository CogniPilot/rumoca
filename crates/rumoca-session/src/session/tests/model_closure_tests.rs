use super::*;

fn strict_model_closure(session: &mut Session, model_name: &str) -> ReachableModelClosure {
    let (resolved, _) = session
        .build_resolved_for_strict_compile_with_diagnostics()
        .expect("strict compile tree should build");
    session.reachable_model_closure_query(
        &resolved.0,
        ResolveBuildMode::StrictCompileRecovery,
        model_name,
    )
}

fn strict_model_closure_cache_key(
    session: &mut Session,
    model_name: &str,
) -> ReachableModelClosureCacheKey {
    ReachableModelClosureCacheKey::new(
        session
            .model_key_query(model_name)
            .expect("model key should resolve"),
        ResolveBuildMode::StrictCompileRecovery,
    )
}

#[test]
fn reachable_model_closure_query_collects_dependency_closure() {
    let mut session = Session::default();
    session
        .add_document(
            "base.mo",
            "package Lib\n  model Base\n    Real x;\n  end Base;\nend Lib;\n",
        )
        .expect("base should parse");
    session
        .add_document(
            "derived.mo",
            "within Lib;\nmodel Derived\n  Base base;\nend Derived;\n",
        )
        .expect("derived should parse");
    session
        .add_document(
            "other.mo",
            "package Other\n  model Unused\n    Real z;\n  end Unused;\nend Other;\n",
        )
        .expect("other should parse");

    let closure = strict_model_closure(&mut session, "Lib.Derived");
    assert_eq!(
        closure.reachable_classes,
        vec!["Lib.Derived".to_string(), "Lib.Base".to_string()],
        "closure query should track the requested model and its reachable class dependencies"
    );
    assert_eq!(
        closure.compile_targets,
        vec!["Lib.Derived".to_string()],
        "strict closure planning should compile only the requested model root"
    );
}

#[test]
fn reachable_model_closure_query_cache_stays_warm_for_unrelated_edits_and_rebuilds_for_dependencies()
 {
    let mut session = Session::default();
    session
        .add_document(
            "base.mo",
            "package Lib\n  model Base\n    Real x;\n  end Base;\nend Lib;\n",
        )
        .expect("base should parse");
    session
        .add_document(
            "derived.mo",
            "within Lib;\nmodel Derived\n  Base base;\nend Derived;\n",
        )
        .expect("derived should parse");
    session
        .add_document("other.mo", "model Other\n  Real y;\nend Other;\n")
        .expect("other should parse");

    let initial = strict_model_closure(&mut session, "Lib.Derived");
    assert_eq!(
        initial.reachable_classes,
        vec!["Lib.Derived".to_string(), "Lib.Base".to_string()]
    );

    let cache_key = strict_model_closure_cache_key(&mut session, "Lib.Derived");
    session
        .query_state
        .resolved
        .reachable_model_closures
        .get_mut(&cache_key)
        .expect("closure query should be cached")
        .closure = ReachableModelClosure {
        reachable_classes: vec!["SENTINEL".to_string()],
        compile_targets: vec!["Lib.Derived".to_string()],
    };

    let parse_error = session.update_document(
        "other.mo",
        "model Other\n  Real y;\n  Real q;\nend Other;\n",
    );
    assert!(parse_error.is_none(), "unrelated edit should remain valid");

    let warm = strict_model_closure(&mut session, "Lib.Derived");
    assert_eq!(
        warm.reachable_classes,
        vec!["SENTINEL".to_string()],
        "unrelated edits should preserve the cached model-closure artifact"
    );

    let parse_error = session.update_document(
        "base.mo",
        "package Lib\n  model Base\n    Real x;\n    Real w;\n  end Base;\nend Lib;\n",
    );
    assert!(parse_error.is_none(), "dependency edit should remain valid");

    let rebuilt = strict_model_closure(&mut session, "Lib.Derived");
    assert_eq!(
        rebuilt.reachable_classes,
        vec!["Lib.Derived".to_string(), "Lib.Base".to_string()],
        "dependency edits should rebuild the reachable model closure"
    );
    assert_eq!(
        rebuilt.compile_targets,
        vec!["Lib.Derived".to_string()],
        "rebuild should keep strict compile targets scoped to the requested model"
    );
}
