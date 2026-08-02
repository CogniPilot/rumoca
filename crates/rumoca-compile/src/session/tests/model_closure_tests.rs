use super::*;

fn write_source_root_file(path: &std::path::Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("mkdirs");
    }
    std::fs::write(path, contents).expect("write source-root file");
}

fn write_dependency_source_root(root: &std::path::Path) {
    write_source_root_file(&root.join("package.mo"), "package Lib\nend Lib;\n");
    write_source_root_file(
        &root.join("Base.mo"),
        "within Lib;\nmodel Base\n  Real x;\nend Base;\n",
    );
    write_source_root_file(
        &root.join("Derived.mo"),
        "within Lib;\nmodel Derived\n  Base base;\nend Derived;\n",
    );
}

fn index_dependency_source_root_with_cache(
    session: &mut Session,
    cache_dir: &std::path::Path,
    source_root_dir: &std::path::Path,
) -> SourceRootLoadReport {
    session.load_source_root_tolerant_with_cache_dir_for_tests(
        "external::Lib",
        SourceRootKind::External,
        source_root_dir,
        None,
        Some(cache_dir),
    )
}

fn strict_model_closure(session: &mut Session, model_name: &str) -> ReachableModelClosure {
    let (plan, _) = session
        .build_resolution_plan_for_strict_compile()
        .expect("strict compile tree should build");
    session.reachable_model_closure_query(
        plan.tree(),
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

#[test]
fn warm_source_root_restore_keeps_reachable_model_closure_warm_for_unchanged_targets() {
    let temp = tempfile::tempdir().expect("tempdir");
    let source_root_dir = temp.path().join("Lib");
    let cache_dir = temp.path().join("cache");
    write_dependency_source_root(&source_root_dir);

    let mut first = Session::default();
    let first_report =
        index_dependency_source_root_with_cache(&mut first, &cache_dir, &source_root_dir);
    assert_eq!(
        first_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Miss),
        "initial load should populate the source-root cache",
    );

    let mut second = Session::default();
    let second_report =
        index_dependency_source_root_with_cache(&mut second, &cache_dir, &source_root_dir);
    assert_eq!(
        second_report.cache_status,
        Some(crate::source_roots::SourceRootCacheStatus::Hit),
        "warm reopen should reuse the outer parsed source-root snapshot",
    );

    let (plan, _) = second
        .build_resolution_plan_for_strict_compile()
        .expect("strict compile tree should build after warm reopen");
    second
        .query_state
        .resolved
        .dependency_fingerprints
        .strict_compile_recovery
        .as_mut()
        .expect("warm reopen should hydrate strict dependency inputs")
        .replace_class_dependencies_for_test(
            "Lib.Derived",
            vec!["Lib.Base".to_string(), "Lib.Extra".to_string()],
        );

    let first_closure = second.reachable_model_closure_query(
        plan.tree(),
        ResolveBuildMode::StrictCompileRecovery,
        "Lib.Derived",
    );
    assert_eq!(
        first_closure.reachable_classes,
        vec![
            "Lib.Derived".to_string(),
            "Lib.Base".to_string(),
            "Lib.Extra".to_string(),
        ],
        "warm reopen should build the reachable-model closure from the hydrated dependency graph inputs"
    );
    assert_eq!(
        first_closure.compile_targets,
        vec!["Lib.Derived".to_string()],
        "warm reopen should keep strict compile targets scoped to the requested model"
    );

    let cache_key = strict_model_closure_cache_key(&mut second, "Lib.Derived");
    second
        .query_state
        .resolved
        .reachable_model_closures
        .get_mut(&cache_key)
        .expect("closure query should be cached after the first warm-reopen query")
        .closure = ReachableModelClosure {
        reachable_classes: vec!["SENTINEL".to_string()],
        compile_targets: vec!["Lib.Derived".to_string()],
    };

    let warm = second.reachable_model_closure_query(
        plan.tree(),
        ResolveBuildMode::StrictCompileRecovery,
        "Lib.Derived",
    );
    assert_eq!(
        warm.reachable_classes,
        vec!["SENTINEL".to_string()],
        "unchanged targets should keep the reachable-model closure warm after warm reopen"
    );
}

#[test]
fn reachable_model_closure_follows_redeclared_package_members() {
    // MLS §7.3: a lookup written against a replaceable package slot cannot be
    // bound by Resolve, because the class occupying the slot is chosen by a
    // redeclaration. Strict pruning must still keep every member such a lookup
    // can select, otherwise the pruned tree loses the nested function and
    // record the instantiated model needs.
    let mut session = Session::default();
    session
        .add_document(
            "redeclared_package.mo",
            r#"
package P
  partial package PartialRotation
    replaceable record Orientation
      Real interfaceMarker[0];
    end Orientation;

    replaceable function first
      input Orientation element;
      output Real y;
    end first;
  end PartialRotation;

  package Quaternion
    extends PartialRotation;

    redeclare record extends Orientation
      Real q[4];
    end Orientation;

    redeclare function first
      input Orientation element;
      output Real y;
    algorithm
      y := element.q[1];
    end first;
  end Quaternion;

  package Generic
    replaceable package Rotation = Quaternion
      constrainedby PartialRotation;

    record Element
      Real position[3];
      Rotation.Orientation rotation;
    end Element;

    function product
      input Element left;
      output Real y;
    algorithm
      y := Rotation.first(left.rotation);
    end product;
  end Generic;

  package Concrete
    extends Generic(redeclare package Rotation = Quaternion);
  end Concrete;

  model Probe
    parameter Concrete.Element left;
    Real x(start = 0.0, fixed = true);
  equation
    der(x) = Concrete.product(left);
  end Probe;
end P;
"#,
        )
        .expect("redeclared package fixture should parse");

    let closure = strict_model_closure(&mut session, "P.Probe");

    for expected in [
        "P.Quaternion.first",
        "P.PartialRotation.first",
        "P.Quaternion.Orientation",
        "P.PartialRotation.Orientation",
    ] {
        assert!(
            closure
                .reachable_classes
                .iter()
                .any(|name| name == expected),
            "strict closure must retain `{expected}` reached through the redeclared package: {:?}",
            closure.reachable_classes
        );
    }
}

#[test]
fn strict_model_closure_retains_component_redeclare_function_value() {
    // MLS §7.2: the function substituted by a component modifier is looked
    // up where the modifier occurs. Its exact source-ordered identity must keep
    // the function definition in the pruned strict-target Resolve proof.
    let mut session = Session::default();
    session
        .add_document(
            "redeclared_function.mo",
            r#"
package Shapes
  partial function Characteristic
    input Real length = 1;
    output Real x;
  end Characteristic;

  function defaultCharacteristic
    extends Characteristic;
  algorithm
    x := 0;
  end defaultCharacteristic;

  function rectangle
    extends Characteristic;
  algorithm
    x := length;
  end rectangle;

  partial model PartialSurface
    replaceable function surfaceCharacteristic = defaultCharacteristic
      constrainedby Characteristic;
  end PartialSurface;

  model Surface
    extends PartialSurface;
  end Surface;

  model Probe
    Surface surface(
      redeclare function surfaceCharacteristic = rectangle(length = 2));
  end Probe;
end Shapes;
"#,
        )
        .expect("redeclared function fixture should parse");

    let closure = strict_model_closure(&mut session, "Shapes.Probe");
    assert!(
        closure
            .reachable_classes
            .iter()
            .any(|name| name == "Shapes.rectangle"),
        "strict closure must retain the exact redeclared function: {:?}",
        closure.reachable_classes
    );
    assert!(
        closure
            .reachable_classes
            .iter()
            .any(|name| name == "Shapes.PartialSurface.surfaceCharacteristic"),
        "strict closure must retain the inherited redeclare slot: {:?}",
        closure.reachable_classes
    );
    session
        .resolve_strict_target("Shapes.Probe")
        .unwrap_or_else(|_| panic!("pruned strict Resolve must retain the redeclared function"));
}
