use super::*;

const OVERCONSTRAINED_REFERENCE: &str = r#"
package P
  record Reference
    Real gamma;
    function equalityConstraint
      input Reference left;
      input Reference right;
      output Real residue[0];
    algorithm
    end equalityConstraint;
  end Reference;

  connector Pin
    Real v;
    flow Real i;
    Reference reference;
  end Pin;

  model Source
    Pin p;
  equation
    Connections.root(p.reference);
    if Connections.isRoot(p.reference) then
      p.reference.gamma = 0;
    end if;
    p.v = 1;
  end Source;

  model Link
    Pin a;
    Pin b;
  equation
    Connections.branch(a.reference, b.reference);
    a.reference.gamma = b.reference.gamma;
    a.v = b.v;
    a.i + b.i = 0;
  end Link;

  model Probe
    Source source;
    Link link;
  equation
    connect(source.p, link.a);
  end Probe;
end P;
"#;

#[test]
fn strict_closure_keeps_implicit_equality_constraint_for_vcg_classification() {
    let mut session = Session::default();
    session
        .add_document("overconstrained_reference.mo", OVERCONSTRAINED_REFERENCE)
        .expect("overconstrained fixture parses");

    let target = session
        .resolve_strict_target("P.Probe")
        .unwrap_or_else(|failure| {
            panic!(
                "strict resolution must retain the implicit equalityConstraint owner: {:?}",
                failure.failures
            )
        });
    let index = rumoca_ir_ast::ClassDefIndex::from_tree(target.resolved.inner());
    let equality_constraint = index
        .get_by_qualified_name("P.Reference.equalityConstraint")
        .expect("the overdetermined record's implicit function remains in the strict closure");

    assert_eq!(
        equality_constraint.class_type,
        rumoca_core::ClassType::Function
    );
    assert!(equality_constraint.def_id.is_some());

    let flat = session
        .compile_model_flat_strict_reachable_uncached_with_recovery("P.Probe")
        .expect("retained equalityConstraint metadata connects the rooted VCG");
    assert!(flat.optional_edges.iter().any(|(left, right)| {
        (left == "source.p.reference" && right == "link.a.reference")
            || (left == "link.a.reference" && right == "source.p.reference")
    }));
}

#[test]
fn strict_closure_retains_non_function_equality_constraint_without_vcg_classification() {
    let mut session = Session::default();
    session
        .add_document(
            "non_function_equality_constraint.mo",
            r#"
package P
  record Reference
    Real gamma;
    model equalityConstraint
      Real x;
    end equalityConstraint;
  end Reference;

  connector Pin
    Real v;
    flow Real i;
    Reference reference;
  end Pin;

  model Probe
    Pin left;
    Pin right;
  equation
    connect(left, right);
  end Probe;
end P;
"#,
        )
        .expect("non-function equalityConstraint fixture parses");

    let target = session
        .resolve_strict_target("P.Probe")
        .unwrap_or_else(|failure| {
            panic!(
                "a same-name non-function is retained but is not an overconstrained prototype: {:?}",
                failure.failures
            )
        });
    let index = rumoca_ir_ast::ClassDefIndex::from_tree(target.resolved.inner());
    let retained = index
        .get_by_qualified_name("P.Reference.equalityConstraint")
        .expect("strict pruning conservatively retains the exact same-name child");

    assert_eq!(retained.class_type, rumoca_core::ClassType::Model);
    assert!(
        target
            .diagnostics
            .iter()
            .all(|diagnostic| diagnostic.code.as_deref() != Some("ER117")),
        "a non-function does not claim the equalityConstraint prototype"
    );

    let flat = session
        .compile_model_flat_strict_reachable_uncached_with_recovery("P.Probe")
        .expect("the inert same-name child must not create a virtual connection graph");
    assert!(flat.optional_edges.is_empty());
    assert!(
        flat.variables
            .values()
            .all(|variable| !variable.is_overconstrained && variable.oc_record_path.is_none()),
        "a non-function same-name child must never classify connector fields as overconstrained"
    );
}
