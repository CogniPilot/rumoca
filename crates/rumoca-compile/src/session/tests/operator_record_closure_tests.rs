use super::*;

fn strict_flat_from_source(
    source_name: &str,
    source: &str,
    model: &str,
) -> Result<rumoca_ir_flat::Model, String> {
    let mut session = Session::default();
    session
        .add_document(source_name, source)
        .expect("operator-record fixture parses");
    session.compile_model_flat_strict_reachable_uncached_with_recovery(model)
}

const DIRECT_OPERATOR_CONSTRUCTOR_DEFAULT: &str = r#"
operator record C
  replaceable Real re;
  replaceable Real im;
  encapsulated operator 'constructor'
    function fromReal
      import C;
      input Real re;
      input Real im = 0;
      output C result(re = re, im = im);
    algorithm
    end fromReal;
  end 'constructor';
  encapsulated operator function '0'
    import C;
    output C result;
  algorithm
    result := C(0);
  end '0';
end C;
model UsesC
  C value;
equation
  value = C(1);
end UsesC;
"#;

const INHERITED_OPERATOR_CONSTRUCTOR_DEFAULT: &str = r#"
operator record C
  replaceable Real re;
  replaceable Real im;
  encapsulated operator 'constructor'
    function fromReal
      import C;
      input Real re;
      input Real im = 0;
      output C result(re = re, im = im);
    algorithm
    end fromReal;
  end 'constructor';
end C;
operator record D = C(redeclare Real re, redeclare Real im);
model UsesD
  D value;
equation
  value = D(1);
end UsesD;
"#;

#[test]
fn strict_closure_keeps_direct_operator_constructor_default() {
    let mut session = Session::default();
    session
        .add_document(
            "direct_operator_constructor.mo",
            DIRECT_OPERATOR_CONSTRUCTOR_DEFAULT,
        )
        .expect("direct operator-record fixture parses");
    let target = session
        .resolve_strict_target("UsesC")
        .unwrap_or_else(|_| panic!("strict closure keeps every typed operator member"));
    let index = rumoca_ir_ast::ClassDefIndex::from_tree(target.resolved.inner());
    let zero = index
        .get_by_qualified_name("C.'0'")
        .expect("the shorthand operator function remains in the strict closure");
    assert_eq!(zero.class_type, rumoca_core::ClassType::Function);
    assert!(zero.def_id.is_some());
    let flat = session
        .compile_model_flat_strict_reachable_uncached_with_recovery("UsesC")
        .expect("strict closure keeps the selected constructor declaration");
    let constructor = flat
        .functions
        .get(&rumoca_core::VarName::new("C"))
        .expect("structural C constructor is reachable");
    assert!(constructor.is_constructor);
    assert!(constructor.inputs[1].default.is_some());
}

#[test]
fn strict_closure_keeps_inherited_operator_constructor_default() {
    let flat = strict_flat_from_source(
        "inherited_operator_constructor.mo",
        INHERITED_OPERATOR_CONSTRUCTOR_DEFAULT,
        "UsesD",
    )
    .expect("strict closure reaches the base constructor through the short operator record");
    let constructor = flat
        .functions
        .get(&rumoca_core::VarName::new("D"))
        .expect("structural D constructor is reachable");
    assert!(constructor.is_constructor);
    assert!(constructor.inputs[1].default.is_some());
}

#[test]
fn strict_closure_does_not_fabricate_a_missing_constructor_default() {
    let error = strict_flat_from_source(
        "missing_operator_constructor.mo",
        r#"
operator record C
  Real re;
  Real im;
end C;
model UsesC
  C value;
equation
  value = C(1);
end UsesC;
"#,
        "UsesC",
    )
    .expect_err("an absent field default stays absent");
    assert!(error.contains("input `im` has no argument and no default"));
}

#[test]
fn strict_closure_retains_ambiguous_constructor_set_and_fails_closed() {
    let source = r#"
operator record C
  replaceable Real re;
  replaceable Real im;
  encapsulated operator 'constructor'
    function fromReal
      import C;
      input Real re;
      input Real im = 0;
      output C result(re = re, im = im);
    algorithm
    end fromReal;
    function fromInteger
      import C;
      input Integer re;
      input Real im = 1;
      output C result(re = re, im = im);
    algorithm
    end fromInteger;
  end 'constructor';
end C;
model UsesC
  C value;
equation
  value = C(1);
end UsesC;
"#;
    let mut session = Session::default();
    session
        .add_document("ambiguous_operator_constructor.mo", source)
        .expect("ambiguous fixture parses");
    let target = session
        .resolve_strict_target("UsesC")
        .unwrap_or_else(|_| panic!("strict closure re-resolves the complete overload set"));
    let index = rumoca_ir_ast::ClassDefIndex::from_tree(target.resolved.inner());
    assert!(
        index
            .get_by_qualified_name("C.'constructor'.fromReal")
            .is_some()
    );
    assert!(
        index
            .get_by_qualified_name("C.'constructor'.fromInteger")
            .is_some()
    );
    // The current typed boundary cannot yet choose between the retained
    // overloads. MLS §14.3.3 selection may later make this source legal; until
    // then it must fail closed without borrowing a default from either child.
    let error = session
        .compile_model_flat_strict_reachable_uncached_with_recovery("UsesC")
        .expect_err("two identity constructors cannot donate an arbitrary default");
    assert!(error.contains("input `im` has no argument and no default"));
}
