use super::*;

#[test]
fn test_flat_output_preserves_symbolic_modifier_binding_for_subcomponent_parameter() {
    let source = r#"
model Parent
  parameter Real k = 2;

  block GainBlock
    parameter Real g = 1;
    input Real u;
    output Real y;
  equation
    y = g * u;
  end GainBlock;

  GainBlock gain(g = k);
  Real u = 1;
equation
  gain.u = u;
end Parent;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("Parent")
        .expect("compile should succeed");

    let flat_code =
        render_flat_template_with_name(&result.flat, templates::FLAT_MODELICA, "Parent")
            .expect("flat rendering should succeed");

    assert!(
        flat_code.contains("parameter Real gain.g") && flat_code.contains("= k;"),
        "expected symbolic subcomponent binding `gain.g = k` in flat output, got:\n{flat_code}"
    );
    assert!(
        !flat_code.contains("parameter Real gain.g(start = 2) = 2;")
            && !flat_code.contains("parameter Real gain.g = 2;"),
        "subcomponent modifier binding should not collapse to default literal; got:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_preserves_boolean_and_enum_type_identity() {
    let source = r#"
model TypeIdentity
  type InitType = enumeration(NoInit, SteadyState);

  parameter Boolean use_reset = true;
  Boolean local_reset = false;
  parameter InitType init_type = InitType.NoInit;
equation
  local_reset = use_reset;
end TypeIdentity;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("TypeIdentity")
        .expect("compile should succeed");

    let flat_code =
        render_flat_template_with_name(&result.flat, templates::FLAT_MODELICA, "TypeIdentity")
            .expect("flat rendering should succeed");

    assert!(
        flat_code.contains("parameter Boolean use_reset"),
        "expected Boolean parameter type in flat output, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains("Boolean local_reset"),
        "expected Boolean variable type in flat output, got:\n{flat_code}"
    );

    let init_type_line = flat_code
        .lines()
        .find(|line| line.contains("init_type"))
        .unwrap_or("");
    assert!(
        !init_type_line.contains("parameter Real"),
        "enum parameter type should not collapse to Real, got line: {init_type_line}\nfull output:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_qualifies_enum_literals_in_bindings_and_equations() {
    let source = r#"
package TypesPkg
  type Init = enumeration(NoInit, SteadyState);
end TypesPkg;

model EnumRefQualification
  import Init = TypesPkg.Init;

  parameter Init init_type = Init.NoInit;
  Boolean is_no_init;
equation
  is_no_init = init_type == Init.NoInit;
end EnumRefQualification;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("EnumRefQualification")
        .expect("compile should succeed");

    let flat_code = render_flat_template_with_name(
        &result.flat,
        templates::FLAT_MODELICA,
        "EnumRefQualification",
    )
    .expect("flat rendering should succeed");

    assert!(
        flat_code.contains("TypesPkg.Init.NoInit"),
        "expected fully qualified enum literal in flat output, got:\n{flat_code}"
    );
    assert!(
        !flat_code.contains(" = Init.NoInit")
            && !flat_code.contains("== Init.NoInit")
            && !flat_code.contains("== (Init.NoInit)"),
        "flat output should not contain unresolved short enum literal `Init.NoInit`, got:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_preserves_final_and_protected_qualifiers() {
    let source = r#"
model QualifierPreservation
  final parameter Real k = 2;
protected
  Real secret = k;
  final parameter Real kp = 3;
public
  Real y;
equation
  y = secret + kp;
end QualifierPreservation;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("QualifierPreservation")
        .expect("compile should succeed");

    let flat_code = render_flat_template_with_name(
        &result.flat,
        templates::FLAT_MODELICA,
        "QualifierPreservation",
    )
    .expect("flat rendering should succeed");
    let flat_code_normalized = flat_code.replace('\r', "");

    assert!(
        flat_code.contains("final parameter Real k"),
        "expected `final` qualifier on public parameter declaration, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains("final parameter Real kp"),
        "expected `final` qualifier on protected parameter declaration, got:\n{flat_code}"
    );
    assert!(
        flat_code_normalized.contains("\nprotected\n"),
        "expected `protected` section marker in flat output, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains("Real secret"),
        "expected protected declaration for `secret`, got:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_emits_assert_equations() {
    let source = r#"
model AssertEmission
  parameter Real k = 1;
  Real x;
equation
  assert(k > 0, "k must stay positive");
  x = k;
initial equation
  assert(k >= 0, "k must stay nonnegative");
end AssertEmission;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("AssertEmission")
        .expect("compile should succeed");

    let flat_code =
        render_flat_template_with_name(&result.flat, templates::FLAT_MODELICA, "AssertEmission")
            .expect("flat rendering should succeed");

    assert!(
        flat_code.contains(r#"assert((k > 0), "k must stay positive");"#),
        "expected equation-section assert in flat output, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains(r#"assert((k >= 0), "k must stay nonnegative");"#),
        "expected initial-equation assert in flat output, got:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_preserves_unit_quantity_and_display_unit() {
    let source = r#"
model UnitQuantityDisplayUnit
  Real T(quantity = "ThermodynamicTemperature", unit = "K", displayUnit = "degC", nominal = 300);
equation
  T = 300;
end UnitQuantityDisplayUnit;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("UnitQuantityDisplayUnit")
        .expect("compile should succeed");

    let flat_code = render_flat_template_with_name(
        &result.flat,
        templates::FLAT_MODELICA,
        "UnitQuantityDisplayUnit",
    )
    .expect("flat rendering should succeed");

    assert!(
        flat_code.contains(r#"quantity = "ThermodynamicTemperature""#),
        "expected quantity attribute in flat output, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains(r#"unit = "K""#),
        "expected unit attribute in flat output, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains(r#"displayUnit = "degC""#),
        "expected displayUnit attribute in flat output, got:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_preserves_class_and_component_descriptions() {
    let source = r#"
model DescriptionPreservation "Top-level model description"
  Real x "State variable description";
equation
  x = 1;
end DescriptionPreservation;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("DescriptionPreservation")
        .expect("compile should succeed");

    let flat_code = render_flat_template_with_name(
        &result.flat,
        templates::FLAT_MODELICA,
        "DescriptionPreservation",
    )
    .expect("flat rendering should succeed");

    assert!(
        flat_code.contains(r#"class DescriptionPreservation "Top-level model description""#),
        "expected class description in flat output, got:\n{flat_code}"
    );
    assert!(
        flat_code.contains(r#"x"#) && flat_code.contains(r#""State variable description""#),
        "expected variable description in flat output, got:\n{flat_code}"
    );
}

#[test]
fn test_flat_output_does_not_promote_mod_binding_to_self_referential_start() {
    let source = r#"
block GainBlock
  parameter Real k(start = 1);
  parameter Real Ni = 2;
  input Real u;
  output Real y;
equation
  y = k * u;
end GainBlock;

model StartPromotion
  parameter Real k = 3;
  parameter Real Ni = 4;
  GainBlock gainTrack(k = 1 / (k * Ni));
  Real u = 1;
equation
  gainTrack.u = u;
end StartPromotion;
"#;

    let mut session = Session::new(SessionConfig::default());
    session
        .add_document("test.mo", source)
        .expect("parse/resolve/typecheck failed");

    let result = session
        .compile_model("StartPromotion")
        .expect("compile should succeed");

    let flat_code =
        render_flat_template_with_name(&result.flat, templates::FLAT_MODELICA, "StartPromotion")
            .expect("flat rendering should succeed");

    assert!(
        flat_code.contains("parameter Real gainTrack.k(start = 1)")
            && flat_code.contains("= (1 / (k * Ni));"),
        "expected gainTrack.k binding to use outer parameters with declared start preserved, got:\n{flat_code}"
    );
    assert!(
        !flat_code.contains("start = (1 / (gainTrack.k * gainTrack.Ni))"),
        "flat output must not rewrite modified bindings into self-referential start expressions, got:\n{flat_code}"
    );
}
