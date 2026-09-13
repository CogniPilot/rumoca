//! Regression coverage for `redeclare function` selection (MLS §7.3).
//!
//! A validated redeclare of a replaceable function must retarget every call
//! to the redeclared implementation; silently resolving the declared default
//! is a wrong result, not a fallback. Each test covers one source shape the
//! compiler used to ignore: extends-modification (clocked and continuous),
//! instance-modification on a component, and an element redeclare written in
//! a derived class body.

use rumoca::Compiler;

fn simulated_final_value(source: &str, file: &str, model: &str, variable: &str) -> f64 {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(source, file)
        .expect("redeclare fixture should compile");
    let simulation = rumoca_sim::simulate_dae(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            ..Default::default()
        },
    )
    .expect("redeclare fixture should simulate");
    let index = simulation
        .names
        .iter()
        .position(|name| name == variable)
        .unwrap_or_else(|| panic!("simulation should expose {variable}"));
    *simulation.data[index]
        .last()
        .expect("simulation should contain the final value")
}

fn fixture(package: &str, consumer_equations: &str, selector: &str) -> String {
    format!(
        r#"
package {package}
  partial function PartialF
    input Real x;
    output Real y;
  end PartialF;

  function Double
    extends PartialF;
  algorithm
    y := 2*x;
  end Double;

  function Triple
    extends PartialF;
  algorithm
    y := 3*x;
  end Triple;

  block Consumer
    replaceable function F = Double constrainedby PartialF;
    Real y(start=0);
  equation
{consumer_equations}
  end Consumer;

{selector}
end {package};
"#
    )
}

const CLOCKED_CALL: &str = "    when sample(0.0, 0.1) then\n      y = F(2.0);\n    end when;";
const CONTINUOUS_CALL: &str = "    y = F(2.0);";

/// The original priority-zero repro: `extends Consumer(redeclare function
/// F = Triple)` with the call inside a clocked when-equation. The declared
/// default `Double` yields 4; the redeclared `Triple` yields 6.
#[test]
fn extends_modification_redeclare_retargets_clocked_call() {
    let source = fixture(
        "RedeclExtendsClocked",
        CLOCKED_CALL,
        "  block UsesTriple\n    extends Consumer(redeclare function F = Triple);\n  end UsesTriple;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_extends_clocked.mo",
        "RedeclExtendsClocked.UsesTriple",
        "y",
    );
    assert!(
        (y - 6.0).abs() < 1.0e-9,
        "extends-modification redeclare must select Triple (y = 6), got {y}"
    );
}

#[test]
fn extends_modification_redeclare_retargets_continuous_call() {
    let source = fixture(
        "RedeclExtendsCont",
        CONTINUOUS_CALL,
        "  block UsesTriple\n    extends Consumer(redeclare function F = Triple);\n  end UsesTriple;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_extends_cont.mo",
        "RedeclExtendsCont.UsesTriple",
        "y",
    );
    assert!(
        (y - 6.0).abs() < 1.0e-9,
        "extends-modification redeclare must select Triple (y = 6), got {y}"
    );
}

/// Instance-modification form: `Consumer c(redeclare function F = Triple)`.
#[test]
fn instance_modification_redeclare_retargets_clocked_call() {
    let source = fixture(
        "RedeclInstClocked",
        CLOCKED_CALL,
        "  block Wrapper\n    Consumer c(redeclare function F = Triple);\n    Real y;\n  equation\n    y = c.y;\n  end Wrapper;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_inst_clocked.mo",
        "RedeclInstClocked.Wrapper",
        "y",
    );
    assert!(
        (y - 6.0).abs() < 1.0e-9,
        "instance-modification redeclare must select Triple (y = 6), got {y}"
    );
}

#[test]
fn instance_modification_redeclare_retargets_continuous_call() {
    let source = fixture(
        "RedeclInstCont",
        CONTINUOUS_CALL,
        "  block Wrapper\n    Consumer c(redeclare function F = Triple);\n    Real y;\n  equation\n    y = c.y;\n  end Wrapper;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_inst_cont.mo",
        "RedeclInstCont.Wrapper",
        "y",
    );
    assert!(
        (y - 6.0).abs() < 1.0e-9,
        "instance-modification redeclare must select Triple (y = 6), got {y}"
    );
}

/// Element redeclare written in the derived class body.
#[test]
fn element_redeclare_retargets_clocked_call() {
    let source = fixture(
        "RedeclElemClocked",
        CLOCKED_CALL,
        "  block UsesTriple\n    extends Consumer;\n    redeclare function F = Triple;\n  end UsesTriple;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_elem_clocked.mo",
        "RedeclElemClocked.UsesTriple",
        "y",
    );
    assert!(
        (y - 6.0).abs() < 1.0e-9,
        "element redeclare must select Triple (y = 6), got {y}"
    );
}

fn forwarded_function_source(declaration: &str) -> String {
    let selector = format!(
        r#"
  block Wrapper
    {declaration}
    Consumer c(redeclare function F = F);
    Real y;
  equation
    y = c.y;
  end Wrapper;
"#
    );
    fixture("RedeclForwardedAlias", CONTINUOUS_CALL, &selector)
}

fn forwarded_function_result(declaration: &str) -> f64 {
    let source = forwarded_function_source(declaration);
    simulated_final_value(
        &source,
        "redecl_forwarded_alias.mo",
        "RedeclForwardedAlias.Wrapper",
        "y",
    )
}

#[test]
fn non_replaceable_function_alias_can_be_forwarded() {
    let y = forwarded_function_result("function F = Triple;");
    assert!((y - 6.0).abs() < 1.0e-9, "forwarded Triple must yield 6");
}

/// MLS §6.3/§7.3: references to replaceable declarations use their
/// constraining interfaces, including when forwarded in a redeclaration.
#[test]
fn replaceable_function_alias_can_be_forwarded() {
    let y = forwarded_function_result("replaceable function F = Triple constrainedby PartialF;");
    assert!((y - 6.0).abs() < 1.0e-9, "forwarded Triple must yield 6");
}

#[test]
fn replaceable_function_with_implicit_constraint_can_be_forwarded() {
    let y = forwarded_function_result("replaceable function F = Triple;");
    assert!((y - 6.0).abs() < 1.0e-9, "forwarded Triple must yield 6");
}

#[test]
fn non_replaceable_alias_of_replaceable_function_keeps_its_constraint() {
    let source = forwarded_function_source(
        "replaceable function Outer = Triple constrainedby PartialF;\n    function F = Outer;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_forwarded_alias_chain.mo",
        "RedeclForwardedAlias.Wrapper",
        "y",
    );
    assert!((y - 6.0).abs() < 1.0e-9, "forwarded Triple must yield 6");
}

#[test]
fn extends_redeclare_can_forward_replaceable_function() {
    let source = fixture(
        "RedeclExtendsForwarded",
        CONTINUOUS_CALL,
        "  block Wrapper\n    replaceable function Forwarded = Triple constrainedby PartialF;\n    extends Consumer(redeclare function F = Forwarded);\n  end Wrapper;",
    );
    let y = simulated_final_value(
        &source,
        "redecl_extends_forwarded.mo",
        "RedeclExtendsForwarded.Wrapper",
        "y",
    );
    assert!((y - 6.0).abs() < 1.0e-9, "forwarded Triple must yield 6");
}

#[test]
fn forwarding_alias_cannot_hide_impure_default() {
    let source =
        forwarded_function_source("replaceable impure function F = Triple constrainedby PartialF;")
            .replace("  function Triple", "  impure function Triple");
    let error = Compiler::new()
        .model("RedeclForwardedAlias.Wrapper")
        .compile_str(&source, "redecl_forwarded_impure.mo")
        .expect_err("the pure constraint cannot expose an impure default");
    assert!(
        error.to_string().contains("violates constrainedby"),
        "{error}"
    );
}

#[test]
fn forwarding_alias_keeps_bound_additional_input() {
    let source = forwarded_function_source(
        "replaceable function F = Triple(gain=3) constrainedby PartialF;",
    )
    .replace(
        "  function Triple\n    extends PartialF;",
        "  function Triple\n    extends PartialF;\n    input Real gain;",
    )
    .replace("y := 3*x;", "y := gain*x;");
    let y = simulated_final_value(
        &source,
        "redecl_forwarded_bound_input.mo",
        "RedeclForwardedAlias.Wrapper",
        "y",
    );
    assert!((y - 6.0).abs() < 1.0e-9, "forwarded gain must yield 6");
}

#[test]
fn forwarding_alias_rejects_required_additional_input() {
    let source =
        forwarded_function_source("replaceable function F = Triple constrainedby PartialF;")
            .replace(
                "  function Triple\n    extends PartialF;",
                "  function Triple\n    extends PartialF;\n    input Real gain;",
            )
            .replace("y := 3*x;", "y := gain*x;");
    let error = Compiler::new()
        .model("RedeclForwardedAlias.Wrapper")
        .compile_str(&source, "redecl_forwarded_required_input.mo")
        .expect_err("a required extra input is incompatible with PartialF");
    assert!(
        error.to_string().contains("violates constrainedby"),
        "{error}"
    );
}

#[test]
fn forwarding_checks_constraint_even_when_default_has_required_output() {
    let source = forwarded_function_source(
        "replaceable function F = Triple constrainedby PartialF;",
    )
    .replace(
        "  function Double",
        "  partial function WithOutput\n    extends PartialF;\n    output Real z;\n  end WithOutput;\n\n  function Double",
    )
    .replace("function F = Double constrainedby PartialF", "function F = Double constrainedby WithOutput")
    .replace("  function Double\n    extends PartialF;", "  function Double\n    extends WithOutput;")
    .replace("  function Triple\n    extends PartialF;", "  function Triple\n    extends WithOutput;")
    .replace("y := 2*x;", "y := 2*x;\n    z := x;")
    .replace("y := 3*x;", "y := 3*x;\n    z := x;");
    let error = Compiler::new()
        .model("RedeclForwardedAlias.Wrapper")
        .compile_str(&source, "redecl_forwarded_exposed_interface.mo")
        .expect_err("PartialF does not promise the receiver's required output z");
    assert!(
        error.to_string().contains("violates constrainedby"),
        "{error}"
    );
}

#[test]
fn forwarded_function_rejects_incompatible_default_before_lowering() {
    let source =
        forwarded_function_source("replaceable function F = Triple constrainedby PartialF;")
            .replace(
                "  function Triple\n    extends PartialF;",
                "  function Triple\n    input Boolean x;\n    output Real y;",
            )
            .replace("y := 3*x;", "y := if x then 3 else 0;");
    let error = Compiler::new()
        .model("RedeclForwardedAlias.Wrapper")
        .compile_str(&source, "redecl_forwarded_invalid_default.mo")
        .expect_err("the named constraint does not validate an incompatible default");
    assert!(
        error.to_string().contains("violates constrainedby"),
        "the default must be rejected by redeclaration validation: {error}"
    );
}
