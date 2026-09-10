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
