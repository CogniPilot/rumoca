//! A branch combination that leaves an algebraic block without a unique
//! solution (EX004).
//!
//! Two closed ports each state `m = 0`, and nothing then fixes the pressures
//! of the connection between them. The matching holds over the union of the
//! branches, and the open combination is regular, so construction accepts the
//! model; the run reports the singular combination it reaches by its unknowns
//! and the branch selectors that chose it.

use rumoca::Compiler;

const SOURCE: &str = r#"
model Ports
  parameter Real opening = 0.5;
  Boolean openA = time > opening;
  Boolean openB = time > opening;
  Real m1;
  Real m2;
  Real pa;
  Real pb;
  Real x(start = 1, fixed = true);
equation
  if openA then
    pa = 1 + m1;
  else
    m1 = 0;
  end if;
  if openB then
    pb = 2 + m2;
  else
    m2 = 0;
  end if;
  m1 = m2;
  der(x) = pa - x;
  pa - pb = m1*abs(m1) + 0.1*m1;
end Ports;

model OpenPorts
  extends Ports(opening = -1);
end OpenPorts;
"#;

fn simulate(model: &str) -> Result<rumoca_sim::SimResult, String> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(SOURCE, "SingularActiveMode.mo")
        .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
    rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .map_err(|error| error.to_string())
}

#[test]
fn closed_closed_ports_report_the_singular_branch_combination() {
    let message = simulate("Ports").expect_err("both ports closed leave the pressures free");
    assert!(message.contains("[EX004]"), "{message}");
    assert!(message.contains("openA = false"), "{message}");
    assert!(message.contains("openB = false"), "{message}");
}

#[test]
fn open_ports_are_regular() {
    let result = simulate("OpenPorts").expect("both ports open determine the pressures");
    assert!(result.times.last().is_some_and(|t| (t - 1.0).abs() < 1e-9));
}
