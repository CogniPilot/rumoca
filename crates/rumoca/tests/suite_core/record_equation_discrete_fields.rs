//! Record equations whose record has a discrete-valued field.
//!
//! An equality of records is the set of its field equations (MLS 3.7
//! §8.3.1). The `phase` field of a `Modelica.Media.Water` state record is an
//! Integer, so its field equation is a discrete-valued assignment (Appendix
//! B) that changes only at events, while `h` and `p` stay continuous
//! residuals. Counting the Integer field as a continuous equation left every
//! such model one equation per state record over-determined.

use rumoca::Compiler;

const SOURCE: &str = r#"
record State
  Integer phase;
  Real h;
  Real p;
end State;

function setState
  input Real p;
  input Real h;
  output State s;
algorithm
  s := State(phase = if h > 2 then 2 else 1, h = h, p = p);
end setState;

model Medium
  Real x(start = 0, fixed = true);
  State a;
  State b;
  Real y;
equation
  der(x) = 1;
  a = setState(1e5 + x, if x > 0.5 then 3 else 1);
  b = a;
  y = a.h + (if b.phase == 2 then 10 else 0);
end Medium;
"#;

#[test]
fn discrete_record_fields_are_assigned_at_events_and_continuous_fields_stay_residuals() {
    let compiled = Compiler::new()
        .model("Medium")
        .compile_str(SOURCE, "RecordEquationDiscreteFields.mo")
        .unwrap_or_else(|error| panic!("Medium compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 1.0,
            ..Default::default()
        },
    )
    .expect("Medium simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, _) in result.times.iter().enumerate() {
        let x = column("x")[index];
        if (x - 0.5).abs() < 1e-6 {
            continue;
        }
        let (phase, y) = if x > 0.5 { (2.0, 13.0) } else { (1.0, 1.0) };
        assert_eq!(column("a.phase")[index], phase, "a.phase at x = {x}");
        assert_eq!(column("b.phase")[index], phase, "b.phase at x = {x}");
        assert!((column("y")[index] - y).abs() < 1e-12, "y at x = {x}");
        assert!(
            (column("a.p")[index] - (1e5 + x)).abs() < 1e-6,
            "a.p at x = {x}"
        );
    }
}
