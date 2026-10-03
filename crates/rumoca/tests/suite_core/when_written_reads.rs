//! Reads of `when`-written values after the `when` statement (MLS §11.1.2,
//! §11.2.7).
//!
//! An algorithm section runs its statements in order. A value written inside
//! a `when` statement and read by a later statement of the same section is,
//! where it is read, the value the algorithm leaves behind whenever no later
//! statement writes it again: the new value at an event that activates the
//! branch, its `pre` value otherwise. `Modelica.Electrical.Digital` delay
//! blocks read their delayed output this way after the scheduling `when`.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const READ_AFTER_WHEN: &str = r#"
model ReadAfterWhen
  Integer x = if time >= 0.3 then 1 else 0;
  Integer yAux(start = -1, fixed = true);
  Integer y;
  discrete Real tNext(start = 0, fixed = true);
algorithm
  when change(x) then
    tNext := time + 0.2;
  elsewhen time >= tNext then
    yAux := x;
  end when;
  y := yAux + 10;
end ReadAfterWhen;
"#;

fn column<'r>(result: &'r rumoca_sim::SimResult, name: &str) -> &'r [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("simulation exposes {name}"));
    &result.data[index]
}

#[test]
fn a_later_statement_reads_the_value_the_when_leaves() {
    let compiled = Compiler::new()
        .model("ReadAfterWhen")
        .compile_str(READ_AFTER_WHEN, "ReadAfterWhen.mo")
        .unwrap_or_else(|error| panic!("ReadAfterWhen compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("ReadAfterWhen simulates");
    let y = column(&result, "y");
    let y_aux = column(&result, "yAux");
    for ((time, y), y_aux) in result.times.iter().zip(y).zip(y_aux) {
        assert_eq!(*y, y_aux + 10.0, "y tracks yAux at t = {time}");
        let expected = if *time > 0.5 + 1.0e-9 { 1.0 } else { -1.0 };
        if (*time - 0.5).abs() > 1.0e-6 {
            assert_eq!(*y_aux, expected, "yAux at t = {time}");
        }
    }
}
