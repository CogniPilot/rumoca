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

const RECORD_AFTER_WHEN: &str = r#"
model RecordAfterWhen
  record R
    Real a;
    Real b;
  end R;
  function make
    input Real a;
    input Real b;
    output R r;
  algorithm
    r := R(a, b);
  end make;
  discrete R r(a(start = 0, fixed = true), b(start = 0, fixed = true));
  discrete Real y(start = 0, fixed = true);
  Boolean c = time >= 0.5;
algorithm
  when c then
    r := make(1, 2);
  end when;
  when c then
    y := r.a + 10*r.b;
  end when;
end RecordAfterWhen;
"#;

/// A whole-record write in the `when` releases every field it writes, so a
/// later statement reads each field's current value, not its `pre` value.
#[test]
fn a_later_statement_reads_the_fields_a_whole_record_write_leaves() {
    let compiled = Compiler::new()
        .model("RecordAfterWhen")
        .compile_str(RECORD_AFTER_WHEN, "RecordAfterWhen.mo")
        .unwrap_or_else(|error| panic!("RecordAfterWhen compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            ..SimOptions::default()
        },
    )
    .expect("RecordAfterWhen simulates");
    let y = column(&result, "y");
    for (time, y) in result.times.iter().zip(y) {
        if (*time - 0.5).abs() > 1.0e-6 {
            let expected = if *time > 0.5 { 21.0 } else { 0.0 };
            assert_eq!(*y, expected, "y at t = {time}");
        }
    }
}

/// A field the `when` writes as part of the whole record and a later
/// statement writes again has no owner where it is read in between.
#[test]
fn a_field_written_again_after_a_whole_record_write_is_refused_where_read() {
    let source = RECORD_AFTER_WHEN.replace(
        "  when c then\n    y := r.a + 10*r.b;\n  end when;\n",
        "  when c then\n    y := r.a;\n  end when;\n  when c then\n    r.a := 3;\n  end when;\n",
    );
    let error = Compiler::new()
        .model("RecordAfterWhen")
        .compile_str(&source, "RecordAfterWhenRewrite.mo")
        .err()
        .unwrap_or_else(|| panic!("a read between two writes of r.a is refused"));
    let rendered = format!("{error:?}");
    assert!(rendered.contains("sequential read of `r.a`"), "{rendered}");
}
