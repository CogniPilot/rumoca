//! MLS §10.5/§11: the slice selects RHS ordinals only on its guarded branch.
use rumoca_eval_galec::{Evaluator, IntegerDomain, Value};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

const MODEL: &str = r#"
function place
  input Real v[3];
  input Boolean reverse;
  output Real y[6];
algorithm
  y := {10.0,20.0,30.0,40.0,50.0,60.0};
  if reverse then
    y[6:-1:4] := v + {1.0,2.0,3.0};
  else
    y[4:6] := v + {1.0,2.0,3.0};
  end if;
end place;
model SliceProjection
  input Real u;
  discrete output Real up[6](each start=0.0);
  discrete output Real down[6](each start=0.0);
equation
  when sample(0.0,0.01) then
    up = place({u,2.0*u,3.0*u},false);
    down = place({u,2.0*u,3.0*u},true);
  end when;
end SliceProjection;
"#;

#[test]
fn sliced_vector_sum_preserves_ordinal_order_and_untouched_elements() {
    let compiled = rumoca::Compiler::new()
        .model("SliceProjection")
        .compile_str(MODEL, "SliceProjection.mo")
        .expect("fixed-shape source compiles");
    let package = lower_to_algorithm_code(
        &GalecInput::new(&compiled.dae, "SliceProjection"),
        &GalecOptions::default(),
    )
    .expect("slice guards prove every RHS ordinal");
    let mut evaluator = Evaluator::new(package.checked_block(), IntegerDomain::signed_32())
        .expect("checked GALEC evaluator");
    evaluator.startup().unwrap();
    evaluator.recalibrate().unwrap();
    for u in [-2.0, 0.0, 1.0, 3.0] {
        evaluator.set_state("u", Value::Real(u)).unwrap();
        evaluator.do_step().unwrap();
        assert_array(
            &evaluator,
            "up",
            [10.0, 20.0, 30.0, u + 1.0, 2.0 * u + 2.0, 3.0 * u + 3.0],
        );
        assert_array(
            &evaluator,
            "down",
            [10.0, 20.0, 30.0, 3.0 * u + 3.0, 2.0 * u + 2.0, u + 1.0],
        );
        assert!(evaluator.active_signals().is_empty());
    }
}

fn assert_array(evaluator: &Evaluator<'_>, name: &str, expected: [f64; 6]) {
    let Value::Array(array) = evaluator.state(name).unwrap() else {
        panic!("{name} is the checked array output");
    };
    assert_eq!(array.len(), expected.len());
    for (got, expected) in array.iter().zip(expected) {
        assert_eq!(got, &Value::Real(expected));
    }
}
