//! Runtime controls for MLS §11.2.6 lazy early-return predicates.
//! An explicit Boolean input prevents compile-time function evaluation hiding
//! the production return-normalization path. The probe supplies its value;
//! an event-generating time relation would read uninitialized event history.

use super::compile_model;
use crate::{SimOptions, eval_dae_at};

pub(super) const FUNCTIONS: &str = r#"
function orderedReturningElseif
  input Boolean first;
  input Real samples[1];
  input Integer k;
  output Real result;
algorithm
  result := 0;
  if first then
    result := 1;
    return;
  elseif samples[k] > 0 then
    result := 2;
    return;
  end if;
  result := 3;
end orderedReturningElseif;

function inactiveLaterReturn
  input Boolean first;
  input Real samples[1];
  input Integer k;
  output Real result;
algorithm
  result := 0;
  if first then
    result := 1;
    return;
  end if;
  if samples[k] > 0 then
    result := 2;
    return;
  end if;
  result := 3;
end inactiveLaterReturn;
"#;

fn evaluate(function: &str, sample: f64, k: i64, time: f64) -> crate::EvalAtReport {
    let source = format!(
        "{FUNCTIONS}\nmodel LazyReturn\n input Boolean first;\n\
         Real x(start=0, fixed=true);\n\
         equation\n der(x) = {function}(first, {{{sample}}}, {k});\nend LazyReturn;"
    );
    let compiled = compile_model("LazyReturn", &source, "lazy-return.mo")
        .expect("runtime predicate model constructs checked DAE");
    let wire = serde_json::to_string(&compiled.dae).unwrap();
    let decoded =
        serde_json::from_str(&wire).expect("lazy predicates retain checked wire ownership");
    let options = SimOptions {
        initial_inputs: vec![("first".into(), f64::from(time >= 0.0))],
        ..SimOptions::default()
    };
    eval_dae_at(&decoded, &options, &[], time)
        .expect("the checked DAE lowers through Solve IR")
        .report
}

#[test]
fn inactive_return_predicates_do_not_read_invalid_array_elements() {
    for function in ["orderedReturningElseif", "inactiveLaterReturn"] {
        let report = evaluate(function, 5.0, 2, 0.0);
        assert!(report.error.is_none(), "{function}: {:?}", report.error);
        let derivative = report
            .derivatives
            .iter()
            .find(|slot| slot.name == "der(x)")
            .unwrap();
        assert_eq!(
            derivative.value, 1.0,
            "{function} must return from its first branch"
        );
    }
}

#[test]
fn active_return_predicates_keep_invalid_array_access_faults() {
    for function in ["orderedReturningElseif", "inactiveLaterReturn"] {
        let report = evaluate(function, 5.0, 2, -1.0);
        assert!(
            report.error.is_some(),
            "{function} must retain the selected gather fault"
        );
    }
}

#[test]
fn return_predicates_keep_the_second_branch_and_fallthrough() {
    for function in ["orderedReturningElseif", "inactiveLaterReturn"] {
        for (sample, expected) in [(5.0, 2.0), (-5.0, 3.0)] {
            let report = evaluate(function, sample, 1, -1.0);
            assert!(report.error.is_none(), "{function}: {:?}", report.error);
            let derivative = report
                .derivatives
                .iter()
                .find(|slot| slot.name == "der(x)")
                .unwrap();
            assert_eq!(
                derivative.value, expected,
                "{function}, samples[1]={sample}"
            );
        }
    }
}
