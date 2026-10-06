//! Runtime-valued subscript controls complement the constant-index controls.
//! Every case compiles the same graph; named state overrides choose the index
//! only after dependency projection and Solve construction have completed.

use super::{compile_model, lazy::FUNCTIONS};
use crate::{SimOptions, eval_dae_at};

fn evaluate(function: &str, sample: f64, k: f64, time: f64) -> crate::EvalAtReport {
    let source = format!(
        "{FUNCTIONS}\nmodel DynamicReturn\n\
         input Boolean first;\n\
         Real x(start=0, fixed=true);\n\
         Real indexValue(start=1, fixed=true);\n\
         equation\n der(indexValue)=1;\n\
         der(x)={function}(first, {{{sample}}}, integer(indexValue));\n\
         end DynamicReturn;"
    );
    let compiled = compile_model("DynamicReturn", &source, "dynamic-return.mo")
        .expect("runtime-valued index constructs checked DAE");
    let wire = serde_json::to_string(&compiled.dae).unwrap();
    let decoded = serde_json::from_str(&wire).expect("checked DAE survives wire replay");
    eval_dae_at(
        &decoded,
        &SimOptions {
            initial_inputs: vec![("first".into(), f64::from(time >= 0.0))],
            ..SimOptions::default()
        },
        &[("indexValue".into(), k)],
        time,
    )
    .expect("runtime-valued index lowers through Solve IR")
    .report
}

#[test]
fn dynamic_inactive_return_predicates_skip_invalid_gathers() {
    for function in ["orderedReturningElseif", "inactiveLaterReturn"] {
        let report = evaluate(function, 5.0, 2.0, 0.0);
        assert!(report.error.is_none(), "{function}: {:?}", report.error);
        let derivative = report
            .derivatives
            .iter()
            .find(|slot| slot.name == "der(x)")
            .unwrap();
        assert_eq!(derivative.value, 1.0, "{function}");
    }
}

#[test]
fn dynamic_active_return_predicates_keep_invalid_gather_faults() {
    for function in ["orderedReturningElseif", "inactiveLaterReturn"] {
        let report = evaluate(function, 5.0, 2.0, -1.0);
        let error = report.error.expect("selected gather must fault");
        assert!(
            error.contains("project aggregate element"),
            "{function}: {error}"
        );
    }
}

#[test]
fn dynamic_return_predicates_keep_second_branch_and_fallthrough() {
    for function in ["orderedReturningElseif", "inactiveLaterReturn"] {
        for (sample, expected) in [(5.0, 2.0), (-5.0, 3.0)] {
            let report = evaluate(function, sample, 1.0, -1.0);
            assert!(report.error.is_none(), "{function}: {:?}", report.error);
            let derivative = report
                .derivatives
                .iter()
                .find(|slot| slot.name == "der(x)")
                .unwrap();
            assert_eq!(derivative.value, expected, "{function}, sample={sample}");
        }
    }
}
