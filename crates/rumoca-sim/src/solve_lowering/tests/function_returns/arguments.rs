//! Selected call arguments retain their index checks; guarding the entire call
//! protects its arguments too. Pure unused-value omission follows MLS §3.3.

use super::compile_model;
use crate::{SimOptions, eval_dae_at};

const FUNCTION: &str = r#"
function argumentReturn
  input Boolean first;
  input Real sample;
  output Real result;
algorithm
  result := 0;
  if first then
    result := 1;
    return;
  elseif sample > 0 then
    result := 2;
    return;
  end if;
  result := 3;
end argumentReturn;
"#;

fn evaluate(first: bool, index: f64, guarded: bool) -> crate::EvalAtReport {
    let call = "argumentReturn(first, samples[integer(indexValue)])";
    let expression = if guarded {
        format!("if first then 1 else {call}")
    } else {
        call.into()
    };
    let source = format!(
        "{FUNCTION}\nmodel ReturnArgument\n\
         input Boolean first;\n\
         Real x(start=0, fixed=true);\n\
         Real indexValue(start=1, fixed=true);\n\
         Real samples[1] = {{5}};\n\
         equation\n der(indexValue)=1;\n der(x)={expression};\n\
         end ReturnArgument;"
    );
    let compiled = compile_model("ReturnArgument", &source, "return-argument.mo")
        .expect("runtime-valued actual argument constructs checked DAE");
    let wire = serde_json::to_string(&compiled.dae).unwrap();
    let decoded = serde_json::from_str(&wire).expect("checked DAE survives wire replay");
    eval_dae_at(
        &decoded,
        &SimOptions {
            initial_inputs: vec![("first".into(), f64::from(first))],
            ..SimOptions::default()
        },
        &[("indexValue".into(), index)],
        0.0,
    )
    .expect("runtime-valued call argument lowers through Solve IR")
    .report
}

#[test]
fn selected_return_argument_cannot_erase_its_index_fault() {
    let report = evaluate(false, 2.0, false);
    assert!(
        report.error.is_some(),
        "selected actual argument must fault"
    );
}

#[test]
fn inactive_call_skips_its_invalid_actual_argument() {
    let report = evaluate(true, 2.0, true);
    assert!(report.error.is_none(), "{:?}", report.error);
    assert_eq!(
        report
            .derivatives
            .iter()
            .find(|slot| slot.name == "der(x)")
            .unwrap()
            .value,
        1.0
    );
    let report = evaluate(false, 2.0, true);
    assert!(
        report.error.is_some(),
        "selected call must evaluate its actual"
    );
}

#[test]
fn valid_call_arguments_preserve_both_return_arms() {
    for (first, expected) in [(true, 1.0), (false, 2.0)] {
        let report = evaluate(first, 1.0, false);
        assert!(report.error.is_none(), "{:?}", report.error);
        assert_eq!(
            report
                .derivatives
                .iter()
                .find(|slot| slot.name == "der(x)")
                .unwrap()
                .value,
            expected
        );
    }
}
