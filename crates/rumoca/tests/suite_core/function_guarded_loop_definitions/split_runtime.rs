use super::*;

#[test]
fn split_record_state_follows_each_runtime_call_input() {
    let source = format!(
        "{SPLIT_BRANCHES}\n\
         model RuntimeSplit\n\
           input Real weights[3] = {{2,3,4}};\n\
           input Boolean reset = false;\n\
           input Boolean requested = true;\n\
           Real y = U.Score(U.State(2,0,weights), reset, requested);\n\
         end RuntimeSplit;\n"
    );
    let compiled = Compiler::new()
        .model("RuntimeSplit")
        .compile_str(&source, "RuntimeSplit.mo")
        .expect("the unchanged Score body admits external inputs");
    assert_runtime_call_retained(&compiled.dae);
    for (reset, requested, middle, expected) in [
        (0.0, 1.0, 3.0, 11.0),
        (1.0, 1.0, 3.0, 5.0),
        (0.0, 0.0, 3.0, -1.0),
        (0.0, 1.0, -3.0, -1.0),
        (0.0, 1.0, 3.0, 11.0),
    ] {
        let probe = eval_dae_at(
            &compiled.dae,
            &SimOptions {
                initial_inputs: vec![
                    ("reset".into(), reset),
                    ("requested".into(), requested),
                    ("weights[2]".into(), middle),
                ],
                ..SimOptions::default()
            },
            &[],
            0.0,
        )
        .unwrap();
        assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
        assert_eq!(solver_value(&probe.report, "y"), expected);
    }
}
