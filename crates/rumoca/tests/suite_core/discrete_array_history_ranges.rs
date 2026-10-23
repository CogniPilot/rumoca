use rumoca::Compiler;
use rumoca_sim::{SimExecutionPolicy, SimOptions, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
model ArrayHistoryRanges
  discrete Real a[2, 2](start = {{1, 2}, {3, 4}}, each fixed = true);
  discrete Real prior[2, 2](each start = 0, each fixed = true);
  discrete Real between(start = -1, fixed = true);
algorithm
  when sample(0, 1) then
    prior := pre(a);
    a := {{pre(a[2, 2]) + 1, pre(a[2, 1]) + 2},
          {pre(a[1, 2]) + 3, pre(a[1, 1]) + 4}};
  end when;
equation
  when time >= 1.5 then
    between = pre(a[1, 1]);
  end when;
end ArrayHistoryRanges;
"#;

#[test]
fn whole_array_history_matches_scalar_values_across_ticks_and_unrelated_events() {
    let compiled = Compiler::new()
        .model("ArrayHistoryRanges")
        .compile_str(SOURCE, "ArrayHistoryRanges.mo")
        .unwrap();
    let mut traces = Vec::new();
    for execution_policy in [
        SimExecutionPolicy::Interpreter,
        SimExecutionPolicy::NativeRequired,
    ] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 3.0,
                dt: Some(0.25),
                execution_policy,
                ..SimOptions::default()
            },
        )
        .unwrap();
        for (name, expected) in [
            ("a[1,1]", 11.0),
            ("a[1,2]", 12.0),
            ("a[2,1]", 13.0),
            ("a[2,2]", 14.0),
            ("prior[1,1]", 10.0),
            ("prior[1,2]", 10.0),
            ("prior[2,1]", 10.0),
            ("prior[2,2]", 10.0),
            ("between", 6.0),
        ] {
            let index = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap();
            assert_eq!(
                *result.data[index].last().unwrap(),
                expected,
                "{execution_policy:?}: {name}"
            );
        }
        traces.push((
            result.names,
            result
                .data
                .iter()
                .map(|column| {
                    column
                        .iter()
                        .map(|value| value.to_bits())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>(),
        ));
    }
    assert_eq!(traces[0], traces[1]);
}
