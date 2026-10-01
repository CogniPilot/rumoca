//! A record component bound to a call of a record-valued function takes the
//! function's result (MLS §12.6 constructs records only through a record
//! class). Projecting the call's arguments onto record fields by name or
//! position, as for a record constructor, replaced the function's result by
//! its inputs whenever the input names matched the field names.

use rumoca::Compiler;

const SOURCE: &str = r#"
record State
  Real T;
  Real p;
end State;

function scaled
  input Real p;
  input Real T;
  output State s;
algorithm
  s := State(p = 10*p, T = 10*T);
end scaled;

function swapped
  input Real T;
  input Real p;
  output State s;
algorithm
  s := State(p = T, T = p);
end swapped;

model Top
  State a = scaled(1, 2 + time);
  State b = swapped(1, 2 + time);
  State c = scaled(T = 3, p = 4);
end Top;
"#;

#[test]
fn a_record_bound_to_a_function_call_takes_the_function_result() {
    let compiled = Compiler::new()
        .model("Top")
        .compile_str(SOURCE, "RecordFunctionBinding.mo")
        .unwrap_or_else(|error| panic!("Top compiles: {error:?}"));
    let result = rumoca_sim::simulate_dae_with_diagnostics(
        &compiled.dae,
        &rumoca_sim::SimOptions {
            t_end: 0.1,
            ..Default::default()
        },
    )
    .expect("Top simulates");
    let column = |name: &str| {
        let index = result.names.iter().position(|n| n == name);
        &result.data[index.unwrap_or_else(|| panic!("{name} in {:?}", result.names))]
    };
    for (index, time) in result.times.iter().enumerate() {
        let expected = [
            ("a.p", 10.0),
            ("a.T", 10.0 * (2.0 + time)),
            ("b.p", 1.0),
            ("b.T", 2.0 + time),
            ("c.p", 40.0),
            ("c.T", 30.0),
        ];
        for (name, value) in expected {
            let actual = column(name)[index];
            assert!(
                (actual - value).abs() < 1e-9,
                "{name} = {actual}, expected {value}"
            );
        }
    }
}
