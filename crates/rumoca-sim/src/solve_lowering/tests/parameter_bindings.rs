//! Independent native regression for dependent binding and override ownership.

use super::*;

#[test]
fn indexed_row_swaps_read_the_complete_right_hand_side_before_updating() {
    let model = compile(
        r#"
function swapRows
 input Real matrix[3,2];
 input Integer row;
 output Real swapped[3,2];
 output Real sampled[2,2];
 output Real products[2];
algorithm
 swapped := matrix;
 swapped[{row, 3}, :] := swapped[{3, row}, :];
 sampled := matrix[3:-2:1, :];
 sampled[{1,1}, 2] := {7,8};
 products := {sum(swapped[r,c]*matrix[r,c] for r in 1:3) for c in 1:2};
end swapRows;
model IndexedRowSwap
 parameter Integer row = 1;
 Real x(start=0, fixed=true);
 output Real swapped[3,2];
 output Real sampled[2,2];
 output Real products[2];
equation
 (swapped, sampled, products) = swapRows({{x,2},{3,4},{5,6}}, row);
 der(x) = 1;
end IndexedRowSwap;
"#,
        "IndexedRowSwap",
    );
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        for row in [1, 2] {
            let mut session = SimulationSession::new(
                &model,
                SimOptions {
                    solver_mode,
                    param_overrides: vec![("row".into(), f64::from(row))],
                    ..SimOptions::default()
                },
            )
            .unwrap();
            for time in [0.0, 0.25] {
                session.advance_to(time).unwrap();
                assert_row_swap_observations(&mut session, time, (row - 1) as usize);
            }
        }
        for row in [0.0, 4.0] {
            assert!(
                SimulationSession::new(
                    &model,
                    SimOptions {
                        solver_mode,
                        param_overrides: vec![("row".into(), row)],
                        ..SimOptions::default()
                    }
                )
                .is_err(),
                "out-of-bounds row {row} must fail in {solver_mode:?}"
            );
        }
    }
}

fn assert_row_swap_observations(session: &mut SimulationSession, time: f64, row: usize) {
    let original = [[time, 2.0], [3.0, 4.0], [5.0, 6.0]];
    let mut expected = original;
    expected.swap(row, 2);
    for column in 0..2 {
        let product: f64 = (0..3)
            .map(|r| original[r][column] * expected[r][column])
            .sum();
        let actual = session
            .get(&format!("products[{}]", column + 1))
            .unwrap()
            .unwrap();
        assert!((actual - product).abs() < 1e-8, "mapped column {column}");
    }
    for (index, expected) in expected.iter().flatten().enumerate() {
        let name = format!("swapped[{},{}]", index / 2 + 1, index % 2 + 1);
        let actual = session.get(&name).unwrap().unwrap();
        assert!(
            (actual - expected).abs() < 1e-8,
            "{name}: {actual} != {expected}"
        );
    }
    for (name, expected) in [
        ("sampled[1,1]", 5.0),
        ("sampled[1,2]", 8.0),
        ("sampled[2,1]", time),
        ("sampled[2,2]", 2.0),
    ] {
        assert!(
            (session.get(name).unwrap().unwrap() - expected).abs() < 1e-8,
            "{name}"
        );
    }
}

#[test]
fn integer_arguments_are_widened_for_real_function_inputs_and_builtins() {
    let model = compile(
        r#"
function rootDifference
 input Integer n;
 input Integer m;
 output Real value;
algorithm
 value := sqrt((n+m)*(n-m));
end rootDifference;
function realVectorSum
 input Real values[3];
 output Real value;
algorithm
 value := sum(values);
end realVectorSum;
function nestedSum
 input Integer n;
 output Real value;
algorithm
 value := realVectorSum({n, 2, 3});
end nestedSum;
model WidenedParameterCalls
 parameter Integer n = 5;
 parameter Real root = rootDifference(n, 3);
 parameter Real nested = nestedSum(n);
 Real x(start=0, fixed=true);
 output Real seen[2] = {root, nested};
equation
 der(x) = root + nested;
end WidenedParameterCalls;
"#,
        "WidenedParameterCalls",
    );
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                solver_mode,
                ..SimOptions::default()
            },
        )
        .unwrap();
        assert_eq!(session.get("seen[1]").unwrap().unwrap(), 4.0);
        assert_eq!(session.get("seen[2]").unwrap().unwrap(), 10.0);
        session.advance_to(0.25).unwrap();
        assert!((session.get("x").unwrap().unwrap() - 3.5).abs() < 1e-7);
    }
}

#[test]
fn dependent_parameter_updates_preserve_explicit_scalar_overrides() {
    let model = compile(
        r#"
model ParameterBindings
 parameter Real a = 2;
 parameter Real b[2] = {a, 2*a};
 parameter Real c = b[1] + b[2];
 Real x(start=0, fixed=true);
 output Real seen[4] = {a, b[1], b[2], c};
equation
 der(x) = c;
end ParameterBindings;
"#,
        "ParameterBindings",
    );
    for solver_mode in [SimSolverMode::RkLike, SimSolverMode::Bdf] {
        let mut session = SimulationSession::new(
            &model,
            SimOptions {
                solver_mode,
                param_overrides: vec![("a".into(), 3.0), ("b[1]".into(), 7.0)],
                ..SimOptions::default()
            },
        )
        .unwrap();
        for (name, expected) in [
            ("seen[1]", 3.0),
            ("seen[2]", 7.0),
            ("seen[3]", 6.0),
            ("seen[4]", 13.0),
        ] {
            assert_eq!(
                session.get(name).unwrap().unwrap(),
                expected,
                "{solver_mode:?}: {name}"
            );
        }
        session.advance_to(0.25).unwrap();
        assert!((session.get("x").unwrap().unwrap() - 3.25).abs() < 1e-7);
    }
}

#[test]
fn recursive_parameter_binding_returns_a_diagnostic_without_expanding_call_owners() {
    let model = compile(
        r#"
function countDown
 input Integer n;
 output Real value;
algorithm
 value := if n > 0 then countDown(n-1) + 1 else 0;
end countDown;
model RecursiveParameterBinding
 parameter Integer n = 3;
 parameter Real rate = countDown(n);
 Real x(start=0, fixed=true);
equation
 der(x) = rate;
end RecursiveParameterBinding;
"#,
        "RecursiveParameterBinding",
    );
    let error = match SimulationSession::new(&model, SimOptions::default()) {
        Ok(_) => panic!("recursive function must not enter the finite acyclic call profile"),
        Err(error) => error,
    };
    assert!(
        error
            .to_string()
            .contains("recursive functions cannot enter an acyclic pure-call graph"),
        "{error}"
    );
}
