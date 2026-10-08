//! Source-sized window controls; these are not full-frame FAST admission.

use super::*;

#[test]
fn source_input_ownership_keeps_connected_component_unknowns() {
    let _lock = session_test_guard();
    let source = "model TopGuard
      parameter Boolean enabled=true;
      input Real u[3]={1,2,3}; output Real y[3];
      equation for i in 1:3 loop
        if enabled then y[i]=u[i]; else y[i]=0; end if;
      end for; end TopGuard;
      model ChildGuard
        parameter Boolean enabled=true; input Real u[3]; output Real y[3];
        equation for i in 1:3 loop
          if enabled then y[i]=u[i]; else y[i]=0; end if;
        end for; end ChildGuard;
      model ConnectedGuard
        input Real externalValue[3]={1,2,3}; Real unknown[3]; ChildGuard child;
        equation unknown=externalValue; child.u=unknown; end ConnectedGuard;";
    for (model_name, parameter_name, input_name, tunable) in [
        ("TopGuard", "enabled", "u", true),
        ("ConnectedGuard", "child.enabled", "child.u", false),
    ] {
        assert_source_input_ownership(source, model_name, parameter_name, input_name, tunable);
    }
}

fn assert_source_input_ownership(
    source: &str,
    model_name: &str,
    parameter_name: &str,
    input_name: &str,
    tunable: bool,
) {
    crate::with_singleton_session(|session| {
        session.update_document("input.mo", source);
        let requested = crate::qualify_input_model_name(session, model_name);
        let compilation = crate::compile_requested_model(session, &requested)?;
        compilation.dae.inspect(|view| {
            let variables: Vec<_> = (0..view.variable_count())
                .map(|index| view.variable(view.variable_id(index).unwrap()).unwrap())
                .collect();
            let parameter = variables
                .iter()
                .find(|variable| variable.name().as_str() == parameter_name)
                .unwrap();
            let input = variables
                .iter()
                .find(|variable| variable.name().as_str() == input_name)
                .unwrap();
            assert_eq!(parameter.is_tunable(), tunable);
            assert_eq!(
                input.role(),
                if tunable {
                    rumoca_compile::compile::VariableRole::Input
                } else {
                    rumoca_compile::compile::VariableRole::Algebraic
                }
            );
        });
        Ok(())
    })
    .unwrap();
}

#[test]
fn ordinary_parameter_guard_in_structured_family_retains_runtime_selection() {
    let _lock = session_test_guard();
    let source = "model StructuredRuntimeGuard
        parameter Boolean enabled=true;
        input Real x[3]={2.0,-5.0,0.25}; output Real y[3];
        equation for i in 1:3 loop
          if enabled then y[i]=x[i]+1.0; else y[i]=x[i]-1.0; end if;
        end for;
      end StructuredRuntimeGuard;";
    let artifact = crate::native_assignment_api::with_prepared_native_model(
        source,
        "StructuredRuntimeGuard",
        crate::native_program_api::model_artifact,
    )
    .unwrap();
    let artifact: serde_json::Value = serde_json::from_str(&artifact).unwrap();
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for enabled in [0.0, 1.0, 0.0] {
        parameters[slot(&artifact, "enabled", "P")] = enabled;
        let actual = execution.run(&parameters);
        for (index, input) in [2.0_f64, -5.0, 0.25].into_iter().enumerate() {
            let expected = if enabled == 1.0 {
                input + 1.0
            } else {
                input - 1.0
            };
            let name = format!("y[{}]", index + 1);
            assert_eq!(
                actual[slot(&artifact, &name, "Y")].to_bits(),
                expected.to_bits()
            );
        }
    }
}

fn source() -> &'static str {
    "function WindowCorners
       input Real patch[7,7]; output Real value;
       algorithm value := patch[1,1]-patch[7,7]; end WindowCorners;
     model LazyAffineWindows
       input Real x[9,10] = zeros(9,10); output Real y[9,10];
       equation for row in 1:9 loop
         for column in 1:10 loop
           if row > 3 and row < 7 and column > 3 and column < 8 then
             y[row,column] = WindowCorners(x[row-3:row+3,column-3:column+3]);
           else y[row,column] = 0.0; end if;
         end for;
       end for;
     end LazyAffineWindows;"
}

#[test]
fn runtime_first_guard_keeps_later_structural_windows_lazy_and_recalibratable() {
    let _lock = session_test_guard();
    let mixed = source()
        .replace(
            "input Real x[9,10]",
            "parameter Boolean override=false; input Real x[9,10]",
        )
        .replace(
            "if row > 3",
            "if override then y[row,column]=17.0; elseif row > 3",
        );
    let artifact = artifact(&mixed);
    assert_eq!(artifact["profile"], "native-direct-program-f64-v3");
    let mut execution = CallExecution::new(&artifact);
    let mut parameters = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    let input = std::array::from_fn::<_, 90, _>(|index| index as f64 * 0.25 + 1.0);
    for (index, value) in input.iter().copied().enumerate() {
        parameters[slot(
            &artifact,
            &format!("x[{},{}]", index / 10 + 1, index % 10 + 1),
            "P",
        )] = value;
    }
    for override_value in [0.0, 1.0, 0.0] {
        parameters[slot(&artifact, "override", "P")] = override_value;
        let actual = execution.run(&parameters);
        for index in 0..90 {
            let row = index / 10;
            let column = index % 10;
            let expected = if override_value == 1.0 {
                17.0
            } else if (3..6).contains(&row) && (3..7).contains(&column) {
                corner_window(&input, row, column, false)
            } else {
                0.0
            };
            assert_eq!(
                actual[slot(&artifact, &format!("y[{},{}]", row + 1, column + 1), "Y")].to_bits(),
                expected.to_bits()
            );
        }
    }
}

fn artifact(source: &str) -> serde_json::Value {
    artifact_with_owners(source, 1)
}

/// The artifact of `source`, whose call table holds exactly `owners` owners.
fn artifact_with_owners(source: &str, owners: usize) -> serde_json::Value {
    let artifact = crate::native_assignment_api::with_prepared_native_model(
        source,
        "LazyAffineWindows",
        |model, source, name| {
            assert_eq!(model.pure_calls.owners().len(), owners);
            crate::native_program_api::model_artifact(model, source, name)
        },
    )
    .unwrap();
    serde_json::from_str(&artifact).unwrap()
}

fn check(artifact: &serde_json::Value, edited: bool, weighted: bool, input: &[f64; 90]) {
    let mut execution = CallExecution::new(artifact);
    let mut p = artifact["parameters"]
        .as_array()
        .unwrap()
        .iter()
        .map(|value| value.as_f64().unwrap())
        .collect::<Vec<_>>();
    for (index, value) in input.iter().copied().enumerate() {
        let name = format!("x[{},{}]", index / 10 + 1, index % 10 + 1);
        p[slot(artifact, &name, "P")] = value;
    }
    for _ in 0..2 {
        let actual = execution.run(&p);
        check_outputs(artifact, edited, weighted, input, &actual);
    }
}

fn check_outputs(
    artifact: &serde_json::Value,
    edited: bool,
    weighted: bool,
    input: &[f64; 90],
    actual: &[f64],
) {
    for index in 0..90 {
        let row = index / 10;
        let column = index % 10;
        let expected = if (3..6).contains(&row) && (3..7).contains(&column) {
            if weighted {
                ordered_weighted_window(input, row, column, edited)
            } else {
                corner_window(input, row, column, edited)
            }
        } else {
            // The unselected slices include index -2 and index 13. Native
            // preparation/execution must not evaluate those invalid windows.
            0.0
        };
        let name = format!("y[{},{}]", row + 1, column + 1);
        let value = actual[slot(artifact, &name, "Y")];
        if expected.is_nan() {
            assert!(value.is_nan());
        } else {
            assert_eq!(value.to_bits(), expected.to_bits(), "{name}");
        }
    }
}

fn corner_window(input: &[f64; 90], row: usize, column: usize, edited: bool) -> f64 {
    let first = input[(row - 3) * 10 + column - 3];
    let last = input[(row + 3) * 10 + column + 3];
    if edited { first + last } else { first - last }
}

fn ordered_weighted_window(input: &[f64; 90], row: usize, column: usize, edited: bool) -> f64 {
    let mut result = 0.0;
    for patch_row in 0..7 {
        for patch_column in 0..7 {
            let column_weight = (patch_column + 1) as i64;
            let row_weight = 10 * (patch_row + 1) as i64;
            let weight = if edited {
                row_weight - column_weight
            } else {
                row_weight + column_weight
            };
            result += input[(row - 3 + patch_row) * 10 + column - 3 + patch_column] * weight as f64;
        }
    }
    result
}

#[test]
fn parameter_dimension_guards_preserve_inactive_windows_over_algebraic_inputs() {
    let _lock = session_test_guard();
    let source = source()
        .replace("input Real x[9,10]", "parameter Integer height=9, width=10; constant Integer radius=3; input Real x[9,10]")
        .replace("x[9,10] = zeros(9,10); output Real y[9,10]", "x[height,width] = zeros(height,width); output Real y[height,width]")
        .replace("equation for row", "protected Real gray[height,width]; equation for row")
        .replace("row in 1:9", "row in 1:height")
        .replace("column in 1:10", "column in 1:width")
        .replace("if row > 3 and row < 7 and column > 3 and column < 8 then", "gray[row,column]=x[row,column]; if row>radius and row<=height-radius and column>radius and column<=width-radius then")
        .replace("WindowCorners(x[row-3:row+3,column-3:column+3])", "WindowCorners(gray[row-radius:row+radius,column-radius:column+radius])");
    for edited in [false, true] {
        let source = if edited {
            source.replace("patch[1,1]-patch[7,7]", "patch[1,1]+patch[7,7]")
        } else {
            source.clone()
        };
        // The algebraic intermediate keeps the row and column nest one compact
        // family with its own synthesized owner (SOLVE-C70), and every interior
        // window shares the one window owner.
        let artifact = artifact_with_owners(&source, 2);
        let input = std::array::from_fn(|index| (index as f64 - 45.0) / 8.0);
        check(&artifact, edited, false, &input);
    }
}

#[test]
fn original_affine_seven_by_seven_windows_preserve_lazy_borders_and_source_edit() {
    let _lock = session_test_guard();
    for edited in [false, true] {
        let source = if edited {
            source().replace("patch[1,1]-patch[7,7]", "patch[1,1]+patch[7,7]")
        } else {
            source().to_owned()
        };
        let artifact = artifact(&source);
        let finite = std::array::from_fn(|index| (index as f64 - 45.0) / 8.0);
        check(&artifact, edited, false, &finite);
        let ieee = std::array::from_fn(|index| match index % 4 {
            0 => -0.0,
            1 => 0.0,
            2 => f64::INFINITY,
            _ => f64::NEG_INFINITY,
        });
        check(&artifact, edited, false, &ieee);
    }
}

#[test]
fn original_affine_seven_by_seven_windows_preserve_all_cells_order_and_source_edit() {
    let _lock = session_test_guard();
    let weighted = source().replace(
        "algorithm value := patch[1,1]-patch[7,7];",
        "algorithm value := 0.0; for r in 1:7 loop for c in 1:7 loop
           value := value + patch[r,c]*(10*r+c); end for; end for;",
    );
    for edited in [false, true] {
        let source = if edited {
            weighted.replace("10*r+c", "10*r-c")
        } else {
            weighted.clone()
        };
        let artifact = artifact(&source);
        let input = std::array::from_fn(|index| {
            let row = index / 10;
            let column = index % 10;
            ((row * 19 + column * 7) as f64 - 79.0) / 8.0
        });
        check(&artifact, edited, true, &input);
    }
}

#[test]
fn named_integer_radius_windows_preserve_one_owner_lazy_borders_and_source_edit() {
    let _lock = session_test_guard();
    for prefix in ["constant", "parameter"] {
        let named = source()
            .replace(
                "input Real x[9,10]",
                &format!("{prefix} Integer radius=3; input Real x[9,10]"),
            )
            .replace(
                "row-3:row+3,column-3:column+3",
                "row-radius:row+radius,column-radius:column+radius",
            );
        for edited in [false, true] {
            let source = if edited {
                named.replace("patch[1,1]-patch[7,7]", "patch[1,1]+patch[7,7]")
            } else {
                named.clone()
            };
            let artifact = artifact(&source);
            let input = std::array::from_fn(|index| (index as f64 - 45.0) / 8.0);
            check(&artifact, edited, false, &input);
        }
    }
}
