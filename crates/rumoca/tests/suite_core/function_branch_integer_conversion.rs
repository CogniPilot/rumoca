//! MLS 3.6 §10.6.13: an Integer expression assigned to a Real variable is
//! converted to Real. Inside a runtime conditional branch the assigned value
//! stands in for the variable until the branch join, so later statements of the
//! same branch must read the converted Real value: an element update and
//! matrix algebra on `zeros(..)` (an Integer-valued builtin) behave exactly as
//! they would on the declared Real array. Expected values are
//! computed by hand.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, eval_dae_at};

const MODELS: &str = r#"
function elementUpdate
  input Boolean mask;
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  if mask then
    J := zeros(3,6);
    J[1,4] := 1.5;
    N := N + transpose(J)*J;
  end if;
end elementUpdate;

model ElementUpdate
  input Boolean mask = true;
  output Real N[6,6];
equation
  N = elementUpdate(mask);
end ElementUpdate;

function skew
  input Real v[3];
  output Real S[3,3];
algorithm
  S := {{0,-v[3],v[2]},{v[3],0,-v[1]},{-v[2],v[1],0}};
end skew;

function normalMatrix
  input Boolean mask[2];
  input Real p[2,3];
  output Real N[6,6];
protected
  Real J[3,6];
algorithm
  N := zeros(6,6);
  J := zeros(3,6);
  for i in 1:2 loop
    if mask[i] then
      J := zeros(3,6);
      J[:,1:3] := identity(3);
      J[:,4:6] := -skew(p[i,:]);
      N := N + transpose(J)*J;
    end if;
  end for;
end normalMatrix;

model NormalMatrix
  input Boolean mask[2] = {true, false};
  input Real p[2,3] = [1,2,3; 4,5,6];
  output Real N[6,6];
equation
  N = normalMatrix(mask, p);
end NormalMatrix;
"#;

fn matrix(model: &str) -> Vec<Vec<f64>> {
    let compiled = Compiler::new()
        .model(model)
        .compile_str(MODELS, "BranchIntegerConversion.mo")
        .unwrap_or_else(|error| panic!("{model} should compile: {error}"));
    let probe = eval_dae_at(&compiled.dae, &SimOptions::default(), &[], 0.0)
        .unwrap_or_else(|error| panic!("{model} should evaluate: {error}"));
    assert!(probe.report.error.is_none(), "{:?}", probe.report.error);
    (1..=6)
        .map(|row| {
            (1..=6)
                .map(|column| {
                    let name = format!("N[{row},{column}]");
                    probe
                        .report
                        .solver_y
                        .iter()
                        .find(|slot| slot.name.replace(' ', "") == name)
                        .unwrap_or_else(|| panic!("{model} has no {name}"))
                        .value
                })
                .collect()
        })
        .collect()
}

#[test]
fn a_real_element_update_of_a_converted_branch_value_is_exact() {
    // J has the single entry J[1,4] = 1.5, so J'J has N[4,4] = 2.25 only.
    let n = matrix("ElementUpdate");
    for (row, values) in n.iter().enumerate() {
        for (column, value) in values.iter().enumerate() {
            let expected = if (row, column) == (3, 3) { 2.25 } else { 0.0 };
            assert_eq!(*value, expected, "N[{},{}]", row + 1, column + 1);
        }
    }
}

#[test]
fn a_block_assembled_jacobian_in_a_selected_loop_branch_matches_its_normal_matrix() {
    // J = [I, -S(p)] with p = (1,2,3), so J'J = [[I, -S], [S', S'S]] and
    // S'S = |p|^2 I - p p'. The second row of p is never selected.
    let n = matrix("NormalMatrix");
    let p = [1.0, 2.0, 3.0];
    let s = [[0.0, -3.0, 2.0], [3.0, 0.0, -1.0], [-2.0, 1.0, 0.0]];
    let norm = p.iter().map(|value| value * value).sum::<f64>();
    for row in 0..3 {
        for column in 0..3 {
            let identity = if row == column { 1.0 } else { 0.0 };
            assert_eq!(n[row][column], identity, "top-left");
            assert_eq!(n[row][column + 3], -s[row][column], "top-right");
            assert_eq!(n[row + 3][column], -s[column][row], "bottom-left");
            assert_eq!(
                n[row + 3][column + 3],
                norm * identity - p[row] * p[column],
                "bottom-right"
            );
        }
    }
}
