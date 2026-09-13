use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"
record Products
  Real tensor[3,3];
  Real axial[3];
end Products;
function products
  input Real u[3];
  input Real v[3];
  output Products result;
algorithm
  result := Products(outerProduct(u,v) + identity(3), cross(u,v));
  annotation(Inline=true);
end products;
model BilinearConstraint
  Real x[3](start={1,2,3}, each fixed=true, each stateSelect=StateSelect.always);
  Real matrix[3,3](each stateSelect=StateSelect.always);
  Real spin[3](each stateSelect=StateSelect.always);
  Real matrix_rate[3,3];
  Real spin_rate[3];
  Products p;
equation
  der(x) = -x;
  p = products(x, {2,-1,1});
  matrix = p.tensor;
  spin = p.axial;
  matrix_rate = der(matrix);
  spin_rate = der(spin);
end BilinearConstraint;
"#;

#[test]
fn retained_manifolds_preserve_bilinear_tensor_function_values() {
    check_bilinear_constraint(SOURCE);
}

#[test]
fn retained_manifolds_preserve_bilinear_values_with_real_arguments() {
    check_bilinear_constraint(&SOURCE.replace("{2,-1,1}", "{2.0,-1.0,1.0}"));
}

#[test]
fn direct_state_substitution_preserves_real_function_argument_types() {
    check_bilinear_constraint(
        &SOURCE
            .replace(", each stateSelect=StateSelect.always", "")
            .replace("(each stateSelect=StateSelect.always)", ""),
    );
}

fn check_bilinear_constraint(source: &str) {
    let compiled = Compiler::new()
        .model("BilinearConstraint")
        .compile_str(source, "bilinear_constraint.mo")
        .unwrap();
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap_or_else(|error| panic!("{solver_mode:?}: {error}"));
        let expected = expected_channels();
        assert_eq!(result.names.len(), expected.len());
        for (name, coefficient, offset) in &expected {
            let column = result.names.iter().position(|value| value == name).unwrap();
            for (&time, &actual) in result.times.iter().zip(&result.data[column]) {
                let expected = coefficient * (-time).exp() + offset;
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "{solver_mode:?} {name}({time}): {actual} != {expected}"
                );
            }
        }
    }
}

fn expected_channels() -> Vec<(String, f64, f64)> {
    let mut channels = Vec::new();
    for (i, x) in [1.0, 2.0, 3.0].into_iter().enumerate() {
        let index = i + 1;
        channels.push((format!("x[{index}]"), x, 0.0));
        let axial = [5.0, 5.0, -5.0][i];
        for prefix in ["spin", "p.axial"] {
            channels.push((format!("{prefix}[{index}]"), axial, 0.0));
        }
        channels.push((format!("spin_rate[{index}]"), -axial, 0.0));
        for (j, v) in [2.0, -1.0, 1.0].into_iter().enumerate() {
            let column = j + 1;
            let diagonal = f64::from(i == j);
            for prefix in ["matrix", "p.tensor"] {
                channels.push((format!("{prefix}[{index},{column}]"), x * v, diagonal));
            }
            channels.push((format!("matrix_rate[{index},{column}]"), -x * v, 0.0));
        }
    }
    channels
}
