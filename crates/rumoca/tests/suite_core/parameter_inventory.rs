//! MLS §§5.3, 10.1, and 12.4: component parameter scopes and function formals.

use rumoca::Compiler;
use rumoca_sim::{SimOptions, SimSolverMode, simulate_dae_with_diagnostics};

const SOURCE: &str = r#"package ParameterInventoryProbe
  function bump
    input Integer n;
    output Integer y;
  algorithm
    y := n + 1;
  end bump;
  model Component
    parameter Integer n;
    parameter Real gain;
    parameter Boolean enabled = true;
    parameter Real values[:,2] = fill(gain, n, 2);
    final parameter Integer count = size(values, 1);
    Real x(start=0, fixed=true);
  equation
    der(x) = if enabled then bump(count)*gain else 0;
  end Component;
  model Root
    parameter Integer n=99;
    Component left(n=2, gain=0.5);
    Component right(n=5, gain=2);
    Real y=left.x+right.x;
  end Root;
end ParameterInventoryProbe;
"#;

#[test]
fn parameter_inventory_preserves_sibling_shapes_and_function_formals() {
    let compiled = Compiler::new()
        .model("ParameterInventoryProbe.Root")
        .compile_str(SOURCE, "parameter_inventory.mo")
        .unwrap();
    for (component, rows) in [("left", 2), ("right", 5)] {
        let name = rumoca_core::VarName::new(format!("{component}.values"));
        assert_eq!(compiled.flat.variables[&name].dims, vec![rows, 2]);
    }
    for solver_mode in [SimSolverMode::Bdf, SimSolverMode::RkLike] {
        let result = simulate_dae_with_diagnostics(
            &compiled.dae,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.05),
                solver_mode,
                ..Default::default()
            },
        )
        .unwrap();
        for (name, rate) in [("left.x", 1.5), ("right.x", 12.0), ("y", 13.5)] {
            let channel = result
                .names
                .iter()
                .position(|candidate| candidate == name)
                .unwrap();
            assert!(!result.times.is_empty());
            for (&time, &actual) in result.times.iter().zip(&result.data[channel]) {
                assert!(
                    (actual - rate * time).abs() < 1e-7,
                    "{solver_mode:?}: {name}({time})={actual}"
                );
            }
        }
    }
}
