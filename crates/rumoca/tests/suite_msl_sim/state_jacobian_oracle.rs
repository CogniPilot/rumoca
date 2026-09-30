//! Issue #365: the Solve IR's certified state-Jacobian relation, which the BDF
//! backend colors its Jacobian with, contains every nonzero of the exact dense
//! state Jacobian (the kernel's forward-mode JVP, one unit seed per state) of
//! MSL models with algebraic loops, tearing, and reduced mechanics.

use rumoca_sim::{SimOptions, jacobian_for_dae, lower_dae_for_simulation};

use super::msl_sim_regression::require_msl_compiler;

const MODELS: &[&str] = &[
    "Modelica.Electrical.Machines.Examples.DCMachines.DCPM_Start",
    "Modelica.Electrical.Analog.Examples.ChuaCircuit",
    "Modelica.Electrical.Analog.Examples.CauerLowPassAnalog",
    "Modelica.Mechanics.MultiBody.Examples.Elementary.DoublePendulum",
    "Modelica.Mechanics.MultiBody.Examples.Loops.Fourbar1",
    "Modelica.Thermal.HeatTransfer.Examples.TwoMasses",
    "Modelica.Blocks.Examples.PID_Controller",
];

#[test]
fn the_state_jacobian_relation_contains_every_exact_jacobian_nonzero() {
    for &model in MODELS {
        let source = format!("model Oracle\n  extends {model};\nend Oracle;\n");
        let compiled = require_msl_compiler()
            .model("Oracle")
            .compile_str(&source, "Oracle.mo")
            .unwrap_or_else(|error| panic!("compile {model}: {error:#}"));
        let opts = SimOptions::default();
        let lowered = lower_dae_for_simulation(&compiled.dae, &opts)
            .unwrap_or_else(|error| panic!("lower {model}: {error:?}"));
        let pattern = lowered
            .artifacts
            .continuous
            .structural
            .state_jacobian()
            .unwrap_or_else(|| panic!("{model} carries no state-Jacobian relation"));
        let probe = jacobian_for_dae(&compiled.dae, &opts, &[], 0.0)
            .unwrap_or_else(|error| panic!("probe {model}: {error:?}"));
        assert!(
            probe.report.error.is_none(),
            "{model}: {:?}",
            probe.report.error
        );
        for (row, values) in probe.report.matrix.iter().enumerate() {
            for (column, &value) in values.iter().enumerate() {
                assert!(
                    value == 0.0 || pattern.contains(row as u32, column as u32),
                    "{model}: d(der {row})/d(state {column}) = {value} is outside the relation"
                );
            }
        }
    }
}
