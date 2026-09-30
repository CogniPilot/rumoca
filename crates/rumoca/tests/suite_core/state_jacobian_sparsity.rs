//! Issue #365: the BDF Jacobian of a 400-cell heat equation is colored from
//! the Solve IR's certified state-Jacobian relation, so preparing it takes
//! memory linear in the cell count. The backend used to probe a dense
//! pattern, whose column-conflict graph has about N^3/2 edges (1.9 GB peak at
//! N = 400); a run under a 256 MiB address-space limit aborted.

use std::process::Command;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, lower_correlated_for_simulation_with_overrides};
use tempfile::tempdir;

const CELLS: usize = 400;

pub(super) fn heat_source() -> String {
    format!(
        "model HeatL
  parameter Integer N = {CELLS};
  parameter Real k = 1000.0;
  Real u[N](start = {{sin(3.14159 * i / (N + 1)) for i in 1:N}}, each fixed = true);
equation
  der(u[1]) = k * (-2 * u[1] + u[2]);
  for i in 2:N-1 loop
    der(u[i]) = k * (u[i-1] - 2 * u[i] + u[i+1]);
  end for;
  der(u[N]) = k * (u[N-1] - 2 * u[N]);
end HeatL;
"
    )
}

#[test]
fn the_state_jacobian_relation_of_a_heat_equation_is_tridiagonal() {
    let compiled = Compiler::new()
        .model("HeatL")
        .compile_str(&heat_source(), "HeatL.mo")
        .expect("compile the heat equation");
    let lowered =
        lower_correlated_for_simulation_with_overrides(&compiled.dae, &SimOptions::default())
            .expect("lower the heat equation");
    let pattern = lowered
        .model()
        .artifacts
        .continuous
        .structural
        .state_jacobian()
        .expect("the Solve IR carries the state-Jacobian relation");
    assert_eq!(
        (pattern.rows(), pattern.columns()),
        (CELLS as u32, CELLS as u32)
    );
    let nonzeros = pattern.nonzero_coordinates();
    assert_eq!(nonzeros.len(), 3 * CELLS - 2);
    assert!(
        nonzeros
            .iter()
            .all(|&(row, column)| row.abs_diff(column) <= 1)
    );
}

/// The BDF run of the same shape fits a 256 MiB address space on one worker.
#[cfg(target_os = "linux")]
#[test]
fn a_400_cell_heat_equation_simulates_with_bdf_under_a_256_mib_address_space() {
    let dir = tempdir().expect("work directory");
    let file = dir.path().join("HeatL.mo");
    std::fs::write(&file, heat_source()).expect("write the heat equation");
    let csv = dir.path().join("result.csv");
    let output = Command::new("sh")
        .arg("-c")
        .arg("ulimit -v 262144 && exec \"$0\" \"$@\"")
        .arg(env!("CARGO_BIN_EXE_rumoca"))
        .arg("sim")
        .arg(&file)
        .args(["--model", "HeatL", "--solver", "bdf", "--t-end", "0.01"])
        .args(["--dt", "0.001", "--output"])
        .arg(&csv)
        .env("RAYON_NUM_THREADS", "1")
        .output()
        .expect("run rumoca sim under an address-space limit");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}
