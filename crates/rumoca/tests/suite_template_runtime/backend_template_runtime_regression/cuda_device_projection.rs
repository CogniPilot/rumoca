//! NVCC compiles the ME projection kernel built for a device: each test
//! model's device unit, from the model its FMI component retains and from the
//! simulation lowering a device target passes, with a kernel that runs one
//! lane per point, so every kernel entry a lane reaches is compiled to device
//! code.

use super::*;
use crate::me_projection_device::{
    device_unit, lane_harness, models, solve_device_unit, state_count,
};

#[test]
fn cuda_ode_shared_projection_kernel_compiles_with_required_nvcc() {
    let available = Command::new("nvcc").arg("--version").output().is_ok();
    if !crate::template_runtime_policy::prerequisites_are_available(
        "NVCC device projection build",
        &[("NVCC", available)],
    ) {
        return;
    }
    let work = tempfile::tempdir().expect("device projection work directory");
    for (compiled, model) in models() {
        let units = [
            ("fmi", device_unit(&compiled)),
            ("solve", solve_device_unit(&compiled)),
        ];
        for (lowering, unit) in units {
            let source = work.path().join(format!("{model}_{lowering}.cu"));
            let unit = lane_harness(&unit, "Y 0\nP 0\n", state_count(&compiled));
            fs::write(&source, unit).expect("write the device unit");
            run_checked(
                Command::new("nvcc")
                    .args([
                        "-std=c++17",
                        "--fmad=false",
                        "-Werror",
                        "all-warnings",
                        "-c",
                    ])
                    .arg(&source)
                    .arg("-o")
                    .arg(work.path().join(format!("{model}_{lowering}.o"))),
                &format!("compile the device projection kernel of {model} ({lowering}) with NVCC"),
            );
        }
    }
}
