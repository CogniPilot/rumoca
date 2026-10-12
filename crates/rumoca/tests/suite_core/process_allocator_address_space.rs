//! Issue #365: the rayon global pool of a `rumoca sim` run reserves address
//! space only for its workers' stacks, so a run under an address-space limit
//! (`RLIMIT_AS`, as containers and sandboxes set) needs no more than the
//! simulation uses plus those stacks, whatever the pool size.
//!
//! The executable's process allocator fixes this at startup
//! (`rumoca_allocator`, SPEC_0041 §6). Without it, two reservations that are
//! never used exhaust the limit before the pool is up, and the run aborts
//! with `ThreadPoolBuildError` (the next worker stack's `mmap` fails):
//!
//! - mimalloc's default 1 GiB arena, reserved before `main`;
//! - one glibc malloc arena per worker, 64 MiB each, created by the thread
//!   start-up's own `pthread_getattr_np` call.

#![cfg(target_os = "linux")]

use std::process::Command;

use tempfile::tempdir;

use super::state_jacobian_sparsity::heat_source;

/// Workers of the global pool. Pinned through `RAYON_NUM_THREADS` so the
/// proof is the same on a 2-core and a 64-core host: the claim is about the
/// reservation per worker, not about the host.
const POOL_WORKERS: u64 = 256;

/// Stack of each worker thread, pinned through `RUST_MIN_STACK` (it is also
/// the standard library's default).
const WORKER_STACK_MIB: u64 = 2;

/// mimalloc's built-in 64-bit arena reservation.
const MIMALLOC_DEFAULT_ARENA_MIB: u64 = 1024;

/// The limit is exactly one default mimalloc arena plus the workers' stacks
/// (1536 MiB). Without the startup configuration the executable's own
/// mappings (at least about 75 MiB) push the run over it, even if only one
/// of the two reservations above were left unbounded (the glibc arenas alone
/// ask for 16 GiB). With the configuration the run reserves one 64 MiB
/// mimalloc arena and one glibc arena, and fits with over 800 MiB to spare.
#[test]
fn a_256_worker_pool_simulates_under_one_default_arena_plus_its_stacks() {
    let limit_kib = (MIMALLOC_DEFAULT_ARENA_MIB + POOL_WORKERS * WORKER_STACK_MIB) * 1024;
    let dir = tempdir().expect("work directory");
    let file = dir.path().join("HeatL.mo");
    std::fs::write(&file, heat_source()).expect("write the heat equation");
    let csv = dir.path().join("result.csv");
    let output = Command::new("sh")
        .arg("-c")
        .arg(format!("ulimit -v {limit_kib} && exec \"$0\" \"$@\""))
        .arg(env!("CARGO_BIN_EXE_rumoca"))
        .arg("sim")
        .arg(&file)
        .args(["--model", "HeatL", "--solver", "bdf", "--t-end", "0.01"])
        .args(["--dt", "0.001", "--output"])
        .arg(&csv)
        .env("RAYON_NUM_THREADS", POOL_WORKERS.to_string())
        .env("RUST_MIN_STACK", (WORKER_STACK_MIB << 20).to_string())
        // The configuration is fixed in code; these must not move it.
        .env("MIMALLOC_ARENA_RESERVE", "1GiB")
        .env("MALLOC_ARENA_MAX", "64")
        .output()
        .expect("run rumoca sim under an address-space limit");
    assert!(
        output.status.success(),
        "stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(csv.is_file(), "the run wrote its trace");
}

#[test]
fn issue_425_original_38_bench_fits_a_two_gib_address_space() {
    let dir = tempdir().expect("original Euler benchmark directory");
    let file = dir.path().join("issue_425_38.mo");
    std::fs::write(
        &file,
        super::reported_bug_runtime::issue_425_original_38_source(),
    )
    .expect("write the original model with only nx/ny changed to 38");
    let output = Command::new("sh")
        .arg("-c")
        .arg("ulimit -v 2097152 && exec \"$0\" \"$@\"")
        .arg(env!("CARGO_BIN_EXE_rumoca"))
        .args(["sim", "bench"])
        .arg(&file)
        .args([
            "--model",
            "E30x30",
            "--t-end",
            "0.1",
            "--dt",
            "0.01",
            "--iterations",
            "1",
            "--warmups",
            "0",
            "--solver",
            "rk-like",
            "--json",
        ])
        .env("RAYON_NUM_THREADS", "1")
        .output()
        .expect("run the original 38x38 benchmark under a 2 GiB address-space limit");
    eprintln!("original Euler38 benchmark status: {}", output.status);
    eprintln!("stdout: {}", String::from_utf8_lossy(&output.stdout));
    eprintln!("stderr: {}", String::from_utf8_lossy(&output.stderr));
    assert!(output.status.success(), "original benchmark must complete");
    let report: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("benchmark emits its JSON report");
    assert_eq!(report["model"], "E30x30");
    assert_eq!(report["solver"], "rk-like");
    assert_eq!(report["t_end"], 0.1);
    assert_eq!(report["dt"], 0.01);
    assert_eq!(report["iterations"], 1);
    assert_eq!(report["warmups"], 0);
    assert_eq!(report["last_points"], 11);
    assert_eq!(report["last_final_time"], 0.1);
    for field in [
        "compile_seconds",
        "prepare_seconds",
        "prepare_ir_solve_seconds",
        "prepare_backend_build_seconds",
        "hot_total_seconds",
    ] {
        let value = report[field].as_f64().expect("reported timing is numeric");
        assert!(value.is_finite() && value >= 0.0, "{field}={value}");
    }
}
