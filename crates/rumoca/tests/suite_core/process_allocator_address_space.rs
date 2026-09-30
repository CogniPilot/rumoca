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
