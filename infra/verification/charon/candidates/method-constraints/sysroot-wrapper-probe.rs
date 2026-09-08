//! Logging `rustc` wrapper used to measure which crates a cargo entry routes
//! through `RUSTC_WRAPPER` and `RUSTC_WORKSPACE_WRAPPER`.
//!
//! Records every invocation's `--crate-name` to the file named by
//! `MIRI_PROBE_LOG`, then execs the real `rustc` with the arguments cargo
//! supplied and propagates its exit status. Results measured with this binary
//! are recorded in `sysroot-fact-collection.md`.
//!
//! Build and use:
//!
//!     rustc -O -o w sysroot-wrapper-probe.rs
//!     MIRI_PROBE_LOG=/path/to/log RUSTC_WRAPPER=/path/to/w cargo build ...
//!
//! Every `unwrap` below is deliberate. The measurement this probe supports is a
//! count of invocations, and a low count is the interesting outcome, so a probe
//! that failed to log while letting the build succeed would manufacture exactly
//! the result being looked for. Missing `MIRI_PROBE_LOG`, an unopenable log, and
//! a failed write must all abort the compilation they were meant to record.
//! For the same reason, read a zero count only alongside the build's exit
//! status: a build that fails before compiling anything also logs zero.
//!
//! The code below is kept token-identical to the binary that produced the
//! recorded numbers, so it is not reformatted to satisfy pedantic lints.
//! `clippy::pedantic` reports `items_after_statements` on the inner `use` and
//! `assigning_clones` on the `--crate-name` capture; both are accepted here in
//! exchange for that correspondence.

use std::process::Command;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let log = std::env::var("MIRI_PROBE_LOG").unwrap();
    let mut name = String::from("?");
    for p in args.windows(2) {
        if p[0] == "--crate-name" {
            name = p[1].clone();
        }
    }
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log)
        .unwrap();
    writeln!(f, "crate={name}").unwrap();
    let st = Command::new(&args[0]).args(&args[1..]).status().unwrap();
    std::process::exit(st.code().unwrap_or(1));
}
