// Which wrapper changes invalidate Cargo's fingerprint, per crate class.
//
// Charon sets both `RUSTC_WRAPPER` and a fresh `RUSTC_WORKSPACE_WRAPPER` marker,
// and the two do not cover the same crates. This measures the six cells that
// matter for sidecar transport: a producer whose output depends on the wrapper
// is only re-run for crates whose fingerprint covers the wrapper that changed.
//
// Layout: a workspace member (`cc_root`) and a path dependency OUTSIDE the
// workspace (`cc_dep`). The path dependency stands in for a registry dependency
// because the only property that matters is that Cargo does not set
// `CARGO_PRIMARY_PACKAGE` for it, which the run reports.
//
// This binary is both the harness and the rustc wrapper. With
// `CACHE_PROBE_LOG` set it behaves as the wrapper: it appends the crate name and
// exec's the real rustc. Without it, it builds the fixtures and runs the matrix.
//
//   cargo-cache-probe <scratch-dir>

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::process::Command;

fn wrapper_mode(log: PathBuf) -> ! {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let mut line = String::new();
    for pair in args.windows(2) {
        if pair[0] == "--crate-name" {
            line = format!(
                "crate={} primary={}\n",
                pair[1],
                std::env::var("CARGO_PRIMARY_PACKAGE").unwrap_or_else(|_| "unset".into())
            );
        }
    }
    if !line.is_empty() {
        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&log)
            .expect("probe log is writable");
        file.write_all(line.as_bytes())
            .expect("probe log is writable");
    }
    let status = Command::new(&args[0])
        .args(&args[1..])
        .status()
        .expect("real rustc runs");
    std::process::exit(status.code().unwrap_or(1));
}

fn write(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("fixture directory is creatable");
    }
    std::fs::write(path, contents).expect("fixture is writable");
}

fn fixtures(work: &Path) {
    write(
        &work.join("dep/Cargo.toml"),
        "[package]\nname = \"cc_dep\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    );
    write(
        &work.join("dep/src/lib.rs"),
        "pub fn make(value: &u8) -> impl Fn() -> u8 + '_ {\n    move || *value\n}\n",
    );
    write(
        &work.join("root/Cargo.toml"),
        "[package]\nname = \"cc_root\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n\
         [dependencies]\ncc_dep = { path = \"../dep\" }\n",
    );
    write(
        &work.join("root/src/lib.rs"),
        "pub fn use_it(value: &u8) -> u8 {\n    cc_dep::make(value)()\n}\n",
    );
}

/// One cargo build. Returns how many crates Cargo recompiled and which ones the
/// wrapper saw.
fn build(
    work: &Path,
    cache: &str,
    label: &str,
    wrapper: &Path,
    workspace_wrapper: Option<&Path>,
) -> (usize, BTreeSet<String>) {
    let log = work.join(format!("{label}.log"));
    std::fs::File::create_new(&log).expect("fresh case log");
    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--offline")
        .current_dir(work.join("root"))
        .env("CARGO_TARGET_DIR", work.join(cache))
        .env("RUSTC_WRAPPER", wrapper)
        .env("CACHE_PROBE_LOG", &log);
    if let Some(ww) = workspace_wrapper {
        cmd.env("RUSTC_WORKSPACE_WRAPPER", ww);
    } else {
        cmd.env_remove("RUSTC_WORKSPACE_WRAPPER");
    }
    let out = cmd.output().expect("cargo runs");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(out.status.success(), "{label}: Cargo failed: {stderr}");
    let recompiled = stderr
        .lines()
        .filter(|l| l.trim_start().starts_with("Compiling"))
        .count();
    let seen = std::fs::read_to_string(&log)
        .expect("case log must remain readable")
        .lines()
        .filter_map(|line| {
            let rest = line.strip_prefix("crate=")?;
            let (name, _) = rest.split_once(' ')?;
            name.starts_with("cc_").then(|| name.to_owned())
        })
        .collect();
    (recompiled, seen)
}

fn main() {
    if let Ok(log) = std::env::var("CACHE_PROBE_LOG") {
        wrapper_mode(PathBuf::from(log));
    }
    let work = PathBuf::from(std::env::args().nth(1).expect("scratch directory required"));
    std::fs::create_dir(&work).expect("scratch directory must not already exist");
    fixtures(&work);

    // Two wrapper identities: this binary, at two distinct paths.
    let me = std::env::current_exe().expect("probe path");
    let (a, b) = (work.join("wrap_a"), work.join("wrap_b"));
    std::fs::copy(&me, &a).expect("wrapper a");
    std::fs::copy(&me, &b).expect("wrapper b");

    let mut results: BTreeMap<&str, (usize, BTreeSet<String>)> = BTreeMap::new();
    results.insert("baseline", build(&work, "target", "baseline", &a, None));
    results.insert(
        "same-wrapper",
        build(&work, "target", "same-wrapper", &a, None),
    );
    results.insert(
        "changed-wrapper",
        build(&work, "target", "changed-wrapper", &b, None),
    );
    results.insert(
        "ws-baseline",
        build(&work, "target-ws", "ws-baseline", &a, Some(&a)),
    );
    results.insert(
        "ws-same",
        build(&work, "target-ws", "ws-same", &a, Some(&a)),
    );
    results.insert(
        "ws-changed",
        build(&work, "target-ws", "ws-changed", &a, Some(&b)),
    );

    for (label, (recompiled, seen)) in &results {
        println!("{label:<18} recompiled={recompiled} invoked_for={seen:?}");
    }

    // Assert the exact INVOCATION SET per cell. The `Compiling` count is a
    // display string; the wrapper log is the direct observation, and a cell that
    // silently changed which crates it re-ran would keep the same count while
    // meaning something else.
    let set =
        |names: &[&str]| -> BTreeSet<String> { names.iter().map(|n| (*n).to_owned()).collect() };
    let expected: [(&str, BTreeSet<String>); 6] = [
        ("baseline", set(&["cc_dep", "cc_root"])),
        ("same-wrapper", set(&[])),
        ("changed-wrapper", set(&[])),
        ("ws-baseline", set(&["cc_dep", "cc_root"])),
        ("ws-same", set(&[])),
        ("ws-changed", set(&["cc_root"])),
    ];
    for (label, want) in &expected {
        assert_eq!(
            &results[label].1, want,
            "{label}: unexpected invocation set"
        );
    }
    let changed_wrapper = results["changed-wrapper"].0;
    let ws_changed = &results["ws-changed"];
    println!("RUSTC_WRAPPER_INVALIDATES {}", changed_wrapper > 0);
    println!(
        "RUSTC_WORKSPACE_WRAPPER_INVALIDATES_MEMBER {}",
        ws_changed.1.iter().any(|l| l.contains("cc_root"))
    );
    println!(
        "RUSTC_WORKSPACE_WRAPPER_INVALIDATES_NONMEMBER {}",
        ws_changed.1.iter().any(|l| l.contains("cc_dep"))
    );
}
