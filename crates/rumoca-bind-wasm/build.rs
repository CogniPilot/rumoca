use std::env;
use std::fs;
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn command_output(args: &[&str]) -> Option<String> {
    let output = Command::new(args[0]).args(&args[1..]).output().ok()?;
    if !output.status.success() {
        return None;
    }
    let text = String::from_utf8(output.stdout).ok()?;
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return None;
    }
    Some(trimmed.to_string())
}

fn main() {
    // Re-run when HEAD changes in source checkouts.
    println!("cargo:rerun-if-changed=../../.git/HEAD");
    println!("cargo:rerun-if-env-changed=SOURCE_DATE_EPOCH");

    let git_commit = command_output(&["git", "rev-parse", "--short=12", "HEAD"])
        .unwrap_or_else(|| "unknown".to_string());

    let build_time_utc =
        command_output(&["date", "-u", "+%Y-%m-%dT%H:%M:%SZ"]).unwrap_or_else(|| {
            match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(d) => d.as_secs().to_string(),
                Err(_) => "unknown".to_string(),
            }
        });

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR set by cargo"));

    // Emit build metadata as a generated Rust source included by lib.rs, rather
    // than RUMOCA_* rustc-env vars read with option_env!.
    let metadata = format!(
        "pub const GIT_COMMIT: &str = {git_commit:?};\n\
         pub const BUILD_TIME_UTC: &str = {build_time_utc:?};\n"
    );
    fs::write(out_dir.join("build_metadata.rs"), metadata).expect("write build_metadata.rs");

    write_bundled_source_root_assets(&out_dir);
}

fn write_bundled_source_root_assets(out_dir: &std::path::Path) {
    // Bundled source-root assets (offline MSL in the WASM build) are not wired up
    // here; write empty placeholders so the `include_*!` sites in lib.rs compile.
    // A future bundling path should pass these via a build-script input, not an
    // environment variable.
    let _ = fs::write(
        out_dir.join("bundled_source_root_manifest.json"),
        r#"{"archives":[]}"#,
    );
    let _ = fs::write(out_dir.join("bundled_source_root_cache.bin"), []);
}
