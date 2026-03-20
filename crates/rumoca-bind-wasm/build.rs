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
    println!("cargo:rerun-if-env-changed=RUMOCA_WASM_BUNDLED_LIBRARY_MANIFEST");
    println!("cargo:rerun-if-env-changed=RUMOCA_WASM_BUNDLED_LIBRARY_BINARY");

    let git_commit = command_output(&["git", "rev-parse", "--short=12", "HEAD"])
        .unwrap_or_else(|| "unknown".to_string());

    let build_time_utc =
        command_output(&["date", "-u", "+%Y-%m-%dT%H:%M:%SZ"]).unwrap_or_else(|| {
            match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(d) => d.as_secs().to_string(),
                Err(_) => "unknown".to_string(),
            }
        });

    println!("cargo:rustc-env=RUMOCA_GIT_COMMIT={git_commit}");
    println!("cargo:rustc-env=RUMOCA_BUILD_TIME_UTC={build_time_utc}");
    write_bundled_library_assets();
}

fn write_bundled_library_assets() {
    let out_dir = match env::var_os("OUT_DIR") {
        Some(value) => PathBuf::from(value),
        None => return,
    };
    let manifest_out = out_dir.join("bundled_library_manifest.json");
    let binary_out = out_dir.join("bundled_library_cache.bin");

    let manifest_src = env::var_os("RUMOCA_WASM_BUNDLED_LIBRARY_MANIFEST").map(PathBuf::from);
    let binary_src = env::var_os("RUMOCA_WASM_BUNDLED_LIBRARY_BINARY").map(PathBuf::from);

    if let (Some(manifest_src), Some(binary_src)) = (manifest_src, binary_src)
        && manifest_src.is_file()
        && binary_src.is_file()
    {
        let _ = fs::copy(manifest_src, &manifest_out);
        let _ = fs::copy(binary_src, &binary_out);
        return;
    }

    let _ = fs::write(&manifest_out, r#"{"libraries":[]}"#);
    let _ = fs::write(&binary_out, []);
}
