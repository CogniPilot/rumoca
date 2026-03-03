use anyhow::{Context, Result, ensure};
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;
use std::process::Command;

const DEFAULT_WASM_PACK_VERSION: &str = "0.13.1";

fn parse_tool_version(output: &str) -> Option<String> {
    output
        .split_whitespace()
        .find_map(|token| {
            let cleaned = token.trim_matches(|c: char| {
                !(c.is_ascii_alphanumeric() || c == '.' || c == '-' || c == '_')
            });
            let cleaned = cleaned.strip_prefix('v').unwrap_or(cleaned);
            if cleaned.chars().next().is_some_and(|c| c.is_ascii_digit()) {
                Some(cleaned.to_string())
            } else {
                None
            }
        })
        .filter(|version| !version.is_empty())
}

fn installed_tool_version(root: &Path, program: &str) -> Option<String> {
    let mut cmd = Command::new(program);
    cmd.arg("--version").current_dir(root);
    let output = cmd.output().ok()?;
    if !output.status.success() {
        return None;
    }
    parse_tool_version(&String::from_utf8_lossy(&output.stdout))
}

fn lockfile_wasm_bindgen_versions(root: &Path) -> Result<Vec<String>> {
    let lock_path = root.join("Cargo.lock");
    let lock_text = fs::read_to_string(&lock_path)
        .with_context(|| format!("failed to read {}", lock_path.display()))?;
    let lock_value: toml::Value =
        toml::from_str(&lock_text).context("failed to parse Cargo.lock as TOML")?;
    let packages = lock_value
        .get("package")
        .and_then(toml::Value::as_array)
        .context("missing [package] entries in Cargo.lock")?;

    let mut versions = BTreeSet::new();
    for package in packages {
        let Some(name) = package.get("name").and_then(toml::Value::as_str) else {
            continue;
        };
        if name != "wasm-bindgen" {
            continue;
        }
        if let Some(version) = package.get("version").and_then(toml::Value::as_str) {
            versions.insert(version.to_string());
        }
    }
    ensure!(
        !versions.is_empty(),
        "failed to discover wasm-bindgen version in Cargo.lock"
    );
    Ok(versions.into_iter().collect())
}

fn parse_semver_triplet(version: &str) -> Option<(u64, u64, u64)> {
    let mut parts = version.split('.');
    let major = parts.next()?.parse::<u64>().ok()?;
    let minor = parts.next()?.parse::<u64>().ok()?;
    let patch = parts.next()?.parse::<u64>().ok()?;
    if parts.next().is_some() {
        return None;
    }
    Some((major, minor, patch))
}

fn expected_wasm_bindgen_cli_version(root: &Path) -> Result<String> {
    let versions = lockfile_wasm_bindgen_versions(root)?;
    let mut newest: Option<((u64, u64, u64), String)> = None;
    for version in versions {
        let Some(parsed) = parse_semver_triplet(&version) else {
            continue;
        };
        match &newest {
            Some((best, _)) if *best >= parsed => {}
            _ => newest = Some((parsed, version)),
        }
    }
    newest
        .map(|(_, version)| version)
        .context("failed to pick wasm-bindgen semver version from Cargo.lock")
}

pub(crate) fn ensure_wasm_bindgen_cli(root: &Path) -> Result<String> {
    let expected = expected_wasm_bindgen_cli_version(root)?;
    let installed = installed_tool_version(root, "wasm-bindgen");
    if installed.as_deref() == Some(expected.as_str()) {
        return Ok(expected);
    }

    match installed {
        Some(version) => println!(
            "wasm-bindgen-cli version mismatch (installed {version}, expected {expected}); fixing..."
        ),
        None => println!("wasm-bindgen-cli not found; installing version {expected}..."),
    }
    let mut install = Command::new("cargo");
    install
        .arg("install")
        .arg("wasm-bindgen-cli")
        .arg("--version")
        .arg(&expected)
        .arg("--locked")
        .arg("--force")
        .current_dir(root);
    crate::run_status(install)?;

    let now_installed = installed_tool_version(root, "wasm-bindgen")
        .context("failed to resolve installed wasm-bindgen-cli version after install")?;
    ensure!(
        now_installed == expected,
        "installed wasm-bindgen-cli version mismatch after install: expected {}, got {}",
        expected,
        now_installed
    );
    Ok(expected)
}

fn expected_wasm_pack_version() -> String {
    std::env::var("RUMOCA_WASM_PACK_VERSION")
        .ok()
        .filter(|v| !v.trim().is_empty())
        .unwrap_or_else(|| DEFAULT_WASM_PACK_VERSION.to_string())
}

pub(crate) fn ensure_wasm_pack(root: &Path) -> Result<String> {
    let expected = expected_wasm_pack_version();
    let installed = installed_tool_version(root, "wasm-pack");
    if installed.as_deref() == Some(expected.as_str()) {
        return Ok(expected);
    }

    match installed {
        Some(version) => println!(
            "wasm-pack version mismatch (installed {version}, expected {expected}); fixing..."
        ),
        None => println!("wasm-pack not found; installing version {expected}..."),
    }
    let mut install = Command::new("cargo");
    install
        .arg("install")
        .arg("wasm-pack")
        .arg("--version")
        .arg(&expected)
        .arg("--locked")
        .arg("--force")
        .current_dir(root);
    crate::run_status(install)?;

    let now_installed = installed_tool_version(root, "wasm-pack")
        .context("failed to resolve installed wasm-pack version after install")?;
    ensure!(
        now_installed == expected,
        "installed wasm-pack version mismatch after install: expected {}, got {}",
        expected,
        now_installed
    );
    Ok(expected)
}
