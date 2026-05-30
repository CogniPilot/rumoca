//! FMU packaging: compile generated C into a shared library and zip it (plus
//! `modelDescription.xml` / `sources/`) into a `.fmu`. Split out of `main.rs` to
//! keep it under the SPEC_0021 file-size limit.

use std::path::Path;

use anyhow::{Result, bail};

/// Compile the generated C source into a shared library and package as .fmu.
pub(crate) fn build_fmu(
    out_dir: &Path,
    model_identifier: &str,
    target_name: Option<&str>,
) -> Result<()> {
    use std::process::Command;

    let (platform, lib_ext) = fmu_binary_platform(target_name)?;

    // Compile shared library
    let bin_dir = out_dir.join("binaries").join(platform);
    std::fs::create_dir_all(&bin_dir)?;

    let c_path = out_dir
        .join("sources")
        .join(format!("{model_identifier}.c"));
    let lib_path = bin_dir.join(format!("{model_identifier}.{lib_ext}"));

    eprintln!("  compiling {}", c_path.display());
    let status = Command::new("cc")
        .args(["-shared", "-fPIC", "-O2", "-o"])
        .arg(&lib_path)
        .arg(&c_path)
        .arg("-lm")
        .status()?;

    if !status.success() {
        bail!(
            "C compiler failed with exit code {}",
            status.code().unwrap_or(-1)
        );
    }
    eprintln!("  wrote {}", lib_path.display());

    // Package as .fmu (ZIP archive)
    let fmu_path = out_dir.join(format!("{model_identifier}.fmu"));
    create_fmu_zip(out_dir, &fmu_path)?;
    eprintln!("\nCreated {}", fmu_path.display());

    Ok(())
}

pub(crate) fn fmu_binary_platform(
    target_name: Option<&str>,
) -> Result<(&'static str, &'static str)> {
    let is_fmi3 = target_name == Some("fmi3");
    if cfg!(target_os = "linux") {
        return Ok((if is_fmi3 { "x86_64-linux" } else { "linux64" }, "so"));
    }
    if cfg!(target_os = "macos") {
        if is_fmi3 {
            return Ok((
                if cfg!(target_arch = "aarch64") {
                    "aarch64-darwin"
                } else {
                    "x86_64-darwin"
                },
                "dylib",
            ));
        }
        return Ok(("darwin64", "dylib"));
    }
    if cfg!(target_os = "windows") {
        return Ok((if is_fmi3 { "x86_64-windows" } else { "win64" }, "dll"));
    }
    bail!("Unsupported platform for FMU packaging")
}

/// Create the .fmu ZIP archive containing modelDescription.xml, binaries/, and sources/.
fn create_fmu_zip(out_dir: &Path, fmu_path: &Path) -> Result<()> {
    use std::io::{Read as _, Write as _};
    use zip::ZipWriter;
    use zip::write::SimpleFileOptions;

    let file = std::fs::File::create(fmu_path)?;
    let mut zip = ZipWriter::new(file);
    let options = SimpleFileOptions::default().compression_method(zip::CompressionMethod::Deflated);

    // Walk the output directory and add relevant files
    for entry in walkdir::WalkDir::new(out_dir) {
        let entry = entry?;
        let path = entry.path();

        // Skip the .fmu file itself and build.sh
        if path == fmu_path || path == out_dir.join("build.sh") {
            continue;
        }

        let rel_path = path.strip_prefix(out_dir)?;
        let rel_str = rel_path.to_string_lossy();

        if rel_str.is_empty() {
            continue;
        }

        if entry.file_type().is_dir() {
            zip.add_directory(format!("{rel_str}/"), options)?;
        } else {
            zip.start_file(rel_str.to_string(), options)?;
            let mut f = std::fs::File::open(path)?;
            let mut buf = Vec::new();
            f.read_to_end(&mut buf)?;
            zip.write_all(&buf)?;
        }
    }

    zip.finish()?;
    Ok(())
}
