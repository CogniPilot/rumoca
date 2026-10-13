//! Capture the fixed guest source only for its native tool build owner.

use std::{
    env, fs,
    path::{Path, PathBuf},
    process::Command,
};

/// One staged file: the path it takes inside the fixed guest tree and the file
/// whose bytes are embedded for it.
struct Staged {
    relative: PathBuf,
    source: PathBuf,
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    if env::var_os("CARGO_FEATURE_FIXED_NATIVE_GUEST_BUILD").is_none()
        || env::var("CARGO_CFG_TARGET_ARCH").as_deref() == Ok("wasm32")
    {
        return;
    }
    let manifest = env::var_os("CARGO_MANIFEST_DIR").expect("Cargo manifest directory");
    let root = Path::new(&manifest).parent().unwrap().parent().unwrap();
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut files = ["Cargo.toml", "Cargo.lock", "rust-toolchain.toml"]
        .into_iter()
        .map(|name| verbatim(root, &root.join(name)))
        .collect::<Vec<_>>();
    let crates = root.join("crates");
    for entry in fs::read_dir(&crates).expect("canonical workspace crates") {
        let package = entry.expect("workspace entry").path();
        if !package.join("Cargo.toml").is_file() {
            continue;
        }
        files.push(library_only_manifest(root, &out_dir, &package));
        // A build script may split into sibling modules, so every crate-root
        // Rust file travels with it.
        for entry in fs::read_dir(&package).expect("canonical package directory") {
            let path = entry.expect("canonical package entry").path();
            if path.is_file() && path.extension().is_some_and(|extension| extension == "rs") {
                files.push(verbatim(root, &path));
            }
        }
        capture_tree(root, &package.join("src"), &mut files);
    }
    // The root's existing patch must remain resolvable even when the fixed guest
    // does not select that package. It is not substituted or upgraded here.
    capture_tree(root, &root.join("vendor/diffsol"), &mut files);
    files.sort_by(|a, b| a.relative.cmp(&b.relative));
    files.dedup_by(|a, b| a.relative == b.relative);
    let mut generated = String::from("static FIXED_SOURCES: &[(&str, &[u8])] = &[\n");
    for staged in files {
        generated.push_str(&format!(
            "({:?}, include_bytes!({:?})),\n",
            staged.relative.to_str().unwrap(),
            staged.source.to_str().unwrap()
        ));
    }
    generated.push_str("] ;\n");
    let rustc = env::var_os("RUSTC").expect("issuing Rust compiler");
    let version = Command::new(&rustc)
        .arg("-vV")
        .output()
        .expect("compiler identity");
    assert!(version.status.success(), "compiler identity failed");
    let version = String::from_utf8(version.stdout).expect("compiler identity text");
    generated.push_str(&format!(
        "static ISSUING_RUSTC: &str = {:?};\n",
        rustc.to_str().unwrap()
    ));
    generated.push_str(&format!("static ISSUING_VERSION: &str = {version:?};\n"));
    fs::write(out_dir.join("fixed_guest_sources.rs"), generated)
        .expect("retained fixed source inventory");
}

/// Stage one source file unchanged.
fn verbatim(root: &Path, path: &Path) -> Staged {
    println!("cargo:rerun-if-changed={}", path.display());
    Staged {
        relative: path.strip_prefix(root).unwrap().to_owned(),
        source: path.to_owned(),
    }
}

/// Stage one crate manifest without its example, test and bench targets.
///
/// The fixed guest builds libraries only and stages only crate-root files and
/// `src`, so a declared target whose source is absent would stop cargo from
/// reading the manifest at all. Discovery of those targets is switched off for
/// the same reason.
fn library_only_manifest(root: &Path, out_dir: &Path, package: &Path) -> Staged {
    let manifest = package.join("Cargo.toml");
    println!("cargo:rerun-if-changed={}", manifest.display());
    let text = fs::read_to_string(&manifest).expect("canonical crate manifest");
    let relative = manifest.strip_prefix(root).unwrap().to_owned();
    let source = out_dir.join("fixed_manifests").join(&relative);
    fs::create_dir_all(source.parent().unwrap()).expect("staged manifest directory");
    fs::write(&source, strip_non_library_targets(&text)).expect("staged manifest");
    Staged { relative, source }
}

fn strip_non_library_targets(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut skipping = false;
    for line in text.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with('[') {
            skipping = matches!(trimmed, "[[example]]" | "[[test]]" | "[[bench]]");
        }
        if skipping {
            continue;
        }
        out.push_str(line);
        out.push('\n');
        if trimmed == "[package]" {
            disable_target_discovery(text, &mut out);
        }
    }
    out
}

/// Switch off example, test and bench discovery unless the manifest already
/// states a value for it.
fn disable_target_discovery(text: &str, out: &mut String) {
    for key in ["autoexamples", "autotests", "autobenches"] {
        if text.lines().any(|line| line.trim_start().starts_with(key)) {
            continue;
        }
        out.push_str(key);
        out.push_str(" = false\n");
    }
}

fn capture_tree(root: &Path, directory: &Path, files: &mut Vec<Staged>) {
    if !directory.is_dir() {
        return;
    }
    println!("cargo:rerun-if-changed={}", directory.display());
    for entry in fs::read_dir(directory).expect("canonical source directory") {
        let path = entry.expect("canonical source entry").path();
        assert!(
            !path.is_symlink(),
            "fixed source inventory cannot follow symlinks"
        );
        if path.is_dir() {
            if path.file_name().unwrap() != "target" && path.file_name().unwrap() != ".git" {
                capture_tree(root, &path, files);
            }
        } else if path.is_file() {
            println!("cargo:rerun-if-changed={}", path.display());
            files.push(Staged {
                relative: path.strip_prefix(root).unwrap().to_owned(),
                source: path,
            });
        }
    }
}
