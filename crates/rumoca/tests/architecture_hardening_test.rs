use std::fs;
use std::path::{Path, PathBuf};

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("workspace root")
        .to_path_buf()
}

fn collect_rs_files(dir: &Path, out: &mut Vec<PathBuf>) {
    let entries = fs::read_dir(dir).unwrap_or_else(|error| {
        panic!("failed to read {}: {error}", dir.display());
    });
    for entry in entries {
        let entry = entry.expect("directory entry");
        let path = entry.path();
        if path.is_dir() {
            collect_rs_files(&path, out);
            continue;
        }
        if path.extension().is_some_and(|ext| ext == "rs") {
            out.push(path);
        }
    }
}

fn has_direct_ir_symbol_import(line: &str) -> bool {
    [
        "use rumoca_ir_ast::{",
        "use rumoca_ir_flat::{",
        "use rumoca_ir_dae::{",
    ]
    .iter()
    .any(|needle| line.contains(needle))
}

fn collect_direct_import_offenders(path: &Path) -> Vec<String> {
    let Ok(content) = fs::read_to_string(path) else {
        return Vec::new();
    };

    content
        .lines()
        .enumerate()
        .filter(|(_, line)| has_direct_ir_symbol_import(line))
        .map(|(line_idx, _)| format!("{}:{}", path.display(), line_idx + 1))
        .collect()
}

#[test]
fn test_no_tail_rs_files_in_crates() {
    let root = workspace_root().join("crates");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let offenders: Vec<String> = rs_files
        .iter()
        .filter_map(|path| {
            let stem = path.file_stem()?.to_string_lossy();
            (stem.contains("_tail")).then(|| path.display().to_string())
        })
        .collect();

    assert!(
        offenders.is_empty(),
        "found banned *_tail*.rs files: {offenders:?}"
    );
}

#[test]
fn test_no_manual_msl_ignore_markers() {
    let root = workspace_root().join("crates").join("rumoca").join("tests");
    let mut rs_files = Vec::new();
    collect_rs_files(&root, &mut rs_files);

    let mut offenders = Vec::new();
    for path in rs_files {
        if path
            .file_name()
            .is_some_and(|name| name == "architecture_hardening_test.rs")
        {
            continue;
        }
        let Ok(content) = fs::read_to_string(&path) else {
            continue;
        };
        for (line_idx, line) in content.lines().enumerate() {
            if line.contains("#[ignore") && line.contains("manual-msl-") {
                offenders.push(format!("{}:{}", path.display(), line_idx + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "found legacy manual MSL ignore markers: {offenders:?}"
    );
}

#[test]
fn test_sim_sources_use_ir_namespace_aliases() {
    let root = workspace_root();
    let sim_dirs = [
        root.join("crates/rumoca-sim-core/src"),
        root.join("crates/rumoca-sim-diffsol/src"),
    ];

    let mut offenders = Vec::new();
    for dir in sim_dirs {
        let mut rs_files = Vec::new();
        collect_rs_files(&dir, &mut rs_files);
        for path in rs_files {
            offenders.extend(collect_direct_import_offenders(&path));
        }
    }

    assert!(
        offenders.is_empty(),
        "found direct IR symbol imports in sim sources; use namespace aliases instead: {offenders:?}"
    );
}

#[test]
fn test_sim_diffsol_dag_boundary_no_flat_or_ast_dependency() {
    let cargo_toml = workspace_root().join("crates/rumoca-sim-diffsol/Cargo.toml");
    let content = fs::read_to_string(&cargo_toml).expect("read sim-diffsol Cargo.toml");

    assert!(
        !content.contains("rumoca-ir-flat"),
        "sim-diffsol must not depend on rumoca-ir-flat"
    );
    assert!(
        !content.contains("rumoca-ir-ast"),
        "sim-diffsol must not depend on rumoca-ir-ast"
    );
}
