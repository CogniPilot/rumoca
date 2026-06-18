use std::fs;
use std::path::Path;

use super::*;

#[test]
fn visible_workspace_config_filename_is_detected() {
    assert!(is_workspace_config_filename("rumoca-workspace.toml"));
    assert!(!is_workspace_config_filename("RUMOCA-WORKSPACE.TOML"));
    assert!(!is_workspace_config_filename(".rumoca-workspace.toml"));
    assert!(!is_workspace_config_filename("rumoca-scenario.toml"));
}

#[test]
fn parent_and_child_source_roots_append_in_order() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path();
    let child = root.join("examples").join("ball");
    fs::create_dir_all(&child).expect("mkdir child");
    write_workspace_config(root, r#"source_roots = ["vendor/Modelica"]"#);
    write_workspace_config(&child, r#"source_roots = ["../LocalLib"]"#);

    let config = WorkspaceConfig::load(root, &child.join("Ball.mo")).expect("load config");

    assert_eq!(
        normalized_source_roots(config.effective_source_roots_for(&child.join("Ball.mo"))),
        vec![
            normalized_path(root.join("vendor/Modelica")),
            normalized_path(root.join("examples/LocalLib")),
        ],
    );
}

#[test]
fn scoped_source_roots_apply_only_inside_scope() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path();
    let scoped_file = root.join("examples/control/PID.mo");
    let other_file = root.join("examples/other/Ball.mo");
    fs::create_dir_all(scoped_file.parent().unwrap()).expect("mkdir scoped");
    fs::create_dir_all(other_file.parent().unwrap()).expect("mkdir other");
    write_workspace_config(
        root,
        r#"
source_roots = ["vendor/Modelica"]

[source_root_scopes."examples/control"]
source_roots = ["vendor/ControlLib"]
"#,
    );

    let scoped = WorkspaceConfig::load(root, &scoped_file).expect("load scoped");
    let other = WorkspaceConfig::load(root, &other_file).expect("load other");

    assert_eq!(
        normalized_source_roots(scoped.effective_source_roots_for(&scoped_file)),
        vec![
            normalized_path(root.join("vendor/Modelica")),
            normalized_path(root.join("vendor/ControlLib")),
        ],
    );
    assert_eq!(
        normalized_source_roots(other.effective_source_roots_for(&other_file)),
        vec![normalized_path(root.join("vendor/Modelica"))],
    );
}

#[test]
fn replace_source_roots_clears_inherited_roots() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path();
    let child = root.join("examples").join("ball");
    fs::create_dir_all(&child).expect("mkdir child");
    write_workspace_config(root, r#"source_roots = ["vendor/Modelica"]"#);
    write_workspace_config(
        &child,
        r#"
replace_source_roots = true
source_roots = ["local/Only"]
"#,
    );

    let config = WorkspaceConfig::load(root, &child.join("Ball.mo")).expect("load config");

    assert_eq!(
        normalized_source_roots(config.effective_source_roots_for(&child.join("Ball.mo"))),
        vec![normalized_path(child.join("local/Only"))],
    );
}

#[test]
fn duplicate_source_roots_keep_first_resolved_occurrence() {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path();
    let child = root.join("examples");
    fs::create_dir_all(&child).expect("mkdir child");
    write_workspace_config(root, r#"source_roots = ["vendor/Modelica"]"#);
    write_workspace_config(&child, r#"source_roots = ["../vendor/Modelica"]"#);

    let config = WorkspaceConfig::load(root, &child.join("Ball.mo")).expect("load config");

    assert_eq!(
        normalized_source_roots(config.effective_source_roots_for(&child.join("Ball.mo"))),
        vec![normalized_path(root.join("vendor/Modelica"))],
    );
}

#[test]
fn in_memory_workspace_files_cascade_like_filesystem_paths() {
    let config = WorkspaceConfig::load_from_files(
        Path::new(""),
        Path::new("examples/control/PID.mo"),
        vec![
            (
                Path::new("rumoca-workspace.toml").to_path_buf(),
                r#"source_roots = ["vendor/Modelica"]"#.to_string(),
            ),
            (
                Path::new("examples/control/rumoca-workspace.toml").to_path_buf(),
                r#"source_roots = ["../ControlLib"]"#.to_string(),
            ),
            (
                Path::new("examples/other/rumoca-workspace.toml").to_path_buf(),
                r#"source_roots = ["../OtherLib"]"#.to_string(),
            ),
        ],
    )
    .expect("load in-memory config");

    assert_eq!(
        normalized_source_roots(
            config.effective_source_roots_for(Path::new("examples/control/PID.mo")),
        ),
        vec!["vendor/Modelica", "examples/ControlLib"],
    );
}

#[test]
fn in_memory_workspace_files_apply_scoped_roots_for_focus_path() {
    let config = WorkspaceConfig::load_from_files(
        Path::new(""),
        Path::new("examples/control/PID.mo"),
        vec![(
            Path::new("rumoca-workspace.toml").to_path_buf(),
            r#"
source_roots = ["vendor/Modelica"]

[source_root_scopes."examples/control"]
source_roots = ["vendor/ControlLib"]
"#
            .to_string(),
        )],
    )
    .expect("load in-memory scoped config");

    assert_eq!(
        normalized_source_roots(
            config.effective_source_roots_for(Path::new("examples/control/PID.mo")),
        ),
        vec!["vendor/Modelica", "vendor/ControlLib"],
    );
}

fn normalized_source_roots(paths: Vec<String>) -> Vec<String> {
    paths
        .into_iter()
        .map(|path| path.replace('\\', "/"))
        .collect()
}

fn normalized_path(path: impl AsRef<Path>) -> String {
    path.as_ref().display().to_string().replace('\\', "/")
}

fn write_workspace_config(dir: &Path, text: &str) {
    fs::write(dir.join(WORKSPACE_CONFIG_FILE_NAME), text).expect("write workspace config");
}
