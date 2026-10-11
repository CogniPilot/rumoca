//! Disposable caller-owned projects for the guide's literal placeholder paths.

use std::path::Path;
use std::process::Command;

use super::{Entry, repository_root};

const MODEL: &str = "model Model\n  Real x(start=1, fixed=true);\n  Real v(start=0, fixed=true);\nequation\n  der(x)=v;\n  der(v)=-x;\nend Model;\n";

fn write(root: &Path, path: &str, source: &str) {
    let path = root.join(path);
    std::fs::create_dir_all(path.parent().expect("fixture parent")).unwrap();
    std::fs::write(path, source).unwrap();
}

fn fixtures(root: &Path) {
    write(root, "Model.mo", MODEL);
    write(root, "MyModel.mo", &MODEL.replace("Model", "MyModel"));
    write(root, "src/Model.mo", MODEL);
    for name in ["my.jinja", "my_template.jinja"] {
        write(root, name, "caller-owned flat template: {{ model_name }}\n");
    }
    let source = repository_root().join("examples/codegen/checked_dae_report");
    for name in ["target.toml", "checked_dae_report.txt.jinja"] {
        write(
            root,
            &format!("path/to/my_target/{name}"),
            &std::fs::read_to_string(source.join(name)).unwrap(),
        );
    }
}

pub(super) fn selected(entry: &Entry) -> bool {
    matches!(
        entry.page.as_str(),
        "codegen/custom-targets.md"
            | "simulation/inspect.md"
            | "language/support-status.md"
            | "tools/fmt-lint.md"
    ) || (entry.page == "tools/cli.md"
        && entry.argv.iter().all(|argv| {
            matches!(
                argv.first().map(String::as_str),
                Some("compile" | "cache" | "completions")
            )
        }))
        || (entry.page == "codegen/targets.md" && entry.argv == vec![vec!["targets".to_owned()]])
}

pub(super) fn execute(entries: &[Entry]) {
    let mut executed = 0;
    for entry in entries.iter().filter(|entry| selected(entry)) {
        assert!(
            !entry.argv.is_empty(),
            "teaching context contains no CLI command: {}:{}",
            entry.page,
            entry.line
        );
        let dir = tempfile::tempdir().expect("disposable teaching project");
        fixtures(dir.path());
        for argv in &entry.argv {
            let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
                .args(argv)
                .arg("--cache-dir")
                .arg(dir.path().join("cache"))
                .current_dir(dir.path())
                .output()
                .expect("execute the original teaching-context argv");
            assert!(
                output.status.success(),
                "{}:{} {argv:?}\n{}\n{}",
                entry.page,
                entry.line,
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr)
            );
            executed += 1;
        }
        verify_artifacts(entry, dir.path());
    }
    assert_eq!(
        executed, 30,
        "review teaching-context execution roster changes"
    );
    eprintln!("{executed} actual CLI teaching-project commands executed");
}

pub(super) fn first_model() {
    let output = super::python()
        .arg(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/suite_examples_smoke/guide_cli_contexts/first_model.py"),
        )
        .arg(repository_root())
        .arg(env!("CARGO_BIN_EXE_rumoca"))
        .output()
        .expect("execute both authored first-model commands");
    assert!(
        output.status.success(),
        "{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    eprintln!("{}", String::from_utf8_lossy(&output.stdout));
}

fn verify_artifacts(entry: &Entry, root: &Path) {
    let has_argument = |argument: &str| entry.argv.iter().flatten().any(|value| value == argument);
    if entry.page == "codegen/custom-targets.md" && has_argument("my_template.jinja") {
        assert_eq!(
            std::fs::read_to_string(root.join("out.txt")).unwrap(),
            "caller-owned flat template: Model\n"
        );
    }
    if entry.page == "codegen/custom-targets.md" && has_argument("path/to/my_target") {
        let report = std::fs::read_to_string(root.join("out/Model_checked_dae.txt")).unwrap();
        assert!(
            report.contains("model Model")
                && report.contains("variables 2")
                && report.contains("checked-dae-report"),
            "{report}"
        );
    }
    if entry.page == "simulation/inspect.md" && has_argument("m.json") {
        let json: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(root.join("m.json")).unwrap()).unwrap();
        assert!(json.get("schema_version").is_some(), "{json}");
    }
    if entry.page == "tools/cli.md" && has_argument("out.json") {
        let json: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(root.join("out.json")).unwrap()).unwrap();
        assert!(json.is_object(), "{json}");
        assert!(
            std::fs::read_dir(root.join("out"))
                .unwrap()
                .next()
                .is_some()
        );
    }
}
