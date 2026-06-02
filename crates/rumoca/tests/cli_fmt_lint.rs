use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use tempfile::tempdir;

fn write_model(path: &Path, source: &str) {
    fs::write(path, source).expect("write model file");
}

fn copy_dir_recursive(src: &Path, dst: &Path) {
    fs::create_dir_all(dst).expect("create destination directory");
    for entry in fs::read_dir(src).expect("read source directory") {
        let entry = entry.expect("read directory entry");
        let src_path = entry.path();
        let dst_path = dst.join(entry.file_name());
        if src_path.is_dir() {
            copy_dir_recursive(&src_path, &dst_path);
        } else {
            fs::copy(&src_path, &dst_path).expect("copy file");
        }
    }
}

fn assert_dir_tree_bytes_equal(expected_root: &Path, actual_root: &Path) {
    for entry in fs::read_dir(expected_root).expect("read expected directory") {
        let entry = entry.expect("read expected directory entry");
        let expected_path = entry.path();
        let actual_path = actual_root.join(entry.file_name());
        if expected_path.is_dir() {
            assert!(
                actual_path.is_dir(),
                "missing formatted directory {}",
                actual_path.display()
            );
            assert_dir_tree_bytes_equal(&expected_path, &actual_path);
        } else {
            assert!(
                actual_path.is_file(),
                "missing formatted file {}",
                actual_path.display()
            );
            assert_eq!(
                fs::read(&actual_path).expect("read formatted file"),
                fs::read(&expected_path).expect("read expected file"),
                "formatted MSL file drifted: {}",
                expected_path.display()
            );
        }
    }

    for entry in fs::read_dir(actual_root).expect("read actual directory") {
        let entry = entry.expect("read actual directory entry");
        let actual_path = entry.path();
        let expected_path = expected_root.join(entry.file_name());
        assert!(
            expected_path.exists(),
            "formatter created unexpected MSL path {}",
            actual_path.display()
        );
    }
}

fn cached_msl_root() -> Option<PathBuf> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let roots = std::env::current_dir()
        .ok()
        .into_iter()
        .chain(manifest_dir.ancestors().map(Path::to_path_buf));
    for root in roots {
        let candidate = root
            .join("target")
            .join("msl")
            .join("ModelicaStandardLibrary-4.1.0");
        if candidate.is_dir() {
            return Some(candidate);
        }
    }
    None
}

#[test]
fn fmt_check_reports_compact_equation_assignment_with_normalization_enabled() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("dymola_style.mo");
    let source =
        "model DymolaStyle\n  Real x(start=1);  \nequation\n  der(x)= -9.8;\nend DymolaStyle;";
    write_model(&file, source);

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--check")
        .arg("--normalize-equation-spacing=true")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --check");

    assert!(!output.status.success(), "fmt --check should report drift");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stdout.contains("Would reformat"));
    assert!(stderr.contains("1 need formatting"));
}

#[test]
fn fmt_check_succeeds_for_already_formatted_file() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("formatted.mo");
    write_model(
        &file,
        "model Formatted\n  Real x(start = 1);\nend Formatted;\n",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--check")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --check");

    assert!(
        output.status.success(),
        "fmt --check should succeed for already formatted file"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("files checked"));
    assert!(stderr.contains("0 need formatting"));
}

#[test]
fn fmt_check_fails_on_syntax_error() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("bad.mo");
    write_model(&file, "model Bad\n  Real x =\nend Bad;\n");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--check")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --check");

    assert!(
        !output.status.success(),
        "fmt --check should fail on syntax errors"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("Error formatting"));
    assert!(stderr.contains("1 errors"));
}

#[test]
fn fmt_respects_dymola_profile_config_without_rewriting_source() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("dymola_style.mo");
    let source = "model DymolaStyle\r\n  Real x(start=1);  \r\n  Real y;\r\n\r\nequation\r\n  der(x)=y;\r\n  y=-9.8;\r\nend DymolaStyle;";
    write_model(&file, source);
    fs::write(
        dir.path().join(".rumoca_fmt.toml"),
        concat!(
            "profile = \"dymola\"\n",
            "normalize_equation_spacing = false\n",
            "trim_trailing_whitespace = false\n",
            "insert_final_newline = false\n",
            "line_ending = \"auto\"\n",
        ),
    )
    .expect("write fmt config");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg(&file)
        .output()
        .expect("run rumoca fmt");

    assert!(
        output.status.success(),
        "Dymola profile config should preserve source bytes"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(!stdout.contains("Formatted"));
    assert!(stderr.contains("1 unchanged"));
    assert_eq!(
        fs::read(&file).expect("read formatted file"),
        source.as_bytes()
    );
}

#[test]
fn fmt_dymola_equation_spacing_option_preserves_modifiers() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("rewrite.mo");
    write_model(
        &file,
        "model Rewrite\n  Real x(start=1);  \nequation\n  x=1;\nend Rewrite;",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--normalize-equation-spacing=true")
        .arg(&file)
        .output()
        .expect("run rumoca fmt");

    assert!(output.status.success(), "fmt should succeed");
    let formatted = fs::read_to_string(&file).expect("read formatted");
    assert_eq!(
        formatted,
        "model Rewrite\n  Real x(start=1);\nequation\n  x = 1;\nend Rewrite;\n"
    );
}

#[test]
fn fmt_canonical_profile_normalizes_spacing_and_indentation() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("canonical.mo");
    write_model(
        &file,
        "model Canonical\nReal x;\nequation\nx=1;\nend Canonical;",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--profile")
        .arg("canonical")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --profile canonical");

    assert!(output.status.success(), "fmt should succeed");
    let formatted = fs::read_to_string(&file).expect("read formatted");
    assert_eq!(
        formatted,
        "model Canonical\n  Real x;\nequation\n  x = 1;\nend Canonical;\n"
    );
}

#[test]
fn fmt_canonical_profile_config_can_override_individual_knobs() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("canonical_override.mo");
    write_model(
        &file,
        "model CanonicalOverride\nReal x(start = 1);\nequation\nx=1+2;\nend CanonicalOverride;",
    );
    fs::write(
        dir.path().join(".rumoca_fmt.toml"),
        concat!(
            "profile = \"canonical\"\n",
            "normalize_equation_spacing = false\n",
            "normalize_operator_spacing = false\n",
            "normalize_argument_assignment_spacing = false\n",
        ),
    )
    .expect("write fmt config");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg(&file)
        .output()
        .expect("run rumoca fmt");

    assert!(output.status.success(), "fmt should succeed");
    let formatted = fs::read_to_string(&file).expect("read formatted");
    assert_eq!(
        formatted,
        "model CanonicalOverride\n  Real x(start = 1);\nequation\n  x=1+2;\nend CanonicalOverride;\n"
    );
}

#[test]
fn fmt_operator_spacing_can_be_enabled_by_cli() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("operators.mo");
    write_model(
        &file,
        "model Operators\n  Real x;\nequation\n  x=1+2*3;\nend Operators;",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--normalize-operator-spacing")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --normalize-operator-spacing");

    assert!(output.status.success(), "fmt should succeed");
    let formatted = fs::read_to_string(&file).expect("read formatted");
    assert_eq!(
        formatted,
        "model Operators\n  Real x;\nequation\n  x=1 + 2 * 3;\nend Operators;\n"
    );
}

#[test]
fn fmt_coverage_reports_canonical_classified_gap_percentage() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("coverage.mo");
    write_model(
        &file,
        "model Coverage\nReal x;\nequation\nx=1+2*3;\nconnect(a,b);\nend Coverage;",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--coverage")
        .arg("--profile")
        .arg("canonical")
        .arg(&file)
        .output()
        .expect("run rumoca fmt --coverage --profile canonical");

    assert!(output.status.success(), "fmt coverage should succeed");
    assert!(
        String::from_utf8_lossy(&output.stdout).is_empty(),
        "coverage mode should not emit changed-file stdout"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("coverage"));
    assert!(stderr.contains("100.00% coverage"));
    assert!(stderr.contains("binary-operator"));
    assert!(stderr.contains("connect-comma"));
    assert_eq!(
        fs::read_to_string(&file).expect("read source"),
        "model Coverage\nReal x;\nequation\nx=1+2*3;\nconnect(a,b);\nend Coverage;"
    );
}

#[test]
fn fmt_default_trims_trailing_whitespace_outside_literals() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("trailing.mo");
    write_model(
        &file,
        "model Trailing\n  Real x;  \n  annotation(Documentation(info=\"<html>\n<p>keep this literal space </p> \n</html>\"));\nend Trailing;",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg(&file)
        .output()
        .expect("run rumoca fmt");

    assert!(output.status.success(), "fmt should succeed");
    let formatted = fs::read_to_string(&file).expect("read formatted");
    assert_eq!(
        formatted,
        "model Trailing\n  Real x;\n  annotation(Documentation(info=\"<html>\n<p>keep this literal space </p> \n</html>\"));\nend Trailing;\n"
    );
}

#[test]
fn fmt_repair_missing_indentation_can_be_disabled_by_cli() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("missing_indent.mo");
    let source = "model MissingIndent\nReal x;\nequation\nx=1;\nend MissingIndent;";
    write_model(&file, source);

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--repair-missing-indentation=false")
        .arg(&file)
        .output()
        .expect("run rumoca fmt");

    assert!(output.status.success(), "fmt should succeed");
    assert_eq!(
        fs::read_to_string(&file).expect("read formatted"),
        "model MissingIndent\nReal x;\nequation\nx=1;\nend MissingIndent;\n"
    );
}

#[test]
fn fmt_common_cli_options_control_rewrites() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("options.mo");
    let source = "model Options\r\nReal x(start=1);  \r\nequation\r\nx=1;\r\nend Options;";
    write_model(&file, source);

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--normalize-indentation")
        .arg("--use-tabs")
        .arg("--normalize-equation-spacing=false")
        .arg("--trim-trailing-whitespace=false")
        .arg("--insert-final-newline=false")
        .arg("--line-ending")
        .arg("crlf")
        .arg(&file)
        .output()
        .expect("run rumoca fmt");

    assert!(output.status.success(), "fmt should succeed");
    let formatted = fs::read_to_string(&file).expect("read formatted");
    assert_eq!(
        formatted,
        "model Options\r\n\tReal x(start=1);  \r\nequation\r\n\tx=1;\r\nend Options;"
    );
}

#[test]
fn fmt_msl_copy_has_no_drift_and_bad_file_is_rewritten() {
    let Some(msl_root) = cached_msl_root() else {
        let message = "cached MSL not found under target/msl; skipping MSL formatter drift test";
        if std::env::var_os("REQUIRE_MSL_FMT_DRIFT").is_some() {
            panic!("{message}");
        }
        eprintln!("{message}");
        return;
    };
    let dir = tempdir().expect("tempdir");
    let copy_root = dir.path().join("msl-copy");
    copy_dir_recursive(&msl_root, &copy_root);

    let msl_fmt_output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg(&copy_root)
        .output()
        .expect("run rumoca fmt on MSL copy");
    assert!(
        msl_fmt_output.status.success(),
        "MSL copy formatting should succeed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&msl_fmt_output.stdout),
        String::from_utf8_lossy(&msl_fmt_output.stderr)
    );
    assert_dir_tree_bytes_equal(&msl_root, &copy_root);

    let canonical_coverage_output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--coverage")
        .arg("--profile")
        .arg("canonical")
        .arg(&copy_root)
        .output()
        .expect("measure canonical formatter coverage on MSL copy");
    assert!(
        canonical_coverage_output.status.success(),
        "canonical MSL formatter coverage should succeed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&canonical_coverage_output.stdout),
        String::from_utf8_lossy(&canonical_coverage_output.stderr)
    );
    let canonical_coverage = String::from_utf8_lossy(&canonical_coverage_output.stderr);
    let summary_line = canonical_coverage
        .lines()
        .find(|line| line.contains("files measured"))
        .expect("canonical coverage should print a summary line");
    assert!(
        summary_line.contains("100.00% coverage"),
        "canonical MSL formatter coverage should be complete, got: {summary_line}"
    );

    let missing_indent_file = copy_root.join("MissingDymolaIndent.mo");
    write_model(
        &missing_indent_file,
        "model MissingDymolaIndent\nReal x(start=1);\nequation\nx=1;\nend MissingDymolaIndent;",
    );
    let default_fmt_output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg(&missing_indent_file)
        .output()
        .expect("run rumoca fmt on missing-indent Dymola file");
    assert!(
        default_fmt_output.status.success(),
        "missing-indent file formatting should succeed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&default_fmt_output.stdout),
        String::from_utf8_lossy(&default_fmt_output.stderr)
    );
    assert_eq!(
        fs::read_to_string(&missing_indent_file)
            .expect("read missing-indent file after formatting"),
        "model MissingDymolaIndent\n  Real x(start=1);\nequation\nx=1;\nend MissingDymolaIndent;\n"
    );

    let bad_file = copy_root.join("BadDymolaFormat.mo");
    write_model(
        &bad_file,
        "model BadDymolaFormat\nReal x(start=1);\nequation\nx=1;\nend BadDymolaFormat;",
    );
    let fmt_output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg("--normalize-indentation")
        .arg("--normalize-equation-spacing")
        .arg(&bad_file)
        .output()
        .expect("run rumoca fmt on bad Dymola file");
    assert!(
        fmt_output.status.success(),
        "bad file formatting should succeed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&fmt_output.stdout),
        String::from_utf8_lossy(&fmt_output.stderr)
    );
    assert_eq!(
        fs::read_to_string(&bad_file).expect("read bad file after formatting"),
        "model BadDymolaFormat\n  Real x(start=1);\nequation\n  x = 1;\nend BadDymolaFormat;\n"
    );
}

#[test]
fn fmt_loads_config_from_each_file_directory() {
    let dir = tempdir().expect("tempdir");
    let dymola_dir = dir.path().join("dymola");
    let default_dir = dir.path().join("default");
    fs::create_dir_all(&dymola_dir).expect("create dymola dir");
    fs::create_dir_all(&default_dir).expect("create default dir");

    let dymola_file = dymola_dir.join("dymola.mo");
    let default_file = default_dir.join("default.mo");
    let compact_source = "model Compact\n  Real x(start=1);\nend Compact;\n";
    write_model(&dymola_file, compact_source);
    write_model(&default_file, compact_source);
    fs::write(
        dymola_dir.join(".rumoca_fmt.toml"),
        "profile = \"dymola\"\n",
    )
    .expect("write dymola fmt config");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("fmt")
        .arg(dir.path())
        .output()
        .expect("run rumoca fmt");

    assert!(output.status.success(), "fmt should succeed");
    assert_eq!(
        fs::read(&dymola_file).expect("read dymola file"),
        compact_source.as_bytes(),
        "Dymola config should apply to files under that directory"
    );
    assert_eq!(
        fs::read(&default_file).expect("read default file"),
        compact_source.as_bytes(),
        "default Dymola profile should keep declaration modifier spacing"
    );
}

#[test]
fn lint_fails_on_syntax_error() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("bad.mo");
    write_model(&file, "model Bad\n  Real x\nend Bad;\n");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("lint")
        .arg(&file)
        .output()
        .expect("run rumoca lint");

    assert!(!output.status.success(), "lint should fail on syntax error");
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stdout.contains("syntax-error"));
    assert!(stdout.contains("[error]"));
    assert!(stderr.contains("errors="));
}

#[test]
fn lint_warnings_as_errors_fails_on_warning() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("warn.mo");
    write_model(&file, "model badModel\n  Real x;\nend badModel;\n");

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("lint")
        .arg("--warnings-as-errors")
        .arg(&file)
        .output()
        .expect("run rumoca lint --warnings-as-errors");

    assert!(
        !output.status.success(),
        "lint --warnings-as-errors should fail when warnings are emitted"
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("[warning]"));
}

#[test]
fn lint_succeeds_for_clean_model() {
    let dir = tempdir().expect("tempdir");
    let file = dir.path().join("clean.mo");
    write_model(
        &file,
        "model CleanModel\n  Real x;\nequation\n  der(x) = x;\nend CleanModel;\n",
    );

    let output = Command::new(env!("CARGO_BIN_EXE_rumoca"))
        .arg("lint")
        .arg(&file)
        .output()
        .expect("run rumoca lint");

    assert!(
        output.status.success(),
        "lint should succeed for clean model"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("errors=0"));
}
