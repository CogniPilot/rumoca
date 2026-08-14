use super::*;

#[test]
fn test_generated_parser_contract_is_pinned_and_documented() {
    let root = workspace_root();
    let workspace_toml =
        fs::read_to_string(root.join("Cargo.toml")).expect("read workspace Cargo.toml");

    assert!(
        section_dependency_line(&workspace_toml, "workspace.dependencies", "parol")
            .is_some_and(|line| line == r#"parol = "=4.2.2""#),
        "workspace parol dependency must be pinned exactly so generated parser output is reproducible"
    );
    assert!(
        section_dependency_line(&workspace_toml, "workspace.dependencies", "parol_runtime")
            .is_some_and(|line| line == r#"parol_runtime = "=4.2.0""#),
        "workspace parol_runtime dependency must be pinned exactly with the generated parser"
    );

    let build_rs =
        fs::read_to_string(root.join("crates/rumoca-phase-parse/build.rs")).expect("read build.rs");
    for required in [
        r#"env::var_os("OUT_DIR")"#,
        r#".join("modelica-generated")"#,
        r#"Builder::with_explicit_output_dir(&staging_dir)"#,
        r#".grammar_file(par_file)"#,
        r#".parser_output_file("modelica_parser.rs")"#,
        r#".actions_output_file("modelica_grammar_trait.rs")"#,
        r#"install_generated_files(&staging_dir);"#,
        r#"let checked_in_dir = Path::new("src/generated");"#,
        r#"install_generated_if_changed(&staged, &checked_in)"#,
        r#"println!("cargo:rerun-if-changed=src/modelica.par");"#,
        r#"println!("cargo:rerun-if-changed=build_support.rs");"#,
    ] {
        assert!(
            build_rs.contains(required),
            "parser build script must keep the generated parser contract stable; missing `{required}`"
        );
    }
    assert!(
        !build_rs.contains(r#"Builder::with_explicit_output_dir("src/generated")"#),
        "Modelica parser generation must stage under Cargo OUT_DIR instead of writing directly to the source tree"
    );

    let build_support = fs::read_to_string(root.join("crates/rumoca-phase-parse/build_support.rs"))
        .expect("read parser build support");
    for required in [
        "pub(crate) fn install_generated_if_changed",
        "Ok(existing) if existing == generated => return Ok(false)",
        "fs::write(checked_in, generated)?",
    ] {
        assert!(
            build_support.contains(required),
            "parser build support must preserve guarded checked-in installation; missing `{required}`"
        );
    }

    let galec_build_rs = fs::read_to_string(root.join("crates/rumoca-phase-parse-galec/build.rs"))
        .expect("read GALEC parser build.rs");
    for required in [
        r#"Builder::with_explicit_output_dir("src/parse/generated")"#,
        r#".grammar_file(par_file)"#,
        r#".parser_output_file("galec_parser.rs")"#,
        r#".actions_output_file("galec_grammar_trait.rs")"#,
        r#"println!("cargo:rerun-if-changed=src/parse/galec.par");"#,
    ] {
        assert!(
            galec_build_rs.contains(required),
            "GALEC parser build script must keep the generated parser contract stable; missing `{required}`"
        );
    }

    let contributing =
        fs::read_to_string(root.join("CONTRIBUTING.md")).expect("read CONTRIBUTING.md");
    for required in [
        "## Parser Grammar Regeneration",
        "cargo check -p rumoca-phase-parse",
        "cargo test -p rumoca-phase-parse --test recovery_corpus --quiet",
        "cargo check -p rumoca-phase-parse-galec",
        "cargo test -p rumoca-phase-parse-galec --quiet",
        "git diff -- crates/rumoca-phase-parse/src/generated",
        "git diff -- crates/rumoca-phase-parse-galec/src/parse/generated",
    ] {
        assert!(
            contributing.contains(required),
            "CONTRIBUTING.md must document parser regeneration; missing `{required}`"
        );
    }

    let ci = fs::read_to_string(root.join(".github/workflows/ci.yml")).expect("read CI workflow");
    assert!(
        ci.contains("git diff --exit-code --")
            && ci.contains("crates/rumoca-phase-parse/src/generated"),
        "CI lint gate must fail when the checked-in generated parser is stale"
    );
    assert!(
        ci.contains("crates/rumoca-phase-parse-galec/src/parse/generated"),
        "CI lint gate must fail when the checked-in generated GALEC parser is stale"
    );
}
