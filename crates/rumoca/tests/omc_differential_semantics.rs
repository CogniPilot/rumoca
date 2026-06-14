use std::fs;
use std::process::Command;

use tempfile::tempdir_in;

const ENCAPSULATED_SCOPE_SOURCE: &str = r#"
package P
  constant Real c = 1;
  encapsulated model M
    Real x = c;
  end M;
end P;
"#;

fn omc_available() -> bool {
    Command::new("omc").arg("--version").output().is_ok()
}

#[test]
fn encapsulated_scope_rejection_matches_omc() {
    if !omc_available() {
        eprintln!("skipping OMC differential test: omc not available");
        return;
    }

    let cwd = std::env::current_dir().expect("current dir");
    let dir = tempdir_in(cwd).expect("tempdir in mounted workspace");
    let model_path = dir.path().join("EncapsulatedScope.mo");
    let script_path = dir.path().join("check.mos");
    fs::write(&model_path, ENCAPSULATED_SCOPE_SOURCE).expect("write model");
    fs::write(
        &script_path,
        format!(
            r#"loadFile("{}");
checkModel(P.M);
getErrorString();
"#,
            model_path.display()
        ),
    )
    .expect("write OMC script");

    let omc = Command::new("omc")
        .arg(&script_path)
        .output()
        .expect("run omc");
    let omc_output = format!(
        "{}{}",
        String::from_utf8_lossy(&omc.stdout),
        String::from_utf8_lossy(&omc.stderr)
    );
    assert!(
        omc.status.success() && omc_output.contains("Variable c not found in scope M"),
        "expected OMC to reject unqualified enclosing-scope lookup, got:\n{omc_output}"
    );

    let rumoca = rumoca::Compiler::new()
        .model("P.M")
        .compile_str(ENCAPSULATED_SCOPE_SOURCE, "EncapsulatedScope.mo");
    let err = rumoca.expect_err("Rumoca should reject the same encapsulated lookup");
    let err_text = format!("{err:?}");
    assert!(
        err_text.contains("unresolved component reference: 'c'"),
        "expected Rumoca unresolved-name diagnostic, got:\n{err_text}"
    );
}
