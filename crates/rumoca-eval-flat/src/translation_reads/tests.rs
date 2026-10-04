use std::path::{Path, PathBuf};

use super::*;

/// A scratch directory unique to one test.
fn scratch(name: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "rumoca-translation-reads-{}-{name}",
        std::process::id()
    ));
    std::fs::create_dir_all(&dir).expect("scratch directory");
    dir
}

/// A level 4 MAT file holding `[11 12; 21 22; 31 32]` as `Matrix_A`.
fn write_mat(path: &Path) {
    let mut bytes = Vec::new();
    for value in [0_u32, 3, 2, 0, 9] {
        bytes.extend(value.to_le_bytes());
    }
    bytes.extend(b"Matrix_A\0");
    for value in [11.0_f64, 21.0, 31.0, 12.0, 22.0, 32.0] {
        bytes.extend(value.to_le_bytes());
    }
    std::fs::write(path, bytes).expect("MAT file");
}

fn string(text: &str) -> Value {
    Value::String(text.to_string())
}

#[test]
fn modelica_uris_resolve_against_the_package_directory() {
    let mut roots = ResourceRoots::new();
    roots.insert("Lib", "/libs/Lib 1.0");
    roots.insert("Lib.Sub", "/libs/Lib 1.0/Sub");
    assert_eq!(
        roots.file_name("modelica://Lib/Resources/Data/a%20b.mat"),
        Ok(PathBuf::from("/libs/Lib 1.0/Resources/Data/a b.mat"))
    );
    assert_eq!(
        roots.file_name("MODELICA://Lib.Sub/c.txt"),
        Ok(PathBuf::from("/libs/Lib 1.0/Sub/c.txt"))
    );
    assert_eq!(
        roots.file_name("file:///tmp/x.mat"),
        Ok(PathBuf::from("/tmp/x.mat"))
    );
    assert_eq!(
        roots.file_name("file://localhost/tmp/x.mat"),
        Ok(PathBuf::from("/tmp/x.mat"))
    );
    assert_eq!(
        roots.file_name("data/x.mat"),
        Ok(PathBuf::from("data/x.mat"))
    );
    assert!(roots.file_name("modelica://Other/x").is_err());
    assert!(roots.file_name("modelica://Lib").is_err());
    assert!(roots.file_name("modelica://Lib/%zz").is_err());
}

#[test]
fn catalog_rows_are_identified_by_language_and_entry_point() {
    for read in [
        TranslationRead::FullPathName,
        TranslationRead::ReadMatrixSizes,
        TranslationRead::ReadRealMatrix,
    ] {
        assert_eq!(
            TranslationRead::from_external("C", read.entry_point()),
            Some(read)
        );
        assert_eq!(
            TranslationRead::from_external("FORTRAN 77", read.entry_point()),
            None
        );
        let outputs = read
            .interface()
            .iter()
            .filter(|argument| matches!(argument, ReadArgument::Output(..)))
            .count();
        assert_eq!(outputs + usize::from(read.returns().is_some()), 1);
    }
    assert_eq!(
        TranslationRead::from_external("C", "ModelicaIO_writeRealMatrix"),
        None
    );
}

#[test]
fn full_path_name_resolves_existing_and_missing_names() {
    use super::resources::path_name_text;
    let dir = scratch("full-path");
    let roots = ResourceRoots::new();
    let existing = dir.join("present.txt");
    std::fs::write(&existing, "x").expect("file");
    // Canonicalization yields a verbatim `\\?\D:\...` path on Windows; the
    // result is its Modelica path name.
    let canonical = std::fs::canonicalize(&existing).expect("canonical path");
    let name = existing.to_string_lossy().into_owned();
    let expected = path_name_text(&canonical);
    assert!(
        !expected.contains('\\') && !expected.starts_with("//?/"),
        "{expected}"
    );
    assert_eq!(
        TranslationRead::FullPathName.evaluate(&[string(&name)], &roots),
        Ok(vec![string(&expected)])
    );
    let cwd = std::env::current_dir().expect("current directory");
    assert_eq!(
        TranslationRead::FullPathName.evaluate(&[string("missing/name/")], &roots),
        Ok(vec![string(&format!(
            "{}/",
            path_name_text(&cwd.join("missing/name"))
        ))])
    );
}

#[test]
fn mat_readers_return_sizes_and_row_major_values() {
    let dir = scratch("mat");
    write_mat(&dir.join("m.mat"));
    let mut roots = ResourceRoots::new();
    roots.insert("Lib", &dir);
    let file = string("modelica://Lib/m.mat");
    assert_eq!(
        TranslationRead::ReadMatrixSizes.evaluate(&[file.clone(), string("Matrix_A")], &roots),
        Ok(vec![Value::Array(vec![
            Value::Integer(3),
            Value::Integer(2)
        ])])
    );
    let row = |values: [f64; 2]| Value::Array(values.map(Value::Real).to_vec());
    assert_eq!(
        TranslationRead::ReadRealMatrix.evaluate(
            &[
                file.clone(),
                string("Matrix_A"),
                Value::Integer(3),
                Value::Integer(2),
                Value::Bool(true),
            ],
            &roots,
        ),
        Ok(vec![Value::Array(vec![
            row([11.0, 12.0]),
            row([21.0, 22.0]),
            row([31.0, 32.0]),
        ])])
    );
    let rows_mismatch = TranslationRead::ReadRealMatrix
        .evaluate(
            &[
                file.clone(),
                string("Matrix_A"),
                Value::Integer(2),
                Value::Integer(2),
                Value::Bool(false),
            ],
            &roots,
        )
        .expect_err("a declared extent that differs from the file fails");
    assert!(rows_mismatch.to_string().contains("Cannot read 2 rows"));
    let missing = TranslationRead::ReadMatrixSizes
        .evaluate(&[file.clone(), string("Matrix_B")], &roots)
        .expect_err("an absent variable fails");
    assert!(missing.to_string().contains("not found"));
    let no_file = TranslationRead::ReadMatrixSizes
        .evaluate(
            &[string("modelica://Lib/absent.mat"), string("Matrix_A")],
            &roots,
        )
        .expect_err("an absent file fails");
    assert!(no_file.to_string().contains("Not possible to open file"));
    let field = TranslationRead::ReadMatrixSizes
        .evaluate(&[file, string("s.Matrix_A")], &roots)
        .expect_err("a struct field path is refused");
    assert!(field.to_string().contains("struct field path"));
}

#[test]
fn operands_outside_the_interface_are_mismatches() {
    let roots = ResourceRoots::new();
    assert_eq!(
        TranslationRead::ReadMatrixSizes.evaluate(&[string("a")], &roots),
        Err(TranslationReadError::OperandMismatch {
            read: TranslationRead::ReadMatrixSizes
        })
    );
    let message = TranslationReadError::OperandMismatch {
        read: TranslationRead::FullPathName,
    }
    .to_string();
    assert!(message.contains("ModelicaInternal_fullPathName"));
}

#[test]
fn verbatim_windows_prefixes_map_back_to_user_path_forms() {
    use super::resources::strip_verbatim_prefix;
    assert_eq!(
        strip_verbatim_prefix("//?/D:/dir/file".into()),
        "D:/dir/file"
    );
    assert_eq!(
        strip_verbatim_prefix("//?/UNC/host/share/x".into()),
        "//host/share/x"
    );
    assert_eq!(strip_verbatim_prefix("/usr/lib".into()), "/usr/lib");
    assert_eq!(strip_verbatim_prefix("D:/dir".into()), "D:/dir");
}
