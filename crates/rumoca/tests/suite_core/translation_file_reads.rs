//! Cataloged foreign file readers at translation (MLS 3.7 §12.9, §13.5;
//! SPEC_0040 FLAT-C06).
//!
//! `Modelica.Utilities.Examples.ReadRealMatrixFromFile` sizes a matrix from
//! `ModelicaIO_readMatrixSizes` and fills it from `ModelicaIO_readRealMatrix`,
//! both reading a MAT file named by a `modelica://` URI that
//! `ModelicaInternal_fullPathName` resolves. Their String operands exist only
//! at translation, so the readers execute there: the URI names the resource
//! under the loaded package's directory, and the matrix becomes a literal.

use std::path::PathBuf;

use rumoca::Compiler;
use rumoca_sim::{SimOptions, simulate_dae};

const PACKAGE: &str = r#"
package Lib
  impure function fullPathName
    input String name;
    output String fullName;
  external "C" fullName = ModelicaInternal_fullPathName(name);
  end fullPathName;
  function loadResource
    input String uri;
    output String fileReference;
  algorithm
    fileReference := fullPathName(uri);
  end loadResource;
  impure function readMatrixSize
    input String fileName;
    input String matrixName;
    output Integer dim[2];
  external "C" ModelicaIO_readMatrixSizes(fileName, matrixName, dim);
  end readMatrixSize;
  impure function readRealMatrix
    input String fileName;
    input String matrixName;
    input Integer nrow;
    input Integer ncol;
    input Boolean verboseRead = true;
    output Real matrix[nrow, ncol];
  external "C" ModelicaIO_readRealMatrix(fileName, matrixName, matrix,
    size(matrix, 1), size(matrix, 2), verboseRead);
  end readRealMatrix;
  model ReadMatrix
    parameter String file = loadResource("modelica://Lib/Resources/m.mat");
    final parameter Integer dim[2] = readMatrixSize(file, "Matrix_A");
    final parameter Real A[:, :] = readRealMatrix(file, "Matrix_A", dim[1], dim[2]);
    Real x(start = 1, fixed = true);
    Real n = 10*size(A, 1) + size(A, 2);
  equation
    der(x) = -(A[3, 2] - A[1, 1] - 20)*x;
  end ReadMatrix;
  package Data
    constant Integer offset = 0;
  end Data;
  model ReadNested
    parameter String file = loadResource("modelica://Lib.Data/m.mat");
    final parameter Integer dim[2] = readMatrixSize(file, "Matrix_A");
    final parameter Real A[:, :] = readRealMatrix(file, "Matrix_A", dim[1], dim[2]);
    Real n = 10*size(A, 1) + size(A, 2) + Data.offset;
  end ReadNested;
  model ReadThroughComprehension
    final parameter String files[2] =
      {loadResource("modelica://Lib/Resources/m.mat") for i in 1:2};
    final parameter Integer dim[2] = readMatrixSize(files[2], "Matrix_A");
    final parameter Real A[:, :] = readRealMatrix(files[2], "Matrix_A", dim[1], dim[2]);
    Real n = 10*size(A, 1) + size(A, 2);
  end ReadThroughComprehension;
  model MissingVariable
    parameter String file = loadResource("modelica://Lib/Resources/m.mat");
    final parameter Integer dim[2] = readMatrixSize(file, "Matrix_B");
    Real x[dim[1]](each start = 1, each fixed = true);
  equation
    der(x) = -x;
  end MissingVariable;
end Lib;
"#;

/// `Lib/package.mo` and the level 4 MAT files `Lib/Resources/m.mat` and
/// `Lib/Data/m.mat` (the resource directory of `Lib.Data`) holding
/// `Matrix_A = [11 12; 21 22; 31 32]`.
fn library(name: &str) -> PathBuf {
    let root = std::env::temp_dir().join(format!(
        "rumoca-translation-file-reads-{}-{name}",
        std::process::id()
    ));
    let resources = root.join("Lib").join("Resources");
    std::fs::create_dir_all(&resources).expect("library directory");
    std::fs::write(root.join("Lib").join("package.mo"), PACKAGE).expect("package file");
    let mut bytes = Vec::new();
    for value in [0_u32, 3, 2, 0, 9] {
        bytes.extend(value.to_le_bytes());
    }
    bytes.extend(b"Matrix_A\0");
    for value in [11.0_f64, 21.0, 31.0, 12.0, 22.0, 32.0] {
        bytes.extend(value.to_le_bytes());
    }
    std::fs::write(resources.join("m.mat"), &bytes).expect("MAT file");
    let data = root.join("Lib").join("Data");
    std::fs::create_dir_all(&data).expect("nested package directory");
    std::fs::write(data.join("m.mat"), bytes).expect("MAT file");
    root.join("Lib").join("package.mo")
}

#[test]
fn a_matrix_is_sized_and_read_from_a_modelica_uri_at_translation() {
    let package = library("read");
    let compiled = Compiler::new()
        .model("Lib.ReadMatrix")
        .compile_file(&package.to_string_lossy())
        .unwrap_or_else(|error| panic!("ReadMatrix compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.25),
            ..SimOptions::default()
        },
    )
    .expect("ReadMatrix simulates");
    let last = |variable: &str| {
        let column = result
            .names
            .iter()
            .position(|name| name == variable)
            .unwrap_or_else(|| panic!("{variable} is recorded"));
        *result.data[column].last().expect("trace is nonempty")
    };
    assert_eq!(last("n"), 32.0, "the file stores a 3 x 2 matrix");
    let x = last("x");
    assert!(
        (x - (-1.0_f64).exp()).abs() < 1e-5,
        "A(3,2) - A(1,1) - 20 = 1 read row-major, x(1) = {x}"
    );
}

#[test]
fn a_variable_absent_from_the_file_is_not_given_a_size() {
    let package = library("missing");
    let error = Compiler::new()
        .model("Lib.MissingVariable")
        .compile_file(&package.to_string_lossy())
        .expect_err("an unreadable dimension is refused");
    let error = format!("{error:?}");
    assert!(
        error.contains("dim"),
        "the dimension stays unresolved: {error}"
    );
}

#[test]
fn a_nested_package_uri_names_the_subdirectory_of_its_name() {
    let package = library("nested");
    let compiled = Compiler::new()
        .model("Lib.ReadNested")
        .compile_file(&package.to_string_lossy())
        .unwrap_or_else(|error| panic!("ReadNested compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("ReadNested simulates");
    let column = result
        .names
        .iter()
        .position(|name| name == "n")
        .expect("n is recorded");
    assert_eq!(result.data[column].last().copied(), Some(32.0));
}

#[test]
fn a_reader_inside_an_array_comprehension_runs_at_translation() {
    let package = library("comprehension");
    let compiled = Compiler::new()
        .model("Lib.ReadThroughComprehension")
        .compile_file(&package.to_string_lossy())
        .unwrap_or_else(|error| panic!("ReadThroughComprehension compiles: {error:?}"));
    let result = simulate_dae(
        &compiled.dae,
        &SimOptions {
            t_end: 1.0,
            dt: Some(0.5),
            ..SimOptions::default()
        },
    )
    .expect("ReadThroughComprehension simulates");
    let column = result
        .names
        .iter()
        .position(|name| name == "n")
        .expect("n is recorded");
    assert_eq!(result.data[column].last().copied(), Some(32.0));
}
