//! Compiler-defined bodies of cataloged foreign functions that run only at
//! translation (SPEC_0040 FLAT-C06).
//!
//! An external function has the meaning of its foreign body (MLS §12.9). A
//! foreign entry point that takes or returns String values, as the file
//! readers and the String functions of `ModelicaStrings.c` do, can run only
//! at translation: the runtime owns no String storage.
//! Each row of this closed catalog names its exact entry point, its ordered
//! external argument interface, and one evaluator that reproduces the MSL 4.1
//! C source over the values of its input arguments. A file name that is an
//! MLS §13.5 URI names the resource the URI resolves to, as the tool's
//! `ModelicaServices` resolves it; any other name has the C meaning.

mod mat;
mod resources;

pub use resources::ResourceRoots;

use crate::constant::Value;

/// One cataloged translation-only foreign function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TranslationRead {
    /// `fullName = ModelicaInternal_fullPathName(name)`.
    FullPathName,
    /// `ModelicaIO_readMatrixSizes(fileName, matrixName, dim)`.
    ReadMatrixSizes,
    /// `ModelicaIO_readRealMatrix(fileName, matrixName, matrix, nrow, ncol,
    /// verboseRead)`.
    ReadRealMatrix,
    /// `result = ModelicaStrings_compare(string1, string2, caseSensitive)`.
    StringsCompare,
    /// `result = ModelicaStrings_length(string)`: `strlen`, the number of bytes.
    StringsLength,
}

/// The Modelica type of one value crossing a reader's interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadElement {
    String,
    Integer,
    Real,
    Boolean,
    /// An enumeration, by its literals in ordinal order; the value crosses the
    /// interface as its one-based ordinal.
    Enumeration(&'static [&'static str]),
}

/// One ordered external argument position of a reader's interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ReadArgument {
    /// An argument expression the reader reads.
    Input(ReadElement),
    /// An output formal of the function the reader writes, of the element
    /// type and rank given.
    Output(ReadElement, usize),
}

/// Why a reader produced no result.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TranslationReadError {
    /// The operands do not match the row's interface.
    OperandMismatch { read: TranslationRead },
    /// The foreign body reports an error, as it does through `ModelicaError`.
    Failure {
        read: TranslationRead,
        message: String,
    },
}

impl std::fmt::Display for TranslationReadError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OperandMismatch { read } => write!(
                formatter,
                "foreign reader `{}` received operands outside its interface",
                read.entry_point()
            ),
            Self::Failure { read, message } => {
                write!(formatter, "`{}`: {message}", read.entry_point())
            }
        }
    }
}

impl std::error::Error for TranslationReadError {}

use ReadArgument::{Input, Output};
use ReadElement::{Boolean, Integer, Real};

impl TranslationRead {
    /// The catalog row of an external declaration, identified by its exact
    /// language and entry point.
    pub fn from_external(language: &str, entry_point: &str) -> Option<Self> {
        if language != "C" {
            return None;
        }
        [
            Self::FullPathName,
            Self::ReadMatrixSizes,
            Self::ReadRealMatrix,
            Self::StringsCompare,
            Self::StringsLength,
        ]
        .into_iter()
        .find(|read| read.entry_point() == entry_point)
    }

    /// The foreign entry point.
    pub const fn entry_point(self) -> &'static str {
        match self {
            Self::FullPathName => "ModelicaInternal_fullPathName",
            Self::ReadMatrixSizes => "ModelicaIO_readMatrixSizes",
            Self::ReadRealMatrix => "ModelicaIO_readRealMatrix",
            Self::StringsCompare => "ModelicaStrings_compare",
            Self::StringsLength => "ModelicaStrings_length",
        }
    }

    /// The ordered external argument interface.
    pub const fn interface(self) -> &'static [ReadArgument] {
        const STRING: ReadArgument = Input(ReadElement::String);
        match self {
            Self::FullPathName => &[STRING],
            Self::ReadMatrixSizes => &[STRING, STRING, Output(Integer, 1)],
            Self::ReadRealMatrix => &[
                STRING,
                STRING,
                Output(Real, 2),
                Input(Integer),
                Input(Integer),
                Input(Boolean),
            ],
            Self::StringsCompare => &[STRING, STRING, Input(Boolean)],
            Self::StringsLength => &[STRING],
        }
    }

    /// The element type of the `output = symbol(...)` return form, if any.
    pub const fn returns(self) -> Option<ReadElement> {
        match self {
            Self::FullPathName => Some(ReadElement::String),
            Self::ReadMatrixSizes | Self::ReadRealMatrix => None,
            Self::StringsCompare => Some(ReadElement::Enumeration(COMPARE_LITERALS)),
            Self::StringsLength => Some(Integer),
        }
    }

    /// Evaluate the row over its input values, in interface order, returning
    /// its output values in interface order followed by the returned value.
    /// An enumeration is returned as its one-based ordinal.
    pub fn evaluate(
        self,
        inputs: &[Value],
        resources: &ResourceRoots,
    ) -> Result<Vec<Value>, TranslationReadError> {
        let mismatch = || TranslationReadError::OperandMismatch { read: self };
        let failure = |message: String| TranslationReadError::Failure {
            read: self,
            message,
        };
        match (self, inputs) {
            (Self::FullPathName, [Value::String(name)]) => {
                let path = resources.file_name(name).map_err(failure)?;
                Ok(vec![Value::String(resources::full_path_name(&path, name))])
            }
            (Self::ReadMatrixSizes, [Value::String(file), Value::String(name)]) => {
                let matrix = self.read(file, name, resources)?;
                Ok(vec![Value::Array(vec![
                    Value::Integer(extent(matrix.rows)),
                    Value::Integer(extent(matrix.cols)),
                ])])
            }
            (
                Self::ReadRealMatrix,
                [
                    Value::String(file),
                    Value::String(name),
                    Value::Integer(rows),
                    Value::Integer(cols),
                    Value::Bool(_),
                ],
            ) => {
                let matrix = self.read(file, name, resources)?;
                let shape = format!("\"{name}({},{})\"", matrix.rows, matrix.cols);
                if usize::try_from(*rows).ok() != Some(matrix.rows) {
                    return Err(failure(format!(
                        "Cannot read {rows} rows of array {shape} from file \"{file}\""
                    )));
                }
                if usize::try_from(*cols).ok() != Some(matrix.cols) {
                    return Err(failure(format!(
                        "Cannot read {cols} columns of array {shape} from file \"{file}\""
                    )));
                }
                Ok(vec![row_major(&matrix)])
            }
            (
                Self::StringsCompare,
                [
                    Value::String(left),
                    Value::String(right),
                    Value::Bool(case_sensitive),
                ],
            ) => Ok(vec![Value::Integer(compare(left, right, *case_sensitive))]),
            (Self::StringsLength, [Value::String(text)]) => {
                Ok(vec![Value::Integer(extent(text.len()))])
            }
            _ => Err(mismatch()),
        }
    }

    /// The rank-2 real numeric variable `name` of the MAT file `file`.
    fn read(
        self,
        file: &str,
        name: &str,
        resources: &ResourceRoots,
    ) -> Result<mat::MatMatrix, TranslationReadError> {
        let failure = |message: String| TranslationReadError::Failure {
            read: self,
            message,
        };
        if name.contains('.') {
            return Err(failure(format!(
                "struct field path \"{name}\" is not a supported variable name"
            )));
        }
        let path = resources.file_name(file).map_err(failure)?;
        let bytes = std::fs::read(&path).map_err(|_| {
            failure(format!(
                "Not possible to open file \"{file}\": No such file or directory"
            ))
        })?;
        mat::read_matrix(&bytes, name).map_err(|error| {
            failure(match error {
                mat::MatError::NotFound => {
                    format!("Variable \"{name}\" not found in file \"{file}\".")
                }
                mat::MatError::NotRank2 => format!("Variable \"{name}\" is not of rank 2."),
                mat::MatError::NotNumeric => {
                    format!("Matrix \"{name}\" is not a numeric array.")
                }
                mat::MatError::Complex => format!("Matrix \"{name}\" must not be complex."),
                mat::MatError::Hdf5 => {
                    format!("File \"{file}\" is a v7.3 (HDF5) MAT file, which is not supported")
                }
                mat::MatError::Malformed => {
                    format!(
                        "Error when reading numeric data of matrix \"{name}\" from file \"{file}\""
                    )
                }
            })
        })
    }
}

/// The literals of `Modelica.Utilities.Types.Compare`, in the ordinal order of
/// the `ModelicaStrings.c` result (`Modelica_Utilities_Types_Compare`).
const COMPARE_LITERALS: &[&str] = &["Less", "Equal", "Greater"];

/// `ModelicaStrings_compare`: `strcmp` of the two strings, or of their
/// `tolower` images when case is ignored, as the ordinal of its sign. The "C"
/// locale folds only ASCII letters, and bytes compare unsigned.
fn compare(left: &str, right: &str, case_sensitive: bool) -> i64 {
    let fold = |byte: u8| {
        if case_sensitive {
            byte
        } else {
            byte.to_ascii_lowercase()
        }
    };
    let ordering = left.bytes().map(fold).cmp(right.bytes().map(fold));
    match ordering {
        std::cmp::Ordering::Less => 1,
        std::cmp::Ordering::Equal => 2,
        std::cmp::Ordering::Greater => 3,
    }
}

fn extent(value: usize) -> i64 {
    i64::try_from(value).unwrap_or(i64::MAX)
}

/// The Modelica row-major array of a column-major matrix.
fn row_major(matrix: &mat::MatMatrix) -> Value {
    Value::Array(
        (0..matrix.rows)
            .map(|row| {
                Value::Array(
                    (0..matrix.cols)
                        .map(|col| Value::Real(matrix.column_major[col * matrix.rows + row]))
                        .collect(),
                )
            })
            .collect(),
    )
}

#[cfg(test)]
mod tests;
