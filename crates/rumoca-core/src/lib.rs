//! Common types for the Rumoca compiler.
//!
//! This crate provides shared types used across compiler phases.
//!
//! # Error Infrastructure
//!
//! This crate provides common error infrastructure for compiler phases:
//!
//! - [`SourceSpan`] - Re-exported from miette for span conversion
//! - [`BoxedResult`] - Type alias for `Result<T, Box<E>>` pattern
//! - [`error_constructor!`] - Macro for generating error constructors
//!
//! ## Example Usage
//!
//! ```ignore
//! use rumoca_core::{BoxedResult, Span, SourceSpan, error_constructor};
//!
//! pub type FlattenResult<T> = BoxedResult<T, FlattenError>;
//!
//! #[derive(Debug, Clone, Error, Diagnostic)]
//! pub enum FlattenError {
//!     #[error("undefined variable: {name}")]
//!     #[diagnostic(code(rumoca::flatten::EF001))]
//!     UndefinedVariable { name: String, span: SourceSpan },
//! }
//!
//! impl FlattenError {
//!     error_constructor!(undefined_variable, UndefinedVariable { name: String });
//! }
//! ```

use miette::{Diagnostic as MietteDiagnostic, LabeledSpan, NamedSource, Severity};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

pub mod eval_lookup;
pub use eval_lookup::EvalLookup;
pub mod enum_compare;
pub use enum_compare::enum_values_equal;
pub mod integer_binary;
pub use integer_binary::{IntegerBinaryOperator, eval_integer_binary};
pub mod integer_division;
pub use integer_division::{eval_integer_div_builtin, eval_integer_slash};

/// Resolve the workspace root from a crate manifest directory.
///
/// For crates under `<workspace>/crates/*`, this returns `<workspace>`.
pub fn workspace_root_from_manifest_dir(manifest_dir: &str) -> PathBuf {
    PathBuf::from(manifest_dir).join("../..")
}

/// Resolve the MSL cache directory with workspace-root semantics.
///
/// Rules:
/// - If `RUMOCA_MSL_CACHE_DIR` is absolute, use it as-is.
/// - If `RUMOCA_MSL_CACHE_DIR` is relative, resolve it against workspace root.
/// - If unset, default to `<workspace>/target/msl`.
pub fn msl_cache_dir_from_manifest(manifest_dir: &str) -> PathBuf {
    let workspace_root = workspace_root_from_manifest_dir(manifest_dir);
    std::env::var_os("RUMOCA_MSL_CACHE_DIR")
        .map(PathBuf::from)
        .map(|path| {
            if path.is_absolute() {
                path
            } else {
                workspace_root.join(path)
            }
        })
        .unwrap_or_else(|| workspace_root.join("target/msl"))
}

/// A unique identifier for a definition (class, component, etc.).
///
/// DefIds are assigned during semantic analysis to enable efficient
/// lookup and cross-referencing between compiler phases.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct DefId(pub u32);

impl DefId {
    /// Create a new DefId from an index.
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    /// Get the underlying index.
    pub fn index(&self) -> u32 {
        self.0
    }
}

impl std::fmt::Display for DefId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "DefId({})", self.0)
    }
}

/// A unique identifier for a type.
///
/// TypeIds reference entries in the TypeTable and are used throughout
/// the compiler to refer to types without copying type information.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct TypeId(pub u32);

impl TypeId {
    /// A sentinel value representing an unknown/unresolved type.
    pub const UNKNOWN: TypeId = TypeId(u32::MAX);

    /// Create a new TypeId from an index.
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    /// Get the underlying index.
    pub fn index(&self) -> u32 {
        self.0
    }

    /// Check if this is the unknown type sentinel.
    pub fn is_unknown(&self) -> bool {
        *self == Self::UNKNOWN
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_unknown() {
            write!(f, "TypeId(UNKNOWN)")
        } else {
            write!(f, "TypeId({})", self.0)
        }
    }
}

/// A unique identifier for a scope in the scope tree.
///
/// ScopeIds are used for name lookup during semantic analysis (MLS §5.3).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct ScopeId(pub u32);

impl ScopeId {
    /// The global scope (root of scope tree).
    pub const GLOBAL: ScopeId = ScopeId(0);

    /// Create a new ScopeId from an index.
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    /// Get the underlying index.
    pub fn index(&self) -> u32 {
        self.0
    }

    /// Check if this is the global scope.
    pub fn is_global(&self) -> bool {
        *self == Self::GLOBAL
    }
}

impl std::fmt::Display for ScopeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_global() {
            write!(f, "ScopeId(GLOBAL)")
        } else {
            write!(f, "ScopeId({})", self.0)
        }
    }
}

/// A source file identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct SourceId(pub u32);

/// A byte position in source code.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub struct BytePos(pub usize);

/// A span in source code (source, start, end).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct Span {
    pub source: SourceId,
    pub start: BytePos,
    pub end: BytePos,
}

impl Span {
    /// A dummy span for errors without location info.
    pub const DUMMY: Span = Span {
        source: SourceId(0),
        start: BytePos(0),
        end: BytePos(0),
    };

    /// Create a new span.
    pub fn new(source: SourceId, start: BytePos, end: BytePos) -> Self {
        Self { source, start, end }
    }

    /// Create a span from byte offsets.
    pub fn from_offsets(source: SourceId, start: usize, end: usize) -> Self {
        Self {
            source,
            start: BytePos(start),
            end: BytePos(end),
        }
    }

    /// Convert to miette's SourceSpan for error reporting.
    ///
    /// This is used by phase-specific error types to create miette diagnostics.
    pub fn to_source_span(&self) -> SourceSpan {
        let start = self.start.0;
        let len = self.end.0.saturating_sub(self.start.0);
        (start, len).into()
    }
}

// =============================================================================
// Modelica Built-in Types and Functions (MLS §3.7, §4.9, §16)
// =============================================================================

/// Built-in types that can be extended (MLS §4.9, §16).
///
/// - `Real`, `Integer`, `Boolean`, `String` - Core numeric and logic types (MLS §4.9)
/// - `ExternalObject` - Base for external object types (MLS §12.9.7)
/// - `Clock` - Synchronous clock type (MLS §16)
/// - `StateSelect` - Enumeration for state selection hints (MLS §4.4.4.2)
/// - `AssertionLevel` - Enumeration for assertion levels (MLS §8.3.7)
pub const BUILTIN_TYPES: &[&str] = &[
    "Real",
    "Integer",
    "Boolean",
    "String",
    "ExternalObject",
    "Clock",
    // Built-in enumerations (MLS §4.4.4.2, §8.3.7)
    "StateSelect",
    "AssertionLevel",
];

/// Built-in functions (MLS §3.7).
///
/// These are predefined operators and functions available in all scopes.
pub const BUILTIN_FUNCTIONS: &[&str] = &[
    // Classes that act like functions (MLS §4.9)
    "Real",
    "Integer",
    "Boolean",
    "String",
    // Event/state functions (MLS §3.7.3)
    "der",
    "pre",
    "edge",
    "change",
    "initial",
    "terminal",
    "sample",
    "smooth",
    "delay",
    "cardinality",
    "homotopy",
    "semiLinear",
    "inStream",
    "actualStream",
    "getInstanceName",
    "spatialDistribution",
    "reinit",
    "assert",
    "terminate",
    // Math functions (MLS §3.7.1)
    "abs",
    "sign",
    "sqrt",
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "sinh",
    "cosh",
    "tanh",
    "exp",
    "log",
    "log10",
    "floor",
    "ceil",
    "mod",
    "rem",
    "div",
    "integer",
    // Array functions (MLS §10.3)
    "size",
    "ndims",
    "scalar",
    "vector",
    "matrix",
    "transpose",
    "outerProduct",
    "symmetric",
    "cross",
    "skew",
    "identity",
    "diagonal",
    "zeros",
    "ones",
    "fill",
    "linspace",
    "min",
    "max",
    "sum",
    "product",
    "cat",
    // Special (MLS §3.7.4)
    "noEvent",
    "connect",
];

/// Built-in variables/constants available in all scopes.
///
/// `Connections` is a built-in namespace used by overconstrained connector
/// operators (MLS §9.4), e.g. `Connections.root(...)`.
pub const BUILTIN_VARIABLES: &[&str] = &["time", "Connections"];

/// Check if a type name is a built-in primitive type.
pub fn is_builtin_type(name: &str) -> bool {
    BUILTIN_TYPES.contains(&name)
}

/// Check if a name is a built-in function.
pub fn is_builtin_function(name: &str) -> bool {
    BUILTIN_FUNCTIONS.contains(&name)
}

/// Check if a name is a built-in variable.
pub fn is_builtin_variable(name: &str) -> bool {
    BUILTIN_VARIABLES.contains(&name)
}

// =============================================================================
// Source Map
// =============================================================================

/// Maps file names to SourceIds and stores source content for diagnostics.
///
/// This enables diagnostics to point to the correct source file when
/// compiling models that span multiple files.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SourceMap {
    /// (name, content) indexed by SourceId.
    files: Vec<(String, String)>,
    /// Reverse lookup from file name to SourceId.
    #[serde(skip)]
    name_to_id: HashMap<String, SourceId>,
}

impl SourceMap {
    /// Create a new empty source map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a source file and return its SourceId.
    ///
    /// If the file was already added, returns the existing SourceId.
    pub fn add(&mut self, name: &str, content: &str) -> SourceId {
        if let Some(&id) = self.name_to_id.get(name) {
            return id;
        }
        let id = SourceId(self.files.len() as u32);
        self.files.push((name.to_string(), content.to_string()));
        self.name_to_id.insert(name.to_string(), id);
        id
    }

    /// Look up a SourceId by file name.
    pub fn get_id(&self, name: &str) -> Option<SourceId> {
        self.name_to_id.get(name).copied()
    }

    /// Get (name, content) for a SourceId.
    pub fn get_source(&self, id: SourceId) -> Option<(&str, &str)> {
        self.files
            .get(id.0 as usize)
            .map(|(name, content)| (name.as_str(), content.as_str()))
    }

    /// Create a Span from a file name and byte offsets.
    ///
    /// Looks up the SourceId from the file name. Falls back to SourceId(0)
    /// if the file is not found in the source map.
    pub fn location_to_span(&self, file_name: &str, start: usize, end: usize) -> Span {
        let source_id = self
            .name_to_id
            .get(file_name)
            .copied()
            .unwrap_or(SourceId(0));
        Span::from_offsets(source_id, start, end)
    }

    /// Rebuild the name_to_id index after deserialization.
    pub fn rebuild_index(&mut self) {
        self.name_to_id.clear();
        for (i, (name, _)) in self.files.iter().enumerate() {
            self.name_to_id.insert(name.clone(), SourceId(i as u32));
        }
    }

    /// Snapshot file-name to source-id mappings.
    pub fn source_ids(&self) -> HashMap<String, SourceId> {
        self.name_to_id.clone()
    }
}

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Note,
}

/// A label pointing to a span in source code.
#[derive(Debug, Clone)]
pub struct Label {
    pub span: Span,
    pub message: Option<String>,
    pub primary: bool,
}

impl Label {
    /// Create a primary label (the main error location).
    pub fn primary(span: Span) -> Self {
        Self {
            span,
            message: None,
            primary: true,
        }
    }

    /// Create a secondary label (additional context).
    pub fn secondary(span: Span) -> Self {
        Self {
            span,
            message: None,
            primary: false,
        }
    }

    /// Add a message to this label.
    pub fn with_message(mut self, msg: impl Into<String>) -> Self {
        self.message = Some(msg.into());
        self
    }
}

/// A required primary label for source-backed diagnostics.
#[derive(Debug, Clone)]
pub struct PrimaryLabel {
    span: Span,
    message: Option<String>,
}

impl PrimaryLabel {
    /// Create a new primary label from a source span.
    pub fn new(span: Span) -> Self {
        Self {
            span,
            message: None,
        }
    }

    /// Attach a human-readable message to the label.
    pub fn with_message(mut self, msg: impl Into<String>) -> Self {
        self.message = Some(msg.into());
        self
    }
}

impl From<PrimaryLabel> for Label {
    fn from(value: PrimaryLabel) -> Self {
        Self {
            span: value.span,
            message: value.message,
            primary: true,
        }
    }
}

/// A diagnostic message (error, warning, or note).
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: DiagnosticSeverity,
    pub code: Option<String>,
    pub message: String,
    pub labels: Vec<Label>,
    pub notes: Vec<String>,
}

impl Diagnostic {
    /// Create an error diagnostic.
    pub fn error(
        code: impl Into<String>,
        message: impl Into<String>,
        primary_label: PrimaryLabel,
    ) -> Self {
        Self {
            severity: DiagnosticSeverity::Error,
            code: Some(code.into()),
            message: message.into(),
            labels: vec![primary_label.into()],
            notes: Vec::new(),
        }
    }

    /// Create a warning diagnostic.
    pub fn warning(
        code: impl Into<String>,
        message: impl Into<String>,
        primary_label: PrimaryLabel,
    ) -> Self {
        Self {
            severity: DiagnosticSeverity::Warning,
            code: Some(code.into()),
            message: message.into(),
            labels: vec![primary_label.into()],
            notes: Vec::new(),
        }
    }

    /// Create a global (non-source) error diagnostic.
    pub fn global_error(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Error,
            code: Some(code.into()),
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a global (non-source) warning diagnostic.
    pub fn global_warning(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Warning,
            code: Some(code.into()),
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create an informational diagnostic.
    pub fn note(message: impl Into<String>) -> Self {
        Self {
            severity: DiagnosticSeverity::Note,
            code: None,
            message: message.into(),
            labels: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Add a label.
    pub fn with_label(mut self, label: Label) -> Self {
        self.labels.push(label);
        self
    }

    /// Add a note.
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    /// Check if this is an error.
    pub fn is_error(&self) -> bool {
        matches!(self.severity, DiagnosticSeverity::Error)
    }

    /// Convert to a miette report for pretty display.
    /// Convert to a miette report using the source map for automatic source lookup.
    ///
    /// Looks up the source file from the first label's SourceId. Falls back to
    /// empty source if the SourceId is not found in the source map.
    pub fn to_miette_with_source_map(&self, source_map: &SourceMap) -> MietteReport {
        let source_id = self
            .labels
            .first()
            .map(|l| l.span.source)
            .unwrap_or(SourceId(0));
        let (name, content) = source_map.get_source(source_id).unwrap_or(("unknown", ""));
        self.to_miette(name, content)
    }

    pub fn to_miette(&self, source_name: &str, source: &str) -> MietteReport {
        MietteReport {
            message: self.message.clone(),
            code: self.code.clone(),
            severity: match self.severity {
                DiagnosticSeverity::Error => Severity::Error,
                DiagnosticSeverity::Warning => Severity::Warning,
                DiagnosticSeverity::Note => Severity::Advice,
            },
            labels: self
                .labels
                .iter()
                .map(|l| {
                    LabeledSpan::new(
                        l.message.clone(),
                        l.span.start.0,
                        l.span.end.0.saturating_sub(l.span.start.0),
                    )
                })
                .collect(),
            notes: self.notes.clone(),
            source_code: NamedSource::new(source_name, source.to_string()),
        }
    }
}

/// A miette-compatible report for pretty error display.
#[derive(Debug)]
pub struct MietteReport {
    message: String,
    code: Option<String>,
    severity: Severity,
    labels: Vec<LabeledSpan>,
    notes: Vec<String>,
    source_code: NamedSource<String>,
}

impl std::fmt::Display for MietteReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for MietteReport {}

impl MietteDiagnostic for MietteReport {
    fn code<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.code
            .as_ref()
            .map(|c| Box::new(c.clone()) as Box<dyn std::fmt::Display>)
    }

    fn severity(&self) -> Option<Severity> {
        Some(self.severity)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        Some(Box::new(self.labels.iter().cloned()))
    }

    fn help<'a>(&'a self) -> Option<Box<dyn std::fmt::Display + 'a>> {
        if self.notes.is_empty() {
            None
        } else {
            Some(Box::new(self.notes.join("\n")))
        }
    }

    fn source_code(&self) -> Option<&dyn miette::SourceCode> {
        Some(&self.source_code)
    }
}

/// Trait for phase-specific errors.
pub trait PhaseError {
    /// Convert this error to a diagnostic.
    fn to_diagnostic(&self) -> Diagnostic;
}

/// A collection of diagnostics.
#[derive(Debug, Default)]
pub struct Diagnostics {
    diags: Vec<Diagnostic>,
    error_count: usize,
}

impl Diagnostics {
    /// Create a new empty diagnostics collection.
    pub fn new() -> Self {
        Self::default()
    }

    /// Emit a diagnostic.
    pub fn emit(&mut self, diag: Diagnostic) {
        if diag.is_error() {
            self.error_count += 1;
        }
        self.diags.push(diag);
    }

    /// Check if any errors were emitted.
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Get the number of errors emitted.
    pub fn error_count(&self) -> usize {
        self.error_count
    }

    /// Get the total number of diagnostics (errors + warnings).
    pub fn len(&self) -> usize {
        self.diags.len()
    }

    /// Check if no diagnostics have been emitted.
    pub fn is_empty(&self) -> bool {
        self.diags.is_empty()
    }

    /// Get all diagnostics.
    pub fn iter(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diags.iter()
    }
}

// =============================================================================
// Error Infrastructure for Compiler Phases
// =============================================================================

/// Re-export SourceSpan from miette for use in phase error types.
///
/// This allows phase crates to use `SourceSpan` without directly depending
/// on miette for this single type.
pub use miette::SourceSpan;

/// Type alias for results with boxed errors.
///
/// Boxing error types avoids clippy::result_large_err warnings while
/// preserving rich diagnostic information. The error path is cold (errors
/// are exceptional), so the allocation overhead is negligible.
///
/// # Usage
///
/// ```ignore
/// use rumoca_core::BoxedResult;
///
/// pub type FlattenResult<T> = BoxedResult<T, FlattenError>;
/// ```
pub type BoxedResult<T, E> = Result<T, Box<E>>;

/// Macro for generating error constructor methods with span conversion.
///
/// This macro generates constructor methods for error enum variants that
/// contain a `span: SourceSpan` field. It handles the common pattern of:
/// 1. Taking `impl Into<String>` for string fields
/// 2. Taking `Span` and converting to `SourceSpan`
///
/// # Syntax
///
/// ```ignore
/// error_constructor!(method_name, VariantName { field1: Type1, field2: Type2 });
/// ```
///
/// The last field is assumed to be `span: Span` which gets converted to `SourceSpan`.
///
/// # Example
///
/// ```ignore
/// impl FlattenError {
///     // Generates: pub fn undefined_variable(name: impl Into<String>, span: Span) -> Self
///     error_constructor!(undefined_variable, UndefinedVariable { name: String });
///
///     // Generates: pub fn incompatible_connectors(a: impl Into<String>, b: impl Into<String>, span: Span) -> Self
///     error_constructor!(incompatible_connectors, IncompatibleConnectors { a: String, b: String });
/// }
/// ```
#[macro_export]
macro_rules! error_constructor {
    // Single String field + span
    ($fn_name:ident, $variant:ident { $field:ident : String }) => {
        /// Create an error with the given field and span.
        pub fn $fn_name($field: impl Into<String>, span: $crate::Span) -> Self {
            Self::$variant {
                $field: $field.into(),
                span: span.to_source_span(),
            }
        }
    };

    // Two String fields + span
    ($fn_name:ident, $variant:ident { $f1:ident : String, $f2:ident : String }) => {
        /// Create an error with the given fields and span.
        pub fn $fn_name(
            $f1: impl Into<String>,
            $f2: impl Into<String>,
            span: $crate::Span,
        ) -> Self {
            Self::$variant {
                $f1: $f1.into(),
                $f2: $f2.into(),
                span: span.to_source_span(),
            }
        }
    };

    // Three String fields + span
    ($fn_name:ident, $variant:ident { $f1:ident : String, $f2:ident : String, $f3:ident : String }) => {
        /// Create an error with the given fields and span.
        pub fn $fn_name(
            $f1: impl Into<String>,
            $f2: impl Into<String>,
            $f3: impl Into<String>,
            span: $crate::Span,
        ) -> Self {
            Self::$variant {
                $f1: $f1.into(),
                $f2: $f2.into(),
                $f3: $f3.into(),
                span: span.to_source_span(),
            }
        }
    };

    // Four String fields + span
    ($fn_name:ident, $variant:ident { $f1:ident : String, $f2:ident : String, $f3:ident : String, $f4:ident : String }) => {
        /// Create an error with the given fields and span.
        pub fn $fn_name(
            $f1: impl Into<String>,
            $f2: impl Into<String>,
            $f3: impl Into<String>,
            $f4: impl Into<String>,
            span: $crate::Span,
        ) -> Self {
            Self::$variant {
                $f1: $f1.into(),
                $f2: $f2.into(),
                $f3: $f3.into(),
                $f4: $f4.into(),
                span: span.to_source_span(),
            }
        }
    };

    // Five String fields + span
    ($fn_name:ident, $variant:ident { $f1:ident : String, $f2:ident : String, $f3:ident : String, $f4:ident : String, $f5:ident : String }) => {
        /// Create an error with the given fields and span.
        pub fn $fn_name(
            $f1: impl Into<String>,
            $f2: impl Into<String>,
            $f3: impl Into<String>,
            $f4: impl Into<String>,
            $f5: impl Into<String>,
            span: $crate::Span,
        ) -> Self {
            Self::$variant {
                $f1: $f1.into(),
                $f2: $f2.into(),
                $f3: $f3.into(),
                $f4: $f4.into(),
                $f5: $f5.into(),
                span: span.to_source_span(),
            }
        }
    };

    // Span only (no other fields)
    ($fn_name:ident, $variant:ident {}) => {
        /// Create an error with the given span.
        pub fn $fn_name(span: $crate::Span) -> Self {
            Self::$variant {
                span: span.to_source_span(),
            }
        }
    };

    // Single String field, no span
    ($fn_name:ident, $variant:ident, no_span { $field:ident : String }) => {
        /// Create an error with the given field.
        pub fn $fn_name($field: impl Into<String>) -> Self {
            Self::$variant($field.into())
        }
    };
}

#[cfg(test)]
pub mod error_macro_tests {
    use super::*;

    // Test enum for the macro
    #[derive(Debug, Clone)]
    pub enum TestError {
        SingleField {
            name: String,
            span: SourceSpan,
        },
        TwoFields {
            a: String,
            b: String,
            span: SourceSpan,
        },
        SpanOnly {
            span: SourceSpan,
        },
    }

    impl TestError {
        error_constructor!(single_field, SingleField { name: String });
        error_constructor!(
            two_fields,
            TwoFields {
                a: String,
                b: String
            }
        );
        error_constructor!(span_only, SpanOnly {});
    }

    #[test]
    fn test_single_field_constructor() {
        let span = Span::from_offsets(SourceId(0), 10, 20);
        let err = TestError::single_field("test_name", span);
        match err {
            TestError::SingleField { name, span: s } => {
                assert_eq!(name, "test_name");
                assert_eq!(s.offset(), 10);
                assert_eq!(s.len(), 10);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_two_fields_constructor() {
        let span = Span::from_offsets(SourceId(0), 10, 20);
        let err = TestError::two_fields("first", "second", span);
        match err {
            TestError::TwoFields { a, b, span: s } => {
                assert_eq!(a, "first");
                assert_eq!(b, "second");
                assert_eq!(s.offset(), 10);
            }
            _ => panic!("Wrong variant"),
        }
    }

    #[test]
    fn test_span_only_constructor() {
        let span = Span::from_offsets(SourceId(0), 10, 20);
        let err = TestError::span_only(span);
        match err {
            TestError::SpanOnly { span: s } => {
                assert_eq!(s.offset(), 10);
                assert_eq!(s.len(), 10);
            }
            _ => panic!("Wrong variant"),
        }
    }
}
