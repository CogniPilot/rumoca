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
//! use rumoca_core::{BoxedResult, SourceSpan, error_constructor};
//! use rumoca_core::Span;
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

use indexmap::IndexSet;
use miette::{Diagnostic as MietteDiagnostic, LabeledSpan, NamedSource, Severity};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

// IR vocabulary and foundation primitives (DefId, Span, Expression, ...).
// Previously lived in `rumoca-ir-core`; merged here per SPEC_0029 §3a.
mod expression_rewriter;
mod expression_visitor;
mod ir_primitives;
mod modelica_builtins;
mod statement_rewriter;
mod subscript;
pub use expression_rewriter::ExpressionRewriter;
pub use expression_visitor::{ExpressionScope, ExpressionVisitor, FallibleExpressionVisitor};
pub use ir_primitives::*;
pub use modelica_builtins::*;
pub use statement_rewriter::StatementRewriter;
pub use subscript::Subscript;

/// Internal DAE-level sample tick callable emitted after source `sample(...)`
/// has been lowered out of the DAE expression language.
pub const INTERNAL_SAMPLE_FUNCTION_NAME: &str = "__rumoca_sample";

/// MLS §16.5.1: `sample(u)` is the clocked value-sampling operator with an
/// inferred clock. It is not the event-generating `sample(start, interval)`
/// form from MLS §3.7.3 / §16.
pub fn sample_call_is_inferred_clock_value_form(args: &[Expression]) -> bool {
    args.len() == 1
}

/// Return the canonical name for source temporal function-call operators that
/// must be lowered before DAE/Solve exits.
pub fn source_temporal_function_name(name: &str) -> Option<&'static str> {
    match name {
        "pre" => Some("pre"),
        "edge" => Some("edge"),
        "change" => Some("change"),
        "sample" => Some("sample"),
        "previous" => Some("previous"),
        "reinit" => Some("reinit"),
        _ => None,
    }
}

/// Return the canonical name for source temporal function-call operators after
/// removing a qualified package prefix.
pub fn source_temporal_function_short_name(name: &str) -> Option<&'static str> {
    source_temporal_function_name(top_level_last_segment(name))
}

/// Return the canonical name for source-level runtime flow actions.
pub fn runtime_flow_action_function_name(name: &str) -> Option<&'static str> {
    match name {
        "assert" => Some("assert"),
        "terminate" => Some("terminate"),
        "reinit" => Some("reinit"),
        _ => None,
    }
}

/// Return the canonical runtime flow action name after removing a qualified
/// package prefix.
pub fn runtime_flow_action_function_short_name(name: &str) -> Option<&'static str> {
    runtime_flow_action_function_name(top_level_last_segment(name))
}

/// Return the canonical name for source temporal builtin operators that must
/// be lowered before DAE/Solve exits.
pub fn source_temporal_builtin_name(function: BuiltinFunction) -> Option<&'static str> {
    match function {
        BuiltinFunction::Pre => Some("pre"),
        BuiltinFunction::Edge => Some("edge"),
        BuiltinFunction::Change => Some("change"),
        BuiltinFunction::Sample => Some("sample"),
        BuiltinFunction::Reinit => Some("reinit"),
        _ => None,
    }
}

pub mod eval_lookup;
pub use eval_lookup::EvalLookup;
pub mod enum_compare;
pub use enum_compare::enum_values_equal;
pub mod integer_binary;
pub use integer_binary::{IntegerBinaryOperator, eval_ast_integer_binary, eval_integer_binary};
pub mod integer_division;
pub use integer_division::{eval_integer_div_builtin, eval_integer_slash};
pub mod timing;
pub use timing::{
    OptionalTimer, maybe_elapsed_duration, maybe_elapsed_ms, maybe_elapsed_seconds,
    maybe_start_timer, maybe_start_timer_if,
};

/// Resolve the workspace root from a crate manifest directory.
///
/// For crates under `<workspace>/crates/*`, this returns `<workspace>`.
pub fn workspace_root_from_manifest_dir(manifest_dir: &str) -> PathBuf {
    PathBuf::from(manifest_dir).join("../..")
}

/// Resolve the MSL cache directory: `<workspace>/target/msl`.
pub fn msl_cache_dir_from_manifest(manifest_dir: &str) -> PathBuf {
    workspace_root_from_manifest_dir(manifest_dir).join("target/msl")
}

// =============================================================================
// Path Utilities (shared across compiler phases)
// =============================================================================

/// Split a dotted path while preserving dots inside bracket expressions.
///
/// For `bus[data.medium].pin.v`, returns `["bus[data.medium]", "pin", "v"]`.
pub fn split_path_with_indices(path: &str) -> Vec<&str> {
    let mut parts = Vec::with_capacity(4);
    visit_top_level_path_segments(path, |segment| parts.push(segment));
    parts
}

/// Visit non-empty top-level dotted path segments while preserving dots inside
/// bracket expressions.
pub fn visit_top_level_path_segments<'a>(path: &'a str, mut visit: impl FnMut(&'a str)) {
    let mut start = 0;
    let mut bracket_depth = 0usize;
    for (idx, byte) in path.bytes().enumerate() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => {
                if start < idx {
                    visit(&path[start..idx]);
                }
                start = idx + 1;
            }
            _ => {}
        }
    }
    if start < path.len() {
        visit(&path[start..]);
    }
}

/// Return the byte index of the last top-level `.` in `path`.
///
/// Dots inside bracketed subscripts (e.g. `a[b.c]`) are ignored.
pub fn find_last_top_level_dot(path: &str) -> Option<usize> {
    let bytes = path.as_bytes();
    let mut idx = bytes.len();
    while idx > 0 {
        idx -= 1;
        match bytes[idx] {
            b'.' => return Some(idx),
            b']' => break,
            _ => {}
        }
    }
    if idx == 0 && bytes.first().copied() != Some(b']') {
        return None;
    }

    let mut bracket_depth = 0usize;
    for (idx, byte) in path.bytes().enumerate().rev() {
        match byte {
            b']' => bracket_depth += 1,
            b'[' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => return Some(idx),
            _ => {}
        }
    }
    None
}

/// Return the byte index of the first top-level `.` in `path`.
///
/// Dots inside bracketed subscripts (e.g. `a[b.c]`) are ignored.
pub fn find_first_top_level_dot(path: &str) -> Option<usize> {
    let mut bracket_depth = 0usize;
    for (idx, byte) in path.bytes().enumerate() {
        match byte {
            b'[' => bracket_depth += 1,
            b']' => bracket_depth = bracket_depth.saturating_sub(1),
            b'.' if bracket_depth == 0 => return Some(idx),
            _ => {}
        }
    }
    None
}

/// True when `path` has at least one top-level `.`.
pub fn has_top_level_dot(path: &str) -> bool {
    find_last_top_level_dot(path).is_some()
}

/// Return the final top-level segment of `path`.
pub fn top_level_last_segment(path: &str) -> &str {
    find_last_top_level_dot(path)
        .map(|dot_idx| &path[dot_idx + 1..])
        .unwrap_or(path)
}

/// Return the parent scope prefix of `path` (before the last top-level `.`).
pub fn parent_scope(path: &str) -> Option<&str> {
    find_last_top_level_dot(path).map(|dot_idx| &path[..dot_idx])
}

/// Split `path` at the first top-level `.`.
pub fn split_first_top_level(path: &str) -> Option<(&str, &str)> {
    find_first_top_level_dot(path).map(|dot_idx| (&path[..dot_idx], &path[dot_idx + 1..]))
}

/// Split `path` at the last top-level `.`.
pub fn split_last_top_level(path: &str) -> Option<(&str, &str)> {
    find_last_top_level_dot(path).map(|dot_idx| (&path[..dot_idx], &path[dot_idx + 1..]))
}

/// Visit top-level dotted split points from right to left and return the first
/// value produced by `f`.
///
/// For `a.b.c`, calls `f("a.b", "c")` and then `f("a", "b.c")`.
/// Dots inside bracketed subscripts are ignored.
pub fn find_map_top_level_splits_rev<'a, T>(
    path: &'a str,
    mut f: impl FnMut(&'a str, &'a str) -> Option<T>,
) -> Option<T> {
    let mut end = path.len();
    while let Some(dot_idx) = find_last_top_level_dot(&path[..end]) {
        let base = &path[..dot_idx];
        let suffix = &path[dot_idx + 1..];
        if let Some(value) = f(base, suffix) {
            return Some(value);
        }
        end = dot_idx;
    }
    None
}

/// Return true when `candidate` ends with `suffix` at whole top-level path
/// segment boundaries.
///
/// The suffix may be supplied with or without a leading top-level dot. Dots
/// inside bracketed subscripts are ignored when segmenting either path.
pub fn top_level_path_ends_with(candidate: &str, suffix: &str) -> bool {
    let suffix = suffix.strip_prefix('.').unwrap_or(suffix);
    if suffix.is_empty() {
        return false;
    }
    let mut candidate_end = candidate.len();
    let mut suffix_end = suffix.len();
    loop {
        match (
            previous_top_level_path_segment(candidate, &mut candidate_end),
            previous_top_level_path_segment(suffix, &mut suffix_end),
        ) {
            (_, None) => return true,
            (Some(candidate_part), Some(suffix_part)) if candidate_part == suffix_part => {}
            _ => return false,
        }
    }
}

fn previous_top_level_path_segment<'a>(path: &'a str, end: &mut usize) -> Option<&'a str> {
    while *end > 0 {
        let segment_end = *end;
        let Some(dot_idx) = find_last_top_level_dot(&path[..segment_end]) else {
            *end = 0;
            return (segment_end > 0).then_some(&path[..segment_end]);
        };
        *end = dot_idx;
        if dot_idx + 1 < segment_end {
            return Some(&path[dot_idx + 1..segment_end]);
        }
    }
    None
}

/// Return true when two qualified type names refer to the same leaf type.
///
/// This is a compatibility helper for phase boundaries that still carry type
/// names as strings. It requires whole top-level path segment matches, so
/// `A.B.Record` matches `Record`, but `MyRecord` does not match `Record`.
pub fn qualified_type_name_matches(candidate: &str, expected: &str) -> bool {
    top_level_path_ends_with(candidate, expected)
}

/// True when any top-level path segment equals `segment`.
pub fn top_level_path_contains_segment(path: &str, segment: &str) -> bool {
    let mut found = false;
    visit_top_level_path_segments(path, |part| found |= part == segment);
    found
}

/// Strip array indexing from one path segment.
///
/// Examples:
/// - `resistor[1]` -> `resistor`
/// - `p` -> `p`
pub fn strip_array_index(segment: &str) -> &str {
    let mut base = segment;
    while let Some((next_base, _subscript)) = split_trailing_subscript_suffix(base) {
        base = next_base;
    }
    base
}

/// Return the first top-level path segment with array indices removed.
pub fn first_path_segment_without_index(path: &str) -> Option<&str> {
    rendered_top_level_segment(path).map(strip_array_index)
}

/// Return the top-level segment of an already-rendered path, preserving any
/// index expression.
///
/// For `bus[data.medium].pin.v`, returns `bus[data.medium]`.
pub fn rendered_top_level_segment(path: &str) -> Option<&str> {
    let mut depth = 0usize;
    for (idx, ch) in path.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => depth = depth.checked_sub(1)?,
            '.' if depth == 0 => return Some(&path[..idx]),
            _ => {}
        }
    }
    (depth == 0 && !path.is_empty()).then_some(path)
}

/// Strip array indexing from a top-level path segment.
///
/// For `plug[1]` returns `plug`.
pub fn normalize_top_level_segment(segment: &str) -> &str {
    strip_array_index(segment)
}

/// Extract the normalized top-level prefix from a variable path.
pub fn get_top_level_prefix(path: &str) -> Option<String> {
    rendered_top_level_segment(path).map(|segment| normalize_top_level_segment(segment).to_string())
}

/// Strip all bracketed subscript groups from a path while preserving dots and identifiers.
///
/// Examples:
/// - `pc[2*i-1].i` -> `pc.i`
/// - `pin_n[1].v` -> `pin_n.v`
/// - `R[1].w` -> `R.w`
pub fn strip_all_subscripts(path: &str) -> String {
    let mut out = String::with_capacity(path.len());
    let mut depth = 0usize;
    for ch in path.chars() {
        match ch {
            '[' => depth += 1,
            ']' if depth > 0 => depth -= 1,
            ']' => {}
            _ if depth == 0 => out.push(ch),
            _ => {}
        }
    }
    out
}

/// Normalize top-level names by stripping array indexing from each entry.
pub fn normalized_top_level_names<'a>(names: impl Iterator<Item = &'a String>) -> IndexSet<String> {
    names
        .map(|name| normalize_top_level_segment(name).to_string())
        .collect()
}

/// Check whether a path belongs to a known top-level component set.
///
/// The provided set is expected to contain normalized top-level names.
pub fn path_is_in_top_level_set(path: &str, normalized_top_level_names: &IndexSet<String>) -> bool {
    get_top_level_prefix(path)
        .is_some_and(|prefix| normalized_top_level_names.contains(prefix.as_str()))
}

/// Check whether a variable belongs to a top-level member path.
pub fn is_top_level_member(name: &VarName, normalized_top_level_names: &IndexSet<String>) -> bool {
    has_top_level_dot(name.as_str())
        && path_is_in_top_level_set(name.as_str(), normalized_top_level_names)
}

/// Strip the last top-level array subscript group from a variable name.
pub fn strip_subscript(name: &str) -> Option<VarName> {
    let s = name;
    let (start, end) = last_top_level_subscript_span(s)?;
    let mut out = String::with_capacity(s.len());
    out.push_str(&s[..start]);
    out.push_str(&s[end + 1..]);
    Some(VarName::new(out))
}

/// Build a fallback chain by repeatedly stripping one top-level subscript group.
pub fn subscript_fallback_chain(name: &str) -> Vec<VarName> {
    let mut chain = Vec::new();
    let mut current = VarName::new(name);

    while let Some(stripped) = strip_subscript(current.as_str()) {
        if stripped == current {
            break;
        }
        chain.push(stripped.clone());
        current = stripped;
    }

    chain
}

/// Return the byte-span of the last top-level `[ ... ]` group in a name.
pub fn last_top_level_subscript_span(name: &str) -> Option<(usize, usize)> {
    let mut depth = 0usize;
    let mut group_start = None;
    let mut last_group = None;

    for (idx, ch) in name.char_indices() {
        match ch {
            '[' => {
                if depth == 0 {
                    group_start = Some(idx);
                }
                depth += 1;
            }
            ']' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    let start = group_start?;
                    last_group = Some((start, idx));
                }
            }
            _ => {}
        }
    }

    (depth == 0).then_some(last_group).flatten()
}

/// True when `name` contains a balanced top-level subscript group.
pub fn has_top_level_subscript(name: &str) -> bool {
    last_top_level_subscript_span(name).is_some()
}

/// Convert an IR source span to miette's SourceSpan for error reporting.
///
/// This is used by phase-specific error types to create miette diagnostics
/// without making `rumoca-ir-core` depend on `miette`.
pub fn span_to_source_span(span: Span) -> SourceSpan {
    let start = span.start.0;
    let len = span.end.0.saturating_sub(span.start.0);
    (start, len).into()
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
    "Clock",
    // Event/state functions (MLS §3.7.3)
    "der",
    "pre",
    "previous",
    "edge",
    "change",
    "initial",
    "terminal",
    "sample",
    "hold",
    "subSample",
    "superSample",
    "shiftSample",
    "backSample",
    "noClock",
    "firstTick",
    "interval",
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
    "array",
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
    matches!(
        name,
        "Real"
            | "Integer"
            | "Boolean"
            | "String"
            | "ExternalObject"
            | "Clock"
            | "StateSelect"
            | "AssertionLevel"
    )
}

/// Check if a name is a built-in function.
pub fn is_builtin_function(name: &str) -> bool {
    matches!(
        name,
        "Real"
            | "Integer"
            | "Boolean"
            | "String"
            | "Clock"
            | "der"
            | "pre"
            | "previous"
            | "edge"
            | "change"
            | "initial"
            | "terminal"
            | "sample"
            | "hold"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
            | "noClock"
            | "firstTick"
            | "interval"
            | "smooth"
            | "delay"
            | "cardinality"
            | "homotopy"
            | "semiLinear"
            | "inStream"
            | "actualStream"
            | "getInstanceName"
            | "spatialDistribution"
            | "reinit"
            | "assert"
            | "terminate"
            | "abs"
            | "sign"
            | "sqrt"
            | "sin"
            | "cos"
            | "tan"
            | "asin"
            | "acos"
            | "atan"
            | "atan2"
            | "sinh"
            | "cosh"
            | "tanh"
            | "exp"
            | "log"
            | "log10"
            | "floor"
            | "ceil"
            | "mod"
            | "rem"
            | "div"
            | "integer"
            | "size"
            | "ndims"
            | "scalar"
            | "vector"
            | "matrix"
            | "transpose"
            | "outerProduct"
            | "symmetric"
            | "cross"
            | "skew"
            | "identity"
            | "diagonal"
            | "zeros"
            | "ones"
            | "fill"
            | "linspace"
            | "min"
            | "max"
            | "sum"
            | "product"
            | "cat"
            | "array"
            | "noEvent"
            | "connect"
    )
}

/// Check if a name is a built-in variable.
pub fn is_builtin_variable(name: &str) -> bool {
    matches!(name, "time" | "Connections")
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
    /// (stable source id, name, content) in deterministic insertion order.
    files: Vec<(SourceId, String, Arc<str>)>,
    /// Reverse lookup from file name to SourceId.
    #[serde(skip)]
    name_to_id: HashMap<String, SourceId>,
    /// Reverse lookup from SourceId to `files` index.
    #[serde(skip)]
    id_to_index: HashMap<SourceId, usize>,
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
        self.add_shared(name, Arc::<str>::from(content))
    }

    /// Add a source file using shared source content and return its SourceId.
    ///
    /// This lets LSP/session caches share source text with diagnostics instead
    /// of copying whole files into every `SourceMap`.
    pub fn add_shared(&mut self, name: &str, content: Arc<str>) -> SourceId {
        if let Some(&id) = self.name_to_id.get(name) {
            return id;
        }
        if let Some((id, _, _)) = self
            .files
            .iter()
            .find(|(_, file_name, _)| file_name == name)
        {
            self.name_to_id.insert(name.to_string(), *id);
            return *id;
        }
        let id = SourceId::from_source_name(name);
        if self.id_to_index.contains_key(&id)
            || self.files.iter().any(|(source_id, _, _)| *source_id == id)
        {
            self.name_to_id.insert(name.to_string(), id);
            return id;
        }
        let index = self.files.len();
        self.files.push((id, name.to_string(), content));
        self.name_to_id.insert(name.to_string(), id);
        self.id_to_index.insert(id, index);
        id
    }

    /// Look up a SourceId by file name.
    pub fn get_id(&self, name: &str) -> Option<SourceId> {
        self.name_to_id.get(name).copied().or_else(|| {
            self.files
                .iter()
                .find(|(_, file_name, _)| file_name == name)
                .map(|(id, _, _)| *id)
                .or_else(|| {
                    let id = SourceId::from_source_name(name);
                    self.get_source(id).map(|_| id)
                })
        })
    }

    /// Get (name, content) for a SourceId.
    pub fn get_source(&self, id: SourceId) -> Option<(&str, &str)> {
        self.id_to_index
            .get(&id)
            .and_then(|&index| self.files.get(index))
            .or_else(|| self.files.iter().find(|(source_id, _, _)| *source_id == id))
            .map(|(_, name, content)| (name.as_str(), content.as_ref()))
    }

    /// Get the first source id in deterministic map order.
    pub fn first_source_id(&self) -> Option<SourceId> {
        self.files.first().map(|(id, _, _)| *id)
    }

    /// Create a Span from a file name and byte offsets.
    ///
    /// Looks up the SourceId from the file name. Returns `Span::DUMMY` when
    /// the file is not found so diagnostics do not get misattributed to the
    /// first source file in the map.
    pub fn location_to_span(&self, file_name: &str, start: usize, end: usize) -> Span {
        self.try_location_to_span(file_name, start, end)
            .unwrap_or(Span::DUMMY)
    }

    /// Try to create a Span from a file name and byte offsets.
    pub fn try_location_to_span(&self, file_name: &str, start: usize, end: usize) -> Option<Span> {
        let source_id = self.name_to_id.get(file_name).copied().or_else(|| {
            self.get_source(SourceId::from_source_name(file_name))
                .map(|_| SourceId::from_source_name(file_name))
        })?;
        Some(Span::from_offsets(source_id, start, end))
    }

    /// Rebuild the name_to_id index after deserialization.
    pub fn rebuild_index(&mut self) {
        self.name_to_id.clear();
        self.id_to_index.clear();
        for (i, (id, name, _)) in self.files.iter().enumerate() {
            self.name_to_id.insert(name.clone(), *id);
            self.id_to_index.insert(*id, i);
        }
    }

    /// Snapshot file-name to source-id mappings.
    pub fn source_ids(&self) -> HashMap<String, SourceId> {
        if !self.name_to_id.is_empty() {
            return self.name_to_id.clone();
        }
        self.files
            .iter()
            .map(|(id, name, _)| (name.clone(), *id))
            .collect()
    }

    /// Return a copy that preserves source-id/name mappings but omits source text.
    pub fn without_source_contents(&self) -> Self {
        let files = self
            .files
            .iter()
            .map(|(id, name, _)| (*id, name.clone(), Arc::<str>::from("")))
            .collect();
        let mut source_map = Self {
            files,
            name_to_id: HashMap::new(),
            id_to_index: HashMap::new(),
        };
        source_map.rebuild_index();
        source_map
    }
}

/// Severity level for diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
    Note,
}

/// A label pointing to a span in source code.
#[derive(Debug, Clone, Serialize, Deserialize)]
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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

    /// Source span covered by this label.
    pub fn span(&self) -> Span {
        self.span
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
#[derive(Debug, Clone, Serialize, Deserialize)]
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

    /// Convert to a miette report using the source map for automatic source lookup.
    ///
    /// Looks up the source file from the primary label's SourceId. Falls back
    /// to the first label, then to empty source if no label source is found.
    pub fn to_miette_with_source_map(&self, source_map: &SourceMap) -> MietteReport {
        let source_id = self
            .labels
            .iter()
            .find(|label| label.primary)
            .or_else(|| self.labels.first())
            .map(|l| l.span.source)
            .and_then(|source_id| {
                source_map
                    .get_source(source_id)
                    .map(|source| (source_id, source))
            });
        match source_id {
            Some((source_id, (name, content))) => {
                self.to_miette_filtered(name, content, Some(source_id))
            }
            None => self.to_miette_filtered("unknown", "", None),
        }
    }

    pub fn to_miette(&self, source_name: &str, source: &str) -> MietteReport {
        self.to_miette_filtered(source_name, source, None)
    }

    fn to_miette_filtered(
        &self,
        source_name: &str,
        source: &str,
        source_id: Option<SourceId>,
    ) -> MietteReport {
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
                .filter(|label| source_id.is_none_or(|source_id| label.span.source == source_id))
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
#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Diagnostics {
    diags: Vec<Diagnostic>,
}

impl Diagnostics {
    /// Create a new empty diagnostics collection.
    pub fn new() -> Self {
        Self::default()
    }

    /// Emit a diagnostic.
    pub fn emit(&mut self, diag: Diagnostic) {
        self.diags.push(diag);
    }

    /// Check if any errors were emitted.
    pub fn has_errors(&self) -> bool {
        self.diags.iter().any(Diagnostic::is_error)
    }

    /// Get the number of errors emitted.
    pub fn error_count(&self) -> usize {
        self.diags.iter().filter(|diag| diag.is_error()).count()
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
                span: $crate::span_to_source_span(span),
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
                span: $crate::span_to_source_span(span),
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
                span: $crate::span_to_source_span(span),
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
                span: $crate::span_to_source_span(span),
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
                span: $crate::span_to_source_span(span),
            }
        }
    };

    // Span only (no other fields)
    ($fn_name:ident, $variant:ident {}) => {
        /// Create an error with the given span.
        pub fn $fn_name(span: $crate::Span) -> Self {
            Self::$variant {
                span: $crate::span_to_source_span(span),
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
mod path_utils_tests {
    use super::*;

    #[test]
    fn rendered_top_level_segment_preserves_dots_inside_subscripts() {
        assert_eq!(
            rendered_top_level_segment("bus[data.medium].pin.v"),
            Some("bus[data.medium]")
        );
        assert_eq!(
            first_path_segment_without_index("plug_p[a.b].pin[2].i"),
            Some("plug_p")
        );
    }

    #[test]
    fn rendered_top_level_segment_rejects_unbalanced_brackets() {
        assert_eq!(rendered_top_level_segment("arr[record.value.field"), None);
        assert_eq!(rendered_top_level_segment("arr].field"), None);
        assert_eq!(get_top_level_prefix("arr[record.value.field"), None);
    }

    #[test]
    fn normalized_top_level_names_deduplicates_indices() {
        let names = [
            "plug".to_string(),
            "plug[1]".to_string(),
            "frame_a[2]".to_string(),
        ];
        let normalized = normalized_top_level_names(names.iter());
        assert!(normalized.contains("plug"));
        assert!(normalized.contains("frame_a"));
        assert_eq!(normalized.len(), 2);
    }

    #[test]
    fn qualified_type_name_matching_requires_segment_boundaries() {
        assert!(qualified_type_name_matches("Modelica.Complex", "Complex"));
        assert!(qualified_type_name_matches(
            "Modelica.Units.SI.ComplexVoltage",
            "Units.SI.ComplexVoltage"
        ));
        assert!(qualified_type_name_matches(
            "recordArray[1].Nested.Type",
            "Nested.Type"
        ));
        assert!(!qualified_type_name_matches("MyComplex", "Complex"));
        assert!(!qualified_type_name_matches("A.B", "A.B.C"));
    }

    #[test]
    fn top_level_path_suffix_matching_requires_segment_boundaries() {
        assert!(top_level_path_ends_with("source.medium.nXi", ".medium.nXi"));
        assert!(top_level_path_ends_with("source.medium.nXi", "medium.nXi"));
        assert!(top_level_path_ends_with("a[index.with.dot].b.c", "b.c"));
        assert!(top_level_path_ends_with("source..medium.nXi", "medium.nXi"));
        assert!(!top_level_path_ends_with(
            "source.amediumnXi",
            ".medium.nXi"
        ));
        assert!(!top_level_path_ends_with("source.medium.nXi", "ium.nXi"));
        assert!(!top_level_path_ends_with("source.medium.nXi", ""));
    }

    #[test]
    fn split_last_top_level_ignores_dots_inside_brackets() {
        assert_eq!(
            split_last_top_level("a[index.with.dot].b.c"),
            Some(("a[index.with.dot].b", "c"))
        );
        assert_eq!(
            split_last_top_level("a.b[index.with.dot]"),
            Some(("a", "b[index.with.dot]"))
        );
        assert_eq!(split_last_top_level("a[index.with.dot]"), None);
    }

    #[test]
    fn find_map_top_level_splits_rev_ignores_dots_inside_brackets() {
        let mut splits = Vec::new();
        let found = find_map_top_level_splits_rev("a[index.with.dot].b.c", |base, suffix| {
            splits.push((base.to_string(), suffix.to_string()));
            (base == "a[index.with.dot]").then_some((base.to_string(), suffix.to_string()))
        });
        assert_eq!(
            splits,
            vec![
                ("a[index.with.dot].b".to_string(), "c".to_string()),
                ("a[index.with.dot]".to_string(), "b.c".to_string()),
            ]
        );
        assert_eq!(
            found,
            Some(("a[index.with.dot]".to_string(), "b.c".to_string()))
        );
    }

    #[test]
    fn subscript_fallback_chain_peels_all_subscript_layers() {
        let chain = subscript_fallback_chain("a[1].b[2].c[3]");
        assert_eq!(
            chain,
            vec![
                VarName::new("a[1].b[2].c"),
                VarName::new("a[1].b.c"),
                VarName::new("a.b.c")
            ]
        );
    }

    #[test]
    fn top_level_subscript_detection_requires_balanced_brackets() {
        assert!(has_top_level_subscript("a[index.with.dot]"));
        assert!(has_top_level_subscript("a[1].b"));
        assert!(!has_top_level_subscript("a"));
        assert!(!has_top_level_subscript("a[1"));
        assert!(!has_top_level_subscript("a]"));
    }

    #[test]
    fn strip_all_subscripts_normalizes_embedded_indices() {
        assert_eq!(strip_all_subscripts("pc[((2*1)-1)].i"), "pc.i");
        assert_eq!(strip_all_subscripts("pin_n[1].v"), "pin_n.v");
        assert_eq!(strip_all_subscripts("R[1].w"), "R.w");
    }
}

#[cfg(test)]
mod source_map_tests {
    use super::*;

    #[test]
    fn location_to_span_does_not_misattribute_unknown_files_to_source_zero() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("known.mo", "model Known end Known;");

        assert_eq!(
            source_map.location_to_span("known.mo", 1, 5),
            Span::from_offsets(source, 1, 5)
        );
        assert_eq!(source_map.location_to_span("missing.mo", 1, 5), Span::DUMMY);
        assert_eq!(source_map.try_location_to_span("missing.mo", 1, 5), None);
    }

    #[test]
    fn source_ids_are_stable_across_source_map_insertion_order() {
        let mut first_order = SourceMap::new();
        let first_known = first_order.add("known.mo", "model Known end Known;");
        first_order.add("other.mo", "model Other end Other;");

        let mut second_order = SourceMap::new();
        second_order.add("other.mo", "model Other end Other;");
        let second_known = second_order.add("known.mo", "model Known end Known;");

        assert_eq!(first_known, second_known);
        assert_eq!(first_known, SourceId::from_source_name("known.mo"));
    }

    #[test]
    fn miette_report_uses_primary_label_source_before_first_label() {
        let mut source_map = SourceMap::new();
        let first = source_map.add("first.mo", "model First end First;");
        let second = source_map.add("second.mo", "model Second end Second;");
        let diagnostic = Diagnostic::error(
            "E000",
            "primary source selection",
            PrimaryLabel::new(Span::from_offsets(second, 6, 12)).with_message("primary source"),
        )
        .with_label(Label::secondary(Span::from_offsets(first, 0, 5)).with_message("secondary"));

        let report = diagnostic.to_miette_with_source_map(&source_map);
        assert_eq!(report.labels.len(), 1);
        assert_eq!(report.labels[0].label(), Some("primary source"));
        assert_eq!(report.labels[0].offset(), 6);
    }
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

#[cfg(test)]
mod builtin_registry_tests {
    use super::BUILTIN_FUNCTIONS;

    #[test]
    fn builtin_registry_covers_ir_builtin_call_variants() {
        for builtin in crate::BuiltinFunction::ALL {
            assert!(
                BUILTIN_FUNCTIONS.contains(&builtin.name()),
                "{} is an IR builtin but is missing from rumoca-core::BUILTIN_FUNCTIONS",
                builtin.name()
            );
        }
    }
}
