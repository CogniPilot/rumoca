//! Shared IR primitives used by multiple Rumoca IR crates.

use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::{Arc, OnceLock, RwLock};

use crate::{Subscript, split_path_with_indices};

mod component_refs_and_functions;
pub use component_refs_and_functions::*;

mod generated_names;
pub use generated_names::*;

mod reference_serde;

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

impl Display for DefId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "DefId({})", self.0)
    }
}

/// Identity of one exposed function in flattened model scope.
///
/// Unlike a source [`DefId`], this distinguishes inherited or redeclared
/// function instances that originate from the same declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct FunctionInstanceId(pub u32);

impl FunctionInstanceId {
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> u32 {
        self.0
    }
}

/// Resolved function target plus the structured base-path boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ResolvedFunctionReference {
    pub instance_id: FunctionInstanceId,
    pub base_part_count: usize,
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

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

impl Display for ScopeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_global() {
            write!(f, "ScopeId(GLOBAL)")
        } else {
            write!(f, "ScopeId({})", self.0)
        }
    }
}

/// A source file identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct SourceId(pub u64);

impl SourceId {
    /// Reserved source id for compiler-generated constructs or missing source
    /// information.
    pub const DUMMY: Self = Self(0);

    /// Build a stable source identity from a source name.
    ///
    /// Source ids are intentionally not `SourceMap` insertion indexes: AST
    /// spans are created by the parser before documents are merged, so the id
    /// must survive session/source-map reconstruction without rebasing.
    pub fn from_source_name(name: &str) -> Self {
        if name.is_empty() {
            return Self::DUMMY;
        }
        let mut hash = 0xcbf2_9ce4_8422_2325_u64;
        for byte in normalized_source_name_bytes(name) {
            hash ^= u64::from(byte);
            hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        }
        if hash == 0 { Self(1) } else { Self(hash) }
    }
}

fn normalized_source_name_bytes(name: &str) -> impl Iterator<Item = u8> + '_ {
    name.bytes()
        .map(|byte| if byte == b'\\' { b'/' } else { byte })
}

/// A byte position in source code.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Serialize, Deserialize,
)]
pub struct BytePos(pub usize);

/// Marker prefix used to encode a named function argument
/// (`f(x = expr)`) as a `FunctionCall { name: "__rumoca_named_arg__.x" }`
/// node in the flat IR.
pub const NAMED_FUNCTION_ARG_PREFIX: &str = "__rumoca_named_arg__.";

/// A span in source code (source, start, end).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Span {
    pub source: SourceId,
    pub start: BytePos,
    pub end: BytePos,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProvenanceSpan {
    span: Span,
}

impl ProvenanceSpan {
    pub fn new(span: Span, context: &'static str) -> Result<Self, MissingProvenanceSpan> {
        if span.is_dummy() {
            Err(MissingProvenanceSpan { context })
        } else {
            Ok(Self { span })
        }
    }

    pub fn span(self) -> Span {
        self.span
    }
}

impl From<ProvenanceSpan> for Span {
    fn from(value: ProvenanceSpan) -> Self {
        value.span
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MissingProvenanceSpan {
    context: &'static str,
}

impl MissingProvenanceSpan {
    pub fn new(context: &'static str) -> Self {
        Self { context }
    }

    pub fn context(&self) -> &'static str {
        self.context
    }
}

impl std::fmt::Display for MissingProvenanceSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "missing source provenance for {}", self.context)
    }
}

impl std::error::Error for MissingProvenanceSpan {}

impl Span {
    /// A dummy span for explicitly source-free constructs.
    ///
    /// Generated source-derived IR should use the nearest owner/context span
    /// instead of this sentinel.
    pub const DUMMY: Span = Span {
        source: SourceId::DUMMY,
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

    /// True when this span is the compiler-generated dummy sentinel.
    pub fn is_dummy(&self) -> bool {
        *self == Self::DUMMY
    }

    pub fn source_free_serde_default() -> Self {
        Self::DUMMY
    }

    pub fn require_provenance(
        self,
        context: &'static str,
    ) -> Result<ProvenanceSpan, MissingProvenanceSpan> {
        ProvenanceSpan::new(self, context)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Location {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub start: u32,
    pub end: u32,
    pub file_name: String,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}",
            self.file_name, self.start_line, self.start_column
        )
    }
}

#[derive(Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Token {
    /// Token text.
    pub text: Arc<str>,
    /// Source location.
    pub location: Location,
    pub token_number: u32,
    pub token_type: u16,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.text)
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum OpBinary {
    Empty,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Exp,
    ExpElem,
    AddElem,
    SubElem,
    MulElem,
    DivElem,
    Assign,
}

impl OpBinary {
    pub fn is_relational(&self) -> bool {
        matches!(
            self,
            Self::Lt | Self::Le | Self::Gt | Self::Ge | Self::Eq | Self::Neq
        )
    }
}

impl Display for OpBinary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpBinary::Empty => write!(f, ""),
            OpBinary::Add => write!(f, "+"),
            OpBinary::Sub => write!(f, "-"),
            OpBinary::Mul => write!(f, "*"),
            OpBinary::Div => write!(f, "/"),
            OpBinary::Eq => write!(f, "=="),
            OpBinary::Neq => write!(f, "<>"),
            OpBinary::Lt => write!(f, "<"),
            OpBinary::Le => write!(f, "<="),
            OpBinary::Gt => write!(f, ">"),
            OpBinary::Ge => write!(f, ">="),
            OpBinary::And => write!(f, "and"),
            OpBinary::Or => write!(f, "or"),
            OpBinary::Exp => write!(f, "^"),
            OpBinary::ExpElem => write!(f, ".^"),
            OpBinary::AddElem => write!(f, ".+"),
            OpBinary::SubElem => write!(f, ".-"),
            OpBinary::MulElem => write!(f, ".*"),
            OpBinary::DivElem => write!(f, "./"),
            OpBinary::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum OpUnary {
    Empty,
    Minus,
    Plus,
    DotMinus,
    DotPlus,
    Not,
}

impl Display for OpUnary {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            OpUnary::Empty => write!(f, ""),
            OpUnary::Minus => write!(f, "-"),
            OpUnary::Plus => write!(f, "+"),
            OpUnary::DotMinus => write!(f, ".-"),
            OpUnary::DotPlus => write!(f, ".+"),
            OpUnary::Not => write!(f, "not "),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Variability {
    Empty,
    Constant(Token),
    Parameter(Token),
    Discrete(Token),
    Continuous(Token),
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Causality {
    Empty,
    Input(Token),
    Output(Token),
}

/// Type of class (model, function, connector, etc.).
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum ClassType {
    #[default]
    Model,
    Class,
    Block,
    Connector,
    Record,
    Type,
    Package,
    Function,
    Operator,
}

impl ClassType {
    /// Get the human-readable name for this class type.
    pub fn as_str(&self) -> &'static str {
        match self {
            ClassType::Model => "model",
            ClassType::Class => "class",
            ClassType::Block => "block",
            ClassType::Connector => "connector",
            ClassType::Record => "record",
            ClassType::Type => "type",
            ClassType::Package => "package",
            ClassType::Function => "function",
            ClassType::Operator => "operator",
        }
    }
}

mod var_name;
pub use var_name::{VarName, VarNameId};

/// Structured semantic reference used by Flat/DAE expressions.
///
/// `name` is a cached display/serialization spelling. `component_ref` preserves
/// the source/resolved reference structure carried forward from lowering. Its
/// `def_id` records the declaration being referenced; it does not assign a
/// `DefId` to this expression.
#[derive(Debug, Clone)]
pub struct Reference {
    name: VarName,
    component_ref: Option<ComponentReference>,
    resolved_function: Option<ResolvedFunctionReference>,
    generated: bool,
}

impl Reference {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: VarName::new(name),
            component_ref: None,
            resolved_function: None,
            generated: false,
        }
    }

    pub fn from_var_name(name: VarName) -> Self {
        Self {
            name,
            component_ref: None,
            resolved_function: None,
            generated: false,
        }
    }

    pub fn generated(name: impl Into<String>) -> Self {
        Self {
            name: VarName::new(name),
            component_ref: None,
            resolved_function: None,
            generated: true,
        }
    }

    pub fn generated_component(
        ident: impl Into<String>,
        subscripts: Vec<Subscript>,
        span: Span,
    ) -> Self {
        Self::generated_component_reference(ComponentReference {
            local: false,
            span,
            parts: vec![ComponentRefPart {
                ident: ident.into(),
                span,
                subs: subscripts,
            }],
            def_id: None,
        })
    }

    pub fn generated_component_reference(component_ref: ComponentReference) -> Self {
        let name = ComponentPath::from_component_reference(&component_ref).to_flat_string();
        Self {
            name: VarName::new(name),
            component_ref: Some(component_ref),
            resolved_function: None,
            generated: true,
        }
    }

    pub fn with_var_name(&self, name: VarName) -> Self {
        Self {
            name,
            component_ref: self.component_ref.clone(),
            resolved_function: self.resolved_function,
            generated: self.generated,
        }
    }

    pub fn with_component_reference(
        name: impl Into<String>,
        component_ref: ComponentReference,
    ) -> Self {
        Self {
            name: VarName::new(name),
            component_ref: Some(component_ref),
            resolved_function: None,
            generated: false,
        }
    }

    pub fn from_component_reference(component_ref: ComponentReference) -> Self {
        let name = ComponentPath::from_component_reference(&component_ref).to_flat_string();
        Self {
            name: VarName::new(name),
            component_ref: Some(component_ref),
            resolved_function: None,
            generated: false,
        }
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    /// Split into `(enclosing scope, last segment)` when the name is nested.
    pub fn scope_split(&self) -> Option<(&str, &str)> {
        self.name.scope_split()
    }

    /// True when the referenced name is nested inside a component scope.
    pub fn is_nested(&self) -> bool {
        self.name.is_nested()
    }

    /// Top-level segments of the referenced name (see [`VarName::segments`]).
    pub fn segments(&self) -> Vec<&str> {
        self.name.segments()
    }

    pub fn var_name(&self) -> &VarName {
        &self.name
    }

    pub fn into_var_name(self) -> VarName {
        self.name
    }

    pub fn component_ref(&self) -> Option<&ComponentReference> {
        self.component_ref.as_ref()
    }

    pub fn resolved_function(&self) -> Option<ResolvedFunctionReference> {
        self.resolved_function
    }

    pub fn with_resolved_function(mut self, resolved: ResolvedFunctionReference) -> Self {
        self.resolved_function = Some(resolved);
        self
    }

    pub fn span(&self) -> Option<Span> {
        self.component_ref
            .as_ref()
            .and_then(|reference| (!reference.span.is_dummy()).then_some(reference.span))
    }

    /// Element reference: this reference with a literal index appended to its
    /// last part, keeping rendered text and structure in lockstep.
    pub fn with_appended_index(&self, index: i64, span: ProvenanceSpan) -> Self {
        let rendered = format!("{}[{index}]", self.as_str());
        match self.component_ref.clone() {
            Some(mut reference) if !reference.parts.is_empty() => {
                if let Some(part) = reference.parts.last_mut() {
                    part.subs
                        .push(Subscript::generated_index_with_provenance(index, span));
                }
                Self::with_component_reference(rendered, reference)
                    .with_optional_resolved_function(self.resolved_function)
            }
            _ if self.generated => {
                Self::generated(rendered).with_optional_resolved_function(self.resolved_function)
            }
            _ => Self::new(rendered).with_optional_resolved_function(self.resolved_function),
        }
    }

    /// Member reference: this reference with a field part appended, keeping
    /// rendered text and structure in lockstep.
    pub fn with_appended_field(&self, field: &str) -> Self {
        let rendered = format!("{}.{field}", self.as_str());
        match self.component_ref.clone() {
            Some(mut reference) if !reference.parts.is_empty() => {
                reference.parts.push(ComponentRefPart {
                    ident: field.to_string(),
                    span: reference.span,
                    subs: Vec::new(),
                });
                Self::with_component_reference(rendered, reference)
                    .with_optional_resolved_function(self.resolved_function)
            }
            _ if self.generated => {
                Self::generated(rendered).with_optional_resolved_function(self.resolved_function)
            }
            _ => Self::new(rendered).with_optional_resolved_function(self.resolved_function),
        }
    }

    /// Append compiler-owned structured component parts while retaining the
    /// resolved declaration identity.
    pub fn with_appended_parts(&self, parts: &[ComponentRefPart], span: Span) -> Option<Self> {
        let mut reference = self.component_ref.clone()?;
        if parts.is_empty() {
            return None;
        }
        reference.parts.extend_from_slice(parts);
        reference.span = span;
        Some(if self.generated {
            Self::generated_component_reference(reference)
                .with_optional_resolved_function(self.resolved_function)
        } else {
            Self::from_component_reference(reference)
                .with_optional_resolved_function(self.resolved_function)
        })
    }

    fn with_optional_resolved_function(
        mut self,
        resolved: Option<ResolvedFunctionReference>,
    ) -> Self {
        self.resolved_function = resolved;
        self
    }

    pub fn is_generated(&self) -> bool {
        self.generated
    }

    pub fn component_scope(&self) -> Option<ComponentReferenceScope<'_>> {
        self.component_ref
            .as_ref()
            .map(ComponentReference::component_scope)
    }

    pub fn parts(&self) -> &[ComponentRefPart] {
        self.component_ref
            .as_ref()
            .map(|component_ref| component_ref.parts.as_slice())
            .unwrap_or(&[])
    }

    pub fn target_def_id(&self) -> Option<DefId> {
        self.component_ref
            .as_ref()
            .and_then(|component_ref| component_ref.def_id)
    }

    pub fn has_structure(&self) -> bool {
        self.component_ref.is_some()
    }

    pub fn last_segment(&self) -> &str {
        self.component_ref
            .as_ref()
            .and_then(ComponentReference::last_ident)
            .unwrap_or_else(|| self.name.last_segment())
    }
}

impl PartialEq for Reference {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.component_ref == other.component_ref
            && self.resolved_function == other.resolved_function
            && self.generated == other.generated
    }
}

impl std::fmt::Display for Reference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl From<VarName> for Reference {
    fn from(name: VarName) -> Self {
        Self::from_var_name(name)
    }
}

impl From<&str> for Reference {
    fn from(name: &str) -> Self {
        Self::new(name)
    }
}

impl From<String> for Reference {
    fn from(name: String) -> Self {
        Self::new(name)
    }
}

/// Structured view of a flattened scalar name such as `x[1,2]`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ScalarNameRef<'a> {
    pub base: &'a str,
    pub indices: Vec<i64>,
}

/// Parse a flattened scalar name into its base name and integer subscripts.
pub fn parse_scalar_name(name: &str) -> Option<ScalarNameRef<'_>> {
    let (base, raw_indices) = split_trailing_subscript_suffix(name)?;
    let indices = parse_scalar_indices(raw_indices)?;
    (!indices.is_empty()).then_some(ScalarNameRef { base, indices })
}

/// Return the base name for a flattened scalar name.
pub fn strip_scalar_name_subscripts(name: &str) -> Option<&str> {
    parse_scalar_name(name).map(|scalar| scalar.base)
}

/// Return the base before a syntactic trailing subscript suffix.
///
/// This is intentionally broader than [`strip_scalar_name_subscripts`]: state
/// detection must recognize `der(x[2:n])` even though `2:n` is not a scalar
/// integer index list.
pub fn strip_trailing_subscript_suffix(name: &str) -> Option<&str> {
    if let Some(base) = strip_scalar_name_subscripts(name) {
        return Some(base);
    }
    split_trailing_subscript_suffix(name).map(|(base, _subscript)| base)
}

/// Split the final syntactic subscript suffix from a Modelica-style reference.
///
/// This recognizes a balanced trailing bracket group without requiring integer
/// scalar indices, so display/codegen boundaries can preserve text such as
/// `a[i + 1]` while still ignoring dots or brackets inside earlier segments.
pub fn split_trailing_subscript_suffix(name: &str) -> Option<(&str, &str)> {
    if !name.ends_with(']') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, ch) in name.char_indices().rev() {
        match ch {
            ']' => depth += 1,
            '[' => {
                depth = depth.checked_sub(1)?;
                if depth == 0 {
                    let body = &name[idx + 1..name.len() - 1];
                    let base = &name[..idx];
                    return valid_trailing_subscript_split(base, body).then_some((base, body));
                }
            }
            _ => {}
        }
    }
    None
}

fn valid_trailing_subscript_split(base: &str, body: &str) -> bool {
    !body.trim().is_empty() && !base.is_empty() && has_balanced_subscripts(base)
}

fn parse_scalar_indices(raw_indices: &str) -> Option<Vec<i64>> {
    raw_indices
        .split(',')
        .map(str::trim)
        .map(str::parse::<i64>)
        .collect::<Result<Vec<_>, _>>()
        .ok()
}

fn has_balanced_subscripts(name: &str) -> bool {
    let mut depth = 0usize;
    for ch in name.chars() {
        match ch {
            '[' => depth += 1,
            ']' => {
                let Some(next_depth) = depth.checked_sub(1) else {
                    return false;
                };
                depth = next_depth;
            }
            _ => {}
        }
    }
    depth == 0
}

/// Modelica builtin functions (shared by flat and DAE IRs).
///
/// These are distinguished from user functions because they have
/// special semantics (e.g., `der()` identifies state variables).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum BuiltinFunction {
    // Differential operators
    /// Time derivative: der(x)
    Der,
    /// Previous value (discrete): pre(x)
    Pre,

    // Math functions
    /// Absolute value: abs(x)
    Abs,
    /// Sign function: sign(x)
    Sign,
    /// Square root: sqrt(x)
    Sqrt,
    /// Integer division: div(x, y)
    Div,
    /// Modulo: mod(x, y)
    Mod,
    /// Remainder: rem(x, y)
    Rem,
    /// Floor: floor(x)
    Floor,
    /// Ceiling: ceil(x)
    Ceil,
    /// Minimum: min(x, y)
    Min,
    /// Maximum: max(x, y)
    Max,

    // Trigonometric functions
    /// Sine: sin(x)
    Sin,
    /// Cosine: cos(x)
    Cos,
    /// Tangent: tan(x)
    Tan,
    /// Arcsine: asin(x)
    Asin,
    /// Arccosine: acos(x)
    Acos,
    /// Arctangent: atan(x)
    Atan,
    /// Two-argument arctangent: atan2(y, x)
    Atan2,

    // Hyperbolic functions
    /// Hyperbolic sine: sinh(x)
    Sinh,
    /// Hyperbolic cosine: cosh(x)
    Cosh,
    /// Hyperbolic tangent: tanh(x)
    Tanh,

    // Exponential and logarithmic
    /// Exponential: exp(x)
    Exp,
    /// Natural logarithm: log(x)
    Log,
    /// Base-10 logarithm: log10(x)
    Log10,

    // Event-related
    /// Edge detection: edge(b) - true when b changes to true
    Edge,
    /// Change detection: change(v) - true when v changes
    Change,
    /// Reinitialize state: reinit(x, expr)
    Reinit,
    /// Overloaded sample operator: sample(start, interval) event tick or
    /// sample(u[, clock]) clocked value sample.
    Sample,
    /// Initial condition: initial() - true during initialization
    Initial,
    /// Terminal condition: terminal() - true during termination
    Terminal,
    /// Suppress event generation: noEvent(expr) - pass-through
    NoEvent,
    /// Smooth operator: smooth(p, expr) - pass-through expr
    Smooth,
    /// Homotopy: homotopy(actual, simplified) - returns actual
    Homotopy,
    /// Semi-linear: semiLinear(x, k1, k2) = if x >= 0 then k1*x else k2*x
    SemiLinear,
    /// Delay: delay(expr, delayTime) - returns expr (no delay in continuous sim)
    Delay,
    /// Integer conversion: integer(x)
    Integer,

    // Reduction operators
    /// Sum of array elements: sum(A)
    Sum,
    /// Product of array elements: product(A)
    Product,

    // Array functions
    /// Number of dimensions: ndims(A)
    Ndims,
    /// Size of dimension: size(A, i)
    Size,
    /// Scalar from single-element array: scalar(A)
    Scalar,
    /// Vector from array: vector(A)
    Vector,
    /// Matrix from array: matrix(A)
    Matrix,
    /// Identity matrix: identity(n)
    Identity,
    /// Diagonal matrix: diagonal(v)
    Diagonal,
    /// Zero array: zeros(n1, n2, ...)
    Zeros,
    /// Ones array: ones(n1, n2, ...)
    Ones,
    /// Fill array: fill(s, n1, n2, ...)
    Fill,
    /// Linearly spaced vector: linspace(x1, x2, n)
    Linspace,
    /// Transpose: transpose(A)
    Transpose,
    /// Outer product: outerProduct(v1, v2)
    OuterProduct,
    /// Symmetric matrix: symmetric(A)
    Symmetric,
    /// Cross product: cross(x, y)
    Cross,
    /// Skew symmetric matrix: skew(x)
    Skew,

    // Linear algebra
    /// Concatenate arrays: cat(dim, A, B, ...)
    Cat,
}

impl BuiltinFunction {
    /// Builtin variants that are represented as `Expression::BuiltinCall`.
    pub const ALL: &'static [Self] = &[
        Self::Der,
        Self::Pre,
        Self::Abs,
        Self::Sign,
        Self::Sqrt,
        Self::Div,
        Self::Mod,
        Self::Rem,
        Self::Floor,
        Self::Ceil,
        Self::Min,
        Self::Max,
        Self::Sin,
        Self::Cos,
        Self::Tan,
        Self::Asin,
        Self::Acos,
        Self::Atan,
        Self::Atan2,
        Self::Sinh,
        Self::Cosh,
        Self::Tanh,
        Self::Exp,
        Self::Log,
        Self::Log10,
        Self::Edge,
        Self::Change,
        Self::Reinit,
        Self::Sample,
        Self::Initial,
        Self::Terminal,
        Self::NoEvent,
        Self::Smooth,
        Self::Homotopy,
        Self::SemiLinear,
        Self::Delay,
        Self::Integer,
        Self::Sum,
        Self::Product,
        Self::Ndims,
        Self::Size,
        Self::Scalar,
        Self::Vector,
        Self::Matrix,
        Self::Identity,
        Self::Diagonal,
        Self::Zeros,
        Self::Ones,
        Self::Fill,
        Self::Linspace,
        Self::Transpose,
        Self::OuterProduct,
        Self::Symmetric,
        Self::Cross,
        Self::Skew,
        Self::Cat,
    ];

    /// Try to parse a function name as a builtin.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            // Differential
            "der" => Some(Self::Der),
            "pre" => Some(Self::Pre),
            // Math
            "abs" => Some(Self::Abs),
            "sign" => Some(Self::Sign),
            "sqrt" => Some(Self::Sqrt),
            "div" => Some(Self::Div),
            "mod" => Some(Self::Mod),
            "rem" => Some(Self::Rem),
            "floor" => Some(Self::Floor),
            "ceil" => Some(Self::Ceil),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            // Trig
            "sin" => Some(Self::Sin),
            "cos" => Some(Self::Cos),
            "tan" => Some(Self::Tan),
            "asin" => Some(Self::Asin),
            "acos" => Some(Self::Acos),
            "atan" => Some(Self::Atan),
            "atan2" => Some(Self::Atan2),
            // Hyperbolic
            "sinh" => Some(Self::Sinh),
            "cosh" => Some(Self::Cosh),
            "tanh" => Some(Self::Tanh),
            // Exp/Log
            "exp" => Some(Self::Exp),
            "log" => Some(Self::Log),
            "log10" => Some(Self::Log10),
            // Event
            "edge" => Some(Self::Edge),
            "change" => Some(Self::Change),
            "reinit" => Some(Self::Reinit),
            "sample" => Some(Self::Sample),
            "initial" => Some(Self::Initial),
            "terminal" => Some(Self::Terminal),
            "noEvent" => Some(Self::NoEvent),
            "smooth" => Some(Self::Smooth),
            "homotopy" => Some(Self::Homotopy),
            "semiLinear" => Some(Self::SemiLinear),
            "delay" => Some(Self::Delay),
            "integer" | "Integer" => Some(Self::Integer),
            // Reduction
            "sum" => Some(Self::Sum),
            "product" => Some(Self::Product),
            // Array
            "ndims" => Some(Self::Ndims),
            "size" => Some(Self::Size),
            "scalar" => Some(Self::Scalar),
            "vector" => Some(Self::Vector),
            "matrix" => Some(Self::Matrix),
            "identity" => Some(Self::Identity),
            "diagonal" => Some(Self::Diagonal),
            "zeros" => Some(Self::Zeros),
            "ones" => Some(Self::Ones),
            "fill" => Some(Self::Fill),
            "linspace" => Some(Self::Linspace),
            "transpose" => Some(Self::Transpose),
            "outerProduct" => Some(Self::OuterProduct),
            "symmetric" => Some(Self::Symmetric),
            "cross" => Some(Self::Cross),
            "skew" => Some(Self::Skew),
            "cat" => Some(Self::Cat),
            _ => None,
        }
    }

    /// Get the function name as a string.
    pub fn name(&self) -> &'static str {
        match self {
            Self::Der => "der",
            Self::Pre => "pre",
            Self::Abs => "abs",
            Self::Sign => "sign",
            Self::Sqrt => "sqrt",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Rem => "rem",
            Self::Floor => "floor",
            Self::Ceil => "ceil",
            Self::Min => "min",
            Self::Max => "max",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Atan => "atan",
            Self::Atan2 => "atan2",
            Self::Sinh => "sinh",
            Self::Cosh => "cosh",
            Self::Tanh => "tanh",
            Self::Exp => "exp",
            Self::Log => "log",
            Self::Log10 => "log10",
            Self::Edge => "edge",
            Self::Change => "change",
            Self::Reinit => "reinit",
            Self::Sample => "sample",
            Self::Initial => "initial",
            Self::Terminal => "terminal",
            Self::NoEvent => "noEvent",
            Self::Smooth => "smooth",
            Self::Homotopy => "homotopy",
            Self::SemiLinear => "semiLinear",
            Self::Delay => "delay",
            Self::Integer => "integer",
            Self::Sum => "sum",
            Self::Product => "product",
            Self::Ndims => "ndims",
            Self::Size => "size",
            Self::Scalar => "scalar",
            Self::Vector => "vector",
            Self::Matrix => "matrix",
            Self::Identity => "identity",
            Self::Diagonal => "diagonal",
            Self::Zeros => "zeros",
            Self::Ones => "ones",
            Self::Fill => "fill",
            Self::Linspace => "linspace",
            Self::Transpose => "transpose",
            Self::OuterProduct => "outerProduct",
            Self::Symmetric => "symmetric",
            Self::Cross => "cross",
            Self::Skew => "skew",
            Self::Cat => "cat",
        }
    }
}

/// State selection hint for variables.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum StateSelect {
    /// Default behavior.
    #[default]
    Default,
    /// Never use as state.
    Never,
    /// Avoid using as state.
    Avoid,
    /// Prefer using as state.
    Prefer,
    /// Always use as state.
    Always,
}

/// A Modelica literal value (shared by flat and DAE IRs).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    /// Real number literal.
    Real(f64),
    /// Integer literal.
    Integer(i64),
    /// Boolean literal.
    Boolean(bool),
    /// String literal.
    String(String),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Real(v) => write!(f, "{}", v),
            Literal::Integer(v) => write!(f, "{}", v),
            Literal::Boolean(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "\"{}\"", v),
        }
    }
}

/// External function declaration (MLS §12.9).
///
/// For functions declared with `external` to call C/Fortran code.
/// Shared by the flat and DAE IRs.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ExternalFunction {
    /// Language specification (e.g., "C", "FORTRAN 77"). Default is "C".
    pub language: String,
    /// External function name (defaults to Modelica function name if not specified).
    pub function_name: Option<String>,
    /// Output variable that receives the return value (if any).
    pub output_name: Option<String>,
    /// Argument names passed to the external function.
    pub arg_names: Vec<String>,
}

/// Function derivative annotation (MLS §12.7.1).
///
/// Specifies the derivative function for automatic differentiation.
/// Shared by the flat and DAE IRs.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct DerivativeAnnotation {
    /// Name of the derivative function.
    pub derivative_function: String,
    /// Derivative order (default is 1).
    pub order: u32,
    /// Input variables whose derivatives are zero (treated as constants).
    pub zero_derivative: Vec<String>,
    /// Input variables with no derivative (not differentiated at all).
    pub no_derivative: Vec<String>,
}

/// Loaded external table descriptor.
///
/// Carries the evaluated numeric contents of a Modelica `ExternalObject`
/// table (e.g. `Modelica.Blocks.Tables.CombiTable1D`) across the
/// eval-DAE → solver boundary. Shared by the eval and solve crates so
/// neither side needs to depend on the other for this type alone.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ExternalTableData {
    pub id: u64,
    pub data: Vec<Vec<f64>>,
    pub columns: Vec<usize>,
    pub smoothness: i64,
    pub extrapolation: i64,
}

/// Semantic expression tree shared by Flat and DAE IR.
///
/// AST keeps a separate syntax-preserving expression type with tokens,
/// parentheses, named arguments, and class-modification syntax. This type is the
/// post-AST semantic expression grammar used once names have been lowered to
/// structured `Reference`s and builtin calls have been identified.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Expression {
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Unary {
        op: OpUnary,
        rhs: Box<Expression>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    VarRef {
        name: Reference,
        subscripts: Vec<Subscript>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    BuiltinCall {
        function: BuiltinFunction,
        args: Vec<Expression>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    FunctionCall {
        name: Reference,
        args: Vec<Expression>,
        #[serde(default)]
        is_constructor: bool,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Literal {
        value: Literal,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    If {
        branches: Vec<(Expression, Expression)>,
        else_branch: Box<Expression>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Array {
        elements: Vec<Expression>,
        is_matrix: bool,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Tuple {
        elements: Vec<Expression>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Range {
        start: Box<Expression>,
        step: Option<Box<Expression>>,
        end: Box<Expression>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    ArrayComprehension {
        expr: Box<Expression>,
        indices: Vec<ComprehensionIndex>,
        filter: Option<Box<Expression>>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Index {
        base: Box<Expression>,
        subscripts: Vec<Subscript>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    FieldAccess {
        base: Box<Expression>,
        field: String,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Empty {
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
}

impl Expression {
    pub fn with_span(self, span: Span) -> Self {
        if span.is_dummy() {
            self
        } else {
            self.map_span(|_| span)
        }
    }

    pub fn span(&self) -> Option<Span> {
        let span = match self {
            Expression::VarRef { name, span, .. } => {
                return (!span.is_dummy()).then_some(*span).or_else(|| name.span());
            }
            Expression::Binary { span, .. }
            | Expression::Unary { span, .. }
            | Expression::BuiltinCall { span, .. }
            | Expression::FunctionCall { span, .. }
            | Expression::Literal { span, .. }
            | Expression::If { span, .. }
            | Expression::Array { span, .. }
            | Expression::Tuple { span, .. }
            | Expression::Range { span, .. }
            | Expression::ArrayComprehension { span, .. }
            | Expression::Index { span, .. }
            | Expression::FieldAccess { span, .. }
            | Expression::Empty { span } => *span,
        };
        (!span.is_dummy()).then_some(span)
    }

    pub fn require_span(
        &self,
        context: &'static str,
    ) -> Result<ProvenanceSpan, MissingProvenanceSpan> {
        self.span()
            .map(|span| span.require_provenance(context))
            .unwrap_or_else(|| Err(MissingProvenanceSpan::new(context)))
    }

    pub fn unspan(&self) -> &Expression {
        self
    }

    fn map_span(mut self, f: impl FnOnce(Span) -> Span) -> Self {
        let span_slot = match &mut self {
            Expression::Binary { span, .. }
            | Expression::Unary { span, .. }
            | Expression::VarRef { span, .. }
            | Expression::BuiltinCall { span, .. }
            | Expression::FunctionCall { span, .. }
            | Expression::Literal { span, .. }
            | Expression::If { span, .. }
            | Expression::Array { span, .. }
            | Expression::Tuple { span, .. }
            | Expression::Range { span, .. }
            | Expression::ArrayComprehension { span, .. }
            | Expression::Index { span, .. }
            | Expression::FieldAccess { span, .. }
            | Expression::Empty { span } => span,
        };
        *span_slot = f(*span_slot);
        self
    }

    pub fn contains_der(&self) -> bool {
        self.contains_subexpression(|expr| {
            matches!(
                expr,
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    ..
                }
            )
        })
    }

    pub fn contains_relational_operator(&self) -> bool {
        self.contains_subexpression(
            |expr| matches!(expr, Expression::Binary { op, .. } if op.is_relational()),
        )
    }

    pub fn contains_der_of_state<'a>(
        &self,
        state_vars: impl IntoIterator<Item = &'a VarName>,
    ) -> bool {
        let state_vars = state_vars.into_iter().collect::<Vec<_>>();
        self.contains_subexpression(|expr| {
            matches!(
                expr,
                Expression::BuiltinCall {
                    function: BuiltinFunction::Der,
                    args,
                    ..
                } if der_call_matches_any_state(args, &state_vars)
            )
        })
    }

    pub fn contains_subexpression(&self, mut predicate: impl FnMut(&Expression) -> bool) -> bool {
        let mut checker = ContainsExpressionChecker {
            found: false,
            predicate: &mut predicate,
        };
        crate::ExpressionVisitor::visit_expression(&mut checker, self);
        checker.found
    }

    pub fn get_der_variable(&self) -> Option<&VarName> {
        match self {
            Expression::BuiltinCall { function, args, .. } if *function == BuiltinFunction::Der => {
                args.first().and_then(|arg| match arg {
                    Expression::VarRef { name, .. } => Some(name.var_name()),
                    _ => None,
                })
            }
            _ => None,
        }
    }

    pub fn collect_state_variables(&self, states: &mut impl Extend<VarName>) {
        let mut out = IndexSet::new();
        self.collect_state_variables_into(&mut out);
        states.extend(out);
    }

    pub fn collect_var_refs(&self, vars: &mut impl Extend<VarName>) {
        let mut out = IndexSet::new();
        self.collect_var_refs_into(&mut out);
        vars.extend(out);
    }

    fn collect_state_variables_into(&self, states: &mut IndexSet<VarName>) {
        let mut collector = StateVariableCollector { states };
        crate::ExpressionVisitor::visit_expression(&mut collector, self);
    }

    fn collect_var_refs_into(&self, vars: &mut IndexSet<VarName>) {
        let mut collector = VarRefCollector { vars };
        crate::ExpressionVisitor::visit_expression(&mut collector, self);
    }

    /// Structural expression equality for semantic IR consumers.
    ///
    /// Source spans are intentionally ignored: they identify where a semantic
    /// expression came from, not the expression's mathematical identity.
    pub fn semantically_eq_ignoring_spans(&self, rhs: &Expression) -> bool {
        expressions_semantically_equal(self, rhs)
    }
}

struct ContainsExpressionChecker<'a, F>
where
    F: FnMut(&Expression) -> bool,
{
    found: bool,
    predicate: &'a mut F,
}

impl<F> crate::ExpressionVisitor for ContainsExpressionChecker<'_, F>
where
    F: FnMut(&Expression) -> bool,
{
    fn visit_expression(&mut self, expr: &Expression) {
        if self.found {
            return;
        }
        if (self.predicate)(expr) {
            self.found = true;
            return;
        }
        self.walk_expression(expr);
    }
}

fn der_call_matches_any_state(args: &[Expression], state_vars: &[&VarName]) -> bool {
    matches!(
        args.first(),
        Some(Expression::VarRef { name, .. })
            if state_vars
                .iter()
                .any(|state| derivative_name_matches_state(name.var_name(), state))
    )
}

struct StateVariableCollector<'a> {
    states: &'a mut IndexSet<VarName>,
}

impl crate::ExpressionVisitor for StateVariableCollector<'_> {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            if let Some(Expression::VarRef { name, .. }) = args.first() {
                self.states.insert(derivative_state_name(name.var_name()));
            }
            return;
        }
        self.walk_builtin_call(function, args);
    }
}

struct VarRefCollector<'a> {
    vars: &'a mut IndexSet<VarName>,
}

impl crate::ExpressionVisitor for VarRefCollector<'_> {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.vars.insert(name.var_name().clone());
        self.walk_var_ref(name, subscripts);
    }
}

/// Structural expression equality for the shared Flat/DAE expression grammar.
///
/// This is a syntactic IR query, not expression evaluation: it never folds,
/// resolves, or executes expressions. It exists in `rumoca-core` because
/// multiple phases need one span-insensitive definition of shared-expression
/// identity.
pub fn expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    match (lhs, rhs) {
        (Expression::Binary { .. }, Expression::Binary { .. }) => {
            binary_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::Unary { .. }, Expression::Unary { .. }) => {
            unary_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::VarRef { .. }, Expression::VarRef { .. }) => {
            var_refs_semantically_equal(lhs, rhs)
        }
        (Expression::BuiltinCall { .. }, Expression::BuiltinCall { .. }) => {
            builtin_calls_semantically_equal(lhs, rhs)
        }
        (Expression::FunctionCall { .. }, Expression::FunctionCall { .. }) => {
            function_calls_semantically_equal(lhs, rhs)
        }
        (Expression::Literal { value: lhs, .. }, Expression::Literal { value: rhs, .. }) => {
            lhs == rhs
        }
        (Expression::If { .. }, Expression::If { .. }) => {
            if_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::Array { .. }, Expression::Array { .. }) => arrays_semantically_equal(lhs, rhs),
        (Expression::Tuple { .. }, Expression::Tuple { .. }) => tuples_semantically_equal(lhs, rhs),
        (Expression::Range { .. }, Expression::Range { .. }) => ranges_semantically_equal(lhs, rhs),
        (Expression::ArrayComprehension { .. }, Expression::ArrayComprehension { .. }) => {
            array_comprehensions_semantically_equal(lhs, rhs)
        }
        (Expression::Index { .. }, Expression::Index { .. }) => {
            index_expressions_semantically_equal(lhs, rhs)
        }
        (Expression::FieldAccess { .. }, Expression::FieldAccess { .. }) => {
            field_accesses_semantically_equal(lhs, rhs)
        }
        (Expression::Empty { .. }, Expression::Empty { .. }) => true,
        _ => false,
    }
}

fn binary_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Binary {
            op: lhs_op,
            lhs: lhs_lhs,
            rhs: lhs_rhs,
            ..
        },
        Expression::Binary {
            op: rhs_op,
            lhs: rhs_lhs,
            rhs: rhs_rhs,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_op == rhs_op
        && expressions_semantically_equal(lhs_lhs, rhs_lhs)
        && expressions_semantically_equal(lhs_rhs, rhs_rhs)
}

fn unary_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Unary {
            op: lhs_op,
            rhs: lhs_rhs,
            ..
        },
        Expression::Unary {
            op: rhs_op,
            rhs: rhs_rhs,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_op == rhs_op && expressions_semantically_equal(lhs_rhs, rhs_rhs)
}

fn var_refs_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::VarRef {
            name: lhs_name,
            subscripts: lhs_subscripts,
            ..
        },
        Expression::VarRef {
            name: rhs_name,
            subscripts: rhs_subscripts,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    // Flat names are globally unique — `flat::Model::variables` is keyed by
    // VarName and flatten's name simplification fails loudly on rename
    // collisions — so two references denote the same variable iff their
    // rendered names match; attached resolution metadata (spans, def-ids,
    // component structure) does not change the meaning.
    lhs_name.var_name() == rhs_name.var_name()
        && subscripts_semantically_equal(lhs_subscripts, rhs_subscripts)
}

fn builtin_calls_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::BuiltinCall {
            function: lhs_function,
            args: lhs_args,
            ..
        },
        Expression::BuiltinCall {
            function: rhs_function,
            args: rhs_args,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_function == rhs_function && expression_slices_semantically_equal(lhs_args, rhs_args)
}

fn function_calls_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::FunctionCall {
            name: lhs_name,
            args: lhs_args,
            is_constructor: lhs_constructor,
            ..
        },
        Expression::FunctionCall {
            name: rhs_name,
            args: rhs_args,
            is_constructor: rhs_constructor,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_name == rhs_name
        && lhs_constructor == rhs_constructor
        && expression_slices_semantically_equal(lhs_args, rhs_args)
}

fn if_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::If {
            branches: lhs_branches,
            else_branch: lhs_else,
            ..
        },
        Expression::If {
            branches: rhs_branches,
            else_branch: rhs_else,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_branches.len() == rhs_branches.len()
        && lhs_branches
            .iter()
            .zip(rhs_branches)
            .all(expression_branch_pairs_semantically_equal)
        && expressions_semantically_equal(lhs_else, rhs_else)
}

fn expression_branch_pairs_semantically_equal(
    ((lhs_cond, lhs_value), (rhs_cond, rhs_value)): (
        &(Expression, Expression),
        &(Expression, Expression),
    ),
) -> bool {
    expressions_semantically_equal(lhs_cond, rhs_cond)
        && expressions_semantically_equal(lhs_value, rhs_value)
}

fn arrays_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Array {
            elements: lhs_elements,
            is_matrix: lhs_matrix,
            ..
        },
        Expression::Array {
            elements: rhs_elements,
            is_matrix: rhs_matrix,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_matrix == rhs_matrix && expression_slices_semantically_equal(lhs_elements, rhs_elements)
}

fn tuples_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Tuple {
            elements: lhs_elements,
            ..
        },
        Expression::Tuple {
            elements: rhs_elements,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expression_slices_semantically_equal(lhs_elements, rhs_elements)
}

fn ranges_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Range {
            start: lhs_start,
            step: lhs_step,
            end: lhs_end,
            ..
        },
        Expression::Range {
            start: rhs_start,
            step: rhs_step,
            end: rhs_end,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expressions_semantically_equal(lhs_start, rhs_start)
        && optional_expressions_semantically_equal(lhs_step.as_deref(), rhs_step.as_deref())
        && expressions_semantically_equal(lhs_end, rhs_end)
}

fn array_comprehensions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::ArrayComprehension {
            expr: lhs_expr,
            indices: lhs_indices,
            filter: lhs_filter,
            ..
        },
        Expression::ArrayComprehension {
            expr: rhs_expr,
            indices: rhs_indices,
            filter: rhs_filter,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expressions_semantically_equal(lhs_expr, rhs_expr)
        && lhs_indices.len() == rhs_indices.len()
        && lhs_indices.iter().zip(rhs_indices).all(|(lhs, rhs)| {
            lhs.name == rhs.name && expressions_semantically_equal(&lhs.range, &rhs.range)
        })
        && optional_expressions_semantically_equal(lhs_filter.as_deref(), rhs_filter.as_deref())
}

fn index_expressions_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::Index {
            base: lhs_base,
            subscripts: lhs_subscripts,
            ..
        },
        Expression::Index {
            base: rhs_base,
            subscripts: rhs_subscripts,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    expressions_semantically_equal(lhs_base, rhs_base)
        && subscripts_semantically_equal(lhs_subscripts, rhs_subscripts)
}

fn field_accesses_semantically_equal(lhs: &Expression, rhs: &Expression) -> bool {
    let (
        Expression::FieldAccess {
            base: lhs_base,
            field: lhs_field,
            ..
        },
        Expression::FieldAccess {
            base: rhs_base,
            field: rhs_field,
            ..
        },
    ) = (lhs, rhs)
    else {
        return false;
    };
    lhs_field == rhs_field && expressions_semantically_equal(lhs_base, rhs_base)
}

fn expression_slices_semantically_equal(lhs: &[Expression], rhs: &[Expression]) -> bool {
    lhs.len() == rhs.len()
        && lhs
            .iter()
            .zip(rhs)
            .all(|(lhs, rhs)| expressions_semantically_equal(lhs, rhs))
}

fn optional_expressions_semantically_equal(
    lhs: Option<&Expression>,
    rhs: Option<&Expression>,
) -> bool {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) => expressions_semantically_equal(lhs, rhs),
        (None, None) => true,
        _ => false,
    }
}

fn subscripts_semantically_equal(lhs: &[Subscript], rhs: &[Subscript]) -> bool {
    lhs.len() == rhs.len()
        && lhs.iter().zip(rhs).all(|(lhs, rhs)| match (lhs, rhs) {
            (Subscript::Index { value: lhs, .. }, Subscript::Index { value: rhs, .. }) => {
                lhs == rhs
            }
            (Subscript::Colon { .. }, Subscript::Colon { .. }) => true,
            (Subscript::Expr { expr: lhs, .. }, Subscript::Expr { expr: rhs, .. }) => {
                expressions_semantically_equal(lhs, rhs)
            }
            _ => false,
        })
}

#[cfg(test)]
mod tests;
