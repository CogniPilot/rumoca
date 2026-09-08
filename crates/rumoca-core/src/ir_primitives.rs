//! Shared IR primitives used by multiple Rumoca IR crates.

use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;
use std::sync::{Arc, OnceLock, RwLock};

use crate::{StructuredIndexBinderId, Subscript, split_path_with_indices};

mod component_refs_and_functions;
pub use component_refs_and_functions::*;

mod generated_names;
pub use generated_names::*;

mod reference_serde;
pub use reference_serde::ReferenceContractError;

mod scalar_name;
pub use scalar_name::*;

/// A unique identifier for a definition (class, component, etc.).
///
/// DefIds are assigned during semantic analysis to enable efficient
/// lookup and cross-referencing between compiler phases.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct DefId(pub u32);

/// Unique identity of one concrete component or class instance.
///
/// Unlike [`DefId`], this identifies a runtime occurrence rather than its
/// source declaration. The identity is allocated by instantiation and carried
/// through Flat so phase boundaries never reconstruct occurrence identity from
/// rendered names.
///
/// Instantiation allocates occurrence identities from one, so [`InstanceId::UNSET`]
/// (the `Default`) can never name a concrete occurrence. Stage contracts reject
/// it instead of accepting a defaulted identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct InstanceId(pub u32);

impl InstanceId {
    /// Reserved identity meaning "no allocated occurrence".
    ///
    /// Occurrence allocation is one-based, so this value is unreachable for a
    /// real instance and identifies an unset field.
    pub const UNSET: InstanceId = InstanceId(0);

    pub fn new(index: u32) -> Self {
        Self(index)
    }

    pub fn index(self) -> u32 {
        self.0
    }

    /// True when no occurrence identity has been allocated for this field.
    pub fn is_unset(self) -> bool {
        self == Self::UNSET
    }
}

impl Display for InstanceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "InstanceId({})", self.0)
    }
}

/// Non-sentinel occurrence identity admitted at a semantic IR boundary.
///
/// `InstanceId` retains its pre-construction `UNSET` state in the current
/// Instance/Flat migration surface. Later semantic roots store this narrower
/// type so an absent occurrence cannot be represented after construction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SourceOccurrenceId(NonZeroU32);

impl SourceOccurrenceId {
    #[must_use]
    pub const fn index(self) -> u32 {
        self.0.get()
    }

    #[must_use]
    pub const fn instance_id(self) -> InstanceId {
        InstanceId(self.index())
    }
}

impl TryFrom<InstanceId> for SourceOccurrenceId {
    type Error = UnsetSourceOccurrence;

    fn try_from(instance: InstanceId) -> Result<Self, Self::Error> {
        NonZeroU32::new(instance.index())
            .map(Self)
            .ok_or(UnsetSourceOccurrence)
    }
}

/// Refusal to admit the reserved `InstanceId::UNSET` sentinel as an IR
/// occurrence.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UnsetSourceOccurrence;

impl Display for UnsetSourceOccurrence {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str("source occurrence identity must not be InstanceId::UNSET")
    }
}

impl std::error::Error for UnsetSourceOccurrence {}

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
    /// This exact call occurrence proved its complete MLS §6.4 exposure path
    /// transitively non-replaceable. Automatic vectorization requires this
    /// occurrence-level fact in addition to exact function-instance identity.
    pub transitively_non_replaceable: bool,
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

/// Classification of the generated semantic wrapper used to retain one named
/// function argument after AST lowering.
///
/// Reserved spelling alone is not authority: source-owned references with the
/// same text remain ordinary expressions. Once a reference is marked generated
/// and enters the reserved namespace, however, malformed structure is invalid
/// semantic IR rather than an ordinary positional argument.
pub enum NamedFunctionArgMarker<'a, T> {
    NotMarker,
    Valid { name: &'a str, value: &'a T },
    Malformed,
}

/// Classify the exact generated named-argument wrapper contract.
pub fn classify_named_function_arg_marker<'a, T>(
    reference: &'a Reference,
    arguments: &'a [T],
    is_constructor: bool,
    call_kind: FunctionCallKind,
) -> NamedFunctionArgMarker<'a, T> {
    if !reference.is_generated() {
        return NamedFunctionArgMarker::NotMarker;
    }
    let Some(name) = reference.as_str().strip_prefix(NAMED_FUNCTION_ARG_PREFIX) else {
        return NamedFunctionArgMarker::NotMarker;
    };
    let [value] = arguments else {
        return NamedFunctionArgMarker::Malformed;
    };
    if name.is_empty() || !is_constructor || call_kind != FunctionCallKind::Invocation {
        return NamedFunctionArgMarker::Malformed;
    }
    NamedFunctionArgMarker::Valid { name, value }
}

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

    pub(crate) fn source_free_serde_default() -> Self {
        Self::DUMMY
    }

    pub fn require_provenance(
        self,
        context: &'static str,
    ) -> Result<ProvenanceSpan, MissingProvenanceSpan> {
        ProvenanceSpan::new(self, context)
    }
}

/// A parser source location.
///
/// The owning file is identified by [`SourceId`], not by an owned path string:
/// the id is computed once per file by the parser and copied into every token,
/// so cloning a `Location` is a plain memcpy with no heap traffic. Resolve the
/// human readable file name through [`crate::SourceMap`] when a diagnostic
/// needs to print it.
///
/// `Copy` is deliberately NOT derived: ~100 existing call sites clone locations
/// explicitly, and `clippy::clone_on_copy` would reject all of them at once.
#[derive(Default, Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Location {
    pub start_line: u32,
    pub start_column: u32,
    pub end_line: u32,
    pub end_column: u32,
    pub start: u32,
    pub end: u32,
    pub source: SourceId,
}

impl Location {
    /// The span covered by this location.
    ///
    /// Callers that need to reject source-free locations should gate on
    /// [`Location::has_source`] first; this method performs no validation.
    pub fn span(&self) -> Span {
        Span::from_offsets(self.source, self.start as usize, self.end as usize)
    }

    /// True when this location carries real parser provenance.
    pub fn has_source(&self) -> bool {
        self.source != SourceId::DUMMY && self.end > self.start
    }

    /// Build a location spanning from the start of `self` to the end of `end`.
    ///
    /// The source identity of `self` wins; merging locations from two different
    /// files is a caller bug and is not detected here.
    pub fn merged_with(&self, end: &Location) -> Location {
        Location {
            start_line: self.start_line,
            start_column: self.start_column,
            end_line: end.end_line,
            end_column: end.end_column,
            start: self.start,
            end: end.end,
            source: self.source,
        }
    }
}

impl Display for Location {
    /// Debug-only rendering. The source is printed as its numeric id because a
    /// `Location` cannot resolve its own file name; user-facing diagnostics must
    /// resolve the name through [`crate::SourceMap::name`].
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "source#{}:{}:{}",
            self.source.0, self.start_line, self.start_column
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
/// the source/resolved reference structure carried forward from lowering.
/// The first and final component parts identify the resolved root occurrence
/// and exact final declaration respectively.
#[derive(Debug, Clone)]
pub struct Reference {
    name: VarName,
    component_ref: Option<ComponentReference>,
    resolved_function: Option<ResolvedFunctionReference>,
    instance_id: Option<InstanceId>,
    structured_binder: Option<StructuredIndexBinderId>,
    generated: bool,
}

impl Reference {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: VarName::new(name),
            component_ref: None,
            resolved_function: None,
            instance_id: None,
            structured_binder: None,
            generated: false,
        }
    }

    pub fn from_var_name(name: VarName) -> Self {
        Self {
            name,
            component_ref: None,
            resolved_function: None,
            instance_id: None,
            structured_binder: None,
            generated: false,
        }
    }

    pub fn generated(name: impl Into<String>) -> Self {
        Self {
            name: VarName::new(name),
            component_ref: None,
            resolved_function: None,
            instance_id: None,
            structured_binder: None,
            generated: true,
        }
    }

    pub fn generated_component_reference(component_ref: ComponentReference) -> Self {
        let name = ComponentPath::from_component_reference(&component_ref).to_flat_string();
        Self {
            name: VarName::new(name),
            component_ref: Some(component_ref),
            resolved_function: None,
            instance_id: None,
            structured_binder: None,
            generated: true,
        }
    }

    pub fn with_var_name(&self, name: VarName) -> Self {
        Self {
            name,
            component_ref: self.component_ref.clone(),
            resolved_function: self.resolved_function,
            instance_id: self.instance_id,
            structured_binder: self.structured_binder,
            generated: self.generated,
        }
    }

    pub fn with_rewritten_component_reference(
        &self,
        name: impl Into<String>,
        component_ref: ComponentReference,
    ) -> Self {
        Self {
            name: VarName::new(name),
            component_ref: Some(component_ref),
            resolved_function: self.resolved_function,
            instance_id: self.instance_id,
            structured_binder: None,
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
            instance_id: None,
            structured_binder: None,
            generated: false,
        }
    }

    pub fn from_component_reference(component_ref: ComponentReference) -> Self {
        let name = ComponentPath::from_component_reference(&component_ref).to_flat_string();
        Self {
            name: VarName::new(name),
            component_ref: Some(component_ref),
            resolved_function: None,
            instance_id: None,
            structured_binder: None,
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

    pub fn component_ref(&self) -> Option<&ComponentReference> {
        self.component_ref.as_ref()
    }

    pub fn resolved_function(&self) -> Option<ResolvedFunctionReference> {
        self.resolved_function
    }

    pub fn with_resolved_function(mut self, resolved: ResolvedFunctionReference) -> Self {
        self.structured_binder = None;
        self.resolved_function = Some(resolved);
        self
    }

    /// Invalidate callable-instance evidence after changing the semantic target.
    pub fn without_resolved_function(mut self) -> Self {
        self.resolved_function = None;
        self
    }

    pub fn instance_id(&self) -> Option<InstanceId> {
        self.instance_id
    }

    pub fn structured_binder(&self) -> Option<StructuredIndexBinderId> {
        self.structured_binder
    }

    /// Bind this source loop-token occurrence to one compact family domain.
    ///
    /// The Flat producer retains the component reference so replay can check
    /// the exact source spelling and span as correlation evidence. Loop-token
    /// identity is the domain-local ID; this API never manufactures a `DefId`.
    pub fn with_structured_binder(
        mut self,
        binder: StructuredIndexBinderId,
    ) -> Result<Self, ReferenceContractError> {
        if self.component_ref.is_none()
            || self.instance_id.is_some()
            || self.resolved_function.is_some()
        {
            return Err(ReferenceContractError::InvalidStructuredBinderTarget);
        }
        self.structured_binder = Some(binder);
        Ok(self)
    }

    pub fn with_instance_id(mut self, instance_id: InstanceId) -> Self {
        self.structured_binder = None;
        self.instance_id = Some(instance_id);
        self
    }

    pub fn span(&self) -> Option<Span> {
        self.component_ref
            .as_ref()
            .and_then(|reference| (!reference.span().is_dummy()).then_some(reference.span()))
    }

    /// Element reference: this reference with a literal index appended to its
    /// last part, keeping rendered text and structure in lockstep.
    pub fn with_appended_index(&self, index: i64, span: ProvenanceSpan) -> Self {
        let rendered = format!("{}[{index}]", self.as_str());
        match self.component_ref.as_ref() {
            Some(reference) => {
                let mut parts = reference.parts().to_vec();
                parts
                    .last_mut()
                    .expect("checked component references are nonempty")
                    .subs
                    .push(Subscript::generated_index_with_provenance(index, span));
                let reference = reference
                    .with_replaced_parts(parts)
                    .expect("appending a subscript preserves every exact part identity");
                Self::with_component_reference(rendered, reference)
                    .with_optional_instance_id(self.instance_id)
                    .with_optional_resolved_function(self.resolved_function)
            }
            _ if self.generated => Self::generated(rendered)
                .with_optional_instance_id(self.instance_id)
                .with_optional_resolved_function(self.resolved_function),
            _ => Self::new(rendered)
                .with_optional_instance_id(self.instance_id)
                .with_optional_resolved_function(self.resolved_function),
        }
    }

    /// Member reference: this reference with a field part appended, keeping
    /// rendered text and structure in lockstep.
    pub fn with_appended_field(
        &self,
        field: &str,
        def_id: DefId,
        span: ProvenanceSpan,
    ) -> Result<Self, ComponentReferenceError> {
        let rendered = format!("{}.{field}", self.as_str());
        let reference = self
            .component_ref
            .as_ref()
            .ok_or(ComponentReferenceError::MissingStructuredBase)?;
        let mut parts = reference.parts().to_vec();
        parts.push(ComponentRefPart {
            ident: field.to_string(),
            span: span.span(),
            subs: Vec::new(),
            def_id,
        });
        let extended = ComponentReference::construct(reference.local(), reference.span(), parts)?;
        Ok(Self {
            name: VarName::new(rendered),
            component_ref: Some(extended),
            resolved_function: None,
            instance_id: self.instance_id,
            structured_binder: None,
            generated: self.generated,
        })
    }

    fn with_optional_resolved_function(
        mut self,
        resolved: Option<ResolvedFunctionReference>,
    ) -> Self {
        if resolved.is_some() {
            self.structured_binder = None;
        }
        self.resolved_function = resolved;
        self
    }

    fn with_optional_instance_id(mut self, instance_id: Option<InstanceId>) -> Self {
        if instance_id.is_some() {
            self.structured_binder = None;
        }
        self.instance_id = instance_id;
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
            .map(ComponentReference::parts)
            .unwrap_or(&[])
    }

    pub fn target_def_id(&self) -> Option<DefId> {
        self.component_ref
            .as_ref()
            .map(ComponentReference::target_def_id)
    }

    pub fn root_def_id(&self) -> Option<DefId> {
        self.component_ref
            .as_ref()
            .map(ComponentReference::root_def_id)
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
            && self.instance_id == other.instance_id
            && self.structured_binder == other.structured_binder
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
    /// Clock constructor: Clock(...)
    Clock,
    /// Clocked-to-continuous value conversion: hold(u)
    Hold,
    /// Previous value on the owning clock: previous(u)
    Previous,
    /// Interval of the owning clock: interval(u)
    Interval,
    /// Integer sub-clock conversion: subSample(u, factor)
    SubSample,
    /// Integer super-clock conversion: superSample(u, factor)
    SuperSample,
    /// Rational forward phase shift: shiftSample(u, counter[, resolution])
    ShiftSample,
    /// Rational backward phase shift: backSample(u, counter[, resolution])
    BackSample,
    /// Remove a clock association: noClock(u)
    NoClock,
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
    /// Transport delay: delay(expr, delayTime[, delayMax]).
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
    /// Intrinsics whose spelling may be shadowed and therefore require their
    /// exact predefined Resolve identity.
    pub const PREDEFINED_IDENTITY_REQUIRED: &'static [Self] = &[
        Self::Sample,
        Self::Clock,
        Self::Hold,
        Self::Previous,
        Self::Interval,
        Self::SubSample,
        Self::SuperSample,
        Self::ShiftSample,
        Self::BackSample,
        Self::NoClock,
        Self::Sum,
        Self::Product,
    ];

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
        Self::Clock,
        Self::Hold,
        Self::Previous,
        Self::Interval,
        Self::SubSample,
        Self::SuperSample,
        Self::ShiftSample,
        Self::BackSample,
        Self::NoClock,
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

    /// Whether Resolve identity is required before this intrinsic can be
    /// distinguished from a same-spelling user declaration.
    pub const fn requires_predefined_identity(self) -> bool {
        matches!(
            self,
            Self::Sample
                | Self::Clock
                | Self::Hold
                | Self::Previous
                | Self::Interval
                | Self::SubSample
                | Self::SuperSample
                | Self::ShiftSample
                | Self::BackSample
                | Self::NoClock
                | Self::Sum
                | Self::Product
        )
    }

    /// Inclusive positional-argument bounds for this builtin.
    ///
    /// `None` as the upper bound denotes a variadic tail. Keeping this table on
    /// the shared builtin identity prevents type checking, constant evaluation,
    /// and later checked-IR consumers from inventing independent signatures.
    pub const fn argument_count_range(self) -> (usize, Option<usize>) {
        use BuiltinFunction::{
            Abs, Acos, Asin, Atan, Atan2, BackSample, Cat, Ceil, Change, Clock, Cos, Cosh, Cross,
            Delay, Der, Diagonal, Div, Edge, Exp, Fill, Floor, Hold, Homotopy, Identity, Initial,
            Integer, Interval, Linspace, Log, Log10, Matrix, Max, Min, Mod, Ndims, NoClock,
            NoEvent, Ones, OuterProduct, Pre, Previous, Product, Reinit, Rem, Sample, Scalar,
            SemiLinear, ShiftSample, Sign, Sin, Sinh, Size, Skew, Smooth, Sqrt, SubSample, Sum,
            SuperSample, Symmetric, Tan, Tanh, Terminal, Transpose, Vector, Zeros,
        };

        match self {
            Initial | Terminal => (0, Some(0)),
            Der | Pre | Abs | Sign | Sqrt | Floor | Ceil | Sin | Cos | Tan | Asin | Acos | Atan
            | Sinh | Cosh | Tanh | Exp | Log | Log10 | Edge | Change | NoEvent | Integer | Sum
            | Product | Ndims | Scalar | Vector | Matrix | Identity | Diagonal | Transpose
            | Symmetric | Skew | Hold | Previous | NoClock => (1, Some(1)),
            Div | Mod | Rem | Atan2 | Smooth | Homotopy | Reinit | OuterProduct | Cross => {
                (2, Some(2))
            }
            SemiLinear | Linspace => (3, Some(3)),
            Min | Max | Size | Sample | SubSample | SuperSample => (1, Some(2)),
            Clock => (0, Some(2)),
            Interval => (0, Some(1)),
            Delay | ShiftSample | BackSample => (2, Some(3)),
            Zeros | Ones => (1, None),
            Fill => (2, None),
            Cat => (3, None),
        }
    }

    /// Ordered formal-parameter names for the builtin operators that MLS gives
    /// named formals, used to project named actuals into positional order.
    ///
    /// Only operators whose named-argument spelling is defined by the language
    /// or exercised by the standard library are listed; every other operator
    /// returns an empty slice so that a named actual to it is refused rather
    /// than bound against an invented name.
    ///
    /// - `homotopy(actual, simplified)` per MLS 3.6 §3.7.2.5.
    /// - `Clock(c, solverMethod)`, the solver-clock constructor of MLS 3.6
    ///   §16.3. `solverMethod` is the only Clock formal the standard library
    ///   passes by name, and it always trails the clock argument `c`; the other
    ///   Clock overloads are only ever called positionally, so their formals do
    ///   not need naming here.
    pub const fn named_formals(self) -> &'static [&'static str] {
        match self {
            Self::Homotopy => &["actual", "simplified"],
            Self::Clock => &["c", "solverMethod"],
            _ => &[],
        }
    }

    /// Whether `actual` positional arguments satisfy this builtin's signature.
    pub const fn accepts_argument_count(self, actual: usize) -> bool {
        let (minimum, maximum) = self.argument_count_range();
        actual >= minimum
            && match maximum {
                Some(maximum) => actual <= maximum,
                None => true,
            }
    }

    /// Try to parse a builtin spelling that needs no declaration check.
    ///
    /// Synchronous intrinsics are intentionally absent: they must be minted
    /// from their exact predefined `DefId` after Resolve.
    pub fn from_name(name: &str) -> Option<Self> {
        match name {
            // Differential. `der` is a reserved grammar production carried
            // by the AST's dedicated derivative-call arm.
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
            "initial" => Some(Self::Initial),
            "terminal" => Some(Self::Terminal),
            "noEvent" => Some(Self::NoEvent),
            "smooth" => Some(Self::Smooth),
            "homotopy" => Some(Self::Homotopy),
            "semiLinear" => Some(Self::SemiLinear),
            "delay" => Some(Self::Delay),
            "integer" | "Integer" => Some(Self::Integer),
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
            Self::Clock => "Clock",
            Self::Hold => "hold",
            Self::Previous => "previous",
            Self::Interval => "interval",
            Self::SubSample => "subSample",
            Self::SuperSample => "superSample",
            Self::ShiftSample => "shiftSample",
            Self::BackSample => "backSample",
            Self::NoClock => "noClock",
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

/// The effective MLS 3.6 §4.8.1 `fixed` attribute, total by construction.
///
/// Modelica source spells `fixed` as an optional boolean modification; the
/// declaration may omit it, and the default depends on the variable's role:
/// `true` for parameters and constants, `false` for every other variable.
/// Products at and after the checked DAE never carry that absence. The one
/// place an omitted spelling becomes a semantic value is DAE variable
/// definition, which owns the role; everything downstream copies the total
/// value.
///
/// There is deliberately no `Default` impl: a `Fixity` cannot be filled in by
/// struct-update or derived defaults, so a product that forgets to decide the
/// attribute fails to construct instead of silently defaulting a second time.
///
/// The wire shape is the plain boolean the attribute means: `Fixed`
/// serializes as `true` and `Free` as `false`; absent or null is rejected.
///
/// The variants deliberately avoid FMI vocabulary: FMI's
/// `initial = exact / approximate` classification is a different concept that
/// belongs to `SolveStateInitialization`, and the mapping between the two
/// happens only at the Solve/FMI projection boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(from = "bool", into = "bool")]
pub enum Fixity {
    /// Effective `fixed = true`: MLS 3.6 §8.6 adds `v = startExpression` to
    /// the initialization equations, and a `fixed = false` parameter cannot
    /// hide behind it.
    Fixed,
    /// Effective `fixed = false`: the `start` value is only an initialization
    /// guess, and a parameter with this fixity is an initialization unknown.
    Free,
}

impl From<bool> for Fixity {
    /// Embed an explicit source spelling: `fixed = true` is [`Fixity::Fixed`],
    /// `fixed = false` is [`Fixity::Free`]. Only an explicit boolean converts;
    /// an absent spelling has no embedding and must go through the
    /// role-defaulting decision at DAE variable definition.
    fn from(declared: bool) -> Self {
        if declared { Self::Fixed } else { Self::Free }
    }
}

impl From<Fixity> for bool {
    /// Project the total value back to the boolean the MLS attribute means,
    /// for presentation surfaces (JSON payloads, language bindings) that
    /// expose `fixed` as a plain boolean.
    fn from(fixity: Fixity) -> Self {
        matches!(fixity, Fixity::Fixed)
    }
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

/// Semantic form of a user-function expression.
///
/// An invocation computes the function's outputs. An MLS §12.4.2.1 partial
/// application (`function F(bound = value)`) computes a function value and
/// must never be inferred from the presence of named arguments on an ordinary
/// invocation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FunctionCallKind {
    Invocation,
    PartialApplication,
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Real(v) => write!(f, "{}", v),
            Literal::Integer(v) => write!(f, "{}", v),
            Literal::Boolean(v) => write!(f, "{}", v),
            Literal::String(v) => write!(f, "\"{}\"", crate::escape_modelica_string(v)),
        }
    }
}

/// One structured annotation attached to an external function interface.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExternalFunctionAnnotation {
    /// Structured annotation name segments, such as `["Library"]`.
    pub name: Vec<String>,
    /// Semantic annotation value; never a rendered source-expression string.
    pub value: Expression,
    /// Source span of the complete annotation modification.
    #[serde(
        default = "Span::source_free_serde_default",
        skip_serializing_if = "Span::is_dummy"
    )]
    pub span: Span,
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
    /// Ordered argument expressions passed to the external function.
    ///
    /// MLS §12.9 permits arbitrary expressions here. Keeping the expressions
    /// in Flat/DAE form preserves ABI position, shape, declaration identity,
    /// and source provenance without recovering semantics from rendered text.
    pub args: Vec<Expression>,
    /// Structured annotations attached to the external function interface.
    pub annotations: Vec<ExternalFunctionAnnotation>,
}

/// What a function declaration's `Inline`/`LateInline` annotation asks for
/// (MLS §18.3).
///
/// This is the author's request, not the compiler's decision. A backend that
/// can substitute a body reads it as the highest authority it has to answer to
/// (`Never` is absolute; `Requested` asks and may still be declined for
/// legality), and a backend that cannot substitute bodies ignores it, since
/// both spellings are annotations and neither changes what the function means.
///
/// `LateInline = true` reads as `Requested` here. The two differ in *when* a
/// symbolic pipeline substitutes the body, and a compiler that substitutes at
/// one point only has one answer to give: the author asked for the call to
/// disappear.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InlineAnnotation {
    /// The declaration wrote neither `Inline` nor `LateInline`, or wrote
    /// `LateInline = false`, which asks for nothing.
    #[default]
    Unstated,
    /// `annotation(Inline = true)` or `annotation(LateInline = true)`.
    Requested,
    /// `annotation(Inline = false)`. Absolute: no policy raises it.
    Never,
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
        is_constructor: bool,
        /// Exact invocation/function-value distinction preserved from AST.
        call_kind: FunctionCallKind,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    /// One semantically resolved invocation of the predefined `String`
    /// conversion operator (MLS §3.7.1).
    ///
    /// Keeping this distinct from a user function or type constructor makes
    /// the accepted overload explicit and retains the resolved predefined
    /// declaration identity without marker calls or argument-name strings.
    StringConversion {
        declaration: DefId,
        value: Box<Expression>,
        format: StringConversionFormat,
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
        field_def_id: DefId,
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

/// Return the concrete component path denoted by a Flat/DAE expression.
///
/// Projected record fields and expanded component-array elements are represented
/// structurally as `FieldAccess` and `Index` nodes. Evaluators must retain those
/// indices when looking up a parameter such as `records[1,2].n`; rendering only
/// the base field silently falls back to declaration defaults.
pub fn flat_expression_component_path(expr: &Expression) -> Option<ComponentPath> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => append_concrete_subscripts(ComponentPath::from_flat_path(name.as_str()), subscripts),
        Expression::Index {
            base, subscripts, ..
        } => append_concrete_subscripts(flat_expression_component_path(base)?, subscripts),
        Expression::FieldAccess { base, field, .. } => Some(
            flat_expression_component_path(base)?.join(&ComponentPath::from_parts([field.clone()])),
        ),
        _ => None,
    }
}

fn append_concrete_subscripts(
    path: ComponentPath,
    subscripts: &[Subscript],
) -> Option<ComponentPath> {
    if subscripts.is_empty() {
        return Some(path);
    }
    let mut parts = path.into_parts();
    let last = parts.last_mut()?;
    let mut values = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let value = match subscript {
            Subscript::Index { value, .. } => *value,
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => *value,
                _ => return None,
            },
            Subscript::Colon { .. } => return None,
        };
        values.push(value.to_string());
    }
    last.push('[');
    last.push_str(&values.join(","));
    last.push(']');
    Some(ComponentPath::from_parts(parts))
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
            | Expression::StringConversion { span, .. }
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

    fn map_span(mut self, f: impl FnOnce(Span) -> Span) -> Self {
        let span_slot = match &mut self {
            Expression::Binary { span, .. }
            | Expression::Unary { span, .. }
            | Expression::VarRef { span, .. }
            | Expression::BuiltinCall { span, .. }
            | Expression::FunctionCall { span, .. }
            | Expression::StringConversion { span, .. }
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

    pub fn contains_subexpression(&self, mut predicate: impl FnMut(&Expression) -> bool) -> bool {
        let mut checker = ContainsExpressionChecker {
            found: false,
            predicate: &mut predicate,
        };
        crate::ExpressionVisitor::visit_expression(&mut checker, self);
        checker.found
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

/// The two mutually exclusive formatting overload families of predefined
/// `String(...)`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum StringConversionFormat {
    Options {
        minimum_length: Option<Box<Expression>>,
        left_justified: Option<Box<Expression>>,
        significant_digits: Option<Box<Expression>>,
    },
    Format {
        value: Box<Expression>,
    },
}

impl StringConversionFormat {
    pub fn operands(&self) -> impl Iterator<Item = &Expression> {
        let operands = match self {
            Self::Options {
                minimum_length,
                left_justified,
                significant_digits,
            } => [
                minimum_length.as_deref(),
                left_justified.as_deref(),
                significant_digits.as_deref(),
            ],
            Self::Format { value } => [Some(value.as_ref()), None, None],
        };
        operands.into_iter().flatten()
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

mod expression_semantics;
pub use expression_semantics::{
    expression_semantic_fingerprint, expressions_semantically_equal,
    subscripts_semantic_fingerprint, subscripts_semantically_equal,
};

#[cfg(test)]
mod tests;
