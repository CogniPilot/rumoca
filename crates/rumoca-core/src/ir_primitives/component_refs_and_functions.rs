use super::*;
use crate::{EffectiveType, strip_array_index};

fn deserialize_required_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: serde::Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComprehensionIndex {
    pub name: String,
    pub range: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComponentRefPart {
    pub ident: String,
    pub span: Span,
    pub subs: Vec<Subscript>,
    pub def_id: DefId,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ComponentReference {
    local: bool,
    span: Span,
    parts: Vec<ComponentRefPart>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ComponentReferenceError {
    Empty,
    MissingStructuredBase,
    MissingPartIdentity { part_index: usize },
}

impl std::fmt::Display for ComponentReferenceError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => formatter
                .write_str("component reference requires at least one identity-bearing part"),
            Self::MissingStructuredBase => {
                formatter.write_str("component projection requires a structured base reference")
            }
            Self::MissingPartIdentity { part_index } => write!(
                formatter,
                "component reference part {part_index} has the reserved unresolved DefId(0)"
            ),
        }
    }
}

impl std::error::Error for ComponentReferenceError {}

impl ComponentReference {
    pub fn construct(
        local: bool,
        span: Span,
        parts: Vec<ComponentRefPart>,
    ) -> Result<Self, ComponentReferenceError> {
        if parts.is_empty() {
            return Err(ComponentReferenceError::Empty);
        }
        if let Some(part_index) = parts.iter().position(|part| part.def_id.index() == 0) {
            return Err(ComponentReferenceError::MissingPartIdentity { part_index });
        }
        Ok(Self { local, span, parts })
    }

    pub fn local(&self) -> bool {
        self.local
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn parts(&self) -> &[ComponentRefPart] {
        &self.parts
    }

    /// Rebuild this reference with replacement parts under the same source
    /// span and locality.
    ///
    /// The original reference remains unchanged, and the replacement is
    /// admitted only after replaying the checked construction contract.
    pub fn with_replaced_parts(
        &self,
        parts: Vec<ComponentRefPart>,
    ) -> Result<Self, ComponentReferenceError> {
        Self::construct(self.local, self.span, parts)
    }
}

impl<'de> Deserialize<'de> for ComponentReference {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            local: bool,
            span: Span,
            parts: Vec<ComponentRefPart>,
        }

        let wire = Wire::deserialize(deserializer)?;
        Self::construct(wire.local, wire.span, wire.parts).map_err(serde::de::Error::custom)
    }
}

impl ComponentReference {
    pub fn component_scope(&self) -> ComponentReferenceScope<'_> {
        ComponentReferenceScope::new(&self.parts)
    }

    pub fn to_var_name(&self) -> VarName {
        ComponentPath::from_component_reference(self).name
    }

    pub fn last_ident(&self) -> Option<&str> {
        self.parts.last().map(|part| part.ident.as_str())
    }

    pub fn root_def_id(&self) -> DefId {
        self.parts[0].def_id
    }

    pub fn target_def_id(&self) -> DefId {
        self.parts[self.parts.len() - 1].def_id
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ComponentReferenceScope<'a> {
    parts: &'a [ComponentRefPart],
}

impl<'a> ComponentReferenceScope<'a> {
    pub fn new(parts: &'a [ComponentRefPart]) -> Self {
        Self { parts }
    }

    pub fn parts(self) -> &'a [ComponentRefPart] {
        self.parts
    }

    pub fn leaf_ident(self) -> Option<&'a str> {
        self.parts.last().map(|part| part.ident.as_str())
    }

    pub fn prefix_parts(self) -> &'a [ComponentRefPart] {
        self.parts
            .len()
            .checked_sub(1)
            .map_or(&[], |end| &self.parts[..end])
    }
}

/// Owned component-reference path used for scope-aware lookups.
///
/// This type keeps path segmentation centralized so phases do not recover
/// scope by ad hoc string splitting. Its textual rendering is still the flat
/// IR spelling because Flat/DAE maps are serialized by name.
///
/// **Invariant:** `name` is the interned form of `parts.join(".")`, maintained
/// by every constructor. `name` is the path's identity (see the `PartialEq` and
/// `Hash` impls below); `parts` is the segmentation payload.
#[derive(Debug, Clone)]
pub struct ComponentPath {
    name: VarName,
    parts: Vec<String>,
}

// Serialize as the flat textual path (e.g. `bus[data.medium].pin.v`) so that
// maps keyed by `ComponentPath` round-trip through JSON (which requires string
// keys) and match the "serialized by name" convention used by Flat/DAE IR.
impl serde::Serialize for ComponentPath {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> serde::Deserialize<'de> for ComponentPath {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let flat = String::deserialize(deserializer)?;
        Ok(Self::from_flat_path(&flat))
    }
}

impl ComponentPath {
    pub fn root() -> Self {
        Self::from_parts(std::iter::empty::<String>())
    }

    pub fn from_flat_path(path: &str) -> Self {
        // The interner has already segmented every name it has seen; reuse
        // those boundaries instead of re-parsing. `from_parts` re-joins (and
        // thereby normalizes empty segments), so only fall back to it when
        // normalization would change the text.
        let interned = VarName::new(path);
        let parts: Vec<String> = interned
            .segments()
            .into_iter()
            .map(ToString::to_string)
            .collect();
        if interned.as_str().len() == path.len() && !parts.is_empty() {
            let joined_len: usize = parts.iter().map(|part| part.len() + 1).sum::<usize>() - 1;
            if joined_len == path.len() {
                return Self {
                    name: interned,
                    parts,
                };
            }
        }
        Self::from_parts(parts)
    }

    pub fn from_parts(parts: impl IntoIterator<Item = impl Into<String>>) -> Self {
        let parts = parts.into_iter().map(Into::into).collect::<Vec<_>>();
        let name = VarName::new(parts.join("."));
        Self { name, parts }
    }

    pub fn from_reference(reference: &Reference) -> Self {
        reference
            .component_ref()
            .map(Self::from_component_reference)
            .unwrap_or_else(|| Self::from_flat_path(reference.as_str()))
    }

    pub fn from_component_reference(reference: &ComponentReference) -> Self {
        Self::from_parts(reference.parts.iter().map(render_component_path_part))
    }

    pub fn is_root(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    pub fn len(&self) -> usize {
        self.parts.len()
    }

    pub fn parts(&self) -> &[String] {
        &self.parts
    }

    pub fn into_parts(self) -> Vec<String> {
        self.parts
    }

    pub fn parent(&self) -> Option<Self> {
        (!self.parts.is_empty())
            .then(|| Self::from_parts(self.parts[..self.parts.len() - 1].iter().cloned()))
    }

    pub fn prefix(&self, end: usize) -> Option<Self> {
        (end <= self.parts.len()).then(|| Self::from_parts(self.parts[..end].iter().cloned()))
    }

    pub fn suffix_from(&self, start: usize) -> Option<Self> {
        (start <= self.parts.len()).then(|| Self::from_parts(self.parts[start..].iter().cloned()))
    }

    pub fn starts_with(&self, prefix: &Self) -> bool {
        !prefix.parts.is_empty()
            && prefix.parts.len() <= self.parts.len()
            && self.parts[..prefix.parts.len()] == prefix.parts[..]
    }

    pub fn strip_prefix(&self, prefix: &Self) -> Option<Self> {
        if prefix.is_root() {
            return Some(self.clone());
        }
        self.starts_with(prefix)
            .then(|| Self::from_parts(self.parts[prefix.parts.len()..].iter().cloned()))
    }

    pub fn join(&self, relative: &Self) -> Self {
        if self.is_root() {
            return relative.clone();
        }
        if relative.is_root() {
            return self.clone();
        }
        let mut parts = self.parts.clone();
        parts.extend(relative.parts.iter().cloned());
        Self::from_parts(parts)
    }

    pub fn join_part_slice(&self, relative_parts: &[String]) -> Self {
        if self.is_root() {
            return Self::from_parts(relative_parts.iter().cloned());
        }
        if relative_parts.is_empty() {
            return self.clone();
        }
        let mut parts = Vec::with_capacity(self.parts.len() + relative_parts.len());
        parts.extend(self.parts.iter().cloned());
        parts.extend(relative_parts.iter().cloned());
        Self::from_parts(parts)
    }

    pub fn to_flat_string(&self) -> String {
        self.name.as_str().to_string()
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }

    /// Interned identity of this path, computed once at construction.
    ///
    /// Callers that key a map on a path prefix should hold this rather than a
    /// `&[String]` slice: comparing the id is one `u32` compare, comparing the
    /// slice walks every segment.
    pub fn var_name(&self) -> &VarName {
        &self.name
    }
}

impl Default for ComponentPath {
    fn default() -> Self {
        Self::root()
    }
}

// Identity is the interned `name`, not the `parts` vector. Both constructors
// keep `name == parts.join(".")` (`from_parts` joins; `from_flat_path` splits
// on the interner's own segmentation and falls back to `from_parts` whenever
// re-joining would not reproduce the input), so the two are the same partition
// — but `parts` is a `Vec<String>`, and comparing or hashing it walks every
// segment of a flattened component path on every map probe. `name` is a
// `VarName`, whose identity is a `VarNameId(u32)` the interner assigned once.
//
// `parts` therefore stays as the *segmentation payload* (rendering, prefixing,
// scope walks) and is no longer identity. Interning the parts individually was
// the alternative, and is worse on both counts: it would intern N segments per
// path instead of one whole path, and callers ask for `&[String]` slices.
impl PartialEq for ComponentPath {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for ComponentPath {}

impl Hash for ComponentPath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl std::fmt::Display for ComponentPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_flat_string())
    }
}

/// Candidate flat keys for resolving `name` from `scope` outward to root.
pub fn scoped_component_path_candidates(
    name: &ComponentPath,
    scope: &ComponentPath,
) -> Vec<String> {
    let mut candidates = Vec::new();
    let mut current = Some(scope.clone());
    while let Some(scope_path) = current {
        candidates.push(scope_path.join(name).to_flat_string());
        current = scope_path.parent();
    }
    candidates
}

fn render_component_path_part(part: &ComponentRefPart) -> String {
    if part.subs.is_empty() {
        return part.ident.clone();
    }
    let subs = part
        .subs
        .iter()
        .map(render_component_path_subscript)
        .collect::<Vec<_>>()
        .join(",");
    format!("{}[{subs}]", part.ident)
}

fn render_component_path_subscript(subscript: &Subscript) -> String {
    match subscript {
        Subscript::Index { value, .. } => value.to_string(),
        Subscript::Colon { .. } => ":".to_string(),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::VarRef { name, .. } => name.to_string(),
            Expression::Literal { value, .. } => value.to_string(),
            _ => format!("{expr:?}"),
        },
    }
}

impl std::fmt::Display for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_var_name())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ForIndex {
    pub ident: String,
    pub range: Expression,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StatementBlock {
    pub cond: Expression,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Statement {
    Empty {
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Assignment {
        comp: ComponentReference,
        value: Expression,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Return {
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Break {
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Statement>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    While {
        block: StatementBlock,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    If {
        cond_blocks: Vec<StatementBlock>,
        else_block: Option<Vec<Statement>>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    When {
        blocks: Vec<StatementBlock>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    FunctionCall {
        /// Callable identity. Flattening resolves this to the exact collected
        /// function instance before the statement crosses the Flat boundary.
        comp: Reference,
        args: Vec<Expression>,
        /// Positional output targets. `None` preserves an MLS skipped output
        /// slot such as the middle entry in `(x, , z) := f()`.
        outputs: Vec<Option<ComponentReference>>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Reinit {
        variable: ComponentReference,
        value: Expression,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
    Assert {
        condition: Expression,
        message: Box<Expression>,
        level: Option<Box<Expression>>,
        #[serde(
            default = "Span::source_free_serde_default",
            skip_serializing_if = "Span::is_dummy"
        )]
        span: Span,
    },
}

impl Statement {
    pub fn with_span(self, span: Span) -> Self {
        if span.is_dummy() {
            self
        } else {
            self.map_span(|_| span)
        }
    }

    pub fn source_span(&self) -> Option<Span> {
        let span = match self {
            Statement::Empty { span }
            | Statement::Assignment { span, .. }
            | Statement::Return { span }
            | Statement::Break { span }
            | Statement::For { span, .. }
            | Statement::While { span, .. }
            | Statement::If { span, .. }
            | Statement::When { span, .. }
            | Statement::FunctionCall { span, .. }
            | Statement::Reinit { span, .. }
            | Statement::Assert { span, .. } => *span,
        };
        (!span.is_dummy()).then_some(span)
    }

    pub fn as_unspanned(&self) -> &Statement {
        self
    }

    fn map_span(mut self, f: impl FnOnce(Span) -> Span) -> Self {
        let span_slot = match &mut self {
            Statement::Empty { span }
            | Statement::Assignment { span, .. }
            | Statement::Return { span }
            | Statement::Break { span }
            | Statement::For { span, .. }
            | Statement::While { span, .. }
            | Statement::If { span, .. }
            | Statement::When { span, .. }
            | Statement::FunctionCall { span, .. }
            | Statement::Reinit { span, .. }
            | Statement::Assert { span, .. } => span,
        };
        *span_slot = f(*span_slot);
        self
    }
}

pub fn extract_algorithm_outputs(statements: &[Statement]) -> Vec<Reference> {
    let mut outputs = Vec::new();
    for statement in statements {
        collect_statement_outputs(statement, &mut outputs);
    }
    outputs
}

pub fn component_ref_to_base_reference(comp: &ComponentReference) -> Reference {
    let component_ref = ComponentReference::construct(
        comp.local(),
        comp.span(),
        comp.parts()
            .iter()
            .map(|part| ComponentRefPart {
                ident: part.ident.clone(),
                span: part.span,
                subs: Vec::new(),
                def_id: part.def_id,
            })
            .collect(),
    )
    .expect("a nonempty reference remains nonempty when subscripts are removed");
    Reference::from_component_reference(component_ref)
}

/// Return a component-path base name with all bracketed subscripts removed.
pub fn component_path_base_name(name: &str) -> Option<String> {
    if name.is_empty() || name.starts_with('.') || name.ends_with('.') || name.contains("..") {
        return None;
    }
    let mut parts = Vec::new();
    for segment in split_path_with_indices(name) {
        let base = strip_array_index(segment);
        if base.is_empty() || base.contains('[') || base.contains(']') {
            return None;
        }
        parts.push(base.to_string());
    }
    (!parts.is_empty()).then(|| parts.join("."))
}

/// Return the dotted base path and 1-based index of an element name whose
/// only subscript is a single trailing literal index.
///
/// Examples: `c[3]` -> `("c", 3)`, `a.b[2]` -> `("a.b", 2)`.
///
/// Returns `None` for mid-path indices (`x[2].y`), multiple or
/// multi-dimensional subscripts (`c[1][2]`, `c[1,2]`), non-positive or
/// non-numeric indices, missing subscripts, and any path
/// [`component_path_base_name`] rejects as malformed.
pub fn component_path_trailing_index(name: &str) -> Option<(String, usize)> {
    let (base, raw_index) = split_trailing_subscript_suffix(name)?;
    // The trailing group must be the only subscript and the base a well-formed
    // path: subscript stripping through `component_path_base_name` must be the
    // identity on `base`.
    let base_path = component_path_base_name(base)?;
    if base_path != base {
        return None;
    }
    let index = raw_index.parse::<usize>().ok()?;
    (index >= 1).then_some((base_path, index))
}

pub(super) fn derivative_state_name(name: &VarName) -> VarName {
    strip_trailing_subscript_suffix(name.as_str()).map_or_else(|| name.clone(), VarName::new)
}

fn collect_statement_outputs(statement: &Statement, outputs: &mut Vec<Reference>) {
    match statement.as_unspanned() {
        Statement::Assignment { comp, .. } => {
            insert_unique(outputs, component_ref_to_base_reference(comp));
        }
        Statement::For { equations, .. } => {
            for statement in equations {
                collect_statement_outputs(statement, outputs);
            }
        }
        Statement::While { block, .. } => collect_statement_block_outputs(block, outputs),
        Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_statement_block_outputs(block, outputs);
            }
            if let Some(else_block) = else_block {
                for statement in else_block {
                    collect_statement_outputs(statement, outputs);
                }
            }
        }
        Statement::When { blocks, .. } => {
            for block in blocks {
                collect_statement_block_outputs(block, outputs);
            }
        }
        Statement::FunctionCall {
            outputs: values, ..
        } => {
            for output in values.iter().flatten() {
                insert_unique(outputs, component_ref_to_base_reference(output));
            }
        }
        Statement::Reinit { variable, .. } => {
            insert_unique(outputs, component_ref_to_base_reference(variable));
        }
        Statement::Empty { .. }
        | Statement::Return { .. }
        | Statement::Break { .. }
        | Statement::Assert { .. } => {}
    }
}

fn collect_statement_block_outputs(block: &StatementBlock, outputs: &mut Vec<Reference>) {
    for statement in &block.stmts {
        collect_statement_outputs(statement, outputs);
    }
}

fn insert_unique(values: &mut Vec<Reference>, value: Reference) {
    if !values.contains(&value) {
        values.push(value);
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Function {
    pub name: VarName,
    /// Exact source declaration exposed by this collected function instance.
    ///
    /// This is distinct from `def_id` for a replaceable function (slot versus
    /// selected implementation) and for an ExternalObject call (owner versus
    /// lifecycle constructor). One Flat function instance owns one exposure.
    pub exposure_def_id: DefId,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub def_id: Option<DefId>,
    #[serde(deserialize_with = "deserialize_required_option")]
    pub instance_id: Option<FunctionInstanceId>,
    pub inputs: Vec<FunctionParam>,
    pub outputs: Vec<FunctionParam>,
    pub locals: Vec<FunctionParam>,
    pub body: Vec<Statement>,
    pub is_constructor: bool,
    /// Constructor-proven MLS §6.4 fact required by automatic function
    /// vectorization (MLS §12.4.6).
    ///
    /// `false` is the conservative wire/default value: an exact selected
    /// function identity alone does not prove that its class and inherited
    /// interface contain no replaceable element.
    pub transitively_non_replaceable: bool,
    /// MLS 3.7 §12.3 written purity prefix: `false` exactly when the
    /// declaration wrote `impure`.
    ///
    /// This is the fact that restricts call contexts, because §12.3 states the
    /// restriction of the prefix itself: "With the prefix keyword impure it is
    /// stated that a Modelica function is impure and it is only allowed to call
    /// such a function from within: …". A declaration that wrote no prefix is
    /// not restricted by that sentence, whatever its body turns out to be.
    pub pure: bool,
    /// MLS 3.7 §12.3: whether the declaration wrote `pure` or `impure` at all.
    ///
    /// "External functions not explicitly declared with pure or impure is
    /// deprecated." The bare form is reported (WR001) and compiled, and its
    /// *body* is impure regardless of `pure`; see [`Function::body_is_pure`].
    pub purity_declared: bool,
    pub external: Option<ExternalFunction>,
    pub derivatives: Vec<DerivativeAnnotation>,
    /// MLS §18.3 `Inline`/`LateInline`, as the declaration wrote it.
    ///
    /// Carried from the source rather than rediscovered later: by the time a
    /// backend decides whether to substitute this body, the annotation
    /// expressions are long gone, and a compiler that has to guess the author's
    /// intent is a compiler that ignores it.
    pub inline: InlineAnnotation,
    pub span: Span,
}

/// Why one function occurrence cannot authorize MLS automatic vectorization.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AutomaticVectorizationRefusal {
    FunctionMayBeReplaceable,
    FunctionHasNoInstanceIdentity,
    OccurrenceMayBeReplaceable,
    InstanceIdentityMismatch {
        occurrence: FunctionInstanceId,
        function: FunctionInstanceId,
    },
}

impl std::fmt::Display for AutomaticVectorizationRefusal {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FunctionMayBeReplaceable => {
                formatter.write_str("the selected function may be replaceable")
            }
            Self::FunctionHasNoInstanceIdentity => {
                formatter.write_str("the selected function has no exact instance identity")
            }
            Self::OccurrenceMayBeReplaceable => {
                formatter.write_str("the call occurrence may traverse a replaceable exposure")
            }
            Self::InstanceIdentityMismatch {
                occurrence,
                function,
            } => write!(
                formatter,
                "call occurrence instance {} differs from selected function instance {}",
                occurrence.index(),
                function.index()
            ),
        }
    }
}

impl std::error::Error for AutomaticVectorizationRefusal {}

/// Exact callable/occurrence authority required by MLS automatic vectorization.
///
/// The private fields bind the proof to the selected function and occurrence;
/// callers can inspect but cannot construct or relabel it.
#[derive(Debug)]
#[must_use]
pub struct AutomaticVectorizationAuthority<'function> {
    function: &'function Function,
    instance: FunctionInstanceId,
}

impl AutomaticVectorizationAuthority<'_> {
    pub fn function(&self) -> &Function {
        self.function
    }

    pub fn instance(&self) -> FunctionInstanceId {
        self.instance
    }
}

impl Function {
    pub fn new(name: impl Into<String>, exposure_def_id: DefId, span: Span) -> Self {
        Self {
            name: VarName::new(name),
            exposure_def_id,
            def_id: None,
            instance_id: None,
            inputs: Vec::new(),
            outputs: Vec::new(),
            locals: Vec::new(),
            body: Vec::new(),
            is_constructor: false,
            transitively_non_replaceable: false,
            pure: true,
            purity_declared: false,
            external: None,
            derivatives: Vec::new(),
            inline: InlineAnnotation::Unstated,
            span,
        }
    }

    /// Issue the exact MLS §12.4.6 automatic-vectorization authority.
    ///
    /// Non-replaceability is required both for the selected function and for
    /// the exact exposure path of this occurrence. Their concrete instance
    /// identities must also agree; declaration or display-name equality is
    /// not callable identity.
    pub fn automatic_vectorization_authority(
        &self,
        occurrence: ResolvedFunctionReference,
    ) -> Result<AutomaticVectorizationAuthority<'_>, AutomaticVectorizationRefusal> {
        if !self.transitively_non_replaceable {
            return Err(AutomaticVectorizationRefusal::FunctionMayBeReplaceable);
        }
        let function_instance = self
            .instance_id
            .ok_or(AutomaticVectorizationRefusal::FunctionHasNoInstanceIdentity)?;
        if !occurrence.transitively_non_replaceable {
            return Err(AutomaticVectorizationRefusal::OccurrenceMayBeReplaceable);
        }
        if occurrence.instance_id != function_instance {
            return Err(AutomaticVectorizationRefusal::InstanceIdentityMismatch {
                occurrence: occurrence.instance_id,
                function: function_instance,
            });
        }
        Ok(AutomaticVectorizationAuthority {
            function: self,
            instance: function_instance,
        })
    }

    /// MLS 3.7 §12.3 purity of this function *body*, as far as one declaration
    /// can state it.
    ///
    /// [`Function::pure`] records the written prefix. Purity of the body is a
    /// different question, and §12.3 answers it normatively: "For purposes of
    /// symbolic transformations and optimizations, the deprecated semantics
    /// above imply that not only the functions explicitly declared impure are
    /// the ones which cannot be treated as pure. Instead, a function shall be
    /// treated as impure in the following cases (applied recursively): It is
    /// declared impure. It is an external function without explicit purity. It
    /// calls another function treated as impure, except when wrapped in
    /// pure(…)."
    ///
    /// This accessor decides the first two cases, which one declaration owns.
    /// The third is a call-graph closure no single declaration carries, so it
    /// is *not* answered here and a caller that needs it must close over the
    /// call graph it owns (rumoca task #76). Until that lands, a Modelica
    /// function that only reaches an impure body through its own calls reads as
    /// pure here, which is exactly the gap #76 names.
    ///
    /// Callability is a separate fact and stays with [`Function::pure`]: the
    /// hard restriction §12.3 states is stated of the written `impure` prefix,
    /// and for the bare external form §12.3 states a *deprecation*, not an
    /// error — the transitional MLS 3.6 wording made that explicit ("assumed to
    /// be impure, but without any restriction on calling them"), and 3.7 keeps
    /// the call legal while deprecating it. So the compiler reports the bare
    /// form and compiles it.
    pub fn body_is_pure(&self) -> bool {
        if self.external.is_some() && !self.purity_declared {
            return false;
        }
        self.pure
    }

    pub fn add_input(&mut self, param: FunctionParam) {
        self.inputs.push(param);
    }

    pub fn add_output(&mut self, param: FunctionParam) {
        self.outputs.push(param);
    }

    pub fn add_local(&mut self, local: FunctionParam) {
        self.locals.push(local);
    }

    pub fn validate_shape_contract(&self) -> Result<(), FunctionShapeContractError> {
        for param in self
            .inputs
            .iter()
            .chain(self.outputs.iter())
            .chain(self.locals.iter())
        {
            param.validate_shape_contract().map_err(|source| {
                FunctionShapeContractError::Param {
                    function: self.name.clone(),
                    source,
                }
            })?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordConstructorLookupError {
    Missing {
        type_name: String,
        type_def_id: DefId,
    },
    Ambiguous {
        type_name: String,
        type_def_id: DefId,
        candidates: Vec<VarName>,
    },
}

impl std::fmt::Display for RecordConstructorLookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Missing {
                type_name,
                type_def_id,
            } => write!(
                f,
                "record type `{type_name}` ({type_def_id}) has no constructor metadata"
            ),
            Self::Ambiguous {
                type_name,
                type_def_id,
                candidates,
            } => write!(
                f,
                "record type `{type_name}` ({type_def_id}) has ambiguous constructor exposures: {}",
                candidates
                    .iter()
                    .map(VarName::as_str)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl std::error::Error for RecordConstructorLookupError {}

/// Resolve constructor metadata for one exposed record type.
///
/// A source `DefId` can legitimately have several flattened exposures. The
/// exposure-qualified type name therefore disambiguates before a unique-
/// identity fallback is allowed.
pub fn resolve_record_constructor<'a>(
    functions: impl IntoIterator<Item = &'a Function>,
    type_name: &str,
    type_def_id: DefId,
) -> Result<&'a Function, RecordConstructorLookupError> {
    let candidates = functions
        .into_iter()
        .filter(|function| function.is_constructor && function.def_id == Some(type_def_id))
        .collect::<Vec<_>>();
    let exact = candidates
        .iter()
        .copied()
        .filter(|function| function.name.as_str() == type_name)
        .collect::<Vec<_>>();
    if let [constructor] = exact.as_slice() {
        return Ok(*constructor);
    }
    if exact.is_empty()
        && let [constructor] = candidates.as_slice()
    {
        return Ok(*constructor);
    }
    if exact.is_empty()
        && let Some(first) = candidates.first().copied()
        && candidates
            .iter()
            .copied()
            .all(|candidate| same_record_layout(first, candidate))
    {
        return candidates
            .into_iter()
            .min_by_key(|function| function.name.as_str())
            .ok_or_else(|| RecordConstructorLookupError::Missing {
                type_name: type_name.to_string(),
                type_def_id,
            });
    }
    if candidates.is_empty() {
        return Err(RecordConstructorLookupError::Missing {
            type_name: type_name.to_string(),
            type_def_id,
        });
    }
    Err(RecordConstructorLookupError::Ambiguous {
        type_name: type_name.to_string(),
        type_def_id,
        candidates: candidates
            .into_iter()
            .map(|function| function.name.clone())
            .collect(),
    })
}

fn same_record_layout(lhs: &Function, rhs: &Function) -> bool {
    lhs.inputs.len() == rhs.inputs.len()
        && lhs.inputs.iter().zip(&rhs.inputs).all(|(lhs, rhs)| {
            lhs.name == rhs.name
                && lhs.type_def_id == rhs.type_def_id
                && lhs.effective_type == rhs.effective_type
                && lhs.type_class == rhs.type_class
        })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionInstanceLookupError {
    Missing(FunctionInstanceId),
    Duplicate(FunctionInstanceId),
}

impl std::fmt::Display for FunctionInstanceLookupError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Missing(instance_id) => {
                write!(f, "function instance {} is missing", instance_id.index())
            }
            Self::Duplicate(instance_id) => write!(
                f,
                "function instance {} has duplicate definitions",
                instance_id.index()
            ),
        }
    }
}

impl std::error::Error for FunctionInstanceLookupError {}

pub fn resolve_function_instance<'a>(
    functions: impl IntoIterator<Item = &'a Function>,
    instance_id: FunctionInstanceId,
) -> Result<&'a Function, FunctionInstanceLookupError> {
    let mut matches = functions
        .into_iter()
        .filter(|function| function.instance_id == Some(instance_id));
    let function = matches
        .next()
        .ok_or(FunctionInstanceLookupError::Missing(instance_id))?;
    if matches.next().is_some() {
        return Err(FunctionInstanceLookupError::Duplicate(instance_id));
    }
    Ok(function)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionShapeContractError {
    Param {
        function: VarName,
        source: FunctionParamShapeContractError,
    },
}

impl FunctionShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::Param { source, .. } => source.span(),
        }
    }
}

impl std::fmt::Display for FunctionShapeContractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param { function, source } => {
                write!(
                    f,
                    "function `{function}` parameter shape contract failed: {source}"
                )
            }
        }
    }
}

impl std::error::Error for FunctionShapeContractError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::Param { source, .. } => Some(source),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct FunctionParam {
    #[serde(deserialize_with = "deserialize_required_option")]
    pub def_id: Option<DefId>,
    /// Resolved source declaration identity of this parameter's type.
    ///
    /// This is distinct from `def_id`, which identifies the parameter
    /// declaration itself. Downstream phases use `type_def_id` for semantic
    /// type metadata lookup instead of reconstructing identity from
    /// `type_name` display text (SPEC_0001).
    #[serde(deserialize_with = "deserialize_required_option")]
    pub type_def_id: Option<DefId>,
    pub name: String,
    pub span: Span,
    /// Exact resolved nominal/canonical type and declared dimensions.
    ///
    /// `type_name` is retained only for readable display. Semantic consumers
    /// classify the value through this checked descriptor.
    pub effective_type: EffectiveType,
    pub type_name: String,
    pub type_class: Option<ClassType>,
    pub shape_expr: Vec<Subscript>,
    pub default: Option<Expression>,
    /// Optional lower bound from the parameter declaration (for example
    /// `Integer i(min=1)`).  Function lowering must retain this metadata so
    /// finite runtime integer domains can be lowered without an interpreter.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub min: Option<Expression>,
    /// Optional upper bound from the parameter declaration.
    #[serde(deserialize_with = "deserialize_required_option")]
    pub max: Option<Expression>,
    pub description: Option<String>,
}

impl FunctionParam {
    pub fn new(
        name: impl Into<String>,
        type_name: impl Into<String>,
        effective_type: EffectiveType,
        span: Span,
    ) -> Self {
        Self {
            def_id: None,
            type_def_id: None,
            name: name.into(),
            span,
            effective_type,
            type_name: type_name.into(),
            type_class: None,
            shape_expr: Vec::new(),
            default: None,
            min: None,
            max: None,
            description: None,
        }
    }

    pub fn dimensions(&self) -> &[i64] {
        self.effective_type.dimensions()
    }

    pub fn with_shape_expr(mut self, shape_expr: Vec<Subscript>) -> Self {
        self.shape_expr = shape_expr;
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_bounds(mut self, min: Option<Expression>, max: Option<Expression>) -> Self {
        self.min = min;
        self.max = max;
        self
    }

    pub fn with_def_id(mut self, def_id: DefId) -> Self {
        self.def_id = Some(def_id);
        self
    }

    pub fn with_type_def_id(mut self, type_def_id: DefId) -> Self {
        self.type_def_id = Some(type_def_id);
        self
    }

    pub fn with_type_class(mut self, type_class: ClassType) -> Self {
        self.type_class = Some(type_class);
        self
    }

    pub fn with_default(mut self, default: Expression) -> Self {
        self.default = Some(default);
        self
    }

    pub fn validate_shape_contract(&self) -> Result<(), FunctionParamShapeContractError> {
        if self.name.is_empty() {
            return Err(FunctionParamShapeContractError::EmptyName { span: self.span });
        }
        if self.type_name.is_empty() {
            return Err(FunctionParamShapeContractError::EmptyTypeName {
                param: self.name.clone(),
                span: self.span,
            });
        }
        if !self.shape_expr.is_empty()
            && self.shape_expr.len() != self.effective_type.dimensions().len()
        {
            return Err(FunctionParamShapeContractError::ShapeExprLengthMismatch {
                param: self.name.clone(),
                dims: self.effective_type.dimensions().len(),
                shape_expr: self.shape_expr.len(),
                span: self.span,
            });
        }
        for subscript in &self.shape_expr {
            if let Subscript::Index { value, .. } = subscript
                && *value < 0
            {
                return Err(FunctionParamShapeContractError::NegativeShapeIndex {
                    param: self.name.clone(),
                    index: *value,
                    span: self.span,
                });
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FunctionParamShapeContractError {
    EmptyName {
        span: Span,
    },
    EmptyTypeName {
        param: String,
        span: Span,
    },
    NegativeShapeIndex {
        param: String,
        index: i64,
        span: Span,
    },
    ShapeExprLengthMismatch {
        param: String,
        dims: usize,
        shape_expr: usize,
        span: Span,
    },
}

impl FunctionParamShapeContractError {
    pub fn span(&self) -> Span {
        match self {
            Self::EmptyName { span }
            | Self::EmptyTypeName { span, .. }
            | Self::NegativeShapeIndex { span, .. }
            | Self::ShapeExprLengthMismatch { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for FunctionParamShapeContractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyName { .. } => write!(f, "function parameter has an empty name"),
            Self::EmptyTypeName { param, .. } => {
                write!(f, "function parameter `{param}` has an empty type name")
            }
            Self::NegativeShapeIndex { param, index, .. } => write!(
                f,
                "function parameter `{param}` has negative shape index {index}"
            ),
            Self::ShapeExprLengthMismatch {
                param,
                dims,
                shape_expr,
                ..
            } => write!(
                f,
                "function parameter `{param}` has {dims} dimensions but {shape_expr} shape expressions"
            ),
        }
    }
}

impl std::error::Error for FunctionParamShapeContractError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn automatic_vectorization_authority_requires_both_proofs_and_exact_identity() {
        let selected = FunctionInstanceId::new(80_001);
        let mut function = Function::new("f", DefId::new(80_000), Span::DUMMY);
        function.instance_id = Some(selected);
        function.transitively_non_replaceable = true;
        let occurrence = ResolvedFunctionReference {
            instance_id: selected,
            base_part_count: 1,
            transitively_non_replaceable: true,
        };

        let authority = function
            .automatic_vectorization_authority(occurrence)
            .expect("matching non-replaceable function and occurrence issue authority");
        assert_eq!(authority.instance(), selected);
        assert!(std::ptr::eq(authority.function(), &function));

        let mut replaceable_function = function.clone();
        replaceable_function.transitively_non_replaceable = false;
        assert!(matches!(
            replaceable_function.automatic_vectorization_authority(occurrence),
            Err(AutomaticVectorizationRefusal::FunctionMayBeReplaceable)
        ));

        let mut unidentified_function = function.clone();
        unidentified_function.instance_id = None;
        assert!(matches!(
            unidentified_function.automatic_vectorization_authority(occurrence),
            Err(AutomaticVectorizationRefusal::FunctionHasNoInstanceIdentity)
        ));

        let unproven_occurrence = ResolvedFunctionReference {
            transitively_non_replaceable: false,
            ..occurrence
        };
        assert!(matches!(
            function.automatic_vectorization_authority(unproven_occurrence),
            Err(AutomaticVectorizationRefusal::OccurrenceMayBeReplaceable)
        ));

        let foreign = FunctionInstanceId::new(80_002);
        let foreign_occurrence = ResolvedFunctionReference {
            instance_id: foreign,
            ..occurrence
        };
        assert!(matches!(
            function.automatic_vectorization_authority(foreign_occurrence),
            Err(AutomaticVectorizationRefusal::InstanceIdentityMismatch {
                occurrence,
                function,
            }) if occurrence == foreign && function == selected
        ));
    }

    #[test]
    fn current_function_wire_rejects_deleted_semantic_keys() {
        let function = Function::new("f", DefId::new(701), Span::DUMMY);
        let complete = serde_json::to_value(function).expect("function serializes");
        assert_eq!(complete["exposure_def_id"], serde_json::json!(701));
        assert!(complete["def_id"].is_null());
        assert!(complete["instance_id"].is_null());
        for key in [
            "exposure_def_id",
            "def_id",
            "instance_id",
            "transitively_non_replaceable",
            "inline",
        ] {
            let mut missing = complete.clone();
            missing
                .as_object_mut()
                .expect("function wire is an object")
                .remove(key)
                .unwrap_or_else(|| panic!("function wire contains `{key}`"));
            assert!(
                serde_json::from_value::<Function>(missing).is_err(),
                "deleted `{key}` must not invent function semantics"
            );
        }
        let mut unknown = complete.clone();
        unknown
            .as_object_mut()
            .expect("function wire is an object")
            .insert("legacy_selected_def_id".into(), serde_json::json!(701));
        assert!(serde_json::from_value::<Function>(unknown).is_err());

        let effective = EffectiveType::new(TypeId::new(1), TypeId::new(1), Vec::new()).unwrap();
        let parameter = FunctionParam::new("x", "Real", effective, Span::DUMMY);
        let complete = serde_json::to_value(parameter).expect("function parameter serializes");
        for key in ["def_id", "type_def_id", "min", "max"] {
            let mut missing = complete.clone();
            missing
                .as_object_mut()
                .expect("function-parameter wire is an object")
                .remove(key)
                .unwrap_or_else(|| panic!("function-parameter wire contains `{key}`"));
            assert!(
                serde_json::from_value::<FunctionParam>(missing).is_err(),
                "deleted `{key}` must not invent parameter semantics"
            );
        }
        let mut unknown = complete;
        unknown
            .as_object_mut()
            .expect("function-parameter wire is an object")
            .insert("legacy_type_name".into(), serde_json::json!("Real"));
        assert!(serde_json::from_value::<FunctionParam>(unknown).is_err());
    }

    #[test]
    fn checked_component_reference_rejects_empty_parts() {
        assert_eq!(
            ComponentReference::construct(false, Span::DUMMY, Vec::new()),
            Err(ComponentReferenceError::Empty)
        );
    }

    #[test]
    fn checked_component_reference_rejects_reserved_identity() {
        assert_eq!(
            ComponentReference::construct(
                false,
                Span::DUMMY,
                vec![ComponentRefPart {
                    ident: "unresolved".to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                    def_id: DefId::new(0),
                }],
            ),
            Err(ComponentReferenceError::MissingPartIdentity { part_index: 0 })
        );
    }

    #[test]
    fn replacing_parts_replays_identity_contract_and_preserves_owner_metadata() {
        let span = Span::from_offsets(SourceId::from_source_name("rewrite.mo"), 4, 10);
        let reference = ComponentReference::construct(
            true,
            span,
            vec![ComponentRefPart {
                ident: "value".to_string(),
                span,
                subs: Vec::new(),
                def_id: DefId::new(17),
            }],
        )
        .expect("test reference is resolved");

        let mut indexed_parts = reference.parts().to_vec();
        indexed_parts[0]
            .subs
            .push(Subscript::Index { value: 2, span });
        let indexed = reference
            .with_replaced_parts(indexed_parts)
            .expect("adding a subscript preserves exact identity");

        assert!(indexed.local());
        assert_eq!(indexed.span(), span);
        assert_eq!(indexed.target_def_id(), DefId::new(17));
        assert_eq!(
            indexed.parts()[0].subs,
            vec![Subscript::Index { value: 2, span }]
        );
        assert!(reference.parts()[0].subs.is_empty());

        let mut unresolved_parts = indexed.parts().to_vec();
        unresolved_parts[0].def_id = DefId::new(0);
        assert_eq!(
            indexed.with_replaced_parts(unresolved_parts),
            Err(ComponentReferenceError::MissingPartIdentity { part_index: 0 })
        );
    }

    #[test]
    fn component_path_from_reference_uses_structured_component_reference() {
        let component_ref = ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts: vec![
                ComponentRefPart {
                    ident: "plant".to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                    def_id: DefId::new(7),
                },
                ComponentRefPart {
                    ident: "motor".to_string(),
                    span: Span::DUMMY,
                    subs: vec![Subscript::Index {
                        value: 2,
                        span: Span::DUMMY,
                    }],
                    def_id: DefId::new(8),
                },
                ComponentRefPart {
                    ident: "tau".to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                    def_id: DefId::new(9),
                },
            ],
        };
        let reference =
            Reference::with_component_reference("flat_display_is_not_authoritative", component_ref);

        let path = ComponentPath::from_reference(&reference);

        assert_eq!(
            path.parts(),
            &[
                "plant".to_string(),
                "motor[2]".to_string(),
                "tau".to_string()
            ]
        );
        assert_eq!(path.to_flat_string(), "plant.motor[2].tau");
    }

    #[test]
    fn record_constructor_lookup_uses_exposure_name_for_shared_definition() {
        let def_id = DefId::new(77);
        let mut first = Function::new("First.State", DefId::new(7_701), Span::DUMMY);
        first.def_id = Some(def_id);
        first.is_constructor = true;
        let mut second = Function::new("Second.State", DefId::new(7_702), Span::DUMMY);
        second.def_id = Some(def_id);
        second.is_constructor = true;

        let resolved = resolve_record_constructor([&first, &second], "Second.State", def_id)
            .expect("exposure name disambiguates a shared declaration");

        assert_eq!(resolved.name.as_str(), "Second.State");
    }

    #[test]
    fn record_constructor_lookup_accepts_equivalent_shared_exposures() {
        let def_id = DefId::new(78);
        let field_type = EffectiveType::new(TypeId::new(12), TypeId::new(12), Vec::new()).unwrap();
        let mut first = Function::new("First.State", DefId::new(7_801), Span::DUMMY);
        first.def_id = Some(def_id);
        first.is_constructor = true;
        first.add_input(FunctionParam::new(
            "x",
            "Real",
            field_type.clone(),
            Span::DUMMY,
        ));
        let mut second = Function::new("Second.State", DefId::new(7_802), Span::DUMMY);
        second.def_id = Some(def_id);
        second.is_constructor = true;
        second.add_input(FunctionParam::new("x", "Real", field_type, Span::DUMMY));

        let resolved = resolve_record_constructor([&second, &first], "Canonical.State", def_id)
            .expect("one declaration may have equivalent flattened exposures");

        assert_eq!(resolved.name.as_str(), "First.State");
    }

    #[test]
    fn record_constructor_lookup_rejects_distinct_shared_layouts() {
        let def_id = DefId::new(79);
        let mut first = Function::new("First.State", DefId::new(7_901), Span::DUMMY);
        first.def_id = Some(def_id);
        first.is_constructor = true;
        first.add_input(FunctionParam::new(
            "x",
            "Real",
            EffectiveType::new(TypeId::new(12), TypeId::new(12), Vec::new()).unwrap(),
            Span::DUMMY,
        ));
        let mut second = Function::new("Second.State", DefId::new(7_902), Span::DUMMY);
        second.def_id = Some(def_id);
        second.is_constructor = true;
        second.add_input(FunctionParam::new(
            "y",
            "Real",
            EffectiveType::new(TypeId::new(12), TypeId::new(12), Vec::new()).unwrap(),
            Span::DUMMY,
        ));

        assert!(matches!(
            resolve_record_constructor([&first, &second], "Canonical.State", def_id),
            Err(RecordConstructorLookupError::Ambiguous { .. })
        ));
    }

    #[test]
    fn function_instance_lookup_rejects_duplicate_identity() {
        let instance_id = FunctionInstanceId::new(9);
        let mut first = Function::new("First.f", DefId::new(8_001), Span::DUMMY);
        first.instance_id = Some(instance_id);
        let mut second = Function::new("Second.f", DefId::new(8_002), Span::DUMMY);
        second.instance_id = Some(instance_id);

        assert_eq!(
            resolve_function_instance([&first, &second], instance_id),
            Err(FunctionInstanceLookupError::Duplicate(instance_id))
        );
    }
}
