use super::*;
use crate::strip_array_index;
use std::borrow::Borrow;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComprehensionIndex {
    pub name: String,
    pub range: Expression,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ComponentRefPart {
    pub ident: String,
    pub span: Span,
    #[serde(default)]
    pub subs: Vec<Subscript>,
}

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct ComponentReference {
    #[serde(default)]
    pub local: bool,
    pub span: Span,
    pub parts: Vec<ComponentRefPart>,
    #[serde(default)]
    pub def_id: Option<DefId>,
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
}

pub fn component_reference_from_flat_name(
    name: &VarName,
    span: Span,
) -> Option<ComponentReference> {
    let parts = split_path_with_indices(name.as_str())
        .into_iter()
        .map(|segment| component_ref_part_from_flat_segment(segment, span))
        .collect::<Option<Vec<_>>>()?;
    (!parts.is_empty()).then_some(ComponentReference {
        local: false,
        span,
        parts,
        def_id: None,
    })
}

fn component_ref_part_from_flat_segment(segment: &str, span: Span) -> Option<ComponentRefPart> {
    let mut base = segment;
    let mut groups = Vec::new();
    while let Some((next_base, raw_subscripts)) = split_trailing_subscript_suffix(base) {
        groups.push(component_ref_subscripts_from_flat_suffix(
            raw_subscripts,
            span,
        )?);
        base = next_base;
    }
    (!base.is_empty()).then_some(ComponentRefPart {
        ident: base.to_string(),
        span,
        subs: groups.into_iter().rev().flatten().collect(),
    })
}

fn component_ref_subscripts_from_flat_suffix(raw: &str, span: Span) -> Option<Vec<Subscript>> {
    raw.split(',')
        .map(str::trim)
        .map(|subscript| match subscript {
            ":" => Some(Subscript::colon(span)),
            _ => subscript
                .parse::<i64>()
                .ok()
                .map(|value| Subscript::index(value, span)),
        })
        .collect()
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

    pub fn parent_ident(self) -> Option<&'a str> {
        self.parts
            .len()
            .checked_sub(2)
            .and_then(|index| self.parts.get(index))
            .map(|part| part.ident.as_str())
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
        Self::from_parts(
            split_path_with_indices(path)
                .into_iter()
                .filter(|part| !part.is_empty()),
        )
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

    pub fn suffixes_excluding_self(&self) -> impl Iterator<Item = Self> + '_ {
        (1..self.parts.len()).map(|start| Self::from_parts(self.parts[start..].iter().cloned()))
    }

    pub fn to_flat_string(&self) -> String {
        self.name.as_str().to_string()
    }

    pub fn as_str(&self) -> &str {
        self.name.as_str()
    }
}

impl Default for ComponentPath {
    fn default() -> Self {
        Self::root()
    }
}

impl PartialEq for ComponentPath {
    fn eq(&self, other: &Self) -> bool {
        self.parts == other.parts
    }
}

impl Eq for ComponentPath {}

impl Hash for ComponentPath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.parts.hash(state);
    }
}

impl Borrow<[String]> for ComponentPath {
    fn borrow(&self) -> &[String] {
        &self.parts
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
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    Assignment {
        comp: ComponentReference,
        value: Expression,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    Return {
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    Break {
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Statement>,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    While {
        block: StatementBlock,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    If {
        cond_blocks: Vec<StatementBlock>,
        else_block: Option<Vec<Statement>>,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    When {
        blocks: Vec<StatementBlock>,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
        outputs: Vec<ComponentReference>,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    Reinit {
        variable: ComponentReference,
        value: Expression,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
    Assert {
        condition: Expression,
        message: Box<Expression>,
        level: Option<Box<Expression>>,
        #[serde(default, skip_serializing_if = "Span::is_dummy")]
        span: Span,
    },
}

impl Default for Statement {
    fn default() -> Self {
        Statement::Empty { span: Span::DUMMY }
    }
}

impl Statement {
    pub fn with_span(self, span: Span) -> Self {
        if span == Span::DUMMY {
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
        (span != Span::DUMMY).then_some(span)
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
    let component_ref = ComponentReference {
        local: comp.local,
        span: comp.span,
        parts: comp
            .parts
            .iter()
            .map(|part| ComponentRefPart {
                ident: part.ident.clone(),
                span: part.span,
                subs: Vec::new(),
            })
            .collect(),
        def_id: comp.def_id,
    };
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

pub(super) fn derivative_state_name(name: &VarName) -> VarName {
    strip_trailing_subscript_suffix(name.as_str()).map_or_else(|| name.clone(), VarName::new)
}

pub(super) fn derivative_name_matches_state(name: &VarName, state: &VarName) -> bool {
    name == state
        || strip_trailing_subscript_suffix(name.as_str()).is_some_and(|base| base == state.as_str())
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
            for output in values {
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
pub struct Function {
    pub name: VarName,
    #[serde(default)]
    pub def_id: Option<DefId>,
    pub inputs: Vec<FunctionParam>,
    pub outputs: Vec<FunctionParam>,
    pub locals: Vec<FunctionParam>,
    pub body: Vec<Statement>,
    pub is_constructor: bool,
    pub pure: bool,
    pub external: Option<ExternalFunction>,
    pub derivatives: Vec<DerivativeAnnotation>,
    pub span: Span,
}

impl Function {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: VarName::new(name),
            def_id: None,
            inputs: Vec::new(),
            outputs: Vec::new(),
            locals: Vec::new(),
            body: Vec::new(),
            is_constructor: false,
            pure: true,
            external: None,
            derivatives: Vec::new(),
            span,
        }
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FunctionParam {
    #[serde(default)]
    pub def_id: Option<DefId>,
    pub name: String,
    pub span: Span,
    pub type_name: String,
    pub type_class: Option<ClassType>,
    pub dims: Vec<i64>,
    pub shape_expr: Vec<Subscript>,
    pub default: Option<Expression>,
    pub description: Option<String>,
}

impl FunctionParam {
    pub fn new(name: impl Into<String>, type_name: impl Into<String>) -> Self {
        Self {
            def_id: None,
            name: name.into(),
            span: Span::DUMMY,
            type_name: type_name.into(),
            type_class: None,
            dims: Vec::new(),
            shape_expr: Vec::new(),
            default: None,
            description: None,
        }
    }

    pub fn with_dims(mut self, dims: Vec<i64>) -> Self {
        self.dims = dims;
        self
    }

    pub fn with_shape_expr(mut self, shape_expr: Vec<Subscript>) -> Self {
        self.shape_expr = shape_expr;
        self
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = span;
        self
    }

    pub fn with_def_id(mut self, def_id: DefId) -> Self {
        self.def_id = Some(def_id);
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
        if !self.shape_expr.is_empty() && self.shape_expr.len() != self.dims.len() {
            return Err(FunctionParamShapeContractError::ShapeExprLengthMismatch {
                param: self.name.clone(),
                dims: self.dims.len(),
                shape_expr: self.shape_expr.len(),
                span: self.span,
            });
        }
        for &dimension in &self.dims {
            if dimension < 0 {
                return Err(FunctionParamShapeContractError::NegativeDimension {
                    param: self.name.clone(),
                    dimension,
                    span: self.span,
                });
            }
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
    NegativeDimension {
        param: String,
        dimension: i64,
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
            | Self::NegativeDimension { span, .. }
            | Self::NegativeShapeIndex { span, .. }
            | Self::ShapeExprLengthMismatch { span, .. } => *span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
                },
                ComponentRefPart {
                    ident: "motor".to_string(),
                    span: Span::DUMMY,
                    subs: vec![Subscript::Index {
                        value: 2,
                        span: Span::DUMMY,
                    }],
                },
                ComponentRefPart {
                    ident: "tau".to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(DefId::new(7)),
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
}
