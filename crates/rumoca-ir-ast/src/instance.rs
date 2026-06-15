//! Instance Tree data structures for the Rumoca compiler.
//!
//! This module defines instance-related types (MLS §5.6), which represent
//! the instantiated elements with merged modifications applied.
//!
//! Uses an overlay approach where instance data is stored separately
//! and keyed by DefId, rather than bloating the core AST types with
//! optional instance fields.

use crate::AstIndexMap as IndexMap;
use indexmap::IndexSet;
use rumoca_core::{
    ComponentPath, ComponentRefPart as CoreComponentRefPart,
    ComponentReference as CoreComponentReference, DefId, ScopeId, Span, Subscript as CoreSubscript,
    TypeId,
};
use serde::{Deserialize, Serialize};

use crate::{
    Causality, ClassTree, ClassType, ComponentReference, Equation, Expression, StateSelect,
    Statement, Variability,
};

type FastIndexMap<K, V> = IndexMap<K, V>;

// =============================================================================
// Core Instance Types
// =============================================================================

/// Unique identifier for an instance in the instance tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct InstanceId(pub u32);

impl InstanceId {
    /// Create a new InstanceId from an index.
    pub fn new(index: u32) -> Self {
        Self(index)
    }

    /// Get the underlying index.
    pub fn index(&self) -> u32 {
        self.0
    }
}

impl std::fmt::Display for InstanceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InstanceId({})", self.0)
    }
}

/// A fully qualified path with resolved subscripts.
///
/// Example: `"body.position[1].x"` would be represented as:
/// `[("body", []), ("position", [1]), ("x", [])]`
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedName {
    /// Sequence of (name, subscripts) pairs.
    pub parts: Vec<(String, Vec<i64>)>,
}

impl QualifiedName {
    /// Create a new empty qualified name.
    pub fn new() -> Self {
        Self { parts: Vec::new() }
    }

    /// Create a qualified name from a single identifier.
    pub fn from_ident(name: &str) -> Self {
        Self {
            parts: vec![(name.to_string(), Vec::new())],
        }
    }

    /// Create a qualified name from a structured component reference.
    pub fn from_component_reference(reference: &ComponentReference) -> Self {
        Self {
            parts: reference
                .parts
                .iter()
                .map(|part| (part.to_string(), Vec::new()))
                .collect(),
        }
    }

    /// Create a qualified name from a dot-separated string.
    ///
    /// Example: "x.start" becomes `[("x", []), ("start", [])]`
    ///
    /// Note: This does not handle array subscripts in the string format.
    /// For paths with subscripts, use the structured API instead.
    pub fn from_dotted(s: &str) -> Self {
        let parts: Vec<(String, Vec<i64>)> = rumoca_core::ComponentPath::from_flat_path(s)
            .into_parts()
            .into_iter()
            .map(|p| (p, Vec::new()))
            .collect();
        Self { parts }
    }

    /// Check if this qualified name starts with a given component name.
    ///
    /// Returns true if the first part matches the given name (ignoring subscripts).
    ///
    /// # Example
    /// ```ignore
    /// let qn = QualifiedName::from_dotted("l2.x.start");
    /// assert!(qn.starts_with("l2"));
    /// assert!(!qn.starts_with("l1"));
    /// ```
    pub fn starts_with(&self, prefix_name: &str) -> bool {
        self.parts
            .first()
            .map(|(name, _)| name == prefix_name)
            .unwrap_or(false)
    }

    /// Strip a single-component prefix from this qualified name.
    ///
    /// If the first part matches `prefix_name`, returns a new QualifiedName
    /// with the first part removed (preserving subscripts on remaining parts).
    ///
    /// Returns `None` if the name doesn't start with the prefix or has only one part.
    ///
    /// # Example
    /// ```ignore
    /// let qn = QualifiedName::from_dotted("l2.x.start");
    /// let stripped = qn.strip_prefix("l2").unwrap();
    /// assert_eq!(stripped.to_flat_string(), "x.start");
    /// ```
    pub fn strip_prefix(&self, prefix_name: &str) -> Option<Self> {
        if self.starts_with(prefix_name) && self.parts.len() > 1 {
            Some(Self {
                parts: self.parts[1..].to_vec(),
            })
        } else {
            None
        }
    }

    /// Get the first component name, if any.
    pub fn first_name(&self) -> Option<&str> {
        self.parts.first().map(|(name, _)| name.as_str())
    }

    /// Get the last component name, if any.
    pub fn last_name(&self) -> Option<&str> {
        self.parts.last().map(|(name, _)| name.as_str())
    }

    /// Return this path's parent scope.
    pub fn parent(&self) -> Option<Self> {
        if self.parts.is_empty() {
            return None;
        }
        Some(Self {
            parts: self.parts[..self.parts.len() - 1].to_vec(),
        })
    }

    /// Return true when this path contains more than one top-level segment.
    pub fn is_dotted(&self) -> bool {
        self.parts.len() > 1
    }

    /// Append a relative path to this path.
    pub fn join(&self, relative: &Self) -> Self {
        if self.is_empty() {
            return relative.clone();
        }
        if relative.is_empty() {
            return self.clone();
        }
        let mut parts = self.parts.clone();
        parts.extend(relative.parts.iter().cloned());
        Self { parts }
    }

    /// Return true when this path ends with the supplied top-level path.
    pub fn ends_with_path(&self, suffix: &Self) -> bool {
        if suffix.parts.is_empty() || suffix.parts.len() > self.parts.len() {
            return false;
        }
        self.parts[self.parts.len() - suffix.parts.len()..]
            .iter()
            .zip(&suffix.parts)
            .all(|((lhs_name, lhs_subs), (rhs_name, rhs_subs))| {
                lhs_name == rhs_name && lhs_subs == rhs_subs
            })
    }

    /// Return true when this path starts with a structured component path.
    pub fn starts_with_component_path(&self, prefix: &ComponentPath) -> bool {
        if prefix.is_root() || prefix.len() > self.parts.len() {
            return false;
        }
        self.parts
            .iter()
            .zip(prefix.parts())
            .all(|((name, subs), prefix_part)| {
                if subs.is_empty() {
                    name == prefix_part
                } else {
                    subscripted_part_matches_rendered(name, subs, prefix_part)
                }
            })
    }

    fn render_part(&self, name: &str, subs: &[i64]) -> String {
        let mut rendered = name.to_string();
        if !subs.is_empty() {
            write_subscripts(&mut rendered, subs);
        }
        rendered
    }

    /// Append a part to this qualified name.
    pub fn push(&mut self, name: String, subscripts: Vec<i64>) {
        self.parts.push((name, subscripts));
    }

    /// Create a child qualified name by appending a part.
    pub fn child(&self, name: &str) -> Self {
        let mut result = self.clone();
        result.parts.push((name.to_string(), Vec::new()));
        result
    }

    /// Check if this qualified name is empty.
    pub fn is_empty(&self) -> bool {
        self.parts.is_empty()
    }

    /// Convert to a segmented component path without flattening and reparsing.
    pub fn to_component_path(&self) -> ComponentPath {
        ComponentPath::from_parts(
            self.parts
                .iter()
                .map(|(name, subs)| self.render_part(name, subs)),
        )
    }

    /// Convert to a flat string representation (e.g., "body.position.x").
    pub fn to_flat_string(&self) -> String {
        let mut out = String::new();
        for (part_index, (name, subs)) in self.parts.iter().enumerate() {
            if part_index > 0 {
                out.push('.');
            }
            out.push_str(name);
            if subs.is_empty() {
                continue;
            }
            write_subscripts(&mut out, subs);
        }
        out
    }
}

fn write_subscripts(out: &mut String, subs: &[i64]) {
    use std::fmt::Write as _;

    out.push('[');
    for (sub_index, subscript) in subs.iter().enumerate() {
        if sub_index > 0 {
            out.push(',');
        }
        let _ = write!(out, "{subscript}");
    }
    out.push(']');
}

fn subscripted_part_matches_rendered(name: &str, subs: &[i64], rendered: &str) -> bool {
    let Some(rest) = rendered.strip_prefix(name) else {
        return false;
    };
    let Some(inner) = rest
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
    else {
        return false;
    };
    let mut rendered_subs = inner.split(',');
    for expected in subs {
        let Some(rendered_sub) = rendered_subs.next() else {
            return false;
        };
        if !rendered_subscript_matches(rendered_sub, *expected) {
            return false;
        }
    }
    rendered_subs.next().is_none()
}

fn rendered_subscript_matches(rendered: &str, expected: i64) -> bool {
    let (negative, digits) = if expected.is_negative() {
        let Some(digits) = rendered.strip_prefix('-') else {
            return false;
        };
        (true, digits.as_bytes())
    } else {
        if rendered.starts_with('-') {
            return false;
        }
        (false, rendered.as_bytes())
    };
    if digits.is_empty() {
        return false;
    }
    let mut magnitude = expected.unsigned_abs();
    if magnitude == 0 {
        return !negative && digits == b"0";
    }
    let mut index = digits.len();
    while magnitude > 0 {
        if index == 0 {
            return false;
        }
        index -= 1;
        if digits[index] != b'0' + (magnitude % 10) as u8 {
            return false;
        }
        magnitude /= 10;
    }
    index == 0
}

impl std::fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, (name, subs)) in self.parts.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            write!(f, "{}", name)?;
            if !subs.is_empty() {
                write!(
                    f,
                    "[{}]",
                    subs.iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<_>>()
                        .join(",")
                )?;
            }
        }
        Ok(())
    }
}

// =============================================================================
// Modification Environment (MLS §7.2)
// =============================================================================

/// MLS §7.2: "modification environment determines the values of modifiers"
///
/// This is built during instantiation and applied to produce instance data.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ModificationEnvironment {
    /// Active modifications by target path.
    pub active: IndexMap<QualifiedName, ModificationValue>,
}

impl ModificationEnvironment {
    /// Create a new empty modification environment.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a modification to the environment.
    ///
    /// MLS §7.2.4: Outer modifications take precedence over inner modifications.
    /// If a modification already exists for this target, the existing one is kept
    /// (it's from an outer scope).
    pub fn add(&mut self, target: QualifiedName, value: ModificationValue) {
        // Only insert if not already present (outer modifications take precedence)
        self.active.entry(target).or_insert(value);
    }

    /// Look up a modification by path.
    pub fn get(&self, target: &QualifiedName) -> Option<&ModificationValue> {
        self.active.get(target)
    }

    /// Look up an attribute modification for a component.
    ///
    /// Constructs a path like `comp_name.attr_name` and looks it up.
    /// Returns the expression value if found.
    ///
    /// # Example
    /// ```ignore
    /// // Look up x.start modification
    /// let start = mod_env.get_attr("x", "start");
    /// ```
    pub fn get_attr(&self, comp_name: &str, attr_name: &str) -> Option<&Expression> {
        let path = QualifiedName::from_ident(comp_name).child(attr_name);
        self.get(&path).map(|v| &v.value)
    }

    /// Remove all modifications that start with the given prefix name.
    ///
    /// Used when exiting a nested component scope to clean up modifications
    /// that were only relevant to that scope.
    pub fn remove_with_prefix(&mut self, prefix_name: &str) {
        self.active.retain(|k, _| !k.starts_with(prefix_name));
    }
}

/// A modification value in the modification environment.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModificationValue {
    /// The expression value of the modification.
    ///
    /// This is the resolved/evaluated form used for semantic checks during
    /// instantiation (e.g., conditional component activation).
    pub value: Expression,
    /// Optional source expression before eager resolution.
    ///
    /// When present, this preserves the symbolic modifier form (`k = parentK`)
    /// so downstream flat-output generation can keep parameter propagation
    /// relationships instead of hard-coding evaluated defaults.
    pub source: Option<Expression>,
    /// Optional lexical scope where the modifier expression was written.
    ///
    /// MLS §7.2.4: component modifications are evaluated in the scope where the
    /// modification appears, which may differ from the modified component's scope.
    pub source_scope: Option<QualifiedName>,
    /// True if the modification has `each` prefix.
    /// Note: `each` prefix handling is not yet implemented in the parser.
    pub each: bool,
    /// True if the modification has `final` prefix.
    /// Note: `final` prefix handling is not yet implemented in the parser.
    pub final_: bool,
}

impl ModificationValue {
    /// Create a simple modification value without `each` or `final` prefixes.
    ///
    /// This is the common case for most modifications.
    pub fn simple(value: Expression) -> Self {
        Self {
            value,
            source: None,
            source_scope: None,
            each: false,
            final_: false,
        }
    }

    /// Create a modification value with `each` prefix (MLS §7.2.5).
    ///
    /// The `each` prefix means the modification applies to every element
    /// of an array component.
    pub fn with_each(value: Expression, each: bool) -> Self {
        Self {
            value,
            source: None,
            source_scope: None,
            each,
            final_: false,
        }
    }

    /// Create a modification value with both `each` and `final` prefixes.
    ///
    /// MLS §7.2.5: `each` applies modification to array elements.
    /// MLS §7.2.6: `final` prevents further modification.
    pub fn with_prefixes(value: Expression, each: bool, final_: bool) -> Self {
        Self {
            value,
            source: None,
            source_scope: None,
            each,
            final_,
        }
    }

    /// Create a modification value with an explicit symbolic source expression.
    pub fn with_source(value: Expression, source: Option<Expression>) -> Self {
        Self::with_source_scope(value, source, None)
    }

    /// Create a modification value with source expression and lexical source scope.
    pub fn with_source_scope(
        value: Expression,
        source: Option<Expression>,
        source_scope: Option<QualifiedName>,
    ) -> Self {
        Self::with_source_scope_and_prefixes(value, source, source_scope, false, false)
    }

    /// Create a modification value with source metadata and modifier prefixes.
    pub fn with_source_scope_and_prefixes(
        value: Expression,
        source: Option<Expression>,
        source_scope: Option<QualifiedName>,
        each: bool,
        final_: bool,
    ) -> Self {
        Self {
            value,
            source,
            source_scope,
            each,
            final_,
        }
    }
}

// =============================================================================
// Instance Data (Overlay)
// =============================================================================

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassOverride {
    pub alias: String,
    pub alias_def_id: DefId,
    pub target_def_id: DefId,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub target_ref: Option<ComponentReference>,
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub modifier_args: Vec<Expression>,
}

pub type ClassOverrideMap = FastIndexMap<DefId, ClassOverride>;

impl ClassOverride {
    pub fn new(
        alias: impl Into<String>,
        alias_def_id: DefId,
        target_def_id: DefId,
        target_ref: Option<ComponentReference>,
    ) -> Self {
        Self {
            alias: alias.into(),
            alias_def_id,
            target_def_id,
            target_ref,
            modifier_args: Vec::new(),
        }
    }

    pub fn with_modifier_args(mut self, modifier_args: Vec<Expression>) -> Self {
        self.modifier_args = modifier_args;
        self
    }
}

/// Instance-specific data for a component.
///
/// This is stored in an overlay map keyed by DefId, rather than
/// being embedded in Component directly.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceData {
    /// Unique identifier for this instance.
    pub instance_id: InstanceId,
    /// Structured resolved component reference for this concrete instance.
    ///
    /// This is the semantic carrier for downstream Flat/DAE phases. The
    /// rendered `qualified_name` remains useful for stable output spelling, but
    /// compiler logic should prefer this structured reference when available.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub component_ref: Option<CoreComponentReference>,
    /// Fully qualified name in the instance tree.
    pub qualified_name: QualifiedName,
    /// Source location of the component declaration that created this instance.
    pub source_location: rumoca_core::Location,
    /// Resolved array dimensions.
    pub dims: Vec<i64>,
    /// Unevaluated dimension expressions for parameter-dependent sizes.
    /// These are evaluated during flattening when parameter values are known.
    pub dims_expr: Vec<crate::Subscript>,
    /// Resolved type identity when known.
    /// Populated during instantiation/typecheck and consumed by flatten.
    pub type_id: TypeId,
    /// Declared type name from the component declaration.
    /// Used by post-instantiation type resolution to recover user-defined type IDs.
    pub type_name: String,
    /// DefId of the declared component type when available from resolve phase.
    /// Builtin types typically do not have a DefId.
    pub type_def_id: Option<DefId>,
    /// Lexical scope where this component declaration was written.
    #[serde(default)]
    pub declaration_source_scope: Option<QualifiedName>,
    /// Active replaceable class/package redeclare overrides for this component instance.
    ///
    /// Keys are the DefIds of the redeclared local classes (e.g., `Medium`).
    /// Values retain the source alias text, effective target DefId, and the
    /// redeclare value component reference.
    /// Populated during instantiation so downstream phases can apply instance-specific
    /// package/class constants consistently (MLS §7.3).
    pub class_overrides: ClassOverrideMap,
    /// True when this component applies a self-forwarding class/package redeclare
    /// (e.g., `redeclare package Medium = Medium`) that is remapped to an active
    /// enclosing override during instantiation (MLS §7.3).
    pub has_forwarding_class_redeclare: bool,

    // Type prefixes (MLS §4.4.2, SPEC_0022 §3.19-3.20)
    /// Variability (constant, parameter, discrete, continuous).
    pub variability: Variability,
    /// Causality (input, output, or default).
    pub causality: Causality,
    /// Flow prefix (for connectors).
    pub flow: bool,
    /// Stream prefix (for connectors).
    pub stream: bool,

    // Resolved attribute values (MLS §4.4)
    /// Start value attribute.
    pub start: Option<Expression>,
    /// Fixed attribute.
    pub fixed: Option<bool>,
    /// Minimum value attribute.
    pub min: Option<Expression>,
    /// Maximum value attribute.
    pub max: Option<Expression>,
    /// Nominal value attribute.
    pub nominal: Option<Expression>,
    /// Quantity string attribute.
    pub quantity: Option<String>,
    /// Unit string attribute.
    pub unit: Option<String>,
    /// Display-unit string attribute.
    pub display_unit: Option<String>,
    /// Optional declaration description string (`"..."` after component declaration).
    pub description: Option<String>,
    /// State selection hint.
    pub state_select: StateSelect,

    /// Binding equation value (resolved).
    pub binding: Option<Expression>,
    /// Optional symbolic binding source expression for modification-derived bindings.
    ///
    /// MLS §7.2.4: component modifications are written in an outer scope and may
    /// intentionally reference outer parameters (e.g., `gain(g = k)`).
    /// We retain this source form for flat-output rendering while keeping `binding`
    /// available as a resolved value for semantic passes.
    pub binding_source: Option<Expression>,
    /// Lexical scope where a modification-derived binding was written.
    ///
    /// Used during flattening to qualify symbolic modifier references according
    /// to MLS §7.2.4 without path-depth heuristics.
    pub binding_source_scope: Option<QualifiedName>,
    /// Lexical scopes where attribute modifiers were written, keyed by attribute
    /// name (`start`, `min`, `max`, `nominal`).
    pub attribute_source_scopes: IndexMap<String, QualifiedName>,
    /// True if binding came from a modification rather than declaration.
    pub binding_from_modification: bool,
    /// True if this is a primitive type (Real, Integer, Boolean, String).
    /// False for class types (connectors, models, records, etc.) which are
    /// containers and should not appear as flat variables.
    pub is_primitive: bool,
    /// True if the base type is Integer or Boolean (MLS §4.5).
    /// These types are discrete by default even without explicit `discrete` prefix.
    pub is_discrete_type: bool,
    /// True if this variable comes from an expandable connector (MLS §9.1.3).
    /// Unconnected expandable connector members without bindings are unused.
    pub from_expandable_connector: bool,
    /// True if this parameter has annotation(Evaluate=true) or is declared final.
    /// Structural parameters can be evaluated at compile time for if-equation
    /// branch selection (MLS §18.3).
    pub evaluate: bool,
    /// True if this component declaration has the `final` prefix (MLS §7.2.6).
    /// Used for preserving flat-output declaration qualifiers.
    pub is_final: bool,
    /// True if this variable belongs to an overconstrained connector (MLS §9.4).
    /// A connector is overconstrained if its type defines an `equalityConstraint` function.
    pub is_overconstrained: bool,
    /// True if this component is declared in a protected section (MLS §4.7).
    /// Protected components are not part of the public interface and their flow
    /// variables should not count as interface flows for balance checking.
    pub is_protected: bool,
    /// True if this component's type is a `connector` class (MLS §4.7).
    /// Per MLS §4.7, only flow variables in top-level public connector components
    /// count toward the local equation size. Models/blocks (like Delta) are NOT
    /// interface connectors even if they contain sub-connectors.
    pub is_connector_type: bool,
    /// The path of the enclosing overconstrained record (MLS §9.4).
    /// E.g., "frame_a.R" for variables frame_a.R.T and frame_a.R.w.
    /// Used to group OC variables into VCG nodes for balance correction.
    pub oc_record_path: Option<String>,
    /// The output size of the enclosing record's equalityConstraint function.
    /// E.g., 3 for Orientation (returns `Real[3]`).
    pub oc_eq_constraint_size: Option<usize>,
}

impl Default for InstanceData {
    fn default() -> Self {
        Self {
            instance_id: InstanceId::default(),
            component_ref: None,
            qualified_name: QualifiedName::default(),
            source_location: rumoca_core::Location::default(),
            dims: Vec::new(),
            dims_expr: Vec::new(),
            type_id: TypeId::default(),
            type_name: String::new(),
            type_def_id: None,
            declaration_source_scope: None,
            class_overrides: IndexMap::default(),
            has_forwarding_class_redeclare: false,
            variability: Variability::Empty,
            causality: Causality::Empty,
            flow: false,
            stream: false,
            start: None,
            fixed: None,
            min: None,
            max: None,
            nominal: None,
            quantity: None,
            unit: None,
            display_unit: None,
            description: None,
            state_select: StateSelect::default(),
            binding: None,
            binding_source: None,
            binding_source_scope: None,
            attribute_source_scopes: IndexMap::default(),
            binding_from_modification: false,
            is_primitive: false,
            is_discrete_type: false,
            from_expandable_connector: false,
            evaluate: false,
            is_final: false,
            is_overconstrained: false,
            is_protected: false,
            is_connector_type: false,
            oc_record_path: None,
            oc_eq_constraint_size: None,
        }
    }
}

pub fn component_reference_for_instance(
    qualified_name: &QualifiedName,
    span: Span,
    def_id: Option<DefId>,
) -> CoreComponentReference {
    CoreComponentReference {
        local: false,
        span,
        parts: qualified_name
            .parts
            .iter()
            .map(|(ident, subs)| CoreComponentRefPart {
                ident: ident.clone(),
                span,
                subs: subs
                    .iter()
                    .map(|sub| CoreSubscript::generated_index(*sub, span))
                    .collect(),
            })
            .collect(),
        def_id,
    }
}

/// Instance data for a class/model.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClassInstanceData {
    /// Unique identifier for this class instance.
    pub instance_id: InstanceId,
    /// DefId of the class definition this instance was instantiated from.
    pub class_def_id: Option<DefId>,
    /// Fully qualified name in the instance tree.
    pub qualified_name: QualifiedName,
    /// Lexical scope of the class declaration that produced this instance.
    #[serde(default)]
    pub source_scope: Option<QualifiedName>,
    /// Resolved lexical scope of the class declaration that produced this instance.
    #[serde(default)]
    pub source_scope_id: Option<ScopeId>,
    /// Equations from this instance (not inherited).
    pub equations: Vec<InstanceEquation>,
    /// Initial equations from this instance.
    pub initial_equations: Vec<InstanceEquation>,
    /// Algorithm sections from this instance.
    pub algorithms: Vec<Vec<InstanceStatement>>,
    /// Initial algorithm sections from this instance.
    pub initial_algorithms: Vec<Vec<InstanceStatement>>,
    /// Connection statements from this instance.
    pub connections: Vec<InstanceConnection>,
    /// Resolved import map: short name → fully-qualified name (MLS §13.2).
    ///
    /// Collected from the class definition and its entire inheritance chain.
    /// Used during flattening to resolve imported short names (e.g., `pi` →
    /// `Modelica.Constants.pi`) instead of incorrectly qualifying them with
    /// the component instance prefix.
    pub resolved_imports: Vec<(String, String)>,
}

/// An equation in the instance tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceEquation {
    /// The equation from the AST.
    pub equation: Equation,
    /// Origin of this equation (qualified name of the class it came from).
    pub origin: QualifiedName,
    /// Lexical source scope containing this equation.
    pub source_scope: Option<QualifiedName>,
    /// Resolved lexical source scope containing this equation.
    pub source_scope_id: Option<ScopeId>,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
}

/// A statement in the instance tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceStatement {
    /// The statement from the AST.
    pub statement: Statement,
    /// Origin of this statement (qualified name of the class it came from).
    pub origin: QualifiedName,
    /// Lexical source scope containing this statement.
    pub source_scope: Option<QualifiedName>,
    /// Resolved lexical source scope containing this statement.
    pub source_scope_id: Option<ScopeId>,
    /// Source span for error reporting. Never loses source location.
    pub span: Span,
}

/// A connection statement in the instance tree.
///
/// MLS §9: Connection equations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InstanceConnection {
    /// First connector.
    pub a: QualifiedName,
    /// Second connector.
    pub b: QualifiedName,
    /// Type of the connectors.
    pub connector_type: Option<DefId>,
    /// Source span for error reporting.
    pub span: Span,
    /// Scope where the connect statement was declared (flattened prefix).
    /// Used to determine the correct hierarchy level for flow sum equations.
    /// Empty string means root level.
    pub scope: String,
}

// =============================================================================
// Instance Overlay
// =============================================================================

/// Overlay containing instance-specific data keyed by InstanceId.
///
/// Keeps instance data separate from the core AST types to avoid
/// polluting shared definitions with per-instance state.
///
/// Note: We use InstanceId as the key because each instance gets a unique
/// InstanceId during instantiation, whereas DefIds identify declarations
/// (which can have multiple instances). InstanceId is a simple u32, so
/// lookups are O(1) with a cheap hasher for numeric keys.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct InstanceOverlay {
    /// Instance data for components, keyed by their InstanceId.
    pub components: FastIndexMap<InstanceId, InstanceData>,
    /// Instance data for classes, keyed by their InstanceId.
    pub classes: FastIndexMap<InstanceId, ClassInstanceData>,
    /// True if the root model is declared with the `partial` keyword.
    /// MLS §4.7: Partial models are incomplete and shouldn't be balance-checked.
    pub is_partial: bool,
    /// MLS §4.7: The class type of the root model (model, connector, record, etc.)
    pub class_type: ClassType,
    /// Optional description string from the root class declaration.
    pub root_description: Option<String>,
    /// Disabled conditional components (MLS §4.8).
    /// Contains structured instance component paths whose conditions evaluated to false.
    /// These components and their sub-components should be excluded from flattening.
    pub disabled_components: IndexSet<ComponentPath>,
    /// Array parent dimensions for expanded array components.
    /// When an array component like `plug_p.pin[3]` is expanded to indexed instances
    /// (`plug_p.pin[1]`, `plug_p.pin[2]`, `plug_p.pin[3]`), this map stores the parent
    /// path `plug_p.pin` with dimensions `[3]` for use in array equation expansion.
    pub array_parent_dims: IndexMap<String, Vec<i64>>,
    /// Mapping from outer-prefixed paths to their corresponding inner paths (MLS §5.4).
    /// When an outer component `initialStep.stateGraphRoot` references inner `stateGraphRoot`,
    /// equations/connections using the outer prefix are redirected to the inner path.
    pub outer_prefix_to_inner: IndexMap<String, String>,
    /// Mapping from inner-outer component paths to their parent inner paths (MLS §5.4).
    /// When a component is declared `inner outer` (e.g., `inner outer StateGraphRoot stateGraphRoot`),
    /// it bridges two scopes: it serves as `inner` for children and as `outer` referencing the parent.
    /// Same-level connections involving the `inner outer` component should redirect to the parent's
    /// inner for flow equation scoping (e.g., `makeProduct.stateGraphRoot` → `stateGraphRoot`).
    pub inner_outer_to_parent_inner: IndexMap<String, String>,
    /// Names of inner declarations synthesized during instantiation retry (MLS §5.4).
    /// Populated when `outer` components had no matching `inner` and automatic
    /// synthesis succeeded.
    pub synthesized_inners: Vec<String>,
    /// Canonical type roots for compatibility checks (alias/enumeration normalization).
    /// Keys are resolved type identities and values are canonical root type identities.
    /// Populated by typecheck_instanced for flatten-time type compatibility.
    pub type_roots: IndexMap<TypeId, TypeId>,
    /// Next available instance ID.
    next_id: u32,
}

impl InstanceOverlay {
    /// Create a new empty overlay.
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a new unique InstanceId.
    pub fn alloc_id(&mut self) -> InstanceId {
        let id = InstanceId(self.next_id);
        self.next_id += 1;
        id
    }

    /// Add instance data for a component.
    ///
    /// The component is keyed by its InstanceId to ensure uniqueness,
    /// since multiple instances can share the same DefId.
    pub fn add_component(&mut self, data: InstanceData) {
        let key = data.instance_id;
        self.components.insert(key, data);
    }

    /// Add instance data for a class.
    ///
    /// The class is keyed by its InstanceId to ensure uniqueness.
    pub fn add_class(&mut self, data: ClassInstanceData) {
        let key = data.instance_id;
        self.classes.insert(key, data);
    }

    /// Get instance data for a component by InstanceId.
    pub fn get_component(&self, instance_id: InstanceId) -> Option<&InstanceData> {
        self.components.get(&instance_id)
    }

    /// Get instance data for a class by InstanceId.
    pub fn get_class(&self, instance_id: InstanceId) -> Option<&ClassInstanceData> {
        self.classes.get(&instance_id)
    }

    /// True when automatic inner synthesis was used for this instantiation.
    pub fn used_synthesized_inners(&self) -> bool {
        !self.synthesized_inners.is_empty()
    }
}

// =============================================================================
// Instanced Tree (Phase Wrapper)
// =============================================================================

/// A ClassTree that has completed instantiation.
///
/// At this stage:
/// - All `def_id` fields are populated
/// - All `scope_id` fields are populated
/// - All `type_id` fields are populated
/// - Instance data is available in the overlay
/// - Modifications have been merged
/// - inner/outer references are resolved
#[derive(Debug, Clone)]
pub struct InstancedTree {
    /// The underlying class tree.
    pub tree: ClassTree,
    /// Instance-specific data overlay.
    pub overlay: InstanceOverlay,
}

impl InstancedTree {
    /// Create a new InstancedTree from a ClassTree and overlay.
    pub fn new(tree: ClassTree, overlay: InstanceOverlay) -> Self {
        Self { tree, overlay }
    }

    /// Get a reference to the inner ClassTree.
    pub fn inner(&self) -> &ClassTree {
        &self.tree
    }

    /// Consume and return the inner ClassTree.
    pub fn into_inner(self) -> ClassTree {
        self.tree
    }

    /// Get the instance overlay.
    pub fn overlay(&self) -> &InstanceOverlay {
        &self.overlay
    }

    /// Get a mutable reference to the instance overlay.
    pub fn overlay_mut(&mut self) -> &mut InstanceOverlay {
        &mut self.overlay
    }
}

impl std::ops::Deref for InstancedTree {
    type Target = ClassTree;
    fn deref(&self) -> &Self::Target {
        &self.tree
    }
}

impl std::ops::DerefMut for InstancedTree {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tree
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ComponentRefPart, ComponentReference, Location, Token};

    // -------------------------------------------------------------------------
    // QualifiedName tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_from_dotted_simple() {
        let qn = QualifiedName::from_dotted("x.start");
        assert_eq!(qn.parts.len(), 2);
        assert_eq!(qn.parts[0].0, "x");
        assert_eq!(qn.parts[1].0, "start");
        assert_eq!(qn.to_flat_string(), "x.start");
    }

    #[test]
    fn test_from_dotted_single() {
        let qn = QualifiedName::from_dotted("x");
        assert_eq!(qn.parts.len(), 1);
        assert_eq!(qn.parts[0].0, "x");
    }

    #[test]
    fn test_from_dotted_empty() {
        let qn = QualifiedName::from_dotted("");
        assert!(qn.is_empty());
    }

    #[test]
    fn test_from_dotted_trailing_dot() {
        // Trailing dots should be filtered out
        let qn = QualifiedName::from_dotted("x.y.");
        assert_eq!(qn.parts.len(), 2);
        assert_eq!(qn.to_flat_string(), "x.y");
    }

    #[test]
    fn test_from_dotted_leading_dot() {
        // Leading dots should be filtered out
        let qn = QualifiedName::from_dotted(".x.y");
        assert_eq!(qn.parts.len(), 2);
        assert_eq!(qn.to_flat_string(), "x.y");
    }

    #[test]
    fn test_from_dotted_consecutive_dots() {
        // Consecutive dots should be filtered out
        let qn = QualifiedName::from_dotted("x..y");
        assert_eq!(qn.parts.len(), 2);
        assert_eq!(qn.to_flat_string(), "x.y");
    }

    #[test]
    fn test_starts_with_match() {
        let qn = QualifiedName::from_dotted("l2.x.start");
        assert!(qn.starts_with("l2"));
    }

    #[test]
    fn test_starts_with_no_match() {
        let qn = QualifiedName::from_dotted("l2.x.start");
        assert!(!qn.starts_with("l1"));
        assert!(!qn.starts_with("x"));
    }

    #[test]
    fn test_starts_with_empty() {
        let qn = QualifiedName::new();
        assert!(!qn.starts_with("anything"));
    }

    #[test]
    fn test_strip_prefix_success() {
        let qn = QualifiedName::from_dotted("l2.x.start");
        let stripped = qn.strip_prefix("l2").unwrap();
        assert_eq!(stripped.to_flat_string(), "x.start");
    }

    #[test]
    fn test_strip_prefix_no_match() {
        let qn = QualifiedName::from_dotted("l2.x.start");
        assert!(qn.strip_prefix("l1").is_none());
    }

    #[test]
    fn test_strip_prefix_single_part() {
        // Cannot strip if only one part remains
        let qn = QualifiedName::from_dotted("x");
        assert!(qn.strip_prefix("x").is_none());
    }

    #[test]
    fn test_strip_prefix_preserves_subscripts() {
        // Ensure subscripts on remaining parts are preserved
        let mut qn = QualifiedName::new();
        qn.push("comp".to_string(), vec![]);
        qn.push("array".to_string(), vec![1, 2]);
        qn.push("x".to_string(), vec![]);

        let stripped = qn.strip_prefix("comp").unwrap();
        assert_eq!(stripped.parts.len(), 2);
        assert_eq!(stripped.parts[0].0, "array");
        assert_eq!(stripped.parts[0].1, vec![1, 2]);
        assert_eq!(stripped.to_flat_string(), "array[1,2].x");
    }

    #[test]
    fn test_to_component_path_preserves_structured_subscripts() {
        let mut qn = QualifiedName::new();
        qn.push("sys".to_string(), vec![]);
        qn.push("arr".to_string(), vec![1, 2]);
        qn.push("state".to_string(), vec![]);

        let path = qn.to_component_path();
        assert_eq!(
            path.parts(),
            &[
                "sys".to_string(),
                "arr[1,2]".to_string(),
                "state".to_string(),
            ]
        );
        assert_eq!(path.to_flat_string(), "sys.arr[1,2].state");
    }

    #[test]
    fn test_starts_with_component_path_matches_structured_subscripts() {
        let mut qn = QualifiedName::new();
        qn.push("sys".to_string(), vec![]);
        qn.push("arr".to_string(), vec![1, 2]);
        qn.push("state".to_string(), vec![]);

        let prefix = ComponentPath::from_flat_path("sys.arr[1,2]");

        assert!(qn.starts_with_component_path(&prefix));
    }

    #[test]
    fn component_reference_for_instance_preserves_parts_subscripts_and_def_id() {
        let def_id = DefId::new(9);
        let span = Span::DUMMY;
        let mut qn = QualifiedName::new();
        qn.push("body".to_string(), vec![]);
        qn.push("frame".to_string(), vec![2]);
        qn.push("r_0".to_string(), vec![]);

        let reference = component_reference_for_instance(&qn, span, Some(def_id));

        assert_eq!(reference.def_id, Some(def_id));
        assert_eq!(reference.parts.len(), 3);
        assert_eq!(reference.parts[0].ident, "body");
        assert_eq!(reference.parts[1].ident, "frame");
        assert_eq!(
            reference.parts[1].subs,
            vec![CoreSubscript::generated_index(2, span)]
        );
        assert_eq!(reference.parts[2].ident, "r_0");
    }

    #[test]
    fn test_starts_with_component_path_rejects_subscript_mismatch() {
        let mut qn = QualifiedName::new();
        qn.push("sys".to_string(), vec![]);
        qn.push("arr".to_string(), vec![1, 2]);
        qn.push("state".to_string(), vec![]);

        let prefix = ComponentPath::from_flat_path("sys.arr[1,3]");

        assert!(!qn.starts_with_component_path(&prefix));
    }

    #[test]
    fn test_subscripted_part_match_uses_canonical_integer_text() {
        assert!(subscripted_part_matches_rendered("arr", &[0], "arr[0]"));
        assert!(subscripted_part_matches_rendered("arr", &[-2], "arr[-2]"));
        assert!(!subscripted_part_matches_rendered("arr", &[1], "arr[01]"));
        assert!(!subscripted_part_matches_rendered("arr", &[0], "arr[-0]"));
    }

    #[test]
    fn test_first_name() {
        let qn = QualifiedName::from_dotted("a.b.c");
        assert_eq!(qn.first_name(), Some("a"));

        let empty = QualifiedName::new();
        assert_eq!(empty.first_name(), None);
    }

    #[test]
    fn test_child() {
        let qn = QualifiedName::from_ident("x");
        let child = qn.child("start");
        assert_eq!(child.to_flat_string(), "x.start");
    }

    #[test]
    fn test_parent_join_use_structured_parts() {
        let qn = QualifiedName::from_dotted("system.medium.nXi");
        assert_eq!(qn.parent().unwrap().to_flat_string(), "system.medium");
        assert_eq!(
            QualifiedName::from_dotted("system").join(&QualifiedName::from_dotted("medium.nXi")),
            qn
        );
    }

    #[test]
    fn test_display_with_subscripts() {
        let mut qn = QualifiedName::new();
        qn.push("matrix".to_string(), vec![1, 2]);
        qn.push("element".to_string(), vec![]);
        assert_eq!(format!("{}", qn), "matrix[1,2].element");
    }

    // -------------------------------------------------------------------------
    // ModificationEnvironment tests
    // -------------------------------------------------------------------------

    /// Helper to create a distinguishable expression for testing.
    /// Uses ComponentReference with a marker name to identify values.
    fn test_expr(marker: &str) -> Expression {
        Expression::ComponentReference(ComponentReference {
            local: false,
            def_id: None,
            span: rumoca_core::Span::DUMMY,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: std::sync::Arc::from(marker),
                    location: Location::default(),
                    token_number: 0,
                    token_type: 0,
                },
                subs: None,
            }],
        })
    }

    /// Check if an expression matches our test marker.
    fn is_test_expr(expr: &Expression, marker: &str) -> bool {
        match expr {
            Expression::ComponentReference(cr) => {
                cr.parts.first().map(|p| &*p.ident.text) == Some(marker)
            }
            _ => false,
        }
    }

    #[test]
    fn test_mod_env_add_and_get() {
        let mut env = ModificationEnvironment::new();
        let path = QualifiedName::from_dotted("x.start");
        let value = ModificationValue::simple(test_expr("value_1"));

        env.add(path.clone(), value);

        let retrieved = env.get(&path);
        assert!(retrieved.is_some());
        assert!(is_test_expr(&retrieved.unwrap().value, "value_1"));
    }

    #[test]
    fn test_mod_env_outer_precedence() {
        // MLS §7.2.4: Outer modifications take precedence
        let mut env = ModificationEnvironment::new();
        let path = QualifiedName::from_dotted("x.start");

        // First add (simulating outer modification)
        let outer_value = ModificationValue::simple(test_expr("outer_10"));
        env.add(path.clone(), outer_value);

        // Second add (simulating inner modification) - should NOT overwrite
        let inner_value = ModificationValue::simple(test_expr("inner_5"));
        env.add(path.clone(), inner_value);

        // Should still have the outer value
        let retrieved = env.get(&path).unwrap();
        assert!(is_test_expr(&retrieved.value, "outer_10"));
    }

    #[test]
    fn test_mod_env_get_attr() {
        let mut env = ModificationEnvironment::new();

        // Add x.start modification
        let path = QualifiedName::from_ident("x").child("start");
        let value = ModificationValue::simple(test_expr("start_42"));
        env.add(path, value);

        // Look up via get_attr
        let start = env.get_attr("x", "start");
        assert!(start.is_some());
        assert!(is_test_expr(start.unwrap(), "start_42"));

        // Non-existent attribute
        assert!(env.get_attr("x", "min").is_none());
        assert!(env.get_attr("y", "start").is_none());
    }

    #[test]
    fn test_mod_env_remove_with_prefix() {
        let mut env = ModificationEnvironment::new();

        // Add modifications for different components
        env.add(
            QualifiedName::from_dotted("comp1.x.start"),
            ModificationValue::simple(test_expr("c1_x")),
        );
        env.add(
            QualifiedName::from_dotted("comp1.y.start"),
            ModificationValue::simple(test_expr("c1_y")),
        );
        env.add(
            QualifiedName::from_dotted("comp2.x.start"),
            ModificationValue::simple(test_expr("c2_x")),
        );

        assert_eq!(env.active.len(), 3);

        // Remove all comp1 modifications
        env.remove_with_prefix("comp1");

        assert_eq!(env.active.len(), 1);
        assert!(
            env.get(&QualifiedName::from_dotted("comp2.x.start"))
                .is_some()
        );
        assert!(
            env.get(&QualifiedName::from_dotted("comp1.x.start"))
                .is_none()
        );
    }

    // -------------------------------------------------------------------------
    // ModificationValue tests
    // -------------------------------------------------------------------------

    #[test]
    fn test_modification_value_simple() {
        let value = ModificationValue::simple(Expression::Empty {
            span: rumoca_core::Span::DUMMY,
        });
        assert!(!value.each);
        assert!(!value.final_);
        assert!(matches!(value.value, Expression::Empty { .. }));
    }

    #[test]
    fn test_instance_overlay_component_lookup_by_instance_id() {
        let mut overlay = InstanceOverlay::new();
        let id_a = overlay.alloc_id();
        let id_b = overlay.alloc_id();

        overlay.add_component(InstanceData {
            instance_id: id_a,
            qualified_name: QualifiedName::from_dotted("a"),
            ..Default::default()
        });
        overlay.add_component(InstanceData {
            instance_id: id_b,
            qualified_name: QualifiedName::from_dotted("b"),
            ..Default::default()
        });

        let component_a = overlay
            .get_component(id_a)
            .expect("component for id_a should exist");
        let component_b = overlay
            .get_component(id_b)
            .expect("component for id_b should exist");

        assert_eq!(component_a.qualified_name.to_flat_string(), "a");
        assert_eq!(component_b.qualified_name.to_flat_string(), "b");
    }
}
