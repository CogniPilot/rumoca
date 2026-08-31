//! Instance Tree data structures for the Rumoca compiler.
//!
//! This module defines instance-related types (MLS §5.6), which represent
//! the instantiated elements with merged modifications applied.
//!
//! Uses an overlay approach where instance data is stored separately
//! and keyed by DefId, rather than bloating the core AST types with
//! optional instance fields.

mod equality_constraint;
mod semantic_catalogs;

pub use equality_constraint::{
    EffectiveTypePublicationError, EqualityConstraintCardinality,
    EqualityConstraintDeclarationIndex, EqualityConstraintEffectiveRecordIdentity,
    EqualityConstraintExposureError, EqualityConstraintOccurrenceError,
    EqualityConstraintOccurrenceExposure, EqualityConstraintPrototype,
    EqualityConstraintSelectionProof, EqualityConstraintSpecializationKey,
    FinalizedOverconstrainedCatalog, FinalizedOverconstrainedComponent,
    FinalizedOverconstrainedRecord,
};
use equality_constraint::{
    FinalizedOverconstrainedOccurrence, PendingEqualityConstraintOccurrenceExposure,
};
#[cfg(test)]
pub(crate) use semantic_catalogs::test_semantic_catalog_projection;
pub use semantic_catalogs::{
    ConnectionOperatorCatalog, ExternalObjectLifecycleCatalog, ExternalObjectLifecycleIdentity,
    SemanticCatalogProjection,
};

use crate::AstIndexMap as IndexMap;
use indexmap::IndexSet;
use rumoca_core::{
    ComponentPath, ComponentReference as CoreComponentReference, DefId, EffectiveType, InstanceId,
    ScopeId, Span, TypeId,
};
use serde::{Deserialize, Serialize};

use crate::{
    Causality, ClassTree, ClassType, ComponentReference, Equation, Expression, StateSelect,
    Statement, Variability,
};
#[cfg(test)]
use crate::{ClassDef, Subscript};

type FastIndexMap<K, V> = IndexMap<K, V>;

fn required<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

/// A fully qualified path with resolved subscripts.
///
/// Example: `"body.position[1].x"` would be represented as:
/// `[("body", []), ("position", [1]), ("x", [])]`
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct QualifiedName {
    /// Sequence of (name, subscripts) pairs.
    pub parts: Vec<(String, Vec<i64>)>,
}

/// Hashes the path's *shape* — segment count, per-segment subscripts, and
/// per-segment identifier length — never the identifier bytes.
///
/// `QualifiedName` is a live map key (`ModificationEnvironment::active`), and a
/// derived `Hash` over `Vec<(String, Vec<i64>)>` walked every identifier on
/// every probe. Hashing the shape is a strict subset of what the derived
/// `PartialEq` compares, so equal names still hash equal and the map stays
/// correct; unequal names of the same shape share a bucket and are separated by
/// the equality check. Modification environments hold one entry per modifier of
/// one instance, so those buckets stay small.
///
/// The alternative — interning each segment inside `hash` — would pay a global
/// interner probe (which hashes the identifier anyway) per map probe, i.e. more
/// work than the derive it replaces. Interning belongs at construction; that
/// means holding `VarName` segments here, which is a wider IR change than this
/// type can make on its own.
impl std::hash::Hash for QualifiedName {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.parts.len().hash(state);
        for (ident, subscripts) in &self.parts {
            ident.len().hash(state);
            subscripts.hash(state);
        }
    }
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
        write!(out, "{subscript}").expect("writing to a String cannot fail");
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
    pub each: bool,
    /// True if the modification has `final` prefix.
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ClassOverride {
    pub alias: String,
    pub alias_def_id: DefId,
    pub target_def_id: DefId,
    #[serde(deserialize_with = "crate::deserialize_required_option")]
    pub target_ref: Option<ComponentReference>,
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
#[serde(deny_unknown_fields)]
pub struct InstanceData {
    /// Unique identifier for this instance.
    pub instance_id: InstanceId,
    /// Exact source component declaration that produced this occurrence.
    ///
    /// This identity is distinct from `type_def_id`: two record occurrences of
    /// the same type may select different nested replaceable functions.
    #[serde(deserialize_with = "required")]
    pub declaration_def_id: Option<DefId>,
    /// Exact class occurrence that owns this component.
    #[serde(deserialize_with = "required")]
    pub owner_class_id: Option<InstanceId>,
    /// Structured resolved component reference for this concrete instance.
    ///
    /// This is the semantic carrier for downstream Flat/DAE phases. The
    /// rendered `qualified_name` remains useful for stable output spelling, but
    /// compiler logic should prefer this structured reference when available.
    #[serde(deserialize_with = "required")]
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
    #[serde(deserialize_with = "required")]
    pub type_def_id: Option<DefId>,
    /// Resolved first-segment declaration of a qualified type reference.
    ///
    /// For `Medium.State`, this identifies the `Medium` class/package slot
    /// without recovering semantic structure from the rendered type name.
    #[serde(deserialize_with = "required")]
    pub type_reference_root_def_id: Option<DefId>,
    /// Lexical scope where this component declaration was written.
    #[serde(deserialize_with = "required")]
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
    /// True when a redeclare modification was consumed for this component —
    /// either an `extends` modification that redeclared it
    /// (`extends Base(redeclare C a[2])`) or a redeclare modifier written on
    /// its own declaration (`Holder h(redeclare C a[2])`), MLS §7.3.
    ///
    /// Instantiation consumes only the redeclared *type*; the redeclaration's
    /// array dimensions are dropped. `dims` on such an instance (and on
    /// anything instantiated underneath it) is therefore this compiler's
    /// residue of the *original* declaration, not a statement about the model,
    /// and must never be reported to the user as one.
    pub had_redeclare: bool,

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
    #[serde(deserialize_with = "required")]
    pub start: Option<Expression>,
    /// Fixed attribute.
    #[serde(deserialize_with = "required")]
    pub fixed: Option<bool>,
    /// Minimum value attribute.
    #[serde(deserialize_with = "required")]
    pub min: Option<Expression>,
    /// Maximum value attribute.
    #[serde(deserialize_with = "required")]
    pub max: Option<Expression>,
    /// Nominal value attribute.
    #[serde(deserialize_with = "required")]
    pub nominal: Option<Expression>,
    /// Quantity string attribute.
    #[serde(deserialize_with = "required")]
    pub quantity: Option<String>,
    /// Unit string attribute.
    #[serde(deserialize_with = "required")]
    pub unit: Option<String>,
    /// Display-unit string attribute.
    #[serde(deserialize_with = "required")]
    pub display_unit: Option<String>,
    /// Optional declaration description string (`"..."` after component declaration).
    #[serde(deserialize_with = "required")]
    pub description: Option<String>,
    /// State selection hint.
    pub state_select: StateSelect,

    /// Binding equation value (resolved).
    #[serde(deserialize_with = "required")]
    pub binding: Option<Expression>,
    /// Optional symbolic binding source expression for modification-derived bindings.
    ///
    /// MLS §7.2.4: component modifications are written in an outer scope and may
    /// intentionally reference outer parameters (e.g., `gain(g = k)`).
    /// We retain this source form for flat-output rendering while keeping `binding`
    /// available as a resolved value for semantic passes.
    #[serde(deserialize_with = "required")]
    pub binding_source: Option<Expression>,
    /// Lexical scope where a modification-derived binding was written.
    ///
    /// Used during flattening to qualify symbolic modifier references according
    /// to MLS §7.2.4 without path-depth heuristics.
    #[serde(deserialize_with = "required")]
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
    /// True if this component is declared in a protected section (MLS §4.7).
    /// Protected components are not part of the public interface and their flow
    /// variables should not count as interface flows for balance checking.
    pub is_protected: bool,
    /// True if this component's type is a `connector` class (MLS §4.7).
    /// Per MLS §4.7, only flow variables in top-level public connector components
    /// count toward the local equation size. Models/blocks (like Delta) are NOT
    /// interface connectors even if they contain sub-connectors.
    pub is_connector_type: bool,
    /// True if this component's type is an expandable connector (MLS §9.1.3).
    ///
    /// Kept on the container instance because an empty expandable connector has
    /// no flattened descendants from which later phases could recover this
    /// semantic fact.
    pub is_expandable_connector_type: bool,
}

impl Default for InstanceData {
    fn default() -> Self {
        Self {
            instance_id: InstanceId::default(),
            declaration_def_id: None,
            owner_class_id: None,
            component_ref: None,
            qualified_name: QualifiedName::default(),
            source_location: rumoca_core::Location::default(),
            dims: Vec::new(),
            dims_expr: Vec::new(),
            type_id: TypeId::default(),
            type_name: String::new(),
            type_def_id: None,
            type_reference_root_def_id: None,
            declaration_source_scope: None,
            class_overrides: IndexMap::default(),
            has_forwarding_class_redeclare: false,
            had_redeclare: false,
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
            is_protected: false,
            is_connector_type: false,
            is_expandable_connector_type: false,
        }
    }
}

/// Instance data for a class/model.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClassInstanceData {
    /// Unique identifier for this class instance.
    pub instance_id: InstanceId,
    /// Exact structured component occurrence whose type instantiated this
    /// class. The root model has no component owner.
    pub owner_component_id: Option<InstanceId>,
    /// DefId of the class definition this instance was instantiated from.
    pub class_def_id: Option<DefId>,
    /// Fully qualified name in the instance tree.
    pub qualified_name: QualifiedName,
    /// Lexical scope of the class declaration that produced this instance.
    #[serde(deserialize_with = "crate::deserialize_required_option")]
    pub source_scope: Option<QualifiedName>,
    /// Resolved lexical scope of the class declaration that produced this instance.
    #[serde(deserialize_with = "crate::deserialize_required_option")]
    pub source_scope_id: Option<ScopeId>,
    /// Effective replaceable class/package selections in this concrete class instance.
    ///
    /// Instantiation owns this context because it is the first phase where the
    /// complete modification environment and enclosing redeclares are known.
    /// Downstream phases must consume it directly instead of reconstructing
    /// virtual class identity from instance paths.
    pub class_overrides: ClassOverrideMap,
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
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(
    tag = "kind",
    content = "connection",
    rename_all = "snake_case",
    deny_unknown_fields
)]
pub enum InstanceConnection {
    /// One source connection between two concrete scalar endpoints.
    Scalar(InstanceScalarConnection),
    /// One authoritative compact family of scalar connections.
    Family(InstanceConnectionFamily),
}

impl InstanceConnection {
    pub fn scalar(
        a: QualifiedName,
        b: QualifiedName,
        connector_type: Option<DefId>,
        span: Span,
        scope: String,
    ) -> Result<Self, InstanceConnectionConstructionError> {
        Ok(Self::Scalar(InstanceScalarConnection::new(
            a,
            b,
            connector_type,
            span,
            scope,
        )?))
    }

    pub fn family(
        domain: rumoca_core::StructuredIndexDomain,
        a: InstanceConnectionEndpoint,
        b: InstanceConnectionEndpoint,
        connector_type: Option<DefId>,
        span: Span,
        scope: String,
    ) -> Result<Self, InstanceConnectionConstructionError> {
        Ok(Self::Family(InstanceConnectionFamily::new(
            domain,
            a,
            b,
            connector_type,
            span,
            scope,
        )?))
    }

    pub fn as_scalar(&self) -> Option<&InstanceScalarConnection> {
        match self {
            Self::Scalar(connection) => Some(connection),
            Self::Family(_) => None,
        }
    }

    pub fn as_family(&self) -> Option<&InstanceConnectionFamily> {
        match self {
            Self::Scalar(_) => None,
            Self::Family(family) => Some(family),
        }
    }
}

/// One concrete scalar member of an instance connection.
///
/// Its invariants cannot be bypassed with a field literal; callers must use
/// [`InstanceScalarConnection::new`] or [`InstanceConnection::scalar`].
///
/// ```compile_fail
/// use rumoca_ir_ast::{InstanceScalarConnection, QualifiedName};
/// use rumoca_core::Span;
///
/// let _forged = InstanceScalarConnection {
///     a: QualifiedName::new(),
///     b: QualifiedName::new(),
///     connector_type: None,
///     span: Span::DUMMY,
///     scope: String::new(),
/// };
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct InstanceScalarConnection {
    /// First connector.
    a: QualifiedName,
    /// Second connector.
    b: QualifiedName,
    /// Type of the connectors.
    connector_type: Option<DefId>,
    /// Source span for error reporting.
    span: Span,
    /// Scope where the connect statement was declared (flattened prefix).
    /// Used to determine the correct hierarchy level for flow sum equations.
    /// Empty string means root level.
    scope: String,
}

impl InstanceScalarConnection {
    pub fn new(
        a: QualifiedName,
        b: QualifiedName,
        connector_type: Option<DefId>,
        span: Span,
        scope: String,
    ) -> Result<Self, InstanceConnectionConstructionError> {
        validate_scalar_connection_endpoint(&a, "left")?;
        validate_scalar_connection_endpoint(&b, "right")?;
        let span = span
            .require_provenance("constructing a scalar instance connection")
            .map_err(|_| InstanceConnectionConstructionError::MissingProvenance)?
            .span();
        Ok(Self {
            a,
            b,
            connector_type,
            span,
            scope,
        })
    }

    pub fn a(&self) -> &QualifiedName {
        &self.a
    }

    pub fn b(&self) -> &QualifiedName {
        &self.b
    }

    pub fn connector_type(&self) -> Option<DefId> {
        self.connector_type
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn scope(&self) -> &str {
        &self.scope
    }
}

impl<'de> Deserialize<'de> for InstanceScalarConnection {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            a: QualifiedName,
            b: QualifiedName,
            connector_type: Option<DefId>,
            span: Span,
            scope: String,
        }

        let wire = Wire::deserialize(deserializer)?;
        Self::new(wire.a, wire.b, wire.connector_type, wire.span, wire.scope)
            .map_err(serde::de::Error::custom)
    }
}

/// A qualified connection endpoint whose subscripts are affine in a structured
/// connection family's binders.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct InstanceConnectionEndpoint {
    parts: Vec<(String, Vec<rumoca_core::AffineForm>)>,
}

impl InstanceConnectionEndpoint {
    pub fn new(
        parts: Vec<(String, Vec<rumoca_core::AffineForm>)>,
    ) -> Result<Self, InstanceConnectionConstructionError> {
        if parts.is_empty() {
            return Err(InstanceConnectionConstructionError::EmptyEndpoint);
        }
        if parts.iter().any(|(name, _)| name.is_empty()) {
            return Err(InstanceConnectionConstructionError::EmptyEndpointPart);
        }
        Ok(Self { parts })
    }

    pub fn parts(&self) -> &[(String, Vec<rumoca_core::AffineForm>)] {
        &self.parts
    }
}

impl<'de> Deserialize<'de> for InstanceConnectionEndpoint {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            parts: Vec<(String, Vec<rumoca_core::AffineForm>)>,
        }

        let wire = Wire::deserialize(deserializer)?;
        Self::new(wire.parts).map_err(serde::de::Error::custom)
    }
}

/// Compact instance-IR representation of a regular vectorized `connect`.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct InstanceConnectionFamily {
    domain: rumoca_core::StructuredIndexDomain,
    a: InstanceConnectionEndpoint,
    b: InstanceConnectionEndpoint,
    /// Type of the connectors.
    connector_type: Option<DefId>,
    /// Source span for error reporting.
    span: Span,
    /// Scope where the connect statement was declared (flattened prefix).
    scope: String,
}

impl InstanceConnectionFamily {
    fn new(
        domain: rumoca_core::StructuredIndexDomain,
        a: InstanceConnectionEndpoint,
        b: InstanceConnectionEndpoint,
        connector_type: Option<DefId>,
        span: Span,
        scope: String,
    ) -> Result<Self, InstanceConnectionConstructionError> {
        let scalar_count = domain.validate().map_err(|error| {
            InstanceConnectionConstructionError::InvalidDomain(error.to_string())
        })?;
        for (expected, binder) in domain.binders.iter().enumerate() {
            let expected = rumoca_core::StructuredIndexBinderId::from_ordinal(expected)
                .expect("structured-domain rank must fit its typed binder identity");
            if binder.id != expected {
                return Err(InstanceConnectionConstructionError::NonCanonicalBinderId {
                    expected,
                    actual: binder.id,
                });
            }
        }
        for endpoint in [&a, &b] {
            validate_connection_endpoint_forms(endpoint, &domain, scalar_count)?;
        }
        let span = span
            .require_provenance("constructing an instance connection family")
            .map_err(|_| InstanceConnectionConstructionError::MissingProvenance)?
            .span();
        Ok(Self {
            domain,
            a,
            b,
            connector_type,
            span,
            scope,
        })
    }

    pub fn domain(&self) -> &rumoca_core::StructuredIndexDomain {
        &self.domain
    }

    pub fn a(&self) -> &InstanceConnectionEndpoint {
        &self.a
    }

    pub fn b(&self) -> &InstanceConnectionEndpoint {
        &self.b
    }

    pub fn connector_type(&self) -> Option<DefId> {
        self.connector_type
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn scope(&self) -> &str {
        &self.scope
    }
}

impl<'de> Deserialize<'de> for InstanceConnectionFamily {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Wire {
            domain: rumoca_core::StructuredIndexDomain,
            a: InstanceConnectionEndpoint,
            b: InstanceConnectionEndpoint,
            connector_type: Option<DefId>,
            span: Span,
            scope: String,
        }

        let wire = Wire::deserialize(deserializer)?;
        Self::new(
            wire.domain,
            wire.a,
            wire.b,
            wire.connector_type,
            wire.span,
            wire.scope,
        )
        .map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstanceConnectionConstructionError {
    EmptyEndpoint,
    EmptyEndpointPart,
    EmptyScalarEndpoint {
        side: &'static str,
    },
    InvalidDomain(String),
    NonCanonicalBinderId {
        expected: rumoca_core::StructuredIndexBinderId,
        actual: rumoca_core::StructuredIndexBinderId,
    },
    AffineRank {
        expected: usize,
        actual: usize,
    },
    AffineValueRange,
    MissingProvenance,
}

impl std::fmt::Display for InstanceConnectionConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::EmptyEndpoint => formatter.write_str("connection family endpoint is empty"),
            Self::EmptyEndpointPart => {
                formatter.write_str("connection family endpoint contains an empty path part")
            }
            Self::EmptyScalarEndpoint { side } => {
                write!(formatter, "scalar connection {side} endpoint is empty")
            }
            Self::InvalidDomain(reason) => {
                write!(formatter, "invalid connection family domain: {reason}")
            }
            Self::NonCanonicalBinderId { expected, actual } => write!(
                formatter,
                "connection family binder id {actual} is not canonical id {expected}"
            ),
            Self::AffineRank { expected, actual } => write!(
                formatter,
                "connection endpoint affine rank {actual} does not match domain rank {expected}"
            ),
            Self::AffineValueRange => formatter.write_str(
                "connection endpoint affine subscript exceeds i64 over its nonempty domain",
            ),
            Self::MissingProvenance => {
                formatter.write_str("connection family requires non-dummy source provenance")
            }
        }
    }
}

impl std::error::Error for InstanceConnectionConstructionError {}

fn validate_scalar_connection_endpoint(
    endpoint: &QualifiedName,
    side: &'static str,
) -> Result<(), InstanceConnectionConstructionError> {
    if endpoint.parts.is_empty() || endpoint.parts.iter().any(|(name, _)| name.is_empty()) {
        return Err(InstanceConnectionConstructionError::EmptyScalarEndpoint { side });
    }
    Ok(())
}

fn validate_connection_endpoint_forms(
    endpoint: &InstanceConnectionEndpoint,
    domain: &rumoca_core::StructuredIndexDomain,
    scalar_count: usize,
) -> Result<(), InstanceConnectionConstructionError> {
    let rank = domain.binders.len();
    let forms = endpoint
        .parts()
        .iter()
        .flat_map(|(_, subscripts)| subscripts);
    for form in forms {
        if form.coeffs.len() != rank {
            return Err(InstanceConnectionConstructionError::AffineRank {
                expected: rank,
                actual: form.coeffs.len(),
            });
        }
        if scalar_count != 0 {
            validate_affine_connection_range(form, domain)?;
        }
    }
    Ok(())
}

fn validate_affine_connection_range(
    form: &rumoca_core::AffineForm,
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<(), InstanceConnectionConstructionError> {
    let mut minimum = i128::from(form.constant);
    let mut maximum = minimum;
    for (coefficient, binder) in form.coeffs.iter().zip(&domain.binders) {
        let lower = i128::from(binder.lower);
        let upper = i128::from(binder.upper);
        let step = i128::from(binder.step);
        let distance = if step > 0 {
            upper - lower
        } else {
            lower - upper
        };
        let last = lower + (distance / step.abs()) * step;
        let first_term = i128::from(*coefficient) * i128::from(binder.lower);
        let last_term = i128::from(*coefficient) * last;
        minimum = minimum
            .checked_add(first_term.min(last_term))
            .ok_or(InstanceConnectionConstructionError::AffineValueRange)?;
        maximum = maximum
            .checked_add(first_term.max(last_term))
            .ok_or(InstanceConnectionConstructionError::AffineValueRange)?;
    }
    if minimum < i128::from(i64::MIN) || maximum > i128::from(i64::MAX) {
        return Err(InstanceConnectionConstructionError::AffineValueRange);
    }
    Ok(())
}

/// Overlay containing instance-specific data keyed by InstanceId.
///
/// Keeps instance data separate from the core AST types to avoid
/// polluting shared definitions with per-instance state.
///
/// Note: We use InstanceId as the key because each instance gets a unique
/// InstanceId during instantiation, whereas DefIds identify declarations
/// (which can have multiple instances). InstanceId is a simple u32, so
/// lookups are O(1) with a cheap hasher for numeric keys.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstanceOverlayInsertError {
    UnsetComponentIdentity,
    UnsetClassIdentity,
    UnallocatedComponentIdentity(InstanceId),
    UnallocatedClassIdentity(InstanceId),
    DuplicateComponent(InstanceId),
    DuplicateClass(InstanceId),
    MismatchedComponentClassPair(InstanceId),
}

impl std::fmt::Display for InstanceOverlayInsertError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnsetComponentIdentity => {
                formatter.write_str("component occurrence cannot use InstanceId::UNSET")
            }
            Self::UnsetClassIdentity => {
                formatter.write_str("class occurrence cannot use InstanceId::UNSET")
            }
            Self::UnallocatedComponentIdentity(instance) => write!(
                formatter,
                "component occurrence {instance:?} was not allocated by this overlay"
            ),
            Self::UnallocatedClassIdentity(instance) => write!(
                formatter,
                "class occurrence {instance:?} was not allocated by this overlay"
            ),
            Self::DuplicateComponent(instance) => write!(
                formatter,
                "component occurrence {instance:?} is already registered"
            ),
            Self::DuplicateClass(instance) => write!(
                formatter,
                "class occurrence {instance:?} is already registered"
            ),
            Self::MismatchedComponentClassPair(instance) => write!(
                formatter,
                "component/class occurrence pair {instance:?} has mismatched owner or qualified-name evidence"
            ),
        }
    }
}

impl std::error::Error for InstanceOverlayInsertError {}

#[derive(Debug, Clone, Default)]
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
    /// Component bindings introduced by an `each` modifier (MLS §7.2.5).
    ///
    /// This remains structured instantiation metadata so flattening can preserve
    /// element-wise modifier semantics when an array component stays compact.
    pub each_modifier_bindings: IndexSet<ComponentPath>,
    /// Array parent dimensions for expanded array components.
    /// When an array component like `plug_p.pin[3]` is expanded to indexed instances
    /// (`plug_p.pin[1]`, `plug_p.pin[2]`, `plug_p.pin[3]`), this map stores the parent
    /// path `plug_p.pin` with dimensions `[3]` for use in array equation expansion.
    pub array_parent_dims: IndexMap<ComponentPath, Vec<i64>>,
    /// Mapping from outer-prefixed paths to their corresponding inner paths (MLS §5.4).
    /// When an outer component `initialStep.stateGraphRoot` references inner `stateGraphRoot`,
    /// equations/connections using the outer prefix are redirected to the inner path.
    pub outer_prefix_to_inner: IndexMap<ComponentPath, ComponentPath>,
    /// Mapping from inner-outer component paths to their parent inner paths (MLS §5.4).
    /// When a component is declared `inner outer` (e.g., `inner outer StateGraphRoot stateGraphRoot`),
    /// it bridges two scopes: it serves as `inner` for children and as `outer` referencing the parent.
    /// Same-level connections involving the `inner outer` component should redirect to the parent's
    /// inner for flow equation scoping (e.g., `makeProduct.stateGraphRoot` → `stateGraphRoot`).
    pub inner_outer_to_parent_inner: IndexMap<ComponentPath, ComponentPath>,
    /// Names of inner declarations synthesized during instantiation retry (MLS §5.4).
    /// Populated when `outer` components had no matching `inner` and automatic
    /// synthesis succeeded.
    pub synthesized_inners: Vec<String>,
    /// Canonical type roots for compatibility checks (alias/enumeration normalization).
    /// Keys are resolved type identities and values are canonical root type identities.
    /// Populated by typecheck_instanced for flatten-time type compatibility.
    pub type_roots: IndexMap<TypeId, TypeId>,
    /// Exact nominal type identity keyed by resolved source declaration identity.
    ///
    /// This is producer-owned transition evidence from post-instantiation
    /// typechecking. Flattening uses it to type function values without
    /// interpreting their display names.
    pub type_ids_by_def_id: FastIndexMap<DefId, TypeId>,
    /// Canonical `TypeId`s proven by typechecking to denote enumerations.
    ///
    /// Effective component identities remain in `enumeration_types`; this
    /// root catalog also covers function values that do not have component
    /// occurrences in the instance overlay.
    pub enumeration_type_roots: IndexSet<TypeId>,
    /// Concrete effective types produced after instance dimensions are resolved.
    ///
    /// Each component `type_id` names exactly one descriptor in this catalog
    /// after successful post-instantiation type checking.
    pub effective_types: FastIndexMap<TypeId, EffectiveType>,
    /// Effective type identities whose exact canonical root is an enumeration.
    ///
    /// Typecheck constructs this set from its resolved `TypeTable`; later
    /// phases never infer enumeration semantics from a rendered type name.
    pub enumeration_types: IndexSet<TypeId>,
    /// Exact declaration/occurrence proof awaiting effective TypeId issuance.
    pending_overconstrained_records:
        FastIndexMap<InstanceId, PendingEqualityConstraintOccurrenceExposure>,
    /// Exact checked `equalityConstraint` exposure after typecheck binds the
    /// effective record TypeId. This catalog is construction-owned.
    overconstrained_records: FastIndexMap<InstanceId, FinalizedOverconstrainedOccurrence>,
    /// Exact innermost overconstrained record occurrence that owns each
    /// instantiated primitive descendant.
    overconstrained_record_owners: FastIndexMap<InstanceId, InstanceId>,
    /// The descendant-owner catalog is issued exactly once after the complete
    /// component/class overlay has been built.
    overconstrained_record_owners_finalized: bool,
    /// Typecheck atomically upgraded every pending record exposure.
    overconstrained_effective_types_finalized: bool,
    /// Closed semantic identities published by the same successful Typecheck
    /// transition as effective types. No earlier phase can expose a partial
    /// operator or ExternalObject catalog to Flatten.
    semantic_catalogs: Option<SemanticCatalogProjection>,
    /// Number of occurrence identities allocated so far.
    ///
    /// Allocation is one-based because `InstanceId::UNSET` reserves zero, so
    /// this is also the last identity handed out.
    next_id: u32,
}

fn instance_component_class_pair_matches(
    component: &InstanceData,
    class: &ClassInstanceData,
) -> bool {
    !component.is_primitive
        && class.owner_component_id == Some(component.instance_id)
        && component.qualified_name == class.qualified_name
        && component.type_def_id.is_some()
        && component.type_def_id == class.class_def_id
}

impl InstanceOverlay {
    /// Create a new empty overlay.
    pub fn new() -> Self {
        Self::default()
    }

    /// Allocate a new unique InstanceId.
    ///
    /// Identities are one-based: `InstanceId::UNSET` is reserved so a defaulted
    /// occurrence field can never be mistaken for an allocated instance.
    pub fn alloc_id(&mut self) -> InstanceId {
        self.next_id = self
            .next_id
            .checked_add(1)
            .expect("instantiated occurrence identity space exhausted");
        InstanceId(self.next_id)
    }

    /// Number of `InstanceId`s allocated so far.
    ///
    /// Callers that snapshot a subtree use this to check that every id they
    /// allocated is still reachable through `components`/`classes`.
    pub fn allocated_instance_count(&self) -> u32 {
        self.next_id
    }

    /// Add instance data for a component.
    ///
    /// The component is keyed by its InstanceId to ensure uniqueness,
    /// since multiple instances can share the same DefId.
    pub fn add_component(&mut self, data: InstanceData) -> Result<(), InstanceOverlayInsertError> {
        let key = data.instance_id;
        if key.is_unset() {
            return Err(InstanceOverlayInsertError::UnsetComponentIdentity);
        }
        if key.index() > self.next_id {
            return Err(InstanceOverlayInsertError::UnallocatedComponentIdentity(
                key,
            ));
        }
        if self.components.contains_key(&key) {
            return Err(InstanceOverlayInsertError::DuplicateComponent(key));
        }
        if let Some(class) = self.classes.get(&key)
            && !instance_component_class_pair_matches(&data, class)
        {
            return Err(InstanceOverlayInsertError::MismatchedComponentClassPair(
                key,
            ));
        }
        self.components.insert(key, data);
        Ok(())
    }

    /// Add instance data for a class.
    ///
    /// The class is keyed by its InstanceId to ensure uniqueness.
    pub fn add_class(&mut self, data: ClassInstanceData) -> Result<(), InstanceOverlayInsertError> {
        let key = data.instance_id;
        if key.is_unset() {
            return Err(InstanceOverlayInsertError::UnsetClassIdentity);
        }
        if key.index() > self.next_id {
            return Err(InstanceOverlayInsertError::UnallocatedClassIdentity(key));
        }
        if self.classes.contains_key(&key) {
            return Err(InstanceOverlayInsertError::DuplicateClass(key));
        }
        if let Some(component) = self.components.get(&key)
            && !instance_component_class_pair_matches(component, &data)
        {
            return Err(InstanceOverlayInsertError::MismatchedComponentClassPair(
                key,
            ));
        }
        self.classes.insert(key, data);
        Ok(())
    }

    /// Get instance data for a component by InstanceId.
    pub fn get_component(&self, instance_id: InstanceId) -> Option<&InstanceData> {
        self.components.get(&instance_id)
    }
}

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

#[cfg(test)]
mod tests;
