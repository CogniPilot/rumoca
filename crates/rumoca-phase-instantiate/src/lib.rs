//! Instantiation phase for the Rumoca compiler.
//!
//! This crate implements the instantiation pass that converts a
//! a resolved class tree to an [`ast::InstanceOverlay`].
//! It finds the root model, applies modifications recursively, evaluates structural
//! parameters, and builds the instance overlay.
//!
//! # Overview
//!
//! Instantiation is responsible for:
//! - Finding the root model to instantiate
//! - Processing extends clauses (inheritance) - MLS §7.1
//! - Applying modifications (parameter values, redeclarations) - MLS §7.2, §7.3
//! - Evaluating structural parameters to determine array sizes
//! - Building the instance overlay with qualified names
//! - Resolving inner/outer component references - MLS §5.4
//! - Extracting connections for later expansion - MLS §9
//!
//! # MLS Compliance
//!
//! See the `inheritance` module for detailed MLS §7 compliance status.
//!
//! Key features implemented:
//! - **MLS §5.4**: Inner/outer component resolution with type compatibility
//! - **MLS §7.1**: Extends clause processing with inheritance caching
//! - **MLS §7.2**: Modification environment (outer overrides inner)
//! - **MLS §7.3**: Redeclaration validation (replaceable/final)
//! - **MLS §7.4**: Selective extension (`break` names)
//!
//! # Example
//!
//! ```ignore
//! use rumoca_phase_instantiate::{InstantiationOutcome, instantiate_model_with_outcome};
//!
//! let resolved: rumoca_phase_resolve::ResolvedTree = resolve(parsed)?;
//! match instantiate_model_with_outcome(resolved.inner(), "MyModel") {
//!     InstantiationOutcome::Success(overlay) => use_overlay(overlay),
//!     InstantiationOutcome::NeedsInner { missing_inners, .. } => {
//!         report_missing_inners(missing_inners)
//!     }
//!     InstantiationOutcome::Error(error) => return Err(error),
//! }
//! ```

mod array_expansion;
mod attributes;
mod component_loop;
mod conditional_components;
mod connections;
mod dims;
mod entry;
mod equality_constraint;
mod errors;
mod inheritance;
mod inner_outer;
mod instance_sections;
mod mod_env;
mod nested_instantiation;
mod nested_scope;
mod package_constant_imports;
mod path_utils;
mod plug_compat;
mod source_scope;
mod templates;
mod traversal_adapter;
mod type_lookup;
mod type_overrides;

pub(crate) use entry::description_tokens_to_string;
pub use entry::{instantiate_model_with_outcome, instantiate_model_with_outcome_options};

use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, OuterValues, array_index_tuples, component_allows_structural_evaluation,
    component_explicitly_disables_structural_evaluation, component_has_evaluate_annotation,
    evaluate_array_dimensions_with_index, evaluate_component_condition_with_outer_values,
    extract_binding, extract_bool_params_with_mods, extract_int_params_with_mods,
    extract_real_params_with_mods, propagate_record_alias_integer_params,
    propagate_scoped_record_alias_integer_params, try_eval_integer_expr, try_eval_real_expr,
};

use rumoca_core::Diagnostics;
use rumoca_core::{DefId, Span, TypeId};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::sync::Arc;

use array_expansion::{ArrayExpansionScope, expand_array_component};
use attributes::*;
use component_loop::{
    ComponentImports, EffectiveComponentConstruction, component_flow_stream, component_type_id,
    instantiate_effective_components,
};
use conditional_components::{ConditionScope, mark_disabled_component_if_needed};
use dims::{
    qualify_shape_subscripts_imports, resolve_component_dimensions, resolve_type_alias_dimensions,
};
use equality_constraint::construct_equality_constraint_exposure;
#[cfg(test)]
pub(crate) use inner_outer::inner_visible_to_outer;
pub(crate) use inner_outer::{
    SyntheticInnerError, handle_inner_outer, preregister_class_inners, retry_with_synthetic_inners,
};
use instance_sections::{
    algorithms_to_instance, equations_to_instance_cloned, equations_to_instance_without_connections,
};
use mod_env::{
    PopulateModEnvInput, RecordBindingProjection, populate_modification_environment,
    propagate_record_binding_to_fields,
};
use nested_instantiation::{NestedInstantiationInput, instantiate_nested_class};
use nested_scope::{
    collect_referenced_mod_roots, collect_shifted_parent_mod_keys, collect_targeted_mod_keys,
    key_matches_referenced_root, resolve_component_nested_type_overrides, shift_modifications_down,
};
use package_constant_imports::{
    resolved_imports_with_active_package_constants,
    resolved_imports_with_enclosing_package_constants,
};
use source_scope::{
    SourceScopeIndex, class_declaration_source_scope, component_declaration_source_scope,
    component_effective_imports, expression_source_scope, register_zero_sized_array_component,
};
use templates::get_or_compute_template;
#[cfg(test)]
use type_lookup::is_type_compatible;
use type_lookup::{
    TypeInfo, is_type_compatible_with_def_id, lookup_type_info, resolve_primitive_type_id,
};
use type_overrides::{
    SelectedComponentTypeCatalog, SelectedComponentTypes, TypeOverrideMap, build_type_override_map,
    component_with_issued_type_selection, issue_selected_component_types,
    resolve_dynamic_equation_targets_at_occurrence,
    resolve_dynamic_expression_targets_at_occurrence,
    resolve_dynamic_statement_targets_at_occurrence,
    resolve_dynamic_subscript_targets_at_occurrence,
};

pub use connections::{ConnectionParams, extract_connections, filter_out_connections};
pub(crate) use errors::InstantiateResult;
pub use errors::{InstantiateError, InstantiateWarning, InstantiationOutcome};
pub use inheritance::resolve_effective_components_for_eval;
pub use inheritance::{
    InheritanceCache, InheritedContent, SubtypeCache, class_extends, class_extends_cached,
    find_class_in_tree, get_effective_components, get_effective_components_with_cache,
    get_effective_equations, get_effective_equations_with_cache, is_type_subtype,
    is_type_subtype_cached, location_to_span, process_extends, process_extends_with_cache,
};
pub use templates::{ClassTemplate, ClassTemplateCache};

/// Extracted attribute values from a component's modifications.
#[derive(Debug, Clone, Default)]
pub struct ExtractedAttributes {
    pub start: Option<ast::Expression>,
    pub start_is_explicit: bool,
    pub fixed: Option<bool>,
    pub min: Option<ast::Expression>,
    pub max: Option<ast::Expression>,
    pub nominal: Option<ast::Expression>,
    /// Lexical scope in which each attribute expression was written, keyed by
    /// attribute name. These are class-namespace scopes used downstream for
    /// import-sensitive name resolution (MLS §13.2). They are NOT instance
    /// occurrences and must never key the selected-component-type catalog.
    pub source_scopes: IndexMap<String, ast::QualifiedName>,
    /// Instance occurrence that wrote each attribute modification, keyed by
    /// attribute name. Populated only for modifications carried through the
    /// modification environment, whose source scope is an instance path in the
    /// same namespace as the selected-component-type catalog (the same
    /// provenance as `binding_source_scope`). An attribute written locally on
    /// the declaration or through an inherited `extends` modifier has no entry
    /// here; its instance occurrence is the component's declaration occurrence.
    pub attribute_instance_scopes: IndexMap<String, ast::QualifiedName>,
    pub quantity: Option<String>,
    pub unit: Option<String>,
    pub display_unit: Option<String>,
    pub state_select: rumoca_core::StateSelect,
}

/// Information about a missing inner declaration, collected during instantiation.
/// Used to synthesize default inner declarations for retry (MLS §5.4).
#[derive(Debug, Clone)]
struct MissingInnerInfo {
    name: String,
    type_name: String,
    type_def_id: Option<DefId>,
    span: Span,
    source_location: rumoca_core::Location,
    outer_path: ast::QualifiedName,
    is_inner_outer: bool,
}

/// An inner declaration for inner/outer resolution (MLS §5.4).
#[derive(Debug, Clone)]
struct InnerDeclaration {
    /// Qualified name of the inner component in the instance tree.
    qualified_name: ast::QualifiedName,
    /// Type name of the inner component (for error messages).
    type_name: String,
    /// DefId of the inner component's type (for O(1) comparison).
    type_def_id: Option<DefId>,
}

/// Default path depth limit used to prevent stack overflow from malformed input.
/// Conservative because each level creates multiple Rust stack frames.
pub const DEFAULT_INSTANTIATION_DEPTH_LIMIT: usize = 30;

/// Rendered scope path of the simulated root (MLS §5.3).
///
/// Instance scope paths are rendered as dot-joined component paths, so the
/// root scope — the scope of a component declared directly in the simulated
/// model — renders as the empty path.
const ROOT_SCOPE_PATH: &str = "";

#[derive(Debug, Clone)]
pub struct InstantiateOptions {
    pub depth_limit: usize,
    /// Synthetic root modifications (`parameter = <literal>`) injected into the
    /// root model's modification environment before instantiation. This is how
    /// structural parameter overrides re-evaluate array dimensions and
    /// conditional-component activation — the modification flows down to nested
    /// components via the normal `shift_modifications_down` mechanism, exactly as
    /// a source-level modification would. Empty by default.
    pub root_modifications: Vec<(ast::QualifiedName, ast::ModificationValue)>,
    /// Instantiate homogeneous arrays of structured components once and derive
    /// the remaining domain points from that template (SPEC_0032 §1).
    ///
    /// Enabled by default. Turning it off forces the element-by-element
    /// expansion and exists so differential tests can prove the two paths
    /// produce identical overlays.
    pub compact_component_families: bool,
}

impl Default for InstantiateOptions {
    fn default() -> Self {
        Self {
            depth_limit: DEFAULT_INSTANTIATION_DEPTH_LIMIT,
            root_modifications: Vec::new(),
            compact_component_families: true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum InstantiationFrameKey {
    Def(DefId),
}

#[derive(Clone, Debug)]
struct InstantiationFrame {
    key: InstantiationFrameKey,
    class_name: String,
    instance_path: String,
}

#[derive(Clone, Debug, Default)]
struct ScopeFrame {
    variability: Option<rumoca_core::Variability>,
    /// MLS section 18.6: the component or an enclosing one carries
    /// annotation(Evaluate = true), so everything instantiated beneath it is
    /// evaluated during translation, record members included.
    evaluate: bool,
    /// An enclosing parameter occurrence explicitly set `fixed=false` or
    /// `Evaluate=false`; nested values must not re-enter structural maps.
    structural_evaluation_blocked: bool,
    causality: Option<rumoca_core::Causality>,
    flow: bool,
    stream: bool,
    expandable: bool,
    protected: bool,
}

struct ScopeFrameInput<'a> {
    variability: &'a rumoca_core::Variability,
    evaluate: bool,
    structural_evaluation_blocked: bool,
    causality: &'a rumoca_core::Causality,
    flow: bool,
    stream: bool,
    expandable: bool,
    protected: bool,
}

impl ScopeFrame {
    fn inherited_from_component(
        input: ScopeFrameInput<'_>,
        _context_path: &ast::QualifiedName,
    ) -> Self {
        let variability = matches!(
            input.variability,
            rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
        )
        .then(|| input.variability.clone());
        let causality = matches!(
            input.causality,
            rumoca_core::Causality::Input(_) | rumoca_core::Causality::Output(_)
        )
        .then(|| input.causality.clone());
        Self {
            variability,
            evaluate: input.evaluate,
            structural_evaluation_blocked: input.structural_evaluation_blocked,
            causality,
            flow: input.flow,
            stream: input.stream,
            expandable: input.expandable,
            protected: input.protected,
        }
    }
}

/// Context for instantiation.
pub struct InstantiateContext {
    /// Diagnostics collector.
    pub diags: Diagnostics,
    /// Current context path during instantiation.
    context_path: Vec<(String, Vec<i64>)>,
    /// Resolve identity for each corresponding component-path segment.
    ///
    /// Non-component path probes may temporarily leave an entry unresolved,
    /// but `instantiate_component` must prove the current segment before it can
    /// construct instance data.
    context_path_def_ids: Vec<Option<rumoca_core::DefId>>,
    /// Next available instance ID.
    next_instance_id: u32,
    /// Modification environment for the current scope.
    mod_env: ast::ModificationEnvironment,
    /// Inner declarations visible in the current scope (MLS §5.4).
    /// Maps component name to inner declaration info.
    /// Stack-based: each entry contains the inner declarations at that scope level.
    inner_scopes: Vec<IndexMap<String, InnerDeclaration>>,
    /// Missing inner declarations encountered during instantiation (MLS §5.4).
    /// These are outer components without matching inner declarations.
    /// Collected with type info for synthetic inner synthesis.
    missing_inners: Vec<MissingInnerInfo>,
    /// Per-scope inherited prefixes and connector metadata.
    scope_frames: Vec<ScopeFrame>,
    /// Cache for class templates to avoid recomputation.
    /// When instantiating the same class multiple times (e.g., Resistor r[100]),
    /// we cache the template and only apply per-instance modifications.
    template_cache: ClassTemplateCache,
    /// Integer parameter values discovered during instantiation, keyed by
    /// qualified path (e.g., `cellData.nRC`).
    known_int_params: rustc_hash::FxHashMap<String, i64>,
    /// Boolean parameter values discovered during instantiation, keyed by
    /// qualified instance path (e.g., `world.driveTrainMechanics3D`).
    /// MLS §5.4 outer references in conditional-component conditions are
    /// resolved against this map through the matching inner instance path.
    known_bool_params: rustc_hash::FxHashMap<String, bool>,
    /// Real parameter values of pre-scanned `inner` instances, keyed by qualified
    /// instance path (e.g., `world.defaultBodyDiameter`). MLS §4.4.5 conditions
    /// that compare a Real parameter reached through an `outer` reference are
    /// resolved against this map (MLS §5.4).
    known_real_params: rustc_hash::FxHashMap<String, f64>,
    /// Whether partial class components are allowed in the current instantiation.
    /// This is true when the selected root model is declared partial.
    allow_partial_instantiation: bool,
    /// Instantiation behavior configured by the session or direct phase caller.
    options: InstantiateOptions,
    /// Stable identity stack for detecting recursive class/type instantiation.
    active_instantiations: Vec<InstantiationFrame>,
    /// Source declaration scopes keyed by resolved DefId.
    source_scope_index: SourceScopeIndex,
    /// Duplicate-aware equalityConstraint declarations, built once per phase
    /// invocation and replayed for every effective record occurrence.
    equality_constraint_declarations: Option<ast::EqualityConstraintDeclarationIndex>,
    /// Active package/type redeclarations inherited from enclosing component scopes.
    active_type_overrides: Vec<TypeOverrideMap>,
    /// All occurrence plans issued by the recursive traversal. Retention lets
    /// an enclosing expression traverse nested redeclare selections after the
    /// nested component has finished, without rebuilding facts from the overlay.
    selected_component_type_catalog: SelectedComponentTypeCatalog,
    active_package_constant_aliases: Vec<(String, DefId)>,
    /// Monotonic count of `inner`/`outer` registrations performed so far
    /// (MLS §5.4). Compact component-array replication is only sound when a
    /// template element performed none, because inner/outer resolution is
    /// path-dependent and cannot be derived by reindexing.
    inner_outer_events: usize,
}

impl InstantiateContext {
    /// Check if instantiation depth is too deep (prevents stack overflow).
    fn validate_depth_limit(
        &self,
        class: &ast::ClassDef,
        source_map: &rumoca_core::SourceMap,
    ) -> InstantiateResult<()> {
        let depth = self.context_path.len();
        if depth <= self.options.depth_limit {
            return Ok(());
        }

        Err(Box::new(InstantiateError::instantiation_depth_limit(
            self.current_path().to_string(),
            depth,
            self.options.depth_limit,
            location_to_span(
                &class.name.location,
                source_map,
                "instantiation depth class name",
            )?,
        )))
    }

    /// Create a new instantiate context.
    pub fn new() -> Self {
        Self::with_options(InstantiateOptions::default())
    }

    /// Create a new instantiate context with caller-supplied options.
    pub fn with_options(options: InstantiateOptions) -> Self {
        Self {
            diags: Diagnostics::new(),
            context_path: Vec::new(),
            context_path_def_ids: Vec::new(),
            next_instance_id: 0,
            mod_env: ast::ModificationEnvironment::new(),
            inner_scopes: vec![IndexMap::default()],
            missing_inners: Vec::new(),
            scope_frames: vec![ScopeFrame::default()],
            template_cache: ClassTemplateCache::default(),
            known_int_params: rustc_hash::FxHashMap::default(),
            known_bool_params: rustc_hash::FxHashMap::default(),
            known_real_params: rustc_hash::FxHashMap::default(),
            allow_partial_instantiation: false,
            options,
            active_instantiations: Vec::new(),
            source_scope_index: SourceScopeIndex::default(),
            equality_constraint_declarations: None,
            active_type_overrides: Vec::new(),
            selected_component_type_catalog: SelectedComponentTypeCatalog::new(),
            active_package_constant_aliases: Vec::new(),
            inner_outer_events: 0,
        }
    }

    fn index_source_scopes(&mut self, tree: &ast::ClassTree) {
        self.source_scope_index = SourceScopeIndex::from_tree(tree);
        self.equality_constraint_declarations =
            Some(ast::EqualityConstraintDeclarationIndex::new(tree));
    }

    fn equality_constraint_declarations(&self) -> &ast::EqualityConstraintDeclarationIndex {
        self.equality_constraint_declarations
            .as_ref()
            .expect("Instantiate entry initializes equalityConstraint declarations")
    }

    fn class_frame_key(class: &ast::ClassDef) -> Option<InstantiationFrameKey> {
        class.def_id.map(InstantiationFrameKey::Def)
    }

    fn enter_instantiation_class(
        &mut self,
        class: &ast::ClassDef,
        source_map: &rumoca_core::SourceMap,
    ) -> InstantiateResult<()> {
        let Some(key) = Self::class_frame_key(class) else {
            return Ok(());
        };

        let class_name = class.name.text.to_string();
        let current_path = self.current_path().to_string();
        if let Some(cycle_start) = self
            .active_instantiations
            .iter()
            .position(|frame| frame.key == key)
        {
            let mut cycle: Vec<String> = self.active_instantiations[cycle_start..]
                .iter()
                .map(|frame| format!("{} ({})", frame.class_name, frame.instance_path))
                .collect();
            cycle.push(format!("{class_name} ({current_path})"));
            return Err(Box::new(InstantiateError::instantiation_cycle(
                cycle.join(" -> "),
                location_to_span(
                    &class.name.location,
                    source_map,
                    "instantiation cycle class name",
                )?,
            )));
        }

        self.active_instantiations.push(InstantiationFrame {
            key,
            class_name,
            instance_path: current_path,
        });
        Ok(())
    }

    fn exit_instantiation_class(&mut self, class: &ast::ClassDef) {
        if Self::class_frame_key(class).is_some() {
            self.active_instantiations.pop();
        }
    }

    /// Configure whether partial class components may be instantiated.
    fn set_allow_partial_instantiation(&mut self, allow: bool) {
        self.allow_partial_instantiation = allow;
    }

    /// Register integer parameters discovered for a class scope.
    fn register_known_int_params(
        &mut self,
        scope: &ast::QualifiedName,
        local: &rustc_hash::FxHashMap<String, i64>,
    ) {
        let scope_prefix = scope.to_flat_string();
        for (k, v) in local {
            if !scope_prefix.is_empty() {
                self.known_int_params
                    .insert(format!("{scope_prefix}.{k}"), *v);
            } else {
                self.known_int_params.insert(k.clone(), *v);
            }
        }
    }

    /// Build the integer environment visible from one concrete class instance.
    fn merged_int_params_for_connections(
        &self,
        scope: &ast::QualifiedName,
        local: &rustc_hash::FxHashMap<String, i64>,
        outer: &rustc_hash::FxHashMap<String, i64>,
    ) -> rustc_hash::FxHashMap<String, i64> {
        scoped_connection_values(&self.known_int_params, scope, outer, local)
    }

    /// Build the structural Boolean environment used to select conditional
    /// connection branches. Child instances have already been instantiated at
    /// this point, so their qualified parameter values must participate too.
    fn merged_bool_params_for_connections(
        &self,
        scope: &ast::QualifiedName,
        local: &rustc_hash::FxHashMap<String, bool>,
        outer: &rustc_hash::FxHashMap<String, bool>,
    ) -> rustc_hash::FxHashMap<String, bool> {
        scoped_connection_values(&self.known_bool_params, scope, outer, local)
    }

    fn merged_real_params_for_connections(
        &self,
        scope: &ast::QualifiedName,
        local: &rustc_hash::FxHashMap<String, f64>,
        outer: &rustc_hash::FxHashMap<String, f64>,
    ) -> rustc_hash::FxHashMap<String, f64> {
        scoped_connection_values(&self.known_real_params, scope, outer, local)
    }

    /// Record the effective value of an instantiated structural integer.
    ///
    /// Class-level parameter extraction seeds declaration defaults before child
    /// components are instantiated. Re-evaluating the concrete instance binding
    /// here replaces that seed at the earliest point where modifier source scope
    /// and projected record fields are both known.
    fn register_known_integer_instance(
        &mut self,
        data: &ast::InstanceData,
        occurrence_allows_structural_evaluation: bool,
    ) {
        if !occurrence_allows_structural_evaluation
            || self.structural_evaluation_blocked()
            || data.fixed == Some(false)
            || !data.is_discrete_type
            || !matches!(
                data.variability,
                rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
            )
        {
            return;
        }
        let Some(binding) = data.binding.as_ref() else {
            return;
        };
        let declared_scope = if data.binding_from_modification {
            data.binding_source_scope
                .as_ref()
                .map(ast::QualifiedName::to_flat_string)
        } else {
            data.qualified_name
                .to_component_path()
                .parent()
                .map(|path| path.to_flat_string())
        };
        // A component declared directly in the simulated root has no parent
        // scope; MLS §5.3 lookup for it starts at the root, whose rendered
        // scope path is empty. This is the root scope, not a silent default.
        let scope = declared_scope.unwrap_or_else(|| ROOT_SCOPE_PATH.to_string());
        let mut eval_ctx =
            rumoca_eval_ast::eval::TypeCheckEvalContext::for_pre_identity_structural();
        eval_ctx.integers.extend(
            self.known_int_params
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        let Some(value) =
            rumoca_eval_ast::eval::eval_integer_with_scope(binding, &eval_ctx, &scope)
        else {
            return;
        };
        self.known_int_params
            .insert(data.qualified_name.to_flat_string(), value);
    }

    /// Check if we're inside a flow record.
    fn inherited_flow(&self) -> bool {
        self.scope_frames.iter().rev().any(|frame| frame.flow)
    }

    /// Check if we're inside a stream record.
    fn inherited_stream(&self) -> bool {
        self.scope_frames.iter().rev().any(|frame| frame.stream)
    }

    /// Check if we're inside an expandable connector.
    fn is_in_expandable_connector(&self) -> bool {
        self.scope_frames.iter().any(|frame| frame.expandable)
    }

    /// Check if we're inside a protected component.
    fn is_in_protected(&self) -> bool {
        self.scope_frames.iter().any(|frame| frame.protected)
    }

    /// Get the inherited variability from the stack.
    /// Returns the most restrictive variability (parameter or constant).
    fn inherited_variability(&self) -> Option<&rumoca_core::Variability> {
        self.scope_frames
            .iter()
            .rev()
            .find_map(|frame| frame.variability.as_ref())
    }

    /// Get the inherited causality from the stack.
    /// MLS §4.4.2.2: Record fields inherit input/output causality from parent.
    fn inherited_causality(&self) -> Option<&rumoca_core::Causality> {
        self.scope_frames
            .iter()
            .rev()
            .find_map(|frame| frame.causality.as_ref())
    }

    /// Whether any enclosing component carries annotation(Evaluate = true).
    /// MLS §18.6: evaluating a record-typed parameter during translation
    /// evaluates the whole component, so its members inherit the mark.
    fn inherited_evaluate(&self) -> bool {
        self.scope_frames.iter().any(|frame| frame.evaluate)
    }

    fn structural_evaluation_blocked(&self) -> bool {
        self.scope_frames
            .iter()
            .any(|frame| frame.structural_evaluation_blocked)
    }

    /// Push inherited scope metadata for nested class instantiation.
    /// MLS §4.4.2.1: Record fields inherit variability
    /// MLS §4.4.2.2: Record fields inherit causality
    /// MLS §9.3: Record fields inherit flow/stream
    /// MLS §9.1.3: Track expandable connector membership
    fn push_scope_frame(&mut self, input: ScopeFrameInput<'_>) {
        let current_path = self.current_path();
        self.scope_frames
            .push(ScopeFrame::inherited_from_component(input, &current_path));
    }

    /// Pop inherited scope metadata.
    fn pop_scope_frame(&mut self) {
        debug_assert!(self.scope_frames.len() > 1);
        if self.scope_frames.len() > 1 {
            self.scope_frames.pop();
        }
    }

    /// Record a missing inner declaration (outer without matching inner).
    fn record_missing_inner(&mut self, missing: MissingInnerInfo) {
        let already_recorded = self
            .missing_inners
            .iter()
            .any(|mi| mi.name == missing.name && mi.outer_path == missing.outer_path);
        if !already_recorded {
            self.missing_inners.push(missing);
        }
    }

    /// Check if there are any missing inner declarations.
    fn has_missing_inners(&self) -> bool {
        !self.missing_inners.is_empty()
    }

    /// Get the missing inner declaration info (with type data).
    fn missing_inner_infos(&self) -> &[MissingInnerInfo] {
        &self.missing_inners
    }

    /// Return each unresolved name and its source span once, in discovery order.
    fn unique_missing_inner_summary(&self) -> (Vec<String>, Vec<Span>) {
        let mut names = Vec::new();
        let mut spans = Vec::new();
        for mi in &self.missing_inners {
            if !names.contains(&mi.name) {
                names.push(mi.name.clone());
                spans.push(mi.span);
            }
        }
        (names, spans)
    }

    /// Get the current qualified path.
    pub fn current_path(&self) -> ast::QualifiedName {
        ast::QualifiedName {
            parts: self.context_path.clone(),
        }
    }

    /// Push a name onto the context path.
    pub fn push_path(&mut self, name: &str) {
        self.push_path_part(name, Vec::new());
    }

    /// Push a structured path part onto the context path.
    pub fn push_path_part(&mut self, name: &str, subscripts: Vec<i64>) {
        self.context_path.push((name.to_string(), subscripts));
        self.context_path_def_ids.push(None);
    }

    /// Pop a name from the context path.
    pub fn pop_path(&mut self) {
        self.context_path.pop();
        self.context_path_def_ids.pop();
    }

    fn prove_current_path_identity(&mut self, def_id: rumoca_core::DefId) {
        *self
            .context_path_def_ids
            .last_mut()
            .expect("component instantiation always has a current path segment") = Some(def_id);
    }

    fn current_component_reference(
        &self,
        provenance: rumoca_core::ProvenanceSpan,
    ) -> Result<rumoca_core::ComponentReference, rumoca_core::ComponentReferenceError> {
        let span = provenance.span();
        let parts =
            self.context_path
                .iter()
                .zip(&self.context_path_def_ids)
                .enumerate()
                .map(|(part_index, ((ident, subscripts), def_id))| {
                    let def_id = def_id.ok_or(
                        rumoca_core::ComponentReferenceError::MissingPartIdentity { part_index },
                    )?;
                    Ok(rumoca_core::ComponentRefPart {
                        ident: ident.clone(),
                        span,
                        subs: subscripts
                            .iter()
                            .map(|subscript| {
                                rumoca_core::Subscript::generated_index_with_provenance(
                                    *subscript, provenance,
                                )
                            })
                            .collect(),
                        def_id,
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;
        rumoca_core::ComponentReference::construct(false, span, parts)
    }

    /// Allocate a new unique instance ID.
    pub fn alloc_id(&mut self) -> u32 {
        let id = self.next_instance_id;
        self.next_instance_id += 1;
        id
    }

    /// Get the modification environment.
    pub fn mod_env(&self) -> &ast::ModificationEnvironment {
        &self.mod_env
    }

    /// Get a mutable reference to the modification environment.
    pub fn mod_env_mut(&mut self) -> &mut ast::ModificationEnvironment {
        &mut self.mod_env
    }

    fn active_type_override_map(&self) -> TypeOverrideMap {
        let mut overrides = TypeOverrideMap::new();
        for scoped_overrides in &self.active_type_overrides {
            overrides.extend_from(scoped_overrides);
        }
        overrides
    }

    fn issue_selected_component_types(
        &mut self,
        selections: Arc<SelectedComponentTypes>,
    ) -> InstantiateResult<()> {
        let instance_scope = self.current_path();
        self.selected_component_type_catalog
            .issue(instance_scope, selections)
    }

    fn selected_component_types_for_scope(
        &self,
        source_scope: &ast::QualifiedName,
    ) -> Option<&SelectedComponentTypes> {
        self.selected_component_type_catalog.plan(source_scope)
    }

    fn selected_component_type_catalog(&self) -> &SelectedComponentTypeCatalog {
        &self.selected_component_type_catalog
    }

    fn active_package_constant_aliases(&self) -> Vec<(String, DefId)> {
        self.active_package_constant_aliases.clone()
    }

    /// Push a new inner scope when entering a class/component.
    ///
    /// MLS §5.4: Inner declarations are visible in nested scopes.
    fn push_inner_scope(&mut self) {
        self.inner_scopes.push(IndexMap::default());
    }

    /// Pop the current inner scope when leaving a class/component.
    fn pop_inner_scope(&mut self) {
        self.inner_scopes.pop();
    }

    /// Register an inner declaration in the current scope.
    ///
    /// MLS §5.4: Components declared with `inner` provide instances for `outer` references.
    fn register_inner(
        &mut self,
        name: &str,
        qualified_name: ast::QualifiedName,
        type_name: &str,
        type_def_id: Option<DefId>,
    ) {
        let decl = InnerDeclaration {
            qualified_name,
            type_name: type_name.to_string(),
            type_def_id,
        };
        self.register_inner_decl(name, decl);
    }

    fn register_inner_decl(&mut self, name: &str, decl: InnerDeclaration) {
        if let Some(scope) = self.inner_scopes.last_mut() {
            scope.insert(name.to_string(), decl);
        }
    }

    /// Register a synthetic inner declaration in the root scope (index 0).
    ///
    /// MLS §5.4: Used for synthetic inner synthesis — registers the inner in
    /// the outermost scope so all nested outers can find it.
    fn register_inner_in_root(
        &mut self,
        name: &str,
        qualified_name: ast::QualifiedName,
        type_name: &str,
        type_def_id: Option<DefId>,
    ) {
        if let Some(root_scope) = self.inner_scopes.first_mut() {
            root_scope.insert(
                name.to_string(),
                InnerDeclaration {
                    qualified_name,
                    type_name: type_name.to_string(),
                    type_def_id,
                },
            );
        }
    }

    /// Look up an inner declaration by name, searching all enclosing scopes.
    ///
    /// MLS §5.4: An outer element references the closest inner element with the same name.
    /// Search starts from the innermost scope and works outward.
    fn find_inner(&self, name: &str) -> Option<&InnerDeclaration> {
        // Search from innermost to outermost scope
        for scope in self.inner_scopes.iter().rev() {
            if let Some(inner) = scope.get(name) {
                return Some(inner);
            }
        }
        None
    }

    /// Find an inner declaration, skipping the innermost scope.
    /// Used for `inner outer` components that need to find the PARENT's inner,
    /// not their own inner declaration (which would be self-referential).
    fn find_parent_inner(&self, name: &str) -> Option<&InnerDeclaration> {
        // Skip the innermost scope (index len-1), search from second-innermost
        for scope in self.inner_scopes.iter().rev().skip(1) {
            if let Some(inner) = scope.get(name) {
                return Some(inner);
            }
        }
        None
    }
}

impl Default for InstantiateContext {
    fn default() -> Self {
        Self::new()
    }
}

fn scoped_connection_values<T: Copy>(
    known: &rustc_hash::FxHashMap<String, T>,
    scope: &ast::QualifiedName,
    outer: &rustc_hash::FxHashMap<String, T>,
    local: &rustc_hash::FxHashMap<String, T>,
) -> rustc_hash::FxHashMap<String, T> {
    let prefix = scope.to_flat_string();
    let mut values = rustc_hash::FxHashMap::default();
    for (path, value) in known {
        let visible_path = if prefix.is_empty() {
            Some(path.as_str())
        } else {
            path.strip_prefix(prefix.as_str())
                .and_then(|tail| tail.strip_prefix('.'))
        };
        if let Some(visible_path) = visible_path {
            values.insert(visible_path.to_string(), *value);
        }
    }
    values.extend(outer.iter().map(|(name, value)| (name.clone(), *value)));
    values.extend(local.iter().map(|(name, value)| (name.clone(), *value)));
    values
}

struct LocalStructuralParams {
    bools: rustc_hash::FxHashMap<String, bool>,
    integers: rustc_hash::FxHashMap<String, i64>,
    reals: rustc_hash::FxHashMap<String, f64>,
}

fn extract_local_structural_params(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    ctx: &InstantiateContext,
) -> LocalStructuralParams {
    if ctx.structural_evaluation_blocked() {
        return LocalStructuralParams {
            bools: rustc_hash::FxHashMap::default(),
            integers: rustc_hash::FxHashMap::default(),
            reals: rustc_hash::FxHashMap::default(),
        };
    }
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: ctx.mod_env(),
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let bools = extract_bool_params_with_mods(&eval_ctx);
    let integers = extract_int_params_with_mods(&eval_ctx);
    let reals = extract_real_params_with_mods(&eval_ctx, &rustc_hash::FxHashMap::default());
    LocalStructuralParams {
        bools,
        integers,
        reals,
    }
}

/// Instantiate a class and all its components.
enum ClassOccurrenceConstruction {
    FreshRoot,
    ReservedRoot(rumoca_core::InstanceId),
    Nested {
        owner_component_id: rumoca_core::InstanceId,
        component_type_selections: nested_scope::NestedComponentTypeSelections,
    },
}

fn instantiate_class(
    tree: &ast::ClassTree,
    class_index: &ast::ClassDefIndex<'_>,
    class: &ast::ClassDef,
    occurrence: ClassOccurrenceConstruction,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
) -> InstantiateResult<()> {
    ctx.validate_depth_limit(class, &tree.source_map)?;
    ctx.enter_instantiation_class(class, &tree.source_map)?;
    ctx.push_inner_scope(); // Push a new inner scope for this class (MLS §5.4)
    let result = (|| {
        let (owner_component_id, instance_id, nested_component_selections) = match occurrence {
            ClassOccurrenceConstruction::FreshRoot => (None, overlay.alloc_id(), None),
            ClassOccurrenceConstruction::ReservedRoot(instance_id) => (None, instance_id, None),
            ClassOccurrenceConstruction::Nested {
                owner_component_id,
                component_type_selections,
            } => (
                Some(owner_component_id),
                overlay.alloc_id(),
                Some(component_type_selections),
            ),
        };
        let qualified_name = ctx.current_path();
        // Get or compute the class template (cached to avoid recomputing inheritance)
        // For example, if we have `Resistor r[100]`, we compute the template once and
        // reuse it for all 100 instances, only applying per-instance modifications.
        let template = get_or_compute_template(tree, class, &mut ctx.template_cache)?;
        // Borrow cached template structures directly to avoid per-instance deep clones.
        let effective_components = &template.effective_components;
        let all_equations = &template.effective_equations;
        // MLS §7.3: Build type override map for replaceable type redeclarations.
        // When a record type like ThermodynamicState is redeclared in the enclosing
        // package, components referencing the old type need to use the redeclared version.
        let mut type_overrides = build_type_override_map(tree, class, Some(ctx.mod_env()))?;
        type_overrides.extend_from(&ctx.active_type_override_map());
        let class_overrides = type_overrides.class_overrides(tree, class.location.span())?;

        let local_params = extract_local_structural_params(tree, effective_components, ctx);
        // MLS §5.4: record this scope's booleans so `outer` references from nested
        // classes can be resolved back to the matching `inner` instance.
        ctx.register_known_bool_params(&qualified_name, &local_params.bools);

        // MLS §5.4/§4.5: `inner` elements are visible to the entire class that
        // declares them, independently of where in the class they appear, so make
        // them resolvable before the first component is instantiated.
        preregister_class_inners(tree, class_index, effective_components, ctx)?;

        ctx.register_known_int_params(&qualified_name, &local_params.integers);
        ctx.register_known_real_params(&qualified_name, &local_params.reals);

        // Instantiate each effective component (MLS §4.8 conditional components)
        // Components with conditions are only instantiated if the condition evaluates to true.
        // When a conditional component is disabled, we skip it entirely - its variables and
        // equations should not exist in the flat model.
        // MLS §10.1: Array components of structured types are expanded to indexed instances.
        let active_package_constant_aliases = ctx.active_package_constant_aliases();
        // Constant-alias channels for expression rewriting. Imports are NOT
        // part of these pairs: import rewriting consults the lookup
        // authority's effective bindings instead.
        let overriding_constants =
            resolved_imports_with_active_package_constants(tree, &active_package_constant_aliases);
        let enclosing_constants = resolved_imports_with_enclosing_package_constants(tree, class);
        // MLS §13.2: the class's own import bindings, minted by the one
        // lookup authority for its resolved scope.
        let class_imports = class.scope_id.map(|scope| tree.effective_imports(scope));

        let selected_component_types = instantiate_effective_components(
            tree,
            effective_components,
            &type_overrides,
            instance_id,
            ctx,
            overlay,
            EffectiveComponentConstruction {
                imports: ComponentImports {
                    class_index,
                    overriding_constants: &overriding_constants,
                    enclosing_constants: &enclosing_constants,
                    class_imports: class_imports.as_ref(),
                },
                nested_selections: nested_component_selections,
            },
        )?;

        let connections = extract_class_connections(
            tree,
            effective_components,
            all_equations,
            &qualified_name,
            &local_params,
            ctx,
        )?;

        // MLS §7.3: a reference rooted in a replaceable component (`b.v`) has an
        // instance-dependent member set, so Resolve deferred its tail. The
        // component occurrences of this class instance were just materialized,
        // so their selected types now prove those members exactly.
        let sections = class_instance_sections(
            tree,
            ctx,
            &template,
            &qualified_name,
            &type_overrides,
            &selected_component_types,
        )?;

        let class_data = ast::ClassInstanceData {
            instance_id,
            owner_component_id,
            class_def_id: class.def_id,
            qualified_name: qualified_name.clone(),
            source_scope: class_declaration_source_scope(ctx, class),
            source_scope_id: class.scope_id,
            class_overrides,
            equations: sections.equations,
            initial_equations: sections.initial_equations,
            algorithms: sections.algorithms,
            initial_algorithms: sections.initial_algorithms,
            connections,
        };
        let class_span = location_to_span(
            &class.name.location,
            &tree.source_map,
            "instance class occurrence",
        )?;
        overlay.add_class(class_data).map_err(|reason| {
            Box::new(InstantiateError::invalid_instance_occurrence(
                reason.to_string(),
                class_span,
            ))
        })?;

        Ok(())
    })();

    ctx.pop_inner_scope();
    ctx.exit_instantiation_class(class);

    result
}

fn extract_class_connections(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    equations: &[ast::Equation],
    qualified_name: &ast::QualifiedName,
    local_params: &LocalStructuralParams,
    ctx: &InstantiateContext,
) -> InstantiateResult<Vec<ast::InstanceConnection>> {
    // Nested instances can expose record-field parameters used by this
    // class's structural for/if-equations, so assemble the environment only
    // after component instantiation has completed.
    let outer_ints = ctx.outer_reference_int_values(effective_components);
    let outer_bools = ctx.outer_reference_bool_values(effective_components);
    let outer_reals = ctx.outer_reference_real_values(effective_components);
    let mut integers =
        ctx.merged_int_params_for_connections(qualified_name, &local_params.integers, &outer_ints);
    propagate_record_alias_integer_params(&mut integers, ctx.mod_env());
    propagate_scoped_record_alias_integer_params(&mut integers, ctx.mod_env(), qualified_name);
    let bools =
        ctx.merged_bool_params_for_connections(qualified_name, &local_params.bools, &outer_bools);
    let reals =
        ctx.merged_real_params_for_connections(qualified_name, &local_params.reals, &outer_reals);
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: ctx.mod_env(),
        effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    connections::extract_connections(
        equations,
        qualified_name,
        &connections::ConnectionParams {
            bools,
            integers,
            reals,
            eval_ctx: Some(&eval_ctx),
        },
        &tree.source_map,
    )
}

/// Instance-tree sections converted from one class template.
struct ClassSections {
    equations: Vec<ast::InstanceEquation>,
    initial_equations: Vec<ast::InstanceEquation>,
    algorithms: Vec<Vec<ast::InstanceStatement>>,
    initial_algorithms: Vec<Vec<ast::InstanceStatement>>,
}

/// Convert a class template's equation and algorithm sections to instance form.
fn class_instance_sections(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    template: &templates::ClassTemplate,
    qualified_name: &ast::QualifiedName,
    type_overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
) -> InstantiateResult<ClassSections> {
    let source_map = &tree.source_map;
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: ctx.mod_env(),
        effective_components: &template.effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    // Convert regular equations in one pass without intermediate equation vectors.
    let sections = ClassSections {
        equations: equations_to_instance_without_connections(
            ctx,
            &template.effective_equations,
            qualified_name,
            source_map,
            Some(&eval_ctx),
        )?,
        initial_equations: equations_to_instance_cloned(
            ctx,
            &template.initial_equations,
            qualified_name,
            source_map,
            Some(&eval_ctx),
        )?,
        algorithms: algorithms_to_instance(ctx, &template.algorithms, qualified_name, source_map)?,
        initial_algorithms: algorithms_to_instance(
            ctx,
            &template.initial_algorithms,
            qualified_name,
            source_map,
        )?,
    };
    resolve_dynamic_section_targets(
        tree,
        ctx,
        type_overrides,
        selected_component_types,
        sections,
    )
}

fn resolve_dynamic_section_targets(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    type_overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
    sections: ClassSections,
) -> InstantiateResult<ClassSections> {
    let ClassSections {
        equations,
        initial_equations,
        algorithms,
        initial_algorithms,
    } = sections;
    let resolve_equation = |equation| {
        resolve_dynamic_instance_equation(
            tree,
            ctx,
            type_overrides,
            selected_component_types,
            equation,
        )
    };
    let resolve_algorithm = |algorithm: Vec<ast::InstanceStatement>| {
        algorithm
            .into_iter()
            .map(|statement| {
                resolve_dynamic_instance_statement(
                    tree,
                    ctx,
                    type_overrides,
                    selected_component_types,
                    statement,
                )
            })
            .collect::<InstantiateResult<Vec<_>>>()
    };

    Ok(ClassSections {
        equations: equations
            .into_iter()
            .map(resolve_equation)
            .collect::<InstantiateResult<Vec<_>>>()?,
        initial_equations: initial_equations
            .into_iter()
            .map(resolve_equation)
            .collect::<InstantiateResult<Vec<_>>>()?,
        algorithms: algorithms
            .into_iter()
            .map(resolve_algorithm)
            .collect::<InstantiateResult<Vec<_>>>()?,
        initial_algorithms: initial_algorithms
            .into_iter()
            .map(resolve_algorithm)
            .collect::<InstantiateResult<Vec<_>>>()?,
    })
}

fn resolve_dynamic_instance_equation(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    type_overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
    equation: ast::InstanceEquation,
) -> InstantiateResult<ast::InstanceEquation> {
    let ast::InstanceEquation {
        equation,
        origin,
        source_scope,
        source_scope_id,
        span,
    } = equation;
    Ok(ast::InstanceEquation {
        equation: resolve_dynamic_equation_targets_at_occurrence(
            tree,
            type_overrides,
            ctx.selected_component_type_catalog(),
            &origin,
            selected_component_types,
            equation,
        )?,
        origin,
        source_scope,
        source_scope_id,
        span,
    })
}

fn resolve_dynamic_instance_statement(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    type_overrides: &TypeOverrideMap,
    selected_component_types: &SelectedComponentTypes,
    statement: ast::InstanceStatement,
) -> InstantiateResult<ast::InstanceStatement> {
    let ast::InstanceStatement {
        statement,
        origin,
        source_scope,
        source_scope_id,
        span,
    } = statement;
    Ok(ast::InstanceStatement {
        statement: resolve_dynamic_statement_targets_at_occurrence(
            tree,
            type_overrides,
            ctx.selected_component_type_catalog(),
            &origin,
            selected_component_types,
            statement,
        )?,
        origin,
        source_scope,
        source_scope_id,
        span,
    })
}

struct InstanceDataBuild<'a> {
    instance_id: rumoca_core::InstanceId,
    owner_class_id: Option<rumoca_core::InstanceId>,
    qualified_name: ast::QualifiedName,
    dims: Vec<i64>,
    dims_expr: Vec<rumoca_ir_ast::Subscript>,
    type_name: String,
    type_def_id: Option<DefId>,
    type_reference_root_def_id: Option<DefId>,
    declaration_source_scope: Option<ast::QualifiedName>,
    class_overrides: ast::ClassOverrideMap,
    has_forwarding_class_redeclare: bool,
    effective_variability: rumoca_core::Variability,
    causality: rumoca_core::Causality,
    flow: bool,
    stream: bool,
    attrs: ExtractedAttributes,
    binding: Option<ast::Expression>,
    binding_source: Option<ast::Expression>,
    binding_source_scope: Option<ast::QualifiedName>,
    binding_from_modification: bool,
    type_id: TypeId,
    is_primitive: bool,
    is_discrete_type: bool,
    evaluate: bool,
    source_map: &'a rumoca_core::SourceMap,
    ctx: &'a InstantiateContext,
    comp: &'a ast::Component,
    class_def: Option<&'a ast::ClassDef>,
}

/// True when this declaration carries a redeclare modifier of its own
/// (`Holder h(redeclare C a[2])`, MLS §7.3).
///
/// The parser records one redeclare flag per source modifier, which settles the
/// direct form. The redeclaration may also sit deeper inside an ordinary
/// modifier — `Wrap w(h(redeclare C a[2]))` modifies `w.h` and redeclares
/// `w.h.a` — so the modifier subtrees are searched as well. Only the redeclared
/// type is ever propagated, so either shape leaves everything instantiated
/// beneath this declaration carrying unproven dimensions.
fn declaration_carries_redeclare_modifier(comp: &ast::Component) -> bool {
    comp.source_modification_redeclare_flags
        .iter()
        .any(|is_redeclare| *is_redeclare)
        || comp
            .source_modifications
            .iter()
            .any(traversal_adapter::expression_contains_redeclare)
}

fn build_instance_data(
    args: InstanceDataBuild<'_>,
) -> InstantiateResult<(
    ast::InstanceData,
    Option<ast::Expression>,
    Option<ast::Expression>,
)> {
    let binding_for_record_expansion = args.binding.clone();
    let binding_source_for_record_expansion = args.binding_source.clone();
    let component_span = location_to_span(
        &args.comp.location,
        args.source_map,
        "instance component reference",
    )?;
    let component_ref = args
        .ctx
        .current_component_reference(require_component_ref_provenance(
            component_span,
            "instance component reference",
        )?)
        .map_err(|_| {
            Box::new(InstantiateError::missing_resolved_identity(
                args.qualified_name.to_flat_string(),
                component_span,
            ))
        })?;
    let instance_data = ast::InstanceData {
        instance_id: args.instance_id,
        declaration_def_id: args.comp.def_id,
        owner_class_id: args.owner_class_id,
        component_ref: Some(component_ref),
        qualified_name: args.qualified_name,
        source_location: args.comp.location.clone(),
        dims: args.dims,
        dims_expr: args.dims_expr,
        type_id: args.type_id,
        type_name: args.type_name,
        // Keep partial first-segment anchors (e.g. `Medium` in
        // `Medium.AbsolutePressure`) so instanced typecheck can resolve dotted
        // type names using lexical package anchors.
        type_def_id: args.type_def_id.or(args.comp.type_name.def_id),
        type_reference_root_def_id: args.type_reference_root_def_id,
        declaration_source_scope: args.declaration_source_scope,
        class_overrides: args.class_overrides,
        has_forwarding_class_redeclare: args.has_forwarding_class_redeclare,
        // MLS §7.3 redeclarations reach a component from two directions: an
        // `extends` modification (recorded on the merged declaration by
        // `merge_extends`) or a redeclare modifier on this very declaration
        // (`Holder h(redeclare C a[2])`). Only the redeclared type is consumed
        // either way, so both must be recorded.
        had_redeclare: args.comp.redeclared_by_modification
            || declaration_carries_redeclare_modifier(args.comp),
        // Type prefixes (MLS §4.4.2, SPEC_0022 §3.19-3.20)
        variability: args.effective_variability.clone(),
        causality: args.causality.clone(),
        flow: args.flow,
        stream: args.stream,
        // Attributes
        start: args.attrs.start,
        fixed: args.attrs.fixed,
        min: args.attrs.min,
        max: args.attrs.max,
        nominal: args.attrs.nominal,
        quantity: args.attrs.quantity,
        unit: args.attrs.unit,
        display_unit: args.attrs.display_unit,
        description: description_tokens_to_string(&args.comp.description),
        state_select: args.attrs.state_select,
        binding: args.binding,
        binding_source: args.binding_source,
        binding_source_scope: args.binding_source_scope,
        attribute_source_scopes: args.attrs.source_scopes,
        binding_from_modification: args.binding_from_modification,
        is_primitive: args.is_primitive,
        is_discrete_type: args.is_discrete_type,
        from_expandable_connector: args.ctx.is_in_expandable_connector(),
        evaluate: args.evaluate,
        is_final: args.comp.is_final,
        is_protected: args.comp.is_protected || args.ctx.is_in_protected(),
        is_connector_type: args
            .class_def
            .map(|c| matches!(c.class_type, rumoca_core::ClassType::Connector))
            .unwrap_or(false),
        is_expandable_connector_type: args.class_def.is_some_and(|class| class.expandable),
    };

    Ok((
        instance_data,
        binding_for_record_expansion,
        binding_source_for_record_expansion,
    ))
}

fn require_component_ref_provenance(
    span: rumoca_core::Span,
    context: &'static str,
) -> InstantiateResult<rumoca_core::ProvenanceSpan> {
    span.require_provenance(context).map_err(|err| {
        Box::new(InstantiateError::missing_source_context(err.to_string())) as Box<InstantiateError>
    })
}

fn resolve_component_causality(
    comp: &ast::Component,
    class_def: Option<&ast::ClassDef>,
    inherited_causality: Option<&rumoca_core::Causality>,
) -> rumoca_core::Causality {
    // MLS §4.4.2.2: record fields inherit input/output from the enclosing component.
    // Connector aliases like `RealInput = input Real` also propagate causality.
    if !matches!(comp.causality, rumoca_core::Causality::Empty) {
        return comp.causality.clone();
    }

    inherited_causality.cloned().unwrap_or_else(|| {
        class_def
            .map(|c| c.causality.clone())
            .unwrap_or_else(|| comp.causality.clone())
    })
}

fn resolve_effective_variability(
    comp: &ast::Component,
    inherited_variability: Option<&rumoca_core::Variability>,
) -> rumoca_core::Variability {
    // MLS §4.4.2.1: fields of parameter/constant records inherit variability.
    if matches!(comp.variability, rumoca_core::Variability::Empty) {
        inherited_variability
            .cloned()
            .unwrap_or_else(|| comp.variability.clone())
    } else {
        comp.variability.clone()
    }
}

fn validate_partial_component_instantiation(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    class_def: Option<&ast::ClassDef>,
    qualified_name: &ast::QualifiedName,
    type_name: &str,
    allow_partial_instantiation: bool,
) -> InstantiateResult<()> {
    if allow_partial_instantiation {
        return Ok(());
    }

    let instantiates_partial = class_def.is_some_and(|class| {
        !matches!(
            class.class_type,
            rumoca_core::ClassType::Package | rumoca_core::ClassType::Function
        ) && class.partial
    });
    if !instantiates_partial {
        return Ok(());
    }

    let span = location_to_span(&comp.location, &tree.source_map, "partial component")?;
    Err(Box::new(InstantiateError::partial_class_instantiation(
        qualified_name.to_flat_string(),
        type_name.to_string(),
        span,
    )))
}

#[derive(Clone, Copy)]
struct ComponentInstantiationScope<'a> {
    owner_class_id: Option<rumoca_core::InstanceId>,
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a TypeOverrideMap,
    selected_component_types: &'a SelectedComponentTypes,
    imports: ComponentImports<'a>,
}

#[derive(Clone, Copy)]
struct ComponentSourceSemantics<'a> {
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a TypeOverrideMap,
    selected_component_types: &'a SelectedComponentTypes,
    imports: crate::dims::ImportRewrite<'a>,
}

// SPEC_0021: Exception - component instantiation is the phase entry point that
// coordinates the independently extracted type, binding, shape, and nesting helpers.
// SPEC_0021: Exception - cohesive exhaustive flow stays contiguous so ordering remains auditable.
#[allow(clippy::too_many_lines)]
fn instantiate_component(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    scope: ComponentInstantiationScope<'_>,
) -> InstantiateResult<()> {
    let type_name = comp.type_name.to_string();
    let component_span = location_to_span(
        &comp.location,
        &tree.source_map,
        "resolved component identity",
    )?;
    let component_def_id = comp.def_id.ok_or_else(|| {
        Box::new(InstantiateError::missing_resolved_identity(
            comp.name.as_str(),
            component_span,
        ))
    })?;
    ctx.prove_current_path_identity(component_def_id);
    let instance_id = overlay.alloc_id();
    let qualified_name = ctx.current_path();
    handle_inner_outer(tree, comp, ctx, overlay, &qualified_name, &type_name)?;
    let TypeInfo {
        class_def,
        is_primitive,
        is_discrete: is_discrete_type,
    } = validated_component_type_info(tree, comp, ctx, &qualified_name, &type_name)?;
    // MLS §13.2: declaration-side expressions (shape, binding, attributes)
    // were written in the component's declaring class, so its import bindings
    // govern their qualification.
    let declaring_imports = component_effective_imports(tree, ctx, comp);
    let ComponentBindingInfo {
        mut attrs,
        binding,
        binding_source,
        binding_source_scope,
        binding_from_modification,
        binding_is_each,
    } = prepare_component_binding_info(
        tree,
        comp,
        ctx,
        ComponentSourceSemantics {
            effective_components: scope.effective_components,
            type_overrides: scope.type_overrides,
            selected_component_types: scope.selected_component_types,
            imports: scope.imports.attributes(declaring_imports.as_ref()),
        },
        is_discrete_type,
    )?;
    let (flow, stream) = component_flow_stream(comp, ctx);
    validate_final_type_attribute_overrides(tree, class_def, comp, ctx.mod_env())?;
    merge_type_hierarchy_string_attributes(tree, class_def, &mut attrs);
    let (dims, dims_expr) = resolve_component_shape(
        tree,
        comp,
        ctx,
        class_def,
        ComponentSourceSemantics {
            effective_components: scope.effective_components,
            type_overrides: scope.type_overrides,
            selected_component_types: scope.selected_component_types,
            imports: scope.imports.qualification(declaring_imports.as_ref()),
        },
    )?;
    let type_id = component_type_id(tree, &type_name, class_def, is_primitive)?;
    let declaration_source_scope = component_declaration_source_scope(ctx, comp);
    let binding_scope_for_record_expansion = binding_scope_for_record_expansion(
        &qualified_name,
        binding_from_modification,
        binding_source_scope.as_ref(),
    );
    let causality = resolve_component_causality(comp, class_def, ctx.inherited_causality());
    let inherited_evaluate = ctx.inherited_evaluate();
    let effective_variability = resolve_effective_variability(comp, ctx.inherited_variability());
    let (evaluate_annotation, explicitly_disables_structural_evaluation) = {
        let eval_ctx = InstantiateEvalCtx {
            tree,
            mod_env: ctx.mod_env(),
            effective_components: scope.effective_components,
            resolve_class_components: resolve_effective_components_for_eval,
        };
        (
            component_has_evaluate_annotation(comp, &eval_ctx),
            component_explicitly_disables_structural_evaluation(&comp.name, comp, &eval_ctx),
        )
    };
    let evaluate = evaluate_annotation || inherited_evaluate;
    let occurrence_allows_structural_evaluation = match effective_variability {
        rumoca_core::Variability::Constant(_) => true,
        rumoca_core::Variability::Parameter(_) => !explicitly_disables_structural_evaluation,
        _ => false,
    };
    let (
        class_overrides,
        has_forwarding_class_redeclare,
        nested_type_overrides,
        nested_component_selections,
    ) = resolve_component_nested_type_overrides(
        tree,
        comp,
        class_def,
        ctx.mod_env(),
        scope.type_overrides,
    )?
    .into_parts();

    let (instance_data, binding_for_record_expansion, binding_source_for_record_expansion) =
        build_instance_data(InstanceDataBuild {
            instance_id,
            owner_class_id: scope.owner_class_id,
            qualified_name,
            dims,
            dims_expr,
            type_name: type_name.clone(),
            type_def_id: comp.type_def_id,
            type_reference_root_def_id: (comp.type_name.name.len() > 1)
                .then_some(comp.type_name.def_id)
                .flatten()
                .filter(|root_def_id| Some(*root_def_id) != comp.type_def_id),
            declaration_source_scope: declaration_source_scope.clone(),
            class_overrides: class_overrides.clone(),
            has_forwarding_class_redeclare,
            effective_variability: effective_variability.clone(),
            causality: causality.clone(),
            flow,
            stream,
            attrs,
            binding,
            binding_source,
            binding_source_scope: binding_source_scope.clone(),
            binding_from_modification,
            type_id,
            is_primitive,
            is_discrete_type,
            evaluate,
            source_map: &tree.source_map,
            ctx,
            comp,
            class_def,
        })?;

    if binding_is_each {
        overlay
            .each_modifier_bindings
            .insert(instance_data.qualified_name.to_component_path());
    }
    ctx.register_known_integer_instance(&instance_data, occurrence_allows_structural_evaluation);
    overlay.add_component(instance_data).map_err(|reason| {
        Box::new(InstantiateError::invalid_instance_occurrence(
            reason.to_string(),
            component_span,
        ))
    })?;

    instantiate_nested_component_if_needed(
        tree,
        ctx,
        overlay,
        NestedComponentRequest {
            instance_id,
            comp,
            class_def,
            is_primitive,
            effective_variability: &effective_variability,
            causality: &causality,
            flow,
            stream,
            binding_for_record_expansion: binding_for_record_expansion.as_ref(),
            binding_source_for_record_expansion: binding_source_for_record_expansion.as_ref(),
            binding_scope_for_record_expansion: binding_scope_for_record_expansion.as_ref(),
            binding_is_each,
            effective_components: scope.effective_components,
            type_overrides: &nested_type_overrides,
            component_type_selections: nested_component_selections,
            // Modifications applied to this component were written in the
            // instantiating class, so its own import bindings apply.
            modifier_imports: scope.imports.attributes(None),
        },
    )?;

    Ok(())
}

fn validated_component_type_info<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
    ctx: &InstantiateContext,
    qualified_name: &ast::QualifiedName,
    type_name: &str,
) -> InstantiateResult<TypeInfo<'a>> {
    let type_info = lookup_type_info(tree, comp, type_name)?;
    validate_partial_component_instantiation(
        tree,
        comp,
        type_info.class_def,
        qualified_name,
        type_name,
        ctx.allow_partial_instantiation,
    )?;
    Ok(type_info)
}

fn resolve_component_shape(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &InstantiateContext,
    class_def: Option<&ast::ClassDef>,
    source: ComponentSourceSemantics<'_>,
) -> InstantiateResult<(Vec<i64>, Vec<ast::Subscript>)> {
    let mut component = comp.clone();
    let declaration_occurrence = parent_instance_scope(&ctx.current_path());
    component.shape_expr = resolve_dynamic_subscript_targets_at_occurrence(
        tree,
        source.type_overrides,
        ctx.selected_component_type_catalog(),
        &declaration_occurrence,
        source.selected_component_types,
        component.shape_expr,
    )?;
    let type_dims = resolve_type_alias_dimensions(
        tree,
        class_def,
        ctx.mod_env(),
        source.effective_components,
        source.imports.class_index,
    )?;
    let (dims, dims_expr) = resolve_component_dimensions(
        &component,
        &type_dims,
        ctx.mod_env(),
        source.effective_components,
        tree,
        source.imports,
    )?;
    let dims_expr = resolve_dynamic_subscript_targets_at_occurrence(
        tree,
        source.type_overrides,
        ctx.selected_component_type_catalog(),
        &declaration_occurrence,
        source.selected_component_types,
        dims_expr,
    )?;
    Ok((dims, dims_expr))
}

struct ComponentBindingInfo {
    attrs: ExtractedAttributes,
    binding: Option<ast::Expression>,
    binding_source: Option<ast::Expression>,
    binding_source_scope: Option<ast::QualifiedName>,
    binding_from_modification: bool,
    binding_is_each: bool,
}

fn prepare_component_binding_info(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &mut InstantiateContext,
    source: ComponentSourceSemantics<'_>,
    is_discrete_type: bool,
) -> InstantiateResult<ComponentBindingInfo> {
    let eval_ctx = InstantiateEvalCtx {
        tree,
        mod_env: ctx.mod_env(),
        effective_components: source.effective_components,
        resolve_class_components: resolve_effective_components_for_eval,
    };
    let ComponentAttrsAndBinding {
        mut attrs,
        mut binding,
        mut binding_source,
        binding_source_scope,
        binding_from_modification,
        binding_is_each,
    } = extract_component_attrs_and_binding(comp, ctx.mod_env(), &eval_ctx, source.imports)?;
    infer_local_attribute_source_scopes(ctx, comp, &mut attrs);
    let declaration_occurrence = parent_instance_scope(&ctx.current_path());
    let binding_occurrence = if binding_from_modification {
        binding_source_scope.as_ref().ok_or_else(|| {
            Box::new(InstantiateError::missing_source_context(format!(
                "modifier binding of `{}` has no exact writing occurrence",
                comp.name
            )))
        })?
    } else {
        &declaration_occurrence
    };
    (binding, binding_source) = resolve_component_bindings_from_occurrence(
        tree,
        ctx,
        source,
        &declaration_occurrence,
        binding_occurrence,
        binding,
        binding_source,
    )?;
    let attribute_occurrences = ["start", "min", "max", "nominal"].map(|attribute_name| {
        (
            attribute_name,
            attrs
                .attribute_instance_scopes
                .get(attribute_name)
                .cloned()
                .unwrap_or_else(|| declaration_occurrence.clone()),
        )
    });
    for ((attribute_name, written_occurrence), expression) in
        attribute_occurrences.into_iter().zip([
            &mut attrs.start,
            &mut attrs.min,
            &mut attrs.max,
            &mut attrs.nominal,
        ])
    {
        let selections = selected_types_for_written_occurrence(
            ctx,
            &declaration_occurrence,
            source.selected_component_types,
            &written_occurrence,
            attribute_name,
        )?;
        *expression = resolve_component_expression_with_plan(
            tree,
            ctx,
            source,
            &written_occurrence,
            selections,
            expression.take(),
        )?;
    }
    let start_from_declaration_binding =
        !binding_from_modification && binding.is_some() && attrs.start == binding;
    if !binding_from_modification
        && declaration_binding_allows_structural_resolution(comp, is_discrete_type)
        && let Some(declaration_binding) = binding.as_ref()
    {
        let resolved_binding = mod_env::resolve_declaration_binding_expr(
            declaration_binding,
            ctx.mod_env(),
            source.effective_components,
            tree,
            source.imports.class_index,
        )?;
        if start_from_declaration_binding {
            attrs.start = Some(resolved_binding.clone());
        }
        binding = Some(resolved_binding);
    }
    Ok(ComponentBindingInfo {
        attrs,
        binding,
        binding_source,
        binding_source_scope,
        binding_from_modification,
        binding_is_each,
    })
}

fn resolve_component_bindings_from_occurrence(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    source: ComponentSourceSemantics<'_>,
    declaration_occurrence: &ast::QualifiedName,
    written_occurrence: &ast::QualifiedName,
    binding: Option<ast::Expression>,
    binding_source: Option<ast::Expression>,
) -> InstantiateResult<(Option<ast::Expression>, Option<ast::Expression>)> {
    let selections = selected_types_for_written_occurrence(
        ctx,
        declaration_occurrence,
        source.selected_component_types,
        written_occurrence,
        "binding",
    )?;
    Ok((
        resolve_component_expression_with_plan(
            tree,
            ctx,
            source,
            written_occurrence,
            selections,
            binding,
        )?,
        resolve_component_expression_with_plan(
            tree,
            ctx,
            source,
            written_occurrence,
            selections,
            binding_source,
        )?,
    ))
}

fn selected_types_for_written_occurrence<'a>(
    ctx: &'a InstantiateContext,
    declaration_occurrence: &ast::QualifiedName,
    declaration_selections: &'a SelectedComponentTypes,
    written_occurrence: &ast::QualifiedName,
    surface: &str,
) -> InstantiateResult<&'a SelectedComponentTypes> {
    if written_occurrence == declaration_occurrence {
        return Ok(declaration_selections);
    }
    ctx.selected_component_types_for_scope(written_occurrence)
        .ok_or_else(|| {
            Box::new(InstantiateError::missing_source_context(format!(
                "component {surface} written in `{written_occurrence}` has no selected component-type plan"
            )))
        })
}

fn resolve_component_expression_with_plan(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    source: ComponentSourceSemantics<'_>,
    written_occurrence: &ast::QualifiedName,
    selections: &SelectedComponentTypes,
    expression: Option<ast::Expression>,
) -> InstantiateResult<Option<ast::Expression>> {
    let Some(expression) = expression else {
        return Ok(None);
    };
    resolve_dynamic_expression_targets_at_occurrence(
        tree,
        source.type_overrides,
        ctx.selected_component_type_catalog(),
        written_occurrence,
        selections,
        expression,
    )
    .map(Some)
}

fn declaration_binding_allows_structural_resolution(
    comp: &ast::Component,
    is_discrete_type: bool,
) -> bool {
    matches!(
        comp.variability,
        rumoca_core::Variability::Parameter(_) | rumoca_core::Variability::Constant(_)
    ) || comp.is_structural
        || is_discrete_type
}

struct NestedComponentRequest<'a> {
    instance_id: rumoca_core::InstanceId,
    comp: &'a ast::Component,
    class_def: Option<&'a ast::ClassDef>,
    is_primitive: bool,
    effective_variability: &'a rumoca_core::Variability,
    causality: &'a rumoca_core::Causality,
    flow: bool,
    stream: bool,
    binding_for_record_expansion: Option<&'a ast::Expression>,
    binding_source_for_record_expansion: Option<&'a ast::Expression>,
    binding_scope_for_record_expansion: Option<&'a ast::QualifiedName>,
    binding_is_each: bool,
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a TypeOverrideMap,
    component_type_selections: nested_scope::NestedComponentTypeSelections,
    /// Import aliases of the class that wrote these modifications (MLS §13.2),
    /// used to qualify unqualified names in modifier expressions.
    modifier_imports: crate::dims::ImportRewrite<'a>,
}

fn instantiate_nested_component_if_needed(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    request: NestedComponentRequest<'_>,
) -> InstantiateResult<()> {
    if request.is_primitive || request.comp.outer && !request.comp.inner {
        return request.component_type_selections.finish();
    }
    let Some(nested_class) = request.class_def else {
        return request.component_type_selections.finish();
    };
    instantiate_nested_class(
        tree,
        ctx,
        overlay,
        NestedInstantiationInput {
            instance_id: request.instance_id,
            nested_class,
            comp: request.comp,
            effective_variability: request.effective_variability,
            causality: request.causality,
            flow: request.flow,
            stream: request.stream,
            binding_for_record_expansion: request.binding_for_record_expansion,
            binding_source_for_record_expansion: request.binding_source_for_record_expansion,
            binding_scope_for_record_expansion: request.binding_scope_for_record_expansion,
            binding_is_each: request.binding_is_each,
            effective_components: request.effective_components,
            type_overrides: request.type_overrides,
            component_type_selections: request.component_type_selections,
            modifier_imports: request.modifier_imports,
        },
    )
}

fn binding_scope_for_record_expansion(
    qualified_name: &ast::QualifiedName,
    binding_from_modification: bool,
    binding_source_scope: Option<&ast::QualifiedName>,
) -> Option<ast::QualifiedName> {
    if binding_from_modification {
        return binding_source_scope.cloned();
    }

    Some(parent_instance_scope(qualified_name))
}

fn parent_instance_scope(qualified_name: &ast::QualifiedName) -> ast::QualifiedName {
    if qualified_name.parts.len() <= 1 {
        ast::QualifiedName::new()
    } else {
        ast::QualifiedName {
            parts: qualified_name.parts[..qualified_name.parts.len() - 1].to_vec(),
        }
    }
}

/// Handle nested class instantiation: set up modification environment,
/// push inheritance flags, instantiate the class, and clean up.
fn active_package_constant_alias(
    comp: &ast::Component,
    type_overrides: &TypeOverrideMap,
) -> Option<(String, DefId)> {
    let alias = comp.type_name.name.first()?.text.as_ref();
    let alias_def_id = comp.type_name.def_id?;
    let target_def_id = type_overrides.target_for_alias_def_id(alias_def_id)?;
    Some((alias.to_string(), target_def_id))
}

#[cfg(test)]
mod alias_dims_and_builtin_tests;
#[cfg(test)]
mod conditional_outer_tests;
#[cfg(test)]
mod conditional_scope_tests;
#[cfg(test)]
mod equality_constraint_tests;
#[cfg(test)]
mod test_support;
#[cfg(test)]
mod tests;
