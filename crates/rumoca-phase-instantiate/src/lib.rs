//! Instantiation phase for the Rumoca compiler.
//!
//! This crate implements the instantiation pass that converts a ast::ResolvedTree to an ast::InstancedTree.
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
//! use rumoca_phase_instantiate::instantiate;
//!
//! let resolved: ast::ResolvedTree = resolve(parsed)?;
//! let instanced: ast::InstancedTree = instantiate(resolved, "MyModel")?;
//! ```

mod array_expansion;
mod attributes;
mod connections;
mod dims;
mod errors;
mod evaluate_annotation;
mod inheritance;
mod instance_sections;
mod mod_env;
mod nested_scope;
mod package_constant_imports;
mod source_scope;
mod templates;
mod traversal_adapter;
mod type_lookup;
mod type_overrides;

use rumoca_eval_ast::eval_instantiate::{
    InstantiateEvalCtx, ResolveClassComponents, evaluate_array_dimensions,
    evaluate_component_condition, extract_binding, extract_bool_params_with_mods,
    extract_int_params_with_mods, generate_array_indices, propagate_record_alias_integer_params,
};

use rumoca_core::Diagnostics;
use rumoca_core::{DefId, Span, TypeId, split_path_with_indices};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;
use std::cell::RefCell;
use std::sync::OnceLock;

use array_expansion::{ArrayExpansionScope, expand_array_component};
use attributes::*;
use dims::{
    qualify_shape_subscripts_imports, resolve_component_dimensions, resolve_type_alias_dimensions,
};
use evaluate_annotation::has_evaluate_annotation;
use instance_sections::{
    algorithms_to_instance, equations_to_instance_cloned, equations_to_instance_without_connections,
};
use mod_env::{
    PopulateModEnvInput, populate_modification_environment, propagate_record_binding_to_fields,
};
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
    expression_source_scope, register_zero_sized_array_component,
};
use templates::get_or_compute_template;
#[cfg(test)]
use type_lookup::is_type_compatible;
use type_lookup::{
    TypeInfo, is_type_compatible_with_def_id, lookup_type_info, resolve_primitive_type_id,
};
use type_overrides::{TypeOverrideMap, apply_type_override, build_type_override_map};

pub use connections::{ConnectionParams, extract_connections, filter_out_connections};
pub use errors::{InstantiateError, InstantiateResult, InstantiationOutcome};
pub use inheritance::resolve_effective_components_for_eval;
pub use inheritance::{
    InheritanceCache, InheritedContent, SubtypeCache, class_extends, class_extends_cached,
    find_class_in_tree, get_effective_components, get_effective_components_with_cache,
    get_effective_equations, get_effective_equations_with_cache, is_type_subtype,
    is_type_subtype_cached, location_to_span, process_extends, process_extends_with_cache,
    type_names_match,
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
    pub source_scopes: IndexMap<String, ast::QualifiedName>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct InstantiateOptions {
    pub depth_limit: usize,
}

impl Default for InstantiateOptions {
    fn default() -> Self {
        Self {
            depth_limit: DEFAULT_INSTANTIATION_DEPTH_LIMIT,
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
    causality: Option<rumoca_core::Causality>,
    flow: bool,
    stream: bool,
    expandable: bool,
    overconstrained: Option<(usize, String)>,
    protected: bool,
}

struct ScopeFrameInput<'a> {
    variability: &'a rumoca_core::Variability,
    causality: &'a rumoca_core::Causality,
    flow: bool,
    stream: bool,
    expandable: bool,
    overconstrained_eq_size: Option<usize>,
    protected: bool,
}

impl ScopeFrame {
    fn inherited_from_component(
        input: ScopeFrameInput<'_>,
        context_path: &ast::QualifiedName,
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
        let overconstrained = input
            .overconstrained_eq_size
            .map(|size| (size, context_path.to_flat_string()));

        Self {
            variability,
            causality,
            flow: input.flow,
            stream: input.stream,
            expandable: input.expandable,
            overconstrained,
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
    /// Whether partial class components are allowed in the current instantiation.
    /// This is true when the selected root model is declared partial.
    allow_partial_instantiation: bool,
    /// Instantiation behavior configured by the session or direct phase caller.
    options: InstantiateOptions,
    /// Stable identity stack for detecting recursive class/type instantiation.
    active_instantiations: Vec<InstantiationFrame>,
    /// Source declaration scopes keyed by resolved DefId.
    source_scope_index: SourceScopeIndex,
    /// Active package/type redeclarations inherited from enclosing component scopes.
    active_type_overrides: Vec<TypeOverrideMap>,
    active_package_constant_aliases: Vec<(String, DefId)>,
    /// Diagnostic-only count of entered class instantiations.
    instantiated_class_entries: u64,
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
            location_to_span(&class.name.location, source_map),
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
            next_instance_id: 0,
            mod_env: ast::ModificationEnvironment::new(),
            // Start with one empty scope for the root
            inner_scopes: vec![IndexMap::default()],
            missing_inners: Vec::new(),
            scope_frames: vec![ScopeFrame::default()],
            template_cache: ClassTemplateCache::default(),
            known_int_params: rustc_hash::FxHashMap::default(),
            allow_partial_instantiation: false,
            options,
            active_instantiations: Vec::new(),
            source_scope_index: SourceScopeIndex::default(),
            active_type_overrides: Vec::new(),
            active_package_constant_aliases: Vec::new(),
            instantiated_class_entries: 0,
        }
    }

    fn index_source_scopes(&mut self, tree: &ast::ClassTree) {
        self.source_scope_index = SourceScopeIndex::from_tree(tree);
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
                location_to_span(&class.name.location, source_map),
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

    /// Build a connection integer-parameter map by combining globally known and local values.
    fn merged_int_params_for_connections(
        &self,
        local: &rustc_hash::FxHashMap<String, i64>,
    ) -> rustc_hash::FxHashMap<String, i64> {
        let mut merged = self.known_int_params.clone();
        for (k, v) in local {
            merged.insert(k.clone(), *v);
        }
        merged
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

    /// Check if we're inside an overconstrained connector.
    fn is_in_overconstrained(&self) -> bool {
        self.scope_frames
            .iter()
            .any(|frame| frame.overconstrained.is_some())
    }

    /// Return the equalityConstraint output size from the innermost OC scope.
    fn overconstrained_eq_size(&self) -> Option<usize> {
        self.scope_frames
            .iter()
            .rev()
            .find_map(|frame| frame.overconstrained.as_ref().map(|(n, _)| *n))
    }

    /// Return the OC record path from the innermost OC scope.
    fn overconstrained_record_path(&self) -> Option<String> {
        self.scope_frames
            .iter()
            .rev()
            .find_map(|frame| frame.overconstrained.as_ref().map(|(_, path)| path.clone()))
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

    /// Push inherited scope metadata for nested class instantiation.
    /// MLS §4.4.2.1: Record fields inherit variability
    /// MLS §4.4.2.2: Record fields inherit causality
    /// MLS §9.3: Record fields inherit flow/stream
    /// MLS §9.1.3: Track expandable connector membership
    /// MLS §9.4: Track overconstrained connector scopes
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
    fn record_missing_inner(
        &mut self,
        name: &str,
        type_name: &str,
        type_def_id: Option<DefId>,
        span: Span,
        outer_path: ast::QualifiedName,
        is_inner_outer: bool,
    ) {
        let already_recorded = self
            .missing_inners
            .iter()
            .any(|mi| mi.name == name && mi.outer_path == outer_path);
        if !already_recorded {
            self.missing_inners.push(MissingInnerInfo {
                name: name.to_string(),
                type_name: type_name.to_string(),
                type_def_id,
                span,
                outer_path,
                is_inner_outer,
            });
        }
    }

    /// Check if there are any missing inner declarations.
    pub fn has_missing_inners(&self) -> bool {
        !self.missing_inners.is_empty()
    }

    /// Get the missing inner declaration info (with type data).
    fn missing_inner_infos(&self) -> &[MissingInnerInfo] {
        &self.missing_inners
    }

    /// Get the list of missing inner declaration names (for public API compatibility).
    pub fn missing_inner_names(&self) -> Vec<String> {
        let mut names = Vec::new();
        for mi in &self.missing_inners {
            if !names.contains(&mi.name) {
                names.push(mi.name.clone());
            }
        }
        names
    }

    /// Get missing inner source spans.
    pub fn missing_inner_spans(&self) -> Vec<Span> {
        self.missing_inners.iter().map(|mi| mi.span).collect()
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
    }

    /// Pop a name from the context path.
    pub fn pop_path(&mut self) {
        self.context_path.pop();
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

fn instantiate_progress_interval() -> Option<u64> {
    static INTERVAL: OnceLock<Option<u64>> = OnceLock::new();
    *INTERVAL.get_or_init(|| {
        std::env::var("RUMOCA_INSTANTIATE_PROGRESS_INTERVAL")
            .ok()
            .and_then(|value| value.parse::<u64>().ok())
            .filter(|value| *value > 0)
    })
}

fn maybe_log_instantiation_progress(ctx: &mut InstantiateContext, class: &ast::ClassDef) {
    ctx.instantiated_class_entries += 1;
    let Some(interval) = instantiate_progress_interval() else {
        return;
    };
    if ctx.instantiated_class_entries % interval != 0 {
        return;
    }

    eprintln!(
        "[rumoca-instantiate] classes={} depth={} class={} path={}",
        ctx.instantiated_class_entries,
        ctx.context_path.len(),
        class.name.text,
        ctx.current_path().to_flat_string(),
    );
}

/// Instantiate a ast::ResolvedTree, finding and instantiating the named model.
///
/// This is the main entry point for instantiation.
///
/// # Arguments
///
/// * `resolved` - The resolved class tree (after name resolution, before type checking)
/// * `model_name` - Name of the model to instantiate as root
///
/// # Returns
///
/// An `ast::InstancedTree` with the class tree and instance overlay, or an error.
pub fn instantiate(
    resolved: ast::ResolvedTree,
    model_name: &str,
) -> InstantiateResult<ast::InstancedTree> {
    instantiate_with_options(resolved, model_name, InstantiateOptions::default())
}

/// Instantiate a resolved tree with caller-supplied instantiation options.
pub fn instantiate_with_options(
    resolved: ast::ResolvedTree,
    model_name: &str,
    options: InstantiateOptions,
) -> InstantiateResult<ast::InstancedTree> {
    let tree = resolved.into_inner();
    let overlay = instantiate_model_with_options(&tree, model_name, options)?;
    Ok(ast::InstancedTree::new(tree, overlay))
}

/// Error type for synthetic inner retry attempts.
enum SyntheticInnerError {
    /// Some missing inners could not be resolved (type not found or transitive outers).
    StillMissing { names: Vec<String> },
    /// The retry instantiation itself failed.
    InstantiationFailed,
}

/// Create a minimal synthetic inner `ast::Component` for a missing inner declaration.
///
/// MLS §5.4: When no matching inner is found, the compiler synthesizes a default
/// inner declaration using the type from the outer declaration.
fn create_synthetic_inner_component(
    mi: &MissingInnerInfo,
    class: &ast::ClassDef,
) -> ast::Component {
    ast::Component {
        name: mi.name.clone(),
        type_name: rumoca_ir_ast::Name {
            name: split_path_with_indices(&mi.type_name)
                .into_iter()
                .map(|s| rumoca_core::Token {
                    text: s.to_string().into(),
                    ..rumoca_core::Token::default()
                })
                .collect(),
            def_id: mi.type_def_id,
        },
        type_def_id: mi.type_def_id,
        inner: true,
        // Use the class's own def_id if available
        def_id: class.def_id,
        ..ast::Component::default()
    }
}

fn description_tokens_to_string(tokens: &[rumoca_core::Token]) -> Option<String> {
    if tokens.is_empty() {
        return None;
    }
    Some(tokens.iter().map(|token| token.text.as_ref()).collect())
}

/// Retry instantiation with synthetic inner declarations.
///
/// MLS §5.4: Creates a fresh context with synthetic inners pre-registered at root
/// scope, then re-runs instantiation. The synthetic inners are instantiated first
/// so their sub-components exist in the overlay before the main model references them.
fn retry_with_synthetic_inners(
    tree: &ast::ClassTree,
    model: &ast::ClassDef,
    missing: &[MissingInnerInfo],
    options: InstantiateOptions,
) -> Result<ast::InstanceOverlay, SyntheticInnerError> {
    let mut ctx = InstantiateContext::with_options(options);
    ctx.index_source_scopes(tree);
    let mut overlay = ast::InstanceOverlay::new();
    ctx.set_allow_partial_instantiation(model.partial);
    overlay.is_partial = model.partial;
    overlay.class_type = model.class_type.clone();
    overlay.root_description = description_tokens_to_string(&model.description);

    // For each missing inner, look up the class, register it in root scope,
    // and instantiate its sub-components at root level.
    for mi in missing {
        let inner_class = match find_class_in_tree(tree, &mi.type_name) {
            Some(c) => c,
            None => continue, // Skip if type not found; will remain missing
        };

        let synthetic = create_synthetic_inner_component(mi, inner_class);

        // Build the qualified name for the root-level synthetic inner
        let qn = ast::QualifiedName::from_ident(&mi.name);

        // Register in root scope so outer lookups will find it
        ctx.register_inner_in_root(&mi.name, qn, &mi.type_name, mi.type_def_id);

        // Instantiate the synthetic inner component at root level
        let empty_siblings = IndexMap::default();
        let empty_type_overrides = TypeOverrideMap::new();
        ctx.push_path(&mi.name);
        if instantiate_component(
            tree,
            &synthetic,
            &mut ctx,
            &mut overlay,
            &empty_siblings,
            &empty_type_overrides,
            &[],
            &resolve_effective_components_for_eval,
        )
        .is_err()
        {
            return Err(SyntheticInnerError::InstantiationFailed);
        }
        ctx.pop_path();
    }

    // Re-run the main model instantiation with inners now available
    if instantiate_class(tree, model, &mut ctx, &mut overlay).is_err() {
        return Err(SyntheticInnerError::InstantiationFailed);
    }

    // Check if there are still missing inners (transitive)
    if ctx.has_missing_inners() {
        return Err(SyntheticInnerError::StillMissing {
            names: ctx.missing_inner_names(),
        });
    }

    Ok(overlay)
}

/// Instantiate a model and return structured outcome.
///
/// This function distinguishes between:
/// - `Success`: Model instantiated successfully
/// - `NeedsInner`: Model has outer components without matching inner declarations
/// - `Error`: Actual instantiation error
///
/// MLS §5.4: Models with `outer` components need `inner` declarations from
/// an enclosing scope. These are not failures - they're context-dependent models.
pub fn instantiate_model_with_outcome(
    tree: &ast::ClassTree,
    model_name: &str,
) -> InstantiationOutcome {
    instantiate_model_with_outcome_options(tree, model_name, InstantiateOptions::default())
}

/// Instantiate a model and return structured outcome with caller-supplied options.
pub fn instantiate_model_with_outcome_options(
    tree: &ast::ClassTree,
    model_name: &str,
    options: InstantiateOptions,
) -> InstantiationOutcome {
    let mut ctx = InstantiateContext::with_options(options);
    ctx.index_source_scopes(tree);

    // Find the model to instantiate using qualified name lookup
    let model = match find_class_in_tree(tree, model_name) {
        Some(m) => m,
        None => {
            return InstantiationOutcome::Error(Box::new(InstantiateError::ModelNotFound(
                model_name.to_string(),
            )));
        }
    };

    // Create the instance overlay
    let mut overlay = ast::InstanceOverlay::new();

    // MLS §4.7: Track if the root model is partial (incomplete for standalone use).
    // Partial models may legally contain partial components.
    ctx.set_allow_partial_instantiation(model.partial);
    overlay.is_partial = model.partial;
    overlay.class_type = model.class_type.clone();
    overlay.root_description = description_tokens_to_string(&model.description);

    // Instantiate the root model
    if let Err(e) = instantiate_class(tree, model, &mut ctx, &mut overlay) {
        return InstantiationOutcome::Error(e);
    }

    // Check if there are missing inner declarations
    if ctx.has_missing_inners() {
        // MLS §5.4: Attempt to synthesize default inner declarations and retry.
        let missing = ctx.missing_inner_infos().to_vec();
        match retry_with_synthetic_inners(tree, model, &missing, options) {
            Ok(mut retry_overlay) => {
                retry_overlay.synthesized_inners = missing
                    .iter()
                    .map(|info| info.name.clone())
                    .collect::<std::collections::BTreeSet<_>>()
                    .into_iter()
                    .collect();
                InstantiationOutcome::Success(retry_overlay)
            }
            Err(SyntheticInnerError::StillMissing { names }) => {
                let span_by_name: std::collections::HashMap<_, _> = missing
                    .iter()
                    .map(|info| (info.name.as_str(), info.span))
                    .collect();
                let missing_spans = names
                    .iter()
                    .filter_map(|name| span_by_name.get(name.as_str()).copied())
                    .collect();
                InstantiationOutcome::NeedsInner {
                    missing_inners: names,
                    missing_spans,
                    partial_overlay: overlay,
                }
            }
            Err(SyntheticInnerError::InstantiationFailed) => {
                // Retry failed; fall back to original NeedsInner result.
                InstantiationOutcome::NeedsInner {
                    missing_inners: ctx.missing_inner_names(),
                    missing_spans: ctx.missing_inner_spans(),
                    partial_overlay: overlay,
                }
            }
        }
    } else {
        InstantiationOutcome::Success(overlay)
    }
}

/// Instantiate a model, returning an error if instantiation fails.
///
/// Convenience wrapper that treats missing inner declarations as errors.
/// For more nuanced handling, use [`instantiate_model_with_outcome`].
///
/// # Arguments
///
/// * `tree` - Reference to the class tree
/// * `model_name` - Name of the model to instantiate as root
///
/// # Returns
///
/// An `ast::InstanceOverlay` with the instantiation results, or an error.
pub fn instantiate_model(
    tree: &ast::ClassTree,
    model_name: &str,
) -> InstantiateResult<ast::InstanceOverlay> {
    instantiate_model_with_options(tree, model_name, InstantiateOptions::default())
}

/// Instantiate a model with caller-supplied instantiation options.
pub fn instantiate_model_with_options(
    tree: &ast::ClassTree,
    model_name: &str,
    options: InstantiateOptions,
) -> InstantiateResult<ast::InstanceOverlay> {
    instantiate_model_with_outcome_options(tree, model_name, options).into_result()
}

/// Instantiate a class and all its components.
fn instantiate_class(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
) -> InstantiateResult<()> {
    ctx.validate_depth_limit(class, &tree.source_map)?;
    ctx.enter_instantiation_class(class, &tree.source_map)?;
    maybe_log_instantiation_progress(ctx, class);
    ctx.push_inner_scope(); // Push a new inner scope for this class (MLS §5.4)
    let result = (|| {
        let instance_id = overlay.alloc_id();
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
        let mut type_overrides = build_type_override_map(tree, class, Some(ctx.mod_env()));
        type_overrides.extend_from(&ctx.active_type_override_map());

        // Extract boolean parameter values for conditional connection evaluation
        // This enables proper handling of patterns like:
        // if use_numberPort then connect(numberPort, showNumber); else ... end if;
        // Check both the component definitions and the modification environment
        let bool_params = extract_bool_params_with_mods(effective_components, ctx.mod_env());

        // Extract integer parameter values for for-loop range evaluation
        // This enables proper handling of patterns like:
        // for k in 1:m loop connect(plug_p.pin[k], resistor[k].p); end for;
        let eval_inheritance_cache = RefCell::new(InheritanceCache::default());
        let eval_effective_components_cache =
            RefCell::new(IndexMap::<DefId, IndexMap<String, ast::Component>>::default());
        let resolve_eval_components = |tree: &ast::ClassTree, class: &ast::ClassDef| {
            if let Some(def_id) = class.def_id
                && let Some(cached) = eval_effective_components_cache.borrow().get(&def_id)
            {
                return cached.clone();
            }
            let components = get_effective_components_with_cache(
                tree,
                class,
                &mut eval_inheritance_cache.borrow_mut(),
            )
            .expect("inheritance must be validated before resolving components for eval");
            if let Some(def_id) = class.def_id {
                eval_effective_components_cache
                    .borrow_mut()
                    .insert(def_id, components.clone());
            }
            components
        };
        let eval_ctx = InstantiateEvalCtx {
            tree,
            mod_env: ctx.mod_env(),
            effective_components,
            resolve_class_components: &resolve_eval_components,
        };
        let int_params = extract_int_params_with_mods(&eval_ctx);
        ctx.register_known_int_params(&qualified_name, &int_params);

        // Instantiate each effective component (MLS §4.8 conditional components)
        // Components with conditions are only instantiated if the condition evaluates to true.
        // When a conditional component is disabled, we skip it entirely - its variables and
        // equations should not exist in the flat model.
        // MLS §10.1: Array components of structured types are expanded to indexed instances.
        let active_package_constant_aliases = ctx.active_package_constant_aliases();
        let component_imports = resolved_imports_with_active_package_constants(
            tree,
            &template.resolved_imports,
            &active_package_constant_aliases,
        );
        let resolved_imports =
            resolved_imports_with_enclosing_package_constants(tree, class, &component_imports);

        instantiate_effective_components(
            tree,
            effective_components,
            &type_overrides,
            ctx,
            overlay,
            &component_imports,
            &resolve_eval_components,
        )?;

        // Rebuild merged integer params after nested component instantiation so that
        // record-field integers (e.g., cellData.nRC) are available for top-level
        // for-loop and if-equation connection extraction.
        let mut conn_int_params = ctx.merged_int_params_for_connections(&int_params);
        propagate_record_alias_integer_params(&mut conn_int_params, ctx.mod_env());
        let conn_params = connections::ConnectionParams {
            bools: bool_params,
            integers: conn_int_params,
        };

        // Extract connections from all equations (including conditional connections)
        let source_map = &tree.source_map;
        let connections = connections::extract_connections(
            all_equations,
            &qualified_name,
            &conn_params,
            source_map,
        )?;

        // Convert regular equations in one pass without intermediate equation vectors.
        let instance_equations = equations_to_instance_without_connections(
            ctx,
            all_equations,
            &qualified_name,
            source_map,
        );
        let instance_initial_equations = equations_to_instance_cloned(
            ctx,
            &template.initial_equations,
            &qualified_name,
            source_map,
        );
        let instance_algorithms =
            algorithms_to_instance(ctx, &template.algorithms, &qualified_name, source_map);
        let instance_initial_algorithms = algorithms_to_instance(
            ctx,
            &template.initial_algorithms,
            &qualified_name,
            source_map,
        );

        let class_data = ast::ClassInstanceData {
            instance_id,
            class_def_id: class.def_id,
            qualified_name: qualified_name.clone(),
            source_scope: class_declaration_source_scope(ctx, class),
            source_scope_id: class.scope_id,
            equations: instance_equations,
            initial_equations: instance_initial_equations,
            algorithms: instance_algorithms,
            initial_algorithms: instance_initial_algorithms,
            connections,
            resolved_imports,
        };
        overlay.add_class(class_data);

        Ok(())
    })();

    ctx.pop_inner_scope();
    ctx.exit_instantiation_class(class);

    result
}

fn instantiate_effective_components(
    tree: &ast::ClassTree,
    effective_components: &IndexMap<String, ast::Component>,
    type_overrides: &TypeOverrideMap,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    imports: &[(String, String)],
    resolve_class_components: &ResolveClassComponents<'_>,
) -> InstantiateResult<()> {
    let array_expansion_scope = ArrayExpansionScope {
        tree,
        effective_components,
        type_overrides,
        imports,
        resolve_class_components,
    };

    for (name, comp) in effective_components {
        if mark_disabled_component_if_needed(
            comp,
            name,
            ctx,
            effective_components,
            tree,
            overlay,
            resolve_class_components,
        ) {
            continue;
        }

        // MLS §7.3: Apply type override for replaceable type redeclarations.
        let comp_ref = apply_type_override(tree, comp, type_overrides, Some(ctx.mod_env()));
        let comp = comp_ref.as_ref();
        let type_name = comp.type_name.to_string();

        let qualified_shape_expr = qualify_shape_subscripts_imports(&comp.shape_expr, imports);
        let dims = evaluate_array_dimensions(
            &comp.shape,
            &qualified_shape_expr,
            ctx.mod_env(),
            effective_components,
            tree,
            resolve_class_components,
        );
        if let Some(dims) = dims.as_ref()
            && dims.contains(&0)
        {
            register_zero_sized_array_component(ctx, overlay, name, dims);
            continue;
        }

        let type_info = lookup_type_info(tree, comp, &type_name);
        let should_expand = !type_info.is_primitive && dims.as_ref().is_some_and(|d| !d.is_empty());

        if should_expand {
            expand_array_component(
                &array_expansion_scope,
                name,
                comp,
                dims.as_ref().unwrap(),
                ctx,
                overlay,
            )?;
            continue;
        }

        ctx.push_path(name);
        instantiate_component(
            tree,
            comp,
            ctx,
            overlay,
            effective_components,
            type_overrides,
            imports,
            resolve_class_components,
        )?;
        ctx.pop_path();
    }
    Ok(())
}

fn mark_disabled_component_if_needed(
    comp: &ast::Component,
    name: &str,
    ctx: &mut InstantiateContext,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    overlay: &mut ast::InstanceOverlay,
    resolve_class_components: &ResolveClassComponents<'_>,
) -> bool {
    // MLS §4.8: Conditional components
    // Evaluate the condition and skip components where condition is false.
    // Disabled component paths are recorded in overlay.disabled_components
    // so the flatten phase can filter out connections involving them.
    // If the condition cannot be evaluated here, keep component instantiated.
    let condition_is_false = comp.condition.as_ref().is_some_and(|cond| {
        let eval_ctx = InstantiateEvalCtx {
            tree,
            mod_env: ctx.mod_env(),
            effective_components,
            resolve_class_components,
        };
        evaluate_component_condition(&eval_ctx, cond) == Some(false)
    });
    if !condition_is_false {
        return false;
    }

    ctx.push_path(name);
    overlay
        .disabled_components
        .insert(ctx.current_path().to_component_path());
    ctx.pop_path();
    true
}

/// Instantiate a component.
///
/// Handle inner/outer component declarations (MLS §5.4).
fn handle_inner_outer(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    qualified_name: &ast::QualifiedName,
    type_name: &str,
) -> InstantiateResult<()> {
    let resolved_type_name = resolve_inner_outer_type_name(tree, ctx, comp, type_name);
    let resolved_type_def_id = tree
        .name_map
        .get(&resolved_type_name)
        .copied()
        .or(comp.type_def_id);
    if comp.inner {
        let inner_decl = InnerDeclaration {
            qualified_name: qualified_name.clone(),
            type_name: resolved_type_name.clone(),
            type_def_id: resolved_type_def_id,
        };
        ctx.register_inner(
            &comp.name,
            qualified_name.clone(),
            &resolved_type_name,
            resolved_type_def_id,
        );
        resolve_pending_outer_refs_for_inner(tree, ctx, overlay, &comp.name, &inner_decl);
    }
    if comp.outer {
        let span = location_to_span(&comp.location, &tree.source_map);
        // MLS §5.4: For `inner outer`, find the PARENT's inner (skip self).
        // For pure `outer`, find the nearest inner (may be self if inner outer).
        let inner_result = if comp.inner {
            ctx.find_parent_inner(&comp.name)
        } else {
            ctx.find_inner(&comp.name)
        };
        if let Some(inner_decl) = inner_result {
            let outer_path = qualified_name.to_flat_string();
            let inner_path = inner_decl.qualified_name.to_flat_string();
            // MLS §5.4: Record prefix mapping for flatten-phase redirection.
            // Pure outer → outer_prefix_to_inner (child refs redirected to inner).
            // Inner outer → inner_outer_to_parent_inner (same-level flow bridge).
            let target_map = if comp.inner {
                &mut overlay.inner_outer_to_parent_inner
            } else {
                &mut overlay.outer_prefix_to_inner
            };
            if outer_path != inner_path {
                target_map.insert(outer_path, inner_path);
            }
            let types_compatible = is_type_compatible_with_def_id(
                tree,
                &resolved_type_name,
                resolved_type_def_id,
                &inner_decl.type_name,
                inner_decl.type_def_id,
            );
            if !types_compatible {
                return Err(Box::new(InstantiateError::inner_outer_type_mismatch(
                    &comp.name,
                    &resolved_type_name,
                    &inner_decl.type_name,
                    span,
                )));
            }
        } else {
            ctx.record_missing_inner(
                &comp.name,
                &resolved_type_name,
                resolved_type_def_id,
                span,
                qualified_name.clone(),
                comp.inner,
            );
        }
    }
    Ok(())
}

fn resolve_inner_outer_type_name(
    tree: &ast::ClassTree,
    ctx: &InstantiateContext,
    comp: &ast::Component,
    type_name: &str,
) -> String {
    if tree.name_map.contains_key(type_name) {
        return type_name.to_string();
    }

    if let Some(qualified) = comp
        .type_def_id
        .and_then(|def_id| tree.def_map.get(&def_id))
        && rumoca_core::top_level_last_segment(qualified)
            == rumoca_core::top_level_last_segment(type_name)
    {
        return qualified.clone();
    }

    let Some(source_scope) = component_declaration_source_scope(ctx, comp) else {
        return type_name.to_string();
    };
    resolve_type_name_in_source_scope(tree, type_name, &source_scope)
        .unwrap_or_else(|| type_name.to_string())
}

fn resolve_type_name_in_source_scope(
    tree: &ast::ClassTree,
    type_name: &str,
    source_scope: &ast::QualifiedName,
) -> Option<String> {
    let mut scope = source_scope.to_flat_string();
    loop {
        let candidate = if scope.is_empty() {
            type_name.to_string()
        } else {
            format!("{scope}.{type_name}")
        };
        if tree.name_map.contains_key(&candidate) {
            return Some(candidate);
        }

        let Some(parent_end) = rumoca_core::find_last_top_level_dot(&scope) else {
            break;
        };
        scope.truncate(parent_end);
    }

    None
}

fn resolve_pending_outer_refs_for_inner(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    name: &str,
    inner_decl: &InnerDeclaration,
) {
    let mut remaining = Vec::new();
    for missing in ctx.missing_inners.drain(..) {
        if can_resolve_missing_inner(tree, name, inner_decl, &missing) {
            record_late_inner_outer_mapping(overlay, &missing, inner_decl);
        } else {
            remaining.push(missing);
        }
    }
    ctx.missing_inners = remaining;
}

fn can_resolve_missing_inner(
    tree: &ast::ClassTree,
    name: &str,
    inner_decl: &InnerDeclaration,
    missing: &MissingInnerInfo,
) -> bool {
    missing.name == name
        && is_type_compatible_with_def_id(
            tree,
            &missing.type_name,
            missing.type_def_id,
            &inner_decl.type_name,
            inner_decl.type_def_id,
        )
}

fn record_late_inner_outer_mapping(
    overlay: &mut ast::InstanceOverlay,
    missing: &MissingInnerInfo,
    inner_decl: &InnerDeclaration,
) {
    let outer_path = missing.outer_path.to_flat_string();
    let inner_path = inner_decl.qualified_name.to_flat_string();
    if outer_path == inner_path {
        return;
    }

    if missing.is_inner_outer {
        overlay
            .inner_outer_to_parent_inner
            .insert(outer_path, inner_path);
    } else {
        overlay.outer_prefix_to_inner.insert(outer_path, inner_path);
    }
}

struct InstanceDataBuild<'a> {
    instance_id: ast::InstanceId,
    qualified_name: ast::QualifiedName,
    dims: Vec<i64>,
    dims_expr: Vec<rumoca_ir_ast::Subscript>,
    type_name: String,
    type_def_id: Option<DefId>,
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
    ctx: &'a InstantiateContext,
    comp: &'a ast::Component,
    class_def: Option<&'a ast::ClassDef>,
}

fn build_instance_data(
    args: InstanceDataBuild<'_>,
) -> (ast::InstanceData, Option<ast::Expression>) {
    let binding_for_record_expansion = args.binding.clone();
    let component_ref = ast::instance::component_reference_for_instance(
        &args.qualified_name,
        rumoca_core::Span::DUMMY,
        args.comp.def_id,
    );
    let instance_data = ast::InstanceData {
        instance_id: args.instance_id,
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
        declaration_source_scope: args.declaration_source_scope,
        class_overrides: args.class_overrides,
        has_forwarding_class_redeclare: args.has_forwarding_class_redeclare,
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
        is_overconstrained: args.ctx.is_in_overconstrained(),
        is_protected: args.comp.is_protected || args.ctx.is_in_protected(),
        is_connector_type: args
            .class_def
            .map(|c| matches!(c.class_type, rumoca_core::ClassType::Connector))
            .unwrap_or(false),
        oc_record_path: if args.ctx.is_in_overconstrained() {
            args.ctx.overconstrained_record_path()
        } else {
            None
        },
        oc_eq_constraint_size: args.ctx.overconstrained_eq_size(),
    };

    (instance_data, binding_for_record_expansion)
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

    let span = location_to_span(&comp.location, &tree.source_map);
    Err(Box::new(InstantiateError::partial_class_instantiation(
        qualified_name.to_flat_string(),
        type_name.to_string(),
        span,
    )))
}

///
/// Note (MLS §4.8): Conditional components are handled in `instantiate_class`.
/// Components whose condition evaluates to false are skipped and recorded
/// in `overlay.disabled_components`. The flatten phase filters out connections
/// and equations involving disabled components.
///
/// MLS §10.1: Array components of structured types (connectors, models) are expanded
/// to indexed instances. For example, `Resistor r[3]` becomes `r[1]`, `r[2]`, `r[3]`.
fn instantiate_component(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    effective_components: &IndexMap<String, ast::Component>,
    type_overrides: &TypeOverrideMap,
    imports: &[(String, String)],
    resolve_class_components: &ResolveClassComponents<'_>,
) -> InstantiateResult<()> {
    let type_name = comp.type_name.to_string();

    let instance_id = overlay.alloc_id();
    let qualified_name = ctx.current_path();

    handle_inner_outer(tree, comp, ctx, overlay, &qualified_name, &type_name)?;

    let TypeInfo {
        class_def,
        is_primitive,
        is_discrete: is_discrete_type,
    } = validated_component_type_info(tree, comp, ctx, &qualified_name, &type_name)?;

    let ComponentBindingInfo {
        mut attrs,
        binding,
        binding_source,
        binding_source_scope,
        binding_from_modification,
    } = prepare_component_binding_info(tree, comp, ctx, effective_components, is_discrete_type)?;

    // Extract flow/stream from connection prefix (MLS §9.3)
    // Also inherit from parent for record fields (e.g., `flow Complex i` → i.re and i.im are flow)
    let (flow, stream) = match &comp.connection {
        rumoca_ir_ast::Connection::Flow(_) => (true, false),
        rumoca_ir_ast::Connection::Stream(_) => (false, true),
        rumoca_ir_ast::Connection::Empty => (ctx.inherited_flow(), ctx.inherited_stream()),
    };

    validate_final_type_attribute_overrides(tree, class_def, comp, ctx.mod_env())?;
    merge_type_hierarchy_string_attributes(tree, class_def, &mut attrs);

    let (dims, dims_expr) = resolve_component_shape(
        tree,
        comp,
        ctx,
        class_def,
        effective_components,
        imports,
        resolve_class_components,
    );

    let type_id = if is_primitive {
        resolve_primitive_type_id(tree, &type_name, class_def)
    } else {
        TypeId::UNKNOWN
    };
    let declaration_source_scope = component_declaration_source_scope(ctx, comp);
    let binding_scope_for_record_expansion = binding_scope_for_record_expansion(
        &qualified_name,
        binding_from_modification,
        binding_source_scope.as_ref(),
    );

    let causality = resolve_component_causality(comp, class_def, ctx.inherited_causality());

    let evaluate = has_evaluate_annotation(comp);

    let effective_variability = resolve_effective_variability(comp, ctx.inherited_variability());

    let (class_overrides, has_forwarding_class_redeclare, nested_type_overrides) =
        resolve_component_nested_type_overrides(
            tree,
            comp,
            class_def,
            ctx.mod_env(),
            type_overrides,
        )?;

    let (instance_data, binding_for_record_expansion) = build_instance_data(InstanceDataBuild {
        instance_id,
        qualified_name,
        dims,
        dims_expr,
        type_name: type_name.clone(),
        type_def_id: comp.type_def_id,
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
        ctx,
        comp,
        class_def,
    });

    overlay.add_component(instance_data);

    instantiate_nested_component_if_needed(
        tree,
        ctx,
        overlay,
        NestedComponentRequest {
            comp,
            class_def,
            is_primitive,
            effective_variability: &effective_variability,
            causality: &causality,
            flow,
            stream,
            binding_for_record_expansion: binding_for_record_expansion.as_ref(),
            binding_scope_for_record_expansion: binding_scope_for_record_expansion.as_ref(),
            effective_components,
            type_overrides: &nested_type_overrides,
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
    let type_info = lookup_type_info(tree, comp, type_name);
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
    effective_components: &IndexMap<String, ast::Component>,
    imports: &[(String, String)],
    resolve_class_components: &ResolveClassComponents<'_>,
) -> (Vec<i64>, Vec<ast::Subscript>) {
    let type_dims = resolve_type_alias_dimensions(
        tree,
        class_def,
        ctx.mod_env(),
        effective_components,
        resolve_class_components,
    );
    resolve_component_dimensions(
        comp,
        &type_dims,
        ctx.mod_env(),
        effective_components,
        tree,
        imports,
        resolve_class_components,
    )
}

struct ComponentBindingInfo {
    attrs: ExtractedAttributes,
    binding: Option<ast::Expression>,
    binding_source: Option<ast::Expression>,
    binding_source_scope: Option<ast::QualifiedName>,
    binding_from_modification: bool,
}

fn prepare_component_binding_info(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    ctx: &mut InstantiateContext,
    effective_components: &IndexMap<String, ast::Component>,
    is_discrete_type: bool,
) -> InstantiateResult<ComponentBindingInfo> {
    let (mut attrs, mut binding, binding_source, binding_source_scope, binding_from_modification) =
        extract_component_attrs_and_binding(comp, ctx.mod_env());
    infer_local_attribute_source_scopes(ctx, comp, &mut attrs);
    let start_from_declaration_binding =
        !binding_from_modification && binding.is_some() && attrs.start == binding;
    if !binding_from_modification
        && declaration_binding_allows_structural_resolution(comp, is_discrete_type)
        && let Some(declaration_binding) = binding.as_ref()
    {
        let resolved_binding = mod_env::resolve_declaration_binding_expr(
            declaration_binding,
            ctx.mod_env(),
            effective_components,
            tree,
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
    })
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
    comp: &'a ast::Component,
    class_def: Option<&'a ast::ClassDef>,
    is_primitive: bool,
    effective_variability: &'a rumoca_core::Variability,
    causality: &'a rumoca_core::Causality,
    flow: bool,
    stream: bool,
    binding_for_record_expansion: Option<&'a ast::Expression>,
    binding_scope_for_record_expansion: Option<&'a ast::QualifiedName>,
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a TypeOverrideMap,
}

fn instantiate_nested_component_if_needed(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    request: NestedComponentRequest<'_>,
) -> InstantiateResult<()> {
    if request.is_primitive || request.comp.outer && !request.comp.inner {
        return Ok(());
    }
    let Some(nested_class) = request.class_def else {
        return Ok(());
    };
    instantiate_nested_class(
        tree,
        ctx,
        overlay,
        NestedInstantiationInput {
            nested_class,
            comp: request.comp,
            effective_variability: request.effective_variability,
            causality: request.causality,
            flow: request.flow,
            stream: request.stream,
            binding_for_record_expansion: request.binding_for_record_expansion,
            binding_scope_for_record_expansion: request.binding_scope_for_record_expansion,
            effective_components: request.effective_components,
            type_overrides: request.type_overrides,
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
struct NestedInstantiationInput<'a> {
    nested_class: &'a ast::ClassDef,
    comp: &'a ast::Component,
    effective_variability: &'a rumoca_core::Variability,
    causality: &'a rumoca_core::Causality,
    flow: bool,
    stream: bool,
    binding_for_record_expansion: Option<&'a ast::Expression>,
    binding_scope_for_record_expansion: Option<&'a ast::QualifiedName>,
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a TypeOverrideMap,
}

fn instantiate_nested_class(
    tree: &ast::ClassTree,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
    input: NestedInstantiationInput<'_>,
) -> InstantiateResult<()> {
    let NestedInstantiationInput {
        nested_class,
        comp,
        effective_variability,
        causality,
        flow,
        stream,
        binding_for_record_expansion,
        binding_scope_for_record_expansion,
        effective_components,
        type_overrides,
    } = input;

    // Snapshot mod_env before modifications so we can restore it after.
    // MLS §7.2: Modifications added for this nested component (via shift_modifications_down,
    // populate_modification_environment, and propagate_record_binding_to_fields) are scoped
    // to this component's instantiation. Parent-scope entries with names that coincidentally
    // match nested class component names must NOT leak through (e.g., parent has parameter `T`
    // and nested HeatPort connector also has field `T`).
    let mod_env_snapshot = ctx.mod_env().active.clone();
    let shifted_parent_keys = collect_shifted_parent_mod_keys(comp, &mod_env_snapshot);
    let targeted_keys = collect_targeted_mod_keys(comp, &mod_env_snapshot);

    // Step 1: Shift existing modifications that target this component's children.
    // These are resolved with the parent scope's mod_env available, then collected.
    shift_modifications_down(ctx, &comp.name);

    // Step 2: Add this component's own modifications to mod_env.
    // Resolution of modification values (e.g., resolving `n` in `sub(n=n)`) uses
    // the parent scope's mod_env, which is still fully available at this point.
    populate_modification_environment(
        ctx,
        tree,
        PopulateModEnvInput {
            comp,
            effective_components,
            type_overrides,
            target_class: Some(nested_class),
            parent_snapshot: &mod_env_snapshot,
            shifted_parent_keys: &shifted_parent_keys,
        },
    )?;

    // Step 2.5: Propagate record bindings to field bindings (MLS §7.2)
    if let Some(binding_expr) = binding_for_record_expansion {
        propagate_record_binding_to_fields(
            tree,
            ctx,
            binding_expr,
            binding_scope_for_record_expansion.cloned(),
            nested_class,
            &targeted_keys,
        );
    }

    // Step 2.6: Scope the mod_env to only contain entries relevant to this nested class.
    // After steps 1-2.5, the mod_env contains both parent-scope entries and newly added
    // entries (shifted, populated, record-propagated). We remove parent-scope entries that
    // were NOT explicitly targeted at this component. This prevents name collisions where
    // a parent parameter (e.g., `T`) leaks into a nested class that has a component with
    // the same name (e.g., HeatPort's `T` field). See MLS §7.2.
    let referenced_mod_roots = collect_referenced_mod_roots(comp);
    ctx.mod_env_mut().active.retain(|key, _| {
        // Keep entries not in the snapshot (they were newly added)
        !mod_env_snapshot.contains_key(key)
        // Keep entries that were explicitly targeted at this component
        || targeted_keys.contains_key(key)
        // Keep parent keys referenced by this component's modifier RHS expressions.
        || key_matches_referenced_root(key, &referenced_mod_roots)
    });

    let eq_size = inheritance::equality_constraint_output_size(nested_class);

    // Step 3: Push inherited scope metadata and instantiate nested class.
    ctx.push_scope_frame(ScopeFrameInput {
        variability: effective_variability,
        causality,
        flow,
        stream,
        expandable: nested_class.expandable,
        overconstrained_eq_size: eq_size,
        protected: comp.is_protected,
    });

    let active_package_alias = active_package_constant_alias(comp, type_overrides);
    if let Some(alias) = active_package_alias.as_ref() {
        ctx.active_package_constant_aliases.push(alias.clone());
    }
    ctx.active_type_overrides.push(type_overrides.clone());
    let result = instantiate_class(tree, nested_class, ctx, overlay);
    ctx.active_type_overrides.pop();
    if active_package_alias.is_some() {
        ctx.active_package_constant_aliases.pop();
    }
    ctx.pop_scope_frame();

    // Restore mod_env to pre-modification state, preserving outer scope modifications
    ctx.mod_env_mut().active = mod_env_snapshot;
    result?;

    Ok(())
}

fn active_package_constant_alias(
    comp: &ast::Component,
    type_overrides: &TypeOverrideMap,
) -> Option<(String, DefId)> {
    let alias = comp.type_name.name.first()?.text.as_ref();
    let target_def_id = type_overrides.target_for_alias_name(alias)?;
    Some((alias.to_string(), target_def_id))
}

#[cfg(test)]
mod tests;
