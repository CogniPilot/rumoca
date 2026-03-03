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
mod connections;
mod dims;
mod errors;
mod evaluator;
mod inheritance;
mod mod_env;
mod nested_scope;
mod templates;
mod type_overrides;

use evaluator::{
    evaluate_array_dimensions, evaluate_component_condition, expr_to_bool, expr_to_string,
    extract_binding, extract_bool_params_with_mods, extract_int_params_with_mods,
    generate_array_indices, parse_state_select, propagate_record_alias_integer_params,
    try_eval_integer_expr,
};

use indexmap::IndexMap;
use rumoca_core::{DefId, Diagnostics, Span, TypeId};
use rumoca_ir_ast as ast;

use array_expansion::{ArrayExpansionScope, expand_array_component};
use dims::{resolve_component_dimensions, resolve_type_alias_dimensions};
use inheritance::option_location_to_span;
use mod_env::{
    PopulateModEnvInput, populate_modification_environment, propagate_record_binding_to_fields,
};
use nested_scope::{
    collect_referenced_mod_roots, collect_shifted_parent_mod_keys, collect_targeted_mod_keys,
    key_matches_referenced_root, resolve_component_nested_type_overrides, shift_modifications_down,
};
use templates::get_or_compute_template;
use type_overrides::{
    apply_type_override, build_type_override_map, resolve_redeclare_value_def_id,
};

pub use connections::{ConnectionParams, extract_connections, filter_out_connections};
pub use errors::{InstantiateError, InstantiateResult, InstantiationOutcome};
pub use inheritance::{
    InheritanceCache, SubtypeCache, class_extends, class_extends_cached, find_class_in_tree,
    get_effective_components, get_effective_components_with_cache, get_effective_equations,
    get_effective_equations_with_cache, is_type_subtype, is_type_subtype_cached, location_to_span,
    process_extends, process_extends_with_cache, type_names_match,
};
pub use templates::{ClassTemplate, ClassTemplateCache};

/// Extracted attribute values from a component's modifications.
#[derive(Debug, Clone, Default)]
pub struct ExtractedAttributes {
    pub start: Option<ast::Expression>,
    pub fixed: Option<bool>,
    pub min: Option<ast::Expression>,
    pub max: Option<ast::Expression>,
    pub nominal: Option<ast::Expression>,
    pub quantity: Option<String>,
    pub unit: Option<String>,
    pub display_unit: Option<String>,
    pub state_select: rumoca_ir_ast::StateSelect,
}

/// Information about a missing inner declaration, collected during instantiation.
/// Used to synthesize default inner declarations for retry (MLS §5.4).
#[derive(Debug, Clone)]
struct MissingInnerInfo {
    name: String,
    type_name: String,
    type_def_id: Option<DefId>,
    span: Span,
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

/// Maximum path depth to prevent stack overflow from circular type references.
/// Conservative limit since each level creates multiple Rust stack frames.
const MAX_INSTANTIATION_DEPTH: usize = 30;

/// Flags indicating which inheritance stacks were pushed during nested instantiation.
/// Used to properly pop only the stacks that were pushed.
struct InheritanceFlags {
    variability: bool,
    causality: bool,
    connection: bool,
}

/// Context for instantiation.
pub struct InstantiateContext {
    /// Diagnostics collector.
    pub diags: Diagnostics,
    /// Current context path during instantiation.
    context_path: Vec<String>,
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
    /// Stack of inherited variabilities from parent components.
    /// When a record is declared as `parameter Record r`, all nested fields
    /// inherit the `parameter` variability (MLS §4.4.2.1).
    variability_stack: Vec<rumoca_ir_ast::Variability>,
    /// Stack of inherited causalities from parent components.
    /// When a record is declared as `input Record r` or `output Record r`,
    /// all nested fields inherit the causality (MLS §4.4.2.2).
    causality_stack: Vec<rumoca_ir_ast::Causality>,
    /// Stack of inherited flow prefixes from parent components.
    /// When a record is declared as `flow Record r`, all nested fields
    /// inherit the `flow` prefix (MLS §9.3).
    flow_stack: Vec<bool>,
    /// Stack of inherited stream prefixes from parent components.
    /// When a record is declared as `stream Record r`, all nested fields
    /// inherit the `stream` prefix (MLS §15).
    stream_stack: Vec<bool>,
    /// Stack of expandable connector flags.
    /// When inside an expandable connector (MLS §9.1.3), nested fields
    /// are marked as from_expandable_connector.
    expandable_stack: Vec<bool>,
    /// Stack of overconstrained connector info (MLS §9.4).
    /// When inside a connector that defines `equalityConstraint`, nested fields
    /// are marked as overconstrained. Entries are `(eq_size, record_path)` pairs:
    /// `Some((n, path))` = OC with n constraint scalars and the record path,
    /// `None` = not OC.
    overconstrained_stack: Vec<Option<(usize, String)>>,
    /// Stack of protected visibility flags (MLS §4.7).
    /// When inside a protected component, nested fields inherit protected status.
    /// Protected connector flows should not count as interface flows.
    protected_stack: Vec<bool>,
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
}

impl InstantiateContext {
    /// Check if instantiation depth is too deep (prevents stack overflow).
    fn is_depth_exceeded(&self) -> bool {
        self.context_path.len() > MAX_INSTANTIATION_DEPTH
    }

    /// Create a new instantiate context.
    pub fn new() -> Self {
        Self {
            diags: Diagnostics::new(),
            context_path: Vec::new(),
            next_instance_id: 0,
            mod_env: ast::ModificationEnvironment::new(),
            // Start with one empty scope for the root
            inner_scopes: vec![IndexMap::new()],
            missing_inners: Vec::new(),
            variability_stack: Vec::new(),
            causality_stack: Vec::new(),
            flow_stack: Vec::new(),
            stream_stack: Vec::new(),
            expandable_stack: Vec::new(),
            overconstrained_stack: Vec::new(),
            protected_stack: Vec::new(),
            template_cache: ClassTemplateCache::new(),
            known_int_params: rustc_hash::FxHashMap::default(),
            allow_partial_instantiation: false,
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

    /// Push flow/stream prefixes onto the stack (for nested record fields).
    /// MLS §9.3: Fields of a flow/stream record inherit the prefix.
    fn push_connection_prefixes(&mut self, flow: bool, stream: bool) {
        self.flow_stack.push(flow);
        self.stream_stack.push(stream);
    }

    /// Pop the flow/stream stack.
    fn pop_connection_prefixes(&mut self) {
        self.flow_stack.pop();
        self.stream_stack.pop();
    }

    /// Check if we're inside a flow record.
    fn inherited_flow(&self) -> bool {
        self.flow_stack.last().copied().unwrap_or(false)
    }

    /// Check if we're inside a stream record.
    fn inherited_stream(&self) -> bool {
        self.stream_stack.last().copied().unwrap_or(false)
    }

    /// Push expandable connector flag onto the stack.
    /// MLS §9.1.3: Members of expandable connectors are tracked.
    fn push_expandable(&mut self, expandable: bool) {
        self.expandable_stack.push(expandable);
    }

    /// Pop the expandable stack.
    fn pop_expandable(&mut self) {
        self.expandable_stack.pop();
    }

    /// Check if we're inside an expandable connector.
    fn is_in_expandable_connector(&self) -> bool {
        self.expandable_stack.iter().any(|&e| e)
    }

    /// Push overconstrained info onto the stack (MLS §9.4).
    /// `eq_size`: `Some(n)` if this class has equalityConstraint with output size n, else `None`.
    /// Captures the current context_path as the OC record path when entering an OC class.
    fn push_overconstrained(&mut self, eq_size: Option<usize>) {
        self.overconstrained_stack
            .push(eq_size.map(|n| (n, self.context_path.join("."))));
    }

    /// Pop the overconstrained stack.
    fn pop_overconstrained(&mut self) {
        self.overconstrained_stack.pop();
    }

    /// Check if we're inside an overconstrained connector.
    fn is_in_overconstrained(&self) -> bool {
        self.overconstrained_stack.iter().any(|oc| oc.is_some())
    }

    /// Return the equalityConstraint output size from the innermost OC scope.
    fn overconstrained_eq_size(&self) -> Option<usize> {
        self.overconstrained_stack
            .iter()
            .rev()
            .find_map(|oc| oc.as_ref().map(|(n, _)| *n))
    }

    /// Return the OC record path from the innermost OC scope.
    fn overconstrained_record_path(&self) -> Option<String> {
        self.overconstrained_stack
            .iter()
            .rev()
            .find_map(|oc| oc.as_ref().map(|(_, path)| path.clone()))
    }

    /// Push protected visibility flag onto the stack (MLS §4.7).
    fn push_protected(&mut self, is_protected: bool) {
        self.protected_stack.push(is_protected);
    }

    /// Pop the protected stack.
    fn pop_protected(&mut self) {
        self.protected_stack.pop();
    }

    /// Check if we're inside a protected component.
    fn is_in_protected(&self) -> bool {
        self.protected_stack.iter().any(|&p| p)
    }

    /// Push a variability onto the stack (for nested record fields).
    /// MLS §4.4.2.1: Fields of a parameter/constant record inherit variability.
    fn push_variability(&mut self, v: rumoca_ir_ast::Variability) {
        self.variability_stack.push(v);
    }

    /// Pop the variability stack.
    fn pop_variability(&mut self) {
        self.variability_stack.pop();
    }

    /// Get the inherited variability from the stack.
    /// Returns the most restrictive variability (parameter or constant).
    fn inherited_variability(&self) -> Option<&rumoca_ir_ast::Variability> {
        self.variability_stack.last()
    }

    /// Push a causality onto the stack (for nested record fields).
    /// MLS §4.4.2.2: Fields of an input/output record inherit causality.
    fn push_causality(&mut self, c: rumoca_ir_ast::Causality) {
        self.causality_stack.push(c);
    }

    /// Pop the causality stack.
    fn pop_causality(&mut self) {
        self.causality_stack.pop();
    }

    /// Get the inherited causality from the stack.
    /// MLS §4.4.2.2: Record fields inherit input/output causality from parent.
    fn inherited_causality(&self) -> Option<&rumoca_ir_ast::Causality> {
        self.causality_stack.last()
    }

    /// Push all inheritance flags for nested class instantiation.
    /// Returns flags indicating which stacks were pushed (for later popping).
    /// MLS §4.4.2.1: Record fields inherit variability
    /// MLS §4.4.2.2: Record fields inherit causality
    /// MLS §9.3: Record fields inherit flow/stream
    /// MLS §9.1.3: Track expandable connector membership
    fn push_inheritance(
        &mut self,
        variability: &rumoca_ir_ast::Variability,
        causality: &rumoca_ir_ast::Causality,
        flow: bool,
        stream: bool,
        expandable: bool,
    ) -> InheritanceFlags {
        let push_variability = matches!(
            variability,
            rumoca_ir_ast::Variability::Parameter(_) | rumoca_ir_ast::Variability::Constant(_)
        );
        let push_causality = matches!(
            causality,
            rumoca_ir_ast::Causality::Input(_) | rumoca_ir_ast::Causality::Output(_)
        );
        let push_connection = flow || stream;

        if push_variability {
            self.push_variability(variability.clone());
        }
        if push_causality {
            self.push_causality(causality.clone());
        }
        if push_connection {
            self.push_connection_prefixes(flow, stream);
        }
        self.push_expandable(expandable);

        InheritanceFlags {
            variability: push_variability,
            causality: push_causality,
            connection: push_connection,
        }
    }

    /// Pop inheritance flags based on what was pushed.
    fn pop_inheritance(&mut self, flags: InheritanceFlags) {
        self.pop_expandable();
        if flags.connection {
            self.pop_connection_prefixes();
        }
        if flags.causality {
            self.pop_causality();
        }
        if flags.variability {
            self.pop_variability();
        }
    }

    /// Record a missing inner declaration (outer without matching inner).
    fn record_missing_inner(
        &mut self,
        name: &str,
        type_name: &str,
        type_def_id: Option<DefId>,
        span: Span,
    ) {
        if !self.missing_inners.iter().any(|mi| mi.name == name) {
            self.missing_inners.push(MissingInnerInfo {
                name: name.to_string(),
                type_name: type_name.to_string(),
                type_def_id,
                span,
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
        self.missing_inners
            .iter()
            .map(|mi| mi.name.clone())
            .collect()
    }

    /// Get missing inner source spans.
    pub fn missing_inner_spans(&self) -> Vec<Span> {
        self.missing_inners.iter().map(|mi| mi.span).collect()
    }

    /// Get the current qualified path.
    pub fn current_path(&self) -> ast::QualifiedName {
        let mut qn = ast::QualifiedName::new();
        for part in &self.context_path {
            qn.push(part.clone(), Vec::new());
        }
        qn
    }

    /// Push a name onto the context path.
    pub fn push_path(&mut self, name: &str) {
        self.context_path.push(name.to_string());
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

    /// Push a new inner scope when entering a class/component.
    ///
    /// MLS §5.4: Inner declarations are visible in nested scopes.
    fn push_inner_scope(&mut self) {
        self.inner_scopes.push(IndexMap::new());
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
        if let Some(scope) = self.inner_scopes.last_mut() {
            scope.insert(
                name.to_string(),
                InnerDeclaration {
                    qualified_name,
                    type_name: type_name.to_string(),
                    type_def_id,
                },
            );
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
    let tree = resolved.into_inner();
    let overlay = instantiate_model(&tree, model_name)?;
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
            name: mi
                .type_name
                .split('.')
                .map(|s| ast::Token {
                    text: s.to_string().into(),
                    ..ast::Token::default()
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

fn description_tokens_to_string(tokens: &[ast::Token]) -> Option<String> {
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
) -> Result<ast::InstanceOverlay, SyntheticInnerError> {
    let mut ctx = InstantiateContext::new();
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
        let empty_siblings = IndexMap::new();
        let empty_type_overrides = IndexMap::new();
        ctx.push_path(&mi.name);
        if instantiate_component(
            tree,
            &synthetic,
            &mut ctx,
            &mut overlay,
            &empty_siblings,
            &empty_type_overrides,
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
    let mut ctx = InstantiateContext::new();

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
        match retry_with_synthetic_inners(tree, model, &missing) {
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
    instantiate_model_with_outcome(tree, model_name).into_result()
}

/// Convert algorithm statements to instance statements.
fn algorithms_to_instance(
    algorithms: &[Vec<rumoca_ir_ast::Statement>],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> Vec<Vec<ast::InstanceStatement>> {
    algorithms
        .iter()
        .map(|stmts| {
            stmts
                .iter()
                .map(|stmt| ast::InstanceStatement {
                    statement: stmt.clone(),
                    origin: origin.clone(),
                    span: option_location_to_span(stmt.get_location(), source_map),
                })
                .collect()
        })
        .collect()
}

/// Convert borrowed equations to instance equations (cloning each equation).
fn equations_to_instance_cloned(
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> Vec<ast::InstanceEquation> {
    equations
        .iter()
        .map(|eq| {
            let span = option_location_to_span(eq.get_location(), source_map);
            ast::InstanceEquation {
                equation: eq.clone(),
                origin: origin.clone(),
                span,
            }
        })
        .collect()
}

/// Convert non-connection equations to instance equations in one pass.
///
/// This avoids cloning an intermediate `Vec<ast::Equation>` before creating
/// `ast::InstanceEquation` values.
fn equations_to_instance_without_connections(
    equations: &[ast::Equation],
    origin: &ast::QualifiedName,
    source_map: &rumoca_core::SourceMap,
) -> Vec<ast::InstanceEquation> {
    equations
        .iter()
        .filter(|eq| !connections::is_connect_equation(eq))
        .map(|eq| {
            let span = option_location_to_span(eq.get_location(), source_map);
            ast::InstanceEquation {
                equation: eq.clone(),
                origin: origin.clone(),
                span,
            }
        })
        .collect()
}

/// Instantiate a class and all its components.
fn instantiate_class(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    ctx: &mut InstantiateContext,
    overlay: &mut ast::InstanceOverlay,
) -> InstantiateResult<()> {
    ctx.push_inner_scope(); // Push a new inner scope for this class (MLS §5.4)
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
    let type_overrides = build_type_override_map(tree, class, Some(ctx.mod_env()));

    // Extract boolean parameter values for conditional connection evaluation
    // This enables proper handling of patterns like:
    // if use_numberPort then connect(numberPort, showNumber); else ... end if;
    // Check both the component definitions and the modification environment
    let bool_params = extract_bool_params_with_mods(effective_components, ctx.mod_env());

    // Extract integer parameter values for for-loop range evaluation
    // This enables proper handling of patterns like:
    // for k in 1:m loop connect(plug_p.pin[k], resistor[k].p); end for;
    let int_params = extract_int_params_with_mods(effective_components, ctx.mod_env(), tree);
    ctx.register_known_int_params(&qualified_name, &int_params);

    // Instantiate each effective component (MLS §4.8 conditional components)
    // Components with conditions are only instantiated if the condition evaluates to true.
    // When a conditional component is disabled, we skip it entirely - its variables and
    // equations should not exist in the flat model.
    // MLS §10.1: Array components of structured types are expanded to indexed instances.

    // Check depth to prevent stack overflow from circular references
    if ctx.is_depth_exceeded() {
        return Err(Box::new(InstantiateError::structural_param_error(
            ctx.current_path().to_string(),
            format!("Instantiation depth exceeded ({})", MAX_INSTANTIATION_DEPTH),
            rumoca_core::Span::DUMMY,
        )));
    }

    let array_expansion_scope = ArrayExpansionScope {
        tree,
        effective_components,
        type_overrides: &type_overrides,
    };

    for (name, comp) in effective_components {
        if mark_disabled_component_if_needed(comp, name, ctx, effective_components, tree, overlay) {
            continue;
        }

        // MLS §7.3: Apply type override for replaceable type redeclarations.
        let type_name = comp.type_name.to_string();
        let comp_ref =
            apply_type_override(tree, comp, &type_overrides, &type_name, Some(ctx.mod_env()));
        let comp = comp_ref.as_ref();

        // MLS §10.1: Array component expansion
        // For structured types (models, connectors, blocks), expand array components
        // to indexed instances. For primitive types, keep as arrays for CasADi.
        let type_info = lookup_type_info(tree, comp, &type_name);
        let dims = evaluate_array_dimensions(
            &comp.shape,
            &comp.shape_expr,
            ctx.mod_env(),
            effective_components,
            tree,
        );
        // Only expand arrays if not near depth limit (leave room for nested components)
        let depth_ok = ctx.context_path.len() + 10 < MAX_INSTANTIATION_DEPTH;
        let should_expand =
            depth_ok && !type_info.is_primitive && dims.as_ref().is_some_and(|d| !d.is_empty());

        // Skip zero-sized array components (MLS §10.1, e.g., inPort[nIn] with nIn=0)
        if dims.as_ref().is_some_and(|d| d.contains(&0)) {
            continue;
        }

        if should_expand {
            expand_array_component(
                &array_expansion_scope,
                name,
                comp,
                dims.as_ref().unwrap(),
                ctx,
                overlay,
            )?;
        } else {
            ctx.push_path(name);
            instantiate_component(
                tree,
                comp,
                ctx,
                overlay,
                effective_components,
                &type_overrides,
            )?;
            ctx.pop_path();
        }
    }

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
    let connections =
        connections::extract_connections(all_equations, &qualified_name, &conn_params, source_map);

    // Convert regular equations in one pass without intermediate equation vectors.
    let instance_equations =
        equations_to_instance_without_connections(all_equations, &qualified_name, source_map);
    let instance_initial_equations =
        equations_to_instance_cloned(&template.initial_equations, &qualified_name, source_map);
    let instance_algorithms =
        algorithms_to_instance(&template.algorithms, &qualified_name, source_map);
    let instance_initial_algorithms =
        algorithms_to_instance(&template.initial_algorithms, &qualified_name, source_map);

    let class_data = ast::ClassInstanceData {
        instance_id,
        qualified_name: qualified_name.clone(),
        equations: instance_equations,
        initial_equations: instance_initial_equations,
        algorithms: instance_algorithms,
        initial_algorithms: instance_initial_algorithms,
        connections,
        resolved_imports: template.resolved_imports.clone(),
    };
    overlay.add_class(class_data);

    // Pop the inner scope when leaving this class (MLS §5.4)
    ctx.pop_inner_scope();

    Ok(())
}

fn mark_disabled_component_if_needed(
    comp: &ast::Component,
    name: &str,
    ctx: &mut InstantiateContext,
    effective_components: &IndexMap<String, ast::Component>,
    tree: &ast::ClassTree,
    overlay: &mut ast::InstanceOverlay,
) -> bool {
    // MLS §4.8: Conditional components
    // Evaluate the condition and skip components where condition is false.
    // Disabled component paths are recorded in overlay.disabled_components
    // so the flatten phase can filter out connections involving them.
    // If the condition cannot be evaluated here, keep component instantiated.
    let condition_is_false = comp.condition.as_ref().is_some_and(|cond| {
        evaluate_component_condition(cond, ctx.mod_env(), effective_components, tree) == Some(false)
    });
    if !condition_is_false {
        return false;
    }

    ctx.push_path(name);
    let disabled_path = ctx.current_path().to_string();
    overlay.disabled_components.insert(disabled_path);
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
    if comp.inner {
        ctx.register_inner(
            &comp.name,
            qualified_name.clone(),
            type_name,
            comp.type_def_id,
        );
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
                type_name,
                comp.type_def_id,
                &inner_decl.type_name,
                inner_decl.type_def_id,
            );
            if !types_compatible {
                return Err(Box::new(InstantiateError::inner_outer_type_mismatch(
                    &comp.name,
                    type_name,
                    &inner_decl.type_name,
                    span,
                )));
            }
        } else {
            ctx.record_missing_inner(&comp.name, type_name, comp.type_def_id, span);
        }
    }
    Ok(())
}

/// Type information for a component, resolved from the class tree.
struct TypeInfo<'a> {
    class_def: Option<&'a ast::ClassDef>,
    is_primitive: bool,
    is_discrete: bool,
}

/// Map a builtin primitive name to its TypeId in the class tree.
fn builtin_type_id(tree: &ast::ClassTree, name: &str) -> Option<TypeId> {
    let simple = name.rsplit('.').next().unwrap_or(name);
    match simple {
        "Real" => Some(tree.type_table.real()),
        "Integer" => Some(tree.type_table.integer()),
        "Boolean" => Some(tree.type_table.boolean()),
        "String" => Some(tree.type_table.string()),
        "Clock" => tree.type_table.lookup("Clock"),
        _ => None,
    }
}

/// Resolve the primitive base TypeId for a component type when available.
///
/// For direct builtins this returns the corresponding TypeId. For type aliases
/// and short class definitions, this follows a single-inheritance chain until
/// a builtin primitive is found, or returns UNKNOWN when unresolved.
fn resolve_primitive_type_id(
    tree: &ast::ClassTree,
    type_name: &str,
    class_def: Option<&ast::ClassDef>,
) -> TypeId {
    if let Some(id) = builtin_type_id(tree, type_name) {
        return id;
    }

    const MAX_DEPTH: usize = 10;
    let mut current = class_def;

    for _ in 0..MAX_DEPTH {
        let Some(class) = current else {
            return TypeId::UNKNOWN;
        };
        if class.extends.len() != 1 {
            return TypeId::UNKNOWN;
        }

        let ext = &class.extends[0];
        let base_name = ext.base_name.to_string();
        if let Some(id) = builtin_type_id(tree, &base_name) {
            return id;
        }

        current = ext
            .base_def_id
            .and_then(|def_id| tree.get_class_by_def_id(def_id))
            .or_else(|| find_class_in_tree(tree, &base_name));
    }

    TypeId::UNKNOWN
}

/// Find a nested type member within a class, following the extends chain.
///
/// When `Medium.AbsolutePressure` resolves to the `Medium` package alias,
/// we need to find `AbsolutePressure` by looking in the package's own classes
/// and then in its base classes (via extends). This handles replaceable packages
/// like `package Medium = PartialMedium` where types are defined in the base.
fn find_member_type_in_class<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_name: &str,
) -> Option<&'a ast::ClassDef> {
    // MLS §7.3: extends-modification redeclarations override inherited
    // replaceable declarations in this class context.
    if let Some(redeclared) = find_extends_redeclared_member_type(tree, class, member_name) {
        return Some(redeclared);
    }

    // Then check direct nested classes.
    if let Some(member) = class.classes.get(member_name) {
        return Some(member);
    }

    // Follow extends chain to find the member in base classes.
    // Also follow ALL extends (not just single), since packages can have
    // multiple inheritance-like extends clauses.
    const MAX_DEPTH: usize = 15;
    let mut visited = Vec::new();
    let mut to_visit: Vec<Option<&ast::ClassDef>> = class
        .extends
        .iter()
        .map(|ext| {
            let base_name = ext.base_name.to_string();
            ext.base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &base_name))
        })
        .collect();

    for _ in 0..MAX_DEPTH {
        if to_visit.is_empty() {
            break;
        }
        let mut next_visit = Vec::new();
        for current in to_visit.drain(..) {
            let Some(bc) = current else { continue };
            // Avoid revisiting
            let bc_name = bc.name.text.as_ref();
            if visited.contains(&bc_name) {
                continue;
            }
            visited.push(bc_name);

            if let Some(member) = find_extends_redeclared_member_type(tree, bc, member_name) {
                return Some(member);
            }
            if let Some(member) = bc.classes.get(member_name) {
                return Some(member);
            }
            // Queue base classes for next level
            for ext in &bc.extends {
                let next_name = ext.base_name.to_string();
                next_visit.push(
                    ext.base_def_id
                        .and_then(|def_id| tree.get_class_by_def_id(def_id))
                        .or_else(|| find_class_in_tree(tree, &next_name)),
                );
            }
        }
        to_visit = next_visit;
    }

    None
}

fn find_extends_redeclared_member_type<'a>(
    tree: &'a ast::ClassTree,
    class: &ast::ClassDef,
    member_name: &str,
) -> Option<&'a ast::ClassDef> {
    for ext in &class.extends {
        for ext_mod in &ext.modifications {
            if !ext_mod.redeclare {
                continue;
            }
            let ast::Expression::Modification { target, value } = &ext_mod.expr else {
                continue;
            };
            let Some(first_target) = target.parts.first() else {
                continue;
            };
            if first_target.ident.text.as_ref() != member_name {
                continue;
            }
            let Some(redeclared_def_id) = resolve_redeclare_value_def_id(tree, value, None) else {
                continue;
            };
            if let Some(redeclared_class) = tree.get_class_by_def_id(redeclared_def_id) {
                return Some(redeclared_class);
            }
        }
    }
    None
}

/// Resolve a dotted member path against a class, following the extends chain
/// at each segment via [`find_member_type_in_class`].
fn find_member_type_path_in_class<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
    member_path: &str,
) -> Option<&'a ast::ClassDef> {
    let mut current = class;
    for segment in member_path.split('.') {
        current = find_member_type_in_class(tree, current, segment)?;
    }
    Some(current)
}

/// Look up type information for a component.
/// Uses type_def_id for O(1) lookup when available, falling back to name lookup.
fn lookup_type_info<'a>(
    tree: &'a ast::ClassTree,
    comp: &ast::Component,
    type_name: &str,
) -> TypeInfo<'a> {
    let mut class_def = comp
        .type_def_id
        .or(comp.type_name.def_id)
        .and_then(|def_id| tree.get_class_by_def_id(def_id))
        .or_else(|| find_class_in_tree(tree, type_name));

    // When a dotted type name like "Medium.AbsolutePressure",
    // "Medium.ThermodynamicState", or "Medium.BaseProperties" resolves to the
    // package ("Medium") rather than the nested member type, drill down into
    // the package member.
    //
    // Components must instantiate the referenced member class, not the
    // containing package. This is required for package-member model components
    // such as `Medium.BaseProperties medium`.
    if let Some(cd) = class_def
        && matches!(cd.class_type, rumoca_ir_ast::ClassType::Package)
        && let Some(last_dot) = type_name.rfind('.')
    {
        let member_name = &type_name[last_dot + 1..];
        if let Some(member) = find_member_type_in_class(tree, cd, member_name) {
            class_def = Some(member);
        }
    }

    // is_effectively_primitive_transitive follows inheritance chains to detect:
    // - Type aliases to Real/Integer/Boolean/String (primitive)
    // - Records like Complex with components .re/.im (not primitive)
    // - Operator records like SI.ComplexVoltage extending Complex (not primitive)
    let is_primitive = class_def.is_none()
        || class_def
            .map(|c| inheritance::is_effectively_primitive_transitive(tree, c))
            .unwrap_or(false);

    let is_discrete = inheritance::is_discrete_by_type(tree, type_name, class_def);

    TypeInfo {
        class_def,
        is_primitive,
        is_discrete,
    }
}

struct InstanceDataBuild<'a> {
    instance_id: ast::InstanceId,
    qualified_name: ast::QualifiedName,
    dims: Vec<i64>,
    dims_expr: Vec<rumoca_ir_ast::Subscript>,
    type_name: String,
    type_def_id: Option<DefId>,
    class_overrides: IndexMap<String, DefId>,
    has_forwarding_class_redeclare: bool,
    effective_variability: rumoca_ir_ast::Variability,
    causality: rumoca_ir_ast::Causality,
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
    let instance_data = ast::InstanceData {
        instance_id: args.instance_id,
        qualified_name: args.qualified_name,
        dims: args.dims,
        dims_expr: args.dims_expr,
        type_id: args.type_id,
        type_name: args.type_name,
        // Keep partial first-segment anchors (e.g. `Medium` in
        // `Medium.AbsolutePressure`) so instanced typecheck can resolve dotted
        // type names using lexical package anchors.
        type_def_id: args.type_def_id.or(args.comp.type_name.def_id),
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
            .map(|c| matches!(c.class_type, rumoca_ir_ast::ClassType::Connector))
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
    inherited_causality: Option<&rumoca_ir_ast::Causality>,
) -> rumoca_ir_ast::Causality {
    // MLS §4.4.2.2: record fields inherit input/output from the enclosing component.
    // Connector aliases like `RealInput = input Real` also propagate causality.
    if !matches!(comp.causality, rumoca_ir_ast::Causality::Empty) {
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
    inherited_variability: Option<&rumoca_ir_ast::Variability>,
) -> rumoca_ir_ast::Variability {
    // MLS §4.4.2.1: fields of parameter/constant records inherit variability.
    if matches!(comp.variability, rumoca_ir_ast::Variability::Empty) {
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
            rumoca_ir_ast::ClassType::Package | rumoca_ir_ast::ClassType::Function
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

fn extract_component_attrs_and_binding(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
) -> (
    ExtractedAttributes,
    Option<ast::Expression>,
    Option<ast::Expression>,
    Option<ast::QualifiedName>,
    bool,
) {
    // Pass component name so mod_env can be checked for outer modifications.
    let mut attrs = extract_attributes(comp, mod_env, &comp.name);
    let (binding, binding_from_modification, binding_source_scope) = extract_binding(comp, mod_env);
    let binding_source = if binding_from_modification {
        let binding_path = ast::QualifiedName::from_ident(&comp.name);
        mod_env
            .get(&binding_path)
            .and_then(|mod_value| mod_value.source.clone())
    } else {
        None
    };

    // MLS §4.4.4: declaration binding may provide a default start for
    // parameter/constant declarations when no explicit start is present.
    // MLS §7.2.4: outer *modification* bindings do not rewrite the declared
    // start attribute; they set the binding equation value.
    if should_promote_binding_to_start(comp, &attrs, binding_from_modification) && binding.is_some()
    {
        attrs.start = binding.clone();
    }

    (
        attrs,
        binding,
        binding_source,
        binding_source_scope,
        binding_from_modification,
    )
}

fn should_promote_binding_to_start(
    comp: &ast::Component,
    attrs: &ExtractedAttributes,
    binding_from_modification: bool,
) -> bool {
    matches!(
        comp.variability,
        rumoca_ir_ast::Variability::Parameter(_) | rumoca_ir_ast::Variability::Constant(_)
    ) && attrs.start.is_none()
        && !binding_from_modification
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
    type_overrides: &IndexMap<String, DefId>,
) -> InstantiateResult<()> {
    let type_name = comp.type_name.to_string();

    let instance_id = overlay.alloc_id();
    let qualified_name = ctx.current_path();

    // MLS §5.4: Handle inner/outer components
    handle_inner_outer(tree, comp, ctx, overlay, &qualified_name, &type_name)?;

    let (attrs, binding, binding_source, binding_source_scope, binding_from_modification) =
        extract_component_attrs_and_binding(comp, ctx.mod_env());

    // Extract flow/stream from connection prefix (MLS §9.3)
    // Also inherit from parent for record fields (e.g., `flow Complex i` → i.re and i.im are flow)
    let (flow, stream) = match &comp.connection {
        rumoca_ir_ast::Connection::Flow(_) => (true, false),
        rumoca_ir_ast::Connection::Stream(_) => (false, true),
        rumoca_ir_ast::Connection::Empty => (ctx.inherited_flow(), ctx.inherited_stream()),
    };

    // Look up type information (primitive status, discrete type, class definition)
    let type_info = lookup_type_info(tree, comp, &type_name);
    let TypeInfo {
        class_def,
        is_primitive,
        is_discrete: is_discrete_type,
    } = type_info;
    validate_partial_component_instantiation(
        tree,
        comp,
        class_def,
        &qualified_name,
        &type_name,
        ctx.allow_partial_instantiation,
    )?;

    let type_dims =
        resolve_type_alias_dimensions(tree, class_def, ctx.mod_env(), effective_components);

    // Resolve array dimensions (component shape + inherited type alias shape).
    let (dims, dims_expr) =
        resolve_component_dimensions(comp, &type_dims, ctx.mod_env(), effective_components, tree);

    let type_id = if is_primitive {
        resolve_primitive_type_id(tree, &type_name, class_def)
    } else {
        TypeId::UNKNOWN
    };

    let causality = resolve_component_causality(comp, class_def, ctx.inherited_causality());

    // Check for Evaluate=true annotation (MLS §18.3)
    let evaluate = has_evaluate_annotation(comp);

    let effective_variability = resolve_effective_variability(comp, ctx.inherited_variability());

    let (class_overrides, has_forwarding_class_redeclare, nested_type_overrides) =
        resolve_component_nested_type_overrides(
            tree,
            comp,
            class_def,
            ctx.mod_env(),
            type_overrides,
        );

    let (instance_data, binding_for_record_expansion) = build_instance_data(InstanceDataBuild {
        instance_id,
        qualified_name,
        dims,
        dims_expr,
        type_name: type_name.clone(),
        type_def_id: comp.type_def_id,
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

    // Recursively instantiate only structured, non-pure-outer components.
    // Primitive aliases remain scalar values (MLS §4.8), and pure `outer`
    // components reuse the corresponding `inner` instance tree (MLS §5.4).
    if !is_primitive
        && (!comp.outer || comp.inner)
        && let Some(nested_class) = class_def
    {
        let nested_input = NestedInstantiationInput {
            nested_class,
            comp,
            effective_variability: &effective_variability,
            causality: &causality,
            flow,
            stream,
            binding_for_record_expansion: binding_for_record_expansion.as_ref(),
            binding_scope_for_record_expansion: binding_source_scope.as_ref(),
            effective_components,
            type_overrides: &nested_type_overrides,
        };
        instantiate_nested_class(tree, ctx, overlay, nested_input)?;
    }

    Ok(())
}

/// Handle nested class instantiation: set up modification environment,
/// push inheritance flags, instantiate the class, and clean up.
struct NestedInstantiationInput<'a> {
    nested_class: &'a ast::ClassDef,
    comp: &'a ast::Component,
    effective_variability: &'a rumoca_ir_ast::Variability,
    causality: &'a rumoca_ir_ast::Causality,
    flow: bool,
    stream: bool,
    binding_for_record_expansion: Option<&'a ast::Expression>,
    binding_scope_for_record_expansion: Option<&'a ast::QualifiedName>,
    effective_components: &'a IndexMap<String, ast::Component>,
    type_overrides: &'a IndexMap<String, DefId>,
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

    // Step 3: Push inheritance flags and instantiate nested class
    let inheritance_flags = ctx.push_inheritance(
        effective_variability,
        causality,
        flow,
        stream,
        nested_class.expandable,
    );

    let eq_size = inheritance::equality_constraint_output_size(nested_class);
    ctx.push_overconstrained(eq_size);
    ctx.push_protected(comp.is_protected);

    instantiate_class(tree, nested_class, ctx, overlay)?;

    ctx.pop_protected();
    ctx.pop_overconstrained();
    ctx.pop_inheritance(inheritance_flags);

    // Restore mod_env to pre-modification state, preserving outer scope modifications
    ctx.mod_env_mut().active = mod_env_snapshot;

    Ok(())
}

/// Extract attributes from a component's modifications.
///
/// MLS §4.9: Attributes like start, fixed, min, max, nominal, quantity, unit,
/// displayUnit, stateSelect
/// can be specified via modifications.
///
/// MLS §7.2: The modification environment is checked for overriding modifications
/// from outer scopes. Outer modifications override inner ones per MLS §7.2.4.
///
/// Attribute lookup priority (highest to lowest):
/// 1. mod_env (outer modifications from enclosing scopes)
/// 2. comp.modifications (local modifications on this component)
/// 3. comp.start (default value from declaration)
fn extract_attributes(
    comp: &ast::Component,
    mod_env: &ast::ModificationEnvironment,
    comp_name: &str,
) -> ExtractedAttributes {
    // First, check the modification environment for outer modifications
    // These have precedence over local modifications per MLS §7.2.4
    let mut attrs = ExtractedAttributes {
        start: mod_env.get_attr(comp_name, "start").cloned(),
        fixed: mod_env.get_attr(comp_name, "fixed").and_then(expr_to_bool),
        min: mod_env.get_attr(comp_name, "min").cloned(),
        max: mod_env.get_attr(comp_name, "max").cloned(),
        nominal: mod_env.get_attr(comp_name, "nominal").cloned(),
        quantity: mod_env
            .get_attr(comp_name, "quantity")
            .and_then(expr_to_string),
        unit: mod_env.get_attr(comp_name, "unit").and_then(expr_to_string),
        display_unit: mod_env
            .get_attr(comp_name, "displayUnit")
            .and_then(expr_to_string),
        state_select: mod_env
            .get_attr(comp_name, "stateSelect")
            .map(parse_state_select)
            .unwrap_or_default(),
    };

    // If no outer modifications, check local modifications on this component
    // We only fall back to local mods if outer mods didn't set the attribute
    if attrs.start.is_none() {
        for (name, value) in &comp.modifications {
            match name.as_str() {
                "start" if attrs.start.is_none() => attrs.start = Some(value.clone()),
                "fixed" if attrs.fixed.is_none() => attrs.fixed = expr_to_bool(value),
                "min" if attrs.min.is_none() => attrs.min = Some(value.clone()),
                "max" if attrs.max.is_none() => attrs.max = Some(value.clone()),
                "nominal" if attrs.nominal.is_none() => attrs.nominal = Some(value.clone()),
                "quantity" if attrs.quantity.is_none() => attrs.quantity = expr_to_string(value),
                "unit" if attrs.unit.is_none() => attrs.unit = expr_to_string(value),
                "displayUnit" if attrs.display_unit.is_none() => {
                    attrs.display_unit = expr_to_string(value)
                }
                "stateSelect" => attrs.state_select = parse_state_select(value),
                _ => {}
            }
        }
    }

    // Use the component's start field if no modification provided
    if attrs.start.is_none() && !matches!(comp.start, ast::Expression::Empty) {
        attrs.start = Some(comp.start.clone());
    }

    attrs
}

/// Check if inner and outer types are compatible using DefIds when available.
///
/// MLS §5.4: The inner declaration's type must be a subtype of the outer's type.
/// This version uses DefId comparison first (O(1), alias-aware), then falls back
/// to string-based subtype checking.
///
/// Handles cases where type names differ in qualification level:
/// - outer uses short form: "Interfaces.CompositeStepState"
/// - inner uses qualified form: "StateGraph.Interfaces.CompositeStepState"
fn is_type_compatible_with_def_id(
    tree: &ast::ClassTree,
    outer_type: &str,
    outer_def_id: Option<DefId>,
    inner_type: &str,
    inner_def_id: Option<DefId>,
) -> bool {
    // Fast path: If both have DefIds and they match, types are the same
    if let (Some(outer_id), Some(inner_id)) = (outer_def_id, inner_def_id)
        && outer_id == inner_id
    {
        return true;
    }

    // Check if one name is a suffix of the other (short vs qualified name)
    // e.g., "Interfaces.CompositeStepState" matches "StateGraph.Interfaces.CompositeStepState"
    if type_names_match(tree, outer_type, inner_type) {
        return true;
    }

    // Fallback to string-based subtype checking (handles inheritance)
    is_type_subtype(tree, inner_type, outer_type)
}

/// Check if inner type is compatible with outer type (for tests and simple cases).
/// Inner must be a subtype of outer for compatibility.
#[cfg(test)]
fn is_type_compatible(tree: &ast::ClassTree, outer_type: &str, inner_type: &str) -> bool {
    is_type_subtype(tree, inner_type, outer_type)
}

/// Check if a component has annotation(Evaluate=true).
///
/// MLS §18.3: The Evaluate annotation indicates that a parameter should be
/// evaluated at compile time. This is used for structural parameters that
/// affect equation structure (e.g., if-equation branch selection).
///
/// Returns true if:
/// - The component has `annotation(Evaluate=true)`, or
/// - The component is declared `final` (implies compile-time evaluation)
fn has_evaluate_annotation(comp: &ast::Component) -> bool {
    // Final parameters are always structural (can be evaluated at compile time)
    if comp.is_final {
        return true;
    }

    // Check annotation for Evaluate=true
    comp.annotation.iter().any(is_evaluate_true_annotation)
}

/// Check if an annotation expression is `Evaluate=true`.
fn is_evaluate_true_annotation(anno_expr: &ast::Expression) -> bool {
    // Annotations can be parsed as either NamedArgument or Modification
    let (name_text, value) = match anno_expr {
        ast::Expression::NamedArgument { name, value } => (name.text.as_ref(), value.as_ref()),
        ast::Expression::Modification { target, value } => {
            // Get the first part of the ComponentReference as the identifier
            let Some(first_part) = target.parts.first() else {
                return false;
            };
            (first_part.ident.text.as_ref(), value.as_ref())
        }
        _ => return false,
    };

    if name_text != "Evaluate" {
        return false;
    }
    // Check if value is true (booleans are Terminal with ast::TerminalType::Bool)
    matches!(
        value,
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } if token.text.as_ref() == "true"
    )
}

#[cfg(test)]
mod tests;
