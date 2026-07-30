use crate::Function;

/// Context for flattening.
pub(crate) struct Context {
    /// Parameter values for evaluating for-equation ranges (name -> integer value).
    pub parameter_values: rustc_hash::FxHashMap<String, i64>,
    /// Real parameter values for evaluating function arguments (name -> real value).
    pub real_parameter_values: rustc_hash::FxHashMap<String, f64>,
    /// Boolean parameter values for evaluating if-equation conditions.
    pub boolean_parameter_values: rustc_hash::FxHashMap<String, bool>,
    /// Enumeration parameter values (name -> qualified enum literal string).
    pub enum_parameter_values: rustc_hash::FxHashMap<String, String>,
    /// General constant expression values (scalars/arrays) extracted from
    /// class/package constants and redeclare/extends modifications.
    pub constant_values: rustc_hash::FxHashMap<String, rumoca_core::Expression>,
    /// Constant values keyed by their exact Resolve declaration identity.
    pub constant_values_by_def_id:
        rustc_hash::FxHashMap<rumoca_core::DefId, rumoca_core::Expression>,
    /// Qualified declaration names keyed by semantic target DefId.
    pub target_def_names: rustc_hash::FxHashMap<rumoca_core::DefId, String>,
    /// Exact Resolve identity of the predefined `String` declaration.
    pub predefined_string_declaration: Option<rumoca_core::DefId>,
    /// Fully qualified constant names explicitly modified by extends clauses.
    /// These must not be overwritten by inherited declaration defaults.
    pub(crate) modified_constant_keys: rustc_hash::FxHashSet<String>,
    /// Parameter/constant variable keys materialized from the instantiated flat model.
    /// Injected class defaults must not overwrite these effective instance values.
    pub flat_parameter_constant_keys: rustc_hash::FxHashSet<String>,
    /// Component paths that exist in the flat model only through their expanded
    /// members (`src.Phi` is present as `src.Phi.re` / `src.Phi.im`).
    ///
    /// The instantiated members already carry the component modification, while
    /// `constant_values` may still hold the declaration default recorded from the
    /// class body. Folding that default into a reference to the whole component
    /// would silently discard the modification (MLS §7.2.4), so such references
    /// stay symbolic and are expanded member-wise instead.
    pub(crate) expanded_component_keys: rustc_hash::FxHashSet<String>,
    /// Array dimensions for evaluating size() calls (name -> dims).
    pub array_dimensions: rustc_hash::FxHashMap<String, Vec<i64>>,
    /// Parameters marked with annotation(Evaluate=true) or declared final (MLS §18.3).
    /// Only these structural parameters can be used for compile-time branch selection.
    pub structural_params: std::collections::HashSet<String>,
    /// Parameters explicitly declared with `fixed = false` and without
    /// `Evaluate=true`. These must not be folded for structural branch
    /// selection, even when a provisional value is available.
    pub non_structural_params: std::collections::HashSet<String>,
    /// User-defined function definitions for compile-time evaluation (MLS §12.3).
    /// Functions are looked up by qualified name during constant expression evaluation.
    pub functions: rustc_hash::FxHashMap<String, Function>,
    /// Record aliases for resolving field access through record parameter bindings.
    /// Maps record parameter component path -> alias target component path (MLS §7.2.3).
    /// Example: "battery2.cellData" -> "cellData2" allows resolving
    /// "battery2.cellData.nRC" to "cellData2.nRC".
    pub record_aliases:
        rustc_hash::FxHashMap<rumoca_core::ComponentPath, rumoca_core::ComponentPath>,
    /// Direct instance members keyed by their parent instance scope.
    ///
    /// Flatten qualification uses this to apply MLS §5.3 direct-member lookup
    /// before imports without recovering hierarchy from rendered flat names.
    pub(crate) component_members: super::super::component_member_scope::ComponentMemberScopes,
    /// VCG isRoot results: path -> true if this node is the root of its component (MLS §9.4).
    pub vcg_is_root: rustc_hash::FxHashMap<String, bool>,
    /// VCG rooted results: path -> true if this node is on the "rooted" side (MLS §9.4).
    pub vcg_rooted: rustc_hash::FxHashMap<String, bool>,
    /// Cardinality counts: connector path -> number of connect() statements referencing it (MLS §3.7.2.3).
    pub cardinality_counts: rustc_hash::FxHashMap<String, i64>,
    /// Lazy base evaluator for flatten expression fallback evaluation.
    /// This is built once per flatten context after structural lookup stabilizes.
    pub(crate) eval_fallback_context: std::cell::OnceCell<rumoca_eval_flat::constant::EvalContext>,
    /// Current import map for the class instance being processed (MLS §13.2).
    /// Set before processing each class instance's equations, cleared after.
    pub current_imports: crate::qualify::ImportMap,
    /// Set of DefIds that correspond to class definitions in the current tree.
    /// Used by qualification to distinguish class/type references from components.
    pub class_def_ids: std::sync::Arc<rustc_hash::FxHashSet<rumoca_core::DefId>>,
    /// Canonical class scope path for the class instance currently being flattened.
    /// Derived from `def_map` via the owning class DefId.
    pub current_class_scope_path: Option<String>,
    /// Unqualified name of the simulated root model/block for MLS getInstanceName().
    pub simulated_root_name: Option<String>,
    /// Mirror of `FlattenOptions::materialize_structured_families`. When false, a
    /// regular elementwise for-family materializes only its corner cells (base + one
    /// neighbor per binder) with full bodies; interior cells get a cheap placeholder
    /// body and the family is marked `interiors_materialized = false` so downstream
    /// phases reconstruct interior incidence/strides from the corners.
    pub materialize_structured_families: bool,
    /// Array base-names assigned by parameter-variability for-equation families: a
    /// family every reference of which resolves (transitively) to a parameter/constant
    /// or another such family. Their per-cell values are time-invariant, so flatten may
    /// cheapen their interior cells (the DAE promotes them array-natively from the
    /// captured comprehension template). Computed once before equation expansion in
    /// `prepare_context_for_equation_flattening`.
    pub param_variability_family_bases: rustc_hash::FxHashSet<String>,
}
