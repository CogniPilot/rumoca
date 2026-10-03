use crate::Function;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub(crate) struct ConstantOccurrenceId {
    owner: rumoca_core::InstanceId,
    declaration: rumoca_core::DefId,
}

impl ConstantOccurrenceId {
    pub(crate) fn new(owner: rumoca_core::InstanceId, declaration: rumoca_core::DefId) -> Self {
        Self { owner, declaration }
    }

    pub(crate) fn owner(self) -> rumoca_core::InstanceId {
        self.owner
    }
}

/// Context for flattening.
pub(crate) struct Context {
    pub(crate) declared_dimensions: std::sync::Arc<rumoca_eval_ast::eval::DeclaredDimensions>,
    /// Parameter values for evaluating for-equation ranges (name -> integer value).
    pub parameter_values: rustc_hash::FxHashMap<String, i64>,
    /// Real parameter values for evaluating function arguments (name -> real value).
    pub real_parameter_values: rustc_hash::FxHashMap<String, f64>,
    /// Boolean parameter values for evaluating if-equation conditions.
    pub boolean_parameter_values: rustc_hash::FxHashMap<String, bool>,
    /// Enumeration parameter values (name -> qualified enum literal string).
    pub enum_parameter_values: rustc_hash::FxHashMap<String, String>,
    /// String and array parameter values (MLS §10.1): a dimension or a
    /// function result shape reads them whole or by element.
    pub(crate) aggregate_parameter_values:
        rustc_hash::FxHashMap<String, rumoca_eval_flat::constant::Value>,
    /// The directory of each loaded top-level package, against which a
    /// foreign file reader resolves MLS §13.5 URIs (SPEC_0040 FLAT-C06).
    pub(crate) resource_roots: rumoca_eval_flat::translation_reads::ResourceRoots,
    /// General constant expression values (scalars/arrays) extracted from
    /// class/package constants and redeclare/extends modifications.
    pub constant_values: rustc_hash::FxHashMap<String, rumoca_core::Expression>,
    /// Constant values keyed by their exact Resolve declaration identity.
    pub constant_values_by_def_id:
        rustc_hash::FxHashMap<rumoca_core::DefId, rumoca_core::Expression>,
    /// Constant values keyed by the package scope that exposes the declaration
    /// and its exact Resolve declaration identity (MLS §7.3: a package that
    /// extends another with modifications gives an inherited constant its own
    /// value, so the declaration identity alone does not determine it).
    pub(crate) constant_values_by_scope:
        rustc_hash::FxHashMap<(String, rumoca_core::DefId), rumoca_core::Expression>,
    /// The value every recorded exposure of a constant declaration agrees on,
    /// or `None` when two packages give it different values.
    pub(crate) constant_values_by_declaration:
        rustc_hash::FxHashMap<rumoca_core::DefId, Option<rumoca_core::Expression>>,
    /// Component-local overrides keyed by exact instantiated occurrence and
    /// exact Resolve declaration identity.
    pub(crate) constant_values_by_occurrence:
        rustc_hash::FxHashMap<ConstantOccurrenceId, rumoca_core::Expression>,
    /// Owning component occurrence for each instantiated class occurrence.
    pub(crate) class_owner_components:
        rustc_hash::FxHashMap<rumoca_core::InstanceId, rumoca_core::InstanceId>,
    /// Package redeclarations each class occurrence applies: slot to selected
    /// package (MLS §7.3).
    pub(crate) class_package_selections: rustc_hash::FxHashMap<
        rumoca_core::InstanceId,
        rustc_hash::FxHashMap<rumoca_core::DefId, rumoca_core::DefId>,
    >,
    /// The class slot each component occurrence's type is spelled through
    /// (`Medium` in `Medium.BaseProperties medium`) and its owning class
    /// occurrence.
    pub(crate) component_type_slots: rustc_hash::FxHashMap<
        rumoca_core::InstanceId,
        (rumoca_core::DefId, Option<rumoca_core::InstanceId>),
    >,
    /// Instance path of each instantiated component occurrence, as the exact
    /// reference Instantiate proved for it (one part per enclosing component,
    /// each carrying its Resolve declaration identity).
    pub(crate) component_instance_references:
        rustc_hash::FxHashMap<rumoca_core::InstanceId, rumoca_core::ComponentReference>,
    /// Root class occurrence, which is its own semantic owner because it has no
    /// containing component occurrence.
    pub(crate) root_class_instance: Option<rumoca_core::InstanceId>,
    /// Qualified declaration names keyed by semantic target DefId.
    pub target_def_names: rustc_hash::FxHashMap<rumoca_core::DefId, String>,
    /// Exact Resolve identity of the predefined `String` declaration.
    pub predefined_string_declaration: Option<rumoca_core::DefId>,
    /// Exact Resolve identities of predefined intrinsics and array constructors.
    pub predefined_intrinsics: crate::ast_lower::PredefinedIntrinsicIds,
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
    /// Ordinary parameters: fixed, without `Evaluate=true` or `final`. A
    /// branch selection that reads one is not structural (SPEC_0040 DAE-C22).
    pub tunable_params: std::collections::HashSet<String>,
    /// Non-evaluable parameters (MLS 3.7 section 4.5): `fixed = false` or
    /// `Evaluate = false`. No branch is selected at translation on them.
    pub non_evaluable_params: std::collections::HashSet<String>,
    /// User-defined function definitions for compile-time evaluation (MLS §12.3).
    /// Functions are looked up by qualified name during constant expression evaluation.
    pub functions: rustc_hash::FxHashMap<String, Function>,
    /// Static array result shapes keyed by their exact source declaration.
    pub(crate) function_result_shapes: crate::function_precollect::FunctionResultShapes,
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
    /// Resolve identities of package classes, including package aliases. A
    /// constant reference spelled through one of them names the package that
    /// exposes the constant (MLS §7.1).
    pub package_def_ids: std::sync::Arc<rustc_hash::FxHashSet<rumoca_core::DefId>>,
    /// Canonical class scope path for the class instance currently being flattened.
    /// Derived from `def_map` via the owning class DefId.
    pub current_class_scope_path: Option<String>,
    /// Exact class occurrence currently being flattened. This semantic identity is
    /// paired with declaration identity when consuming occurrence-local proofs.
    pub current_class_instance_id: Option<rumoca_core::InstanceId>,
    /// Unqualified name of the simulated root model/block for MLS getInstanceName().
    pub simulated_root_name: Option<String>,
    /// Mirror of `FlattenOptions::materialize_structured_families`. When false, a
    /// regular elementwise for-family materializes only its corner cells (base + one
    /// neighbor per binder) with full bodies; interior cells get a cheap placeholder
    /// body and the family is marked `interiors_materialized = false` so downstream
    /// phases reconstruct interior incidence/strides from the corners.
    pub materialize_structured_families: bool,
    /// Checked occurrence-scoped evidence for parameter-variability families. Its
    /// private representation prevents equation lowering from manufacturing a proof
    /// from display names.
    pub param_variability_families: crate::param_variability::ParameterVariabilityFamilies,
}
