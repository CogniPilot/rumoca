//! Structural public-surface ratchet for success-bearing IR roots.
//!
//! This inspects Rust visibility and construction authority only. It never
//! evaluates, reconstructs, or validates a model.
//! Typed external/wire decoding may replay the root's sole constructor once;
//! that trust-boundary adapter is intentionally not classified as a second
//! validator by this surface scan.
//! Carrier discovery is a conservative crate-wide lexical-name closure. Name
//! collisions may over-report routes; only compiler assertions below are type
//! resolution proofs.

mod compiler_proofs;
mod type_analysis;

use std::collections::BTreeSet;
use std::path::Path;

use quote::ToTokens;
use syn::{
    Fields, FnArg, ForeignItem, GenericArgument, ImplItem, Item, PathArguments, ReturnType,
    TraitItem, Type, Visibility, punctuated::Punctuated,
};

use crate::architecture_hardening_support::{
    ProductionRustSourceContext, attributes_require_test, production_rust_source_contexts,
    workspace_crate_dirs, workspace_root,
};

use super::MIGRATION_NOTICE;
use type_analysis::{
    generic_root_capabilities, type_callback_mutates_root, type_contains_mutable_capability,
    type_contains_reference, type_is_root_factory, type_mentions_names,
    type_param_bound_mentions_names,
};

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(super) struct RootSurface {
    pub(super) public_fields: BTreeSet<String>,
    pub(super) derived_traits: BTreeSet<String>,
    pub(super) trait_impls: BTreeSet<String>,
    pub(super) default_authorities: BTreeSet<String>,
    pub(super) deserialization_authorities: usize,
    pub(super) serde_attributes: usize,
    pub(super) conversion_constructors: usize,
    pub(super) deref: usize,
    pub(super) public_new: usize,
    pub(super) public_validate: usize,
    pub(super) public_mutators: usize,
    pub(super) deref_mut: usize,
    pub(super) public_result_construct: usize,
    pub(super) public_root_producers: BTreeSet<String>,
    pub(super) public_mutable_projections: BTreeSet<String>,
    pub(super) public_consuming_extractions: BTreeSet<String>,
    pub(super) public_semantic_checks: BTreeSet<String>,
    /// Exact normalized signatures of every syntactically root-relevant route
    /// with public or restricted visibility in the owning crate.
    pub(super) routes: BTreeSet<RouteSignature>,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(super) enum RouteKind {
    BorrowsRoot,
    CarrierDefinition,
    ConsumesRoot,
    Decode,
    DerivedCapability,
    Factory,
    GenericConstraint,
    MutableProjection,
    MutableTrait,
    MutatesRoot,
    ProducesRoot,
    Reexport,
    RootField,
    SemanticCheckCandidate,
    SerdeAttribute,
    TraitCapability,
    UnexpandedMacroBoundary,
    UnresolvedGlobReexportBoundary,
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(super) struct RouteSignature {
    pub(super) identity: String,
    pub(super) signature: String,
    pub(super) kinds: BTreeSet<RouteKind>,
    disposition: RouteDisposition,
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum RouteDisposition {
    Checked,
    Sealed,
    MigrationDebt,
}

const _: [RouteDisposition; 3] = [
    RouteDisposition::Checked,
    RouteDisposition::Sealed,
    RouteDisposition::MigrationDebt,
];

#[derive(Clone, Copy, Debug)]
struct ClassifiedRoute {
    identity: &'static str,
    signature: &'static str,
    kinds: &'static [RouteKind],
    disposition: RouteDisposition,
    catalog_id: &'static str,
}

enum RouteCatalogExpectation {
    FrozenDigest(&'static str),
    ExactOverrides,
}

struct RootSpec {
    crate_name: &'static str,
    declaration_module: &'static str,
    root: &'static str,
    expected_public_fields: &'static [&'static str],
    expected_derived_traits: &'static [&'static str],
    expected_trait_impls: &'static [&'static str],
    expected_default_authorities: &'static [&'static str],
    deserialization_authorities: usize,
    serde_attributes: usize,
    conversion_constructors: usize,
    deref: usize,
    public_new: bool,
    public_validate: bool,
    public_mutators: usize,
    deref_mut: bool,
    public_result_construct: bool,
    expected_public_root_producers: &'static [&'static str],
    expected_public_mutable_projections: &'static [&'static str],
    expected_public_consuming_extractions: &'static [&'static str],
    expected_public_semantic_checks: &'static [&'static str],
    route_overrides: &'static [ClassifiedRoute],
    expected_route_catalog: RouteCatalogExpectation,
    expected_disposition_counts: [usize; 3],
    scan_private_routes: bool,
    route_catalog_id: &'static str,
}

const ROOTS: &[RootSpec] = &[
    RootSpec {
        crate_name: "rumoca-phase-resolve",
        declaration_module: "",
        root: "ResolvedTreeProjection",
        expected_public_fields: &[],
        expected_derived_traits: &["Clone", "Debug"],
        expected_trait_impls: &[],
        expected_default_authorities: &[],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: false,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &[],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[],
        route_overrides: &[
            ClassifiedRoute {
                identity: "rumoca-phase-resolve::<ResolvedTree>::<inherent>::project_for_typecheck",
                signature: "pub fn project_for_typecheck (& self) -> ResolvedTreeProjection",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-RESOLVE-TREE-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-resolve::ResolvedTreeProjection",
                signature: "# [derive (Debug , Clone)] pub struct ResolvedTreeProjection { tree : Arc < ClassTree > , }",
                kinds: &[RouteKind::CarrierDefinition],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-RESOLVE-TREE-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-resolve::ResolvedTreeProjection::<derive:Clone>",
                signature: "derive Clone",
                kinds: &[RouteKind::DerivedCapability],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-RESOLVE-TREE-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-resolve::ResolvedTreeProjection::<derive:Debug>",
                signature: "derive Debug",
                kinds: &[RouteKind::DerivedCapability],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-RESOLVE-TREE-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-resolve::<ResolvedTreeProjection>::<inherent>::inner",
                signature: "pub fn inner (& self) -> & ClassTree",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-RESOLVE-TREE-PROJECTION",
            },
        ],
        expected_route_catalog: RouteCatalogExpectation::ExactOverrides,
        expected_disposition_counts: [2, 3, 0],
        scan_private_routes: true,
        route_catalog_id: "SPEC_0043/ROOT-RESOLVE-TREE-PROJECTION",
    },
    RootSpec {
        crate_name: "rumoca-phase-typecheck",
        declaration_module: "typed_instanced",
        root: "TypedInstancedTree",
        expected_public_fields: &[],
        expected_derived_traits: &["Debug"],
        expected_trait_impls: &[],
        expected_default_authorities: &[],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: false,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &[],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[],
        route_overrides: &[
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::instanced::<TypeChecker>::<inherent>::check_instanced",
                signature: "pub (super) fn check_instanced (self , resolved : & ResolvedTree , mut overlay : InstanceOverlay , model_name : & str ,) -> Result < crate :: TypedInstancedTree , Diagnostics >",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedInstancedTree>::<inherent>::mint",
                signature: "pub (crate) fn mint (resolved : ResolvedTreeProjection , overlay : InstanceOverlay , model_name : String ,) -> Self",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typechecker::api::typecheck_instanced_tree",
                signature: "pub fn typecheck_instanced_tree (resolved : & ResolvedTree , overlay : InstanceOverlay , model_name : & str ,) -> Result < crate :: TypedInstancedTree , Diagnostics >",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::TypedInstancedTree",
                signature: "# [derive (Debug)] pub struct TypedInstancedTree { resolved : ResolvedTreeProjection , overlay : Arc < InstanceOverlay > , model_name : String , }",
                kinds: &[RouteKind::CarrierDefinition],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::TypedInstancedTree::<derive:Debug>",
                signature: "derive Debug",
                kinds: &[RouteKind::DerivedCapability],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedInstancedTree>::<inherent>::resolved_tree",
                signature: "pub fn resolved_tree (& self) -> & rumoca_ir_ast :: ClassTree",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedInstancedTree>::<inherent>::overlay",
                signature: "pub fn overlay (& self) -> & InstanceOverlay",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedInstancedTree>::<inherent>::model_name",
                signature: "pub fn model_name (& self) -> & str",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedInstancedTree>::<inherent>::shared_projection",
                signature: "pub fn shared_projection (& self) -> TypedOverlayProjection",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::use:typed_instanced :: { TypedInstancedTree , TypedOverlayProjection }",
                signature: "pub use typed_instanced :: { TypedInstancedTree , TypedOverlayProjection } ;",
                kinds: &[RouteKind::Reexport],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
            },
        ],
        expected_route_catalog: RouteCatalogExpectation::ExactOverrides,
        expected_disposition_counts: [4, 6, 0],
        scan_private_routes: true,
        route_catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    RootSpec {
        crate_name: "rumoca-phase-typecheck",
        declaration_module: "typed_instanced",
        root: "TypedOverlayProjection",
        expected_public_fields: &[],
        expected_derived_traits: &["Clone", "Debug"],
        expected_trait_impls: &[],
        expected_default_authorities: &[],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: false,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &[],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[],
        route_overrides: &[
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedInstancedTree>::<inherent>::shared_projection",
                signature: "pub fn shared_projection (& self) -> TypedOverlayProjection",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::TypedOverlayProjection",
                signature: "# [derive (Debug , Clone)] pub struct TypedOverlayProjection { overlay : Arc < InstanceOverlay > , model_name : String , }",
                kinds: &[RouteKind::CarrierDefinition],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::TypedOverlayProjection::<derive:Clone>",
                signature: "derive Clone",
                kinds: &[RouteKind::DerivedCapability],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::TypedOverlayProjection::<derive:Debug>",
                signature: "derive Debug",
                kinds: &[RouteKind::DerivedCapability],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedOverlayProjection>::<inherent>::overlay",
                signature: "pub fn overlay (& self) -> & InstanceOverlay",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::typed_instanced::<TypedOverlayProjection>::<inherent>::model_name",
                signature: "pub fn model_name (& self) -> & str",
                kinds: &[RouteKind::BorrowsRoot],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
            ClassifiedRoute {
                identity: "rumoca-phase-typecheck::use:typed_instanced :: { TypedInstancedTree , TypedOverlayProjection }",
                signature: "pub use typed_instanced :: { TypedInstancedTree , TypedOverlayProjection } ;",
                kinds: &[RouteKind::Reexport],
                disposition: RouteDisposition::Sealed,
                catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
            },
        ],
        expected_route_catalog: RouteCatalogExpectation::ExactOverrides,
        expected_disposition_counts: [2, 5, 0],
        scan_private_routes: true,
        route_catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-OVERLAY-PROJECTION",
    },
    RootSpec {
        crate_name: "rumoca-ir-ast",
        declaration_module: "instance",
        root: "InstanceOverlay",
        expected_public_fields: &[
            "array_parent_dims",
            "classes",
            "class_type",
            "components",
            "disabled_components",
            "each_modifier_bindings",
            "effective_types",
            "enumeration_type_roots",
            "enumeration_types",
            "inner_outer_to_parent_inner",
            "is_partial",
            "outer_prefix_to_inner",
            "root_description",
            "synthesized_inners",
            "type_ids_by_def_id",
            "type_roots",
        ],
        expected_derived_traits: &["Clone", "Debug", "Default"],
        expected_trait_impls: &[],
        expected_default_authorities: &["derive"],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: true,
        public_validate: false,
        public_mutators: 6,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &["new"],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[
            "finalize_effective_type_publication",
            "finalize_overconstrained_record_owners",
        ],
        route_overrides: &[],
        expected_route_catalog: RouteCatalogExpectation::FrozenDigest(
            "72d1a3de65035c055fdc27f3c6fbb6328f79ddd9f79ba4607050302e8548b042",
        ),
        expected_disposition_counts: [0, 21, 33],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-AST-INSTANCE-OVERLAY",
    },
    RootSpec {
        crate_name: "rumoca-ir-ast",
        declaration_module: "instance",
        root: "InstancedTree",
        expected_public_fields: &["overlay", "tree"],
        expected_derived_traits: &["Clone", "Debug"],
        expected_trait_impls: &["std :: ops :: Deref", "std :: ops :: DerefMut"],
        expected_default_authorities: &[],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 1,
        public_new: true,
        public_validate: false,
        public_mutators: 1,
        deref_mut: true,
        public_result_construct: false,
        expected_public_root_producers: &["new"],
        expected_public_mutable_projections: &[
            "<InstancedTree as std :: ops :: DerefMut>::deref_mut",
        ],
        expected_public_consuming_extractions: &["into_inner"],
        expected_public_semantic_checks: &[],
        route_overrides: &[],
        expected_route_catalog: RouteCatalogExpectation::FrozenDigest(
            "24a50034412014f808865875e369f36e5a3533d698316d83e44c0e9336d2ce94",
        ),
        expected_disposition_counts: [0, 7, 8],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-AST-INSTANCED",
    },
    RootSpec {
        crate_name: "rumoca-ir-flat",
        declaration_module: "",
        root: "Model",
        expected_public_fields: &[
            "algorithms",
            "assert_equations",
            "branches",
            "class_type",
            "definite_roots",
            "effective_types",
            "enum_literal_ordinals",
            "enumeration_type_roots",
            "enumeration_types",
            "equations",
            "functions",
            "initial_algorithms",
            "initial_assert_equations",
            "initial_equations",
            "initial_structured_equations",
            "instance_relations",
            "is_partial",
            "model_description",
            "oc_break_edge_scalar_count",
            "optional_edges",
            "potential_roots",
            "predefined_string_declaration",
            "predefined_types",
            "record_instances",
            "record_types",
            "structured_equations",
            "top_level_connectors",
            "top_level_input_components",
            "type_ids_by_def_id",
            "type_roots",
            "variable_final_flags",
            "variable_type_names",
            "variables",
            "when_chains",
        ],
        expected_derived_traits: &["Clone", "Debug", "Default", "Serialize"],
        expected_trait_impls: &["Deserialize < 'de >"],
        expected_default_authorities: &["derive"],
        deserialization_authorities: 1,
        serde_attributes: 16,
        conversion_constructors: 0,
        deref: 0,
        public_new: true,
        public_validate: true,
        public_mutators: 8,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &["<Model as Deserialize < 'de >>::deserialize", "new"],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[
            "finalize_effective_type_shapes",
            "validate",
            "validate_shape_contract",
        ],
        route_overrides: &[],
        expected_route_catalog: RouteCatalogExpectation::FrozenDigest(
            "7499f2e3eb798845966d56b5286fb9eda8dc75e4429694ba28f53f7a237be486",
        ),
        expected_disposition_counts: [0, 17, 81],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-FLAT-MODEL",
    },
    RootSpec {
        crate_name: "rumoca-ir-dae",
        declaration_module: "model",
        root: "Dae",
        expected_public_fields: &[],
        expected_derived_traits: &["Debug", "Serialize"],
        expected_trait_impls: &["Deserialize < 'de >"],
        expected_default_authorities: &[],
        deserialization_authorities: 1,
        serde_attributes: 1,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: false,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: true,
        expected_public_root_producers: &["<Dae as Deserialize < 'de >>::deserialize", "construct"],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[],
        route_overrides: &[
            ClassifiedRoute {
                identity: "rumoca-ir-dae::model::<Dae>::<inherent>::construct",
                signature: "pub fn construct < F > (source_map : SourceMap , build : F) -> Result < Self , DaeConstructionError > where F : for < 'dae > FnOnce (& mut DaeConstruction < 'dae >) -> Result < () , DaeConstructionError > ,",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-DAE",
            },
            ClassifiedRoute {
                identity: "rumoca-ir-dae::model::wire::<Dae as Deserialize < 'de >>",
                signature: "impl < 'de >Deserialize < 'de > for Dae",
                kinds: &[
                    RouteKind::Decode,
                    RouteKind::ProducesRoot,
                    RouteKind::TraitCapability,
                ],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-DAE",
            },
            ClassifiedRoute {
                identity: "rumoca-ir-dae::model::wire::<Dae as Deserialize < 'de >>::deserialize",
                signature: "fn deserialize < D > (deserializer : D) -> Result < Self , D :: Error > where D : serde :: Deserializer < 'de > ,",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-DAE",
            },
        ],
        expected_route_catalog: RouteCatalogExpectation::FrozenDigest(
            "d8ecde0b926f6a5a3a834d233753285b341accfd74a1d9e286b58bd8ba86663f",
        ),
        expected_disposition_counts: [3, 12, 99],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-DAE",
    },
    RootSpec {
        crate_name: "rumoca-ir-solve",
        declaration_module: "",
        root: "SolveProblem",
        expected_public_fields: &[],
        expected_derived_traits: &["Clone", "Debug", "Serialize"],
        expected_trait_impls: &["Deserialize < 'de >"],
        expected_default_authorities: &[],
        deserialization_authorities: 1,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: false,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: true,
        expected_public_root_producers: &[
            "<SolveProblem as Deserialize < 'de >>::deserialize",
            "construct",
        ],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &[],
        route_overrides: &[
            ClassifiedRoute {
                identity: "rumoca-ir-solve::<SolveProblem>::<inherent>::construct",
                signature: "pub fn construct (layout : VarLayout , solve_layout : SolveLayout , continuous : ContinuousSolveSystem , initialization : InitializationSolveSystem , discrete : DiscreteSolveSystem , events : SolveEventPartition , clocks : SolveClockPartition ,) -> Result < Self , SolveProblemShapeContractError >",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-SOLVE-PROBLEM",
            },
            ClassifiedRoute {
                identity: "rumoca-ir-solve::<SolveProblem as Deserialize < 'de >>",
                signature: "impl < 'de >Deserialize < 'de > for SolveProblem",
                kinds: &[
                    RouteKind::Decode,
                    RouteKind::ProducesRoot,
                    RouteKind::TraitCapability,
                ],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-SOLVE-PROBLEM",
            },
            ClassifiedRoute {
                identity: "rumoca-ir-solve::<SolveProblem as Deserialize < 'de >>::deserialize",
                signature: "fn deserialize < D > (deserializer : D) -> Result < Self , D :: Error > where D : serde :: Deserializer < 'de > ,",
                kinds: &[RouteKind::ProducesRoot],
                disposition: RouteDisposition::Checked,
                catalog_id: "SPEC_0043/ROOT-SOLVE-PROBLEM",
            },
        ],
        expected_route_catalog: RouteCatalogExpectation::FrozenDigest(
            "9ecbe92a140a8b2813eddf45d9dadd08f10199498b9022c96e76ed59f038307d",
        ),
        expected_disposition_counts: [3, 64, 24],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-SOLVE-PROBLEM",
    },
    RootSpec {
        crate_name: "rumoca-ir-solve",
        declaration_module: "model",
        root: "SolveModel",
        expected_public_fields: &[
            "artifacts",
            "initial_y",
            "parameters",
            "problem",
            "pure_calls",
            "solver_nominals",
            "variable_catalog",
            "visible_names",
            "visible_value_rows",
        ],
        expected_derived_traits: &["Clone", "Debug"],
        expected_trait_impls: &[],
        expected_default_authorities: &[],
        deserialization_authorities: 0,
        serde_attributes: 0,
        conversion_constructors: 0,
        deref: 0,
        public_new: false,
        public_validate: true,
        public_mutators: 0,
        deref_mut: false,
        public_result_construct: false,
        expected_public_root_producers: &["empty", "resolved_periodic_schedules_at"],
        expected_public_mutable_projections: &[],
        expected_public_consuming_extractions: &[],
        expected_public_semantic_checks: &["validate"],
        route_overrides: &[],
        expected_route_catalog: RouteCatalogExpectation::FrozenDigest(
            "48beb6d9d3ee0b1d23f9130f71aa3558ff192143861faada650c4bacf052033a",
        ),
        expected_disposition_counts: [0, 41, 29],
        scan_private_routes: false,
        route_catalog_id: "SPEC_0043/ROOT-SOLVE-MODEL",
    },
];

#[test]
fn compiler_proves_exact_negative_root_capabilities() {
    // The module-level assertions are compiler obligations. This named test
    // keeps the exact, deliberately non-exhaustive trait frontier discoverable
    // in filtered architecture output.
}

#[test]
fn test_success_root_public_proof_escapes_do_not_expand() {
    let workspace = workspace_root();
    let mut differences = Vec::new();
    for spec in ROOTS {
        let crate_root = workspace.join("crates").join(spec.crate_name);
        let actual = production_root_surface(&crate_root, &workspace, spec);
        differences.extend(route_catalog_differences(spec, &actual.routes));
        let mut expected = expected_surface(spec);
        expected.routes = actual.routes.clone();
        if actual != expected {
            differences.push(format!(
                "  {}::{} public proof surface changed\n    expected: {expected:#?}\n    measured: {actual:#?}",
                spec.crate_name, spec.root
            ));
        }
    }
    assert!(
        differences.is_empty(),
        "success-root proof escapes changed:\n{}\n\n{MIGRATION_NOTICE}",
        differences.join("\n")
    );
}

/// Scan the actual Typecheck proof owners and report any route outside their
/// closed ROOT-* ownership catalogs.
pub(super) fn closed_frontend_proof_owner_differences() -> Vec<String> {
    let workspace = workspace_root();
    let mut differences = ROOTS
        .iter()
        .filter(|spec| {
            matches!(
                spec.root,
                "ResolvedTreeProjection" | "TypedInstancedTree" | "TypedOverlayProjection"
            )
        })
        .flat_map(|spec| {
            let crate_root = workspace.join("crates").join(spec.crate_name);
            let actual = production_root_surface(&crate_root, &workspace, spec);
            let mut differences = route_catalog_differences(spec, &actual.routes);
            let mut expected = expected_surface(spec);
            expected.routes = actual.routes.clone();
            if actual != expected {
                differences.push(format!(
                    "  {}::{} public proof surface changed\n    expected: {expected:#?}\n    measured: {actual:#?}",
                    spec.crate_name, spec.root
                ));
            }
            differences
        })
        .collect::<Vec<_>>();
    differences.extend(typed_proof_downstream_differences(&workspace));
    differences
}

const TYPED_PROOF_DOWNSTREAM_CATALOG: &[ClassifiedRoute] = &[
    ClassifiedRoute {
        identity: "rumoca-compile::session::TypedModelOutcome",
        signature: "# [derive (Debug)] enum TypedModelOutcome { Success (rumoca_phase_typecheck :: TypedInstancedTree) , NeedsInner { missing_inners : Vec < String > , missing_spans : Vec < Span > , } , InstantiateError (Box < InstantiateError >) , TypecheckError (Vec < CommonDiagnostic >) , }",
        kinds: &[RouteKind::CarrierDefinition],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-compile::session::<TypedModelOutcome>::<inherent>::record",
        signature: "fn record (& self) -> TypedModelRecord",
        kinds: &[RouteKind::BorrowsRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-compile::session::<TypedModelRecord>::<inherent>::into_failure_outcome",
        signature: "fn into_failure_outcome (self) -> Option < TypedModelOutcome >",
        kinds: &[RouteKind::ProducesRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-compile::session::compile_support::typed_model_outcome_from_instantiated",
        signature: "pub (super) fn typed_model_outcome_from_instantiated (resolved : & ResolvedTree , model_name : & str , instantiate_outcome : InstantiatedModelOutcome ,) -> (TypedModelOutcome , bool)",
        kinds: &[RouteKind::ProducesRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-compile::session::compile_support::flat_model_outcome_from_typed",
        signature: "pub (super) fn flat_model_outcome_from_typed (typed_outcome : TypedModelOutcome ,) -> (FlatModelOutcome , bool)",
        kinds: &[RouteKind::ConsumesRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-compile::session::session_impl_model_queries::<Session>::<inherent>::typed_model_outcome_build",
        signature: "fn typed_model_outcome_build (& mut self , resolved : & ResolvedTree , mode : ResolveBuildMode , model_name : & str , record_compile_timings : bool ,) -> TypedModelOutcome",
        kinds: &[RouteKind::ProducesRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-compile::session::session_impl_model_queries::<Session>::<inherent>::typed_model_query_impl",
        signature: "fn typed_model_query_impl (& mut self , resolved : & ResolvedTree , mode : ResolveBuildMode , model_name : & str , record_compile_timings : bool ,) -> TypedModelOutcome",
        kinds: &[RouteKind::ProducesRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
    ClassifiedRoute {
        identity: "rumoca-phase-flatten::flatten_typed",
        signature: "pub fn flatten_typed (typed : rumoca_phase_typecheck :: TypedInstancedTree , options : FlattenOptions ,) -> Result < flat :: Model , FlattenError >",
        kinds: &[RouteKind::ConsumesRoot],
        disposition: RouteDisposition::Checked,
        catalog_id: "SPEC_0043/ROOT-TYPECHECK-TYPED-INSTANCED",
    },
];

fn typed_proof_downstream_differences(workspace: &Path) -> Vec<String> {
    let mut actual = BTreeSet::new();
    for crate_root in workspace_crate_dirs(workspace) {
        let Some(crate_name) = crate_root.file_name().and_then(|name| name.to_str()) else {
            continue;
        };
        if crate_name == "rumoca-phase-typecheck" {
            continue;
        }
        let contexts = production_rust_source_contexts(&crate_root, workspace);
        let mut spec = fixture_root_spec("TypedInstancedTree");
        spec.crate_name = Box::leak(crate_name.to_owned().into_boxed_str());
        spec.scan_private_routes = true;
        actual.extend(collect_route_inventory(&contexts, &spec));
    }
    let expected = TYPED_PROOF_DOWNSTREAM_CATALOG
        .iter()
        .map(|route| RouteSignature {
            identity: route.identity.to_owned(),
            signature: route.signature.to_owned(),
            kinds: route.kinds.iter().copied().collect(),
            disposition: route.disposition,
        })
        .collect::<BTreeSet<_>>();
    actual = actual
        .into_iter()
        .map(|mut route| {
            if expected.iter().any(|candidate| {
                candidate.identity == route.identity
                    && candidate.signature == route.signature
                    && candidate.kinds == route.kinds
            }) {
                route.disposition = RouteDisposition::Checked;
            }
            route
        })
        .collect();
    if actual == expected {
        Vec::new()
    } else {
        vec![format!(
            "  TypedInstancedTree workspace production routes changed\n    expected digest: {}\n    measured digest: {}\n    expected: {expected:#?}\n    measured: {actual:#?}",
            route_digest(&expected),
            route_digest(&actual),
        )]
    }
}

fn expected_surface(spec: &RootSpec) -> RootSurface {
    RootSurface {
        public_fields: spec
            .expected_public_fields
            .iter()
            .map(|field| (*field).to_string())
            .collect(),
        derived_traits: string_set(spec.expected_derived_traits),
        trait_impls: string_set(spec.expected_trait_impls),
        default_authorities: spec
            .expected_default_authorities
            .iter()
            .map(|authority| (*authority).to_string())
            .collect(),
        deserialization_authorities: spec.deserialization_authorities,
        serde_attributes: spec.serde_attributes,
        conversion_constructors: spec.conversion_constructors,
        deref: spec.deref,
        public_new: usize::from(spec.public_new),
        public_validate: usize::from(spec.public_validate),
        public_mutators: spec.public_mutators,
        deref_mut: usize::from(spec.deref_mut),
        public_result_construct: usize::from(spec.public_result_construct),
        public_root_producers: string_set(spec.expected_public_root_producers),
        public_mutable_projections: string_set(spec.expected_public_mutable_projections),
        public_consuming_extractions: string_set(spec.expected_public_consuming_extractions),
        public_semantic_checks: string_set(spec.expected_public_semantic_checks),
        routes: BTreeSet::new(),
    }
}

fn string_set(values: &[&str]) -> BTreeSet<String> {
    values.iter().map(|value| (*value).to_owned()).collect()
}

fn route_catalog_differences(spec: &RootSpec, routes: &BTreeSet<RouteSignature>) -> Vec<String> {
    assert!(
        spec.route_catalog_id.starts_with("SPEC_0043/ROOT-"),
        "{}::{} has no SPEC_0043 route catalog id",
        spec.crate_name,
        spec.root
    );
    let digest = route_digest(routes);
    let counts = disposition_counts(routes);
    let expected_digest = match &spec.expected_route_catalog {
        RouteCatalogExpectation::FrozenDigest(digest) => (*digest).to_owned(),
        RouteCatalogExpectation::ExactOverrides => {
            let expected_routes = spec
                .route_overrides
                .iter()
                .map(|route| RouteSignature {
                    identity: route.identity.to_owned(),
                    signature: route.signature.to_owned(),
                    kinds: route.kinds.iter().copied().collect(),
                    disposition: route.disposition,
                })
                .collect::<BTreeSet<_>>();
            assert_eq!(
                expected_routes.len(),
                spec.route_overrides.len(),
                "{}::{} exact catalog has duplicate routes",
                spec.crate_name,
                spec.root
            );
            route_digest(&expected_routes)
        }
    };
    if digest == expected_digest && counts == spec.expected_disposition_counts {
        return Vec::new();
    }
    vec![format!(
        "  {}::{} exact route catalog changed ({})\n    expected digest/counts: {} {:?}\n    measured digest/counts: {digest} {counts:?}\n    measured routes: {routes:#?}",
        spec.crate_name,
        spec.root,
        spec.route_catalog_id,
        expected_digest,
        spec.expected_disposition_counts,
    )]
}

fn route_digest(routes: &BTreeSet<RouteSignature>) -> String {
    let mut normalized = String::new();
    for route in routes {
        normalized.push_str(&route.identity);
        normalized.push('\n');
        normalized.push_str(&route.signature);
        normalized.push('\n');
        normalized.push_str(&format!("{:?}\n{:?}\n", route.kinds, route.disposition));
    }
    blake3::hash(normalized.as_bytes()).to_hex().to_string()
}

fn disposition_counts(routes: &BTreeSet<RouteSignature>) -> [usize; 3] {
    let mut counts = [0usize; 3];
    for route in routes {
        let index = match route.disposition {
            RouteDisposition::Checked => 0,
            RouteDisposition::Sealed => 1,
            RouteDisposition::MigrationDebt => 2,
        };
        counts[index] += 1;
    }
    counts
}

#[derive(Clone)]
struct NamedTypeDefinition {
    name: String,
    component_types: Vec<Type>,
}

fn collect_route_inventory(
    contexts: &[ProductionRustSourceContext],
    spec: &RootSpec,
) -> BTreeSet<RouteSignature> {
    let parsed = contexts
        .iter()
        .map(|context| {
            let syntax = syn::parse_file(&context.source).unwrap_or_else(|error| {
                panic!(
                    "parse {} for exact root-route inventory: {error}",
                    context.path.display()
                )
            });
            (context, syntax)
        })
        .collect::<Vec<_>>();
    let mut definitions = Vec::new();
    for (context, syntax) in &parsed {
        collect_named_type_definitions(&syntax.items, &context.module_path, &mut definitions);
    }
    let carriers = carrier_names(spec.root, &definitions);
    let mut routes = BTreeSet::new();
    for (context, syntax) in parsed {
        collect_item_routes(
            &syntax.items,
            &context.module_path,
            spec,
            &carriers,
            &mut routes,
        );
    }
    classify_routes(routes, spec)
}

fn classify_routes(routes: BTreeSet<RouteSignature>, spec: &RootSpec) -> BTreeSet<RouteSignature> {
    let mut seen_overrides = BTreeSet::new();
    let classified = routes
        .into_iter()
        .map(|mut route| {
            let matching = spec.route_overrides.iter().filter(|expected| {
                expected.identity == route.identity && expected.signature == route.signature
            });
            for expected in matching {
                assert_eq!(
                    expected.kinds.iter().copied().collect::<BTreeSet<_>>(),
                    route.kinds,
                    "{} override kinds drifted",
                    expected.identity
                );
                assert_eq!(
                    expected.catalog_id, spec.route_catalog_id,
                    "{} override points at the wrong SPEC_0043 row",
                    expected.identity
                );
                assert!(
                    seen_overrides.insert(expected.identity),
                    "duplicate route override for {}",
                    expected.identity
                );
                route.disposition = expected.disposition;
            }
            if !seen_overrides.contains(route.identity.as_str()) {
                route.disposition = structural_route_disposition(&route);
            }
            route
        })
        .collect::<BTreeSet<_>>();
    assert_eq!(
        seen_overrides.len(),
        spec.route_overrides.len(),
        "{}::{} has stale/deleted exact route overrides",
        spec.crate_name,
        spec.root
    );
    classified
}

fn structural_route_disposition(route: &RouteSignature) -> RouteDisposition {
    let sealed_kinds = BTreeSet::from([
        RouteKind::BorrowsRoot,
        RouteKind::DerivedCapability,
        RouteKind::Reexport,
        RouteKind::TraitCapability,
    ]);
    if route.kinds.is_subset(&sealed_kinds)
        && !route.identity.contains("derive:Default")
        && !route.identity.contains("derive:Deserialize")
        && !route.identity.contains("Deserialize")
    {
        RouteDisposition::Sealed
    } else {
        RouteDisposition::MigrationDebt
    }
}

fn collect_named_type_definitions(
    items: &[Item],
    module_path: &[String],
    definitions: &mut Vec<NamedTypeDefinition>,
) {
    for item in items {
        match item {
            Item::Type(alias) if !attributes_require_test(&alias.attrs) => {
                definitions.push(NamedTypeDefinition {
                    name: alias.ident.to_string(),
                    component_types: vec![alias.ty.as_ref().clone()],
                });
            }
            Item::Struct(structure) if !attributes_require_test(&structure.attrs) => {
                definitions.push(NamedTypeDefinition {
                    name: structure.ident.to_string(),
                    component_types: fields_types(&structure.fields),
                });
            }
            Item::Enum(enumeration) if !attributes_require_test(&enumeration.attrs) => {
                definitions.push(NamedTypeDefinition {
                    name: enumeration.ident.to_string(),
                    component_types: enumeration
                        .variants
                        .iter()
                        .flat_map(|variant| fields_types(&variant.fields))
                        .collect(),
                });
            }
            Item::Union(union) if !attributes_require_test(&union.attrs) => {
                definitions.push(NamedTypeDefinition {
                    name: union.ident.to_string(),
                    component_types: union
                        .fields
                        .named
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect(),
                });
            }
            Item::Use(item_use) if !attributes_require_test(&item_use.attrs) => {
                collect_use_alias_definitions(&item_use.tree, &mut Vec::new(), definitions);
            }
            Item::Mod(nested) if !attributes_require_test(&nested.attrs) => {
                let Some((_, nested_items)) = &nested.content else {
                    continue;
                };
                let mut nested_module = module_path.to_vec();
                nested_module.push(nested.ident.to_string());
                collect_named_type_definitions(nested_items, &nested_module, definitions);
            }
            _ => {}
        }
    }
}

fn collect_use_alias_definitions(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    definitions: &mut Vec<NamedTypeDefinition>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            collect_use_alias_definitions(&path.tree, prefix, definitions);
            prefix.pop();
        }
        syn::UseTree::Rename(rename) => {
            let mut source = prefix.clone();
            source.push(rename.ident.to_string());
            let ty = syn::parse_str::<Type>(&source.join("::"))
                .expect("a use-renamed type path parses as a type");
            definitions.push(NamedTypeDefinition {
                name: rename.rename.to_string(),
                component_types: vec![ty],
            });
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_use_alias_definitions(item, prefix, definitions);
            }
        }
        syn::UseTree::Glob(_) | syn::UseTree::Name(_) => {}
    }
}

fn fields_types(fields: &Fields) -> Vec<Type> {
    match fields {
        Fields::Named(fields) => fields.named.iter().map(|field| field.ty.clone()).collect(),
        Fields::Unnamed(fields) => fields
            .unnamed
            .iter()
            .map(|field| field.ty.clone())
            .collect(),
        Fields::Unit => Vec::new(),
    }
}

fn carrier_names(root: &str, definitions: &[NamedTypeDefinition]) -> BTreeSet<String> {
    let mut carriers = [root.to_owned()].into_iter().collect::<BTreeSet<_>>();
    loop {
        let additions = definitions
            .iter()
            .filter(|definition| !carriers.contains(&definition.name))
            .filter(|definition| {
                definition
                    .component_types
                    .iter()
                    .any(|ty| type_mentions_names(ty, &carriers))
            })
            .map(|definition| definition.name.clone())
            .collect::<Vec<_>>();
        if additions.is_empty() {
            return carriers;
        }
        carriers.extend(additions);
    }
}

fn collect_named_carrier_item_route(
    item: &Item,
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) -> bool {
    let (attributes, visibility, name, signature) = match item {
        Item::Enum(item) => (
            &item.attrs,
            &item.vis,
            item.ident.to_string(),
            enum_signature(item),
        ),
        Item::Union(item) => (
            &item.attrs,
            &item.vis,
            item.ident.to_string(),
            union_signature(item),
        ),
        Item::Type(item) => (
            &item.attrs,
            &item.vis,
            item.ident.to_string(),
            type_alias_signature(item),
        ),
        _ => return false,
    };
    if !attributes_require_test(attributes)
        && (spec.scan_private_routes || is_visible(visibility))
        && carriers.contains(&name)
    {
        insert_route(
            routes,
            item_identity(spec, module_path, &name),
            signature,
            [RouteKind::CarrierDefinition],
        );
    }
    true
}

fn collect_item_routes(
    items: &[Item],
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    for item in items {
        if collect_named_carrier_item_route(item, module_path, spec, carriers, routes) {
            continue;
        }
        match item {
            Item::Struct(structure) if !attributes_require_test(&structure.attrs) => {
                collect_structure_routes(structure, module_path, spec, carriers, routes);
            }
            Item::Fn(function)
                if !attributes_require_test(&function.attrs)
                    && (spec.scan_private_routes || is_visible(&function.vis)) =>
            {
                collect_signature_route(
                    SignatureRouteContext {
                        identity: item_identity(spec, module_path, &function.sig.ident.to_string()),
                        attributes: &function.attrs,
                        visibility: &function.vis,
                        signature: &function.sig,
                        self_is_carrier: false,
                        trait_path: None,
                    },
                    carriers,
                    routes,
                );
            }
            Item::Impl(implementation) if !attributes_require_test(&implementation.attrs) => {
                collect_whole_impl_routes(implementation, module_path, spec, carriers, routes);
            }
            Item::Trait(item_trait)
                if !attributes_require_test(&item_trait.attrs)
                    && (spec.scan_private_routes || is_visible(&item_trait.vis)) =>
            {
                collect_trait_routes(item_trait, module_path, spec, carriers, routes);
            }
            Item::ForeignMod(foreign) if !attributes_require_test(&foreign.attrs) => {
                collect_foreign_routes(foreign, module_path, spec, carriers, routes);
            }
            Item::Use(item_use)
                if !attributes_require_test(&item_use.attrs)
                    && (spec.scan_private_routes || is_visible(&item_use.vis)) =>
            {
                collect_reexport_route(item_use, module_path, spec, carriers, routes);
            }
            Item::Const(item_const)
                if !attributes_require_test(&item_const.attrs)
                    && (spec.scan_private_routes || is_visible(&item_const.vis))
                    && type_mentions_names(&item_const.ty, carriers) =>
            {
                insert_route(
                    routes,
                    item_identity(spec, module_path, &item_const.ident.to_string()),
                    const_signature(item_const),
                    [RouteKind::ProducesRoot],
                );
            }
            Item::Static(item_static)
                if !attributes_require_test(&item_static.attrs)
                    && (spec.scan_private_routes || is_visible(&item_static.vis))
                    && type_mentions_names(&item_static.ty, carriers) =>
            {
                let mut kinds = BTreeSet::from([RouteKind::ProducesRoot]);
                if matches!(item_static.mutability, syn::StaticMutability::Mut(_)) {
                    kinds.insert(RouteKind::MutatesRoot);
                }
                insert_route_set(
                    routes,
                    item_identity(spec, module_path, &item_static.ident.to_string()),
                    static_signature(item_static),
                    kinds,
                );
            }
            Item::Macro(item_macro) if !attributes_require_test(&item_macro.attrs) => {
                let identity = item_macro.ident.as_ref().map_or_else(
                    || item_macro.mac.path.to_token_stream().to_string(),
                    ToString::to_string,
                );
                collect_root_spelled_macro_route(
                    &item_macro.mac,
                    &item_macro.attrs,
                    item_identity(spec, module_path, &format!("macro:{identity}")),
                    carriers,
                    false,
                    routes,
                );
            }
            Item::Mod(nested) if !attributes_require_test(&nested.attrs) => {
                let Some((_, nested_items)) = &nested.content else {
                    continue;
                };
                let mut nested_module = module_path.to_vec();
                nested_module.push(nested.ident.to_string());
                collect_item_routes(nested_items, &nested_module, spec, carriers, routes);
            }
            _ => {}
        }
    }
}

fn collect_root_spelled_macro_route(
    item_macro: &syn::Macro,
    attributes: &[syn::Attribute],
    identity: String,
    carriers: &BTreeSet<String>,
    associated_with_carrier: bool,
    routes: &mut BTreeSet<RouteSignature>,
) {
    let tokens = item_macro.tokens.to_token_stream().to_string();
    if associated_with_carrier
        || carriers
            .iter()
            .any(|name| token_string_mentions(&tokens, name))
    {
        insert_route(
            routes,
            identity,
            normalized_attributes(attributes) + &item_macro.to_token_stream().to_string(),
            [RouteKind::UnexpandedMacroBoundary],
        );
    }
}

fn collect_foreign_routes(
    foreign: &syn::ItemForeignMod,
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    for item in &foreign.items {
        match item {
            ForeignItem::Fn(function)
                if !attributes_require_test(&function.attrs)
                    && (spec.scan_private_routes || is_visible(&function.vis)) =>
            {
                collect_signature_route(
                    SignatureRouteContext {
                        identity: item_identity(spec, module_path, &function.sig.ident.to_string()),
                        attributes: &function.attrs,
                        visibility: &function.vis,
                        signature: &function.sig,
                        self_is_carrier: false,
                        trait_path: None,
                    },
                    carriers,
                    routes,
                );
            }
            ForeignItem::Static(item_static)
                if !attributes_require_test(&item_static.attrs)
                    && (spec.scan_private_routes || is_visible(&item_static.vis))
                    && type_mentions_names(&item_static.ty, carriers) =>
            {
                let mut kinds = BTreeSet::from([RouteKind::ProducesRoot]);
                if matches!(item_static.mutability, syn::StaticMutability::Mut(_)) {
                    kinds.insert(RouteKind::MutatesRoot);
                }
                insert_route_set(
                    routes,
                    item_identity(spec, module_path, &item_static.ident.to_string()),
                    foreign_static_signature(item_static),
                    kinds,
                );
            }
            _ => {}
        }
    }
}

fn collect_structure_routes(
    structure: &syn::ItemStruct,
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    let name = structure.ident.to_string();
    if (spec.scan_private_routes || is_visible(&structure.vis)) && carriers.contains(&name) {
        insert_route(
            routes,
            item_identity(spec, module_path, &name),
            struct_signature(structure),
            [RouteKind::CarrierDefinition],
        );
    }
    if name != spec.root {
        return;
    }
    for derived in derived_trait_names(&structure.attrs) {
        insert_route(
            routes,
            item_identity(spec, module_path, &format!("{name}::<derive:{derived}>")),
            format!("derive {derived}"),
            [RouteKind::DerivedCapability],
        );
    }
    for (index, field) in structure.fields.iter().enumerate() {
        let field_name = field
            .ident
            .as_ref()
            .map_or_else(|| format!("#{index}"), ToString::to_string);
        if is_visible(&field.vis) {
            insert_route(
                routes,
                item_identity(spec, module_path, &format!("{name}::{field_name}")),
                field_signature(field),
                [RouteKind::RootField],
            );
        }
        for attribute in field
            .attrs
            .iter()
            .filter(|attribute| attribute.path().is_ident("serde"))
        {
            insert_route(
                routes,
                item_identity(spec, module_path, &format!("{name}::{field_name}::<serde>")),
                attribute.to_token_stream().to_string(),
                [RouteKind::SerdeAttribute],
            );
        }
    }
    for (index, attribute) in structure
        .attrs
        .iter()
        .filter(|attribute| attribute.path().is_ident("serde"))
        .enumerate()
    {
        insert_route(
            routes,
            item_identity(spec, module_path, &format!("{name}::<serde:{index}>")),
            attribute.to_token_stream().to_string(),
            [RouteKind::SerdeAttribute],
        );
    }
}

fn collect_whole_impl_routes(
    implementation: &syn::ItemImpl,
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    let self_is_carrier = type_mentions_names(&implementation.self_ty, carriers);
    let trait_path = implementation
        .trait_
        .as_ref()
        .map(|(_, path, _)| path.to_token_stream().to_string());
    if self_is_carrier && trait_path.is_some() {
        let mut kinds = BTreeSet::from([RouteKind::TraitCapability]);
        let trait_name = trait_path.as_deref().unwrap_or_default();
        if ["AsMut", "BorrowMut", "DerefMut", "IndexMut"]
            .iter()
            .any(|name| token_string_mentions(trait_name, name))
        {
            kinds.insert(RouteKind::MutableTrait);
        }
        if ["Deserialize", "DeserializeOwned"]
            .iter()
            .any(|name| token_string_mentions(trait_name, name))
        {
            kinds.insert(RouteKind::Decode);
            kinds.insert(RouteKind::ProducesRoot);
        }
        insert_route_set(
            routes,
            impl_identity(spec, module_path, implementation),
            impl_header_signature(implementation),
            kinds,
        );
    }

    let context = ImplRouteContext {
        implementation,
        module_path,
        spec,
        self_is_carrier,
        trait_path: trait_path.as_deref(),
        carriers,
    };
    for item in &implementation.items {
        collect_impl_item_route(item, &context, routes);
    }
}

struct ImplRouteContext<'a> {
    implementation: &'a syn::ItemImpl,
    module_path: &'a [String],
    spec: &'a RootSpec,
    self_is_carrier: bool,
    trait_path: Option<&'a str>,
    carriers: &'a BTreeSet<String>,
}

fn collect_impl_item_route(
    item: &ImplItem,
    context: &ImplRouteContext<'_>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    match item {
        ImplItem::Fn(method)
            if !attributes_require_test(&method.attrs)
                && (context.spec.scan_private_routes
                    || context.implementation.trait_.is_some()
                    || is_visible(&method.vis)) =>
        {
            collect_signature_route(
                SignatureRouteContext {
                    identity: method_identity(
                        context.spec,
                        context.module_path,
                        context.implementation,
                        &method.sig.ident.to_string(),
                    ),
                    attributes: &method.attrs,
                    visibility: &method.vis,
                    signature: &method.sig,
                    self_is_carrier: context.self_is_carrier,
                    trait_path: context.trait_path,
                },
                context.carriers,
                routes,
            );
        }
        ImplItem::Const(item_const)
            if !attributes_require_test(&item_const.attrs)
                && (context.spec.scan_private_routes
                    || context.implementation.trait_.is_some()
                    || is_visible(&item_const.vis)) =>
        {
            let mentions_root = type_mentions_names(&item_const.ty, context.carriers)
                || (context.self_is_carrier && type_mentions_self(&item_const.ty));
            if mentions_root {
                insert_route(
                    routes,
                    method_identity(
                        context.spec,
                        context.module_path,
                        context.implementation,
                        &format!("<const:{}>", item_const.ident),
                    ),
                    impl_const_signature(item_const),
                    [RouteKind::ProducesRoot],
                );
            }
        }
        ImplItem::Type(item_type)
            if !attributes_require_test(&item_type.attrs)
                && (context.spec.scan_private_routes
                    || context.implementation.trait_.is_some()
                    || is_visible(&item_type.vis)) =>
        {
            let mentions_root = type_mentions_names(&item_type.ty, context.carriers)
                || (context.self_is_carrier && type_mentions_self(&item_type.ty));
            if mentions_root {
                insert_route(
                    routes,
                    method_identity(
                        context.spec,
                        context.module_path,
                        context.implementation,
                        &format!("<type:{}>", item_type.ident),
                    ),
                    impl_type_signature(item_type),
                    [RouteKind::CarrierDefinition],
                );
            }
        }
        ImplItem::Macro(item_macro) if !attributes_require_test(&item_macro.attrs) => {
            collect_root_spelled_macro_route(
                &item_macro.mac,
                &item_macro.attrs,
                method_identity(
                    context.spec,
                    context.module_path,
                    context.implementation,
                    &format!("<macro:{}>", item_macro.mac.path.to_token_stream()),
                ),
                context.carriers,
                context.self_is_carrier,
                routes,
            );
        }
        _ => {}
    }
}

fn collect_trait_routes(
    item_trait: &syn::ItemTrait,
    module_path: &[String],
    spec: &RootSpec,
    carriers: &BTreeSet<String>,
    routes: &mut BTreeSet<RouteSignature>,
) {
    for item in &item_trait.items {
        match item {
            TraitItem::Fn(method) if !attributes_require_test(&method.attrs) => {
                let identity = item_identity(
                    spec,
                    module_path,
                    &format!("<trait {}>::{}", item_trait.ident, method.sig.ident),
                );
                collect_signature_route(
                    SignatureRouteContext {
                        identity,
                        attributes: &method.attrs,
                        visibility: &item_trait.vis,
                        signature: &method.sig,
                        self_is_carrier: false,
                        trait_path: Some(&item_trait.ident.to_string()),
                    },
                    carriers,
                    routes,
                );
            }
            TraitItem::Const(item_const)
                if !attributes_require_test(&item_const.attrs)
                    && type_mentions_names(&item_const.ty, carriers) =>
            {
                insert_route(
                    routes,
                    item_identity(
                        spec,
                        module_path,
                        &format!("<trait {}>::<const:{}>", item_trait.ident, item_const.ident),
                    ),
                    trait_const_signature(item_const),
                    [RouteKind::ProducesRoot],
                );
            }
            TraitItem::Type(item_type) if !attributes_require_test(&item_type.attrs) => {
                let generic_facts = generic_root_capabilities(&item_type.generics, carriers);
                let bounds_mention_root = item_type
                    .bounds
                    .iter()
                    .any(|bound| type_param_bound_mentions_names(bound, carriers));
                let default_mentions_root = item_type
                    .default
                    .as_ref()
                    .is_some_and(|(_, ty)| type_mentions_names(ty, carriers));
                if generic_facts.mentions_root || bounds_mention_root || default_mentions_root {
                    insert_route(
                        routes,
                        item_identity(
                            spec,
                            module_path,
                            &format!("<trait {}>::<type:{}>", item_trait.ident, item_type.ident),
                        ),
                        trait_type_signature(item_type),
                        [RouteKind::CarrierDefinition],
                    );
                }
            }
            TraitItem::Macro(item_macro) if !attributes_require_test(&item_macro.attrs) => {
                collect_root_spelled_macro_route(
                    &item_macro.mac,
                    &item_macro.attrs,
                    item_identity(
                        spec,
                        module_path,
                        &format!(
                            "<trait {}>::<macro:{}>",
                            item_trait.ident,
                            item_macro.mac.path.to_token_stream()
                        ),
                    ),
                    carriers,
                    false,
                    routes,
                );
            }
            _ => {}
        }
    }
}

struct SignatureRouteContext<'a> {
    identity: String,
    attributes: &'a [syn::Attribute],
    visibility: &'a Visibility,
    signature: &'a syn::Signature,
    self_is_carrier: bool,
    trait_path: Option<&'a str>,
}

mod signature_routes;
use signature_routes::*;
pub(super) use signature_routes::{
    fixture_closed_proof_route_inventory, fixture_root_surface, fixture_route_inventory,
};
