//! Reviewed production-owner debt.
//!
//! Rows fingerprint the complete normalized function or method item. They are
//! a review tripwire, not a correctness certificate. The three arrays remain
//! separate because each records a different SPEC_0036 migration obligation.

use super::owner_scan::OwnerDigest;

macro_rules! debt {
    ($identity:literal, $len:literal, $digest:literal) => {
        OwnerDigest {
            identity: $identity,
            normalized_len: $len,
            blake3: $digest,
        }
    };
}

/// Downstream checkers, semantic recovery authorities, and interpretation from
/// display spelling that must disappear when the producing boundary publishes
/// an opaque proof-carrying result.
pub(super) const RECOVERY_AUTHORITY_DEBT: &[OwnerDigest] = &[
    // Whole-root or downstream validators/repairs: these re-establish facts
    // after a success-bearing handoff instead of receiving an opaque root.
    debt!(
        "rumoca-phase-flatten::ast_validation::validate_flatten_input",
        2740,
        "d467bbe598a50613fbcd0974babfe7f36327f28a32e25c8f1b31a516a60decaf"
    ),
    debt!(
        "rumoca-phase-flatten::functions::validate_flat_function_bindings",
        324,
        "4eda8eaa07c5e4d652f38d7307b698a4bc36c61aea1d8b8cb2dbc0de85574c72"
    ),
    debt!(
        "rumoca-ir-flat::Model::validate_shape_contract",
        2313,
        "fdc238918852834932e4db489750be33bd7235d817bfa1e4ab5fd4b064b63807"
    ),
    debt!(
        "rumoca-ir-flat::Model::finalize_effective_type_shapes",
        2071,
        "693a5719edf11064b0388cfdf38b8e1d9408980973e2cd9fd90cb107303a1e18"
    ),
    debt!(
        "rumoca-ir-flat::Model::validate",
        523,
        "92029e2e262e5be157a02be88d32a868fc86a2680e003f349877a50a0adf672c"
    ),
    debt!(
        "rumoca-phase-flatten::connections::equation_generation::validate_closed_connection_inputs",
        1068,
        "21b16cd9018e2464e7443d4d0a1cc973cde4b37c845d59fa7b228b4cbe6a9ea1"
    ),
    debt!(
        "rumoca-phase-dae::construction::analysis::validate_flat_shape",
        559,
        "093a9b5b2243dedb4dfbe1751561b53211685e2804dfed2726cc5844b8e522b4"
    ),
    debt!(
        "rumoca-ir-solve::SolveModel::validate",
        344,
        "73468c1d232c69b5c161396ec6d38c21db7ca73f9ff7c22bba8bd4feb099ff1d"
    ),
    debt!(
        "rumoca-ir-solve::validate_problem_pure_call_sites",
        429,
        "24268d05a61b8bdb0780d5155a2ceca703ada581126a7692a56448867533076b"
    ),
    debt!(
        "rumoca-phase-solve::lower::lower_solve_problem",
        2219,
        "daaf606ff1903c0a5cb4e9b541b6588a093f8b3fb72f2e35e621a6f5499eff66"
    ),
    debt!(
        "rumoca-phase-codegen::codegen::solve_lazy::explicit_algebraic_assignment_complete",
        2501,
        "51ca832d207f66e67f824a7abd57e4bac8a58a07ea10a271a42aceebe3aab8b2"
    ),
    // Spelling/case/unrelated-dimension recovery: these choose semantic
    // identity or fabricate Medium constants from presentation data.
    debt!(
        "rumoca-phase-instantiate::dims::mod_env_has_package_alias_bindings",
        375,
        "496f2880b036b8aaa8f6d38cc5657df26a1b669887e3643fdb634295d7a3501f"
    ),
    debt!(
        "rumoca-phase-flatten::exposes_unprefixed_prefix",
        200,
        "c44b9b721afae56fb2134c5d7b8d47e4bc755f13ca4ac573a186c7f41931d002"
    ),
    debt!(
        "rumoca-phase-flatten::equations::lookup_parameter_in_scope",
        2392,
        "c601f9eae479ae2d7474b8e58e76eb40526d8c6306c3fa2e3919463349929fb1"
    ),
    debt!(
        "rumoca-phase-flatten::equations::try_lowercase_type_ref",
        824,
        "e6803fb7b4f0e7b4a2524289f7ec3ad867e05ab2f092e858050a4f04d6b77f64"
    ),
    debt!(
        "rumoca-phase-flatten::equations::try_uppercase_instance_ref",
        719,
        "623ee6115a399056cffc3bf3eb33265838f85933a2eb8cefcc1b3a866f5e39ff"
    ),
    debt!(
        "rumoca-phase-flatten::equations::infer_size_constant_from_dims",
        1006,
        "d685528caa2a13615b369cf27fc1435289bdd1e33c1fc5061787086f137feebb"
    ),
    debt!(
        "rumoca-phase-flatten::constant_extraction::resolve_model_redeclare_package_entry",
        1235,
        "64ee870facab8b463727bb049669903d4450aea25aeba6fb5e0131b5945f50fe"
    ),
    debt!(
        "rumoca-phase-flatten::constant_extraction::extract_extends_redeclare_package_constants",
        2139,
        "1f88c19d632bbfdbdbdaf0ea87efbdc6cb7e602a540335d84be9638afd619caa"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::constant_injection::inject_alias_component_package_constants",
        2017,
        "d9e728cf973b002855d525cc5b745e2cf43499c0a940e6d469c3614c06d79672"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::constant_injection::try_eval_const_function_call_expr",
        1594,
        "ec9184f2e9a8e8b5dd1623e611a26e7f372dd406df093db64b79e60437d0c784"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::component_alias_injection::lower_initial",
        271,
        "2ebce495f4eb27c848b3f12d74291db7db7f500f5929e90300826e7931affb05"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::component_alias_injection::split_alias_declared_type",
        294,
        "da8982a03609281d708dd72158e409b4aa1933613daec0c50384e233f0903cac"
    ),
    // Structural-name recovery in connection/Fluid handling: these select an
    // occurrence, scope, flow partner, or stream endpoint from rendered paths.
    debt!(
        "rumoca-phase-instantiate::dims::qualify_component_ref_imports",
        2133,
        "169cc1d1987918c315e14dad3af1467fbadded8234bafa47acec44edfb9cfe22"
    ),
    debt!(
        "rumoca-phase-flatten::connections::validation::validate_connections",
        2154,
        "f861eb2863ae9e823ab14cc6b6091915e98d6df7dc404a8230ee3dee84bec2a9"
    ),
    debt!(
        "rumoca-phase-flatten::connections::equality_projection::checked_record_instance",
        395,
        "3ab8f7d33c8da70c599808712ed36ecd3b50b307a2c6b13292697e6a525b0937"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::collect_flow_candidates",
        544,
        "acbdac4063a15615d15643efe593737d3fa4e78706dc935c57f30dda603f9d27"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::collect_declared_paths",
        595,
        "2d4b5a8a7dd2fa19a695771ef61a756fbbe4fc32cd0b71188baaf18e89e3131a"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::collect_stream_connectors",
        341,
        "55bc3dd4811aefc75dd09380d011c65f504959fbea45890ab975cf638eb1f71b"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::associated_flow",
        1258,
        "aa71cc9ef6cd1bdca1b838be8044205ee997f0447e1ab83ca58270c2a31927d7"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::StreamOperatorRewriter::is_empty_stream_member",
        1076,
        "8846bea6122c8e8bb708ad54433c2edb624e37c079ed62bbaa7b9ef3d276b8bb"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::StreamOperatorRewriter::indexed_endpoint_matches",
        578,
        "d4a1f9492573d3d1db8837d13bb0e8602104c8a812d3d98d97dee1783dffcdd4"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::StreamOperatorRewriter::endpoint_for",
        1212,
        "0f774ad8022b5f465ced2a5ecf8d344e82a2d994b96e66af85afdf300b937640"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::embedded_parent_indices",
        630,
        "6573a2b303b7c073925b69675e432d4e91678c5e58a658c5cda9d18224498030"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::stream_access",
        1123,
        "7987c17a7c7bd03ecec6a116a5f47ab096d8e4c4071719084d07315b4ebe7d80"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::reference_name_without_subscripts",
        574,
        "a0112335d8a88b52ec45f54928015b7223bc62894e74674af95d2c64f934fdab"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::stream_variable_is_valid",
        398,
        "59d85d7b7e064cb0593d2fc7255f04797433e80d7369101021591e74a799bde5"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::associated_flow_from_candidates",
        938,
        "25034d4a94fb4565fb566f4c1746da2f742787e1a113db90d9c57bcd745e0270"
    ),
    debt!(
        "rumoca-phase-flatten::connections::stream_operators::stream_member_reference",
        1142,
        "f244ac79b7f66a06ffb7bb45cbb45d2c7eff78a116bb87acea931141f6fb9f6a"
    ),
    debt!(
        "rumoca-phase-flatten::connections::equation_generation::is_interface_connection_path_for_scope",
        537,
        "9015222ebff51f777c015c365813821a550482588899d5fd4f5e5e18039c4328"
    ),
    debt!(
        "rumoca-phase-flatten::connections::equation_generation::relative_component_path_from_path",
        379,
        "45d139c822c8fa0160175fd09caeef6c4092715f09fbab6c7840918b4f3ceedc"
    ),
    debt!(
        "rumoca-phase-flatten::connections::equation_generation::is_at_ancestor_scope",
        620,
        "fc9a07d6500443f81f0fa79efb09d2deacd629b0693fe209ba9ce84fc7966fcc"
    ),
    debt!(
        "rumoca-phase-flatten::connections::equation_generation::bridge_scope_matches_connection_scope",
        292,
        "0e3b55479e8e349aa1eb358f320b1fa66a344b3b70389f7f5898c3eeb2b03791"
    ),
];

/// Fixed-pass/depth owners that can currently publish absence or partial state
/// instead of a typed terminal error when required semantic work is exhausted.
pub(super) const FAIL_OPEN_CUTOFF_DEBT: &[OwnerDigest] = &[
    debt!(
        "rumoca-phase-typecheck::TypeChecker::collect_instance_class_override_constants",
        1320,
        "588566af507a6cd94b69fac0a89ca1495697a2860c22617dd82626aaa5cb3c64"
    ),
    debt!(
        "rumoca-phase-typecheck::TypeChecker::extract_enclosing_constants_multi_pass",
        655,
        "62a3dc8e36a3fdaecda7b799be49f2a4d53a2d44c83937682a24629ab3a22082"
    ),
    debt!(
        "rumoca-phase-typecheck::TypeChecker::evaluate_all_dimensions_multi_pass",
        1746,
        "38c9fa4a52a9f46b2fdf28041b2a5b338f7fc2903258cd4876ba91c35893f541"
    ),
    debt!(
        "rumoca-phase-typecheck::constant_collection::TypeChecker::collect_model_extends_redeclare_constants",
        1327,
        "873b3d2a1f655a1b5caf7c1d36bb7902a986f2e69befae872b4b43817168b39f"
    ),
    debt!(
        "rumoca-phase-typecheck::constant_collection::TypeChecker::collect_nested_class_constants",
        1336,
        "72d1b97ca17632842770614a3b44b111287a0e5007ce14e9a34df09779c9979a"
    ),
    debt!(
        "rumoca-phase-typecheck::constant_collection::TypeChecker::collect_component_type_nested_constants",
        766,
        "a33c3bf8c976d4c6c038a79286a855f478522be15415752b1ce105fe546730db"
    ),
    debt!(
        "rumoca-phase-typecheck::constant_collection::TypeChecker::collect_component_type_enclosing_constants",
        1260,
        "c89dd0adc94aa00074931dc51d692df8feeed547c3d840114e871f0a0ed87ba6"
    ),
    debt!(
        "rumoca-phase-typecheck::typechecker::record_aliases::TypeChecker::collapse_alias_chains",
        434,
        "a2863d43cf523bf6586a29cef012c8717db1269bb48066d83a958f43131fb4c5"
    ),
    debt!(
        "rumoca-phase-typecheck::function_signatures::class_hierarchy",
        632,
        "d803e856bd14f30e8811face9e9bc1e215d64d541c581ba57ab093d0ce57743e"
    ),
    debt!(
        "rumoca-phase-typecheck::function_signatures::find_effective_nested_class",
        926,
        "08f4f1e3cff7a24a2724a6f388b5a5b638dedade51081a12273cad29b32dbbdf"
    ),
    debt!(
        "rumoca-phase-typecheck::function_signatures::find_declared_nested_class",
        1144,
        "9cc8ce7fc0884fc3da4dc2405a7f297a94b739f260abcca40fcb8f518e8fff1c"
    ),
    debt!(
        "rumoca-phase-typecheck::function_signatures::class_identity_reaches",
        755,
        "5a34ac86a4ca268450035c91b3fe8b2f5270e48287f7c110ef267aee3f3f2263"
    ),
    debt!(
        "rumoca-phase-flatten::constant_extraction::inject_referenced_qualified_class_constants",
        2005,
        "dc7d8d864bb0d037d435b239ae587094697e72e21d719250c2d4930eea383d56"
    ),
    debt!(
        "rumoca-phase-flatten::constant_extraction::inject_model_nested_class_constants",
        1747,
        "407986f24e0cbe07bc94a323dacdd2bd4ec8a60564bb0901c126cdea6f41a2d0"
    ),
    debt!(
        "rumoca-phase-flatten::constant_extraction::inject_model_extends_redeclare_constants",
        2090,
        "aec653b9efa1df884560927f6b765374ea19603511af5e480461a3400d3b6e40"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::constant_injection::extract_ancestor_constants_multi_pass",
        1984,
        "7fde92ead4b49dd8b5a1581b93e23025878ee14f3f3ee2e78d324e446345c7f7"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::component_alias_injection::inject_component_instance_nested_class_constants",
        3222,
        "f6904c5403a9091223c4c31141d7eacf35674b4c92e4893808134c0b5c44197b"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::component_alias_injection::inject_component_enclosing_class_constants",
        1511,
        "19f740c6ef75f031697285d479699687043895d1734c8886056d34fe7775abf7"
    ),
    debt!(
        "rumoca-phase-flatten::pipeline::dim_recovery::recover_nested_dims_from_bindings",
        730,
        "3f91eeb0fd0cf067fdf99c7111079491b4cc86bcfad9c7772d449b4541e6d22c"
    ),
    debt!(
        "rumoca-phase-flatten::resolve_alias_chain",
        601,
        "c60989640d3f9595057e8a713213e53861da52d62a62b97f6f3c94b7b37c57b3"
    ),
    debt!(
        "rumoca-phase-flatten::compute_closure_iterations",
        573,
        "e62d95fcdb5010b5cf8b81b1ed6468a663eb5cb741d0654248e405a5397c05b4"
    ),
    debt!(
        "rumoca-phase-flatten::variables::resolve_flat_output_type_name",
        914,
        "04e042aad9f0e8838975cd47e8355ea3301f106322e088dfac670c05dace2578"
    ),
    debt!(
        "rumoca-phase-flatten::postprocess::occurrence_graph::OccurrenceGraph::select_in_scope",
        1271,
        "fc8b511a7e15b15d2d5f18887326a037e4a84d32cedc88b4f07697c04305b83c"
    ),
    debt!(
        "rumoca-phase-flatten::postprocess::index_collapse::fold_subscript_expr",
        1322,
        "c518bcaa4de006599b1516b91c31898e1227d290ba8fe58eea7f250244cd36f1"
    ),
];

/// Public raw-input entry points that bypass an opaque success-bearing handoff.
/// Root fields/traits/method surfaces are checked structurally in root_surface.
pub(super) const PUBLIC_PROOF_ESCAPE_OWNER_DEBT: &[OwnerDigest] = &[];

/// Exact owner identities that have completed their migration. Add a name
/// here only in the same change that deletes its reviewed debt owner.
pub(super) const RETIRED_OWNER_TOMBSTONES: &[&str] = &[
    "rumoca-phase-resolve::validation::validate_resolution",
    // Retired by the linear proof-chain cutover: Flatten's sole production
    // entry now consumes the Typecheck-minted `TypedInstancedTree` by value,
    // and the Typecheck mint replaced the mutable-overlay publication entry.
    "rumoca-phase-typecheck::typechecker::api::typecheck_instanced",
    "rumoca-phase-flatten::flatten_ref",
    "rumoca-phase-flatten::flatten_ref_with_options",
];
