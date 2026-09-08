//! FLAT-C02 and SPEC_0043 §7 aggregate-function architecture ratchets.
//!
//! This syntax gate tombstones the superseded scalar record ABI and cloned-call
//! coalescer declarations, and prevents a replacement pass from mutating Flat
//! function signature vectors. It complements semantic call-cardinality tests; it
//! is not a proof over macro expansion or runtime behavior.

use super::architecture_hardening_support::{
    attributes_require_test, production_rust_sources, workspace_root,
};
use quote::ToTokens;
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use syn::visit::{self, Visit};

const FLATTEN_CRATE: &str = "crates/rumoca-phase-flatten";

const SOLVE_COMPACT_PROGRAM_SURFACE: &[(&str, &[&str], &[&str], usize)] = &[
    (
        "FunctionFoldProgram",
        &[
            "domain",
            "domain_scalar_count",
            "carried_count",
            "capture_count",
            "register_count",
            "update",
        ],
        &[
            "checked",
            "domain",
            "domain_scalar_count",
            "carried_count",
            "capture_count",
            "register_count",
            "update",
        ],
        1,
    ),
    (
        "FunctionConditionalArmProgram",
        &[
            "condition_register_count",
            "result_register_count",
            "condition",
            "result",
        ],
        &[
            "condition_register_count",
            "result_register_count",
            "condition",
            "result",
        ],
        0,
    ),
    (
        "FunctionConditionalProgram",
        &[
            "owner",
            "capture_count",
            "target_widths",
            "result_count",
            "arms",
            "fallback_register_count",
            "fallback",
        ],
        &[
            "checked",
            "checked_owned",
            "owner",
            "capture_count",
            "target_widths",
            "result_count",
            "arms",
            "fallback_register_count",
            "fallback",
        ],
        1,
    ),
    ("FunctionConditionalOwnerId", &["0"], &["checked", "get"], 1),
];

/// Private current-wire mirrors of the sealed compact programs.
///
/// Each wire stays crate-private, denies unknown fields, and mirrors its sealed
/// owner field for field, so decoding cannot accept an unreviewed key, drop a
/// checked one, or bypass the checked replay through a public wire type.
const SOLVE_COMPACT_PROGRAM_WIRE_SURFACE: &[(&str, &str)] = &[
    ("FunctionFoldProgramWire", "FunctionFoldProgram"),
    (
        "FunctionConditionalProgramWire",
        "FunctionConditionalProgram",
    ),
    (
        "FunctionConditionalArmProgramWire",
        "FunctionConditionalArmProgram",
    ),
];

#[test]
fn solve_compact_function_programs_keep_checked_private_surfaces() {
    let root = workspace_root();
    let source = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/linear_op.rs"))
        .expect("read Solve compact-program owner");
    let findings = solve_compact_program_surface_findings(&source);
    assert!(
        findings.is_empty(),
        "SPEC_0036/SPEC_0043 require private fold/conditional program fields, checked wire replay, and one reviewed immutable public surface:\n{}",
        findings.join("\n")
    );
}

#[test]
fn solve_compact_function_program_privacy_mutations_are_detected() {
    let mutation = r#"
        pub struct FunctionFoldProgram {
            pub domain: (),
            carried_count: usize,
            capture_count: usize,
            register_count: usize,
            update: Vec<()>,
        }
        impl FunctionFoldProgram {
            pub fn checked() {}
            pub fn domain() {}
            pub fn carried_count() {}
            pub fn capture_count() {}
            pub fn register_count() {}
            pub fn update() {}
            pub fn register_flow() {}
        }
    "#;
    let findings = solve_compact_program_surface_findings(mutation);
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("public fields"))
            && findings
                .iter()
                .any(|finding| finding.contains("public method surface")),
        "a public invariant field and second validator must stale the gate: {findings:#?}"
    );
}

#[test]
fn solve_compact_owner_identity_publicity_mutation_is_detected() {
    let mutation = r#"
        pub struct FunctionConditionalOwnerId(pub u64);
        impl FunctionConditionalOwnerId {
            pub const fn checked() {}
            pub const fn get() {}
        }
        impl<'de> Deserialize<'de> for FunctionConditionalOwnerId {}
    "#;

    let findings = solve_compact_program_surface_findings(mutation);

    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("`FunctionConditionalOwnerId` has public fields")),
        "an exposed owner payload must stale the gate: {findings:#?}"
    );
}

#[test]
fn solve_compact_owner_identity_derive_mutation_is_detected() {
    let mutation = r#"
        #[derive(Serialize, Deserialize)]
        pub struct FunctionConditionalOwnerId(u64);
        impl FunctionConditionalOwnerId {
            pub const fn checked() {}
            pub const fn get() {}
        }
    "#;

    let findings = solve_compact_program_surface_findings(mutation);

    assert!(
        findings.iter().any(|finding| finding
            .contains("`FunctionConditionalOwnerId` derives fieldwise Deserialize"))
            && findings.iter().any(|finding| finding.contains(
                "`FunctionConditionalOwnerId` must have 1 custom Deserialize impl(s), found 0"
            )),
        "a fieldwise owner decode must stale the gate: {findings:#?}"
    );
}

#[test]
fn solve_compact_function_program_wires_are_private_strict_mirrors() {
    let root = workspace_root();
    let source = fs::read_to_string(root.join("crates/rumoca-ir-solve/src/linear_op.rs"))
        .expect("read Solve compact-program owner");

    let findings = solve_compact_program_wire_findings(&source);

    assert!(
        findings.is_empty(),
        "SPEC_0036/SPEC_0043 require private strict wire mirrors of every sealed compact program:\n{}",
        findings.join("\n")
    );
}

#[test]
fn solve_compact_function_program_wire_mutations_are_detected() {
    let mutation = r#"
        struct FunctionFoldProgram {
            domain: (),
            domain_scalar_count: usize,
            carried_count: usize,
            capture_count: usize,
            register_count: usize,
            update: Vec<()>,
        }
        pub struct FunctionFoldProgramWire {
            domain: (),
            carried_count: usize,
            capture_count: usize,
            register_count: usize,
            update: Vec<()>,
        }
        struct FunctionConditionalProgram {
            owner: (),
        }
        #[derive(Deserialize)]
        struct FunctionConditionalProgramWire {
            pub owner: (),
        }
        struct FunctionConditionalArmProgram {
            condition: Vec<()>,
        }
        #[derive(Deserialize)]
        #[serde(deny_unknown_fields)]
        struct FunctionConditionalArmProgramWire {
            condition: Vec<()>,
        }
    "#;

    let findings = solve_compact_program_wire_findings(mutation);

    for expected in [
        "`FunctionFoldProgramWire` must stay crate-private",
        "`FunctionFoldProgramWire` must derive the fieldwise wire decode",
        "`FunctionFoldProgramWire` must deny unknown fields",
        "`FunctionFoldProgramWire` field parity with `FunctionFoldProgram` changed",
        "`FunctionConditionalProgramWire` has public fields",
        "`FunctionConditionalProgramWire` must deny unknown fields",
    ] {
        assert!(
            findings.iter().any(|finding| finding.contains(expected)),
            "the wire ledger missed `{expected}`: {findings:#?}"
        );
    }
    assert!(
        !findings
            .iter()
            .any(|finding| finding.contains("FunctionConditionalArmProgramWire")),
        "an unchanged strict wire mirror must stay silent: {findings:#?}"
    );
}

fn solve_compact_program_surface_findings(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("parse Solve compact-program source");
    let mut findings = Vec::new();
    for &(name, fields, methods, deserialize_impls) in SOLVE_COMPACT_PROGRAM_SURFACE {
        let Some(item) = syntax.items.iter().find_map(|item| match item {
            syn::Item::Struct(item) if item.ident == name => Some(item),
            _ => None,
        }) else {
            findings.push(format!("missing `{name}`"));
            continue;
        };
        let actual_fields = field_ledger(item).collect::<BTreeSet<_>>();
        let expected_fields = fields
            .iter()
            .copied()
            .map(str::to_string)
            .collect::<BTreeSet<_>>();
        if actual_fields != expected_fields {
            findings.push(format!(
                "`{name}` field ledger changed: expected {expected_fields:?}, found {actual_fields:?}"
            ));
        }
        let public_fields = item
            .fields
            .iter()
            .zip(field_ledger(item))
            .filter(|(field, _)| !matches!(field.vis, syn::Visibility::Inherited))
            .map(|(_, ledger)| ledger)
            .collect::<Vec<_>>();
        if !public_fields.is_empty() {
            findings.push(format!("`{name}` has public fields: {public_fields:?}"));
        }
        if derives(item, "Deserialize") {
            findings.push(format!(
                "`{name}` derives fieldwise Deserialize instead of checked wire replay"
            ));
        }

        let actual_methods = syntax
            .items
            .iter()
            .filter_map(|item| match item {
                syn::Item::Impl(item)
                    if impl_self_ident(item).is_some_and(|ident| ident == name) =>
                {
                    Some(item)
                }
                _ => None,
            })
            .flat_map(|item| item.items.iter())
            .filter_map(|member| match member {
                syn::ImplItem::Fn(method) if matches!(method.vis, syn::Visibility::Public(_)) => {
                    Some(method.sig.ident.to_string())
                }
                _ => None,
            })
            .collect::<BTreeSet<_>>();
        let expected_methods = methods
            .iter()
            .copied()
            .map(str::to_string)
            .collect::<BTreeSet<_>>();
        if actual_methods != expected_methods {
            findings.push(format!(
                "`{name}` public method surface changed: expected {expected_methods:?}, found {actual_methods:?}"
            ));
        }

        let actual_deserialize_impls = syntax
            .items
            .iter()
            .filter(|item| match item {
                syn::Item::Impl(item) => {
                    impl_self_ident(item).is_some_and(|ident| ident == name)
                        && item.trait_.as_ref().is_some_and(|(_, path, _)| {
                            path.segments
                                .last()
                                .is_some_and(|segment| segment.ident == "Deserialize")
                        })
                }
                _ => false,
            })
            .count();
        if actual_deserialize_impls != deserialize_impls {
            findings.push(format!(
                "`{name}` must have {deserialize_impls} custom Deserialize impl(s), found {actual_deserialize_impls}"
            ));
        }
    }
    findings
}

/// Ledger name of every field of one struct: its identifier, or its ordinal
/// when the struct is a tuple struct.
fn field_ledger(item: &syn::ItemStruct) -> impl Iterator<Item = String> + '_ {
    item.fields.iter().enumerate().map(|(ordinal, field)| {
        field
            .ident
            .as_ref()
            .map_or_else(|| ordinal.to_string(), ToString::to_string)
    })
}

fn derives(item: &syn::ItemStruct, trait_name: &str) -> bool {
    item.attrs.iter().any(|attribute| {
        attribute.path().is_ident("derive")
            && attribute
                .meta
                .to_token_stream()
                .to_string()
                .contains(trait_name)
    })
}

fn solve_compact_program_wire_findings(source: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("parse Solve compact-program source");
    let mut findings = Vec::new();
    for &(wire, sealed) in SOLVE_COMPACT_PROGRAM_WIRE_SURFACE {
        let structs = |name: &str| {
            syntax.items.iter().find_map(move |item| match item {
                syn::Item::Struct(item) if item.ident == name => Some(item),
                _ => None,
            })
        };
        let (Some(wire_item), Some(sealed_item)) = (structs(wire), structs(sealed)) else {
            findings.push(format!("missing `{wire}` or its sealed owner `{sealed}`"));
            continue;
        };
        if !matches!(wire_item.vis, syn::Visibility::Inherited) {
            findings.push(format!("`{wire}` must stay crate-private"));
        }
        let public_wire_fields = wire_item
            .fields
            .iter()
            .zip(field_ledger(wire_item))
            .filter(|(field, _)| !matches!(field.vis, syn::Visibility::Inherited))
            .map(|(_, ledger)| ledger)
            .collect::<Vec<_>>();
        if !public_wire_fields.is_empty() {
            findings.push(format!(
                "`{wire}` has public fields: {public_wire_fields:?}"
            ));
        }
        if !derives(wire_item, "Deserialize") {
            findings.push(format!("`{wire}` must derive the fieldwise wire decode"));
        }
        let denies_unknown_fields = wire_item.attrs.iter().any(|attribute| {
            attribute.path().is_ident("serde")
                && attribute
                    .meta
                    .to_token_stream()
                    .to_string()
                    .contains("deny_unknown_fields")
        });
        if !denies_unknown_fields {
            findings.push(format!("`{wire}` must deny unknown fields"));
        }
        let wire_fields = field_ledger(wire_item).collect::<BTreeSet<_>>();
        let sealed_fields = field_ledger(sealed_item).collect::<BTreeSet<_>>();
        if wire_fields != sealed_fields {
            findings.push(format!(
                "`{wire}` field parity with `{sealed}` changed: sealed {sealed_fields:?}, wire {wire_fields:?}"
            ));
        }
    }
    findings
}

fn impl_self_ident(item: &syn::ItemImpl) -> Option<&syn::Ident> {
    let syn::Type::Path(path) = item.self_ty.as_ref() else {
        return None;
    };
    path.path.segments.last().map(|segment| &segment.ident)
}

const TOMBSTONED_IDENTIFIERS: &[&str] = &[
    "DecomposedParam",
    "PriorRecordFieldRewriter",
    "RecordCallArgDecomposer",
    "RecordDecompositionMap",
    "RecordParamSizeRewriter",
    "WholeRecordParamRewriter",
    "coalesce_one_record_output",
    "coalesce_proven_record_output_assignments",
    "decompose_record_call_args",
    "decompose_record_call_args_in_expr",
    "decompose_record_call_args_in_expr_scoped",
    "decompose_record_call_args_in_stmt",
    "decompose_record_call_args_in_stmt_scoped",
    "decompose_record_calls_in_function_parameter",
    "decompose_record_calls_in_when_equations",
    "decomposed_record_field_param",
    "expand_record_arg",
    "expand_record_constructor_arg",
    "expand_record_var_ref",
    "fuse_record_param_path",
    "indexed_record_param_field",
    "inline_prior_record_field_values",
    "lower_record_function_params",
    "lower_record_function_params_once",
    "record_fields_from_constructor_metadata",
    "record_output_field_assignment",
    "record_param_field_reference",
    "record_param_field_var_ref",
    "record_param_path",
    "record_param_reference",
    "record_param_shape_source",
    "record_write_may_move",
    "rewrite_decomposed_record_call_sites",
    "rewrite_record_param_size_refs_in_function",
    "rewrite_whole_record_params_in_statement",
    "seed_complete_record_defaults",
];

const SIGNATURE_MUTATORS: &[&str] = &[
    "append",
    "clear",
    "dedup",
    "dedup_by",
    "dedup_by_key",
    "drain",
    "extend",
    "insert",
    "pop",
    "push",
    "remove",
    "resize",
    "retain",
    "reverse",
    "sort",
    "sort_by",
    "sort_by_key",
    "splice",
    "split_off",
    "swap_remove",
    "truncate",
];

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct SignatureOwnerFingerprint {
    path: String,
    owner: String,
    method: String,
    ordinal: usize,
    call: u64,
    body: u64,
}

// Generated from every observed production owner that constructs Flat function
// signatures. Any signature-owner edit deliberately updates this ledger and
// receives focused review; a renamed scalarizer cannot become another owner.
const SIGNATURE_OWNER_LEDGER: &[(&str, &str, &str, usize, u64, u64)] = &[
    (
        "crates/rumoca-phase-flatten/src/functions.rs",
        "convert_function",
        "add_input",
        0,
        3_509_535_177_801_760_813,
        6_307_820_156_066_991_244,
    ),
    (
        "crates/rumoca-phase-flatten/src/functions.rs",
        "convert_function",
        "add_output",
        1,
        2_206_464_988_371_185_010,
        6_307_820_156_066_991_244,
    ),
    (
        "crates/rumoca-phase-flatten/src/functions/constructor_signature.rs",
        "convert_constructor_signature",
        "add_input",
        0,
        3_509_535_177_801_760_813,
        18_230_115_793_874_488_095,
    ),
];

#[test]
fn flat_record_function_scalarization_and_call_cloning_remain_deleted() {
    let root = workspace_root();
    let sources = production_rust_sources(&root.join(FLATTEN_CRATE), &root);
    let findings = analyze_sources(&sources);
    assert!(
        findings.is_empty(),
        "FLAT-C02/SPEC_0043 §7 require nominal aggregate record calls through Flat and DAE; scalar signature rewrites and cloned-call coalescing are tombstoned:\n{}",
        findings.into_iter().collect::<Vec<_>>().join("\n")
    );

    let owner = root.join(FLATTEN_CRATE).join("src/function_lowering.rs");
    let source = fs::read_to_string(&owner).expect("read aggregate function normalizer");
    let syntax = syn::parse_file(&source).expect("parse aggregate function normalizer");
    let declarations = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Fn(function)
                if function.sig.ident == "materialize_complete_record_value_defaults" =>
            {
                Some(())
            }
            _ => None,
        })
        .count();
    assert_eq!(
        declarations, 1,
        "the complete aggregate record-default owner must remain exact"
    );
}

#[test]
fn flat_function_signature_construction_has_one_exact_owner_ledger() {
    let root = workspace_root();
    let sources = production_rust_sources(&root.join(FLATTEN_CRATE), &root);
    let actual = signature_owner_fingerprints(&sources);
    let expected = SIGNATURE_OWNER_LEDGER
        .iter()
        .map(
            |&(path, owner, method, ordinal, call, body)| SignatureOwnerFingerprint {
                path: path.to_string(),
                owner: owner.to_string(),
                method: method.to_string(),
                ordinal,
                call,
                body,
            },
        )
        .collect::<BTreeSet<_>>();
    assert_eq!(
        actual, expected,
        "FLAT-C02 signature construction owner ledger changed; every add_input/add_output owner and body requires explicit aggregate-ABI review"
    );
}

#[test]
fn dae_record_equation_plan_is_not_revalidated_by_consumers() {
    let root = workspace_root();
    let analysis =
        fs::read_to_string(root.join("crates/rumoca-phase-dae/src/construction/analysis.rs"))
            .expect("read DAE analysis plan");
    let syntax = syn::parse_file(&analysis).expect("parse DAE analysis plan");
    let coordinate_fields = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Struct(item) if item.ident == "RecordEquationCoordinate" => Some(
                item.fields
                    .iter()
                    .filter_map(|field| field.ident.as_ref().map(ToString::to_string))
                    .collect::<BTreeSet<_>>(),
            ),
            _ => None,
        })
        .expect("RecordEquationCoordinate remains an explicit private plan fact");
    assert_eq!(
        coordinate_fields,
        ["discrete_unknown", "instance_id", "name", "scalar_count"]
            .into_iter()
            .map(str::to_string)
            .collect(),
        "record-equation consumers may receive only constructor-derived facts"
    );

    for (relative, owner) in [
        (
            "analysis/discrete_values.rs",
            "collect_record_equation_owner",
        ),
        (
            "analysis/equation_partitions.rs",
            "add_record_discrete_coordinates",
        ),
        ("analysis/source_balance.rs", "add_record_equation_balance"),
        ("record_equation.rs", "checked_coordinate"),
    ] {
        let path = root
            .join("crates/rumoca-phase-dae/src/construction")
            .join(relative);
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        assert_record_consumer_uses_only_issued_facts(&source, owner);
    }
}

#[test]
fn record_equation_consumer_revalidation_mutation_is_detected() {
    let mutation = "fn checked_coordinate(flat: &flat::Model) { let _ = &flat.effective_types; }";
    let result = std::panic::catch_unwind(|| {
        assert_record_consumer_uses_only_issued_facts(mutation, "checked_coordinate");
    });
    assert!(
        result.is_err(),
        "a renamed catalog replay must stale the gate"
    );
}

#[test]
fn equality_constraint_occurrence_selection_has_one_atomic_owner() {
    let root = workspace_root();
    let occurrence = fs::read_to_string(
        root.join("crates/rumoca-ir-ast/src/instance/equality_constraint/occurrence.rs"),
    )
    .expect("read equalityConstraint occurrence constructor");
    let selection = fs::read_to_string(
        root.join("crates/rumoca-ir-ast/src/instance/equality_constraint/selection.rs"),
    )
    .expect("read equalityConstraint selection owner");
    let instantiate =
        fs::read_to_string(root.join("crates/rumoca-phase-instantiate/src/equality_constraint.rs"))
            .expect("read equalityConstraint phase transition");
    assert_equality_constraint_occurrence_single_owner(&occurrence, &selection, &instantiate);
}

#[test]
fn equality_constraint_occurrence_replay_mutation_is_detected() {
    let occurrence = r#"
        fn construct_and_register_equality_constraint_occurrence(
            prototype: EqualityConstraintPrototype,
        ) { prove_equality_constraint_occurrence_selection(); }
    "#;
    let selection = r#"
        fn occurrence_equality_constraint_selection(component: &Component) {
            let _ = &component.source_modifications;
        }
    "#;
    let instantiate = "fn reject_selected_class_override_specialization() {}";
    let result = std::panic::catch_unwind(|| {
        assert_equality_constraint_occurrence_single_owner(occurrence, selection, instantiate);
    });
    assert!(
        result.is_err(),
        "a caller-supplied prototype or raw modifier replay must stale the gate"
    );
}

fn assert_equality_constraint_occurrence_single_owner(
    occurrence: &str,
    selection: &str,
    instantiate: &str,
) {
    let occurrence_syntax = syn::parse_file(occurrence).expect("parse occurrence constructor");
    let (signature, body) = occurrence_syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item) => item.items.iter().find_map(|member| match member {
                syn::ImplItem::Fn(function)
                    if function.sig.ident
                        == "construct_and_register_equality_constraint_occurrence" =>
                {
                    Some((
                        function.sig.to_token_stream().to_string(),
                        function.block.to_token_stream().to_string(),
                    ))
                }
                _ => None,
            }),
            syn::Item::Fn(function)
                if function.sig.ident
                    == "construct_and_register_equality_constraint_occurrence" =>
            {
                Some((
                    function.sig.to_token_stream().to_string(),
                    function.block.to_token_stream().to_string(),
                ))
            }
            _ => None,
        })
        .next()
        .expect("the atomic equalityConstraint occurrence constructor remains explicit");
    assert!(
        !signature.contains("EqualityConstraintPrototype"),
        "callers must not supply an independently constructed occurrence prototype"
    );
    assert!(
        body.contains("prove_equality_constraint_occurrence_selection"),
        "the atomic root must issue the exact occurrence selection itself"
    );

    let selection_syntax = syn::parse_file(selection).expect("parse selection owner");
    let selection_owner = selection_syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function)
                if function.sig.ident == "occurrence_equality_constraint_selection" =>
            {
                Some(function)
            }
            _ => None,
        })
        .expect("the exact occurrence selection owner remains explicit");
    let signature = selection_owner.sig.to_token_stream().to_string();
    let body = selection_owner.block.to_token_stream().to_string();
    assert!(
        signature.contains("ClassOverrideMap"),
        "occurrence selection must consume Instantiate's exact override catalog"
    );
    for forbidden in [
        "source_modifications",
        "source_modification_redeclare_flags",
        ". modifications",
    ] {
        assert!(
            !body.contains(forbidden),
            "occurrence selection reopens raw modifier syntax through `{forbidden}`"
        );
    }
    assert!(
        !instantiate.contains("reject_selected_class_override_specialization"),
        "phase-instantiate must not duplicate the atomic occurrence selection check"
    );
}

fn assert_record_consumer_uses_only_issued_facts(source: &str, owner: &str) {
    let syntax = syn::parse_file(source).expect("parse record-equation consumer");
    let function = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(function) if function.sig.ident == owner => Some(function),
            _ => None,
        })
        .unwrap_or_else(|| panic!("record-equation consumer `{owner}` must remain explicit"));
    let body = function.block.to_token_stream().to_string();
    for forbidden in [
        "checked_variable",
        "effective_types",
        "instance_relations",
        "record_types",
        "type_roots",
    ] {
        assert!(
            !body.contains(forbidden),
            "record-equation consumer `{owner}` reopens `{forbidden}` instead of consuming its issued plan"
        );
    }
}

#[test]
fn tombstoned_identifier_mutation_is_detected() {
    let findings = analyze_source(
        Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs"),
        "struct DecomposedParam; fn coalesce_proven_record_output_assignments() {}",
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("DecomposedParam"))
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("coalesce_proven_record_output_assignments"))
    );
}

#[test]
fn renamed_scalar_signature_mutation_is_detected() {
    let findings = analyze_source(
        Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs"),
        "fn lower(function: &mut Function) { let old = std::mem::take(&mut function.inputs); function.inputs.extend(old); }",
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("mutable-function-signature")),
        "renaming the pass must not evade the aggregate-signature gate: {findings:?}"
    );
}

#[test]
fn output_signature_mutation_is_detected() {
    let findings = analyze_source(
        Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs"),
        "fn lower(function: &mut Function) { function.outputs.clear(); }",
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("mutable-function-signature")),
        "result scalarization must not evade the signature-vector gate: {findings:?}"
    );
}

#[test]
fn signature_builder_and_struct_literal_mutations_are_detected() {
    let path = Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs");
    let source = "fn lower(function: &mut Function, field: FunctionParam) { function.add_input(field); function.add_output(field); let _ = Function { inputs: vec![field], outputs: vec![field], ..function }; }";
    let findings = analyze_source(path, source);
    let fingerprints = signature_owner_fingerprints(&[(path.to_path_buf(), source.to_string())]);
    assert_eq!(
        fingerprints.len(),
        2,
        "both builder calls must be inventoried"
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("signature-struct-literal")),
        "direct signature-vector authorship must be rejected: {findings:?}"
    );
}

#[test]
fn associated_signature_builder_call_is_detected() {
    let findings = analyze_source(
        Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs"),
        "fn lower(function: &mut Function, field: FunctionParam) { Function::add_input(function, field); rumoca_core::Function::add_output(function, field); }",
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("signature-associated-call")),
        "qualified associated-call syntax must not bypass the builder ledger: {findings:?}"
    );
}

#[test]
fn destructured_signature_alias_is_detected() {
    let findings = analyze_source(
        Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs"),
        "fn lower(function: &mut Function) { let Function { inputs, .. } = function; inputs.push(field()); }",
    );
    assert!(
        findings
            .iter()
            .any(|finding| finding.contains("signature-destructure")),
        "destructuring must not create an untracked signature-vector alias: {findings:?}"
    );
}

#[test]
fn signature_owner_body_mutation_changes_its_fingerprint() {
    let path = Path::new("crates/rumoca-phase-flatten/src/reintroduced.rs");
    let original = signature_owner_fingerprints(&[(
        path.to_path_buf(),
        "fn owner(function: &mut Function, field: FunctionParam) { function.add_input(field); }"
            .to_string(),
    )]);
    let mutated = signature_owner_fingerprints(&[(
        path.to_path_buf(),
        "fn owner(function: &mut Function, field: FunctionParam) { audit(field); function.add_input(field); }"
            .to_string(),
    )]);
    assert_ne!(
        original, mutated,
        "changing an authorized owner body must stale its ledger fingerprint"
    );
}

fn analyze_sources(sources: &[(PathBuf, String)]) -> BTreeSet<String> {
    sources
        .iter()
        .flat_map(|(path, source)| analyze_source(path, source))
        .collect()
}

fn analyze_source(path: &Path, source: &str) -> BTreeSet<String> {
    let syntax =
        syn::parse_file(source).unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
    let mut visitor = AggregateBoundaryVisitor {
        path,
        findings: BTreeSet::new(),
        algorithm_bindings: BTreeSet::new(),
        signature_element_bindings: BTreeSet::new(),
    };
    visitor.visit_file(&syntax);
    visitor.findings
}

struct AggregateBoundaryVisitor<'a> {
    path: &'a Path,
    findings: BTreeSet<String>,
    algorithm_bindings: BTreeSet<String>,
    signature_element_bindings: BTreeSet<String>,
}

impl AggregateBoundaryVisitor<'_> {
    fn record_identifier(&mut self, identifier: &syn::Ident) {
        let name = identifier.to_string();
        if TOMBSTONED_IDENTIFIERS.contains(&name.as_str()) {
            self.findings.insert(format!(
                "{}:tombstoned-identifier:{name}",
                self.path.display()
            ));
        }
    }

    fn record_signature_mutation(&mut self) {
        self.findings.insert(format!(
            "{}:mutable-function-signature",
            self.path.display()
        ));
    }
}

impl<'ast> Visit<'ast> for AggregateBoundaryVisitor<'_> {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        self.record_identifier(&item.sig.ident);
        let previous_algorithms =
            std::mem::replace(&mut self.algorithm_bindings, algorithm_bindings(&item.sig));
        let previous_signature_elements = std::mem::take(&mut self.signature_element_bindings);
        visit::visit_item_fn(self, item);
        self.algorithm_bindings = previous_algorithms;
        self.signature_element_bindings = previous_signature_elements;
    }

    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        if !attributes_require_test(&item.attrs) {
            visit::visit_item_mod(self, item);
        }
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        self.record_identifier(&item.ident);
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        self.record_identifier(&item.ident);
        visit::visit_item_enum(self, item);
    }

    fn visit_item_type(&mut self, item: &'ast syn::ItemType) {
        self.record_identifier(&item.ident);
        visit::visit_item_type(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        self.record_identifier(&item.sig.ident);
        let previous_algorithms =
            std::mem::replace(&mut self.algorithm_bindings, algorithm_bindings(&item.sig));
        let previous_signature_elements = std::mem::take(&mut self.signature_element_bindings);
        visit::visit_impl_item_fn(self, item);
        self.algorithm_bindings = previous_algorithms;
        self.signature_element_bindings = previous_signature_elements;
    }

    fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
        self.record_identifier(&segment.ident);
        visit::visit_path_segment(self, segment);
    }

    fn visit_expr_assign(&mut self, expression: &'ast syn::ExprAssign) {
        let mutates_signature_vector = expression_mentions_signature_vector(&expression.left)
            && !expression_is_algorithm_io(&expression.left, &self.algorithm_bindings);
        let mutates_signature_element = expression_targets_signature_element_contract(
            &expression.left,
            &self.signature_element_bindings,
        );
        if mutates_signature_vector || mutates_signature_element {
            self.record_signature_mutation();
        }
        visit::visit_expr_assign(self, expression);
    }

    fn visit_expr_reference(&mut self, expression: &'ast syn::ExprReference) {
        if expression.mutability.is_some()
            && ((expression_mentions_signature_vector(&expression.expr)
                && !expression_is_algorithm_io(&expression.expr, &self.algorithm_bindings))
                || expression_targets_signature_element_contract(
                    &expression.expr,
                    &self.signature_element_bindings,
                ))
        {
            self.record_signature_mutation();
        }
        visit::visit_expr_reference(self, expression);
    }

    fn visit_expr_method_call(&mut self, expression: &'ast syn::ExprMethodCall) {
        if SIGNATURE_MUTATORS.contains(&expression.method.to_string().as_str())
            && (expression_mentions_signature_vector(&expression.receiver)
                || expression_targets_signature_element_contract(
                    &expression.receiver,
                    &self.signature_element_bindings,
                ))
        {
            self.record_signature_mutation();
        }
        visit::visit_expr_method_call(self, expression);
    }

    fn visit_expr_for_loop(&mut self, expression: &'ast syn::ExprForLoop) {
        let pattern_binding = match expression.pat.as_ref() {
            syn::Pat::Ident(pattern) => Some(pattern.ident.to_string()),
            _ => None,
        };
        let algorithm_binding = pattern_binding.clone().filter(|_| {
            expression_mentions_algorithm_catalog(&expression.expr)
                || expression_is_named_binding(&expression.expr, &self.algorithm_bindings)
        });
        let signature_binding =
            pattern_binding.filter(|_| expression_is_mutable_signature_iteration(&expression.expr));

        if signature_binding.is_none() {
            self.visit_expr(&expression.expr);
        }
        let algorithm_inserted = algorithm_binding
            .as_ref()
            .is_some_and(|binding| self.algorithm_bindings.insert(binding.clone()));
        let signature_inserted = signature_binding
            .as_ref()
            .is_some_and(|binding| self.signature_element_bindings.insert(binding.clone()));
        self.visit_block(&expression.body);
        if algorithm_inserted && let Some(binding) = algorithm_binding {
            self.algorithm_bindings.remove(&binding);
        }
        if signature_inserted && let Some(binding) = signature_binding {
            self.signature_element_bindings.remove(&binding);
        }
    }

    fn visit_expr_call(&mut self, expression: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = expression.func.as_ref()
            && path.path.segments.last().is_some_and(|segment| {
                matches!(
                    segment.ident.to_string().as_str(),
                    "add_input" | "add_output"
                )
            })
        {
            self.findings
                .insert(format!("{}:signature-associated-call", self.path.display()));
        }
        visit::visit_expr_call(self, expression);
    }

    fn visit_pat_struct(&mut self, pattern: &'ast syn::PatStruct) {
        if pattern
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "Function")
            && pattern.fields.iter().any(|field| {
                matches!(
                    &field.member,
                    syn::Member::Named(name) if name == "inputs" || name == "outputs"
                )
            })
        {
            self.findings
                .insert(format!("{}:signature-destructure", self.path.display()));
        }
        visit::visit_pat_struct(self, pattern);
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        if expression
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "Function")
            && expression.fields.iter().any(|field| {
                matches!(
                    &field.member,
                    syn::Member::Named(name) if name == "inputs" || name == "outputs"
                )
            })
        {
            self.findings
                .insert(format!("{}:signature-struct-literal", self.path.display()));
        }
        visit::visit_expr_struct(self, expression);
    }
}

fn expression_mentions_signature_vector(expression: &syn::Expr) -> bool {
    struct SignatureVectorVisitor(bool);

    impl<'ast> Visit<'ast> for SignatureVectorVisitor {
        fn visit_member(&mut self, member: &'ast syn::Member) {
            if matches!(member, syn::Member::Named(name) if name == "inputs" || name == "outputs") {
                self.0 = true;
            }
            visit::visit_member(self, member);
        }
    }

    let mut visitor = SignatureVectorVisitor(false);
    visitor.visit_expr(expression);
    visitor.0
}

fn expression_is_algorithm_io(
    expression: &syn::Expr,
    algorithm_bindings: &BTreeSet<String>,
) -> bool {
    let syn::Expr::Field(field) = expression else {
        return false;
    };
    if !matches!(
        &field.member,
        syn::Member::Named(name) if name == "inputs" || name == "outputs"
    ) {
        return false;
    }
    matches!(
        field.base.as_ref(),
        syn::Expr::Path(path) if path.path.segments.last().is_some_and(|segment| {
            algorithm_bindings.contains(&segment.ident.to_string())
        })
    )
}

fn expression_is_named_binding(expression: &syn::Expr, bindings: &BTreeSet<String>) -> bool {
    matches!(
        expression,
        syn::Expr::Path(path) if path.path.segments.last().is_some_and(|segment| {
            bindings.contains(&segment.ident.to_string())
        })
    )
}

fn expression_is_mutable_signature_iteration(expression: &syn::Expr) -> bool {
    let syn::Expr::Reference(reference) = expression else {
        return false;
    };
    let syn::Expr::Field(field) = reference.expr.as_ref() else {
        return false;
    };
    reference.mutability.is_some()
        && matches!(
            &field.member,
            syn::Member::Named(name) if name == "inputs" || name == "outputs"
        )
}

fn expression_targets_signature_element_contract(
    expression: &syn::Expr,
    bindings: &BTreeSet<String>,
) -> bool {
    match expression {
        syn::Expr::Field(field) => {
            let syn::Member::Named(member) = &field.member else {
                return expression_targets_signature_element_contract(&field.base, bindings);
            };
            if expression_is_named_binding(&field.base, bindings) {
                return member != "default";
            }
            expression_targets_signature_element_contract(&field.base, bindings)
        }
        _ => expression_is_named_binding(expression, bindings),
    }
}

fn algorithm_bindings(signature: &syn::Signature) -> BTreeSet<String> {
    signature
        .inputs
        .iter()
        .filter_map(|argument| match argument {
            syn::FnArg::Typed(argument) if type_mentions_algorithm(&argument.ty) => {
                match argument.pat.as_ref() {
                    syn::Pat::Ident(pattern) => Some(pattern.ident.to_string()),
                    _ => None,
                }
            }
            _ => None,
        })
        .collect()
}

fn type_mentions_algorithm(ty: &syn::Type) -> bool {
    match ty {
        syn::Type::Array(ty) => type_mentions_algorithm(&ty.elem),
        syn::Type::Group(ty) => type_mentions_algorithm(&ty.elem),
        syn::Type::Paren(ty) => type_mentions_algorithm(&ty.elem),
        syn::Type::Path(ty) => ty
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "Algorithm"),
        syn::Type::Reference(ty) => type_mentions_algorithm(&ty.elem),
        syn::Type::Slice(ty) => type_mentions_algorithm(&ty.elem),
        _ => false,
    }
}

fn expression_mentions_algorithm_catalog(expression: &syn::Expr) -> bool {
    struct AlgorithmCatalogVisitor(bool);

    impl<'ast> Visit<'ast> for AlgorithmCatalogVisitor {
        fn visit_member(&mut self, member: &'ast syn::Member) {
            if matches!(
                member,
                syn::Member::Named(name)
                    if name == "algorithms" || name == "initial_algorithms"
            ) {
                self.0 = true;
            }
            visit::visit_member(self, member);
        }
    }

    let mut visitor = AlgorithmCatalogVisitor(false);
    visitor.visit_expr(expression);
    visitor.0
}

fn signature_owner_fingerprints(
    sources: &[(PathBuf, String)],
) -> BTreeSet<SignatureOwnerFingerprint> {
    sources
        .iter()
        .flat_map(|(path, source)| {
            let syntax = syn::parse_file(source)
                .unwrap_or_else(|error| panic!("parse {}: {error}", path.display()));
            let mut visitor = SignatureOwnerVisitor {
                path,
                owner: None,
                calls: Vec::new(),
            };
            visitor.visit_file(&syntax);
            visitor.calls
        })
        .collect()
}

struct SignatureOwnerVisitor<'scope> {
    path: &'scope Path,
    owner: Option<SignatureOwnerContext>,
    calls: Vec<SignatureOwnerFingerprint>,
}

struct SignatureOwnerContext {
    name: String,
    body: u64,
    next_ordinal: usize,
}

impl SignatureOwnerVisitor<'_> {
    fn visit_owner(&mut self, name: &syn::Ident, block: &syn::Block) {
        let previous = self.owner.replace(SignatureOwnerContext {
            name: name.to_string(),
            body: syntax_fingerprint(block),
            next_ordinal: 0,
        });
        self.visit_block(block);
        self.owner = previous;
    }
}

impl<'ast> Visit<'ast> for SignatureOwnerVisitor<'_> {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        if !attributes_require_test(&item.attrs) {
            self.visit_owner(&item.sig.ident, &item.block);
        }
    }

    fn visit_item_mod(&mut self, item: &'ast syn::ItemMod) {
        if !attributes_require_test(&item.attrs) {
            visit::visit_item_mod(self, item);
        }
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        if !attributes_require_test(&item.attrs) {
            self.visit_owner(&item.sig.ident, &item.block);
        }
    }

    fn visit_expr_method_call(&mut self, expression: &'ast syn::ExprMethodCall) {
        let method = expression.method.to_string();
        if matches!(method.as_str(), "add_input" | "add_output") {
            let owner = self
                .owner
                .as_mut()
                .expect("a production signature builder call belongs to a function body");
            let ordinal = owner.next_ordinal;
            owner.next_ordinal += 1;
            self.calls.push(SignatureOwnerFingerprint {
                path: self.path.display().to_string(),
                owner: owner.name.clone(),
                method,
                ordinal,
                call: syntax_fingerprint(expression),
                body: owner.body,
            });
        }
        visit::visit_expr_method_call(self, expression);
    }
}

fn syntax_fingerprint(node: &impl ToTokens) -> u64 {
    node.to_token_stream()
        .to_string()
        .bytes()
        .fold(14_695_981_039_346_656_037_u64, |hash, byte| {
            (hash ^ u64::from(byte)).wrapping_mul(1_099_511_628_211)
        })
}
