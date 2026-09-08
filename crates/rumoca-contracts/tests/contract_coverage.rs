//! Contract coverage guards: keep registry status aligned with explicit test-case mapping.

mod support;

use rumoca_contracts::{ContractStatus, create_registry, registry_template};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;
use support::contract_cases_manifest::{
    ContractCaseKind, ContractCaseOutcome, contract_cases, implemented_contract_cases,
    manifest_contract_status,
};

/// Contracts currently required to keep dual-case coverage (at least one accept and one reject).
///
/// This list represents the first strictness gate and can be expanded as contract
/// tests are hardened.
const STRICT_DUAL_CASE_CONTRACT_IDS: &[&str] = &[
    "ALG-001", "CONN-017", "DECL-001", "DECL-003", "DECL-024", "DECL-032", "EQN-001", "EQN-015",
    "EQN-013", "EQN-016", "EQN-025", "EXPR-012", "EXPR-013", "EXPR-016", "FUNC-001", "FUNC-010",
    "FUNC-011", "FUNC-012", "FUNC-015", "FUNC-017", "LEX-001", "PKG-002", "SIM-005", "SIM-009",
    "TYPE-013",
];

/// Contracts that must keep semantic coverage (compile or balance), not parse-only checks.
const STRICT_NON_PARSE_CONTRACT_IDS: &[&str] = &[
    "ALG-005", "ALG-012", "ALG-013", "CONN-017", "DECL-003", "DECL-022", "DECL-036", "EQN-013",
    "EQN-016", "EQN-021", "EXPR-013", "FUNC-001", "FUNC-010", "FUNC-011", "FUNC-012", "FUNC-015",
    "SIM-005", "SIM-009", "TYPE-013",
];

/// Parse-enforced non-LEX contracts where grammar-level rejection is the intended guard.
const PARSE_ENFORCED_NON_LEX_CONTRACT_IDS: &[&str] = &[
    "ALG-001", "ANN-001", "DECL-001", "DECL-006", "DECL-012", "DECL-015", "DECL-024", "EQN-024",
    "EXPR-014", "FUNC-006", "FUNC-016", "FUNC-017", "INST-003", "INST-012", "INST-017", "INST-023",
    "PKG-012",
];

/// The exact bounded case set every `Partial` registry row is allowed to carry.
///
/// A `Partial` row states that part of its MLS requirement is enforced and the
/// rest is not. The declared list is the enforced part, so it is asserted for
/// *exact* equality, never membership: adding a case to a `Partial` row without
/// promoting the row is how a bounded credit silently grows into a claim the
/// row does not make, and dropping one is how the enforced part silently
/// shrinks. Both directions must fail here.
///
/// The key set is derived from `registry.by_status(Partial)` rather than
/// maintained by hand, so a row newly downgraded to `Partial` cannot evade the
/// guard by simply not appearing. Rows with no mapped case declare `&[]`, which
/// is a disposition, not an omission: they are `Partial` with nothing yet
/// enforced.
const PARTIAL_CONTRACT_BOUNDED_CASES: &[(&str, &[&str])] = &[
    (
        // Bounded: the parser refuses an omitted iterator range with EP004 in
        // every syntax that admits one, and explicit ranges stay accepted.
        // Not bounded, and unowned: range inference itself, the
        // identical-range agreement rule, and the whole-array restriction that
        // only applies once a range has been inferred.
        "ALG-004",
        &[
            "alg_004_explicit_range_comprehension_is_allowed",
            "alg_004_explicit_range_equation_loop_is_allowed",
            "alg_004_explicit_ranges_are_accepted_in_every_iterator_syntax",
            "alg_004_implicit_comprehension_range_fails_closed_until_supported",
            "alg_004_implicit_equation_range_fails_closed_until_supported",
            "alg_004_implicit_later_index_range_fails_closed_until_supported",
            "alg_004_implicit_reduction_range_fails_closed_until_supported",
            "alg_004_implicit_statement_range_fails_closed_until_supported",
            "alg_004_unrelated_rejections_cannot_satisfy_the_implicit_range_witness",
            "alg_004_whole_array_assignment_with_explicit_range_is_allowed",
        ],
    ),
    (
        "CONN-011",
        &["conn_011_expandable_connect_neither_declared_rejected"],
    ),
    ("EXPR-040", &[]),
    ("FUNC-038", &[]),
    ("FUNC-039", &[]),
    (
        "INST-034",
        &[
            "inst_034_encapsulated_basic",
            "inst_034_encapsulated_self_lookup_ok",
            "inst_034_encapsulated_upward_lookup_rejected",
        ],
    ),
    (
        "INST-035",
        &["inst_035_record_constructor_and_type_lookup_coexist"],
    ),
    (
        "INST-036",
        &["inst_036_enum_conversion_and_type_lookup_coexist"],
    ),
    ("INST-042", &["inst_042_break_modification_removes_binding"]),
    (
        "INST-053",
        &[
            "inst_053_conditional_false_removed",
            "inst_053_conditional_true_kept",
        ],
    ),
    (
        "INST-054",
        &[
            "inst_054_scope_lookup_order",
            "inst_054_wildcard_import_ambiguity_rejected",
        ],
    ),
    ("SIM-010", &[]),
    ("SM-002", &[]),
    ("SM-003", &[]),
    ("SM-004", &[]),
    ("SM-005", &[]),
    ("SM-006", &[]),
    ("SM-007", &[]),
    ("TYPE-002", &[]),
    ("TYPE-003", &[]),
    ("TYPE-022", &[]),
];

/// Public helper API in `rumoca_contracts::test_support` that must stay in use.
///
/// If a helper is no longer referenced by any contract test, remove it to keep
/// the support API lean.
const TEST_SUPPORT_HELPERS: &[&str] = &[
    "expect_success",
    "expect_resolve_failure_with_code",
    "expect_failure_in_phase_with_code",
    "expect_balanced",
    "is_standalone_simulatable",
    "unbound_fixed_parameter_names",
    "expect_parse_ok",
    "expect_parse_err_with_code",
    "expect_parse_err_with_only_code",
];

/// The manifest and the registry must agree, and the unknown-ID diagnostic must
/// be reachable.
///
/// Order is load-bearing here. `contract_cases()` reads the shipped manifest and
/// consults no registry, so the complete unknown-ID diff below can be computed
/// even when the manifest names IDs the registry has never held.
/// `implemented_contract_cases()` filters through `manifest_contract_status`,
/// which fails closed on the *first* unknown ID it meets; running it first
/// aborts on one ID and makes the set-valued diagnostic unreachable in the only
/// situation it exists for. The one-ID fail-closed boundary is witnessed
/// directly by `unknown_contract_id_fails_closed_at_the_manifest_lookup`, so
/// deferring the filtered view costs no fail-closed coverage.
#[test]
fn implemented_contract_status_matches_manifest() {
    let registry = registry_template();
    let registry_ids: BTreeSet<String> = registry.all().map(|c| c.id.to_string()).collect();

    let manifest_ids: BTreeSet<String> = contract_cases()
        .map(|case| case.contract_id.to_string())
        .collect();
    let unknown_manifest_ids: Vec<&String> = manifest_ids.difference(&registry_ids).collect();
    assert!(
        unknown_manifest_ids.is_empty(),
        "Contract-case manifest references unknown contract IDs: {unknown_manifest_ids:?}"
    );

    let implemented_registry_ids: BTreeSet<String> = registry
        .by_status(ContractStatus::Implemented)
        .map(|c| c.id.to_string())
        .collect();
    let manifest_implemented_ids: BTreeSet<String> = implemented_contract_cases()
        .map(|case| case.contract_id.to_string())
        .collect();

    assert_eq!(
        manifest_implemented_ids, implemented_registry_ids,
        "Every Implemented registry row must have an explicit contract case, and only Implemented rows may enter that view"
    );
}

/// Direct negative witness for the manifest lookup boundary.
///
/// The lookup must abort naming the exact unknown ID rather than answering
/// "not Implemented" for a contract that does not exist. This calls the
/// boundary itself with an ID the registry cannot hold, so restoring a
/// membership-style lookup fails here immediately, and it fails even if
/// `implemented_contract_status_matches_manifest` is deleted. That sibling
/// test remains complementary rather than redundant: it diffs the whole
/// shipped manifest against the registry, which is a different question from
/// what the boundary does when asked about one ID it does not know.
#[test]
#[should_panic(expected = "unknown contract ID: ZZZ-999")]
fn unknown_contract_id_fails_closed_at_the_manifest_lookup() {
    let _ = manifest_contract_status("ZZZ-999");
}

#[test]
fn contract_case_manifest_is_well_formed() {
    assert!(
        contract_cases().next().is_some(),
        "Contract-case manifest must not be empty"
    );

    let mut case_ids = BTreeSet::new();
    let tests_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut file_cache: BTreeMap<String, String> = BTreeMap::new();

    for case in contract_cases() {
        assert!(
            is_contract_id(case.contract_id.as_str()),
            "Invalid contract_id format in manifest: {}",
            case.contract_id
        );
        assert!(
            case_ids.insert(case.case_id.to_string()),
            "Duplicate case_id in manifest: {}",
            case.case_id
        );
        assert!(
            !case.test_file.is_empty(),
            "test_file must be set for case {}",
            case.case_id
        );

        let test_path = tests_root.join(&case.test_file);
        assert!(
            test_path.exists(),
            "Manifest case {} references missing test file: {}",
            case.case_id,
            test_path.display()
        );

        let test_path_str = test_path.to_string_lossy().to_string();
        let file_content = file_cache.entry(test_path_str.clone()).or_insert_with(|| {
            fs::read_to_string(&test_path).unwrap_or_else(|e| {
                panic!(
                    "Failed to read manifest-referenced test file {}: {e}",
                    test_path.display()
                )
            })
        });
        let fn_pattern = format!("fn {}(", case.case_id);
        assert!(
            file_content.contains(&fn_pattern),
            "Manifest case {} does not map to an existing test function in {}",
            case.case_id,
            case.test_file
        );
    }
}

#[test]
fn every_partial_contract_declares_its_exact_bounded_case_set() {
    let registry = create_registry();

    let registry_partial_ids: BTreeSet<&str> = registry
        .by_status(ContractStatus::Partial)
        .map(|contract| contract.id.as_str())
        .collect();
    let declared_partial_ids: BTreeSet<&str> = PARTIAL_CONTRACT_BOUNDED_CASES
        .iter()
        .map(|(id, _)| *id)
        .collect();
    assert_eq!(
        registry_partial_ids, declared_partial_ids,
        "every registry Partial row must declare its bounded case disposition here, \
         and only Partial rows may appear; derive the set, do not curate it"
    );

    for (id, expected_case_ids) in PARTIAL_CONTRACT_BOUNDED_CASES {
        let actual_case_ids: BTreeSet<&str> = contract_cases()
            .filter(|case| case.contract_id == *id)
            .map(|case| case.case_id.as_str())
            .collect();
        let expected_case_ids: BTreeSet<&str> = expected_case_ids.iter().copied().collect();
        assert_eq!(
            actual_case_ids, expected_case_ids,
            "{id} must carry exactly its declared bounded case set while Partial"
        );
        assert!(
            implemented_contract_cases().all(|case| case.contract_id != *id),
            "a bounded Partial case must not make {id} look fully Implemented"
        );
    }
}

#[test]
fn strict_dual_case_contracts_keep_accept_and_reject_coverage() {
    let mut counts: BTreeMap<&str, (usize, usize)> = BTreeMap::new(); // (accept, reject)
    for case in implemented_contract_cases() {
        let entry = counts.entry(case.contract_id.as_str()).or_insert((0, 0));
        match case.outcome {
            ContractCaseOutcome::Accept => entry.0 += 1,
            ContractCaseOutcome::Reject => entry.1 += 1,
        }
    }

    for id in STRICT_DUAL_CASE_CONTRACT_IDS {
        let (accept, reject) = counts
            .get(id)
            .copied()
            .unwrap_or_else(|| panic!("Strict dual-case contract {id} has no cases"));
        assert!(
            accept > 0 && reject > 0,
            "Strict dual-case contract {id} must keep both accept and reject cases (accept={accept}, reject={reject})"
        );
    }
}

#[test]
fn strict_non_parse_contracts_keep_semantic_coverage() {
    for id in STRICT_NON_PARSE_CONTRACT_IDS {
        let mut has_case = false;
        for case in implemented_contract_cases().filter(|case| case.contract_id == *id) {
            has_case = true;
            assert!(
                matches!(
                    case.kind,
                    ContractCaseKind::Compile | ContractCaseKind::Balance | ContractCaseKind::Sim
                ),
                "Strict non-parse contract {id} must use semantic coverage, found parse case {}",
                case.case_id
            );
        }
        assert!(
            has_case,
            "Strict non-parse contract {id} has no manifest cases"
        );
    }
}

#[test]
fn parse_only_coverage_is_limited_to_lex_contracts() {
    for case in implemented_contract_cases() {
        if matches!(case.kind, ContractCaseKind::Parse) {
            let is_lex = case.contract_id.starts_with("LEX-");
            let is_parse_enforced_non_lex =
                PARSE_ENFORCED_NON_LEX_CONTRACT_IDS.contains(&case.contract_id.as_str());
            assert!(
                is_lex || is_parse_enforced_non_lex,
                "Parse-only coverage must be lexical or explicitly parse-enforced; {} uses parse case {}",
                case.contract_id,
                case.case_id
            );
        }
    }
}

#[test]
fn contract_tests_do_not_use_generic_failure_helper() {
    let tests_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let entries = fs::read_dir(&tests_root).unwrap_or_else(|e| {
        panic!(
            "failed to read tests directory {}: {e}",
            tests_root.display()
        )
    });

    for entry in entries {
        let entry = entry.unwrap_or_else(|e| panic!("failed to read tests dir entry: {e}"));
        let path = entry.path();
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if !file_name.ends_with("_contracts.rs") {
            continue;
        }
        let content = fs::read_to_string(&path)
            .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
        assert!(
            !content.contains("expect_failure("),
            "generic expect_failure helper is disallowed in {}; use phase/code or parse/resolve code helpers",
            path.display()
        );
        assert!(
            !content.contains("expect_parse_err("),
            "generic expect_parse_err helper is disallowed in {}; use expect_parse_err_with_code",
            path.display()
        );
    }
}

#[test]
fn contract_tests_use_all_test_support_helpers() {
    let tests_root = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests");
    let entries = fs::read_dir(&tests_root).unwrap_or_else(|e| {
        panic!(
            "failed to read tests directory {}: {e}",
            tests_root.display()
        )
    });

    let mut content = String::new();
    for entry in entries {
        let entry = entry.unwrap_or_else(|e| panic!("failed to read tests dir entry: {e}"));
        let path = entry.path();
        let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if !file_name.ends_with("_contracts.rs") {
            continue;
        }
        content.push_str(
            &fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display())),
        );
        content.push('\n');
    }

    for helper in TEST_SUPPORT_HELPERS {
        let call = format!("{helper}(");
        assert!(
            content.contains(&call),
            "unused test_support helper {helper}; remove it from test_support or add a contract test that uses it"
        );
    }
}

fn is_contract_id(id: &str) -> bool {
    let Some((prefix, digits)) = id.split_once('-') else {
        return false;
    };
    !prefix.is_empty()
        && prefix.chars().all(|c| c.is_ascii_uppercase())
        && digits.len() == 3
        && digits.chars().all(|c| c.is_ascii_digit())
}
