//! Contract coverage guards: keep registry status aligned with explicit test-case mapping.

mod support;

use rumoca_contracts::{ContractStatus, IMPLEMENTED_CONTRACT_IDS, create_registry};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;
use support::contract_cases_manifest::{
    ContractCaseKind, ContractCaseOutcome, implemented_contract_cases,
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
    "EXPR-014", "FUNC-006", "FUNC-017", "INST-003", "INST-012", "PKG-012",
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
];

#[test]
fn implemented_contract_ids_match_registry_and_manifest() {
    let registry = create_registry();

    let registry_ids: BTreeSet<String> = registry.all().map(|c| c.id.to_string()).collect();
    let implemented_registry_ids: BTreeSet<String> = registry
        .by_status(ContractStatus::Implemented)
        .map(|c| c.id.to_string())
        .collect();
    let expected_implemented_ids: BTreeSet<String> = IMPLEMENTED_CONTRACT_IDS
        .iter()
        .map(|id| id.to_string())
        .collect();
    let manifest_implemented_ids: BTreeSet<String> = implemented_contract_cases()
        .map(|case| case.contract_id.to_string())
        .collect();

    assert_eq!(
        implemented_registry_ids, expected_implemented_ids,
        "Registry Implemented status set diverges from IMPLEMENTED_CONTRACT_IDS"
    );
    assert_eq!(
        manifest_implemented_ids, expected_implemented_ids,
        "Explicit contract-case manifest diverges from IMPLEMENTED_CONTRACT_IDS"
    );

    let unknown_manifest_ids: Vec<String> = manifest_implemented_ids
        .difference(&registry_ids)
        .cloned()
        .collect();
    assert!(
        unknown_manifest_ids.is_empty(),
        "Contract-case manifest references unknown contract IDs: {unknown_manifest_ids:?}"
    );
}

#[test]
fn contract_case_manifest_is_well_formed() {
    assert!(
        implemented_contract_cases().next().is_some(),
        "Contract-case manifest must not be empty"
    );

    let mut case_ids = BTreeSet::new();
    let tests_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut file_cache: BTreeMap<String, String> = BTreeMap::new();

    for case in implemented_contract_cases() {
        assert!(
            is_contract_id(case.contract_id),
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

        let test_path = tests_root.join(case.test_file);
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
fn every_implemented_contract_has_explicit_cases() {
    let expected_implemented_ids: BTreeSet<&str> =
        IMPLEMENTED_CONTRACT_IDS.iter().copied().collect();
    let mut counts: BTreeMap<&str, (usize, usize)> = BTreeMap::new(); // (accept, reject)

    for case in implemented_contract_cases() {
        let entry = counts.entry(case.contract_id).or_insert((0, 0));
        match case.outcome {
            ContractCaseOutcome::Accept => entry.0 += 1,
            ContractCaseOutcome::Reject => entry.1 += 1,
        }
    }

    for id in expected_implemented_ids {
        let (accept, reject) = counts
            .get(id)
            .copied()
            .unwrap_or_else(|| panic!("Implemented contract {id} has no explicit cases"));
        assert!(
            accept + reject > 0,
            "Implemented contract {id} has no accepted/rejected case mapping"
        );
    }
}

#[test]
fn strict_dual_case_contracts_keep_accept_and_reject_coverage() {
    let mut counts: BTreeMap<&str, (usize, usize)> = BTreeMap::new(); // (accept, reject)
    for case in implemented_contract_cases() {
        let entry = counts.entry(case.contract_id).or_insert((0, 0));
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
                    ContractCaseKind::Compile | ContractCaseKind::Balance
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
                PARSE_ENFORCED_NON_LEX_CONTRACT_IDS.contains(&case.contract_id);
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
