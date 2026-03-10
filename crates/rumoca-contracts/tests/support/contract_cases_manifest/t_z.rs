//! Explicit contract-case manifest entries for TYPE contracts.

use super::{ContractCase, ContractCaseKind, ContractCaseOutcome};

pub(super) static IMPLEMENTED_CONTRACT_CASES_T_Z: &[ContractCase] = &[
    ContractCase {
        contract_id: "TYPE-005",
        case_id: "type_005_class_component_mismatch",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Reject,
    },
    ContractCase {
        contract_id: "TYPE-009",
        case_id: "type_009_variability_compatible",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-011",
        case_id: "type_011_dimension_match",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-013",
        case_id: "type_013_enumeration_basic",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-013",
        case_id: "type_013_enumeration_mismatch_fails",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Reject,
    },
    ContractCase {
        contract_id: "TYPE-013",
        case_id: "type_enumeration_usage",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-014",
        case_id: "type_014_builtin_type_match",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Balance,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-028",
        case_id: "type_028_record_mismatch_fails",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Reject,
    },
    ContractCase {
        contract_id: "TYPE-030",
        case_id: "type_030_existing_modifier_target_is_allowed",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-030",
        case_id: "type_030_missing_modifier_target_fails",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Reject,
    },
    ContractCase {
        contract_id: "TYPE-033",
        case_id: "type_033_integer_to_real_coercion",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Balance,
        outcome: ContractCaseOutcome::Accept,
    },
    ContractCase {
        contract_id: "TYPE-034",
        case_id: "type_034_integer_division_real",
        test_file: "tests/type_contracts.rs",
        kind: ContractCaseKind::Compile,
        outcome: ContractCaseOutcome::Accept,
    },
];
