//! MLS Contract Testing Framework for the Rumoca Compiler.
//!
//! This crate provides infrastructure for testing compliance with the
//! Modelica Language Specification (MLS) based on SPEC_0022.
//!
//! # Overview
//!
//! The MLS defines 431 contracts across 18 categories. This framework:
//! - Registers all contracts with metadata
//! - Provides test infrastructure and macros
//! - Tracks compliance status
//! - Generates compliance reports
//!
//! # Usage
//!
//! ```rust,ignore
//! use rumoca_contracts::{contract_test, ContractStatus};
//!
//! contract_test!(LEX_001, "ASCII identifiers", {
//!     // Test that identifiers are restricted to ASCII
//!     let result = parse("model Tëst end Tëst;");
//!     assert!(result.is_err());
//! });
//! ```

pub mod registry;
pub mod report;
pub mod runner;
pub mod test_support;
use std::sync::OnceLock;

// Re-export main types
pub use registry::{Contract, ContractCategory, ContractId, ContractRegistry, ContractStatus};
pub use report::ComplianceReport;
pub use runner::{ContractResult, TestRunner};

/// Contract IDs currently backed by explicit tests in `crates/rumoca-contracts/tests`.
///
/// This list is intentionally strict and is validated by
/// `tests/contract_coverage.rs` so registry status and test coverage stay aligned.
pub const IMPLEMENTED_CONTRACT_IDS: &[&str] = &[
    "ALG-001",
    "ALG-002",
    "ALG-003",
    "ALG-004",
    "ALG-005",
    "ALG-006",
    "ALG-007",
    "ALG-008",
    "ALG-009",
    "ALG-010",
    "ALG-011",
    "ALG-012",
    "ALG-013",
    "ALG-014",
    "ALG-015",
    "ALG-016",
    "ALG-017",
    "ANN-001",
    "ANN-002",
    "ANN-003",
    "ANN-004",
    "ANN-005",
    "ANN-006",
    "ANN-007",
    "ANN-008",
    "ANN-009",
    "ANN-010",
    "ANN-011",
    "ANN-012",
    "ANN-013",
    "ANN-014",
    "ANN-015",
    "ARR-001",
    "ARR-002",
    "ARR-003",
    "ARR-004",
    "ARR-005",
    "ARR-006",
    "ARR-007",
    "ARR-008",
    "ARR-009",
    "ARR-010",
    "ARR-011",
    "ARR-012",
    "ARR-013",
    "ARR-014",
    "ARR-015",
    "ARR-016",
    "ARR-017",
    "ARR-018",
    "ARR-019",
    "ARR-020",
    "ARR-021",
    "ARR-022",
    "ARR-023",
    "ARR-024",
    "ARR-025",
    "ARR-026",
    "ARR-027",
    "ARR-028",
    "ARR-029",
    "ARR-030",
    "ARR-031",
    "ARR-032",
    "ARR-033",
    "ARR-034",
    "ARR-035",
    "ARR-036",
    "ARR-037",
    "ARR-038",
    "ARR-039",
    "ARR-040",
    "CLK-001",
    "CLK-002",
    "CLK-003",
    "CLK-004",
    "CLK-005",
    "CLK-006",
    "CLK-007",
    "CLK-008",
    "CLK-009",
    "CLK-010",
    "CLK-011",
    "CLK-012",
    "CLK-013",
    "CLK-014",
    "CLK-015",
    "CLK-016",
    "CLK-017",
    "CLK-018",
    "CLK-019",
    "CLK-020",
    "CONN-001",
    "CONN-002",
    "CONN-007",
    "CONN-008",
    "CONN-017",
    "CONN-026",
    "CONN-029",
    "DECL-001",
    "DECL-002",
    "DECL-003",
    "DECL-004",
    "DECL-005",
    "DECL-006",
    "DECL-007",
    "DECL-009",
    "DECL-012",
    "DECL-014",
    "DECL-015",
    "DECL-018",
    "DECL-019",
    "DECL-020",
    "DECL-022",
    "DECL-024",
    "DECL-032",
    "DECL-036",
    "EQN-001",
    "EQN-002",
    "EQN-003",
    "EQN-004",
    "EQN-005",
    "EQN-006",
    "EQN-008",
    "EQN-010",
    "EQN-011",
    "EQN-013",
    "EQN-015",
    "EQN-016",
    "EQN-021",
    "EQN-024",
    "EQN-025",
    "EQN-029",
    "EQN-033",
    "EQN-037",
    "EXPR-001",
    "EXPR-002",
    "EXPR-003",
    "EXPR-004",
    "EXPR-005",
    "EXPR-012",
    "EXPR-013",
    "EXPR-014",
    "EXPR-015",
    "EXPR-016",
    "EXPR-017",
    "EXPR-018",
    "EXPR-019",
    "EXPR-024",
    "EXPR-025",
    "EXPR-039",
    "EXPR-040",
    "FUNC-001",
    "FUNC-002",
    "FUNC-003",
    "FUNC-004",
    "FUNC-005",
    "FUNC-006",
    "FUNC-007",
    "FUNC-008",
    "FUNC-009",
    "FUNC-010",
    "FUNC-011",
    "FUNC-012",
    "FUNC-013",
    "FUNC-014",
    "FUNC-015",
    "FUNC-016",
    "FUNC-017",
    "FUNC-018",
    "FUNC-019",
    "FUNC-020",
    "FUNC-021",
    "FUNC-022",
    "FUNC-023",
    "FUNC-024",
    "FUNC-025",
    "FUNC-026",
    "FUNC-027",
    "FUNC-028",
    "FUNC-029",
    "FUNC-030",
    "FUNC-031",
    "FUNC-032",
    "FUNC-033",
    "FUNC-034",
    "FUNC-035",
    "INST-001",
    "INST-002",
    "INST-003",
    "INST-004",
    "INST-005",
    "INST-006",
    "INST-007",
    "INST-008",
    "INST-009",
    "INST-010",
    "INST-011",
    "INST-012",
    "INST-013",
    "INST-014",
    "INST-015",
    "INST-016",
    "INST-017",
    "INST-018",
    "INST-019",
    "INST-020",
    "INST-021",
    "INST-022",
    "INST-023",
    "INST-024",
    "INST-025",
    "INST-026",
    "INST-027",
    "INST-028",
    "INST-029",
    "INST-030",
    "INST-031",
    "INST-032",
    "INST-033",
    "INST-034",
    "INST-035",
    "INST-036",
    "INST-037",
    "INST-038",
    "INST-039",
    "INST-040",
    "INST-041",
    "INST-042",
    "INST-043",
    "INST-044",
    "INST-045",
    "INST-046",
    "INST-047",
    "INST-048",
    "INST-049",
    "INST-050",
    "INST-051",
    "INST-052",
    "INST-053",
    "LEX-001",
    "LEX-002",
    "LEX-003",
    "LEX-004",
    "LEX-005",
    "LEX-006",
    "LEX-007",
    "LEX-008",
    "LEX-009",
    "LEX-010",
    "LEX-011",
    "LEX-012",
    "LEX-013",
    "PKG-001",
    "PKG-002",
    "PKG-003",
    "PKG-004",
    "PKG-005",
    "PKG-006",
    "PKG-007",
    "PKG-008",
    "PKG-009",
    "PKG-010",
    "PKG-011",
    "PKG-012",
    "OPREC-001",
    "OPREC-002",
    "OPREC-003",
    "OPREC-004",
    "OPREC-005",
    "OPREC-006",
    "OPREC-007",
    "OPREC-008",
    "OPREC-009",
    "OPREC-010",
    "OPREC-011",
    "SIM-001",
    "SIM-002",
    "SIM-003",
    "SIM-004",
    "SIM-005",
    "SIM-006",
    "SIM-007",
    "SIM-008",
    "SIM-009",
    "SM-001",
    "SM-002",
    "SM-003",
    "SM-004",
    "SM-005",
    "SM-006",
    "SM-007",
    "SM-008",
    "STRM-001",
    "STRM-002",
    "STRM-003",
    "STRM-004",
    "STRM-005",
    "STRM-006",
    "STRM-007",
    "STRM-008",
    "STRM-009",
    "STRM-010",
    "STRM-011",
    "TYPE-001",
    "TYPE-002",
    "TYPE-003",
    "TYPE-004",
    "TYPE-005",
    "TYPE-006",
    "TYPE-007",
    "TYPE-008",
    "TYPE-009",
    "TYPE-010",
    "TYPE-011",
    "TYPE-012",
    "TYPE-013",
    "TYPE-014",
    "TYPE-015",
    "TYPE-016",
    "TYPE-017",
    "TYPE-018",
    "TYPE-019",
    "TYPE-020",
    "TYPE-021",
    "TYPE-022",
    "TYPE-023",
    "TYPE-024",
    "TYPE-025",
    "TYPE-026",
    "TYPE-027",
    "TYPE-028",
    "TYPE-029",
    "TYPE-030",
    "TYPE-031",
    "TYPE-032",
    "TYPE-033",
    "TYPE-034",
    "TYPE-035",
    "UNIT-001",
    "UNIT-002",
    "UNIT-003",
    "UNIT-004",
    "UNIT-005",
    "UNIT-006",
    "UNIT-007",
    "UNIT-008",
    "UNIT-009",
];

static REGISTRY_TEMPLATE: OnceLock<ContractRegistry> = OnceLock::new();

fn build_registry() -> ContractRegistry {
    ContractRegistry::from_static_tables(registry::CONTRACT_TABLE)
}

/// Get a shared read-only registry template.
///
/// The template is built once per process and reused by [`create_registry`].
pub fn registry_template() -> &'static ContractRegistry {
    REGISTRY_TEMPLATE.get_or_init(build_registry)
}

/// Create the global contract registry with all MLS contracts.
pub fn create_registry() -> ContractRegistry {
    registry_template().clone()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_has_all_contracts() {
        let registry = create_registry();
        // SPEC_0022 defines 431 contracts
        assert_eq!(
            registry.len(),
            431,
            "Expected 431 contracts, got {}",
            registry.len()
        );
    }

    #[test]
    fn test_registry_categories() {
        let registry = create_registry();

        // Verify counts per category match SPEC_0022
        assert_eq!(registry.count_by_category(ContractCategory::Lexical), 13);
        assert_eq!(
            registry.count_by_category(ContractCategory::Declaration),
            36
        );
        assert_eq!(
            registry.count_by_category(ContractCategory::Instantiation),
            53
        );
        assert_eq!(registry.count_by_category(ContractCategory::Expression), 40);
        assert_eq!(registry.count_by_category(ContractCategory::Equation), 38);
        assert_eq!(registry.count_by_category(ContractCategory::Algorithm), 17);
        assert_eq!(registry.count_by_category(ContractCategory::Connection), 29);
        assert_eq!(registry.count_by_category(ContractCategory::Function), 35);
        assert_eq!(registry.count_by_category(ContractCategory::Type), 35);
        assert_eq!(registry.count_by_category(ContractCategory::Array), 40);
        assert_eq!(registry.count_by_category(ContractCategory::Package), 12);
        assert_eq!(
            registry.count_by_category(ContractCategory::OperatorRecord),
            11
        );
        assert_eq!(registry.count_by_category(ContractCategory::Simulation), 9);
        assert_eq!(registry.count_by_category(ContractCategory::Clock), 20);
        assert_eq!(registry.count_by_category(ContractCategory::Stream), 11);
        assert_eq!(
            registry.count_by_category(ContractCategory::StateMachine),
            8
        );
        assert_eq!(registry.count_by_category(ContractCategory::Annotation), 15);
        assert_eq!(registry.count_by_category(ContractCategory::Unit), 9);
    }

    #[test]
    fn test_registry_implemented_contract_count() {
        let registry = create_registry();
        assert_eq!(
            registry.count_by_status(ContractStatus::Implemented),
            IMPLEMENTED_CONTRACT_IDS.len()
        );
    }

    #[test]
    fn test_implemented_list_matches_registry_status_set() {
        use std::collections::BTreeSet;
        let registry = create_registry();
        let implemented_registry: BTreeSet<String> = registry
            .by_status(ContractStatus::Implemented)
            .map(|c| c.id.to_string())
            .collect();
        let implemented_const: BTreeSet<String> = IMPLEMENTED_CONTRACT_IDS
            .iter()
            .map(|id| id.to_string())
            .collect();
        assert_eq!(
            implemented_registry, implemented_const,
            "Static registry table Implemented statuses must match IMPLEMENTED_CONTRACT_IDS"
        );
    }

    #[test]
    fn test_create_registry_returns_isolated_clone() {
        let mut first = create_registry();
        first.set_status("LEX-001", ContractStatus::Deferred);
        let second = create_registry();
        assert_eq!(
            second.get("LEX-001").map(|c| c.status),
            Some(ContractStatus::Implemented)
        );
    }

    #[test]
    fn test_registry_template_singleton_address() {
        let a = registry_template() as *const ContractRegistry;
        let b = registry_template() as *const ContractRegistry;
        assert_eq!(a, b, "template should be initialized once");
    }
}
