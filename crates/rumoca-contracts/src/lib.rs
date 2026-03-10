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
    "ALG-005",
    "ALG-007",
    "ALG-008",
    "ALG-009",
    "ALG-010",
    "ALG-012",
    "ALG-013",
    "ALG-016",
    "ANN-001",
    "ANN-008",
    "ARR-001",
    "ARR-002",
    "ARR-003",
    "ARR-005",
    "ARR-006",
    "ARR-009",
    "ARR-017",
    "ARR-018",
    "ARR-019",
    "ARR-023",
    "ARR-027",
    "ARR-029",
    "ARR-030",
    "CLK-001",
    "CLK-002",
    "CLK-013",
    "CLK-020",
    "CONN-001",
    "CONN-002",
    "CONN-003",
    "CONN-007",
    "CONN-008",
    "CONN-009",
    "CONN-010",
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
    "EXPR-006",
    "EXPR-008",
    "EXPR-009",
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
    "EXPR-026",
    "EXPR-027",
    "EXPR-028",
    "EXPR-029",
    "EXPR-033",
    "EXPR-035",
    "EXPR-036",
    "EXPR-037",
    "EXPR-039",
    "EXPR-040",
    "FUNC-001",
    "FUNC-002",
    "FUNC-006",
    "FUNC-007",
    "FUNC-010",
    "FUNC-011",
    "FUNC-012",
    "FUNC-014",
    "FUNC-015",
    "FUNC-017",
    "INST-001",
    "INST-002",
    "INST-003",
    "INST-006",
    "INST-007",
    "INST-008",
    "INST-010",
    "INST-011",
    "INST-012",
    "INST-014",
    "INST-016",
    "INST-022",
    "INST-027",
    "INST-029",
    "INST-034",
    "INST-037",
    "INST-039",
    "INST-043",
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
    "OPREC-001",
    "OPREC-002",
    "OPREC-003",
    "OPREC-004",
    "OPREC-008",
    "OPREC-010",
    "PKG-001",
    "PKG-002",
    "PKG-003",
    "PKG-005",
    "PKG-006",
    "PKG-007",
    "PKG-008",
    "PKG-009",
    "PKG-010",
    "PKG-011",
    "PKG-012",
    "SIM-002",
    "SIM-003",
    "SIM-004",
    "SIM-005",
    "SIM-009",
    "STRM-001",
    "STRM-002",
    "STRM-003",
    "STRM-006",
    "STRM-010",
    "STRM-011",
    "TYPE-005",
    "TYPE-009",
    "TYPE-011",
    "TYPE-013",
    "TYPE-014",
    "TYPE-028",
    "TYPE-030",
    "TYPE-033",
    "TYPE-034",
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
