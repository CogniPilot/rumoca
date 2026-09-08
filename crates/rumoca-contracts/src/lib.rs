//! MLS Contract Testing Framework for the Rumoca Compiler.
//!
//! This crate provides infrastructure for testing compliance with the
//! Modelica Language Specification (MLS) based on SPEC_0022.
//!
//! # Overview
//!
//! This framework catalogs MLS contracts across their language categories and:
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
pub use registry::formal::{
    EnforcementStatus, FormalStatement, FormalStatementError, MlsEdition, PinPolarity, QuoteKind,
    StatementPin, StatementTier, load_all_formal_statements, parse_formal_statements,
};
pub use registry::{Contract, ContractCategory, ContractId, ContractRegistry, ContractStatus};
pub use report::ComplianceReport;
pub use runner::{ContractResult, TestRunner};

static FORMAL_STATEMENTS: OnceLock<Vec<FormalStatement>> = OnceLock::new();

/// The formal-statement registry: which formal statement justifies each pinned
/// behavior, and whether it is spec-sourced or oracle-implied.
///
/// See [`registry::formal`] for the tiers and how rows are added. The table is
/// parsed once per process; a malformed row panics on first access, because it
/// ships inside the binary.
pub fn formal_statements() -> &'static [FormalStatement] {
    FORMAL_STATEMENTS.get_or_init(load_all_formal_statements)
}

static REGISTRY_TEMPLATE: OnceLock<ContractRegistry> = OnceLock::new();

fn build_registry() -> ContractRegistry {
    ContractRegistry::from_contracts(registry::load_all_contracts())
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
