//! Contract-case manifest loaded from TOML data.
//!
//! This module provides the mapping from contract IDs to concrete test cases.
//! The data lives in `data/contract_cases.toml`; do not add entries here directly.

use rumoca_contracts::ContractStatus;
use serde::Deserialize;
use std::sync::OnceLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub(crate) enum ContractCaseKind {
    Parse,
    Compile,
    Balance,
    /// Compile + simulate, asserting runtime-semantic behavior.
    Sim,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub(crate) enum ContractCaseOutcome {
    Accept,
    Reject,
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub(crate) struct ContractCase {
    pub contract_id: String,
    pub case_id: String,
    pub test_file: String,
    pub kind: ContractCaseKind,
    pub outcome: ContractCaseOutcome,
}

#[derive(Deserialize)]
struct CasesFile {
    cases: Vec<ContractCase>,
}

static CASES: OnceLock<Vec<ContractCase>> = OnceLock::new();

fn load_cases() -> Vec<ContractCase> {
    let raw = include_str!("../../data/contract_cases.toml");
    let parsed: CasesFile = toml::from_str(raw).expect("contract_cases.toml must be valid TOML");
    parsed.cases
}

pub(crate) fn contract_cases() -> impl Iterator<Item = &'static ContractCase> {
    CASES.get_or_init(load_cases).iter()
}

/// The canonical status lookup for a manifest row.
///
/// The manifest names contract IDs as text; the registry is the only authority
/// for what a contract is. An ID the registry does not know is a broken
/// manifest, not a contract whose status happens to be "not Implemented", so
/// this fails closed and names the exact ID. A membership test that answered
/// `false` here would drop the row out of every filtered view and leave the
/// breakage to be noticed, or not, by whichever sibling test happens to run.
pub(crate) fn manifest_contract_status(contract_id: &str) -> ContractStatus {
    rumoca_contracts::registry_template()
        .get(contract_id)
        .unwrap_or_else(|| {
            panic!("contract-case manifest references unknown contract ID: {contract_id}")
        })
        .status
}

pub(crate) fn implemented_contract_cases() -> impl Iterator<Item = &'static ContractCase> {
    contract_cases()
        .filter(|case| manifest_contract_status(&case.contract_id) == ContractStatus::Implemented)
}
