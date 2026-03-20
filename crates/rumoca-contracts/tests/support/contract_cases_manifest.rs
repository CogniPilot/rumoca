//! Contract-case manifest loaded from TOML data.
//!
//! This module provides the mapping from contract IDs to concrete test cases.
//! The data lives in `data/contract_cases.toml`; do not add entries here directly.

use serde::Deserialize;
use std::sync::OnceLock;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
pub(crate) enum ContractCaseKind {
    Parse,
    Compile,
    Balance,
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

pub(crate) fn implemented_contract_cases() -> impl Iterator<Item = &'static ContractCase> {
    CASES.get_or_init(load_cases).iter()
}
