//! Explicit contract-case manifest for implemented MLS contracts.
//!
//! This table is the source of truth for mapping contract IDs to concrete tests.
//! It replaces heuristic discovery from test function names.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContractCaseKind {
    Parse,
    Compile,
    Balance,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ContractCaseOutcome {
    Accept,
    Reject,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ContractCase {
    pub contract_id: &'static str,
    pub case_id: &'static str,
    pub test_file: &'static str,
    pub kind: ContractCaseKind,
    pub outcome: ContractCaseOutcome,
}

mod a_c;
mod d_f;
mod i_l;
mod m_s;
mod t_z;

pub(crate) static IMPLEMENTED_CONTRACT_CASE_GROUPS: &[&[ContractCase]] = &[
    a_c::IMPLEMENTED_CONTRACT_CASES_A_C,
    d_f::IMPLEMENTED_CONTRACT_CASES_D_F,
    i_l::IMPLEMENTED_CONTRACT_CASES_I_L,
    m_s::IMPLEMENTED_CONTRACT_CASES_M_S,
    t_z::IMPLEMENTED_CONTRACT_CASES_T_Z,
];

pub(crate) fn implemented_contract_cases() -> impl Iterator<Item = &'static ContractCase> {
    IMPLEMENTED_CONTRACT_CASE_GROUPS
        .iter()
        .flat_map(|cases| cases.iter())
}
