// Auto-generated from dump_contract_table.rs. Do not edit by hand.
// Source: rumoca-contracts registry snapshot
use super::*;

mod alg;
mod ann;
mod arr;
mod clk;
mod conn;
mod decl;
mod eqn;
mod expr;
mod func;
mod inst;
mod lex;
mod oprec;
mod pkg;
mod sim;
mod sm;
mod strm;
mod r#type;
mod unit;

pub static CONTRACT_TABLE: &[&[StaticContract]] = &[
    lex::CONTRACTS_LEX,
    decl::CONTRACTS_DECL,
    inst::CONTRACTS_INST,
    expr::CONTRACTS_EXPR,
    eqn::CONTRACTS_EQN,
    alg::CONTRACTS_ALG,
    conn::CONTRACTS_CONN,
    func::CONTRACTS_FUNC,
    r#type::CONTRACTS_TYPE,
    arr::CONTRACTS_ARR,
    pkg::CONTRACTS_PKG,
    oprec::CONTRACTS_OPREC,
    sim::CONTRACTS_SIM,
    clk::CONTRACTS_CLK,
    strm::CONTRACTS_STRM,
    sm::CONTRACTS_SM,
    ann::CONTRACTS_ANN,
    unit::CONTRACTS_UNIT,
];
