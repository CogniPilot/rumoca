//! The value enums that select what a `rumoca compile` invocation produces.
//!
//! `EmitTarget` deliberately stays in `cli.rs`: the acceptance-surface gate
//! reads that file for the emit stages the CLI registers, so the declaration
//! and the gate that enumerates it live in one place.

use crate::TemplateIr;

/// Compiler stage selected by one closed `--emit <stage>-<format>` spelling.
///
/// This is deliberately not a clap value: users select an exact artifact with
/// `EmitTarget`, while this internal key shares the stage-specific dump code.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum EmitStage {
    /// Abstract syntax tree (resolved).
    Ast,
    /// Flattened model.
    Flat,
    /// DAE system.
    Dae,
    /// Solver IR.
    Solve,
}

impl From<EmitStage> for TemplateIr {
    fn from(phase: EmitStage) -> Self {
        match phase {
            EmitStage::Ast => TemplateIr::Ast,
            EmitStage::Flat => TemplateIr::Flat,
            EmitStage::Dae => TemplateIr::Dae,
            EmitStage::Solve => TemplateIr::Solve,
        }
    }
}
