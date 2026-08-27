//! The value enums that select what a `rumoca compile` invocation produces.
//!
//! Which IR a raw `.jinja` template receives (`--phase`), and the two
//! independent emission axes a GALEC-derived target runs under: how much CALL
//! structure it keeps (`--inline-policy`) and whether it may expand a tensor
//! operation (`--scalarize-policy`), plus the `--emission-policy` shorthand
//! that names three points in that space. They live together because each is a
//! pure clap-facing selector with a single conversion into the compiler's own
//! vocabulary, and none carries behaviour beyond that conversion.
//!
//! `EmitTarget` deliberately stays in `cli.rs`: the acceptance-surface gate
//! reads that file for the emit stages the CLI registers, so the declaration
//! and the gate that enumerates it live in one place.

use clap::ValueEnum;

use crate::TemplateIr;

/// Compiler stage whose IR a raw `.jinja` `--target` consumes (`compile --phase`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum CompilePhase {
    /// Abstract syntax tree (resolved).
    Ast,
    /// Flattened model.
    Flat,
    /// DAE system.
    Dae,
    /// Solver IR.
    Solve,
}

impl From<CompilePhase> for TemplateIr {
    fn from(phase: CompilePhase) -> Self {
        match phase {
            CompilePhase::Ast => TemplateIr::Ast,
            CompilePhase::Flat => TemplateIr::Flat,
            CompilePhase::Dae => TemplateIr::Dae,
            CompilePhase::Solve => TemplateIr::Solve,
        }
    }
}

/// How much of the source CALL structure a GALEC-derived artifact keeps
/// (`--inline-policy`).
///
/// Information-preserving: an inlined contraction is still a contraction, and
/// no floating-point operation is reordered, so every setting computes
/// bit-identical values and every setting stays eligible for the certification
/// path. It trades flash per duplicated call site for the marshalling at each
/// boundary and for the call chain that pins working memory.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, ValueEnum)]
pub enum InlinePolicyArg {
    /// Every call stays a call, whatever the model asks for.
    None,
    /// Only what `annotation(Inline = true)` / `LateInline = true` asks for.
    Annotated,
    /// Annotated requests plus the sites the cost model decides pay off.
    #[default]
    CostModel,
    /// Every call site the legality rules allow.
    All,
}

/// Whether a tensor operation may be expanded into per-element statements
/// (`--scalarize-policy`).
///
/// Information-destroying: index sets, symmetry, bandedness and tensor
/// identity do not survive expansion and no later pass can recover them, only
/// guess. The default declines, and any other setting taints the artifact for
/// the certification path and says so in the emitted header.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, ValueEnum)]
pub enum ScalarizePolicyArg {
    /// A tensor operation stays one operation.
    #[default]
    Never,
    /// Expand where a cost model decides the per-element form is cheaper.
    CostModel,
    /// Expand every tensor operation.
    All,
}

/// A shorthand naming one point in the (inline, scalarize) space
/// (`--emission-policy`).
///
/// Convenience only. Every point the presets name is also reachable by setting
/// the two axes directly, and the combinations they do NOT name (inlined code
/// that keeps its tensors, most of all) are reachable only that way.
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmissionPolicyArg {
    /// `--inline-policy none --scalarize-policy never`: every boundary and
    /// every tensor the model wrote is still there.
    Reviewable,
    /// `--inline-policy cost-model --scalarize-policy never`.
    Balanced,
    /// `--inline-policy all --scalarize-policy all`: the CasADi shape.
    Flat,
}

impl From<InlinePolicyArg> for rumoca_ir_galec::package::InlinePolicy {
    fn from(value: InlinePolicyArg) -> Self {
        match value {
            InlinePolicyArg::None => Self::None,
            InlinePolicyArg::Annotated => Self::Annotated,
            InlinePolicyArg::CostModel => Self::CostModel,
            InlinePolicyArg::All => Self::All,
        }
    }
}

impl From<ScalarizePolicyArg> for rumoca_ir_galec::package::ScalarizePolicy {
    fn from(value: ScalarizePolicyArg) -> Self {
        match value {
            ScalarizePolicyArg::Never => Self::Never,
            ScalarizePolicyArg::CostModel => Self::CostModel,
            ScalarizePolicyArg::All => Self::All,
        }
    }
}

impl From<EmissionPolicyArg> for rumoca_ir_galec::package::EmissionPolicy {
    fn from(value: EmissionPolicyArg) -> Self {
        match value {
            EmissionPolicyArg::Reviewable => Self::reviewable(),
            EmissionPolicyArg::Balanced => Self::balanced(),
            EmissionPolicyArg::Flat => Self::flat(),
        }
    }
}

/// Resolve the two axes a `compile` invocation runs under.
///
/// The preset and the axes are alternative spellings of the same thing, so
/// naming both is a mistake worth reporting rather than a precedence rule
/// worth inventing.
pub(super) fn resolve_emission_policy(
    preset: Option<EmissionPolicyArg>,
    inline: Option<InlinePolicyArg>,
    scalarize: Option<ScalarizePolicyArg>,
) -> rumoca_ir_galec::package::EmissionPolicy {
    if let Some(preset) = preset {
        return preset.into();
    }
    rumoca_ir_galec::package::EmissionPolicy {
        inline: inline.unwrap_or_default().into(),
        scalarize: scalarize.unwrap_or_default().into(),
    }
}
