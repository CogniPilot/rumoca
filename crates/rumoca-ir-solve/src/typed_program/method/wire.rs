//! Constructor-replayed wire decoding for checked methods.
//!
//! The wire carries constructor *inputs* only. Every fact a constructor
//! produces — method identity, the derived call ABI, scope/cell/closure arenas,
//! loop binders, the caught set of a signal check, and the closure that check
//! issues — is absent and re-issued by replaying the same builder production
//! code uses. Records are private and deny unknown fields, so bytes can neither
//! forge a derived fact nor smuggle one back in beside its input.
//!
//! An addressed entity is named by a wire-local ordinal, never by a Solve
//! identity: no `SolveMethodId`, `SolveCellId`, or `SolveSignalClosureId` is
//! decodable, and every ordinal is resolved into the identity the builder
//! already issued, which also proves that the entity exists.

use rumoca_core::{Span, StructuredIndexDomain};
use serde::{Deserialize, Deserializer};

use super::super::call::SolvePureCallTable;
use super::super::effect::{SolveSignalSet, SolveValueRange};
use super::super::program::wire::{TypedProgramWire, replay_program};
use super::super::program::{SolveProgramRegion, construct_region};
use super::super::types::{SolveArithmeticProfile, SolveValueType};
use super::action::{SolveBranchConditionSpec, SolveSignalTest, SolveValueProgram};
use super::builder::{MethodCell, SolveLimitTargetSpec, SolveMethodBuilder};
use super::{
    SolveActionConstructionError, SolveMethodInterface, SolveMethodKind, SolveMethodTable,
};

/// The wire-local ordinal of one entity a method body addresses.
///
/// It is deliberately not a Solve identity: it carries no authority of its own
/// and is only ever resolved through the constructor that issued the entity.
type Ordinal = u32;

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct MethodTableWire {
    arithmetic: SolveArithmeticProfile,
    signals: SolveSignalSet,
    calls: SolvePureCallTable,
    methods: Vec<MethodWire>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct MethodWire {
    kind: SolveMethodKind,
    interface: SolveMethodInterface,
    body: BlockWire,
    provenance: Span,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct BlockWire {
    locals: Vec<LocalWire>,
    actions: Vec<SpannedActionWire>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct LocalWire {
    value_type: SolveValueType,
    range: Option<SolveValueRange>,
    provenance: Span,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SpannedActionWire {
    action: ActionWire,
    provenance: Span,
}

#[derive(Deserialize)]
#[serde(tag = "action", rename_all = "snake_case", deny_unknown_fields)]
enum ActionWire {
    Assign {
        program: ValueProgramWire,
        targets: Vec<Ordinal>,
    },
    Branch {
        condition: ConditionWire,
        if_true: BlockWire,
        if_false: BlockWire,
    },
    Loop {
        domain: StructuredIndexDomain,
        body: BlockWire,
    },
    Invoke {
        method: Ordinal,
        arguments: Vec<Ordinal>,
        results: Vec<Ordinal>,
    },
    Limit {
        targets: Vec<LimitTargetWire>,
    },
    Signal {
        signals: SolveSignalSet,
        closures: Vec<Ordinal>,
    },
}

#[derive(Deserialize)]
#[serde(
    tag = "condition",
    content = "check",
    rename_all = "snake_case",
    deny_unknown_fields
)]
enum ConditionWire {
    Value(ValueProgramWire),
    Signal(SignalCheckWire),
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SignalCheckWire {
    test: Option<SignalTestWire>,
    capture_closure: bool,
    fallback: Option<ValueProgramWire>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SignalTestWire {
    negated: bool,
    signals: SolveSignalSet,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ValueProgramWire {
    reads: Vec<Ordinal>,
    region: RegionWire,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RegionWire {
    inputs: Vec<SolveValueType>,
    outputs: Vec<SolveValueType>,
    body: TypedProgramWire,
    provenance: Span,
}

#[derive(Deserialize)]
#[serde(
    tag = "target",
    content = "cell",
    rename_all = "snake_case",
    deny_unknown_fields
)]
enum LimitTargetWire {
    RangedState,
    Cell(Ordinal),
}

impl<'de> Deserialize<'de> for SolveMethodTable {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = MethodTableWire::deserialize(deserializer)?;
        replay_table(&wire).map_err(serde::de::Error::custom)
    }
}

fn replay_table(wire: &MethodTableWire) -> Result<SolveMethodTable, SolveActionConstructionError> {
    let mut builder = SolveMethodTable::builder(wire.arithmetic, wire.signals, wire.calls.clone())?;
    for method in &wire.methods {
        builder.add_method(
            method.kind,
            method.interface.clone(),
            method.provenance,
            |builder, _cells| replay_block(builder, &method.body),
        )?;
    }
    Ok(builder.finish())
}

fn replay_block(
    builder: &mut SolveMethodBuilder<'_>,
    block: &BlockWire,
) -> Result<(), SolveActionConstructionError> {
    for local in &block.locals {
        builder.declare_local(
            local.value_type.clone(),
            local.range.clone(),
            local.provenance,
        )?;
    }
    for action in &block.actions {
        replay_action(builder, action)?;
    }
    Ok(())
}

fn replay_action(
    builder: &mut SolveMethodBuilder<'_>,
    spanned: &SpannedActionWire,
) -> Result<(), SolveActionConstructionError> {
    let at = spanned.provenance;
    match &spanned.action {
        ActionWire::Assign { program, targets } => {
            let (reads, region) = replay_reads_and_region(builder, program)?;
            let targets = cell_handles(builder, targets, at)?;
            builder.assign_from_region(&targets, &reads, region, at)
        }
        ActionWire::Branch {
            condition,
            if_true,
            if_false,
        } => {
            let spec = replay_condition(builder, condition, at)?;
            builder.branch(
                spec,
                at,
                |builder, _issued| replay_block(builder, if_true),
                |builder| replay_block(builder, if_false),
            )
        }
        ActionWire::Loop { domain, body } => {
            builder.iterate(domain.clone(), at, |builder, _| replay_block(builder, body))
        }
        ActionWire::Invoke {
            method: callee,
            arguments,
            results,
        } => {
            let callee = builder.callee_identity(*callee, at)?;
            let arguments = cell_handles(builder, arguments, at)?;
            let results = cell_handles(builder, results, at)?;
            builder.invoke(callee, &arguments, &results, at)
        }
        ActionWire::Limit { targets } => {
            let mut specs = Vec::with_capacity(targets.len());
            for target in targets {
                specs.push(match target {
                    LimitTargetWire::RangedState => SolveLimitTargetSpec::RangedState,
                    LimitTargetWire::Cell(ordinal) => {
                        SolveLimitTargetSpec::Cell(builder.cell_handle(*ordinal, at)?)
                    }
                });
            }
            builder.limit(&specs, at)
        }
        ActionWire::Signal { signals, closures } => {
            let mut handles = Vec::with_capacity(closures.len());
            for closure in closures {
                handles.push(builder.closure_handle(*closure, at)?);
            }
            builder.raise(*signals, &handles, at)
        }
    }
}

fn replay_condition(
    builder: &SolveMethodBuilder<'_>,
    wire: &ConditionWire,
    provenance: Span,
) -> Result<SolveBranchConditionSpec, SolveActionConstructionError> {
    match wire {
        ConditionWire::Value(program) => Ok(SolveBranchConditionSpec::Value(replay_value_program(
            builder, program,
        )?)),
        ConditionWire::Signal(check) => {
            let test = match &check.test {
                Some(test) => Some(SolveSignalTest::construct(
                    test.negated,
                    test.signals,
                    provenance,
                )?),
                None => None,
            };
            let fallback = match &check.fallback {
                Some(program) => Some(replay_value_program(builder, program)?),
                None => None,
            };
            Ok(SolveBranchConditionSpec::Signal {
                test,
                capture_closure: check.capture_closure,
                fallback,
            })
        }
    }
}

fn cell_handles<'method>(
    builder: &SolveMethodBuilder<'method>,
    ordinals: &[Ordinal],
    provenance: Span,
) -> Result<Vec<MethodCell<'method>>, SolveActionConstructionError> {
    ordinals
        .iter()
        .map(|ordinal| builder.cell_handle(*ordinal, provenance))
        .collect()
}

fn replay_region(
    builder: &SolveMethodBuilder<'_>,
    wire: &RegionWire,
) -> Result<SolveProgramRegion, SolveActionConstructionError> {
    let body = replay_program(&wire.body, builder.call_interfaces())?;
    Ok(construct_region(
        wire.inputs.clone(),
        wire.outputs.clone(),
        body,
        wire.provenance,
    )?)
}

fn replay_reads_and_region<'method>(
    builder: &SolveMethodBuilder<'method>,
    wire: &ValueProgramWire,
) -> Result<(Vec<MethodCell<'method>>, SolveProgramRegion), SolveActionConstructionError> {
    let reads = cell_handles(builder, &wire.reads, wire.region.provenance)?;
    let region = replay_region(builder, &wire.region)?;
    Ok((reads, region))
}

fn replay_value_program(
    builder: &SolveMethodBuilder<'_>,
    wire: &ValueProgramWire,
) -> Result<SolveValueProgram, SolveActionConstructionError> {
    let (reads, region) = replay_reads_and_region(builder, wire)?;
    builder.value_program_from_region(&reads, region, wire.region.provenance)
}
