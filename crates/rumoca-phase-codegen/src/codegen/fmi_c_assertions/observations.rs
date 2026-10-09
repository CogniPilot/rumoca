//! C message selectors consumed from checked invocation bindings.

use super::*;
use rumoca_ir_solve::{
    CheckedAssertionInvocation, CheckedAssertionMessagePart, SolvePureCallTable, SolveVisitor,
};
use std::collections::BTreeMap;

#[derive(Serialize)]
pub(in crate::codegen) struct ObservedAction {
    index: usize,
    kind: &'static str,
    parts: Vec<MessagePart>,
}

#[derive(Serialize)]
pub(in crate::codegen) struct Observation {
    owner: u32,
    predicate: usize,
    predicate_index: usize,
    capture_count: usize,
    actions: Vec<ObservedAction>,
}

pub(in crate::codegen) fn observations(
    problem: &SolveProblem,
    table: &SolvePureCallTable,
) -> Result<Vec<Observation>, CodegenError> {
    let mut visitor = Observations {
        problem,
        table,
        issued: BTreeMap::new(),
    };
    visitor.visit_scalar_program_block(&problem.events.root_conditions)?;
    visitor.visit_scalar_program_block(&problem.events.action_conditions)?;
    Ok(visitor.issued.into_values().collect())
}

struct Observations<'model> {
    problem: &'model SolveProblem,
    table: &'model SolvePureCallTable,
    issued: BTreeMap<(u32, usize), Observation>,
}

impl SolveVisitor for Observations<'_> {
    type Error = CodegenError;

    fn visit_linear_op_slice(
        &mut self,
        kind: rumoca_ir_solve::LinearOpSliceKind,
        ops: &[LinearOp],
    ) -> Result<(), Self::Error> {
        for (index, op) in ops.iter().enumerate() {
            if !matches!(op, LinearOp::PureCallObservation { .. }) {
                continue;
            }
            let invocation = CheckedAssertionInvocation::new(
                self.table,
                ops,
                index,
                &self.problem.events.actions,
            )
            .ok_or_else(|| {
                CodegenError::template("assertion observation lost its checked invocation")
            })?;
            self.insert(&invocation)?;
        }
        rumoca_ir_solve::visitor::walk_linear_op_slice(self, kind, ops)
    }
}

impl Observations<'_> {
    fn insert(&mut self, invocation: &CheckedAssertionInvocation<'_>) -> Result<(), CodegenError> {
        for projection in invocation.projections() {
            let predicate = projection.predicate_output();
            let owner = invocation.owner();
            let captures: Vec<_> = owner
                .outputs()
                .iter()
                .enumerate()
                .filter_map(|(index, output)| {
                    (output.message_predicate_output(index) == Some(predicate)).then_some(index)
                })
                .collect();
            let key = (owner.id().index(), predicate);
            if self.issued.get(&key).is_some_and(|observation| {
                observation
                    .actions
                    .iter()
                    .any(|action| action.index == projection.action_index())
            }) {
                continue;
            }
            let parts = projection
                .message()
                .iter()
                .map(|part| message_part(part, &captures))
                .collect::<Result<Vec<_>, _>>()?;
            let observation = self.issued.entry(key).or_insert_with(|| Observation {
                owner: key.0,
                predicate,
                predicate_index: owner.outputs()[..predicate]
                    .iter()
                    .filter(|output| output.assertion_level().is_some())
                    .count(),
                capture_count: captures.len(),
                actions: Vec::new(),
            });
            observation.actions.push(ObservedAction {
                index: projection.action_index(),
                kind: match projection.action().kind {
                    rumoca_ir_solve::SolveEventActionKind::Assert => "error",
                    rumoca_ir_solve::SolveEventActionKind::Warning => "warning",
                    _ => {
                        return Err(CodegenError::template(
                            "checked assertion action changed its severity",
                        ));
                    }
                },
                parts,
            });
        }
        Ok(())
    }
}

fn message_part(
    part: &CheckedAssertionMessagePart<'_>,
    captures: &[usize],
) -> Result<MessagePart, CodegenError> {
    match part {
        CheckedAssertionMessagePart::Text(text) => Ok(MessagePart::Text(text.as_bytes().to_vec())),
        CheckedAssertionMessagePart::Conversion(conversion) => {
            let selector = |value: rumoca_ir_solve::AssertionCaptureSelector| {
                captures
                    .iter()
                    .position(|&index| index == value.output())
                    .ok_or_else(|| {
                        CodegenError::template(
                            "checked assertion capture changed its output association",
                        )
                    })
            };
            Ok(MessagePart::Conversion(Conversion {
                source: conversion_source(conversion.source),
                value: selector(conversion.value)?,
                minimum_length: conversion.minimum_length.map(selector).transpose()?,
                left_justified: conversion.left_justified.map(selector).transpose()?,
                significant_digits: conversion.significant_digits.map(selector).transpose()?,
            }))
        }
    }
}
