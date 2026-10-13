//! Checked predicate-only execution of exact event observation programs.

use std::borrow::Cow;
use std::cell::RefCell;
use std::collections::BTreeMap;

use rumoca_ir_solve::{
    AssertionProgramProof, CheckedAssertionInvocation, LinearOp, ScalarProgramBlock,
    SolveEventAction, SolvePureCallTable, SolveVisitor,
};

use crate::{
    AssertionInvocationEvaluation, AssertionReport, EvalSolveError, Reg, TypedValue,
    eval_assertion_invocation, invalid_row, typed_kind_from_scalar,
};

/// The exact programs one runtime has checked against its pure-call table and
/// event actions, derived once when the runtime is built. A program is a
/// program of a block or a region nested in one, identified by the slice it
/// occupies, so ownership is total over every block it was derived from.
#[derive(Clone, Debug, Default)]
pub struct EventObservationAuthority {
    programs: BTreeMap<(usize, usize), Option<AssertionProgramProof>>,
}

impl EventObservationAuthority {
    /// Check every observation of `blocks` against `table` and `actions` and
    /// record every program they contain.
    pub fn derive<'block>(
        table: &SolvePureCallTable,
        actions: &[SolveEventAction],
        blocks: impl IntoIterator<Item = &'block ScalarProgramBlock>,
    ) -> Result<Self, EvalSolveError> {
        let mut collector = ObservationPrograms {
            table,
            actions,
            programs: BTreeMap::new(),
        };
        for block in blocks {
            collector.visit_scalar_program_block(block)?;
        }
        Ok(Self {
            programs: collector.programs,
        })
    }
}

/// Immutable invocation/action authority, with reports private until the
/// complete accepted evaluation succeeds. Ordinary row contexts have none.
pub struct CheckedEventObservationContext<'model> {
    table: &'model SolvePureCallTable,
    actions: &'model [SolveEventAction],
    authority: Cow<'model, EventObservationAuthority>,
    reports: RefCell<Vec<AssertionReport>>,
}

impl<'model> CheckedEventObservationContext<'model> {
    /// A context over an authority derived when the runtime was built: no
    /// program is checked again.
    pub fn new(
        authority: &'model EventObservationAuthority,
        table: &'model SolvePureCallTable,
        actions: &'model [SolveEventAction],
    ) -> Self {
        Self {
            table,
            actions,
            authority: Cow::Borrowed(authority),
            reports: RefCell::default(),
        }
    }

    /// A context over the programs of one block, checked now.
    pub fn construct(
        table: &'model SolvePureCallTable,
        block: &'model ScalarProgramBlock,
        actions: &'model [SolveEventAction],
    ) -> Result<Self, EvalSolveError> {
        let authority = EventObservationAuthority::derive(table, actions, [block])?;
        Ok(Self {
            table,
            actions,
            authority: Cow::Owned(authority),
            reports: RefCell::default(),
        })
    }

    /// Consume reports only after the caller has accepted its entire batch.
    pub fn take_reports(&self) -> Vec<AssertionReport> {
        std::mem::take(&mut *self.reports.borrow_mut())
    }

    pub(crate) fn message_for_action(&self, action: &SolveEventAction) -> Option<String> {
        let index = self
            .actions
            .iter()
            .position(|issued| std::ptr::eq(issued, action))?;
        self.reports
            .borrow()
            .iter()
            .find(|report| {
                report.action_index == index
                    && report.kind == action.kind
                    && report.span == action.span
            })
            .map(|report| report.message.clone())
    }

    pub(crate) fn evaluate(
        &self,
        table: &SolvePureCallTable,
        program: &[LinearOp],
        operation: usize,
        mut read: impl FnMut(Reg) -> Result<f64, EvalSolveError>,
    ) -> Result<Vec<f64>, EvalSolveError> {
        let proof = self.authority.programs.get(&program_identity(program));
        let (true, Some(Some(proof))) = (std::ptr::eq(table, self.table), proof) else {
            return Err(invalid_row(
                "observation context does not own this exact program",
            ));
        };
        let Some(LinearOp::PureCallObservation {
            input_starts, site, ..
        }) = program.get(operation)
        else {
            return Err(invalid_row(
                "observation operation does not match its checked program",
            ));
        };
        let invocation = CheckedAssertionInvocation::with_proof(
            proof,
            self.table,
            program,
            operation,
            self.actions,
        )
        .ok_or_else(|| invalid_row("observation lost its exact invocation/action binding"))?;
        let mut arguments = Vec::with_capacity(input_starts.len());
        for (&start, value_type) in input_starts.iter().zip(site.value_site().inputs()) {
            let elements = (0..value_type.scalar_count())
                .map(|offset| {
                    let register = start
                        .checked_add(offset)
                        .ok_or_else(|| invalid_row("observation input range overflows"))?;
                    typed_kind_from_scalar(read(register)?, value_type)
                })
                .collect::<Result<Vec<_>, _>>()?;
            arguments.push(
                TypedValue::construct(value_type.clone(), elements)
                    .map_err(|error| invalid_row(error.to_string()))?,
            );
        }
        let reports = match eval_assertion_invocation(&invocation, &arguments)? {
            AssertionInvocationEvaluation::Complete { reports, .. }
            | AssertionInvocationEvaluation::Failed { reports } => reports,
            AssertionInvocationEvaluation::Fault { error, .. } => return Err(error),
        };
        let mut predicates = vec![1.0; site.output_scalar_count()];
        for report in &reports {
            let projection = invocation
                .projections()
                .iter()
                .find(|projection| projection.action_index() == report.action_index)
                .ok_or_else(|| invalid_row("observation report lost its checked action"))?;
            let position = site
                .predicate_outputs()
                .iter()
                .position(|&predicate| predicate == projection.predicate_output())
                .ok_or_else(|| invalid_row("observation report lost its predicate projection"))?;
            predicates[position] = 0.0;
        }
        self.reports.borrow_mut().extend(reports);
        Ok(predicates)
    }
}

fn program_identity(program: &[LinearOp]) -> (usize, usize) {
    (program.as_ptr() as usize, program.len())
}

struct ObservationPrograms<'model> {
    table: &'model SolvePureCallTable,
    actions: &'model [SolveEventAction],
    programs: BTreeMap<(usize, usize), Option<AssertionProgramProof>>,
}

impl ObservationPrograms<'_> {
    /// The register-flow proof of a program holding observations, after every
    /// observation of it is bound to its invocation and actions.
    fn prove(&self, program: &[LinearOp]) -> Result<Option<AssertionProgramProof>, EvalSolveError> {
        let mut observations = program
            .iter()
            .enumerate()
            .filter(|(_, op)| matches!(op, LinearOp::PureCallObservation { .. }))
            .peekable();
        if observations.peek().is_none() {
            return Ok(None);
        }
        let proof = AssertionProgramProof::derive(program)
            .ok_or_else(|| invalid_row("event observation program has no register-flow proof"))?;
        for (operation, _) in observations {
            CheckedAssertionInvocation::with_proof(
                &proof,
                self.table,
                program,
                operation,
                self.actions,
            )
            .ok_or_else(|| {
                invalid_row("event observation has no checked invocation/action binding")
            })?;
        }
        Ok(Some(proof))
    }
}

impl SolveVisitor for ObservationPrograms<'_> {
    type Error = EvalSolveError;

    fn enter_linear_op_slice(
        &mut self,
        _kind: rumoca_ir_solve::LinearOpSliceKind,
        program: &[LinearOp],
    ) -> Result<(), Self::Error> {
        let proof = self.prove(program)?;
        self.programs.insert(program_identity(program), proof);
        Ok(())
    }
}
