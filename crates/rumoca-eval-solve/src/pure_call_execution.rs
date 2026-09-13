use rumoca_ir_solve::{
    SolvePureCallDirectionalSite, SolvePureCallOutput, SolvePureCallOwnerId, SolvePureCallSite,
    SolveValueType,
};

use crate::{EvalSolveError, Reg, invalid_row, typed_kind_from_scalar, typed_kind_to_scalar};

/// One checked call site at the final scalar runtime ABI boundary.
#[derive(Clone, Copy)]
pub enum PureCallInvocation<'a> {
    Primal(&'a SolvePureCallSite),
    Directional(&'a SolvePureCallDirectionalSite),
}

impl PureCallInvocation<'_> {
    pub fn owner(self) -> SolvePureCallOwnerId {
        match self {
            Self::Primal(site) => site.owner(),
            Self::Directional(site) => site.owner(),
        }
    }

    pub fn inputs(&self) -> &[SolveValueType] {
        match self {
            Self::Primal(site) => site.inputs(),
            Self::Directional(site) => site.inputs(),
        }
    }

    pub fn outputs(&self) -> &[SolvePureCallOutput] {
        match self {
            Self::Primal(site) => site.outputs(),
            Self::Directional(site) => site.outputs(),
        }
    }

    pub fn output_scalar_count(self) -> Option<usize> {
        match self {
            Self::Primal(site) => site.output_scalar_count(),
            Self::Directional(site) => site.output_scalar_count(),
        }
    }
}

/// Optional execution of a checked model's typed call relation.
///
/// Implementations belong to the execution adapter for that exact model and
/// must preserve its typed arithmetic, lazy regions, output order, and errors.
/// Inputs and outputs are invocation-local ABI payloads, never retained values.
pub trait PureCallExecution {
    fn call(
        &self,
        invocation: PureCallInvocation<'_>,
        input: &[f64],
        output: &mut [f64],
    ) -> Result<(), EvalSolveError>;
}

pub(super) fn eval_compiled_call(
    execution: &dyn PureCallExecution,
    invocation: PureCallInvocation<'_>,
    input_starts: &[Reg],
    read: &mut impl FnMut(Reg) -> Result<f64, EvalSolveError>,
) -> Result<Vec<f64>, EvalSolveError> {
    let mut input = Vec::new();
    for (&start, value_type) in input_starts.iter().zip(invocation.inputs()) {
        for offset in 0..value_type.scalar_count() as usize {
            input.push(typed_kind_to_scalar(read_input_element(
                start, offset, value_type, read,
            )?));
        }
    }
    let mut output = vec![
        0.0;
        invocation
            .output_scalar_count()
            .ok_or_else(|| invalid_row("pure-call output width overflows"))?
    ];
    execution.call(invocation, &input, &mut output)?;
    Ok(output)
}

pub(super) fn read_input_element(
    start: Reg,
    offset: usize,
    value_type: &SolveValueType,
    read: &mut impl FnMut(Reg) -> Result<f64, EvalSolveError>,
) -> Result<rumoca_ir_solve::SolveValueKind, EvalSolveError> {
    let register = start
        .checked_add(
            Reg::try_from(offset)
                .map_err(|_| invalid_row("pure-call input range exceeds register identity"))?,
        )
        .ok_or_else(|| invalid_row("pure-call input range overflows"))?;
    typed_kind_from_scalar(read(register)?, value_type)
}
