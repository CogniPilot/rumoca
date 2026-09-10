//! Final evaluator adapter for one checked tensor-native event transaction.

use rumoca_ir_solve as solve;

use crate::{EvalSolveError, TypedValue, typed_kind_from_scalar, typed_kind_to_scalar};

/// Prepared evaluator boundary for one [`solve::EventTransactionProgram`].
///
/// Aggregate inputs and outputs remain typed ranges in Solve IR. This adapter
/// materializes their scalar payload only while invoking the checked owner.
#[derive(Clone)]
pub struct PreparedEventTransactionProgram {
    program: solve::EventTransactionProgram,
    input_scalar_count: usize,
    output_scalar_count: usize,
    target_scalar_count: usize,
    scalar_targets: Box<[solve::ScalarSlot]>,
}

impl PreparedEventTransactionProgram {
    pub fn new(
        program: &solve::EventTransactionProgram,
        table: &solve::SolvePureCallTable,
    ) -> Result<Self, EvalSolveError> {
        if !table.matches_site(program.site()) {
            return Err(shape_error(
                program,
                "event transaction does not match its issued pure-call owner",
            ));
        }
        let input_scalar_count = checked_scalar_count(
            program.inputs().iter().map(|input| input.value_type()),
            program,
        )?;
        let output_scalar_count = program.site().output_scalar_count().ok_or_else(|| {
            shape_error(program, "event transaction output scalar count overflows")
        })?;
        let target_scalar_count = checked_scalar_count(
            program.targets().iter().map(|target| target.value_type()),
            program,
        )?;
        let mut scalar_targets = Vec::with_capacity(target_scalar_count);
        for target in program.targets() {
            for offset in 0..target.value_type().scalar_count() as usize {
                scalar_targets.push(offset_slot(target.base(), offset, program)?);
            }
        }
        Ok(Self {
            program: program.clone(),
            input_scalar_count,
            output_scalar_count,
            target_scalar_count,
            scalar_targets: scalar_targets.into_boxed_slice(),
        })
    }

    #[must_use]
    pub const fn program(&self) -> &solve::EventTransactionProgram {
        &self.program
    }

    #[must_use]
    pub const fn input_scalar_count(&self) -> usize {
        self.input_scalar_count
    }

    #[must_use]
    pub const fn output_scalar_count(&self) -> usize {
        self.output_scalar_count
    }

    #[must_use]
    pub const fn target_scalar_count(&self) -> usize {
        self.target_scalar_count
    }

    /// Final evaluator storage adapter for the checked compact target ranges.
    #[must_use]
    pub const fn scalar_targets(&self) -> &[solve::ScalarSlot] {
        &self.scalar_targets
    }

    /// Evaluate the complete target/predicate tuple in one invocation scope.
    pub fn eval(
        &self,
        table: &solve::SolvePureCallTable,
        y: &[f64],
        p: &[f64],
        time: f64,
        output: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        let mut input = Vec::with_capacity(self.input_scalar_count);
        self.load_input_payload(y, p, time, &mut input)?;
        self.eval_payload(table, &input, output)
    }

    /// Materialize the checked aggregate input tuple at the evaluator/native
    /// ABI boundary without changing its IR ownership or shape.
    pub fn load_input_payload(
        &self,
        y: &[f64],
        p: &[f64],
        time: f64,
        input: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        input.clear();
        input.reserve(self.input_scalar_count);
        for aggregate in self.program.inputs() {
            for offset in 0..aggregate.value_type().scalar_count() as usize {
                input.push(read_storage(
                    aggregate.source(),
                    offset,
                    y,
                    p,
                    time,
                    &self.program,
                )?);
            }
        }
        Ok(())
    }

    /// Evaluate an already materialized checked aggregate input tuple.
    pub fn eval_payload(
        &self,
        table: &solve::SolvePureCallTable,
        input: &[f64],
        output: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        if input.len() != self.input_scalar_count {
            return Err(shape_error(
                &self.program,
                "event transaction input payload has the wrong scalar width",
            ));
        }
        let mut arguments = Vec::with_capacity(self.program.inputs().len());
        let mut input_offset = 0usize;
        for aggregate in self.program.inputs() {
            let value_type = aggregate.value_type();
            let mut elements = Vec::with_capacity(value_type.scalar_count() as usize);
            let end = input_offset + value_type.scalar_count() as usize;
            for &scalar in &input[input_offset..end] {
                elements.push(typed_kind_from_scalar(scalar, value_type)?);
            }
            input_offset = end;
            arguments.push(
                TypedValue::construct(value_type.clone(), elements)
                    .map_err(|error| shape_error(&self.program, error.to_string()))?,
            );
        }
        let values = crate::eval_pure_call(table, self.program.site().owner(), &arguments)
            .map_err(|error| EvalSolveError::ShapeContract {
                message: error.to_string(),
                span: error.source_span().or(Some(self.program.span())),
            })?;
        output.clear();
        output.reserve(self.output_scalar_count);
        for value in values {
            output.extend(value.elements().iter().copied().map(typed_kind_to_scalar));
        }
        if output.len() != self.output_scalar_count {
            return Err(shape_error(
                &self.program,
                "event transaction owner returned the wrong scalar payload width",
            ));
        }
        Ok(())
    }
}

fn checked_scalar_count<'a>(
    types: impl IntoIterator<Item = &'a solve::SolveValueType>,
    program: &solve::EventTransactionProgram,
) -> Result<usize, EvalSolveError> {
    types.into_iter().try_fold(0usize, |count, value_type| {
        count
            .checked_add(value_type.scalar_count() as usize)
            .ok_or_else(|| shape_error(program, "event transaction scalar count overflows"))
    })
}

fn read_storage(
    source: solve::ScalarSlot,
    offset: usize,
    y: &[f64],
    p: &[f64],
    time: f64,
    program: &solve::EventTransactionProgram,
) -> Result<f64, EvalSolveError> {
    let value = match source {
        solve::ScalarSlot::Y { index, .. } => y.get(
            index
                .checked_add(offset)
                .ok_or_else(|| shape_error(program, "event transaction Y input range overflows"))?,
        ),
        solve::ScalarSlot::P { index, .. } => p.get(
            index
                .checked_add(offset)
                .ok_or_else(|| shape_error(program, "event transaction P input range overflows"))?,
        ),
        solve::ScalarSlot::Time if offset == 0 => return Ok(time),
        solve::ScalarSlot::Constant(value) if offset == 0 => return Ok(value),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    };
    value.copied().ok_or_else(|| {
        shape_error(
            program,
            "event transaction input is outside runtime storage",
        )
    })
}

fn offset_slot(
    base: solve::ScalarSlot,
    offset: usize,
    program: &solve::EventTransactionProgram,
) -> Result<solve::ScalarSlot, EvalSolveError> {
    match base {
        solve::ScalarSlot::Y { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_y)
            .ok_or_else(|| shape_error(program, "event transaction Y target range overflows")),
        solve::ScalarSlot::P { index, .. } => index
            .checked_add(offset)
            .map(solve::scalar_slot_p)
            .ok_or_else(|| shape_error(program, "event transaction P target range overflows")),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => Err(shape_error(
            program,
            "event transaction target is not mutable storage",
        )),
    }
}

fn shape_error(
    program: &solve::EventTransactionProgram,
    message: impl Into<String>,
) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: message.into(),
        span: Some(program.span()),
    }
}
