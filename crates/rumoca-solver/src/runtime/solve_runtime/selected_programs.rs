//! Required selected entry points backed by prepared whole-program kernels.

use super::*;
use indexmap::IndexMap;

struct Program<C: ?Sized> {
    code: Rc<C>,
    outputs: usize,
    span: rumoca_core::Span,
}

fn prepare<C: ?Sized>(
    label: &str,
    source: &solve::ScalarProgramBlock,
    whole: &Rc<C>,
    mut compile: impl FnMut(&solve::ScalarProgramBlock) -> Result<Rc<C>, String>,
) -> Result<Box<[Program<C>]>, EvalSolveError> {
    let one_dense_program = source.row_count() == 1
        && source
            .output_indices()
            .iter()
            .copied()
            .eq(0..source.stored_output_count());
    source.programs().iter().enumerate().map(|(index, ops)| {
        let span = source.program_span(index).ok_or_else(|| EvalSolveError::InvalidRow {
            message: format!("selected {label} program {index} has no source span"), span: None,
        })?;
        let outputs = solve::ScalarProgramBlock::program_output_count(ops);
        let code = if one_dense_program {
            Rc::clone(whole)
        } else {
            let block = solve::ScalarProgramBlock::with_program_spans(vec![ops.clone()], vec![span])
                .map_err(|error| EvalSolveError::InvalidRow { message: error.to_string(), span: Some(span) })?;
            compile(&block).map_err(|message| EvalSolveError::CompiledExecution {
                message: format!("required selected program `{label}[{index}]` could not be lowered: {message}"), span: Some(span),
            })?
        };
        Ok(Program { code, outputs, span })
    }).collect()
}

fn program<C: ?Sized>(
    programs: &[Program<C>],
    index: usize,
) -> Result<&Program<C>, RuntimeSolveError> {
    programs.get(index).ok_or_else(|| {
        RuntimeSolveError::solve_ir(format!(
            "selected program {index} is outside its prepared source"
        ))
    })
}

fn output<C: ?Sized>(
    programs: &[Program<C>],
    coordinate: (usize, usize),
) -> Result<&Program<C>, RuntimeSolveError> {
    let code = program(programs, coordinate.0)?;
    if coordinate.1 >= code.outputs {
        return Err(RuntimeSolveError::solve_ir(
            "selected output is outside its prepared program",
        ));
    }
    Ok(code)
}

type BulkPlan = IndexMap<usize, Vec<(usize, usize)>>;

fn bulk_plan<C: ?Sized>(
    programs: &[Program<C>],
    coordinates: &[(usize, usize)],
    out_len: usize,
) -> Result<BulkPlan, RuntimeSolveError> {
    validate_runtime_output_len("selected bulk outputs", coordinates.len(), out_len)?;
    let mut plan = BulkPlan::new();
    for (destination, &coordinate) in coordinates.iter().enumerate() {
        output(programs, coordinate)?;
        plan.entry(coordinate.0)
            .or_default()
            .push((coordinate.1, destination));
    }
    Ok(plan)
}

fn execute_bulk(
    plan: BulkPlan,
    out: &mut [f64],
    mut call: impl FnMut(usize, &mut Vec<f64>) -> Result<(), RuntimeSolveError>,
) -> Result<(), RuntimeSolveError> {
    let mut values = Vec::new();
    for (index, placements) in plan {
        call(index, &mut values)?;
        for (offset, destination) in placements {
            out[destination] = *values.get(offset).ok_or_else(|| {
                RuntimeSolveError::solve_ir("prepared selected bulk output is absent")
            })?;
        }
    }
    Ok(())
}

struct Expression {
    whole: Rc<dyn CompiledSolveExpression>,
    programs: Box<[Program<dyn CompiledSolveExpression>]>,
    scratch: RefCell<Vec<f64>>,
    bulk_scratch: RefCell<Vec<f64>>,
}

pub(super) fn expression(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    source: &solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveExpression>,
) -> Result<Rc<dyn CompiledSolveExpression>, EvalSolveError> {
    let programs = prepare(label, source, &whole, |block| {
        backend.compile_expression(block)
    })?;
    Ok(Rc::new(Expression {
        whole,
        programs,
        scratch: RefCell::default(),
        bulk_scratch: RefCell::default(),
    }))
}

impl CompiledSolveExpression for Expression {
    fn call_program_outputs(
        &self,
        index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        let code = program(&self.programs, index)?;
        if self
            .whole
            .call_program_outputs(index, y, p, t, tables, out)?
        {
            return Ok(true);
        }
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(&mut values, code.outputs, 0.0, "selected program outputs")?;
        code.code
            .call(y, p, t, tables, &mut values)
            .map_err(|error| admitted_compiled_error("selected program", &[code.span], error))?;
        copy_runtime_values_into(out, &values, "selected program output commit")?;
        Ok(true)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let code = output(&self.programs, coordinate)?;
        if let Some(value) = self
            .whole
            .call_program_output(coordinate, y, p, t, tables)?
        {
            return Ok(Some(value));
        }
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(&mut values, code.outputs, 0.0, "selected program outputs")?;
        code.code
            .call(y, p, t, tables, &mut values)
            .map_err(|error| admitted_compiled_error("selected program", &[code.span], error))?;
        Ok(Some(values[coordinate.1]))
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: (&[f64], &[f64], f64),
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let plan = bulk_plan(&self.programs, coordinates, out.len())?;
        let mut staged = self.bulk_scratch.borrow_mut();
        resize_runtime_values(&mut staged, out.len(), 0.0, "selected bulk transaction")?;
        if !self
            .whole
            .call_program_outputs_at(coordinates, inputs, tables, &mut staged)?
        {
            execute_bulk(plan, &mut staged, |index, values| {
                self.call_program_outputs(index, inputs.0, inputs.1, inputs.2, tables, values)
                    .map(|_| ())
            })?;
        }
        out.copy_from_slice(&staged);
        Ok(true)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.whole.call(y, p, t, tables, out)
    }
}

struct Jacobian {
    whole: Rc<dyn CompiledSolveJacobianExpression>,
    programs: Box<[Program<dyn CompiledSolveJacobianExpression>]>,
    scratch: RefCell<Vec<f64>>,
    bulk_scratch: RefCell<Vec<f64>>,
}

pub(super) fn jacobian(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    source: &solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveJacobianExpression>,
) -> Result<Rc<dyn CompiledSolveJacobianExpression>, EvalSolveError> {
    let programs = prepare(label, source, &whole, |block| {
        backend.compile_jacobian_expression(block)
    })?;
    Ok(Rc::new(Jacobian {
        whole,
        programs,
        scratch: RefCell::default(),
        bulk_scratch: RefCell::default(),
    }))
}

impl CompiledSolveJacobianExpression for Jacobian {
    fn prepare_projections(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        self.whole.prepare_projections(applications)
    }

    fn call_program_outputs(
        &self,
        index: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        let code = program(&self.programs, index)?;
        if self
            .whole
            .call_program_outputs(index, inputs, tables, out)?
        {
            return Ok(true);
        }
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut values,
            code.outputs,
            0.0,
            "selected directional outputs",
        )?;
        code.code
            .call(
                inputs.y,
                inputs.p,
                inputs.t,
                inputs.seed,
                tables,
                &mut values,
            )
            .map_err(|error| {
                admitted_compiled_error("selected directional program", &[code.span], error)
            })?;
        copy_runtime_values_into(out, &values, "selected directional output commit")?;
        Ok(true)
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let code = output(&self.programs, coordinate)?;
        if let Some(value) = self
            .whole
            .call_program_output(coordinate, y, p, t, seed, tables)?
        {
            return Ok(Some(value));
        }
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut values,
            code.outputs,
            0.0,
            "selected directional outputs",
        )?;
        code.code
            .call(y, p, t, seed, tables, &mut values)
            .map_err(|error| {
                admitted_compiled_error("selected directional program", &[code.span], error)
            })?;
        Ok(Some(values[coordinate.1]))
    }

    fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let plan = bulk_plan(&self.programs, coordinates, out.len())?;
        let mut staged = self.bulk_scratch.borrow_mut();
        resize_runtime_values(
            &mut staged,
            out.len(),
            0.0,
            "selected directional bulk transaction",
        )?;
        if !self
            .whole
            .call_program_outputs_at(coordinates, inputs, tables, &mut staged)?
        {
            execute_bulk(plan, &mut staged, |index, values| {
                self.call_program_outputs(index, inputs, tables, values)
                    .map(|_| ())
            })?;
        }
        out.copy_from_slice(&staged);
        Ok(true)
    }

    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.whole.call(y, p, t, seed, tables, out)
    }
}
