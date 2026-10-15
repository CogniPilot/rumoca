//! Complete whole and selected numeric execution from checked scalar owners.
//! Backend optional hooks are private inputs, never runtime capabilities.

use super::*;
use indexmap::IndexMap;
mod reuse;
pub(super) use reuse::{BlockReuse, shared_projection_jacobians};

struct Program<C: ?Sized> {
    code: Rc<C>,
    outputs: usize,
    span: rumoca_core::Span,
}

fn prepare<C: ?Sized>(
    label: &str,
    source: &solve::ScalarProgramBlock,
    whole: &Rc<C>,
    mut bind: impl FnMut(&C, &solve::ScalarProgramBlock, usize) -> Result<Option<Rc<C>>, String>,
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
        let bound = bind(whole, source, index).map_err(|message| EvalSolveError::CompiledExecution {
            message: format!("selected {label} entry {index} could not bind its source: {message}"), span: Some(span),
        })?;
        let code = if let Some(bound) = bound {
            bound
        } else if one_dense_program {
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

fn source_program_outputs(
    source: &solve::ScalarProgramBlock,
    index: usize,
) -> Result<usize, RuntimeSolveError> {
    source
        .program(index)
        .map(solve::ScalarProgramBlock::program_output_count)
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir("selected program is outside its complete source")
        })
}
fn source_output(
    source: &solve::ScalarProgramBlock,
    coordinate: (usize, usize),
) -> Result<(), RuntimeSolveError> {
    if coordinate.1 >= source_program_outputs(source, coordinate.0)? {
        return Err(RuntimeSolveError::solve_ir(
            "selected output is outside its complete source",
        ));
    }
    Ok(())
}
fn source_bulk_plan(
    source: &solve::ScalarProgramBlock,
    coordinates: &[(usize, usize)],
    out_len: usize,
) -> Result<BulkPlan, RuntimeSolveError> {
    validate_runtime_output_len("selected bulk outputs", coordinates.len(), out_len)?;
    let mut plan = BulkPlan::new();
    for (destination, &coordinate) in coordinates.iter().enumerate() {
        source_output(source, coordinate)?;
        plan.entry(coordinate.0)
            .or_default()
            .push((coordinate.1, destination));
    }
    Ok(plan)
}
enum ExpressionExecution {
    Native {
        whole: Rc<dyn CompiledSolveExpression>,
        programs: Box<[Program<dyn CompiledSolveExpression>]>,
    },
    Shared(Rc<CompleteExpression>),
    Composite(reuse::ReplacedExpression),
}

pub(super) struct CompleteExpression {
    source: solve::ScalarProgramBlock,
    execution: ExpressionExecution,
    scratch: RefCell<Vec<f64>>,
    bulk_scratch: RefCell<Vec<f64>>,
    whole_scratch: RefCell<Vec<f64>>,
}

pub(super) fn expression(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    source: &solve::ScalarProgramBlock,
) -> Result<Rc<CompleteExpression>, EvalSolveError> {
    let whole = required_compiled(
        label,
        source.program_spans(),
        backend.compile_selectable_expression(source),
    )?;
    expression_from_whole(backend, label, source, whole)
}

pub(super) fn jacobian(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    source: &solve::ScalarProgramBlock,
) -> Result<Rc<CompleteJacobian>, EvalSolveError> {
    let whole = required_compiled(
        label,
        source.program_spans(),
        backend.compile_jacobian_expression(source),
    )?;
    jacobian_from_whole(backend, label, source, whole)
}

// Preparation receives the canonical scalar view of this retained ComputeBlock.
// Optional compact lowering may decline; required scalar lowering then supplies
// the whole compiled kernel. An admitted compile error remains an error.
pub(super) fn compute_expression(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    compute: &solve::ComputeBlock,
    source: &solve::ScalarProgramBlock,
) -> Result<Rc<CompleteExpression>, EvalSolveError> {
    let whole = match backend.compile_compute_expression(compute) {
        Ok(Some(code)) => code,
        Ok(None) => required_compiled(
            label,
            source.program_spans(),
            backend.compile_expression(source),
        )?,
        Err(error) => return required_compiled(label, source.program_spans(), Err(error)),
    };
    expression_from_whole(backend, label, source, whole)
}

pub(super) fn compute_jacobian(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    compute: &solve::ComputeBlock,
    source: &solve::ScalarProgramBlock,
) -> Result<Rc<CompleteJacobian>, EvalSolveError> {
    let whole = match backend.compile_compute_jacobian_expression(compute) {
        Ok(Some(code)) => code,
        Ok(None) => required_compiled(
            label,
            source.program_spans(),
            backend.compile_jacobian_expression(source),
        )?,
        Err(error) => return required_compiled(label, source.program_spans(), Err(error)),
    };
    jacobian_from_whole(backend, label, source, whole)
}

pub(super) fn expression_from_whole(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    source: &solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveExpression>,
) -> Result<Rc<CompleteExpression>, EvalSolveError> {
    let programs = prepare(
        label,
        source,
        &whole,
        |code, source, index| code.bind_program(source, index),
        |block| backend.compile_expression(block),
    )?;
    Ok(Rc::new(CompleteExpression {
        source: source.clone(),
        execution: ExpressionExecution::Native { whole, programs },
        scratch: RefCell::default(),
        bulk_scratch: RefCell::default(),
        whole_scratch: RefCell::default(),
    }))
}

impl CompleteExpression {
    fn shared(source: &solve::ScalarProgramBlock, original: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            source: source.clone(),
            execution: ExpressionExecution::Shared(original),
            scratch: RefCell::default(),
            bulk_scratch: RefCell::default(),
            whole_scratch: RefCell::default(),
        })
    }

    fn replaced(
        source: &solve::ScalarProgramBlock,
        primary: Rc<CompleteExpression>,
        mapping: reuse::ReplacedPrograms,
        replacement: Rc<CompleteExpression>,
    ) -> Result<Rc<Self>, EvalSolveError> {
        Ok(Rc::new(Self {
            source: source.clone(),
            execution: ExpressionExecution::Composite(reuse::ReplacedExpression::new(
                primary,
                mapping,
                replacement,
            )),
            scratch: RefCell::default(),
            bulk_scratch: RefCell::default(),
            whole_scratch: RefCell::default(),
        }))
    }

    pub(super) fn shares_source(&self, source: &solve::ScalarProgramBlock) -> bool {
        self.source.shares_program_owner(source)
    }

    pub(super) fn call_program_outputs(
        &self,
        index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        source_program_outputs(&self.source, index)?;
        let (whole, programs) = match &self.execution {
            ExpressionExecution::Native { whole, programs } => (whole, programs),
            ExpressionExecution::Shared(code) => {
                return code.call_program_outputs(index, y, p, t, tables, out);
            }
            ExpressionExecution::Composite(code) => {
                return code.call_program_outputs(index, y, p, t, tables, out);
            }
        };

        let code = program(programs, index)?;
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(&mut values, code.outputs, 0.0, "selected program outputs")?;
        if !whole
            .call_program_outputs(index, y, p, t, tables, &mut values)
            .map_err(|error| admitted_compiled_error("selected program", &[code.span], error))?
        {
            code.code
                .call(y, p, t, tables, &mut values)
                .map_err(|error| {
                    admitted_compiled_error("selected program", &[code.span], error)
                })?;
        }
        validate_runtime_output_len("selected program outputs", code.outputs, values.len())?;
        copy_runtime_values_into(out, &values, "selected program output commit")?;
        Ok(())
    }

    pub(super) fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, RuntimeSolveError> {
        source_output(&self.source, coordinate)?;
        let (whole, programs) = match &self.execution {
            ExpressionExecution::Native { whole, programs } => (whole, programs),
            ExpressionExecution::Shared(code) => {
                return code.call_program_output(coordinate, y, p, t, tables);
            }
            ExpressionExecution::Composite(code) => {
                return code.call_program_output(coordinate, y, p, t, tables);
            }
        };

        let code = output(programs, coordinate)?;
        if let Some(value) = whole
            .call_program_output(coordinate, y, p, t, tables)
            .map_err(|error| admitted_compiled_error("selected program", &[code.span], error))?
        {
            return Ok(value);
        }
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(&mut values, code.outputs, 0.0, "selected program outputs")?;
        code.code
            .call(y, p, t, tables, &mut values)
            .map_err(|error| admitted_compiled_error("selected program", &[code.span], error))?;
        Ok(values[coordinate.1])
    }

    pub(super) fn call_program_outputs_at(
        &self,
        coordinates: &[(usize, usize)],
        inputs: (&[f64], &[f64], f64),
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let plan = source_bulk_plan(&self.source, coordinates, out.len())?;
        let mut staged = self.bulk_scratch.borrow_mut();
        resize_runtime_values(&mut staged, out.len(), 0.0, "selected bulk transaction")?;
        let completed = match &self.execution {
            ExpressionExecution::Native { whole, .. } => whole
                .call_program_outputs_at(coordinates, inputs, tables, &mut staged)
                .map_err(|error| {
                    admitted_compiled_error(
                        "selected bulk outputs",
                        self.source.program_spans(),
                        error,
                    )
                })?,
            ExpressionExecution::Shared(code) => {
                code.call_program_outputs_at(coordinates, inputs, tables, &mut staged)?;
                true
            }
            ExpressionExecution::Composite(code) => {
                execute_bulk(plan, &mut staged, |index, values| {
                    code.call_program_outputs(index, inputs.0, inputs.1, inputs.2, tables, values)
                })?;
                out.copy_from_slice(&staged);
                return Ok(());
            }
        };
        if !completed {
            execute_bulk(plan, &mut staged, |index, values| {
                self.call_program_outputs(index, inputs.0, inputs.1, inputs.2, tables, values)
            })?;
        }
        out.copy_from_slice(&staged);
        Ok(())
    }

    pub(super) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        validate_runtime_output_len(
            "whole compiled outputs",
            self.source.output_count(),
            out.len(),
        )?;
        let mut staged = self.whole_scratch.borrow_mut();
        resize_runtime_values(&mut staged, out.len(), 0.0, "whole compiled transaction")?;
        staged.fill(0.0);
        match &self.execution {
            ExpressionExecution::Native { whole, .. } => {
                whole.call(y, p, t, tables, &mut staged).map_err(|error| {
                    admitted_compiled_error(
                        "whole compiled expression",
                        self.source.program_spans(),
                        error,
                    )
                })?;
            }
            ExpressionExecution::Shared(code) => {
                code.call(y, p, t, tables, &mut staged)?;
            }
            ExpressionExecution::Composite(code) => {
                code.call(y, p, t, tables, &mut staged)?;
            }
        }
        out.copy_from_slice(&staged);
        Ok(())
    }
}

enum JacobianExecution {
    Native {
        whole: Rc<dyn CompiledSolveJacobianExpression>,
        programs: Box<[Program<dyn CompiledSolveJacobianExpression>]>,
    },
    Shared(Rc<CompleteJacobian>),
    Composite(reuse::ReplacedJacobian),
}

pub(super) struct CompleteJacobian {
    source: solve::ScalarProgramBlock,
    execution: JacobianExecution,
    scratch: RefCell<Vec<f64>>,
    whole_scratch: RefCell<Vec<f64>>,
}

pub(super) fn jacobian_from_whole(
    backend: &dyn SolveExecutionBackend,
    label: &str,
    source: &solve::ScalarProgramBlock,
    whole: Rc<dyn CompiledSolveJacobianExpression>,
) -> Result<Rc<CompleteJacobian>, EvalSolveError> {
    let programs = prepare(
        label,
        source,
        &whole,
        |code, source, index| code.bind_program(source, index),
        |block| backend.compile_jacobian_expression(block),
    )?;
    Ok(Rc::new(CompleteJacobian {
        source: source.clone(),
        execution: JacobianExecution::Native { whole, programs },
        scratch: RefCell::default(),
        whole_scratch: RefCell::default(),
    }))
}

impl CompleteJacobian {
    fn shared(source: &solve::ScalarProgramBlock, original: Rc<Self>) -> Rc<Self> {
        Rc::new(Self {
            source: source.clone(),
            execution: JacobianExecution::Shared(original),
            scratch: RefCell::default(),
            whole_scratch: RefCell::default(),
        })
    }

    fn replaced(
        source: &solve::ScalarProgramBlock,
        primary: Rc<CompleteJacobian>,
        mapping: reuse::ReplacedPrograms,
        replacement: Rc<CompleteJacobian>,
    ) -> Result<Rc<Self>, EvalSolveError> {
        Ok(Rc::new(Self {
            source: source.clone(),
            execution: JacobianExecution::Composite(reuse::ReplacedJacobian::new(
                primary,
                mapping,
                replacement,
            )),
            scratch: RefCell::default(),
            whole_scratch: RefCell::default(),
        }))
    }

    pub(super) fn shares_source(&self, source: &solve::ScalarProgramBlock) -> bool {
        self.source.shares_program_owner(source)
    }

    pub(super) fn prepare_projections(
        &self,
        applications: &[&solve::ProjectionJacobianApplication],
    ) -> Result<Vec<Option<Rc<dyn CompiledSolveProjectionJacobian>>>, String> {
        match &self.execution {
            JacobianExecution::Native { whole, .. } => whole.prepare_projections(applications),
            JacobianExecution::Shared(_) => Ok(vec![None; applications.len()]),
            JacobianExecution::Composite(_) => Ok(vec![None; applications.len()]),
        }
    }

    pub(super) fn call_program_outputs(
        &self,
        index: usize,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        source_program_outputs(&self.source, index)?;
        let (whole, programs) = match &self.execution {
            JacobianExecution::Native { whole, programs } => (whole, programs),
            JacobianExecution::Shared(code) => {
                return code.call_program_outputs(index, inputs, tables, out);
            }
            JacobianExecution::Composite(code) => {
                return code.call_program_outputs(index, inputs, tables, out);
            }
        };

        let code = program(programs, index)?;
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut values,
            code.outputs,
            0.0,
            "selected directional outputs",
        )?;
        if !whole
            .call_program_outputs(index, inputs, tables, &mut values)
            .map_err(|error| {
                admitted_compiled_error("selected directional program", &[code.span], error)
            })?
        {
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
        }
        validate_runtime_output_len("selected directional outputs", code.outputs, values.len())?;
        copy_runtime_values_into(out, &values, "selected directional output commit")?;
        Ok(())
    }

    pub(super) fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
    ) -> Result<f64, RuntimeSolveError> {
        source_output(&self.source, coordinate)?;
        let (whole, programs) = match &self.execution {
            JacobianExecution::Native { whole, programs } => (whole, programs),
            JacobianExecution::Shared(code) => {
                return code.call_program_output(coordinate, y, p, t, seed, tables);
            }
            JacobianExecution::Composite(code) => {
                return code.call_program_output(coordinate, y, p, t, seed, tables);
            }
        };

        let code = output(programs, coordinate)?;
        if let Some(value) = whole
            .call_program_output(coordinate, y, p, t, seed, tables)
            .map_err(|error| {
                admitted_compiled_error("selected directional program", &[code.span], error)
            })?
        {
            return Ok(value);
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
        Ok(values[coordinate.1])
    }

    pub(super) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        validate_runtime_output_len(
            "whole compiled directional outputs",
            self.source.output_count(),
            out.len(),
        )?;
        let mut staged = self.whole_scratch.borrow_mut();
        resize_runtime_values(
            &mut staged,
            out.len(),
            0.0,
            "whole compiled directional transaction",
        )?;
        staged.fill(0.0);
        match &self.execution {
            JacobianExecution::Native { whole, .. } => {
                whole
                    .call(y, p, t, seed, tables, &mut staged)
                    .map_err(|error| {
                        admitted_compiled_error(
                            "whole compiled directional",
                            self.source.program_spans(),
                            error,
                        )
                    })?;
            }
            JacobianExecution::Shared(code) => {
                code.call(y, p, t, seed, tables, &mut staged)?;
            }
            JacobianExecution::Composite(code) => {
                code.call(y, p, t, seed, tables, &mut staged)?;
            }
        }
        out.copy_from_slice(&staged);
        Ok(())
    }
}
