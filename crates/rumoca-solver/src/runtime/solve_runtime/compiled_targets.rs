//! Private target tuples executed through an already compiled source program.

use super::*;

#[derive(Clone)]
struct TargetBinding {
    code: Rc<dyn CompiledSolveTargetValues>,
    selected: usize,
}

#[derive(Clone, Default)]
pub(super) struct TargetQueries {
    bindings: FxHashMap<(usize, usize, usize), TargetBinding>,
}

impl TargetQueries {
    pub(super) fn prepare(
        source: &PreparedScalarProgramBlock,
        candidates: impl IntoIterator<Item = (usize, usize)>,
        backend: &dyn SolveExecutionBackend,
        context: RowEvalContext<'_>,
    ) -> Result<Self, EvalSolveError> {
        let mut result = Self::default();
        let mut families = FxHashMap::default();
        for (row, target) in candidates {
            let (program, output) =
                source
                    .row_output_position(row)
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: format!("issued target-query row {row} has no source program"),
                        span: None,
                    })?;
            let key = (program, output, target);
            if result.bindings.contains_key(&key) {
                continue;
            }
            let Some(plan) = source.portable_target_value_plan(program, output, target)? else {
                continue;
            };
            let family = (program, plan.canonical_prefix_len());
            let code = match families.get(&family) {
                Some(code) => Rc::clone(code),
                None => {
                    let code =
                        backend
                            .compile_target_values(&plan, context)
                            .map_err(|message| EvalSolveError::CompiledExecution {
                                message: format!(
                                    "required private target tuple could not be lowered: {message}"
                                ),
                                span: source.block().program_span(program),
                            })?;
                    families.insert(family, Rc::clone(&code));
                    code
                }
            };
            result.bindings.insert(
                key,
                TargetBinding {
                    code,
                    selected: plan.private_result_offset(),
                },
            );
        }
        Ok(result)
    }

    pub(super) fn call(
        &self,
        coordinate: (usize, usize, usize),
        inputs: (&[f64], &[f64], f64),
        context: RowEvalContext<'_>,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.bindings
            .get(&coordinate)
            .map(|binding| {
                binding
                    .code
                    .call(binding.selected, inputs.0, inputs.1, inputs.2, context)
            })
            .transpose()
    }
}

struct TargetValues {
    code: Rc<dyn CompiledSolveExpression>,
    output_count: usize,
    spans: Box<[rumoca_core::Span]>,
    scratch: RefCell<Vec<f64>>,
}

pub(super) fn compile(
    backend: &(impl SolveExecutionBackend + ?Sized),
    plan: &solve_eval::PreparedTargetValuePlan,
    context: RowEvalContext<'_>,
) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
    if context.seed.is_some() {
        return Err("a primal target tuple cannot capture a directional seed".into());
    }
    let source = plan.program();
    if plan.private_result_offset() >= source.output_count() {
        return Err("private target tuple projection exceeds its source outputs".into());
    }
    Ok(Rc::new(TargetValues {
        code: backend.compile_expression(source)?,
        output_count: source.output_count(),
        spans: source.program_spans().into(),
        scratch: RefCell::default(),
    }))
}

impl CompiledSolveTargetValues for TargetValues {
    fn call(
        &self,
        selected: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, RuntimeSolveError> {
        if selected >= self.output_count || context.seed.is_some() {
            return Err(RuntimeSolveError::solve_ir(
                "target tuple query is outside its prepared primal outputs",
            ));
        }
        let mut values = self.scratch.borrow_mut();
        resize_runtime_values(
            &mut values,
            self.output_count,
            0.0,
            "private target tuple outputs",
        )?;
        self.code
            .call(y, p, t, context.external_tables.unwrap_or(&[]), &mut values)
            .map_err(|error| admitted_compiled_error("private target tuple", &self.spans, error))?;
        Ok(values[selected])
    }
}
