//! Private target tuples executed through an already compiled source program.

use super::*;

#[derive(Clone)]
struct TargetBinding {
    code: Rc<dyn CompiledSolveTargetValues>,
    family: Rc<solve_eval::PreparedTargetValuePlan>,
    selected: usize,
}

#[derive(Clone, Default)]
pub(super) struct TargetQueries {
    bindings: FxHashMap<(usize, usize, usize), TargetBinding>,
    source: Option<solve::ScalarProgramBlock>,
    calls: Option<solve::SolvePureCallTable>,
    table_free: bool,
}

enum TargetSource<'a> {
    #[cfg(test)]
    Prepared(&'a PreparedScalarProgramBlock),
    Implicit {
        canonical: &'a solve::ScalarProgramBlock,
        analysis: ProgramAnalysis<'a>,
    },
}

impl TargetSource<'_> {
    fn canonical(&self) -> &solve::ScalarProgramBlock {
        match self {
            #[cfg(test)]
            Self::Prepared(source) => source.block(),
            Self::Implicit { canonical, .. } => canonical,
        }
    }

    fn row_output_position(&self, row: usize) -> Option<(usize, usize)> {
        match self {
            #[cfg(test)]
            Self::Prepared(source) => source.row_output_position(row),
            Self::Implicit { analysis, .. } => analysis.row_output_position(row),
        }
    }

    fn portable_target_value_plan(
        &self,
        program: usize,
        offset: usize,
        target: usize,
    ) -> Result<Option<solve_eval::PreparedTargetValuePlan>, EvalSolveError> {
        match self {
            #[cfg(test)]
            Self::Prepared(source) => source.portable_target_value_plan(program, offset, target),
            Self::Implicit { analysis, .. } => {
                analysis.portable_target_value_plan(program, offset, target)
            }
        }
    }
}

impl TargetQueries {
    #[cfg(test)]
    pub(super) fn prepare(
        source: &PreparedScalarProgramBlock,
        candidates: impl IntoIterator<Item = (usize, usize)>,
        backend: &dyn SolveExecutionBackend,
        context: RowEvalContext<'_>,
    ) -> Result<Self, EvalSolveError> {
        Self::prepare_with_primary(source, candidates, backend, context, None)
    }

    #[cfg(test)]
    pub(super) fn prepare_with_primary(
        source: &PreparedScalarProgramBlock,
        candidates: impl IntoIterator<Item = (usize, usize)>,
        backend: &dyn SolveExecutionBackend,
        context: RowEvalContext<'_>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        Self::prepare_source(
            TargetSource::Prepared(source),
            candidates,
            backend,
            context,
            primary,
        )
    }

    pub(super) fn prepare_owned_with_primary(
        canonical: &solve::ScalarProgramBlock,
        analysis: ProgramAnalysis<'_>,
        candidates: impl IntoIterator<Item = (usize, usize)>,
        backend: &dyn SolveExecutionBackend,
        context: RowEvalContext<'_>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        if !analysis.shares_program_owner(canonical) {
            return Err(EvalSolveError::InvalidRow {
                message: "target-query facts belong to a different canonical source".into(),
                span: canonical.first_source_span(),
            });
        }
        Self::prepare_source(
            TargetSource::Implicit {
                canonical,
                analysis,
            },
            candidates,
            backend,
            context,
            primary,
        )
    }

    fn prepare_source(
        source: TargetSource<'_>,
        candidates: impl IntoIterator<Item = (usize, usize)>,
        backend: &dyn SolveExecutionBackend,
        context: RowEvalContext<'_>,
        primary: Option<&Self>,
    ) -> Result<Self, EvalSolveError> {
        let unchanged = primary
            .filter(|primary| primary.matches_context(context))
            .and_then(|primary| {
                let base = primary.source.as_ref()?;
                let replaced = solve_eval::replaced_programs(base, source.canonical())?;
                Some((primary, base, replaced))
            });
        let mut result = Self {
            source: Some(source.canonical().clone()),
            calls: context.pure_calls.cloned(),
            table_free: context.external_tables.is_none_or(<[_]>::is_empty),
            ..Self::default()
        };
        let mut families = FxHashMap::default();
        let mut plans = Vec::new();
        let mut pending_bindings = Vec::new();
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
            if let Some((primary, base, replaced)) = &unchanged
                && replaced.binary_search(&program).is_err()
                && base.program_span(program) == source.canonical().program_span(program)
                && let Some(binding) = primary.bindings.get(&key)
                && binding.matches(&plan)
            {
                result.bindings.insert(key, binding.clone());
                continue;
            }
            let family = (program, plan.canonical_prefix_len());
            let selected = plan.private_result_offset();
            let index = match families.get(&family) {
                Some(index) => *index,
                None => {
                    let index = plans.len();
                    plans.push((program, Rc::new(plan)));
                    families.insert(family, index);
                    index
                }
            };
            pending_bindings.push((key, index, selected));
        }
        let codes = compile_families(backend, context, source.canonical(), &plans)?;
        for (key, index, selected) in pending_bindings {
            result.bindings.insert(
                key,
                TargetBinding {
                    code: Rc::clone(&codes[index]),
                    family: Rc::clone(&plans[index].1),
                    selected,
                },
            );
        }
        Ok(result)
    }

    fn matches_context(&self, context: RowEvalContext<'_>) -> bool {
        let empty_calls = |calls: Option<&solve::SolvePureCallTable>| {
            calls.is_none_or(|calls| calls.owners().is_empty())
        };
        self.table_free
            && context.external_tables.is_none_or(<[_]>::is_empty)
            && (self
                .calls
                .as_ref()
                .zip(context.pure_calls)
                .is_some_and(|(base, target)| base.shares_table_owner(target))
                || empty_calls(self.calls.as_ref()) && empty_calls(context.pure_calls))
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

fn compile_families(
    backend: &dyn SolveExecutionBackend,
    context: RowEvalContext<'_>,
    source: &solve::ScalarProgramBlock,
    plans: &[(usize, Rc<solve_eval::PreparedTargetValuePlan>)],
) -> Result<Vec<Rc<dyn CompiledSolveTargetValues>>, EvalSolveError> {
    for (_, plan) in plans {
        validate_plan(plan, context).map_err(|message| EvalSolveError::CompiledExecution {
            message,
            span: source.first_source_span(),
        })?;
    }
    let blocks = plans
        .iter()
        .map(|(_, plan)| plan.program())
        .collect::<Vec<_>>();
    let compiled = if plans.is_empty() {
        None
    } else {
        backend
            .compile_target_expression_family(&blocks)
            .map_err(|(entry, message)| family_compile_error(source, plans, entry, message))?
    };
    if compiled
        .as_ref()
        .is_some_and(|entries| entries.len() != plans.len())
    {
        return Err(EvalSolveError::CompiledExecution {
            message: "target family entry count differs from issued plans".into(),
            span: source.first_source_span(),
        });
    }
    let mut entries = compiled.map(Vec::into_iter);
    let mut codes = Vec::with_capacity(plans.len());
    for (program, plan) in plans {
        let code = match &mut entries {
            Some(entries) => {
                bind_expression(plan, context, entries.next().expect("checked family size"))
            }
            None => backend.compile_target_values(plan, context),
        }
        .map_err(|message| EvalSolveError::CompiledExecution {
            message: format!("required private target tuple could not be lowered: {message}"),
            span: source.program_span(*program),
        })?;
        codes.push(code);
    }
    Ok(codes)
}

fn family_compile_error(
    source: &solve::ScalarProgramBlock,
    plans: &[(usize, Rc<solve_eval::PreparedTargetValuePlan>)],
    entry: Option<usize>,
    message: String,
) -> EvalSolveError {
    match entry {
        Some(entry) => match plans.get(entry) {
            Some((program, _)) => EvalSolveError::CompiledExecution {
                message,
                span: source.program_span(*program),
            },
            None => EvalSolveError::CompiledExecution {
                message: format!(
                    "backend returned invalid family failure entry {entry}: {message}"
                ),
                span: None,
            },
        },
        None => EvalSolveError::CompiledExecution {
            message,
            span: None,
        },
    }
}

impl TargetBinding {
    fn matches(&self, plan: &solve_eval::PreparedTargetValuePlan) -> bool {
        self.selected == plan.private_result_offset()
            && self.family.row() == plan.row()
            && solve_eval::replaced_programs(self.family.program(), plan.program())
                .is_some_and(|replaced| replaced.is_empty())
            && self.family.program().program_spans() == plan.program().program_spans()
            && self.family.program().output_indices() == plan.program().output_indices()
            && self.family.coefficient_outputs() == plan.coefficient_outputs()
    }
}

struct TargetValues {
    code: Rc<dyn CompiledSolveExpression>,
    output_count: usize,
    spans: Box<[rumoca_core::Span]>,
    scratch: RefCell<Vec<f64>>,
    row: usize,
    coefficient_outputs: Box<[(usize, usize, usize)]>,
}

pub(super) fn compile(
    backend: &(impl SolveExecutionBackend + ?Sized),
    plan: &solve_eval::PreparedTargetValuePlan,
    context: RowEvalContext<'_>,
) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
    validate_plan(plan, context)?;
    bind_expression(plan, context, backend.compile_expression(plan.program())?)
}

fn bind_expression(
    plan: &solve_eval::PreparedTargetValuePlan,
    context: RowEvalContext<'_>,
    code: Rc<dyn CompiledSolveExpression>,
) -> Result<Rc<dyn CompiledSolveTargetValues>, String> {
    validate_plan(plan, context)?;
    let source = plan.program();
    Ok(Rc::new(TargetValues {
        code,
        output_count: source.output_count(),
        spans: source.program_spans().into(),
        scratch: RefCell::default(),
        row: plan.row(),
        coefficient_outputs: plan.coefficient_outputs().into(),
    }))
}

fn validate_plan(
    plan: &solve_eval::PreparedTargetValuePlan,
    context: RowEvalContext<'_>,
) -> Result<(), String> {
    if context.seed.is_some() {
        return Err("a primal target tuple cannot capture a directional seed".into());
    }
    let source = plan.program();
    if plan.private_result_offset() >= source.output_count() {
        return Err("private target tuple projection exceeds its source outputs".into());
    }
    Ok(())
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
        if let Ok(index) = self
            .coefficient_outputs
            .binary_search_by_key(&selected, |&(value_offset, _, _)| value_offset)
        {
            let (_, target_y_index, coefficient_offset) = self.coefficient_outputs[index];
            let coefficient = values[coefficient_offset];
            if coefficient == 0.0 || !coefficient.is_finite() {
                return Err(RuntimeSolveError::RefreshTargetSingular {
                    row: self.row,
                    target_y_index,
                    coefficient,
                    span: self.spans.first().copied(),
                });
            }
        }
        Ok(values[selected])
    }
}
