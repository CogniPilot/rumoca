use super::*;

impl VisibleProjection {
    pub(super) fn scalar_rows(
        &self,
        problem: &SolveProblem,
    ) -> Result<ScalarProgramBlock, VisibleProjectionError> {
        let mut rows = Vec::new();
        let mut spans = Vec::new();
        for member in &self.members {
            append_scalar_member(problem, member, &mut rows, &mut spans)?;
        }
        ScalarProgramBlock::with_program_spans(rows, spans)
            .map_err(|err| error(err.to_string(), None))
    }
}

fn append_scalar_member(
    problem: &SolveProblem,
    member: &VisibleProjectionInput,
    rows: &mut Vec<Vec<LinearOp>>,
    spans: &mut Vec<Span>,
) -> Result<(), VisibleProjectionError> {
    match member {
        VisibleProjectionInput::Declaration(source) => {
            let run = &problem.solve_layout.variable_storage_runs[source.declaration];
            for ordinal in 0..run.scalar_count {
                let point = storage_point(run.base, ordinal).ok_or_else(|| {
                    error(
                        "storage point overflow",
                        Some(source.attributes.source_span),
                    )
                })?;
                let load = match point {
                    ScalarSlot::Y { index, .. } => LinearOp::LoadY { dst: 0, index },
                    ScalarSlot::P { index, .. } => LinearOp::LoadP { dst: 0, index },
                    _ => {
                        return Err(error(
                            "visible point has no writable storage",
                            Some(source.attributes.source_span),
                        ));
                    }
                };
                rows.push(vec![load, LinearOp::StoreOutput { src: 0 }]);
                spans.push(source.attributes.source_span);
            }
        }
        VisibleProjectionInput::Expression {
            rows: expressions, ..
        } => {
            for (index, row) in expressions.programs().iter().enumerate() {
                rows.push(row.clone());
                spans.push(
                    expressions
                        .program_span(index)
                        .ok_or_else(|| error("authored expression has no source span", None))?,
                );
            }
        }
    }
    Ok(())
}
