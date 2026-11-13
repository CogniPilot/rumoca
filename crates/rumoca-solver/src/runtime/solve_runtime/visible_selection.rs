//! Selected visible values: the full read restricted to the outputs a caller
//! names, so a few names cost a few rows.

use super::*;

impl ScalarExecutionView<'_> {
    /// The visible values `names` select into `selected`, in request order
    /// with repeats; `None` for a name that is no visible output. Returns how
    /// many values the full read holds at the same point.
    ///
    /// Each value is the one [`Self::visible_values`] holds for that output. A
    /// direct source is read alone; an expression output takes the value of
    /// its group's source row, evaluated by the evaluator the full read runs
    /// over every source row. Without a plan the full read is the only
    /// evaluator, so it runs whole into the reused scratch.
    pub fn visible_values_selected<N: AsRef<str>>(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[N],
        selected: &mut Vec<Option<f64>>,
    ) -> Result<usize, RuntimeSolveError> {
        selected.clear();
        reserve_runtime_vec_capacity(selected, names.len(), "selected visible values")?;
        let index = &self.scalar.visible_name_index;
        let mut values = self.scalar.visible_scratch.borrow_mut();
        let Some(plan) = &self.scalar.visible_value_plan else {
            self.visible_values_into(y, params, t, &mut values)?;
            selected.extend(names.iter().map(|name| {
                let output = *index.get(name.as_ref())?;
                values.get(output).copied()
            }));
            return Ok(values.len());
        };
        // Request positions still waiting for the value of a source row.
        let mut pending = Vec::new();
        for (position, name) in names.iter().enumerate() {
            let name = name.as_ref();
            let Some(&output) = index.get(name) else {
                selected.push(None);
                continue;
            };
            let entry = plan.entries.get(output).ok_or_else(|| {
                visible_value_index_error(name, output, plan.entries.len(), "visible value plan")
            })?;
            selected.push(match *entry {
                VisibleValuePlanEntry::Direct(source) => {
                    Some(direct_visible_value(source, y, params, t)?)
                }
                VisibleValuePlanEntry::Expression { source_row } => {
                    pending.push((position, source_row));
                    None
                }
            });
        }
        resize_runtime_values(&mut values, plan.entries.len(), 0.0, "visible values")?;
        self.fill_selected_expressions(&pending, RowEvalPoint { y, p: params, t }, &mut values)?;
        for (position, source_row) in pending {
            let value = values
                .get(source_row)
                .copied()
                .ok_or_else(|| plans::visible_plan_output_index_error(source_row, values.len()))?;
            selected[position] = Some(value);
        }
        Ok(plan.entries.len())
    }

    /// Evaluate each pending source row once, in the full read's row order.
    fn fill_selected_expressions(
        &self,
        pending: &[(usize, usize)],
        point: RowEvalPoint<'_>,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if pending.is_empty() {
            return Ok(());
        }
        let mut rows: Vec<usize> = pending.iter().map(|&(_, row)| row).collect();
        rows.sort_unstable();
        rows.dedup();
        self.eval_visible_expression_rows(&rows, point, values)
    }
}
