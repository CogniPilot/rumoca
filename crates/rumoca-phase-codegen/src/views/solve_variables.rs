//! Per-variable storage of a Solve problem, named from its DAE template entry.

use minijinja::Value;
use rumoca_ir_solve as solve;
use serde::Serialize;

/// One model variable's checked Solve storage run.
#[derive(Debug, PartialEq, Serialize)]
pub(crate) struct SolveVariable {
    pub(crate) name: String,
    pub(crate) description: Option<String>,
    pub(crate) role: solve::SolveVariableStorageRole,
    pub(crate) value_kind: solve::SolveVariableValueKind,
    /// `"y"` or `"p"`.
    pub(crate) column: &'static str,
    /// Base slot; scalar `k` is at `index + k`, row-major.
    pub(crate) index: usize,
    pub(crate) scalar_count: usize,
    /// Array extents; `None` for a scalar.
    pub(crate) shape: Option<Vec<usize>>,
}

/// Every variable in dense DAE ordinal order, the order of
/// `solve_layout.variable_storage_runs`. `None` when `dae_entry` carries no
/// variables or does not list exactly one variable per storage run by ordinal.
pub(crate) fn solve_variables(
    problem: &solve::SolveProblem,
    dae_entry: &Value,
) -> Option<Vec<SolveVariable>> {
    let runs = &problem.solve_layout.variable_storage_runs;
    let variables = dae_entry.get_attr("variables").ok()?;
    if variables.len()? != runs.len() {
        return None;
    }
    runs.iter()
        .enumerate()
        .map(|(ordinal, run)| {
            let variable = variables.get_item(&Value::from(ordinal)).ok()?;
            let id = variable.get_attr("id").ok()?;
            if usize::try_from(id).ok()? != ordinal {
                return None;
            }
            let name = variable.get_attr("name").ok()?.as_str()?.to_string();
            let description = variable
                .get_attr("attributes")
                .and_then(|attributes| attributes.get_attr("description"))
                .ok()
                .and_then(|description| description.as_str().map(str::to_string));
            let (column, index) = match run.base {
                solve::ScalarSlot::Y { index, .. } => ("y", index),
                solve::ScalarSlot::P { index, .. } => ("p", index),
                solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => return None,
            };
            Some(SolveVariable {
                shape: problem.layout.shape(&name).map(<[usize]>::to_vec),
                name,
                description,
                role: run.role,
                value_kind: run.value_kind,
                column,
                index,
                scalar_count: run.scalar_count,
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn missing_dae_entry_has_no_view() {
        let problem = solve::SolveProblem::default();
        assert_eq!(solve_variables(&problem, &Value::default()), None);
    }

    #[test]
    fn empty_problem_has_an_empty_view() {
        let problem = solve::SolveProblem::default();
        let dae = Value::from_serialize(serde_json::json!({ "variables": [] }));
        assert_eq!(solve_variables(&problem, &dae), Some(Vec::new()));
    }
}
