//! Solver display views borrow the same problem's checked declaration layout.

use crate::{
    ScalarSlot, SolveProblem, SolveProblemShapeContractError, SolveVariableStorageRole,
    scalar_slot_y,
};
use std::ops::Range;

fn storage_error(detail: impl Into<String>) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::SolverStorage {
        detail: detail.into(),
    }
}

impl SolveProblem {
    /// Physical public solver extent, independent of any display-name view.
    pub fn solver_scalar_count(&self) -> usize {
        self.solve_layout.solver_scalar_count()
    }

    /// Demand one display name; private unnamed slots remain unnamed.
    pub fn solver_name(&self, index: usize) -> Option<String> {
        (index < self.solver_scalar_count())
            .then(|| self.layout.scalar_name_for_slot(scalar_slot_y(index)))
            .flatten()
    }

    /// External name lookup in this problem's original checked layout.
    pub fn solver_index(&self, name: &str) -> Option<usize> {
        if self
            .layout
            .shape(name)
            .is_some_and(|shape| shape.contains(&0))
        {
            return None;
        }
        match self.layout.binding(name)? {
            slot @ ScalarSlot::Y { index, .. }
                if index < self.solver_scalar_count() && slot == scalar_slot_y(index) =>
            {
                Some(index)
            }
            _ => None,
        }
    }

    /// Lazy scalar report view. Calling this does not allocate a name table.
    pub fn solver_names(&self) -> impl Iterator<Item = Option<String>> + '_ {
        (0..self.solver_scalar_count()).map(|index| self.solver_name(index))
    }

    /// Materialize a complete named report only at an explicit caller boundary.
    pub fn solver_named_report(
        &self,
        range: Range<usize>,
    ) -> Result<Vec<String>, SolveProblemShapeContractError> {
        if range.start > range.end || range.end > self.solver_scalar_count() {
            return Err(storage_error(
                "named solver report exceeds the physical solver extent",
            ));
        }
        range
            .map(|index| {
                self.solver_name(index).ok_or_else(|| {
                    storage_error(format!(
                        "requested solver report has no name for Y[{index}]"
                    ))
                })
            })
            .collect()
    }

    /// Original declaration range, not a scalar-index vector or identity recovery.
    pub fn solver_declaration_range(&self, name: &str) -> Option<Range<usize>> {
        if self
            .layout
            .has_indexed_shape_binding(&rumoca_core::VarName::intern(name))
        {
            return None;
        }
        let slot = *self
            .layout
            .bindings()
            .get(&rumoca_core::VarName::intern(name))?;
        let ScalarSlot::Y { index, .. } = slot else {
            return None;
        };
        if slot != scalar_slot_y(index) {
            return None;
        }
        let count = self.layout.shape(name).map_or(Some(1), |shape| {
            shape.iter().try_fold(1usize, |n, d| n.checked_mul(*d))
        })?;
        let end = index.checked_add(count)?;
        (end <= self.solver_scalar_count()).then_some(index..end)
    }

    pub(crate) fn validate_solver_storage(&self) -> Result<(), SolveProblemShapeContractError> {
        let counts = &self.solve_layout;
        let algebraic_end = counts
            .state_scalar_count
            .checked_add(counts.algebraic_scalar_count)
            .ok_or_else(|| storage_error("solver role extent overflow"))?;
        let end = algebraic_end
            .checked_add(counts.output_scalar_count)
            .ok_or_else(|| storage_error("solver role extent overflow"))?;
        if counts.variable_storage_runs.iter().any(|run| {
            matches!(
                run.role,
                SolveVariableStorageRole::State
                    | SolveVariableStorageRole::Algebraic
                    | SolveVariableStorageRole::Output
            ) && !matches!(run.base, ScalarSlot::Y { .. })
        }) {
            return Err(storage_error("solver allocation has non-Y storage"));
        }
        let has_y_allocations = counts
            .variable_storage_runs
            .iter()
            .any(|run| matches!(run.base, ScalarSlot::Y { .. }));
        if !has_y_allocations && end <= self.layout.y_scalars() {
            return Ok(());
        }
        if end != self.layout.y_scalars() {
            return Err(storage_error(format!(
                "solver role extent {end} disagrees with Y extent {}",
                self.layout.y_scalars()
            )));
        }
        let mut ranges = Vec::new();
        for run in &counts.variable_storage_runs {
            let ScalarSlot::Y { index, byte_offset } = run.base else {
                continue;
            };
            if scalar_slot_y(index) != run.base {
                return Err(storage_error(format!(
                    "Y allocation {index} has foreign byte offset {byte_offset}"
                )));
            }
            let limit = index
                .checked_add(run.scalar_count)
                .ok_or_else(|| storage_error("Y declaration extent overflow"))?;
            let prefix = match run.role {
                SolveVariableStorageRole::State => 0..counts.state_scalar_count,
                SolveVariableStorageRole::Algebraic => counts.state_scalar_count..algebraic_end,
                SolveVariableStorageRole::Output => algebraic_end..end,
                _ => return Err(storage_error("Y allocation has a non-solver role")),
            };
            if index < prefix.start || limit > prefix.end {
                return Err(storage_error("Y declaration crosses its role prefix"));
            }
            if run.scalar_count != 0 {
                ranges.push(index..limit);
            }
        }
        ranges.sort_by_key(|range| range.start);
        let mut cursor = 0;
        for range in &ranges {
            if range.start != cursor {
                return Err(storage_error("Y declaration coverage has a gap or overlap"));
            }
            cursor = range.end;
        }
        if cursor != end {
            return Err(storage_error(
                "Y declaration coverage does not reach the solver extent",
            ));
        }
        let named = self.layout.y_declaration_ranges()?;
        if named.windows(2).any(|pair| pair[0].end > pair[1].start)
            || named.iter().any(|range| {
                ranges
                    .binary_search_by_key(&range.start, |stored| stored.start)
                    .ok()
                    .is_none_or(|index| ranges[index] != *range)
            })
        {
            return Err(storage_error(
                "Y layout declarations disagree with original storage allocations",
            ));
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        SolveLayout, SolveVariableDeclaration, SolveVariableStorageRun, SolveVariableValueKind,
        VarLayout,
    };
    use rumoca_core::{SourceId, Span};

    fn ranged_problem(count: usize) -> SolveProblem {
        let span = Span::from_offsets(SourceId::from_source_name("YDeclarations.mo"), 1, 9);
        let roles = [
            SolveVariableStorageRole::State,
            SolveVariableStorageRole::Algebraic,
            SolveVariableStorageRole::Output,
        ];
        SolveProblem {
            layout: VarLayout::from_parts_with_shapes_and_spans(
                [
                    ("state".into(), scalar_slot_y(0)),
                    ("algebraic".into(), scalar_slot_y(count)),
                    ("output".into(), scalar_slot_y(2 * count)),
                ]
                .into(),
                ["state", "algebraic", "output"]
                    .into_iter()
                    .map(|name| (name.into(), vec![count]))
                    .collect(),
                ["state", "algebraic", "output"]
                    .into_iter()
                    .map(|name| (name.into(), span))
                    .collect(),
                3 * count,
                0,
            )
            .unwrap(),
            solve_layout: SolveLayout {
                state_scalar_count: count,
                algebraic_scalar_count: count,
                output_scalar_count: count,
                variable_storage_runs: roles
                    .iter()
                    .enumerate()
                    .map(|(index, role)| SolveVariableStorageRun {
                        base: scalar_slot_y(index * count),
                        scalar_count: count,
                        role: *role,
                        value_kind: SolveVariableValueKind::Real,
                    })
                    .collect(),
                variable_declarations: roles
                    .into_iter()
                    .map(|role| SolveVariableDeclaration::new(role, SolveVariableValueKind::Real))
                    .collect(),
                ..Default::default()
            },
            ..Default::default()
        }
    }

    #[test]
    fn same_root_solver_views_keep_tensor_extent_out_of_stored_metadata() {
        let mut sizes = Vec::new();
        for count in [2, 65536] {
            let problem = ranged_problem(count);
            problem.validate_solver_storage().unwrap();
            assert_eq!(problem.solver_scalar_count(), 3 * count);
            assert_eq!(problem.layout.bindings().len(), 3);
            assert_eq!(problem.solve_layout.variable_storage_runs.len(), 3);
            let metadata = serde_json::json!({"layout": &problem.layout, "declarations": &problem.solve_layout});
            assert!(metadata["declarations"].get("solver_maps").is_none());
            sizes.push(serde_json::to_vec(&metadata).unwrap().len());
            for (declaration, name) in ["state", "algebraic", "output"].into_iter().enumerate() {
                assert_eq!(
                    problem.solver_declaration_range(name),
                    Some(declaration * count..(declaration + 1) * count)
                );
                assert_declaration_points(&problem, declaration, name, count);
            }
            assert_eq!(
                problem.solver_names().take(2).collect::<Vec<_>>(),
                vec![Some("state[1]".into()), Some("state[2]".into())]
            );
            assert_eq!(
                problem.solver_named_report(count..count + 2).unwrap(),
                ["algebraic[1]", "algebraic[2]"]
            );
            for name in ["state[0]", "output[:]", "foreign.state[1]", "state[1,1]"] {
                assert_eq!(problem.solver_index(name), None);
            }
            assert_eq!(problem.solver_name(3 * count), None);
            assert!(problem.solver_named_report(0..3 * count + 1).is_err());
        }
        assert!(
            sizes[1] < sizes[0] + 512,
            "private Y metadata grew {} -> {} bytes",
            sizes[0],
            sizes[1]
        );
    }

    #[test]
    fn solver_storage_refuses_wrong_roles_extents_bytes_and_original_allocation() {
        let original = ranged_problem(2);
        let mut wrong_extent = original.clone();
        wrong_extent.solve_layout.output_scalar_count = 3;
        assert!(wrong_extent.validate_solver_storage().is_err());
        let mut wrong_role = original.clone();
        wrong_role.solve_layout.variable_storage_runs[0].role = SolveVariableStorageRole::Algebraic;
        assert!(wrong_role.validate_solver_storage().is_err());
        let mut overlap = original.clone();
        overlap.solve_layout.variable_storage_runs[1].base = scalar_slot_y(1);
        assert!(overlap.validate_solver_storage().is_err());
        let mut wrong_bytes = original.clone();
        wrong_bytes.solve_layout.variable_storage_runs[1].base = ScalarSlot::Y {
            index: 2,
            byte_offset: 999,
        };
        assert!(wrong_bytes.validate_solver_storage().is_err());
        let mut foreign_layout = original.clone();
        foreign_layout.layout =
            VarLayout::from_parts([("foreign".into(), scalar_slot_y(1))].into(), 6, 0);
        assert!(foreign_layout.validate_solver_storage().is_err());
        let mut overflow = original.clone();
        overflow.solve_layout.state_scalar_count = usize::MAX;
        assert!(overflow.validate_solver_storage().is_err());
        let mut wrong_wire = serde_json::to_value(&original.solve_layout).unwrap();
        wrong_wire["solver_maps"] = serde_json::json!({"names": ["forged"]});
        assert!(serde_json::from_value::<SolveLayout>(wrong_wire).is_err());
    }

    #[test]
    fn unnamed_program_coordinates_remain_numerical_and_named_reports_refuse() {
        let problem = SolveProblem {
            layout: VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout: SolveLayout {
                algebraic_scalar_count: 2,
                ..Default::default()
            },
            ..Default::default()
        };
        problem.validate_solver_storage().unwrap();
        assert_eq!(problem.solver_scalar_count(), 2);
        assert_eq!(problem.solver_name(0), None);
        assert!(problem.solver_named_report(0..2).is_err());
        assert!(problem.layout.validate_named_y_coverage(2).is_err());
        let mut private = problem.clone();
        private.solve_layout.algebraic_scalar_count = 0;
        private.validate_solver_storage().unwrap();
        assert_eq!(private.layout.y_scalars(), 2);
        assert_eq!(private.solver_scalar_count(), 0);
    }

    #[test]
    fn parameter_allocations_preserve_private_y_without_permitting_solver_roles() {
        let parameter = SolveVariableStorageRun {
            base: crate::scalar_slot_p(0),
            scalar_count: 1,
            role: SolveVariableStorageRole::Parameter,
            value_kind: SolveVariableValueKind::Real,
        };
        let mut problem = SolveProblem {
            layout: VarLayout::from_parts([("parameter".into(), parameter.base)].into(), 2, 1),
            solve_layout: SolveLayout {
                variable_storage_runs: vec![parameter],
                variable_declarations: vec![SolveVariableDeclaration::new(
                    SolveVariableStorageRole::Parameter,
                    SolveVariableValueKind::Real,
                )],
                ..Default::default()
            },
            ..Default::default()
        };
        problem.validate_solver_storage().unwrap();
        assert_eq!(problem.solver_scalar_count(), 0);
        assert_eq!(problem.layout.y_scalars(), 2);
        assert!(problem.layout.validate_named_y_coverage(2).is_err());
        problem.solve_layout.algebraic_scalar_count = 2;
        problem.validate_solver_storage().unwrap();
        assert!(problem.solver_named_report(0..2).is_err());
        for role in [
            SolveVariableStorageRole::State,
            SolveVariableStorageRole::Algebraic,
            SolveVariableStorageRole::Output,
        ] {
            problem.solve_layout.variable_storage_runs[0].role = role;
            assert!(problem.validate_solver_storage().is_err());
        }
    }

    #[test]
    fn a_parameter_catalog_does_not_excuse_incomplete_y_allocations() {
        let mut problem = ranged_problem(2);
        let mut bindings = problem
            .layout
            .bindings()
            .iter()
            .map(|(name, slot)| (name.to_string(), *slot))
            .collect::<indexmap::IndexMap<_, _>>();
        bindings.insert("parameter".into(), crate::scalar_slot_p(0));
        let names = ["state", "algebraic", "output"];
        problem.layout = VarLayout::from_parts_with_shapes_and_spans(
            bindings,
            names.iter().map(|name| ((*name).into(), vec![2])).collect(),
            names
                .iter()
                .map(|name| ((*name).into(), problem.layout.shape_span(name).unwrap()))
                .collect(),
            6,
            1,
        )
        .unwrap();
        problem
            .solve_layout
            .variable_storage_runs
            .push(SolveVariableStorageRun {
                base: crate::scalar_slot_p(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::Parameter,
                value_kind: SolveVariableValueKind::Real,
            });
        problem
            .solve_layout
            .variable_declarations
            .push(SolveVariableDeclaration::new(
                SolveVariableStorageRole::Parameter,
                SolveVariableValueKind::Real,
            ));
        problem.validate_solver_storage().unwrap();
        problem.solve_layout.variable_storage_runs[1].scalar_count = 1;
        assert!(problem.validate_solver_storage().is_err());
    }
    fn assert_declaration_points(
        problem: &SolveProblem,
        declaration: usize,
        name: &str,
        count: usize,
    ) {
        for ordinal in [0, count / 2, count - 1] {
            let index = declaration * count + ordinal;
            let name = format!("{name}[{}]", ordinal + 1);
            assert_eq!(problem.solver_name(index), Some(name.clone()));
            assert_eq!(problem.solver_index(&name), Some(index));
        }
    }
}
