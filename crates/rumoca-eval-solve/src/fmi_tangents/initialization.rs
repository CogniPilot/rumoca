//! Initialization sensitivity of the same retained, admitted C component.
//!
//! Bindings, algebraic residuals and solved initialization residuals are one
//! coupled relation. Given initial states remain independent. Unowned rows
//! remain explicit consistency obligations; neither their values nor their
//! tangents are discarded to manufacture a square successful system.

use super::{
    BTreeSet, FmiCCodegenView, FmiImplicitTangentSystem, FmiTangentConsistency, FmiTangentMode,
    FmiTangentPlan, FmiTangentPlanError, ad, algebraic_inventory, derive_rhs, join_program_blocks,
    select_residual_programs, solve, to_scalar_program_block,
};

pub(super) fn derive(
    component: &FmiCCodegenView,
) -> Result<FmiTangentPlan<'_>, FmiTangentPlanError> {
    let problem = component.problem();
    let continuous = &problem.continuous;
    if !continuous.reduced_chart_set.charts.is_empty()
        || !continuous.manifold_projection_plan.is_empty()
    {
        return Err(FmiTangentPlanError::AlternateCoordinates);
    }
    if component.requires_event_iteration() {
        return Err(FmiTangentPlanError::InitializationInventory(
            "scalar-event initialization transactions need their sensitivity owner".into(),
        ));
    }
    let init = &problem.initialization;
    let delays = &problem.events.delays;
    if init.manifold_row_count() > 0
        || init.projection_plan().iterates_discretes
        || problem
            .solve_layout
            .initial_homotopy_parameter_index
            .is_some()
        || !delays.source_rhs.is_empty()
        || !delays.value_parameter_indices.is_empty()
    {
        return Err(FmiTangentPlanError::InitializationInventory(
            "initialization continuation, discrete iteration or history needs its sensitivity owner".into(),
        ));
    }
    let y_len = problem.layout.y_scalars();
    let seed_len = y_len
        .checked_add(problem.layout.p_scalars())
        .and_then(|length| length.checked_add(1))
        .ok_or_else(|| FmiTangentPlanError::Contract("initial seed extent overflow".into()))?;
    let mut relation = Relation::default();
    let (rows, targets) = algebraic_inventory(component)?;
    let algebraic = to_scalar_program_block(&continuous.implicit_rhs)
        .map_err(|error| FmiTangentPlanError::Derivation(error.into()))?;
    relation.append(&algebraic, &rows, &targets, false)?;
    let bindings = combined_targets(component, init.update_targets())?;
    let binding_rows = (0..bindings.len()).collect::<Vec<_>>();
    relation.append(init.update_rhs(), &binding_rows, &bindings, true)?;
    append_parameter_discretes(component, &mut relation)?;
    let initial = to_scalar_program_block(init.residual())
        .map_err(|error| FmiTangentPlanError::Derivation(error.into()))?;
    let (solved, checks) = initial_rows(component, &initial, &mut relation)?;
    if solved != init.projection_unknowns().len() {
        return Err(FmiTangentPlanError::Contract(
            "initial projection unknowns differ from their rows".into(),
        ));
    }
    let given = init
        .given_state_indices()
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    if relation.targets.iter().any(|target| given.contains(target)) {
        return Err(FmiTangentPlanError::Contract(
            "a given initial state has a tangent writer".into(),
        ));
    }
    let implicit = relation.finish(y_len, seed_len - 1)?;
    let consistency = derive_consistency(&initial, &checks, y_len, problem.layout.p_scalars())?;
    Ok(FmiTangentPlan {
        component,
        mode: FmiTangentMode::Initialization,
        assignments: Vec::new(),
        implicit,
        rhs: derive_rhs(component)?,
        seed_len,
        consistency,
    })
}

pub(super) fn combined_targets(
    component: &FmiCCodegenView,
    slots: &[solve::ScalarSlot],
) -> Result<Vec<usize>, FmiTangentPlanError> {
    let layout = &component.problem().layout;
    slots
        .iter()
        .map(|slot| match *slot {
            solve::ScalarSlot::Y { index, .. } if index < layout.y_scalars() => Ok(index),
            solve::ScalarSlot::P { index, .. } if index < layout.p_scalars() => {
                layout.y_scalars().checked_add(index).ok_or_else(|| {
                    FmiTangentPlanError::Contract("initial target extent overflow".into())
                })
            }
            _ => Err(FmiTangentPlanError::Contract(
                "initial target is not bounded Y|P storage".into(),
            )),
        })
        .collect()
}

/// The exact program identities used by update_discrete_equations, excluding
/// condition/pre memories and all event actions. Whole shared programs retain
/// their original row identities and the targets their C stores use.
pub(super) fn append_parameter_discretes(
    component: &FmiCCodegenView,
    relation: &mut Relation,
) -> Result<(), FmiTangentPlanError> {
    let discrete = &component.problem().discrete;
    let mut rows = Vec::new();
    let mut slots = Vec::new();
    for &program in component.parameter_discrete_programs() {
        let ordinals = discrete
            .rhs
            .program_output_ordinals(program)
            .ok_or_else(|| {
                FmiTangentPlanError::Contract(
                    "parameter-discrete program has no output range".into(),
                )
            })?;
        for ordinal in ordinals {
            let row = discrete.rhs.output_index_at(ordinal).ok_or_else(|| {
                FmiTangentPlanError::Contract("parameter-discrete output has no row".into())
            })?;
            let target = discrete.update_targets.get(row).copied().ok_or_else(|| {
                FmiTangentPlanError::Contract("parameter-discrete row has no target".into())
            })?;
            rows.push(row);
            slots.push(target);
        }
    }
    let targets = combined_targets(component, &slots)?;
    relation.append(&discrete.rhs, &rows, &targets, true)
}

/// Every source row is either part of the solved relation or an explicit
/// check. The checked root supplies the projection row/target alignment.
fn initial_rows(
    component: &FmiCCodegenView,
    primal: &solve::ScalarProgramBlock,
    relation: &mut Relation,
) -> Result<(usize, Vec<usize>), FmiTangentPlanError> {
    let init = &component.problem().initialization;
    if primal.output_count() != init.row_targets().len()
        || primal.output_count() != init.row_roles().len()
    {
        return Err(FmiTangentPlanError::Contract(
            "initial residual inventory differs from its row owners".into(),
        ));
    }
    let mut solved = Vec::new();
    let mut checks = Vec::new();
    let mut slots = Vec::new();
    for (row, target) in init.row_targets().iter().enumerate() {
        if let Some(target) = target {
            solved.push(row);
            slots.push(*target);
        } else {
            checks.push(row);
        }
    }
    let targets = combined_targets(component, &slots)?;
    let expected = combined_targets(component, init.projection_unknowns())?;
    if targets.iter().copied().collect::<BTreeSet<_>>() != expected.iter().copied().collect() {
        return Err(FmiTangentPlanError::Contract(
            "initial row targets differ from the checked projection inventory".into(),
        ));
    }
    relation.append(primal, &solved, &targets, false)?;
    Ok((solved.len(), checks))
}

fn derive_consistency(
    primal: &solve::ScalarProgramBlock,
    rows: &[usize],
    y_len: usize,
    p_len: usize,
) -> Result<Option<FmiTangentConsistency>, FmiTangentPlanError> {
    if rows.is_empty() {
        return Ok(None);
    }
    let (selected, positions) = select_residual_programs(primal, rows)?;
    let program = ad::lower_scalar_program_block_full_time_jvp(&selected, y_len, p_len)
        .map_err(FmiTangentPlanError::Derivation)?;
    Ok(Some(FmiTangentConsistency {
        program,
        rows: positions.into_boxed_slice(),
    }))
}

/// Private preparation accumulator, never an independently issued Solve root.
#[derive(Default)]
pub(super) struct Relation {
    primal: solve::ScalarProgramBlock,
    rows: Vec<usize>,
    targets: Vec<usize>,
    subtract_targets: Vec<usize>,
}

impl Relation {
    pub(super) fn append(
        &mut self,
        source: &solve::ScalarProgramBlock,
        rows: &[usize],
        targets: &[usize],
        values: bool,
    ) -> Result<(), FmiTangentPlanError> {
        if rows.len() != targets.len() {
            return Err(FmiTangentPlanError::Contract(
                "initial equation/target cardinality mismatch".into(),
            ));
        }
        if rows.is_empty() {
            return Ok(());
        }
        if rows.iter().copied().collect::<BTreeSet<_>>().len() != rows.len() {
            return Err(FmiTangentPlanError::Contract(
                "initial source row is selected twice".into(),
            ));
        }
        let (selected, positions) = select_residual_programs(source, rows)?;
        let offset = self.primal.output_count();
        for (position, &target) in positions.into_iter().zip(targets) {
            self.rows.push(offset.checked_add(position).ok_or_else(|| {
                FmiTangentPlanError::Contract("initial output extent overflow".into())
            })?);
            self.targets.push(target);
            self.subtract_targets.push(if values {
                super::encode_value_target(target)?
            } else {
                0
            });
        }
        self.primal = join_program_blocks(&self.primal, &selected)?;
        Ok(())
    }

    pub(super) fn finish(
        self,
        y_len: usize,
        storage_len: usize,
    ) -> Result<Option<FmiImplicitTangentSystem>, FmiTangentPlanError> {
        if self.targets.is_empty() {
            return Ok(None);
        }
        if self.targets.iter().copied().collect::<BTreeSet<_>>().len() != self.targets.len()
            || self.targets.iter().any(|&target| target >= storage_len)
        {
            return Err(FmiTangentPlanError::Contract(
                "initial tangent target is duplicated or out of bounds".into(),
            ));
        }
        let p_len = storage_len.checked_sub(y_len).ok_or_else(|| {
            FmiTangentPlanError::Contract("relation storage does not contain Y".into())
        })?;
        let program = ad::lower_scalar_program_block_full_time_jvp(&self.primal, y_len, p_len)
            .map_err(FmiTangentPlanError::Derivation)?;
        Ok(Some(FmiImplicitTangentSystem {
            program,
            rows: self.rows.into_boxed_slice(),
            targets: self.targets.into_boxed_slice(),
            subtract_targets: self.subtract_targets.into_boxed_slice(),
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use solve::{BinaryOp, LinearOp};

    fn block(name: &str, text: &str, ops: Vec<LinearOp>) -> solve::ScalarProgramBlock {
        let mut sources = rumoca_core::SourceMap::new();
        let source = sources.add(name, text);
        let span = rumoca_core::Span::from_offsets(source, 0, text.len());
        solve::ScalarProgramBlock::with_output_indices(vec![ops], vec![span], vec![0]).unwrap()
    }

    fn algebraic() -> solve::ScalarProgramBlock {
        block(
            "initial-output.mo",
            "y-offset*x-u",
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::LoadY { dst: 1, index: 1 },
                LinearOp::LoadP { dst: 2, index: 2 },
                LinearOp::LoadP { dst: 3, index: 1 },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Mul,
                    lhs: 2,
                    rhs: 0,
                },
                LinearOp::Binary {
                    dst: 5,
                    op: BinaryOp::Sub,
                    lhs: 1,
                    rhs: 4,
                },
                LinearOp::Binary {
                    dst: 6,
                    op: BinaryOp::Sub,
                    lhs: 5,
                    rhs: 3,
                },
                LinearOp::StoreOutput { src: 6 },
            ],
        )
    }

    fn initial_residual() -> solve::ScalarProgramBlock {
        block(
            "initial-equation.mo",
            "offset-3*gain-u",
            vec![
                LinearOp::LoadP { dst: 0, index: 2 },
                LinearOp::LoadP { dst: 1, index: 0 },
                LinearOp::Const { dst: 2, value: 3.0 },
                LinearOp::Binary {
                    dst: 3,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 2,
                },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                LinearOp::LoadP { dst: 5, index: 1 },
                LinearOp::Binary {
                    dst: 6,
                    op: BinaryOp::Sub,
                    lhs: 4,
                    rhs: 5,
                },
                LinearOp::StoreOutput { src: 6 },
            ],
        )
    }

    fn evaluate(program: &solve::ScalarProgramBlock, seed: &[f64; 5]) -> Vec<f64> {
        let mut output = vec![0.0; program.output_count()];
        crate::eval_scalar_program_block(
            program,
            &[1.0, 6.0],
            &[2.0, 0.0, 6.0],
            0.0,
            Some(seed),
            &mut output,
        )
        .unwrap();
        output
    }

    #[test]
    fn initialization_parameter_solve_propagates_to_output_without_reseeding_given_state() {
        let mut relation = Relation::default();
        relation.append(&algebraic(), &[0], &[1], false).unwrap();
        relation
            .append(&initial_residual(), &[0], &[4], false)
            .unwrap();
        let system = relation.finish(2, 5).unwrap().unwrap();
        assert_eq!(system.targets(), [1, 4]);
        assert_eq!(system.rows(), [0, 1]);
        assert_eq!(system.subtract_targets(), [0, 0]);
        // Known order x/gain/u, unknowns y/offset. InitialP's mixed seed is
        // dx=11,dgain=5,du=7. The complete relation must solve dy=95,doffset=22.
        assert_eq!(
            evaluate(system.program(), &[11.0, 0.0, 5.0, 7.0, 0.0]),
            [-73.0, -22.0]
        );
        assert_eq!(
            evaluate(system.program(), &[0.0, 1.0, 0.0, 0.0, 0.0]),
            [1.0, 0.0]
        );
        assert_eq!(
            evaluate(system.program(), &[0.0, 0.0, 0.0, 0.0, 1.0]),
            [-1.0, 1.0]
        );
        assert_eq!(
            evaluate(system.program(), &[11.0, 95.0, 5.0, 7.0, 22.0]),
            [0.0, 0.0]
        );
        assert!(!system.targets().contains(&0));
    }

    #[test]
    fn initialization_binding_is_a_value_equation_not_an_extra_residual_subtraction() {
        let binding = block(
            "initial-binding.mo",
            "offset=3*gain",
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
        );
        let mut relation = Relation::default();
        relation.append(&binding, &[0], &[4], true).unwrap();
        let system = relation.finish(2, 5).unwrap().unwrap();
        assert_eq!(system.subtract_targets(), [5]);
        assert_eq!(
            evaluate(system.program(), &[0.0, 0.0, 5.0, 0.0, 0.0]),
            [15.0]
        );
        assert_eq!(
            evaluate(system.program(), &[0.0, 0.0, 0.0, 0.0, 1.0]),
            [0.0]
        );
        // The value row's own target contributes -1 exactly once.
    }

    #[test]
    fn initialization_duplicate_and_out_of_bounds_writers_are_refused() {
        let source = initial_residual();
        let mut duplicated = Relation::default();
        duplicated.append(&source, &[0], &[4], false).unwrap();
        duplicated.append(&source, &[0], &[4], false).unwrap();
        assert!(matches!(
            duplicated.finish(2, 5),
            Err(FmiTangentPlanError::Contract(_))
        ));
        let mut unbounded = Relation::default();
        unbounded.append(&source, &[0], &[5], false).unwrap();
        assert!(matches!(
            unbounded.finish(2, 5),
            Err(FmiTangentPlanError::Contract(_))
        ));
    }

    #[test]
    fn unowned_initialization_rows_remain_explicit_consistency_obligations() {
        let source = initial_residual();
        let consistency = derive_consistency(&source, &[0], 2, 3).unwrap().unwrap();
        assert_eq!(consistency.rows(), [0]);
        assert_eq!(
            evaluate(consistency.program(), &[11.0, 95.0, 5.0, 7.0, 22.0]),
            [0.0]
        );
        assert_eq!(
            evaluate(consistency.program(), &[11.0, 95.0, 5.0, 7.0, 23.0]),
            [1.0]
        );
        assert!(derive_consistency(&source, &[], 2, 3).unwrap().is_none());
    }
}
