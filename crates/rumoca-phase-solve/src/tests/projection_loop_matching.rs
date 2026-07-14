use super::*;

#[test]
fn algebraic_projection_loop_does_not_promote_structural_pairs_to_causal_steps()
-> Result<(), LowerError> {
    let projection_incidence = ProjectionIncidence {
        incidence: Incidence::new(
            vec![
                BTreeSet::from([0, 1]).into_iter().collect(),
                BTreeSet::from([0, 1]).into_iter().collect(),
            ],
            vec![EquationRef(3), EquationRef(4)],
            vec![UnknownId::SolverY(20), UnknownId::SolverY(21)],
        ),
        unknown_y_indices: vec![20, 21],
    };

    let block = super::super::lower_algebraic_loop_projection_block(
        &[EquationRef(3), EquationRef(4)],
        &[UnknownId::SolverY(21), UnknownId::SolverY(20)],
        &[],
        &projection_incidence,
        solve_test_span(),
    )?
    .expect("loop block should lower");

    assert_eq!(block.rows, vec![3, 4]);
    assert_eq!(block.y_indices, vec![20, 21]);
    assert!(block.causal_steps.is_empty());
    Ok(())
}

#[test]
fn algebraic_projection_loop_keeps_assignment_targets_in_simultaneous_solve()
-> Result<(), LowerError> {
    let projection_incidence = ProjectionIncidence {
        incidence: Incidence::new(
            vec![BTreeSet::from([0, 1]).into_iter().collect()],
            vec![EquationRef(3)],
            vec![UnknownId::SolverY(20), UnknownId::SolverY(21)],
        ),
        unknown_y_indices: vec![20, 21],
    };
    let mut row_targets = vec![None; 4];
    row_targets[3] = Some(solve::scalar_slot_y(20));

    let block = super::super::lower_algebraic_loop_projection_block(
        &[EquationRef(3)],
        &[UnknownId::SolverY(21)],
        &row_targets,
        &projection_incidence,
        solve_test_span(),
    )?
    .expect("loop block should lower");

    assert_eq!(block.y_indices, vec![20, 21]);
    assert!(block.causal_steps.is_empty());
    Ok(())
}
