use super::*;

#[test]
fn algebraic_projection_loop_records_structural_matching_steps() -> Result<(), LowerError> {
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
    assert_eq!(block.causal_steps.len(), 2);
    assert_eq!(block.causal_steps[0].row, 3);
    assert_eq!(block.causal_steps[0].y_index, 21);
    assert_eq!(block.causal_steps[1].row, 4);
    assert_eq!(block.causal_steps[1].y_index, 20);
    Ok(())
}
