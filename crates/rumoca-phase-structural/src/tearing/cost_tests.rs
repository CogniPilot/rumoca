use super::*;

// The source graph is retained in the MultiBody working ledger. Vector force,
// torque and acceleration balances are coupled by scalar connector equalities.
fn vector_balance_graph() -> Vec<HashSet<usize>> {
    [
        vec![0, 7, 8, 9],
        vec![0, 1],
        vec![2, 7, 8, 9],
        vec![2, 3],
        vec![4, 13],
        vec![5, 12],
        vec![6, 14],
        vec![6, 7, 8, 9],
        vec![5, 7, 8, 9],
        vec![4, 7, 8, 9],
        vec![7, 8, 9, 10],
        vec![10, 11],
        vec![11, 12, 14, 16, 18, 19],
        vec![3, 12, 13, 16, 18, 19],
        vec![1, 13, 14, 16, 18, 19],
        vec![14, 15, 16, 17, 19, 20],
        vec![16, 21, 22, 23],
        vec![12, 15, 16, 17, 18, 20],
        vec![18, 21, 22, 23],
        vec![19, 21, 22, 23],
        vec![13, 15, 17, 18, 19, 20],
        vec![20, 21, 22, 23],
        vec![17, 21, 22, 23],
        vec![15, 21, 22, 23],
    ]
    .into_iter()
    .map(|row| row.into_iter().collect())
    .collect()
}

fn check_partition(graph: &[HashSet<usize>], plan: &TearingResult) {
    let mut known = plan
        .tear_var_local_indices
        .iter()
        .copied()
        .collect::<HashSet<_>>();
    assert_eq!(known.len(), plan.tear_var_local_indices.len());
    let mut equations = HashSet::new();
    for &(equation, variable) in &plan.causal_sequence {
        assert!(equations.insert(equation));
        assert!(!known.contains(&variable));
        assert_eq!(
            graph[equation]
                .difference(&known)
                .copied()
                .collect::<HashSet<_>>(),
            HashSet::from([variable])
        );
        known.insert(variable);
    }
    for &equation in &plan.residual_eq_local_indices {
        assert!(equations.insert(equation));
    }
    assert_eq!(known.len(), graph.len());
    assert_eq!(equations.len(), graph.len());
    assert_eq!(
        plan.residual_eq_local_indices.len(),
        plan.tear_var_local_indices.len()
    );
}

#[test]
fn vector_balances_do_not_pay_for_locally_attractive_connector_tears() {
    let graph = vector_balance_graph();
    let plan = tear_algebraic_loop(graph.len(), &graph).unwrap();
    check_partition(&graph, &plan);
    assert!(
        plan.tear_var_local_indices.len() <= 7,
        "a complete degree-based candidate uses seven tears; got {:?}",
        plan.tear_var_local_indices
    );
}

#[test]
fn alternate_tearing_respects_restricted_candidates_and_input_order() {
    let graph = vector_balance_graph();
    let candidates = (0..graph.len())
        .map(|row| HashSet::from([row]))
        .collect::<Vec<_>>();
    let plan =
        tear_algebraic_loop_with_causal_candidates(graph.len(), &graph, &candidates).unwrap();
    check_partition(&graph, &plan);
    for &(equation, variable) in &plan.causal_sequence {
        assert!(candidates[equation].contains(&variable));
    }
    let reordered = graph
        .iter()
        .map(|row| {
            let mut entries = row.iter().copied().collect::<Vec<_>>();
            entries.sort_unstable_by(|lhs, rhs| rhs.cmp(lhs));
            entries.into_iter().collect()
        })
        .collect::<Vec<_>>();
    let replay =
        tear_algebraic_loop_with_causal_candidates(graph.len(), &reordered, &candidates).unwrap();
    assert_eq!(plan.tear_var_local_indices, replay.tear_var_local_indices);
    assert_eq!(
        plan.residual_eq_local_indices,
        replay.residual_eq_local_indices
    );
    assert_eq!(plan.causal_sequence, replay.causal_sequence);
}
