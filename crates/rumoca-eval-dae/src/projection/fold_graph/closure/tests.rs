use super::*;
use std::collections::VecDeque;

// Original dense fixed point, retained independently of sorted-set sharing.
fn dense(
    edges: &[Vec<usize>],
    direct: &[Arc<[FunctionParameterDependency]>],
) -> Vec<Vec<FunctionParameterDependency>> {
    let mut predecessors = vec![Vec::new(); edges.len()];
    for (source, targets) in edges.iter().enumerate() {
        for target in targets {
            predecessors[*target].push(source);
        }
    }
    let mut parameters = Vec::new();
    let mut parameter_ids = HashMap::default();
    let mut closures = vec![HashSet::default(); edges.len()];
    let mut pending = VecDeque::new();
    for (node, values) in direct.iter().enumerate() {
        for parameter in values.iter() {
            let next = parameters.len();
            let id = *parameter_ids.entry(parameter.clone()).or_insert_with(|| {
                parameters.push(parameter.clone());
                next
            });
            if closures[node].insert(id) {
                pending.push_back((node, id));
            }
        }
    }
    while let Some((node, parameter)) = pending.pop_front() {
        for predecessor in &predecessors[node] {
            if closures[*predecessor].insert(parameter) {
                pending.push_back((*predecessor, parameter));
            }
        }
    }
    closures
        .into_iter()
        .map(|closure| {
            parameters
                .iter()
                .enumerate()
                .filter(|(id, _)| closure.contains(id))
                .map(|(_, value)| value.clone())
                .collect()
        })
        .collect()
}

fn key(parameter: u32, field: Option<usize>, scalar: usize) -> FunctionParameterDependency {
    match field {
        Some(field) => FunctionParameterDependency::RecordField {
            activation: crate::projection::Activation::Guaranteed,
            parameter,
            field: crate::projection::FieldPath::Field(field),
            scalar,
        },
        None => FunctionParameterDependency::Scalar {
            activation: crate::projection::Activation::Guaranteed,
            parameter,
            scalar,
        },
    }
}

fn compare(edges: &[Vec<usize>], direct: &[Arc<[FunctionParameterDependency]>]) {
    let (actual, _) = complete(edges, direct);
    let expected = dense(edges, direct);
    for (actual, expected) in actual.iter().zip(expected) {
        assert_eq!(actual.as_ref(), expected);
    }
}

#[test]
fn ordered_closures_match_dense_oracle_with_cycles_disjoint_fields_and_duplicates() {
    let a = key(0, None, 2);
    let b = key(0, Some(0), 2);
    let c = key(0, Some(1), 2);
    let d = key(1, None, 2);
    let e = key(0, None, 1);
    let direct = [
        Arc::from([c.clone(), a.clone()]),
        Arc::from([b.clone(), e]),
        Arc::from([d]),
        Arc::from([a, b, c]),
        Arc::from([]),
    ];
    compare(&[vec![1, 1], vec![0, 2], vec![2], vec![], vec![4]], &direct);
    compare(&[vec![1], vec![2], vec![3], vec![4], vec![0]], &direct);
    compare(&[vec![], vec![], vec![], vec![], vec![]], &direct);
}

#[test]
fn every_three_node_graph_matches_dense_ordered_fixed_point() {
    let direct = [
        Arc::from([key(0, None, 4), key(0, Some(1), 2)]),
        Arc::from([key(1, None, 3)]),
        Arc::from([key(0, Some(0), 2), key(0, None, 4)]),
    ];
    for mask in 0..512 {
        let edges = (0..3)
            .map(|source| {
                (0..3)
                    .filter(|target| mask & (1 << (3 * source + target)) != 0)
                    .collect()
            })
            .collect::<Vec<_>>();
        compare(&edges, &direct);
    }
}

#[test]
fn full_capture_inventory_shares_only_equal_completed_results() {
    let capture: Arc<[FunctionParameterDependency]> = (0..14400)
        .map(|scalar| key(0, None, scalar))
        .collect::<Vec<_>>()
        .into();
    let direct = [
        Arc::clone(&capture),
        Arc::clone(&capture),
        Arc::from([key(1, None, 0)]),
        Arc::from([]),
    ];
    let edges = [vec![1], vec![0], vec![], vec![0]];
    compare(&edges, &direct);
    let (actual, metrics) = complete(&edges, &direct);
    assert!(Arc::ptr_eq(&actual[0], &actual[1]));
    assert!(Arc::ptr_eq(&actual[0], &actual[3]));
    assert!(!Arc::ptr_eq(&actual[0], &actual[2]));
    assert_eq!(metrics.output_occurrences, 43201);
    assert_eq!(metrics.retained_ids, 14401);
}

#[test]
fn empty_graph_and_empty_cycles_match_dense_oracle() {
    compare(&[], &[]);
    compare(&[vec![1], vec![0]], &[Arc::from([]), Arc::from([])]);
}

#[test]
fn completed_component_sets_match_dense_disconnected_cyclic_and_repeated_edges() {
    let direct = (0..128)
        .map(|node| Arc::from([key((node % 3) as u32, Some(node % 2), node % 7)]))
        .collect::<Vec<_>>();
    let edges = (0..128)
        .map(|node| {
            if node % 11 == 0 {
                vec![node]
            } else if node % 5 == 0 {
                vec![node - 1, node - 1]
            } else {
                vec![(node + 1).min(127), node]
            }
        })
        .collect::<Vec<_>>();
    compare(&edges, &direct);
    let components = components::derive(&edges);
    let mut retained = components
        .nodes
        .iter()
        .flatten()
        .copied()
        .collect::<Vec<_>>();
    retained.sort_unstable();
    assert_eq!(
        retained,
        (0..128).collect::<Vec<_>>(),
        "every original node retained exactly once"
    );
}

#[test]
fn full_capacity_deep_graph_retains_only_completed_shared_capture_sets() {
    let capacity = 14400;
    let capture: Arc<[FunctionParameterDependency]> = (0..capacity)
        .map(|scalar| key(0, None, scalar))
        .collect::<Vec<_>>()
        .into();
    let mut direct = vec![Arc::from([]); capacity];
    direct[capacity - 1] = Arc::clone(&capture);
    let edges = (0..capacity)
        .map(|node| {
            if node + 1 == capacity {
                vec![node]
            } else {
                vec![node + 1, node + 1, node]
            }
        })
        .collect::<Vec<_>>();
    let (actual, metrics) = complete(&edges, &direct);
    assert_eq!(actual.len(), capacity);
    for completed in &actual {
        assert_eq!(completed.as_ref(), capture.as_ref());
        assert!(Arc::ptr_eq(completed, &actual[0]));
    }
    assert_eq!(
        metrics.unique_sets, 2,
        "only empty and completed capture inventories retained"
    );
    assert_eq!(metrics.retained_ids, capacity);
    assert_eq!(metrics.output_occurrences, capacity * capacity);
    assert_eq!(
        metrics.comparisons, 0,
        "already checked immutable captures reused without rescanning"
    );
}
