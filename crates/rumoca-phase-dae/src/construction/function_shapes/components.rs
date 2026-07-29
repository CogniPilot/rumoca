#[derive(Clone, Debug, PartialEq, Eq)]
pub(in crate::construction) struct FunctionComponent {
    pub(in crate::construction) specializations: Box<[usize]>,
    pub(in crate::construction) recursive: bool,
}

/// Compute deterministic dependency-first strongly connected components.
///
/// Edges point from a caller to its callees. Both graph walks are iterative so
/// a large generated function graph cannot overflow the compiler stack.
pub(in crate::construction) fn dependency_first_components(
    dependencies: &[Vec<usize>],
) -> Vec<FunctionComponent> {
    let finish_order = finish_order(dependencies);
    let transposed = transpose(dependencies);
    let mut seen = vec![false; dependencies.len()];
    let mut components = Vec::new();
    for &start in finish_order.iter().rev() {
        if seen[start] {
            continue;
        }
        let mut members = collect_component(start, &transposed, &mut seen);
        members.sort_unstable();
        let recursive = members.len() > 1
            || dependencies[members[0]]
                .iter()
                .any(|&callee| callee == members[0]);
        components.push(FunctionComponent {
            specializations: members.into_boxed_slice(),
            recursive,
        });
    }
    components.reverse();
    components
}

fn finish_order(edges: &[Vec<usize>]) -> Vec<usize> {
    let mut seen = vec![false; edges.len()];
    let mut finished = Vec::with_capacity(edges.len());
    for start in 0..edges.len() {
        if seen[start] {
            continue;
        }
        seen[start] = true;
        let mut stack = vec![(start, 0_usize)];
        while let Some((node, next_edge)) = stack.last_mut() {
            if let Some(&next) = edges[*node].get(*next_edge) {
                *next_edge += 1;
                push_unseen(next, &mut seen, &mut stack);
            } else {
                finished.push(*node);
                stack.pop();
            }
        }
    }
    finished
}

fn push_unseen(next: usize, seen: &mut [bool], stack: &mut Vec<(usize, usize)>) {
    if !seen[next] {
        seen[next] = true;
        stack.push((next, 0));
    }
}

fn transpose(edges: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let mut transposed = vec![Vec::new(); edges.len()];
    for (caller, dependencies) in edges.iter().enumerate() {
        for &dependency in dependencies {
            transposed[dependency].push(caller);
        }
    }
    transposed
}

fn collect_component(start: usize, edges: &[Vec<usize>], seen: &mut [bool]) -> Vec<usize> {
    seen[start] = true;
    let mut members = Vec::new();
    let mut stack = vec![start];
    while let Some(node) = stack.pop() {
        members.push(node);
        for &next in edges[node].iter().rev() {
            if !seen[next] {
                seen[next] = true;
                stack.push(next);
            }
        }
    }
    members
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn components_are_dependency_first_and_classify_recursion() {
        // 0 -> 1 -> 2, 1 -> 3, and 3 -> 1 form one recursive SCC.
        let components =
            dependency_first_components(&[vec![1], vec![2, 3], vec![], vec![1], vec![4]]);

        assert_eq!(
            components,
            vec![
                FunctionComponent {
                    specializations: Box::new([2]),
                    recursive: false,
                },
                FunctionComponent {
                    specializations: Box::new([1, 3]),
                    recursive: true,
                },
                FunctionComponent {
                    specializations: Box::new([0]),
                    recursive: false,
                },
                FunctionComponent {
                    specializations: Box::new([4]),
                    recursive: true,
                },
            ]
        );
    }

    #[test]
    fn empty_graph_has_no_components() {
        assert!(dependency_first_components(&[]).is_empty());
    }
}
