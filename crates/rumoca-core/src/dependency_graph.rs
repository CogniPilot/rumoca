/// One strongly connected component in deterministic dependency-first order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DependencyScc {
    pub members: Box<[usize]>,
    pub recursive: bool,
}

/// An edge referenced a node outside the dependency graph.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DependencyGraphError {
    pub source: usize,
    pub target: usize,
    pub node_count: usize,
}

impl std::fmt::Display for DependencyGraphError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "dependency edge {} -> {} exceeds graph size {}",
            self.source, self.target, self.node_count
        )
    }
}

impl std::error::Error for DependencyGraphError {}

/// Compute deterministic dependency-first strongly connected components.
///
/// Edges point from a dependent node to its dependencies. Both graph walks are
/// iterative, so generated dependency chains cannot overflow the call stack.
pub fn dependency_first_sccs(
    dependencies: &[Vec<usize>],
) -> Result<Vec<DependencyScc>, DependencyGraphError> {
    check_targets(dependencies)?;
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
        components.push(DependencyScc {
            members: members.into_boxed_slice(),
            recursive,
        });
    }
    components.reverse();
    Ok(components)
}

fn check_targets(dependencies: &[Vec<usize>]) -> Result<(), DependencyGraphError> {
    for (source, targets) in dependencies.iter().enumerate() {
        if let Some(&target) = targets.iter().find(|&&target| target >= dependencies.len()) {
            return Err(DependencyGraphError {
                source,
                target,
                node_count: dependencies.len(),
            });
        }
    }
    Ok(())
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
        let components =
            dependency_first_sccs(&[vec![1], vec![2, 3], vec![], vec![1], vec![4]]).unwrap();

        assert_eq!(
            components,
            vec![
                DependencyScc {
                    members: Box::new([2]),
                    recursive: false,
                },
                DependencyScc {
                    members: Box::new([1, 3]),
                    recursive: true,
                },
                DependencyScc {
                    members: Box::new([0]),
                    recursive: false,
                },
                DependencyScc {
                    members: Box::new([4]),
                    recursive: true,
                },
            ]
        );
    }

    #[test]
    fn empty_graph_has_no_components() {
        assert!(dependency_first_sccs(&[]).unwrap().is_empty());
    }

    #[test]
    fn invalid_target_is_a_typed_error() {
        assert_eq!(
            dependency_first_sccs(&[vec![1]]),
            Err(DependencyGraphError {
                source: 0,
                target: 1,
                node_count: 1,
            })
        );
    }
}
