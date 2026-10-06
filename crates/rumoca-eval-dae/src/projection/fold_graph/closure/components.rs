//! Construction-local SCC evaluation order; original graph owners are unchanged.
pub(super) struct Components {
    pub(super) nodes: Vec<Vec<usize>>,
    pub(super) of: Vec<usize>,
}

pub(super) fn derive(edges: &[Vec<usize>]) -> Components {
    let mut reverse = vec![Vec::new(); edges.len()];
    for (source, targets) in edges.iter().enumerate() {
        for target in targets {
            reverse[*target].push(source);
        }
    }
    let order = finishing_order(edges);
    let mut of = vec![usize::MAX; edges.len()];
    let mut nodes = Vec::new();
    for root in order.into_iter().rev() {
        if of[root] == usize::MAX {
            nodes.push(component(root, nodes.len(), &reverse, &mut of));
        }
    }
    Components { nodes, of }
}

fn finishing_order(edges: &[Vec<usize>]) -> Vec<usize> {
    let mut visited = vec![false; edges.len()];
    let mut order = Vec::with_capacity(edges.len());
    for root in 0..edges.len() {
        if !visited[root] {
            finish(root, edges, &mut visited, &mut order);
        }
    }
    order
}

fn finish(root: usize, edges: &[Vec<usize>], visited: &mut [bool], order: &mut Vec<usize>) {
    visited[root] = true;
    let mut pending = vec![(root, 0)];
    while let Some((node, ordinal)) = pending.last_mut() {
        if let Some(target) = edges[*node].get(*ordinal) {
            *ordinal += 1;
            if !visited[*target] {
                visited[*target] = true;
                pending.push((*target, 0));
            }
        } else {
            order.push(*node);
            pending.pop();
        }
    }
}

fn component(root: usize, id: usize, reverse: &[Vec<usize>], of: &mut [usize]) -> Vec<usize> {
    of[root] = id;
    let mut nodes = Vec::new();
    let mut pending = vec![root];
    while let Some(node) = pending.pop() {
        nodes.push(node);
        for predecessor in &reverse[node] {
            if of[*predecessor] == usize::MAX {
                of[*predecessor] = id;
                pending.push(*predecessor);
            }
        }
    }
    nodes
}
