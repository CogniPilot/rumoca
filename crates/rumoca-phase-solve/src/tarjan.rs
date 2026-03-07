//! Tarjan's algorithm for strongly connected components.

/// Find all strongly connected components using Tarjan's algorithm.
///
/// Returns SCCs in reverse topological order. Each SCC is a `Vec` of node indices.
pub(crate) fn tarjan_scc(n: usize, adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let mut state = TarjanState::new(n);
    for v in 0..n {
        if state.index[v].is_none() {
            state.strongconnect(v, adj);
        }
    }
    state.sccs
}

struct TarjanState {
    index_counter: usize,
    stack: Vec<usize>,
    on_stack: Vec<bool>,
    index: Vec<Option<usize>>,
    lowlink: Vec<usize>,
    sccs: Vec<Vec<usize>>,
}

impl TarjanState {
    fn new(n: usize) -> Self {
        Self {
            index_counter: 0,
            stack: Vec::new(),
            on_stack: vec![false; n],
            index: vec![None; n],
            lowlink: vec![0; n],
            sccs: Vec::new(),
        }
    }

    fn strongconnect(&mut self, v: usize, adj: &[Vec<usize>]) {
        self.index[v] = Some(self.index_counter);
        self.lowlink[v] = self.index_counter;
        self.index_counter += 1;
        self.stack.push(v);
        self.on_stack[v] = true;

        for &w in &adj[v] {
            if self.index[w].is_none() {
                self.strongconnect(w, adj);
                self.lowlink[v] = self.lowlink[v].min(self.lowlink[w]);
            } else if self.on_stack[w] {
                self.lowlink[v] = self.lowlink[v]
                    .min(self.index[w].expect("tarjan invariant: on-stack node must have index"));
            }
        }

        if self.lowlink[v] == self.index[v].expect("tarjan invariant: visited node must have index")
        {
            self.pop_scc(v);
        }
    }

    fn pop_scc(&mut self, root: usize) {
        let mut scc = Vec::new();
        loop {
            let w = self
                .stack
                .pop()
                .expect("tarjan invariant: stack must contain SCC root");
            self.on_stack[w] = false;
            scc.push(w);
            if w == root {
                break;
            }
        }
        self.sccs.push(scc);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tarjan_no_loops() {
        let adj = vec![vec![1], vec![2], vec![]];
        let sccs = tarjan_scc(3, &adj);
        assert!(
            sccs.iter().all(|scc| scc.len() == 1),
            "no cycles means all SCCs are singletons"
        );
    }

    #[test]
    fn test_tarjan_single_loop() {
        let adj = vec![vec![1], vec![2], vec![0]];
        let sccs = tarjan_scc(3, &adj);
        let loops: Vec<_> = sccs.iter().filter(|scc| scc.len() > 1).collect();
        assert_eq!(loops.len(), 1, "should find one loop");
        assert_eq!(loops[0].len(), 3, "loop should contain all 3 nodes");
    }

    #[test]
    fn test_tarjan_two_loops() {
        let adj = vec![vec![1], vec![0], vec![3], vec![2]];
        let sccs = tarjan_scc(4, &adj);
        let loops: Vec<_> = sccs.iter().filter(|scc| scc.len() > 1).collect();
        assert_eq!(loops.len(), 2, "should find two loops");
    }
}
