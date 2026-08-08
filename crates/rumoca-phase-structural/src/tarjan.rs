//! Tarjan's algorithm for strongly connected components.
//!
//! The depth-first search runs on an explicit stack. Array-shaped models
//! produce dependency chains hundreds of thousands of equations long, and the
//! recursive form overflows the main thread's stack on those. Children are
//! pushed in `adj[v]` order and the `lowlink` update is applied on child pop,
//! so the emitted SCC sequence is byte-identical to the recursive form's --
//! `build_blt_blocks` depends on that order being the BLT evaluation order.

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

/// One node on the explicit DFS stack: the node and how far its adjacency list
/// has been scanned.
#[derive(Clone, Copy)]
struct TarjanFrame {
    node: usize,
    cursor: usize,
}

struct TarjanState {
    index_counter: usize,
    stack: Vec<usize>,
    frames: Vec<TarjanFrame>,
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
            frames: Vec::new(),
            on_stack: vec![false; n],
            index: vec![None; n],
            lowlink: vec![0; n],
            sccs: Vec::new(),
        }
    }

    fn strongconnect(&mut self, root: usize, adj: &[Vec<usize>]) {
        self.enter(root);
        self.frames.push(TarjanFrame {
            node: root,
            cursor: 0,
        });
        while let Some(frame) = self.frames.last().copied() {
            match self.next_child(frame, adj) {
                Some(child) => self.descend(child),
                None => self.finish_node(frame.node),
            }
        }
    }

    fn enter(&mut self, node: usize) {
        self.index[node] = Some(self.index_counter);
        self.lowlink[node] = self.index_counter;
        self.index_counter += 1;
        self.stack.push(node);
        self.on_stack[node] = true;
    }

    /// Scan `frame.node`'s adjacency from its cursor, folding already-visited
    /// on-stack neighbours into the lowlink, and return the first unvisited
    /// child to descend into.
    fn next_child(&mut self, frame: TarjanFrame, adj: &[Vec<usize>]) -> Option<usize> {
        let neighbors = adj.get(frame.node)?;
        let mut cursor = frame.cursor;
        while let Some(&w) = neighbors.get(cursor) {
            cursor += 1;
            if self.index[w].is_none() {
                self.set_cursor(cursor);
                return Some(w);
            }
            if self.on_stack[w] {
                let child_index =
                    self.index[w].expect("tarjan invariant: on-stack node must have index");
                self.lowlink[frame.node] = self.lowlink[frame.node].min(child_index);
            }
        }
        self.set_cursor(cursor);
        None
    }

    fn set_cursor(&mut self, cursor: usize) {
        if let Some(frame) = self.frames.last_mut() {
            frame.cursor = cursor;
        }
    }

    fn descend(&mut self, child: usize) {
        self.enter(child);
        self.frames.push(TarjanFrame {
            node: child,
            cursor: 0,
        });
    }

    /// Pop `node`, propagating its lowlink to its parent -- the recursive
    /// form's post-call `lowlink[v] = min(lowlink[v], lowlink[w])`.
    fn finish_node(&mut self, node: usize) {
        self.frames.pop();
        if self.lowlink[node]
            == self.index[node].expect("tarjan invariant: visited node must have index")
        {
            self.pop_scc(node);
        }
        if let Some(parent) = self.frames.last().copied() {
            self.lowlink[parent.node] = self.lowlink[parent.node].min(self.lowlink[node]);
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

    /// Pins the exact SCC sequence on a mixed graph. `build_blt_blocks` treats
    /// this order as the BLT evaluation order, so any drift here moves every
    /// model's block sequence.
    #[test]
    fn tarjan_scc_order_is_unchanged_on_mixed_graph() {
        // 0 -> 1 -> {2,3}; 2 -> 4 -> 5 -> 2 is a 3-cycle; 6 -> 7; 7 isolated sink.
        let adj = vec![
            vec![1],
            vec![2, 3],
            vec![4],
            vec![],
            vec![5],
            vec![2],
            vec![7],
            vec![],
        ];
        let sccs = tarjan_scc(8, &adj);
        assert_eq!(
            sccs,
            vec![vec![5, 4, 2], vec![3], vec![1], vec![0], vec![7], vec![6],]
        );
    }

    /// A 500_000-node linear chain. The recursive form recurses once per node
    /// and blows the 8 MB main-thread stack; the explicit stack must not.
    #[test]
    fn tarjan_scc_handles_a_500k_linear_chain() {
        const NODES: usize = 500_000;
        let mut adj = vec![Vec::new(); NODES];
        for (node, neighbors) in adj.iter_mut().enumerate().take(NODES - 1) {
            neighbors.push(node + 1);
        }
        let sccs = tarjan_scc(NODES, &adj);
        assert_eq!(sccs.len(), NODES);
        assert!(sccs.iter().all(|scc| scc.len() == 1));
        // Reverse topological order: the chain's sink is emitted first.
        assert_eq!(sccs[0], vec![NODES - 1]);
        assert_eq!(sccs[NODES - 1], vec![0]);
    }
    /// The pre-change recursive Tarjan, kept verbatim so the explicit-stack
    /// rewrite can be pinned against it. `build_blt_blocks` treats the emitted
    /// SCC sequence as the BLT evaluation order, so equivalence here is what
    /// keeps every model's block sequence stable.
    mod reference {
        // VERBATIM copy of the pre-change recursive Tarjan. Its nesting is what
        // the explicit-stack rewrite removed; refactoring it here would weaken
        // the equivalence assertion below.
        #![allow(
            clippy::excessive_nesting,
            reason = "verbatim copy of the pre-change recursive Tarjan, kept unmodified so the equivalence assertion is meaningful"
        )]

        struct State {
            counter: usize,
            stack: Vec<usize>,
            on_stack: Vec<bool>,
            index: Vec<Option<usize>>,
            lowlink: Vec<usize>,
            sccs: Vec<Vec<usize>>,
        }

        impl State {
            fn strongconnect(&mut self, v: usize, adj: &[Vec<usize>]) {
                self.index[v] = Some(self.counter);
                self.lowlink[v] = self.counter;
                self.counter += 1;
                self.stack.push(v);
                self.on_stack[v] = true;
                for &w in &adj[v] {
                    if self.index[w].is_none() {
                        self.strongconnect(w, adj);
                        self.lowlink[v] = self.lowlink[v].min(self.lowlink[w]);
                    } else if self.on_stack[w] {
                        let seen = self.index[w].expect("visited node has an index");
                        self.lowlink[v] = self.lowlink[v].min(seen);
                    }
                }
                if self.lowlink[v] == self.index[v].expect("visited node has an index") {
                    let mut scc = Vec::new();
                    loop {
                        let w = self.stack.pop().expect("stack holds the SCC root");
                        self.on_stack[w] = false;
                        scc.push(w);
                        if w == v {
                            break;
                        }
                    }
                    self.sccs.push(scc);
                }
            }
        }

        pub(super) fn tarjan_scc(n: usize, adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
            let mut state = State {
                counter: 0,
                stack: Vec::new(),
                on_stack: vec![false; n],
                index: vec![None; n],
                lowlink: vec![0; n],
                sccs: Vec::new(),
            };
            for v in 0..n {
                if state.index[v].is_none() {
                    state.strongconnect(v, adj);
                }
            }
            state.sccs
        }
    }

    fn next_random(state: &mut u64) -> u64 {
        *state ^= *state << 13;
        *state ^= *state >> 7;
        *state ^= *state << 17;
        *state
    }

    #[test]
    fn iterative_tarjan_reproduces_the_recursive_reference_on_random_graphs() {
        let mut state = 0x2545_F491_4F6C_DD1Du64;
        for case in 0..400u32 {
            let n = 1 + (next_random(&mut state) % 12) as usize;
            let adj: Vec<Vec<usize>> = (0..n)
                .map(|_| {
                    (0..n)
                        .filter(|_| next_random(&mut state).is_multiple_of(4))
                        .collect()
                })
                .collect();
            assert_eq!(
                tarjan_scc(n, &adj),
                reference::tarjan_scc(n, &adj),
                "case {case}: adj {adj:?}"
            );
        }
    }
}
