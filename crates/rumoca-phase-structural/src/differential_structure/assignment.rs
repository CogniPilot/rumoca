//! Sparse unit-capacity assignment by reduced-cost augmenting paths.

use std::cmp::Reverse;
use std::collections::BinaryHeap;

use super::SignatureEntry;

#[cfg(test)]
thread_local! {
    pub(super) static SEARCHES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

#[derive(Clone, Copy)]
struct Edge {
    target: usize,
    reverse: usize,
    cost: i64,
    available: bool,
}

struct Assignment {
    edges: Vec<Vec<Edge>>,
    potentials: Vec<i64>,
    source: usize,
    sink: usize,
}

pub(super) fn maximum_weight_matching(
    rows: &[Vec<SignatureEntry>],
    columns: usize,
) -> Result<Vec<Option<usize>>, &'static str> {
    let source = rows
        .len()
        .checked_add(columns)
        .ok_or("assignment size overflow")?;
    let sink = source.checked_add(1).ok_or("assignment size overflow")?;
    let nodes = sink.checked_add(1).ok_or("assignment size overflow")?;
    let mut graph = Assignment {
        edges: vec![Vec::new(); nodes],
        potentials: vec![0; nodes],
        source,
        sink,
    };
    let maximum = rows
        .iter()
        .flatten()
        .map(|entry| entry.order)
        .max()
        .unwrap_or(0);
    for (row, entries) in rows.iter().enumerate() {
        graph.add_edge(source, row, 0);
        for entry in entries {
            if entry.column >= columns {
                return Err("signature column is outside the variable view");
            }
            graph.add_edge(
                row,
                rows.len() + entry.column,
                i64::from(maximum - entry.order),
            );
        }
    }
    for column in 0..columns {
        graph.add_edge(rows.len() + column, sink, 0);
    }
    let limit = rows.len().min(columns);
    let mut matched = 0;
    while matched < limit {
        matched += graph.augment_direct()?;
        if matched == limit || !graph.augment()? {
            break;
        }
        matched += 1;
    }
    Ok((0..rows.len())
        .map(|row| {
            graph.edges[row].iter().find_map(|edge| {
                (!edge.available && (rows.len()..source).contains(&edge.target))
                    .then(|| edge.target - rows.len())
            })
        })
        .collect())
}

impl Assignment {
    fn add_edge(&mut self, source: usize, target: usize, cost: i64) {
        let forward = self.edges[source].len();
        let reverse = self.edges[target].len();
        self.edges[source].push(Edge {
            target,
            reverse,
            cost,
            available: true,
        });
        self.edges[target].push(Edge {
            target: source,
            reverse: forward,
            cost: -cost,
            available: false,
        });
    }

    /// Zero reduced-cost source/row/column/sink paths are already shortest.
    /// Reversing only zero-cost edges preserves feasible potentials, so these
    /// independent augmentations need neither a heap search nor a dual update.
    fn augment_direct(&mut self) -> Result<usize, &'static str> {
        let mut matched = 0;
        for source_edge in 0..self.edges[self.source].len() {
            let edge = self.edges[self.source][source_edge];
            if !edge.available || self.reduced_cost(self.source, edge)? != 0 {
                continue;
            }
            let row = edge.target;
            let Some((row_edge, column_edge)) = self.direct_column(row)? else {
                continue;
            };
            let column = self.edges[row][row_edge].target;
            self.reverse_edge(self.source, source_edge);
            self.reverse_edge(row, row_edge);
            self.reverse_edge(column, column_edge);
            matched += 1;
        }
        Ok(matched)
    }

    fn direct_column(&self, row: usize) -> Result<Option<(usize, usize)>, &'static str> {
        for (index, &edge) in self.edges[row].iter().enumerate() {
            if !edge.available || edge.target == self.source || self.reduced_cost(row, edge)? != 0 {
                continue;
            }
            // Column-to-sink edges are appended after all row-to-column edges.
            let column_edge = self.edges[edge.target].len() - 1;
            let terminal = self.edges[edge.target][column_edge];
            if terminal.available && self.reduced_cost(edge.target, terminal)? == 0 {
                return Ok(Some((index, column_edge)));
            }
        }
        Ok(None)
    }

    fn reverse_edge(&mut self, node: usize, index: usize) {
        let edge = &mut self.edges[node][index];
        edge.available = false;
        let (target, reverse) = (edge.target, edge.reverse);
        self.edges[target][reverse].available = true;
    }

    fn reduced_cost(&self, node: usize, edge: Edge) -> Result<i64, &'static str> {
        edge.cost
            .checked_add(self.potentials[node])
            .and_then(|value| value.checked_sub(self.potentials[edge.target]))
            .filter(|value| *value >= 0)
            .ok_or("invalid reduced assignment cost")
    }

    fn augment(&mut self) -> Result<bool, &'static str> {
        #[cfg(test)]
        SEARCHES.with(|count| count.set(count.get() + 1));
        let mut distance = vec![i64::MAX; self.edges.len()];
        let mut previous = vec![None; self.edges.len()];
        let mut queue = BinaryHeap::new();
        distance[self.source] = 0;
        queue.push(Reverse((0, self.source)));
        while let Some(Reverse((cost, node))) = queue.pop() {
            if cost != distance[node] {
                continue;
            }
            self.relax_neighbors(node, cost, &mut distance, &mut previous, &mut queue)?;
        }
        if previous[self.sink].is_none() {
            return Ok(false);
        }
        for (potential, distance) in self.potentials.iter_mut().zip(distance) {
            if distance != i64::MAX {
                *potential = potential
                    .checked_add(distance)
                    .ok_or("assignment potential overflow")?;
            }
        }
        let mut node = self.sink;
        while node != self.source {
            let (parent, index) = previous[node].ok_or("broken assignment augmenting path")?;
            self.reverse_edge(parent, index);
            node = parent;
        }
        Ok(true)
    }

    fn relax_neighbors(
        &self,
        node: usize,
        cost: i64,
        distance: &mut [i64],
        previous: &mut [Option<(usize, usize)>],
        queue: &mut BinaryHeap<Reverse<(i64, usize)>>,
    ) -> Result<(), &'static str> {
        for (index, edge) in self.edges[node]
            .iter()
            .enumerate()
            .filter(|(_, e)| e.available)
        {
            let reduced = self.reduced_cost(node, *edge)?;
            let candidate = cost
                .checked_add(reduced)
                .ok_or("assignment distance overflow")?;
            if candidate < distance[edge.target] {
                distance[edge.target] = candidate;
                previous[edge.target] = Some((node, index));
                queue.push(Reverse((candidate, edge.target)));
            }
        }
        Ok(())
    }
}
