//! Sparse unit-capacity assignment by reduced-cost augmenting paths.

use std::cmp::Reverse;
use std::collections::BinaryHeap;

use super::SignatureEntry;

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
    for _ in 0..rows.len().min(columns) {
        if !graph.augment()? {
            break;
        }
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

    fn augment(&mut self) -> Result<bool, &'static str> {
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
            let edge = &mut self.edges[parent][index];
            edge.available = false;
            let reverse = edge.reverse;
            self.edges[node][reverse].available = true;
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
            let reduced = edge
                .cost
                .checked_add(self.potentials[node])
                .and_then(|value| value.checked_sub(self.potentials[edge.target]))
                .filter(|value| *value >= 0)
                .ok_or("invalid reduced assignment cost")?;
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
