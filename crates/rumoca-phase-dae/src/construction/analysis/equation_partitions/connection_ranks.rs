//! Connection distance of every discrete-valued element from its producer.
//!
//! A connection set is a set of scalar coordinates (MLS 3.7 §9.2): `connect(b,
//! and.x[1])` and `connect(a, and.x[2])` put the two elements of `and.x` in two
//! different sets, each with its own producer. The distance that orients a
//! connection equation is therefore a property of each element, not of the
//! declared array: one rank per array would let the element nearer its
//! producer decide the orientation of the other, which then reads a
//! pass-through coordinate as its source and leaves an element undefined.
//!
//! A reference selects the elements of a literal-index prefix of its
//! variable, or all of them. A selection this cannot prove (a computed
//! subscript, an unsettled extent) stands for every element of the variable,
//! the coarse reading every element shares.
//!
//! Every element of a set equals the set's producer (MLS §9.2), so an element
//! connection is defined by that producer directly. Defining it by the
//! neighbouring element the connection equation names would make two arrays
//! whose elements feed each other in opposite directions (`and.x[1]` from
//! `xor.x[1]`, `xor.x[2]` from `and.x[2]`) depend on each other as whole
//! coordinates, a cycle no element ordering has.

use super::*;

/// The connection rank of every element of every connected discrete-valued
/// variable, in row-major element order.
#[derive(Default)]
pub(in crate::construction) struct DiscreteConnectionRanks {
    elements: HashMap<VarName, Vec<Option<usize>>>,
    /// The producer element each ranked element was reached from.
    roots: HashMap<ElementNode, ElementNode>,
    /// A connection-equation reference that selects exactly one element.
    references: HashMap<ElementNode, Expression>,
}

/// Which side of a connection equation `lhs = rhs` an orientation defines.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(in crate::construction) enum ConnectionOrientation {
    DefinesLhs,
    DefinesRhs,
}

type ElementNode = (VarName, usize);

impl DiscreteConnectionRanks {
    /// The orientation every selected element pair agrees on: the element
    /// farther from its producer is defined by the nearer one. `None` when no
    /// pair is ranked apart, and `Err(())` when pairs disagree, since one
    /// equation cannot define both of its sides.
    pub(in crate::construction) fn orientation(
        &self,
        flat: &flat::Model,
        lhs: (&VarName, &[Subscript]),
        rhs: (&VarName, &[Subscript]),
    ) -> Result<Option<ConnectionOrientation>, ()> {
        let lhs = self.selected_ranks(flat, lhs.0, lhs.1);
        let rhs = self.selected_ranks(flat, rhs.0, rhs.1);
        let mut agreed = None;
        for (lhs, rhs) in pair_selections(&lhs, &rhs) {
            let orientation = match (lhs, rhs) {
                (Some(lhs), Some(rhs)) if lhs < rhs => ConnectionOrientation::DefinesRhs,
                (Some(lhs), Some(rhs)) if rhs < lhs => ConnectionOrientation::DefinesLhs,
                (Some(_), None) => ConnectionOrientation::DefinesRhs,
                (None, Some(_)) => ConnectionOrientation::DefinesLhs,
                _ => continue,
            };
            match agreed {
                Some(previous) if previous != orientation => return Err(()),
                _ => agreed = Some(orientation),
            }
        }
        Ok(agreed)
    }

    /// The producer of the one element a reference selects, as a reference
    /// a connection equation states for it, when that producer is another
    /// element.
    pub(in crate::construction) fn producer_reference(
        &self,
        flat: &flat::Model,
        name: &VarName,
        subscripts: &[Subscript],
    ) -> Option<&Expression> {
        let [ordinal] = selected_elements(flat, name, subscripts)[..] else {
            return None;
        };
        let node = (name.clone(), ordinal);
        let root = self.roots.get(&node)?;
        (root != &node).then(|| self.references.get(root)).flatten()
    }

    /// Whether no element of `name` is ranked, or every ranked one is a
    /// connection source (rank 0).
    pub(in crate::construction) fn is_source_or_unranked(&self, name: &VarName) -> bool {
        self.elements
            .get(name)
            .is_none_or(|ranks| ranks.iter().flatten().all(|rank| *rank == 0))
    }

    fn is_ranked(&self, name: &VarName) -> bool {
        self.elements
            .get(name)
            .is_some_and(|ranks| ranks.iter().any(Option::is_some))
    }

    fn selected_ranks(
        &self,
        flat: &flat::Model,
        name: &VarName,
        subscripts: &[Subscript],
    ) -> Vec<Option<usize>> {
        let Some(ranks) = self.elements.get(name) else {
            return vec![None; selected_elements(flat, name, subscripts).len()];
        };
        selected_elements(flat, name, subscripts)
            .into_iter()
            .map(|ordinal| ranks.get(ordinal).copied().flatten())
            .collect()
    }

    fn rank(&self, (name, ordinal): &ElementNode) -> Option<usize> {
        self.elements
            .get(name)
            .and_then(|ranks| ranks.get(*ordinal).copied().flatten())
    }

    fn set_rank(&mut self, flat: &flat::Model, (name, ordinal): &ElementNode, rank: usize) {
        let ranks = self
            .elements
            .entry(name.clone())
            .or_insert_with(|| vec![None; element_count(flat, name)]);
        if let Some(slot) = ranks.get_mut(*ordinal) {
            *slot = Some(rank);
        }
        if rank == 0 {
            self.roots
                .insert((name.clone(), *ordinal), (name.clone(), *ordinal));
        }
    }

    fn set_variable_rank(&mut self, flat: &flat::Model, name: &VarName, rank: usize) {
        for ordinal in 0..element_count(flat, name) {
            self.set_rank(flat, &(name.clone(), ordinal), rank);
        }
    }
}

/// Element pairs of two selections a connection equates. Equal-size
/// selections pair positionally; a selection that stands for its whole
/// variable as one node pairs with every element of the other side.
fn pair_selections<T: Copy>(lhs: &[T], rhs: &[T]) -> Vec<(T, T)> {
    if lhs.len() == rhs.len() {
        return lhs.iter().copied().zip(rhs.iter().copied()).collect();
    }
    lhs.iter()
        .flat_map(|lhs| rhs.iter().map(move |rhs| (*lhs, *rhs)))
        .collect()
}

/// The number of element nodes of a variable: the product of its settled
/// extents, or one node standing for the whole variable when an extent is
/// not settled or the variable has no elements.
fn element_count(flat: &flat::Model, name: &VarName) -> usize {
    flat.variables
        .get(name)
        .and_then(|variable| {
            variable.dims.iter().try_fold(1usize, |count, extent| {
                usize::try_from(*extent)
                    .ok()
                    .and_then(|extent| count.checked_mul(extent))
            })
        })
        .filter(|count| *count > 0)
        .unwrap_or(1)
}

/// The row-major element ordinals a reference selects.
fn selected_elements(flat: &flat::Model, name: &VarName, subscripts: &[Subscript]) -> Vec<usize> {
    let count = element_count(flat, name);
    let whole = || (0..count).collect::<Vec<_>>();
    if subscripts.is_empty() {
        return whole();
    }
    let Some(variable) = flat.variables.get(name) else {
        return whole();
    };
    if subscripts.len() > variable.dims.len() {
        return whole();
    }
    let mut ordinal = 0usize;
    let mut selected = 1usize;
    for (subscript, extent) in subscripts.iter().zip(&variable.dims) {
        let (Subscript::Index { value, .. }, Ok(extent)) = (subscript, usize::try_from(*extent))
        else {
            return whole();
        };
        let Some(index) = usize::try_from(*value)
            .ok()
            .filter(|index| (1..=extent).contains(index))
        else {
            return whole();
        };
        let Some(next) = ordinal
            .checked_mul(extent)
            .and_then(|base| base.checked_add(index - 1))
        else {
            return whole();
        };
        ordinal = next;
        selected = selected.saturating_mul(extent);
    }
    let tail = count / selected.max(1);
    (ordinal * tail..(ordinal + 1) * tail).collect()
}

/// Prove the exact discrete coordinates that already have a semantic owner
/// outside the connection graph.
///
/// An `output` connector may either produce a value or merely forward one to
/// an enclosing connector. Causality therefore cannot orient output-to-output
/// edges by itself. The source owners are the exact evidence: bindings,
/// ordinary equations, algorithms, and when chains define producers, while
/// connection equations do not. A multi-source graph walk then assigns every
/// pass-through element its minimum distance from a proven producer, so a
/// chain is oriented outward without rendered-name or model-specific rules.
/// Building the ranks once keeps connection classification linear in the model
/// size.
pub(in crate::construction) fn discrete_connection_ranks(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> DiscreteConnectionRanks {
    let mut producers = flat
        .variables
        .iter()
        .filter(|(name, variable)| {
            variable.binding.is_some() && matches!(roles[*name], PlannedRole::DiscreteValue)
        })
        .map(|(name, _)| name.clone())
        .collect::<HashSet<_>>();
    for equation in &flat.equations {
        if matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
            continue;
        }
        if let Ok(Some(plan)) = discrete_value_assignment(&equation.residual, roles, equation.span)
        {
            producers.insert(plan.target.clone());
        }
        // An element equation `x[i] = e` defines the coordinate `x` outside the
        // connection graph, so `x` is a producer that orients any connections
        // from it outward. Without this, a discrete array fed by a for-loop of
        // element equations looks source-free and a fan-out to same-causality
        // consumers cannot be oriented from the true producer.
        if let Some((target, _, _)) = discrete_element_assignment(equation, roles) {
            producers.insert(target.clone());
        }
    }
    producers.extend(event_targets(flat));
    producers.extend(algorithm_targets(flat));

    let mut ranks = DiscreteConnectionRanks::default();
    let neighbors = element_neighbors(flat, roles, &mut ranks.references);
    let mut producers = producers.into_iter().collect::<Vec<_>>();
    producers.sort_unstable();
    let mut frontier = Vec::new();
    for producer in &producers {
        ranks.set_variable_rank(flat, producer, 0);
        frontier
            .extend((0..element_count(flat, producer)).map(|ordinal| (producer.clone(), ordinal)));
    }
    spread_connection_ranks(flat, &mut ranks, frontier, &neighbors);
    // A connection set no producer reaches is fed by a plain alias `a = b`
    // whose written side already has its own definition (a binding
    // modification, as `CompositeStepState.suspend = subgraphStatePort.suspend`
    // of `Modelica.StateGraph`): the alias defines `b`, so `b` produces the
    // set (see `alias_orientation`).
    let connected = neighbors
        .keys()
        .map(|(name, _)| name.clone())
        .collect::<HashSet<_>>();
    let sources = alias_orientation::alias_fed_connection_sources(
        flat,
        roles,
        |name| ranks.is_ranked(name),
        |name| connected.contains(name),
    );
    let mut frontier = Vec::new();
    for source in &sources {
        ranks.set_variable_rank(flat, source, 0);
        frontier.extend((0..element_count(flat, source)).map(|ordinal| (source.clone(), ordinal)));
    }
    spread_connection_ranks(flat, &mut ranks, frontier, &neighbors);
    ranks
}

/// The element-level connection graph of the discrete-valued coordinates.
fn element_neighbors(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    references: &mut HashMap<ElementNode, Expression>,
) -> HashMap<ElementNode, Vec<ElementNode>> {
    let mut neighbors = HashMap::<ElementNode, Vec<ElementNode>>::new();
    for equation in &flat.equations {
        if !matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
            continue;
        }
        let Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } = &equation.residual
        else {
            continue;
        };
        for side in [lhs.as_ref(), rhs.as_ref()] {
            if let Some((name, subscripts)) = discrete_value_base_reference(side, roles)
                && flat
                    .variables
                    .get(name)
                    .is_some_and(|variable| variable.dims.len() == subscripts.len())
                && let [ordinal] = selected_elements(flat, name, subscripts)[..]
            {
                references
                    .entry((name.clone(), ordinal))
                    .or_insert_with(|| side.clone());
            }
        }
        let Some((lhs, lhs_subscripts)) = discrete_value_base_reference(lhs, roles) else {
            continue;
        };
        let Some((rhs, rhs_subscripts)) = discrete_value_base_reference(rhs, roles) else {
            continue;
        };
        let lhs_elements = selected_elements(flat, lhs, lhs_subscripts);
        let rhs_elements = selected_elements(flat, rhs, rhs_subscripts);
        for (lhs_ordinal, rhs_ordinal) in pair_selections(&lhs_elements, &rhs_elements) {
            let lhs_node = (lhs.clone(), lhs_ordinal);
            let rhs_node = (rhs.clone(), rhs_ordinal);
            neighbors
                .entry(lhs_node.clone())
                .or_default()
                .push(rhs_node.clone());
            neighbors.entry(rhs_node).or_default().push(lhs_node);
        }
    }
    neighbors
}

/// Breadth-first connection distance from `frontier` to every element its
/// connection sets reach that has no rank yet.
fn spread_connection_ranks(
    flat: &flat::Model,
    ranks: &mut DiscreteConnectionRanks,
    mut frontier: Vec<ElementNode>,
    neighbors: &HashMap<ElementNode, Vec<ElementNode>>,
) {
    let mut cursor = 0usize;
    while let Some(current) = frontier.get(cursor).cloned() {
        cursor += 1;
        let Some(rank) = ranks.rank(&current) else {
            continue;
        };
        for neighbor in neighbors.get(&current).into_iter().flatten() {
            if ranks.rank(neighbor).is_some() {
                continue;
            }
            ranks.set_rank(flat, neighbor, rank + 1);
            if let Some(root) = ranks.roots.get(&current).cloned() {
                ranks.roots.insert(neighbor.clone(), root);
            }
            frontier.push(neighbor.clone());
        }
    }
}
