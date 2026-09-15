//! Independent source equations supporting an additive coordinate lift.

use rumoca_ir_dae as dae;

use super::super::equalities::{AdditiveOperands, additive_operands};

pub(in crate::dae_transform) struct AdditiveValueFacts {
    rows: Vec<ValueRow>,
    incident: Vec<Vec<usize>>,
    coordinates: Vec<Option<u32>>,
}

struct ValueRow {
    residual: u32,
    operands: AdditiveOperands,
}

/// A postordered derivation over whole source payloads, independent of the
/// equation being lifted. Only its constructor can issue nodes or references.
pub(in crate::dae_transform) struct LiftedValueProof {
    nodes: Vec<ValueNode>,
    root: usize,
}

pub(in crate::dae_transform) enum ValueNode {
    Source(u32),
    Sum {
        source: u32,
        terms: Box<[(usize, bool)]>,
    },
}

impl LiftedValueProof {
    fn issue(nodes: Vec<ValueNode>, root: usize) -> Self {
        let mut needed = vec![false; nodes.len()];
        needed[root] = true;
        for index in (0..nodes.len()).rev() {
            if !needed[index] {
                continue;
            }
            let ValueNode::Sum { terms, .. } = &nodes[index] else {
                continue;
            };
            for &(child, _) in terms {
                needed[child] = true;
            }
        }
        let mut remapped = vec![0; nodes.len()];
        let mut retained = Vec::new();
        for (index, node) in nodes.into_iter().enumerate() {
            if !needed[index] {
                continue;
            }
            remapped[index] = retained.len();
            retained.push(match node {
                ValueNode::Source(source) => ValueNode::Source(source),
                ValueNode::Sum { source, terms } => ValueNode::Sum {
                    source,
                    terms: terms
                        .iter()
                        .map(|&(child, sign)| (remapped[child], sign))
                        .collect(),
                },
            });
        }
        Self {
            root: remapped[root],
            nodes: retained,
        }
    }

    pub(in crate::dae_transform) fn nodes(&self) -> &[ValueNode] {
        &self.nodes
    }

    pub(in crate::dae_transform) fn root(&self) -> usize {
        self.root
    }

    pub(super) fn sources(&self) -> impl Iterator<Item = u32> + '_ {
        self.nodes.iter().filter_map(|node| match node {
            ValueNode::Source(source) => Some(*source),
            ValueNode::Sum { .. } => None,
        })
    }
}

impl AdditiveValueFacts {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let mut facts = Self {
            rows: Vec::new(),
            incident: vec![Vec::new(); view.variable_count()],
            coordinates: whole_coordinates(view),
        };
        for owner in view.continuous_owners() {
            facts.add_owner(view, owner);
        }
        facts
    }

    fn add_owner<'dae>(&mut self, view: dae::DaeView<'dae>, owner: dae::ContinuousOwnerView<'dae>) {
        match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                self.add_row(view, equation.residual());
            }
            dae::ContinuousOwnerView::Structured { family, .. }
                if family.scalar_view()
                    == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
            {
                for residual in family.bodies().iter() {
                    self.add_row(view, residual);
                }
            }
            dae::ContinuousOwnerView::Structured { .. } => {}
        }
    }

    fn add_row<'dae>(&mut self, view: dae::DaeView<'dae>, residual: dae::ExprId<'dae>) {
        let Some(operands) = additive_operands(view, residual) else {
            return;
        };
        for &(variable, _) in &operands.variables {
            self.incident[variable as usize].push(self.rows.len());
        }
        self.rows.push(ValueRow {
            residual: residual.index(),
            operands,
        });
    }

    pub(super) fn prove<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        owner: u32,
        lifted: u32,
        definition: dae::ExprId<'dae>,
    ) -> Option<LiftedValueProof> {
        let operands = additive_operands(view, definition)?;
        let mut walk = ValueWalk {
            facts: self,
            view,
            owner,
            visiting: vec![false; view.variable_count()],
            proved: vec![None; view.variable_count()],
            nodes: Vec::new(),
        };
        walk.visiting[lifted as usize] = true;
        let root = walk.sum(definition.index(), &operands, None)?;
        Some(LiftedValueProof::issue(walk.nodes, root))
    }
}

struct ValueWalk<'facts, 'dae> {
    facts: &'facts AdditiveValueFacts,
    view: dae::DaeView<'dae>,
    owner: u32,
    visiting: Vec<bool>,
    proved: Vec<Option<usize>>,
    nodes: Vec<ValueNode>,
}

impl ValueWalk<'_, '_> {
    fn source(&mut self, expression: u32) -> usize {
        let index = self.nodes.len();
        self.nodes.push(ValueNode::Source(expression));
        index
    }

    fn variable(&mut self, variable: u32) -> Option<usize> {
        let index = variable as usize;
        if self.visiting[index] {
            return None;
        }
        if let Some(proved) = self.proved[index] {
            return Some(proved);
        }
        let coordinate = self.facts.coordinates[index]?;
        let declaration = self.view.variable(self.view.variable_id(index)?)?;
        let proof = if declaration.role() == dae::VariableRole::State {
            Some(self.source(coordinate))
        } else {
            self.visiting[index] = true;
            let proof = self.definition(variable, coordinate);
            self.visiting[index] = false;
            proof
        };
        self.proved[index] = proof;
        proof
    }

    fn definition(&mut self, variable: u32, coordinate: u32) -> Option<usize> {
        let facts = self.facts;
        for &row in &facts.incident[variable as usize] {
            let row = &facts.rows[row];
            if row.residual == self.owner {
                continue;
            }
            let mut occurrences = row
                .operands
                .variables
                .iter()
                .filter(|&&(id, _)| id == variable);
            let &(_, negated) = occurrences.next()?;
            if occurrences.next().is_some() {
                continue;
            }
            if let Some(proof) = self.sum(coordinate, &row.operands, Some((variable, negated))) {
                return Some(proof);
            }
        }
        None
    }

    fn sum(
        &mut self,
        source: u32,
        operands: &AdditiveOperands,
        solved: Option<(u32, bool)>,
    ) -> Option<usize> {
        let mut terms = Vec::new();
        let flip = solved.is_some_and(|(_, negated)| !negated);
        for &(variable, negated) in &operands.variables {
            if solved.is_some_and(|(target, _)| variable == target) {
                continue;
            }
            let coordinate = self.facts.coordinates[variable as usize]?;
            if !self.same_shape(source, coordinate) {
                return None;
            }
            terms.push((self.variable(variable)?, negated != flip));
        }
        for invariant in &operands.invariants {
            if !invariant.zero {
                terms.push((self.source(invariant.expression), invariant.negated != flip));
            }
        }
        let index = self.nodes.len();
        self.nodes.push(ValueNode::Sum {
            source,
            terms: terms.into_boxed_slice(),
        });
        Some(index)
    }

    fn same_shape(&self, left: u32, right: u32) -> bool {
        let dimensions = |expression| {
            self.view
                .expression(self.view.expression_id(expression as usize).unwrap())
                .unwrap()
                .value_type()
                .dimensions()
        };
        dimensions(left) == dimensions(right)
    }
}

fn whole_coordinates(view: dae::DaeView<'_>) -> Vec<Option<u32>> {
    let mut coordinates = vec![None; view.variable_count()];
    for index in 0..view.expression_count() {
        let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
        if expression.function_scope().is_some() || expression.binder_domain().is_some() {
            continue;
        }
        let variable = match expression.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
                state.index()
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) => {
                variable.index()
            }
            _ => continue,
        };
        coordinates[variable as usize].get_or_insert(index as u32);
    }
    coordinates
}
