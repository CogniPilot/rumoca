//! Alias quotient of whole-coordinate copies and negations (SPEC_0040 STRUCT-T02).
//!
//! A continuous owner stating `a = b`, `a = -b`, or `a + b = 0` over two whole
//! continuous Real coordinates of one value type is an alias edge (see
//! [`edges`]). Edges form classes; each acyclic class with at most one anchored
//! member is quotiented onto a representative. The reconstruction reads every
//! eliminated member through `±representative` and keeps the member's
//! spanning-tree edge as its defining equation, so every declaration stays a
//! computed, observable variable and the solution set is unchanged.

mod edges;
#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use rumoca_ir_dae as dae;

use crate::StructuralError;
use edges::{AliasEdge, alias_edges};

/// How one eliminated member reads its class representative.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct AliasSubstitution {
    pub(super) representative: u32,
    pub(super) negated: bool,
}

/// The spanning-tree edge that becomes one eliminated member's definition.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct AliasDefinition {
    pub(super) member: u32,
    /// The member's own source term (`m` or `m[binders]`) in that owner.
    pub(super) access: u32,
    pub(super) body: Option<usize>,
    pub(super) residual: u32,
}

/// The accepted quotient: a substitution per eliminated declaration ordinal,
/// and the definition each eliminated member keeps, keyed by owner ordinal.
#[derive(Debug, Default)]
pub(super) struct AliasPlan {
    pub(super) substitutions: Vec<Option<AliasSubstitution>>,
    pub(super) definitions: BTreeMap<usize, AliasDefinition>,
}

impl AliasPlan {
    fn is_empty(&self) -> bool {
        self.definitions.is_empty()
    }
}

/// Quotient the copy and negation aliases of `model`.
///
/// Returns `None` when no class is eligible, leaving the source untouched.
pub fn quotient_aliases(model: &dae::Dae) -> Result<Option<dae::Dae>, StructuralError> {
    let plan = model.inspect(derive_plan);
    if plan.is_empty() {
        return Ok(None);
    }
    super::reconstruction::rebuild_alias_quotient(model, &plan).map(Some)
}

pub(super) fn derive_plan(view: dae::DaeView<'_>) -> AliasPlan {
    let members = member_facts(view);
    let edges = alias_edges(view, &members);
    let mut plan = AliasPlan {
        substitutions: vec![None; view.variable_count()],
        definitions: BTreeMap::new(),
    };
    for class in acyclic_classes(view.variable_count(), &edges) {
        quotient_class(&class, &members, &mut plan);
    }
    plan
}

/// Per-declaration eligibility and anchoring, indexed by declaration ordinal.
#[derive(Clone, Copy, Default)]
pub(super) struct MemberFacts {
    pub(super) eligible: bool,
    state: bool,
    /// `StateSelect.avoid` or `never`: a state the model prefers not to keep.
    avoided: bool,
    anchored: bool,
}

fn member_facts(view: dae::DaeView<'_>) -> Vec<MemberFacts> {
    let pre_read = pre_read_declarations(view);
    view.variables()
        .map(|(id, variable)| {
            let eligible = matches!(
                variable.role(),
                dae::VariableRole::Algebraic | dae::VariableRole::Output | dae::VariableRole::State
            ) && variable.variability() == dae::ExpressionVariability::Continuous
                && variable.value_type().scalar_type() == dae::ScalarType::Real
                && !pre_read.contains(&id.index());
            MemberFacts {
                eligible,
                state: variable.role() == dae::VariableRole::State,
                avoided: matches!(
                    variable.state_select(),
                    rumoca_core::StateSelect::Avoid | rumoca_core::StateSelect::Never
                ),
                anchored: eligible && is_anchored(view, variable),
            }
        })
        .collect()
}

/// A member whose coordinate preference or seed information a quotient
/// must keep (MLS §4.8.1, §4.8.7.1, §8.6).
fn is_anchored<'dae>(view: dae::DaeView<'dae>, variable: dae::VariableView<'dae>) -> bool {
    matches!(
        variable.state_select(),
        rumoca_core::StateSelect::Always | rumoca_core::StateSelect::Prefer
    ) || variable.fixed().is_some_and(|fixed| fixed.contains(&true))
        || variable.nominal().is_some()
        || variable
            .start()
            .is_some_and(|start| !is_zero_seed(view, start))
}

/// Whether a start expression is the neutral zero guess of its shape.
fn is_zero_seed<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> bool {
    let Some(node) = view.expression(expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => *value == 0.0,
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => *value == 0,
        dae::ExpressionOperation::Unary { operand, .. } => is_zero_seed(view, operand),
        dae::ExpressionOperation::Array(operands) => {
            operands.iter().all(|operand| is_zero_seed(view, operand))
        }
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Zeros,
            ..
        } => true,
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Fill,
            arguments,
        } => arguments
            .iter()
            .next()
            .is_some_and(|value| is_zero_seed(view, value)),
        _ => false,
    }
}

/// Declarations whose event-entry left limit is read anywhere in the model.
fn pre_read_declarations(view: dae::DaeView<'_>) -> BTreeSet<u32> {
    (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| match view.expression(id)?.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreAlgebraic(variable)) => {
                Some(variable.index())
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreState(variable)) => {
                Some(variable.index())
            }
            _ => None,
        })
        .collect()
}

/// One connected alias class with its spanning-tree edges.
struct AliasClass {
    members: Vec<u32>,
    edges: Vec<AliasEdge>,
}

/// Union the edges in owner order; a class that closes a cycle is dropped.
fn acyclic_classes(variables: usize, edges: &[AliasEdge]) -> Vec<AliasClass> {
    let mut parent = (0..variables as u32).collect::<Vec<_>>();
    let mut cyclic = Vec::new();
    for edge in edges {
        let first = find(&mut parent, edge.first.variable);
        let second = find(&mut parent, edge.second.variable);
        if first == second {
            cyclic.push(first);
        } else {
            parent[first.max(second) as usize] = first.min(second);
        }
    }
    let cyclic = cyclic
        .into_iter()
        .map(|root| find(&mut parent, root))
        .collect::<BTreeSet<_>>();
    let mut classes: BTreeMap<u32, Vec<AliasEdge>> = BTreeMap::new();
    for edge in edges {
        let root = find(&mut parent, edge.first.variable);
        if !cyclic.contains(&root) {
            classes.entry(root).or_default().push(*edge);
        }
    }
    classes
        .into_values()
        .map(|edges| AliasClass {
            members: edges
                .iter()
                .flat_map(|edge| [edge.first.variable, edge.second.variable])
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect(),
            edges,
        })
        .collect()
}

fn find(parent: &mut [u32], mut node: u32) -> u32 {
    while parent[node as usize] != node {
        let grandparent = parent[parent[node as usize] as usize];
        parent[node as usize] = grandparent;
        node = grandparent;
    }
    node
}

/// The unique anchored member, else the lowest-ordinal state not avoided by
/// `StateSelect`, else the lowest-ordinal state, else the lowest ordinal. `None` when two anchored members would force a silent choice, or
/// when the anchored member is not a state although the class has one: a
/// state's derivative can only be read through another state.
fn representative(class: &AliasClass, members: &[MemberFacts]) -> Option<u32> {
    let facts = |member: &u32| members[*member as usize];
    let mut anchored = class.members.iter().filter(|member| facts(member).anchored);
    let has_state = class.members.iter().any(|member| facts(member).state);
    match (anchored.next(), anchored.next()) {
        (Some(_), Some(_)) => None,
        (Some(anchor), None) => (facts(anchor).state || !has_state).then_some(*anchor),
        (None, _) => class
            .members
            .iter()
            .find(|member| facts(member).state && !facts(member).avoided)
            .or_else(|| class.members.iter().find(|member| facts(member).state))
            .or(class.members.first())
            .copied(),
    }
}

/// Orient one class from its representative and record each member's
/// substitution and defining owner.
fn quotient_class(class: &AliasClass, members: &[MemberFacts], plan: &mut AliasPlan) {
    let Some(representative) = representative(class, members) else {
        return;
    };
    let mut adjacency: BTreeMap<u32, Vec<(u32, &AliasEdge)>> = BTreeMap::new();
    for edge in &class.edges {
        let (first, second) = (edge.first.variable, edge.second.variable);
        adjacency.entry(first).or_default().push((second, edge));
        adjacency.entry(second).or_default().push((first, edge));
    }
    let mut negated = BTreeMap::from([(representative, false)]);
    let mut pending = VecDeque::from([representative]);
    while let Some(node) = pending.pop_front() {
        let parity = negated[&node];
        for &(next, edge) in adjacency.get(&node).into_iter().flatten() {
            if negated.contains_key(&next) {
                continue;
            }
            let next_negated = parity ^ edge.negated;
            negated.insert(next, next_negated);
            pending.push_back(next);
            if members[next as usize].state {
                // Another state keeps its role and its edge equation; members
                // beyond it still read the representative through that equation.
                continue;
            }
            plan.substitutions[next as usize] = Some(AliasSubstitution {
                representative,
                negated: next_negated,
            });
            let access = if edge.first.variable == next {
                edge.first.expression
            } else {
                edge.second.expression
            };
            plan.definitions.insert(
                edge.owner,
                AliasDefinition {
                    member: next,
                    access,
                    body: edge.body,
                    residual: edge.residual,
                },
            );
        }
    }
}
