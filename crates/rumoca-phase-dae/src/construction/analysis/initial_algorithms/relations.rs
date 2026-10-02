//! Initial equations that relate two discrete coordinates (MLS 3.7 §8.6).
//!
//! `pre(newActive) = pre(localActive)` in a `Modelica.StateGraph` step names
//! two initialization unknowns. Initialization solves the initial equations
//! together with the model equations, so the side it leaves open is fixed by
//! the equations around it: `InitialStep` states `active = true` with
//! `active = localActive` and `localActive = pre(newActive)`.
//!
//! The coordinates such equations join form an identity graph. Its edges are
//! the initial relations and the plain aliases of the equation section and
//! of connections (`a = b`, `a = pre(b)`), each of which equates the
//! initialization values of its two coordinates (current and `pre` storage are
//! seeded with one value, SPEC_0022 EQN-040). A component of that graph that
//! contains an initial relation is determined exactly when it contains one
//! determined coordinate: an initial definition, a value an initial algorithm
//! assigns, or a `fixed = true` start value (§8.6 adds `pre(vd) = start`). Its
//! value then determines every member, in breadth-first order from that
//! source; every other edge of the component equates two members that already
//! hold that one value, so the order has no cycle to iterate. A component with
//! no determined coordinate is underdetermined and one with two is
//! over-determined; both are refused, never settled by a guessed start value.

use std::collections::{BTreeMap, BTreeSet, VecDeque};

use super::*;

/// The shape of one initial equation over discrete coordinates.
pub(super) enum InitialEquationShape<'flat> {
    /// `m = e` or `pre(m) = e` with `e` free of discrete coordinates.
    Definition(InitialTargetRef<'flat>, &'flat Expression),
    /// `a = b` over two whole discrete coordinates (either side under `pre`).
    Relation(&'flat VarName, &'flat VarName),
}

pub(super) struct InitialRelation<'flat> {
    pub(super) row: usize,
    pub(super) lhs: &'flat VarName,
    pub(super) rhs: &'flat VarName,
    pub(super) span: Span,
}

/// Determine every coordinate of each component an initial relation joins.
pub(super) fn settle_initial_relations(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    relations: &[InitialRelation<'_>],
    definitions: &mut HashMap<VarName, InitialDiscreteValue>,
    claimed: &mut HashSet<usize>,
) -> Result<(), ToDaeError> {
    if relations.is_empty() {
        return Ok(());
    }
    let pairs = relations
        .iter()
        .map(|relation| (relation.lhs, relation.rhs))
        .chain(
            flat.equations
                .iter()
                .filter_map(|equation| plain_discrete_alias(&equation.residual, roles)),
        );
    let mut edges = BTreeMap::<VarName, BTreeSet<VarName>>::new();
    for (a, b) in pairs {
        edges.entry(a.clone()).or_default().insert(b.clone());
        edges.entry(b.clone()).or_default().insert(a.clone());
    }
    let mut settled = HashSet::<VarName>::new();
    for relation in relations {
        claimed.insert(relation.row);
        if settled.contains(relation.lhs) {
            continue;
        }
        let component = component_of(&edges, relation.lhs);
        let mut sources = component
            .iter()
            .filter_map(|member| {
                source_value(flat, definitions, member).map(|value| (member, value))
            })
            .collect::<Vec<_>>();
        let (source, value) = match sources.len() {
            1 => sources.remove(0),
            0 => {
                return Err(unsupported(
                    format!(
                        "the initial equation relating `{}` and `{}` leaves their initialization \
                         value undetermined: no coordinate it joins through initial relations and \
                         aliases has an initial definition or a fixed start value",
                        relation.lhs, relation.rhs
                    ),
                    relation.span,
                ));
            }
            _ => {
                let (first, second) = (sources[0].0, sources[1].0);
                return Err(unsupported(
                    format!(
                        "the initial equation relating `{}` and `{}` joins `{}` and `{}`, which \
                         are both determined at initialization, so the relation over-determines \
                         it",
                        relation.lhs, relation.rhs, first, second
                    ),
                    relation.span,
                ));
            }
        };
        let source = source.clone();
        for member in &component {
            settled.insert(member.clone());
            if *member != source && !definitions.contains_key(member) {
                insert_initial_definition(
                    definitions,
                    member,
                    InitialDiscreteValue {
                        value: value.value.clone(),
                        span: relation.span,
                    },
                )?;
            }
        }
    }
    Ok(())
}

/// The members of `start`'s component, in breadth-first order.
fn component_of(edges: &BTreeMap<VarName, BTreeSet<VarName>>, start: &VarName) -> Vec<VarName> {
    let mut seen = BTreeSet::from([start.clone()]);
    let mut order = Vec::new();
    let mut queue = VecDeque::from([start.clone()]);
    while let Some(member) = queue.pop_front() {
        for next in edges.get(&member).into_iter().flatten() {
            if seen.insert(next.clone()) {
                queue.push_back(next.clone());
            }
        }
        order.push(member);
    }
    order
}

/// The value a determined coordinate holds at initialization: its initial
/// definition, or its start value when it is declared `fixed = true`.
fn source_value(
    flat: &flat::Model,
    definitions: &HashMap<VarName, InitialDiscreteValue>,
    name: &VarName,
) -> Option<InitialDiscreteValue> {
    if let Some(definition) = definitions.get(name) {
        return Some(InitialDiscreteValue {
            value: definition.value.clone(),
            span: definition.span,
        });
    }
    let variable = flat.variables.get(name)?;
    (variable.fixed_uniform() == Some(true))
        .then(|| variable.start.clone())
        .flatten()
        .map(|value| InitialDiscreteValue {
            value,
            span: variable.source_span,
        })
}

/// `(a, b)` of an alias `a = b` or `a = pre(b)` over two whole discrete
/// coordinates.
fn plain_discrete_alias<'flat>(
    residual: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<(&'flat VarName, &'flat VarName)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = residual
    else {
        return None;
    };
    Some((whole_discrete(lhs, roles)?, whole_discrete(rhs, roles)?))
}

fn whole_discrete<'flat>(
    expression: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<&'flat VarName> {
    let expression = match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
            ..
        } if args.len() == 1 => &args[0],
        expression => expression,
    };
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    (subscripts.is_empty()
        && matches!(
            roles.get(name.var_name()),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ))
    .then(|| name.var_name())
}
