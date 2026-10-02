//! Initial equations that relate two discrete coordinates (MLS 3.7 §8.6).
//!
//! `pre(newActive) = pre(localActive)` in `Modelica.StateGraph.Step` names two
//! initialization unknowns. When one side is determined by another initial
//! owner, the relation determines the other side from that value. When
//! neither is, the initialization problem leaves the right side free: MLS 3.7
//! §8.6 lets "a missing initial value of a discrete-time variable" take its
//! start value, so the right side is determined by its start expression and the
//! left side by the relation. A relation whose two sides are both determined
//! elsewhere is over-determined and stays refused.

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

/// Determine one side of each relation, in source order, until every relation
/// is settled.
pub(super) fn settle_initial_relations(
    flat: &flat::Model,
    mut relations: Vec<InitialRelation<'_>>,
    definitions: &mut HashMap<VarName, InitialDiscreteValue>,
    claimed: &mut HashSet<usize>,
) -> Result<(), ToDaeError> {
    while !relations.is_empty() {
        let progress = relations.iter().position(|relation| {
            relation.lhs == relation.rhs
                || definitions.contains_key(relation.lhs) != definitions.contains_key(relation.rhs)
        });
        let index = progress.unwrap_or(0);
        let relation = relations.remove(index);
        claimed.insert(relation.row);
        if relation.lhs == relation.rhs {
            continue;
        }
        let (target, source) = match (
            definitions.contains_key(relation.lhs),
            definitions.contains_key(relation.rhs),
        ) {
            (false, true) => (relation.lhs, relation.rhs),
            (true, false) => (relation.rhs, relation.lhs),
            (true, true) => {
                return Err(unsupported(
                    format!(
                        "`{}` and `{}` are both determined by other initial owners, so the \
                         initial equation relating them over-determines the initialization",
                        relation.lhs, relation.rhs
                    ),
                    relation.span,
                ));
            }
            (false, false) => {
                let start = start_definition(flat, relation.rhs, relation.span)?;
                insert_initial_definition(definitions, relation.rhs, start)?;
                (relation.lhs, relation.rhs)
            }
        };
        let value = definitions[source].value.clone();
        insert_initial_definition(
            definitions,
            target,
            InitialDiscreteValue {
                value,
                span: relation.span,
            },
        )?;
    }
    Ok(())
}

/// The start expression that determines an otherwise free discrete
/// coordinate (MLS 3.7 §8.6).
fn start_definition(
    flat: &flat::Model,
    name: &VarName,
    span: Span,
) -> Result<InitialDiscreteValue, ToDaeError> {
    let start = flat
        .variables
        .get(name)
        .and_then(|variable| variable.start.clone())
        .ok_or_else(|| {
            unsupported(
                format!(
                    "`{name}` is related to another discrete coordinate by an initial equation, \
                     but neither is determined and `{name}` has no start value"
                ),
                span,
            )
        })?;
    Ok(InitialDiscreteValue { value: start, span })
}
