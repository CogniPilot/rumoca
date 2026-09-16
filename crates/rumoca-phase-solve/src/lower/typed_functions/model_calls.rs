//! Shared registration of nested calls and their assertion ownership.

use super::{PureCallRegistry, RegisteredAssertion, RegisteredCall};
use crate::lower::call_scoped_actions::CallAssertionProjection;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use std::{
    collections::{HashMap, HashSet},
    ops::Range,
};

#[derive(Clone)]
pub(super) struct RegisteredExpressionAssertion<'dae, Scope> {
    pub(super) assertion: RegisteredAssertion<'dae>,
    pub(super) projection: CallAssertionProjection,
    pub(super) scope: Scope,
}

type RegisteredExpressionCalls<'dae, Scope> = (
    HashMap<dae::ExprId<'dae>, RegisteredCall<'dae>>,
    HashMap<dae::ExprId<'dae>, Range<usize>>,
    Vec<RegisteredExpressionAssertion<'dae, Scope>>,
);

impl<'dae> PureCallRegistry<'dae> {
    // SPEC_0021: Exception - exhaustive expression-tree walk for nested call ownership.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn register_expression_calls<Scope: Copy + Eq>(
        &mut self,
        view: dae::DaeView<'dae>,
        expressions: impl IntoIterator<Item = (dae::ExprId<'dae>, Scope)>,
    ) -> Result<RegisteredExpressionCalls<'dae, Scope>, solve::SolveProgramConstructionError> {
        let mut roots = Vec::new();
        let mut seen = HashMap::new();
        let mut conflicts = HashSet::new();
        for (expression, scope) in expressions {
            dae::for_each_expression(view, expression, |projection, node| {
                let dae::ExpressionOperation::Call { owner, .. } = node.operation() else {
                    return;
                };
                match seen.insert(owner, scope) {
                    None => roots.push((owner, projection, scope)),
                    Some(previous) if previous != scope => {
                        conflicts.insert(owner);
                    }
                    Some(_) => {}
                }
            });
        }
        for (_, expression, scope) in &roots {
            let dae::ExpressionOperation::Call { owner, .. } = view
                .expression(*expression)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .operation()
            else {
                return Err(solve::SolveProgramConstructionError::WireMismatch);
            };
            if conflicts.contains(&owner) || seen.get(&owner) != Some(scope) {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: view
                        .expression(*expression)
                        .expect("checked call projection resolves")
                        .provenance()
                        .span(),
                });
            }
        }
        let mut callees = HashMap::new();
        let mut predicate_ranges = HashMap::new();
        let mut predicate_count = 0usize;
        let mut assertions = Vec::new();
        for (owner, projection, scope) in roots {
            let registered = self.register_root(view, projection)?;
            let end = predicate_count
                .checked_add(registered.assertion_count)
                .ok_or(solve::SolveProgramConstructionError::IdentityOverflow {
                    provenance: view
                        .expression(projection)
                        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                        .provenance()
                        .span(),
                })?;
            predicate_ranges.insert(owner, predicate_count..end);
            predicate_count = end;
            assertions.extend(registered.assertions.iter().cloned().enumerate().map(
                |(output_offset, assertion)| RegisteredExpressionAssertion {
                    assertion,
                    projection: CallAssertionProjection {
                        owner: registered.owner,
                        output_offset,
                    },
                    scope,
                },
            ));
            callees.insert(owner, registered);
        }
        Ok((callees, predicate_ranges, assertions))
    }
}
