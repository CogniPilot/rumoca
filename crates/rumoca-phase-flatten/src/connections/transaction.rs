//! Build-local atomic projection for Flat connection lowering.

use indexmap::IndexMap;
use rumoca_core::{InstanceId, VarName};
use rumoca_ir_flat as flat;

use super::{FlattenError, StreamConnectionSet};

/// One connection member resolved to its exact Flat declaration plus the
/// leading selection (MLS §10.5) it denotes on that declaration.
///
/// The selection is part of the owner so that connected state can only be
/// issued for the elements a member actually denotes: an owner for `c[1]`
/// cannot mark `c[2]`, and an owner for the whole declaration says so with an
/// empty selection rather than by omission.
#[derive(Clone)]
pub(super) struct ConnectionDeclarationOwner {
    key: VarName,
    instance_id: InstanceId,
    selection: Vec<i64>,
}

impl ConnectionDeclarationOwner {
    pub(super) fn from_evidence(
        evidence: &super::ConnectionDeclarationEvidence<'_>,
    ) -> Result<Self, FlattenError> {
        let declaration = evidence.declaration();
        if declaration.instance_id.is_unset() {
            return Err(FlattenError::invalid_connection_evidence(
                format!(
                    "connection declaration `{}` has no exact Flat occurrence identity",
                    evidence.base()
                ),
                declaration.source_span,
            ));
        }
        Ok(Self {
            key: evidence.base().clone(),
            instance_id: declaration.instance_id,
            selection: evidence.indices().to_vec(),
        })
    }

    #[cfg(test)]
    fn fixture(key: &str, instance_id: InstanceId, selection: Vec<i64>) -> Self {
        Self {
            key: VarName::new(key),
            instance_id,
            selection,
        }
    }
}

/// Connected elements planned for one declaration, bound to the occurrence
/// identity they were checked against.
struct PendingConnectedOwner {
    instance_id: InstanceId,
    domain: flat::ConnectedDomain,
}

pub(super) struct PlannedConnectionEquation {
    pub(super) equation: flat::Equation,
    pub(super) family: Option<flat::StructuredEquationFamily>,
}

/// Complete connection-owned Flat delta. Every fallible semantic operation
/// populates this local value; applying it only moves already-checked owners
/// and merges prevalidated connected domains.
struct ProjectionOwners {
    first_equation_index: usize,
    equations: Vec<PlannedConnectionEquation>,
    assertions: Vec<flat::AssertEquation>,
    connected: IndexMap<VarName, PendingConnectedOwner>,
    /// Selections already summed by a flow connection set, keyed by the scope
    /// that declared the set and the declaration they select. MLS §9.2 puts
    /// every flow element in at most one connection set per scope; two sets
    /// claiming overlapping selections of one declaration at one scope would
    /// count that element twice, so the overlap refuses the transaction.
    flow_claims: IndexMap<(String, VarName), Vec<Vec<i64>>>,
}

/// Open, appendable connection transaction. Consuming stream sealing is its
/// only production path to a commit-capable projection.
pub(super) struct OpenConnectionProjection {
    owners: ProjectionOwners,
}

/// Closed connection transaction. No equation, assertion, or connected-state
/// append operation is available after stream-expression rewriting.
pub(super) struct SealedConnectionProjection {
    owners: ProjectionOwners,
    stream_rewrite: super::stream_operators::StreamRewriteProjection,
}

#[cfg(test)]
pub(super) struct StreamFreeTestProjection {
    owners: ProjectionOwners,
}

impl OpenConnectionProjection {
    pub(super) fn new(flat: &flat::Model) -> Self {
        Self {
            owners: ProjectionOwners {
                first_equation_index: flat.equations.len(),
                equations: Vec::new(),
                assertions: Vec::new(),
                connected: IndexMap::new(),
                flow_claims: IndexMap::new(),
            },
        }
    }

    pub(super) fn next_equation_index(&self) -> usize {
        self.owners.first_equation_index + self.owners.equations.len()
    }

    pub(super) fn plan_equation(
        &mut self,
        equation: flat::Equation,
        preferred_dims: Option<&[i64]>,
    ) -> Result<(), FlattenError> {
        self.owners.equations.push(plan_connection_equation(
            self.next_equation_index(),
            equation,
            preferred_dims,
        )?);
        Ok(())
    }

    pub(super) fn mark_connected(
        &mut self,
        flat: &flat::Model,
        owner: &ConnectionDeclarationOwner,
    ) -> Result<(), FlattenError> {
        let declaration = flat.variables.get(&owner.key).ok_or_else(|| {
            FlattenError::internal(format!(
                "connected-state target `{}` has no exact Flat declaration owner",
                owner.key
            ))
        })?;
        if declaration.instance_id != owner.instance_id {
            return Err(FlattenError::invalid_connection_evidence(
                format!(
                    "connected-state target `{}` contradicts its exact Flat occurrence identity",
                    owner.key
                ),
                declaration.source_span,
            ));
        }
        let pending = match self.owners.connected.entry(owner.key.clone()) {
            indexmap::map::Entry::Vacant(entry) => entry.insert(PendingConnectedOwner {
                instance_id: owner.instance_id,
                domain: flat::ConnectedDomain::unconnected(),
            }),
            indexmap::map::Entry::Occupied(entry)
                if entry.get().instance_id != owner.instance_id =>
            {
                return Err(FlattenError::invalid_connection_evidence(
                    format!(
                        "connected-state target `{}` was issued with two occurrence identities",
                        owner.key
                    ),
                    declaration.source_span,
                ));
            }
            indexmap::map::Entry::Occupied(entry) => entry.into_mut(),
        };
        // The selection was proved against this declaration's dimensions when
        // the member was resolved; the domain re-checks it so that no path can
        // record elements the declaration does not have.
        pending
            .domain
            .mark(&declaration.dims, &owner.selection)
            .map_err(|reason| {
                FlattenError::invalid_connection_evidence(
                    format!(
                        "connected-state target `{}` selects outside its declaration: {reason}",
                        owner.key
                    ),
                    declaration.source_span,
                )
            })
    }

    /// Record that `owner` is summed by the flow connection set declared in
    /// `scope`, refusing when an earlier set at that scope already sums any of
    /// the same elements.
    pub(super) fn claim_flow_member(
        &mut self,
        flat: &flat::Model,
        scope: &str,
        owner: &ConnectionDeclarationOwner,
    ) -> Result<(), FlattenError> {
        let claims = self
            .owners
            .flow_claims
            .entry((scope.to_string(), owner.key.clone()))
            .or_default();
        // Two leading selections denote overlapping element blocks exactly
        // when one is a prefix of the other (MLS §10.5).
        if let Some(previous) = claims.iter().find(|previous| {
            previous.starts_with(&owner.selection) || owner.selection.starts_with(previous)
        }) {
            let span = flat
                .variables
                .get(&owner.key)
                .map(|declaration| declaration.source_span)
                .unwrap_or(rumoca_core::Span::DUMMY);
            return Err(FlattenError::invalid_connection_evidence(
                format!(
                    "flow element `{}` selected by {:?} is summed by two connection sets at scope `{scope}` (already claimed by selection {previous:?})",
                    owner.key, owner.selection
                ),
                span,
            ));
        }
        claims.push(owner.selection.clone());
        Ok(())
    }

    /// Elements of `variable` marked connected by this open transaction.
    pub(super) fn pending_domain(&self, variable: &VarName) -> Option<&flat::ConnectedDomain> {
        self.owners
            .connected
            .get(variable)
            .map(|pending| &pending.domain)
    }

    pub(super) fn push_planned_equation(&mut self, mut equation: PlannedConnectionEquation) {
        if let Some(family) = equation.family.as_mut() {
            family.first_equation_index = self.next_equation_index();
        }
        self.owners.equations.push(equation);
    }

    pub(super) fn extend_assertions(
        &mut self,
        assertions: impl IntoIterator<Item = flat::AssertEquation>,
    ) {
        self.owners.assertions.extend(assertions);
    }

    pub(super) fn seal_stream_rewrite(
        mut self,
        flat: &flat::Model,
        stream_sets: &[StreamConnectionSet],
        endpoints: &super::stream_operators::StreamConnectionEndpoints,
        operator_identities: super::stream_operators::StreamOperatorIdentities,
    ) -> Result<SealedConnectionProjection, FlattenError> {
        let stream_rewrite = super::stream_operators::plan_stream_operator_rewrite(
            flat,
            stream_sets,
            endpoints,
            operator_identities,
            &mut self.owners.equations,
            &mut self.owners.assertions,
        )?;
        Ok(SealedConnectionProjection {
            owners: self.owners,
            stream_rewrite,
        })
    }

    #[cfg(test)]
    pub(super) fn seal_without_stream(self) -> StreamFreeTestProjection {
        StreamFreeTestProjection {
            owners: self.owners,
        }
    }
}

impl SealedConnectionProjection {
    pub(super) fn commit(self, flat: &mut flat::Model) -> Result<(), FlattenError> {
        self.owners.validate_commit_invariants(flat)?;
        let validated_stream = self.stream_rewrite.validate(flat)?;
        validated_stream.apply(flat);
        commit_owners(flat, self.owners);
        Ok(())
    }
}

#[cfg(test)]
impl StreamFreeTestProjection {
    pub(super) fn commit(self, flat: &mut flat::Model) -> Result<(), FlattenError> {
        self.owners.validate_commit_invariants(flat)?;
        commit_owners(flat, self.owners);
        Ok(())
    }
}

impl ProjectionOwners {
    fn validate_commit_invariants(&self, flat: &flat::Model) -> Result<(), FlattenError> {
        if flat.equations.len() != self.first_equation_index {
            return Err(changed_connection_owner("equation insertion point"));
        }
        for (offset, planned) in self.equations.iter().enumerate() {
            if let Some(family) = &planned.family
                && family.first_equation_index != self.first_equation_index + offset
            {
                return Err(changed_connection_owner("structured equation owner"));
            }
        }
        for (variable, pending) in &self.connected {
            let declaration = flat
                .variables
                .get(variable)
                .ok_or_else(|| changed_connection_owner("connected declaration key"))?;
            if declaration.instance_id != pending.instance_id {
                return Err(changed_connection_owner(
                    "connected declaration occurrence identity",
                ));
            }
            // The pending domain was marked against the dimensions the
            // declaration had when its members were resolved; a reshaped
            // declaration would make those selections meaningless.
            if pending.domain.coverage(&declaration.dims).is_err() {
                return Err(changed_connection_owner("connected declaration shape"));
            }
        }
        Ok(())
    }
}

fn changed_connection_owner(description: &'static str) -> FlattenError {
    FlattenError::internal(format!(
        "a completed connection projection no longer owns its exact {description}"
    ))
}

fn commit_owners(flat: &mut flat::Model, owners: ProjectionOwners) {
    for planned in owners.equations {
        commit_connection_equation(flat, planned);
    }
    flat.assert_equations.extend(owners.assertions);
    for (variable, pending) in owners.connected {
        let declaration = flat
            .variables
            .get_mut(&variable)
            .expect("a planned connected-state target retains its exact Flat owner");
        assert_eq!(
            declaration.instance_id, pending.instance_id,
            "a planned connected-state target retains its exact occurrence identity"
        );
        declaration.connected.union_with(&pending.domain);
    }
}

pub(super) fn plan_connection_equation(
    equation_index: usize,
    equation: flat::Equation,
    preferred_dims: Option<&[i64]>,
) -> Result<PlannedConnectionEquation, FlattenError> {
    let family = crate::equations::array_family::structured_array_equation_family(
        equation_index,
        &equation,
        preferred_dims,
    )?;
    Ok(PlannedConnectionEquation { equation, family })
}

fn commit_connection_equation(flat: &mut flat::Model, planned: PlannedConnectionEquation) {
    flat.add_equation(planned.equation);
    if let Some(family) = planned.family {
        flat.structured_equations.push(family);
    }
}

/// Exact snapshot of every Flat owner the connection transaction may mutate.
///
/// This test oracle deliberately uses typed structural equality rather than
/// Debug, serialization, or hashing, any of which can omit semantic fields.
#[cfg(test)]
#[derive(Clone, Debug, PartialEq)]
pub(super) struct ConnectionMutationSnapshot {
    variables: flat::VarNameIndexMap<flat::Variable>,
    equations: Vec<flat::Equation>,
    structured_equations: Vec<flat::StructuredEquationFamily>,
    assertions: Vec<flat::AssertEquation>,
    initial_equations: Vec<flat::Equation>,
    initial_structured_equations: Vec<flat::StructuredEquationFamily>,
    initial_assertions: Vec<flat::AssertEquation>,
    algorithms: Vec<flat::Algorithm>,
    initial_algorithms: Vec<flat::Algorithm>,
    when_chains: Vec<flat::WhenChain>,
}

#[cfg(test)]
pub(super) fn connection_mutation_snapshot(flat: &flat::Model) -> ConnectionMutationSnapshot {
    ConnectionMutationSnapshot {
        variables: flat.variables.clone(),
        equations: flat.equations.clone(),
        structured_equations: flat.structured_equations.clone(),
        assertions: flat.assert_equations.clone(),
        initial_equations: flat.initial_equations.clone(),
        initial_structured_equations: flat.initial_structured_equations.clone(),
        initial_assertions: flat.initial_assert_equations.clone(),
        algorithms: flat.algorithms.clone(),
        initial_algorithms: flat.initial_algorithms.clone(),
        when_chains: flat.when_chains.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn model_with_owner(instance_id: InstanceId) -> flat::Model {
        let mut model = flat::Model::new();
        let key = VarName::new("x");
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("connection_transaction_test.mo"),
            1,
            2,
        );
        model.add_variable(
            key.clone(),
            flat::Variable {
                name: key,
                instance_id,
                ..flat::Variable::empty_with_span(span)
            },
        );
        model
    }

    #[test]
    fn connected_owner_refuses_same_spelling_with_a_colliding_occurrence() {
        let model = model_with_owner(InstanceId::new(1));
        let mut projection = OpenConnectionProjection::new(&model);
        let error = projection
            .mark_connected(
                &model,
                &ConnectionDeclarationOwner::fixture("x", InstanceId::new(2), Vec::new()),
            )
            .expect_err("rendered spelling cannot override exact occurrence identity");
        assert!(error.to_string().contains("occurrence identity"));
    }

    #[test]
    fn connected_owner_refuses_a_rendered_selection_without_an_exact_key() {
        let model = model_with_owner(InstanceId::new(1));
        let mut projection = OpenConnectionProjection::new(&model);
        let error = projection
            .mark_connected(
                &model,
                &ConnectionDeclarationOwner::fixture("x[1]", InstanceId::new(1), Vec::new()),
            )
            .expect_err("commit ownership cannot be recovered from rendered selection text");
        assert!(
            error
                .to_string()
                .contains("no exact Flat declaration owner")
        );
    }

    #[test]
    fn failed_stream_seal_cannot_mutate_flat_or_return_an_open_projection() {
        let mut model = flat::Model::new();
        model.add_equation(flat::Equation::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::new("inStream"),
                args: Vec::new(),
                is_constructor: false,
                call_kind: rumoca_core::FunctionCallKind::Invocation,
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            flat::EquationOrigin::ComponentEquation {
                component: "invalid_stream_identity".to_string(),
            },
        ));
        let endpoints = super::super::stream_operators::build_stream_connection_endpoints(
            &model,
            &[],
            &super::super::equation_generation::InterfaceStreamEndpointsByScope::default(),
        )
        .expect("an empty stream topology is valid");
        let projection = OpenConnectionProjection::new(&model);
        let before = connection_mutation_snapshot(&model);

        let error = match projection.seal_stream_rewrite(
            &model,
            &[],
            &endpoints,
            super::super::stream_operators::StreamOperatorIdentities::fixture(),
        ) {
            Err(error) => error,
            Ok(_) => panic!("an apparent builtin without Resolve identity must refuse sealing"),
        };

        assert!(error.to_string().contains("lacks exact Resolve target"));
        assert_eq!(connection_mutation_snapshot(&model), before);
    }

    #[test]
    fn commit_cannot_silently_drop_a_removed_connected_owner() {
        let mut model = model_with_owner(InstanceId::new(1));
        let mut projection = OpenConnectionProjection::new(&model);
        projection
            .mark_connected(
                &model,
                &ConnectionDeclarationOwner::fixture("x", InstanceId::new(1), Vec::new()),
            )
            .unwrap();
        let projection = projection.seal_without_stream();
        model.variables.clear();
        let before = connection_mutation_snapshot(&model);
        let result = projection.commit(&mut model);
        assert!(result.is_err());
        assert_eq!(connection_mutation_snapshot(&model), before);
    }

    #[test]
    fn commit_cannot_retarget_connected_state_to_a_colliding_occurrence() {
        let mut model = model_with_owner(InstanceId::new(1));
        let mut projection = OpenConnectionProjection::new(&model);
        projection
            .mark_connected(
                &model,
                &ConnectionDeclarationOwner::fixture("x", InstanceId::new(1), Vec::new()),
            )
            .unwrap();
        let projection = projection.seal_without_stream();
        model
            .variables
            .get_mut(&VarName::new("x"))
            .unwrap()
            .instance_id = InstanceId::new(2);
        let before = connection_mutation_snapshot(&model);
        let result = projection.commit(&mut model);
        assert!(result.is_err());
        assert_eq!(connection_mutation_snapshot(&model), before);
    }
}
