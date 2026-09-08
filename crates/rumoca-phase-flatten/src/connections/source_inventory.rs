//! Closure-local consumption of canonical Instance connection sources.

use std::num::NonZeroUsize;

use indexmap::IndexSet;
use rumoca_core::{InstanceId, Span};
use rumoca_ir_ast as ast;

use super::{FlattenError, connection_involves_disabled, redirect_connection_for_inner_outer};

/// Build-local identity of one canonical Instance connection source.
///
/// The pair is structural and never enters Flat or a global allocator. A
/// family remains one source owner; its scalar compatibility projections carry
/// only a domain ordinal below this owner.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct ConnectionSourcePosition {
    pub(super) class_occurrence: InstanceId,
    pub(super) source_slot: NonZeroUsize,
}

#[derive(Clone)]
pub(super) struct PlannedScalarConnection {
    pub(super) source: ConnectionSourcePosition,
    pub(super) domain_ordinal: Option<usize>,
    pub(super) connection: ast::InstanceScalarConnection,
}

/// Exact build-local identity retained by topology admission.
///
/// This value is deliberately not a Flat/global ID. It is the coordinate of
/// one scalar compatibility projection within its canonical Instance source.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct ConnectionScalarSource {
    pub(super) source: ConnectionSourcePosition,
    pub(super) domain_ordinal: Option<usize>,
}

#[derive(Clone, Copy)]
pub(super) struct ConnectionTopologyInput<'a> {
    pub(super) source: ConnectionScalarSource,
    pub(super) connection: &'a ast::InstanceScalarConnection,
}

#[derive(Clone)]
struct PlannedConnectionSource {
    position: ConnectionSourcePosition,
    projected_member_count: usize,
    is_family: bool,
    active: bool,
    span: Span,
    members: Vec<PlannedScalarConnection>,
}

struct PrunedConnectionSource {
    class_occurrence: InstanceId,
    zero_based_slot: usize,
    projected_member_count: usize,
    is_family: bool,
    active: bool,
    span: Span,
    members: Vec<ast::InstanceScalarConnection>,
}

/// Active scalar views after false-conditional pruning but before canonical
/// source positions are issued. Expandable augmentation must accept or refuse
/// this closed candidate set before [`Self::plan`] can identify source owners.
pub(super) struct PrunedConnectionSources {
    sources: Vec<PrunedConnectionSource>,
}

#[derive(Clone)]
pub(super) struct ConnectionSourceInventory {
    source_count: usize,
    sources: Vec<PlannedConnectionSource>,
}

pub(super) struct ClosedConnectionSources {
    sources: Vec<PlannedConnectionSource>,
}

/// Dense consume-once proof owned only by one connection transaction.
///
/// The actual topology admission path records every exact scalar source. A
/// successful close therefore proves that topology neither skipped nor
/// replayed a source-domain member; merely revalidating the inventory cannot
/// establish that property.
pub(super) struct ConnectionSourceConsumption {
    dense_index: rustc_hash::FxHashMap<ConnectionScalarSource, usize>,
    consumed: Vec<u8>,
    span: Option<Span>,
}

impl ConnectionSourceInventory {
    pub(super) fn close(self) -> Result<ClosedConnectionSources, FlattenError> {
        validate_source_coverage(self.source_count, &self.sources)?;
        Ok(ClosedConnectionSources {
            sources: self.sources,
        })
    }
}

impl PrunedConnectionSources {
    fn connections(&self) -> Vec<&ast::InstanceScalarConnection> {
        self.sources
            .iter()
            .flat_map(|source| source.members.iter())
            .collect()
    }

    /// Issue canonical source positions only through successful expandable
    /// augmentation preflight. No sibling module can bypass this boundary.
    pub(super) fn plan_after_expandable_check(
        self,
        flat: &rumoca_ir_flat::Model,
        endpoint_index: &super::ConnectionEndpointIndex,
        prefix_children: &rustc_hash::FxHashMap<String, Vec<rumoca_core::VarName>>,
        var_index: &super::ConnectionVarIndex,
    ) -> Result<ConnectionSourceInventory, FlattenError> {
        let connections = self.connections();
        endpoint_index.check_connection_endpoint_subscripts(&connections)?;
        super::reject_expandable_connector_augmentation(
            &connections,
            flat,
            endpoint_index,
            prefix_children,
            var_index,
        )?;
        self.plan()
    }

    fn plan(self) -> Result<ConnectionSourceInventory, FlattenError> {
        let source_count = self.sources.len();
        let mut seen = rustc_hash::FxHashSet::default();
        let mut sources = Vec::with_capacity(source_count);
        for source in self.sources {
            let source_slot = source
                .zero_based_slot
                .checked_add(1)
                .and_then(NonZeroUsize::new)
                .ok_or_else(|| {
                    FlattenError::invalid_connection_evidence(
                        "a canonical connection source slot exceeds the host index range",
                        source.span,
                    )
                })?;
            let position = ConnectionSourcePosition {
                class_occurrence: source.class_occurrence,
                source_slot,
            };
            if !seen.insert(position) {
                return Err(FlattenError::invalid_connection_evidence(
                    "two canonical connection sources have the same class occurrence and vector slot",
                    source.span,
                ));
            }
            let members = source
                .members
                .into_iter()
                .enumerate()
                .map(|(ordinal, connection)| PlannedScalarConnection {
                    source: position,
                    domain_ordinal: source.is_family.then_some(ordinal),
                    connection,
                })
                .collect();
            sources.push(PlannedConnectionSource {
                position,
                projected_member_count: source.projected_member_count,
                is_family: source.is_family,
                active: source.active,
                span: source.span,
                members,
            });
        }
        Ok(ConnectionSourceInventory {
            source_count,
            sources,
        })
    }
}

impl ClosedConnectionSources {
    #[cfg(feature = "tracing")]
    pub(super) fn source_count(&self) -> usize {
        self.sources.len()
    }

    pub(super) fn topology_inputs(&self) -> Vec<ConnectionTopologyInput<'_>> {
        self.sources
            .iter()
            .flat_map(|source| source.members.iter())
            .map(|member| ConnectionTopologyInput {
                source: ConnectionScalarSource {
                    source: member.source,
                    domain_ordinal: member.domain_ordinal,
                },
                connection: &member.connection,
            })
            .collect()
    }

    pub(super) fn consumption(&self) -> Result<ConnectionSourceConsumption, FlattenError> {
        ConnectionSourceConsumption::new(self)
    }
}

impl ConnectionSourceConsumption {
    fn new(sources: &ClosedConnectionSources) -> Result<Self, FlattenError> {
        let inputs = sources.topology_inputs();
        let mut dense_index = rustc_hash::FxHashMap::with_capacity_and_hasher(
            inputs.len(),
            rustc_hash::FxBuildHasher,
        );
        for input in inputs {
            let dense = dense_index.len();
            if dense_index.insert(input.source, dense).is_some() {
                return Err(FlattenError::invalid_connection_evidence(
                    "the closed connection plan issued one scalar source-domain member twice",
                    input.connection.span(),
                ));
            }
        }
        Ok(Self {
            consumed: vec![0; dense_index.len()],
            dense_index,
            span: sources
                .sources
                .iter()
                .map(|source| source.span)
                .find(|span| !span.is_dummy()),
        })
    }

    /// Called by the exact loop that admits a source member to topology.
    pub(super) fn admit(
        &mut self,
        source: ConnectionScalarSource,
        span: Span,
    ) -> Result<(), FlattenError> {
        let dense = self.dense_index.get(&source).copied().ok_or_else(|| {
            FlattenError::invalid_connection_evidence(
                "topology admitted a scalar member not issued by the closed source plan",
                span,
            )
        })?;
        let count = self
            .consumed
            .get_mut(dense)
            .expect("dense source index was issued with its count slot");
        *count = count.checked_add(1).ok_or_else(|| {
            FlattenError::invalid_connection_evidence(
                "topology admission count overflowed for one scalar source-domain member",
                span,
            )
        })?;
        if *count != 1 {
            return Err(FlattenError::invalid_connection_evidence(
                "topology consumed one scalar source-domain member more than once",
                span,
            ));
        }
        Ok(())
    }

    /// Close only after topology and its dependent semantic plans are complete.
    pub(super) fn finish(self) -> Result<(), FlattenError> {
        if self.consumed.iter().all(|count| *count == 1) {
            return Ok(());
        }
        let omitted = self.consumed.iter().filter(|count| **count == 0).count();
        let reason = format!(
            "topology omitted {omitted} scalar source-domain member(s) from the closed connection plan"
        );
        match self.span {
            Some(span) => Err(FlattenError::invalid_connection_evidence(reason, span)),
            None => Err(FlattenError::missing_source_context(reason)),
        }
    }
}

/// Consume every canonical source vector exactly once into closure-local
/// planning state. Disabled scalar sources are consumed without projection;
/// zero-domain families are vacuous. A partly active family is refused until
/// Instance can retain its active subdomain compactly.
pub(super) fn prune_connection_sources(
    overlay: &ast::InstanceOverlay,
) -> Result<PrunedConnectionSources, FlattenError> {
    let mut sources = Vec::new();
    for class_data in overlay.classes.values() {
        if class_data.instance_id.is_unset() && !class_data.connections.is_empty() {
            return Err(FlattenError::invalid_connection_evidence(
                "a class owning connect sources lacks exact occurrence identity",
                instance_connection_span(&class_data.connections[0]),
            ));
        }
        for (zero_based_slot, source) in class_data.connections.iter().enumerate() {
            sources.push(prune_connection_source(
                overlay,
                class_data.instance_id,
                zero_based_slot,
                source,
            )?);
        }
    }
    Ok(PrunedConnectionSources { sources })
}

fn prune_connection_source(
    overlay: &ast::InstanceOverlay,
    class_occurrence: InstanceId,
    zero_based_slot: usize,
    source: &ast::InstanceConnection,
) -> Result<PrunedConnectionSource, FlattenError> {
    let mut projected = Vec::new();
    for member in rumoca_eval_ast::connection::scalar_connection_view(std::slice::from_ref(source))
    {
        projected.push(member.map_err(crate::structured_connection_error)?);
    }
    let active = projected
        .iter()
        .map(|member| !connection_involves_disabled(member, &overlay.disabled_components))
        .collect::<Vec<_>>();
    if source.as_family().is_some()
        && active.iter().any(|value| *value)
        && active.iter().any(|value| !*value)
    {
        return Err(FlattenError::invalid_connection_evidence(
            "a connection family has a partially active domain but no compact checked active-subdomain owner",
            instance_connection_span(source),
        ));
    }
    let source_active = active.iter().any(|value| *value);
    let projected_member_count = projected.len();
    let mut members = Vec::with_capacity(projected_member_count);
    for (member, active) in projected.into_iter().zip(active) {
        if !active {
            continue;
        }
        let redirected = redirect_connection_for_inner_outer(&member, overlay)
            .map_err(|error| crate::structured_connection_error(error.to_string()))?;
        members.push(redirected);
    }
    Ok(PrunedConnectionSource {
        class_occurrence,
        zero_based_slot,
        projected_member_count,
        is_family: source.as_family().is_some(),
        active: source_active,
        span: instance_connection_span(source),
        members,
    })
}

fn validate_source_coverage(
    source_count: usize,
    sources: &[PlannedConnectionSource],
) -> Result<(), FlattenError> {
    if sources.len() != source_count {
        let reason = "the closed connection plan omitted or duplicated a canonical source owner";
        return sources.first().map_or_else(
            || Err(FlattenError::internal(reason)),
            |source| {
                Err(FlattenError::invalid_connection_evidence(
                    reason,
                    source.span,
                ))
            },
        );
    }
    let mut positions = IndexSet::new();
    for source in sources {
        if !positions.insert(source.position) {
            return Err(FlattenError::invalid_connection_evidence(
                "the closed connection plan consumed one canonical source owner more than once",
                source.span,
            ));
        }
        let expected_members = if source.active {
            source.projected_member_count
        } else {
            0
        };
        if source.members.len() != expected_members {
            return Err(FlattenError::invalid_connection_evidence(
                "the closed connection plan omitted or duplicated a scalar source-domain member",
                source.span,
            ));
        }
        for (ordinal, member) in source.members.iter().enumerate() {
            let expected_ordinal = source.is_family.then_some(ordinal);
            if member.source != source.position || member.domain_ordinal != expected_ordinal {
                return Err(FlattenError::invalid_connection_evidence(
                    "the closed connection plan reassigned a scalar member to another source/domain owner",
                    source.span,
                ));
            }
        }
    }
    Ok(())
}

fn instance_connection_span(source: &ast::InstanceConnection) -> Span {
    match source {
        ast::InstanceConnection::Scalar(connection) => connection.span(),
        ast::InstanceConnection::Family(family) => family.span(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("connection_source_coverage_test.mo"),
            1,
            2,
        )
    }

    fn inventory() -> ConnectionSourceInventory {
        let position = ConnectionSourcePosition {
            class_occurrence: InstanceId::new(1),
            source_slot: NonZeroUsize::new(1).unwrap(),
        };
        let connection = ast::InstanceScalarConnection::new(
            ast::QualifiedName::from_ident("a"),
            ast::QualifiedName::from_ident("b"),
            None,
            span(),
            String::new(),
        )
        .expect("fixture scalar connection is valid");
        ConnectionSourceInventory {
            source_count: 1,
            sources: vec![PlannedConnectionSource {
                position,
                projected_member_count: 1,
                is_family: false,
                active: true,
                span: span(),
                members: vec![PlannedScalarConnection {
                    source: position,
                    domain_ordinal: None,
                    connection,
                }],
            }],
        }
    }

    #[test]
    fn topology_closure_refuses_a_skipped_admission_callback() {
        let closed = inventory().close().expect("fixture inventory closes");
        let consumption = closed.consumption().expect("fixture consumption opens");
        let Err(error) = consumption.finish() else {
            panic!("topology must admit every active scalar source member");
        };
        assert!(error.to_string().contains("topology omitted"));
    }

    #[test]
    fn topology_refuses_a_duplicated_admission_callback() {
        let closed = inventory().close().expect("fixture inventory closes");
        let input = closed.topology_inputs()[0];
        let mut consumption = closed.consumption().expect("fixture consumption opens");
        consumption
            .admit(input.source, input.connection.span())
            .expect("first topology admission succeeds");
        let Err(error) = consumption.admit(input.source, input.connection.span()) else {
            panic!("topology cannot admit a scalar source member twice");
        };
        assert!(error.to_string().contains("more than once"));
    }

    #[test]
    fn closing_refuses_a_duplicated_canonical_source_owner() {
        let mut inventory = inventory();
        inventory.source_count = 2;
        inventory.sources.push(inventory.sources[0].clone());
        let Err(error) = inventory.close() else {
            panic!("a canonical source position cannot be consumed twice");
        };
        assert!(error.to_string().contains("more than once"));
    }
}
