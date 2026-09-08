use super::FastIndexMap;
use rumoca_core::{ConnectionGraphOperatorRole, DefId};

#[derive(Debug, Clone)]
pub struct ConnectionOperatorCatalog {
    branch: DefId,
    root: DefId,
    potential_root: DefId,
    is_root: DefId,
    rooted: DefId,
}

/// Exact ExternalObject identities accumulated by Resolve's single lifecycle
/// traversal. The owner index prevents final projection assembly from
/// introducing a second identity relation or repeating the lifecycle query.
/// The mechanically public cross-crate issuer remains SPEC_0036 migration
/// debt; architecture tests pin its production callsites to Resolve.
#[derive(Debug, Clone)]
pub struct ExternalObjectLifecycleCatalog {
    by_owner: FastIndexMap<DefId, ExternalObjectLifecycleIdentity>,
}

impl ExternalObjectLifecycleCatalog {
    #[doc(hidden)]
    pub fn begin_resolve_check() -> Self {
        Self {
            by_owner: FastIndexMap::default(),
        }
    }

    #[doc(hidden)]
    pub fn insert_from_resolve_check(
        &mut self,
        identity: ExternalObjectLifecycleIdentity,
    ) -> Result<(), DefId> {
        let owner = identity.owner();
        if self.by_owner.contains_key(&owner) {
            return Err(owner);
        }
        self.by_owner.insert(owner, identity);
        Ok(())
    }

    fn get(&self, owner: DefId) -> Option<ExternalObjectLifecycleIdentity> {
        self.by_owner.get(&owner).copied()
    }
}

impl ConnectionOperatorCatalog {
    #[doc(hidden)]
    pub fn from_resolve_registration(
        mut register: impl FnMut(ConnectionGraphOperatorRole) -> DefId,
    ) -> Self {
        Self {
            branch: register(ConnectionGraphOperatorRole::Branch),
            root: register(ConnectionGraphOperatorRole::Root),
            potential_root: register(ConnectionGraphOperatorRole::PotentialRoot),
            is_root: register(ConnectionGraphOperatorRole::IsRoot),
            rooted: register(ConnectionGraphOperatorRole::Rooted),
        }
    }

    pub fn role(&self, declaration: DefId) -> Option<ConnectionGraphOperatorRole> {
        if declaration == self.branch {
            Some(ConnectionGraphOperatorRole::Branch)
        } else if declaration == self.root {
            Some(ConnectionGraphOperatorRole::Root)
        } else if declaration == self.potential_root {
            Some(ConnectionGraphOperatorRole::PotentialRoot)
        } else if declaration == self.is_root {
            Some(ConnectionGraphOperatorRole::IsRoot)
        } else if declaration == self.rooted {
            Some(ConnectionGraphOperatorRole::Rooted)
        } else {
            None
        }
    }

    pub fn declaration(&self, role: ConnectionGraphOperatorRole) -> DefId {
        match role {
            ConnectionGraphOperatorRole::Branch => self.branch,
            ConnectionGraphOperatorRole::Root => self.root,
            ConnectionGraphOperatorRole::PotentialRoot => self.potential_root,
            ConnectionGraphOperatorRole::IsRoot => self.is_root,
            ConnectionGraphOperatorRole::Rooted => self.rooted,
        }
    }
}

/// Tree-independent exact ExternalObject lifecycle projection.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExternalObjectLifecycleIdentity {
    owner: DefId,
    constructor: DefId,
    destructor: DefId,
}

impl ExternalObjectLifecycleIdentity {
    #[doc(hidden)]
    pub fn from_resolve_check(owner: DefId, constructor: DefId, destructor: DefId) -> Self {
        Self {
            owner,
            constructor,
            destructor,
        }
    }

    pub fn owner(self) -> DefId {
        self.owner
    }

    pub fn constructor(self) -> DefId {
        self.constructor
    }

    pub fn destructor(self) -> DefId {
        self.destructor
    }
}

/// Detached exact-identity projection from a resolved class graph.
///
/// This projection does not prove that Resolve's MLS semantic checks passed;
/// only `rumoca_phase_resolve::ResolvedSemanticCatalogs` brands that fact.
/// Its mechanically public cross-crate assembly remains SPEC_0036 migration
/// debt; architecture tests pin production assembly to successful Resolve.
#[derive(Debug, Clone)]
pub struct SemanticCatalogProjection {
    connections: ConnectionOperatorCatalog,
    external_objects: ExternalObjectLifecycleCatalog,
}

impl SemanticCatalogProjection {
    #[doc(hidden)]
    pub fn from_resolve_issued(
        connections: ConnectionOperatorCatalog,
        external_objects: ExternalObjectLifecycleCatalog,
    ) -> Self {
        Self {
            connections,
            external_objects,
        }
    }

    pub fn connections(&self) -> &ConnectionOperatorCatalog {
        &self.connections
    }

    pub fn external_object(&self, owner: DefId) -> Option<ExternalObjectLifecycleIdentity> {
        self.external_objects.get(owner)
    }
}

#[cfg(test)]
pub(crate) fn test_semantic_catalog_projection() -> SemanticCatalogProjection {
    SemanticCatalogProjection::from_resolve_issued(
        ConnectionOperatorCatalog::from_resolve_registration(|role| match role {
            ConnectionGraphOperatorRole::Branch => DefId::new(70_000),
            ConnectionGraphOperatorRole::Root => DefId::new(70_001),
            ConnectionGraphOperatorRole::PotentialRoot => DefId::new(70_002),
            ConnectionGraphOperatorRole::IsRoot => DefId::new(70_003),
            ConnectionGraphOperatorRole::Rooted => DefId::new(70_004),
        }),
        ExternalObjectLifecycleCatalog::begin_resolve_check(),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn connection_catalog_is_a_total_five_role_product() {
        let catalog = ConnectionOperatorCatalog::from_resolve_registration(|role| match role {
            ConnectionGraphOperatorRole::Branch => DefId::new(20),
            ConnectionGraphOperatorRole::Root => DefId::new(21),
            ConnectionGraphOperatorRole::PotentialRoot => DefId::new(22),
            ConnectionGraphOperatorRole::IsRoot => DefId::new(23),
            ConnectionGraphOperatorRole::Rooted => DefId::new(24),
        });
        for (role, declaration) in [
            (ConnectionGraphOperatorRole::Branch, DefId::new(20)),
            (ConnectionGraphOperatorRole::Root, DefId::new(21)),
            (ConnectionGraphOperatorRole::PotentialRoot, DefId::new(22)),
            (ConnectionGraphOperatorRole::IsRoot, DefId::new(23)),
            (ConnectionGraphOperatorRole::Rooted, DefId::new(24)),
        ] {
            assert_eq!(catalog.declaration(role), declaration);
            assert_eq!(catalog.role(declaration), Some(role));
        }
        assert_eq!(catalog.role(DefId::new(25)), None);
    }

    #[test]
    fn resolve_lifecycle_catalog_refuses_duplicate_owner_before_projection() {
        let identity = ExternalObjectLifecycleIdentity::from_resolve_check(
            DefId::new(30),
            DefId::new(31),
            DefId::new(32),
        );
        let mut external_objects = ExternalObjectLifecycleCatalog::begin_resolve_check();
        external_objects
            .insert_from_resolve_check(identity)
            .expect("first exact owner is unique");
        assert_eq!(
            external_objects.insert_from_resolve_check(identity),
            Err(DefId::new(30))
        );

        let catalogs = SemanticCatalogProjection::from_resolve_issued(
            ConnectionOperatorCatalog::from_resolve_registration(|role| match role {
                ConnectionGraphOperatorRole::Branch => DefId::new(20),
                ConnectionGraphOperatorRole::Root => DefId::new(21),
                ConnectionGraphOperatorRole::PotentialRoot => DefId::new(22),
                ConnectionGraphOperatorRole::IsRoot => DefId::new(23),
                ConnectionGraphOperatorRole::Rooted => DefId::new(24),
            }),
            external_objects,
        );
        let projected = catalogs
            .external_object(DefId(30))
            .expect("the first exact lifecycle identity remains authoritative");
        assert_eq!(projected, identity);
    }
}
