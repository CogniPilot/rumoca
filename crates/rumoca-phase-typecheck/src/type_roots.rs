//! Cycle-aware canonical type-root traversal shared by typecheck consumers.

use rumoca_core::TypeId;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeRootResolutionError {
    UnknownTarget { source: TypeId },
    Cycle { source: TypeId, target: TypeId },
}

impl TypeRootResolutionError {
    pub(crate) fn source(self) -> TypeId {
        match self {
            Self::UnknownTarget { source } | Self::Cycle { source, .. } => source,
        }
    }

    pub(crate) fn target(self) -> Option<TypeId> {
        match self {
            Self::UnknownTarget { .. } => None,
            Self::Cycle { target, .. } => Some(target),
        }
    }
}

/// Follow a finite type-identity graph to its terminal canonical root.
///
/// `next` returns `None` only for a terminal type. Unknown edges and cycles are
/// rejected; no traversal-depth cutoff is allowed to mint an intermediate root.
pub(crate) fn resolve_type_root(
    start: TypeId,
    mut next: impl FnMut(TypeId) -> Result<Option<TypeId>, TypeRootResolutionError>,
) -> Result<TypeId, TypeRootResolutionError> {
    if start.is_unknown() {
        return Err(TypeRootResolutionError::UnknownTarget { source: start });
    }

    let mut visited = HashSet::from([start]);
    let mut current = start;
    loop {
        let Some(candidate) = next(current)? else {
            return Ok(current);
        };
        if candidate.is_unknown() {
            return Err(TypeRootResolutionError::UnknownTarget { source: current });
        }
        if !visited.insert(candidate) {
            return Err(TypeRootResolutionError::Cycle {
                source: current,
                target: candidate,
            });
        }
        current = candidate;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn resolves_chains_longer_than_the_removed_depth_limit() {
        let terminal = TypeId::new(24);
        let edges = (7..24)
            .map(|index| (TypeId::new(index), TypeId::new(index + 1)))
            .collect::<HashMap<_, _>>();
        assert_eq!(
            resolve_type_root(TypeId::new(7), |type_id| {
                Ok(edges.get(&type_id).copied())
            }),
            Ok(terminal),
        );
    }

    #[test]
    fn rejects_unknown_targets_and_cycles_without_inventing_a_root() {
        let unknown = HashMap::from([(TypeId::new(7), TypeId::UNKNOWN)]);
        assert_eq!(
            resolve_type_root(TypeId::new(7), |type_id| {
                Ok(unknown.get(&type_id).copied())
            }),
            Err(TypeRootResolutionError::UnknownTarget {
                source: TypeId::new(7),
            }),
        );

        let cycle = HashMap::from([
            (TypeId::new(7), TypeId::new(8)),
            (TypeId::new(8), TypeId::new(7)),
        ]);
        assert_eq!(
            resolve_type_root(TypeId::new(7), |type_id| Ok(cycle.get(&type_id).copied())),
            Err(TypeRootResolutionError::Cycle {
                source: TypeId::new(8),
                target: TypeId::new(7),
            }),
        );
    }
}
