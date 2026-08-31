/// Built-in namespace containing the MLS section 9.4 connection-graph
/// operators.
pub const CONNECTIONS_NAMESPACE: &str = "Connections";

/// Closed semantic roles of the predefined MLS section 9.4 connection-graph
/// operators.
///
/// The role is target-neutral vocabulary. Resolve issues the declaration
/// identity for each role; later phases classify only those identities.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionGraphOperatorRole {
    Branch,
    Root,
    PotentialRoot,
    IsRoot,
    Rooted,
}

impl ConnectionGraphOperatorRole {
    /// Every role in the closed MLS section 9.4 vocabulary.
    pub const ALL: [Self; 5] = [
        Self::Branch,
        Self::Root,
        Self::PotentialRoot,
        Self::IsRoot,
        Self::Rooted,
    ];

    /// Return the one predefined source path projected from this semantic role.
    pub const fn predefined_path(self) -> [&'static str; 2] {
        let member = match self {
            Self::Branch => "branch",
            Self::Root => "root",
            Self::PotentialRoot => "potentialRoot",
            Self::IsRoot => "isRoot",
            Self::Rooted => "rooted",
        };
        [CONNECTIONS_NAMESPACE, member]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn connection_graph_roles_project_five_distinct_predefined_paths() {
        let paths = ConnectionGraphOperatorRole::ALL.map(|role| role.predefined_path());
        for (position, path) in paths.iter().enumerate() {
            assert_eq!(path[0], CONNECTIONS_NAMESPACE);
            assert!(!path[1].is_empty());
            assert!(
                paths[..position].iter().all(|earlier| earlier != path),
                "each semantic role must project a distinct predefined path"
            );
        }
    }
}
