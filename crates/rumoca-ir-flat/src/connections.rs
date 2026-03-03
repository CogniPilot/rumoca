//! Connection structures for the Flat Model (MLS §9).
//!
//! This module provides data structures for processing connect() equations:
//! - Connection sets for tracking connected variables
//! - Connection graphs for overconstrained connectors
//! - Spanning trees for equation generation

use indexmap::{IndexMap, IndexSet};
use rumoca_core::DefId;
use serde::{Deserialize, Serialize};

use crate::VarName;

// =============================================================================
// Connection Set (MLS §9.2)
// =============================================================================

/// MLS §9.2: Connection Set.
///
/// "A connection set is a set of variables connected by means of connect-equations."
/// Connection sets are used to generate the appropriate equations:
/// - For non-flow variables: equality equations (all equal)
/// - For flow variables: sum-to-zero equation
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ConnectionSet {
    /// Variables in this connection set.
    pub variables: IndexSet<ConnectedVariable>,
    /// Whether all variables in this set have flow prefix.
    pub is_flow: bool,
    /// Whether all variables in this set have stream prefix.
    pub is_stream: bool,
}

impl ConnectionSet {
    /// Create a new empty connection set.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new flow connection set.
    pub fn flow() -> Self {
        Self {
            is_flow: true,
            ..Default::default()
        }
    }

    /// Create a new stream connection set.
    pub fn stream() -> Self {
        Self {
            is_stream: true,
            ..Default::default()
        }
    }

    /// Add a variable to this connection set.
    pub fn add(&mut self, var: ConnectedVariable) {
        self.variables.insert(var);
    }

    /// Get the number of variables in this set.
    pub fn len(&self) -> usize {
        self.variables.len()
    }

    /// Check if this set is empty.
    pub fn is_empty(&self) -> bool {
        self.variables.is_empty()
    }

    /// Check if this set contains a specific variable.
    pub fn contains(&self, name: &VarName) -> bool {
        self.variables.iter().any(|v| &v.name == name)
    }
}

/// A variable in a connection set.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConnectedVariable {
    /// Globally unique variable name.
    pub name: VarName,
    /// Whether this is an inside connector (sign = +1) or outside connector (sign = -1).
    /// MLS §9.2: "The sign is +1 for inside connectors and -1 for outside connectors."
    pub is_inside: bool,
    /// Reference to the connector type definition.
    pub connector_type: Option<DefId>,
}

impl ConnectedVariable {
    /// Create a new connected variable.
    pub fn new(name: VarName, is_inside: bool) -> Self {
        Self {
            name,
            is_inside,
            connector_type: None,
        }
    }

    /// Get the sign for flow equation generation.
    /// MLS §9.2: +1 for inside, -1 for outside.
    pub fn sign(&self) -> i32 {
        if self.is_inside { 1 } else { -1 }
    }
}

/// Collection of all connection sets in a model.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ConnectionSets {
    /// All connection sets, indexed by a representative variable name.
    pub sets: IndexMap<VarName, ConnectionSet>,
}

impl ConnectionSets {
    /// Create a new empty collection.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add or merge a connection set.
    pub fn add_set(&mut self, representative: VarName, set: ConnectionSet) {
        self.sets.insert(representative, set);
    }

    /// Get the number of connection sets.
    pub fn len(&self) -> usize {
        self.sets.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.sets.is_empty()
    }
}

// =============================================================================
// Connection Graph (MLS §9.4)
// =============================================================================

/// MLS §9.4: Virtual Connection Graph.
///
/// "For an overdetermined connector, a virtual connection graph is formed from all
/// connections of the overdetermined type or record and the connect-edges are grouped
/// into optional spanning tree edges and required spanning tree edges."
///
/// This is used for overconstrained connectors (e.g., MultiBody frames) where
/// redundant constraints must be removed.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ConnectionGraph {
    /// Nodes in the graph (overdetermined type/record instances).
    pub nodes: IndexMap<VarName, GraphNode>,
    /// Edges from connect() statements (optional - can be removed).
    pub optional_edges: Vec<GraphEdge>,
    /// Edges from Connections.branch() (required - cannot be removed).
    pub required_edges: Vec<GraphEdge>,
}

impl ConnectionGraph {
    /// Create a new empty connection graph.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a node to the graph.
    pub fn add_node(&mut self, name: VarName, node: GraphNode) {
        self.nodes.insert(name, node);
    }

    /// Add an optional edge (from connect()).
    pub fn add_optional_edge(&mut self, edge: GraphEdge) {
        self.optional_edges.push(edge);
    }

    /// Add a required edge (from Connections.branch()).
    pub fn add_required_edge(&mut self, edge: GraphEdge) {
        self.required_edges.push(edge);
    }

    /// Get the number of nodes.
    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }

    /// Get the number of edges (optional + required).
    pub fn num_edges(&self) -> usize {
        self.optional_edges.len() + self.required_edges.len()
    }
}

/// A node in the connection graph.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphNode {
    /// Variable name for this node.
    pub name: VarName,
    /// Root status of this node.
    pub root_status: RootStatus,
    /// Reference to the overdetermined type.
    pub overdetermined_type: Option<DefId>,
}

impl GraphNode {
    /// Create a new graph node.
    pub fn new(name: VarName) -> Self {
        Self {
            name,
            root_status: RootStatus::None,
            overdetermined_type: None,
        }
    }
}

/// MLS §9.4: Root status for connection graph nodes.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum RootStatus {
    /// Not a root node.
    #[default]
    None,
    /// Definite root (from Connections.root()).
    /// "The overdetermined type or record instance R in connector instance a is a (definite) root node."
    DefiniteRoot,
    /// Potential root (from Connections.potentialRoot()).
    /// Selected by priority if no definite root exists in subgraph.
    PotentialRoot {
        /// Priority value (lower = higher priority). Must be >= 0.
        priority: i32,
    },
}

/// An edge in the connection graph.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphEdge {
    /// First endpoint.
    pub a: VarName,
    /// Second endpoint.
    pub b: VarName,
    /// Whether this edge is in the spanning tree.
    pub in_spanning_tree: bool,
}

impl GraphEdge {
    /// Create a new edge.
    pub fn new(a: VarName, b: VarName) -> Self {
        Self {
            a,
            b,
            in_spanning_tree: false,
        }
    }
}

// =============================================================================
// Spanning Tree (MLS §9.4)
// =============================================================================

/// MLS §9.4: Spanning Tree.
///
/// "A spanning-tree is constructed from the virtual connection graph by removing
/// optional spanning tree edges."
///
/// The spanning tree determines which connection equations are kept (those in the tree)
/// and which are replaced by equalityConstraint() calls (those removed).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SpanningTree {
    /// Root nodes selected for this tree.
    pub roots: Vec<VarName>,
    /// Edges included in the spanning tree.
    pub edges: Vec<SpanningTreeEdge>,
    /// Edges removed from the spanning tree (will use equalityConstraint).
    pub removed_edges: Vec<GraphEdge>,
}

impl SpanningTree {
    /// Create a new empty spanning tree.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a root node.
    pub fn add_root(&mut self, root: VarName) {
        self.roots.push(root);
    }

    /// Add an edge to the spanning tree.
    pub fn add_edge(&mut self, edge: SpanningTreeEdge) {
        self.edges.push(edge);
    }

    /// Record a removed edge.
    pub fn remove_edge(&mut self, edge: GraphEdge) {
        self.removed_edges.push(edge);
    }
}

/// An edge in the spanning tree.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpanningTreeEdge {
    /// Parent node (closer to root).
    pub parent: VarName,
    /// Child node.
    pub child: VarName,
    /// Whether this was a required edge (from Connections.branch()).
    pub required: bool,
}

impl SpanningTreeEdge {
    /// Create a new spanning tree edge.
    pub fn new(parent: VarName, child: VarName, required: bool) -> Self {
        Self {
            parent,
            child,
            required,
        }
    }
}

// =============================================================================
// Overconstrained Connector Support (MLS §9.4.1)
// =============================================================================

/// MLS §9.4.1: Equality Constraint Definition.
///
/// For overconstrained connectors, the equalityConstraint function defines
/// how to compute residuals for removed connection equations.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EqualityConstraint {
    /// The function that computes the residual.
    /// Prototype: `function equalityConstraint(a: T, b: T) -> Real[n]`
    pub function_def: Option<DefId>,
    /// Number of scalar constraint equations (n in `Real[n]`).
    pub num_constraints: usize,
}

impl EqualityConstraint {
    /// Create a new equality constraint definition.
    pub fn new(num_constraints: usize) -> Self {
        Self {
            function_def: None,
            num_constraints,
        }
    }
}
