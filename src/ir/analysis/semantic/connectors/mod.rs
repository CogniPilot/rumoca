//! Connector validation checks.
//!
//! MLS §9: Connectors

mod cardinality;
mod connect;

pub use cardinality::{check_cardinality_arguments, check_cardinality_context};
pub use connect::{
    check_connect_types, check_connector_no_parameter_constant, check_expandable_no_flow,
    check_flow_compatibility, check_self_connections, check_stream_only_in_connector,
    check_stream_requires_flow,
};
