use std::collections::HashMap;

use serde::Deserialize;

#[derive(Debug, Clone, Deserialize)]
pub struct SchemaConfig {
    pub bfbs: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct MessageConfig {
    pub root_type: String,
    pub route: HashMap<String, RouteEntry>,
}

/// A route entry can be either a simple string `"var_name"` or a table
/// `{ var = "var_name", scale = 1100.0 }`.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum RouteEntry {
    Simple(String),
    Full { var: String, scale: Option<f64> },
}

impl RouteEntry {
    #[must_use]
    pub fn var(&self) -> &str {
        match self {
            Self::Simple(name) => name,
            Self::Full { var, .. } => var,
        }
    }

    #[must_use]
    pub fn scale(&self) -> f64 {
        match self {
            Self::Simple(_) => 1.0,
            Self::Full { scale, .. } => scale.unwrap_or(1.0),
        }
    }
}
