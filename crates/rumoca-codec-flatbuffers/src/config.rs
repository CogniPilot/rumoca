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

/// A route entry can be either a simple string `"logical_key"` or a table.
/// The table form accepts any of `to`, `key`, or `var` (legacy) for the
/// logical key — semantic layer above the codec decides what the key means.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum RouteEntry {
    Simple(String),
    Full {
        #[serde(alias = "to", alias = "key", alias = "var")]
        name: String,
        scale: Option<f64>,
    },
}

impl RouteEntry {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Simple(n) => n,
            Self::Full { name, .. } => name,
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
