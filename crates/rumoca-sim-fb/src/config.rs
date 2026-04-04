use serde::Deserialize;
use std::collections::HashMap;
use std::path::Path;

#[derive(Debug, Deserialize)]
pub struct SilConfig {
    pub sim: SimConfig,
    #[serde(default)]
    pub udp: Option<UdpConfig>,
    pub schema: SchemaConfig,
    pub receive: MessageConfig,
    pub send: MessageConfig,
    #[serde(default)]
    pub autopilot: Option<AutopilotConfig>,
}

#[derive(Debug, Deserialize)]
pub struct AutopilotConfig {
    pub command: String,
}

#[derive(Debug, Deserialize)]
pub struct SimConfig {
    #[serde(default = "default_dt")]
    pub dt: f64,
    #[serde(default = "default_true")]
    pub realtime: bool,
    #[serde(default)]
    pub test: bool,
}

fn default_dt() -> f64 {
    0.004
}
fn default_true() -> bool {
    true
}

#[derive(Debug, Deserialize)]
pub struct UdpConfig {
    pub listen: String,
    pub send: String,
}

#[derive(Debug, Deserialize)]
pub struct SchemaConfig {
    pub bfbs: Vec<String>,
}

#[derive(Debug, Deserialize)]
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
    pub fn var(&self) -> &str {
        match self {
            RouteEntry::Simple(s) => s,
            RouteEntry::Full { var, .. } => var,
        }
    }

    pub fn scale(&self) -> f64 {
        match self {
            RouteEntry::Simple(_) => 1.0,
            RouteEntry::Full { scale, .. } => scale.unwrap_or(1.0),
        }
    }
}

impl SilConfig {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let text = std::fs::read_to_string(path)?;
        let config: SilConfig = toml::from_str(&text)?;
        Ok(config)
    }
}
