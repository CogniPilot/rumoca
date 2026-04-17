use serde::Deserialize;
use std::path::Path;

use rumoca_io_fb::config::{
    MessageConfig as FlatbufferMessageConfig, SchemaConfig as FlatbufferSchemaConfig,
};

#[derive(Debug, Deserialize)]
pub struct SimFbConfig {
    pub sim: SimConfig,
    #[serde(default)]
    pub udp: Option<UdpConfig>,
    pub schema: FlatbufferSchemaConfig,
    pub receive: FlatbufferMessageConfig,
    pub send: FlatbufferMessageConfig,
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

impl SimFbConfig {
    pub fn load(path: &Path) -> anyhow::Result<Self> {
        let text = std::fs::read_to_string(path)?;
        let config: SimFbConfig = toml::from_str(&text)?;
        Ok(config)
    }
}
