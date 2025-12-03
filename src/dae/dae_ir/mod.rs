//! Native DAE IR JSON serialization using serde_json.
//!
//! This module provides native Rust serialization for the DAE IR format (dae-0.1.0),
//! which is a superset of Base Modelica IR (MCP-0031) that adds explicit variable
//! classification matching the Modelica specification's DAE formalism (Appendix B).
//!
//! Key advantages over Base Modelica IR:
//! - Explicit state/derivative/algebraic classification (no der() scanning needed)
//! - Direct state/derivative linkage (like FMI's derivative attribute)
//! - Event indicators for zero-crossing detection
//! - Structural metadata (n_states, n_algebraic, dae_index)

mod equations;
pub mod expressions;
pub mod helpers;
mod variables;

use crate::dae::ast::Dae;
use equations::{Algorithms, ClassifiedEquations, EventIndicators};
use helpers::{EmptyArray, EmptyObject, Metadata, Structure};
use serde::ser::{Serialize, SerializeMap, Serializer};
use variables::ClassifiedVariables;

/// Wrapper struct for the complete DAE IR
#[derive(Debug)]
pub struct DaeIR<'a> {
    dae: &'a Dae,
}

impl<'a> DaeIR<'a> {
    pub fn from_dae(dae: &'a Dae) -> Self {
        Self { dae }
    }
}

impl<'a> Serialize for DaeIR<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(12))?;

        map.serialize_entry("ir_version", "dae-0.1.0")?;
        map.serialize_entry("base_modelica_version", "0.1")?;
        map.serialize_entry("model_name", &self.dae.model_name)?;

        // Variables - classified according to DAE formalism
        map.serialize_entry("variables", &ClassifiedVariables { dae: self.dae })?;

        // Equations - classified by type
        map.serialize_entry("equations", &ClassifiedEquations { dae: self.dae })?;

        // Event indicators from conditions
        map.serialize_entry("event_indicators", &EventIndicators { dae: self.dae })?;

        // Algorithms - reconstruct from fr (reset expressions)
        map.serialize_entry("algorithms", &Algorithms { dae: self.dae })?;

        map.serialize_entry("initial_algorithms", &EmptyArray)?;
        map.serialize_entry("functions", &EmptyArray)?;

        // Structure metadata
        map.serialize_entry("structure", &Structure { dae: self.dae })?;

        map.serialize_entry("source_info", &EmptyObject)?;

        // Metadata
        map.serialize_entry("metadata", &Metadata { dae: self.dae })?;

        map.end()
    }
}
