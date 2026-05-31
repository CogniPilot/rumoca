use std::collections::HashMap;
use std::sync::Mutex;

use rumoca_core as core;

#[derive(Debug, Clone, PartialEq)]
pub(super) struct ExternalTableSpec {
    /// Flattened row-major table matrix.
    pub(super) data: Vec<Vec<f64>>,
    /// Selected output columns (Modelica 1-based indexing).
    pub(super) columns: Vec<usize>,
    /// Smoothness enum value (Modelica.Blocks.Types.Smoothness).
    pub(super) smoothness: i64,
    /// Extrapolation enum value (Modelica.Blocks.Types.Extrapolation).
    pub(super) extrapolation: i64,
}

#[derive(Debug, Default)]
pub(super) struct ExternalTableRegistry {
    next_id: u64,
    by_hash: HashMap<u64, u64>,
    tables: HashMap<u64, ExternalTableSpec>,
}

fn hash_table_spec(spec: &ExternalTableSpec) -> u64 {
    let mut hasher = blake3::Hasher::new();
    hasher.update(&spec.smoothness.to_le_bytes());
    hasher.update(&spec.extrapolation.to_le_bytes());
    hasher.update(&(spec.columns.len() as u64).to_le_bytes());
    for column in &spec.columns {
        hasher.update(&(*column as u64).to_le_bytes());
    }
    hasher.update(&(spec.data.len() as u64).to_le_bytes());
    for row in &spec.data {
        hasher.update(&(row.len() as u64).to_le_bytes());
        for value in row {
            hasher.update(&value.to_bits().to_le_bytes());
        }
    }
    stable_u64_from_hash(hasher.finalize())
}

fn stable_u64_from_hash(hash: blake3::Hash) -> u64 {
    let mut bytes = [0u8; 8];
    bytes.copy_from_slice(&hash.as_bytes()[..8]);
    u64::from_le_bytes(bytes)
}

fn register_external_table_in_registry(
    registry: &Mutex<ExternalTableRegistry>,
    spec: ExternalTableSpec,
) -> u64 {
    let hash = hash_table_spec(&spec);
    let mut reg = registry
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    if let Some(existing_id) = reg.by_hash.get(&hash).copied()
        && reg.tables.get(&existing_id) == Some(&spec)
    {
        return existing_id;
    }
    reg.next_id = reg.next_id.saturating_add(1);
    let id = reg.next_id;
    reg.by_hash.insert(hash, id);
    reg.tables.insert(id, spec);
    id
}

pub(super) fn register_external_table_in(
    registry: &Mutex<ExternalTableRegistry>,
    spec: ExternalTableSpec,
) -> u64 {
    register_external_table_in_registry(registry, spec)
}

pub(super) fn external_table_data(id: u64, spec: &ExternalTableSpec) -> core::ExternalTableData {
    core::ExternalTableData {
        id,
        data: spec.data.clone(),
        columns: spec.columns.clone(),
        smoothness: spec.smoothness,
        extrapolation: spec.extrapolation,
    }
}

pub(super) fn external_table_spec(data: &core::ExternalTableData) -> ExternalTableSpec {
    ExternalTableSpec {
        data: data.data.clone(),
        columns: data.columns.clone(),
        smoothness: data.smoothness,
        extrapolation: data.extrapolation,
    }
}

pub(super) fn external_table_data_for_values(values: &[f64]) -> Vec<core::ExternalTableData> {
    let _ = values;
    Vec::new()
}

pub(super) fn external_table_data_for_values_in(
    registry: &Mutex<ExternalTableRegistry>,
    values: &[f64],
) -> Vec<core::ExternalTableData> {
    let Ok(reg) = registry.lock() else {
        return Vec::new();
    };
    let mut tables = Vec::new();
    let mut seen = std::collections::HashSet::new();
    for value in values {
        if !value.is_finite() {
            continue;
        }
        let rounded = value.round();
        if (rounded - value).abs() > 1e-6 || rounded <= 0.0 {
            continue;
        }
        let id = rounded as u64;
        if seen.insert(id)
            && let Some(spec) = reg.tables.get(&id)
        {
            tables.push(external_table_data(id, spec));
        }
    }
    tables
}

pub(super) fn lookup_external_table_in_registry(
    registry: &Mutex<ExternalTableRegistry>,
    id_real: f64,
) -> Option<ExternalTableSpec> {
    let id = external_table_id(id_real)?;
    let reg = registry.lock().ok()?;
    reg.tables.get(&id).cloned()
}

pub(super) fn lookup_external_table_in(
    id_real: f64,
    tables: &[core::ExternalTableData],
) -> Option<ExternalTableSpec> {
    let id = external_table_id(id_real)?;
    tables
        .iter()
        .find(|table| table.id == id)
        .map(external_table_spec)
}

fn external_table_id(id_real: f64) -> Option<u64> {
    if !id_real.is_finite() {
        return None;
    }
    let rounded = id_real.round();
    if (rounded - id_real).abs() > 1e-6 || rounded <= 0.0 {
        return None;
    }
    Some(rounded as u64)
}
