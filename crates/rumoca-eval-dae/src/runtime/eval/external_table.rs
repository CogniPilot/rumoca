use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Mutex, OnceLock};

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

#[derive(Default)]
struct ExternalTableRegistry {
    next_id: u64,
    by_hash: HashMap<u64, u64>,
    tables: HashMap<u64, ExternalTableSpec>,
}

static EXTERNAL_TABLE_REGISTRY: OnceLock<Mutex<ExternalTableRegistry>> = OnceLock::new();

fn table_registry() -> &'static Mutex<ExternalTableRegistry> {
    EXTERNAL_TABLE_REGISTRY.get_or_init(|| Mutex::new(ExternalTableRegistry::default()))
}

fn hash_table_spec(spec: &ExternalTableSpec) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    spec.smoothness.hash(&mut hasher);
    spec.extrapolation.hash(&mut hasher);
    spec.columns.hash(&mut hasher);
    spec.data.len().hash(&mut hasher);
    for row in &spec.data {
        row.len().hash(&mut hasher);
        for value in row {
            value.to_bits().hash(&mut hasher);
        }
    }
    hasher.finish()
}

pub(super) fn register_external_table(spec: ExternalTableSpec) -> u64 {
    let hash = hash_table_spec(&spec);
    let mut reg = table_registry()
        .lock()
        .expect("external table registry poisoned");
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

pub(super) fn lookup_external_table(id_real: f64) -> Option<ExternalTableSpec> {
    if !id_real.is_finite() {
        return None;
    }
    let rounded = id_real.round();
    if (rounded - id_real).abs() > 1e-6 || rounded <= 0.0 {
        return None;
    }
    let id = rounded as u64;
    let reg = table_registry().lock().ok()?;
    reg.tables.get(&id).cloned()
}
