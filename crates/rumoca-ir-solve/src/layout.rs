use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

const F64_BYTES: usize = std::mem::size_of::<f64>();

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum ScalarSlot {
    Time,
    Y { index: usize, byte_offset: usize },
    P { index: usize, byte_offset: usize },
    Constant(f64),
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VarLayout {
    bindings: IndexMap<String, ScalarSlot>,
    y_scalars: usize,
    p_scalars: usize,
}

impl VarLayout {
    pub fn from_parts(
        bindings: IndexMap<String, ScalarSlot>,
        y_scalars: usize,
        p_scalars: usize,
    ) -> Self {
        Self {
            bindings,
            y_scalars,
            p_scalars,
        }
    }

    pub fn bindings(&self) -> &IndexMap<String, ScalarSlot> {
        &self.bindings
    }

    pub fn binding(&self, name: &str) -> Option<ScalarSlot> {
        if let Some(slot) = self.bindings.get(name).copied() {
            return Some(slot);
        }
        let alternate = alternate_projected_field_key(name)?;
        self.bindings.get(alternate.as_str()).copied()
    }

    pub fn y_scalars(&self) -> usize {
        self.y_scalars
    }

    pub fn p_scalars(&self) -> usize {
        self.p_scalars
    }
}

pub fn scalar_slot_y(index: usize) -> ScalarSlot {
    ScalarSlot::Y {
        index,
        byte_offset: index.saturating_mul(F64_BYTES),
    }
}

pub fn scalar_slot_p(index: usize) -> ScalarSlot {
    ScalarSlot::P {
        index,
        byte_offset: index.saturating_mul(F64_BYTES),
    }
}

fn projected_field_alias(name: &str) -> Option<String> {
    let open = name.find('[')?;
    let close = name[open + 1..].find(']')? + open + 1;
    let suffix = name.get(close + 1..)?;
    if !suffix.starts_with('.') {
        return None;
    }
    let prefix = &name[..open];
    let indices = &name[open + 1..close];
    Some(format!("{prefix}{suffix}[{indices}]"))
}

fn alternate_projected_field_key(name: &str) -> Option<String> {
    projected_field_alias(name).or_else(|| projected_field_base_key(name))
}

fn projected_field_base_key(name: &str) -> Option<String> {
    let open = name.rfind('[')?;
    let close = name[open + 1..].find(']')? + open + 1;
    if close != name.len() - 1 {
        return None;
    }
    let prefix_with_field = &name[..open];
    let (prefix, field) = prefix_with_field.rsplit_once('.')?;
    let indices = &name[open + 1..close];
    Some(format!("{prefix}[{indices}].{field}"))
}
