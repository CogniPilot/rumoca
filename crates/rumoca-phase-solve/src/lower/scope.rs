use super::*;
use indexmap::IndexSet;

#[derive(Debug, Clone, Default)]
pub(super) struct Scope {
    pub(super) frames: Vec<ScopeFrame>,
}

#[derive(Debug, Clone, Default)]
pub(super) struct ScopeFrame {
    bindings: IndexMap<ComponentReferenceKey, Reg>,
    indexed_bindings: IndexMap<ComponentReferenceKey, Vec<LocalIndexedBinding>>,
}

impl Scope {
    pub(super) fn new() -> Self {
        Self {
            frames: vec![ScopeFrame::default()],
        }
    }

    pub(super) fn push_frame(&mut self) {
        self.frames.push(ScopeFrame::default());
    }

    pub(super) fn pop_frame(&mut self) {
        debug_assert!(self.frames.len() > 1);
        if self.frames.len() > 1 {
            self.frames.pop();
        }
    }

    pub(super) fn with_frame<T, E>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        self.push_frame();
        let result = f(self);
        self.pop_frame();
        result
    }

    pub(super) fn get(&self, key: &ComponentReferenceKey) -> Option<&Reg> {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.bindings.get(key))
    }

    pub(super) fn contains_key(&self, key: &ComponentReferenceKey) -> bool {
        self.get(key).is_some()
    }

    pub(super) fn insert(&mut self, key: ComponentReferenceKey, reg: Reg) -> Option<Reg> {
        self.insert_key(key, reg)
    }

    pub(super) fn insert_scoped(&mut self, key: ComponentReferenceKey, reg: Reg) -> Option<Reg> {
        let frame = self.current_frame_mut();
        frame.bindings.insert(key, reg)
    }

    pub(super) fn shift_remove(&mut self, key: &ComponentReferenceKey) -> Option<Reg> {
        for frame in self.frames.iter_mut().rev() {
            if frame.bindings.contains_key(key) {
                return frame.bindings.shift_remove(key);
            }
        }
        None
    }

    pub(super) fn keys(&self) -> Vec<ComponentReferenceKey> {
        let mut keys = IndexSet::new();
        for frame in &self.frames {
            for key in frame.bindings.keys() {
                keys.insert(key.clone());
            }
        }
        keys.into_iter().collect()
    }

    pub(super) fn iter(&self) -> Vec<(ComponentReferenceKey, Reg)> {
        let mut entries = IndexMap::new();
        for frame in &self.frames {
            for (key, reg) in &frame.bindings {
                entries.insert(key.clone(), *reg);
            }
        }
        entries.into_iter().collect()
    }

    pub(super) fn indexed_values(&self, key: &ComponentReferenceKey) -> Option<Vec<Reg>> {
        let bindings = self.indexed_entries(key)?;
        if bindings.is_empty() {
            return None;
        }
        let mut values = bindings
            .iter()
            .map(|binding| (binding.indices.clone(), binding.reg))
            .collect::<Vec<_>>();
        let rank = values
            .iter()
            .map(|(indices, _)| indices.len())
            .max()
            .unwrap_or(0);
        if rank == 0 {
            return None;
        }
        values.retain(|(indices, _)| indices.len() == rank);
        values.sort_by(|(lhs, _), (rhs, _)| lhs.cmp(rhs));
        Some(values.into_iter().map(|(_, reg)| reg).collect())
    }

    pub(super) fn indexed_entries(
        &self,
        key: &ComponentReferenceKey,
    ) -> Option<&[LocalIndexedBinding]> {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.indexed_bindings.get(key))
            .map(Vec::as_slice)
    }

    pub(super) fn insert_key(&mut self, key: ComponentReferenceKey, reg: Reg) -> Option<Reg> {
        let frame_index = self
            .frames
            .iter()
            .rposition(|frame| frame.bindings.contains_key(&key))
            .unwrap_or(0);
        let frame = &mut self.frames[frame_index];
        frame.bindings.insert(key, reg)
    }

    pub(super) fn insert_indexed(
        &mut self,
        base_key: &ComponentReferenceKey,
        indices: &[usize],
        reg: Reg,
    ) {
        let frame_index = self
            .frames
            .iter()
            .rposition(|frame| frame.bindings.contains_key(base_key))
            .unwrap_or(0);
        let frame = &mut self.frames[frame_index];
        upsert_local_indexed_binding(
            frame.indexed_bindings.entry(base_key.clone()).or_default(),
            indices,
            reg,
        );
    }

    pub(super) fn clear_indexed(&mut self, base_key: &ComponentReferenceKey) {
        for frame in self.frames.iter_mut().rev() {
            frame.indexed_bindings.shift_remove(base_key);
        }
    }

    pub(super) fn current_frame_mut(&mut self) -> &mut ScopeFrame {
        self.frames
            .last_mut()
            .expect("solve lowering scope must always have a root frame")
    }
}

pub(super) fn upsert_local_indexed_binding(
    entries: &mut Vec<LocalIndexedBinding>,
    indices: &[usize],
    reg: Reg,
) {
    if let Some(entry) = entries.iter_mut().find(|entry| entry.indices == indices) {
        entry.reg = reg;
    } else {
        entries.push(LocalIndexedBinding {
            reg,
            indices: indices.to_vec(),
        });
    }
}

pub(super) fn generated_scope_key(name: impl Into<String>) -> ComponentReferenceKey {
    ComponentReferenceKey::generated(name)
}

pub(super) fn generated_scope_key_name(key: &ComponentReferenceKey) -> Option<&str> {
    match key {
        ComponentReferenceKey::Generated { name } => Some(name.as_str()),
        ComponentReferenceKey::Source { .. } => None,
    }
}

pub(super) fn generated_scope_key_suffix<'a>(
    key: &'a ComponentReferenceKey,
    prefix: &str,
) -> Option<&'a str> {
    generated_scope_key_name(key)?.strip_prefix(prefix)
}

pub(super) fn scope_key_direct_child_suffix(
    key: &ComponentReferenceKey,
    prefix: &ComponentReferenceKey,
) -> Option<String> {
    match (key, prefix) {
        (
            ComponentReferenceKey::Generated { name },
            ComponentReferenceKey::Generated { name: prefix },
        ) => {
            let suffix = name
                .as_str()
                .strip_prefix(&format!("{}.", prefix.as_str()))?;
            (!suffix.contains('.') && !suffix.contains('[')).then(|| suffix.to_string())
        }
        (
            ComponentReferenceKey::Source { parts, .. },
            ComponentReferenceKey::Source { parts: prefix, .. },
        ) => {
            let suffix = parts.strip_prefix(prefix.as_slice())?;
            (suffix.len() == 1 && suffix[0].subscripts.is_empty()).then(|| suffix[0].ident.clone())
        }
        _ => None,
    }
}

pub(super) fn component_field_key(
    base_key: &ComponentReferenceKey,
    field: &str,
) -> Option<ComponentReferenceKey> {
    match base_key {
        ComponentReferenceKey::Source { .. } => None,
        ComponentReferenceKey::Generated { name } => Some(ComponentReferenceKey::generated(
            format!("{}.{}", name.as_str(), field),
        )),
    }
}

pub(super) fn scope_field_available(
    scope: &Scope,
    base_key: &ComponentReferenceKey,
    field: &str,
) -> bool {
    component_field_key(base_key, field).is_some_and(|key| scope.contains_key(&key))
}

pub(super) fn field_access_expr(
    base: &rumoca_core::Expression,
    field: &str,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base.clone()),
        field: field.to_string(),
        span: base.span().unwrap_or(rumoca_core::Span::DUMMY),
    }
}

#[derive(Debug, Clone)]
pub(super) struct FunctionInputBindings {
    pub(super) scope: Scope,
    pub(super) const_bindings: IndexMap<String, f64>,
}

#[derive(Debug, Clone)]
pub(super) struct FunctionClosure {
    pub(super) target_name: rumoca_core::Reference,
    pub(super) bound_args: Vec<rumoca_core::Expression>,
    pub(super) captured_scope: Scope,
}

pub(super) struct FunctionInputBindState<'a> {
    pub(in crate::lower) scope: &'a mut Scope,
    pub(in crate::lower) const_scope: &'a mut IndexMap<String, f64>,
    pub(in crate::lower) const_bindings: &'a mut IndexMap<String, f64>,
}

pub(super) struct MaterializedRecordComponent {
    pub(super) suffix: String,
    pub(super) reg: Reg,
    pub(super) dims: Option<Vec<i64>>,
    pub(super) known_empty: bool,
}

pub(super) struct MaterializedRecordComponents {
    pub(super) components: Vec<MaterializedRecordComponent>,
    pub(super) indexed_components: Vec<(String, Vec<LocalIndexedBinding>)>,
}

#[derive(Clone)]
pub(super) struct LocalLowerFrame {
    pub(super) structural_bindings: IndexMap<String, f64>,
    pub(super) local_indexed_bindings: IndexMap<String, Vec<LocalIndexedBinding>>,
    pub(super) local_binding_dims: IndexMap<String, Vec<i64>>,
    pub(super) known_empty_local_arrays: IndexSet<String>,
    pub(super) local_const_bindings: IndexMap<String, f64>,
    pub(super) function_closures: IndexMap<ComponentReferenceKey, FunctionClosure>,
}

#[derive(Debug, Clone)]
pub(super) struct DirectAssignmentValue {
    pub(super) rhs: rumoca_core::Expression,
    pub(super) flat_index: Option<usize>,
    pub(super) repeat_period: Option<usize>,
}

impl DirectAssignmentValue {
    pub(super) fn full(rhs: rumoca_core::Expression) -> Self {
        Self {
            rhs,
            flat_index: None,
            repeat_period: None,
        }
    }

    pub(super) fn scalar(
        rhs: rumoca_core::Expression,
        flat_index: usize,
        repeat_period: Option<usize>,
    ) -> Self {
        Self {
            rhs,
            flat_index: Some(flat_index),
            repeat_period,
        }
    }
}
