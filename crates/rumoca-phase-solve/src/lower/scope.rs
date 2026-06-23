use super::*;
use indexmap::IndexSet;

#[derive(Debug, Clone, Default)]
pub(super) struct Scope {
    pub(super) frames: Vec<ScopeFrame>,
}

#[derive(Debug, Clone, Default)]
pub(super) struct ScopeFrame {
    bindings: IndexMap<ComponentPath, Reg>,
    indexed_bindings: IndexMap<ComponentPath, Vec<LocalIndexedBinding>>,
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

    pub(super) fn get(&self, path: &ComponentPath) -> Option<&Reg> {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.bindings.get(path))
    }

    pub(super) fn contains_key(&self, path: &ComponentPath) -> bool {
        self.get(path).is_some()
    }

    pub(super) fn insert(&mut self, path: ComponentPath, reg: Reg) -> Option<Reg> {
        self.insert_path(path, reg)
    }

    pub(super) fn insert_scoped(&mut self, path: ComponentPath, reg: Reg) -> Option<Reg> {
        let frame = self.current_frame_mut();
        upsert_scope_indexed_binding(&mut frame.indexed_bindings, &path, reg);
        frame.bindings.insert(path, reg)
    }

    pub(super) fn shift_remove(&mut self, path: &ComponentPath) -> Option<Reg> {
        for frame in self.frames.iter_mut().rev() {
            if frame.bindings.contains_key(path) {
                remove_scope_indexed_binding(&mut frame.indexed_bindings, path);
                return frame.bindings.shift_remove(path);
            }
        }
        None
    }

    pub(super) fn keys(&self) -> Vec<ComponentPath> {
        let mut keys = IndexSet::new();
        for frame in &self.frames {
            for key in frame.bindings.keys() {
                keys.insert(key.clone());
            }
        }
        keys.into_iter().collect()
    }

    pub(super) fn iter(&self) -> Vec<(ComponentPath, Reg)> {
        let mut entries = IndexMap::new();
        for frame in &self.frames {
            for (key, reg) in &frame.bindings {
                entries.insert(key.clone(), *reg);
            }
        }
        entries.into_iter().collect()
    }

    pub(super) fn is_empty(&self) -> bool {
        self.frames.iter().all(|frame| frame.bindings.is_empty())
    }

    pub(super) fn indexed_values(&self, path: &ComponentPath) -> Option<Vec<Reg>> {
        let bindings = self.indexed_entries(path)?;
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

    pub(super) fn indexed_entries(&self, path: &ComponentPath) -> Option<&[LocalIndexedBinding]> {
        self.frames
            .iter()
            .rev()
            .find_map(|frame| frame.indexed_bindings.get(path))
            .map(Vec::as_slice)
    }

    pub(super) fn insert_path(&mut self, path: ComponentPath, reg: Reg) -> Option<Reg> {
        let frame_index = self
            .frames
            .iter()
            .rposition(|frame| frame.bindings.contains_key(&path))
            .unwrap_or(0);
        let frame = &mut self.frames[frame_index];
        upsert_scope_indexed_binding(&mut frame.indexed_bindings, &path, reg);
        frame.bindings.insert(path, reg)
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

fn upsert_scope_indexed_binding(
    indexed_bindings: &mut IndexMap<ComponentPath, Vec<LocalIndexedBinding>>,
    path: &ComponentPath,
    reg: Reg,
) {
    let Some((base, indices)) = parse_indexed_binding_path(path) else {
        return;
    };
    upsert_local_indexed_binding(indexed_bindings.entry(base).or_default(), &indices, reg);
}

fn remove_scope_indexed_binding(
    indexed_bindings: &mut IndexMap<ComponentPath, Vec<LocalIndexedBinding>>,
    path: &ComponentPath,
) {
    let Some((base, indices)) = parse_indexed_binding_path(path) else {
        return;
    };
    if let Some(entries) = indexed_bindings.get_mut(&base) {
        entries.retain(|entry| entry.indices != indices);
        if entries.is_empty() {
            indexed_bindings.shift_remove(&base);
        }
    }
}

fn parse_indexed_binding_path(path: &ComponentPath) -> Option<(ComponentPath, Vec<usize>)> {
    let (base, indices) = parse_indexed_binding_key(path.as_str())?;
    Some((ComponentPath::from_flat_path(&base), indices))
}

pub(super) fn component_field_path(base_path: &ComponentPath, field: &str) -> ComponentPath {
    base_path.join_part_slice(&[field.to_string()])
}

pub(super) fn scope_field_available(scope: &Scope, base_path: &ComponentPath, field: &str) -> bool {
    scope.contains_key(&component_field_path(base_path, field))
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
    pub(super) local_indexed_bindings: IndexMap<String, Vec<LocalIndexedBinding>>,
    pub(super) local_binding_dims: IndexMap<String, Vec<i64>>,
    pub(super) known_empty_local_arrays: IndexSet<String>,
    pub(super) local_const_bindings: IndexMap<String, f64>,
    pub(super) function_closures: IndexMap<ComponentPath, FunctionClosure>,
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
