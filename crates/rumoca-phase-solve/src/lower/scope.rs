use super::*;
use indexmap::IndexSet;

#[derive(Debug, Clone)]
pub(super) struct Scope {
    frames: Vec<ScopeFrame>,
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

    pub(super) fn insert_scoped(
        &mut self,
        key: ComponentReferenceKey,
        reg: Reg,
    ) -> Result<Option<Reg>, LowerError> {
        let frame = self.current_frame_mut()?;
        Ok(frame.bindings.insert(key, reg))
    }

    pub(super) fn shift_remove(&mut self, key: &ComponentReferenceKey) -> Option<Reg> {
        for frame in self.frames.iter_mut().rev() {
            if frame.bindings.contains_key(key) {
                return frame.bindings.shift_remove(key);
            }
        }
        None
    }

    pub(super) fn keys_checked(
        &self,
        context: &'static str,
        span: rumoca_core::Span,
    ) -> Result<Vec<ComponentReferenceKey>, LowerError> {
        let capacity = self.binding_count(context, span)?;
        let mut keys = IndexSet::new();
        keys.try_reserve(capacity).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} capacity exceeds host memory limits"),
                span,
            )
        })?;
        for frame in &self.frames {
            for key in frame.bindings.keys() {
                keys.insert(key.clone());
            }
        }
        let mut out = crate::lower_vec_with_capacity(keys.len(), context, span)?;
        out.extend(keys);
        Ok(out)
    }

    pub(super) fn iter_checked(
        &self,
        context: &'static str,
        span: rumoca_core::Span,
    ) -> Result<Vec<(ComponentReferenceKey, Reg)>, LowerError> {
        let capacity = self.binding_count(context, span)?;
        let mut entries = IndexMap::new();
        entries.try_reserve(capacity).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} capacity exceeds host memory limits"),
                span,
            )
        })?;
        for frame in &self.frames {
            for (key, reg) in &frame.bindings {
                entries.insert(key.clone(), *reg);
            }
        }
        let mut out = crate::lower_vec_with_capacity(entries.len(), context, span)?;
        out.extend(entries);
        Ok(out)
    }

    pub(super) fn indexed_values(
        &self,
        key: &ComponentReferenceKey,
        span: rumoca_core::Span,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(bindings) = self.indexed_entries(key) else {
            return Ok(None);
        };
        if bindings.is_empty() {
            return Ok(None);
        }
        let mut values =
            crate::lower_vec_with_capacity(bindings.len(), "indexed binding value count", span)?;
        values.extend(bindings.iter());
        let rank = values
            .iter()
            .map(|binding| binding.indices.len())
            .max()
            .unwrap_or(0);
        if rank == 0 {
            return Ok(None);
        }
        values.retain(|binding| binding.indices.len() == rank);
        values.sort_by(|lhs, rhs| lhs.indices.cmp(&rhs.indices));
        let mut regs =
            crate::lower_vec_with_capacity(values.len(), "indexed binding register count", span)?;
        regs.extend(values.into_iter().map(|binding| binding.reg));
        Ok(Some(regs))
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
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
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
            span,
        )
    }

    pub(super) fn clear_indexed(&mut self, base_key: &ComponentReferenceKey) {
        for frame in self.frames.iter_mut().rev() {
            frame.indexed_bindings.shift_remove(base_key);
        }
    }

    fn current_frame_mut(&mut self) -> Result<&mut ScopeFrame, LowerError> {
        self.frames
            .last_mut()
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "solve lowering scope has no root frame".to_string(),
            })
    }

    fn binding_count(
        &self,
        context: &'static str,
        span: rumoca_core::Span,
    ) -> Result<usize, LowerError> {
        let mut count = 0usize;
        for frame in &self.frames {
            count = count.checked_add(frame.bindings.len()).ok_or_else(|| {
                LowerError::contract_violation(
                    format!("{context} capacity exceeds host limits"),
                    span,
                )
            })?;
        }
        Ok(count)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

pub(super) fn upsert_local_indexed_binding(
    entries: &mut Vec<LocalIndexedBinding>,
    indices: &[usize],
    reg: Reg,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if let Some(entry) = entries.iter_mut().find(|entry| entry.indices == indices) {
        entry.reg = reg;
        return Ok(());
    }
    entries.push(local_indexed_binding(reg, indices, span)?);
    Ok(())
}

pub(super) fn local_indexed_binding(
    reg: Reg,
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<LocalIndexedBinding, LowerError> {
    let mut copied =
        crate::lower_vec_with_capacity(indices.len(), "local indexed binding rank", span)?;
    copied.extend(indices.iter().copied());
    Ok(LocalIndexedBinding {
        reg,
        indices: copied,
    })
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

pub(super) fn field_access_expr_with_owner(
    base: &rumoca_core::Expression,
    field: &str,
    owner_span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base.clone()),
        field: field.to_string(),
        span: base.span().unwrap_or(owner_span),
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
    pub(super) structural_bindings: std::sync::Arc<IndexMap<String, f64>>,
    pub(super) local_indexed_bindings: IndexMap<String, Vec<LocalIndexedBinding>>,
    pub(super) local_binding_dims: IndexMap<String, Vec<i64>>,
    pub(super) known_empty_local_arrays: IndexSet<String>,
    pub(super) guarded_uninitialized_locals: IndexSet<String>,
    pub(super) local_const_bindings: IndexMap<String, f64>,
    pub(super) local_integer_bounds: IndexMap<String, (i64, i64)>,
    pub(super) function_closures: IndexMap<ComponentReferenceKey, FunctionClosure>,
}

#[derive(Debug, Clone)]
pub(super) struct DirectAssignmentValue {
    pub(super) rhs: rumoca_core::Expression,
    pub(super) span: rumoca_core::Span,
    pub(super) flat_index: Option<usize>,
    pub(super) repeat_period: Option<usize>,
}

impl DirectAssignmentValue {
    pub(super) fn full(rhs: rumoca_core::Expression, span: rumoca_core::Span) -> Self {
        Self {
            rhs,
            span,
            flat_index: None,
            repeat_period: None,
        }
    }

    pub(super) fn scalar(
        rhs: rumoca_core::Expression,
        flat_index: usize,
        repeat_period: Option<usize>,
        span: rumoca_core::Span,
    ) -> Self {
        Self {
            rhs,
            span,
            flat_index: Some(flat_index),
            repeat_period,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scoped_insert_rejects_missing_root_frame() {
        let mut scope = Scope { frames: Vec::new() };

        let result = scope.insert_scoped(generated_scope_key("i"), 0);

        assert!(matches!(
            result,
            Err(LowerError::UnspannedContractViolation { .. })
        ));
        let reason = match result {
            Err(err) => err.reason(),
            Ok(_) => String::new(),
        };
        assert!(reason.contains("no root frame"));
    }

    #[test]
    fn default_scope_has_root_frame() {
        let mut scope = Scope::default();

        let result = scope.insert_scoped(generated_scope_key("i"), 0);

        assert!(result.is_ok());
        assert_eq!(scope.frames.len(), 1);
    }
}
