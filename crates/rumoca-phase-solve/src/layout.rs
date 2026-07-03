use indexmap::IndexMap;
use rumoca_core::Span;
use rumoca_ir_dae as dae;

use crate::lower::LowerError;
use rumoca_ir_solve::{ComponentReferenceKey, IndexedScalarSlot, ScalarSlot, VarLayout};

pub(crate) const INITIAL_EVENT_PARAMETER_NAME: &str = "__rumoca.initial_event";
const F64_BYTES: usize = std::mem::size_of::<f64>();

#[derive(Debug, Clone, Copy)]
enum SlotStorage {
    Y,
    P,
}

pub fn build_var_layout(dae_model: &dae::Dae) -> Result<VarLayout, LowerError> {
    build_var_layout_with_solver_len(dae_model, usize::MAX)
}

pub fn build_var_layout_with_solver_len(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<VarLayout, LowerError> {
    let mut bindings = IndexMap::new();
    let mut shapes = IndexMap::new();
    let mut shape_spans = IndexMap::new();
    let mut shape_indexed_keys = IndexMap::new();
    let mut indexed_bindings = IndexMap::new();
    bindings.insert("time".to_string(), ScalarSlot::Time);

    let y_scalars = map_y_bindings(
        dae_model,
        &mut bindings,
        &mut shapes,
        &mut shape_spans,
        &mut shape_indexed_keys,
        &mut indexed_bindings,
        solver_len,
    )?;
    let mut p_scalars = map_p_bindings(
        dae_model,
        LayoutBindingMaps {
            bindings: &mut bindings,
            shapes: &mut shapes,
            shape_spans: &mut shape_spans,
            shape_indexed_keys: &mut shape_indexed_keys,
            indexed_bindings: &mut indexed_bindings,
        },
    )?;
    bindings.insert(
        INITIAL_EVENT_PARAMETER_NAME.to_string(),
        synthetic_scalar_slot(SlotStorage::P, p_scalars, INITIAL_EVENT_PARAMETER_NAME)?,
    );
    p_scalars = add_synthetic_slot_count(p_scalars, 1, INITIAL_EVENT_PARAMETER_NAME)?;
    map_enum_literal_bindings(dae_model, &mut bindings);
    map_constant_bindings(
        dae_model,
        LayoutBindingMaps {
            bindings: &mut bindings,
            shapes: &mut shapes,
            shape_spans: &mut shape_spans,
            shape_indexed_keys: &mut shape_indexed_keys,
            indexed_bindings: &mut indexed_bindings,
        },
    )?;

    VarLayout::from_parts_with_shapes_spans_keys_and_indexed_bindings(
        bindings,
        shapes,
        shape_spans,
        shape_indexed_keys,
        indexed_bindings,
        y_scalars,
        p_scalars,
    )
    .map_err(|err| {
        layout_optional_contract_violation(
            format!("invalid solve variable layout contract: {err}"),
            err.source_span(),
        )
    })
}

fn layout_contract_violation(reason: impl Into<String>, span: Span) -> LowerError {
    let reason = reason.into();
    if span.is_dummy() {
        LowerError::UnspannedContractViolation { reason }
    } else {
        LowerError::ContractViolation { reason, span }
    }
}

fn layout_optional_contract_violation(reason: impl Into<String>, span: Option<Span>) -> LowerError {
    let reason = reason.into();
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => LowerError::ContractViolation { reason, span },
        None => LowerError::UnspannedContractViolation { reason },
    }
}

fn map_y_bindings(
    dae_model: &dae::Dae,
    bindings: &mut IndexMap<String, ScalarSlot>,
    shapes: &mut IndexMap<String, Vec<usize>>,
    shape_spans: &mut IndexMap<String, Span>,
    shape_indexed_keys: &mut IndexMap<String, ComponentReferenceKey>,
    indexed_bindings: &mut IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    solver_len: usize,
) -> Result<usize, LowerError> {
    let mut offset = 0usize;
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        if is_runtime_parameter_tail_variable(dae_model, name) {
            continue;
        }
        if offset >= solver_len {
            break;
        }
        let size = variable_size(name.as_str(), var)?;
        let visible_size = size.min(solver_len - offset);
        let consumed = insert_var_bindings_limited(
            LayoutBindingMaps {
                bindings: &mut *bindings,
                shapes: &mut *shapes,
                shape_spans: &mut *shape_spans,
                shape_indexed_keys: &mut *shape_indexed_keys,
                indexed_bindings: &mut *indexed_bindings,
            },
            name.as_str(),
            var,
            SlotStorage::Y,
            offset,
            visible_size,
        )?;
        offset = add_slot_count(offset, consumed, name.as_str(), var.source_span)?;
        if visible_size < size {
            break;
        }
    }
    Ok(offset)
}

pub(crate) fn is_runtime_parameter_tail_variable(
    dae_model: &dae::Dae,
    name: &rumoca_core::VarName,
) -> bool {
    dae_model.variables.parameters.contains_key(name)
        || dae_model.variables.inputs.contains_key(name)
        || dae_model.variables.discrete_reals.contains_key(name)
        || dae_model.variables.discrete_valued.contains_key(name)
}

fn map_p_bindings(
    dae_model: &dae::Dae,
    mut maps: LayoutBindingMaps<'_>,
) -> Result<usize, LowerError> {
    let mut offset = 0usize;
    for (name, var) in dae_model
        .variables
        .parameters
        .iter()
        .chain(dae_model.variables.inputs.iter())
        .chain(dae_model.variables.discrete_reals.iter())
        .chain(dae_model.variables.discrete_valued.iter())
    {
        let consumed =
            insert_var_bindings(maps.reborrow(), name.as_str(), var, SlotStorage::P, offset)?;
        offset = add_slot_count(offset, consumed, name.as_str(), var.source_span)?;
    }
    Ok(offset)
}

struct LayoutBindingMaps<'a> {
    bindings: &'a mut IndexMap<String, ScalarSlot>,
    shapes: &'a mut IndexMap<String, Vec<usize>>,
    shape_spans: &'a mut IndexMap<String, Span>,
    shape_indexed_keys: &'a mut IndexMap<String, ComponentReferenceKey>,
    indexed_bindings: &'a mut IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
}

struct ArraySlotBinding<'a> {
    name: &'a str,
    component_key: ComponentReferenceKey,
    dims: &'a [i64],
    size: usize,
    storage: SlotStorage,
    start_index: usize,
    span: Span,
}

impl<'a> LayoutBindingMaps<'a> {
    fn reborrow(&mut self) -> LayoutBindingMaps<'_> {
        LayoutBindingMaps {
            bindings: self.bindings,
            shapes: self.shapes,
            shape_spans: self.shape_spans,
            shape_indexed_keys: self.shape_indexed_keys,
            indexed_bindings: self.indexed_bindings,
        }
    }
}

fn map_constant_bindings(
    dae_model: &dae::Dae,
    mut maps: LayoutBindingMaps<'_>,
) -> Result<(), LowerError> {
    for _ in 0..dae_model.variables.constants.len().max(1) {
        let before = maps.bindings.len();
        for (name, var) in &dae_model.variables.constants {
            if maps.bindings.contains_key(name.as_str()) {
                continue;
            }
            insert_constant_bindings(
                maps.reborrow(),
                name.as_str(),
                var,
                &dae_model.symbols.enum_literal_ordinals,
            )?;
        }
        if maps.bindings.len() == before {
            break;
        }
    }
    Ok(())
}

fn map_enum_literal_bindings(dae_model: &dae::Dae, bindings: &mut IndexMap<String, ScalarSlot>) {
    for (name, ordinal) in &dae_model.symbols.enum_literal_ordinals {
        insert_enum_literal_binding_aliases(bindings, name, *ordinal as f64);
    }
}

fn insert_var_bindings(
    maps: LayoutBindingMaps<'_>,
    name: &str,
    var: &dae::Variable,
    storage: SlotStorage,
    start_index: usize,
) -> Result<usize, LowerError> {
    let size = variable_size(name, var)?;
    insert_var_shape(maps.shapes, maps.shape_spans, name, var)?;
    if size == 0 {
        return Ok(0);
    }

    if size <= 1 && var.dims.is_empty() {
        let slot = scalar_slot(storage, start_index, name, var.source_span)?;
        maps.bindings.insert(name.to_string(), slot);
        return Ok(1);
    }

    let component_key = variable_component_key(name, var)?;
    insert_array_slot_bindings(
        maps,
        ArraySlotBinding {
            name,
            component_key,
            dims: &var.dims,
            size,
            storage,
            start_index,
            span: var.source_span,
        },
    )?;
    Ok(size)
}

fn insert_var_bindings_limited(
    maps: LayoutBindingMaps<'_>,
    name: &str,
    var: &dae::Variable,
    storage: SlotStorage,
    start_index: usize,
    visible_size: usize,
) -> Result<usize, LowerError> {
    let size = variable_size(name, var)?;
    if visible_size == size {
        insert_var_shape(maps.shapes, maps.shape_spans, name, var)?;
    }
    if size == 0 || visible_size == 0 {
        return Ok(0);
    }

    if size <= 1 && var.dims.is_empty() {
        let slot = scalar_slot(storage, start_index, name, var.source_span)?;
        maps.bindings.insert(name.to_string(), slot);
        return Ok(1);
    }

    let component_key = variable_component_key(name, var)?;
    insert_array_slot_bindings(
        maps,
        ArraySlotBinding {
            name,
            component_key,
            dims: &var.dims,
            size: visible_size,
            storage,
            start_index,
            span: var.source_span,
        },
    )?;
    Ok(visible_size)
}

fn insert_constant_bindings(
    maps: LayoutBindingMaps<'_>,
    name: &str,
    var: &dae::Variable,
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Result<(), LowerError> {
    let Some(start) = var.start.as_ref() else {
        return Ok(());
    };
    let Some(raw_values) =
        eval_const_values_with_bindings(start, enum_literal_ordinals, &*maps.bindings)
    else {
        return Ok(());
    };

    let size = variable_size(name, var)?;
    insert_var_shape(maps.shapes, maps.shape_spans, name, var)?;
    if size == 0 {
        return Ok(());
    }
    let values = expand_values_to_size(raw_values, size, name, var.source_span)?;

    if size <= 1 && var.dims.is_empty() {
        let slot = ScalarSlot::Constant(values[0]);
        maps.bindings.insert(name.to_string(), slot);
        return Ok(());
    }

    let component_key = variable_component_key(name, var)?;
    maps.shape_indexed_keys
        .insert(name.to_string(), component_key.clone());
    insert_array_constant_bindings(
        maps.bindings,
        maps.indexed_bindings,
        name,
        component_key,
        &var.dims,
        &values,
        var.source_span,
    )?;
    Ok(())
}

fn insert_array_slot_bindings(
    maps: LayoutBindingMaps<'_>,
    binding: ArraySlotBinding<'_>,
) -> Result<(), LowerError> {
    let root_slot = scalar_slot(
        binding.storage,
        binding.start_index,
        binding.name,
        binding.span,
    )?;
    let mut indexed = Vec::new();
    reserve_indexed_slot_capacity(&mut indexed, binding.size, binding.name, binding.span)?;
    maps.bindings.insert(binding.name.to_string(), root_slot);
    for flat_index in 0..binding.size {
        let scalar_index =
            add_slot_count(binding.start_index, flat_index, binding.name, binding.span)?;
        let slot = scalar_slot(binding.storage, scalar_index, binding.name, binding.span)?;
        if let Some(indices) = dae::flat_index_to_subscripts(binding.dims, flat_index) {
            indexed.push(IndexedScalarSlot {
                indices: indices.clone(),
                slot,
            });
        }
        maps.bindings.insert(
            dae::scalar_name_text_for_flat_index(binding.name, binding.dims, flat_index),
            slot,
        );
    }
    if !indexed.is_empty() {
        maps.shape_indexed_keys
            .insert(binding.name.to_string(), binding.component_key.clone());
        maps.indexed_bindings.insert(binding.component_key, indexed);
    }
    Ok(())
}

fn insert_array_constant_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    indexed_bindings: &mut IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    name: &str,
    component_key: ComponentReferenceKey,
    dims: &[i64],
    values: &[f64],
    span: Span,
) -> Result<(), LowerError> {
    let Some(first) = values.first().copied() else {
        return Ok(());
    };
    let mut indexed = Vec::new();
    reserve_indexed_slot_capacity(&mut indexed, values.len(), name, span)?;
    bindings.insert(name.to_string(), ScalarSlot::Constant(first));
    for (flat_index, value) in values.iter().copied().enumerate() {
        if let Some(indices) = dae::flat_index_to_subscripts(dims, flat_index) {
            indexed.push(IndexedScalarSlot {
                indices: indices.clone(),
                slot: ScalarSlot::Constant(value),
            });
        }
        bindings.insert(
            dae::scalar_name_text_for_flat_index(name, dims, flat_index),
            ScalarSlot::Constant(value),
        );
    }
    if !indexed.is_empty() {
        indexed_bindings.insert(component_key, indexed);
    }
    Ok(())
}

fn variable_component_key(
    name: &str,
    var: &dae::Variable,
) -> Result<ComponentReferenceKey, LowerError> {
    // Generated DAE variables (event conditions, `__pre__` parameters) carry
    // explicit compiler identity; their structured reference exists for
    // provenance, not for source-layout keying.
    if source_free_layout_name(name, var) {
        return Ok(ComponentReferenceKey::generated(name));
    }
    if let Some(component_ref) = var.component_ref.as_ref() {
        #[cfg(test)]
        if let Some(key) = crate::test_support::fixture_key_for_component_ref(component_ref, name) {
            return Ok(key);
        }
        return ComponentReferenceKey::from_component_reference(component_ref).map_err(|err| {
            layout_contract_violation(
                format!(
                    "array variable `{name}` has non-static structured component reference: {err}"
                ),
                err.span,
            )
        });
    }
    #[cfg(test)]
    if let Some(key) = crate::test_support::fixture_key_for_variable(name, var) {
        return Ok(key);
    }
    Err(layout_contract_violation(
        format!("array variable `{name}` is missing structured component reference"),
        var.source_span,
    ))
}

fn source_free_layout_name(_name: &str, var: &dae::Variable) -> bool {
    // `ComponentReference` is required for source Modelica variables.
    // Generated DAE variables have explicit compiler identity instead.
    var.origin == dae::VariableOrigin::Generated
}

fn insert_var_shape(
    shapes: &mut IndexMap<String, Vec<usize>>,
    shape_spans: &mut IndexMap<String, Span>,
    name: &str,
    var: &dae::Variable,
) -> Result<(), LowerError> {
    if var.dims.is_empty() {
        return Ok(());
    }
    let dims = var
        .dims
        .iter()
        .map(|dim| usize::try_from(*dim).ok())
        .collect::<Option<Vec<_>>>();
    let Some(dims) = dims else {
        return Err(invalid_variable_shape(name, var));
    };
    shapes.insert(name.to_string(), dims);
    shape_spans.insert(name.to_string(), var.source_span);
    Ok(())
}

fn variable_size(name: &str, var: &dae::Variable) -> Result<usize, LowerError> {
    var.try_size()
        .map_err(|_| invalid_variable_shape(name, var))
}

fn invalid_variable_shape(name: &str, var: &dae::Variable) -> LowerError {
    layout_contract_violation(
        format!(
            "invalid DAE dimensions {} for `{}`",
            format_i64_dims(&var.dims),
            name
        ),
        var.source_span,
    )
}

fn format_i64_dims(dims: &[i64]) -> String {
    let dims = dims
        .iter()
        .map(i64::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{dims}]")
}

fn scalar_slot(
    storage: SlotStorage,
    index: usize,
    name: &str,
    span: Span,
) -> Result<ScalarSlot, LowerError> {
    let byte_offset = index
        .checked_mul(F64_BYTES)
        .ok_or_else(|| slot_byte_offset_overflow(name, span))?;
    Ok(match storage {
        SlotStorage::Y => ScalarSlot::Y { index, byte_offset },
        SlotStorage::P => ScalarSlot::P { index, byte_offset },
    })
}

fn synthetic_scalar_slot(
    storage: SlotStorage,
    index: usize,
    name: &str,
) -> Result<ScalarSlot, LowerError> {
    let byte_offset = index
        .checked_mul(F64_BYTES)
        .ok_or_else(|| unspanned_slot_byte_offset_overflow(name))?;
    Ok(match storage {
        SlotStorage::Y => ScalarSlot::Y { index, byte_offset },
        SlotStorage::P => ScalarSlot::P { index, byte_offset },
    })
}

fn add_slot_count(start: usize, count: usize, name: &str, span: Span) -> Result<usize, LowerError> {
    start
        .checked_add(count)
        .ok_or_else(|| slot_index_overflow(name, span))
}

fn add_synthetic_slot_count(start: usize, count: usize, name: &str) -> Result<usize, LowerError> {
    start
        .checked_add(count)
        .ok_or_else(|| unspanned_slot_index_overflow(name))
}

fn slot_index_overflow(name: &str, span: Span) -> LowerError {
    layout_contract_violation(
        format!("solve layout scalar slot index for `{name}` overflows"),
        span,
    )
}

fn slot_byte_offset_overflow(name: &str, span: Span) -> LowerError {
    layout_contract_violation(
        format!("solve layout scalar slot byte offset for `{name}` overflows"),
        span,
    )
}

fn unspanned_slot_index_overflow(name: &str) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: format!("solve layout scalar slot index for `{name}` overflows"),
    }
}

fn unspanned_slot_byte_offset_overflow(name: &str) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: format!("solve layout scalar slot byte offset for `{name}` overflows"),
    }
}

fn reserve_indexed_slot_capacity(
    slots: &mut Vec<IndexedScalarSlot>,
    capacity: usize,
    name: &str,
    span: Span,
) -> Result<(), LowerError> {
    slots
        .try_reserve_exact(capacity)
        .map_err(|_| indexed_slot_capacity_overflow(name, span))
}

fn indexed_slot_capacity_overflow(name: &str, span: Span) -> LowerError {
    layout_contract_violation(
        format!("solve layout indexed slot capacity for `{name}` overflows"),
        span,
    )
}

fn reserve_constant_value_capacity(
    values: &mut Vec<f64>,
    capacity: usize,
    name: &str,
    span: Span,
) -> Result<(), LowerError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| constant_value_capacity_overflow(name, span))
}

fn constant_value_capacity_overflow(name: &str, span: Span) -> LowerError {
    layout_contract_violation(
        format!("solve layout constant value capacity for `{name}` overflows"),
        span,
    )
}

fn literal_to_f64(literal: &rumoca_core::Literal) -> Option<f64> {
    match literal {
        rumoca_core::Literal::Real(v) => Some(*v),
        rumoca_core::Literal::Integer(v) => Some(*v as f64),
        rumoca_core::Literal::Boolean(v) => Some(if *v { 1.0 } else { 0.0 }),
        rumoca_core::Literal::String(_) => None,
    }
}

fn positive_i64_to_usize(value: i64) -> Option<usize> {
    if value > 0 {
        usize::try_from(value).ok()
    } else {
        None
    }
}

fn positive_usize_from_f64(value: f64) -> Option<usize> {
    if value.is_finite() && value >= 1.0 && value.fract().abs() <= f64::EPSILON {
        usize::try_from(value as u64).ok()
    } else {
        None
    }
}

fn insert_enum_literal_binding_aliases(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    value: f64,
) {
    insert_enum_literal_binding_key(bindings, name, value);
    if let Some(alternate) = alternate_enum_literal_key(name) {
        insert_enum_literal_binding_key(bindings, alternate.as_str(), value);
    }
}

fn insert_enum_literal_binding_key(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    value: f64,
) {
    bindings
        .entry(name.to_string())
        .or_insert(ScalarSlot::Constant(value));
}

#[cfg(test)]
fn eval_const_values(
    expr: &rumoca_core::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Option<Vec<f64>> {
    eval_const_values_with_bindings(expr, enum_literal_ordinals, &IndexMap::new())
}

fn eval_const_scalar_with_bindings(
    expr: &rumoca_core::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
    bindings: &IndexMap<String, ScalarSlot>,
) -> Option<f64> {
    let values = eval_const_values_with_bindings(expr, enum_literal_ordinals, bindings)?;
    if values.len() == 1 {
        return values.first().copied();
    }
    None
}

fn eval_const_values_with_bindings(
    expr: &rumoca_core::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
    bindings: &IndexMap<String, ScalarSlot>,
) -> Option<Vec<f64>> {
    match expr {
        rumoca_core::Expression::Literal { value: literal, .. } => {
            Some(vec![literal_to_f64(literal)?])
        }
        // MLS §4.9.5 / SPEC_0022 EXPR-021: enumeration literals are
        // translation-time constants with 1-based ordinal numeric semantics.
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
            let key = const_var_key(name, subscripts, bindings)?;
            if let Some(ordinal) = lookup_enum_literal_ordinal(key.as_str(), enum_literal_ordinals)
            {
                return Some(vec![ordinal as f64]);
            }
            constant_slot_value(bindings.get(key.as_str())?).map(|value| vec![value])
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            eval_const_builtin(*function, args, enum_literal_ordinals, bindings)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let values = eval_const_values_with_bindings(rhs, enum_literal_ordinals, bindings)?;
            match op {
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Some(values),
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                    Some(values.into_iter().map(|v| -v).collect())
                }
                rumoca_core::OpUnary::Not | rumoca_core::OpUnary::Empty => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_const_scalar_with_bindings(lhs, enum_literal_ordinals, bindings)?;
            let rhs = eval_const_scalar_with_bindings(rhs, enum_literal_ordinals, bindings)?;
            let value = match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => lhs + rhs,
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => lhs - rhs,
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => lhs * rhs,
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => lhs / rhs,
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => lhs.powf(rhs),
                _ => return None,
            };
            Some(vec![value])
        }
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            let mut values = Vec::new();
            for element in elements {
                values.extend(eval_const_values_with_bindings(
                    element,
                    enum_literal_ordinals,
                    bindings,
                )?);
            }
            Some(values)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            let start = eval_const_scalar_with_bindings(start, enum_literal_ordinals, bindings)?;
            let end = eval_const_scalar_with_bindings(end, enum_literal_ordinals, bindings)?;
            let step = if let Some(step_expr) = step {
                eval_const_scalar_with_bindings(step_expr, enum_literal_ordinals, bindings)?
            } else if end >= start {
                1.0
            } else {
                -1.0
            };
            if !start.is_finite()
                || !end.is_finite()
                || !step.is_finite()
                || step.abs() <= f64::EPSILON
            {
                return None;
            }

            let mut values = Vec::new();
            let mut value = start;
            let tol = step.abs() * 1.0e-9 + 1.0e-12;
            for _ in 0..100_000 {
                let is_past_end =
                    (step > 0.0 && value > end + tol) || (step < 0.0 && value < end - tol);
                if is_past_end {
                    break;
                }
                values.push(value);
                value += step;
                if !value.is_finite() {
                    return None;
                }
            }
            Some(values)
        }
        _ => None,
    }
}

fn eval_const_builtin(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    enum_literal_ordinals: &IndexMap<String, i64>,
    bindings: &IndexMap<String, ScalarSlot>,
) -> Option<Vec<f64>> {
    use rumoca_core::BuiltinFunction as Builtin;

    let unary = |f: fn(f64) -> f64| {
        let value =
            eval_const_scalar_with_bindings(args.first()?, enum_literal_ordinals, bindings)?;
        Some(vec![f(value)])
    };
    let binary = |f: fn(f64, f64) -> f64| {
        let lhs = eval_const_scalar_with_bindings(args.first()?, enum_literal_ordinals, bindings)?;
        let rhs = eval_const_scalar_with_bindings(args.get(1)?, enum_literal_ordinals, bindings)?;
        Some(vec![f(lhs, rhs)])
    };
    let binary_builtin = |function| {
        let lhs = eval_const_scalar_with_bindings(args.first()?, enum_literal_ordinals, bindings)?;
        let rhs = eval_const_scalar_with_bindings(args.get(1)?, enum_literal_ordinals, bindings)?;
        rumoca_core::apply_scalar_binary_math(function, lhs, rhs).map(|value| vec![value])
    };

    match function {
        Builtin::Abs => unary(f64::abs),
        Builtin::Sign => unary(f64::signum),
        Builtin::Sqrt => unary(f64::sqrt),
        Builtin::Floor => unary(f64::floor),
        Builtin::Ceil => unary(f64::ceil),
        Builtin::Sin => unary(f64::sin),
        Builtin::Cos => unary(f64::cos),
        Builtin::Tan => unary(f64::tan),
        Builtin::Asin => unary(f64::asin),
        Builtin::Acos => unary(f64::acos),
        Builtin::Atan => unary(f64::atan),
        Builtin::Sinh => unary(f64::sinh),
        Builtin::Cosh => unary(f64::cosh),
        Builtin::Tanh => unary(f64::tanh),
        Builtin::Exp => unary(f64::exp),
        Builtin::Log => unary(f64::ln),
        Builtin::Log10 => unary(f64::log10),
        Builtin::Integer => unary(f64::trunc),
        Builtin::Atan2 => binary(f64::atan2),
        Builtin::Min => binary(f64::min),
        Builtin::Max => binary(f64::max),
        Builtin::Div => binary_builtin(Builtin::Div),
        Builtin::Mod => binary_builtin(Builtin::Mod),
        Builtin::Rem => binary_builtin(Builtin::Rem),
        Builtin::NoEvent => {
            eval_const_values_with_bindings(args.first()?, enum_literal_ordinals, bindings)
        }
        Builtin::Smooth => {
            eval_const_values_with_bindings(args.get(1)?, enum_literal_ordinals, bindings)
        }
        Builtin::Homotopy => {
            eval_const_values_with_bindings(args.first()?, enum_literal_ordinals, bindings)
        }
        _ => None,
    }
}

fn constant_slot_value(slot: &ScalarSlot) -> Option<f64> {
    match slot {
        ScalarSlot::Constant(value) => Some(*value),
        _ => None,
    }
}

fn const_var_key(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    bindings: &IndexMap<String, ScalarSlot>,
) -> Option<String> {
    let name = constant_reference_lookup_name(name, bindings);
    if subscripts.is_empty() {
        return Some(name);
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for subscript in subscripts {
        let index = match subscript {
            rumoca_core::Subscript::Index { value, .. } => positive_i64_to_usize(*value)?,
            rumoca_core::Subscript::Expr { expr, .. } => positive_usize_from_f64(
                eval_const_scalar_with_bindings(expr, &IndexMap::new(), bindings)?,
            )?,
            rumoca_core::Subscript::Colon { .. } => return None,
        };
        indices.push(index);
    }
    Some(dae::format_subscript_key(name.as_str(), &indices))
}

fn constant_reference_lookup_name(
    name: &rumoca_core::Reference,
    bindings: &IndexMap<String, ScalarSlot>,
) -> String {
    if let Some(component_ref) = name.component_ref() {
        let structured = component_ref_flat_name(component_ref);
        if bindings.contains_key(structured.as_str()) || structured != name.as_str() {
            return structured;
        }
    }
    name.as_str().to_string()
}

fn component_ref_flat_name(component_ref: &rumoca_core::ComponentReference) -> String {
    component_ref
        .parts
        .iter()
        .map(|part| part.ident.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

fn lookup_enum_literal_ordinal(raw: &str, ordinals: &IndexMap<String, i64>) -> Option<i64> {
    if let Some(&ordinal) = ordinals.get(raw) {
        return Some(ordinal);
    }
    let alternate = alternate_enum_literal_key(raw)?;
    ordinals.get(&alternate).copied()
}

fn alternate_enum_literal_key(raw: &str) -> Option<String> {
    let (prefix, literal) = crate::path_utils::scope_split(raw)?;
    if literal.len() >= 2 && literal.starts_with('\'') && literal.ends_with('\'') {
        return Some(format!("{prefix}.{}", &literal[1..literal.len() - 1]));
    }
    Some(format!("{prefix}.'{literal}'"))
}

fn expand_values_to_size(
    raw_values: Vec<f64>,
    size: usize,
    name: &str,
    span: Span,
) -> Result<Vec<f64>, LowerError> {
    if size == 0 {
        return Ok(Vec::new());
    }
    if raw_values.len() == size {
        return Ok(raw_values);
    }

    let mut expanded = Vec::new();
    reserve_constant_value_capacity(&mut expanded, size, name, span)?;
    if raw_values.is_empty() {
        expanded.resize(size, 0.0);
        return Ok(expanded);
    }
    if raw_values.len() == 1 {
        expanded.resize(size, raw_values[0]);
        return Ok(expanded);
    }

    let Some(last) = raw_values.last().copied() else {
        return Err(layout_contract_violation(
            format!("constant `{name}` expansion missing tail value"),
            span,
        ));
    };
    for idx in 0..size {
        if idx < raw_values.len() {
            expanded.push(raw_values[idx]);
        } else {
            expanded.push(last);
        }
    }
    Ok(expanded)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            1,
            2,
        )
    }

    fn unspanned_layout_test_span() -> Span {
        Span::DUMMY
    }

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: test_span(),
        }
    }

    fn builtin(function: rumoca_core::BuiltinFunction) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function,
            args: vec![real(-5.5), real(2.0)],
            span: test_span(),
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated(name),
            subscripts: Vec::new(),
            span: test_span(),
        }
    }

    fn constant_variable(
        name: &str,
        start: rumoca_core::Expression,
    ) -> (rumoca_core::VarName, dae::Variable) {
        (
            rumoca_core::VarName::new(name),
            dae::Variable {
                name: rumoca_core::VarName::new(name),
                start: Some(start),
                ..rumoca_ir_dae::Variable::empty_with_span(test_span())
            },
        )
    }

    #[test]
    fn const_builtin_div_mod_and_rem_follow_modelica_rounding() {
        let ordinals = IndexMap::new();
        assert_eq!(
            eval_const_values(&builtin(rumoca_core::BuiltinFunction::Div), &ordinals),
            Some(vec![-2.0])
        );
        assert_eq!(
            eval_const_values(&builtin(rumoca_core::BuiltinFunction::Mod), &ordinals),
            Some(vec![0.5])
        );
        assert_eq!(
            eval_const_values(&builtin(rumoca_core::BuiltinFunction::Rem), &ordinals),
            Some(vec![-1.5])
        );
    }

    #[test]
    fn build_var_layout_resolves_constant_alias_chains() {
        let mut dae_model = dae::Dae::default();
        let (alias_name, alias_var) =
            constant_variable("sineVoltage.pi", var_ref("Modelica.Constants.pi"));
        let (source_name, source_var) =
            constant_variable("Modelica.Constants.pi", real(std::f64::consts::PI));
        dae_model.variables.constants.insert(alias_name, alias_var);
        dae_model
            .variables
            .constants
            .insert(source_name, source_var);

        let layout = build_var_layout(&dae_model).expect("constant alias layout should build");

        assert_eq!(
            layout.binding("sineVoltage.pi"),
            Some(ScalarSlot::Constant(std::f64::consts::PI))
        );
    }

    #[test]
    fn build_var_layout_indexes_arrays_by_component_reference() {
        let span = test_span();
        let component_ref = rumoca_core::ComponentReference {
            local: false,
            span,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "plant".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "motor".to_string(),
                    span,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "tau".to_string(),
                    span,
                    subs: Vec::new(),
                },
            ],
            def_id: Some(rumoca_core::DefId::new(42)),
        };
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("flat_display_is_not_authoritative"),
            dae::Variable {
                name: rumoca_core::VarName::new("flat_display_is_not_authoritative"),
                component_ref: Some(component_ref.clone()),
                dims: vec![2],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
        let component_key =
            ComponentReferenceKey::from_component_reference(&component_ref).unwrap();
        let display_key = ComponentReferenceKey::generated("flat_display_is_not_authoritative");

        assert_eq!(layout.indexed_bindings()[&component_key].len(), 2);
        assert!(!layout.indexed_bindings().contains_key(&display_key));
    }

    #[test]
    fn build_var_layout_rejects_source_array_without_component_reference() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            3,
            8,
        );
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("a"),
            dae::Variable {
                name: rumoca_core::VarName::new("a"),
                source_span: span,
                dims: vec![2],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let err = build_var_layout(&dae_model)
            .expect_err("source arrays must carry structured component references");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason()
                .contains("array variable `a` is missing structured component reference"),
            "{err:?}"
        );
    }

    #[test]
    fn build_var_layout_preserves_zero_size_array_shape_without_scalar_slot() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            5,
            11,
        );
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("empty"),
            dae::Variable {
                name: rumoca_core::VarName::new("empty"),
                source_span: span,
                dims: vec![0],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let layout = build_var_layout(&dae_model).expect("zero-size array layout should build");

        assert_eq!(layout.shape("empty"), Some([0].as_slice()));
        assert_eq!(layout.shape_span("empty"), Some(span));
        assert_eq!(layout.binding("empty"), None);
        assert!(layout.indexed_bindings().is_empty());
        assert_eq!(layout.y_scalars(), 0);
    }

    #[test]
    fn insert_var_bindings_reports_slot_byte_offset_overflow_with_source_span() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            7,
            14,
        );
        let var = dae::Variable {
            name: rumoca_core::VarName::new("huge"),
            source_span: span,
            ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(file!()),
                1,
                2,
            ))
        };
        let mut bindings = IndexMap::new();
        let mut shapes = IndexMap::new();
        let mut shape_spans = IndexMap::new();
        let mut shape_indexed_keys = IndexMap::new();
        let mut indexed_bindings = IndexMap::new();

        let err = insert_var_bindings(
            LayoutBindingMaps {
                bindings: &mut bindings,
                shapes: &mut shapes,
                shape_spans: &mut shape_spans,
                shape_indexed_keys: &mut shape_indexed_keys,
                indexed_bindings: &mut indexed_bindings,
            },
            "huge",
            &var,
            SlotStorage::Y,
            usize::MAX / F64_BYTES + 1,
        )
        .expect_err("oversized scalar slot byte offset must fail layout construction");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout scalar slot byte offset for `huge` overflows"
        );
    }

    #[test]
    fn add_slot_count_reports_overflow_with_source_span() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            11,
            19,
        );

        let err = add_slot_count(usize::MAX, 1, "tail", span)
            .expect_err("slot count overflow must fail layout construction");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout scalar slot index for `tail` overflows"
        );
    }

    #[test]
    fn scalar_slot_reports_byte_offset_overflow_without_dummy_span() {
        let err = scalar_slot(
            SlotStorage::Y,
            usize::MAX / F64_BYTES + 1,
            "dummy_source",
            unspanned_layout_test_span(),
        )
        .expect_err("dummy-span scalar slot byte offset overflow must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout scalar slot byte offset for `dummy_source` overflows"
        );
    }

    #[test]
    fn synthetic_scalar_slot_reports_byte_offset_overflow_without_source_span() {
        let err = synthetic_scalar_slot(
            SlotStorage::P,
            usize::MAX / F64_BYTES + 1,
            INITIAL_EVENT_PARAMETER_NAME,
        )
        .expect_err("synthetic slot byte offset overflow must fail layout construction");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout scalar slot byte offset for `__rumoca.initial_event` overflows"
        );
    }

    #[test]
    fn add_synthetic_slot_count_reports_overflow_without_source_span() {
        let err = add_synthetic_slot_count(usize::MAX, 1, INITIAL_EVENT_PARAMETER_NAME)
            .expect_err("synthetic slot count overflow must fail layout construction");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout scalar slot index for `__rumoca.initial_event` overflows"
        );
    }

    #[test]
    fn insert_array_slot_bindings_reports_capacity_overflow_with_source_span() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            2,
            9,
        );
        let mut bindings = IndexMap::new();
        let mut shapes = IndexMap::new();
        let mut shape_spans = IndexMap::new();
        let mut shape_indexed_keys = IndexMap::new();
        let mut indexed_bindings = IndexMap::new();

        let err = insert_array_slot_bindings(
            LayoutBindingMaps {
                bindings: &mut bindings,
                shapes: &mut shapes,
                shape_spans: &mut shape_spans,
                shape_indexed_keys: &mut shape_indexed_keys,
                indexed_bindings: &mut indexed_bindings,
            },
            ArraySlotBinding {
                name: "huge_array",
                component_key: ComponentReferenceKey::generated("huge_array"),
                dims: &[1],
                size: usize::MAX,
                storage: SlotStorage::Y,
                start_index: 0,
                span,
            },
        )
        .expect_err("oversized indexed slot capacity must fail layout construction");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout indexed slot capacity for `huge_array` overflows"
        );
        assert!(bindings.is_empty());
        assert!(indexed_bindings.is_empty());
    }

    #[test]
    fn reserve_indexed_slot_capacity_reports_overflow_without_dummy_span() {
        let mut slots = Vec::new();

        let err = reserve_indexed_slot_capacity(
            &mut slots,
            usize::MAX,
            "dummy_indexed",
            unspanned_layout_test_span(),
        )
        .expect_err("dummy-span indexed slot capacity overflow must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout indexed slot capacity for `dummy_indexed` overflows"
        );
    }

    #[test]
    fn expand_values_to_size_reports_capacity_overflow_with_source_span() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            4,
            13,
        );

        let err = expand_values_to_size(vec![1.0], usize::MAX, "huge_constant", span)
            .expect_err("oversized constant value expansion must fail layout construction");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout constant value capacity for `huge_constant` overflows"
        );
    }

    #[test]
    fn expand_values_to_size_reports_capacity_overflow_without_dummy_span() {
        let err = expand_values_to_size(
            vec![1.0],
            usize::MAX,
            "dummy_constant",
            unspanned_layout_test_span(),
        )
        .expect_err("dummy-span constant value expansion must fail layout construction");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: solve layout constant value capacity for `dummy_constant` overflows"
        );
    }

    #[test]
    fn expand_values_to_size_preserves_broadcast_and_padding_behavior() {
        let broadcast = expand_values_to_size(vec![2.5], 3, "constant", test_span());
        let Ok(broadcast) = broadcast else {
            panic!("scalar constant should broadcast");
        };
        assert_eq!(broadcast, vec![2.5, 2.5, 2.5]);
        let padded = expand_values_to_size(vec![1.0, 2.0], 4, "constant", test_span());
        let Ok(padded) = padded else {
            panic!("short explicit constant should pad with the last value");
        };
        assert_eq!(padded, vec![1.0, 2.0, 2.0, 2.0]);
    }

    #[test]
    fn build_var_layout_reports_invalid_dims_with_source_span() {
        let span = Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_layout_fixture.mo"),
            7,
            14,
        );
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("bad"),
            dae::Variable {
                name: rumoca_core::VarName::new("bad"),
                source_span: span,
                dims: vec![2, -1],
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let err = build_var_layout(&dae_model)
            .expect_err("negative DAE dimensions must fail layout construction");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: invalid DAE dimensions [2, -1] for `bad`"
        );
    }

    #[test]
    fn build_var_layout_reports_invalid_dims_without_dummy_span() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("bad"),
            dae::Variable {
                name: rumoca_core::VarName::new("bad"),
                source_span: unspanned_layout_test_span(),
                dims: vec![2, -1],
                component_ref: Some(crate::test_support::component_ref_from_name("bad")),
                ..rumoca_ir_dae::Variable::empty_with_span(rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ))
            },
        );

        let err = build_var_layout(&dae_model)
            .expect_err("negative DAE dimensions must fail layout construction");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: invalid DAE dimensions [2, -1] for `bad`"
        );
    }
}
