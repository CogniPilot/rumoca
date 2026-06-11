use indexmap::IndexMap;
use rumoca_core::Span;
use rumoca_ir_dae as dae;

use crate::lower::LowerError;
use rumoca_ir_solve::{
    ComponentReferenceKey, IndexedScalarSlot, ScalarSlot, VarLayout, scalar_slot_p, scalar_slot_y,
};

pub(crate) const INITIAL_EVENT_PARAMETER_NAME: &str = "__rumoca.initial_event";

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
        scalar_slot_p(p_scalars),
    );
    p_scalars += 1;
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

    VarLayout::try_from_parts_with_shapes_spans_keys_and_indexed_bindings(
        bindings,
        shapes,
        shape_spans,
        shape_indexed_keys,
        indexed_bindings,
        y_scalars,
        p_scalars,
    )
    .map_err(|err| LowerError::ContractViolation {
        reason: format!("invalid solve variable layout contract: {err:?}"),
        span: err.span(),
    })
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
        offset += insert_var_bindings_limited(
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
        offset += insert_var_bindings(maps.reborrow(), name.as_str(), var, SlotStorage::P, offset)?;
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
    for (name, var) in &dae_model.variables.constants {
        insert_constant_bindings(
            maps.reborrow(),
            name.as_str(),
            var,
            &dae_model.symbols.enum_literal_ordinals,
        )?;
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
        let slot = scalar_slot(storage, start_index);
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
        },
    );
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
        let slot = scalar_slot(storage, start_index);
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
        },
    );
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
    let Some(raw_values) = eval_const_values(start, enum_literal_ordinals) else {
        return Ok(());
    };

    let size = variable_size(name, var)?;
    insert_var_shape(maps.shapes, maps.shape_spans, name, var)?;
    if size == 0 {
        return Ok(());
    }
    let values = expand_values_to_size(raw_values, size);

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
    );
    Ok(())
}

fn insert_array_slot_bindings(maps: LayoutBindingMaps<'_>, binding: ArraySlotBinding<'_>) {
    maps.bindings.insert(
        binding.name.to_string(),
        scalar_slot(binding.storage, binding.start_index),
    );
    let mut indexed = Vec::with_capacity(binding.size);
    for flat_index in 0..binding.size {
        let scalar_index = binding.start_index + flat_index;
        if let Some(indices) = dae::flat_index_to_subscripts(binding.dims, flat_index) {
            indexed.push(IndexedScalarSlot {
                indices: indices.clone(),
                slot: scalar_slot(binding.storage, scalar_index),
            });
        }
        maps.bindings.insert(
            dae::scalar_name_text_for_flat_index(binding.name, binding.dims, flat_index),
            scalar_slot(binding.storage, scalar_index),
        );
    }
    if !indexed.is_empty() {
        maps.indexed_bindings.insert(binding.component_key, indexed);
    }
}

fn insert_array_constant_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    indexed_bindings: &mut IndexMap<ComponentReferenceKey, Vec<IndexedScalarSlot>>,
    name: &str,
    component_key: ComponentReferenceKey,
    dims: &[i64],
    values: &[f64],
) {
    let Some(first) = values.first().copied() else {
        return;
    };
    bindings.insert(name.to_string(), ScalarSlot::Constant(first));
    let mut indexed = Vec::with_capacity(values.len());
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
            LowerError::ContractViolation {
                reason: format!(
                    "array variable `{name}` has non-static structured component reference"
                ),
                span: err.span,
            }
        });
    }
    #[cfg(test)]
    if let Some(key) = crate::test_support::fixture_key_for_variable(name, var) {
        return Ok(key);
    }
    Err(LowerError::ContractViolation {
        reason: format!("array variable `{name}` is missing structured component reference"),
        span: var.source_span,
    })
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
    LowerError::ContractViolation {
        reason: format!("invalid DAE dimensions {:?} for `{}`", var.dims, name),
        span: var.source_span,
    }
}

fn scalar_slot(storage: SlotStorage, index: usize) -> ScalarSlot {
    match storage {
        SlotStorage::Y => scalar_slot_y(index),
        SlotStorage::P => scalar_slot_p(index),
    }
}

fn literal_to_f64(literal: &rumoca_core::Literal) -> Option<f64> {
    match literal {
        rumoca_core::Literal::Real(v) => Some(*v),
        rumoca_core::Literal::Integer(v) => Some(*v as f64),
        rumoca_core::Literal::Boolean(v) => Some(if *v { 1.0 } else { 0.0 }),
        rumoca_core::Literal::String(_) => None,
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

fn eval_const_scalar(
    expr: &rumoca_core::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Option<f64> {
    let values = eval_const_values(expr, enum_literal_ordinals)?;
    if values.len() == 1 {
        return values.first().copied();
    }
    None
}

fn eval_const_values(
    expr: &rumoca_core::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Option<Vec<f64>> {
    match expr {
        rumoca_core::Expression::Literal { value: literal, .. } => {
            Some(vec![literal_to_f64(literal)?])
        }
        // MLS §4.9.5 / SPEC_0022 EXPR-021: enumeration literals are
        // translation-time constants with 1-based ordinal numeric semantics.
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            lookup_enum_literal_ordinal(name.as_str(), enum_literal_ordinals)
                .map(|ordinal| vec![ordinal as f64])
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            eval_const_builtin(*function, args, enum_literal_ordinals)
        }
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let values = eval_const_values(rhs, enum_literal_ordinals)?;
            match op {
                rumoca_core::OpUnary::Plus | rumoca_core::OpUnary::DotPlus => Some(values),
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                    Some(values.into_iter().map(|v| -v).collect())
                }
                rumoca_core::OpUnary::Not | rumoca_core::OpUnary::Empty => None,
            }
        }
        rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = eval_const_scalar(lhs, enum_literal_ordinals)?;
            let rhs = eval_const_scalar(rhs, enum_literal_ordinals)?;
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
                values.extend(eval_const_values(element, enum_literal_ordinals)?);
            }
            Some(values)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            let start = eval_const_scalar(start, enum_literal_ordinals)?;
            let end = eval_const_scalar(end, enum_literal_ordinals)?;
            let step = if let Some(step_expr) = step {
                eval_const_scalar(step_expr, enum_literal_ordinals)?
            } else if end >= start {
                1.0
            } else {
                -1.0
            };
            if step.abs() <= f64::EPSILON {
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
) -> Option<Vec<f64>> {
    use rumoca_core::BuiltinFunction as Builtin;

    let unary = |f: fn(f64) -> f64| {
        let value = eval_const_scalar(args.first()?, enum_literal_ordinals)?;
        Some(vec![f(value)])
    };
    let binary = |f: fn(f64, f64) -> f64| {
        let lhs = eval_const_scalar(args.first()?, enum_literal_ordinals)?;
        let rhs = eval_const_scalar(args.get(1)?, enum_literal_ordinals)?;
        Some(vec![f(lhs, rhs)])
    };
    let binary_builtin = |function| {
        let lhs = eval_const_scalar(args.first()?, enum_literal_ordinals)?;
        let rhs = eval_const_scalar(args.get(1)?, enum_literal_ordinals)?;
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
        Builtin::NoEvent => eval_const_values(args.first()?, enum_literal_ordinals),
        Builtin::Smooth => eval_const_values(args.get(1)?, enum_literal_ordinals),
        Builtin::Homotopy => eval_const_values(args.first()?, enum_literal_ordinals),
        _ => None,
    }
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

fn expand_values_to_size(raw_values: Vec<f64>, size: usize) -> Vec<f64> {
    if size == 0 {
        return Vec::new();
    }
    if raw_values.len() == size {
        return raw_values;
    }
    if raw_values.is_empty() {
        return vec![0.0; size];
    }
    if raw_values.len() == 1 {
        return vec![raw_values[0]; size];
    }

    let last = *raw_values.last().unwrap_or(&0.0);
    let mut expanded = Vec::with_capacity(size);
    for idx in 0..size {
        expanded.push(raw_values.get(idx).copied().unwrap_or(last));
    }
    expanded
}

#[cfg(test)]
mod tests {
    use super::*;

    fn real(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: Span::DUMMY,
        }
    }

    fn builtin(function: rumoca_core::BuiltinFunction) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function,
            args: vec![real(-5.5), real(2.0)],
            span: Span::DUMMY,
        }
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
    fn build_var_layout_indexes_arrays_by_component_reference() {
        let component_ref = rumoca_core::ComponentReference {
            local: false,
            span: Span::DUMMY,
            parts: vec![
                rumoca_core::ComponentRefPart {
                    ident: "plant".to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "motor".to_string(),
                    span: Span::DUMMY,
                    subs: Vec::new(),
                },
                rumoca_core::ComponentRefPart {
                    ident: "tau".to_string(),
                    span: Span::DUMMY,
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
                ..Default::default()
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
        let span = Span::from_offsets(rumoca_core::SourceId(17), 3, 8);
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("a"),
            dae::Variable {
                name: rumoca_core::VarName::new("a"),
                source_span: span,
                dims: vec![2],
                ..Default::default()
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
        let span = Span::from_offsets(rumoca_core::SourceId(18), 5, 11);
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("empty"),
            dae::Variable {
                name: rumoca_core::VarName::new("empty"),
                source_span: span,
                dims: vec![0],
                ..Default::default()
            },
        );

        let layout = build_var_layout(&dae_model).expect("zero-size array layout should build");

        assert_eq!(layout.shape("empty"), Some([0].as_slice()));
        assert_eq!(layout.shape_span("empty"), Some(span));
        assert_eq!(layout.binding("empty"), None);
        assert!(layout.indexed_bindings().is_empty());
        assert_eq!(layout.y_scalars(), 0);
    }
}
