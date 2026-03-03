use indexmap::IndexMap;
use rumoca_ir_dae as dae;

const F64_BYTES: usize = std::mem::size_of::<f64>();

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ScalarSlot {
    Time,
    Y { index: usize, byte_offset: usize },
    P { index: usize, byte_offset: usize },
    Constant(f64),
}

#[derive(Debug, Clone, Default)]
pub struct VarLayout {
    bindings: IndexMap<String, ScalarSlot>,
    y_scalars: usize,
    p_scalars: usize,
}

#[derive(Debug, Clone, Copy)]
enum SlotStorage {
    Y,
    P,
}

impl VarLayout {
    pub fn from_dae(dae_model: &dae::Dae) -> Self {
        let mut bindings = IndexMap::new();
        bindings.insert("time".to_string(), ScalarSlot::Time);

        let y_scalars = map_y_bindings(dae_model, &mut bindings);
        let p_scalars = map_p_bindings(dae_model, &mut bindings);
        map_constant_bindings(dae_model, &mut bindings);

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
        self.bindings.get(name).copied()
    }

    pub fn y_scalars(&self) -> usize {
        self.y_scalars
    }

    pub fn p_scalars(&self) -> usize {
        self.p_scalars
    }
}

fn map_y_bindings(dae_model: &dae::Dae, bindings: &mut IndexMap<String, ScalarSlot>) -> usize {
    let mut offset = 0usize;
    for (name, var) in dae_model
        .states
        .iter()
        .chain(dae_model.algebraics.iter())
        .chain(dae_model.outputs.iter())
    {
        offset += insert_var_bindings(bindings, name.as_str(), var, SlotStorage::Y, offset);
    }
    offset
}

fn map_p_bindings(dae_model: &dae::Dae, bindings: &mut IndexMap<String, ScalarSlot>) -> usize {
    let mut offset = 0usize;
    for (name, var) in dae_model
        .parameters
        .iter()
        .chain(dae_model.inputs.iter())
        .chain(dae_model.discrete_reals.iter())
        .chain(dae_model.discrete_valued.iter())
    {
        offset += insert_var_bindings(bindings, name.as_str(), var, SlotStorage::P, offset);
    }
    offset
}

fn map_constant_bindings(dae_model: &dae::Dae, bindings: &mut IndexMap<String, ScalarSlot>) {
    for (name, var) in &dae_model.constants {
        insert_constant_bindings(bindings, name.as_str(), var);
    }
}

fn insert_var_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    var: &dae::Variable,
    storage: SlotStorage,
    start_index: usize,
) -> usize {
    let size = var.size();
    if size == 0 {
        return 0;
    }

    if size <= 1 && var.dims.is_empty() {
        bindings.insert(name.to_string(), scalar_slot(storage, start_index));
        return 1;
    }

    insert_array_slot_bindings(bindings, name, &var.dims, size, storage, start_index);
    size
}

fn insert_constant_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    var: &dae::Variable,
) {
    let Some(start) = var.start.as_ref() else {
        return;
    };
    let Some(raw_values) = eval_const_values(start) else {
        return;
    };

    let size = var.size();
    if size == 0 {
        return;
    }
    let values = expand_values_to_size(raw_values, size);

    if size <= 1 && var.dims.is_empty() {
        bindings.insert(name.to_string(), ScalarSlot::Constant(values[0]));
        return;
    }

    insert_array_constant_bindings(bindings, name, &var.dims, &values);
}

fn insert_array_slot_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    dims: &[i64],
    size: usize,
    storage: SlotStorage,
    start_index: usize,
) {
    bindings.insert(name.to_string(), scalar_slot(storage, start_index));
    for flat_index in 0..size {
        let scalar_index = start_index + flat_index;
        bindings.insert(
            format!("{name}[{}]", flat_index + 1),
            scalar_slot(storage, scalar_index),
        );
        if let Some(subs) = flat_index_to_subscripts(flat_index, dims)
            && subs.len() > 1
        {
            bindings.insert(
                format_subscript_key(name, &subs),
                scalar_slot(storage, scalar_index),
            );
        }
    }
}

fn insert_array_constant_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    dims: &[i64],
    values: &[f64],
) {
    let Some(first) = values.first().copied() else {
        return;
    };
    bindings.insert(name.to_string(), ScalarSlot::Constant(first));
    for (flat_index, value) in values.iter().copied().enumerate() {
        bindings.insert(
            format!("{name}[{}]", flat_index + 1),
            ScalarSlot::Constant(value),
        );
        if let Some(subs) = flat_index_to_subscripts(flat_index, dims)
            && subs.len() > 1
        {
            bindings.insert(
                format_subscript_key(name, &subs),
                ScalarSlot::Constant(value),
            );
        }
    }
}

fn scalar_slot(storage: SlotStorage, index: usize) -> ScalarSlot {
    let byte_offset = index.saturating_mul(F64_BYTES);
    match storage {
        SlotStorage::Y => ScalarSlot::Y { index, byte_offset },
        SlotStorage::P => ScalarSlot::P { index, byte_offset },
    }
}

fn flat_index_to_subscripts(flat_index: usize, dims: &[i64]) -> Option<Vec<usize>> {
    if dims.is_empty() {
        return None;
    }
    let mut dims_usize = Vec::with_capacity(dims.len());
    for &d in dims {
        let dim = usize::try_from(d).ok()?;
        if dim == 0 {
            return None;
        }
        dims_usize.push(dim);
    }

    let mut remainder = flat_index;
    let mut subs_rev = Vec::with_capacity(dims_usize.len());
    for dim in dims_usize.iter().rev().copied() {
        subs_rev.push((remainder % dim) + 1);
        remainder /= dim;
    }
    if remainder != 0 {
        return None;
    }
    subs_rev.reverse();
    Some(subs_rev)
}

fn format_subscript_key(name: &str, subs: &[usize]) -> String {
    let mut key = String::from(name);
    key.push('[');
    for (idx, sub) in subs.iter().enumerate() {
        if idx > 0 {
            key.push(',');
        }
        key.push_str(&sub.to_string());
    }
    key.push(']');
    key
}

fn literal_to_f64(literal: &dae::Literal) -> Option<f64> {
    match literal {
        dae::Literal::Real(v) => Some(*v),
        dae::Literal::Integer(v) => Some(*v as f64),
        dae::Literal::Boolean(v) => Some(if *v { 1.0 } else { 0.0 }),
        dae::Literal::String(_) => None,
    }
}

fn eval_const_scalar(expr: &dae::Expression) -> Option<f64> {
    let values = eval_const_values(expr)?;
    if values.len() == 1 {
        return values.first().copied();
    }
    None
}

fn eval_const_values(expr: &dae::Expression) -> Option<Vec<f64>> {
    match expr {
        dae::Expression::Literal(literal) => Some(vec![literal_to_f64(literal)?]),
        dae::Expression::Unary { op, rhs } => {
            let values = eval_const_values(rhs)?;
            match op {
                dae::OpUnary::Plus(_) | dae::OpUnary::DotPlus(_) => Some(values),
                dae::OpUnary::Minus(_) | dae::OpUnary::DotMinus(_) => {
                    Some(values.into_iter().map(|v| -v).collect())
                }
                dae::OpUnary::Not(_) | dae::OpUnary::Empty => None,
            }
        }
        dae::Expression::Binary { op, lhs, rhs } => {
            let lhs = eval_const_scalar(lhs)?;
            let rhs = eval_const_scalar(rhs)?;
            let value = match op {
                dae::OpBinary::Add(_) | dae::OpBinary::AddElem(_) => lhs + rhs,
                dae::OpBinary::Sub(_) | dae::OpBinary::SubElem(_) => lhs - rhs,
                dae::OpBinary::Mul(_) | dae::OpBinary::MulElem(_) => lhs * rhs,
                dae::OpBinary::Div(_) | dae::OpBinary::DivElem(_) => lhs / rhs,
                dae::OpBinary::Exp(_) | dae::OpBinary::ExpElem(_) => lhs.powf(rhs),
                _ => return None,
            };
            Some(vec![value])
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            let mut values = Vec::new();
            for element in elements {
                values.extend(eval_const_values(element)?);
            }
            Some(values)
        }
        dae::Expression::Range { start, step, end } => {
            let start = eval_const_scalar(start)?;
            let end = eval_const_scalar(end)?;
            let step = if let Some(step_expr) = step {
                eval_const_scalar(step_expr)?
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
    use super::{ScalarSlot, VarLayout};
    use rumoca_ir_dae as dae;

    fn scalar(name: &str) -> dae::Variable {
        dae::Variable::new(dae::VarName::new(name))
    }

    fn array(name: &str, dims: &[i64]) -> dae::Variable {
        dae::Variable {
            name: dae::VarName::new(name),
            dims: dims.to_vec(),
            ..Default::default()
        }
    }

    #[test]
    fn from_dae_maps_y_and_p_offsets_in_build_env_order() {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(dae::VarName::new("x"), scalar("x"));
        dae_model
            .states
            .insert(dae::VarName::new("xs"), array("xs", &[2]));
        dae_model
            .algebraics
            .insert(dae::VarName::new("z"), scalar("z"));
        dae_model
            .outputs
            .insert(dae::VarName::new("y"), scalar("y"));

        dae_model
            .parameters
            .insert(dae::VarName::new("p"), scalar("p"));
        dae_model
            .parameters
            .insert(dae::VarName::new("pa"), array("pa", &[2]));

        let layout = VarLayout::from_dae(&dae_model);
        assert_eq!(layout.y_scalars(), 5);
        assert_eq!(layout.p_scalars(), 3);

        assert_eq!(layout.binding("time"), Some(ScalarSlot::Time));
        assert_eq!(
            layout.binding("x"),
            Some(ScalarSlot::Y {
                index: 0,
                byte_offset: 0
            })
        );
        assert_eq!(
            layout.binding("xs"),
            Some(ScalarSlot::Y {
                index: 1,
                byte_offset: 8
            })
        );
        assert_eq!(
            layout.binding("xs[1]"),
            Some(ScalarSlot::Y {
                index: 1,
                byte_offset: 8
            })
        );
        assert_eq!(
            layout.binding("xs[2]"),
            Some(ScalarSlot::Y {
                index: 2,
                byte_offset: 16
            })
        );
        assert_eq!(
            layout.binding("z"),
            Some(ScalarSlot::Y {
                index: 3,
                byte_offset: 24
            })
        );
        assert_eq!(
            layout.binding("y"),
            Some(ScalarSlot::Y {
                index: 4,
                byte_offset: 32
            })
        );

        assert_eq!(
            layout.binding("p"),
            Some(ScalarSlot::P {
                index: 0,
                byte_offset: 0
            })
        );
        assert_eq!(
            layout.binding("pa"),
            Some(ScalarSlot::P {
                index: 1,
                byte_offset: 8
            })
        );
        assert_eq!(
            layout.binding("pa[2]"),
            Some(ScalarSlot::P {
                index: 2,
                byte_offset: 16
            })
        );
    }

    #[test]
    fn from_dae_expands_array_constants_and_inlines_values() {
        let mut dae_model = dae::Dae::default();
        dae_model.constants.insert(
            dae::VarName::new("k"),
            dae::Variable {
                name: dae::VarName::new("k"),
                start: Some(dae::Expression::Literal(dae::Literal::Real(3.5))),
                ..Default::default()
            },
        );
        dae_model.constants.insert(
            dae::VarName::new("ka"),
            dae::Variable {
                name: dae::VarName::new("ka"),
                dims: vec![2, 2],
                start: Some(dae::Expression::Array {
                    elements: vec![
                        dae::Expression::Literal(dae::Literal::Integer(1)),
                        dae::Expression::Literal(dae::Literal::Integer(2)),
                        dae::Expression::Literal(dae::Literal::Integer(3)),
                        dae::Expression::Literal(dae::Literal::Integer(4)),
                    ],
                    is_matrix: false,
                }),
                ..Default::default()
            },
        );

        let layout = VarLayout::from_dae(&dae_model);
        assert_eq!(layout.binding("k"), Some(ScalarSlot::Constant(3.5)));
        assert_eq!(layout.binding("ka"), Some(ScalarSlot::Constant(1.0)));
        assert_eq!(layout.binding("ka[1]"), Some(ScalarSlot::Constant(1.0)));
        assert_eq!(layout.binding("ka[2]"), Some(ScalarSlot::Constant(2.0)));
        assert_eq!(layout.binding("ka[3]"), Some(ScalarSlot::Constant(3.0)));
        assert_eq!(layout.binding("ka[4]"), Some(ScalarSlot::Constant(4.0)));
        assert_eq!(layout.binding("ka[1,1]"), Some(ScalarSlot::Constant(1.0)));
        assert_eq!(layout.binding("ka[1,2]"), Some(ScalarSlot::Constant(2.0)));
        assert_eq!(layout.binding("ka[2,1]"), Some(ScalarSlot::Constant(3.0)));
        assert_eq!(layout.binding("ka[2,2]"), Some(ScalarSlot::Constant(4.0)));
    }

    #[test]
    fn from_dae_maps_inputs_and_discrete_to_parameter_slots() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .parameters
            .insert(dae::VarName::new("p"), scalar("p"));
        dae_model.inputs.insert(
            dae::VarName::new("u"),
            dae::Variable {
                name: dae::VarName::new("u"),
                start: Some(dae::Expression::Literal(dae::Literal::Real(2.0))),
                ..Default::default()
            },
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("zr"),
            dae::Variable {
                name: dae::VarName::new("zr"),
                start: Some(dae::Expression::Literal(dae::Literal::Real(3.0))),
                ..Default::default()
            },
        );
        dae_model.discrete_valued.insert(
            dae::VarName::new("mi"),
            dae::Variable {
                name: dae::VarName::new("mi"),
                start: Some(dae::Expression::Literal(dae::Literal::Integer(4))),
                ..Default::default()
            },
        );

        let layout = VarLayout::from_dae(&dae_model);
        assert_eq!(layout.p_scalars(), 4);
        assert_eq!(
            layout.binding("p"),
            Some(ScalarSlot::P {
                index: 0,
                byte_offset: 0
            })
        );
        assert_eq!(
            layout.binding("u"),
            Some(ScalarSlot::P {
                index: 1,
                byte_offset: 8
            })
        );
        assert_eq!(
            layout.binding("zr"),
            Some(ScalarSlot::P {
                index: 2,
                byte_offset: 16
            })
        );
        assert_eq!(
            layout.binding("mi"),
            Some(ScalarSlot::P {
                index: 3,
                byte_offset: 24
            })
        );
    }
}
