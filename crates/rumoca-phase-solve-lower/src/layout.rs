use indexmap::IndexMap;
use rumoca_ir_dae as dae;

use rumoca_ir_solve::{ScalarSlot, VarLayout, scalar_slot_p, scalar_slot_y};

const MODELICA_CONSTANTS: &[(&str, f64)] = &[
    ("Modelica.Constants.pi", std::f64::consts::PI),
    ("Modelica.Constants.e", std::f64::consts::E),
    ("Modelica.Constants.g_n", 9.80665),
    ("Modelica.Constants.small", 1e-60),
    ("Modelica.Constants.eps", f64::EPSILON),
    ("Modelica.Constants.inf", f64::INFINITY),
    ("Modelica.Constants.sigma", 5.670374419e-8),
    ("Modelica.Constants.R", 8.314462618),
    ("Modelica.Constants.N_A", 6.02214076e23),
    ("Modelica.Constants.k", 1.380649e-23),
    ("Modelica.Constants.q", 1.602176634e-19),
    ("Modelica.Constants.h", 6.62607015e-34),
    ("Modelica.Constants.c", 299792458.0),
    ("Modelica.Constants.F", 96485.33212),
    ("Modelica.Constants.mu_0", 1.25663706212e-6),
    ("Modelica.Constants.epsilon_0", 8.8541878128e-12),
    ("Modelica.Constants.T_zero", -273.15),
];

const MODELICA_COMPLEX_CONSTANTS: &[(&str, f64)] = &[
    ("Modelica.ComplexMath.j.re", 0.0),
    ("Modelica.ComplexMath.j.im", 1.0),
    ("j.re", 0.0),
    ("j.im", 1.0),
];

#[derive(Debug, Clone, Copy)]
enum SlotStorage {
    Y,
    P,
}

pub fn build_var_layout(dae_model: &dae::Dae) -> VarLayout {
    let mut bindings = IndexMap::new();
    bindings.insert("time".to_string(), ScalarSlot::Time);

    let y_scalars = map_y_bindings(dae_model, &mut bindings);
    let p_scalars = map_p_bindings(dae_model, &mut bindings);
    map_enum_literal_bindings(dae_model, &mut bindings);
    map_constant_bindings(dae_model, &mut bindings);

    VarLayout::from_parts(bindings, y_scalars, p_scalars)
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
        insert_constant_bindings(
            bindings,
            name.as_str(),
            var,
            &dae_model.enum_literal_ordinals,
        );
    }
}

fn map_enum_literal_bindings(dae_model: &dae::Dae, bindings: &mut IndexMap<String, ScalarSlot>) {
    for (name, ordinal) in &dae_model.enum_literal_ordinals {
        insert_enum_literal_binding_aliases(bindings, name, *ordinal as f64);
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
        let slot = scalar_slot(storage, start_index);
        bindings.insert(name.to_string(), slot);
        insert_projected_field_alias(bindings, name, slot);
        return 1;
    }

    insert_array_slot_bindings(bindings, name, &var.dims, size, storage, start_index);
    size
}

fn insert_constant_bindings(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    var: &dae::Variable,
    enum_literal_ordinals: &IndexMap<String, i64>,
) {
    let Some(start) = var.start.as_ref() else {
        return;
    };
    let Some(raw_values) = eval_const_values(start, enum_literal_ordinals) else {
        return;
    };

    let size = var.size();
    if size == 0 {
        return;
    }
    let values = expand_values_to_size(raw_values, size);

    if size <= 1 && var.dims.is_empty() {
        let slot = ScalarSlot::Constant(values[0]);
        bindings.insert(name.to_string(), slot);
        insert_projected_field_alias(bindings, name, slot);
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
    match storage {
        SlotStorage::Y => scalar_slot_y(index),
        SlotStorage::P => scalar_slot_p(index),
    }
}

fn insert_projected_field_alias(
    bindings: &mut IndexMap<String, ScalarSlot>,
    name: &str,
    slot: ScalarSlot,
) {
    let Some(alias) = projected_field_alias(name) else {
        return;
    };
    bindings.entry(alias).or_insert(slot);
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
    expr: &dae::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Option<f64> {
    let values = eval_const_values(expr, enum_literal_ordinals)?;
    if values.len() == 1 {
        return values.first().copied();
    }
    None
}

fn eval_const_values(
    expr: &dae::Expression,
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Option<Vec<f64>> {
    match expr {
        dae::Expression::Literal(literal) => Some(vec![literal_to_f64(literal)?]),
        // MLS §4.9.5 / SPEC_0022 EXPR-021: enumeration literals are
        // translation-time constants with 1-based ordinal numeric semantics.
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            lookup_enum_literal_ordinal(name.as_str(), enum_literal_ordinals)
                .map(|ordinal| vec![ordinal as f64])
                .or_else(|| lookup_well_known_constant(name.as_str()).map(|value| vec![value]))
        }
        dae::Expression::FieldAccess { base, field } => {
            let base_name = match base.as_ref() {
                dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
                    Some(name.as_str())
                }
                _ => None,
            }?;
            lookup_well_known_constant(format!("{base_name}.{field}").as_str())
                .map(|value| vec![value])
        }
        dae::Expression::BuiltinCall { function, args } => {
            eval_const_builtin(*function, args, enum_literal_ordinals)
        }
        dae::Expression::Unary { op, rhs } => {
            let values = eval_const_values(rhs, enum_literal_ordinals)?;
            match op {
                rumoca_ir_core::OpUnary::Plus(_) | rumoca_ir_core::OpUnary::DotPlus(_) => {
                    Some(values)
                }
                rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => {
                    Some(values.into_iter().map(|v| -v).collect())
                }
                rumoca_ir_core::OpUnary::Not(_) | rumoca_ir_core::OpUnary::Empty => None,
            }
        }
        dae::Expression::Binary { op, lhs, rhs } => {
            let lhs = eval_const_scalar(lhs, enum_literal_ordinals)?;
            let rhs = eval_const_scalar(rhs, enum_literal_ordinals)?;
            let value = match op {
                rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => {
                    lhs + rhs
                }
                rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => {
                    lhs - rhs
                }
                rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => {
                    lhs * rhs
                }
                rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => {
                    lhs / rhs
                }
                rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => {
                    lhs.powf(rhs)
                }
                _ => return None,
            };
            Some(vec![value])
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            let mut values = Vec::new();
            for element in elements {
                values.extend(eval_const_values(element, enum_literal_ordinals)?);
            }
            Some(values)
        }
        dae::Expression::Range { start, step, end } => {
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

fn lookup_well_known_constant(name: &str) -> Option<f64> {
    MODELICA_CONSTANTS
        .iter()
        .chain(MODELICA_COMPLEX_CONSTANTS.iter())
        .find_map(|(constant_name, value)| (*constant_name == name).then_some(*value))
}

fn eval_const_builtin(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    enum_literal_ordinals: &IndexMap<String, i64>,
) -> Option<Vec<f64>> {
    use dae::BuiltinFunction as Builtin;

    let unary = |f: fn(f64) -> f64| {
        let value = eval_const_scalar(args.first()?, enum_literal_ordinals)?;
        Some(vec![f(value)])
    };
    let binary = |f: fn(f64, f64) -> f64| {
        let lhs = eval_const_scalar(args.first()?, enum_literal_ordinals)?;
        let rhs = eval_const_scalar(args.get(1)?, enum_literal_ordinals)?;
        Some(vec![f(lhs, rhs)])
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
        Builtin::Div => binary(|lhs, rhs| (lhs / rhs).floor()),
        Builtin::Mod => binary(f64::rem_euclid),
        Builtin::Rem => binary(f64::rem_euclid),
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
    let (prefix, literal) = raw.rsplit_once('.')?;
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
