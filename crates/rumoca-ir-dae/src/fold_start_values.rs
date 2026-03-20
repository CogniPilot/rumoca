//! Constant-fold parameter/state `start` expressions to numeric literals.
//!
//! Many Modelica parameters have `start` values that reference other parameters
//! (e.g., `G_T = G_T_ref` where `G_T_ref = 300.15`). Template backends (Julia MTK,
//! SymPy, etc.) need concrete numeric values. This pass iteratively evaluates
//! start expressions using a fixed-point approach, replacing evaluable expressions
//! with `Literal::Real(value)`.

use crate::{BuiltinFunction, Dae, Expression, Literal, VarName, Variable};
use rumoca_ir_core::{OpBinary, OpUnary};
use std::collections::HashMap;

/// Evaluate all parameter/state/constant start expressions to numeric literals
/// where possible. Modifies the DAE in place.
pub fn fold_start_values_to_literals(dae: &mut Dae) {
    // Phase 1: build a name→value map from constants, enum ordinals, and
    // parameter start expressions (fixed-point iteration).
    let mut values: HashMap<String, f64> = HashMap::new();

    // Seed with enum literal ordinals
    for (name, ordinal) in &dae.enum_literal_ordinals {
        values.insert(name.clone(), *ordinal as f64);
    }

    // Collect all named start bindings (constants, parameters, inputs, states,
    // discrete reals, discrete valued, algebraics, outputs)
    let bindings: Vec<(VarName, Expression)> = dae
        .constants
        .iter()
        .chain(dae.parameters.iter())
        .chain(dae.inputs.iter())
        .chain(dae.states.iter())
        .chain(dae.discrete_reals.iter())
        .chain(dae.discrete_valued.iter())
        .chain(dae.algebraics.iter())
        .chain(dae.outputs.iter())
        .filter_map(|(name, var)| {
            var.start
                .as_ref()
                .map(|expr| (name.clone(), expr.clone()))
        })
        .collect();

    // Fixed-point iteration: resolve chains like A = B, B = 3.14
    let max_passes = bindings.len().max(1) * 2;
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, expr) in &bindings {
            if values.contains_key(name.as_str()) {
                continue;
            }
            if let Some(val) = eval_const_expr(expr, &values) {
                if val.is_finite() {
                    values.insert(name.to_string(), val);
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    // Phase 2: rewrite start expressions to literals where we found values.
    // Also clear self-referencing defaults (start = VarRef(self_name)).
    let rewrite = |var: &mut Variable| {
        if let Some(ref start) = var.start {
            // Check for self-reference: start = VarRef(own_name)
            if let Expression::VarRef { name, subscripts } = start {
                if subscripts.is_empty() && name.as_str() == var.name.as_str() {
                    var.start = None;
                    return;
                }
            }
            if let Some(&val) = values.get(var.name.as_str()) {
                var.start = Some(Expression::Literal(Literal::Real(val)));
            }
        }
    };

    for var in dae.constants.values_mut() {
        rewrite(var);
    }
    for var in dae.parameters.values_mut() {
        rewrite(var);
    }
    for var in dae.states.values_mut() {
        rewrite(var);
    }
    for var in dae.inputs.values_mut() {
        rewrite(var);
    }
    for var in dae.discrete_reals.values_mut() {
        rewrite(var);
    }
    for var in dae.discrete_valued.values_mut() {
        rewrite(var);
    }
    for var in dae.algebraics.values_mut() {
        rewrite(var);
    }
    for var in dae.outputs.values_mut() {
        rewrite(var);
    }
}

// ---------------------------------------------------------------------------
// Expression evaluator (subset sufficient for start-value resolution)
// ---------------------------------------------------------------------------

fn eval_const_expr(expr: &Expression, env: &HashMap<String, f64>) -> Option<f64> {
    match expr {
        Expression::Literal(Literal::Integer(v)) => Some(*v as f64),
        Expression::Literal(Literal::Real(v)) => Some(*v),
        Expression::Literal(Literal::Boolean(v)) => Some(if *v { 1.0 } else { 0.0 }),

        Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            env.get(name.as_str()).copied().or_else(|| {
                crate::component_base_name(name.as_str())
                    .and_then(|base| env.get(&base).copied())
            })
        }

        Expression::Unary { op, rhs } => {
            let r = eval_const_expr(rhs, env)?;
            match op {
                OpUnary::Minus(_) | OpUnary::DotMinus(_) => Some(-r),
                OpUnary::Plus(_) | OpUnary::DotPlus(_) => Some(r),
                OpUnary::Not(_) => Some(if r.abs() < 1e-12 { 1.0 } else { 0.0 }),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            let l = eval_const_expr(lhs, env)?;
            let r = eval_const_expr(rhs, env)?;
            match op {
                OpBinary::Add(_) => Some(l + r),
                OpBinary::Sub(_) => Some(l - r),
                OpBinary::Mul(_) => Some(l * r),
                OpBinary::Div(_) => {
                    if r.abs() < 1e-300 {
                        None
                    } else {
                        Some(l / r)
                    }
                }
                OpBinary::Exp(_) | OpBinary::ExpElem(_) => Some(l.powf(r)),
                OpBinary::Lt(_) => Some(if l < r { 1.0 } else { 0.0 }),
                OpBinary::Le(_) => Some(if l <= r { 1.0 } else { 0.0 }),
                OpBinary::Gt(_) => Some(if l > r { 1.0 } else { 0.0 }),
                OpBinary::Ge(_) => Some(if l >= r { 1.0 } else { 0.0 }),
                OpBinary::Eq(_) => Some(if (l - r).abs() < 1e-12 { 1.0 } else { 0.0 }),
                OpBinary::Neq(_) => Some(if (l - r).abs() >= 1e-12 { 1.0 } else { 0.0 }),
                OpBinary::And(_) => {
                    Some(if l.abs() > 1e-12 && r.abs() > 1e-12 { 1.0 } else { 0.0 })
                }
                OpBinary::Or(_) => {
                    Some(if l.abs() > 1e-12 || r.abs() > 1e-12 { 1.0 } else { 0.0 })
                }
                _ => None,
            }
        }

        Expression::BuiltinCall { function, args } => {
            eval_builtin(*function, args, env)
        }

        Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            eval_named_function(short, args, env)
        }

        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                let c = eval_const_expr(cond, env)?;
                if c.abs() > 1e-12 {
                    return eval_const_expr(then_expr, env);
                }
            }
            eval_const_expr(else_branch, env)
        }

        _ => None,
    }
}

fn eval_builtin(func: BuiltinFunction, args: &[Expression], env: &HashMap<String, f64>) -> Option<f64> {
    let arg = |i: usize| args.get(i).and_then(|e| eval_const_expr(e, env));

    match func {
        BuiltinFunction::Abs => arg(0).map(f64::abs),
        BuiltinFunction::Sign => arg(0).map(f64::signum),
        BuiltinFunction::Sqrt => arg(0).map(f64::sqrt),
        BuiltinFunction::Floor => arg(0).map(f64::floor),
        BuiltinFunction::Ceil => arg(0).map(f64::ceil),
        BuiltinFunction::Sin => arg(0).map(f64::sin),
        BuiltinFunction::Cos => arg(0).map(f64::cos),
        BuiltinFunction::Tan => arg(0).map(f64::tan),
        BuiltinFunction::Asin => arg(0).map(f64::asin),
        BuiltinFunction::Acos => arg(0).map(f64::acos),
        BuiltinFunction::Atan => arg(0).map(f64::atan),
        BuiltinFunction::Atan2 => Some(arg(0)?.atan2(arg(1)?)),
        BuiltinFunction::Sinh => arg(0).map(f64::sinh),
        BuiltinFunction::Cosh => arg(0).map(f64::cosh),
        BuiltinFunction::Tanh => arg(0).map(f64::tanh),
        BuiltinFunction::Exp => arg(0).map(f64::exp),
        BuiltinFunction::Log => arg(0).map(f64::ln),
        BuiltinFunction::Log10 => arg(0).map(f64::log10),
        BuiltinFunction::Integer => arg(0).map(f64::floor),
        BuiltinFunction::Min => Some(arg(0)?.min(arg(1)?)),
        BuiltinFunction::Max => Some(arg(0)?.max(arg(1)?)),
        BuiltinFunction::Div => {
            let a = arg(0)?;
            let b = arg(1)?;
            if b.abs() < 1e-300 { None } else { Some((a / b).trunc()) }
        }
        BuiltinFunction::Mod => {
            let a = arg(0)?;
            let b = arg(1)?;
            if b.abs() < 1e-300 { None } else { Some(a - (a / b).floor() * b) }
        }
        BuiltinFunction::Rem => {
            let a = arg(0)?;
            let b = arg(1)?;
            if b.abs() < 1e-300 { None } else { Some(a - (a / b).trunc() * b) }
        }
        _ => None,
    }
}

fn eval_named_function(name: &str, args: &[Expression], env: &HashMap<String, f64>) -> Option<f64> {
    let arg = |i: usize| args.get(i).and_then(|e| eval_const_expr(e, env));
    match name {
        "Integer" | "integer" | "floor" => arg(0).map(f64::floor),
        "ceil" => arg(0).map(f64::ceil),
        "abs" => arg(0).map(f64::abs),
        "sign" | "signum" => arg(0).map(f64::signum),
        "sqrt" => arg(0).map(f64::sqrt),
        "sin" => arg(0).map(f64::sin),
        "cos" => arg(0).map(f64::cos),
        "tan" => arg(0).map(f64::tan),
        "asin" => arg(0).map(f64::asin),
        "acos" => arg(0).map(f64::acos),
        "atan" => arg(0).map(f64::atan),
        "atan2" => Some(arg(0)?.atan2(arg(1)?)),
        "exp" => arg(0).map(f64::exp),
        "log" | "ln" => arg(0).map(f64::ln),
        "log10" => arg(0).map(f64::log10),
        "min" => Some(arg(0)?.min(arg(1)?)),
        "max" => Some(arg(0)?.max(arg(1)?)),
        _ => None,
    }
}
