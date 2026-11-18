//! Translation-time parameter binding and structural-evaluation facts.

use super::*;

pub(super) struct ParameterBindings {
    pub(super) constants: EvalContext,
    pub(super) evaluable: HashSet<VarName>,
}

impl ParameterBindings {
    pub(super) fn evaluate(flat: &flat::Model) -> Result<Self, ToDaeError> {
        Ok(Self {
            constants: fold_parameter_bindings(flat)?,
            evaluable: evaluable_parameters(flat),
        })
    }
}

/// Fold every `constant`/`parameter` binding the Flat model settles at
/// translation time.
///
/// The fixed point is keyed on the exact occurrence identity of each
/// declaration — `validate_flat_shape` has already proven every Flat variable
/// carries a distinct allocated [`InstanceId`], so "has this declaration been
/// settled" is a question about the identity and never about a rendered name
/// two occurrences could share.
///
/// A binding that does not fold is only skipped when the evaluator says *why*
/// in typed terms: MLS §4.4 permits a parameter value to be established during
/// initialization instead, and those failures carry a
/// `RuntimeDependentReason`. Any other failure proves the model or the
/// evaluator wrong and is reported at the binding.
pub(super) fn fold_parameter_bindings(flat: &flat::Model) -> Result<EvalContext, ToDaeError> {
    let mut context = seeded_eval_context(flat);
    for _ in 0..flat.variables.len() {
        let mut progress = false;
        for (name, variable) in &flat.variables {
            if context.instance_value(variable.instance_id).is_some()
                || !matches!(
                    variable.variability,
                    Variability::Constant(_) | Variability::Parameter(_)
                )
                // Parameter `fixed` is uniform here (flatten refuses non-uniform
                // parameter arrays, EF033), so this reduction is exact.
                || matches!(variable.variability, Variability::Parameter(_))
                    && variable.fixed_uniform() == Some(false)
            {
                continue;
            }
            let Some(binding) = &variable.binding else {
                continue;
            };
            match eval_expr(binding, &context) {
                Ok(value) => {
                    context.add_instance_parameter(variable.instance_id, name.to_string(), value);
                    progress = true;
                }
                Err(error) if error.runtime_dependent_reason().is_some() => {}
                Err(error) => {
                    return Err(ToDaeError::unsupported_flat(
                        "parameter binding",
                        format!("`{name}` cannot be evaluated: {error}"),
                        error.span().unwrap_or(variable.source_span),
                    ));
                }
            }
        }
        if !progress {
            break;
        }
    }
    Ok(context)
}

/// An evaluation context holding the model's functions, array dimensions, and
/// enumeration ordinals, with no variable values yet.
fn seeded_eval_context(flat: &flat::Model) -> EvalContext {
    let mut context = EvalContext::with_capacity(flat.variables.len(), 0, flat.functions.len() * 2);
    if flat.predefined_types.is_complete() {
        context.set_predefined_types(flat.predefined_types);
    }
    for function in flat.functions.values() {
        context.add_function(function.clone());
    }
    for (name, variable) in &flat.variables {
        context.add_array_dimensions(name.to_string(), variable.dims.clone());
    }
    // MLS §4.8.5.2: an enumeration literal's semantic identity is its ordinal,
    // and both `Integer(...)` and the relational operators are defined on that
    // ordinal. Seeding the constant table with the model's exact ordinals is
    // what lets a parameter expression over an enumeration (such as the
    // `resolution < Resolution.s` guard of a periodic clock) evaluate.
    for (literal, ordinal) in &flat.enum_literal_ordinals {
        context.add_parameter(literal.clone(), EvalValue::Integer(*ordinal));
    }
    context
}

/// Parameters STRUCT-T10(a) may fold (MLS §4.5, §18.6).
///
/// A parameter qualifies when it keeps the default `fixed=true`, does not
/// write `Evaluate=false`, and its binding evaluates from constants and other
/// qualifying parameters alone, and either it is `final` or `Evaluate=true`,
/// or its binding reads a qualifying parameter. The second kind is a
/// dependent parameter determined solely by translation-time values. An
/// ordinary independent parameter is never admitted to the context, so a
/// binding that reads one stays unevaluated here and its parameter keeps its
/// runtime meaning.
fn evaluable_parameters(flat: &flat::Model) -> HashSet<VarName> {
    let mut context = seeded_eval_context(flat);
    let mut evaluable = HashSet::new();
    for _ in 0..flat.variables.len() {
        let mut progress = false;
        for (name, variable) in &flat.variables {
            let admitted = match variable.variability {
                Variability::Constant(_) => true,
                Variability::Parameter(_) => {
                    !variable.evaluate_refused && variable.fixed_uniform() != Some(false)
                }
                _ => false,
            };
            if !admitted || context.instance_value(variable.instance_id).is_some() {
                continue;
            }
            let Some(binding) = &variable.binding else {
                continue;
            };
            if matches!(variable.variability, Variability::Parameter(_))
                && !reads_only_evaluable(flat, binding, &evaluable, variable.evaluate)
            {
                continue;
            }
            let Ok(value) = eval_expr(binding, &context)
                .and_then(|value| declared_parameter_value(flat, variable, value))
            else {
                continue;
            };
            context.add_instance_parameter(variable.instance_id, name.to_string(), value);
            if matches!(variable.variability, Variability::Parameter(_)) {
                evaluable.insert(name.clone());
            }
            progress = true;
        }
        if !progress {
            break;
        }
    }
    evaluable
}

/// Whether every variable `binding` reads by value is a constant or an
/// evaluable parameter, and, unless `marked` (`final` or `Evaluate=true`), at
/// least one is an evaluable parameter. A `size(a, k)` argument reads only the
/// shape of `a`. This mirrors the reads DAE construction checks for the
/// `evaluable` attribute, so a branch of a conditional binding counts even when
/// translation would never select it.
fn reads_only_evaluable(
    flat: &flat::Model,
    binding: &Expression,
    evaluable: &HashSet<VarName>,
    marked: bool,
) -> bool {
    let mut reads = ValueReads::default();
    rumoca_core::ExpressionVisitor::visit_expression(&mut reads, binding);
    let mut dependent = marked;
    for read in &reads.names {
        match flat.variables.get(read) {
            Some(variable) if matches!(variable.variability, Variability::Constant(_)) => {}
            Some(_) if evaluable.contains(read) => dependent = true,
            Some(_) => return false,
            None => {}
        }
    }
    dependent
}
