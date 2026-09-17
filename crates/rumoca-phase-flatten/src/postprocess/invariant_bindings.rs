//! Specialize fixed scalar bindings after exact occurrence canonicalization.

use std::borrow::Cow;

use rumoca_core::{
    Expression, FallibleExpressionRewriter, Function, InstanceId, Literal, Reference, Span,
    Subscript, Variability,
};
use rumoca_eval_flat::constant::{DeferredParameterSource, EvalEnvironment, Value, eval_expr};
use rumoca_ir_flat as flat;
use rustc_hash::FxHashMap;

pub(crate) fn fold_invariant_scalar_bindings(flat: &mut flat::Model) {
    let mut evaluator = BindingEvaluator::new(flat);
    for variable in flat.variables.values() {
        evaluator.value(variable.instance_id);
    }
    let values = evaluator.values;
    for variable in flat.variables.values_mut() {
        let Some(Some(value)) = values.get(&variable.instance_id) else {
            continue;
        };
        let Some(span) = variable.binding.as_ref().and_then(Expression::span) else {
            continue;
        };
        variable.binding = Some(Expression::Literal {
            value: value.clone(),
            span,
        });
    }
}

struct BindingEvaluator<'a> {
    variables: FxHashMap<InstanceId, Option<&'a flat::Variable>>,
    functions: FunctionInventory<'a>,
    // None also marks an evaluation in progress, so cyclic bindings cannot fold.
    values: FxHashMap<InstanceId, Option<Literal>>,
}

impl<'a> BindingEvaluator<'a> {
    fn new(flat: &'a flat::Model) -> Self {
        let mut variables = FxHashMap::default();
        for variable in flat.variables.values() {
            variables
                .entry(variable.instance_id)
                .and_modify(|entry| *entry = None)
                .or_insert(Some(variable));
        }
        Self {
            variables,
            functions: FunctionInventory(flat),
            values: FxHashMap::default(),
        }
    }

    fn value(&mut self, id: InstanceId) -> Option<Literal> {
        if let Some(value) = self.values.get(&id) {
            return value.clone();
        }
        self.values.insert(id, None);
        let value = self.evaluate_binding(id);
        self.values.insert(id, value.clone());
        value
    }

    fn evaluate_binding(&mut self, id: InstanceId) -> Option<Literal> {
        let variable = self.variables.get(&id).copied().flatten()?;
        let invariant = match variable.variability {
            Variability::Constant(_) => true,
            Variability::Parameter(_) => variable.evaluate,
            _ => false,
        };
        // Guarded to scalars (`dims` empty), so `fixed` holds a single element
        // and the reduction is exact; it also only reaches parameters/constants.
        if !invariant || !variable.dims.is_empty() || variable.fixed_uniform() == Some(false) {
            return None;
        }
        let binding = self.rewrite_expression(variable.binding.as_ref()?).ok()?;
        match eval_expr(&binding, &self.functions).ok()? {
            Value::Real(value) if value.is_finite() => Some(Literal::Real(value)),
            Value::Integer(value) => Some(Literal::Integer(value)),
            Value::Bool(value) => Some(Literal::Boolean(value)),
            Value::String(value) => Some(Literal::String(value)),
            _ => None,
        }
    }
}

impl FallibleExpressionRewriter for BindingEvaluator<'_> {
    type Error = ();

    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Result<Expression, Self::Error> {
        if !subscripts.is_empty() {
            return Err(());
        }
        let id = name.instance_id().ok_or(())?;
        let variable = self.variables.get(&id).copied().flatten().ok_or(())?;
        let declaration = variable.component_ref.as_ref().ok_or(())?.target_def_id();
        if name.component_ref().ok_or(())?.target_def_id() != declaration {
            return Err(());
        }
        Ok(Expression::Literal {
            value: self.value(id).ok_or(())?,
            span,
        })
    }
}

/// Model values are supplied only by exact, recursively proven substitutions.
/// Function locals and recursion remain owned by the constant interpreter.
struct FunctionInventory<'a>(&'a flat::Model);

impl EvalEnvironment for FunctionInventory<'_> {
    fn get_value(&self, _name: &str) -> Option<Cow<'_, Value>> {
        None
    }

    fn get_enum(&self, _name: &str) -> Option<&(String, String)> {
        None
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.0
            .functions
            .values()
            .find(|function| function.name.as_str() == name)
    }

    fn get_array_dimensions(&self, _name: &str) -> Option<&[i64]> {
        None
    }

    fn deferred_parameter(&self, _name: &str) -> Option<DeferredParameterSource> {
        None
    }
}
