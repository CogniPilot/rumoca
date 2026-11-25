//! Specialize fixed bindings after exact occurrence canonicalization.

use std::borrow::Cow;

use rumoca_core::{
    ArrayConstructor, BuiltinFunction, Expression, ExpressionVisitor, FallibleExpressionRewriter,
    Function, InstanceId, Literal, Reference, Span, Subscript, Variability,
};
use rumoca_eval_flat::constant::{DeferredParameterSource, EvalEnvironment, Value, eval_expr};
use rumoca_eval_flat::phase_constant::DeclaredEnumerationLiterals;
use rumoca_eval_flat::translation_reads::ResourceRoots;
use rumoca_ir_flat as flat;

use crate::Context;

mod initial_algorithms;
use rustc_hash::FxHashMap;

/// Fold the invariant bindings SPEC_0040 FLAT-C01 proves into their values.
pub(crate) fn fold_invariant_bindings(
    flat: &mut flat::Model,
    ctx: &Context,
) -> Result<(), crate::FlattenError> {
    let mut evaluator = BindingEvaluator::new(
        flat,
        &ctx.resource_roots,
        &ctx.declared_enumeration_literals,
    );
    for variable in flat.variables.values() {
        evaluator.value(variable.instance_id);
    }
    let values = evaluator.values;
    let string_bound = evaluator.string_bound;
    for variable in flat.variables.values_mut() {
        let Some(Some(value)) = values.get(&variable.instance_id) else {
            continue;
        };
        // Its value is fixed here, so it is not tunable either: setting it
        // at run time would change nothing (MLS §18.3 Evaluate).
        if string_bound.contains(&variable.instance_id) {
            variable.evaluate = true;
        }
        let Some(binding) = variable.binding.as_ref() else {
            continue;
        };
        let Some(span) = binding.span() else {
            continue;
        };
        let folded = value_expression(value, span);
        // A start value that restates the binding is the same value.
        if variable
            .start
            .as_ref()
            .is_some_and(|start| start.semantically_eq_ignoring_spans(binding))
        {
            variable.start.clone_from(&folded);
        }
        variable.binding = folded;
    }
    let inventory = FunctionInventory {
        flat,
        resources: &ctx.resource_roots,
        enumeration_literals: &ctx.declared_enumeration_literals,
    };
    let decided = initial_algorithms::decide_string_initial_algorithms(flat, &inventory)?;
    initial_algorithms::apply(flat, decided);
    Ok(())
}

struct BindingEvaluator<'a> {
    flat: &'a flat::Model,
    variables: FxHashMap<InstanceId, Option<&'a flat::Variable>>,
    functions: FunctionInventory<'a>,
    // None also marks an evaluation in progress, so cyclic bindings cannot fold.
    values: FxHashMap<InstanceId, Option<Value>>,
    /// Non-String parameters evaluated here only because their binding reads
    /// a String's value.
    string_bound: rustc_hash::FxHashSet<InstanceId>,
}

impl<'a> BindingEvaluator<'a> {
    fn new(
        flat: &'a flat::Model,
        resources: &'a ResourceRoots,
        enumeration_literals: &'a DeclaredEnumerationLiterals,
    ) -> Self {
        let mut variables = FxHashMap::default();
        for variable in flat.variables.values() {
            variables
                .entry(variable.instance_id)
                .and_modify(|entry| *entry = None)
                .or_insert(Some(variable));
        }
        Self {
            flat,
            variables,
            functions: FunctionInventory {
                flat,
                resources,
                enumeration_literals,
            },
            values: FxHashMap::default(),
            string_bound: rustc_hash::FxHashSet::default(),
        }
    }

    fn value(&mut self, id: InstanceId) -> Option<Value> {
        if let Some(value) = self.values.get(&id) {
            return value.clone();
        }
        self.values.insert(id, None);
        let value = self.evaluate_binding(id);
        self.values.insert(id, value.clone());
        value
    }

    fn evaluate_binding(&mut self, id: InstanceId) -> Option<Value> {
        let variable = self.variables.get(&id).copied().flatten()?;
        // The runtime owns no String storage (SPEC_0022 ALG-015), so a
        // String parameter is its declared value, and a parameter whose
        // binding reads a String's value can only be evaluated here.
        let string_bound = matches!(variable.variability, Variability::Parameter(_))
            && !variable.evaluate
            && !self.is_string(variable)
            && variable
                .binding
                .as_ref()
                .is_some_and(|binding| self.reads_string_value(binding));
        let invariant = match variable.variability {
            Variability::Constant(_) => true,
            Variability::Parameter(_) => {
                variable.evaluate || self.is_string(variable) || string_bound
            }
            _ => false,
        };
        if !invariant || variable.fixed_uniform() == Some(false) {
            return None;
        }
        let binding = variable.binding.as_ref()?;
        // An array binding stays symbolic unless it reads a String: Solve
        // owns no String storage, so only translation can evaluate it.
        if !variable.dims.is_empty() && !self.reads_string(binding) {
            return None;
        }
        let binding = self.rewrite_expression(binding).ok()?;
        let value = eval_expr(&binding, &self.functions).ok()?;
        let value = is_foldable(&value, variable.dims.is_empty()).then_some(value)?;
        if string_bound {
            self.string_bound.insert(id);
        }
        Some(value)
    }

    fn is_string(&self, variable: &flat::Variable) -> bool {
        self.flat
            .effective_types
            .get(&variable.type_id)
            .is_some_and(|effective| {
                effective.canonical_type() == self.flat.predefined_types.string
            })
    }

    fn reads_string(&self, expression: &Expression) -> bool {
        let mut reads = StringReads {
            evaluator: self,
            values_only: false,
            found: false,
        };
        reads.visit_expression(expression);
        reads.found
    }

    /// Whether an expression reads a String's value: a String read only for
    /// its shape (`size(substanceNames, 1)`) has a value Solve can evaluate.
    fn reads_string_value(&self, expression: &Expression) -> bool {
        let mut reads = StringReads {
            evaluator: self,
            values_only: true,
            found: false,
        };
        reads.visit_expression(expression);
        reads.found
    }

    /// The value of a proven reference, selected by its literal subscripts.
    fn reference_value(&mut self, id: InstanceId, subscripts: &[Subscript]) -> Option<Value> {
        let mut value = self.value(id)?;
        for subscript in subscripts {
            let index = match subscript {
                Subscript::Index { value, .. } => *value,
                Subscript::Expr { expr, .. } => {
                    let expr = self.rewrite_expression(expr).ok()?;
                    eval_expr(&expr, &self.functions).ok()?.as_integer()?
                }
                Subscript::Colon { .. } => return None,
            };
            let Value::Array(elements) = value else {
                return None;
            };
            value = elements
                .into_iter()
                .nth(usize::try_from(index.checked_sub(1)?).ok()?)?;
        }
        Some(value)
    }
}

/// A scalar folds to its literal; an array folds when every element is one.
fn is_foldable(value: &Value, scalar: bool) -> bool {
    match value {
        Value::Real(value) => value.is_finite(),
        Value::Integer(_) | Value::Bool(_) | Value::String(_) => true,
        Value::Array(elements) => {
            !scalar && elements.iter().all(|element| is_foldable(element, false))
        }
        Value::Enum(..) | Value::Record(_) => false,
    }
}

fn value_expression(value: &Value, span: Span) -> Option<Expression> {
    let literal = match value {
        Value::Real(value) => Literal::Real(*value),
        Value::Integer(value) => Literal::Integer(*value),
        Value::Bool(value) => Literal::Boolean(*value),
        Value::String(value) => Literal::String(value.clone()),
        Value::Array(elements) => {
            return Some(Expression::Array {
                elements: elements
                    .iter()
                    .map(|element| value_expression(element, span))
                    .collect::<Option<_>>()?,
                kind: ArrayConstructor::Array,
                span,
            });
        }
        Value::Enum(..) | Value::Record(_) => return None,
    };
    Some(Expression::Literal {
        value: literal,
        span,
    })
}

/// Whether an expression reads a String literal or a String variable.
struct StringReads<'e, 'a> {
    evaluator: &'e BindingEvaluator<'a>,
    /// Skip the operands of `size` and `ndims`, which read a shape only.
    values_only: bool,
    found: bool,
}

impl ExpressionVisitor for StringReads<'_, '_> {
    fn visit_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Literal {
                value: Literal::String(_),
                ..
            } => self.found = true,
            Expression::VarRef { name, .. }
                if name
                    .instance_id()
                    .and_then(|id| self.evaluator.variables.get(&id).copied().flatten())
                    .is_some_and(|variable| self.evaluator.is_string(variable)) =>
            {
                self.found = true;
            }
            Expression::BuiltinCall {
                function: BuiltinFunction::Size | BuiltinFunction::Ndims,
                ..
            } if self.values_only => {}
            _ => self.walk_expression(expr),
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
        // A declared enumeration literal is a constant of its type (MLS
        // §4.9.5), not a model variable: the interpreter reads it by name.
        let variable = name.instance_id().and_then(|id| self.variables.get(&id));
        if variable.is_none()
            && subscripts.is_empty()
            && self.functions.reference_enum(name).is_some()
        {
            return self.walk_var_ref_expression(name, subscripts, span);
        }
        let id = name.instance_id().ok_or(())?;
        let variable = self.variables.get(&id).copied().flatten().ok_or(())?;
        let declaration = variable.component_ref.as_ref().ok_or(())?.target_def_id();
        if name.component_ref().ok_or(())?.target_def_id() != declaration {
            return Err(());
        }
        let value = self.reference_value(id, subscripts).ok_or(())?;
        value_expression(&value, span).ok_or(())
    }
}

/// Model values are supplied only by exact, recursively proven substitutions.
/// Function locals and recursion remain owned by the constant interpreter.
struct FunctionInventory<'a> {
    flat: &'a flat::Model,
    resources: &'a ResourceRoots,
    enumeration_literals: &'a DeclaredEnumerationLiterals,
}

impl EvalEnvironment for FunctionInventory<'_> {
    fn get_value(&self, _name: &str) -> Option<Cow<'_, Value>> {
        None
    }

    fn get_enum(&self, name: &str) -> Option<&(String, String)> {
        self.enumeration_literals.literals.get(name)
    }

    fn enumeration_type(&self, declaration: rumoca_core::DefId) -> Option<&str> {
        self.enumeration_literals
            .types
            .get(&declaration)
            .map(String::as_str)
    }

    fn get_function(&self, name: &str) -> Option<&Function> {
        self.flat
            .functions
            .values()
            .find(|function| function.name.as_str() == name)
    }

    fn resolved_function(&self, call: &Reference) -> Option<&Function> {
        let instance = call.resolved_function()?.instance_id;
        self.flat
            .functions
            .values()
            .find(|function| function.instance_id == Some(instance))
    }

    fn get_array_dimensions(&self, _name: &str) -> Option<&[i64]> {
        None
    }

    fn deferred_parameter(&self, _name: &str) -> Option<DeferredParameterSource> {
        None
    }

    fn translation_resources(&self) -> Option<&ResourceRoots> {
        Some(self.resources)
    }
}
