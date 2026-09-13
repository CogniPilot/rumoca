//! Lexical scalar-index bindings used during dimension inference.

use rumoca_core::{ComprehensionIndex, Expression, ExpressionVisitor, Reference, Subscript};
use std::cell::{OnceCell, RefCell};
use std::rc::Rc;

use super::{ParamEvalContext, ParamEvaluator, Value};

#[derive(Clone)]
pub(super) struct DimensionScope<'a, 'b> {
    parameters: &'a ParamEvalContext<'b>,
    evaluator: Rc<OnceCell<RefCell<ParamEvaluator<'b>>>>,
    indices: Vec<String>,
}

impl<'a, 'b> std::ops::Deref for DimensionScope<'a, 'b> {
    type Target = ParamEvalContext<'b>;
    fn deref(&self) -> &Self::Target {
        self.parameters
    }
}

impl<'a, 'b> DimensionScope<'a, 'b> {
    pub(super) fn new(parameters: &'a ParamEvalContext<'b>) -> Self {
        Self {
            parameters,
            evaluator: Rc::default(),
            indices: Vec::new(),
        }
    }

    pub(super) fn bind_index(&mut self, name: &str) {
        self.indices.push(name.to_owned());
    }

    pub(super) fn is_index(&self, reference: &Reference) -> bool {
        is_lexical_index(reference, &self.indices)
    }

    fn value(&self, expr: &Expression) -> Option<Value> {
        if !self.indices.is_empty() {
            let mut reads = IndexReads {
                indices: self.indices.clone(),
                found: false,
            };
            reads.visit_expression(expr);
            if reads.found {
                return None;
            }
        }
        self.evaluator
            .get_or_init(|| RefCell::new(ParamEvaluator::new(self.parameters)))
            .borrow_mut()
            .eval_value(expr, self.parameters.var_context)
    }

    pub(super) fn integer(&self, expr: &Expression) -> Option<i64> {
        self.value(expr)?.as_integer()
    }

    pub(super) fn real(&self, expr: &Expression) -> Option<f64> {
        self.value(expr)?.to_real()
    }

    pub(super) fn boolean(&self, expr: &Expression) -> Option<bool> {
        self.value(expr)?.as_bool()
    }

    pub(super) fn has_scalar_value(&self, expr: &Expression) -> bool {
        self.value(expr)
            .is_some_and(|value| !matches!(value, Value::Array(_)))
    }
}

struct IndexReads {
    indices: Vec<String>,
    found: bool,
}

fn is_lexical_index(reference: &Reference, indices: &[String]) -> bool {
    reference.instance_id().is_none() && indices.iter().any(|index| index == reference.as_str())
}

impl ExpressionVisitor for IndexReads {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.found |= is_lexical_index(name, &self.indices);
        self.walk_var_ref(name, subscripts);
    }

    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
    ) {
        let outer = self.indices.clone();
        for index in indices {
            self.visit_expression(&index.range);
            self.indices.retain(|name| name != &index.name);
        }
        self.visit_expression(expr);
        if let Some(filter) = filter {
            self.visit_expression(filter);
        }
        self.indices = outer;
    }
}
