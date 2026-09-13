//! Compact coefficient recipes; leaves retain exact checked source call paths.

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;
use std::collections::BTreeSet;
use std::sync::Arc;

#[derive(Clone)]
pub(in crate::dae_transform) struct SourceValue {
    pub(in crate::dae_transform) expression: u32,
    calls: Box<[u32]>,
}

impl SourceValue {
    pub(super) fn new(expression: dae::ExprId<'_>, context: &FunctionCallContext<'_>) -> Self {
        Self {
            expression: expression.index(),
            calls: context.call_path().map(dae::ExprId::index).collect(),
        }
    }

    pub(super) fn model(expression: u32) -> Self {
        Self {
            expression,
            calls: Box::new([]),
        }
    }

    pub(in crate::dae_transform) fn context<'dae>(
        &self,
        view: dae::DaeView<'dae>,
    ) -> FunctionCallContext<'dae> {
        let mut context = FunctionCallContext::default();
        for &call in &self.calls {
            let call = view
                .expression_id(call as usize)
                .expect("source call identity");
            context = context.scoped_to_expression(view, call);
            context = context
                .call_result(view, call)
                .expect("proved source call path")
                .1;
        }
        context
    }
}

#[derive(Clone, Copy)]
pub(super) enum Product {
    Multiply,
    Outer,
}

/// Each node follows a source operation or a compact linear-map identity.
/// Array nodes traverse source-authored elements, never a declared extent.
#[derive(Clone)]
pub(super) enum TensorExpression {
    Source(SourceValue),
    Shared {
        source: u32,
        variable: Option<u32>,
        value: Arc<Self>,
    },
    One,
    Identity(u32),
    Zero(Box<[u32]>),
    Negate(Box<Self>),
    Sum(dae::BinaryOperator, Box<Self>, Box<Self>),
    Product(Product, Box<Self>, Box<Self>),
    Index(Box<Self>, SourceValue),
    Array(Box<[Self]>),
}

impl TensorExpression {
    #[cfg(test)]
    pub(super) fn node_count(&self) -> usize {
        let mut count = 0;
        self.visit(|_| count += 1);
        count
    }

    pub(super) fn operands<'a>(&'a self, result: &mut Vec<&'a SourceValue>) {
        self.visit(|node| {
            if let Self::Source(source) = node {
                result.push(source);
            }
        });
    }

    fn visit<'a>(&'a self, mut visit: impl FnMut(&'a Self)) {
        let mut pending = vec![self];
        let mut shared = BTreeSet::new();
        while let Some(node) = pending.pop() {
            if let Self::Shared {
                source, variable, ..
            } = node
                && !shared.insert((*source, *variable))
            {
                continue;
            }
            visit(node);
            match node {
                Self::Shared { value, .. } => pending.push(value),
                Self::Negate(value) | Self::Index(value, _) => pending.push(value),
                Self::Sum(_, lhs, rhs) | Self::Product(_, lhs, rhs) => {
                    pending.extend([rhs.as_ref(), lhs.as_ref()]);
                }
                Self::Array(elements) => pending.extend(elements.iter().rev()),
                _ => (),
            }
        }
    }

    pub(super) fn product(kind: Product, lhs: Self, rhs: Self) -> Self {
        Self::Product(kind, Box::new(lhs), Box::new(rhs))
    }
}
