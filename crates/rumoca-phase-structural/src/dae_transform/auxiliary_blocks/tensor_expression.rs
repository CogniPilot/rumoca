//! Compact coefficient recipes; leaves retain exact checked source call paths.

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

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
        1 + match self {
            Self::Source(_) | Self::Identity(_) | Self::Zero(_) => 0,
            Self::Negate(value) | Self::Index(value, _) => value.node_count(),
            Self::Sum(_, lhs, rhs) | Self::Product(_, lhs, rhs) => {
                lhs.node_count() + rhs.node_count()
            }
            Self::Array(elements) => elements.iter().map(Self::node_count).sum(),
        }
    }

    pub(super) fn operands<'a>(&'a self, result: &mut Vec<&'a SourceValue>) {
        match self {
            Self::Source(source) => result.push(source),
            Self::Identity(_) | Self::Zero(_) => (),
            Self::Negate(value) | Self::Index(value, _) => value.operands(result),
            Self::Sum(_, lhs, rhs) | Self::Product(_, lhs, rhs) => {
                lhs.operands(result);
                rhs.operands(result);
            }
            Self::Array(elements) => {
                for element in elements {
                    element.operands(result);
                }
            }
        }
    }

    pub(super) fn product(kind: Product, lhs: Self, rhs: Self) -> Self {
        Self::Product(kind, Box::new(lhs), Box::new(rhs))
    }
}
