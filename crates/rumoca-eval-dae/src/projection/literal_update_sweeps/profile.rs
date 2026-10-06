use super::*;
use crate::projection::fold_graph::FoldNode;

pub(super) fn derive<'dae>(
    view: dae::DaeView<'dae>,
    node: &FoldNode<'dae>,
) -> Option<Profile<'dae>> {
    if node.field.is_some() {
        return None;
    }
    let mut checked = Checker {
        view,
        node,
        guards: Vec::new(),
        passthrough: Vec::new(),
        update: false,
    };
    if !checked.value(node.update, crate::projection::Activation::Guaranteed)
        || checked.passthrough.is_empty()
        || !checked.update
        || checked.guards.is_empty()
    {
        return None;
    }
    Some(Profile {
        guards: checked.guards,
        passthrough: checked.passthrough,
    })
}

struct Checker<'view, 'dae> {
    view: dae::DaeView<'dae>,
    node: &'view FoldNode<'dae>,
    guards: Vec<(dae::ExprId<'dae>, crate::projection::Activation)>,
    passthrough: Vec<crate::projection::Activation>,
    update: bool,
}

impl<'dae> Checker<'_, 'dae> {
    fn value(
        &mut self,
        expression: dae::ExprId<'dae>,
        activation: crate::projection::Activation,
    ) -> bool {
        let value = self
            .view
            .expression(expression)
            .expect("checked update expression resolves");
        match value.operation() {
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.value(definition.rhs(), activation)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                self.conditional(operands, activation)
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => {
                let same = fold == self.node.fold && carried == self.node.carried;
                if same && !self.passthrough.contains(&activation) {
                    self.passthrough.push(activation);
                }
                same
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                let checked = self.literal_update(base, value, subscripts);
                self.update |= checked;
                checked
            }
            _ => false,
        }
    }

    fn conditional(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        mut remaining: crate::projection::Activation,
    ) -> bool {
        use crate::projection::Activation;
        for ordinal in (0..operands.len() - 1).step_by(2) {
            let guard = operands.get(ordinal).expect("checked conditional guard");
            self.guards.push((guard, remaining));
            let literal = match self.view.expression(guard).unwrap().operation() {
                dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(value)) => Some(*value),
                _ => None,
            };
            if literal == Some(false) {
                continue;
            }
            let selected = if literal == Some(true) {
                remaining
            } else {
                Activation::Conditional
            };
            if !self.value(
                operands
                    .get(ordinal + 1)
                    .expect("checked conditional branch"),
                selected,
            ) {
                return false;
            }
            if literal == Some(true) {
                return true;
            }
            remaining = Activation::Conditional;
        }
        self.value(
            operands
                .get(operands.len() - 1)
                .expect("checked conditional fallback"),
            remaining,
        )
    }

    fn literal_update(
        &self,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
    ) -> bool {
        let base = transparent(self.view, base);
        let replacement = transparent(self.view, value);
        let Some(base_view) = self.view.expression(base) else {
            return false;
        };
        if !matches!(base_view.operation(),dae::ExpressionOperation::FunctionFoldParameter {fold,carried,..} if fold == self.node.fold && carried == self.node.carried)
        {
            return false;
        }
        if !matches!(
            self.view.expression(replacement).unwrap().operation(),
            dae::ExpressionOperation::Literal(_)
        ) {
            return false;
        }
        let [extent] = base_view.value_type().dimensions() else {
            return false;
        };
        if subscripts.len() != 1 {
            return false;
        }
        let Some(dae::SubscriptView::Index { expression, .. }) = subscripts.get(0) else {
            return false;
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) =
            self.view.expression(expression).unwrap().operation()
        else {
            return false;
        };
        let fold = self.view.function_fold(self.node.fold).unwrap();
        if binder.domain() != fold.domain() || binder.ordinal() != 0 {
            return false;
        }
        let [axis] = self
            .view
            .domain(fold.domain())
            .unwrap()
            .structured()
            .binders
            .as_slice()
        else {
            return false;
        };
        axis.step > 0
            && axis.lower >= 1
            && axis.upper >= axis.lower
            && axis.upper <= i64::from(*extent)
    }
}

fn transparent<'dae>(
    view: dae::DaeView<'dae>,
    mut expression: dae::ExprId<'dae>,
) -> dae::ExprId<'dae> {
    while let dae::ExpressionOperation::FunctionValue { definition, .. } =
        view.expression(expression).unwrap().operation()
    {
        expression = definition.rhs();
    }
    expression
}
