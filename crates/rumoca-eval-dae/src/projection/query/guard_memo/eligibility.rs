//! Eligibility follows the authoritative projector's shape-only Size operands.
use super::*;

struct Scan<'scope, 'dae> {
    root: dae::FunctionId<'dae>,
    actuals: &'scope [dae::ExprId<'dae>],
    dimensions: Vec<dae::ExprId<'dae>>,
    scheduled: HashSet<u32>,
    eligible: bool,
    refusal: Option<(profile::guard::Event, u32)>,
}

impl<'dae> GuardMemo<'dae> {
    pub(super) fn eligible(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> bool {
        if let Some(eligible) = self.eligible.get(&expression.index()) {
            self.diagnostic(
                if *eligible {
                    profile::guard::Event::EligibleCached
                } else {
                    profile::guard::Event::IneligibleCached
                },
                Some(expression.index()),
            );
            return *eligible;
        }
        if self.eligible.len() == self.key_limit {
            self.diagnostic(profile::guard::Event::KeyCapacity, Some(expression.index()));
            return false;
        }
        let mut scan = Scan {
            root: self.root,
            actuals: &self.actuals,
            dimensions: vec![expression],
            scheduled: [expression.index()].into_iter().collect(),
            eligible: true,
            refusal: None,
        };
        while let Some(root) = scan.dimensions.pop() {
            self.traversal.visit_pruned(view, [root], |visited, node| {
                scan.visit(view, visited, node)
            });
            if !scan.eligible {
                break;
            }
        }
        self.diagnostic(
            scan.refusal
                .map_or(profile::guard::Event::EligibleDerived, |(reason, _)| reason),
            Some(
                scan.refusal
                    .map_or(expression.index(), |(_, expression)| expression),
            ),
        );
        self.eligible.insert(expression.index(), scan.eligible);
        scan.eligible
    }
}

impl<'dae> Scan<'_, 'dae> {
    fn visit(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
    ) -> bool {
        if !self.eligible {
            return false;
        }
        if !supported(view, node, self.root, self.actuals) {
            self.eligible = false;
            self.refuse(view, expression, node);
            return false;
        }
        match node.operation() {
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Size,
                arguments,
            } => {
                // The original Size projector does not inspect the array's values.
                // Its optional dimension remains an ordinary checked expression.
                if let Some(dimension) = arguments.get(1) {
                    self.schedule(dimension);
                }
                false
            }
            dae::ExpressionOperation::FunctionFoldParameter { .. }
            | dae::ExpressionOperation::FunctionFoldOutput { .. } => false,
            _ => true,
        }
    }

    fn schedule(&mut self, dimension: dae::ExprId<'dae>) {
        if self.scheduled.contains(&dimension.index()) {
            return;
        }
        if self.scheduled.len() == KEY_LIMIT {
            self.eligible = false;
            self.refusal = Some((profile::guard::Event::KeyCapacity, dimension.index()));
            return;
        }
        self.scheduled.insert(dimension.index());
        self.dimensions.push(dimension);
    }

    fn refuse(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        node: dae::ExpressionView<'dae>,
    ) {
        if !profile::enabled() || self.refusal.is_some() {
            return;
        }
        let reason = if matches!(node.operation(), dae::ExpressionOperation::Call { function, .. } if function == self.root)
        {
            profile::guard::Event::RootCall
        } else {
            profile::guard::unsupported(node)
        };
        profile::guard::refusal(self.root.index(), expression.index(), node, view);
        self.refusal = Some((reason, expression.index()));
    }
}

fn supported<'dae>(
    view: dae::DaeView<'dae>,
    node: dae::ExpressionView<'dae>,
    root: dae::FunctionId<'dae>,
    actuals: &[dae::ExprId<'dae>],
) -> bool {
    if node.value_type().is_record() {
        return false;
    }
    match node.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(_)) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(p)) => {
            p.function() == root && (p.ordinal() as usize) < actuals.len()
        }
        dae::ExpressionOperation::Call { function, .. } => {
            function != root
                && view
                    .function(function)
                    .is_some_and(|definition| definition.external().is_none())
        }
        dae::ExpressionOperation::Builtin { .. } => supported_builtin(view, node),
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Unary { .. }
        | dae::ExpressionOperation::Binary { .. }
        | dae::ExpressionOperation::Conditional(_)
        | dae::ExpressionOperation::Array(_)
        | dae::ExpressionOperation::Range(_)
        | dae::ExpressionOperation::Index { .. }
        | dae::ExpressionOperation::ArrayUpdate { .. }
        | dae::ExpressionOperation::FunctionValue { .. }
        | dae::ExpressionOperation::FunctionFoldParameter { .. }
        | dae::ExpressionOperation::FunctionFoldOutput { .. } => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::projection::tests::shape_only_size::{primitive_model, sizes};

    #[test]
    fn size_eligibility_matches_shape_projection_without_inspecting_clock_or_external_values() {
        primitive_model(dae::DaeLiteral::Integer(1))
            .unwrap()
            .inspect(check_shape_only_eligibility);
    }

    fn check_shape_only_eligibility(view: dae::DaeView<'_>) {
        let function = view.function_id(0).unwrap();
        let mut memo = GuardMemo::new(function, vec![]);
        for expression in sizes(view) {
            let dae::ExpressionOperation::Builtin { arguments, .. } =
                view.expression(expression).unwrap().operation()
            else {
                unreachable!()
            };
            assert!(!supported(
                view,
                view.expression(arguments.get(0).unwrap()).unwrap(),
                function,
                &[]
            ));
            assert!(memo.eligible(view, expression));
            let key = Key {
                activation: Activation::Guaranteed,
                expression: expression.index(),
                context: DomainContextId::default(),
            };
            assert!(matches!(memo.begin(key), Start::Checking));
            memo.finish(true, true);
            let Start::Hit(checked) = memo.begin(key) else {
                panic!("shape-only success reuses")
            };
            assert!(checked.effects.is_empty());
        }
    }
}

#[cfg(test)]
mod floor_tests {
    use super::*;
    use crate::projection::tests::shape_only_size::primitive_floor_model;

    #[test]
    fn floor_does_not_prune_unsupported_clock_or_external_value_operands() {
        primitive_floor_model().inspect(check_floor_operands);
    }

    fn check_floor_operands(view: dae::DaeView<'_>) {
        let function = view.function_id(0).unwrap();
        let mut memo = GuardMemo::new(function, vec![]);
        let mut checked = 0;
        for i in 0..view.expression_count() {
            let expression = view.expression_id(i).unwrap();
            if matches!(
                view.expression(expression).unwrap().operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Floor,
                    ..
                }
            ) {
                assert!(!memo.eligible(view, expression));
                checked += 1;
            }
        }
        assert_eq!(checked, 2);
    }
}

#[cfg(test)]
mod abs_tests {
    use super::*;
    use crate::projection::tests::shape_only_size::primitive_abs_model;

    #[test]
    fn abs_admits_only_scalar_real_and_retains_unsupported_value_operands() {
        primitive_abs_model().inspect(check_abs_operands);
    }

    fn check_abs_operands(view: dae::DaeView<'_>) {
        let mut memo = GuardMemo::new(view.function_id(0).unwrap(), vec![]);
        let mut seen = 0;
        for i in 0..view.expression_count() {
            let expression = view.expression_id(i).unwrap();
            let node = view.expression(expression).unwrap();
            if matches!(
                node.operation(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Abs,
                    ..
                }
            ) {
                check_abs(view, node, expression, &mut memo, seen);
                seen += 1;
            }
        }
        assert_eq!(seen, 5);
    }

    fn check_abs<'dae>(
        view: dae::DaeView<'dae>,
        node: dae::ExpressionView<'dae>,
        expression: dae::ExprId<'dae>,
        memo: &mut GuardMemo<'dae>,
        ordinal: usize,
    ) {
        assert_eq!(supported_builtin(view, node), ordinal < 3);
        assert_eq!(memo.eligible(view, expression), ordinal == 2);
    }
}
