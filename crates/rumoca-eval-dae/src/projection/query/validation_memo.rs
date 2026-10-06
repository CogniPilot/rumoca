//! Invocation-local successful address checks, never dependency inventories.
mod integration;
#[cfg(test)]
mod tests;

use super::super::{domain_context::DomainPoint, *};

const LIMIT: usize = 65_536;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct Key {
    expression: u32,
    scalar: usize,
    binders: [i64; 4],
}

pub(super) enum Start {
    None,
    Hit,
    Checking(Key),
}

/// One root actual-argument tuple owns this cache. It is dropped at the end of
/// the query and is never published in a function-summary cache. Only selected
/// numeric Index DAGs without calls, folds, records or internal binders qualify.
pub(in crate::projection) struct ValidationMemo<'dae> {
    root: dae::FunctionId<'dae>,
    actuals: Vec<dae::ExprId<'dae>>,
    support: HashMap<u32, Option<Vec<dae::DomainBinderId<'dae>>>>,
    traversal: dae::ExpressionTraversal<'dae>,
    success: HashSet<Key>,
    recording: usize,
    #[cfg(test)]
    pub(in crate::projection) hits: u64,
}

impl<'dae> ValidationMemo<'dae> {
    pub(in crate::projection) fn new(
        root: dae::FunctionId<'dae>,
        actuals: Vec<dae::ExprId<'dae>>,
    ) -> Self {
        Self {
            root,
            actuals,
            support: HashMap::default(),
            traversal: dae::ExpressionTraversal::new(),
            success: HashSet::default(),
            recording: 0,
            #[cfg(test)]
            hits: 0,
        }
    }

    fn begin(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        points: &[DomainPoint<'dae>],
    ) -> Start {
        let Some(binders) = self.support(view, expression) else {
            return Start::None;
        };
        let Some(values) = binder_values(binders, points) else {
            return Start::None;
        };
        let key = Key {
            expression: expression.index(),
            scalar,
            binders: values,
        };
        if self.success.contains(&key) {
            profile::validation_memo(self.root.index(), true, self.success.len());
            #[cfg(test)]
            {
                self.hits += 1;
            }
            return Start::Hit;
        }
        if self.success.len() == LIMIT {
            return Start::None;
        }
        profile::validation_memo(self.root.index(), false, self.success.len());
        self.recording += 1;
        Start::Checking(key)
    }

    fn finish(&mut self, key: Key, success: bool) {
        self.recording -= 1;
        if success && self.success.len() < LIMIT {
            self.success.insert(key);
        }
    }

    fn support(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
    ) -> Option<&[dae::DomainBinderId<'dae>]> {
        if !self.support.contains_key(&expression.index()) {
            if self.support.len() == LIMIT {
                return None;
            }
            let node = view
                .expression(expression)
                .expect("checked expression resolves");
            if node.value_type().scalar_type() != dae::ScalarType::Real
                || !matches!(node.operation(), dae::ExpressionOperation::Index { .. })
            {
                return None;
            }
            let support = derive_support(
                view,
                expression,
                self.root,
                &self.actuals,
                &mut self.traversal,
            );
            self.support.insert(expression.index(), support);
        }
        self.support.get(&expression.index())?.as_deref()
    }
}

fn binder_values<'dae>(
    binders: &[dae::DomainBinderId<'dae>],
    points: &[DomainPoint<'dae>],
) -> Option<[i64; 4]> {
    let mut result = [0; 4];
    for (index, binder) in binders.iter().enumerate() {
        let (_, point) = points
            .iter()
            .rev()
            .find(|(domain, _)| *domain == binder.domain())?;
        result[index] = *point.get(binder.ordinal() as usize)?;
    }
    Some(result)
}

fn derive_support<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    root: dae::FunctionId<'dae>,
    actuals: &[dae::ExprId<'dae>],
    traversal: &mut dae::ExpressionTraversal<'dae>,
) -> Option<Vec<dae::DomainBinderId<'dae>>> {
    let mut binders = Vec::new();
    let mut eligible = true;
    traversal.visit_pruned(view, [expression], |_, node| {
        if node.value_type().is_record() || !pure_operation(node, root, actuals, &mut binders) {
            eligible = false;
            return false;
        }
        true
    });
    binders.sort_unstable_by_key(|binder| (binder.domain().index(), binder.ordinal()));
    binders.dedup();
    (eligible && binders.len() <= 4).then_some(binders)
}

fn pure_operation<'dae>(
    node: dae::ExpressionView<'dae>,
    root: dae::FunctionId<'dae>,
    actuals: &[dae::ExprId<'dae>],
    binders: &mut Vec<dae::DomainBinderId<'dae>>,
) -> bool {
    match node.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
            binders.push(binder);
            true
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            // Integer formals trigger actual selector specialization and can
            // change summary cacheability. They retain the original full walk.
            parameter.function() == root
                && (parameter.ordinal() as usize) < actuals.len()
                && node.value_type().scalar_type() == dae::ScalarType::Real
        }
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Unary { .. }
        | dae::ExpressionOperation::Binary { .. }
        | dae::ExpressionOperation::Array(_)
        | dae::ExpressionOperation::Range(_)
        | dae::ExpressionOperation::Index { .. }
        | dae::ExpressionOperation::FunctionValue { .. } => true,
        // A Conditional can defer an inner address fault. This memo certifies
        // strict address checks only; that inventory uses the original walk.
        _ => false,
    }
}
