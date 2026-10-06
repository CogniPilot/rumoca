//! Successful parameter-only projections within one exact function summary.
mod completed;
mod integration;
pub(super) mod reuse;
use std::sync::Arc;

use super::{
    FunctionParameterDependency, ScalarExpressionDependency, dependencies::OrderedDependencies,
};
use rumoca_ir_dae as dae;

pub(super) enum Start {
    None,
    Cached(Arc<[FunctionParameterDependency]>),
    Imported(Arc<[FunctionParameterDependency]>),
    Checking,
}

#[derive(Debug)]
struct Recording {
    key: ScalarExpressionDependency,
    reusable: Option<reuse::Key>,
    complete: bool,
    dependencies: OrderedDependencies,
}

#[derive(Debug, Default)]
pub(super) struct ParameterFragments<'dae> {
    completed: completed::Cache,
    eligibility: completed::Eligibility,
    traversal: dae::ExpressionTraversal<'dae>,
    recording: Vec<Recording>,
    reusable: Vec<(reuse::Key, Arc<[FunctionParameterDependency]>)>,
    #[cfg(test)]
    pub(super) hits: u64,
}

impl<'dae> ParameterFragments<'dae> {
    pub(super) fn begin(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        key: ScalarExpressionDependency,
    ) -> Start {
        if !self.eligible(view, expression) {
            return Start::None;
        }
        if let Some(dependencies) = self.completed.get(&key) {
            #[cfg(test)]
            {
                self.hits += 1;
            }
            return Start::Cached(Arc::clone(dependencies));
        }
        self.recording.push(Recording {
            key,
            reusable: None,
            complete: true,
            dependencies: OrderedDependencies::default(),
        });
        Start::Checking
    }

    pub(super) fn finish(&mut self, success: bool) {
        let recording = self
            .recording
            .pop()
            .expect("a fragment was opened before completion");
        if success && recording.complete {
            let values: Arc<[FunctionParameterDependency]> =
                recording.dependencies.into_values().into();
            self.completed.insert(recording.key, Arc::clone(&values));
            if let Some(key) = recording.reusable
                && self.reusable.len() < 65_536
            {
                self.reusable.push((key, values));
            }
        }
    }

    pub(super) fn into_reusable(self) -> Vec<(reuse::Key, Arc<[FunctionParameterDependency]>)> {
        self.reusable
    }

    pub(super) fn reusable(&mut self, key: reuse::Key) {
        self.recording
            .last_mut()
            .expect("this fragment was opened")
            .reusable = Some(key);
    }

    pub(super) fn import(&mut self, values: Arc<[FunctionParameterDependency]>) {
        let recording = self.recording.pop().expect("this fragment was opened");
        self.completed.insert(recording.key, values);
    }

    pub(super) fn capture(&mut self, dependency: &FunctionParameterDependency) {
        for recording in &mut self.recording {
            recording.dependencies.insert(dependency);
        }
    }

    pub(super) fn capture_completed(&mut self, dependencies: &[FunctionParameterDependency]) {
        for recording in &mut self.recording {
            for dependency in dependencies {
                recording.dependencies.insert(dependency);
            }
        }
    }

    // A suppressed walk without a completed fragment cannot provide an
    // independent inventory. Never publish any ancestor containing that gap.
    pub(super) fn suppress(&mut self) {
        for recording in &mut self.recording {
            recording.complete = false;
        }
    }

    pub(in crate::projection) fn eligible(
        &mut self,
        view: dae::DaeView<'dae>,
        root: dae::ExprId<'dae>,
    ) -> bool {
        if let Some(eligible) = self.eligibility.get(root.index()) {
            return eligible;
        }
        let mut eligible = true;
        self.traversal
            .visit_pruned(view, [root], |expression, node| {
                if let Some(known) = self.eligibility.get(expression.index()) {
                    eligible &= known;
                    return false;
                }
                if excluded(node.operation()) {
                    eligible = false;
                    return false;
                }
                true
            });
        self.eligibility.insert(root.index(), eligible);
        eligible
    }
}

fn excluded(operation: dae::ExpressionOperation<'_>) -> bool {
    match operation {
        dae::ExpressionOperation::Coordinate(coordinate) => !matches!(
            coordinate,
            dae::CoordinateView::FunctionParameter(_) | dae::CoordinateView::Binder(_)
        ),
        dae::ExpressionOperation::Call { .. }
        | dae::ExpressionOperation::FunctionFoldParameter { .. }
        | dae::ExpressionOperation::FunctionFoldOutput { .. }
        | dae::ExpressionOperation::ClockTransfer { .. } => true,
        _ => false,
    }
}
