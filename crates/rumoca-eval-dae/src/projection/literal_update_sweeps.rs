//! Checked finite sweeps of scalar-independent guards on literal slot updates.
mod integration;
mod profile;

use super::{
    FunctionParameterDependency, dependencies::OrderedDependencies, domain_context::Context,
};
use rumoca_ir_dae as dae;
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct Key<'dae> {
    pub(super) activation: super::Activation,
    pub(super) fold: dae::FunctionFoldId<'dae>,
    pub(super) carried: u32,
    pub(super) update: dae::ExprId<'dae>,
    pub(super) parent: Arc<Context>,
}

#[derive(Debug, Clone)]
pub(super) struct Profile<'dae> {
    pub(super) guards: Vec<(dae::ExprId<'dae>, super::Activation)>,
    pub(super) passthrough: Vec<super::Activation>,
}

#[derive(Debug, Default)]
pub(super) struct LiteralUpdateSweeps<'dae> {
    profiles: HashMap<(dae::FunctionFoldId<'dae>, u32, dae::ExprId<'dae>), Option<Profile<'dae>>>,
    pub(super) completed: HashMap<Key<'dae>, Arc<[FunctionParameterDependency]>>,
    recording: Option<OrderedDependencies>,
    #[cfg(test)]
    pub(super) hits: u64,
}

impl<'dae> LiteralUpdateSweeps<'dae> {
    pub(super) fn profile(
        &mut self,
        view: dae::DaeView<'dae>,
        node: &super::fold_graph::FoldNode<'dae>,
    ) -> Option<Profile<'dae>> {
        self.profiles
            .entry((node.fold, node.carried, node.update))
            .or_insert_with(|| profile::derive(view, node))
            .clone()
    }

    pub(super) fn begin(&mut self) {
        assert!(
            self.recording.is_none(),
            "parameter-only guards cannot open nested sweeps"
        );
        self.recording = Some(OrderedDependencies::default());
    }

    pub(super) fn finish(&mut self, key: Key<'dae>, success: bool) {
        let recorded = self.recording.take().expect("this guard sweep was opened");
        if success {
            self.completed.insert(key, recorded.into_values().into());
        }
    }

    pub(super) fn capture(&mut self, dependency: &FunctionParameterDependency) {
        if let Some(recording) = &mut self.recording {
            recording.insert(dependency);
        }
    }

    pub(super) fn capture_completed(&mut self, dependencies: &[FunctionParameterDependency]) {
        if let Some(recording) = &mut self.recording {
            for dependency in dependencies {
                recording.insert(dependency);
            }
        }
    }
}
