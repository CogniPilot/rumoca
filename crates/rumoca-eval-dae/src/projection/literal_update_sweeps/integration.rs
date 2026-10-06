use super::*;
use crate::projection::{Projection, ProjectionError, fold_graph::FoldNode};

impl<'dae> Projection<'_, 'dae> {
    pub(in crate::projection) fn project_literal_update_sweep(
        &mut self,
        node: &FoldNode<'dae>,
    ) -> Result<bool, ProjectionError> {
        #[cfg(test)]
        if self.cache.uncached_fold_reference || self.cache.uncached_literal_update_sweeps {
            return Ok(false);
        }
        let view = self.view;
        let Some(profile) = self
            .fold_summary_capture(node.fold.function())
            .unwrap()
            .sweeps
            .profile(view, node)
        else {
            return Ok(false);
        };
        for (guard, _) in &profile.guards {
            let capture = self.fold_summary_capture(node.fold.function()).unwrap();
            if !capture.fragments.eligible(view, *guard) {
                return Ok(false);
            }
        }
        let key = Key {
            activation: self.activation,
            fold: node.fold,
            carried: node.carried,
            update: node.update,
            parent: Arc::clone(&node.parent),
        };
        let completed = self
            .fold_summary_capture(node.fold.function())
            .unwrap()
            .sweeps
            .completed
            .get(&key)
            .cloned();
        if let Some(dependencies) = completed {
            #[cfg(test)]
            {
                self.fold_summary_capture(node.fold.function())
                    .unwrap()
                    .sweeps
                    .hits += 1;
            }
            self.replay_parameter_fragment(&dependencies, false);
        } else {
            self.fold_summary_capture(node.fold.function())
                .unwrap()
                .sweeps
                .begin();
            let result = self.sweep_parameter_guards(node.fold, &profile.guards);
            self.fold_summary_capture(node.fold.function())
                .unwrap()
                .sweeps
                .finish(key, result.is_ok());
            result?;
        }
        // Preserve the activation of every proved passthrough edge. A fallback
        // read after an unknown guard cannot become a guaranteed predecessor.
        for activation in profile.passthrough {
            self.with_activation(activation, |projection| {
                projection.enqueue_fold(node.fold, node.carried, None, node.scalar)
            })?;
        }
        Ok(true)
    }

    fn sweep_parameter_guards(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        guards: &[(dae::ExprId<'dae>, crate::projection::Activation)],
    ) -> Result<(), ProjectionError> {
        let domain = self.view.function_fold(fold).unwrap().domain();
        let points = self
            .view
            .domain(domain)
            .unwrap()
            .structured()
            .index_tuples()
            .expect("the profile retains a checked nonempty domain");
        for point in points {
            self.domain_contexts.push(domain, point);
            let projected = self.project_parameter_guards(guards);
            self.domain_contexts.pop();
            projected?;
        }
        Ok(())
    }

    fn project_parameter_guards(
        &mut self,
        guards: &[(dae::ExprId<'dae>, crate::projection::Activation)],
    ) -> Result<(), ProjectionError> {
        for (guard, activation) in guards {
            self.with_activation(*activation, |projection| projection.expression(*guard, 0))?;
        }
        Ok(())
    }
}
