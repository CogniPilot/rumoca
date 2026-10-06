use super::*;
use crate::projection::{FunctionFrame, Projection, ProjectionError, fold_graph};

impl<'dae> Projection<'_, 'dae> {
    pub(in crate::projection) fn begin_parameter_fragment(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: Option<usize>,
        scalar: usize,
    ) -> Start {
        #[cfg(test)]
        if self.cache.uncached_fold_reference || self.cache.uncached_parameter_fragments {
            return Start::None;
        }
        let Some(FunctionFrame::Summary { function, .. }) = self.function_frames.last() else {
            return Start::None;
        };
        let function = function.index();
        let key = ScalarExpressionDependency {
            activation: self.activation,
            expression: expression.index(),
            field,
            scalar,
            domain_context: self.expression_domain_context(expression),
        };
        let Some(capture) = self
            .function_summary_captures
            .last_mut()
            .filter(|capture| capture.function == function)
        else {
            return Start::None;
        };
        let context = key.domain_context;
        let result = capture.fragments.begin(self.view, expression, key);
        if !matches!(result, Start::Checking) {
            return result;
        }
        let reusable = reuse::Key {
            activation: self.activation,
            function,
            expression: expression.index(),
            field,
            scalar,
            parent: self.domain_contexts.snapshot(context),
        };
        if let Some(values) = self.cache.parameter_fragments.get(&reusable) {
            capture.fragments.import(Arc::clone(&values));
            #[cfg(test)]
            {
                self.cache.imported_fragment_hits += 1;
            }
            return Start::Imported(values);
        }
        capture.fragments.reusable(reusable);
        result
    }

    pub(in crate::projection) fn finish_parameter_fragment(&mut self, success: bool) {
        self.function_summary_captures
            .last_mut()
            .expect("the active summary opened this fragment")
            .fragments
            .finish(success);
    }

    pub(in crate::projection) fn replay_parameter_fragment(
        &mut self,
        dependencies: &Arc<[FunctionParameterDependency]>,
        imported: bool,
    ) {
        let Some(FunctionFrame::Summary { function, .. }) = self.function_frames.last() else {
            unreachable!("only a function summary can replay its parameter fragment");
        };
        let function = function.index();
        let capture = self
            .function_summary_captures
            .last_mut()
            .expect("the completed fragment belongs to this active summary");
        assert_eq!(
            capture.function, function,
            "fragments cannot cross function-summary invocations"
        );
        if imported {
            for dependency in dependencies.iter() {
                capture.dependencies.insert(dependency);
            }
        }
        // Construction of this completed fragment already captured every key
        // into its original invocation's global ordered summary. An imported
        // fragment first records the same parameter-relative inventory into
        // this invocation. All hits still replay into the active graph node
        // and independently recorded parent fragments.
        capture.folds.capture_completed(dependencies);
        capture.fragments.capture_completed(dependencies);
        capture.sweeps.capture_completed(dependencies);
    }

    /// Project one expression scalar, replaying a completed parameter
    /// fragment when the enclosing summary already captured it.
    pub(in crate::projection) fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar_index: usize,
    ) -> Result<(), ProjectionError> {
        let fragment = self.begin_parameter_fragment(expression, None, scalar_index);
        if let Start::Cached(dependencies) | Start::Imported(dependencies) = &fragment {
            self.replay_parameter_fragment(dependencies, matches!(fragment, Start::Imported(_)));
            return Ok(());
        }
        let result = self.expression_uncached(expression, scalar_index);
        if matches!(fragment, Start::Checking) {
            self.finish_parameter_fragment(result.is_ok());
        }
        result
    }

    /// Defer one carried fold dependency to the enclosing summary's fold
    /// graph, or replay its completed dependencies.
    pub(in crate::projection) fn enqueue_fold(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        field: Option<usize>,
        scalar: usize,
    ) -> Result<(), ProjectionError> {
        let transition = self
            .view
            .function_fold(fold)
            .expect("checked fold resolves");
        let parent = self.view.domain(transition.domain()).unwrap().parent();
        let context = self.domain_contexts.for_domain(self.view, parent);
        let node = fold_graph::FoldNode {
            activation: self.activation,
            fold,
            carried,
            field,
            scalar,
            initial: transition.initial_values().rhs(carried as usize).unwrap(),
            update: transition.update_values().rhs(carried as usize).unwrap(),
            parent: self.domain_contexts.snapshot(context),
        };
        if let Some(dependencies) = self.cache.completed_folds.get(&node).cloned() {
            #[cfg(test)]
            {
                self.cache.fold_reuses += 1;
            }
            for dependency in dependencies.iter().cloned() {
                self.capture_function_parameter(
                    fold.function(),
                    dependency,
                    transition.provenance().span(),
                )?;
            }
        } else {
            self.fold_summary_capture(fold.function())
                .unwrap()
                .folds
                .enqueue(node);
        }
        Ok(())
    }
}
