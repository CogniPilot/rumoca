use super::*;
use crate::projection::{FunctionFrame, Projection};

impl<'dae> Projection<'_, 'dae> {
    pub(in crate::projection) fn begin_parameter_fragment(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: Option<usize>,
        scalar: usize,
    ) -> Start {
        if self.validating_actuals {
            return Start::None;
        }
        #[cfg(test)]
        if self.cache.uncached_fold_reference || self.cache.uncached_parameter_fragments {
            return Start::None;
        }
        let Some(FunctionFrame::Summary { function, .. }) = self.function_frames.last() else {
            return Start::None;
        };
        let function = function.index();
        if self.validation == Some(function) {
            return Start::None;
        }
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
}
