use super::*;

impl<'dae> Projection<'_, 'dae> {
    pub(in crate::projection) fn new_guard_memo(
        &self,
        function: dae::FunctionId<'dae>,
        actuals: &[dae::ExprId<'dae>],
    ) -> Option<GuardMemo<'dae>> {
        #[cfg(test)]
        if self.cache.uncached_guard_memo {
            return None;
        }
        Some(GuardMemo::new(function, actuals.to_vec()))
    }

    fn guard_root_capture(&self) -> bool {
        self.guard_memo.as_ref().is_some_and(|memo| {
            self.validation == Some(memo.root.index()) && self.function_frames.len() == 1
                && self.function_summary_captures.len() == 1
                && matches!(self.function_frames.first(), Some(FunctionFrame::Summary { function, .. }) if *function == memo.root)
                && self.function_summary_captures[0].function == memo.root.index()
        })
    }

    pub(in crate::projection) fn guard_memo_forces_walk(&self) -> bool {
        self.guard_root_capture()
            && self
                .guard_memo
                .as_ref()
                .is_some_and(GuardMemo::independent_walk)
    }

    fn begin_guard(&mut self, expression: dae::ExprId<'dae>) -> Start<'dae> {
        if self.validating_actuals {
            return Start::None;
        }
        if !self.guard_root_capture() {
            if let Some(memo) = &self.guard_memo {
                memo.diagnostic(
                    profile::guard::Event::FrameFallback,
                    Some(expression.index()),
                );
            }
            return Start::None;
        }
        let memo = self.guard_memo.as_mut().unwrap();
        if !memo.eligible(self.view, expression) {
            return Start::None;
        }
        let Some(context) = self
            .domain_contexts
            .full_context(!memo.admission_saturated && memo.checked.len() < memo.key_limit)
        else {
            memo.diagnostic(
                profile::guard::Event::ContextFallback,
                Some(expression.index()),
            );
            return Start::None;
        };
        memo.begin(Key {
            activation: self.activation,
            expression: expression.index(),
            context,
        })
    }

    pub(in crate::projection) fn project_guard(
        &mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<(), ProjectionError> {
        let start = self.begin_guard(expression);
        if let Start::Hit(checked) = start {
            return self.replay_guard(&checked);
        }
        let result = self.expression(expression, 0);
        if matches!(start, Start::Checking) {
            let cacheable = self.function_summary_captures[0].cacheable;
            self.guard_memo
                .as_mut()
                .unwrap()
                .finish(result.is_ok(), cacheable);
        }
        result
    }

    fn replay_guard(&mut self, checked: &Checked<'dae>) -> Result<(), ProjectionError> {
        for effect in checked.effects.iter() {
            match effect {
                Effect::Pending(node) => {
                    self.record_guard_pending(node);
                    self.fold_summary_capture(node.fold.function())
                        .unwrap()
                        .folds
                        .enqueue(node.clone());
                }
                Effect::Parameter {
                    function,
                    dependency,
                    span,
                } => {
                    self.capture_function_parameter(*function, dependency.clone(), *span)?;
                }
            }
        }
        self.function_summary_captures[0].cacheable &= checked.cacheable;
        Ok(())
    }

    pub(in crate::projection) fn record_guard_pending(
        &mut self,
        node: &fold_graph::FoldNode<'dae>,
    ) {
        if self.guard_memo_forces_walk() {
            self.guard_memo
                .as_mut()
                .unwrap()
                .record(Effect::Pending(node.clone()));
        }
    }

    pub(in crate::projection) fn record_guard_parameter(
        &mut self,
        function: dae::FunctionId<'dae>,
        dependency: &FunctionParameterDependency,
        span: Span,
    ) {
        if self.guard_memo_forces_walk() && self.validation == Some(function.index()) {
            self.guard_memo.as_mut().unwrap().record(Effect::Parameter {
                function,
                dependency: dependency.clone(),
                span,
            });
        }
    }

    pub(in crate::projection) fn guard_memo_invalidate(&mut self) {
        if !self.validating_actuals
            && let Some(memo) = self.guard_memo.as_mut()
        {
            memo.invalidate();
        }
    }

    pub(in crate::projection) fn guard_memo_note_call(&mut self, function: dae::FunctionId<'dae>) {
        if self
            .guard_memo
            .as_ref()
            .is_some_and(|memo| memo.root == function && !self.function_frames.is_empty())
        {
            self.guard_memo_invalidate();
        }
    }

    pub(in crate::projection) fn guard_memo_observe(&mut self, expression: dae::ExprId<'dae>) {
        if self
            .guard_memo
            .as_ref()
            .is_none_or(|memo| memo.recording.is_empty())
        {
            return;
        }
        let node = self.node(expression);
        // Nested helper formals use their complete ordinary capture. These
        // values are not keys; only their root substitutions record effects.
        let unsupported = node.value_type().is_record()
            || matches!(
                node.operation(),
                dae::ExpressionOperation::ClockTransfer { .. }
                    | dae::ExpressionOperation::Record(_)
                    | dae::ExpressionOperation::Field { .. }
                    | dae::ExpressionOperation::StringConversion { .. }
                    | dae::ExpressionOperation::Comprehension { .. }
            )
            || matches!(node.operation(), dae::ExpressionOperation::Builtin { .. } if !supported_builtin(self.view, node));
        if unsupported {
            if profile::enabled() {
                self.guard_memo
                    .as_ref()
                    .unwrap()
                    .diagnostic(profile::guard::unsupported(node), Some(expression.index()));
            }
            self.guard_memo_invalidate();
        }
    }
}

#[cfg(test)]
mod owner_tests {
    use super::*;

    fn capture(function: dae::FunctionId<'_>) -> FunctionSummaryCapture<'_> {
        FunctionSummaryCapture {
            function: function.index(),
            dependencies: dependencies::OrderedDependencies::default(),
            needed_integers: Vec::new(),
            cacheable: true,
            visited: visited::Visited::default(),
            folds: fold_graph::FoldGraph::default(),
            fragments: parameter_fragments::ParameterFragments::default(),
            sweeps: literal_update_sweeps::LiteralUpdateSweeps::default(),
        }
    }

    #[test]
    fn guard_memo_requires_one_exact_root_frame_and_capture_and_invalidates_reentry() {
        super::super::tests::with_memo(|view, memo| {
            let root = memo.root;
            let mut cache = ScalarCoordinateProjectionCache::default();
            let mut visit = |_, _| {};
            let mut projection = Projection {
                activation: Activation::Guaranteed,
                validating_actuals: false,
                view,
                domain_contexts: domain_context::DomainContexts::default(),
                integer_stack: vec![false; view.expression_count()],
                function_frames: vec![FunctionFrame::Summary {
                    function: root,
                    integers: Vec::new(),
                }],
                frame_memos: vec![FrameMemo::default()],
                function_call_active: HashSet::default(),
                function_fold_active: HashSet::default(),
                function_summary_captures: vec![capture(root)],
                model_visited: visited::Visited::default(),
                cache: &mut cache,
                visit: &mut visit,
                relevant: None,
                validation: Some(root.index()),
                validation_memo: None,
                guard_memo: Some(memo),
            };
            let key = Key {
                activation: Activation::Guaranteed,
                expression: 0,
                context: DomainContextId::default(),
            };
            assert!(matches!(
                projection.guard_memo.as_mut().unwrap().begin(key),
                Start::Checking
            ));
            assert!(projection.guard_memo_forces_walk());
            projection.push_frame(FunctionFrame::Actual {
                function: root,
                arguments: vec![],
            });
            assert!(
                !projection.guard_memo_forces_walk(),
                "same function ID cannot cross an actual frame"
            );
            projection.pop_frame();
            projection.function_summary_captures.push(capture(root));
            assert!(
                !projection.guard_memo_forces_walk(),
                "same function ID cannot cross a capture"
            );
            projection.function_summary_captures.pop();
            assert!(projection.guard_memo_forces_walk());
            projection.guard_memo_note_call(root);
            assert!(!projection.guard_memo_forces_walk());
            let memo = projection.guard_memo.as_mut().unwrap();
            memo.finish(true, true);
            assert!(memo.checked.is_empty());
        });
    }
}
