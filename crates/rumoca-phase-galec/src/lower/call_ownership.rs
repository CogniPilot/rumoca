//! Call ownership committed by the expression lowering that emits each group.

use super::*;

/// One prefix/memo scope in the emitted `DoStep` body.
///
/// Guard and value stages are distinct because the emitter closes its
/// temporary memo between them. These identities are constructed by the
/// emitter, not reconstructed by a second expression traversal.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum EmissionRegion {
    ClockedRealGuard {
        clock: u32,
        group: usize,
    },
    ClockedRealValue {
        clock: u32,
        group: usize,
    },
    DiscreteValueGuard {
        clock: u32,
        owner: u32,
        branch: usize,
    },
    DiscreteValueValue {
        clock: u32,
        owner: u32,
        branch: usize,
    },
    EventAction {
        clock: u32,
        action: u32,
    },
    CausalAssignment(usize),
}

#[derive(Clone)]
pub(super) struct MaterializedRootCall {
    pub(super) key: MaterializedFunctionCallKey,
    pub(super) names: Vec<gast::Name>,
    pub(super) argument_reads: HashSet<u32>,
}

#[derive(Clone)]
pub(super) struct RootCallAction {
    pub(super) owner: u32,
    pub(super) activation: Vec<ConditionalActivationKey>,
    pub(super) materialized: Option<MaterializedRootCall>,
}

/// One exact source-call activation reached by real expression lowering.
///
/// This is co-produced before memo lookup or emission choice and is never
/// exposed outside the lowering transaction. The committed ledger consumes it
/// against the independently emitted [`RootCallAction`] stream.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct ExpectedRootCallAction {
    owner: u32,
    activation: Vec<ConditionalActivationKey>,
}

impl ExpectedRootCallAction {
    pub(super) fn from_emitted(action: &RootCallAction) -> Self {
        Self {
            owner: action.owner,
            activation: action.activation.clone(),
        }
    }
}

#[derive(Clone)]
pub(super) struct RootCallReuse {
    reached: ExpectedRootCallAction,
    producers: HashSet<ExpectedRootCallAction>,
    /// `None` means the producer belongs to this same emission transaction.
    /// A retained producer names its exact earlier region, which the consumer
    /// must also carry as a scheduler dependency.
    producer_region: Option<EmissionRegion>,
}

/// Exact call actions reached by one committed emission transaction.
pub(super) struct PreparedCallActions {
    region: EmissionRegion,
    actions: Vec<RootCallAction>,
    expected: HashSet<ExpectedRootCallAction>,
    reused: Vec<RootCallReuse>,
    dependencies: HashSet<EmissionRegion>,
}

impl PreparedCallActions {
    pub(super) const fn region(&self) -> EmissionRegion {
        self.region
    }

    pub(super) fn dependencies(&self) -> &HashSet<EmissionRegion> {
        &self.dependencies
    }

    pub(super) fn argument_reads(&self) -> HashSet<u32> {
        self.actions
            .iter()
            .filter_map(|action| action.materialized.as_ref())
            .flat_map(|action| action.argument_reads.iter().copied())
            .collect()
    }
}

/// Affine registry of immutable call-result locals proven reusable later in
/// the same `DoStep`.
///
/// The registry deliberately has no `Clone`: speculative expression lowering
/// cannot duplicate or publish its authority. Entries are installed and
/// consumed only beside the statements whose real lowering produced their
/// argument-read and activation facts.
#[derive(Default)]
pub(super) struct RetainedCallResults {
    entries: Vec<RetainedCallResult>,
}

struct RetainedCallResult {
    key: MaterializedFunctionCallKey,
    names: Vec<gast::Name>,
    argument_reads: HashSet<u32>,
    producer: EmissionRegion,
    activation: Option<RetainedCallActivation>,
    sources: HashSet<ExpectedRootCallAction>,
}

/// Whether one emission group is structurally unguarded on every `DoStep`.
pub(super) enum CrossGroupCallRetention<'retained> {
    Refuse,
    Unguarded(&'retained mut RetainedCallResults),
    ExactGuard {
        retained: &'retained mut RetainedCallResults,
        activation: RetainedCallActivation,
    },
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) struct RetainedCallActivation {
    clock: u32,
    guard: u32,
}

impl RetainedCallActivation {
    pub(super) fn exact<'dae>(clock: dae::ClockId<'dae>, guard: dae::ConditionId<'dae>) -> Self {
        Self {
            clock: clock.index(),
            guard: guard.index(),
        }
    }
}

/// One call action committed while compiling a protected user-function body.
///
/// `scope` identifies the protected function being emitted. `call_path`
/// identifies every enclosing issued call whose body was substituted into
/// that function, and `owner` identifies the call at the leaf. Keeping all
/// three facts prevents a bare expression identity from conflating equal
/// inner expressions reached through distinct outer invocations.
#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) struct FunctionCallAction {
    pub(super) scope: u32,
    pub(super) call_path: Vec<MaterializedCallKey>,
    pub(super) owner: u32,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub(super) enum CallExecutionSource {
    Root(ExpectedRootCallAction),
    Function(FunctionCallAction),
}

#[derive(Clone)]
pub(super) struct FunctionCallReuse {
    reached: FunctionCallAction,
    producers: HashSet<FunctionCallAction>,
}

/// Statements and call ownership co-produced by one real lowering execution.
#[must_use = "a prepared emission group must commit both statements and call actions"]
pub(super) struct PreparedEmissionGroup<T> {
    value: T,
    statements: Vec<gast::Spanned<gast::Statement>>,
    calls: PreparedCallActions,
}

impl<T> PreparedEmissionGroup<T> {
    pub(super) fn commit_into(
        self,
        statements: &mut Vec<gast::Spanned<gast::Statement>>,
        calls: &mut Vec<PreparedCallActions>,
    ) -> T {
        statements.extend(self.statements);
        calls.push(self.calls);
        self.value
    }
}

impl<'a, 'dae> ExpressionLowerer<'a, 'dae> {
    /// Record the exact call activation reached by real lowering before
    /// memo lookup or emission policy can choose how it is represented.
    pub(super) fn record_reached_root_call(&mut self, call: dae::ExprId<'dae>) {
        let node = self.view.exact_expression(call);
        let dae::ExpressionOperation::Call { owner, .. } = node.operation() else {
            unreachable!("reached-call inventory receives one checked call")
        };
        if let Some(scope) = self.function_scope {
            self.expected_function_call_actions
                .insert(FunctionCallAction {
                    scope: scope.index(),
                    call_path: self.materialized_call_path(),
                    owner: owner.index(),
                });
            return;
        }
        if node.function_scope().is_none() {
            self.expected_root_call_actions
                .insert(ExpectedRootCallAction {
                    owner: owner.index(),
                    activation: self.conditional_activation_path.clone(),
                });
        }
    }

    pub(super) fn record_reused_root_call(
        &mut self,
        call: dae::ExprId<'dae>,
        producer: &MaterializedFunctionCallKey,
    ) -> Result<(), GalecTargetError> {
        let node = self.view.exact_expression(call);
        let dae::ExpressionOperation::Call { owner, .. } = node.operation() else {
            unreachable!("reused-call inventory receives one checked call")
        };
        if let Some(scope) = self.function_scope {
            let producers = self
                .materialized_call_sources
                .get(producer)
                .ok_or_else(|| GalecTargetError::LoweringInternal {
                    detail: format!(
                        "protected materialized call owner #{} has no execution capability",
                        owner.index()
                    ),
                })?
                .iter()
                .filter_map(|source| match source {
                    CallExecutionSource::Function(source) => Some(source.clone()),
                    CallExecutionSource::Root(_) => None,
                })
                .collect::<HashSet<_>>();
            if producers.is_empty() {
                return Err(GalecTargetError::LoweringInternal {
                    detail: format!(
                        "protected materialized call owner #{} has an empty execution capability",
                        owner.index()
                    ),
                });
            }
            self.reused_function_call_actions.push(FunctionCallReuse {
                reached: FunctionCallAction {
                    scope: scope.index(),
                    call_path: self.materialized_call_path(),
                    owner: owner.index(),
                },
                producers,
            });
            return Ok(());
        }
        if node.function_scope().is_some() {
            return Ok(());
        }
        let producers = self
            .materialized_call_sources
            .get(producer)
            .ok_or_else(|| GalecTargetError::LoweringInternal {
                detail: format!(
                    "materialized call owner #{} has no retained execution capability",
                    owner.index()
                ),
            })?
            .iter()
            .filter_map(|source| match source {
                CallExecutionSource::Root(source) => Some(source.clone()),
                CallExecutionSource::Function(_) => None,
            })
            .collect::<HashSet<_>>();
        if producers.is_empty() {
            return Err(GalecTargetError::LoweringInternal {
                detail: format!(
                    "materialized call owner #{} has an empty execution capability",
                    owner.index()
                ),
            });
        }
        self.reused_root_call_actions.push(RootCallReuse {
            reached: ExpectedRootCallAction {
                owner: owner.index(),
                activation: self.conditional_activation_path.clone(),
            },
            producers,
            producer_region: self
                .imported_materialized_call_regions
                .get(producer)
                .copied(),
        });
        Ok(())
    }

    /// Execute and close one emission transaction exactly once.
    ///
    /// The closure appends only the group's non-prefix statements. Prefixes,
    /// memo teardown, and the call-action footprint are committed together
    /// here, so no caller can obtain a footprint by replaying expression
    /// lowering on a clone.
    pub(super) fn prepare_emission_group<T>(
        &mut self,
        region: EmissionRegion,
        targets: &HashSet<u32>,
        retention: CrossGroupCallRetention<'_>,
        emit: impl FnOnce(
            &mut ExpressionLowerer<'a, 'dae>,
            &mut Vec<gast::Spanned<gast::Statement>>,
        ) -> Result<T, GalecTargetError>,
    ) -> Result<PreparedEmissionGroup<T>, GalecTargetError> {
        if !self.pending_prefix_statements.is_empty()
            || !self.evaluated_root_call_actions.is_empty()
            || !self.expected_root_call_actions.is_empty()
            || !self.reused_root_call_actions.is_empty()
            || !self.materialized_function_calls.is_empty()
            || !self.materialized_call_sources.is_empty()
            || !self.imported_materialized_call_regions.is_empty()
            || !self.retained_call_dependencies.is_empty()
        {
            return Err(GalecTargetError::LoweringInternal {
                detail: "emission group started with uncommitted statements or call actions"
                    .to_owned(),
            });
        }
        let mut retention = retention;
        match &mut retention {
            CrossGroupCallRetention::Refuse => {}
            CrossGroupCallRetention::Unguarded(retained) => {
                retained.install(self, region, targets, None);
            }
            CrossGroupCallRetention::ExactGuard {
                retained,
                activation,
            } => {
                retained.install(self, region, targets, Some(*activation));
            }
        }
        let mut statements = Vec::new();
        let value = emit(self, &mut statements)?;
        let actions = self.take_evaluated_root_call_actions();
        let expected = self.take_expected_root_call_actions();
        let reused = self.take_reused_root_call_actions();
        match &mut retention {
            CrossGroupCallRetention::Refuse => {}
            CrossGroupCallRetention::Unguarded(retained) => {
                retained.retain_emitted(self, region, targets, &actions, None);
            }
            CrossGroupCallRetention::ExactGuard {
                retained,
                activation,
            } => {
                retained.retain_emitted(self, region, targets, &actions, Some(*activation));
            }
        }
        let dependencies = std::mem::take(&mut self.retained_call_dependencies);
        self.imported_materialized_call_regions.clear();
        let mut prefixes = self.drain_prefix_statements();
        self.finish_statement_group();
        prefixes.append(&mut statements);
        Ok(PreparedEmissionGroup {
            value,
            statements: prefixes,
            calls: PreparedCallActions {
                region,
                actions,
                expected,
                reused,
                dependencies,
            },
        })
    }
}

impl RetainedCallResults {
    fn install<'dae>(
        &mut self,
        lowerer: &mut ExpressionLowerer<'_, 'dae>,
        region: EmissionRegion,
        targets: &HashSet<u32>,
        activation: Option<RetainedCallActivation>,
    ) {
        // Causal groups execute in construction order after the clock
        // schedule, so an intervening causal write permanently ends an older
        // result. Clocked groups are topologically reordered later: their
        // actual argument reads become scheduler edges, placing every unique
        // writer before the producer, so a construction-later writer must not
        // erase a result that will execute after it.
        if matches!(region, EmissionRegion::CausalAssignment(_)) {
            self.entries
                .retain(|retained| retained.argument_reads.is_disjoint(targets));
        }
        for retained in &self.entries {
            if retained.activation != activation || !retained.argument_reads.is_disjoint(targets) {
                continue;
            }
            lowerer
                .materialized_function_calls
                .insert(retained.key.clone(), retained.names.clone());
            lowerer.materialized_call_sources.insert(
                retained.key.clone(),
                retained
                    .sources
                    .iter()
                    .cloned()
                    .map(CallExecutionSource::Root)
                    .collect(),
            );
            lowerer
                .imported_materialized_call_regions
                .insert(retained.key.clone(), retained.producer);
        }
    }

    fn retain_emitted<'dae>(
        &mut self,
        lowerer: &ExpressionLowerer<'_, 'dae>,
        region: EmissionRegion,
        targets: &HashSet<u32>,
        actions: &[RootCallAction],
        activation: Option<RetainedCallActivation>,
    ) {
        for (key, names) in &lowerer.materialized_function_calls {
            if !key.call_path.is_empty()
                || !key.iteration_path.is_empty()
                || !key.activation_path.is_empty()
                || lowerer.imported_materialized_call_regions.contains_key(key)
                || self.entries.iter().any(|entry| entry.key == *key)
            {
                continue;
            }
            let matching = actions
                .iter()
                .filter_map(|action| action.materialized.as_ref())
                .filter(|action| action.key.same_invocation(key) && action.names == *names)
                .collect::<Vec<_>>();
            if matching.is_empty() {
                continue;
            }
            let argument_reads = matching
                .into_iter()
                .flat_map(|action| action.argument_reads.iter().copied())
                .collect::<HashSet<_>>();
            if !argument_reads.is_disjoint(targets) {
                continue;
            }
            let Some(sources) = lowerer.materialized_call_sources.get(key).map(|sources| {
                sources
                    .iter()
                    .filter_map(|source| match source {
                        CallExecutionSource::Root(source) => Some(source.clone()),
                        CallExecutionSource::Function(_) => None,
                    })
                    .collect::<HashSet<_>>()
            }) else {
                continue;
            };
            if sources.is_empty() {
                continue;
            }
            self.entries.push(RetainedCallResult {
                key: key.clone(),
                names: names.clone(),
                argument_reads,
                producer: region,
                activation,
                sources,
            });
        }
    }
}

/// Whole-`DoStep` proof that every committed call owner has one emission
/// region. This consumes only ledgers produced alongside actual statements.
pub(super) struct CommittedCallActionLedger {
    /// Retained until package closure consumes this receipt. The contents are
    /// private and affine; downstream code cannot edit or replay the proof.
    sealed: HashSet<ExpectedRootCallAction>,
}

struct DischargedCallGroup {
    region: EmissionRegion,
    dependencies: HashSet<EmissionRegion>,
    emitted: HashSet<ExpectedRootCallAction>,
    expected: HashSet<ExpectedRootCallAction>,
    reused: Vec<RootCallReuse>,
}

impl std::fmt::Debug for CommittedCallActionLedger {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter
            .debug_struct("CommittedCallActionLedger")
            .field("sealed_count", &self.sealed.len())
            .finish()
    }
}

impl CommittedCallActionLedger {
    pub(super) fn construct<'dae>(
        view: dae::DaeView<'dae>,
        groups: impl IntoIterator<Item = PreparedCallActions>,
    ) -> Result<Self, GalecTargetError> {
        let mut regions_by_owner = HashMap::new();
        let mut expected = HashSet::new();
        let mut emitted_by_region = HashMap::<_, HashSet<_>>::new();
        let mut reused_by_region = Vec::new();
        for group in groups {
            record_group(view, &mut regions_by_owner, &group)?;
            let group = discharge_group(view, group)?;
            expected.extend(group.expected.iter().cloned());
            emitted_by_region
                .entry(group.region)
                .or_default()
                .extend(group.emitted.iter().cloned());
            reused_by_region.push(group);
        }
        for group in &reused_by_region {
            for reuse in &group.reused {
                validate_reuse_producers(view, group, reuse, &emitted_by_region)?;
            }
        }
        Ok(Self { sealed: expected })
    }

    pub(super) fn close_package<'origin>(
        self,
        issuer: rumoca_ir_galec::AlgorithmCodePackageIssuer<'origin>,
        block: gast::Block,
        metadata: AlgorithmCodePackageMetadata,
        function_ledgers: Vec<CommittedFunctionCallActionLedger>,
    ) -> Result<
        rumoca_ir_galec::OriginBoundAlgorithmCodePackage<'origin>,
        rumoca_ir_galec::package::PackageError,
    > {
        let function_seals = function_ledgers
            .into_iter()
            .map(|ledger| ledger.sealed)
            .collect::<Vec<_>>();
        let package = issuer.construct(block, metadata);
        drop(self.sealed);
        drop(function_seals);
        package
    }

    #[cfg(test)]
    pub(super) fn occurrence_count(groups: &[PreparedCallActions]) -> usize {
        groups.iter().map(|group| group.actions.len()).sum()
    }

    #[cfg(test)]
    pub(super) fn duplicate_first(groups: &mut [PreparedCallActions]) {
        let group = groups
            .iter_mut()
            .find(|group| !group.actions.is_empty())
            .expect("test emission has one committed call action");
        let first = &group.actions[0];
        group.actions.push(RootCallAction {
            owner: first.owner,
            activation: first.activation.clone(),
            materialized: None,
        });
    }

    #[cfg(test)]
    pub(super) fn omit_first(groups: &mut [PreparedCallActions]) {
        let group = groups
            .iter_mut()
            .find(|group| !group.actions.is_empty())
            .expect("test emission has one committed call action");
        group.actions.remove(0);
    }

    #[cfg(test)]
    pub(super) fn move_first_to_another_group(groups: &mut [PreparedCallActions]) {
        let source = groups
            .iter()
            .position(|group| !group.actions.is_empty())
            .expect("test emission has one committed call action");
        let destination = (0..groups.len())
            .find(|candidate| *candidate != source)
            .expect("test emission has a second transaction");
        let action = groups[source].actions.remove(0);
        groups[destination].actions.push(action);
    }
}

fn discharge_group<'dae>(
    view: dae::DaeView<'dae>,
    group: PreparedCallActions,
) -> Result<DischargedCallGroup, GalecTargetError> {
    let emitted = group
        .actions
        .iter()
        .map(ExpectedRootCallAction::from_emitted)
        .collect::<HashSet<_>>();
    if let Some(foreign) = emitted.difference(&group.expected).next() {
        return Err(owner_error(
            view,
            foreign.owner,
            "foreign-call-owner",
            "was emitted outside the transaction that reached its source call",
        ));
    }
    let reused_reaches = group
        .reused
        .iter()
        .map(|reuse| validate_group_reuse(view, &group.expected, reuse))
        .collect::<Result<HashSet<_>, _>>()?;
    if let Some(missing) = group
        .expected
        .iter()
        .find(|expected| !emitted.contains(*expected) && !reused_reaches.contains(*expected))
    {
        return Err(owner_error(
            view,
            missing.owner,
            "missing-call-owner",
            "was reached in one emission transaction but not discharged there",
        ));
    }
    Ok(DischargedCallGroup {
        region: group.region,
        dependencies: group.dependencies,
        emitted,
        expected: group.expected,
        reused: group.reused,
    })
}

fn validate_group_reuse<'dae>(
    view: dae::DaeView<'dae>,
    expected: &HashSet<ExpectedRootCallAction>,
    reuse: &RootCallReuse,
) -> Result<ExpectedRootCallAction, GalecTargetError> {
    if expected.contains(&reuse.reached)
        && reuse
            .producers
            .iter()
            .all(|producer| producer.owner == reuse.reached.owner)
    {
        return Ok(reuse.reached.clone());
    }
    Err(owner_error(
        view,
        reuse.reached.owner,
        "foreign-call-reuse",
        "claimed a dominating execution outside the transaction that reached it",
    ))
}

fn validate_reuse_producers<'dae>(
    view: dae::DaeView<'dae>,
    group: &DischargedCallGroup,
    reuse: &RootCallReuse,
    emitted_by_region: &HashMap<EmissionRegion, HashSet<ExpectedRootCallAction>>,
) -> Result<(), GalecTargetError> {
    let producer_actions = match reuse.producer_region {
        Some(producer_region)
            if producer_region != group.region && group.dependencies.contains(&producer_region) =>
        {
            emitted_by_region.get(&producer_region)
        }
        Some(_) => {
            return Err(owner_error(
                view,
                reuse.reached.owner,
                "foreign-call-reuse",
                "retained a producer without its exact scheduler dependency",
            ));
        }
        None => Some(&group.emitted),
    };
    let missing = reuse
        .producers
        .iter()
        .find(|producer| !producer_actions.is_some_and(|set| set.contains(*producer)));
    if let Some(missing) = missing {
        return Err(owner_error(
            view,
            missing.owner,
            "missing-call-owner",
            "was named by a reuse capability but has no committed producer action",
        ));
    }
    Ok(())
}

/// Per-protected-function proof that one complete issued call path has one
/// committed evaluation in the emitted function body.
pub(super) struct CommittedFunctionCallActionLedger {
    sealed: HashSet<FunctionCallAction>,
}

impl CommittedFunctionCallActionLedger {
    pub(super) fn construct<'dae>(
        view: dae::DaeView<'dae>,
        expected: HashSet<FunctionCallAction>,
        reused: Vec<FunctionCallReuse>,
        actions: impl IntoIterator<Item = FunctionCallAction>,
    ) -> Result<Self, GalecTargetError> {
        let mut committed = HashSet::new();
        for action in actions {
            if !committed.insert(action.clone()) {
                return Err(owner_error(
                    view,
                    action.owner,
                    "repeated-function-call-owner",
                    "would be evaluated more than once along one protected-function call path",
                ));
            }
        }
        if let Some(foreign) = committed.difference(&expected).next() {
            return Err(owner_error(
                view,
                foreign.owner,
                "foreign-function-call-owner",
                "was emitted without a reached protected-function call capability",
            ));
        }
        let mut reused_reaches = HashSet::new();
        for reuse in reused {
            if !expected.contains(&reuse.reached)
                || reuse
                    .producers
                    .iter()
                    .any(|producer| producer.scope != reuse.reached.scope)
            {
                return Err(owner_error(
                    view,
                    reuse.reached.owner,
                    "foreign-function-call-reuse",
                    "claimed a dominating protected-function execution without a reached call",
                ));
            }
            if let Some(missing) = reuse
                .producers
                .iter()
                .find(|producer| !committed.contains(*producer))
            {
                return Err(owner_error(
                    view,
                    missing.owner,
                    "missing-function-call-owner",
                    "was named by a protected reuse capability but has no committed action",
                ));
            }
            reused_reaches.insert(reuse.reached);
        }
        if let Some(missing) = expected
            .iter()
            .find(|expected| !committed.contains(*expected) && !reused_reaches.contains(*expected))
        {
            return Err(owner_error(
                view,
                missing.owner,
                "missing-function-call-owner",
                "was reached in a protected function but has no committed action or reuse",
            ));
        }
        Ok(Self { sealed: expected })
    }
}

fn record_group<'dae>(
    view: dae::DaeView<'dae>,
    regions_by_owner: &mut HashMap<u32, Vec<(EmissionRegion, Vec<ConditionalActivationKey>)>>,
    group: &PreparedCallActions,
) -> Result<(), GalecTargetError> {
    for action in &group.actions {
        let previous = regions_by_owner.entry(action.owner).or_default();
        if let Some((region, _)) = previous
            .iter()
            .find(|(_, activation)| !assigned_primitives::disjoint(activation, &action.activation))
        {
            if *region == group.region {
                return Err(repeated_owner(view, action.owner));
            }
            return Err(cross_region_owner(view, action.owner));
        }
        previous.push((group.region, action.activation.clone()));
    }
    Ok(())
}

fn repeated_owner(view: dae::DaeView<'_>, owner: u32) -> GalecTargetError {
    owner_error(
        view,
        owner,
        "repeated-call-owner",
        "would be evaluated more than once in one DoStep emission region",
    )
}

fn cross_region_owner(view: dae::DaeView<'_>, owner: u32) -> GalecTargetError {
    owner_error(
        view,
        owner,
        "cross-region-call-owner",
        "would execute in more than one DoStep emission region; shared execution ownership is not implemented",
    )
}

fn owner_error(
    view: dae::DaeView<'_>,
    owner: u32,
    feature: &'static str,
    detail: &str,
) -> GalecTargetError {
    let owner = view
        .expression_id(usize::try_from(owner).expect("checked expression index fits usize"))
        .expect("committed call owner identity resolves");
    let span = view
        .expression(owner)
        .expect("committed call owner resolves")
        .provenance()
        .span();
    unsupported(
        feature,
        format!("function call owner #{} {detail}", owner.index()),
        span,
    )
}
