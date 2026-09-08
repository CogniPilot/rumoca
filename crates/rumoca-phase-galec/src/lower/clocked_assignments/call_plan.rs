//! Immutable selection and grouping plan for clocked `DoStep` roots.
//!
//! This plan chooses which already-checked B.1b, B.1c, and event roots belong
//! to each clock domain. It deliberately does not lower expressions or infer
//! function-call ownership: the emitter commits that ownership alongside the
//! statements it actually produces.

use super::*;

pub(crate) struct ClockedCallPlan<'refs, 'dae> {
    real_by_clock: HashMap<dae::ClockId<'dae>, ClockedRealDomainPlan<'refs, 'dae>>,
    discrete_values_by_clock: HashMap<dae::ClockId<'dae>, Vec<dae::DiscreteValueOwnerId<'dae>>>,
    event_actions_by_clock: HashMap<dae::ClockId<'dae>, Vec<dae::EventActionId<'dae>>>,
}

pub(super) struct ClockedRealDomainPlan<'refs, 'dae> {
    planned: Vec<PlannedDiscreteReal<'refs, 'dae>>,
    groups: Vec<Vec<usize>>,
}

impl<'refs, 'dae> ClockedRealDomainPlan<'refs, 'dae> {
    pub(super) fn planned(&self) -> &[PlannedDiscreteReal<'refs, 'dae>] {
        &self.planned
    }

    pub(super) fn groups(&self) -> &[Vec<usize>] {
        &self.groups
    }
}

impl<'refs, 'dae> ClockedCallPlan<'refs, 'dae> {
    pub(crate) fn construct(
        lowering: BlockLowering<'refs, 'dae>,
        admitted_clocks: &HashSet<u32>,
        unclocked_owner: dae::ClockId<'dae>,
    ) -> Result<Self, GalecTargetError> {
        let view = lowering.view;
        let mut admitted = admitted_clocks.iter().copied().collect::<Vec<_>>();
        admitted.sort_unstable();
        let admitted = admitted
            .into_iter()
            .filter_map(|clock| {
                usize::try_from(clock)
                    .ok()
                    .and_then(|index| view.clock_id(index))
            })
            .collect::<Vec<_>>();

        let mut real_by_clock = HashMap::new();
        for clock in admitted.iter().copied() {
            let planned = plan_clocked_discrete_reals(view, clock, lowering.by_id)?;
            let groups = group_planned_discrete_reals(view, &planned);
            real_by_clock.insert(clock, ClockedRealDomainPlan { planned, groups });
        }

        let discrete_values_by_clock = plan_discrete_values(view, &admitted, unclocked_owner)?;
        let event_actions_by_clock = plan_event_actions(view, &admitted, unclocked_owner)?;

        Ok(Self {
            real_by_clock,
            discrete_values_by_clock,
            event_actions_by_clock,
        })
    }

    pub(super) fn real_domain(
        &self,
        clock: dae::ClockId<'dae>,
    ) -> &ClockedRealDomainPlan<'refs, 'dae> {
        self.real_by_clock
            .get(&clock)
            .expect("admitted clock has one constructed discrete-Real plan")
    }

    pub(super) fn discrete_value_domain(
        &self,
        clock: dae::ClockId<'dae>,
    ) -> &[dae::DiscreteValueOwnerId<'dae>] {
        self.discrete_values_by_clock
            .get(&clock)
            .map_or(&[], Vec::as_slice)
    }

    pub(super) fn event_action_domain(
        &self,
        clock: dae::ClockId<'dae>,
    ) -> &[dae::EventActionId<'dae>] {
        self.event_actions_by_clock
            .get(&clock)
            .map_or(&[], Vec::as_slice)
    }
}

fn plan_discrete_values<'dae>(
    view: dae::DaeView<'dae>,
    admitted: &[dae::ClockId<'dae>],
    unclocked_owner: dae::ClockId<'dae>,
) -> Result<HashMap<dae::ClockId<'dae>, Vec<dae::DiscreteValueOwnerId<'dae>>>, GalecTargetError> {
    let clock_owners = discrete_value_clock_owners(view);
    let mut by_clock = HashMap::<_, Vec<_>>::new();
    for index in 0..view.discrete_value_owner_count() {
        let owner_id = view
            .discrete_value_owner_id(index)
            .expect("dense checked B.1c owner identity");
        let owner = view
            .discrete_value_owner(owner_id)
            .expect("checked B.1c owner resolves");
        append_discrete_value_owner(
            view,
            admitted,
            unclocked_owner,
            &clock_owners,
            owner_id,
            owner,
            &mut by_clock,
        )?;
    }
    Ok(by_clock)
}

fn append_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    admitted: &[dae::ClockId<'dae>],
    unclocked_owner: dae::ClockId<'dae>,
    clock_owners: &HashMap<u32, u32>,
    owner_id: dae::DiscreteValueOwnerId<'dae>,
    owner: dae::DiscreteValueOwnerView<'dae>,
    by_clock: &mut HashMap<dae::ClockId<'dae>, Vec<dae::DiscreteValueOwnerId<'dae>>>,
) -> Result<(), GalecTargetError> {
    for clock in admitted.iter().copied() {
        if !discrete_value_owner_runs_in_domain(
            view,
            owner,
            clock,
            clock == unclocked_owner,
            clock_owners,
        )? {
            continue;
        }
        by_clock.entry(clock).or_default().push(owner_id);
    }
    Ok(())
}

fn plan_event_actions<'dae>(
    view: dae::DaeView<'dae>,
    admitted: &[dae::ClockId<'dae>],
    unclocked_owner: dae::ClockId<'dae>,
) -> Result<HashMap<dae::ClockId<'dae>, Vec<dae::EventActionId<'dae>>>, GalecTargetError> {
    let mut by_clock = HashMap::<_, Vec<_>>::new();
    for (action_id, action) in view.event_actions() {
        if !matches!(
            action.operation(),
            dae::EventActionOperation::Assert { level: None, .. }
        ) {
            return Err(unsupported(
                "event-action",
                format!(
                    "event action `{}` cannot be represented in GALEC DoStep",
                    event_name(action.operation())
                ),
                action.provenance().span(),
            ));
        }
        append_event_action(
            view,
            admitted,
            unclocked_owner,
            action_id,
            action,
            &mut by_clock,
        );
    }
    Ok(by_clock)
}

fn append_event_action<'dae>(
    view: dae::DaeView<'dae>,
    admitted: &[dae::ClockId<'dae>],
    unclocked_owner: dae::ClockId<'dae>,
    action_id: dae::EventActionId<'dae>,
    action: dae::EventActionView<'dae>,
    by_clock: &mut HashMap<dae::ClockId<'dae>, Vec<dae::EventActionId<'dae>>>,
) {
    let trigger_clocks = condition_clocks(view, action.trigger());
    for clock in admitted.iter().copied() {
        let runs_unclocked = trigger_clocks.is_empty() && clock == unclocked_owner;
        if runs_unclocked || trigger_clocks.contains(&clock.index()) {
            by_clock.entry(clock).or_default().push(action_id);
        }
    }
}
