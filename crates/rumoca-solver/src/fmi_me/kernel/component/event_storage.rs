//! Construction-reserved mutable storage for FMI event transactions.

use super::super::*;
use rumoca_ir_solve::fmi::{
    FmiDeadlineWidth, FmiIndicatorDomainWidth, FmiPublishedIndicatorWidth, FmiRootValueWidth,
};
use std::marker::PhantomData;

pub(in crate::fmi_me::kernel) enum WorkingPublishedIndicatorRole {}
pub(in crate::fmi_me::kernel) enum FmiPublicationIndicatorRole {}
pub(in crate::fmi_me::kernel) enum CachedPublishedIndicatorRole {}
pub(in crate::fmi_me::kernel) enum RootIndicatorRole {}
pub(in crate::fmi_me::kernel) enum DeadlineIndicatorRole {}
pub(in crate::fmi_me::kernel) enum WorkingIndicatorDomainRole {}
pub(in crate::fmi_me::kernel) enum FrozenIndicatorDomainRole {}

mod fixed_role_buffer {
    use super::*;

    pub(in crate::fmi_me::kernel) struct FixedRoleBuffer<T, Role> {
        values: Box<[T]>,
        role: PhantomData<fn() -> Role>,
    }

    impl<T: Clone, Role> Clone for FixedRoleBuffer<T, Role> {
        fn clone(&self) -> Self {
            Self {
                values: self.values.clone(),
                role: PhantomData,
            }
        }
    }

    impl<T: Copy, Role> FixedRoleBuffer<T, Role> {
        fn try_filled(len: usize, value: T, context: &'static str) -> Result<Self, MeError> {
            let mut values = Vec::new();
            values
                .try_reserve_exact(len)
                .map_err(|_| MeError::Allocation {
                    context,
                    entries: len,
                })?;
            values.resize(len, value);
            Ok(Self {
                values: values.into_boxed_slice(),
                role: PhantomData,
            })
        }

        pub(super) fn copy_from(&mut self, source: &Self) {
            self.values.copy_from_slice(&source.values);
        }

        pub(super) fn try_same_shape(
            &self,
            value: T,
            context: &'static str,
        ) -> Result<Self, MeError> {
            Self::try_filled(self.values.len(), value, context)
        }

        pub(in crate::fmi_me::kernel) fn as_slice(&self) -> &[T] {
            &self.values
        }

        pub(in crate::fmi_me::kernel) fn as_mut_slice(&mut self) -> &mut [T] {
            &mut self.values
        }

        pub(in crate::fmi_me::kernel) fn is_empty(&self) -> bool {
            self.values.is_empty()
        }

        #[cfg(test)]
        pub(in crate::fmi_me::kernel) fn storage_identity(&self) -> (usize, usize) {
            (self.values.as_ptr() as usize, self.values.len())
        }
    }

    impl FixedRoleBuffer<f64, RootIndicatorRole> {
        pub(super) fn try_for_width(width: FmiRootValueWidth) -> Result<Self, MeError> {
            Self::try_filled(width.len(), 0.0, "event-indicator roots")
        }
    }

    impl FixedRoleBuffer<f64, DeadlineIndicatorRole> {
        pub(super) fn try_for_width(width: FmiDeadlineWidth) -> Result<Self, MeError> {
            Self::try_filled(width.len(), 0.0, "event-indicator dynamic-time deadlines")
        }
    }

    pub(in crate::fmi_me::kernel) trait PublishedIndicatorPopulation {}

    impl PublishedIndicatorPopulation for WorkingPublishedIndicatorRole {}
    impl PublishedIndicatorPopulation for FmiPublicationIndicatorRole {}
    impl PublishedIndicatorPopulation for CachedPublishedIndicatorRole {}

    impl<Role: PublishedIndicatorPopulation> FixedRoleBuffer<f64, Role> {
        pub(super) fn try_for_published_width(
            width: FmiPublishedIndicatorWidth,
            context: &'static str,
        ) -> Result<Self, MeError> {
            Self::try_filled(width.len(), 0.0, context)
        }
    }

    impl FixedRoleBuffer<f64, CachedPublishedIndicatorRole> {
        pub(super) fn copy_from_publication(
            &mut self,
            source: &FixedRoleBuffer<f64, FmiPublicationIndicatorRole>,
        ) {
            self.values.copy_from_slice(&source.values);
        }

        pub(super) fn copy_into_publication(
            &self,
            target: &mut FixedRoleBuffer<f64, FmiPublicationIndicatorRole>,
        ) {
            target.values.copy_from_slice(&self.values);
        }

        pub(super) fn copy_into_working(
            &self,
            target: &mut FixedRoleBuffer<f64, WorkingPublishedIndicatorRole>,
        ) {
            target.values.copy_from_slice(&self.values);
        }
    }

    impl FixedRoleBuffer<f64, FmiPublicationIndicatorRole> {
        pub(super) fn copy_from_working(
            &mut self,
            source: &FixedRoleBuffer<f64, WorkingPublishedIndicatorRole>,
        ) {
            self.values.copy_from_slice(&source.values);
        }
    }

    pub(in crate::fmi_me::kernel) trait IndicatorDomainPopulation {}

    impl IndicatorDomainPopulation for WorkingIndicatorDomainRole {}
    impl IndicatorDomainPopulation for FrozenIndicatorDomainRole {}

    impl<Role: IndicatorDomainPopulation> FixedRoleBuffer<bool, Role> {
        pub(super) fn try_for_domain_width(
            width: FmiIndicatorDomainWidth,
            context: &'static str,
        ) -> Result<Self, MeError> {
            Self::try_filled(width.len(), false, context)
        }
    }

    impl FixedRoleBuffer<bool, WorkingIndicatorDomainRole> {
        pub(super) fn copy_from_frozen(
            &mut self,
            source: &FixedRoleBuffer<bool, FrozenIndicatorDomainRole>,
        ) {
            self.values.copy_from_slice(&source.values);
        }
    }
}

use fixed_role_buffer::FixedRoleBuffer;

pub(in crate::fmi_me::kernel) type WorkingPublishedIndicatorValues =
    FixedRoleBuffer<f64, WorkingPublishedIndicatorRole>;
pub(in crate::fmi_me::kernel) type FmiPublicationIndicatorValues =
    FixedRoleBuffer<f64, FmiPublicationIndicatorRole>;
type CachedPublishedIndicatorValues = FixedRoleBuffer<f64, CachedPublishedIndicatorRole>;
pub(in crate::fmi_me::kernel) type RootIndicatorValues = FixedRoleBuffer<f64, RootIndicatorRole>;
pub(in crate::fmi_me::kernel) type DeadlineIndicatorValues =
    FixedRoleBuffer<f64, DeadlineIndicatorRole>;
type WorkingIndicatorDomains = FixedRoleBuffer<bool, WorkingIndicatorDomainRole>;
type FrozenIndicatorDomains = FixedRoleBuffer<bool, FrozenIndicatorDomainRole>;

trait IndicatorPlanStorageIssuer {
    fn try_root_values(&self) -> Result<RootIndicatorValues, MeError>;
    fn try_deadline_values(&self) -> Result<DeadlineIndicatorValues, MeError>;
    fn try_working_values(
        &self,
        context: &'static str,
    ) -> Result<WorkingPublishedIndicatorValues, MeError>;
    fn try_publication_values(
        &self,
        context: &'static str,
    ) -> Result<FmiPublicationIndicatorValues, MeError>;
    fn try_cached_values(
        &self,
        context: &'static str,
    ) -> Result<CachedPublishedIndicatorValues, MeError>;
    fn try_working_domains(
        &self,
        context: &'static str,
    ) -> Result<WorkingIndicatorDomains, MeError>;
    fn try_frozen_domains(&self, context: &'static str) -> Result<FrozenIndicatorDomains, MeError>;
}

impl IndicatorPlanStorageIssuer for FmiIndicatorPlan {
    fn try_root_values(&self) -> Result<RootIndicatorValues, MeError> {
        RootIndicatorValues::try_for_width(self.root_value_width())
    }

    fn try_deadline_values(&self) -> Result<DeadlineIndicatorValues, MeError> {
        DeadlineIndicatorValues::try_for_width(self.deadline_width())
    }

    fn try_working_values(
        &self,
        context: &'static str,
    ) -> Result<WorkingPublishedIndicatorValues, MeError> {
        WorkingPublishedIndicatorValues::try_for_published_width(self.published_width(), context)
    }

    fn try_publication_values(
        &self,
        context: &'static str,
    ) -> Result<FmiPublicationIndicatorValues, MeError> {
        FmiPublicationIndicatorValues::try_for_published_width(self.published_width(), context)
    }

    fn try_cached_values(
        &self,
        context: &'static str,
    ) -> Result<CachedPublishedIndicatorValues, MeError> {
        CachedPublishedIndicatorValues::try_for_published_width(self.published_width(), context)
    }

    fn try_working_domains(
        &self,
        context: &'static str,
    ) -> Result<WorkingIndicatorDomains, MeError> {
        WorkingIndicatorDomains::try_for_domain_width(self.domain_width(), context)
    }

    fn try_frozen_domains(&self, context: &'static str) -> Result<FrozenIndicatorDomains, MeError> {
        FrozenIndicatorDomains::try_for_domain_width(self.domain_width(), context)
    }
}

#[derive(Clone)]
struct CachedEventIndicators {
    valid: bool,
    time: f64,
    state: Box<[f64]>,
    values: CachedPublishedIndicatorValues,
}

impl CachedEventIndicators {
    fn try_reserved(
        plan: &FmiIndicatorPlan,
        live_state: &[f64],
        start_time: f64,
    ) -> Result<Self, MeError> {
        let mut state = Vec::new();
        state
            .try_reserve_exact(live_state.len())
            .map_err(|_| MeError::Allocation {
                context: "event-indicator cache state",
                entries: live_state.len(),
            })?;
        state.resize(live_state.len(), 0.0);
        Ok(Self {
            valid: false,
            time: start_time,
            state: state.into_boxed_slice(),
            values: plan.try_cached_values("event-indicator cache values")?,
        })
    }

    fn copy_from(&mut self, source: &Self) {
        self.valid = source.valid;
        self.time = source.time;
        self.state.copy_from_slice(&source.state);
        self.values.copy_from(&source.values);
    }

    fn try_same_shape(&self) -> Result<Self, MeError> {
        let mut state = Vec::new();
        state
            .try_reserve_exact(self.state.len())
            .map_err(|_| MeError::Allocation {
                context: "staged event-indicator cache state",
                entries: self.state.len(),
            })?;
        state.resize(self.state.len(), 0.0);
        Ok(Self {
            valid: false,
            time: self.time,
            state: state.into_boxed_slice(),
            values: self
                .values
                .try_same_shape(0.0, "staged event-indicator cache values")?,
        })
    }

    fn copy_into_publication(
        &self,
        time: f64,
        state: &[f64],
        out: &mut FmiPublicationIndicatorValues,
    ) -> bool {
        if !self.valid
            || self.time.to_bits() != time.to_bits()
            || !state_values_match(&self.state, state)
        {
            return false;
        }
        self.values.copy_into_publication(out);
        true
    }

    fn copy_into_working(
        &self,
        time: f64,
        state: &[f64],
        out: &mut WorkingPublishedIndicatorValues,
    ) -> bool {
        if !self.valid
            || self.time.to_bits() != time.to_bits()
            || !state_values_match(&self.state, state)
        {
            return false;
        }
        self.values.copy_into_working(out);
        true
    }

    fn store_publication(
        &mut self,
        time: f64,
        state: &[f64],
        values: &FmiPublicationIndicatorValues,
    ) {
        self.time = time;
        self.state.copy_from_slice(state);
        self.values.copy_from_publication(values);
        self.valid = true;
    }

    #[cfg(test)]
    fn storage_identities(&self) -> [(usize, usize); 2] {
        [
            (self.state.as_ptr() as usize, self.state.len()),
            self.values.storage_identity(),
        ]
    }
}

#[derive(Clone)]
pub(in crate::fmi_me::kernel) struct EventIndicatorStorageSnapshot {
    frozen_domains: FrozenIndicatorDomains,
    cache: CachedEventIndicators,
}

pub(in crate::fmi_me::kernel) struct EventIndicatorStorage {
    root_values: RefCell<RootIndicatorValues>,
    deadline_values: RefCell<DeadlineIndicatorValues>,
    working_values: RefCell<WorkingPublishedIndicatorValues>,
    publication_values: RefCell<FmiPublicationIndicatorValues>,
    working_domains: WorkingIndicatorDomains,
    frozen_domains: FrozenIndicatorDomains,
    cache: RefCell<CachedEventIndicators>,
}

impl EventIndicatorStorage {
    pub(in crate::fmi_me::kernel) fn try_construct(
        plan: &FmiIndicatorPlan,
        live_state: &[f64],
        start_time: f64,
    ) -> Result<Self, MeError> {
        Ok(Self {
            root_values: RefCell::new(plan.try_root_values()?),
            deadline_values: RefCell::new(plan.try_deadline_values()?),
            working_values: RefCell::new(
                plan.try_working_values("event-indicator working values")?,
            ),
            publication_values: RefCell::new(
                plan.try_publication_values("event-indicator getter output")?,
            ),
            working_domains: plan.try_working_domains("event-indicator working domains")?,
            frozen_domains: plan.try_frozen_domains("event-indicator frozen domains")?,
            cache: RefCell::new(CachedEventIndicators::try_reserved(
                plan, live_state, start_time,
            )?),
        })
    }

    pub(in crate::fmi_me::kernel) fn try_detached_stage(&self) -> Result<Self, MeError> {
        let mut stage = Self {
            root_values: RefCell::new(
                self.root_values
                    .borrow()
                    .try_same_shape(0.0, "staged event-indicator roots")?,
            ),
            deadline_values: RefCell::new(
                self.deadline_values
                    .borrow()
                    .try_same_shape(0.0, "staged event-indicator dynamic-time deadlines")?,
            ),
            working_values: RefCell::new(
                self.working_values
                    .borrow()
                    .try_same_shape(0.0, "staged event-indicator working values")?,
            ),
            publication_values: RefCell::new(
                self.publication_values
                    .borrow()
                    .try_same_shape(0.0, "staged event-indicator getter output")?,
            ),
            working_domains: self
                .working_domains
                .try_same_shape(false, "staged event-indicator working domains")?,
            frozen_domains: self
                .frozen_domains
                .try_same_shape(false, "staged event-indicator frozen domains")?,
            cache: RefCell::new(self.cache.borrow().try_same_shape()?),
        };
        stage.copy_mutable_from(self);
        Ok(stage)
    }

    pub(in crate::fmi_me::kernel) fn copy_mutable_from(&mut self, source: &Self) {
        self.root_values
            .borrow_mut()
            .copy_from(&source.root_values.borrow());
        self.deadline_values
            .borrow_mut()
            .copy_from(&source.deadline_values.borrow());
        self.working_values
            .borrow_mut()
            .copy_from(&source.working_values.borrow());
        self.working_domains.copy_from(&source.working_domains);
        self.frozen_domains.copy_from(&source.frozen_domains);
        self.cache.borrow_mut().copy_from(&source.cache.borrow());
    }

    pub(in crate::fmi_me::kernel) fn snapshot(&self) -> EventIndicatorStorageSnapshot {
        EventIndicatorStorageSnapshot {
            frozen_domains: self.frozen_domains.clone(),
            cache: self.cache.borrow().clone(),
        }
    }

    pub(in crate::fmi_me::kernel) fn restore(&mut self, snapshot: &EventIndicatorStorageSnapshot) {
        self.frozen_domains.copy_from(&snapshot.frozen_domains);
        self.cache.borrow_mut().copy_from(&snapshot.cache);
    }

    pub(in crate::fmi_me::kernel) fn clear_cache(&self) {
        self.cache.borrow_mut().valid = false;
    }

    pub(in crate::fmi_me::kernel) fn copy_cache_into_publication(
        &self,
        time: f64,
        state: &[f64],
        out: &mut FmiPublicationIndicatorValues,
    ) -> bool {
        self.cache.borrow().copy_into_publication(time, state, out)
    }

    pub(in crate::fmi_me::kernel) fn copy_cache_into_working(
        &self,
        time: f64,
        state: &[f64],
        out: &mut WorkingPublishedIndicatorValues,
    ) -> bool {
        self.cache.borrow().copy_into_working(time, state, out)
    }

    pub(in crate::fmi_me::kernel) fn store_publication_cache(
        &self,
        time: f64,
        state: &[f64],
        values: &FmiPublicationIndicatorValues,
    ) {
        self.cache
            .borrow_mut()
            .store_publication(time, state, values);
    }

    pub(in crate::fmi_me::kernel) fn copy_working_to_publication(
        &self,
        working: &WorkingPublishedIndicatorValues,
        publication: &mut FmiPublicationIndicatorValues,
    ) {
        publication.copy_from_working(working);
    }

    pub(in crate::fmi_me::kernel) fn root_values_mut(
        &self,
    ) -> std::cell::RefMut<'_, RootIndicatorValues> {
        self.root_values.borrow_mut()
    }

    pub(in crate::fmi_me::kernel) fn deadline_values_mut(
        &self,
    ) -> std::cell::RefMut<'_, DeadlineIndicatorValues> {
        self.deadline_values.borrow_mut()
    }

    pub(in crate::fmi_me::kernel) fn working_values_mut(
        &self,
    ) -> std::cell::RefMut<'_, WorkingPublishedIndicatorValues> {
        self.working_values.borrow_mut()
    }

    pub(in crate::fmi_me::kernel) fn publication_values_mut(
        &self,
    ) -> std::cell::RefMut<'_, FmiPublicationIndicatorValues> {
        self.publication_values.borrow_mut()
    }

    pub(in crate::fmi_me::kernel) fn working_values(
        &self,
    ) -> std::cell::Ref<'_, WorkingPublishedIndicatorValues> {
        self.working_values.borrow()
    }

    pub(in crate::fmi_me::kernel) fn working_domains(&self) -> &[bool] {
        self.working_domains.as_slice()
    }

    pub(in crate::fmi_me::kernel) fn frozen_domains(&self) -> &[bool] {
        self.frozen_domains.as_slice()
    }

    pub(in crate::fmi_me::kernel) fn freeze_working_domains(&mut self) {
        let values = self.working_values.borrow();
        for ((working, frozen), value) in self
            .working_domains
            .as_mut_slice()
            .iter_mut()
            .zip(self.frozen_domains.as_mut_slice())
            .zip(values.as_slice())
        {
            *working = *frozen;
            *frozen = *value > 0.0;
        }
    }

    pub(in crate::fmi_me::kernel) fn copy_frozen_to_working_domains(&mut self) {
        self.working_domains.copy_from_frozen(&self.frozen_domains);
    }

    pub(in crate::fmi_me::kernel) fn set_domain(&mut self, position: usize, positive: bool) {
        self.frozen_domains.as_mut_slice()[position] = positive;
        self.working_domains.as_mut_slice()[position] = positive;
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn matches_snapshot(
        &self,
        snapshot: &EventIndicatorStorageSnapshot,
    ) -> bool {
        self.frozen_domains.as_slice() == snapshot.frozen_domains.as_slice()
            && cached_event_indicators_bit_eq(&self.cache.borrow(), &snapshot.cache)
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn flip_frozen_domain(&mut self, position: usize) {
        let domain = &mut self.frozen_domains.as_mut_slice()[position];
        *domain = !*domain;
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn role_widths(&self) -> (usize, usize, usize, usize) {
        (
            self.working_values.borrow().as_slice().len(),
            self.root_values.borrow().as_slice().len(),
            self.deadline_values.borrow().as_slice().len(),
            self.working_domains.as_slice().len(),
        )
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn domain_values(&self) -> (Vec<bool>, Vec<bool>) {
        (
            self.working_domains.as_slice().to_vec(),
            self.frozen_domains.as_slice().to_vec(),
        )
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn storage_identities(&self) -> [(usize, usize); 8] {
        let cache = self.cache.borrow();
        let [cache_state, cache_values] = cache.storage_identities();
        [
            self.root_values.borrow().storage_identity(),
            self.deadline_values.borrow().storage_identity(),
            self.working_values.borrow().storage_identity(),
            self.publication_values.borrow().storage_identity(),
            self.working_domains.storage_identity(),
            self.frozen_domains.storage_identity(),
            cache_values,
            cache_state,
        ]
    }
}

#[cfg(test)]
fn cached_event_indicators_bit_eq(
    left: &CachedEventIndicators,
    right: &CachedEventIndicators,
) -> bool {
    left.valid == right.valid
        && left.time.to_bits() == right.time.to_bits()
        && float_slice_bit_eq(&left.state, &right.state)
        && float_slice_bit_eq(left.values.as_slice(), right.values.as_slice())
}

#[derive(Clone)]
pub(in crate::fmi_me::kernel) struct EventVectorLatch {
    values: Vec<f64>,
    occupied: bool,
}

impl EventVectorLatch {
    pub(in crate::fmi_me::kernel) fn reserved(len: usize) -> Self {
        Self {
            values: vec![0.0; len],
            occupied: false,
        }
    }

    pub(in crate::fmi_me::kernel) fn set(
        &mut self,
        values: &[f64],
        context: &'static str,
    ) -> Result<(), MeError> {
        if values.len() != self.values.len() {
            return Err(contract(format!(
                "{context} has {} entries for constructed width {}",
                values.len(),
                self.values.len()
            )));
        }
        self.values.copy_from_slice(values);
        self.occupied = true;
        Ok(())
    }

    pub(in crate::fmi_me::kernel) fn get(&self, context: &'static str) -> Result<&[f64], MeError> {
        self.occupied
            .then_some(self.values.as_slice())
            .ok_or_else(|| contract(context))
    }

    pub(in crate::fmi_me::kernel) fn get_mut(
        &mut self,
        context: &'static str,
    ) -> Result<&mut [f64], MeError> {
        self.occupied
            .then_some(self.values.as_mut_slice())
            .ok_or_else(|| contract(context))
    }

    pub(in crate::fmi_me::kernel) fn clear(&mut self) {
        self.occupied = false;
    }

    pub(in crate::fmi_me::kernel) const fn is_occupied(&self) -> bool {
        self.occupied
    }

    pub(in crate::fmi_me::kernel) fn storage_mut(&mut self) -> &mut Vec<f64> {
        &mut self.values
    }

    pub(in crate::fmi_me::kernel) fn mark_occupied(&mut self) {
        self.occupied = true;
    }

    pub(in crate::fmi_me::kernel) fn copy_from(&mut self, source: &Self) {
        self.values.copy_from_slice(&source.values);
        self.occupied = source.occupied;
    }

    pub(in crate::fmi_me::kernel) fn restore(
        &mut self,
        source: Option<&Vec<f64>>,
    ) -> Result<(), MeError> {
        match source {
            Some(values) => self.set(values, "snapshot event latch"),
            None => {
                self.clear();
                Ok(())
            }
        }
    }

    pub(in crate::fmi_me::kernel) fn snapshot(&self) -> Option<Vec<f64>> {
        self.occupied.then(|| self.values.clone())
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn bit_eq(&self, source: &Option<Vec<f64>>) -> bool {
        match (self.occupied, source) {
            (false, None) => true,
            (true, Some(values)) => super::super::float_slice_bit_eq(&self.values, values),
            _ => false,
        }
    }

    #[cfg(test)]
    pub(in crate::fmi_me::kernel) fn storage_identity(&self) -> usize {
        self.values.as_ptr() as usize
    }
}

impl MeKernelBody {
    /// Reserve the one detached event transaction at construction.
    pub(super) fn construction_event_stage(&self) -> Result<Self, MeError> {
        Ok(Self {
            runtime: Rc::clone(&self.runtime),
            instance_brand: Rc::clone(&self.instance_brand),
            value_references: self.value_references.clone(),
            input_names: self.input_names.clone(),
            directional_state_references: self.directional_state_references.clone(),
            directional_derivative_references: self.directional_derivative_references.clone(),
            instance_name: self.instance_name,
            tolerance: self.tolerance,
            stop_time: self.stop_time,
            time: self.time,
            set_time_bounds: self.set_time_bounds,
            post_event_eval_time: self.post_event_eval_time,
            event_anchor_time: self.event_anchor_time,
            states: self.states.clone(),
            params: self.params.clone(),
            state_domain: self.state_domain,
            stop_schedule: self.stop_schedule.clone(),
            pending_event_entry: self.pending_event_entry,
            pending_state_event_entry: self.pending_state_event_entry,
            pending_event_stop: self.pending_event_stop,
            advance_state_to_event_right_limit: self.advance_state_to_event_right_limit,
            state_time_coincidence: self.state_time_coincidence,
            initial_event_pending: self.initial_event_pending,
            pending_root_crossings: {
                let mut crossings = Vec::with_capacity(self.pending_root_crossings.capacity());
                crossings.extend_from_slice(&self.pending_root_crossings);
                crossings
            },
            indicator_storage: self.indicator_storage.try_detached_stage()?,
            pending_event_pre_y: self.pending_event_pre_y.clone(),
            pending_event_pre_p: self.pending_event_pre_p.clone(),
            boundary_event_pre_y: self.boundary_event_pre_y.clone(),
            boundary_event_pre_p: self.boundary_event_pre_p.clone(),
            event_solver_y_work: self.event_solver_y_work.clone(),
            event_state_before: self.event_state_before.clone(),
            scheduled_root_index_scratch: {
                let mut indices = Vec::with_capacity(self.scheduled_root_index_scratch.capacity());
                indices.extend_from_slice(&self.scheduled_root_index_scratch);
                indices
            },
            root_override_scratch: {
                let mut overrides = Vec::with_capacity(self.root_override_scratch.capacity());
                overrides.extend_from_slice(&self.root_override_scratch);
                overrides
            },
            solver_y_guess: RefCell::new(self.solver_y_guess.borrow().clone()),
            derivative_output_scratch: RefCell::new(
                self.derivative_output_scratch.borrow().clone(),
            ),
            directional_seed_scratch: RefCell::new(self.directional_seed_scratch.borrow().clone()),
            directional_sensitivity_scratch: RefCell::new(
                self.directional_sensitivity_scratch.borrow().clone(),
            ),
            directional_serialized_scratch: RefCell::new(
                self.directional_serialized_scratch.borrow().clone(),
            ),
            accepted_derivative_scratch: RefCell::new(
                self.accepted_derivative_scratch.borrow().clone(),
            ),
            delay_params_scratch: RefCell::new(self.delay_params_scratch.borrow().clone()),
            delay_solver_y_scratch: RefCell::new(self.delay_solver_y_scratch.borrow().clone()),
            derivative_cache: RefCell::new(self.derivative_cache.borrow().clone()),
            continuous_linearization_cache: RefCell::new(
                self.continuous_linearization_cache.borrow().clone(),
            ),
            max_step_duration: self.max_step_duration,
            max_step_duration_value_reference: self.max_step_duration_value_reference,
            termination: self.termination.clone(),
            output_meta: self.output_meta.clone(),
            settled_initialization_y: self.settled_initialization_y.clone(),
            #[cfg(test)]
            verification_fail_next_enter_initialization: self
                .verification_fail_next_enter_initialization,
            #[cfg(test)]
            verification_fail_next_exit_initialization: self
                .verification_fail_next_exit_initialization,
            #[cfg(test)]
            verification_fail_next_update_discrete_states: self
                .verification_fail_next_update_discrete_states,
            #[cfg(test)]
            verification_fail_next_completed_integrator_step: self
                .verification_fail_next_completed_integrator_step,
            #[cfg(test)]
            verification_fail_next_enter_continuous_time_mode: self
                .verification_fail_next_enter_continuous_time_mode,
        })
    }

    /// Synchronize mutable continuation into construction-reserved staging.
    pub(in crate::fmi_me::kernel) fn prepare_event_stage_from(&mut self, source: &Self) {
        self.stop_time = source.stop_time;
        self.time = source.time;
        self.set_time_bounds = source.set_time_bounds;
        self.post_event_eval_time = source.post_event_eval_time;
        self.event_anchor_time = source.event_anchor_time;
        self.states.clone_from(&source.states);
        self.params.clone_from(&source.params);
        self.stop_schedule.clone_from(&source.stop_schedule);
        self.pending_event_entry = source.pending_event_entry;
        self.pending_state_event_entry = source.pending_state_event_entry;
        self.pending_event_stop = source.pending_event_stop;
        self.advance_state_to_event_right_limit = source.advance_state_to_event_right_limit;
        self.state_time_coincidence = source.state_time_coincidence;
        self.initial_event_pending = source.initial_event_pending;
        self.pending_root_crossings
            .clone_from(&source.pending_root_crossings);
        self.indicator_storage
            .copy_mutable_from(&source.indicator_storage);
        self.pending_event_pre_y
            .copy_from(&source.pending_event_pre_y);
        self.pending_event_pre_p
            .copy_from(&source.pending_event_pre_p);
        self.boundary_event_pre_y
            .copy_from(&source.boundary_event_pre_y);
        self.boundary_event_pre_p
            .copy_from(&source.boundary_event_pre_p);
        self.event_solver_y_work
            .copy_from_slice(&source.event_solver_y_work);
        self.event_state_before
            .copy_from_slice(&source.event_state_before);
        self.scheduled_root_index_scratch
            .clone_from(&source.scheduled_root_index_scratch);
        self.root_override_scratch
            .clone_from(&source.root_override_scratch);
        self.solver_y_guess
            .borrow_mut()
            .clone_from(&source.solver_y_guess.borrow());
        self.accepted_derivative_scratch
            .borrow_mut()
            .copy_from_slice(&source.accepted_derivative_scratch.borrow());
        self.delay_params_scratch
            .borrow_mut()
            .clone_from(&source.delay_params_scratch.borrow());
        self.delay_solver_y_scratch
            .borrow_mut()
            .clone_from(&source.delay_solver_y_scratch.borrow());
        self.derivative_cache
            .borrow_mut()
            .copy_from(&source.derivative_cache.borrow());
        self.continuous_linearization_cache
            .borrow_mut()
            .copy_from(&source.continuous_linearization_cache.borrow());
        self.max_step_duration = source.max_step_duration;
        self.termination.clone_from(&source.termination);
        self.settled_initialization_y
            .copy_from(&source.settled_initialization_y);
        #[cfg(test)]
        {
            self.verification_fail_next_exit_initialization =
                source.verification_fail_next_exit_initialization;
            self.verification_fail_next_enter_initialization =
                source.verification_fail_next_enter_initialization;
            self.verification_fail_next_update_discrete_states =
                source.verification_fail_next_update_discrete_states;
            self.verification_fail_next_completed_integrator_step =
                source.verification_fail_next_completed_integrator_step;
            self.verification_fail_next_enter_continuous_time_mode =
                source.verification_fail_next_enter_continuous_time_mode;
        }
    }
}
