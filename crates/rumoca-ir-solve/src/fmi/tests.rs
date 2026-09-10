use super::*;
use crate::SolveVariableValueKind;

mod parameter_profile;

#[test]
fn component_consumes_one_complete_model_into_codegen_view() {
    let component = FmiComponent::construct(SolveModel::default(), Vec::new())
        .expect("empty checked Solve model has an empty FMI inventory");
    let problem_address = std::ptr::from_ref(component.problem());

    let view = component.into_codegen_view();
    assert_eq!(std::ptr::from_ref(view.problem()), problem_address);
}

#[test]
fn component_rejects_an_invalid_complete_model_before_binding_metadata() {
    let mut model = SolveModel::default();
    model.problem.schema_version = 0;

    assert!(matches!(
        FmiComponent::construct(model, Vec::new()),
        Err(FmiComponentError::InvalidSolve(_))
    ));
}

#[test]
fn variable_metadata_is_available_only_through_borrowed_views() {
    let span = Span::DUMMY;
    let variable = FmiVariable {
        name: "x".to_string(),
        value_kind: SolveVariableValueKind::Real,
        dimensions: vec![2],
        backing: FmiValueBacking::SolveStorage {
            role: SolveVariableStorageRole::State,
            storage: FmiStorageRun {
                column: FmiStorageColumn::Y,
                base: 3,
                scalar_count: 2,
            },
            scalar_names: vec!["x[1]".to_string(), "x[2]".to_string()],
        },
        start: Some(vec![1.0, 2.0]),
        minimum: Some(vec![0.0, 0.0]),
        maximum: Some(vec![3.0, 4.0]),
        nominal: Some(vec![1.0, 1.0]),
        unit: Some("m".to_string()),
        description: Some("state".to_string()),
        causality: FmiCausality::Local,
        variability: FmiVariability::Continuous,
        initial: None,
        tunable: false,
        declaration: Some(span),
        value_reference_fmi3: 7,
    };

    assert_eq!(variable.name(), "x");
    assert_eq!(variable.scalar_names(), ["x[1]", "x[2]"]);
    assert_eq!(variable.role(), Some(SolveVariableStorageRole::State));
    assert_eq!(variable.value_kind(), SolveVariableValueKind::Real);
    assert_eq!(variable.dimensions(), [2]);
    let storage = variable
        .storage()
        .expect("a storage-backed entry has a run");
    assert_eq!(storage.column(), FmiStorageColumn::Y);
    assert_eq!(storage.base(), 3);
    assert_eq!(storage.scalar_count(), 2);
    assert_eq!(variable.start(), Some([1.0, 2.0].as_slice()));
    assert_eq!(variable.minimum(), Some([0.0, 0.0].as_slice()));
    assert_eq!(variable.maximum(), Some([3.0, 4.0].as_slice()));
    assert_eq!(variable.nominal(), Some([1.0, 1.0].as_slice()));
    assert_eq!(variable.unit(), Some("m"));
    assert_eq!(variable.description(), Some("state"));
    assert_eq!(variable.causality(), FmiCausality::Local);
    assert_eq!(variable.variability(), FmiVariability::Continuous);
    assert_eq!(variable.initial(), None);
    assert!(!variable.is_tunable());
    assert_eq!(variable.declaration(), Some(span));
    assert_eq!(variable.value_reference_fmi3(), 7);
}

/// Every per-scalar vector an entry may carry is sized by the same declared
/// shape, and each is owned independently.
///
/// One case per vector, so dropping any single vector from the constructor's
/// agreement check fails exactly one of these rather than being absorbed by a
/// neighbour.
mod scalar_count_agreement {
    use super::super::*;
    use super::max_step_duration_local::delay_bearing_model_with_one_run;
    use crate::SolveDelayPartition;

    /// One one-scalar parameter run and nothing else: the shape check runs
    /// before anything the delay partition decides, so it is cleared here to
    /// keep these cases about the vectors alone.
    fn one_scalar_run() -> (SolveModel, FmiVariableInput) {
        let (mut model, input) = delay_bearing_model_with_one_run();
        model.problem.events.delays = SolveDelayPartition::default();
        (model, input)
    }

    /// Construct with one vector lengthened to two scalars against a
    /// one-scalar declaration, and return how the constructor rejected it.
    fn rejection_for(mismatch: impl FnOnce(&mut FmiVariableInput)) -> FmiComponentError {
        let (model, mut input) = one_scalar_run();
        mismatch(&mut input);

        FmiComponent::construct(model, vec![input])
            .expect_err("a per-scalar vector that disagrees with the shape is not constructible")
    }

    fn assert_two_against_one(rejected: &FmiComponentError) {
        assert!(
            matches!(
                rejected,
                FmiComponentError::ScalarCount {
                    name,
                    actual: 2,
                    expected: 1,
                    ..
                } if name == "p"
            ),
            "{rejected:?}"
        );
    }

    #[test]
    fn a_scalar_name_list_that_disagrees_with_the_shape_is_rejected() {
        assert_two_against_one(&rejection_for(|input| {
            input.scalar_names = vec!["p[1]".to_string(), "p[2]".to_string()];
        }));
    }

    #[test]
    fn a_start_vector_that_disagrees_with_the_shape_is_rejected() {
        assert_two_against_one(&rejection_for(|input| input.start = vec![0.0, 0.0]));
    }

    #[test]
    fn a_minimum_vector_that_disagrees_with_the_shape_is_rejected() {
        assert_two_against_one(&rejection_for(|input| input.minimum = Some(vec![0.0, 0.0])));
    }

    #[test]
    fn a_maximum_vector_that_disagrees_with_the_shape_is_rejected() {
        assert_two_against_one(&rejection_for(|input| input.maximum = Some(vec![1.0, 1.0])));
    }

    #[test]
    fn a_nominal_vector_that_disagrees_with_the_shape_is_rejected() {
        assert_two_against_one(&rejection_for(|input| input.nominal = Some(vec![1.0, 1.0])));
    }

    /// The control: the same fixture with every vector at the declared length
    /// constructs, so the cases above pin the length and nothing else.
    #[test]
    fn one_scalar_per_vector_matches_a_one_scalar_declaration() {
        let (model, mut input) = one_scalar_run();
        input.minimum = Some(vec![0.0]);
        input.maximum = Some(vec![1.0]);
        input.nominal = Some(vec![1.0]);

        let component = FmiComponent::construct(model, vec![input])
            .expect("agreeing vectors are a complete inventory");

        assert_eq!(component.variables().len(), 1);
    }
}

mod max_step_duration_local {
    use super::super::*;
    use crate::{
        LinearOp, ScalarProgramBlock, ScalarSlot, SolveDelayPartition, SolveVariableDeclaration,
        SolveVariableStorageRun, SolveVariableValueKind, VarLayout,
    };
    use indexmap::IndexMap;
    use rumoca_core::{SourceId, Span};

    fn fixture_span() -> Span {
        Span::from_offsets(
            SourceId::from_source_name("fmi_annotation_fixture.mo"),
            0,
            1,
        )
    }

    fn delay_rows(rows: usize) -> ScalarProgramBlock {
        let programs = (0..rows)
            .map(|row| {
                vec![
                    LinearOp::Const {
                        dst: 0,
                        value: 0.5 + row as f64,
                    },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect();
        ScalarProgramBlock::with_source_span(
            programs,
            fixture_span()
                .require_provenance("FMI annotation fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture delay programs are computable")
    }

    /// A checked kernel whose only content is a delay partition of `rows` rows,
    /// each with its own runtime-managed value slot.
    fn delay_bearing_model(rows: usize) -> SolveModel {
        let mut model = SolveModel::default();
        model.problem.layout = VarLayout::from_parts(IndexMap::new(), 0, rows);
        model.problem.events.delays = SolveDelayPartition {
            source_rhs: delay_rows(rows),
            delay_time_rhs: delay_rows(rows),
            delay_max_rhs: delay_rows(rows),
            value_parameter_indices: (0..rows).collect(),
            source_is_discrete: vec![false; rows],
        };
        model
    }

    /// The same kernel plus one ordinary parameter storage run, so the derived
    /// local has run-derived value references to follow.
    pub(super) fn delay_bearing_model_with_one_run() -> (SolveModel, FmiVariableInput) {
        let mut model = delay_bearing_model(1);
        model.problem.layout = VarLayout::from_parts(IndexMap::new(), 0, 2);
        model.problem.solve_layout.variable_storage_runs = vec![SolveVariableStorageRun {
            base: ScalarSlot::P {
                index: 1,
                byte_offset: 8,
            },
            scalar_count: 1,
            role: SolveVariableStorageRole::Parameter,
            value_kind: SolveVariableValueKind::Real,
        }];
        model.problem.solve_layout.variable_declarations = vec![SolveVariableDeclaration::new(
            SolveVariableStorageRole::Parameter,
            SolveVariableValueKind::Real,
        )];
        model.problem.solve_layout.parameter_count = 1;
        (model, parameter_input("p"))
    }

    /// A delay-bearing kernel whose storage is one two-scalar state run, so the
    /// state inventory indices can be read back with the local present.
    pub(super) fn delay_bearing_model_with_one_state() -> (SolveModel, FmiVariableInput) {
        let mut model = delay_bearing_model(1);
        model.problem.layout = VarLayout::from_parts(IndexMap::new(), 2, 1);
        model.problem.solve_layout.variable_storage_runs = vec![SolveVariableStorageRun {
            base: ScalarSlot::Y {
                index: 0,
                byte_offset: 0,
            },
            scalar_count: 2,
            role: SolveVariableStorageRole::State,
            value_kind: SolveVariableValueKind::Real,
        }];
        model.problem.solve_layout.variable_declarations = vec![SolveVariableDeclaration::new(
            SolveVariableStorageRole::State,
            SolveVariableValueKind::Real,
        )];
        model.problem.solve_layout.state_scalar_count = 2;
        (model, state_input("x"))
    }

    fn parameter_input(name: &str) -> FmiVariableInput {
        FmiVariableInput {
            name: name.to_string(),
            scalar_names: vec![name.to_string()],
            role: SolveVariableStorageRole::Parameter,
            value_kind: SolveVariableValueKind::Real,
            dimensions: Vec::new(),
            start: vec![0.0],
            minimum: None,
            maximum: None,
            nominal: None,
            unit: None,
            description: None,
            causality: FmiCausality::Parameter,
            variability: FmiVariability::Fixed,
            tunable: false,
            declaration: fixture_span(),
        }
    }

    fn state_input(name: &str) -> FmiVariableInput {
        FmiVariableInput {
            name: name.to_string(),
            scalar_names: vec![format!("{name}[1]"), format!("{name}[2]")],
            role: SolveVariableStorageRole::State,
            value_kind: SolveVariableValueKind::Real,
            dimensions: vec![2],
            start: vec![0.0, 0.0],
            causality: FmiCausality::Local,
            variability: FmiVariability::Continuous,
            ..parameter_input(name)
        }
    }

    mod event_indicator_inventory {
        use super::*;
        use crate::{RootRelationRefreshRole, RootZeroDomain, ScheduledRootCondition};

        fn load_row(operation: LinearOp) -> ScalarProgramBlock {
            ScalarProgramBlock::with_source_span(
                vec![vec![operation, LinearOp::StoreOutput { src: 0 }]],
                fixture_span()
                    .require_provenance("FMI indicator fixture")
                    .unwrap(),
            )
            .unwrap()
        }

        fn state_root_model() -> (SolveModel, FmiVariableInput) {
            let (mut model, input) = delay_bearing_model_with_one_state();
            model.problem.events.delays = SolveDelayPartition::default();
            model.problem.events.root_conditions = load_row(LinearOp::LoadY { dst: 0, index: 0 });
            model.problem.events.root_relation_memory_targets = vec![None];
            model.problem.events.root_zero_domains = vec![RootZeroDomain::Previous];
            model.problem.events.root_relation_refresh_roles =
                vec![RootRelationRefreshRole::Frozen];
            (model, input)
        }

        #[test]
        fn continuously_state_dependent_roots_are_the_ordered_inventory() {
            let (model, input) = state_root_model();
            let component = FmiComponent::construct(model, vec![input]).unwrap();
            assert_eq!(
                component.event_indicators().sources(),
                [FmiEventIndicatorSource::RootCondition { index: 0 }]
            );
        }

        #[test]
        fn scheduled_roots_are_absent_from_the_indicator_inventory() {
            let (mut model, input) = state_root_model();
            model.problem.events.scheduled_root_conditions = vec![ScheduledRootCondition {
                root_index: 0,
                period_seconds: 1.0,
                phase_seconds: 0.0,
            }];
            let component = FmiComponent::construct(model, vec![input]).unwrap();
            assert!(component.event_indicators().is_empty());
        }

        #[test]
        fn parameter_only_roots_and_deadlines_are_time_events_not_indicators() {
            let (mut model, input) = delay_bearing_model_with_one_run();
            model.problem.events.delays = SolveDelayPartition::default();
            model.problem.events.root_conditions = load_row(LinearOp::LoadP { dst: 0, index: 0 });
            model.problem.events.root_relation_memory_targets = vec![None];
            model.problem.events.root_zero_domains = vec![RootZeroDomain::Previous];
            model.problem.events.root_relation_refresh_roles =
                vec![RootRelationRefreshRole::Frozen];
            model.problem.events.dynamic_time_event_rhs =
                load_row(LinearOp::LoadP { dst: 0, index: 0 });
            let component = FmiComponent::construct(model, vec![input]).unwrap();
            assert!(component.event_indicators().is_empty());
        }

        #[test]
        fn state_deadlines_precede_delay_discontinuity_sources_deterministically() {
            let (mut model, input) = delay_bearing_model_with_one_state();
            model.problem.events.dynamic_time_event_rhs =
                load_row(LinearOp::LoadY { dst: 0, index: 0 });
            let component = FmiComponent::construct(model, vec![input]).unwrap();
            assert_eq!(
                component.event_indicators().sources(),
                [
                    FmiEventIndicatorSource::DynamicTimeEvent { index: 0 },
                    FmiEventIndicatorSource::DelayDiscontinuity { index: 0 },
                ]
            );
        }
    }

    fn is_derived_local(variable: &FmiVariable) -> bool {
        matches!(variable.backing(), FmiValueBacking::MaxStepDuration)
    }

    #[test]
    fn delay_free_kernel_publishes_no_local() {
        let component = FmiComponent::construct(SolveModel::default(), Vec::new())
            .expect("a delay-free kernel needs no step bound");

        assert!(component.max_step_duration().is_none());
        assert!(component.variables().is_empty());
        assert_eq!(component.derivative_value_reference_base_fmi3(), 1);
    }

    #[test]
    fn delay_free_inventory_holds_exactly_one_entry_per_storage_run() {
        let (mut model, run) = delay_bearing_model_with_one_run();
        model.problem.events.delays = SolveDelayPartition::default();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one run is a complete delay-free inventory");

        assert_eq!(component.variables().len(), 1);
        assert_eq!(component.variables()[0].name(), "p");
        assert!(component.max_step_duration().is_none());
        assert_eq!(component.derivative_value_reference_base_fmi3(), 2);
    }

    #[test]
    fn delay_bearing_kernel_publishes_one_calculated_float64_local() {
        let component = FmiComponent::construct(delay_bearing_model(2), Vec::new())
            .expect("a delay-bearing kernel publishes its step bound");

        let declared = component
            .max_step_duration()
            .expect("the delay partition decides the local exists");
        assert_eq!(declared.name(), MAX_STEP_DURATION_NAME);
        assert_eq!(declared.value_kind(), SolveVariableValueKind::Real);
        assert_eq!(declared.causality(), FmiCausality::Local);
        assert_eq!(declared.variability(), FmiVariability::Continuous);
        assert_eq!(declared.initial(), Some(FmiInitial::Calculated));
        assert_eq!(declared.unit(), Some(MAX_STEP_DURATION_UNIT));
        assert_eq!(declared.description(), Some(MAX_STEP_DURATION_DESCRIPTION));
        // FMI 3.0.2 forbids `start` where `initial="calculated"`, and an empty
        // start would be a present zero-length one.
        assert_eq!(declared.start(), None);
    }

    #[test]
    fn the_local_is_one_scalar_named_by_the_entry_itself() {
        let component = FmiComponent::construct(delay_bearing_model(1), Vec::new())
            .expect("a delay-bearing kernel publishes its step bound");

        let declared = component
            .max_step_duration()
            .expect("the delay partition decides the local exists");
        assert_eq!(declared.scalar_names().len(), 1);
        assert_eq!(declared.scalar_names()[0], MAX_STEP_DURATION_NAME);
        // The scalar identity is lent from the entry's own name, not a second
        // owned copy of the constant.
        assert!(std::ptr::eq(
            declared.scalar_names()[0].as_ptr(),
            declared.name().as_ptr()
        ));
    }

    #[test]
    fn the_local_is_backed_by_the_component_and_never_by_storage() {
        let (model, run) = delay_bearing_model_with_one_run();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one run plus one delay partition is a complete inventory");

        let backed = &component.variables()[0];
        assert_eq!(
            backed.backing(),
            &FmiValueBacking::SolveStorage {
                role: SolveVariableStorageRole::Parameter,
                storage: backed.storage().expect("a run backs the source variable"),
                scalar_names: vec!["p".to_string()],
            }
        );
        let derived = component
            .max_step_duration()
            .expect("the delay partition decides the local exists");
        assert_eq!(derived.backing(), &FmiValueBacking::MaxStepDuration);
        assert_eq!(derived.storage(), None);
        assert_eq!(derived.role(), None);
        assert_eq!(derived.declaration(), None);
    }

    #[test]
    fn the_inventory_is_one_entry_per_storage_run_plus_the_local() {
        let (model, run) = delay_bearing_model_with_one_run();
        let runs = model.problem.solve_layout.variable_storage_runs.len();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one run plus one delay partition is a complete inventory");

        assert_eq!(component.variables().len(), runs + 1);
        assert_eq!(component.storage_variables().count(), runs);
        assert!(is_derived_local(
            component
                .variables()
                .last()
                .expect("the inventory is not empty")
        ));
    }

    #[test]
    fn the_storage_view_borrows_the_one_inventory_rather_than_copying_it() {
        let (model, run) = delay_bearing_model_with_one_run();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one run plus one delay partition is a complete inventory");

        let borrowed: Vec<_> = component
            .storage_variables()
            .map(std::ptr::from_ref)
            .collect();
        let owned: Vec<_> = component
            .variables()
            .iter()
            .filter(|variable| !is_derived_local(variable))
            .map(std::ptr::from_ref)
            .collect();
        assert_eq!(borrowed, owned);
    }

    #[test]
    fn value_references_number_the_one_inventory_in_order() {
        let (model, run) = delay_bearing_model_with_one_run();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one run plus one delay partition is a complete inventory");

        let references: Vec<u32> = component
            .variables()
            .iter()
            .map(FmiVariable::value_reference_fmi3)
            .collect();
        assert_eq!(references, vec![1, 2]);
        assert_eq!(component.derivative_value_reference_base_fmi3(), 3);
    }

    #[test]
    fn value_reference_assignment_is_deterministic_across_constructions() {
        let inventory = || {
            let (model, run) = delay_bearing_model_with_one_run();
            let component = FmiComponent::construct(model, vec![run])
                .expect("one run plus one delay partition is a complete inventory");
            let entries: Vec<(String, u32)> = component
                .variables()
                .iter()
                .map(|variable| (variable.name().to_string(), variable.value_reference_fmi3()))
                .collect();
            (entries, component.derivative_value_reference_base_fmi3())
        };

        assert_eq!(inventory(), inventory());
    }

    #[test]
    fn both_fmi_versions_project_the_same_ordered_inventory() {
        let (model, run) = delay_bearing_model_with_one_state();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one state run plus one delay partition is a complete inventory");

        // FMI 3 addresses every inventory entry, tensor-valued, by its own
        // value reference.
        let fmi3: Vec<(&str, u32)> = component
            .variables()
            .iter()
            .map(|variable| (variable.name(), variable.value_reference_fmi3()))
            .collect();
        assert_eq!(fmi3, vec![("x", 1), (MAX_STEP_DURATION_NAME, 2)]);

        // FMI 2 walks the same entries per scalar, with no name special case:
        // the derived local yields exactly its own one scalar identity.
        let fmi2: Vec<&str> = component
            .variables()
            .iter()
            .flat_map(|variable| variable.scalar_names().iter().map(String::as_str))
            .collect();
        assert_eq!(fmi2, vec!["x[1]", "x[2]", MAX_STEP_DURATION_NAME]);
    }

    #[test]
    fn state_inventory_indices_stay_correct_with_the_local_present() {
        let (model, run) = delay_bearing_model_with_one_state();

        let component = FmiComponent::construct(model, vec![run])
            .expect("one state run plus one delay partition is a complete inventory");

        assert_eq!(component.state_variable_indices(), [0]);
        let state = &component.variables()[component.state_variable_indices()[0]];
        assert_eq!(state.role(), Some(SolveVariableStorageRole::State));
        assert_eq!(component.derivative_value_reference_base_fmi3(), 3);
    }

    #[test]
    fn the_unconstrained_sentinel_is_the_maximum_finite_float64() {
        assert_eq!(MAX_STEP_DURATION_UNCONSTRAINED, f64::MAX);
        assert!(MAX_STEP_DURATION_UNCONSTRAINED.is_finite());
    }

    #[test]
    fn a_source_variable_may_not_take_the_reserved_name_of_the_published_local() {
        let (model, mut run) = delay_bearing_model_with_one_run();
        run.name = MAX_STEP_DURATION_NAME.to_string();
        run.scalar_names = vec![MAX_STEP_DURATION_NAME.to_string()];

        let rejected = FmiComponent::construct(model, vec![run]);

        assert!(matches!(
            rejected,
            Err(FmiComponentError::ReservedMaxStepDurationName {
                name: MAX_STEP_DURATION_NAME,
                declaration,
            }) if declaration == fixture_span()
        ));
    }

    #[test]
    fn a_delay_free_kernel_leaves_the_namespaced_name_to_its_source() {
        let (mut model, mut run) = delay_bearing_model_with_one_run();
        model.problem.events.delays = SolveDelayPartition::default();
        run.name = MAX_STEP_DURATION_NAME.to_string();
        run.scalar_names = vec![MAX_STEP_DURATION_NAME.to_string()];

        let component = FmiComponent::construct(model, vec![run])
            .expect("a delay-free kernel publishes no local to collide with");

        assert_eq!(component.variables().len(), 1);
        assert!(component.max_step_duration().is_none());
        assert_eq!(
            component.variables()[0].role(),
            Some(SolveVariableStorageRole::Parameter)
        );
    }

    /// The type-state a storage-backed rendering consumes: it exists exactly
    /// where the kernel owns no semantic event class, and it moves the one
    /// checked inventory rather than describing it again.
    mod event_free_type_state {
        use super::*;
        use crate::{
            PeriodicEventSchedule, RootRelationRefreshRole, RootZeroDomain, SolveEventClass,
        };

        /// One ordinary parameter run, no delay partition, and nothing else:
        /// the checked kernel every counterexample below starts from.
        fn event_free_component() -> FmiComponent {
            construct(event_free_kernel())
        }

        fn event_free_kernel() -> SolveModel {
            let (mut model, _) = delay_bearing_model_with_one_run();
            model.problem.events.delays = SolveDelayPartition::default();
            model
        }

        fn construct(model: SolveModel) -> FmiComponent {
            FmiComponent::construct(model, vec![parameter_input("p")])
                .expect("one run is a complete inventory")
        }

        /// Narrow the component built from `mutate`d event-free kernel, and
        /// return how the narrowing refused it.
        ///
        /// Each caller sets exactly one partition, so removing that partition's
        /// term from the single-source presence query in `crate::feature_query`
        /// lets its component narrow and fails exactly that one case.
        fn rejection_for(mutate: impl FnOnce(&mut SolveModel)) -> FmiEventFreeError {
            let mut model = event_free_kernel();
            mutate(&mut model);

            construct(model)
                .into_codegen_view()
                .try_event_free()
                .map(|_| ())
                .expect_err("an event-bearing kernel has no storage-backed rendering")
        }

        /// One computable single-output row, for the partitions that need a
        /// program rather than a scalar.
        fn one_condition_row() -> ScalarProgramBlock {
            super::delay_rows(1)
        }

        #[test]
        fn an_event_free_view_narrows_by_moving_the_one_inventory_and_kernel() {
            let component = event_free_component();
            let inventory_address = component.variables().as_ptr();
            let base = component.derivative_value_reference_base_fmi3();
            let view = component.into_codegen_view();
            let problem_address = std::ptr::from_ref(view.problem());

            let event_free = view
                .try_event_free()
                .expect("an event-free kernel with a storage-backed inventory narrows");

            assert_eq!(event_free.variables().as_ptr(), inventory_address);
            assert_eq!(std::ptr::from_ref(event_free.problem()), problem_address);
            assert_eq!(event_free.derivative_value_reference_base_fmi3(), base);
        }

        /// An ordinary event owner: the hidden condition memories a lowered
        /// DAE condition retains.
        #[test]
        fn a_condition_memory_owner_has_no_event_free_type_state() {
            assert_eq!(
                rejection_for(|model| {
                    model.problem.events.condition_memory_parameter_indices = vec![0];
                }),
                FmiEventFreeError::EventBearingKernel {
                    class: SolveEventClass::Discrete,
                }
            );
        }

        /// A root (state) event: the indicator inventory a host would have to
        /// scan, which these templates emit nothing for.
        #[test]
        fn a_root_event_has_no_event_free_type_state() {
            assert_eq!(
                rejection_for(|model| {
                    let events = &mut model.problem.events;
                    events.root_conditions = one_condition_row();
                    events.root_relation_memory_targets = vec![None];
                    events.root_zero_domains = vec![RootZeroDomain::Previous];
                    events.root_relation_refresh_roles = vec![RootRelationRefreshRole::Frozen];
                }),
                FmiEventFreeError::EventBearingKernel {
                    class: SolveEventClass::Discrete,
                }
            );
        }

        /// A scheduled (time) event, which needs no root condition to exist
        /// and so is a counterexample independent of the one above.
        #[test]
        fn a_scheduled_event_has_no_event_free_type_state() {
            assert_eq!(
                rejection_for(|model| {
                    model.problem.events.scheduled_time_events = vec![1.0];
                }),
                FmiEventFreeError::EventBearingKernel {
                    class: SolveEventClass::Discrete,
                }
            );
        }

        /// A runtime delay: history the generated C keeps no buffer for.
        ///
        /// The refusal names the kernel's event class, not the
        /// maximum-step-duration entry that class causes the component to
        /// publish: the unrenderable thing is the delay behaviour itself.
        #[test]
        fn a_delay_bearing_kernel_has_no_event_free_type_state() {
            let (model, run) = delay_bearing_model_with_one_run();
            let component = FmiComponent::construct(model, vec![run])
                .expect("one run plus one delay partition is a complete inventory");
            assert!(component.max_step_duration().is_some());

            let rejected = component
                .into_codegen_view()
                .try_event_free()
                .map(|_| ())
                .expect_err("a runtime-event kernel has no storage-backed rendering");

            assert_eq!(
                rejected,
                FmiEventFreeError::EventBearingKernel {
                    class: SolveEventClass::Runtime,
                }
            );
        }

        /// The other runtime event, which carries no inventory entry at all
        /// and so cannot be caught by an entry-shape check.
        #[test]
        fn a_terminal_event_has_no_event_free_type_state() {
            assert_eq!(
                rejection_for(|model| {
                    model.problem.events.has_terminal_event = true;
                    model.problem.solve_layout.terminal_event_parameter_index = Some(0);
                }),
                FmiEventFreeError::EventBearingKernel {
                    class: SolveEventClass::Runtime,
                }
            );
        }

        /// A clock partition, whose ticks are announced time events these
        /// templates do not schedule.
        #[test]
        fn a_clock_partition_has_no_event_free_type_state() {
            assert_eq!(
                rejection_for(|model| {
                    model.problem.clocks.periodic_event_schedules = vec![
                        PeriodicEventSchedule::from_seconds(0.1, 0.0)
                            .expect("a positive rational period is a checked schedule"),
                    ];
                    model.problem.clocks.activation_parameter_indices = vec![0];
                }),
                FmiEventFreeError::EventBearingKernel {
                    class: SolveEventClass::Clock,
                }
            );
        }

        /// The narrowed view is the one whole-inventory encoding, and it
        /// carries exactly the entry keys and derivative base a version
        /// template reads.
        #[test]
        fn the_type_state_encodes_the_inventory_a_version_template_reads() {
            let component = event_free_component();
            let base = component.derivative_value_reference_base_fmi3();

            let rendered = serde_json::to_value(
                component
                    .into_codegen_view()
                    .try_event_free()
                    .expect("an event-free kernel with a storage-backed inventory narrows"),
            )
            .expect("the narrowed view is encodable");

            let entries = rendered["variables"]
                .as_array()
                .expect("the narrowed view encodes the inventory as an array");
            assert_eq!(entries.len(), 1);
            assert_eq!(entries[0]["name"], "p");
            assert_eq!(entries[0]["scalar_names"], serde_json::json!(["p"]));
            assert_eq!(entries[0]["backing"], "solve_storage");
            assert_eq!(entries[0]["role"], "Parameter");
            assert_eq!(entries[0]["storage"]["column"], "p");
            assert_eq!(entries[0]["storage"]["base"], 1);
            assert_eq!(entries[0]["storage"]["scalar_count"], 1);
            assert_eq!(entries[0]["start"], serde_json::json!([0.0]));
            assert_eq!(entries[0]["causality"], "parameter");
            assert_eq!(entries[0]["variability"], "fixed");
            assert_eq!(entries[0]["value_reference_fmi3"], 1);
            assert_eq!(rendered["state_variable_indices"], serde_json::json!([]));
            assert_eq!(rendered["derivative_value_reference_base_fmi3"], base);
        }
    }

    /// The construction-owned capability fact SPEC_0044 §8 requires of the
    /// component, derived from the same one event-domain query.
    ///
    /// Only the declaration is owned here. The callback's runtime behaviour,
    /// the namespaced model-description annotation, and the standard Float64
    /// getter belong to the linked component and join the common-host cutover.
    mod completed_integrator_step {
        use super::*;

        #[test]
        fn an_event_free_component_needs_no_completed_integrator_step() {
            let (mut model, run) = delay_bearing_model_with_one_run();
            model.problem.events.delays = SolveDelayPartition::default();

            let component =
                FmiComponent::construct(model, vec![run]).expect("one run is a complete inventory");

            assert!(!component.needs_completed_integrator_step());
        }

        #[test]
        fn a_delay_bearing_component_needs_the_completed_integrator_step() {
            let (model, run) = delay_bearing_model_with_one_run();

            let component = FmiComponent::construct(model, vec![run])
                .expect("one run plus one delay partition is a complete inventory");

            assert!(component.needs_completed_integrator_step());
        }

        /// An event-bearing kernel that publishes no maximum-step-duration
        /// local still needs the callback, because the declaration follows the
        /// event domain rather than the inventory.
        #[test]
        fn an_event_bearing_component_needs_the_completed_integrator_step() {
            let (mut model, run) = delay_bearing_model_with_one_run();
            model.problem.events.delays = SolveDelayPartition::default();
            model.problem.events.scheduled_time_events = vec![1.0];

            let component =
                FmiComponent::construct(model, vec![run]).expect("one run is a complete inventory");

            assert!(component.max_step_duration().is_none());
            assert!(component.needs_completed_integrator_step());
        }
    }
}
