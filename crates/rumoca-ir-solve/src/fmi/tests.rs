use super::*;
use crate::{
    AlgebraicProjectionPlan, ComputeBlock, ContinuousRefreshPlanInputs, ContinuousSolveArtifacts,
    ContinuousSolveSystem, ContinuousSolveSystemInputs, DiscreteSolveSystem,
    InitializationSolveSystem, LinearOp, MassMatrix, ScalarProgramBlock, SolveArithmeticProfile,
    SolveArtifactInputs, SolveClockPartition, SolveEventPartition, SolveIntegerDomain, SolveLayout,
    SolveModelRuntimeInputs, SolveProblem, SolveRealFormat, SolveVariableCatalogError,
    SolveVariableCatalogSourceEntry, SolveVariableCausality, SolveVariableEvaluatedValues,
    SolveVariableSource, SolveVariableSourceAttributes, SolveVariableValueKind,
    SolveVariableVariability, VarLayout,
};
use rumoca_core::{InstanceId, RealMatrixMultiplySemantics, SourceOccurrenceId};

fn empty_binary64_first_product_model() -> SolveModel {
    build_model(ModelFixtureParts::empty(SolveRealFormat::Binary64))
}

fn catalog_fixture_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_catalog_fixture.mo"),
        0,
        1,
    )
}

struct CatalogFixtureEntry {
    source_occurrence: SourceOccurrenceId,
    name: String,
    dimensions: Vec<u32>,
    scalar_names: Vec<String>,
    provenance: Span,
    causality: SolveVariableCausality,
    variability: SolveVariableVariability,
    tunable: bool,
    unit: Option<String>,
    description: Option<String>,
    fixed: rumoca_core::Fixity,
    start: Vec<f64>,
    minimum: Option<Vec<f64>>,
    maximum: Option<Vec<f64>>,
    nominal: Option<Vec<f64>>,
}

impl CatalogFixtureEntry {
    fn parameter(source_occurrence: u32, name: &str, start: f64) -> Self {
        Self {
            source_occurrence: SourceOccurrenceId::try_from(InstanceId::new(source_occurrence))
                .expect("fixture source occurrence is explicitly nonzero"),
            name: name.to_string(),
            dimensions: Vec::new(),
            scalar_names: vec![name.to_string()],
            provenance: catalog_fixture_span(),
            causality: SolveVariableCausality::Parameter,
            variability: SolveVariableVariability::Fixed,
            tunable: false,
            unit: None,
            description: None,
            fixed: rumoca_core::Fixity::Fixed,
            start: vec![start],
            minimum: None,
            maximum: None,
            nominal: None,
        }
    }

    fn state(source_occurrence: u32, name: &str, scalar_count: usize) -> Self {
        Self {
            name: name.to_string(),
            dimensions: vec![u32::try_from(scalar_count).expect("test extent fits u32")],
            scalar_names: (1..=scalar_count)
                .map(|scalar| format!("{name}[{scalar}]"))
                .collect(),
            causality: SolveVariableCausality::Local,
            variability: SolveVariableVariability::Continuous,
            fixed: rumoca_core::Fixity::Free,
            start: vec![0.0; scalar_count],
            nominal: Some(vec![1.0; scalar_count]),
            ..Self::parameter(source_occurrence, name, 0.0)
        }
    }

    fn issue(
        self,
        construction: &mut crate::SolveVariableCatalogConstruction<'_>,
    ) -> Result<crate::SolveVariableId, SolveVariableCatalogError> {
        let (identity, attributes, values) = self.into_source();
        construction.issue(identity, attributes, values)
    }

    fn into_source(self) -> SolveVariableCatalogSourceEntry {
        (
            SolveVariableSource::new(
                self.source_occurrence,
                self.name,
                self.dimensions,
                self.scalar_names,
                self.provenance,
            ),
            SolveVariableSourceAttributes::new(
                self.causality,
                self.variability,
                self.tunable,
                self.unit,
                self.description,
                self.fixed,
            ),
            SolveVariableEvaluatedValues::new(
                Some(self.start),
                self.minimum,
                self.maximum,
                self.nominal,
            ),
        )
    }
}

struct ModelFixtureParts {
    arithmetic: SolveArithmeticProfile,
    layout: VarLayout,
    solve_layout: SolveLayout,
    events: SolveEventPartition,
    clocks: SolveClockPartition,
    initial_y: Vec<f64>,
    solver_nominals: Vec<f64>,
    parameters: Vec<f64>,
    entries: Vec<CatalogFixtureEntry>,
}

impl ModelFixtureParts {
    fn empty(real_format: SolveRealFormat) -> Self {
        Self {
            arithmetic: SolveArithmeticProfile::construct(
                real_format,
                SolveIntegerDomain::FULL,
                RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
            ),
            layout: VarLayout::default(),
            solve_layout: SolveLayout::default(),
            events: SolveEventPartition::default(),
            clocks: SolveClockPartition::default(),
            initial_y: Vec::new(),
            solver_nominals: Vec::new(),
            parameters: Vec::new(),
            entries: Vec::new(),
        }
    }

    fn without_delays(mut self) -> Self {
        self.events.delays = crate::SolveDelayPartition::default();
        self
    }
}

fn fixture_rows(rows: Vec<Vec<LinearOp>>, purpose: &'static str) -> ScalarProgramBlock {
    if rows.is_empty() {
        ScalarProgramBlock::default()
    } else {
        ScalarProgramBlock::with_source_span(
            rows,
            catalog_fixture_span()
                .require_provenance(purpose)
                .expect("fixture span is source-backed"),
        )
        .expect("fixture rows are computable")
    }
}

fn zero_rows(rows: usize) -> ScalarProgramBlock {
    fixture_rows(
        (0..rows)
            .map(|_| {
                vec![
                    LinearOp::Const { dst: 0, value: 0.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]
            })
            .collect(),
        "FMI checked fixture",
    )
}

fn visible_value_rows(solve_layout: &SolveLayout) -> ScalarProgramBlock {
    let rows = solve_layout
        .variable_storage_runs
        .iter()
        .filter(|run| {
            matches!(
                run.role,
                crate::SolveVariableStorageRole::ExternalInput
                    | crate::SolveVariableStorageRole::State
                    | crate::SolveVariableStorageRole::Algebraic
                    | crate::SolveVariableStorageRole::Output
                    | crate::SolveVariableStorageRole::DiscreteReal
                    | crate::SolveVariableStorageRole::DiscreteValue
            )
        })
        .flat_map(|run| {
            (0..run.scalar_count).map(move |offset| {
                let load = match run.base {
                    crate::SolveStorageCoordinate::Y(index) => LinearOp::LoadY {
                        dst: 0,
                        index: index + offset,
                    },
                    crate::SolveStorageCoordinate::P(index) => LinearOp::LoadP {
                        dst: 0,
                        index: index + offset,
                    },
                };
                vec![load, LinearOp::StoreOutput { src: 0 }]
            })
        })
        .collect::<Vec<_>>();
    fixture_rows(rows, "FMI visible-value fixture")
}

fn checked_continuous_system(state_count: usize) -> ContinuousSolveSystem {
    checked_continuous_system_from_rows(state_count, zero_rows(state_count))
}

fn checked_continuous_system_from_rows(
    state_count: usize,
    derivative_rows: ScalarProgramBlock,
) -> ContinuousSolveSystem {
    let implicit_rhs = ComputeBlock::default();
    let solve_layout = SolveLayout {
        state_scalar_count: state_count,
        ..SolveLayout::default()
    };
    ContinuousSolveSystem::construct(
        &solve_layout,
        ContinuousSolveSystemInputs::new(
            implicit_rhs,
            Vec::new(),
            AlgebraicProjectionPlan::default(),
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            ComputeBlock::from_scalar_program_block(derivative_rows),
            ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("fixture has a checked continuous system")
}

fn construct_problem(
    layout: VarLayout,
    solve_layout: SolveLayout,
    events: SolveEventPartition,
    clocks: SolveClockPartition,
) -> SolveProblem {
    construct_problem_from_derivative_rows(layout, solve_layout, events, clocks, None)
}

fn construct_problem_from_derivative_rows(
    layout: VarLayout,
    solve_layout: SolveLayout,
    events: SolveEventPartition,
    clocks: SolveClockPartition,
    derivative_rows: Option<ScalarProgramBlock>,
) -> SolveProblem {
    let state_count = solve_layout.state_scalar_count();
    let continuous = match derivative_rows {
        Some(rows) => checked_continuous_system_from_rows(state_count, rows),
        None => checked_continuous_system(state_count),
    };
    SolveProblem::construct(
        layout,
        solve_layout,
        continuous,
        InitializationSolveSystem::empty(),
        DiscreteSolveSystem::default(),
        events,
        clocks,
    )
    .expect("FMI fixture problem is valid by construction")
}

fn build_model(parts: ModelFixtureParts) -> SolveModel {
    build_model_from_derivative_rows(parts, None)
        .expect("FMI fixture model is valid by construction")
}

fn build_model_from_derivative_rows(
    parts: ModelFixtureParts,
    derivative_rows: Option<ScalarProgramBlock>,
) -> Result<SolveModel, crate::SolveModelConstructionError> {
    let ModelFixtureParts {
        arithmetic,
        layout,
        solve_layout,
        events,
        clocks,
        initial_y,
        solver_nominals,
        parameters,
        entries,
    } = parts;
    let problem = construct_problem_from_derivative_rows(
        layout,
        solve_layout,
        events,
        clocks,
        derivative_rows,
    );
    let state_count = problem.solve_layout().state_scalar_count();
    let full_columns = problem.layout().y_scalars() + problem.layout().p_scalars();
    let visible_value_rows = visible_value_rows(problem.solve_layout());
    let full_jacobian_v = zero_rows(state_count);
    let derivative_pattern = (state_count > 0).then(|| {
        crate::StructuralPattern::full(
            state_count,
            full_columns,
            crate::PatternProvenance::derived(
                crate::PatternDerivation::DependencyPropagation,
                catalog_fixture_span(),
            )
            .expect("fixture span is source-backed"),
        )
        .expect("fixture derivative pattern shape is valid")
    });
    SolveModel::construct(
        problem,
        crate::SolvePureCallTable::empty(arithmetic),
        SolveArtifactInputs {
            continuous: ContinuousSolveArtifacts {
                structural: crate::ContinuousStructuralArtifacts::derived(
                    None,
                    Vec::new(),
                    Vec::new(),
                    None,
                    Vec::new(),
                    derivative_pattern,
                ),
                mass_matrix: MassMatrix::Identity,
                full_jacobian_v,
                ..ContinuousSolveArtifacts::default()
            },
            ..SolveArtifactInputs::empty()
        },
        SolveModelRuntimeInputs {
            initial_y,
            solver_nominals,
            parameters,
        },
        visible_value_rows,
        entries.into_iter().map(CatalogFixtureEntry::into_source),
    )
}

fn with_catalog_construction<T>(
    parts: ModelFixtureParts,
    use_construction: impl FnOnce(&mut crate::SolveVariableCatalogConstruction<'_>) -> T,
) -> T {
    let problem = construct_problem(parts.layout, parts.solve_layout, parts.events, parts.clocks);
    let mut construction = crate::SolveVariableCatalog::begin(
        &problem,
        &parts.initial_y,
        &parts.solver_nominals,
        &parts.parameters,
    );
    use_construction(&mut construction)
}

fn catalog_issue_error(entry: CatalogFixtureEntry) -> SolveVariableCatalogError {
    let mut parts = ModelFixtureParts::empty(SolveRealFormat::Binary64);
    parts.layout = VarLayout::from_parts(indexmap::IndexMap::new(), 0, 1);
    parts.solve_layout.variable_storage_runs = vec![crate::SolveVariableStorageRun {
        base: crate::SolveStorageCoordinate::P(0),
        scalar_count: 1,
        role: crate::SolveVariableStorageRole::Parameter,
        value_kind: SolveVariableValueKind::Real,
    }];
    parts.solve_layout.variable_declarations = vec![crate::SolveVariableDeclaration::new(
        crate::SolveVariableStorageRole::Parameter,
        SolveVariableValueKind::Real,
    )];
    parts.solve_layout.parameter_count = 1;
    parts.solve_layout.static_parameter_names = vec!["p".to_string()];
    parts.solve_layout.compiled_parameter_len = 1;
    parts.parameters = vec![0.0];
    with_catalog_construction(parts, |construction| {
        entry
            .issue(construction)
            .expect_err("the malformed catalog fixture must fail at issue time")
    })
}

fn two_parameter_catalog_parts() -> ModelFixtureParts {
    let mut parts = ModelFixtureParts::empty(SolveRealFormat::Binary64);
    parts.layout = VarLayout::from_parts(indexmap::IndexMap::new(), 0, 2);
    parts.solve_layout.variable_storage_runs = (0..2)
        .map(|index| crate::SolveVariableStorageRun {
            base: crate::SolveStorageCoordinate::P(index),
            scalar_count: 1,
            role: crate::SolveVariableStorageRole::Parameter,
            value_kind: SolveVariableValueKind::Real,
        })
        .collect();
    parts.solve_layout.variable_declarations = (0..2)
        .map(|_| {
            crate::SolveVariableDeclaration::new(
                crate::SolveVariableStorageRole::Parameter,
                SolveVariableValueKind::Real,
            )
        })
        .collect();
    parts.solve_layout.parameter_count = 2;
    parts.solve_layout.static_parameter_names = vec!["p".to_string(), "q".to_string()];
    parts.solve_layout.compiled_parameter_len = 2;
    parts.parameters = vec![0.0, 0.0];
    parts
}

fn duplicate_scalar_identity_error() -> SolveVariableCatalogError {
    with_catalog_construction(two_parameter_catalog_parts(), |construction| {
        CatalogFixtureEntry::parameter(1, "p", 0.0)
            .issue(construction)
            .expect("the first scalar identity is unique");
        let mut duplicate = CatalogFixtureEntry::parameter(2, "q", 0.0);
        duplicate.scalar_names[0] = "p".to_string();
        duplicate
            .issue(construction)
            .expect_err("the second declaration cannot reuse the first scalar identity")
    })
}

#[test]
fn catalog_rejects_duplicate_source_occurrences_before_installation() {
    with_catalog_construction(two_parameter_catalog_parts(), |construction| {
        let first = CatalogFixtureEntry::parameter(41, "p", 0.0);
        let first_occurrence = first.source_occurrence;
        first
            .issue(construction)
            .expect("the first occurrence is unique");
        let error = CatalogFixtureEntry::parameter(41, "q", 0.0)
            .issue(construction)
            .expect_err("distinct names cannot authorize a repeated source occurrence");
        assert!(matches!(
            error,
            SolveVariableCatalogError::DuplicateSourceOccurrence { occurrence, span }
                if occurrence == first_occurrence && span == catalog_fixture_span()
        ));
        CatalogFixtureEntry::parameter(17, "q", 0.0)
            .issue(construction)
            .expect("a refused duplicate leaves the second declaration available");
    });
}

#[test]
fn model_construction_refuses_duplicate_source_occurrences() {
    let mut parts = two_parameter_catalog_parts();
    let first = CatalogFixtureEntry::parameter(41, "p", 0.0);
    let occurrence = first.source_occurrence;
    parts.entries = vec![first, CatalogFixtureEntry::parameter(41, "q", 0.0)];
    let error = build_model_from_derivative_rows(parts, None)
        .expect_err("the public root constructor must not publish duplicate source occurrences");
    assert!(matches!(
        error,
        crate::SolveModelConstructionError::VariableCatalog(
            SolveVariableCatalogError::DuplicateSourceOccurrence { occurrence: actual, span }
        ) if actual == occurrence && span == catalog_fixture_span()
    ));
}

#[test]
fn catalog_refinement_view_retains_distinct_occurrences_in_order() {
    let mut parts = two_parameter_catalog_parts();
    let mut first = CatalogFixtureEntry::parameter(41, "p", 0.0);
    let mut second = CatalogFixtureEntry::parameter(17, "q", 0.0);
    first.dimensions = vec![1];
    second.dimensions = vec![1, 1];
    let expected = [first.source_occurrence, second.source_occurrence];
    parts.entries = vec![first, second];
    let model = build_model(parts);
    let view = model.variable_refinement();
    let catalog = model.variable_catalog().entries();
    assert_eq!(catalog.len(), expected.len());
    assert_eq!(view.entries().len(), expected.len());
    for (ordinal, occurrence) in expected.into_iter().enumerate() {
        let source = &catalog[ordinal];
        let fact = &view.entries()[ordinal];
        assert_eq!(fact.source_occurrence(), occurrence);
        assert_eq!(fact.fixed(), source.fixed());
        assert_eq!(fact.state_initialization(), source.state_initialization());
        assert_eq!(fact.variability(), source.variability());
        assert_eq!(fact.tunable(), source.is_tunable());
        assert_eq!(fact.causality(), source.causality());
        assert_eq!(fact.dimensions(), source.dimensions());
        assert_eq!(fact.role(), source.role());
        assert_eq!(fact.value_kind(), source.value_kind());
        assert_eq!(fact.storage(), source.storage());
    }
    assert!(view.entries().get(expected.len()).is_none());
}

#[test]
fn component_consumes_one_complete_model_into_codegen_view() {
    let component = FmiComponent::construct(empty_binary64_first_product_model())
        .expect("empty checked Solve model has an empty FMI inventory");
    let problem_address = std::ptr::from_ref(component.problem());

    let view = component.into_codegen_view();
    assert_eq!(std::ptr::from_ref(view.problem()), problem_address);
}

#[test]
fn variable_metadata_is_available_only_through_borrowed_views() {
    let span = Span::DUMMY;
    let variable = FmiVariable {
        source_id: None,
        name: "x".to_string(),
        value_kind: SolveVariableValueKind::Real,
        dimensions: vec![2],
        backing: FmiValueBacking::SolveStorage {
            role: SolveVariableStorageRole::State,
            storage: FmiStorageRun {
                column: SolveStorageColumn::Y,
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
        write_policy: FmiWritePolicy::ReadOnly,
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
    assert_eq!(storage.column(), SolveStorageColumn::Y);
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

/// Every per-scalar vector is proved at the sealed catalog boundary. FMI never
/// receives a parallel, forgeable metadata input to validate a second time.
mod scalar_count_agreement {
    use super::{
        CatalogFixtureEntry, SolveVariableCatalogError, Span, catalog_issue_error,
        duplicate_scalar_identity_error,
    };

    #[test]
    fn a_scalar_name_list_that_disagrees_with_the_shape_is_rejected() {
        let mut entry = CatalogFixtureEntry::parameter(1, "p", 0.0);
        entry.scalar_names.push("p[2]".to_string());
        assert!(matches!(
            catalog_issue_error(entry),
            SolveVariableCatalogError::ScalarCount {
                name,
                actual: 2,
                expected: 1,
                ..
            } if name == "p"
        ));
    }

    #[test]
    fn each_numeric_attribute_must_match_the_declared_shape() {
        for attribute in ["start", "minimum", "maximum", "nominal"] {
            let mut entry = CatalogFixtureEntry::parameter(1, "p", 0.0);
            match attribute {
                "start" => entry.start.push(1.0),
                "minimum" => entry.minimum = Some(vec![0.0, 0.0]),
                "maximum" => entry.maximum = Some(vec![1.0, 1.0]),
                "nominal" => entry.nominal = Some(vec![1.0, 1.0]),
                _ => unreachable!(),
            }
            assert!(matches!(
                catalog_issue_error(entry),
                SolveVariableCatalogError::AttributeCount {
                    attribute: actual,
                    ..
                } if actual == attribute
            ));
        }
    }

    #[test]
    fn a_catalog_entry_without_provenance_cannot_be_issued() {
        let mut entry = CatalogFixtureEntry::parameter(1, "p", 0.0);
        entry.provenance = Span::DUMMY;
        assert_eq!(
            catalog_issue_error(entry),
            SolveVariableCatalogError::MissingProvenance
        );
    }

    #[test]
    fn scalar_identities_are_unique_across_declarations() {
        assert!(matches!(
            duplicate_scalar_identity_error(),
            SolveVariableCatalogError::DuplicateScalarName { name, .. } if name == "p"
        ));
    }

    #[test]
    fn non_finite_values_and_inconsistent_tunable_attributes_cannot_be_issued() {
        let non_finite = CatalogFixtureEntry::parameter(1, "p", f64::NAN);
        assert!(matches!(
            catalog_issue_error(non_finite),
            SolveVariableCatalogError::InvalidNumericValue { .. }
        ));
        let mut tunable = CatalogFixtureEntry::parameter(1, "p", 0.0);
        tunable.tunable = true;
        assert!(matches!(
            catalog_issue_error(tunable),
            SolveVariableCatalogError::TunableVariability { .. }
        ));
    }
}

mod max_step_duration_local {
    use super::*;
    use crate::{
        LinearOp, ScalarProgramBlock, SolveDelayPartition, SolveVariableDeclaration,
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

    /// Pre-construction parts whose only content is a delay partition of
    /// `rows` rows, each with its own runtime-managed value slot.
    fn delay_bearing_parts(rows: usize) -> ModelFixtureParts {
        let mut parts = ModelFixtureParts::empty(SolveRealFormat::Binary64);
        parts.layout = VarLayout::from_parts(IndexMap::new(), 0, rows);
        parts.events.delays = SolveDelayPartition {
            source_rhs: delay_rows(rows),
            delay_time_rhs: delay_rows(rows),
            delay_max_rhs: delay_rows(rows),
            value_parameter_indices: (0..rows).collect(),
            source_is_discrete: vec![false; rows],
        };
        parts.parameters = vec![0.0; rows];
        parts
    }

    fn delay_bearing_model(rows: usize) -> SolveModel {
        build_model(delay_bearing_parts(rows))
    }

    /// The same kernel plus one ordinary parameter storage run, so the derived
    /// local has run-derived value references to follow.
    pub(super) fn delay_bearing_model_with_one_run() -> SolveModel {
        build_model(delay_bearing_parts_with_named_run("p"))
    }

    fn delay_bearing_model_with_named_run(name: &str) -> SolveModel {
        build_model(delay_bearing_parts_with_named_run(name))
    }

    fn delay_bearing_parts_with_named_run(name: &str) -> ModelFixtureParts {
        let mut parts = delay_bearing_parts(1);
        parts.layout = VarLayout::from_parts(IndexMap::new(), 0, 2);
        parts.events.delays.value_parameter_indices = vec![1];
        parts.solve_layout.variable_storage_runs = vec![SolveVariableStorageRun {
            base: crate::SolveStorageCoordinate::P(0),
            scalar_count: 1,
            role: SolveVariableStorageRole::Parameter,
            value_kind: SolveVariableValueKind::Real,
        }];
        parts.solve_layout.variable_declarations = vec![SolveVariableDeclaration::new(
            SolveVariableStorageRole::Parameter,
            SolveVariableValueKind::Real,
        )];
        parts.solve_layout.parameter_count = 1;
        parts.solve_layout.static_parameter_names = vec![name.to_string()];
        parts.solve_layout.compiled_parameter_len = 1;
        parts.parameters = vec![0.0, 0.0];
        let mut entry = CatalogFixtureEntry::parameter(1, name, 0.0);
        entry.provenance = fixture_span();
        parts.entries = vec![entry];
        parts
    }

    /// A delay-bearing kernel whose storage is one two-scalar state run, so the
    /// state inventory indices can be read back with the local present.
    pub(super) fn delay_bearing_model_with_one_state() -> SolveModel {
        build_model(delay_bearing_parts_with_one_state())
    }

    fn delay_bearing_parts_with_one_state() -> ModelFixtureParts {
        let mut parts = delay_bearing_parts(1);
        parts.layout = VarLayout::from_parts(IndexMap::new(), 2, 1);
        parts.solve_layout.solver_maps = crate::SolverNameIndexMaps {
            names: vec!["x[1]".to_string(), "x[2]".to_string()],
            name_to_idx: IndexMap::from([("x[1]".to_string(), 0), ("x[2]".to_string(), 1)]),
            base_to_indices: IndexMap::from([("x".to_string(), vec![0, 1])]),
        };
        parts.solve_layout.variable_storage_runs = vec![SolveVariableStorageRun {
            base: crate::SolveStorageCoordinate::Y(0),
            scalar_count: 2,
            role: SolveVariableStorageRole::State,
            value_kind: SolveVariableValueKind::Real,
        }];
        parts.solve_layout.variable_declarations = vec![SolveVariableDeclaration::new(
            SolveVariableStorageRole::State,
            SolveVariableValueKind::Real,
        )];
        parts.solve_layout.state_scalar_count = 2;
        parts.initial_y = vec![0.0, 0.0];
        parts.solver_nominals = vec![1.0, 1.0];
        parts.parameters = vec![0.0];
        let mut entry = CatalogFixtureEntry::state(1, "x", 2);
        entry.provenance = fixture_span();
        parts.entries = vec![entry];
        parts
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

        fn state_root_parts() -> ModelFixtureParts {
            let mut parts = delay_bearing_parts_with_one_state().without_delays();
            parts.events.root_conditions = load_row(LinearOp::LoadY { dst: 0, index: 0 });
            parts.events.root_relation_memory_targets = vec![None];
            parts.events.root_zero_domains = vec![RootZeroDomain::Previous];
            parts.events.root_relation_refresh_roles = vec![RootRelationRefreshRole::Frozen];
            parts
        }

        #[test]
        fn continuously_state_dependent_roots_are_the_ordered_inventory() {
            let component = FmiComponent::construct(build_model(state_root_parts())).unwrap();
            assert_eq!(
                component.event_indicators().sources(),
                [FmiEventIndicatorSource::RootCondition { index: 0 }]
            );
            let runtime_view = component.into_runtime_view();
            let facts = runtime_view.linked_runtime_facts();
            let plan = facts.indicator_plan();
            assert_eq!(plan.published_width().len(), 1);
            assert_eq!(plan.root_value_width().len(), 1);
            assert_eq!(plan.deadline_width().len(), 0);
            assert_eq!(plan.domain_width().len(), 1);
            assert_eq!(
                plan.entries()[0].reading(),
                FmiIndicatorReading::RootValue { index: 0 }
            );
            assert_eq!(plan.entries()[0].zero_side(), FmiIndicatorZeroSide::Frozen);
            assert_eq!(plan.crossing_root_index(0), Some(0));
            assert_eq!(plan.relation_memory_target(0), None);
        }

        #[test]
        fn scheduled_roots_are_absent_from_the_indicator_inventory() {
            let mut parts = state_root_parts();
            parts.events.scheduled_root_conditions = vec![ScheduledRootCondition {
                root_index: 0,
                period_seconds: 1.0,
                phase_seconds: 0.0,
            }];
            let component = FmiComponent::construct(build_model(parts)).unwrap();
            assert!(component.event_indicators().is_empty());
        }

        #[test]
        fn parameter_only_roots_and_deadlines_are_time_events_not_indicators() {
            let mut parts = delay_bearing_parts_with_named_run("p").without_delays();
            parts.events.root_conditions = load_row(LinearOp::LoadP { dst: 0, index: 0 });
            parts.events.root_relation_memory_targets = vec![None];
            parts.events.root_zero_domains = vec![RootZeroDomain::Previous];
            parts.events.root_relation_refresh_roles = vec![RootRelationRefreshRole::Frozen];
            parts.events.dynamic_time_event_rhs = load_row(LinearOp::LoadP { dst: 0, index: 0 });
            let component = FmiComponent::construct(build_model(parts)).unwrap();
            assert!(component.event_indicators().is_empty());
        }

        #[test]
        fn state_deadlines_precede_delay_discontinuity_sources_deterministically() {
            let mut parts = delay_bearing_parts_with_one_state();
            parts.events.dynamic_time_event_rhs = load_row(LinearOp::LoadY { dst: 0, index: 0 });
            let component = FmiComponent::construct(build_model(parts)).unwrap();
            assert_eq!(
                component.event_indicators().sources(),
                [
                    FmiEventIndicatorSource::DynamicTimeEvent { index: 0 },
                    FmiEventIndicatorSource::DelayDiscontinuity { index: 0 },
                ]
            );
            let runtime_view = component.into_runtime_view();
            let facts = runtime_view.linked_runtime_facts();
            let plan = facts.indicator_plan();
            assert_eq!(plan.published_width().len(), 2);
            assert_eq!(plan.root_value_width().len(), 1);
            assert_eq!(plan.deadline_width().len(), 1);
            assert_eq!(plan.domain_width().len(), 2);
            assert_eq!(
                plan.entries()
                    .iter()
                    .map(FmiEventIndicatorEntry::reading)
                    .collect::<Vec<_>>(),
                [
                    FmiIndicatorReading::DeadlineDistance { index: 0 },
                    FmiIndicatorReading::RootValue { index: 0 },
                ]
            );
        }
    }

    fn is_derived_local(variable: &FmiVariable) -> bool {
        matches!(variable.backing(), FmiValueBacking::MaxStepDuration)
    }

    #[test]
    fn delay_free_kernel_publishes_no_local() {
        let component = FmiComponent::construct(empty_binary64_first_product_model())
            .expect("a delay-free kernel needs no step bound");

        assert!(component.max_step_duration().is_none());
        assert!(component.variables().is_empty());
        assert_eq!(component.derivative_value_reference_base_fmi3(), 1);
    }

    #[test]
    fn delay_free_inventory_holds_exactly_one_entry_per_storage_run() {
        let parts = delay_bearing_parts_with_named_run("p").without_delays();

        let component = FmiComponent::construct(build_model(parts))
            .expect("one run is a complete delay-free inventory");

        assert_eq!(component.variables().len(), 1);
        assert_eq!(component.variables()[0].name(), "p");
        assert!(component.max_step_duration().is_none());
        assert_eq!(component.derivative_value_reference_base_fmi3(), 2);
    }

    #[test]
    fn delay_bearing_kernel_publishes_one_calculated_float64_local() {
        let component = FmiComponent::construct(delay_bearing_model(2))
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
        let component = FmiComponent::construct(delay_bearing_model(1))
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
        let model = delay_bearing_model_with_one_run();

        let component = FmiComponent::construct(model)
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
        let model = delay_bearing_model_with_one_run();
        let runs = model.problem().solve_layout().variable_storage_runs.len();

        let component = FmiComponent::construct(model)
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
        let model = delay_bearing_model_with_one_run();

        let component = FmiComponent::construct(model)
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
        let model = delay_bearing_model_with_one_run();

        let component = FmiComponent::construct(model)
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
            let model = delay_bearing_model_with_one_run();
            let component = FmiComponent::construct(model)
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
        let model = delay_bearing_model_with_one_state();

        let component = FmiComponent::construct(model)
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
        let model = delay_bearing_model_with_one_state();

        let component = FmiComponent::construct(model)
            .expect("one state run plus one delay partition is a complete inventory");

        assert_eq!(component.state_variable_indices(), [0]);
        let state = &component.variables()[component.state_variable_indices()[0]];
        assert_eq!(state.role(), Some(SolveVariableStorageRole::State));
        assert_eq!(component.derivative_value_reference_base_fmi3(), 3);
        let runtime_view = component.into_runtime_view();
        let facts = runtime_view.linked_runtime_facts();
        assert_eq!(
            facts.delay(),
            FmiDelayCapability::MaximumStepDuration { value_reference: 2 }
        );
        assert_eq!(facts.directional_references().len(), 1);
        let state_derivative = facts.directional_references()[0];
        assert_eq!(state_derivative.state_value_reference(), 1);
        assert_eq!(state_derivative.derivative_value_reference(), 3);
        assert_eq!(state_derivative.storage_base(), 0);
        assert_eq!(state_derivative.serialized_width(), 2);
    }

    #[test]
    fn the_unconstrained_sentinel_is_the_maximum_finite_float64() {
        assert_eq!(MAX_STEP_DURATION_UNCONSTRAINED, f64::MAX);
        assert!(MAX_STEP_DURATION_UNCONSTRAINED.is_finite());
    }

    #[test]
    fn a_source_variable_may_not_take_the_reserved_name_of_the_published_local() {
        let model = delay_bearing_model_with_named_run(MAX_STEP_DURATION_NAME);

        let rejected = FmiComponent::construct(model);

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
        let parts = delay_bearing_parts_with_named_run(MAX_STEP_DURATION_NAME).without_delays();

        let component = FmiComponent::construct(build_model(parts))
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
            build_model(delay_bearing_parts_with_named_run("p").without_delays())
        }

        fn construct(model: SolveModel) -> FmiComponent {
            FmiComponent::construct(model).expect("one run is a complete inventory")
        }

        /// Narrow the component built from `mutate`d event-free kernel, and
        /// return how the narrowing refused it.
        ///
        /// Each caller sets exactly one partition, so removing that partition's
        /// term from the single-source presence query in `crate::feature_query`
        /// lets its component narrow and fails exactly that one case.
        fn rejection_for(mutate: impl FnOnce(&mut ModelFixtureParts)) -> FmiEventFreeError {
            let mut parts = delay_bearing_parts_with_named_run("p").without_delays();
            mutate(&mut parts);

            construct(build_model(parts))
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
                rejection_for(|parts| {
                    parts.events.condition_memory_parameter_indices = vec![0];
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
                rejection_for(|parts| {
                    let events = &mut parts.events;
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
                rejection_for(|parts| {
                    parts.events.scheduled_time_events = vec![1.0];
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
            let model = delay_bearing_model_with_one_run();
            let component = FmiComponent::construct(model)
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
                rejection_for(|parts| {
                    parts.events.has_terminal_event = true;
                    parts.solve_layout.terminal_event_parameter_index = Some(0);
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
                rejection_for(|parts| {
                    parts.clocks.periodic_event_schedules = vec![
                        PeriodicEventSchedule::from_seconds(0.1, 0.0)
                            .expect("a positive rational period is a checked schedule"),
                    ];
                    parts.clocks.activation_parameter_indices = vec![0];
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
            assert_eq!(entries[0]["storage"]["base"], 0);
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
            let parts = delay_bearing_parts_with_named_run("p").without_delays();

            let component = FmiComponent::construct(build_model(parts))
                .expect("one run is a complete inventory");

            assert!(!component.needs_completed_integrator_step());
        }

        #[test]
        fn a_delay_bearing_component_needs_the_completed_integrator_step() {
            let model = delay_bearing_model_with_one_run();

            let component = FmiComponent::construct(model)
                .expect("one run plus one delay partition is a complete inventory");

            assert!(component.needs_completed_integrator_step());
        }

        /// An event-bearing kernel that publishes no maximum-step-duration
        /// local still needs the callback, because the declaration follows the
        /// event domain rather than the inventory.
        #[test]
        fn an_event_bearing_component_needs_the_completed_integrator_step() {
            let mut parts = delay_bearing_parts_with_named_run("p").without_delays();
            parts.events.scheduled_time_events = vec![1.0];

            let component = FmiComponent::construct(build_model(parts))
                .expect("one run is a complete inventory");

            assert!(component.max_step_duration().is_none());
            assert!(component.needs_completed_integrator_step());
        }
    }
}

mod checked_version_projections {
    use super::*;
    use crate::{
        SolveLayout, SolveVariableDeclaration, SolveVariableStorageRole, SolveVariableStorageRun,
        SolverNameIndexMaps, VarLayout,
    };
    use indexmap::IndexMap;

    fn interleaved_model(
        real_format: SolveRealFormat,
        parameter_kind: SolveVariableValueKind,
    ) -> SolveModel {
        build_model(interleaved_parts(real_format, parameter_kind))
    }

    fn interleaved_parts(
        real_format: SolveRealFormat,
        parameter_kind: SolveVariableValueKind,
    ) -> ModelFixtureParts {
        let mut parts = ModelFixtureParts::empty(real_format);
        parts.layout = VarLayout::from_parts(IndexMap::new(), 4, 1);
        parts.solve_layout = SolveLayout {
            solver_maps: SolverNameIndexMaps {
                names: vec![
                    "x[1]".to_string(),
                    "x[2]".to_string(),
                    "z[1]".to_string(),
                    "z[2]".to_string(),
                ],
                name_to_idx: IndexMap::from([
                    ("x[1]".to_string(), 0),
                    ("x[2]".to_string(), 1),
                    ("z[1]".to_string(), 2),
                    ("z[2]".to_string(), 3),
                ]),
                base_to_indices: IndexMap::from([
                    ("x".to_string(), vec![0, 1]),
                    ("z".to_string(), vec![2, 3]),
                ]),
            },
            variable_storage_runs: vec![
                SolveVariableStorageRun {
                    base: crate::SolveStorageCoordinate::P(0),
                    scalar_count: 1,
                    role: SolveVariableStorageRole::Parameter,
                    value_kind: parameter_kind,
                },
                SolveVariableStorageRun {
                    base: crate::SolveStorageCoordinate::Y(0),
                    scalar_count: 2,
                    role: SolveVariableStorageRole::State,
                    value_kind: SolveVariableValueKind::Real,
                },
                SolveVariableStorageRun {
                    base: crate::SolveStorageCoordinate::Y(2),
                    scalar_count: 2,
                    role: SolveVariableStorageRole::Output,
                    value_kind: SolveVariableValueKind::Real,
                },
            ],
            variable_declarations: vec![
                SolveVariableDeclaration::new(SolveVariableStorageRole::Parameter, parameter_kind),
                SolveVariableDeclaration::new(
                    SolveVariableStorageRole::State,
                    SolveVariableValueKind::Real,
                ),
                SolveVariableDeclaration::new(
                    SolveVariableStorageRole::Output,
                    SolveVariableValueKind::Real,
                ),
            ],
            state_scalar_count: 2,
            algebraic_scalar_count: 0,
            output_scalar_count: 2,
            parameter_count: 1,
            static_parameter_names: vec!["p".to_string()],
            compiled_parameter_len: 1,
            input_scalar_names: Vec::new(),
            discrete_real_scalar_names: Vec::new(),
            discrete_valued_scalar_names: Vec::new(),
            relation_memory_parameter_indices: Vec::new(),
            initial_event_parameter_index: None,
            initial_homotopy_parameter_index: None,
            terminal_event_parameter_index: None,
            pre_param_bindings: Vec::new(),
        };
        parts.initial_y = vec![10.0, 20.0, 30.0, 40.0];
        parts.solver_nominals = vec![1.0, 2.0, 3.0, 4.0];
        parts.parameters = vec![5.0];

        let mut parameter = CatalogFixtureEntry::parameter(1, "p", 5.0);
        parameter.unit = Some("kg".to_string());
        parameter.description = Some("mass".to_string());
        let mut state = CatalogFixtureEntry::state(2, "x", 2);
        state.start = vec![10.0, 20.0];
        state.nominal = Some(vec![1.0, 2.0]);
        state.unit = Some("m".to_string());
        state.description = Some("position".to_string());
        let mut output = CatalogFixtureEntry::state(3, "z", 2);
        output.start = vec![30.0, 40.0];
        output.nominal = Some(vec![3.0, 4.0]);
        output.causality = SolveVariableCausality::Output;
        parts.entries = vec![parameter, state, output];
        parts
    }

    #[test]
    fn interleaved_tensor_declarations_receive_final_checked_fmi_indices() {
        let view = FmiComponent::construct(interleaved_model(
            SolveRealFormat::Binary64,
            SolveVariableValueKind::Real,
        ))
        .expect("interleaved model has checked FMI metadata")
        .into_codegen_view()
        .try_event_free()
        .expect("Binary64 Real model has checked FMI projections");

        let fmi2 = view.fmi2();
        assert_eq!(
            fmi2.unit_definitions()
                .iter()
                .map(FmiUnitDefinition::name)
                .collect::<Vec<_>>(),
            ["kg", "m", "s"]
        );
        assert_eq!(
            fmi2.unit_definitions()[2].base_unit_seconds_exponent(),
            Some(1)
        );
        assert_eq!(fmi2.variables()[0].start(), Some(5.0));
        assert_eq!(fmi2.variables()[0].unit(), Some("kg"));
        assert_eq!(fmi2.variables()[0].description(), Some("mass"));
        assert_eq!(fmi2.variables()[1].start(), Some(10.0));
        assert_eq!(fmi2.variables()[1].nominal(), Some(1.0));
        assert_eq!(fmi2.variables()[1].unit(), Some("m"));
        assert_eq!(fmi2.variables()[1].initial(), Some(FmiInitial::Approx));
        let source_indices = fmi2
            .variables()
            .iter()
            .map(|variable| {
                (
                    variable.name(),
                    variable.value_reference(),
                    variable.model_index(),
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(
            source_indices,
            [
                ("p", 1, 2),
                ("x[1]", 2, 3),
                ("x[2]", 3, 4),
                ("z[1]", 4, 5),
                ("z[2]", 5, 6),
            ]
        );
        assert_eq!(fmi2.output_model_indices(), [5, 6]);
        assert_eq!(fmi2.derivative_model_indices(), [7, 8]);
        assert_eq!(fmi2.initial_unknown_model_indices(), [3, 4, 5, 6, 7, 8]);
        assert_eq!(fmi2.derivatives()[0].name(), "der(x[1])");
        assert_eq!(fmi2.derivatives()[0].state_scalar_index(), 0);
        assert_eq!(fmi2.derivatives()[0].initial(), FmiInitial::Calculated);
        let fmi2_link = fmi2.variables()[1]
            .derivative()
            .expect("state scalar has derivative");
        assert_eq!(fmi2_link.state_value_reference(), 2);
        assert_eq!(fmi2_link.derivative_value_reference(), 6);
        assert_eq!(fmi2_link.state_model_index(), 3);
        assert_eq!(fmi2_link.derivative_model_index(), 7);

        let fmi3 = view.fmi3();
        assert_eq!(fmi3.unit_definitions(), fmi2.unit_definitions());
        assert_eq!(
            fmi3.variables()
                .iter()
                .map(|variable| (variable.name(), variable.value_reference()))
                .collect::<Vec<_>>(),
            [("p", 1), ("x", 2), ("z", 3)]
        );
        assert_eq!(fmi3.output_value_references(), [3]);
        assert_eq!(fmi3.derivative_value_references(), [4]);
        assert_eq!(fmi3.initial_unknown_value_references(), [2, 3, 4]);
        assert_eq!(fmi3.variables()[0].start(), Some([5.0].as_slice()));
        assert_eq!(fmi3.variables()[0].unit(), Some("kg"));
        assert_eq!(fmi3.variables()[1].start(), Some([10.0, 20.0].as_slice()));
        assert_eq!(fmi3.variables()[1].nominal(), Some([1.0, 2.0].as_slice()));
        assert_eq!(fmi3.variables()[1].unit(), Some("m"));
        assert_eq!(fmi3.variables()[1].initial(), Some(FmiInitial::Approx));
        assert_eq!(fmi3.derivatives()[0].storage().base(), 0);
        assert_eq!(fmi3.derivatives()[0].storage().scalar_count(), 2);
        assert_eq!(fmi3.derivatives()[0].name(), "der(x)");
        assert_eq!(fmi3.derivatives()[0].dimensions(), [2]);
        assert_eq!(fmi3.derivatives()[0].initial(), FmiInitial::Calculated);
        let fmi3_link = fmi3.derivatives()[0].link();
        assert_eq!(fmi3_link.state_value_reference(), 2);
        assert_eq!(fmi3_link.derivative_value_reference(), 4);
        assert_eq!(fmi3_link.state_model_index(), 3);
        assert_eq!(fmi3_link.derivative_model_index(), 5);
    }

    #[test]
    fn catalog_values_units_initial_and_write_policy_survive_projection() {
        let component = FmiComponent::construct(interleaved_model(
            SolveRealFormat::Binary64,
            SolveVariableValueKind::Real,
        ))
        .expect("fixture has checked FMI metadata");
        let parameter = &component.variables()[0];
        let state = &component.variables()[1];
        assert_eq!(parameter.start(), Some([5.0].as_slice()));
        assert_eq!(parameter.unit(), Some("kg"));
        assert_eq!(parameter.description(), Some("mass"));
        assert_eq!(parameter.initial(), Some(FmiInitial::Exact));
        assert_eq!(parameter.write_policy(), FmiWritePolicy::FixedParameter);
        assert_eq!(state.start(), Some([10.0, 20.0].as_slice()));
        assert_eq!(state.nominal(), Some([1.0, 2.0].as_slice()));
        assert_eq!(state.unit(), Some("m"));
        assert_eq!(state.initial(), Some(FmiInitial::Approx));
        assert_eq!(
            state.write_policy(),
            FmiWritePolicy::ContinuousState {
                initial: FmiStateInitial::Approx,
                reinit: FmiStateReinit::False,
            }
        );
    }

    #[test]
    fn unsupported_real_format_fails_before_an_event_free_view_escapes() {
        let rejected = FmiComponent::construct(interleaved_model(
            SolveRealFormat::Binary32,
            SolveVariableValueKind::Real,
        ))
        .expect("FMI metadata remains format-neutral")
        .into_codegen_view()
        .try_event_free()
        .expect_err("the Float64 projection rejects Binary32");
        assert_eq!(
            rejected,
            FmiEventFreeError::Projection(FmiProjectionError::UnsupportedRealFormat {
                actual: SolveRealFormat::Binary32,
            })
        );
    }

    #[test]
    fn unsupported_non_real_variable_fails_before_an_event_free_view_escapes() {
        let rejected = FmiComponent::construct(interleaved_model(
            SolveRealFormat::Binary64,
            SolveVariableValueKind::Integer,
        ))
        .expect("FMI metadata retains the typed declaration")
        .into_codegen_view()
        .try_event_free()
        .expect_err("the Float64 projection rejects Integer");
        assert!(matches!(
            rejected,
            FmiEventFreeError::Projection(FmiProjectionError::UnsupportedValueKind {
                variable,
                actual: SolveVariableValueKind::Integer,
                ..
            }) if variable == "p"
        ));
    }

    #[test]
    fn zero_extent_variable_fails_before_an_event_free_view_escapes() {
        let mut parts = interleaved_parts(SolveRealFormat::Binary64, SolveVariableValueKind::Real);
        parts.layout = VarLayout::from_parts(IndexMap::new(), 4, 0);
        parts.solve_layout.variable_storage_runs[0].scalar_count = 0;
        parts.solve_layout.parameter_count = 0;
        parts.solve_layout.static_parameter_names.clear();
        parts.solve_layout.compiled_parameter_len = 0;
        parts.parameters.clear();

        let mut parameter = CatalogFixtureEntry::parameter(1, "p", 5.0);
        parameter.dimensions = vec![0];
        parameter.scalar_names.clear();
        parameter.start.clear();
        let mut state = CatalogFixtureEntry::state(2, "x", 2);
        state.start = vec![10.0, 20.0];
        state.nominal = Some(vec![1.0, 2.0]);
        let mut output = CatalogFixtureEntry::state(3, "z", 2);
        output.start = vec![30.0, 40.0];
        output.nominal = Some(vec![3.0, 4.0]);
        output.causality = SolveVariableCausality::Output;
        parts.entries = vec![parameter, state, output];

        let rejected = FmiComponent::construct(build_model(parts))
            .expect("format-neutral FMI metadata retains zero extent")
            .into_codegen_view()
            .try_event_free()
            .expect_err("the current Float64 rendering path rejects zero extent");
        assert!(matches!(
            rejected,
            FmiEventFreeError::Projection(FmiProjectionError::ZeroExtentVariable {
                variable,
                ..
            }) if variable == "p"
        ));
    }

    #[test]
    fn invalid_unit_name_fails_before_an_event_free_view_escapes() {
        let mut parts = interleaved_parts(SolveRealFormat::Binary64, SolveVariableValueKind::Real);
        let mut parameter = CatalogFixtureEntry::parameter(1, "p", 5.0);
        parameter.unit = Some(" kg\n".to_string());
        let mut state = CatalogFixtureEntry::state(2, "x", 2);
        state.start = vec![10.0, 20.0];
        state.nominal = Some(vec![1.0, 2.0]);
        let mut output = CatalogFixtureEntry::state(3, "z", 2);
        output.start = vec![30.0, 40.0];
        output.nominal = Some(vec![3.0, 4.0]);
        output.causality = SolveVariableCausality::Output;
        parts.entries = vec![parameter, state, output];

        let rejected = FmiComponent::construct(build_model(parts))
            .expect("format-neutral FMI metadata retains the source unit")
            .into_codegen_view()
            .try_event_free()
            .expect_err("invalid XML unit spelling cannot reach an FMI renderer");
        assert!(matches!(
            rejected,
            FmiEventFreeError::Projection(FmiProjectionError::InvalidUnitName {
                variable,
                unit,
                ..
            }) if variable == "p" && unit == " kg\n"
        ));
    }

    #[test]
    fn catalog_shape_overflow_is_rejected_before_storage_projection() {
        let mut entry = CatalogFixtureEntry::parameter(1, "p", 0.0);
        entry.dimensions = vec![u32::MAX, u32::MAX, 2];
        assert!(matches!(
            catalog_issue_error(entry),
            SolveVariableCatalogError::ShapeOverflow { .. }
        ));
    }
}

/// The bounded Solve-to-FMI-3 scalar constant-derivative edge.
///
/// One end-to-end fixture anchors every check: a single Binary64 scalar state
/// with a fixed bitwise start and a constant derivative kernel. The start and
/// the constant are chosen away from every storage default (`0.0`) and away
/// from the `1.0` nominal, so no assertion below can pass by coinciding with
/// a default value. Mutation cases each rebuild the healthy fact pair, prove
/// it still passes, then flip exactly one fact and require the exact typed
/// refusal, so a refusal cannot come from an unrelated earlier check.
mod scalar_constant_derivative_edge {
    use super::*;
    use crate::{
        SolveLayout, SolveVariableDeclaration, SolveVariableStorageRole, SolveVariableStorageRun,
        SolverNameIndexMaps, VarLayout,
    };
    use indexmap::IndexMap;

    const STATE_START: f64 = 0.1;
    const DERIVATIVE_CONSTANT: f64 = 2.5;

    fn scalar_state_parts(start: f64) -> ModelFixtureParts {
        let mut parts = ModelFixtureParts::empty(SolveRealFormat::Binary64);
        parts.layout = VarLayout::from_parts(IndexMap::new(), 1, 0);
        parts.solve_layout = SolveLayout {
            solver_maps: SolverNameIndexMaps {
                names: vec!["x".to_string()],
                name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
            },
            variable_storage_runs: vec![SolveVariableStorageRun {
                base: crate::SolveStorageCoordinate::Y(0),
                scalar_count: 1,
                role: SolveVariableStorageRole::State,
                value_kind: SolveVariableValueKind::Real,
            }],
            variable_declarations: vec![SolveVariableDeclaration::new(
                SolveVariableStorageRole::State,
                SolveVariableValueKind::Real,
            )],
            state_scalar_count: 1,
            ..SolveLayout::default()
        };
        parts.initial_y = vec![start];
        parts.solver_nominals = vec![1.0];
        let mut state = CatalogFixtureEntry::parameter(1, "x", start);
        state.causality = SolveVariableCausality::Local;
        state.variability = SolveVariableVariability::Continuous;
        state.nominal = Some(vec![1.0]);
        parts.entries = vec![state];
        parts
    }

    fn scalar_state_model(start: f64, derivative_rows: Vec<Vec<LinearOp>>) -> SolveModel {
        build_model_from_derivative_rows(
            scalar_state_parts(start),
            Some(fixture_rows(
                derivative_rows,
                "FMI scalar constant-derivative fixture",
            )),
        )
        .expect("FMI scalar-state fixture model is valid by construction")
    }

    fn constant_rows(value: f64) -> Vec<Vec<LinearOp>> {
        vec![vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]]
    }

    fn event_free_view(start: f64, derivative_rows: Vec<Vec<LinearOp>>) -> FmiEventFreeCodegenView {
        FmiComponent::construct(scalar_state_model(start, derivative_rows))
            .expect("the scalar-state fixture has a checked FMI inventory")
            .into_codegen_view()
            .try_event_free()
            .expect("the scalar-state fixture is event-free")
    }

    /// Rebuild the healthy fact pair and prove it passes, so every mutation
    /// below starts from a demonstrated positive control rather than an
    /// empty or vacuously admitted scan.
    fn healthy_checked_facts() -> (
        ScalarConstantDerivativeSolveFacts,
        ScalarConstantDerivativeFmi3Facts,
    ) {
        let component = FmiComponent::construct(scalar_state_model(
            STATE_START,
            constant_rows(DERIVATIVE_CONSTANT),
        ))
        .expect("the scalar-state fixture has a checked FMI inventory");
        let solve = project_scalar_constant_derivative_solve_facts(component.runtime_model());
        let view = component
            .into_codegen_view()
            .try_event_free()
            .expect("the scalar-state fixture is event-free");
        let fmi3 = project_scalar_constant_derivative_fmi3_facts(&view);
        check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect("the unmutated fixture facts are the positive control");
        (solve, fmi3)
    }

    #[test]
    fn the_carrier_admits_the_fixture_and_retains_the_receipt() {
        let carrier = Fmi3ScalarConstantDerivativeCarrier::admit(event_free_view(
            STATE_START,
            constant_rows(DERIVATIVE_CONSTANT),
        ))
        .expect("the healthy fixture is inside the admitted profile");

        let receipt = carrier.receipt();
        assert_eq!(receipt.state_value_reference(), 1);
        assert_eq!(receipt.derivative_value_reference(), 2);
        assert_eq!(receipt.start_bits(), STATE_START.to_bits());
        assert_eq!(
            receipt.derivative_constant_bits(),
            DERIVATIVE_CONSTANT.to_bits()
        );
        assert_eq!(
            receipt.source_occurrence(),
            SourceOccurrenceId::try_from(InstanceId::new(1))
                .expect("the fixture occurrence is nonzero")
        );

        // The carrier retains the same checked FMI 3 projection the facts
        // were drawn from.
        assert_eq!(carrier.event_free().fmi3().variables().len(), 1);
        assert_eq!(carrier.event_free().fmi3().derivatives().len(), 1);
    }

    #[test]
    fn a_state_reading_kernel_is_a_typed_unsupported_capability() {
        let rejected = Fmi3ScalarConstantDerivativeCarrier::admit(event_free_view(
            STATE_START,
            vec![vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
        ))
        .expect_err("a derivative that reads the state is not a constant");

        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Unsupported(
                ScalarConstantDerivativeUnsupported::KernelOperation {
                    position: 0,
                    operation: "LoadY",
                }
            )
        );
    }

    #[test]
    fn an_extra_kernel_operation_is_a_typed_unsupported_capability() {
        let rejected = Fmi3ScalarConstantDerivativeCarrier::admit(event_free_view(
            STATE_START,
            vec![vec![
                LinearOp::Const {
                    dst: 0,
                    value: DERIVATIVE_CONSTANT,
                },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
        ))
        .expect_err("a three-operation kernel is outside the profile");

        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Unsupported(
                ScalarConstantDerivativeUnsupported::KernelOperationCount { actual: 3 }
            )
        );
    }

    #[test]
    fn one_flipped_bit_in_the_xml_start_leg_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        let start = fmi3.variables[0]
            .xml_start_bits
            .as_mut()
            .expect("the healthy projection carries an XML start");
        start[0] ^= 1;

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a one-bit XML start change must refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::StartBits {
                    leg: StartLeg::ProjectionStart,
                    expected_bits: STATE_START.to_bits(),
                    actual_bits: STATE_START.to_bits() ^ 1,
                }
            )
        );
    }

    #[test]
    fn one_flipped_bit_in_the_inventory_start_leg_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        let start = fmi3.inventory[0]
            .start_bits
            .as_mut()
            .expect("the healthy inventory entry carries a start");
        start[0] ^= 1;

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a one-bit inventory start change must refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::StartBits {
                    leg: StartLeg::InventoryEntry,
                    expected_bits: STATE_START.to_bits(),
                    actual_bits: STATE_START.to_bits() ^ 1,
                }
            )
        );
    }

    #[test]
    fn a_catalog_runtime_start_disagreement_is_refused() {
        let (mut solve, fmi3) = healthy_checked_facts();
        solve.initial_y_bits[0] = 0.25f64.to_bits();

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("catalog and runtime starts must agree bitwise");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::StartBits {
                    leg: StartLeg::RuntimeVector,
                    expected_bits: STATE_START.to_bits(),
                    actual_bits: 0.25f64.to_bits(),
                }
            )
        );
    }

    #[test]
    fn a_swapped_state_derivative_pairing_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.variables[0].link_state_value_reference = Some(2);
        fmi3.variables[0].link_derivative_value_reference = Some(1);

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("presenting the derivative as the state must refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::VariableDerivativeLink {
                    expected_state: 1,
                    expected_derivative: 2,
                    actual_state: Some(2),
                    actual_derivative: Some(1),
                }
            )
        );
    }

    #[test]
    fn a_consistently_swapped_derivative_fact_collides_with_the_state_reference() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.variables[0].link_state_value_reference = Some(2);
        fmi3.variables[0].link_derivative_value_reference = Some(1);
        fmi3.derivatives[0].link_state_value_reference = 2;
        fmi3.derivatives[0].link_derivative_value_reference = 1;

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a swap that reuses the state reference must refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::ValueReferenceCollision {
                    time: 0,
                    state: 1,
                    derivative: 1,
                }
            )
        );
    }

    /// The multiset-preserving swap: state takes value reference 2 and the
    /// derivative takes 1, with every internal pairing, list, and base
    /// rewritten consistently. A multiset comparison of issued references
    /// would accept this; only re-deriving the positional assignment rule
    /// (entry `k` owns `k + 1`, derivatives follow the inventory) refuses it.
    #[test]
    fn a_multiset_preserving_value_reference_swap_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.inventory[0].value_reference = 2;
        fmi3.derivative_value_reference_base = 1;
        fmi3.variables[0].value_reference = 2;
        fmi3.variables[0].link_state_value_reference = Some(2);
        fmi3.variables[0].link_derivative_value_reference = Some(1);
        fmi3.derivatives[0].link_state_value_reference = 2;
        fmi3.derivatives[0].link_derivative_value_reference = 1;
        fmi3.derivative_value_references = vec![1];
        fmi3.initial_unknown_value_references = vec![1];

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a consistent whole-inventory swap must still refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::InventoryValueReference {
                    expected: 1,
                    actual: 2,
                }
            )
        );
    }

    #[test]
    fn a_shifted_state_storage_run_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        let shifted = FmiStorageRun {
            column: SolveStorageColumn::Y,
            base: 1,
            scalar_count: 1,
        };
        fmi3.variables[0].storage = shifted;

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a storage offset change must refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::VariableStorage { actual: shifted }
            )
        );
    }

    #[test]
    fn a_shifted_derivative_storage_range_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.derivatives[0].storage_base = 1;

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a derivative storage offset change must refuse");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::DerivativeStorage {
                    base: 1,
                    scalar_count: 1,
                }
            )
        );
    }

    #[test]
    fn a_forged_initial_unknown_membership_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.variables[0].initial_unknown_member = true;

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("an exact-start state is not an initial unknown");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::VariableModelStructure {
                    output: false,
                    continuous_state_derivative: false,
                    initial_unknown: true,
                }
            )
        );
    }

    #[test]
    fn a_forged_output_inventory_entry_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.output_value_references = vec![1];

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("a local state does not enter the output inventory");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::OutputInventory { actual: vec![1] }
            )
        );
    }

    #[test]
    fn a_dropped_derivative_inventory_entry_is_refused() {
        let (solve, mut fmi3) = healthy_checked_facts();
        fmi3.derivative_value_references = Vec::new();

        let rejected = check_scalar_constant_derivative_projection(&solve, &fmi3)
            .expect_err("the derivative inventory cannot be empty");
        assert_eq!(
            rejected,
            ScalarConstantDerivativeError::Disagreement(
                ScalarConstantDerivativeDisagreement::DerivativeInventory {
                    expected: 2,
                    actual: Vec::new(),
                }
            )
        );
    }
}
