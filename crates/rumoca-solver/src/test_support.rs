//! Checked construction helpers shared by the solver crate's unit tests.
//!
//! These helpers only remove fixture boilerplate. Every executable aggregate
//! still passes through the same production constructors as compiler output.

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

pub(crate) struct ContinuousSystemFixture {
    pub implicit_rhs: solve::ComputeBlock,
    pub implicit_row_targets: Vec<Option<solve::ScalarSlot>>,
    pub algebraic_projection_plan: solve::AlgebraicProjectionPlan,
    pub residual: solve::ComputeBlock,
    pub manifold_residual: solve::ComputeBlock,
    pub manifold_projection_plan: solve::AlgebraicProjectionPlan,
    pub derivative_rhs: solve::ComputeBlock,
    pub refresh_plans: Option<solve::ContinuousRefreshPlanInputs>,
}

pub(crate) struct SolveModelFixture {
    pub problem: solve::SolveProblem,
    pub pure_calls: solve::SolvePureCallTable,
    pub artifacts: solve::SolveArtifactInputs,
    pub initial_y: Vec<f64>,
    pub solver_nominals: Vec<f64>,
    pub parameters: Vec<f64>,
    pub visible_value_rows: solve::ScalarProgramBlock,
    pub variable_entries: Vec<solve::SolveVariableCatalogSourceEntry>,
}

pub(crate) struct RealScalarVariableFixture {
    pub source_occurrence: rumoca_core::SourceOccurrenceId,
    pub name: String,
    pub storage: solve::ScalarSlot,
    pub role: solve::SolveVariableStorageRole,
    pub causality: solve::SolveVariableCausality,
    pub variability: solve::SolveVariableVariability,
    pub fixed: rumoca_core::Fixity,
    pub start: f64,
    pub nominal: Option<f64>,
    pub provenance: rumoca_core::Span,
}

pub(crate) struct BooleanScalarVariableFixture {
    pub source_occurrence: rumoca_core::SourceOccurrenceId,
    pub name: String,
    pub start: bool,
    pub provenance: rumoca_core::Span,
}

impl RealScalarVariableFixture {
    pub(crate) fn state(
        source_occurrence: u32,
        name: impl Into<String>,
        index: usize,
        start: f64,
        nominal: f64,
        fixed: bool,
        provenance: rumoca_core::Span,
    ) -> Self {
        Self {
            source_occurrence: fixture_source_occurrence(source_occurrence),
            name: name.into(),
            storage: solve::scalar_slot_y(index),
            role: solve::SolveVariableStorageRole::State,
            causality: solve::SolveVariableCausality::Local,
            variability: solve::SolveVariableVariability::Continuous,
            fixed: rumoca_core::Fixity::from(fixed),
            start,
            nominal: Some(nominal),
            provenance,
        }
    }

    pub(crate) fn algebraic(
        source_occurrence: u32,
        name: impl Into<String>,
        index: usize,
        start: f64,
        nominal: f64,
        provenance: rumoca_core::Span,
    ) -> Self {
        Self {
            source_occurrence: fixture_source_occurrence(source_occurrence),
            name: name.into(),
            storage: solve::scalar_slot_y(index),
            role: solve::SolveVariableStorageRole::Algebraic,
            causality: solve::SolveVariableCausality::Local,
            variability: solve::SolveVariableVariability::Continuous,
            fixed: rumoca_core::Fixity::Free,
            start,
            nominal: Some(nominal),
            provenance,
        }
    }

    pub(crate) fn external_input(
        source_occurrence: u32,
        name: impl Into<String>,
        index: usize,
        start: f64,
        provenance: rumoca_core::Span,
    ) -> Self {
        Self {
            source_occurrence: fixture_source_occurrence(source_occurrence),
            name: name.into(),
            storage: solve::scalar_slot_p(index),
            role: solve::SolveVariableStorageRole::ExternalInput,
            causality: solve::SolveVariableCausality::Input,
            variability: solve::SolveVariableVariability::Continuous,
            fixed: rumoca_core::Fixity::Free,
            start,
            nominal: None,
            provenance,
        }
    }

    pub(crate) fn discrete_real(
        source_occurrence: u32,
        name: impl Into<String>,
        index: usize,
        start: f64,
        provenance: rumoca_core::Span,
    ) -> Self {
        Self {
            source_occurrence: fixture_source_occurrence(source_occurrence),
            name: name.into(),
            storage: solve::scalar_slot_p(index),
            role: solve::SolveVariableStorageRole::DiscreteReal,
            causality: solve::SolveVariableCausality::Local,
            variability: solve::SolveVariableVariability::Discrete,
            fixed: rumoca_core::Fixity::Free,
            start,
            nominal: None,
            provenance,
        }
    }
}

pub(crate) fn fixture_source_occurrence(index: u32) -> rumoca_core::SourceOccurrenceId {
    rumoca_core::SourceOccurrenceId::try_from(rumoca_core::InstanceId::new(index))
        .expect("fixture source occurrence is explicitly nonzero")
}

impl SolveModelFixture {
    pub(crate) fn from_model(model: solve::SolveModel) -> Self {
        let variable_entries = catalog_entries(&model);
        Self {
            problem: model.problem().clone(),
            pure_calls: model.pure_calls().clone(),
            artifacts: solve::SolveArtifactInputs {
                continuous: model.artifacts().continuous().clone(),
                initialization: model.artifacts().initialization().clone(),
            },
            initial_y: model.initial_y().to_vec(),
            solver_nominals: model.solver_nominals().to_vec(),
            parameters: model.parameters().to_vec(),
            visible_value_rows: model.visible_value_rows().clone(),
            variable_entries,
        }
    }

    pub(crate) fn try_seal(
        self,
    ) -> Result<solve::SolveModel, Box<solve::SolveModelConstructionError>> {
        solve::SolveModel::construct(
            self.problem,
            self.pure_calls,
            self.artifacts,
            solve::SolveModelRuntimeInputs {
                initial_y: self.initial_y,
                solver_nominals: self.solver_nominals,
                parameters: self.parameters,
            },
            self.visible_value_rows,
            self.variable_entries,
        )
        .map_err(Box::new)
    }

    #[track_caller]
    pub(crate) fn seal(self) -> solve::SolveModel {
        self.try_seal()
            .expect("fixture satisfies the checked SolveModel contract")
    }

    #[track_caller]
    pub(crate) fn seal_with_derived_artifacts(mut self) -> solve::SolveModel {
        self.artifacts = rumoca_phase_solve::lower_solve_artifacts(&self.problem)
            .expect("fixture programs admit the production Solve artifact derivation");
        self.seal()
    }

    #[track_caller]
    pub(crate) fn seal_with_custom_artifacts(mut self) -> solve::SolveModel {
        let (continuous, initialization) =
            rumoca_eval_solve::derive_solve_structural_artifacts(&self.problem, &self.artifacts)
                .expect("fixture artifacts admit the production structural derivation");
        self.artifacts.continuous.structural = continuous;
        self.artifacts.initialization.structural = initialization;
        self.seal()
    }
}

pub(crate) fn reseal_with(
    model: solve::SolveModel,
    edit: impl FnOnce(&mut SolveModelFixture),
) -> solve::SolveModel {
    let mut fixture = SolveModelFixture::from_model(model);
    edit(&mut fixture);
    fixture.seal()
}

pub(crate) fn with_explicit_real_scalar_catalog(
    model: solve::SolveModel,
    variables: Vec<RealScalarVariableFixture>,
    visible_value_rows: solve::ScalarProgramBlock,
) -> solve::SolveModel {
    let storage_runs = variables
        .iter()
        .map(|variable| solve::SolveVariableStorageRun {
            base: variable
                .storage
                .storage_coordinate()
                .expect("fixture uses writable storage"),
            scalar_count: 1,
            role: variable.role,
            value_kind: solve::SolveVariableValueKind::Real,
        })
        .collect();
    let declarations = variables
        .iter()
        .map(|variable| {
            solve::SolveVariableDeclaration::new(variable.role, solve::SolveVariableValueKind::Real)
        })
        .collect();
    let entries = explicit_real_scalar_catalog_entries(variables);
    let mut solve_layout = model.problem().solve_layout().clone();
    solve_layout.variable_storage_runs = storage_runs;
    solve_layout.variable_declarations = declarations;
    let discrete = model.problem().discrete().clone();
    let events = model.problem().events().clone();
    let clocks = model.problem().clocks().clone();
    let continuous = ContinuousSystemFixture::from_system(model.problem().continuous()).seal(
        &solve_layout,
        &discrete,
        &events,
        &clocks,
    );
    let problem = solve::SolveProblem::construct(
        model.problem().layout().clone(),
        solve_layout,
        continuous,
        model.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("explicit scalar catalog fixture satisfies the checked SolveProblem contract");
    let mut fixture = SolveModelFixture::from_model(model);
    fixture.problem = problem;
    fixture.visible_value_rows = visible_value_rows;
    fixture.variable_entries = entries;
    fixture.seal()
}

pub(crate) fn explicit_real_scalar_catalog_entries(
    variables: Vec<RealScalarVariableFixture>,
) -> Vec<solve::SolveVariableCatalogSourceEntry> {
    variables
        .into_iter()
        .map(real_scalar_catalog_entry)
        .collect()
}

pub(crate) fn explicit_boolean_scalar_catalog_entries(
    variables: Vec<BooleanScalarVariableFixture>,
) -> Vec<solve::SolveVariableCatalogSourceEntry> {
    variables
        .into_iter()
        .map(|variable| {
            (
                solve::SolveVariableSource::new(
                    variable.source_occurrence,
                    variable.name.clone(),
                    Vec::new(),
                    vec![variable.name],
                    variable.provenance,
                ),
                solve::SolveVariableSourceAttributes::new(
                    solve::SolveVariableCausality::Local,
                    solve::SolveVariableVariability::Discrete,
                    false,
                    None,
                    None,
                    rumoca_core::Fixity::Free,
                ),
                solve::SolveVariableEvaluatedValues::new(
                    Some(vec![f64::from(variable.start)]),
                    None,
                    None,
                    None,
                ),
            )
        })
        .collect()
}

pub(crate) fn direct_y_visible_rows(
    indices: impl IntoIterator<Item = usize>,
    provenance: rumoca_core::Span,
) -> solve::ScalarProgramBlock {
    let rows = indices
        .into_iter()
        .map(|index| {
            vec![
                solve::LinearOp::LoadY { dst: 0, index },
                solve::LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();
    solve::ScalarProgramBlock::with_source_span(
        rows,
        provenance
            .require_provenance("explicit direct-Y visibility fixture")
            .expect("fixture visibility provenance is source-backed"),
    )
    .expect("fixture visibility rows are computable")
}

pub(crate) fn direct_p_visible_rows(
    indices: impl IntoIterator<Item = usize>,
    provenance: rumoca_core::Span,
) -> solve::ScalarProgramBlock {
    let rows = indices
        .into_iter()
        .map(|index| {
            vec![
                solve::LinearOp::LoadP { dst: 0, index },
                solve::LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();
    solve::ScalarProgramBlock::with_source_span(
        rows,
        provenance
            .require_provenance("explicit direct-P visibility fixture")
            .expect("fixture visibility provenance is source-backed"),
    )
    .expect("fixture visibility rows are computable")
}

/// Construct an explicitly selected constant-state derivative program.
///
/// `explicit_count` is deliberately supplied by each fixture rather than
/// inferred from a layout. The production `SolveProblem` constructor remains
/// responsible for refusing any mismatch with the declared state cardinality.
pub(crate) fn zero_derivative_rhs(
    explicit_count: usize,
    provenance: rumoca_core::Span,
) -> solve::ComputeBlock {
    let rows = (0..explicit_count)
        .map(|_| {
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();
    let block = solve::ScalarProgramBlock::with_source_span(
        rows,
        provenance
            .require_provenance("explicit constant-state derivative fixture")
            .expect("fixture derivative provenance is source-backed"),
    )
    .expect("constant-state derivative rows are computable");
    solve::ComputeBlock::from_scalar_program_block(block)
}

fn real_scalar_catalog_entry(
    variable: RealScalarVariableFixture,
) -> solve::SolveVariableCatalogSourceEntry {
    (
        solve::SolveVariableSource::new(
            variable.source_occurrence,
            variable.name.clone(),
            Vec::new(),
            vec![variable.name],
            variable.provenance,
        ),
        solve::SolveVariableSourceAttributes::new(
            variable.causality,
            variable.variability,
            variable.variability == solve::SolveVariableVariability::Tunable,
            None,
            None,
            variable.fixed,
        ),
        solve::SolveVariableEvaluatedValues::new(
            Some(vec![variable.start]),
            None,
            None,
            variable.nominal.map(|nominal| vec![nominal]),
        ),
    )
}

macro_rules! checked_solve_model {
    (@collect ($($field:ident : $value:expr,)*) artifacts : $artifacts:expr, $($rest:tt)*) => {
        compile_error!(
            "checked_solve_model! derives artifacts; use custom_artifact_solve_model! to provide them"
        )
    };
    (@collect ($($field:ident : $value:expr,)*) $next_field:ident : $next_value:expr, $($rest:tt)*) => {
        $crate::test_support::checked_solve_model!(@collect (
            $($field : $value,)*
            $next_field : $next_value,
        ) $($rest)*)
    };
    (@collect ($($field:ident : $value:expr,)*) .. $base:expr $(,)?) => {{
        let ($($field,)*) = ($($value,)*);
        let mut fixture = $crate::test_support::SolveModelFixture::from_model($base);
        $(fixture.$field = $field;)*
        fixture.seal_with_derived_artifacts()
    }};
    ($($tokens:tt)*) => {
        $crate::test_support::checked_solve_model!(@collect () $($tokens)*)
    };
}

pub(crate) use checked_solve_model;

macro_rules! custom_artifact_solve_model {
    ($($field:ident : $value:expr,)* .. $base:expr $(,)?) => {{
        let ($($field,)*) = ($($value,)*);
        let mut fixture = $crate::test_support::SolveModelFixture::from_model($base);
        $(fixture.$field = $field;)*
        fixture.seal_with_custom_artifacts()
    }};
}

pub(crate) use custom_artifact_solve_model;

macro_rules! checked_solve_problem {
    (
        $layout:expr,
        $solve_layout:expr,
        crate::test_support::ContinuousSystemFixture::empty(),
        $initialization:expr,
        $discrete:expr,
        $events:expr,
        $clocks:expr $(,)?
    ) => {{
        let layout = $layout;
        let solve_layout = $solve_layout;
        let initialization = $initialization;
        let discrete = $discrete;
        let events = $events;
        let clocks = $clocks;
        let continuous = $crate::test_support::checked_empty_continuous_system(
            &solve_layout,
            &discrete,
            &events,
            &clocks,
        );
        solve::SolveProblem::construct(
            layout,
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
    }};
    (
        $layout:expr,
        $solve_layout:expr,
        $continuous:expr,
        $initialization:expr,
        $discrete:expr,
        $events:expr,
        $clocks:expr $(,)?
    ) => {
        solve::SolveProblem::construct(
            $layout,
            $solve_layout,
            $continuous,
            $initialization,
            $discrete,
            $events,
            $clocks,
        )
    };
}

pub(crate) use checked_solve_problem;

impl ContinuousSystemFixture {
    pub(crate) fn empty() -> Self {
        Self {
            implicit_rhs: solve::ComputeBlock::default(),
            implicit_row_targets: Vec::new(),
            algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
            residual: solve::ComputeBlock::default(),
            manifold_residual: solve::ComputeBlock::default(),
            manifold_projection_plan: solve::AlgebraicProjectionPlan::default(),
            derivative_rhs: solve::ComputeBlock::default(),
            refresh_plans: None,
        }
    }

    pub(crate) fn from_system(system: &solve::ContinuousSolveSystem) -> Self {
        Self {
            implicit_rhs: system.implicit_rhs().clone(),
            implicit_row_targets: system.implicit_row_targets().to_vec(),
            algebraic_projection_plan: system.algebraic_projection_plan().clone(),
            residual: system.residual().clone(),
            manifold_residual: system.manifold_residual().clone(),
            manifold_projection_plan: system.manifold_projection_plan().clone(),
            derivative_rhs: system.derivative_rhs().clone(),
            refresh_plans: None,
        }
    }

    pub(crate) fn try_refresh_plans(
        &mut self,
        solve_layout: &solve::SolveLayout,
        discrete: &solve::DiscreteSolveSystem,
        events: &solve::SolveEventPartition,
        clocks: &solve::SolveClockPartition,
    ) -> Result<solve::ContinuousRefreshPlanInputs, rumoca_eval_solve::EvalSolveError> {
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_plans(
            solve_layout,
            (
                &self.implicit_rhs,
                &self.implicit_row_targets,
                &mut self.algebraic_projection_plan,
                &self.derivative_rhs,
            ),
            discrete,
            events,
            clocks,
        )
    }

    pub(crate) fn try_seal(
        mut self,
        solve_layout: &solve::SolveLayout,
        discrete: &solve::DiscreteSolveSystem,
        events: &solve::SolveEventPartition,
        clocks: &solve::SolveClockPartition,
    ) -> Result<solve::ContinuousSolveSystem, Box<dyn std::error::Error>> {
        let refresh_plans = if let Some(refresh_plans) = self.refresh_plans.take() {
            refresh_plans
        } else {
            self.try_refresh_plans(solve_layout, discrete, events, clocks)
                .map_err(|error| Box::new(error) as Box<dyn std::error::Error>)?
        };
        solve::ContinuousSolveSystem::construct(
            solve_layout,
            solve::ContinuousSolveSystemInputs::new(
                self.implicit_rhs,
                self.implicit_row_targets,
                self.algebraic_projection_plan,
                self.residual,
                (self.manifold_residual, self.manifold_projection_plan),
                self.derivative_rhs,
                refresh_plans,
            ),
        )
        .map_err(|error| Box::new(error) as Box<dyn std::error::Error>)
    }

    pub(crate) fn seal(
        self,
        solve_layout: &solve::SolveLayout,
        discrete: &solve::DiscreteSolveSystem,
        events: &solve::SolveEventPartition,
        clocks: &solve::SolveClockPartition,
    ) -> solve::ContinuousSolveSystem {
        self.try_seal(solve_layout, discrete, events, clocks)
            .expect("fixture refresh owners match their canonical continuous programs")
    }
}

pub(crate) fn arithmetic_profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    )
}

pub(crate) fn checked_empty_continuous_system(
    solve_layout: &solve::SolveLayout,
    discrete: &solve::DiscreteSolveSystem,
    events: &solve::SolveEventPartition,
    clocks: &solve::SolveClockPartition,
) -> solve::ContinuousSolveSystem {
    ContinuousSystemFixture::empty().seal(solve_layout, discrete, events, clocks)
}

pub(crate) fn checked_empty_problem() -> solve::SolveProblem {
    let solve_layout = solve::SolveLayout::default();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = checked_empty_continuous_system(&solve_layout, &discrete, &events, &clocks);
    solve::SolveProblem::construct(
        solve::VarLayout::from_parts(IndexMap::new(), 0, 0),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        discrete,
        events,
        clocks,
    )
    .expect("empty fixture satisfies the checked SolveProblem contract")
}

pub(crate) fn seal_model(
    problem: solve::SolveProblem,
    pure_calls: solve::SolvePureCallTable,
    artifacts: solve::SolveArtifactInputs,
    runtime: solve::SolveModelRuntimeInputs,
    visible_value_rows: solve::ScalarProgramBlock,
    variable_entries: Vec<solve::SolveVariableCatalogSourceEntry>,
) -> solve::SolveModel {
    solve::SolveModel::construct(
        problem,
        pure_calls,
        artifacts,
        runtime,
        visible_value_rows,
        variable_entries,
    )
    .expect("fixture satisfies the checked SolveModel contract")
}

pub(crate) fn empty_binary64_first_product_model() -> solve::SolveModel {
    seal_model(
        checked_empty_problem(),
        solve::SolvePureCallTable::empty(arithmetic_profile()),
        solve::SolveArtifactInputs::empty(),
        solve::SolveModelRuntimeInputs {
            initial_y: Vec::new(),
            solver_nominals: Vec::new(),
            parameters: Vec::new(),
        },
        solve::ScalarProgramBlock::default(),
        Vec::new(),
    )
}

pub(crate) fn fmi_component(model: solve::SolveModel) -> solve::fmi::FmiComponent {
    solve::fmi::FmiComponent::construct(model)
        .expect("fixture satisfies the checked FMI component contract")
}

pub(crate) fn catalog_entries(
    model: &solve::SolveModel,
) -> Vec<solve::SolveVariableCatalogSourceEntry> {
    model
        .variable_catalog()
        .entries()
        .iter()
        .map(|entry| {
            (
                solve::SolveVariableSource::new(
                    entry.source_occurrence(),
                    entry.name().to_string(),
                    entry.dimensions().to_vec(),
                    entry.scalar_names().to_vec(),
                    entry.provenance(),
                ),
                solve::SolveVariableSourceAttributes::new(
                    entry.causality(),
                    entry.variability(),
                    entry.is_tunable(),
                    entry.unit().map(str::to_string),
                    entry.description().map(str::to_string),
                    entry.fixed(),
                ),
                solve::SolveVariableEvaluatedValues::new(
                    entry.start().map(<[f64]>::to_vec),
                    entry.minimum().map(<[f64]>::to_vec),
                    entry.maximum().map(<[f64]>::to_vec),
                    entry.nominal().map(<[f64]>::to_vec),
                ),
            )
        })
        .collect()
}
