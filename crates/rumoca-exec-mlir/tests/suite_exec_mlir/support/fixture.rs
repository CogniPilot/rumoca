use std::sync::Arc;

use indexmap::IndexMap;
use rumoca_core::{InstanceId, SourceId, SourceOccurrenceId, Span};
use rumoca_ir_solve as solve;

const FIXTURE_SOURCE: &str = "exec_mlir_explicit_ode_fixture.mo";

struct FixtureVariable {
    source_occurrence: SourceOccurrenceId,
    name: String,
    role: FixtureRole,
    storage_index: usize,
    provenance: Span,
}

#[derive(Clone, Copy)]
enum FixtureRole {
    State { start: f64, nominal: f64 },
    Algebraic { start: f64, nominal: f64 },
    Parameter { value: f64 },
}

impl FixtureRole {
    const fn storage_role(self) -> solve::SolveVariableStorageRole {
        match self {
            Self::State { .. } => solve::SolveVariableStorageRole::State,
            Self::Algebraic { .. } => solve::SolveVariableStorageRole::Algebraic,
            Self::Parameter { .. } => solve::SolveVariableStorageRole::Parameter,
        }
    }

    const fn inventory_order(self) -> u8 {
        match self {
            Self::State { .. } => 0,
            Self::Algebraic { .. } => 1,
            Self::Parameter { .. } => 2,
        }
    }
}

pub(crate) struct FixtureScalar {
    name: String,
    role: FixtureRole,
}

impl FixtureScalar {
    pub(crate) fn state(name: &str, start: f64, nominal: f64) -> Self {
        Self {
            name: name.to_string(),
            role: FixtureRole::State { start, nominal },
        }
    }

    pub(crate) fn algebraic(name: &str, start: f64, nominal: f64) -> Self {
        Self {
            name: name.to_string(),
            role: FixtureRole::Algebraic { start, nominal },
        }
    }

    pub(crate) fn parameter(name: &str, value: f64) -> Self {
        Self {
            name: name.to_string(),
            role: FixtureRole::Parameter { value },
        }
    }
}

struct InventoryConstruction {
    bindings: IndexMap<String, solve::ScalarSlot>,
    variables: Vec<FixtureVariable>,
    storage_runs: Vec<solve::SolveVariableStorageRun>,
    declarations: Vec<solve::SolveVariableDeclaration>,
}

impl InventoryConstruction {
    fn new() -> Self {
        Self {
            bindings: IndexMap::new(),
            variables: Vec::new(),
            storage_runs: Vec::new(),
            declarations: Vec::new(),
        }
    }

    fn push(&mut self, scalar: FixtureScalar, slot: solve::ScalarSlot, storage_index: usize) {
        let FixtureScalar { name, role } = scalar;
        assert!(!name.is_empty(), "fixture variable name is nonempty");
        assert!(
            self.bindings.insert(name.clone(), slot).is_none(),
            "fixture variable names are unique"
        );
        let ordinal = self.variables.len();
        let source_occurrence = SourceOccurrenceId::try_from(InstanceId::new(
            u32::try_from(ordinal + 1).expect("fixture occurrence ordinal fits u32"),
        ))
        .expect("fixture occurrence ordinal is one-based and nonzero");
        let provenance = Span::from_offsets(
            SourceId::from_source_name(FIXTURE_SOURCE),
            ordinal,
            ordinal + 1,
        );
        self.variables.push(FixtureVariable {
            source_occurrence,
            name,
            role,
            storage_index,
            provenance,
        });
        self.storage_runs.push(solve::SolveVariableStorageRun {
            base: slot
                .storage_coordinate()
                .expect("fixture uses writable storage"),
            scalar_count: 1,
            role: role.storage_role(),
            value_kind: solve::SolveVariableValueKind::Real,
        });
        self.declarations.push(solve::SolveVariableDeclaration::new(
            role.storage_role(),
            solve::SolveVariableValueKind::Real,
        ));
    }
}

/// One explicitly named scalar continuous inventory. It constructs the Var/Solve
/// layouts together so no count, name map, declaration, or storage run can be
/// supplied independently or repaired from absence.
pub(crate) struct ContinuousInventory {
    layout: solve::VarLayout,
    solve_layout: solve::SolveLayout,
    variables: Vec<FixtureVariable>,
}

impl ContinuousInventory {
    pub(crate) fn new(scalars: Vec<FixtureScalar>) -> Self {
        assert!(
            scalars.windows(2).all(|pair| matches!(
                pair,
                [left, right]
                    if left.role.inventory_order() <= right.role.inventory_order()
            )),
            "fixture scalars are authored in state, algebraic, parameter order"
        );
        let mut construction = InventoryConstruction::new();
        let state_count = scalars
            .iter()
            .filter(|scalar| matches!(scalar.role, FixtureRole::State { .. }))
            .count();
        let algebraic_count = scalars
            .iter()
            .filter(|scalar| matches!(scalar.role, FixtureRole::Algebraic { .. }))
            .count();
        let parameter_count = scalars
            .iter()
            .filter(|scalar| matches!(scalar.role, FixtureRole::Parameter { .. }))
            .count();
        let y_names = scalars
            .iter()
            .filter(|scalar| !matches!(scalar.role, FixtureRole::Parameter { .. }))
            .map(|scalar| scalar.name.clone())
            .collect::<Vec<_>>();
        let parameter_names = scalars
            .iter()
            .filter(|scalar| matches!(scalar.role, FixtureRole::Parameter { .. }))
            .map(|scalar| scalar.name.clone())
            .collect::<Vec<_>>();
        let mut next_state = 0;
        let mut next_algebraic = 0;
        let mut next_parameter = 0;
        for scalar in scalars {
            let (slot, storage_index) = match scalar.role {
                FixtureRole::State { .. } => {
                    let index = next_state;
                    next_state += 1;
                    (solve::scalar_slot_y(index), index)
                }
                FixtureRole::Algebraic { .. } => {
                    let index = state_count + next_algebraic;
                    next_algebraic += 1;
                    (solve::scalar_slot_y(index), index)
                }
                FixtureRole::Parameter { .. } => {
                    let index = next_parameter;
                    next_parameter += 1;
                    (solve::scalar_slot_p(index), index)
                }
            };
            construction.push(scalar, slot, storage_index);
        }
        let solver_maps = solve::SolverNameIndexMaps {
            name_to_idx: y_names
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, name)| (name, index))
                .collect(),
            base_to_indices: y_names
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, name)| (name, vec![index]))
                .collect(),
            names: y_names,
        };
        let y_scalars = state_count + algebraic_count;
        let p_scalars = parameter_count;
        let layout = solve::VarLayout::from_parts(construction.bindings, y_scalars, p_scalars);
        let solve_layout = solve::SolveLayout {
            solver_maps,
            variable_storage_runs: construction.storage_runs,
            variable_declarations: construction.declarations,
            state_scalar_count: state_count,
            algebraic_scalar_count: algebraic_count,
            output_scalar_count: 0,
            parameter_count,
            static_parameter_names: parameter_names,
            compiled_parameter_len: p_scalars,
            input_scalar_names: Vec::new(),
            discrete_real_scalar_names: Vec::new(),
            discrete_valued_scalar_names: Vec::new(),
            relation_memory_parameter_indices: Vec::new(),
            initial_event_parameter_index: None,
            terminal_event_parameter_index: None,
            initial_homotopy_parameter_index: None,
            pre_param_bindings: Vec::new(),
        };
        Self {
            layout,
            solve_layout,
            variables: construction.variables,
        }
    }

    pub(crate) const fn solve_layout(&self) -> &solve::SolveLayout {
        &self.solve_layout
    }

    pub(crate) fn seal(
        self,
        continuous: solve::ContinuousSolveSystem,
        initialization: solve::InitializationSolveSystem,
        discrete: solve::DiscreteSolveSystem,
        events: solve::SolveEventPartition,
        clocks: solve::SolveClockPartition,
    ) -> ContinuousProblem {
        let problem = solve::SolveProblem::construct(
            self.layout,
            self.solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("MLIR continuous fixture is valid by construction");
        ContinuousProblem {
            problem,
            variables: self.variables,
        }
    }
}

pub(crate) struct ContinuousProblem {
    problem: solve::SolveProblem,
    variables: Vec<FixtureVariable>,
}

/// Complete one explicitly declared Binary64 continuous fixture through the real
/// artifact and SolveModel constructors.
pub(crate) fn complete_model(fixture: ContinuousProblem) -> Arc<solve::SolveModel> {
    let initial_y = fixture
        .variables
        .iter()
        .filter_map(|variable| match variable.role {
            FixtureRole::State { start, .. } | FixtureRole::Algebraic { start, .. } => Some(start),
            FixtureRole::Parameter { .. } => None,
        })
        .collect();
    let solver_nominals = fixture
        .variables
        .iter()
        .filter_map(|variable| match variable.role {
            FixtureRole::State { nominal, .. } | FixtureRole::Algebraic { nominal, .. } => {
                Some(nominal)
            }
            FixtureRole::Parameter { .. } => None,
        })
        .collect();
    let parameters = fixture
        .variables
        .iter()
        .filter_map(|variable| match variable.role {
            FixtureRole::Parameter { value } => Some(value),
            FixtureRole::State { .. } | FixtureRole::Algebraic { .. } => None,
        })
        .collect();
    let variable_entries = fixture
        .variables
        .iter()
        .map(catalog_entry)
        .collect::<Vec<_>>();
    let visible_value_rows = visible_value_rows(&fixture.variables);
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let artifacts = rumoca_phase_solve::lower_solve_artifacts(&fixture.problem)
        .expect("test Solve artifacts lower");
    let model = solve::SolveModel::construct(
        fixture.problem,
        solve::SolvePureCallTable::empty(arithmetic),
        artifacts,
        solve::SolveModelRuntimeInputs {
            initial_y,
            solver_nominals,
            parameters,
        },
        visible_value_rows,
        variable_entries,
    )
    .expect("complete MLIR fixture is a checked Solve model");
    Arc::new(model)
}

fn catalog_entry(variable: &FixtureVariable) -> solve::SolveVariableCatalogSourceEntry {
    let (causality, variability, fixed, start, nominal) = match variable.role {
        FixtureRole::State { start, nominal } => (
            solve::SolveVariableCausality::Local,
            solve::SolveVariableVariability::Continuous,
            rumoca_core::Fixity::Free,
            start,
            Some(vec![nominal]),
        ),
        FixtureRole::Algebraic { start, nominal } => (
            solve::SolveVariableCausality::Local,
            solve::SolveVariableVariability::Continuous,
            rumoca_core::Fixity::Free,
            start,
            Some(vec![nominal]),
        ),
        FixtureRole::Parameter { value } => (
            solve::SolveVariableCausality::Parameter,
            solve::SolveVariableVariability::Fixed,
            rumoca_core::Fixity::Fixed,
            value,
            None,
        ),
    };
    (
        solve::SolveVariableSource::new(
            variable.source_occurrence,
            variable.name.clone(),
            Vec::new(),
            vec![variable.name.clone()],
            variable.provenance,
        ),
        solve::SolveVariableSourceAttributes::new(causality, variability, false, None, None, fixed),
        solve::SolveVariableEvaluatedValues::new(Some(vec![start]), None, None, nominal),
    )
}

fn visible_value_rows(variables: &[FixtureVariable]) -> solve::ScalarProgramBlock {
    let rows = variables
        .iter()
        .filter_map(|variable| match variable.role {
            FixtureRole::State { .. } | FixtureRole::Algebraic { .. } => Some(vec![
                solve::LinearOp::LoadY {
                    dst: 0,
                    index: variable.storage_index,
                },
                solve::LinearOp::StoreOutput { src: 0 },
            ]),
            FixtureRole::Parameter { .. } => None,
        })
        .collect();
    let provenance = Span::from_offsets(SourceId::from_source_name(FIXTURE_SOURCE), 0, 1)
        .require_provenance("MLIR visible-value fixture")
        .expect("fixture span is source-backed");
    solve::ScalarProgramBlock::with_source_span(rows, provenance)
        .expect("visible fixture rows are checked")
}
