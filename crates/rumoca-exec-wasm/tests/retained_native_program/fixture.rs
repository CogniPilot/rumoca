//! One complete assignment source with Real, Integer and Boolean external inputs.

use indexmap::IndexMap;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as solve;
use rumoca_ir_solve::SolveVariableValueKind as Kind;
use solve::SolveVariableStorageRole as Role;
use solve::fmi::{FmiCausality, FmiVariability, FmiVariableInput};

/// P cell of the Real, Integer and Boolean external inputs.
pub(crate) const REAL: usize = 0;
pub(crate) const INTEGER: usize = 1;
pub(crate) const BOOLEAN: usize = 2;

pub(crate) fn component() -> solve::fmi::FmiComponent {
    let span = Span::from_offsets(SourceId::from_source_name("RetainedNative.mo"), 1, 2);
    let layout = IndexMap::from([
        ("result".into(), solve::scalar_slot_y(0)),
        ("real".into(), solve::scalar_slot_p(REAL)),
        ("integer".into(), solve::scalar_slot_p(INTEGER)),
        ("enabled".into(), solve::scalar_slot_p(BOOLEAN)),
    ]);
    let descriptors = [
        (Role::Output, Kind::Real, solve::scalar_slot_y(0)),
        (Role::ExternalInput, Kind::Real, solve::scalar_slot_p(REAL)),
        (
            Role::ExternalInput,
            Kind::Integer,
            solve::scalar_slot_p(INTEGER),
        ),
        (
            Role::ExternalInput,
            Kind::Boolean,
            solve::scalar_slot_p(BOOLEAN),
        ),
    ];
    let mut model = solve::SolveModel::default();
    model.problem.layout = solve::VarLayout::from_parts(layout, 1, 3);
    model.problem.solve_layout = solve::SolveLayout {
        output_scalar_count: 1,
        parameter_count: 3,
        compiled_parameter_len: 3,
        variable_storage_runs: descriptors
            .iter()
            .map(|&(role, value_kind, base)| solve::SolveVariableStorageRun {
                base,
                scalar_count: 1,
                role,
                value_kind,
            })
            .collect(),
        variable_declarations: descriptors
            .iter()
            .map(|&(role, kind, _)| solve::SolveVariableDeclaration::new(role, kind))
            .collect(),
        ..Default::default()
    };
    model.problem.continuous.implicit_rhs = assignment_source(span);
    let blocks = solve::ProjectionBlocks::from_stages(vec![
        solve::ProjectionStage::SingletonDeclarationRun {
            source: solve::RefreshScalarProgramSource::checked(0, 0).unwrap(),
            first_row: 0,
            declaration: 0,
            storage: model.problem.solve_layout.variable_storage_runs[0],
        },
    ])
    .unwrap();
    model.problem.continuous.implicit_row_targets =
        solve::ImplicitRowTargets::from_projection(&blocks);
    model.problem.continuous.algebraic_projection_plan.blocks = blocks;
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .unwrap();
    let (continuous, initialization) =
        rumoca_eval_solve::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
            .unwrap();
    model.artifacts.continuous.structural = continuous;
    model.artifacts.initialization.structural = initialization;
    model.initial_y = vec![0.0].into();
    model.parameters = vec![2.0, 1.0, 1.0].into();
    solve::fmi::FmiComponent::construct(model, variables(span)).unwrap()
}

fn assignment_source(span: Span) -> solve::ComputeBlock {
    use solve::LinearOp;
    let ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Binary {
            op: solve::BinaryOp::Mul,
            dst: 3,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            op: solve::BinaryOp::Sub,
            dst: 4,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 1,
            stride: 1,
        },
    ];
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span]).unwrap(),
    )
}

fn variables(span: Span) -> Vec<FmiVariableInput> {
    vec![
        variable(
            "result",
            Role::Output,
            solve::SolveDeclarationStart::Real(vec![0.0].into()),
            span,
        ),
        variable(
            "real",
            Role::ExternalInput,
            solve::SolveDeclarationStart::Real(vec![2.0].into()),
            span,
        ),
        variable(
            "integer",
            Role::ExternalInput,
            solve::SolveDeclarationStart::Integer(exact_repeat(1)),
            span,
        ),
        variable(
            "enabled",
            Role::ExternalInput,
            solve::SolveDeclarationStart::Boolean(exact_repeat(true)),
            span,
        ),
    ]
}

fn exact_repeat<T>(value: T) -> solve::SolveExactInitialValues<T> {
    solve::SolveExactInitialValues::construct(
        1,
        vec![solve::SolveExactInitialRun::Repeat {
            start: 0,
            value,
            count: 1,
        }],
    )
    .unwrap()
}

fn variable(
    name: &str,
    role: Role,
    start: solve::SolveDeclarationStart,
    span: Span,
) -> FmiVariableInput {
    FmiVariableInput {
        name: name.into(),
        role,
        value_kind: start.value_kind(),
        dimensions: vec![],
        start,
        effective_input_default: None,
        minimum: None,
        maximum: None,
        nominal: None,
        unit: None,
        description: None,
        causality: match role {
            Role::ExternalInput => FmiCausality::Input,
            _ => FmiCausality::Output,
        },
        declared_causality: None,
        variability: match role {
            Role::ExternalInput => FmiVariability::Discrete,
            _ => FmiVariability::Continuous,
        },
        tunable: false,
        evaluable: false,
        declaration: span,
        text_start: None,
        enumeration: None,
    }
}
