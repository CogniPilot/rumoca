//! Small complete assignment source with original Real/Integer/Boolean declarations.

use indexmap::IndexMap;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve as solve;
use rumoca_ir_solve::SolveVariableValueKind as Kind;
use solve::SolveVariableStorageRole as Role;
use solve::fmi::{FmiCausality, FmiVariability, FmiVariableInput};

pub(super) fn component() -> solve::fmi::FmiComponent {
    with_boolean_start(solve::SolveDeclarationStart::Boolean(exact_repeat(true)))
}

pub(super) fn missing_boolean_component() -> solve::fmi::FmiComponent {
    with_boolean_start(solve::SolveDeclarationStart::Missing {
        value_kind: Kind::Boolean,
        count: 1,
    })
}

pub(super) fn reset_component() -> solve::fmi::FmiComponent {
    with_starts(
        i64::MAX,
        solve::SolveDeclarationStart::Boolean(exact_repeat(false)),
    )
}

pub(super) fn missing_real_component() -> solve::fmi::FmiComponent {
    with_declaration_starts(
        i64::MAX,
        solve::SolveDeclarationStart::Boolean(exact_repeat(false)),
        solve::SolveDeclarationStart::Missing {
            value_kind: Kind::Real,
            count: 1,
        },
    )
}

fn with_boolean_start(boolean_start: solve::SolveDeclarationStart) -> solve::fmi::FmiComponent {
    with_starts(9_007_199_254_740_993, boolean_start)
}

fn with_starts(
    integer: i64,
    boolean_start: solve::SolveDeclarationStart,
) -> solve::fmi::FmiComponent {
    with_declaration_starts(
        integer,
        boolean_start,
        solve::SolveDeclarationStart::Real(vec![2.0].into()),
    )
}

fn with_declaration_starts(
    integer: i64,
    boolean_start: solve::SolveDeclarationStart,
    real_start: solve::SolveDeclarationStart,
) -> solve::fmi::FmiComponent {
    build(integer, boolean_start, real_start, false, false)
}

/// The fixture plus an Integer parameter the program reads (VR 5), a Boolean
/// parameter (6), a Real parameter (7) and an Integer constant (8).
pub(super) fn parameter_component() -> solve::fmi::FmiComponent {
    build(
        1,
        solve::SolveDeclarationStart::Boolean(exact_repeat(false)),
        solve::SolveDeclarationStart::Real(vec![2.0].into()),
        true,
        false,
    )
}

/// The fixture plus a discrete Real `stepped` (VR 5) that the stateless native
/// schedule derives as `1.5 * real` and publishes through a Real output lane.
pub(super) fn discrete_real_component() -> solve::fmi::FmiComponent {
    build(
        i64::MAX,
        solve::SolveDeclarationStart::Boolean(exact_repeat(false)),
        solve::SolveDeclarationStart::Real(vec![2.0].into()),
        false,
        true,
    )
}

fn layout_names(parameters: bool, discrete_real: bool) -> IndexMap<String, solve::ScalarSlot> {
    let mut names = IndexMap::from([
        ("result".into(), solve::scalar_slot_y(0)),
        ("real".into(), solve::scalar_slot_p(0)),
        ("integer".into(), solve::scalar_slot_p(1)),
        ("enabled".into(), solve::scalar_slot_p(2)),
    ]);
    if parameters {
        names.extend([
            ("limit".into(), solve::scalar_slot_p(3)),
            ("flag".into(), solve::scalar_slot_p(4)),
            ("gain".into(), solve::scalar_slot_p(5)),
            ("count".into(), solve::scalar_slot_p(6)),
        ]);
    }
    if discrete_real {
        names.insert("stepped".into(), solve::scalar_slot_p(3));
    }
    names
}

fn descriptors(parameters: bool, discrete_real: bool) -> Vec<(Role, Kind, solve::ScalarSlot)> {
    let mut descriptors = vec![
        (Role::Output, Kind::Real, solve::scalar_slot_y(0)),
        (Role::ExternalInput, Kind::Real, solve::scalar_slot_p(0)),
        (Role::ExternalInput, Kind::Integer, solve::scalar_slot_p(1)),
        (Role::ExternalInput, Kind::Boolean, solve::scalar_slot_p(2)),
    ];
    if parameters {
        descriptors.extend([
            (Role::Parameter, Kind::Integer, solve::scalar_slot_p(3)),
            (Role::Parameter, Kind::Boolean, solve::scalar_slot_p(4)),
            (Role::Parameter, Kind::Real, solve::scalar_slot_p(5)),
            (Role::Constant, Kind::Integer, solve::scalar_slot_p(6)),
        ]);
    }
    if discrete_real {
        descriptors.push((Role::DiscreteReal, Kind::Real, solve::scalar_slot_p(3)));
    }
    descriptors
}

fn build(
    integer: i64,
    boolean_start: solve::SolveDeclarationStart,
    real_start: solve::SolveDeclarationStart,
    parameters: bool,
    discrete_real: bool,
) -> solve::fmi::FmiComponent {
    let missing_real = matches!(&real_start, solve::SolveDeclarationStart::Missing { .. });
    let span = Span::from_offsets(SourceId::from_source_name("NativeAdoption.mo"), 1, 2);
    let mut model = solve::SolveModel::default();
    let p_count = match (parameters, discrete_real) {
        (true, _) => 7,
        (false, true) => 5,
        (false, false) => 3,
    };
    model.problem.layout =
        solve::VarLayout::from_parts(layout_names(parameters, discrete_real), 1, p_count);
    let descriptors = descriptors(parameters, discrete_real);
    model.problem.solve_layout = solve::SolveLayout {
        output_scalar_count: 1,
        parameter_count: p_count,
        compiled_parameter_len: p_count,

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
    if missing_real {
        model.problem.solve_layout.parameter_count = 0;
        model.problem.solve_layout.input_scalar_names = vec![
            "real".to_string(),
            "integer".to_string(),
            "enabled".to_string(),
        ]
        .into();
    }
    model.problem.continuous.implicit_rhs = assignment_source(span, parameters);
    if discrete_real {
        model.problem.discrete = stepped_system(span);
        model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
            dest_p_index: 4,
            source: solve::PreParamSource::P { index: 3 },
            clock_schedule: None,
        }];
    }
    let solve::ComputeNode::ScalarPrograms(program) =
        &model.problem.continuous.implicit_rhs.nodes[0]
    else {
        unreachable!()
    };
    let output = program
        .program_output_span(0, if parameters { 7 } else { 5 })
        .expect("the actual ranged store owns its projection");
    assert_eq!(
        (output.start(), output.count(), output.descending()),
        (0, 1, false)
    );

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
    model.parameters = initial_parameters(parameters, discrete_real).into();
    let mut inputs = variables(span, integer, boolean_start, real_start);
    if parameters {
        inputs.extend(parameter_variables(span));
    }
    if discrete_real {
        inputs.push(variable(
            "stepped",
            Role::DiscreteReal,
            solve::SolveDeclarationStart::Real(vec![0.0].into()),
            span,
        ));
    }
    solve::fmi::FmiComponent::construct(model, inputs).unwrap()
}

fn assignment_source(span: Span, parameters: bool) -> solve::ComputeBlock {
    use solve::LinearOp;
    let mut ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Binary {
            op: solve::BinaryOp::Mul,
            dst: 3,
            lhs: 1,
            rhs: 2,
        },
    ];
    let mut subtrahend = 3;
    if parameters {
        // An untyped Real read of the Integer parameter, so its lane is checked.
        ops.extend([
            LinearOp::LoadP { dst: 5, index: 3 },
            LinearOp::Binary {
                op: solve::BinaryOp::Add,
                dst: 6,
                lhs: 3,
                rhs: 5,
            },
        ]);
        subtrahend = 6;
    }
    ops.extend([
        LinearOp::Binary {
            op: solve::BinaryOp::Sub,
            dst: 4,
            lhs: 0,
            rhs: subtrahend,
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 1,
            stride: 1,
        },
    ]);
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(vec![ops], vec![span]).unwrap(),
    )
}

/// One unclocked B.1c equation `stepped = 1.5 * real` for the discrete Real at P3.
fn stepped_system(span: Span) -> solve::DiscreteSolveSystem {
    use solve::LinearOp;
    let rhs = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 1.5 },
            LinearOp::Binary {
                op: solve::BinaryOp::Mul,
                dst: 2,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        span.require_provenance("native discrete fixture").unwrap(),
    )
    .unwrap();
    solve::DiscreteSolveSystem {
        event_iteration_plan: solve::EventIterationPlan::new(vec![solve::EventIterationRun {
            variable: 4,
            pre_binding_start: 0,
            owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
        }]),
        rhs,
        update_targets: vec![solve::scalar_slot_p(3)],
        row_roles: vec![solve::DiscreteRowRole::Equation],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
        observation_refresh: vec![false],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
        clock_owners: vec![None],
        ..Default::default()
    }
}

fn parameter_variables(span: Span) -> Vec<FmiVariableInput> {
    vec![
        variable(
            "limit",
            Role::Parameter,
            solve::SolveDeclarationStart::Integer(exact_repeat(5)),
            span,
        ),
        variable(
            "flag",
            Role::Parameter,
            solve::SolveDeclarationStart::Boolean(exact_repeat(true)),
            span,
        ),
        variable(
            "gain",
            Role::Parameter,
            solve::SolveDeclarationStart::Real(vec![1.5].into()),
            span,
        ),
        variable(
            "count",
            Role::Constant,
            solve::SolveDeclarationStart::Integer(exact_repeat(7)),
            span,
        ),
    ]
}

fn variables(
    span: Span,
    integer: i64,
    boolean_start: solve::SolveDeclarationStart,
    real_start: solve::SolveDeclarationStart,
) -> Vec<FmiVariableInput> {
    vec![
        variable(
            "result",
            Role::Output,
            solve::SolveDeclarationStart::Real(vec![0.0].into()),
            span,
        ),
        variable("real", Role::ExternalInput, real_start, span),
        variable(
            "integer",
            Role::ExternalInput,
            solve::SolveDeclarationStart::Integer(exact_repeat(integer)),
            span,
        ),
        variable("enabled", Role::ExternalInput, boolean_start, span),
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
            Role::Parameter => FmiCausality::Parameter,
            Role::Constant => FmiCausality::Local,
            _ => FmiCausality::Output,
        },
        declared_causality: None,
        variability: match role {
            Role::ExternalInput => FmiVariability::Discrete,
            Role::DiscreteReal => FmiVariability::Discrete,
            Role::Parameter => FmiVariability::Fixed,
            Role::Constant => FmiVariability::Constant,
            _ => FmiVariability::Continuous,
        },
        tunable: false,
        evaluable: false,
        declaration: span,
        text_start: None,
        enumeration: None,
    }
}

fn initial_parameters(parameters: bool, discrete_real: bool) -> Vec<f64> {
    if parameters {
        vec![2.0, 0.0, 0.0, 5.0, 1.0, 1.5, 7.0]
    } else if discrete_real {
        vec![2.0, 0.0, 0.0, 0.0, 0.0]
    } else {
        vec![2.0, 0.0, 0.0]
    }
}
