use super::*;
use crate::fmi::native_deployment::{
    FmiCarryPair, FmiNativeDeploymentView, NativeCarryError, NativeDeploymentError,
};
use crate::{
    AlgebraicProjectionBlock, ComputeBlock, ComputeNode, ImplicitRowTargets, LinearOp,
    ProjectionBlocks, ScalarProgramBlock, SolveInitialValues, SolveVariableDeclaration,
    SolveVariableStorageRole as Role, SolveVariableStorageRun, SolveVariableValueKind as Kind,
    VarLayout, scalar_slot_p, scalar_slot_y,
};

fn span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_native_carry.mo"),
        0,
        1,
    )
}

fn variable(name: &str, role: Role, causality: FmiCausality) -> FmiVariableInput {
    FmiVariableInput {
        name: name.to_string(),
        role,
        value_kind: Kind::Real,
        dimensions: Vec::new(),
        start: vec![0.0].into(),
        effective_input_default: None,
        minimum: None,
        maximum: None,
        nominal: None,
        unit: None,
        description: None,
        causality,
        declared_causality: None,
        variability: FmiVariability::Continuous,
        tunable: false,
        evaluable: false,
        declaration: span(),
        text_start: None,
        enumeration: None,
    }
}

/// `next := previous`: one Real output computed from one Real external input.
fn stepper() -> (SolveModel, Vec<FmiVariableInput>) {
    let mut model = SolveModel::default();
    let bindings = [
        ("next".to_string(), scalar_slot_y(0)),
        ("previous".to_string(), scalar_slot_p(0)),
    ];
    model.problem.layout = VarLayout::from_parts(bindings.into_iter().collect(), 1, 1);
    model.initial_y = SolveInitialValues::repeat(0.0, 1).unwrap();
    model.parameters = SolveInitialValues::repeat(0.0, 1).unwrap();
    model.problem.solve_layout.variable_storage_runs = vec![
        SolveVariableStorageRun {
            base: scalar_slot_y(0),
            scalar_count: 1,
            role: Role::Output,
            value_kind: Kind::Real,
        },
        SolveVariableStorageRun {
            base: scalar_slot_p(0),
            scalar_count: 1,
            role: Role::ExternalInput,
            value_kind: Kind::Real,
        },
    ];
    model.problem.solve_layout.variable_declarations = vec![
        SolveVariableDeclaration::new(Role::Output, Kind::Real),
        SolveVariableDeclaration::new(Role::ExternalInput, Kind::Real),
    ];
    model.problem.solve_layout.output_scalar_count = 1;
    model.problem.solve_layout.parameter_count = 1;
    let rows = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: crate::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        span().require_provenance("native carry fixture").unwrap(),
    )
    .unwrap();
    model.problem.continuous.implicit_rhs = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(rows)],
    };
    let blocks = ProjectionBlocks::from(vec![AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        ..Default::default()
    }]);
    model.problem.continuous.implicit_row_targets = ImplicitRowTargets::from_projection(&blocks);
    model.problem.continuous.algebraic_projection_plan.blocks = blocks;
    let inputs = vec![
        variable("next", Role::Output, FmiCausality::Output),
        variable("previous", Role::ExternalInput, FmiCausality::Input),
    ];
    (model, inputs)
}

fn view_of(
    model: SolveModel,
    inputs: Vec<FmiVariableInput>,
) -> (FmiNativeDeploymentView, crate::RetainedNativePreparation) {
    let component = FmiComponent::construct(model, inputs).unwrap();
    let preparation = component
        .runtime_view()
        .retain_assertion_root()
        .native_preparation()
        .unwrap();
    let view = FmiNativeDeploymentView::from_preparation(&preparation).unwrap();
    (view, preparation)
}

fn reference(view: &FmiNativeDeploymentView, name: &str) -> u32 {
    view.variables()
        .iter()
        .find(|variable| variable.name() == name)
        .unwrap()
        .value_reference_fmi3()
}

#[test]
fn a_native_view_exposes_the_deployment_it_admitted_and_the_stored_instantiation_values() {
    let (model, inputs) = stepper();
    let (view, _) = view_of(model, inputs);
    assert_eq!(
        view.capabilities().interfaces,
        FmiInterfaceProfile::CoSimulation
    );
    assert!(view.carried().is_empty());
    assert_eq!(
        view.instantiation_values(FmiInstantiationBuffer::Solver)
            .len(),
        1
    );
    assert_eq!(
        view.instantiation_values(FmiInstantiationBuffer::Parameters)
            .len(),
        1
    );
}

#[test]
fn a_declared_carry_pairs_the_output_with_the_input_that_holds_its_previous_value() {
    let (model, inputs) = stepper();
    let (view, preparation) = view_of(model, inputs);
    let pair = FmiCarryPair {
        output: reference(&view, "next"),
        input: reference(&view, "previous"),
    };
    let (view, state) = view
        .declaring_carried(preparation.schedule(), &[pair])
        .unwrap();
    assert_eq!(view.carried(), [pair]);
    assert_eq!(state.runs().len(), 1);
}

#[test]
fn a_carry_naming_an_unknown_reversed_or_unpaired_variable_is_refused() {
    let (model, inputs) = stepper();
    let (view, preparation) = view_of(model, inputs);
    let (next, previous) = (reference(&view, "next"), reference(&view, "previous"));
    let refusal = |pair: FmiCarryPair| match view
        .clone()
        .declaring_carried(preparation.schedule(), &[pair])
    {
        Err(NativeDeploymentError::Carry(error)) => error,
        other => panic!("expected a carry refusal, found {other:?}"),
    };
    assert_eq!(
        refusal(FmiCarryPair {
            output: 9_999,
            input: previous
        }),
        NativeCarryError::UnknownReference(9_999)
    );
    assert_eq!(
        refusal(FmiCarryPair {
            output: previous,
            input: previous
        }),
        NativeCarryError::NotOutput(previous)
    );
    assert_eq!(
        refusal(FmiCarryPair {
            output: next,
            input: next
        }),
        NativeCarryError::NotInput(next)
    );
}
