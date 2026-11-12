//! Source-to-inventory test of the FMI articulated-reduction accessors.
//!
//! One issued C112 owner, one implicit program calling it, fifty canonical
//! source-checked refresh rows, and the enclosing `checked_metadata` inventory
//! feed the production `crate::fmi::articulated_bindings::derive`. This is an
//! internal accessor test, not an `FmiComponent` admission test: it skips
//! `FmiComponent::construct`, `SolveModel::validate`, input defaults and the
//! event inventory.
use std::num::NonZeroU64;

use crate::fmi::articulated_bindings::derive;
use crate::fmi::{FmiCausality, FmiMetadata, FmiVariability, FmiVariableInput, checked_metadata};
use crate::{
    AlgebraicRefreshRow, AlgebraicRefreshRowDraft, BinaryOp, ComputeBlock, ContinuousRefreshOwners,
    LinearOp, ProgramSlot, RefreshPlan, RefreshRowOwnerId, RefreshRowSelection,
    RefreshScalarProgramSource, ScalarProgramBlock, SolveBinaryOperator as B, SolveModel,
    SolveProgramConstructionError, SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallSite,
    SolvePureCallTable, SolveScalarType, SolveValueType, SolveVariableDeclaration,
    SolveVariableStorageRole, SolveVariableStorageRun, SolveVariableValueKind,
    TargetAssignmentShape, TensorInputKind, TypedProgramBuilder, UnaryOp,
    derive_target_assignment_shapes, scalar_slot_p, scalar_slot_y,
};
use rumoca_core::{
    IndexBinderProvenance, ProvenanceSpan, SourceId, Span, StructuredIndexBinder,
    StructuredIndexDomain,
};

/// Name, P column, storage base and shape, in value-reference order.
type Run = (&'static str, bool, usize, &'static [u32]);
const RUNS: [Run; 11] = [
    ("I", true, 0, &[6, 6]),
    ("S", true, 36, &[6]),
    ("armature", true, 42, &[]),
    ("effort", true, 43, &[]),
    ("bias", true, 44, &[6]),
    ("convective", true, 50, &[6]),
    ("U", false, 0, &[6]),
    ("D", false, 6, &[]),
    ("u", false, 7, &[]),
    ("Ia", false, 8, &[6, 6]),
    ("pa", false, 44, &[6]),
];
/// First call result register; results are U(6), D, u, Ia(36), pa(6).
const CALL: u32 = 58;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ArticulatedBindings.mo"), 1, 2)
}

fn ty(shape: &[u32]) -> SolveValueType {
    let real = SolveScalarType::real(SolvePureCallTable::default().arithmetic());
    if shape.is_empty() {
        SolveValueType::scalar(real)
    } else {
        SolveValueType::tensor(real, shape.to_vec()).unwrap()
    }
}

fn six(id: usize) -> StructuredIndexDomain {
    let binder = StructuredIndexBinder {
        provenance: IndexBinderProvenance::Generated,
        id,
        display_name: format!("k{id}"),
        lower: 1,
        upper: 6,
        step: 1,
    };
    StructuredIndexDomain {
        binders: vec![binder],
    }
}

/// `K.articulatedJointReduction` of rumoca-sim `articulated_source.rs`, in
/// the exact SSA order `call/articulated/pattern.rs` matches.
fn factor<'p>(
    b: &mut TypedProgramBuilder<'p>,
    inputs: &[ProgramSlot<'p>],
    outputs: &[ProgramSlot<'p>],
) -> Result<(), SolveProgramConstructionError> {
    let s = span();
    let loads = inputs
        .iter()
        .map(|&slot| b.load(slot, s))
        .collect::<Result<Vec<_>, _>>()?;
    let &[inertia, axis, armature, increment, effort, bias, convective] = loads.as_slice() else {
        unreachable!("seven factor inputs");
    };
    let u = b.matrix_multiply(inertia, axis, s)?;
    let su = b.matrix_multiply(axis, u, s)?;
    let d = b.binary(B::Add, su, armature, s)?;
    let d = b.binary(B::Add, d, increment, s)?;
    let sp = b.matrix_multiply(axis, bias, s)?;
    let shaft = b.binary(B::Subtract, effort, sp, s)?;
    let outer = b.map(six(0), &[u], ty(&[6]), s, outer_row)?;
    let outer = b.broadcast_binary(B::Divide, outer, d, false, s)?;
    let ia = b.binary(B::Subtract, inertia, outer, s)?;
    let iac = b.matrix_multiply(ia, convective, s)?;
    let pa = b.binary(B::Add, bias, iac, s)?;
    let ratio = b.binary(B::Divide, shaft, d, s)?;
    let correction = b.scale(u, ratio, s)?;
    let pa = b.binary(B::Add, pa, correction, s)?;
    for (slot, value) in outputs.iter().zip([u, d, shaft, ia, pa]) {
        b.store(*slot, value, s)?;
    }
    Ok(())
}

/// One row `{U[i]*U[j] for j in 1:6}`, as `call/articulated/outer.rs` reads it.
fn outer_row<'r>(
    b: &mut TypedProgramBuilder<'r>,
    captures: &[ProgramSlot<'r>],
    binders: &[ProgramSlot<'r>],
    output: ProgramSlot<'r>,
) -> Result<(), SolveProgramConstructionError> {
    let u = b.load(captures[0], span())?;
    let i = b.load(binders[0], span())?;
    let row = b.map(six(1), &[u, i], ty(&[]), span(), outer_element)?;
    b.store(output, row, span())
}

fn outer_element<'r>(
    b: &mut TypedProgramBuilder<'r>,
    captures: &[ProgramSlot<'r>],
    binders: &[ProgramSlot<'r>],
    output: ProgramSlot<'r>,
) -> Result<(), SolveProgramConstructionError> {
    let u = b.load(captures[0], span())?;
    let i = b.load(captures[1], span())?;
    let j = b.load(binders[0], span())?;
    let lhs = b.project_element_dynamic(u, &[i], span())?;
    let rhs = b.project_element_dynamic(u, &[j], span())?;
    let product = b.binary(B::Multiply, lhs, rhs, span())?;
    b.store(output, product, span())
}

fn calls() -> SolvePureCallTable {
    let inputs: [&[u32]; 7] = [&[6, 6], &[6], &[], &[], &[], &[6], &[6]];
    let outputs: [&[u32]; 5] = [&[6], &[], &[], &[6, 6], &[6]];
    let arithmetic = SolvePureCallTable::default().arithmetic();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::MIN),
            Vec::from(inputs.map(ty)),
            Vec::from(outputs.map(|shape| SolvePureCallOutput::result(ty(shape)))),
            span(),
            factor,
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    assert!(owner.articulated_joint_reduction().is_some(), "C112 body");
    table
}

/// P-backed call inputs (effort negated), then `y[k] - result[k]` per target.
fn program(increment: f64, targets: usize, site: SolvePureCallSite) -> Vec<LinearOp> {
    let p = |dst_start: u32, input_start: usize, count: usize| LinearOp::TensorLoad {
        dst_start,
        input: TensorInputKind::P,
        input_start,
        count,
        seed_start: None,
        lanes: 1,
    };
    let mut operations = vec![
        p(0, 0, 36),
        p(36, 36, 6),
        LinearOp::LoadP { dst: 42, index: 42 },
        LinearOp::Const {
            dst: 43,
            value: increment,
        },
        LinearOp::LoadP { dst: 44, index: 43 },
        LinearOp::Unary {
            dst: 45,
            op: UnaryOp::Neg,
            arg: 44,
        },
        p(46, 44, 6),
        p(52, 50, 6),
        LinearOp::PureCall {
            dst_start: CALL,
            input_starts: vec![0, 36, 42, 43, 45, 46, 52].into(),
            site,
        },
    ];
    for k in 0..targets as u32 {
        let y = CALL + 50 + 2 * k;
        operations.extend([
            LinearOp::LoadY {
                dst: y,
                index: k as usize,
            },
            LinearOp::Binary {
                dst: y + 1,
                op: BinaryOp::Sub,
                lhs: y,
                rhs: CALL + k,
            },
            LinearOp::StoreOutput { src: y + 1 },
        ]);
    }
    operations
}

fn row(k: usize, shapes: &[(usize, TargetAssignmentShape)]) -> AlgebraicRefreshRow {
    let shape = shapes
        .iter()
        .find(|(offset, shape)| *offset == k && shape.target_y_index() == k)
        .map(|(_, shape)| shape.clone());
    let call_result = CALL as usize + k;
    assert!(
        matches!(shape, Some(TargetAssignmentShape::Direct { expr_reg, .. })
            if expr_reg as usize == call_result),
        "row {k} has no direct certificate on its call result"
    );
    AlgebraicRefreshRow::checked(AlgebraicRefreshRowDraft {
        owner_id: RefreshRowOwnerId::checked(k).unwrap(),
        source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
        equation_index: k,
        output_offset: k,
        target_index: k,
        assignment_target: Some(k),
        assignment_shape: shape,
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .unwrap()
}

fn role(parameter: bool) -> SolveVariableStorageRole {
    if parameter {
        SolveVariableStorageRole::Parameter
    } else {
        SolveVariableStorageRole::Algebraic
    }
}

fn count(shape: &[u32]) -> usize {
    shape.iter().product::<u32>() as usize
}

fn model(increment: f64, targets: usize) -> SolveModel {
    let mut model = SolveModel::default();
    model.pure_calls = calls();
    let operations = program(increment, targets, model.pure_calls.owners()[0].call_site());
    let shapes = derive_target_assignment_shapes(&operations);
    let rows = (0..targets).map(|k| row(k, &shapes)).collect();
    let implicit = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            vec![operations],
            ProvenanceSpan::new(span(), "articulated binding fixture").unwrap(),
        )
        .unwrap(),
    );
    model.problem.continuous.refresh_owners = ContinuousRefreshOwners::checked_for_source(
        &implicit,
        RefreshPlan {
            rows,
            dynamic_causal_seed_rows: RefreshRowSelection::all(targets).unwrap(),
            ..Default::default()
        },
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    model.problem.continuous.implicit_rhs = implicit;
    let layout = &mut model.problem.solve_layout;
    layout.variable_storage_runs = RUNS
        .iter()
        .map(|&(_, parameter, base, shape)| SolveVariableStorageRun {
            base: if parameter {
                scalar_slot_p(base)
            } else {
                scalar_slot_y(base)
            },
            scalar_count: count(shape),
            role: role(parameter),
            value_kind: SolveVariableValueKind::Real,
        })
        .collect();
    layout.variable_declarations = RUNS
        .iter()
        .map(|&(_, parameter, ..)| {
            SolveVariableDeclaration::new(role(parameter), SolveVariableValueKind::Real)
        })
        .collect();
    model
}

fn input(&(name, parameter, _, shape): &Run) -> FmiVariableInput {
    FmiVariableInput {
        name: name.to_string(),
        role: role(parameter),
        value_kind: SolveVariableValueKind::Real,
        dimensions: shape.to_vec(),
        start: vec![0.0; count(shape)].into(),
        effective_input_default: None,
        minimum: None,
        maximum: None,
        nominal: None,
        unit: None,
        description: None,
        causality: if parameter {
            FmiCausality::Parameter
        } else {
            FmiCausality::Local
        },
        declared_causality: None,
        variability: if parameter {
            FmiVariability::Fixed
        } else {
            FmiVariability::Continuous
        },
        tunable: false,
        evaluable: false,
        declaration: span(),
        text_start: None,
        enumeration: None,
    }
}

fn inventory(model: &SolveModel) -> FmiMetadata {
    checked_metadata(&model.problem, RUNS.iter().map(input).collect()).unwrap()
}

#[test]
fn source_channels_project_to_value_references_elements_and_signs() {
    let model = model(0.0, 50);
    let metadata = inventory(&model);
    let reductions = derive(&model, &metadata);
    let [reduction] = reductions.as_slice() else {
        panic!("expected one reduction, got {}", reductions.len());
    };
    let vr = |name: &str| {
        let variable = metadata.variables().iter().find(|v| v.name() == name);
        variable.unwrap().value_reference_fmi3()
    };
    let inputs = reduction.inputs();
    let widths = inputs.iter().map(|group| group.len()).collect::<Vec<_>>();
    assert_eq!(widths, [36, 6, 1, 1, 1, 6, 6]);
    let increment = inputs[3][0].literal().unwrap();
    assert_eq!(increment.to_bits(), 0.0_f64.to_bits());
    assert_eq!(inputs[3][0].variable(), None);
    assert_eq!(inputs[4][0].literal(), None);
    assert_eq!(inputs[4][0].variable(), Some((vr("effort"), 0, true)));
    for (group, name, at) in [
        (0, "I", 35),
        (1, "S", 5),
        (2, "armature", 0),
        (5, "bias", 5),
        (6, "convective", 5),
    ] {
        assert_eq!(inputs[group][0].variable(), Some((vr(name), 0, false)));
        assert_eq!(inputs[group][at].variable(), Some((vr(name), at, false)));
    }
    let outputs = reduction.outputs();
    assert_eq!(outputs.len(), 50);
    for (index, name, element) in [
        (0, "U", 0),
        (5, "U", 5),
        (6, "D", 0),
        (7, "u", 0),
        (8, "Ia", 0),
        (43, "Ia", 35),
        (44, "pa", 0),
        (49, "pa", 5),
    ] {
        assert_eq!(outputs[index].variable(), Some((vr(name), element, false)));
    }
}

#[test]
fn changed_increment_or_missing_target_issues_no_reduction() {
    for (increment, targets, expected) in [(0.0, 50, 1), (1.0, 50, 0), (-0.0, 50, 0), (0.0, 49, 0)]
    {
        let model = model(increment, targets);
        let reductions = derive(&model, &inventory(&model));
        assert_eq!(reductions.len(), expected, "{increment:?}, {targets}");
    }
}
