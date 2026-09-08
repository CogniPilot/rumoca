use super::*;
use crate::lower_solve_model;
use rumoca_core::{InstanceId, SourceMap, Span, TypeId, VarName};

#[derive(Clone, Copy)]
enum ResidualOrientation {
    DerivativeMinusConstant,
    ConstantMinusDerivative,
}

#[test]
fn operation_projection_refuses_incomplete_extra_and_overlong_programs() {
    let constant = solve::LinearOp::Const {
        dst: 0,
        value: 1.25,
    };
    let store = solve::LinearOp::StoreOutput { src: 0 };
    for programs in [
        vec![],
        vec![vec![]],
        vec![vec![constant.clone()]],
        vec![vec![constant.clone(), store.clone(), constant.clone()]],
        vec![vec![constant.clone()], vec![store.clone()]],
        vec![vec![constant, store], vec![]],
    ] {
        let facts = project_operations(&programs);
        assert_eq!(facts, ScalarBlockFacts::UnsupportedShape);
        assert_eq!(
            check_kernel(facts, 1.25_f64.to_bits()),
            Err(ScalarConstantDerivativeMismatch::KernelShape)
        );
    }
}

#[test]
fn operation_projection_preserves_exact_bits_and_order() {
    let bits = 0x7ff8_0000_1234_5678;
    let programs = [vec![
        solve::LinearOp::Const {
            dst: 7,
            value: f64::from_bits(bits),
        },
        solve::LinearOp::LoadY { dst: 8, index: 3 },
    ]];
    assert_eq!(
        project_operations(&programs),
        ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 7,
                bits
            },
            OperationFact::LoadY {
                destination: 8,
                index: 3
            },
        ])
    );
}

#[test]
fn operation_projection_preserves_refused_slots_and_empty_width() {
    let programs = [vec![
        solve::LinearOp::Const {
            dst: 6,
            value: -0.0,
        },
        solve::LinearOp::LoadP { dst: 9, index: 4 },
        solve::LinearOp::StoreOutput { src: 6 },
    ]];
    assert_eq!(
        project_operations(&programs),
        ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 6,
                bits: (-0.0_f64).to_bits(),
            },
            OperationFact::Unsupported,
            OperationFact::StoreOutput { source: 6 },
        ])
    );
    assert_eq!(
        project_operations::<0>(&[vec![]]),
        ScalarBlockFacts::Exact([])
    );
    assert_eq!(
        project_operations::<0>(&[vec![], vec![]]),
        ScalarBlockFacts::UnsupportedShape
    );
}

#[test]
fn exact_program_borrows_complete_storage_for_each_supported_width() {
    let programs = [vec![11_u32, 22]];
    let operations = exact_program::<_, 2>(&programs).expect("exact program");
    assert_eq!(operations, &[11, 22]);
    assert!(std::ptr::eq(operations.as_ptr(), programs[0].as_ptr()));
    assert!(exact_program::<_, 3>(&programs).is_none());
    let programs = [vec![11_u32, 22, 33]];
    assert_eq!(exact_program::<_, 3>(&programs), Some(&[11, 22, 33]));
    assert!(exact_program::<_, 2>(&programs).is_none());
    let empty = [Vec::<u32>::new()];
    assert_eq!(exact_program::<_, 0>(&empty), Some(&[]));
}

fn operations_mut<const WIDTH: usize>(
    facts: &mut ScalarBlockFacts<WIDTH>,
) -> &mut [OperationFact; WIDTH] {
    let ScalarBlockFacts::Exact(operations) = facts else {
        panic!("fixture must have the exact program shape");
    };
    operations
}

#[test]
fn direct_admission_normalizes_both_residual_forms() {
    for orientation in [
        ResidualOrientation::DerivativeMinusConstant,
        ResidualOrientation::ConstantMinusDerivative,
    ] {
        let dae = fixture(-0.0, FixtureRhs::Constant(1.25), orientation);
        let profile = dae
            .inspect(admit_dae_profile)
            .expect("exact profile admits");
        assert_eq!(profile.start_bits, (-0.0_f64).to_bits());
        assert_eq!(profile.derivative_constant_bits, 1.25_f64.to_bits());
    }
}

#[test]
fn direct_admission_rejects_state_dependent_residual() {
    for orientation in [
        ResidualOrientation::DerivativeMinusConstant,
        ResidualOrientation::ConstantMinusDerivative,
    ] {
        let dae = fixture(1.0, FixtureRhs::State, orientation);
        assert_eq!(
            dae.inspect(admit_dae_profile).err(),
            Some(ScalarConstantDerivativeUnsupported::ResidualShape)
        );
    }
}

#[test]
fn direct_admission_rejects_extra_residual_owner() {
    for orientation in [
        ResidualOrientation::DerivativeMinusConstant,
        ResidualOrientation::ConstantMinusDerivative,
    ] {
        let dae = fixture_with_residual_count(2.0, FixtureRhs::Constant(1.0), orientation, 2);
        assert_eq!(dae.inspect(|view| view.continuous_equation_count()), 2);
        assert_eq!(dae.inspect(|view| view.continuous_owner_count()), 2);
        assert_eq!(
            dae.inspect(admit_dae_profile).err(),
            Some(ScalarConstantDerivativeUnsupported::DaeOwner {
                owner: "continuous equations",
                actual: 2,
            })
        );
    }
}

#[test]
fn production_lowering_mints_the_receipt_for_both_residual_orientations() {
    for orientation in [
        ResidualOrientation::DerivativeMinusConstant,
        ResidualOrientation::ConstantMinusDerivative,
    ] {
        let dae = fixture(-0.0, FixtureRhs::Constant(1.25), orientation);
        let lowered = lower_solve_model(&dae, &HashMap::new(), |_| {})
            .expect("the bounded scalar constant-derivative profile lowers");
        assert!(
            lowered.scalar_constant_derivative_refinement().is_ok(),
            "the sole production lowering carries the C61 receipt"
        );
        assert_eq!(
            lowered.model().initial_y()[0].to_bits(),
            (-0.0_f64).to_bits()
        );
    }
}

/// The expected tangent and visible-row programs are the forward-mode
/// lowering of the constant kernel and the state's storage projection;
/// this pins both against the real construction so the checker's
/// expectations are derived from the lowering, not assumed.
#[test]
fn production_lowering_projects_the_expected_jacobian_and_visible_row_programs() {
    let constant = 1.25;
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(constant),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let lowered = lower_solve_model(&dae, &HashMap::new(), |_| {}).expect("fixture lowers");
    let facts = project_solve_facts(lowered.model());
    assert_eq!(
        facts.kernel,
        ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 0,
                bits: constant.to_bits()
            },
            OperationFact::StoreOutput { source: 0 },
        ])
    );
    assert_eq!(
        facts.full_jacobian,
        ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 0,
                bits: constant.to_bits()
            },
            OperationFact::Constant {
                destination: 1,
                bits: 0.0_f64.to_bits()
            },
            OperationFact::StoreOutput { source: 1 },
        ])
    );
    assert_eq!(
        facts.visible_rows,
        ScalarBlockFacts::Exact([
            OperationFact::LoadY {
                destination: 0,
                index: 0
            },
            OperationFact::StoreOutput { source: 0 },
        ])
    );
    assert_eq!(
        facts.metadata.structural_derivative,
        DerivativePatternFact::Empty {
            rows: 1,
            columns: 1,
        }
    );
    assert_eq!(facts.metadata.mass_matrix, MassMatrixFact::Identity);
    assert_eq!(facts.metadata.structural_implicit, 0);
    assert_eq!(facts.metadata.initialization_structural_residual, 0);
}

#[test]
fn numeric_owner_counts_cover_empty_equal_and_boundary_values() {
    assert_eq!(check_owner_counts(&[], &[]), Ok(()));
    assert_eq!(
        check_owner_counts(&[0, usize::MAX], &[0, usize::MAX]),
        Ok(())
    );
    assert_eq!(
        check_owner_counts(&[0, usize::MAX], &[0, 1]),
        Err(OwnerCountMismatch {
            index: 1,
            expected: 1,
            actual: usize::MAX,
        })
    );
}

#[test]
fn start_projection_distinguishes_absence_empty_and_first_value_bits() {
    assert_eq!(project_start_value_facts(None), (false, 0, 0));
    assert_eq!(project_start_value_facts(Some(&[])), (true, 0, 0));
    for bits in [0, 0x8000_0000_0000_0000, 1, 0x3ff4_0000_0000_0000] {
        let values = [f64::from_bits(bits), 7.0];
        assert_eq!(project_start_value_facts(Some(&values)), (true, 2, bits));
    }
}

#[test]
fn actual_root_value_projection_forwards_supplied_owner_premises() {
    let dae = fixture(
        -0.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let lowered = lower_solve_model(&dae, &HashMap::new(), |_| {}).expect("fixture lowers");
    let owners = [usize::MAX; SOLVE_OWNER_WIDTH];
    let mut metadata = project_solve_metadata(lowered.model());
    metadata.pure_call_owners = 17;
    metadata.mass_matrix = MassMatrixFact::Diagonal { entry_count: 9 };
    let facts = project_solve_values_with_owner_premises(lowered.model(), owners, metadata);
    assert_ne!(
        facts.kernel,
        project_kernel(lowered.model().problem().continuous().residual()),
        "this witness must distinguish derivative and residual owners"
    );
    assert_eq!(facts.owners, owners);
    assert_eq!(facts.metadata.pure_call_owners, 17);
    assert_eq!(facts.metadata.mass_matrix, metadata.mass_matrix);
    assert!(facts.catalog_start_present);
    assert_eq!(facts.catalog_start_width, 1);
    assert_eq!(facts.catalog_start_bits, (-0.0_f64).to_bits());
    assert_eq!(facts.initial_y_width, 1);
    assert_eq!(facts.initial_y_bits, (-0.0_f64).to_bits());
    assert_eq!(
        facts.kernel,
        ScalarBlockFacts::Exact([
            OperationFact::Constant {
                destination: 0,
                bits: 1.25_f64.to_bits(),
            },
            OperationFact::StoreOutput { source: 0 },
        ])
    );
}

#[test]
fn empty_constructed_ir_root_projects_absent_starts_without_indexing() {
    let dae = fixture(
        0.0,
        FixtureRhs::Constant(1.0),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let lowered = lower_solve_model(&dae, &HashMap::new(), |_| {}).expect("fixture lowers");
    let model = empty_ir_root_from_scalar_fixture(lowered.model());
    assert!(model.variable_catalog().entries().is_empty());
    assert!(model.initial_y().is_empty());
    let facts = project_solve_values_with_owner_premises(
        &model,
        project_solve_owner_census(&model),
        project_solve_metadata(&model),
    );
    assert!(!facts.catalog_start_present);
    assert_eq!(facts.catalog_start_width, 0);
    assert_eq!(facts.catalog_start_bits, 0);
    assert_eq!(facts.initial_y_width, 0);
    assert_eq!(facts.initial_y_bits, 0);
}

// Exercise the public IR construction domain, not the source compiler's
// separately refused empty-model case. Reuse the scalar fixture's checked
// noncontinuous owners and seal the resized input at both actual constructors.
// Only catalog, initial_y and continuous dimensions are empty; discrete,
// event and clock owners remain those of the scalar fixture.
fn empty_ir_root_from_scalar_fixture(base: &solve::SolveModel) -> solve::SolveModel {
    let mut layout = base.problem().solve_layout().clone();
    layout.solver_maps.names.clear();
    layout.solver_maps.name_to_idx.clear();
    layout.solver_maps.base_to_indices.clear();
    layout.variable_storage_runs.clear();
    layout.variable_declarations.clear();
    layout.state_scalar_count = 0;
    let block = solve::ComputeBlock { nodes: Vec::new() };
    let continuous = solve::ContinuousSolveSystem::construct(
        &layout,
        solve::ContinuousSolveSystemInputs::new(
            block.clone(),
            Vec::new(),
            solve::AlgebraicProjectionPlan { blocks: Vec::new() },
            block.clone(),
            (
                block.clone(),
                solve::AlgebraicProjectionPlan { blocks: Vec::new() },
            ),
            block,
            solve::ContinuousRefreshPlanInputs::empty(),
        ),
    )
    .expect("empty continuous IR constructs");
    let problem = solve::SolveProblem::construct(
        solve::VarLayout::from_parts(indexmap::IndexMap::new(), 0, 0),
        layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        base.problem().discrete().clone(),
        base.problem().events().clone(),
        base.problem().clocks().clone(),
    )
    .expect("empty Solve problem constructs");
    solve::SolveModel::construct(
        problem,
        solve::SolvePureCallTable::empty(base.pure_calls().arithmetic()),
        solve::SolveArtifactInputs::empty(),
        solve::SolveModelRuntimeInputs {
            initial_y: Vec::new(),
            solver_nominals: Vec::new(),
            parameters: Vec::new(),
        },
        solve::ScalarProgramBlock::with_program_spans(Vec::new(), Vec::new())
            .expect("empty visible block constructs"),
        Vec::<solve::SolveVariableCatalogSourceEntry>::new(),
    )
    .expect("empty Solve root constructs")
}

#[test]
fn dae_owner_census_enforces_the_complete_scalar_policy() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.0),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let owners = dae.inspect(project_dae_owner_census);
    assert_eq!(
        owners,
        [
            0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0,
        ]
    );
    assert_eq!(check_dae_owner_census(&owners), Ok(()));
    for (index, expected) in owners.into_iter().enumerate() {
        for actual in [0, 1, 2, usize::MAX]
            .into_iter()
            .filter(|actual| *actual != expected)
        {
            let mut changed = owners;
            changed[index] = actual;
            assert_eq!(
                check_dae_owner_census(&changed),
                Err(ScalarConstantDerivativeUnsupported::DaeOwner {
                    owner: DAE_OWNER_NAMES[index],
                    actual,
                }),
                "DAE owner slot {index} must retain its refusal and actual count"
            );
        }
    }
}

#[test]
fn dae_owner_census_preserves_first_error_labels_and_counts() {
    let mut owners = [0; DAE_CENSUS_WIDTH];
    owners[0] = usize::MAX;
    owners[2] = 1;
    owners[30] = 2;
    for (index, label, actual, restored) in [
        (0, "functions", usize::MAX, 0),
        (1, "continuous equations", 0, 1),
        (30, "discrete value definitions", 2, 0),
    ] {
        assert_eq!(
            check_dae_owner_census(&owners),
            Err(ScalarConstantDerivativeUnsupported::DaeOwner {
                owner: label,
                actual,
            })
        );
        owners[index] = restored;
    }
    assert_eq!(check_dae_owner_census(&owners), Ok(()));
}

#[test]
fn every_owner_count_is_checked_before_receipt_issuance() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.0),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let (dae_facts, lowered) = admitted(&dae);
    let owners = project_solve_facts(lowered.model()).owners;
    assert_eq!(
        owners,
        [
            0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0
        ]
    );
    for (index, expected) in owners.into_iter().enumerate() {
        for actual in [0, 2, usize::MAX]
            .into_iter()
            .filter(|actual| *actual != expected)
        {
            let mut facts = project_solve_facts(lowered.model());
            facts.owners[index] = actual;
            assert!(
                matches!(
                    check_scalar_constant_derivative_refinement(&dae_facts, &facts),
                    Err(ScalarConstantDerivativeMismatch::SolveOwner {
                        owner,
                        expected: required,
                        actual: seen,
                    }) if owner == SOLVE_OWNERS[index]
                        && required == expected
                        && seen == actual
                ),
                "owner {index} with count {actual}, expected {expected}, must refuse the receipt"
            );
        }
    }
}

#[test]
fn owner_census_reports_the_first_unequal_owner() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.0),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let (_, lowered) = admitted(&dae);
    let mut facts = project_solve_facts(lowered.model());
    facts.owners[0] = 2;
    facts.owners[3] = 0;
    facts.owners[23] = 0;
    for (index, expected, actual) in [(0, 0, 2), (3, 1, 0), (23, 1, 0)] {
        assert_eq!(
            check_solve_owner_census(&facts),
            Err(ScalarConstantDerivativeMismatch::SolveOwner {
                owner: SOLVE_OWNERS[index],
                expected,
                actual,
            })
        );
        facts.owners[index] = expected;
    }
    assert_eq!(check_solve_owner_census(&facts), Ok(()));
}

#[test]
fn checker_refuses_catalog_and_runtime_start_bit_mutations() {
    let dae = fixture(
        -0.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::ConstantMinusDerivative,
    );
    let (dae_facts, lowered) = admitted(&dae);
    let mut solve_facts = project_solve_facts(lowered.model());
    solve_facts.catalog_start_bits = 0.0_f64.to_bits();
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::CatalogStartBits { .. })
    ));

    let mut solve_facts = project_solve_facts(lowered.model());
    solve_facts.initial_y_bits = 0.0_f64.to_bits();
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::InitialYBits { .. })
    ));
}

#[test]
fn checker_refuses_negated_constant_and_compact_tensor_mutations() {
    let constant = 1.25;
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(constant),
        ResidualOrientation::ConstantMinusDerivative,
    );
    let (dae_facts, lowered) = admitted(&dae);
    let mut solve_facts = project_solve_facts(lowered.model());
    operations_mut(&mut solve_facts.kernel)[0] = OperationFact::Constant {
        destination: 0,
        bits: (-constant).to_bits(),
    };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::DerivativeConstantBits { .. })
    ));

    let mut solve_facts = project_solve_facts(lowered.model());
    let original = lowered.model().problem().continuous().derivative_rhs();
    let [solve::ComputeNode::ScalarPrograms(program)] = original.nodes.as_slice() else {
        panic!("fixture must contain one scalar compute node");
    };
    let domain = rumoca_core::StructuredIndexDomain { binders: vec![] };
    let tensor = solve::ComputeNode::Map {
        output_map: solve::TensorOutputMap::dense_contiguous(0, &domain).expect("output map"),
        domain,
        base_ops: program.programs()[0].clone(),
        load_strides: vec![],
        const_strides: vec![],
        metadata: solve::TensorNodeMetadata {
            element_type: solve::TensorElementType::Real64,
            layout: solve::TensorLayout::RowMajorDense,
            scalar_fallback: solve::ScalarFallback::Exact,
        },
        span: program.program_span(0).expect("program span"),
    };
    solve_facts.kernel = project_kernel(&solve::ComputeBlock {
        nodes: vec![tensor],
    });
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::KernelShape)
    ));
}

#[test]
fn scalar_projection_refuses_multiple_outputs_even_when_program_width_matches() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let (_, lowered) = admitted(&dae);
    let original = lowered.model().problem().continuous().derivative_rhs();
    let solve::ComputeNode::ScalarPrograms(program) = &original.nodes[0] else {
        panic!("fixture must contain a scalar compute node");
    };
    let block = solve::ScalarProgramBlock::with_output_indices(
        vec![vec![
            solve::LinearOp::Const {
                dst: 0,
                value: 1.25,
            },
            solve::LinearOp::StoreOutput { src: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        vec![program.program_span(0).expect("source program span")],
        vec![0, 7],
    )
    .expect("a valid two-output program with distinct logical coordinates");
    assert!(matches!(
        project_operations::<3>(block.programs()),
        ScalarBlockFacts::Exact(_)
    ));
    assert_eq!(
        project_scalar_block::<3>(&block),
        ScalarBlockFacts::UnsupportedShape
    );
}

#[test]
fn kernel_projection_refuses_empty_extra_and_redirected_owners() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let (_, lowered) = admitted(&dae);
    let original = lowered.model().problem().continuous().derivative_rhs();
    let [solve::ComputeNode::ScalarPrograms(program)] = original.nodes.as_slice() else {
        panic!("fixture must contain one scalar compute node");
    };
    let redirected = solve::ScalarProgramBlock::with_output_indices(
        program.programs().to_vec(),
        program.program_spans().to_vec(),
        vec![1],
    )
    .expect("a well-formed program with an out-of-profile output");
    let empty = solve::ScalarProgramBlock::with_program_spans(vec![], vec![]).expect("empty block");
    for nodes in [
        vec![],
        vec![original.nodes[0].clone(), original.nodes[0].clone()],
        vec![
            original.nodes[0].clone(),
            solve::ComputeNode::ScalarPrograms(empty),
        ],
        vec![solve::ComputeNode::ScalarPrograms(redirected)],
    ] {
        assert_eq!(
            project_kernel(&solve::ComputeBlock { nodes }),
            ScalarBlockFacts::UnsupportedShape
        );
    }
}

#[test]
fn checker_refuses_full_jacobian_bodies_with_the_right_count() {
    let constant = 1.25;
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(constant),
        ResidualOrientation::ConstantMinusDerivative,
    );
    let (dae_facts, lowered) = admitted(&dae);

    // Storing the primal register would report d(der x)/dx = c.
    let mut solve_facts = project_solve_facts(lowered.model());
    operations_mut(&mut solve_facts.full_jacobian)[2] = OperationFact::StoreOutput { source: 0 };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::FullJacobianOperations)
    ));

    // A nonzero tangent constant is a nonzero Jacobian.
    let mut solve_facts = project_solve_facts(lowered.model());
    operations_mut(&mut solve_facts.full_jacobian)[1] = OperationFact::Constant {
        destination: 1,
        bits: 1.0_f64.to_bits(),
    };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::FullJacobianTangentBits { .. })
    ));

    // The primal constant carried by the tangent program must be the
    // same literal the kernel carries.
    let mut solve_facts = project_solve_facts(lowered.model());
    operations_mut(&mut solve_facts.full_jacobian)[0] = OperationFact::Constant {
        destination: 0,
        bits: (-constant).to_bits(),
    };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::FullJacobianPrimalBits { .. })
    ));

    // An extra trailing operation keeps the first three intact but is
    // still not the exact program.
    let mut solve_facts = project_solve_facts(lowered.model());
    let mut programs = lowered
        .model()
        .artifacts()
        .continuous()
        .full_jacobian_v
        .programs()
        .to_vec();
    programs[0].push(solve::LinearOp::StoreOutput { src: 0 });
    solve_facts.full_jacobian = project_operations(&programs);
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::FullJacobianShape)
    ));
}

#[test]
fn checker_refuses_visible_rows_that_read_other_storage() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::ConstantMinusDerivative,
    );
    let (dae_facts, lowered) = admitted(&dae);

    let mut solve_facts = project_solve_facts(lowered.model());
    operations_mut(&mut solve_facts.visible_rows)[0] = OperationFact::LoadY {
        destination: 0,
        index: 1,
    };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::VisibleRowOperations)
    ));

    // Exercise opcode classification, not an already-classified unsupported fact.
    for operation in [
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::LoadSeed { dst: 0, index: 0 },
    ] {
        let mut solve_facts = project_solve_facts(lowered.model());
        let programs = vec![vec![operation, solve::LinearOp::StoreOutput { src: 0 }]];
        solve_facts.visible_rows = project_operations(&programs);
        let result = check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts);
        assert!(
            matches!(
                &result,
                Err(ScalarConstantDerivativeMismatch::VisibleRowOperations)
            ),
            "foreign storage program {programs:?} produced error {:?}",
            result.err()
        );
    }

    let mut solve_facts = project_solve_facts(lowered.model());
    let mut programs = lowered.model().visible_value_rows().programs().to_vec();
    programs.push(Vec::new());
    solve_facts.visible_rows = project_operations(&programs);
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::VisibleRowShape)
    ));
}

#[test]
fn checker_names_the_structural_artifact_kind_not_a_total() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::ConstantMinusDerivative,
    );
    let (dae_facts, lowered) = admitted(&dae);
    // Same total of one artifact, different kind.
    let mut solve_facts = project_solve_facts(lowered.model());
    solve_facts.metadata.structural_derivative = DerivativePatternFact::Absent;
    solve_facts.metadata.structural_implicit = 1;
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(ScalarConstantDerivativeMismatch::SolveMetadata {
            field: SolveMetadataField::StructuralImplicit,
            expected: 0,
            actual: 1,
        })
    ));

    let mut solve_facts = project_solve_facts(lowered.model());
    solve_facts.metadata.structural_derivative = DerivativePatternFact::Absent;
    solve_facts.metadata.initialization_structural_residual = 1;
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts,),
        Err(
            ScalarConstantDerivativeMismatch::DerivativeStructuralPattern {
                kind: DerivativePatternKind::Absent,
                rows: 0,
                columns: 0,
            }
        )
    ));
}

#[test]
fn checker_distinguishes_mass_matrix_kind_and_exact_derivative_pattern() {
    let dae = fixture(
        2.0,
        FixtureRhs::Constant(1.25),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let (dae_facts, lowered) = admitted(&dae);

    for mass_matrix in [
        MassMatrixFact::Diagonal { entry_count: 0 },
        MassMatrixFact::Sparse { entry_count: 0 },
    ] {
        let mut solve_facts = project_solve_facts(lowered.model());
        solve_facts.metadata.mass_matrix = mass_matrix;
        assert!(matches!(
            check_scalar_constant_derivative_refinement(&dae_facts, &solve_facts),
            Err(ScalarConstantDerivativeMismatch::MassMatrix { entry_count: 0, .. })
        ));
    }

    let mut wrong_shape = project_solve_facts(lowered.model());
    wrong_shape.metadata.structural_derivative = DerivativePatternFact::Empty {
        rows: 1,
        columns: 2,
    };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &wrong_shape),
        Err(
            ScalarConstantDerivativeMismatch::DerivativeStructuralPattern {
                kind: DerivativePatternKind::Empty,
                rows: 1,
                columns: 2,
            }
        )
    ));

    let mut wrong_representation = project_solve_facts(lowered.model());
    wrong_representation.metadata.structural_derivative = DerivativePatternFact::Other {
        kind: NonemptyDerivativePatternKind::Full,
        rows: 1,
        columns: 1,
    };
    assert!(matches!(
        check_scalar_constant_derivative_refinement(&dae_facts, &wrong_representation),
        Err(
            ScalarConstantDerivativeMismatch::DerivativeStructuralPattern {
                kind: DerivativePatternKind::Other(NonemptyDerivativePatternKind::Full),
                rows: 1,
                columns: 1,
            }
        )
    ));
}

#[test]
fn runtime_overrides_leave_the_profile_and_the_lowering_proceeds() {
    let dae = fixture(
        0.0,
        FixtureRhs::Constant(1.0),
        ResidualOrientation::DerivativeMinusConstant,
    );
    let mut overrides = HashMap::new();
    overrides.insert("s".to_owned(), 5.0);
    let lowered = lower_solve_model(&dae, &overrides, |_| {})
        .expect("a state start override lowers generically");
    assert_eq!(
        lowered.scalar_constant_derivative_refinement().err(),
        Some(&ScalarConstantDerivativeUnsupported::RuntimeOverrides)
    );
    assert_eq!(lowered.model().initial_y(), [5.0]);
}

#[test]
fn out_of_profile_residual_lowers_without_a_receipt() {
    let dae = fixture(
        1.0,
        FixtureRhs::State,
        ResidualOrientation::DerivativeMinusConstant,
    );
    let lowered = lower_solve_model(&dae, &HashMap::new(), |_| {})
        .expect("der(s) = s lowers through the generic path");
    assert_eq!(
        lowered.scalar_constant_derivative_refinement().err(),
        Some(&ScalarConstantDerivativeUnsupported::ResidualShape)
    );
    assert_eq!(lowered.model().initial_y(), [1.0]);
}

fn admitted(
    dae: &dae::Dae,
) -> (
    AdmittedScalarConstantDerivativeProfile,
    crate::LoweredSolveModel<'_>,
) {
    let dae_facts = dae.inspect(admit_dae_profile).expect("fixture is admitted");
    let lowered = lower_solve_model(dae, &HashMap::new(), |_| {}).expect("fixture lowers");
    (dae_facts, lowered)
}

#[derive(Clone, Copy)]
enum FixtureRhs {
    Constant(f64),
    State,
}

fn fixture(start_value: f64, rhs: FixtureRhs, orientation: ResidualOrientation) -> dae::Dae {
    fixture_with_residual_count(start_value, rhs, orientation, 1)
}

fn fixture_with_residual_count(
    start_value: f64,
    rhs: FixtureRhs,
    orientation: ResidualOrientation,
    residual_count: usize,
) -> dae::Dae {
    let mut source_map = SourceMap::new();
    let source = source_map.add("scalar_constant_derivative.mo", "Real s; der(s)=c;");
    let declaration = dae::DaeProvenance::source(Span::from_offsets(source, 0, 6))
        .expect("fixture declaration has source provenance");
    let equation = dae::DaeProvenance::source(Span::from_offsets(source, 8, 17))
        .expect("fixture equation has source provenance");
    dae::Dae::construct(source_map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(start_value))
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("s"),
                InstanceId::new(1),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(Fixity::Fixed),
                    causality: dae::VariableCausality::Local,
                    origin: dae::VariableOrigin::Source,
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let other = match rhs {
                FixtureRhs::Constant(constant) => expressions
                    .at(equation)
                    .literal(dae::DaeLiteral::Real(constant))?,
                FixtureRhs::State => expressions
                    .at(equation)
                    .coordinate(dae::CoordinateInput::State(state))?,
            };
            let (lhs, rhs) = match orientation {
                ResidualOrientation::DerivativeMinusConstant => (derivative, other),
                ResidualOrientation::ConstantMinusDerivative => (other, derivative),
            };
            expressions
                .at(equation)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| {
            add_fixture_residuals(continuous, equation, residual, residual_count)
        })
    })
    .expect("fixture constructs")
}

fn add_fixture_residuals<'dae>(
    continuous: &mut dae::ContinuousEquations<'_, 'dae>,
    owner: dae::DaeProvenance,
    residual: dae::ExprId<'dae>,
    count: usize,
) -> Result<(), dae::DaeConstructionError> {
    for _ in 0..count {
        continuous.value_equation(owner, residual)?;
    }
    Ok(())
}
