//! One checked bulk program must contribute its complete owner union once.

use rumoca_ir_solve as solve;

pub(super) fn source(count: usize) -> solve::ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exact-algebraic-target.mo"),
        0,
        1,
    );
    let operations = (0..count)
        .flat_map(|index| {
            let base = u32::try_from(3 * index).unwrap();
            [
                solve::LinearOp::LoadY { dst: base, index },
                solve::LinearOp::Const {
                    dst: base + 1,
                    value: 1.0,
                },
                solve::LinearOp::Binary {
                    dst: base + 2,
                    op: solve::BinaryOp::Sub,
                    lhs: base,
                    rhs: base + 1,
                },
                solve::LinearOp::StoreOutput { src: base + 2 },
            ]
        })
        .collect();
    solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_source_span(
            vec![operations],
            rumoca_core::ProvenanceSpan::new(span, "exact algebraic target fixture").unwrap(),
        )
        .unwrap(),
    )
}

fn row(index: usize) -> solve::AlgebraicRefreshRow {
    solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
        owner_id: solve::RefreshRowOwnerId::checked(index).unwrap(),
        source: solve::RefreshScalarProgramSource::checked(0, 0).unwrap(),
        equation_index: index,
        output_offset: index,
        target_index: index,
        assignment_target: Some(index),
        assignment_shape: Some(solve::TargetAssignmentShape::Direct {
            target_y_index: index,
            expr_reg: u32::try_from(3 * index + 1).unwrap(),
            target_scale: 1.0,
            expr_eval_len: 4 * index + 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .unwrap()
}

fn plan(count: usize) -> solve::RefreshPlan {
    let selection = solve::RefreshRowSelection::all(count).unwrap();
    solve::RefreshPlan {
        simultaneous_plan: solve::AlgebraicProjectionPlan {
            blocks: (0..count)
                .map(|index| solve::AlgebraicProjectionBlock {
                    rows: vec![index],
                    y_indices: vec![index],
                    tearing: None,
                    alternate_charts: vec![],
                })
                .collect(),
        },
        simultaneous_block_indices: (0..count).collect(),
        rows: (0..count).map(row).collect(),
        causal_seed_rows: selection.clone(),
        dynamic_causal_seed_rows: selection.clone(),
        value_stages: vec![solve::RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: Default::default(),
            dynamic_rows: selection,
        }],
        causal_solution_certified: true,
        ..Default::default()
    }
}

pub(super) fn fixture(count: usize) -> solve::SolveProblem {
    let implicit_rhs = source(count);
    let algebraic = plan(count);
    let projection = algebraic.simultaneous_plan.clone();
    let refresh_owners = solve::ContinuousRefreshOwners::checked_for_source(
        &implicit_rhs,
        algebraic,
        Default::default(),
        Default::default(),
        Default::default(),
        vec![],
    )
    .unwrap();
    let names = (0..count)
        .map(|index| {
            if count == 1 {
                "y".into()
            } else {
                format!("y{index}")
            }
        })
        .collect::<Vec<String>>();
    solve::SolveProblem {
        layout: solve::VarLayout::from_parts(indexmap::IndexMap::new(), count, 0),
        solve_layout: solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                name_to_idx: names
                    .iter()
                    .cloned()
                    .enumerate()
                    .map(|(i, name)| (name, i))
                    .collect(),
                base_to_indices: names
                    .iter()
                    .cloned()
                    .enumerate()
                    .map(|(i, name)| (name, vec![i]))
                    .collect(),
                names,
            },
            algebraic_scalar_count: count,
            ..Default::default()
        },
        continuous: solve::ContinuousSolveSystem {
            implicit_rhs: implicit_rhs.clone(),
            residual: implicit_rhs,
            implicit_row_targets: (0..count)
                .map(|index| {
                    Some(solve::ScalarSlot::Y {
                        index,
                        byte_offset: index * 8,
                    })
                })
                .collect(),
            algebraic_projection_plan: projection,
            refresh_owners,
            ..Default::default()
        },
        ..Default::default()
    }
}

#[test]
fn checked_bulk_program_has_exact_owner_union_and_explicit_admission() {
    const COUNT: usize = 4096;
    let construction = std::time::Instant::now();
    let problem = fixture(COUNT);
    let construction_elapsed = construction.elapsed();
    problem.validate().unwrap();
    let owners = &problem.continuous.refresh_owners;
    let schedule = owners
        .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
        .unwrap();
    let [program_id] = schedule.program_ids() else {
        panic!("fixture must issue one bulk program");
    };
    let program = owners.exact_assignment_program(*program_id).unwrap();
    assert_eq!(program.target_indices(), &(0..COUNT).collect::<Vec<_>>());
    assert_eq!(program.row_owners().len(), COUNT);
    assert_eq!(program.assignment_shapes().len(), COUNT);
    let issued = program
        .row_owners()
        .iter()
        .copied()
        .collect::<std::collections::BTreeSet<_>>();
    let canonical = owners
        .algebraic()
        .rows
        .iter()
        .map(|row| row.owner_id())
        .collect();
    assert_eq!(issued, canonical);
    assert_eq!(issued.len(), COUNT);
    let admission = std::time::Instant::now();
    assert!(rumoca_phase_codegen::explicit_algebraic_assignment_complete(&problem));
    println!(
        "bulk_owner_union targets={COUNT} programs=1 construction_seconds={:.9} admission_seconds={:.9}",
        construction_elapsed.as_secs_f64(),
        admission.elapsed().as_secs_f64()
    );
}
