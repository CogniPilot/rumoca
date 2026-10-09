use super::super::refresh_row_candidates;
use super::*;

fn span() -> rumoca_core::Span {
    solve::source_span_from_offsets(963, 0, 1)
}

fn problem_with_programs(
    programs: Vec<Vec<solve::LinearOp>>,
    outputs: Vec<usize>,
    targets: Vec<Option<solve::ScalarSlot>>,
) -> solve::SolveProblem {
    let spans = vec![span(); programs.len()];
    let block = solve::ScalarProgramBlock::with_output_indices(programs, spans, outputs).unwrap();
    let mut problem = solve::SolveProblem::default();
    problem.solve_layout.algebraic_scalar_count = targets.len();
    problem.solve_layout.solver_maps.names = (0..targets.len())
        .map(|index| format!("y{index}"))
        .collect();
    problem.continuous.implicit_row_targets = targets;
    problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(block);
    problem
}

fn constant_outputs(count: usize) -> Vec<solve::LinearOp> {
    let mut source = vec![solve::LinearOp::Const { dst: 0, value: 5.0 }];
    source.extend(std::iter::repeat_n(
        solve::LinearOp::StoreOutput { src: 0 },
        count,
    ));
    source
}

fn resident(cache: &RowAnalysisCache<'_>) -> usize {
    cache
        .programs
        .iter()
        .filter(|facts| facts.is_some())
        .count()
}

fn snapshot(
    analysis: Option<RowAnalysis>,
) -> Option<(Option<solve::TargetAssignmentShape>, bool, bool)> {
    analysis.map(|analysis| (analysis.shape, analysis.direct, analysis.exact))
}

#[test]
fn retirement_preserves_interleaved_queries_and_releases_seven_million_register_facts() {
    let wide = |count, outputs| {
        vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::P,
                input_start: 0,
                count,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: outputs,
                stride: 1,
            },
        ]
    };
    let mut programs = vec![wide(7_000_000, 9)];
    programs.extend((0..8).map(|_| wide(14400, 1)));
    let mut outputs = (0..17).step_by(2).collect::<Vec<_>>();
    outputs.extend((1..16).step_by(2));
    let problem = problem_with_programs(
        programs,
        outputs,
        (0..17)
            .map(|index| Some(solve::scalar_slot_y(index)))
            .collect(),
    );
    let catalog =
        CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs).unwrap();
    let mut retiring = RowAnalysisCache::with_last_candidates(
        catalog.len(),
        refresh_row_candidates(&problem, &catalog)
            .map(|candidate| (candidate.position.program_index, candidate.equation)),
        Some(span()),
    )
    .unwrap();
    // The pre-repair cache retains every program's facts until plan completion.
    let mut retaining = RowAnalysisCache::default();
    let mut peak = 0;
    for candidate in refresh_row_candidates(&problem, &catalog) {
        let indices = (
            candidate.position.program_index,
            candidate.equation,
            candidate.position.output_offset,
            candidate.target,
        );
        let actual = analyze_refresh_row(candidate.program, indices, None, &mut retiring).unwrap();
        let previous =
            analyze_refresh_row(candidate.program, indices, None, &mut retaining).unwrap();
        assert_eq!(snapshot(actual), snapshot(previous));
        peak = peak.max(resident(&retiring));
        retiring.finish_candidate(indices.0, indices.1);
        if candidate.equation < 16 {
            assert!(
                retiring.programs[0].is_some(),
                "interleaving retains the long-lived source"
            );
        }
    }
    assert_eq!(
        peak, 2,
        "one interleaved source plus the current single-use source"
    );
    assert_eq!(
        resident(&retaining),
        9,
        "baseline keeps all nine source proofs"
    );
    assert_eq!(
        resident(&retiring),
        0,
        "final query drops complete and scoped proof storage"
    );
    assert_eq!(retiring.fact_builds, 9, "no interleaved source rebuild");
    assert_eq!(retaining.fact_builds, 9);
}

#[test]
fn final_refused_query_retires_facts_without_changing_refusal_or_first_successful_claim() {
    let programs = vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        constant_outputs(4),
    ];
    let mut problem = problem_with_programs(
        programs,
        vec![0, 2, 1, 3, 4, 5],
        vec![
            Some(solve::scalar_slot_y(0)),
            Some(solve::scalar_slot_y(0)),
            Some(solve::scalar_slot_y(0)),
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
            Some(solve::scalar_slot_y(0)),
        ],
    );
    problem.solve_layout.algebraic_scalar_count = 3;
    problem.solve_layout.solver_maps.names.truncate(3);
    let catalog =
        CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs).unwrap();
    let mut cache =
        RowAnalysisCache::with_last_candidates(catalog.len(), [(0, 2), (1, 5)], Some(span()))
            .unwrap();
    for candidate in refresh_row_candidates(&problem, &catalog) {
        let indices = (
            candidate.position.program_index,
            candidate.equation,
            candidate.position.output_offset,
            candidate.target,
        );
        let analysis = analyze_refresh_row(candidate.program, indices, None, &mut cache).unwrap();
        if indices.0 == 0 {
            assert!(
                analysis.is_none(),
                "another target's direct shape refuses this target"
            );
        }
        cache.finish_candidate(indices.0, indices.1);
        if candidate.equation == 2 {
            assert!(
                cache.programs[0].is_none(),
                "the last refusal releases its source"
            );
        }
    }
    assert_eq!(resident(&cache), 0);
    let plan =
        super::super::build_canonical_algebraic_refresh_plan(&problem, &catalog, None).unwrap();
    assert_eq!(
        plan.rows
            .iter()
            .map(|row| (
                row.equation_index(),
                row.target_index(),
                row.source().program(),
                row.output_offset()
            ))
            .collect::<Vec<_>>(),
        vec![(1, 0, 1, 0), (3, 1, 1, 1), (4, 2, 1, 2)]
    );
}

#[test]
fn candidate_inventory_excludes_state_parameter_none_outside_and_missing_outputs() {
    let mut problem = problem_with_programs(
        vec![constant_outputs(4), constant_outputs(3)],
        vec![0, 2, 5, 7, 1, 3, 6],
        vec![
            Some(solve::scalar_slot_y(0)),
            Some(solve::scalar_slot_p(0)),
            None,
            Some(solve::scalar_slot_y(99)),
            Some(solve::scalar_slot_y(2)),
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
            Some(solve::scalar_slot_y(3)),
            Some(solve::scalar_slot_y(4)),
        ],
    );
    problem.solve_layout.state_scalar_count = 1;
    problem.solve_layout.algebraic_scalar_count = 4;
    problem.solve_layout.solver_maps.names.truncate(5);
    let catalog =
        CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs).unwrap();
    let candidates = refresh_row_candidates(&problem, &catalog)
        .map(|candidate| {
            (
                candidate.equation,
                candidate.target,
                candidate.position.program_index,
                candidate.position.output_offset,
            )
        })
        .collect::<Vec<_>>();
    assert_eq!(candidates, vec![(5, 1, 0, 2), (6, 2, 1, 2), (7, 3, 0, 3)]);
    let cache = RowAnalysisCache::with_last_candidates(
        catalog.len(),
        candidates
            .iter()
            .map(|&(equation, _, program, _)| (program, equation)),
        Some(span()),
    )
    .unwrap();
    assert_eq!(cache.last_candidates, [Some(7), Some(6)]);
    assert_eq!(
        catalog.len(),
        2,
        "program capacity counts owners, not seven scalar outputs"
    );
    assert_eq!(catalog.positions().len(), 7);
    for index in 0..2 {
        let program = catalog.program(index).unwrap();
        assert_eq!(catalog.source_index(program.source), Some(index));
        assert_eq!(program.span, span());
    }
}

#[test]
fn candidate_metadata_capacity_and_missing_program_fail_with_checked_errors() {
    assert!(RowAnalysisCache::with_last_candidates(usize::MAX, [], Some(span())).is_err());
    assert!(RowAnalysisCache::with_last_candidates(1, [(1, 0)], Some(span())).is_err());
    assert_eq!(
        resident(&RowAnalysisCache::with_last_candidates(0, [], None).unwrap()),
        0
    );
}

#[test]
fn block_program_reservations_preserve_local_cursors_and_canonical_sources() {
    let scalar = |count| {
        solve::ScalarProgramBlock::with_source_span(
            (0..count).map(|_| constant_outputs(3)).collect(),
            span()
                .require_provenance("catalog reservation fixture")
                .unwrap(),
        )
        .unwrap()
    };
    let block = solve::ComputeBlock {
        nodes: vec![
            solve::ComputeNode::ScalarPrograms(scalar(32)),
            solve::ComputeNode::ScalarPrograms(scalar(3)),
        ],
    };
    let catalog = CanonicalScalarProgramCatalog::construct(&block).unwrap();
    assert_eq!(catalog.len(), 35);
    assert_eq!(catalog.positions().len(), 105);
    for output in 0..105 {
        let position = catalog.positions()[&output];
        let expected_program = output / 3;
        assert_eq!(position.program_index, expected_program);
        assert_eq!(position.output_offset, output % 3);
        let program = catalog.program(expected_program).unwrap();
        let (node, index) = if expected_program < 32 {
            (0, expected_program)
        } else {
            (1, expected_program - 32)
        };
        assert_eq!(
            program.source,
            solve::RefreshScalarProgramSource::checked(node, index).unwrap()
        );
        assert_eq!(catalog.source_index(program.source), Some(expected_program));
        assert_eq!(program.span, span());
        assert!(catalog.produces_output(output));
    }
    assert!(!catalog.produces_output(105));
}

#[test]
fn prior_reused_last_query_releases_facts_built_for_earlier_outputs() {
    let mut problem = problem_with_programs(
        vec![constant_outputs(3)],
        vec![0, 1, 2],
        (0..3)
            .map(|index| Some(solve::scalar_slot_y(index)))
            .collect(),
    );
    problem.continuous.refresh_owners =
        super::super::build_continuous_refresh_owners(&mut problem).unwrap();
    let prior = PriorRowAnalysis::new(&problem).unwrap();
    let catalog =
        CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs).unwrap();
    let mut cache = RowAnalysisCache::with_last_candidates(1, [(0, 2)], Some(span())).unwrap();
    for candidate in refresh_row_candidates(&problem, &catalog) {
        let indices = (
            0,
            candidate.equation,
            candidate.position.output_offset,
            candidate.target,
        );
        let reused = (candidate.equation == 2).then_some(&prior);
        let actual = analyze_refresh_row(candidate.program, indices, reused, &mut cache).unwrap();
        let independent = analyze_refresh_row(
            candidate.program,
            indices,
            None,
            &mut RowAnalysisCache::default(),
        )
        .unwrap();
        assert_eq!(snapshot(actual), snapshot(independent));
        assert_eq!(cache.fact_builds, 1);
        cache.finish_candidate(0, candidate.equation);
    }
    assert_eq!(resident(&cache), 0);
}
