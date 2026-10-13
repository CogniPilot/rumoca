use super::*;
use rumoca_eval_solve::{
    PreparedComputeBlock, RowEvalContext, TypedValue, eval_pure_call, eval_row_with_context,
};
use rumoca_ir_solve::{ComputeNode, LinearOp, SolveValueKind};

#[test]
fn asserted_guarded_loop_values_are_correct_in_canonical_dae() {
    for (enabled, expected) in [("true", vec![0.0, 4.0, 6.0, 0.0]), ("false", vec![0.0; 4])] {
        let source = ASSERTED.replace("time < 0.5", enabled);
        assert_ne!(source, ASSERTED);
        assert_eq!(canonical_static_call_values(&source, "Asserted"), expected);
    }
}

#[test]
fn asserted_canonical_completed_loops_stay_in_their_function_invocation() {
    let (package, _) = ASSERTED.split_once("model Asserted").unwrap();
    let source = format!(
        "{package}
model AssertedRepeated
  Real first[W.n] = W.Scores({{1,2,3,4}}, true);
  Real middle[W.n] = W.Scores({{1,2,3,4}}, false);
  Real last[W.n] = W.Scores({{1,2,3,4}}, true);
end AssertedRepeated;"
    );
    let compiled = Compiler::new()
        .model("AssertedRepeated")
        .compile_str(&source, "AssertedRepeated.mo")
        .unwrap();
    compiled.dae.inspect(|view| {
        let calls = (0..view.expression_count())
            .map(|index| view.expression_id(index).unwrap())
            .filter_map(|id| {
                let expression = view.expression(id).unwrap();
                if expression.function_scope().is_some() {
                    return None;
                }
                let rumoca_ir_dae::ExpressionOperation::Call {
                    function,
                    arguments,
                    ..
                } = expression.operation()
                else {
                    return None;
                };
                let rumoca_ir_dae::ExpressionOperation::Literal(
                    rumoca_ir_dae::DaeLiteral::Boolean(enabled),
                ) = view
                    .expression(arguments.get(1).unwrap())
                    .unwrap()
                    .operation()
                else {
                    panic!("the original literal Boolean input is retained");
                };
                Some((id, function, *enabled))
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 3, "three original model-level checked calls");
        assert!(calls.iter().all(|call| call.1 == calls[0].1));
        let enabled = calls.iter().filter(|call| call.2).collect::<Vec<_>>();
        assert_eq!(enabled.len(), 2);
        let disabled = calls.iter().find(|call| !call.2).unwrap();
        let mut evaluator = rumoca_eval_dae::NumericEvaluator::new(view);
        for (call, expected) in [
            (enabled[0], vec![0.0, 4.0, 6.0, 0.0]),
            (disabled, vec![0.0; 4]),
            (enabled[1], vec![0.0, 4.0, 6.0, 0.0]),
        ] {
            assert_eq!(evaluator.expression(call.0).unwrap(), expected);
        }
    });
}

fn original_solve() -> rumoca_ir_solve::SolveModel {
    let compiled = Compiler::new()
        .model("Asserted")
        .compile_str(ASSERTED, "Asserted.mo")
        .unwrap();
    rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap()
}

fn context(solve: &rumoca_ir_solve::SolveModel) -> RowEvalContext<'_> {
    RowEvalContext {
        pure_calls: Some(&solve.pure_calls),
        external_tables: Some(solve.external_tables.as_slice()),
        ..Default::default()
    }
}

fn original_programs(solve: &rumoca_ir_solve::SolveModel) -> impl Iterator<Item = &[LinearOp]> {
    solve
        .problem
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .filter_map(|node| match node {
            ComputeNode::ScalarPrograms(block) => Some(block),
            _ => None,
        })
        .flat_map(|block| block.programs().iter().map(Vec::as_slice))
}

#[test]
fn asserted_original_model_packs_inputs_and_call_outputs_at_actual_time() {
    let solve = original_solve();
    let y = vec![0.0; solve.solver_scalar_count()];
    let mut calls = 0;
    for program in original_programs(&solve) {
        let Some((index, dst_start, input_starts, site)) =
            program
                .iter()
                .enumerate()
                .find_map(|(index, operation)| match operation {
                    LinearOp::PureCall {
                        dst_start,
                        input_starts,
                        site,
                    } => Some((index, *dst_start, input_starts, site)),
                    _ => None,
                })
        else {
            continue;
        };
        calls += 1;
        assert_eq!(site.inputs().len(), 2);
        assert_eq!(site.inputs()[0].scalar_count(), 4);
        assert_eq!(site.inputs()[1].scalar_count(), 1);
        assert_eq!(site.outputs()[0].value_type().scalar_count(), 4);
        let source_prefix = &program[..index];
        assert!(
            !source_prefix
                .iter()
                .any(|operation| matches!(operation, LinearOp::StoreOutput { .. }))
        );
        for (time, enabled, expected) in [
            (0.0, 1.0, vec![0.0, 4.0, 6.0, 0.0]),
            (1.0, 0.0, vec![0.0; 4]),
            (0.0, 1.0, vec![0.0, 4.0, 6.0, 0.0]),
        ] {
            let read = |prefix: &[LinearOp], register| {
                let mut selected = prefix.to_vec();
                selected.push(LinearOp::StoreOutput { src: register });
                eval_row_with_context(&selected, &y, &solve.parameters, time, context(&solve))
                    .unwrap()
            };
            for (offset, value) in [1.0, 2.0, 3.0, 4.0].into_iter().enumerate() {
                assert_eq!(
                    read(source_prefix, input_starts[0] + offset as u32),
                    value,
                    "original rgb packing at t={time}"
                );
            }
            assert_eq!(
                read(source_prefix, input_starts[1]),
                enabled,
                "original enabled packing at t={time}"
            );
            for (offset, value) in expected.into_iter().enumerate() {
                assert_eq!(
                    read(&program[..=index], dst_start + offset as u32),
                    value,
                    "original flattened result {offset} at t={time}"
                );
            }
        }
    }
    assert!(
        calls > 0,
        "the unchanged model retains its actual pure call"
    );
}

#[test]
fn asserted_original_continuous_residual_retains_all_result_slots() {
    let solve = original_solve();
    assert_eq!(solve.solver_scalar_count(), 4);
    let source = &solve.problem.continuous.implicit_rhs;
    let prepared = PreparedComputeBlock::new(source).unwrap();
    let mut residual = vec![f64::NAN; source.len().unwrap()];
    let mut expected_y = vec![f64::NAN; 4];
    for (index, value) in [0.0, 4.0, 6.0, 0.0].into_iter().enumerate() {
        let name = format!("scores[{}]", index + 1);
        let slot = solve.problem.solver_index(&name).unwrap();
        expected_y[slot] = value;
    }
    for (time, y) in [(0.0, expected_y), (1.0, vec![0.0; 4])] {
        prepared
            .eval_with_context(&y, &solve.parameters, time, context(&solve), &mut residual)
            .unwrap();
        assert_eq!(
            residual,
            vec![0.0; 4],
            "original model equation at t={time}"
        );
    }
    prepared
        .eval_with_context(
            &[0.0; 4],
            &solve.parameters,
            0.0,
            context(&solve),
            &mut residual,
        )
        .unwrap();
    let mut magnitudes = residual.iter().map(|value| value.abs()).collect::<Vec<_>>();
    magnitudes.sort_by(f64::total_cmp);
    assert_eq!(
        magnitudes,
        [0.0, 0.0, 4.0, 6.0],
        "original model residual at zero Y and t=0"
    );
}

#[test]
fn asserted_original_runtime_refresh_retains_declared_output_coverage() {
    let solve = original_solve();
    let layout = &solve.problem.solve_layout;
    let owners = &solve.problem.continuous.refresh_owners;
    let plan = owners.algebraic();
    let rows = plan
        .rows
        .iter()
        .map(|row| {
            (
                row.equation_index(),
                row.source().node(),
                row.source().program(),
                row.output_offset(),
                row.target_index(),
                row.assignment_target(),
                row.direct_assignment_certified(),
                row.exact_assignment_certified(),
            )
        })
        .collect::<Vec<_>>();
    let inventory = format!(
        "state={} algebraic={} output={} compact={} rows={rows:?} blocks={} causal={} static={} dynamic={} stages={}",
        layout.state_scalar_count(),
        layout.algebraic_scalar_count,
        layout.output_scalar_count,
        owners.has_compact_source_recipe(),
        plan.simultaneous_plan.blocks.len(),
        plan.causal_solution_certified,
        plan.static_causal_rows().len(),
        plan.dynamic_causal_rows().len(),
        plan.value_stages.len(),
    );
    let runtime = rumoca_solver::SolveRuntime::new(&solve).unwrap();
    for (time, expected) in [
        (0.0, [0.0, 4.0, 6.0, 0.0]),
        (1.0, [0.0; 4]),
        (0.0, [0.0, 4.0, 6.0, 0.0]),
    ] {
        // Match the original eval_dae_at probe's private refresh settings.
        let report = runtime.eval_at(time, &[], &solve.parameters, 1e-10, 32);
        assert!(report.error.is_none(), "{:?}; {inventory}", report.error);
        let actual = (1..=4)
            .map(|index| {
                let name = format!("scores[{index}]");
                report
                    .solver_y
                    .iter()
                    .find(|slot| slot.name.replace(' ', "") == name)
                    .unwrap()
                    .value
            })
            .collect::<Vec<_>>();
        assert_eq!(actual, expected, "original runtime t={time}; {inventory}");
    }
}

#[test]
fn asserted_guarded_loop_values_are_correct_in_original_typed_owner() {
    let compiled = Compiler::new()
        .model("Asserted")
        .compile_str(ASSERTED, "Asserted.mo")
        .unwrap();
    let solve =
        rumoca_sim::lower_dae_for_simulation(&compiled.dae, &SimOptions::default()).unwrap();
    let mut sites = Vec::new();
    for operation in solve
        .problem
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .filter_map(|node| match node {
            ComputeNode::ScalarPrograms(block) => Some(block),
            _ => None,
        })
        .flat_map(|block| block.programs())
        .flatten()
    {
        if let LinearOp::PureCall { site, .. } = operation
            && !sites
                .iter()
                .any(|candidate: &rumoca_ir_solve::SolvePureCallSite| {
                    candidate.owner() == site.owner()
                })
        {
            sites.push(site.clone());
        }
    }
    assert_eq!(sites.len(), 1, "one original source call owner");
    let site = &sites[0];
    assert_eq!(site.inputs().len(), 2, "original rgb and enabled inputs");
    let rgb = TypedValue::construct(
        site.inputs()[0].clone(),
        [1.0_f64, 2.0, 3.0, 4.0]
            .into_iter()
            .map(|value| SolveValueKind::Real64(value.to_bits()))
            .collect(),
    )
    .unwrap();
    for (enabled, expected) in [
        (true, vec![0.0, 4.0, 6.0, 0.0]),
        (false, vec![0.0; 4]),
        (true, vec![0.0, 4.0, 6.0, 0.0]),
    ] {
        let enabled = TypedValue::construct(
            site.inputs()[1].clone(),
            vec![SolveValueKind::Boolean(enabled)],
        )
        .unwrap();
        let output =
            eval_pure_call(&solve.pure_calls, site.owner(), &[rgb.clone(), enabled]).unwrap();
        let actual = output[0]
            .elements()
            .iter()
            .map(|value| match value {
                SolveValueKind::Real64(bits) => f64::from_bits(*bits),
                _ => panic!("original Scores result is Binary64 Real"),
            })
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
    }
}
