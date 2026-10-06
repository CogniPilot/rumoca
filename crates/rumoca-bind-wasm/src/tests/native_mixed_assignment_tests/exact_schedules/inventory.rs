//! Record complete issued dispatches; a refusal never becomes a smaller pass.
use super::*;

pub(super) fn inspect(model: &solve::SolveModel, source: &str, directory: &Path) -> Value {
    assert_eq!(model.problem.solve_layout.state_scalar_count(), 21);
    assert_eq!(model.initial_y.len(), model.problem.layout.y_scalars());
    assert_eq!(model.parameters.len(), model.problem.layout.p_scalars());
    let owners = &model.problem.continuous.refresh_owners;
    let cases = fixtures::cases(model);
    let mut identities = BTreeSet::new();
    let mut sequences = Vec::new();
    let mut dispatches = Vec::new();
    let plans = [
        ("algebraic", owners.algebraic()),
        ("derivative", owners.derivative()),
        ("root", owners.root()),
        ("event", owners.event()),
    ]
    .into_iter()
    .map(|(n, p)| (n.to_owned(), p))
    .chain(
        owners
            .clock_events()
            .iter()
            .enumerate()
            .map(|(i, p)| (format!("clock-{i}"), p)),
    );
    for (name, plan) in plans {
        for &dispatch in plan.possible_stage_schedules() {
            let steps = match owners.staged_refresh_steps(plan, dispatch) {
                Ok(steps) => steps,
                Err(error) => {
                    dispatches.push(json!({"plan":name,"dispatch":format!("{dispatch:?}"),"refusal":format!("{error:?}")}));
                    continue;
                }
            };
            let entries = steps
                .into_iter()
                .map(|step| {
                    step_report(
                        &Inventory {
                            model,
                            source,
                            cases: &cases,
                            directory,
                        },
                        step,
                        &mut identities,
                        &mut sequences,
                    )
                })
                .collect::<Vec<_>>();
            dispatches
                .push(json!({"plan":name,"dispatch":format!("{dispatch:?}"),"steps":entries}));
        }
    }
    let admitted = sequences.iter().filter(|s| s["admitted"] == true).count();
    let failures = sequences
        .iter()
        .filter_map(|s| s["numerical_failures"].as_u64())
        .sum::<u64>();
    json!({"source_sha256":digest(source.as_bytes()),"y_count":model.problem.layout.y_scalars(),"p_count":model.problem.layout.p_scalars(),"state_count":21,
        "pure_call_owners":model.pure_calls.owners().len(),"dispatches":dispatches,"sequences":sequences,"admitted_sequences":admitted,
        "refused_sequences":sequences.len()-admitted,"numerical_failures":failures,"coverage_scope":"All possible constructor dispatches; static coverage, not runtime hits or complete native RHS"})
}

fn sequence(
    model: &solve::SolveModel,
    source: &str,
    schedule: &solve::ExactRefreshAssignmentSchedule,
    cases: &[fixtures::Case],
    directory: &Path,
    ordinal: usize,
) -> Value {
    let problem = &model.problem;
    let compiled = match rumoca_exec_wasm::compile_exact_assignment_schedule_wasm(
        &problem.continuous.implicit_rhs,
        &problem.continuous.refresh_owners,
        schedule,
        &problem.layout,
        &model.pure_calls,
    ) {
        Ok(compiled) => compiled,
        Err(error) => {
            return json!({"ordinal":ordinal,"identity":format!("{:?}",schedule.sequence_id()),"programs":schedule.program_ids().len(),"admitted":false,"refusal":error.to_string(),"cases":[]});
        }
    };
    let basename = format!("sequence-{ordinal}");
    std::fs::write(
        directory.join(format!("{basename}.wasm")),
        compiled.module_bytes(),
    )
    .unwrap();
    let mut runner = execution::Execution::new(
        compiled.module_bytes(),
        compiled.scratch_bytes(),
        &problem.layout,
    );
    let results = cases
        .iter()
        .map(|case| numerical(model, schedule, case, &mut runner, directory, &basename))
        .collect::<Vec<_>>();
    let failures = results.iter().filter(|r| r["passed"] != true).count();
    let manifest = json!({"source_sha256":digest(source.as_bytes()),"module_sha256":digest(compiled.module_bytes()),
        "layout":{"y_bytes":problem.layout.y_scalars()*8,"p_bytes":problem.layout.p_scalars()*8,"scratch_bytes":compiled.scratch_bytes()},
        "abi":{"export":"eval_assignments","memory_import":"env.memory","arguments":["yPtr:i32","pPtr:i32","time:f64","scratchPtr:i32","reservedZero:i32"],"result":"status:i32"},
        "math_imports":execution::math_imports(compiled.module_bytes()),"producer_manifest":"../producer.json",
        "faults":compiled.faults().iter().map(|f|format!("{f:?}")).collect::<Vec<_>>()});
    std::fs::write(
        directory.join(format!("{basename}.json")),
        serde_json::to_vec_pretty(&manifest).unwrap(),
    )
    .unwrap();
    json!({"ordinal":ordinal,"identity":format!("{:?}",schedule.sequence_id()),"programs":schedule.program_ids().len(),"admitted":true,"module_bytes":compiled.module_bytes().len(),"scratch_bytes":compiled.scratch_bytes(),"numerical_failures":failures,"cases":results})
}

fn numerical(
    model: &solve::SolveModel,
    schedule: &solve::ExactRefreshAssignmentSchedule,
    case: &fixtures::Case,
    runner: &mut execution::Execution,
    directory: &Path,
    basename: &str,
) -> Value {
    let expected = match execution::canonical(model, schedule, case) {
        Ok(bytes) => bytes,
        Err(error) => return json!({"name":case.name,"passed":false,"canonical_refusal":error}),
    };
    let result = runner.run(case);
    let actual = &result.output;
    let stem = format!("{basename}-{}", case.name);
    for (suffix, bytes) in [
        ("input-y", fixtures::bytes(&case.y)),
        ("input-p", fixtures::bytes(&case.p)),
        ("expected-y", expected.clone()),
        ("actual-y", actual.clone()),
    ] {
        std::fs::write(directory.join(format!("{stem}-{suffix}.bin")), bytes).unwrap();
    }
    json!({"name":case.name,"time":case.time,"status":result.status,"trap":result.trap,"parameter_immutable":result.parameter_immutable,"guards_unchanged":result.guards_unchanged,
        "passed":result.status==Some(0) && *actual==expected && result.parameter_immutable && result.guards_unchanged,
        "compared_y_bytes":expected.len(),"expected_y_sha256":digest(&expected),"actual_y_sha256":digest(actual),
        "first_mismatch_byte":actual.iter().zip(&expected).position(|(a,b)|a!=b),"fixture_basename":stem})
}

struct Inventory<'a> {
    model: &'a solve::SolveModel,
    source: &'a str,
    cases: &'a [fixtures::Case],
    directory: &'a Path,
}

fn step_report(
    context: &Inventory<'_>,
    step: solve::StagedRefreshStep<'_>,
    identities: &mut BTreeSet<solve::RefreshSequenceId>,
    sequences: &mut Vec<Value>,
) -> Value {
    let (kind, schedule) = match step {
        solve::StagedRefreshStep::Assignments(s) => ("Assignments", Some(s)),
        solve::StagedRefreshStep::Project { seeds, .. } => ("Project", seeds),
        solve::StagedRefreshStep::ProjectComplete => ("ProjectComplete", None),
    };
    let identity = schedule.map(|s| format!("{:?}", s.sequence_id()));
    if let Some(schedule) = schedule
        && identities.insert(schedule.sequence_id())
    {
        let report = sequence(
            context.model,
            context.source,
            schedule,
            context.cases,
            context.directory,
            sequences.len(),
        );
        sequences.push(report);
    }
    json!({"kind":kind,"sequence":identity})
}
