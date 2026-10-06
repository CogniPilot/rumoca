use super::*;
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("portable_me.mo"), 3, 17)
}

fn block(programs: Vec<Vec<LinearOp>>, outputs: Vec<usize>) -> ScalarProgramBlock {
    let spans = vec![span(); programs.len()];
    ScalarProgramBlock::with_output_indices(programs, spans, outputs).unwrap()
}

fn scalar(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn layout() -> VarLayout {
    VarLayout::from_parts(Default::default(), 2, 1)
}

#[test]
fn portable_me_declines_unsupported_model_context_before_whole_call_admission() {
    use rumoca_solver::SimExecutionPolicy::{Auto, Interpreter};
    // A scalar block alone is eligible even in a model whose unrelated tables
    // cannot be admitted by the whole-call interface. Decline at composition.
    let source = block(vec![scalar(2.0)], vec![0]);
    assert!(single_program(&source, 0, &layout()).is_ok());
    assert!(model_context_admitted(Auto, 1, 0));
    assert!(!model_context_admitted(Auto, 1, 1));
    assert!(!model_context_admitted(Auto, 0, 0));
    assert!(!model_context_admitted(Interpreter, 1, 0));
}

#[test]
fn portable_me_retains_original_program_span_and_signed_zero() {
    let source = block(vec![scalar(-0.0)], vec![7]);
    let admitted = single_program(&source, 0, &layout()).unwrap();
    assert_eq!(admitted.program_span(0), source.program_span(0));
    assert_eq!(admitted.output_indices(), &[0]);
    let LinearOp::Const { value, .. } = admitted.programs()[0][0] else {
        panic!("constant changed")
    };
    assert_eq!(value.to_bits(), (-0.0_f64).to_bits());
    assert_eq!(source.output_indices(), &[7]);
}

#[test]
fn portable_me_selection_keeps_original_indices_around_refused_program() {
    let indexed = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::LoadIndexedP {
            dst: 1,
            base: 0,
            count: 1,
            index: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let source = block(vec![scalar(4.0), indexed, scalar(9.0)], vec![5, 1, 8]);
    assert!(single_program(&source, 0, &layout()).is_ok());
    assert!(single_program(&source, 1, &layout()).is_err());
    assert!(single_program(&source, 2, &layout()).is_ok());
    assert_eq!(source.output_indices(), &[5, 1, 8]);
    assert!(single_program(&source, 3, &layout()).is_err());
}

#[test]
fn portable_me_declines_nan_sensitive_extrema_in_complete_prefix() {
    for op in [BinaryOp::Min, BinaryOp::Max] {
        let source = block(
            vec![vec![
                LinearOp::Const {
                    dst: 0,
                    value: f64::NAN,
                },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![0],
        );
        assert!(single_program(&source, 0, &layout()).is_err());
        let mut used_ops = source.programs()[0].clone();
        *used_ops.last_mut().unwrap() = LinearOp::StoreOutput { src: 2 };
        let used = block(vec![used_ops], vec![0]);
        let mut result = [0.0];
        rumoca_eval_solve::eval_scalar_program_block(
            &used,
            &[0.0; 2],
            &[0.0],
            0.0,
            None,
            &mut result,
        )
        .unwrap();
        assert_eq!(result, [2.0]);
        assert!(single_program(&used, 0, &layout()).is_err());
    }
}

#[test]
fn portable_me_declines_multi_output_and_dense_linear_solve() {
    let source = block(
        vec![vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![0, 1],
    );
    assert!(single_program(&source, 0, &layout()).is_err());
    let source = block(
        vec![vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::Const { dst: 1, value: 8.0 },
            LinearOp::LinearSolveComponent {
                dst: 2,
                matrix_start: 0,
                rhs_start: 1,
                n: 1,
                component: 0,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![0],
    );
    assert!(single_program(&source, 0, &layout()).is_err());
}

#[test]
fn portable_me_checks_every_load_against_the_issued_layout() {
    for op in [
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::LoadP { dst: 0, index: 1 },
    ] {
        let source = block(
            vec![vec![
                op,
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![0],
        );
        assert!(single_program(&source, 0, &layout()).is_err());
    }
    let source = block(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![0],
    );
    assert!(single_program(&source, 0, &layout()).is_ok());
}

#[test]
fn portable_me_never_zero_pads_short_inputs_or_ignores_external_tables() {
    assert!(validate_inputs(&layout(), 2, 1, 0).is_ok());
    for (y, p, tables) in [(1, 1, 0), (3, 1, 0), (2, 0, 0), (2, 2, 0), (2, 1, 1)] {
        assert!(validate_inputs(&layout(), y, p, tables).is_err());
    }
}

#[test]
#[ignore = "requires the exact complete application source through RUMOCA_WASM_ME_SOURCE_FIXTURE"]
fn portable_me_actual_full_source_admission_inventory() {
    let path = std::env::var("RUMOCA_WASM_ME_SOURCE_FIXTURE").unwrap();
    let source = std::fs::read_to_string(path).unwrap();
    let model = std::env::var("RUMOCA_WASM_ME_SOURCE_MODEL").unwrap();
    let mut session = rumoca_compile::Session::default();
    session.add_document("portable_me.mo", &source).unwrap();
    let compiled = session.compile_model(&model).unwrap();
    let package = rumoca_phase_solve::lower_solve_package(&compiled.dae).unwrap();
    let problem = &package.problem;
    let blocks = [
        (
            "implicit_rhs",
            rumoca_eval_solve::to_scalar_program_block(&problem.continuous.implicit_rhs).unwrap(),
        ),
        (
            "derivative_rhs",
            rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs).unwrap(),
        ),
        ("root_conditions", problem.events.root_conditions.clone()),
    ];
    let inventory = blocks
        .iter()
        .map(|(name, block)| block_inventory(name, block, &problem.layout))
        .collect::<Vec<_>>();
    let report = serde_json::json!({"model":model,"source":source,"inventory":inventory,
        "issuedRefreshSchedules":refresh_inventory(problem),
        "preparedTargetValuePlans":target_value_inventory(problem,&package.pure_calls),
        "layout":{"y":problem.layout.y_scalars(),"p":problem.layout.p_scalars()},
        "scope":"Source-issued scalar admission inventory only; not runtime execution or full compiled RHS"});
    std::fs::write(
        std::env::var("RUMOCA_WASM_ME_REPORT").unwrap(),
        serde_json::to_vec_pretty(&report).unwrap(),
    )
    .unwrap();
}

fn refresh_inventory(problem: &rumoca_ir_solve::SolveProblem) -> Vec<serde_json::Value> {
    let owners = &problem.continuous.refresh_owners;
    [
        ("algebraic", owners.algebraic()),
        ("derivative", owners.derivative()),
        ("root", owners.root()),
        ("event", owners.event()),
    ]
    .into_iter()
    .map(|(name, plan)| plan_inventory(name, plan, problem))
    .collect()
}

fn plan_inventory(
    name: &str,
    plan: &rumoca_ir_solve::RefreshPlan,
    problem: &rumoca_ir_solve::SolveProblem,
) -> serde_json::Value {
    let schedules = plan
        .possible_stage_schedules()
        .iter()
        .map(|&dispatch| {
            let steps = problem
                .continuous
                .refresh_owners
                .staged_refresh_steps(plan, dispatch);
            let (steps, refusal) = match steps {
                Ok(steps) => (
                    steps
                        .into_iter()
                        .map(|step| step_inventory(step, problem))
                        .collect::<Vec<_>>(),
                    None,
                ),
                Err(error) => (Vec::new(), Some(format!("{error:?}"))),
            };
            serde_json::json!({"dispatch":format!("{dispatch:?}"),"steps":steps,"refusal":refusal})
        })
        .collect::<Vec<_>>();
    serde_json::json!({"plan":name,"scope":"all constructor-possible dispatches, not measured runtime hit share","schedules":schedules})
}

fn step_inventory(
    step: rumoca_ir_solve::StagedRefreshStep<'_>,
    problem: &rumoca_ir_solve::SolveProblem,
) -> serde_json::Value {
    match step {
        rumoca_ir_solve::StagedRefreshStep::Assignments(schedule) => {
            schedule_inventory(schedule, problem)
        }
        rumoca_ir_solve::StagedRefreshStep::Project {
            block_index, seeds, ..
        } => {
            serde_json::json!({"kind":"Project","block":block_index,"seeds":seeds.map(|schedule|schedule_inventory(schedule,problem))})
        }
        rumoca_ir_solve::StagedRefreshStep::ProjectComplete => {
            serde_json::json!({"kind":"ProjectComplete"})
        }
    }
}

fn schedule_inventory(
    schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
    problem: &rumoca_ir_solve::SolveProblem,
) -> serde_json::Value {
    let owners = &problem.continuous.refresh_owners;
    let programs = schedule.program_ids().iter().map(|&id| {
        let owner = owners.exact_assignment_program(id).unwrap();
        let block = owner.final_scalar_program(&problem.continuous.implicit_rhs).unwrap();
        let admitted = block.row_count()==1 && single_program(&block,0,&problem.layout).is_ok()
            && owner.target_indices().len()==1 && owner.target_indices()[0]<problem.layout.y_scalars();
        serde_json::json!({"identity":format!("{id:?}"),"targets":owner.target_indices(),"admittedScalarProfile":admitted,
            "programs":(0..block.row_count()).map(|index|program_inventory(&block,index,&problem.layout)).collect::<Vec<_>>()})
    }).collect::<Vec<_>>();
    serde_json::json!({"kind":"Assignments","identity":format!("{:?}",schedule.sequence_id()),
        "allProgramsAdmittedScalarProfile":programs.iter().all(|program|program["admittedScalarProfile"]==true),"programs":programs})
}

fn block_inventory(
    name: &str,
    block: &ScalarProgramBlock,
    layout: &VarLayout,
) -> serde_json::Value {
    let rows = (0..block.row_count())
        .map(|program| program_inventory(block, program, layout))
        .collect::<Vec<_>>();
    serde_json::json!({"block":name,"programs":block.row_count(),"outputs":block.len(),
        "admitted":rows.iter().filter(|row|row["admitted"]==true).count(),"rows":rows})
}

fn program_inventory(
    block: &ScalarProgramBlock,
    program: usize,
    layout: &VarLayout,
) -> serde_json::Value {
    let ops = block.program(program).unwrap();
    let result = single_program(block, program, layout);
    serde_json::json!({"program":program,"outputs":ScalarProgramBlock::program_output_count(ops),
        "operations":ops.len(),"span":block.program_span(program),"admitted":result.is_ok(),"refusal":result.err()})
}

fn target_value_inventory(
    problem: &rumoca_ir_solve::SolveProblem,
    calls: &rumoca_ir_solve::SolvePureCallTable,
) -> serde_json::Value {
    let rows=problem.continuous.refresh_owners.algebraic().rows.iter().map(|row| {
        let source=row.source();
        let Some(rumoca_ir_solve::ComputeNode::ScalarPrograms(block))=problem.continuous.implicit_rhs.nodes.get(source.node() as usize) else {
            return serde_json::json!({"owner":format!("{:?}",row.owner_id()),"disposition":"original source node is not a scalar program"});
        };
        let prepared=rumoca_eval_solve::PreparedScalarProgramBlock::new(block.clone()).unwrap();
        let program=source.program() as usize;
        let plan=prepared.portable_target_value_plan(program,row.output_offset(),row.target_index()).unwrap();
        let Some(plan)=plan else {return serde_json::json!({"owner":format!("{:?}",row.owner_id()),"source":source,"output":row.output_offset(),"target":row.target_index(),"disposition":"checked target is outside direct/zero profile"});};
        let compiled=rumoca_exec_wasm::compile_private_program_wasm(plan.program(),&problem.layout,calls);
        let disposition=match compiled {Ok(compiled)=>serde_json::json!({"admitted":true,"moduleBytes":compiled.module_bytes().len(),"faults":format!("{:?}",compiled.faults()),"scratchBytes":compiled.scratch_bytes(),"outputs":compiled.output_count()}),Err(error)=>serde_json::json!({"admitted":false,"refusal":error.to_string()})};
        let call_sites=plan.program().programs()[0].iter().filter_map(|operation|match operation {rumoca_ir_solve::LinearOp::PureCall {site,..}=>Some(format!("{:?}",site)),_=>None}).collect::<Vec<_>>();
        serde_json::json!({"owner":format!("{:?}",row.owner_id()),"source":source,"output":row.output_offset(),"target":row.target_index(),"canonicalPrefix":plan.canonical_prefix_len(),"privateSelection":plan.private_result_offset(),"privateOutputs":plan.program().stored_output_count(),"completeCallSites":call_sites,"span":format!("{:?}",block.program_span(program)),"entry":disposition})
    }).collect::<Vec<_>>();
    serde_json::json!({"scope":"constructor-issued algebraic source target inventory, not measured runtime hits","rows":rows})
}
