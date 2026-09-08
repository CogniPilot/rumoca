use super::*;

macro_rules! assert_not_implemented {
    ($ty:ty, $bound:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }

            impl<T: ?Sized> AmbiguousIfImplemented<()> for T {}

            struct Implements;
            impl<T: ?Sized + $bound> AmbiguousIfImplemented<Implements> for T {}

            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

macro_rules! assert_admitted_carrier_traits {
    ($ty:ty) => {
        assert_not_implemented!($ty, ::core::default::Default);
        assert_not_implemented!($ty, ::core::marker::Copy);
        assert_not_implemented!($ty, ::core::ops::DerefMut);
        assert_not_implemented!($ty, ::core::convert::AsMut<[AdmittedLinearOp]>);
        assert_not_implemented!($ty, ::core::borrow::BorrowMut<[AdmittedLinearOp]>);
        assert_not_implemented!($ty, ::core::convert::From<Vec<LinearOp>>);
        assert_not_implemented!($ty, ::core::convert::TryFrom<Vec<LinearOp>>);
    };
}

assert_admitted_carrier_traits!(AdmittedLinearOp);
assert_admitted_carrier_traits!(AdmittedProgram);
assert_admitted_carrier_traits!(AdmittedFunctionFoldProgram);
assert_admitted_carrier_traits!(AdmittedFunctionConditionalArmProgram);
assert_admitted_carrier_traits!(AdmittedFunctionConditionalProgram);
assert_admitted_carrier_traits!(AdmittedExecutionProgram);
assert_admitted_carrier_traits!(AdmittedAssignmentProgram<'static>);

#[test]
fn compiler_rejects_generic_admitted_carrier_construction_and_mutation_traits() {
    // The compile-time assertions above are the evidence. This named test
    // keeps the trait gate visible in filtered Cranelift test output.
}

fn fixture_scalar_program_block(
    rows: Vec<Vec<LinearOp>>,
) -> Result<ScalarProgramBlock, CompileError> {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exec_cranelift_emit_fixture.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_source_span(
        rows,
        span.require_provenance("Cranelift emission fixture")
            .map_err(|error| CompileError::Input(error.to_string()))?,
    )
    .map_err(|error| CompileError::Input(error.to_string()))
}

fn compile_residual_rows(rows: &[Vec<LinearOp>]) -> Result<CompiledResidualRows, CompileError> {
    let block = fixture_scalar_program_block(rows.to_vec())?;
    super::compile_residual_rows(&block)
}

fn compile_jacobian_rows(rows: &[Vec<LinearOp>]) -> Result<CompiledJacobianRows, CompileError> {
    let block = fixture_scalar_program_block(rows.to_vec())?;
    super::compile_jacobian_rows(&block)
}

fn plan_fixture_row(row: &[LinearOp]) -> Result<RowPlan, CompileError> {
    let block = fixture_scalar_program_block(vec![row.to_vec()])?;
    let execution = block.sole_execution_program().ok_or_else(|| {
        CompileError::Backend("checked row fixture did not retain one program".to_string())
    })?;
    plan_scalar_program(execution)
}

fn admitted_fixture_row(row: Vec<LinearOp>) -> Result<AdmittedExecutionProgram, CompileError> {
    let block = fixture_scalar_program_block(vec![row])?;
    let execution = block.sole_execution_program().ok_or_else(|| {
        CompileError::Backend("checked row fixture did not retain one program".to_string())
    })?;
    AdmittedExecutionProgram::issue(execution, RowKind::Residual)
}

fn fixture_pure_call_table() -> rumoca_ir_solve::SolvePureCallTable {
    use rumoca_ir_solve::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolveRealFormat, SolveScalarType, SolveValue, SolveValueType,
    };

    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exec_cranelift_admission_pure_call.mo"),
        0,
        1,
    );
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::FULL,
        rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
    );
    let real = SolveValueType::scalar(SolveScalarType::real(profile));
    rumoca_ir_solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            Vec::new(),
            vec![SolvePureCallOutput::result(real)],
            span,
            |program, _inputs, outputs| {
                let value = program.constant(SolveValue::real(profile, 1.0), span)?;
                program.store(outputs[0], value, span)
            },
        )?;
        Ok(())
    })
    .expect("construct pure-call admission fixture")
}

fn fixture_pure_call_site() -> rumoca_ir_solve::SolvePureCallSite {
    let table = fixture_pure_call_table();
    table
        .owners()
        .first()
        .expect("pure-call owner was issued")
        .call_site()
}

fn fixture_pure_call_operation(dst_start: u32) -> LinearOp {
    LinearOp::PureCall {
        dst_start,
        input_starts: Box::new([]),
        site: fixture_pure_call_site(),
    }
}

fn fixture_directional_pure_call_operation(dst_start: u32) -> LinearOp {
    let site = fixture_pure_call_site();
    LinearOp::PureCallDirectional {
        dst_start,
        input_starts: Box::new([]),
        site: site
            .directional()
            .expect("fixture owner has a derived directional site")
            .clone(),
    }
}

fn fixture_pure_call_fold() -> rumoca_ir_solve::FunctionFoldProgram {
    rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            fixture_pure_call_operation(0),
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct fold with nested pure call")
}

fn fixture_plain_fold() -> rumoca_ir_solve::FunctionFoldProgram {
    rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct pure-call-free fold")
}

#[test]
fn native_admission_issues_a_closed_program() {
    let admitted = AdmittedProgram::issue(
        &[
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        RowKind::Residual,
    )
    .expect("admit supported native row");

    assert!(matches!(
        admitted.operations.as_ref(),
        [AdmittedLinearOp::Const { dst: 0, value }, AdmittedLinearOp::StoreOutput { src: 0 }]
            if *value == 2.0
    ));
}

#[test]
fn interpreter_support_accepts_plain_admitted_row() {
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit plain row");

    assert!(admitted.interpreter_supported);
}

#[test]
fn interpreter_support_accepts_pure_call_free_function_fold() {
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(fixture_plain_fold()),
        },
        LinearOp::StoreOutput { src: 1 },
    ])
    .expect("admit pure-call-free fold row");

    assert!(admitted.interpreter_supported);
}

#[test]
fn interpreter_support_accepts_pure_call_free_guarded_fold() {
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 0,
            activation: 1,
            program: std::sync::Arc::new(fixture_plain_fold()),
        },
        LinearOp::StoreOutput { src: 2 },
    ])
    .expect("admit pure-call-free guarded-fold row");

    assert!(admitted.interpreter_supported);
}

#[test]
fn interpreter_support_accepts_pure_call_free_store_output_fold() {
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(1),
                display_name: "outer".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![LinearOp::StoreOutputFunctionFold {
            initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                base: 0,
                count: 1,
            }]),
            capture_start: 0,
            program: std::sync::Arc::new(fixture_plain_fold()),
            result_base: 0,
            count: 1,
            condition: None,
            nested_when_true: false,
        }],
    )
    .expect("construct pure-call-free store-output fold");
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 1 },
    ])
    .expect("admit pure-call-free store-output fold row");

    assert!(admitted.interpreter_supported);
}

#[test]
fn interpreter_support_accepts_pure_call_free_conditional_regions() {
    let region = || {
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(region(), region())],
        region(),
    )
    .expect("construct pure-call-free conditional");
    let admitted = admitted_fixture_row(vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit pure-call-free conditional row");

    assert!(admitted.interpreter_supported);
}

#[test]
fn interpreter_support_survives_compiled_residual_and_jacobian_transfer() {
    let residual = compile_residual_rows(&[vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ]])
    .expect("compile interpreter-supported residual row");
    let jacobian = compile_jacobian_rows(&[vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ]])
    .expect("compile interpreter-supported Jacobian row");

    assert!(residual.rows[0].interpreter_supported);
    assert!(jacobian.rows[0].interpreter_supported);
}

#[test]
fn interpreter_refusal_survives_compiled_residual_and_jacobian_transfer() {
    let table = fixture_pure_call_table();
    let site = table
        .owners()
        .first()
        .expect("pure-call owner was issued")
        .call_site();
    let compiled_calls = std::rc::Rc::new(
        typed_program::CompiledPureCallTable::compile(&table)
            .expect("compile pure-call transfer fixture"),
    );
    let residual_block = fixture_scalar_program_block(vec![vec![
        LinearOp::PureCall {
            dst_start: 0,
            input_starts: Box::new([]),
            site: site.clone(),
        },
        LinearOp::StoreOutput { src: 0 },
    ]])
    .expect("construct pure-call residual row");
    let directional = site
        .directional()
        .expect("fixture owner has a derived directional site")
        .clone();
    let jacobian_block = fixture_scalar_program_block(vec![vec![
        LinearOp::PureCallDirectional {
            dst_start: 0,
            input_starts: Box::new([]),
            site: directional,
        },
        LinearOp::StoreOutput { src: 0 },
    ]])
    .expect("construct directional pure-call Jacobian row");
    let residual = compile_residual_rows_with_pure_calls(&residual_block, compiled_calls.clone())
        .expect("compile interpreter-refused residual row");
    let jacobian = compile_jacobian_rows_with_pure_calls(&jacobian_block, compiled_calls)
        .expect("compile interpreter-refused Jacobian row");

    assert!(!residual.rows[0].interpreter_supported);
    assert!(!jacobian.rows[0].interpreter_supported);
}

#[test]
fn interpreter_support_refuses_top_level_pure_call() {
    let admitted = admitted_fixture_row(vec![
        fixture_pure_call_operation(0),
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit native pure-call row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_top_level_directional_pure_call() {
    let admitted = admitted_fixture_row(vec![
        fixture_directional_pure_call_operation(0),
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit native directional pure-call row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_pure_call_in_fold_update() {
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(fixture_pure_call_fold()),
        },
        LinearOp::StoreOutput { src: 1 },
    ])
    .expect("admit native fold row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_pure_call_in_guarded_fold_update() {
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 0,
            activation: 1,
            program: std::sync::Arc::new(fixture_pure_call_fold()),
        },
        LinearOp::StoreOutput { src: 2 },
    ])
    .expect("admit native guarded-fold row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_pure_call_in_store_output_fold() {
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(1),
                display_name: "outer".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![LinearOp::StoreOutputFunctionFold {
            initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                base: 0,
                count: 1,
            }]),
            capture_start: 0,
            program: std::sync::Arc::new(fixture_pure_call_fold()),
            result_base: 0,
            count: 1,
            condition: None,
            nested_when_true: false,
        }],
    )
    .expect("construct outer fold with store-output nested pure call");
    let admitted = admitted_fixture_row(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 1 },
    ])
    .expect("admit native store-output fold row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_pure_call_in_conditional_arm() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                fixture_pure_call_operation(0),
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct conditional with pure-call arm");
    let admitted = admitted_fixture_row(vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit native conditional row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_pure_call_in_conditional_guard() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                fixture_pure_call_operation(0),
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct conditional with pure-call guard");
    let admitted = admitted_fixture_row(vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit native conditional row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn interpreter_support_refuses_pure_call_in_conditional_fallback() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 0.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            fixture_pure_call_operation(0),
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct conditional with pure-call fallback");
    let admitted = admitted_fixture_row(vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(conditional),
        },
        LinearOp::StoreOutput { src: 0 },
    ])
    .expect("admit native conditional row");

    assert!(!admitted.interpreter_supported);
}

#[test]
fn retired_table_host_nan_sentinel_is_unconstructible() {
    let linear_op = include_str!("../../../rumoca-ir-solve/src/linear_op.rs");
    let emitter = include_str!("../emit.rs");
    let host = include_str!("host_runtime.rs");
    let retired_variants = [
        concat!("Table", "Bounds"),
        concat!("Table", "Lookup"),
        concat!("Table", "Lookup", "Slope"),
        concat!("Table", "Next", "Event"),
    ];

    for retired in retired_variants {
        assert!(!linear_op.contains(retired));
        assert!(!emitter.contains(retired));
    }
    assert!(!host.contains(concat!("rumoca_host_", "table_")));
    assert!(!host.contains("unwrap_or(f64::NAN)"));
}

fn row_inputs<'a>(y: &'a [f64], p: &'a [f64], t: f64, seed: Option<&'a [f64]>) -> RowInputs<'a> {
    RowInputs { y, p, t, seed }
}

#[test]
fn checked_program_uses_simple_runtime_plan_for_plain_residual_rows() {
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];

    let plan = plan_fixture_row(&row).expect("simple plan");
    assert!(matches!(plan, RowPlan::Simple(_)));
}

#[test]
fn checked_program_keeps_seed_rows_on_general_runtime_plan() {
    let row = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];

    let plan = plan_fixture_row(&row).expect("general plan");
    assert!(matches!(plan, RowPlan::General(_)));
}

#[test]
fn compile_residual_rows_accepts_linear_solve_component() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Const { dst: 4, value: 4.0 },
        LinearOp::Const { dst: 5, value: 6.0 },
        LinearOp::LinearSolveComponent {
            dst: 6,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            component: 0,
        },
        LinearOp::StoreOutput { src: 6 },
    ];

    let compiled = compile_residual_rows(&[row]).expect("compiled row");
    let mut out = [0.0];
    compiled.call(&[], &[], 0.0, &mut out).expect("row eval");
    assert!((out[0] - 3.0).abs() <= f64::EPSILON);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_rows_use_ieee_division_semantics() {
    let division_row = |lhs| {
        vec![
            LinearOp::Const { dst: 0, value: lhs },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Div,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]
    };
    let compiled =
        compile_residual_rows(&[division_row(0.0), division_row(-1.0)]).expect("compiled rows");
    let mut out = [0.0; 2];

    compiled.call(&[], &[], 0.0, &mut out).expect("row eval");

    assert!(out[0].is_nan());
    assert_eq!(out[1], f64::NEG_INFINITY);
    assert_eq!(
        compiled.jit_call_count(),
        1,
        "small programs share one JIT batch"
    );
}

#[test]
fn compiled_function_fold_executes_a_retained_native_loop() {
    let row = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::FunctionFold {
            dst_start: 1,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(
                rumoca_ir_solve::FunctionFoldProgram::checked(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: rumoca_core::StructuredIndexBinderId::new(0),
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 4,
                            step: 1,
                        }],
                    },
                    1,
                    0,
                    vec![
                        LinearOp::LoadFoldCarried { dst: 0, index: 0 },
                        LinearOp::LoadFoldIndex {
                            dst: 1,
                            dimension: 0,
                        },
                        LinearOp::Binary {
                            dst: 2,
                            op: BinaryOp::Add,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ],
                )
                .expect("construct compact fold"),
            ),
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact fold");

    assert_eq!(out, [10.0]);
}

#[test]
fn compiled_function_fold_executes_matrix_multiply_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..8 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::MatrixMultiply {
        dst_start: 8,
        lhs_start: 0,
        rhs_start: 4,
        rows: 2,
        inner: 2,
        columns: 2,
        lanes: 1,
    });
    for src in 0..12 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            12,
            0,
            update,
        )
        .expect("construct matrix fold"),
    );
    let mut row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::Const { dst: 3, value: 4.0 },
        LinearOp::Const { dst: 4, value: 5.0 },
        LinearOp::Const { dst: 5, value: 6.0 },
        LinearOp::Const { dst: 6, value: 7.0 },
        LinearOp::Const { dst: 7, value: 8.0 },
    ];
    for dst in 8..12 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 12,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 20..24 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile matrix fold");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate matrix fold");

    assert_eq!(out, [19.0, 22.0, 43.0, 50.0]);
}

#[test]
fn compiled_function_fold_executes_tensor_binary_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..10 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorBinary {
        dst_start: 10,
        op: BinaryOp::Mul,
        lhs_start: 0,
        rhs_start: 4,
        count: 2,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes: 2,
    });
    for src in 0..6 {
        update.push(LinearOp::StoreOutput { src });
    }
    for src in 10..14 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            10,
            0,
            update,
        )
        .expect("construct tensor-binary fold"),
    );
    let initial = [1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 0.0, 0.0, 0.0, 0.0];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::FunctionFold {
        dst_start: 10,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 16..20 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-binary fold");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-binary fold");

    assert_eq!(out, [3.0, 60.0, 6.0, 120.0]);
}

#[test]
fn compiled_tensor_cross_matches_interleaved_dual_semantics() {
    let inputs = [1.0, 0.1, 2.0, 0.2, 3.0, 0.3, 4.0, 0.4, 5.0, 0.5, 6.0, 0.6];
    let mut row = inputs
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::TensorCross {
        dst_start: 12,
        lhs_start: 0,
        rhs_start: 6,
        lanes: 2,
    });
    row.push(LinearOp::StoreOutputRange {
        start: 12,
        count: 6,
        stride: 1,
    });

    let compiled = compile_residual_rows(&[row]).expect("compile tensor cross product");
    let mut out = [0.0; 6];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor cross product");

    let expected = [-3.0, -0.6, 6.0, 1.2, -3.0, -0.6];
    for (actual, expected) in out.into_iter().zip(expected) {
        assert!((actual - expected).abs() < 1e-12, "{actual} != {expected}");
    }
}

#[test]
fn compiled_tensor_division_matches_interleaved_dual_semantics() {
    let inputs = [4.0, 1.0, 6.0, 2.0, 2.0, 0.5];
    let mut row = inputs
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::TensorBinary {
        dst_start: 6,
        op: BinaryOp::Div,
        lhs_start: 0,
        rhs_start: 4,
        count: 2,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes: 2,
    });
    row.push(LinearOp::StoreOutputRange {
        start: 6,
        count: 4,
        stride: 1,
    });

    let compiled = compile_residual_rows(&[row]).expect("compile tensor division");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor division");

    assert_eq!(out, [2.0, 0.0, 3.0, 0.25]);
}

#[test]
fn compiled_function_fold_executes_rank_three_tensor_transpose_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..24 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorTranspose {
        dst_start: 24,
        src_start: 0,
        rows: 3,
        columns: 2,
        element_width: 2,
        lanes: 1,
    });
    for src in 0..12 {
        update.push(LinearOp::StoreOutput { src });
    }
    for src in 24..36 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            24,
            0,
            update,
        )
        .expect("construct tensor-transpose fold"),
    );
    let mut row = (0..12)
        .map(|dst| LinearOp::Const {
            dst,
            value: f64::from(dst + 1),
        })
        .collect::<Vec<_>>();
    for dst in 12..24 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 24,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 36..48 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-transpose fold");
    let mut out = [0.0; 12];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-transpose fold");

    assert_eq!(
        out,
        [
            1.0, 2.0, 7.0, 8.0, 3.0, 4.0, 9.0, 10.0, 5.0, 6.0, 11.0, 12.0
        ]
    );
}

#[test]
fn compiled_function_fold_executes_tensor_concatenation_as_a_native_loop() {
    let mut update = Vec::new();
    for index in 0..20 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update.push(LinearOp::TensorConcatenate {
        dst_start: 20,
        sources: vec![
            rumoca_ir_solve::TensorConcatenateSource {
                start: 0,
                dimensions: vec![2, 1].into_boxed_slice(),
            },
            rumoca_ir_solve::TensorConcatenateSource {
                start: 4,
                dimensions: vec![2, 1].into_boxed_slice(),
            },
        ]
        .into_boxed_slice(),
        dimensions: vec![2, 2].into_boxed_slice(),
        axis: 1,
        lanes: 2,
    });
    for src in 0..8 {
        update.push(LinearOp::StoreOutput { src });
    }
    for src in 20..28 {
        update.push(LinearOp::StoreOutput { src });
    }
    for src in 16..20 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            20,
            0,
            update,
        )
        .expect("construct tensor-concatenation fold"),
    );
    let initial = [1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    for dst in 8..20 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 20,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 28..36 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-concatenation fold");
    let mut out = [0.0; 8];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-concatenation fold");

    assert_eq!(out, [1.0, 10.0, 3.0, 30.0, 2.0, 20.0, 4.0, 40.0]);
}

#[test]
fn compiled_function_fold_executes_tensor_update_without_scalar_selection() {
    let mut update = Vec::new();
    for index in 0..21 {
        update.push(LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        });
    }
    update[12] = LinearOp::Const {
        dst: 12,
        value: 2.0,
    };
    update.push(LinearOp::TensorUpdate {
        dst_start: 21,
        base_start: 0,
        value_start: 8,
        dimensions: vec![2, 2].into_boxed_slice(),
        subscripts: vec![
            rumoca_ir_solve::TensorUpdateSubscript::Index(rumoca_ir_solve::TensorIndex::Runtime(
                12,
            )),
            rumoca_ir_solve::TensorUpdateSubscript::Whole,
        ]
        .into_boxed_slice(),
        lanes: 2,
    });
    for src in 0..13 {
        update.push(LinearOp::StoreOutput { src });
    }
    for src in 21..29 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            21,
            0,
            update,
        )
        .expect("construct tensor-update fold"),
    );
    let initial = [
        1.0, 10.0, 2.0, 20.0, 3.0, 30.0, 4.0, 40.0, 9.0, 90.0, 8.0, 80.0, 2.0,
    ];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    for dst in 13..21 {
        row.push(LinearOp::Const { dst, value: 0.0 });
    }
    row.push(LinearOp::FunctionFold {
        dst_start: 21,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 34..42 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile tensor-update fold");
    let mut out = [0.0; 8];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate tensor-update fold");

    assert_eq!(out, [1.0, 10.0, 2.0, 20.0, 9.0, 90.0, 8.0, 80.0]);
}

#[test]
fn compiled_function_fold_executes_tensor_update_slice_as_retained_scan() {
    let mut update = (0..14)
        .map(|index| LinearOp::LoadFoldCarried {
            dst: index as u32,
            index,
        })
        .collect::<Vec<_>>();
    update[8] = LinearOp::Const { dst: 8, value: 2.0 };
    update[9] = LinearOp::Const { dst: 9, value: 1.0 };
    update.push(LinearOp::TensorUpdate {
        dst_start: 14,
        base_start: 0,
        value_start: 4,
        dimensions: Box::new([2, 2]),
        subscripts: Box::new([
            rumoca_ir_solve::TensorUpdateSubscript::Slice {
                start: 8,
                dimensions: Box::new([2]),
            },
            rumoca_ir_solve::TensorUpdateSubscript::Whole,
        ]),
        lanes: 1,
    });
    for src in 0..10 {
        update.push(LinearOp::StoreOutput { src });
    }
    for src in 14..18 {
        update.push(LinearOp::StoreOutput { src });
    }
    let program = std::sync::Arc::new(
        rumoca_ir_solve::FunctionFoldProgram::checked(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 1,
                    step: 1,
                }],
            },
            14,
            0,
            update,
        )
        .expect("construct sliced tensor-update fold"),
    );
    let initial = [
        1.0, 2.0, 3.0, 4.0, 10.0, 20.0, 30.0, 40.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0,
    ];
    let mut row = initial
        .into_iter()
        .enumerate()
        .map(|(dst, value)| LinearOp::Const {
            dst: dst as u32,
            value,
        })
        .collect::<Vec<_>>();
    row.push(LinearOp::FunctionFold {
        dst_start: 14,
        initial_start: 0,
        capture_start: 0,
        program,
    });
    for src in 24..28 {
        row.push(LinearOp::StoreOutput { src });
    }

    let compiled = compile_residual_rows(&[row]).expect("compile sliced tensor-update fold");
    let mut out = [0.0; 4];
    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate sliced tensor-update fold");

    assert_eq!(out, [30.0, 40.0, 10.0, 20.0]);
}

#[test]
fn compiled_runtime_tensor_projection_uses_compact_affine_indexing() {
    for (index, expected) in [(1.0, 10.0), (2.0, 30.0)] {
        let row = vec![
            LinearOp::Const {
                dst: 0,
                value: 10.0,
            },
            LinearOp::Const {
                dst: 1,
                value: 20.0,
            },
            LinearOp::Const {
                dst: 2,
                value: 30.0,
            },
            LinearOp::Const {
                dst: 3,
                value: 40.0,
            },
            LinearOp::Const {
                dst: 4,
                value: index,
            },
            LinearOp::LoadIndexedRegister {
                dst: 5,
                base: 0,
                stride: 1,
                dimensions: Box::new([2, 2]),
                indices: Box::new([
                    rumoca_ir_solve::TensorIndex::Runtime(4),
                    rumoca_ir_solve::TensorIndex::Constant(0),
                ]),
            },
            LinearOp::StoreOutput { src: 5 },
        ];
        let block = rumoca_ir_solve::ScalarProgramBlock::with_program_spans(
            vec![row.clone()],
            vec![rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("RuntimeTensorBoundary.mo"),
                1,
                2,
            )],
        )
        .expect("construct proved compact tensor projection");
        let mut interpreted = [0.0];
        rumoca_eval_solve::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut interpreted)
            .expect("evaluate proved compact projection in the interpreter");

        let compiled = compile_residual_rows(&[row]).expect("compile compact tensor projection");
        let mut native = [0.0];
        compiled
            .call(&[], &[], 0.0, &mut native)
            .expect("evaluate compact tensor projection natively");

        assert_eq!(interpreted, [expected]);
        assert_eq!(native, interpreted);
        assert_eq!(compiled.jit_call_count(), 1);
    }
}

#[test]
fn compiled_function_fold_projects_directly_from_carried_tensor_memory() {
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 20.0,
        },
        LinearOp::Const {
            dst: 2,
            value: 30.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 40.0,
        },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(
                rumoca_ir_solve::FunctionFoldProgram::checked(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: rumoca_core::StructuredIndexBinderId::new(0),
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    4,
                    0,
                    vec![
                        LinearOp::LoadFoldIndex {
                            dst: 0,
                            dimension: 0,
                        },
                        LinearOp::LoadIndexedFoldCarried {
                            dst: 1,
                            base: 0,
                            stride: 1,
                            dimensions: Box::new([2, 2]),
                            indices: Box::new([
                                rumoca_ir_solve::TensorIndex::Runtime(0),
                                rumoca_ir_solve::TensorIndex::Constant(0),
                            ]),
                        },
                        LinearOp::StoreOutput { src: 1 },
                        LinearOp::LoadFoldCarried { dst: 2, index: 1 },
                        LinearOp::StoreOutput { src: 2 },
                        LinearOp::LoadFoldCarried { dst: 3, index: 2 },
                        LinearOp::StoreOutput { src: 3 },
                        LinearOp::LoadFoldCarried { dst: 4, index: 3 },
                        LinearOp::StoreOutput { src: 4 },
                    ],
                )
                .expect("construct compact tensor fold"),
            ),
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact tensor fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact tensor fold");

    assert_eq!(out, [30.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_function_fold_updates_a_tensor_slice_without_scalar_selects() {
    let row = vec![
        LinearOp::Const {
            dst: 0,
            value: 10.0,
        },
        LinearOp::Const {
            dst: 1,
            value: 20.0,
        },
        LinearOp::Const {
            dst: 2,
            value: 30.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 40.0,
        },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(
                rumoca_ir_solve::FunctionFoldProgram::checked(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: rumoca_core::StructuredIndexBinderId::new(0),
                            display_name: "column".to_string(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    4,
                    0,
                    vec![
                        LinearOp::LoadFoldIndex {
                            dst: 0,
                            dimension: 0,
                        },
                        LinearOp::Const {
                            dst: 1,
                            value: 100.0,
                        },
                        LinearOp::Const {
                            dst: 2,
                            value: 200.0,
                        },
                        LinearOp::StoreOutputFoldTensorUpdate {
                            source_base: 0,
                            source_stride: 1,
                            dimensions: Box::new([2, 2]),
                            updates: Box::new([rumoca_ir_solve::FoldTensorUpdate {
                                subscripts: Box::new([
                                    rumoca_ir_solve::TensorSubscript::Whole,
                                    rumoca_ir_solve::TensorSubscript::Index(
                                        rumoca_ir_solve::TensorIndex::Runtime(0),
                                    ),
                                ]),
                                condition: None,
                                value_start: 1,
                                value_stride: 1,
                            }]),
                            nodes: Box::new([rumoca_ir_solve::FoldTensorNode::Update {
                                base: 0,
                                update: 0,
                            }]),
                            result: 1,
                            lanes: 1,
                        },
                    ],
                )
                .expect("construct compact tensor update fold"),
            ),
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 5 },
        LinearOp::StoreOutput { src: 6 },
        LinearOp::StoreOutput { src: 7 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile compact tensor update fold");
    let mut out = [0.0; 4];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate compact tensor update fold");

    assert_eq!(out, [100.0, 100.0, 200.0, 200.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

#[test]
fn compiled_nested_fold_copies_parent_tensor_ranges_without_register_expansion() {
    let nested = four_lane_nested_fold();
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(1),
                display_name: "outer".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        4,
        0,
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutputFunctionFold {
                initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                    base: 0,
                    count: 4,
                }]),
                capture_start: 0,
                program: std::sync::Arc::new(nested),
                result_base: 0,
                count: 4,
                condition: Some(0),
                nested_when_true: false,
            },
        ],
    )
    .expect("construct outer compact fold");
    let row = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Const { dst: 2, value: 3.0 },
        LinearOp::Const { dst: 3, value: 4.0 },
        LinearOp::FunctionFold {
            dst_start: 4,
            initial_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::StoreOutput { src: 5 },
        LinearOp::StoreOutput { src: 6 },
        LinearOp::StoreOutput { src: 7 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile nested compact fold");
    let mut out = [0.0; 4];

    compiled
        .call(&[], &[], 0.0, &mut out)
        .expect("evaluate nested compact fold");

    assert_eq!(out, [4.0, 5.0, 6.0, 7.0]);
    assert_eq!(compiled.jit_call_count(), 1);
}

fn four_lane_nested_fold() -> rumoca_ir_solve::FunctionFoldProgram {
    rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "inner".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        4,
        0,
        vec![
            LinearOp::LoadFoldIndex {
                dst: 0,
                dimension: 0,
            },
            LinearOp::LoadFoldCarried { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 1,
                rhs: 0,
            },
            LinearOp::LoadFoldCarried { dst: 3, index: 1 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Add,
                lhs: 3,
                rhs: 0,
            },
            LinearOp::LoadFoldCarried { dst: 5, index: 2 },
            LinearOp::Binary {
                dst: 6,
                op: BinaryOp::Add,
                lhs: 5,
                rhs: 0,
            },
            LinearOp::LoadFoldCarried { dst: 7, index: 3 },
            LinearOp::Binary {
                dst: 8,
                op: BinaryOp::Add,
                lhs: 7,
                rhs: 0,
            },
            LinearOp::StoreOutput { src: 2 },
            LinearOp::StoreOutput { src: 4 },
            LinearOp::StoreOutput { src: 6 },
            LinearOp::StoreOutput { src: 8 },
        ],
    )
    .expect("construct inner compact fold")
}

#[test]
fn compiled_guarded_nested_fold_selects_at_native_control_flow_boundary() {
    let nested = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "inner".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct guarded inner fold");
    let outer = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(1),
                display_name: "outer".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        1,
        vec![
            LinearOp::LoadFoldCapture { dst: 0, index: 0 },
            LinearOp::StoreOutputFunctionFold {
                initial: Box::new([rumoca_ir_solve::FoldInitialSource::ParentCarried {
                    base: 0,
                    count: 1,
                }]),
                capture_start: 0,
                program: std::sync::Arc::new(nested),
                result_base: 0,
                count: 1,
                condition: Some(0),
                nested_when_true: true,
            },
        ],
    )
    .expect("construct guarded outer fold");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::FunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(outer),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile guarded compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate inactive guarded fold");
    assert_eq!(out, [3.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate active guarded fold");
    assert_eq!(out, [5.0]);
}

#[test]
fn compiled_guarded_function_fold_preserves_inactive_initial_tuple() {
    let fold = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 1,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct compact guarded fold");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 3.0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 0,
            activation: 0,
            program: std::sync::Arc::new(fold),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile guarded compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate inactive guarded fold");
    assert_eq!(out, [3.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate active guarded fold");
    assert_eq!(out, [5.0]);
}

#[test]
fn compiled_function_conditional_selects_one_correlated_result_tuple() {
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked(
        1,
        [1, 1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 1,
                    stride: 1,
                },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::TensorFill {
                    dst_start: 1,
                    value_start: 0,
                    count: 2,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 1,
                    count: 2,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 4.0 },
            LinearOp::TensorFill {
                dst_start: 1,
                value_start: 0,
                count: 2,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: 2,
                stride: 1,
            },
        ],
    )
    .expect("construct correlated conditional");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 1 },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile correlated conditional");
    let mut out = [0.0; 2];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate fallback tuple");
    assert_eq!(out, [4.0, 4.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate selected tuple");
    assert_eq!(out, [2.0, 2.0]);
}

#[test]
fn compiled_guarded_assignment_preserves_one_compact_owner_until_native_execution() {
    let conditional = rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
        rumoca_ir_solve::FunctionConditionalOwnerId::checked(7).expect("nonzero compact owner id"),
        0,
        [2],
        [(
            vec![
                LinearOp::LoadP { dst: 0, index: 2 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 7.0 },
                LinearOp::Const { dst: 1, value: 8.0 },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ],
    )
    .expect("checked lazy tuple");
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("guarded_assignment_native.mo"),
        0,
        1,
    );
    let owner = rumoca_ir_solve::GuardedAssignmentProgram::checked(
        rumoca_ir_solve::GuardedAssignmentProgramInput {
            program: vec![
                LinearOp::FunctionConditional {
                    dst_start: 0,
                    capture_start: 0,
                    program: std::sync::Arc::new(conditional),
                },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 2,
                    stride: 1,
                },
            ],
            provenance: span
                .require_provenance("native guarded assignment fixture")
                .expect("fixture provenance"),
            target_ranges: vec![(rumoca_ir_solve::scalar_slot_p(0), 2)],
            role: rumoca_ir_solve::DiscreteRowRole::EventAction,
            pre_mode: rumoca_ir_solve::DiscreteEventPreMode::FollowCurrent,
            observation_refresh: false,
            integrator_history_effect: rumoca_ir_solve::IntegratorHistoryEffect::Preserve,
            clock_owner: None,
        },
    )
    .expect("checked compact owner");
    let program = owner.program().to_vec();
    let compiled = compile_residual_rows(&[program]).expect("compile compact owner");
    let mut out = [0.0; 2];

    compiled
        .call(&[], &[1.0, 2.0, 0.0], 0.0, &mut out)
        .expect("inactive owner holds its tensor");
    assert_eq!(out, [1.0, 2.0]);
    compiled
        .call(&[], &[1.0, 2.0, 1.0], 0.0, &mut out)
        .expect("active owner updates its tensor");
    assert_eq!(out, [7.0, 8.0]);
}

#[test]
fn compiled_function_conditional_projects_one_compact_capture_range() {
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked(
        4,
        [3],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadFunctionConditionalCaptureRange {
                    dst_start: 0,
                    index_start: 1,
                    count: 3,
                },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 3,
                    stride: 1,
                },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 7.0 },
            LinearOp::TensorFill {
                dst_start: 1,
                value_start: 0,
                count: 3,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 1,
                count: 3,
                stride: 1,
            },
        ],
    )
    .expect("construct compact capture-range conditional");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::LoadP { dst: 2, index: 2 },
        LinearOp::LoadP { dst: 3, index: 3 },
        LinearOp::FunctionConditional {
            dst_start: 4,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 3,
            stride: 1,
        },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile capture-range conditional");
    let mut out = [0.0; 3];

    compiled
        .call(&[], &[0.0, 1.0, 2.0, 3.0], 0.0, &mut out)
        .expect("evaluate fallback tuple");
    assert_eq!(out, [7.0, 7.0, 7.0]);

    compiled
        .call(&[], &[1.0, 1.0, 2.0, 3.0], 0.0, &mut out)
        .expect("evaluate selected capture range");
    assert_eq!(out, [1.0, 2.0, 3.0]);
}

#[test]
fn compiled_function_conditional_calls_fold_only_from_selected_region() {
    let fold = rumoca_ir_solve::FunctionFoldProgram::checked(
        rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: rumoca_core::StructuredIndexBinderId::new(0),
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        },
        1,
        0,
        vec![
            LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .expect("construct compact fold inside conditional region");
    let program = rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
        rumoca_ir_solve::FunctionConditionalOwnerId::checked(11)
            .expect("nonzero conditional-fold owner id"),
        1,
        [1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const {
                    dst: 100,
                    value: 3.0,
                },
                LinearOp::FunctionFold {
                    dst_start: 101,
                    initial_start: 100,
                    capture_start: 0,
                    program: std::sync::Arc::new(fold),
                },
                LinearOp::StoreOutput { src: 101 },
            ],
        )],
        vec![
            LinearOp::Const {
                dst: 0,
                value: 11.0,
            },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("construct conditional containing compact fold");
    let row = vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::TensorFill {
            dst_start: 2,
            value_start: 1,
            count: 1,
            lanes: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let compiled = compile_residual_rows(&[row]).expect("compile conditional compact fold");
    let mut out = [0.0];

    compiled
        .call(&[], &[0.0], 0.0, &mut out)
        .expect("evaluate fallback without fold call");
    assert_eq!(out, [11.0]);

    compiled
        .call(&[], &[1.0], 0.0, &mut out)
        .expect("evaluate selected compact fold");
    assert_eq!(out, [7.0]);
}

mod runtime_refusal_tests;
