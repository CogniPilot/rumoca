//! Primitive controls use only original checked continuous refresh owners.
use super::*;

pub(super) struct Fixture {
    pub(super) source: solve::ComputeBlock,
    pub(super) owners: solve::ContinuousRefreshOwners,
    pub(super) layout: solve::VarLayout,
    pub(super) calls: solve::SolvePureCallTable,
}

pub(super) fn span(offset: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("exact_schedule.mo"),
        offset,
        offset + 1,
    )
}

pub(super) fn arithmetic() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn row(
    program: usize,
    output: usize,
    equation: usize,
    target: usize,
    register: u32,
    length: usize,
) -> solve::AlgebraicRefreshRow {
    solve::AlgebraicRefreshRow::checked(solve::AlgebraicRefreshRowDraft {
        owner_id: solve::RefreshRowOwnerId::checked(equation).unwrap(),
        source: solve::RefreshScalarProgramSource::checked(0, program).unwrap(),
        equation_index: equation,
        output_offset: output,
        target_index: target,
        assignment_target: Some(target),
        assignment_shape: Some(solve::TargetAssignmentShape::Direct {
            target_y_index: target,
            expr_reg: register,
            target_scale: 1.0,
            expr_eval_len: length,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    })
    .unwrap()
}

pub(super) fn plain(unused_index: usize) -> Fixture {
    let programs = vec![
        vec![
            solve::LinearOp::LoadY {
                dst: 100,
                index: unused_index,
            },
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            binary(3, solve::BinaryOp::Mul, 1, 2),
            binary(4, solve::BinaryOp::Sub, 0, 3),
            solve::LinearOp::StoreOutput { src: 4 },
            solve::LinearOp::LoadY { dst: 5, index: 3 },
            solve::LinearOp::LoadY { dst: 6, index: 0 },
            binary(7, solve::BinaryOp::Add, 6, 2),
            binary(8, solve::BinaryOp::Sub, 5, 7),
            solve::LinearOp::StoreOutput { src: 8 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 4 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::LoadY { dst: 2, index: 3 },
            binary(3, solve::BinaryOp::Sub, 1, 2),
            binary(4, solve::BinaryOp::Sub, 0, 3),
            solve::LinearOp::StoreOutput { src: 4 },
        ],
    ];
    let rows = vec![
        row(0, 0, 0, 1, 3, 5),
        row(0, 1, 1, 3, 7, 10),
        row(1, 0, 2, 4, 3, 4),
    ];
    assemble(
        programs,
        rows,
        solve::SolvePureCallTable::builder(arithmetic()).finish(),
        1,
    )
}

/// `y[1] := y[0] * p[0]` after `unread`, pure operations on registers from
/// 100 that nothing reads, so the final program does not hold them.
pub(super) fn unread_prefix(unread: Vec<solve::LinearOp>) -> Fixture {
    let length = unread.len() + 4;
    let mut program = unread;
    program.extend([
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadY { dst: 1, index: 0 },
        solve::LinearOp::LoadP { dst: 2, index: 0 },
        binary(3, solve::BinaryOp::Mul, 1, 2),
        binary(4, solve::BinaryOp::Sub, 0, 3),
        solve::LinearOp::StoreOutput { src: 4 },
    ]);
    assemble(
        vec![program],
        vec![row(0, 0, 0, 1, 3, length)],
        solve::SolvePureCallTable::builder(arithmetic()).finish(),
        1,
    )
}

impl Fixture {
    /// The operations of the schedule's first final program.
    pub(super) fn first_final_program(&self) -> Vec<solve::LinearOp> {
        let owner = self
            .owners
            .exact_assignment_program(self.schedule().program_ids()[0])
            .unwrap();
        owner.final_scalar_program(&self.source).unwrap().programs()[0].clone()
    }
}

/// `y[1] := {42, 64}[p[0]]`: a checked model-scope gather whose runtime
/// index can fail Integer conversion or the array bound.
pub(super) fn gather() -> Fixture {
    assemble(
        vec![gather_program(1, 0)],
        vec![row(0, 0, 0, 1, 4, 5)],
        solve::SolvePureCallTable::builder(arithmetic()).finish(),
        1,
    )
}

/// `y[target] := {42, 64}[p[index]]`, with its expression in register 4.
fn gather_program(target: usize, index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::Const { dst: 1, value: 42. },
        solve::LinearOp::Const { dst: 2, value: 64. },
        solve::LinearOp::LoadP { dst: 3, index },
        solve::LinearOp::LoadIndexedRegister {
            dst: 4,
            base: 1,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![solve::TensorIndex::Runtime(3)].into_boxed_slice(),
        },
        binary(5, solve::BinaryOp::Sub, 0, 4),
        solve::LinearOp::StoreOutput { src: 5 },
    ]
}

/// A gather on `p[0]` into `y[1]`, a chain of `length` into `y[2]`, then a
/// gather on `p[1]` into `y[4]`. With `length` over the chunk budget each
/// program is its own chunk, so the two gathers issue statuses in different
/// functions.
pub(super) fn gathers_across_chunks(length: usize) -> Fixture {
    let (chain, chain_reg, chain_len) = chain(0, 2, length);
    assemble(
        vec![gather_program(1, 0), chain, gather_program(4, 1)],
        vec![
            row(0, 0, 0, 1, 4, 5),
            row(1, 0, 1, 2, chain_reg, chain_len),
            row(2, 0, 2, 4, 4, 5),
        ],
        solve::SolvePureCallTable::builder(arithmetic()).finish(),
        2,
    )
}

pub(super) fn late_fault() -> Fixture {
    let profile = arithmetic();
    let value_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut builder = solve::SolvePureCallTable::builder(profile);
    let owner = builder
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![value_type.clone()],
            vec![solve::SolvePureCallOutput::result(value_type)],
            span(20),
            |body, inputs, outputs| {
                let x = body.load(inputs[0], span(21))?;
                let integer = body.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    x,
                    span(22),
                )?;
                let value = body.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span(23),
                )?;
                body.store(outputs[0], value, span(24))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    let mut fixture = plain(5);
    let solve::ComputeNode::ScalarPrograms(block) = &fixture.source.nodes[0] else {
        unreachable!()
    };
    let mut programs = block.programs().to_vec();
    programs[1] = vec![
        solve::LinearOp::LoadY { dst: 0, index: 4 },
        solve::LinearOp::LoadY { dst: 1, index: 1 },
        solve::LinearOp::LoadP { dst: 2, index: 1 },
        solve::LinearOp::PureCall {
            dst_start: 3,
            input_starts: vec![2].into_boxed_slice(),
            site,
        },
        binary(4, solve::BinaryOp::Add, 1, 3),
        binary(5, solve::BinaryOp::Sub, 0, 4),
        solve::LinearOp::StoreOutput { src: 5 },
    ];
    fixture = assemble(
        programs,
        vec![
            row(0, 0, 0, 1, 3, 5),
            row(0, 1, 1, 3, 7, 10),
            row(1, 0, 2, 4, 4, 5),
        ],
        builder.finish(),
        2,
    );
    fixture
}

pub(super) fn tuple_fault() -> Fixture {
    let fault = late_fault();
    let solve::ComputeNode::ScalarPrograms(block) = &plain(5).source.nodes[0] else {
        unreachable!()
    };
    let mut programs = block.programs().to_vec();
    let site = fault.calls.owners()[0].call_site();
    programs[0].splice(
        10..10,
        [
            solve::LinearOp::LoadP { dst: 101, index: 1 },
            solve::LinearOp::PureCall {
                dst_start: 102,
                input_starts: vec![101].into_boxed_slice(),
                site,
            },
            binary(103, solve::BinaryOp::Add, 7, 102),
        ],
    );
    programs[0][13] = binary(8, solve::BinaryOp::Sub, 5, 103);
    assemble(
        programs,
        vec![
            row(0, 0, 0, 1, 3, 5),
            row(0, 1, 1, 3, 103, 13),
            row(1, 0, 2, 4, 3, 4),
        ],
        fault.calls,
        2,
    )
}

fn assemble(
    programs: Vec<Vec<solve::LinearOp>>,
    rows: Vec<solve::AlgebraicRefreshRow>,
    calls: solve::SolvePureCallTable,
    p_count: usize,
) -> Fixture {
    let count = programs.len();
    let source = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_program_spans(programs, vec![span(1); count]).unwrap(),
    );
    let plan = solve::RefreshPlan {
        dynamic_causal_seed_rows: solve::RefreshRowSelection::checked(rows.len(), 0..rows.len())
            .unwrap(),
        rows,
        ..Default::default()
    };
    let owners = solve::ContinuousRefreshOwners::checked_for_source(
        &source,
        plan,
        Default::default(),
        Default::default(),
        Default::default(),
        Vec::new(),
    )
    .unwrap();
    Fixture {
        source,
        owners,
        calls,
        layout: solve::VarLayout::from_parts(Default::default(), 6, p_count),
    }
}

impl Fixture {
    pub(super) fn schedule(&self) -> &solve::ExactRefreshAssignmentSchedule {
        self.owners
            .exact_assignment_schedule(self.owners.algebraic().dynamic_causal_sequence)
            .unwrap()
    }

    pub(super) fn compile(
        &self,
    ) -> Result<rumoca_exec_wasm::CompiledExactAssignmentWasm, rumoca_exec_wasm::WasmCompileError>
    {
        rumoca_exec_wasm::compile_exact_assignment_schedule_wasm(
            &self.source,
            &self.owners,
            self.schedule(),
            &self.layout,
            &self.calls,
        )
    }

    pub(super) fn canonical(
        &self,
        y: &mut [f64],
        p: &[f64],
    ) -> Result<(), rumoca_eval_solve::EvalSolveError> {
        for id in self.schedule().program_ids() {
            let owner = self.owners.exact_assignment_program(*id).unwrap();
            let block = owner.final_scalar_program(&self.source).unwrap();
            let mut values = vec![0.; owner.target_indices().len()];
            rumoca_eval_solve::eval_scalar_program_block_with_context(
                &block,
                y,
                p,
                0.,
                rumoca_eval_solve::RowEvalContext {
                    pure_calls: Some(&self.calls),
                    ..Default::default()
                },
                &mut values,
            )?;
            for (&target, value) in owner.target_indices().iter().zip(values) {
                y[target] = value;
            }
        }
        Ok(())
    }
}

/// `y[target] := y[input] + length * p[0]` as a chain of `length` additions,
/// with its expression register and evaluation length.
fn chain(input: usize, target: usize, length: usize) -> (Vec<solve::LinearOp>, u32, usize) {
    let mut ops = vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: input,
        },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
    ];
    let mut last = 0;
    for step in 0..length {
        let dst = u32::try_from(step + 2).unwrap();
        ops.push(binary(dst, solve::BinaryOp::Add, last, 1));
        last = dst;
    }
    let eval_len = ops.len();
    ops.extend([
        solve::LinearOp::LoadY {
            dst: last + 1,
            index: target,
        },
        binary(last + 2, solve::BinaryOp::Sub, last + 1, last),
        solve::LinearOp::StoreOutput { src: last + 2 },
    ]);
    (ops, last, eval_len)
}

/// A long chain into `y[1]`, then `late_fault`'s Integer-conversion program
/// into `y[4]`, so the fault happens in a later chunk than the first commit.
pub(super) fn late_fault_after_long_chain(length: usize) -> Fixture {
    let source = late_fault();
    let solve::ComputeNode::ScalarPrograms(block) = &source.source.nodes[0] else {
        unreachable!()
    };
    let faulting = block.programs()[1].clone();
    let (first, first_reg, first_len) = chain(0, 1, length);
    assemble(
        vec![first, faulting],
        vec![row(0, 0, 0, 1, first_reg, first_len), row(1, 0, 1, 4, 4, 5)],
        source.calls,
        2,
    )
}

/// Two dependent chains: `y[1] := y[0] + length * p[0]`, then
/// `y[2] := y[1] + length * p[0]`.
pub(super) fn long_chain(length: usize) -> Fixture {
    let (first, first_reg, first_len) = chain(0, 1, length);
    let (second, second_reg, second_len) = chain(1, 2, length);
    assemble(
        vec![first, second],
        vec![
            row(0, 0, 0, 1, first_reg, first_len),
            row(1, 0, 1, 2, second_reg, second_len),
        ],
        solve::SolvePureCallTable::builder(arithmetic()).finish(),
        1,
    )
}

fn binary(dst: u32, op: solve::BinaryOp, lhs: u32, rhs: u32) -> solve::LinearOp {
    solve::LinearOp::Binary { dst, op, lhs, rhs }
}

pub(super) fn typed_tuple() -> Fixture {
    let profile = arithmetic();
    let value_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let mut builder = solve::SolvePureCallTable::builder(profile);
    let owner = builder
        .add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(7).unwrap()),
            vec![value_type.clone()],
            vec![
                solve::SolvePureCallOutput::result(value_type.clone()),
                solve::SolvePureCallOutput::result(value_type),
            ],
            span(30),
            |body, inputs, outputs| {
                let value = body.load(inputs[0], span(31))?;
                let doubled =
                    body.binary(solve::SolveBinaryOperator::Add, value, value, span(32))?;
                body.store(outputs[0], value, span(33))?;
                body.store(outputs[1], doubled, span(34))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    assemble(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 3 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
            solve::LinearOp::PureCall {
                dst_start: 3,
                input_starts: vec![2].into_boxed_slice(),
                site,
            },
            binary(5, solve::BinaryOp::Sub, 0, 3),
            solve::LinearOp::StoreOutput { src: 5 },
            binary(6, solve::BinaryOp::Sub, 1, 4),
            solve::LinearOp::StoreOutput { src: 6 },
        ]],
        vec![row(0, 0, 0, 1, 3, 4), row(0, 1, 1, 3, 4, 4)],
        builder.finish(),
        1,
    )
}
