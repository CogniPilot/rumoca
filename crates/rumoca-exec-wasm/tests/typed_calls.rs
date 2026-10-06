//! Typed-call payload execution, checked failures, and storage snapshot oracles.
mod native_call_program;
mod suite_typed_calls;
use rumoca_core::{SourceId, Span};
use rumoca_eval_solve::{TypedValue, eval_pure_call};
use rumoca_exec_wasm::{
    CompiledTypedCallWasm, TypedCallCompileError, TypedCallFaultKind, compile_pure_call_wasm,
};
use rumoca_ir_solve as solve;
use std::num::NonZeroU64;
use wasmi::{Engine, Linker, Memory, MemoryType, Module, Store, TypedFunc};

fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("typed_calls.mo"),
        offset,
        offset + 1,
    )
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn identity(id: u64) -> solve::SolvePureCallIdentity {
    solve::SolvePureCallIdentity::issued(NonZeroU64::new(id).unwrap())
}

fn real(value: f64) -> solve::SolveValueKind {
    solve::SolveValueKind::Real64(value.to_bits())
}

fn cells(values: impl IntoIterator<Item = solve::SolveValueKind>) -> Vec<u8> {
    values
        .into_iter()
        .flat_map(|value| match value {
            solve::SolveValueKind::Real64(bits) => bits.to_le_bytes(),
            solve::SolveValueKind::Integer(integer) => integer.to_le_bytes(),
            solve::SolveValueKind::Boolean(boolean) => u64::from(boolean).to_le_bytes(),
            solve::SolveValueKind::Real32(_) => panic!("fixture uses Binary64"),
        })
        .collect()
}

struct Runner {
    store: Store<()>,
    memory: Memory,
    call: TypedFunc<(i32, i32, i32), i32>,
    output: usize,
    scratch: usize,
    output_bytes: usize,
    scratch_bytes: usize,
}

impl Runner {
    fn new(compiled: &CompiledTypedCallWasm) -> Self {
        let engine = Engine::default();
        let module = Module::new(&engine, compiled.module_bytes()).unwrap();
        let mut store = Store::new(&engine, ());
        let layout = compiled.layout();
        let output = layout.input_bytes as usize + 8;
        let scratch = output + layout.output_bytes as usize + 8;
        let total = scratch + layout.scratch_bytes as usize + 8;
        let memory = Memory::new(
            &mut store,
            MemoryType::new(total.div_ceil(65536) as u32, None),
        )
        .unwrap();
        let mut linker = Linker::new(&engine);
        suite_typed_calls::math::bind(&mut linker);
        // The existing native target math ABI preserves the Power operation.
        // This oracle binds the same host intrinsic as canonical evaluation.
        linker
            .func_wrap("env", "pow", |base: f64, exponent: f64| base.powf(exponent))
            .unwrap();
        linker.define("env", "memory", memory).unwrap();
        let instance = linker
            .instantiate(&mut store, &module)
            .unwrap()
            .start(&mut store)
            .unwrap();
        let call = instance.get_typed_func(&store, "eval_typed_call").unwrap();
        Self {
            store,
            memory,
            call,
            output,
            scratch,
            output_bytes: layout.output_bytes as usize,
            scratch_bytes: layout.scratch_bytes as usize,
        }
    }

    fn run(&mut self, inputs: &[u8]) -> (i32, Vec<u8>) {
        self.memory.write(&mut self.store, 0, inputs).unwrap();
        self.memory
            .write(&mut self.store, self.output, &vec![0xa5; self.output_bytes])
            .unwrap();
        self.memory
            .write(
                &mut self.store,
                self.scratch,
                &vec![0x7b; self.scratch_bytes],
            )
            .unwrap();
        let status = self
            .call
            .call(
                &mut self.store,
                (0, self.output as i32, self.scratch as i32),
            )
            .unwrap();
        let mut output = vec![0; self.output_bytes];
        self.memory
            .read(&self.store, self.output, &mut output)
            .unwrap();
        let mut unchanged = vec![0; inputs.len()];
        self.memory.read(&self.store, 0, &mut unchanged).unwrap();
        assert_eq!(unchanged, inputs, "call mutated its read-only input tuple");
        (status, output)
    }
}

fn oracle(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    inputs: &[Vec<solve::SolveValueKind>],
) -> Result<Vec<u8>, rumoca_eval_solve::TypedProgramEvalError> {
    let typed = site
        .inputs()
        .iter()
        .zip(inputs)
        .map(|(value_type, values)| {
            TypedValue::construct(value_type.clone(), values.clone()).unwrap()
        })
        .collect::<Vec<_>>();
    eval_pure_call(table, site.owner(), &typed).map(|outputs| {
        cells(
            outputs
                .iter()
                .flat_map(|value| value.elements().iter().copied()),
        )
    })
}

fn mixed(count: u32) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let r = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let i = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let b = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let tensor = solve::SolveValueType::tensor(r.element_type(), vec![count]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(1),
            vec![tensor.clone(), i.clone(), r.clone(), b.clone()],
            vec![tensor, r, i, b]
                .into_iter()
                .map(solve::SolvePureCallOutput::result)
                .collect(),
            span(1),
            |builder, inputs, outputs| {
                let original = builder.load(inputs[0], span(2))?;
                let index = builder.load(inputs[1], span(3))?;
                let value = builder.load(inputs[2], span(4))?;
                let condition = builder.load(inputs[3], span(5))?;
                let old = builder.project_element_dynamic(original, &[index], span(6))?;
                builder.store(outputs[1], old, span(7))?;
                let first = builder.update_element(original, value, &[index], span(8))?;
                let two = builder.constant(solve::SolveValue::real(p, 2.0), span(9))?;
                let doubled =
                    builder.binary(solve::SolveBinaryOperator::Multiply, value, two, span(10))?;
                let updated = builder.update_element(first, doubled, &[index], span(11))?;
                let selected = builder.select(condition, updated, original, span(12))?;
                builder.store(outputs[0], selected, span(13))?;
                let rounded = builder.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    value,
                    span(14),
                )?;
                builder.store(outputs[2], rounded, span(15))?;
                let inverse = builder.unary(solve::SolveUnaryOperator::Not, condition, span(16))?;
                builder.store(outputs[3], inverse, span(17))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn complete_typed_tuple_matches_oracle_with_changed_inputs_and_private_scratch() {
    let (table, site) = mixed(14400);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert_eq!(compiled.layout().outputs, site.outputs());
    let mut runner = Runner::new(&compiled);
    for (index, value, flag) in [(1, 12.7, true), (14400, -8.3, false), (4000, -0.0, true)] {
        let inputs = vec![
            (0..14400)
                .map(|n| real((n as f64 - 7200.0) / 13.0))
                .collect(),
            vec![solve::SolveValueKind::Integer(index)],
            vec![real(value)],
            vec![solve::SolveValueKind::Boolean(flag)],
        ];
        let expected = oracle(&table, &site, &inputs).unwrap();
        let (status, actual) = runner.run(&cells(inputs.into_iter().flatten()));
        assert_eq!(status, 0);
        assert_eq!(actual, expected);
    }
}

#[test]
fn late_conversion_and_index_failures_do_not_publish_partial_outputs() {
    let (table, site) = mixed(4);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (index, value, kind, at) in [
        (0, 3.0, TypedCallFaultKind::IndexBounds, 6),
        (-1, 3.0, TypedCallFaultKind::IndexBounds, 6),
        (5, 3.0, TypedCallFaultKind::IndexBounds, 6),
        (1, f64::NAN, TypedCallFaultKind::IntegerConversion, 14),
        (1, f64::INFINITY, TypedCallFaultKind::IntegerConversion, 14),
        (1, -f64::INFINITY, TypedCallFaultKind::IntegerConversion, 14),
        (
            1,
            9223372036854775808.0,
            TypedCallFaultKind::IntegerConversion,
            14,
        ),
    ] {
        let inputs = vec![
            vec![real(1.0); 4],
            vec![solve::SolveValueKind::Integer(index)],
            vec![real(value)],
            vec![solve::SolveValueKind::Boolean(true)],
        ];
        assert!(oracle(&table, &site, &inputs).is_err());
        let (status, actual) = runner.run(&cells(inputs.into_iter().flatten()));
        assert_eq!(actual, vec![0xa5; compiled.layout().output_bytes as usize]);
        let fault = compiled
            .faults()
            .iter()
            .find(|fault| fault.status == status as u32)
            .unwrap();
        assert_eq!(fault.kind, kind);
        assert_eq!(fault.provenance, span(at));
        assert!(fault.operation.is_some());
    }
    let (status, _) = runner.run(&cells([
        real(1.0),
        real(2.0),
        real(3.0),
        real(4.0),
        solve::SolveValueKind::Integer(4),
        real(5.25),
        solve::SolveValueKind::Boolean(true),
    ]));
    assert_eq!(status, 0, "failure cannot poison the next invocation");
}

fn update_chain(
    count: u32,
    updates: usize,
    retain_snapshot: bool,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let array =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![count]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let results = vec![
        solve::SolvePureCallOutput::result(array.clone());
        if retain_snapshot { 2 } else { 1 }
    ];
    let owner = table
        .add_owner(
            identity(2),
            vec![],
            results,
            span(30),
            |builder, _, outputs| {
                let zero = builder.constant(solve::SolveValue::real(p, 0.0), span(31))?;
                let mut array = builder.fill(zero, vec![count], span(32))?;
                let old = array;
                let index =
                    builder.constant(solve::SolveValue::integer(p, 1).unwrap(), span(33))?;
                for n in 0..updates {
                    let value = builder
                        .constant(solve::SolveValue::real(p, n as f64 + 1.0), span(34 + n))?;
                    array = builder.update_element(array, value, &[index], span(300 + n))?;
                }
                builder.store(outputs[0], array, span(600))?;
                if retain_snapshot {
                    builder.store(outputs[1], old, span(601))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn consumed_updates_share_private_storage_but_live_old_snapshots_remain_distinct() {
    for retain in [false, true] {
        let (table, site) = update_chain(14400, 100, retain);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let arrays = if retain { 4 } else { 2 };
        assert!(
            compiled.layout().scratch_bytes <= 14400 * 8 * arrays + 102 * 8,
            "functional writes must not allocate a new whole array every time"
        );
        assert!(compiled.module_bytes().len() < 45000);
        let (status, actual) = Runner::new(&compiled).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
    }
}

#[test]
fn abi_rejects_overlap_alignment_bounds_and_noncanonical_boolean_without_effects() {
    let (table, site) = mixed(4);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let inputs = cells([
        real(1.0),
        real(2.0),
        real(3.0),
        real(4.0),
        solve::SolveValueKind::Integer(1),
        real(2.0),
        solve::SolveValueKind::Boolean(true),
    ]);
    assert_eq!(runner.run(&inputs).0, 0);
    let mut before = vec![0; runner.memory.data_size(&runner.store)];
    runner.memory.read(&runner.store, 0, &mut before).unwrap();
    for pointers in [
        (1, runner.output as i32, runner.scratch as i32),
        (0, 0, runner.scratch as i32),
        (0, runner.output as i32, runner.output as i32),
        (-8, runner.output as i32, runner.scratch as i32),
    ] {
        let status = runner.call.call(&mut runner.store, pointers).unwrap();
        assert_eq!(status, 1);
        let mut after = vec![0; before.len()];
        runner.memory.read(&runner.store, 0, &mut after).unwrap();
        assert_eq!(after, before);
    }
    let mut bad = inputs;
    bad[48..56].copy_from_slice(&2u64.to_le_bytes());
    let (status, actual) = runner.run(&bad);
    assert!(compiled.faults().iter().any(
        |fault| fault.status == status as u32 && fault.kind == TypedCallFaultKind::InvalidInput
    ));
    assert_eq!(actual, vec![0xa5; runner.output_bytes]);
}

fn integer_binary(
    operator: solve::SolveBinaryOperator,
    domain: solve::SolveIntegerDomain,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = solve::SolveArithmeticProfile::construct(solve::SolveRealFormat::Binary64, domain);
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(3),
            vec![integer.clone(), integer.clone()],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(40),
            |builder, inputs, outputs| {
                let a = builder.load(inputs[0], span(41))?;
                let b = builder.load(inputs[1], span(42))?;
                let result = builder.binary(operator, a, b, span(43))?;
                builder.store(outputs[0], result, span(44))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn checked_integer_arithmetic_matches_the_reference_including_overflow_and_domain() {
    use solve::SolveBinaryOperator as B;
    for operator in [B::Add, B::Subtract, B::Multiply, B::Min, B::Max] {
        for domain in [
            solve::SolveIntegerDomain::FULL,
            solve::SolveIntegerDomain::construct(-12, 12).unwrap(),
        ] {
            let (table, site) = integer_binary(operator, domain);
            let compiled = compile_pure_call_wasm(&table, &site).unwrap();
            let mut runner = Runner::new(&compiled);
            let cases = [
                (-12, -1),
                (12, 2),
                (0, -12),
                (7, -9),
                (i64::MAX, 1),
                (i64::MIN, -1),
                (i64::MIN, 1),
                (i64::MAX / 2 + 1, 2),
                (3037000500, 3037000500),
            ];
            for (a, b) in cases {
                check_integer_case(&table, &site, &mut runner, domain, a, b);
            }
        }
    }
}

fn check_integer_case(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
    runner: &mut Runner,
    domain: solve::SolveIntegerDomain,
    a: i64,
    b: i64,
) {
    let inputs = vec![
        vec![solve::SolveValueKind::Integer(a)],
        vec![solve::SolveValueKind::Integer(b)],
    ];
    let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
    if !domain.contains(a) || !domain.contains(b) {
        assert_ne!(status, 0);
        return;
    }
    match oracle(table, site, &inputs) {
        Ok(expected) => {
            assert_eq!(status, 0);
            assert_eq!(actual, expected);
        }
        Err(_) => {
            assert_ne!(status, 0);
            assert_eq!(actual, vec![0xa5; 8]);
        }
    }
}

#[test]
fn admitted_source_math_keeps_canonical_values_and_foreign_interfaces_fail_closed() {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(4),
            vec![real_type.clone()],
            vec![solve::SolvePureCallOutput::result(real_type)],
            span(70),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(71))?;
                let result = builder.unary(solve::SolveUnaryOperator::Sin, input, span(72))?;
                builder.store(outputs[0], result, span(73))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert_eq!(compiled.math_imports(), ["sin"]);
    let mut runner = Runner::new(&compiled);
    for input in [0., -0., 0.75, -2., f64::MIN_POSITIVE] {
        let expected = oracle(&table, &site, &[vec![real(input)]]).unwrap();
        let (status, actual) = runner.run(&cells([real(input)]));
        assert_eq!(status, 0);
        assert_eq!(actual, expected);
    }
    let (other, _) = mixed(4);
    assert_eq!(
        compile_pure_call_wasm(&other, &site).unwrap_err(),
        TypedCallCompileError::SiteMismatch
    );
}

fn unary_real(
    operator: solve::SolveUnaryOperator,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(5),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span(80),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(81))?;
                let result = builder.unary(operator, input, span(82))?;
                builder.store(outputs[0], result, span(83))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn unary_real_signed_zero_nan_sign_and_rounding_match_canonical_bits() {
    use solve::SolveUnaryOperator as U;
    for operator in [
        U::Negate,
        U::Abs,
        U::Sign,
        U::Floor,
        U::Ceiling,
        U::Truncate,
        U::Sqrt,
    ] {
        let (table, site) = unary_real(operator);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        let mut values = vec![0.0, -0.0, 1.0, 3.25, f64::INFINITY];
        if operator != U::Sqrt {
            values.extend([-1.0, -3.25, -f64::INFINITY]);
        }
        if operator == U::Sign {
            values.extend([f64::NAN, f64::from_bits(0xfff8000000000042)]);
        }
        for value in values {
            let inputs = vec![vec![real(value)]];
            let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
            assert_eq!(status, 0);
            assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
        }
    }
}

fn conversion(
    operator: solve::SolveConversionOperator,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let r = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let i = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let (input, output) = if operator == solve::SolveConversionOperator::IntegerToReal {
        (i, r)
    } else {
        (r, i)
    };
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(6),
            vec![input],
            vec![solve::SolvePureCallOutput::result(output)],
            span(90),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(91))?;
                let result = builder.convert(operator, input, span(92))?;
                builder.store(outputs[0], result, span(93))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn typed_conversions_preserve_rounding_and_refuse_before_wasm_integer_traps() {
    use solve::SolveConversionOperator as C;
    for operator in [
        C::IntegerToReal,
        C::RealToIntegerTowardZero,
        C::RealToIntegerTowardNegativeInfinity,
    ] {
        let (table, site) = conversion(operator);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        let values = if operator == C::IntegerToReal {
            [i64::MIN, i64::MAX, 9007199254740993, -9007199254740993, 0]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>()
        } else {
            [
                0.0,
                -0.0,
                -1.25,
                1.25,
                -9223372036854775808.0,
                9223372036854774784.0,
                9223372036854775808.0,
                -9223372036854777856.0,
                f64::NAN,
                f64::INFINITY,
                -f64::INFINITY,
            ]
            .into_iter()
            .map(real)
            .collect()
        };
        for value in values {
            let inputs = vec![vec![value]];
            let (status, actual) = runner.run(&cells([value]));
            match oracle(&table, &site, &inputs) {
                Ok(expected) => {
                    assert_eq!(status, 0);
                    assert_eq!(actual, expected);
                }
                Err(_) => {
                    assert_ne!(status, 0);
                    assert_eq!(actual, vec![0xa5; 8]);
                }
            }
        }
    }
}

#[test]
fn mutable_slot_loads_are_snapshots_and_private_stores_never_change_old_registers() {
    let p = profile();
    let array = solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![4]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(7),
            vec![array.clone()],
            vec![solve::SolvePureCallOutput::result(array.clone()); 2],
            span(100),
            |builder, inputs, outputs| {
                let local = builder.declare_slot(
                    array.clone(),
                    solve::SolveStorageClass::MethodLocal,
                    solve::SolveSlotAccess::ReadWrite,
                    span(101),
                )?;
                let input = builder.load(inputs[0], span(102))?;
                builder.store(local, input, span(103))?;
                let old = builder.load(local, span(104))?;
                let index =
                    builder.constant(solve::SolveValue::integer(p, 2).unwrap(), span(105))?;
                let value = builder.constant(solve::SolveValue::real(p, 99.0), span(106))?;
                let updated = builder.update_element(old, value, &[index], span(107))?;
                builder.store(local, updated, span(108))?;
                let current = builder.load(local, span(109))?;
                builder.store(outputs[0], current, span(110))?;
                builder.store(outputs[1], old, span(111))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![real(1.0), real(2.0), real(3.0), real(4.0)]];
    let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    assert_eq!(
        actual,
        cells([
            real(1.0),
            real(99.0),
            real(3.0),
            real(4.0),
            real(1.0),
            real(2.0),
            real(3.0),
            real(4.0)
        ])
    );
}
