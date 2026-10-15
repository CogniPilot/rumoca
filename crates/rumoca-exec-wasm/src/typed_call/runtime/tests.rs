use super::*;
use crate::WasmCompileError;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("typed_entry.mo"),
        5,
        17,
    )
}

fn table() -> solve::SolvePureCallTable {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let integer =
        solve::SolveValueType::scalar(solve::SolveScalarType::Integer(arithmetic.integer_domain()));
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![integer.clone()],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(),
            |body, inputs, outputs| {
                let value = body.load(inputs[0], span())?;
                let one =
                    body.constant(solve::SolveValue::integer(arithmetic, 1).unwrap(), span())?;
                let result = body.binary(solve::SolveBinaryOperator::Add, value, one, span())?;
                body.store(outputs[0], result, span())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn prepared_typed_entry_preserves_integer_bits_fault_span_atomicity_and_recovery() {
    let table = table();
    let site = table.owners()[0].call_site();
    let code = PreparedTypedCallWasm::compile(&table, &site).unwrap();
    assert_eq!(code.artifact().layout().input_bytes, 8);
    assert_eq!(code.artifact().layout().output_bytes, 8);
    let mut scratch = vec![0; code.artifact().layout().scratch_bytes as usize / 8];
    let mut output = [19];
    code.call(&[9_007_199_254_740_993], &mut output, &mut scratch)
        .unwrap();
    assert_eq!(output, [9_007_199_254_740_994]);
    output.fill(19);
    let error = code
        .call(&[i64::MAX as u64], &mut output, &mut scratch)
        .unwrap_err();
    let WasmCompileError::TypedSource(fault) = error else {
        panic!("Integer overflow retains its typed source fault");
    };
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(fault.provenance, span());
    assert_eq!(output, [19]);
    assert!(code.call(&[], &mut output, &mut scratch).is_err());
    assert_eq!(output, [19]);
    code.call(&[(-2_i64) as u64], &mut output, &mut scratch)
        .unwrap();
    assert_eq!(output, [(-1_i64) as u64]);
    assert!(PreparedTypedCallWasm::compile(&solve::SolvePureCallTable::default(), &site).is_err());
}

#[test]
fn host_admission_refuses_a_module_with_the_wrong_typed_entry_signature() {
    use wasm_encoder::*;
    let table = table();
    let mut artifact = compile_pure_call_wasm(&table, &table.owners()[0].call_site()).unwrap();
    let mut module = Module::new();
    let mut types = TypeSection::new();
    types.ty().function([], []);
    module.section(&types);
    let mut imports = ImportSection::new();
    imports.import(
        "env",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        }),
    );
    module.section(&imports);
    let mut functions = FunctionSection::new();
    functions.function(0);
    module.section(&functions);
    let mut exports = ExportSection::new();
    exports.export("eval_typed_call", ExportKind::Func, 0);
    module.section(&exports);
    let mut body = Function::new([]);
    body.instruction(&Instruction::End);
    let mut code = CodeSection::new();
    code.function(&body);
    module.section(&code);
    artifact.module_bytes = module.finish();
    let error = crate::host_typed_call_runtime::HostTypedCallRuntime::new(&artifact)
        .err()
        .unwrap();
    assert!(
        matches!(error, WasmCompileError::Backend(message) if message.starts_with("missing typed call export:"))
    );
}
