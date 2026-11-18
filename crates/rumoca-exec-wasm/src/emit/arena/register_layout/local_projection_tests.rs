use rumoca_ir_solve::LinearOp;

#[test]
fn diagnostic_scalar_entry_requires_opt_in_and_refuses_an_address() {
    use crate::emit::{BodyEmitter, ImportCatalog, arena::ArenaPlan};
    let program = vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let arena = ArenaPlan::required(&[program], 0).unwrap();
    let imports = ImportCatalog {
        eval_function_index: 0,
        function_indices: Default::default(),
    };
    let mut function = wasm_encoder::Function::new([]);
    let mut emitter = BodyEmitter::new(&imports, &mut function);
    emitter.arena = Some(arena);
    emitter.bind_root_program().unwrap();
    assert_eq!(emitter.diagnostic_scalar_local(0).unwrap(), None);
    emitter.diagnostic_scalar_base = Some(32);
    assert_eq!(emitter.diagnostic_scalar_local(0).unwrap(), Some(32));
    assert!(emitter.push_arena_address(0, None, 0).is_err());
    emitter.register_base = 1;
    assert_eq!(emitter.diagnostic_scalar_local(0).unwrap(), None);
}
