use super::*;
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve::{BinaryOp, LinearOp, SolvePureCallTable, TensorIndex};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("selected_wasm.mo"), 7, 29)
}

fn source(directional: bool) -> ScalarProgramBlock {
    let first = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        if directional {
            LinearOp::LoadSeed { dst: 1, index: 0 }
        } else {
            LinearOp::Const { dst: 1, value: 3.0 }
        },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
        LinearOp::Const {
            dst: 3,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    let mut second = vec![
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 0,
            stride: 1,
            dimensions: vec![1].into(),
            indices: vec![TensorIndex::Runtime(1)].into(),
        },
    ];
    if directional {
        second.push(LinearOp::LoadSeed { dst: 3, index: 2 });
    }
    second.push(LinearOp::StoreOutput { src: 2 });
    ScalarProgramBlock::with_output_indices(vec![first, second], vec![span(); 2], vec![4, 1, 0])
        .unwrap()
}

fn numeric(source: &ScalarProgramBlock) -> WasmExpression {
    WasmExpression::compile(
        source,
        &VarLayout::from_parts(Default::default(), 1, 0),
        &SolvePureCallTable::default(),
    )
    .unwrap()
}

#[test]
fn selected_numeric_entry_requires_exact_owner_and_outlives_its_caller() {
    let source = source(false);
    let code = numeric(&source);
    let retained = Rc::downgrade(&code.selected[0]);
    let replay: ScalarProgramBlock =
        serde_json::from_value(serde_json::to_value(&source).unwrap()).unwrap();
    assert_eq!(source.programs(), replay.programs());
    assert_eq!(source.program_spans(), replay.program_spans());
    assert_eq!(source.output_indices(), replay.output_indices());
    assert!(!source.shares_program_owner(&replay));
    assert!(code.bind_program(&replay, 0).is_err());
    assert!(code.bind_program(&source, 2).is_err());
    let selected = code.bind_program(&source.clone(), 0).unwrap().unwrap();
    drop(code);
    drop(source);
    assert!(retained.upgrade().is_some());
    let mut out = [19.0; 2];
    selected.call(&[2.0], &[], 0.0, &[], &mut out).unwrap();
    assert_eq!(out[0], 6.0);
    assert_eq!(out[1].to_bits(), (-0.0_f64).to_bits());
    let mut wrong = [19.0; 5];
    assert!(selected.call(&[2.0], &[], 0.0, &[], &mut wrong).is_err());
    assert_eq!(wrong, [19.0; 5]);
    drop(selected);
    assert!(retained.upgrade().is_none());
}

#[test]
fn binding_selected_programs_shares_existing_code_layout_and_scratch() {
    let source = source(false);
    let code = numeric(&source);
    let pointer = code.selected[0].1.borrow().as_ptr();
    let capacity = code.selected[0].1.borrow().capacity();
    let bindings = (0..20_000)
        .map(|_| code.bind_selected(&source, 0).unwrap())
        .collect::<Vec<_>>();
    for bound in &bindings {
        assert!(Rc::ptr_eq(&bound.code, &code.selected[0]));
        assert!(Rc::ptr_eq(&bound.layout, &code.layout));
        assert!(bound._source.shares_program_owner(&source));
        assert_eq!(bound.code.1.borrow().as_ptr(), pointer);
        assert_eq!(bound.code.1.borrow().capacity(), capacity);
    }
    assert_eq!(Rc::strong_count(&code.selected[0]), 20_001);
    assert!(code.bind_directional(&source, 0, 0).is_err());
}

#[test]
fn selected_numeric_fault_is_lazy_transactional_and_keeps_original_span() {
    let source = source(false);
    let code = numeric(&source);
    let first = code.bind_program(&source, 0).unwrap().unwrap();
    let second = code.bind_program(&source, 1).unwrap().unwrap();
    let mut tuple = [19.0; 2];
    first.call(&[2.0], &[], 0.0, &[], &mut tuple).unwrap();
    assert_eq!(tuple[0], 6.0);
    let mut scalar = [-0.0];
    let error = second.call(&[2.0], &[], 0.0, &[], &mut scalar).unwrap_err();
    assert_eq!(error.source_span(), Some(span()));
    assert_eq!(scalar[0].to_bits(), (-0.0_f64).to_bits());
    second.call(&[1.0], &[], 0.0, &[], &mut scalar).unwrap();
    assert_eq!(scalar, [7.0]);
}

#[test]
fn selected_directional_entry_retains_seed_contract_and_exact_source() {
    let source = source(true);
    let code = crate::wasm_execution::jacobians::WasmJacobian::compile(
        &source,
        &VarLayout::from_parts(Default::default(), 1, 0),
        &SolvePureCallTable::default(),
    )
    .unwrap();
    let replay: ScalarProgramBlock =
        serde_json::from_value(serde_json::to_value(&source).unwrap()).unwrap();
    assert!(code.bind_program(&replay, 0).is_err());
    assert!(code.bind_program(&source, 2).is_err());
    let selected = code.bind_program(&source, 0).unwrap().unwrap();
    drop(code);
    drop(source);
    let mut out = [19.0; 2];
    for seed in [3.0, 5.0, -0.0] {
        selected
            .call(&[2.0], &[], 0.0, &[seed, 0.0, 0.0], &[], &mut out)
            .unwrap();
        assert_eq!(out[0].to_bits(), (2.0_f64 * seed).to_bits());
        assert_eq!(out[1].to_bits(), (-0.0_f64).to_bits());
    }
    let before = out.map(f64::to_bits);
    assert!(
        selected
            .call(&[2.0], &[], 0.0, &[3.0], &[], &mut out)
            .is_err()
    );
    assert_eq!(out.map(f64::to_bits), before);
}
