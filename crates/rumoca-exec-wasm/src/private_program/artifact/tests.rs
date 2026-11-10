//! Actual compiler -> opaque wire -> checked bind -> original runtime controls.
use super::*;
use rumoca_ir_solve::{LinearOp, SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat};

fn constant(value: f64) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("precompiled.mo"),
        0,
        1,
    );
    ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![span],
    )
    .unwrap()
}

#[test]
fn actual_private_module_roundtrip_preserves_outputs_and_refuses_foreign_inputs() {
    let layout = VarLayout::default();
    let calls = SolvePureCallTable::builder(SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::FULL,
    ))
    .finish();
    let blocks = [constant(2.0), constant(-0.0)];
    let requests = blocks.iter().map(|block| (block, None)).collect::<Vec<_>>();
    let original = super::super::compile_private_programs_wasm(&requests, &layout, &calls).unwrap();
    let wire = PrecompiledPrivatePrograms::from_compiled(&original);
    let bytes = serde_json::to_string(&wire).unwrap();
    let decoded: PrecompiledPrivatePrograms = serde_json::from_str(&bytes).unwrap();
    let owner = rumoca_ir_solve::PreparedKernelSource::new(&layout, &calls).unwrap();
    let replayed = decoded.bind(&owner, &requests, &layout).unwrap();
    for (a, b) in original.iter().zip(&replayed) {
        let mut x = vec![0.; (a.scratch_bytes() as usize).div_ceil(8)];
        let mut y = vec![0.; (b.scratch_bytes() as usize).div_ceil(8)];
        a.call(&[], &[], 0., &mut x).unwrap();
        b.call(&[], &[], 0., &mut y).unwrap();
        assert_eq!(
            x.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            y.iter().map(|v| v.to_bits()).collect::<Vec<_>>()
        );
    }
    assert!(decoded.bind(&owner, &requests[..1], &layout).is_err());
    assert!(
        decoded
            .bind(&owner, &[(&blocks[1], None), (&blocks[0], None)], &layout)
            .is_err()
    );
    assert!(
        decoded
            .bind(
                &owner,
                &[(&blocks[0], Some(1)), (&blocks[1], None)],
                &layout
            )
            .is_err()
    );
    let foreign = constant(3.0);
    assert!(
        decoded
            .bind(&owner, &[(&foreign, None), (&blocks[1], None)], &layout)
            .is_err()
    );
    let mut corrupt = serde_json::to_value(&decoded).unwrap();
    corrupt["modules"][0]["bytes"][0] = serde_json::json!(255);
    let corrupt: PrecompiledPrivatePrograms = serde_json::from_value(corrupt).unwrap();
    assert!(corrupt.bind(&owner, &requests, &layout).is_err());
    let mut huge = serde_json::to_value(&decoded).unwrap();
    huge["modules"][0]["scratch_bytes"] = serde_json::json!(u32::MAX);
    let huge: PrecompiledPrivatePrograms = serde_json::from_value(huge).unwrap();
    assert!(huge.bind(&owner, &requests, &layout).is_err());
}
