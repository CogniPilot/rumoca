//! Modelica sign semantics must survive CPU/WASM execution without JavaScript Math.sign.
use rumoca_exec_wasm::compile_expression_compute_block_wasm_bytes;
use rumoca_ir_solve::*;
mod support;
use support::{assert_bits, execute, scalar_reference};

#[test]
fn sign_matches_canonical_scalar_oracle_on_ieee_edges() {
    let inputs = vec![
        f64::NEG_INFINITY,
        -1.0,
        -f64::MIN_POSITIVE,
        -f64::from_bits(1),
        -0.0,
        0.0,
        f64::from_bits(1),
        f64::MIN_POSITIVE,
        1.0,
        f64::INFINITY,
        f64::NAN,
        f64::from_bits(0xfff8_0000_0000_0042),
    ];
    let programs = (0..inputs.len())
        .map(|index| {
            vec![
                LinearOp::LoadP { dst: 0, index },
                LinearOp::Unary {
                    dst: 1,
                    op: UnaryOp::Sign,
                    arg: 0,
                },
                LinearOp::StoreOutput { src: 1 },
            ]
        })
        .collect();
    let source = ScalarProgramBlock::with_source_span(
        programs,
        source_span_from_offsets(1, 0, 1)
            .require_provenance("sign test")
            .unwrap(),
    )
    .unwrap();
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(source)],
    };
    let layout = VarLayout::from_parts(Default::default(), 0, inputs.len());
    let bytes = compile_expression_compute_block_wasm_bytes(&block, &layout).unwrap();
    let module = wasmi::Module::new(&wasmi::Engine::default(), &bytes[..]).unwrap();
    assert!(module.imports().all(|import| import.name() != "sign"));
    let canonical = inputs
        .iter()
        .copied()
        .map(rumoca_core::modelica_sign)
        .collect::<Vec<_>>();
    assert_bits(&scalar_reference(&block, &[], &inputs), &canonical);
    assert_bits(&execute(&bytes, &[], &inputs, inputs.len()), &canonical);
}
