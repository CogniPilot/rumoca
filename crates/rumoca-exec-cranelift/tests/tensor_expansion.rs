//! Current-source fixture census. Historical known-open classifications are
//! deliberately not promoted: a qualified run must bind the current manifest.

#[path = "tensor_expansion/fixtures.rs"]
mod fixtures;

use rumoca_eval_solve::PureCallInvocation;
use rumoca_exec_cranelift::{
    CompiledPureCallTable, ExpansionEntry, ExpansionReport, ExpansionSource, ForcedExpansion,
    compile_expression_scalar_program_block, compile_pure_call_table, record_expansion_report,
};

fn retain(name: &str, report: &ExpansionReport) {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/tensor-expansion-fixtures");
    std::fs::create_dir_all(&root).unwrap();
    std::fs::write(root.join(format!("{name}.txt")), format!("{report:#?}\n")).unwrap();
    println!("E2 raw census retained: {name}");
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

fn invoke(compiled: &CompiledPureCallTable, fixture: &fixtures::TypedFixture) -> Vec<f64> {
    let site = fixture.table.owners()[0].call_site();
    let invocation = PureCallInvocation::Primal(&site);
    assert!(
        compiled.executes(invocation),
        "fixture must use native execution"
    );
    let mut output = vec![-123.0; fixture.expected.len()];
    compiled
        .call_scalar_payload(
            invocation,
            &fixture.input,
            &mut output,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    output
}

fn measure(name: &str, fixture: fixtures::TypedFixture) -> ExpansionReport {
    let plain = compile_pure_call_table(&fixture.table).unwrap();
    let expected = invoke(&plain, &fixture);
    assert_eq!(bits(&expected), bits(&fixture.expected));
    let (compiled, report) = record_expansion_report(|| compile_pure_call_table(&fixture.table));
    retain(name, &report);
    let compiled = compiled.unwrap();
    let actual = invoke(&compiled, &fixture);
    assert_eq!(
        bits(&actual),
        bits(&expected),
        "recording preserves every output bit"
    );
    assert!(report.complete, "{report:#?}");
    assert!(
        !report.entries.is_empty(),
        "missing collector is not an empty pass"
    );
    assert!(
        !report.programs.is_empty(),
        "canonical typed sources must be retained"
    );
    for entry in &report.entries {
        if let ExpansionSource::Typed { program, operation } = entry.source {
            assert_eq!(entry.call_table, report.program_tables[program]);
            let source = &report.programs[program].operations()[operation];
            assert_eq!(source.provenance(), entry.span.unwrap());
            assert!(entry.succeeded);
        }
    }
    report
}

fn operation<'a>(report: &'a ExpansionReport, kind: &str, elements: u64) -> &'a ExpansionEntry {
    report
        .entries
        .iter()
        .find(|entry| entry.kind == kind && entry.elements == elements)
        .unwrap_or_else(|| panic!("missing {kind}/{elements}: {report:#?}"))
}

#[test]
fn literal_source_growth_is_visible_at_four_shapes_and_bits_do_not_change() {
    for n in [16, 17, 64, 128] {
        let report = measure(&format!("literal-{n}"), fixtures::literal(n));
        let construct = operation(&report, "ConstructAggregate", u64::from(n));
        assert!(construct.instructions >= u64::from(n));
        assert_eq!(construct.is_suspect(), n > 16);
        retain(&format!("literal-{n}"), &report);
    }
}

#[test]
fn capture_pack_and_fold_body_keep_their_actual_loop_contexts() {
    for n in [16, 17, 64, 128] {
        let report = measure(&format!("map-{n}"), fixtures::map(n));
        let map = operation(&report, "Map", u64::from(n));
        let packed = map
            .copies
            .iter()
            .find(|copy| copy.cells == u64::from(n * 32))
            .expect("fallback Map packs its immutable capture");
        assert!(
            packed.loop_bounds.is_empty(),
            "capture pack is outside Map iteration"
        );
        retain(&format!("map-{n}"), &report);
        let report = measure(&format!("fold-{n}"), fixtures::fold(n));
        let update = operation(&report, "UpdateElement", u64::from(n));
        assert_eq!(update.loop_bounds, [u64::from(n)]);
        assert!(update.parent.is_some());
        // A current in-place update may copy zero cells. Keep the raw census;
        // do not assert the historical full-vector copy still occurs.
        retain(&format!("fold-{n}"), &report);
    }
}

fn measure_row(n: u32, stride: usize) -> ExpansionReport {
    let source = fixtures::row(n, stride);
    let y: Vec<_> = (0..(n as usize * stride).max(256))
        .map(|i| if i % 2 == 0 { -0.0 } else { i as f64 })
        .collect();
    let plain = compile_expression_scalar_program_block(&source).unwrap();
    let mut expected = vec![0.0; n as usize];
    plain.call(&y, &[], 0.0, &mut expected).unwrap();
    assert_eq!(
        bits(&expected),
        bits(
            &y.iter()
                .step_by(stride)
                .take(n as usize)
                .copied()
                .collect::<Vec<_>>()
        )
    );
    let (compiled, report) =
        record_expansion_report(|| compile_expression_scalar_program_block(&source));
    retain(&format!("row-{stride}-{n}"), &report);
    let mut actual = vec![0.0; n as usize];
    compiled.unwrap().call(&y, &[], 0.0, &mut actual).unwrap();
    assert_eq!(bits(&actual), bits(&expected));
    assert!(report.complete, "{report:#?}");
    assert!(!report.entries.is_empty());
    report
}

#[test]
fn contiguous_output_uses_bounded_code_and_strided_output_is_still_reported() {
    let mut large = Vec::new();
    for n in [16, 17, 64, 128] {
        let report = measure_row(n, 1);
        let output = operation(&report, "StoreOutputRange", u64::from(n));
        assert_eq!(output.path, "row-tape-range");
        assert!(output.copies.iter().any(|copy| copy.cells == u64::from(n)));
        if n >= 64 {
            large.push(output.instructions);
        }
        retain(&format!("row-contiguous-{n}"), &report);
        let strided = measure_row(n, 2);
        assert_eq!(
            operation(&strided, "StoreOutputRange", u64::from(n)).path,
            "row-scalar-range"
        );
        retain(&format!("row-strided-{n}"), &strided);
    }
    assert_eq!(
        large[0], large[1],
        "64->128 cells must not expand the counted output loop"
    );
}

#[test]
fn deliberately_static_matrix_path_reports_its_existing_bound() {
    let report = measure("matrix-static", fixtures::small_product());
    let product = operation(&report, "MatrixMultiply", 51);
    assert_eq!(product.path, "static-matrix");
    assert_eq!(
        product.forced,
        Some(ForcedExpansion::StaticUnroll { limit: 64 })
    );
    retain("matrix-static", &report);
}

#[test]
fn a_row_call_retains_the_exact_table_compiled_outside_the_collector() {
    use rumoca_ir_solve::{LinearOp, ScalarProgramBlock};
    let fixture = fixtures::literal(64);
    let calls = compile_pure_call_table(&fixture.table).unwrap();
    let source = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::PureCall {
                dst_start: 0,
                input_starts: Box::new([]),
                site: fixture.table.owners()[0].call_site(),
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 64,
                stride: 1,
            },
        ]],
        vec![fixtures::span()],
        (0..64).collect(),
    )
    .unwrap();
    let (compiled, report) = record_expansion_report(|| {
        rumoca_exec_cranelift::compile_expression_scalar_program_block_with_pure_calls(
            &source, &calls,
        )
    });
    retain("row-external-call-table", &report);
    let mut output = vec![0.0; 64];
    compiled.unwrap().call(&[], &[], 0.0, &mut output).unwrap();
    assert_eq!(bits(&output), bits(&fixture.expected));
    assert!(report.complete);
    assert_eq!(report.tables.len(), 1);
    assert!(report.tables[0].shares_table_owner(&fixture.table));
    let call = report
        .entries
        .iter()
        .find(|entry| entry.kind == "PureCall")
        .unwrap();
    assert_eq!(call.call_table, Some(0));
}

#[test]
fn fused_emission_names_its_original_producers_and_preserves_term_order() {
    let report = measure("fused-term-order", fixtures::fused(64));
    let deferred = report
        .entries
        .iter()
        .find(|entry| entry.path == "deferred-fusion")
        .unwrap();
    assert_eq!(deferred.instructions, 0);
    assert_eq!(
        deferred.elements, 64,
        "deferred values keep their canonical source shape"
    );
    let ExpansionSource::Typed { program, operation } = deferred.source else {
        panic!("typed source")
    };
    assert!(report.entries.iter().any(|entry| matches!(entry.source,
        ExpansionSource::Typed { program: consumer_program, .. } if consumer_program == program)
        && entry.fused_operations.contains(&operation)));
}
