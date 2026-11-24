//! A block's ES016 fold is read from the problem of the chart the block
//! belongs to: an alternate chart numbers its canonical blocks its own way,
//! so the same index names another block in the primary chart.

use super::*;

/// A split circle whose charts number their blocks differently, beside a
/// limiter loop with a strict `noEvent` guard (ES016) that reads a chart
/// unknown.
const MODEL: &str = "model FoldChart\n  parameter Real V0 = 15000;\n  parameter Real k = 0.5;\n  Real v_in;\n  Real v_out;\n  Real x(start = 1);\n  Real y(start = 0, fixed = true);\n  Real vx(start = 0);\n  Real vy(start = 1, fixed = true);\n  Real lambda;\nequation\n  v_in = k * v_out - 10 * sin(20 * time) + 0.01 * x;\n  v_out = smooth(0, noEvent(if V0 * v_in > 15 then 15 else if V0 * v_in < -15 then -15 else V0 * v_in));\n  der(x) = vx;\n  der(y) = vy;\n  der(vx) = lambda*x;\n  der(vy) = lambda*y;\n  x*x + y*y = 1;\nend FoldChart;\n";

/// The integers of the C array initializer that follows `declaration`.
fn array<'a>(source: &'a str, declaration: &str) -> Vec<&'a str> {
    let start = source
        .find(declaration)
        .unwrap_or_else(|| panic!("no {declaration}"));
    let body = &source[start..];
    let open = body.find('{').expect("an initializer") + 1;
    let close = open + body[open..].find("\n};").expect("the initializer end");
    body[open..close]
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
        .collect()
}

#[test]
fn every_chart_reports_the_fold_of_its_own_guarded_block() {
    let compiled = rumoca::Compiler::new()
        .model("FoldChart")
        .compile_str(MODEL, "FoldChart.mo")
        .expect("compile the folded chart model");
    let work = tempdir().expect("fold work directory");
    let fmu = build_named_fmu(work.path(), &compiled, "fmi3", "FoldChart");
    let source = fs::read_to_string(fmu.root.join("sources/model.c")).expect("read model.c");
    assert!(
        source.contains("#define RMC_NCHARTS 2"),
        "the model switches charts"
    );
    let pool: Vec<usize> = array(&source, "rmc_pool[")
        .iter()
        .map(|item| item.parse().expect("a pool entry"))
        .collect();
    let folds = array(&source, "rmc_block_fold[");
    let start = source
        .find("rmc_block_desc[")
        .expect("the block descriptors");
    let end = start + source[start..].find("\n};").expect("the descriptor end");
    let unknowns: Vec<Vec<usize>> = source[start..end]
        .lines()
        .filter_map(|line| line.trim().strip_prefix('{')?.strip_suffix("},"))
        .map(|fields| {
            let fields: Vec<usize> = fields
                .split(',')
                .take(4)
                .map(|field| field.trim().parse().expect("a descriptor count"))
                .collect();
            // id, canonical, n, y
            let (n, y) = (fields[2], fields[3]);
            let mut unknowns = pool[y..y + n].to_vec();
            unknowns.sort_unstable();
            unknowns
        })
        .collect();
    assert_eq!(unknowns.len(), folds.len(), "one fold per block");
    let folded: std::collections::BTreeSet<&Vec<usize>> = unknowns
        .iter()
        .zip(&folds)
        .filter(|(_, fold)| **fold != "RMC_NO_FOLD")
        .map(|(unknowns, _)| unknowns)
        .collect();
    assert_eq!(
        folded.len(),
        1,
        "the folded blocks solve other unknowns: {folded:?}"
    );
    let guarded = *folded.iter().next().expect("a folded block");
    let blocks = unknowns
        .iter()
        .filter(|unknowns| *unknowns == guarded)
        .count();
    assert_eq!(blocks, 2, "each chart has the guarded block once");
    for (unknowns, fold) in unknowns.iter().zip(&folds) {
        assert_eq!(
            unknowns == guarded,
            *fold != "RMC_NO_FOLD",
            "block over {unknowns:?}"
        );
    }
}
