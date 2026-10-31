//! #483 size pass: a single-use Real expression the Solve C printer folds
//! into its consumer yields the bits of the Solve evaluator, which rounds
//! every operation to the profile's format. The inputs pin what folding could
//! change: signed zeros, subnormal results and ties, half-ULP ties, overflow
//! to infinity and NaN.

use std::fs;
use std::path::Path;

use rumoca_core::SourceMap;
use rumoca_eval_solve::eval_algorithm_method;
use rumoca_ir_galec::ast::{
    BinaryOp, Block, Expression, InterfaceKind, InterfaceVariable, Name, Reference, ScalarType,
};
use rumoca_ir_galec::package::AlgorithmCodePackage;
use rumoca_ir_solve::{SolveAlgorithmBlock, SolveBlockMethodKind, SolveValue};
use rumoca_phase_codegen::SolveBlockTemplateRenderer;
use rumoca_phase_solve::lower_algorithm_block;

use super::cc_support;
use super::galec_solve_c_faults::{
    Spans, assign, bin, decl, profile, scalar, snapshot, sref, zero,
};

/// Inputs `a`, `b`, `c`, `d`; outputs `y = (a * b + c) - d` and
/// `z = -a * c + (b - d)`; tunable `period`.
fn block() -> Block {
    let mut spans = Spans(0);
    let mut block = Block::new(Name::ident("Inline"));
    block.span = spans.next();
    let real = |kind, text, spans: &mut Spans| InterfaceVariable {
        kind,
        decl: decl(ScalarType::Real, text, spans),
        start: Some(zero(ScalarType::Real)),
    };
    block.interface = ["a", "b", "c", "d"]
        .into_iter()
        .map(|text| real(InterfaceKind::Input, text, &mut spans))
        .collect();
    block
        .interface
        .push(real(InterfaceKind::Output, "y", &mut spans));
    block
        .interface
        .push(real(InterfaceKind::Output, "z", &mut spans));
    block.interface.push(InterfaceVariable {
        kind: InterfaceKind::TunableParameter,
        decl: decl(ScalarType::Real, "period", &mut spans),
        start: Some(Expression::Real(0.01)),
    });
    block.startup.statements = vec![
        assign("y", Expression::Real(0.0), &mut spans),
        assign("z", Expression::Real(0.0), &mut spans),
    ];
    let product = bin(BinaryOp::Mul, sref("a"), sref("b"));
    let y = bin(
        BinaryOp::Sub,
        bin(BinaryOp::Add, product, sref("c")),
        sref("d"),
    );
    let negated = Expression::Neg(Reference::state(Name::ident("a")));
    let z = bin(
        BinaryOp::Add,
        bin(BinaryOp::Mul, negated, sref("c")),
        bin(BinaryOp::Sub, sref("b"), sref("d")),
    );
    block.do_step.statements = vec![assign("y", y, &mut spans), assign("z", z, &mut spans)];
    block
}

/// `a`, `b`, `c`, `d` as binary32 bit patterns.
const CASES: [(&str, [u32; 4]); 9] = [
    // y = -0 + -0 - 0 = -0.
    (
        "negative_zero_sum",
        [0x8000_0000, 0x3f80_0000, 0x8000_0000, 0],
    ),
    // z = -0 * 1 + (-0 - 0) = -0.
    ("negative_zero_product", [0, 0x8000_0000, 0x3f80_0000, 0]),
    // y = MIN_POSITIVE * 0.5, a subnormal.
    ("subnormal_product", [0x0080_0000, 0x3f00_0000, 0, 0]),
    // y = 3 * 2^-149 * 0.5 ties to even at 2 * 2^-149.
    ("subnormal_tie", [3, 0x3f00_0000, 0, 0]),
    // 1 + 2^-24 ties to 1, so y = 0, not 2^-24.
    (
        "half_ulp_tie_down",
        [0x3f80_0000, 0x3f80_0000, 0x3380_0000, 0x3f80_0000],
    ),
    // 1 + 3 * 2^-24 ties to 1 + 2^-22, so y = 2^-22.
    (
        "half_ulp_tie_up",
        [0x3f80_0000, 0x3f80_0000, 0x3440_0000, 0x3f80_0000],
    ),
    // MAX * 2 overflows before - MAX can bring it back.
    (
        "overflow_then_subtract",
        [0x7f7f_ffff, 0x4000_0000, 0, 0x7f7f_ffff],
    ),
    // inf * 0 is NaN.
    ("infinity_times_zero", [0x7f80_0000, 0, 0x3f80_0000, 0]),
    // inf - inf in b - d.
    ("infinity_minus_infinity", [0, 0x7f80_0000, 0, 0x7f80_0000]),
];

fn evaluator_trace(block: &SolveAlgorithmBlock, inputs: [u32; 4]) -> String {
    let mut state = vec![None; block.slots().len()];
    let profile = profile();
    for (slot, bits) in state.iter_mut().zip(inputs) {
        let value = SolveValue::real(profile, f64::from(f32::from_bits(bits)));
        *slot = Some(scalar(value));
    }
    state[6] = Some(scalar(SolveValue::real(profile, 0.01)));
    let mut trace = String::new();
    for (method, at) in [
        (SolveBlockMethodKind::Startup, "startup"),
        (SolveBlockMethodKind::DoStep, "dostep"),
    ] {
        let failed = eval_algorithm_method(block, method, &mut state).is_err();
        trace.push_str(&format!("{at} {}\n", snapshot(block, &state, failed)));
    }
    trace
}

const DRIVER: &str = r#"#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include "Inline.h"

static float real(uint32_t word) {
    float value;
    memcpy(&value, &word, sizeof value);
    return value;
}

static void dump(const InlineState *s, const char *at) {
    const float *fields[] = {&s->a, &s->b, &s->c, &s->d, &s->y, &s->z, &s->period};
    printf("%s", at);
    for (size_t i = 0; i < sizeof fields / sizeof fields[0]; ++i) {
        uint32_t word;
        memcpy(&word, fields[i], sizeof word);
        printf(" %08x", (unsigned int)word);
    }
    printf(" %08x\n", (unsigned int)s->rumoca_galec_error_signal_status);
}

int main(void) {
    InlineState s;
    memset(&s, 0, sizeof s);
    s.a = real(A); s.b = real(B); s.c = real(C); s.d = real(D); s.period = 0.01f;
    Inline_startup(&s);
    dump(&s, "startup");
    Inline_dostep(&s);
    dump(&s, "dostep");
    return 0;
}
"#;

/// Renders the block's C into `dir`, returning the C source.
fn render(block: &SolveAlgorithmBlock, package: &AlgorithmCodePackage, dir: &Path) -> String {
    let sources = SourceMap::default();
    let renderer = SolveBlockTemplateRenderer::new(package, block, &sources).expect("renderer");
    let mut c = String::new();
    for (template, file) in [("model.h.jinja", "Inline.h"), ("model.c.jinja", "Inline.c")] {
        let source =
            rumoca_phase_codegen::templates::builtin_template_source("embedded-c-galec", template)
                .expect("bundled");
        c = renderer.render_with_name(source, "Inline").expect("render");
        fs::write(dir.join(file), &c).expect("write");
    }
    c
}

fn c_trace(name: &str, inputs: [u32; 4], dir: &Path) -> String {
    let mut driver = DRIVER.to_owned();
    for (field, bits) in ["(A)", "(B)", "(C)", "(D)"].into_iter().zip(inputs) {
        driver = driver.replace(field, &format!("(0x{bits:08x}u)"));
    }
    fs::write(dir.join("driver.c"), driver).expect("write driver");
    let exe = dir.join(name);
    let output = cc_support::cc()
        .args([
            "-O2",
            "-std=c99",
            "-ffp-contract=off",
            "-Wall",
            "-Wextra",
            "-Werror",
        ])
        .arg("-o")
        .arg(&exe)
        .arg(dir.join("Inline.c"))
        .arg(dir.join("driver.c"))
        .arg("-lm")
        .output()
        .expect("cc");
    assert!(
        output.status.success(),
        "{name}: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let run = std::process::Command::new(&exe).output().expect("run");
    String::from_utf8_lossy(&run.stdout).into_owned()
}

#[test]
fn inlined_real_expressions_keep_the_evaluators_bits_at_fp_edges() {
    let dir = tempfile::tempdir().expect("work directory");
    let package = AlgorithmCodePackage::construct(block(), vec![None; 7], "period").unwrap();
    let solve = lower_algorithm_block(&package, profile()).unwrap();
    let c = render(&solve, &package, dir.path());
    let differing: Vec<_> = CASES
        .into_iter()
        .filter(|(name, inputs)| {
            c_trace(name, *inputs, dir.path()) != evaluator_trace(&solve, *inputs)
        })
        .map(|(name, _)| name)
        .collect();
    assert!(
        differing.is_empty(),
        "C differs from the evaluator: {differing:?}"
    );
    for folded in [
        "(&self->y)[0] = ((((&self->a)[0] * (&self->b)[0]) + (&self->c)[0]) - (&self->d)[0]);",
        "(&self->z)[0] = ((((float)(-(&self->a)[0])) * (&self->c)[0]) + ((&self->b)[0] - (&self->d)[0]));",
    ] {
        assert!(c.contains(folded), "the C folds each expression:\n{c}");
    }
}
