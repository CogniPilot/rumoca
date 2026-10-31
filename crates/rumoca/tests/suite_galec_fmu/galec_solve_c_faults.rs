//! #483: on the fault path the Solve-printed C does what the Solve
//! evaluator does. A failed Recalibrate/DoStep keeps every store and raised
//! signal before the fault, a failed Startup restores the state but keeps its
//! raised signals, and the C status word adds bit 31 (`RUMOCA_GALEC_FAULT`).
//! Each case runs the same lifecycle through C and through the evaluator at
//! the embedded profile and compares every declaration's bits and the status.

use std::fs;
use std::path::Path;

use rumoca_core::{SourceId, SourceMap, Span};
use rumoca_eval_solve::{TypedValue, eval_algorithm_method};
use rumoca_ir_galec::ast::{
    BinaryOp, Block, Condition, Expression, FunctionCall, IfBranch, IfExpression, IfStatement,
    InterfaceKind, InterfaceVariable, Name, ProtectedEntity, ProtectedKind, Reference, ScalarType,
    Spanned, Statement, VariableDeclaration,
};
use rumoca_ir_galec::package::AlgorithmCodePackage;
use rumoca_ir_solve::{
    SolveAlgorithmBlock, SolveArithmeticProfile, SolveBlockMethodKind, SolveIntegerDomain,
    SolveRealFormat, SolveStorageClass, SolveValue, SolveValueKind,
};
use rumoca_phase_codegen::SolveBlockTemplateRenderer;
use rumoca_phase_solve::lower_algorithm_block;

use super::cc_support;

fn profile() -> SolveArithmeticProfile {
    SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).unwrap(),
    )
}

/// Spans numbered in order: Solve provenance must be exact.
struct Spans(usize);

impl Spans {
    fn next(&mut self) -> Span {
        self.0 += 1;
        Span::from_offsets(SourceId::from_source_name("faults.alg"), self.0, self.0 + 1)
    }
}

fn name(text: &str, spans: &mut Spans) -> Name {
    Name::Ident(rumoca_ir_galec::ast::Identifier::new(text), spans.next())
}

fn decl(ty: ScalarType, text: &str, spans: &mut Spans) -> VariableDeclaration {
    let mut decl = VariableDeclaration::scalar(ty, name(text, spans));
    decl.span = spans.next();
    decl
}

fn sref(text: &str) -> Expression {
    Expression::Ref(Reference::state(Name::ident(text)))
}

fn assign(target: &str, value: Expression, spans: &mut Spans) -> Spanned<Statement> {
    Spanned {
        node: Statement::Assignment {
            target: Reference::state(Name::ident(target)),
            value,
        },
        span: spans.next(),
    }
}

fn bin(op: BinaryOp, lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
    }
}

/// A literal start of the type's zero: the Algorithm Code view needs start
/// semantics on every declaration, which a DAE projection always supplies.
fn zero(ty: ScalarType) -> Expression {
    match ty {
        ScalarType::Real => Expression::Real(0.0),
        ScalarType::Integer => Expression::Integer(0),
        _ => Expression::Bool(false),
    }
}

fn interface(
    kind: InterfaceKind,
    ty: ScalarType,
    text: &str,
    spans: &mut Spans,
) -> InterfaceVariable {
    InterfaceVariable {
        kind,
        decl: decl(ty, text, spans),
        start: Some(zero(ty)),
    }
}

/// Inputs `flag`, `x`, `k`, `j`; output `y`; tunable `period`; state `count`.
fn block(
    startup: impl FnOnce(&mut Spans) -> Vec<Spanned<Statement>>,
    do_step: impl FnOnce(&mut Spans) -> Vec<Spanned<Statement>>,
) -> Block {
    let mut spans = Spans(0);
    let mut block = Block::new(Name::ident("Faults"));
    block.span = spans.next();
    block.interface = vec![
        interface(
            InterfaceKind::Input,
            ScalarType::Boolean,
            "flag",
            &mut spans,
        ),
        interface(InterfaceKind::Input, ScalarType::Real, "x", &mut spans),
        interface(InterfaceKind::Input, ScalarType::Integer, "k", &mut spans),
        interface(InterfaceKind::Input, ScalarType::Integer, "j", &mut spans),
        interface(InterfaceKind::Output, ScalarType::Real, "y", &mut spans),
        InterfaceVariable {
            kind: InterfaceKind::TunableParameter,
            decl: decl(ScalarType::Real, "period", &mut spans),
            start: Some(Expression::Real(0.01)),
        },
    ];
    block.protected = vec![ProtectedEntity {
        kind: ProtectedKind::State,
        decl: decl(ScalarType::Integer, "count", &mut spans),
        start: Some(Expression::Integer(0)),
    }];
    block.startup.statements = startup(&mut spans);
    block.do_step.statements = do_step(&mut spans);
    block
}

fn plain_startup(spans: &mut Spans) -> Vec<Spanned<Statement>> {
    vec![
        assign("y", Expression::Real(0.0), spans),
        assign("count", Expression::Integer(0), spans),
    ]
}

/// The external values both sides start from.
#[derive(Clone, Copy)]
struct Inputs {
    flag: bool,
    x: f32,
    k: i32,
    j: i32,
}

struct Case {
    name: &'static str,
    block: Block,
    inputs: Inputs,
}

fn cases() -> Vec<Case> {
    let overflow = Inputs {
        flag: true,
        x: 1.0,
        k: i32::MAX,
        j: 1,
    };
    vec![
        Case {
            // A split `if`: y is stored before count overflows.
            name: "store_then_overflow",
            block: block(plain_startup, |spans| {
                let body = vec![
                    assign("y", Expression::Real(1.0), spans),
                    assign(
                        "count",
                        bin(BinaryOp::Add, sref("k"), Expression::Integer(1)),
                        spans,
                    ),
                ];
                vec![Spanned {
                    node: Statement::If(IfStatement {
                        branches: vec![IfBranch {
                            condition: Condition::Expression(sref("flag")),
                            body,
                            span: spans.next(),
                        }],
                        else_body: None,
                    }),
                    span: spans.next(),
                }]
            }),
            inputs: overflow,
        },
        Case {
            // Startup raises NAN on a NaN input, then overflows.
            name: "startup_signal_then_overflow",
            block: block(
                |spans| {
                    vec![
                        assign(
                            "y",
                            Expression::If(IfExpression::new(
                                vec![(
                                    bin(BinaryOp::Gt, sref("x"), Expression::Real(0.0)),
                                    Expression::Real(1.0),
                                )],
                                Expression::Real(2.0),
                            )),
                            spans,
                        ),
                        assign(
                            "count",
                            bin(BinaryOp::Add, sref("k"), Expression::Integer(1)),
                            spans,
                        ),
                    ]
                },
                |spans| vec![assign("y", Expression::Real(3.0), spans)],
            ),
            inputs: Inputs {
                x: f32::NAN,
                ..overflow
            },
        },
        Case {
            // DoStep writes y and count, then divides by zero.
            name: "store_then_division_by_zero",
            block: block(plain_startup, |spans| {
                vec![
                    assign("y", Expression::Real(5.0), spans),
                    assign("count", Expression::Integer(7), spans),
                    assign(
                        "count",
                        Expression::Call(FunctionCall {
                            function: Name::ident("divisionTowardsZero"),
                            arguments: vec![sref("k"), sref("j")],
                        }),
                        spans,
                    ),
                ]
            }),
            inputs: Inputs {
                k: 3,
                j: 0,
                ..overflow
            },
        },
    ]
}

fn element(value: &TypedValue) -> String {
    value
        .elements()
        .iter()
        .map(|element| match element {
            SolveValueKind::Real32(bits) => format!("{bits:08x}"),
            SolveValueKind::Integer(value) => format!("{:08x}", *value as i32 as u32),
            SolveValueKind::Boolean(value) => format!("{}", u8::from(*value)),
            SolveValueKind::Real64(bits) => panic!("binary64 element {bits:#x}"),
        })
        .collect::<Vec<_>>()
        .join(",")
}

fn scalar(value: SolveValue) -> TypedValue {
    TypedValue::construct(value.value_type().clone(), vec![value.kind()]).unwrap()
}

/// The evaluator's trace: after each method, every declaration then the
/// status word (signal bits, plus bit 31 when the method failed).
fn evaluator_trace(block: &SolveAlgorithmBlock, inputs: Inputs) -> String {
    let mut state = vec![None; block.slots().len()];
    let profile = profile();
    state[0] = Some(scalar(SolveValue::boolean(inputs.flag)));
    state[1] = Some(scalar(SolveValue::real(profile, f64::from(inputs.x))));
    state[2] = Some(scalar(
        SolveValue::integer(profile, inputs.k.into()).unwrap(),
    ));
    state[3] = Some(scalar(
        SolveValue::integer(profile, inputs.j.into()).unwrap(),
    ));
    state[5] = Some(scalar(SolveValue::real(profile, 0.01)));
    let mut trace = String::new();
    for (method, at) in [
        (SolveBlockMethodKind::Startup, "startup"),
        (SolveBlockMethodKind::DoStep, "dostep"),
    ] {
        let failed = eval_algorithm_method(block, method, &mut state).is_err();
        trace.push_str(&format!("{at} {}\n", snapshot(block, &state, failed)));
        if failed && method == SolveBlockMethodKind::Startup {
            break;
        }
    }
    trace
}

/// Every declaration's bits, then the status word (signal bits, plus bit 31
/// when the method failed). A never-written slot prints the zero the C
/// driver started from (and a failed Startup restores).
fn snapshot(block: &SolveAlgorithmBlock, state: &[Option<TypedValue>], failed: bool) -> String {
    let status = block
        .slots()
        .iter()
        .zip(state)
        .filter(|(slot, _)| slot.storage() == SolveStorageClass::SignalStatus)
        .enumerate()
        .filter(|(_, (_, value))| value.as_ref().map(element).as_deref() == Some("1"))
        .fold(
            if failed { 0x8000_0000u32 } else { 0 },
            |status, (bit, _)| status | 1 << bit,
        );
    let fields = block
        .slots()
        .iter()
        .zip(state)
        .filter(|(slot, _)| slot.storage() != SolveStorageClass::SignalStatus)
        .map(|(slot, value)| {
            value.as_ref().map(element).unwrap_or_else(|| {
                let boolean =
                    slot.value_type().element_type() == rumoca_ir_solve::SolveScalarType::Boolean;
                (if boolean { "0" } else { "00000000" }).to_owned()
            })
        })
        .collect::<Vec<_>>()
        .join(" ");
    format!("{fields} {status:08x}")
}

const DRIVER: &str = r#"#include <stdio.h>
#include <string.h>
#include "Faults.h"

static void bits(const void *value, size_t size) {
    unsigned int word = 0;
    memcpy(&word, value, size);
    printf(" %0*x", size == 1 ? 1 : 8, word);
}

static void dump(const FaultsState *s, const char *at) {
    printf("%s", at);
    bits(&s->flag, sizeof s->flag);
    bits(&s->x, sizeof s->x);
    bits(&s->k, sizeof s->k);
    bits(&s->j, sizeof s->j);
    bits(&s->y, sizeof s->y);
    bits(&s->period, sizeof s->period);
    bits(&s->count, sizeof s->count);
    printf(" %08x\n", (unsigned int)s->rumoca_galec_error_signal_status);
}

int main(void) {
    FaultsState s;
    memset(&s, 0, sizeof s);
    s.flag = FLAG; s.x = X; s.k = K; s.j = J; s.period = 0.01f;
    Faults_startup(&s);
    dump(&s, "startup");
    if (s.rumoca_galec_error_signal_status & RUMOCA_GALEC_FAULT) { return 0; }
    Faults_dostep(&s);
    dump(&s, "dostep");
    return 0;
}
"#;

fn c_trace(
    case: &Case,
    block: &SolveAlgorithmBlock,
    package: &AlgorithmCodePackage,
    dir: &Path,
) -> String {
    let sources = SourceMap::default();
    let renderer = SolveBlockTemplateRenderer::new(package, block, &sources).expect("renderer");
    for (template, file) in [("model.h.jinja", "Faults.h"), ("model.c.jinja", "Faults.c")] {
        let source =
            rumoca_phase_codegen::templates::builtin_template_source("embedded-c-galec", template)
                .expect("bundled");
        let rendered = renderer.render_with_name(source, "Faults").expect("render");
        fs::write(dir.join(file), rendered).expect("write");
    }
    let inputs = case.inputs;
    let x = if inputs.x.is_nan() {
        "NAN".to_owned()
    } else {
        format!("{:?}f", inputs.x)
    };
    let driver = DRIVER
        .replace("FLAG", if inputs.flag { "true" } else { "false" })
        .replace("= X;", &format!("= {x};"))
        .replace("= K;", &format!("= (int32_t){};", inputs.k))
        .replace("= J;", &format!("= (int32_t){};", inputs.j));
    let driver = driver.replace(
        "#include <string.h>",
        "#include <math.h>\n#include <string.h>",
    );
    fs::write(dir.join("driver.c"), driver).expect("write driver");
    let exe = dir.join("run");
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
        .arg(dir.join("Faults.c"))
        .arg(dir.join("driver.c"))
        .arg("-lm")
        .output()
        .expect("cc");
    assert!(
        output.status.success(),
        "{}: {}",
        case.name,
        String::from_utf8_lossy(&output.stderr)
    );
    let run = std::process::Command::new(&exe).output().expect("run");
    String::from_utf8_lossy(&run.stdout).into_owned()
}

#[test]
fn solve_c_fault_paths_match_the_solve_evaluator() {
    let root = tempfile::tempdir().expect("work directory");
    for case in cases() {
        let dir = root.path().join(case.name);
        fs::create_dir_all(&dir).expect("case directory");
        let package = AlgorithmCodePackage::construct(case.block.clone(), vec![None; 7], "period")
            .unwrap_or_else(|error| panic!("{}: {error:?}", case.name));
        let block = lower_algorithm_block(&package, profile())
            .unwrap_or_else(|error| panic!("{}: {error}", case.name));
        let expected = evaluator_trace(&block, case.inputs);
        let actual = c_trace(&case, &block, &package, &dir);
        assert_eq!(actual, expected, "{}", case.name);
        assert!(expected.contains("8000"), "{}: the case faults", case.name);
    }
}
