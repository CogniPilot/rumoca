use std::fs;
use std::process::Command;

use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_phase_codegen::{render_checked_algorithm_block_template_with_artifact, templates};
use serde_json::json;
use tempfile::tempdir;

const MODEL: &str = "ArrayProjection";
const NAN_COMPARISON_DRIVER: &str = "\
#include <math.h>
#include \"ArrayProjection.h\"

int main(void) {
    ArrayProjectionState state = {0};
    state.lhs = NAN;
    state.rhs = 1.0;
    ArrayProjection_dostep(&state);
    if (state.lt || state.gt || state.le || state.ge || state.eq || state.ne) return 1;
    if (state.rumoca_galec_error_signal_status != UINT32_C(4)) return 2;

    state.lhs = 2.0;
    ArrayProjection_dostep(&state);
    if (state.lt || !state.gt || state.le || !state.ge || state.eq || !state.ne) return 3;
    return state.rumoca_galec_error_signal_status != UINT32_C(0);
}
";

fn array_declaration(
    kind: galec::ScalarType,
    name: &str,
    extent: i64,
) -> galec::VariableDeclaration {
    let mut declaration = galec::VariableDeclaration::scalar(kind, galec::Name::ident(name));
    declaration.dimensions = vec![galec::Dimension::Expr(galec::Expression::Integer(extent))];
    declaration
}

fn interface(
    kind: galec::InterfaceKind,
    scalar: galec::ScalarType,
    name: &str,
    array: bool,
) -> galec::InterfaceVariable {
    galec::InterfaceVariable {
        kind,
        decl: if array {
            array_declaration(scalar, name, 2)
        } else {
            galec::VariableDeclaration::scalar(scalar, galec::Name::ident(name))
        },
        start: None,
    }
}

fn state(name: &str) -> galec::Reference {
    galec::Reference::state(galec::Name::ident(name))
}

fn expression(name: &str) -> galec::Expression {
    galec::Expression::Ref(state(name))
}

fn local(name: &str) -> galec::Reference {
    galec::Reference::local(galec::Name::ident(name))
}

fn state_assignment(target: &str, value: galec::Expression) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::Assignment {
        target: state(target),
        value,
    })
}

fn local_assignment(target: &str, value: galec::Expression) -> galec::Spanned<galec::Statement> {
    galec::Spanned::dummy(galec::Statement::Assignment {
        target: local(target),
        value,
    })
}

fn checked_array_block() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "a",
            true,
        ),
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "b",
            true,
        ),
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Boolean,
            "choose_a",
            false,
        ),
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "gain",
            false,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "negated",
            true,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "difference",
            true,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "selected",
            true,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "lifted",
            true,
        ),
    ];
    block.do_step.locals = vec![array_declaration(galec::ScalarType::Real, "scratch", 2)];
    block.do_step.statements = vec![
        local_assignment("scratch", galec::Expression::Neg(state("a"))),
        state_assignment("negated", galec::Expression::Ref(local("scratch"))),
        state_assignment(
            "difference",
            galec::Expression::binary(galec::BinaryOp::Sub, expression("a"), expression("gain")),
        ),
        state_assignment(
            "selected",
            galec::Expression::If(galec::IfExpression::new(
                vec![(expression("choose_a"), expression("a"))],
                expression("b"),
            )),
        ),
        state_assignment(
            "lifted",
            galec::Expression::Call(galec::FunctionCall {
                function: galec::Name::ident("sin1D"),
                arguments: vec![expression("a")],
            }),
        ),
    ];
    CheckedAlgorithmBlock::construct(block).expect("array projection fixture must be valid GALEC")
}

fn render(block: &CheckedAlgorithmBlock, path: &str) -> Result<String, String> {
    render_target(block, "embedded-c-galec", path)
}

fn render_target(
    block: &CheckedAlgorithmBlock,
    target: &str,
    path: &str,
) -> Result<String, String> {
    let template = templates::builtin_template_source(target, path).expect("built-in template");
    render_checked_algorithm_block_template_with_artifact(block, &json!({}), template, MODEL)
        .map_err(|error| error.to_string())
}

#[test]
fn generated_statements_retain_modelica_source_anchors() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "selected",
        false,
    )];
    let source_id = rumoca_core::SourceId::from_source_name("trace/Controller.mo");
    block.do_step.statements = vec![galec::Spanned::new(
        galec::Statement::Assignment {
            target: state("selected"),
            value: galec::Expression::Real(1.0),
        },
        rumoca_core::Span::from_offsets(source_id, 41, 58),
    )];
    let checked = CheckedAlgorithmBlock::construct(block).expect("trace fixture must be valid");
    let source = render(&checked, "model.c.jinja").expect("trace fixture must render");

    assert!(
        source.contains(&format!(
            "/* Modelica trace: source-id {}, bytes 41..58. */",
            source_id.0
        )),
        "{source}"
    );
}

#[test]
fn large_checked_if_expression_renders_without_template_recursion() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "selected",
        false,
    )];
    let branches = (0..225)
        .map(|value| {
            (
                galec::Expression::Bool(false),
                galec::Expression::Real(f64::from(value)),
            )
        })
        .collect();
    block.do_step.statements = vec![state_assignment(
        "selected",
        galec::Expression::If(galec::IfExpression::new(
            branches,
            galec::Expression::Real(225.0),
        )),
    )];
    let block = CheckedAlgorithmBlock::construct(block)
        .expect("bounded large conditional must be valid GALEC");
    let source =
        render(&block, "model.c.jinja").expect("bounded conditional must render iteratively");

    assert!(source.contains("224.0f : 225.0f"));
}

#[test]
fn bounded_selection_has_equivalent_galec_and_native_c_legalizations() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: array_declaration(galec::ScalarType::Real, "samples", 3),
            start: None,
        },
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Integer,
            "index",
            false,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "selected",
            false,
        ),
    ];
    let dynamic_reference = galec::Reference::State(vec![galec::RefPart {
        name: galec::Name::ident("samples"),
        subscripts: vec![expression("index")],
        span: rumoca_core::Span::DUMMY,
    }]);
    let selection = galec::IfExpression::bounded_selection(dynamic_reference, vec![3])
        .expect("bounded selection fixture");
    block.do_step.statements = vec![state_assignment(
        "selected",
        galec::Expression::If(selection),
    )];
    let checked = CheckedAlgorithmBlock::construct(block).expect("valid bounded selection block");

    let algorithm_code =
        render_target(&checked, "galec", "model.alg.jinja").expect("bounded selection GALEC");
    assert!(
        algorithm_code.contains("self.samples[1]"),
        "{algorithm_code}"
    );
    assert!(
        algorithm_code.contains("self.samples[3]"),
        "{algorithm_code}"
    );
    assert!(!algorithm_code.contains("self.samples[self.index]"));

    let header = render(&checked, "model.h.jinja").expect("bounded selection header");
    let source = render(&checked, "model.c.jinja").expect("bounded selection C");
    assert!(
        source.contains("self->samples[rumoca_galec_bounded_index(self->index, 3)]"),
        "{source}"
    );
    assert!(!source.contains("? self->samples[0]"), "{source}");

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("bounded-selection");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, source).expect("write generated source");
    fs::write(
        &driver_path,
        "#include \"ArrayProjection.h\"\nint main(void) {\n  ArrayProjectionState state = {0};\n  state.samples[0] = 10.0f; state.samples[1] = 20.0f; state.samples[2] = 30.0f;\n  const int32_t indices[5] = {1, 2, 3, 0, 4};\n  const float expected[5] = {10.0f, 20.0f, 30.0f, 30.0f, 30.0f};\n  for (int32_t k = 0; k < 5; ++k) {\n    state.index = indices[k]; ArrayProjection_dostep(&state);\n    if (state.selected != expected[k]) return (int)(k + 1);\n  }\n  return 0;\n}\n",
    )
    .expect("write generated-C driver");
    let compile = Command::new("cc")
        .args([
            "-std=c99",
            "-pedantic",
            "-Wall",
            "-Wextra",
            "-Wconversion",
            "-Wsign-conversion",
            "-Werror",
        ])
        .arg(&driver_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    assert!(
        Command::new(&executable)
            .status()
            .expect("execute bounded-selection harness")
            .success()
    );
}

#[test]
fn recursive_array_expressions_execute_with_checked_values() {
    let block = checked_array_block();
    let header = render(&block, "model.h.jinja").expect("checked header");
    let source = render(&block, "model.c.jinja").expect("checked source");
    assert!(source.contains("(void)&scratch;"));
    assert!(!source.contains("(void)scratch;"));

    for expected in [
        "scratch[0] = (-self->a[0]);",
        "scratch[1] = (-self->a[1]);",
        "self->negated[0] = scratch[0];",
        "self->negated[1] = scratch[1];",
        "self->difference[0] = (self->a[0] - self->gain);",
        "self->difference[1] = (self->a[1] - self->gain);",
        "self->selected[0] = (self->choose_a ? self->a[0] : self->b[0]);",
        "self->selected[1] = (self->choose_a ? self->a[1] : self->b[1]);",
        "self->lifted[0] = sinf(self->a[0]);",
        "self->lifted[1] = sinf(self->a[1]);",
    ] {
        assert!(
            source.contains(expected),
            "missing recursive scalar projection `{expected}`:\n{source}"
        );
    }

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("array-projection");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, &source).expect("write generated source");
    fs::write(
        &driver_path,
        "\
#include <math.h>
#include \"ArrayProjection.h\"

static int close_enough(float lhs, float rhs) {
    return fabsf(lhs - rhs) < 1.0e-6f;
}

int main(void) {
    ArrayProjectionState state = {0};
    state.a[0] = 1.0;
    state.a[1] = -2.0;
    state.b[0] = 3.0;
    state.b[1] = 4.0;
    state.choose_a = false;
    state.gain = 0.5;
    ArrayProjection_dostep(&state);
    return !(close_enough(state.negated[0], -1.0)
        && close_enough(state.negated[1], 2.0)
        && close_enough(state.difference[0], 0.5)
        && close_enough(state.difference[1], -2.5)
        && close_enough(state.selected[0], 3.0)
        && close_enough(state.selected[1], 4.0)
        && close_enough(state.lifted[0], sinf(1.0f))
        && close_enough(state.lifted[1], sinf(-2.0f)));
}
",
    )
    .expect("write generated-C driver");

    let compile = Command::new("cc")
        .args([
            "-std=c99",
            "-pedantic",
            "-Wall",
            "-Wextra",
            "-Wconversion",
            "-Wsign-conversion",
            "-Werror",
        ])
        .arg(&driver_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&executable)
        .arg("-lm")
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}\nsource:\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&executable)
        .output()
        .expect("execute generated-C array harness");
    assert!(
        run.status.success(),
        "generated-C array harness returned {:?}:\n{}",
        run.status.code(),
        String::from_utf8_lossy(&run.stderr)
    );
}

#[test]
fn multi_output_user_calls_compile_and_copy_every_result() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "input_value",
            false,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "values",
            true,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Boolean,
            "accepted",
            false,
        ),
    ];
    block.protected_functions = vec![make_pair_function()];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::MultiAssignment {
        targets: vec![state("values"), state("accepted")],
        call: galec::FunctionCall {
            function: galec::Name::ident("make_pair"),
            arguments: vec![expression("input_value")],
        },
    })];
    let checked = CheckedAlgorithmBlock::construct(block).expect("valid multi-output GALEC block");
    let header = render(&checked, "model.h.jinja").expect("checked header");
    let source = render(&checked, "model.c.jinja").expect("checked source");
    assert!(source.contains(
        "make_pair(\n        self,\n        self->input_value,\n        self->values,\n        &self->accepted);"
    ));
    assert!(source.contains("(void)unused_input;"));
    assert!(source.contains("float pair[2];"));
    assert!(source.contains("rumoca_galec_out_pair[0] = pair[0];"));

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("multi-output");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, source).expect("write generated source");
    fs::write(
        &driver_path,
        "#include \"ArrayProjection.h\"\nint main(void) {\n  ArrayProjectionState state = {0};\n  ArrayProjection_dostep(&state);\n  return !(state.values[0] == 2.0f && state.values[1] == 3.0f && state.accepted);\n}\n",
    )
    .expect("write generated-C driver");
    let compile = Command::new("cc")
        .args(["-std=c99", "-pedantic", "-Wall", "-Wextra", "-Werror"])
        .arg(&driver_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}\nsource:\n{}",
        String::from_utf8_lossy(&compile.stderr),
        fs::read_to_string(&source_path).expect("read generated source")
    );
    assert!(
        Command::new(&executable)
            .status()
            .expect("execute multi-output harness")
            .success()
    );
}

fn make_pair_function() -> galec::UserFunction {
    galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("make_pair"),
        signals: Vec::new(),
        parameters: vec![
            galec::Parameter {
                direction: galec::Direction::Input,
                decl: galec::VariableDeclaration::scalar(
                    galec::ScalarType::Real,
                    galec::Name::ident("unused_input"),
                ),
            },
            galec::Parameter {
                direction: galec::Direction::Output,
                decl: array_declaration(galec::ScalarType::Real, "pair", 2),
            },
            galec::Parameter {
                direction: galec::Direction::Output,
                decl: galec::VariableDeclaration::scalar(
                    galec::ScalarType::Boolean,
                    galec::Name::ident("ok"),
                ),
            },
        ],
        locals: Vec::new(),
        statements: vec![
            local_assignment(
                "pair",
                galec::Expression::Array(vec![
                    galec::Expression::Real(2.0),
                    galec::Expression::Real(3.0),
                ]),
            ),
            local_assignment("ok", galec::Expression::Bool(true)),
        ],
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn unequal_extents_fail_before_c_rendering() {
    let mut block = galec::Block::new(galec::Name::ident("UnequalExtents"));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: array_declaration(galec::ScalarType::Real, "source", 3),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: array_declaration(galec::ScalarType::Real, "target", 2),
            start: None,
        },
    ];
    block.do_step.statements = vec![state_assignment("target", expression("source"))];
    let checked =
        CheckedAlgorithmBlock::construct(block).expect("rank-only checker accepts this fixture");

    let error = render(&checked, "model.c.jinja").expect_err("extent mismatch must fail closed");
    assert!(
        error.contains("checked assignment extent mismatch"),
        "{error}"
    );
}

#[test]
fn unresolved_local_never_reaches_rendering() {
    let mut block = galec::Block::new(galec::Name::ident("MissingLocal"));
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "target",
        false,
    )];
    block.do_step.statements = vec![state_assignment(
        "target",
        galec::Expression::Ref(local("missing")),
    )];

    let error = CheckedAlgorithmBlock::construct(block)
        .expect_err("unresolved local must be unconstructable");
    assert!(error.to_string().contains("unresolved"), "{error}");
}

#[test]
fn multipart_state_shape_is_rejected_when_the_c_state_view_cannot_represent_it() {
    let mut block = galec::Block::new(galec::Name::ident("MultipartState"));
    block.compartments = vec![galec::StateCompartment {
        name: galec::Name::ident("VectorRecord"),
        entities: vec![galec::ProtectedEntity {
            kind: galec::ProtectedKind::State,
            decl: array_declaration(galec::ScalarType::Real, "values", 2),
            start: None,
        }],
        span: rumoca_core::Span::DUMMY,
    }];
    block.protected = vec![galec::ProtectedEntity {
        kind: galec::ProtectedKind::State,
        decl: galec::VariableDeclaration {
            ty: galec::TypeRef::Compartment(galec::Name::ident("VectorRecord")),
            name: galec::Name::ident("record_state"),
            dimensions: Vec::new(),
            range: galec::RangeAttributes::default(),
            span: rumoca_core::Span::DUMMY,
        },
        start: None,
    }];
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "target",
        true,
    )];
    let source = galec::Reference::State(vec![
        galec::RefPart::plain(galec::Name::ident("record_state")),
        galec::RefPart::plain(galec::Name::ident("values")),
    ]);
    block.do_step.statements = vec![state_assignment("target", galec::Expression::Ref(source))];
    let checked = CheckedAlgorithmBlock::construct(block).expect("valid multipart GALEC block");

    let error =
        render(&checked, "model.c.jinja").expect_err("unsupported C state layout must fail closed");
    assert!(
        error.contains("standalone target does not support compartment root"),
        "{error}"
    );
}

#[test]
fn generated_c_real_nan_comparisons_signal_and_return_false() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "lhs",
            false,
        ),
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "rhs",
            false,
        ),
    ];
    let comparisons = [
        ("lt", galec::BinaryOp::Lt),
        ("gt", galec::BinaryOp::Gt),
        ("le", galec::BinaryOp::Le),
        ("ge", galec::BinaryOp::Ge),
        ("eq", galec::BinaryOp::Eq),
        ("ne", galec::BinaryOp::Ne),
    ];
    block.interface.extend(comparisons.iter().map(|(name, _)| {
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Boolean,
            name,
            false,
        )
    }));
    block.do_step.signals = vec![galec::PredefinedSignal::Nan];
    block.do_step.statements = comparisons
        .iter()
        .map(|(name, operator)| {
            state_assignment(
                name,
                galec::Expression::binary(*operator, expression("lhs"), expression("rhs")),
            )
        })
        .collect();
    let checked = CheckedAlgorithmBlock::construct(block).expect("valid comparing GALEC block");
    let header = render(&checked, "model.h.jinja").expect("checked header");
    let source = render(&checked, "model.c.jinja").expect("checked source");

    for operator in ["lt", "gt", "le", "ge", "eq", "ne"] {
        let call = format!("rumoca_galec_compare_{operator}(");
        assert!(source.contains(&call), "missing `{call}`:\n{source}");
    }

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("nan-comparison");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, &source).expect("write generated source");
    fs::write(&driver_path, NAN_COMPARISON_DRIVER).expect("write generated-C driver");

    let compile = Command::new("cc")
        .args([
            "-std=c99",
            "-pedantic",
            "-Wall",
            "-Wextra",
            "-Wconversion",
            "-Wsign-conversion",
            "-Werror",
        ])
        .arg(&driver_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&executable)
        .arg("-lm")
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}\nsource:\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&executable)
        .output()
        .expect("execute generated-C NaN harness");
    assert!(
        run.status.success(),
        "NaN comparison harness returned {:?}",
        run.status.code()
    );
}

#[test]
fn generated_c_exposes_and_resets_the_standard_error_signal_status() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![interface(
        galec::InterfaceKind::Input,
        galec::ScalarType::Boolean,
        "raise_error",
        false,
    )];
    block.do_step.signals = vec![galec::PredefinedSignal::InvalidArgument];
    block.do_step.statements = vec![galec::Spanned::dummy(galec::Statement::If(
        galec::IfStatement {
            branches: vec![galec::IfBranch {
                condition: galec::Condition::Expression(expression("raise_error")),
                body: vec![galec::Spanned::dummy(galec::Statement::Signal(vec![
                    galec::Identifier::new("INVALID_ARGUMENT"),
                ]))],
                span: rumoca_core::Span::DUMMY,
            }],
            else_body: None,
        },
    ))];
    let checked = CheckedAlgorithmBlock::construct(block).expect("valid signaling GALEC block");
    let header = render(&checked, "model.h.jinja").expect("checked header");
    let source = render(&checked, "model.c.jinja").expect("checked source");

    assert!(
        header.contains("uint32_t rumoca_galec_error_signal_status;"),
        "{header}"
    );
    assert!(
        source.contains("rumoca_galec_error_signal_status |= UINT32_C(1);"),
        "{source}"
    );

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("signal-status");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, source).expect("write generated source");
    fs::write(
        &driver_path,
        "\
#include \"ArrayProjection.h\"

int main(void) {
    ArrayProjectionState state = {0};
    ArrayProjection_dostep(&state);
    if (state.rumoca_galec_error_signal_status != UINT32_C(0)) return 1;
    state.raise_error = true;
    ArrayProjection_dostep(&state);
    if (state.rumoca_galec_error_signal_status != UINT32_C(1)) return 2;
    state.raise_error = false;
    ArrayProjection_dostep(&state);
    return state.rumoca_galec_error_signal_status != UINT32_C(0);
}
",
    )
    .expect("write generated-C driver");

    let compile = Command::new("cc")
        .args(["-std=c99", "-pedantic", "-Wall", "-Wextra", "-Werror"])
        .arg(&driver_path)
        .arg(&source_path)
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&executable)
        .output()
        .expect("execute generated-C signal harness");
    assert!(
        run.status.success(),
        "signal harness returned {:?}",
        run.status.code()
    );
}
