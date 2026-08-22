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
const INTEGER_CONVERSION_DRIVER: &str = "\
#include <math.h>
#include \"ArrayProjection.h\"

int main(void) {
    ArrayProjectionState state = {0};
    state.source = 3.75f;
    ArrayProjection_dostep(&state);
    if (state.converted != INT32_C(3)) return 1;
    if (state.rumoca_galec_error_signal_status != UINT32_C(0)) return 2;

    state.source = -3.75f;
    ArrayProjection_dostep(&state);
    if (state.converted != INT32_C(-3)) return 3;

    state.source = NAN;
    ArrayProjection_dostep(&state);
    if (state.converted != INT32_C(0)) return 4;
    if (state.rumoca_galec_error_signal_status != UINT32_C(4)) return 5;

    state.source = INFINITY;
    ArrayProjection_dostep(&state);
    if (state.converted != INT32_C(0)) return 6;
    if (state.rumoca_galec_error_signal_status != UINT32_C(2)) return 7;

    state.source = 0x1p31f;
    ArrayProjection_dostep(&state);
    if (state.converted != INT32_C(0)) return 8;
    if (state.rumoca_galec_error_signal_status != UINT32_C(2)) return 9;

    state.source = -0x1p31f;
    ArrayProjection_dostep(&state);
    if (state.converted != INT32_MIN) return 10;
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

fn assert_recursive_array_source(header: &str, source: &str) {
    // `scratch` is an array local, so it lives in the method's working-memory
    // region rather than in the frame: no automatic declaration, no
    // `(void)&…` use marker, and every reference goes through the alias. The
    // region is in the caller's instance-owned overlay declared by the header.
    assert!(
        source.contains(
            "ArrayProjectionScratch_dostep *const ctx = \
             &self->rumoca_galec_scratch.rumoca_galec_g0.dostep;"
        ),
        "{source}"
    );
    assert!(!source.contains("(void)&scratch;"), "{source}");
    assert!(source.contains("ctx->scratch["), "{source}");
    assert!(
        header.contains("float scratch[2];") && header.contains("ArrayProjectionScratch_dostep;"),
        "{header}"
    );

    // Four of the five array relationships compute something per element, so
    // each stays one bounded runtime loop. The fifth (`negated := scratch`) is
    // a pure copy and is the one relationship that goes through the shared
    // kernel instead — see `array_copies_go_through_one_shared_kernel`.
    assert_eq!(
        source.matches("for (int32_t rumoca_galec_copy_0").count(),
        4,
        "each COMPUTED array relationship must remain one bounded runtime loop"
    );
    for expected in [
        "ctx->scratch[rumoca_galec_copy_0] = (-self->a[rumoca_galec_copy_0]);",
        "self->difference[rumoca_galec_copy_0] = (self->a[rumoca_galec_copy_0] - self->gain);",
        "self->selected[rumoca_galec_copy_0] = (self->choose_a ? self->a[rumoca_galec_copy_0] : self->b[rumoca_galec_copy_0]);",
        "self->lifted[rumoca_galec_copy_0] = sinf(self->a[rumoca_galec_copy_0]);",
    ] {
        assert!(
            source.contains(expected),
            "missing recursive scalar projection `{expected}`:\n{source}"
        );
    }
    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(2), self->negated, ctx->scratch);"),
        "the one pure copy must go through the kernel:\n{source}"
    );
    assert_model_unit_defines_no_kernels(source);
}

fn render(block: &CheckedAlgorithmBlock, path: &str) -> Result<String, String> {
    render_target(block, "embedded-c-galec", path)
}

/// Write the shared array-kernel library beside a generated model and return
/// its translation unit, which every compile harness below must also build.
///
/// It takes no block on purpose: the library carries no model view at all, so
/// there is no fixture whose library would differ. The property that this is
/// true is pinned separately by
/// `the_kernel_library_is_identical_for_every_model`; here it is simply used.
fn write_kernel_library(directory: &std::path::Path) -> std::path::PathBuf {
    let fixture = copy_fixture(galec::ScalarType::Real);
    fs::write(
        directory.join("rumoca_galec_kernels.h"),
        render(&fixture, "kernels.h.jinja").expect("kernel header must render"),
    )
    .expect("write kernel header");
    let path = directory.join("rumoca_galec_kernels.c");
    fs::write(
        &path,
        render(&fixture, "kernels.c.jinja").expect("kernel library must render"),
    )
    .expect("write kernel library");
    path
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

/// The Modelica text the traceability fixtures anchor into. The traced
/// statement sits on line 4 at column 3, so a correct byte→line/column
/// conversion must report exactly that.
const TRACE_SOURCE_NAME: &str = "trace/Controller.mo";
const TRACE_SOURCE_TEXT: &str = "\
model Controller
  Real selected;
algorithm
  selected := 1.0;
end Controller;
";
const TRACED_STATEMENT: &str = "selected := 1.0;";

fn traced_span(source_name: &str) -> rumoca_core::Span {
    let start = TRACE_SOURCE_TEXT
        .find(TRACED_STATEMENT)
        .expect("fixture must contain the traced statement");
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(source_name),
        start,
        start + TRACED_STATEMENT.len(),
    )
}

fn trace_fixture() -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "selected",
        false,
    )];
    block.do_step.statements = vec![galec::Spanned::new(
        galec::Statement::Assignment {
            target: state("selected"),
            value: galec::Expression::Real(1.0),
        },
        traced_span(TRACE_SOURCE_NAME),
    )];
    CheckedAlgorithmBlock::construct(block).expect("trace fixture must be valid")
}

/// With the session source map, the anchor a reviewer can act on leads: the
/// source path and a 1-based line:column that resolves to the traced statement
/// (SPEC_0034 GAL-032).
#[test]
fn generated_statements_trace_to_a_modelica_path_and_line() {
    let mut sources = rumoca_core::SourceMap::new();
    sources.add(TRACE_SOURCE_NAME, TRACE_SOURCE_TEXT);
    let span = traced_span(TRACE_SOURCE_NAME);
    let source = rumoca_phase_codegen::render_checked_algorithm_block_template_with_sources(
        &trace_fixture(),
        &sources,
        &json!({}),
        templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("built-in template"),
        MODEL,
    )
    .expect("trace fixture must render");

    // A single traced file makes its own directory the common root, so the
    // emitted anchor is the basename — never the build-machine path.
    assert!(
        source.contains("/* Modelica trace: Controller.mo:4:3-4:18. */"),
        "{source}"
    );
    // The path and line ARE the anchor. A resolved trace must not also carry
    // the byte range: it is a second encoding of a location the reviewer
    // already has, and it appeared on every traced statement in the file.
    assert!(
        !source.contains(&format!("bytes {}..{}", span.start.0, span.end.0)),
        "a resolved trace must not repeat the location as byte offsets: {source}"
    );
    assert!(
        source.contains(" *   Controller.mo"),
        "the traced-file legend must list the file: {source}"
    );
    assert!(
        !source.contains("source-id"),
        "a resolved trace must not fall back to the hash: {source}"
    );
}

/// Compiling the same block from two different checkout locations must produce
/// byte-identical C. A build-machine path in a trace comment would break that,
/// and with it the SHA-1 the eFMU manifest records for the code file.
#[test]
fn generated_c_is_byte_identical_from_two_checkout_locations() {
    let render_from = |prefix: &str| {
        let name = format!("{prefix}/{TRACE_SOURCE_NAME}");
        let mut sources = rumoca_core::SourceMap::new();
        sources.add(&name, TRACE_SOURCE_TEXT);
        let mut block = galec::Block::new(galec::Name::ident(MODEL));
        block.interface = vec![interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "selected",
            false,
        )];
        block.do_step.statements = vec![galec::Spanned::new(
            galec::Statement::Assignment {
                target: state("selected"),
                value: galec::Expression::Real(1.0),
            },
            traced_span(&name),
        )];
        let checked = CheckedAlgorithmBlock::construct(block).expect("fixture must be valid");
        rumoca_phase_codegen::render_checked_algorithm_block_template_with_sources(
            &checked,
            &sources,
            &json!({}),
            templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
                .expect("built-in template"),
            MODEL,
        )
        .expect("fixture must render")
    };
    let alice = render_from("/home/alice/git/models");
    let ci = render_from("/builds/ci/9f2a/checkout");
    assert_eq!(
        alice, ci,
        "generated C must not depend on where the source tree lives"
    );
    assert!(
        !alice.contains("/home/alice") && !alice.contains("/builds/ci"),
        "no build-machine path may reach the artifact:\n{alice}"
    );
}

/// Without a source map nothing may be invented: the trace degrades to the
/// hash-and-byte-range form and says so.
#[test]
fn generated_statements_retain_modelica_source_anchors_without_a_source_map() {
    let source_id = rumoca_core::SourceId::from_source_name(TRACE_SOURCE_NAME);
    let span = traced_span(TRACE_SOURCE_NAME);
    let source = render(&trace_fixture(), "model.c.jinja").expect("trace fixture must render");

    assert!(
        source.contains(&format!(
            "/* Modelica trace: source-id {}, bytes {}..{} (unresolved source). */",
            source_id.0, span.start.0, span.end.0
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
        .arg(write_kernel_library(directory.path()))
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
    assert_recursive_array_source(&header, &source);

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
        .arg(write_kernel_library(directory.path()))
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
    let source = render(&checked, "model.c.jinja")
        .expect("checked source")
        .replace("\r\n", "\n");
    // The call passes the block context and the operands only. Both outputs
    // come back out of the callee's own context region, in signature order,
    // and each target is written after the call returns — the same point in
    // the sequence the out pointers used to be written at.
    assert!(
        source.contains("make_pair(\n        self,\n        self->input_value);"),
        "{source}"
    );
    assert!(!source.contains("rumoca_galec_out_"), "{source}");
    assert!(source.contains("(void)unused_input;"), "{source}");
    // A read-back of an array output is a copy by construction, so it is one
    // kernel call rather than an open-coded loop over the run.
    assert!(
        !source.contains("for (int32_t rumoca_galec_read_back_0 = INT32_C(0);"),
        "{source}"
    );
    assert!(
        source.contains(
            "rumoca_galec_copy_real(INT32_C(2), self->values, \
             self->rumoca_galec_scratch.rumoca_galec_g0.make_pair.pair);"
        ),
        "{source}"
    );
    assert_model_unit_defines_no_kernels(&source);
    assert!(
        source
            .contains("self->accepted = self->rumoca_galec_scratch.rumoca_galec_g0.make_pair.ok;"),
        "{source}"
    );
    // The output buffers are declared once, in the instance-owned working
    // memory type, as the callee's region — not as an automatic in every
    // activation.
    assert!(header.contains("float pair[2];"), "{header}");
    assert!(
        header.contains("ArrayProjectionScratch_make_pair make_pair;"),
        "{header}"
    );

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
        .arg(write_kernel_library(directory.path()))
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
        .arg(write_kernel_library(directory.path()))
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
fn generated_c_integer_conversion_matches_beta_1_signals_and_range() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "source",
            false,
        ),
        interface(
            galec::InterfaceKind::Output,
            galec::ScalarType::Integer,
            "converted",
            false,
        ),
    ];
    block.do_step.signals = vec![
        galec::PredefinedSignal::Nan,
        galec::PredefinedSignal::Overflow,
    ];
    block.do_step.statements = vec![state_assignment(
        "converted",
        galec::Expression::Call(galec::FunctionCall {
            function: galec::Name::ident("integer"),
            arguments: vec![expression("source")],
        }),
    )];
    let checked = CheckedAlgorithmBlock::construct(block).expect("valid integer GALEC block");
    let header = render(&checked, "model.h.jinja").expect("checked header");
    let source = render(&checked, "model.c.jinja").expect("checked source");
    assert!(source.contains("rumoca_galec_integer("), "{source}");

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("integer-conversion");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, &source).expect("write generated source");
    fs::write(&driver_path, INTEGER_CONVERSION_DRIVER).expect("write generated-C driver");

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
        .arg(write_kernel_library(directory.path()))
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
        .expect("execute generated-C integer harness");
    assert!(
        run.status.success(),
        "integer-conversion harness returned {:?}",
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
        .arg(write_kernel_library(directory.path()))
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

// ---------------------------------------------------------------------------
// Whole-array assignment kernels
//
// The property under test is a review-surface property, not a behavioural one:
// a whole-array copy must be ONE call to a shared, once-verified kernel rather
// than an open-coded loop repeated at every copy site. The behavioural property
// — that routing a copy through the kernel changes no value — is pinned by the
// compile-and-run harness below and by the ones the loop form already passed.
// ---------------------------------------------------------------------------

const COPY_KERNELS: [&str; 3] = [
    "rumoca_galec_copy_real",
    "rumoca_galec_copy_integer",
    "rumoca_galec_copy_boolean",
];

/// No generated model source may define a kernel.
///
/// The kernels live in one library translation unit with external linkage, so
/// a definition appearing in a model unit would be a second definition of the
/// same name — a link error at best, and at worst a per-model copy that drifts
/// from the shared one. The property is stated here rather than left to the
/// linker because the failure mode it guards is a *silent* one: a `static`
/// redefinition would link fine and simply stop being the reviewed object.
fn assert_model_unit_defines_no_kernels(source: &str) {
    for name in COPY_KERNELS {
        assert!(
            !source.contains(&format!("void {name}(int32_t")),
            "`{name}` must be defined only in the shared kernel library, not in a \
             generated model unit:\n{source}"
        );
    }
    assert!(
        source.contains("#include \"rumoca_galec_kernels.h\""),
        "a model unit that uses the kernels must include their header:\n{source}"
    );
}

fn copy_fixture(scalar: galec::ScalarType) -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: array_declaration(scalar, "source", 3),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: array_declaration(scalar, "target", 3),
            start: None,
        },
    ];
    block.do_step.statements = vec![state_assignment("target", expression("source"))];
    CheckedAlgorithmBlock::construct(block).expect("whole-array copy fixture must be valid GALEC")
}

/// A whole-array copy is one kernel call, with no loop left at the call site.
#[test]
fn array_copies_go_through_one_shared_kernel() {
    let source = render(&copy_fixture(galec::ScalarType::Real), "model.c.jinja")
        .expect("copy fixture must render");

    assert!(
        source.contains("rumoca_galec_copy_real(INT32_C(3), self->target, self->source);"),
        "a whole-array copy must be one kernel call:\n{source}"
    );
    assert!(
        !source.contains("for (int32_t rumoca_galec_copy_"),
        "no copy loop may remain at the call site:\n{source}"
    );
    assert!(!source.contains("memcpy"), "{source}");
    assert!(!source.contains("void *"), "{source}");
    assert_model_unit_defines_no_kernels(&source);

    // Typed, not `void *` and not `memcpy`: the element type is the thing under
    // review, and the source operand is const-qualified. That definition lives
    // in the library unit now, and there is exactly one of it.
    let library = render(&copy_fixture(galec::ScalarType::Real), "kernels.c.jinja")
        .expect("kernel library must render");
    assert_eq!(
        library
            .matches(
                "void rumoca_galec_copy_real(int32_t count, float target[], \
                 const float source[]) {"
            )
            .count(),
        1,
        "{library}"
    );
}

/// A computed source is not a copy and keeps its loop. Forcing every array
/// relationship through a kernel would need one kernel per expression, which is
/// not a shared verification object at all.
#[test]
fn computed_array_sources_keep_their_loop() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: array_declaration(galec::ScalarType::Real, "source", 3),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: array_declaration(galec::ScalarType::Real, "target", 3),
            start: None,
        },
    ];
    block.do_step.statements = vec![state_assignment(
        "target",
        galec::Expression::Neg(state("source")),
    )];
    let checked = CheckedAlgorithmBlock::construct(block).expect("negation fixture must be valid");
    let source = render(&checked, "model.c.jinja").expect("negation fixture must render");

    assert!(
        source.contains("for (int32_t rumoca_galec_copy_0 = INT32_C(0);"),
        "a computed array source must keep its loop:\n{source}"
    );
    assert!(
        source
            .contains("self->target[rumoca_galec_copy_0] = (-self->source[rumoca_galec_copy_0]);"),
        "{source}"
    );
    // Nothing was copied, so no kernel call appears — the library beside it is
    // unchanged either way, and the linker drops what nothing calls.
    assert_model_unit_defines_no_kernels(&source);
    assert!(!source.contains("rumoca_galec_copy_real("), "{source}");
}

/// The emitter also requires the source's residual rank to equal the loop nest
/// before it will call the kernel. That guard is a fail-safe rather than a
/// reachable path: checked GALEC rejects a scalar assigned to an array target
/// outright, so there is no broadcast fixture to write. This records that.
#[test]
fn a_scalar_source_never_reaches_the_array_assignment_emitter() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        interface(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "gain",
            false,
        ),
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: array_declaration(galec::ScalarType::Real, "target", 3),
            start: None,
        },
    ];
    block.do_step.statements = vec![state_assignment("target", expression("gain"))];

    let error = CheckedAlgorithmBlock::construct(block)
        .expect_err("a scalar source for an array target must be unconstructable");
    assert!(error.to_string().contains("EG017"), "{error}");
}

/// The kernel library is MODEL-INDEPENDENT: complete, and byte-identical
/// whatever block it is emitted beside.
///
/// This is the property the whole library architecture rests on, and it is the
/// exact opposite of the earlier per-model subsetting. An integrator building
/// one application out of several generated blocks wants ONE copy of these
/// files; a subset emitted per model would be several files with several
/// checksums, which cannot be deduplicated by content at all. Carrying a kernel
/// the model never calls costs nothing in return: it has external linkage, so —
/// unlike the `static inline` form this replaced — the linker discards it, and
/// a reviewer reads a fixed, once-qualified library rather than a per-model
/// variant.
#[test]
fn the_kernel_library_is_identical_for_every_model() {
    let mut rendered: Option<(String, String)> = None;
    for scalar in [
        galec::ScalarType::Real,
        galec::ScalarType::Integer,
        galec::ScalarType::Boolean,
    ] {
        let fixture = copy_fixture(scalar);
        let header = render(&fixture, "kernels.h.jinja").expect("kernel header must render");
        let library = render(&fixture, "kernels.c.jinja").expect("kernel library must render");

        // Complete, whatever the model's element types are.
        for name in COPY_KERNELS {
            assert!(
                header.contains(&format!("void {name}(int32_t count,")),
                "the library header must declare `{name}`:\n{header}"
            );
            assert!(
                library.contains(&format!("void {name}(int32_t count,")),
                "the library must define `{name}`:\n{library}"
            );
        }
        // External linkage, not `static inline` in every unit that uses it.
        assert!(!library.contains("static"), "{library}");
        // Prototypes before use, and an include guard.
        assert!(
            library.contains("#include \"rumoca_galec_kernels.h\""),
            "{library}"
        );
        assert!(
            header.contains("#ifndef RUMOCA_GALEC_KERNELS_H"),
            "{header}"
        );

        match &rendered {
            None => rendered = Some((header, library)),
            Some((first_header, first_library)) => {
                assert_eq!(
                    (first_header.as_str(), first_library.as_str()),
                    (header.as_str(), library.as_str()),
                    "the kernel library must be byte-identical for every model"
                );
            }
        }
    }
}

/// The library's version define is derived from the library's own content, and
/// every generated model source checks it.
///
/// That is what makes mixing artifacts from two rumoca builds a compile error
/// instead of a silent link against kernels with different semantics — and,
/// equally, what makes mixing artifacts from the SAME build free, which is the
/// case an integrator deduplicating six eFMUs' libraries depends on.
#[test]
fn the_kernel_library_carries_a_content_derived_version_every_model_checks() {
    let fixture = copy_fixture(galec::ScalarType::Real);
    let header = render(&fixture, "kernels.h.jinja").expect("kernel header must render");
    let source = render(&fixture, "model.c.jinja").expect("copy fixture must render");

    let version = format!(
        "UINT32_C(0x{:08x})",
        rumoca_phase_codegen::templates::galec_kernel_library_version()
    );
    assert!(
        header.contains(&format!("#define RUMOCA_GALEC_KERNELS_VERSION {version}")),
        "{header}"
    );
    assert!(
        source.contains(&format!("#if RUMOCA_GALEC_KERNELS_VERSION != {version}")),
        "{source}"
    );
    assert!(source.contains("#error"), "{source}");
    // Nothing date-, machine- or model-dependent may reach the define: the same
    // build must produce the same identity every time it runs.
    assert_eq!(
        render(&copy_fixture(galec::ScalarType::Boolean), "kernels.h.jinja")
            .expect("kernel header must render"),
        header
    );
}

/// One version constant means one set of bytes ACROSS TARGETS, not just
/// across models: `galec-production` renders the kernel pair through its own
/// template chain, and if that chain could change a byte (a target-specific
/// banner once did), two files with different content would carry the same
/// `RUMOCA_GALEC_KERNELS_VERSION` — the `#error` guard silent, and linking a
/// block from each target into one application a multiple-definition error
/// nobody was warned about. The base templates define no overridable block,
/// so equality here is structural; this test pins it against regression.
#[test]
fn the_kernel_library_is_byte_identical_across_both_c_targets() {
    let fixture = copy_fixture(galec::ScalarType::Real);
    for template in ["kernels.h.jinja", "kernels.c.jinja"] {
        assert_eq!(
            render_target(&fixture, "embedded-c-galec", template)
                .expect("embedded-c kernel template must render"),
            render_target(&fixture, "galec-production", template)
                .expect("production kernel template must render"),
            "`{template}` must render byte-identical from both C targets: one \
             RUMOCA_GALEC_KERNELS_VERSION has to mean one set of bytes"
        );
    }
}

/// Rank > 1: the kernel takes a rank-1 run, so a matrix copy is one loop over
/// the rows and one call per row. Parameterising the extent instead of the row
/// type is what keeps the target at one kernel per element type, and what keeps
/// the source operand const-qualifiable under C99.
#[test]
fn rank_two_copies_call_the_kernel_once_per_row() {
    let matrix = |name: &str| {
        let mut decl =
            galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident(name));
        decl.dimensions = vec![
            galec::Dimension::Expr(galec::Expression::Integer(2)),
            galec::Dimension::Expr(galec::Expression::Integer(3)),
        ];
        decl
    };
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Input,
            decl: matrix("source"),
            start: None,
        },
        galec::InterfaceVariable {
            kind: galec::InterfaceKind::Output,
            decl: matrix("target"),
            start: None,
        },
    ];
    block.do_step.statements = vec![state_assignment("target", expression("source"))];
    let checked = CheckedAlgorithmBlock::construct(block).expect("matrix fixture must be valid");
    let source = render(&checked, "model.c.jinja").expect("matrix fixture must render");

    assert!(
        source.contains("for (int32_t rumoca_galec_copy_0 = INT32_C(0);"),
        "the outer dimension keeps its loop:\n{source}"
    );
    assert!(
        !source.contains("for (int32_t rumoca_galec_copy_1"),
        "the innermost dimension must become a call, not a loop:\n{source}"
    );
    assert!(
        source.contains(
            "rumoca_galec_copy_real(INT32_C(3), self->target[rumoca_galec_copy_0], \
             self->source[rumoca_galec_copy_0]);"
        ),
        "{source}"
    );
    assert_model_unit_defines_no_kernels(&source);
}

fn mixed_copy_block() -> CheckedAlgorithmBlock {
    let pair = |kind, scalar, name| galec::InterfaceVariable {
        kind,
        decl: array_declaration(scalar, name, 3),
        start: None,
    };
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![
        // GALEC requires the interface in order: every input, then every output.
        pair(
            galec::InterfaceKind::Input,
            galec::ScalarType::Real,
            "real_source",
        ),
        pair(
            galec::InterfaceKind::Input,
            galec::ScalarType::Integer,
            "int_source",
        ),
        pair(
            galec::InterfaceKind::Input,
            galec::ScalarType::Boolean,
            "bool_source",
        ),
        pair(
            galec::InterfaceKind::Output,
            galec::ScalarType::Real,
            "real_target",
        ),
        pair(
            galec::InterfaceKind::Output,
            galec::ScalarType::Integer,
            "int_target",
        ),
        pair(
            galec::InterfaceKind::Output,
            galec::ScalarType::Boolean,
            "bool_target",
        ),
    ];
    block.do_step.statements = vec![
        state_assignment("real_target", expression("real_source")),
        state_assignment("int_target", expression("int_source")),
        state_assignment("bool_target", expression("bool_source")),
    ];
    CheckedAlgorithmBlock::construct(block).expect("mixed copy fixture must be valid")
}

/// The kernel must move exactly the values the loop moved, for every element
/// type, under the target's own strict preflight flags.
#[test]
fn copy_kernels_move_every_element_unchanged() {
    let checked = mixed_copy_block();
    let header = render(&checked, "model.h.jinja").expect("mixed copy header");
    let source = render(&checked, "model.c.jinja").expect("mixed copy source");
    for name in COPY_KERNELS {
        assert!(source.contains(&format!("{name}(INT32_C(")), "{source}");
    }
    assert_model_unit_defines_no_kernels(&source);

    let directory = tempdir().expect("temporary generated-C directory");
    let header_path = directory.path().join(format!("{MODEL}.h"));
    let source_path = directory.path().join(format!("{MODEL}.c"));
    let driver_path = directory.path().join("main.c");
    let executable = directory.path().join("copy-kernels");
    fs::write(&header_path, header).expect("write generated header");
    fs::write(&source_path, &source).expect("write generated source");
    fs::write(
        &driver_path,
        "\
#include \"ArrayProjection.h\"

int main(void) {
    ArrayProjectionState state = {0};
    const float reals[3] = {1.5f, -2.25f, 3.0f};
    const int32_t ints[3] = {-7, 0, 11};
    const bool bools[3] = {true, false, true};
    int32_t i;
    for (i = 0; i < 3; ++i) {
        state.real_source[i] = reals[i];
        state.int_source[i] = ints[i];
        state.bool_source[i] = bools[i];
    }
    ArrayProjection_dostep(&state);
    for (i = 0; i < 3; ++i) {
        if (state.real_target[i] != reals[i]) return 1;
        if (state.int_target[i] != ints[i]) return 2;
        if (state.bool_target[i] != bools[i]) return 3;
    }
    return 0;
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
            "-Wshadow",
            "-Wundef",
            "-Wcast-qual",
            "-Wstrict-prototypes",
            "-Wmissing-prototypes",
            "-Werror",
        ])
        .arg(&driver_path)
        .arg(&source_path)
        .arg(write_kernel_library(directory.path()))
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("run C compiler");
    assert!(
        compile.status.success(),
        "strict generated-C compile failed:\n{}\nsource:\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let run = Command::new(&executable)
        .output()
        .expect("execute generated-C copy-kernel harness");
    assert!(
        run.status.success(),
        "copy-kernel harness returned {:?}",
        run.status.code()
    );
}

// ---------------------------------------------------------------------------
// A trace comment opens a run, not a line
//
// One Modelica statement can expand to hundreds of C statements, and an anchor
// repeated on every one of them stops being read. The invariant these pin is
// "every statement's provenance is the nearest trace comment above it", which a
// run-collapsed form still satisfies.
// ---------------------------------------------------------------------------

const RUN_SOURCE_NAME: &str = "trace/Runs.mo";
const RUN_SOURCE_TEXT: &str = "\
model Runs
  Real a;
  Real b;
algorithm
  a := 1.0;
  b := 2.0;
end Runs;
";

fn run_span(needle: &str) -> rumoca_core::Span {
    let start = RUN_SOURCE_TEXT
        .find(needle)
        .unwrap_or_else(|| panic!("fixture source must contain `{needle}`"));
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(RUN_SOURCE_NAME),
        start,
        start + needle.len(),
    )
}

fn render_with_run_sources(block: &CheckedAlgorithmBlock) -> String {
    let mut sources = rumoca_core::SourceMap::new();
    sources.add(RUN_SOURCE_NAME, RUN_SOURCE_TEXT);
    rumoca_phase_codegen::render_checked_algorithm_block_template_with_sources(
        block,
        &sources,
        &json!({}),
        templates::builtin_template_source("embedded-c-galec", "model.c.jinja")
            .expect("built-in template"),
        MODEL,
    )
    .expect("run fixture must render")
}

fn scalar_run_block(spans: &[&str]) -> CheckedAlgorithmBlock {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "selected",
        false,
    )];
    block.do_step.statements = spans
        .iter()
        .enumerate()
        .map(|(index, needle)| {
            galec::Spanned::new(
                galec::Statement::Assignment {
                    target: state("selected"),
                    value: galec::Expression::Real(index as f64),
                },
                run_span(needle),
            )
        })
        .collect();
    CheckedAlgorithmBlock::construct(block).expect("trace-run fixture must be valid GALEC")
}

/// An unbroken run of one anchor is announced once, at its head.
#[test]
fn repeated_statement_traces_collapse_to_one_comment_per_run() {
    let source = render_with_run_sources(&scalar_run_block(&[
        "a := 1.0;",
        "a := 1.0;",
        "a := 1.0;",
        "a := 1.0;",
    ]));

    assert_eq!(
        source.matches("/* Modelica trace:").count(),
        1,
        "an unbroken run of one anchor must be announced once:\n{source}"
    );
    assert!(
        source.contains("/* Modelica trace: Runs.mo:5:3-5:11. */"),
        "{source}"
    );
    // The run is collapsed, not thinned: every statement is still emitted.
    assert_eq!(source.matches("self->selected = ").count(), 4, "{source}");
}

/// A different anchor always re-announces — including when it returns to one
/// already seen, which is what keeps "nearest comment above" correct.
#[test]
fn a_changed_trace_reopens_the_run() {
    let source =
        render_with_run_sources(&scalar_run_block(&["a := 1.0;", "b := 2.0;", "a := 1.0;"]));

    assert_eq!(
        source.matches("/* Modelica trace:").count(),
        3,
        "each change of anchor must re-announce:\n{source}"
    );
    assert_eq!(
        source
            .matches("/* Modelica trace: Runs.mo:5:3-5:11. */")
            .count(),
        2,
        "{source}"
    );
    assert_eq!(
        source
            .matches("/* Modelica trace: Runs.mo:6:3-6:11. */")
            .count(),
        1,
        "{source}"
    );
}

/// Reading upward past a function's opening brace lands in a different
/// function's body, which is not provenance. Every emitted C function therefore
/// starts a fresh run even when its first statement repeats the anchor the
/// previously emitted function ended on.
#[test]
fn each_emitted_function_starts_a_fresh_trace_run() {
    let mut block = galec::Block::new(galec::Name::ident(MODEL));
    block.interface = vec![interface(
        galec::InterfaceKind::Output,
        galec::ScalarType::Real,
        "selected",
        false,
    )];
    block.protected_functions = vec![galec::UserFunction {
        kind: galec::FunctionKind::Stateless,
        name: galec::Name::ident("helper"),
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
                decl: galec::VariableDeclaration::scalar(
                    galec::ScalarType::Real,
                    galec::Name::ident("result"),
                ),
            },
        ],
        locals: Vec::new(),
        statements: vec![galec::Spanned::new(
            galec::Statement::Assignment {
                target: local("result"),
                value: galec::Expression::Real(1.0),
            },
            run_span("a := 1.0;"),
        )],
        span: rumoca_core::Span::DUMMY,
    }];
    block.do_step.statements = vec![galec::Spanned::new(
        galec::Statement::Assignment {
            target: state("selected"),
            value: galec::Expression::Call(galec::FunctionCall {
                function: galec::Name::ident("helper"),
                arguments: vec![galec::Expression::Real(0.0)],
            }),
        },
        run_span("a := 1.0;"),
    )];
    let checked =
        CheckedAlgorithmBlock::construct(block).expect("function-boundary fixture must be valid");
    let source = render_with_run_sources(&checked);

    assert_eq!(
        source
            .matches("/* Modelica trace: Runs.mo:5:3-5:11. */")
            .count(),
        2,
        "the callee and the caller must each announce their own anchor:\n{source}"
    );
}
