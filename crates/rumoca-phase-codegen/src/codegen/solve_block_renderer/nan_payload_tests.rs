//! Exercise a typed Solve NaN constant at the C boundary. GALEC source rejects
//! non-finite literals, so this is a direct backend fixture, not a claim about
//! GALEC-to-Solve refinement. The checked GALEC view supplies only the header ABI.

use std::fs;
use std::process::Command;

use minijinja::Value;
use rumoca_core::{SourceId, SourceMap, Span};
use rumoca_ir_galec::ast as galec;
use rumoca_ir_galec::package::CheckedAlgorithmBlock;
use rumoca_ir_solve::{
    SolveAlgorithmBlock, SolveAlgorithmCodeEntityKind, SolveAlgorithmCodeRef,
    SolveArithmeticProfile, SolveBlockSlotDeclaration, SolveIntegerDomain, SolveRealFormat,
    SolveScalarType, SolveSlotAccess, SolveStorageClass, SolveValue, SolveValueType,
};

use super::{create_environment, drop_needless_void_uses, solve_block_value};

const MODEL: &str = "NanPayloadProbe";
const PAYLOAD: u32 = 0x7fc0_1234;

fn nan_block() -> SolveAlgorithmBlock {
    let profile = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary32,
        SolveIntegerDomain::construct(i32::MIN.into(), i32::MAX.into()).expect("signed32"),
    );
    let span = Span::from_offsets(SourceId::from_source_name("nan_payload.solve"), 1, 2);
    let slot = SolveBlockSlotDeclaration::construct(
        SolveValueType::scalar(SolveScalarType::real(profile)),
        SolveStorageClass::Output,
        SolveSlotAccess::ReadWrite,
        SolveAlgorithmCodeRef::construct(SolveAlgorithmCodeEntityKind::Interface, "y", span),
    );
    // Import through SolveValue's checked deserializer, avoiding a floating-point
    // conversion that could canonicalize the payload before the test begins.
    let mut wire = serde_json::to_value(SolveValue::real(profile, 0.0)).expect("typed scalar");
    wire["kind"]["bits"] = serde_json::json!(PAYLOAD);
    let nan: SolveValue = serde_json::from_value(wire).expect("typed exact-bits NaN");
    SolveAlgorithmBlock::construct(
        profile,
        vec![slot],
        |builder, slots| {
            let zero = builder.constant(SolveValue::real(profile, 0.0), span)?;
            builder.store(slots[0], zero, span)?;
            Ok(Vec::new())
        },
        |_, _| Ok(Vec::new()),
        |builder, slots| {
            let value = builder.constant(nan, span)?;
            builder.store(slots[0], value, span)?;
            Ok(Vec::new())
        },
    )
    .expect("checked Solve block")
}

fn context(block: &SolveAlgorithmBlock) -> Value {
    let mut abi = galec::Block::new(galec::Name::ident(MODEL));
    abi.interface.push(galec::InterfaceVariable {
        kind: galec::InterfaceKind::Output,
        decl: galec::VariableDeclaration::scalar(galec::ScalarType::Real, galec::Name::ident("y")),
        start: Some(galec::Expression::Real(0.0)),
    });
    let checked = CheckedAlgorithmBlock::construct(abi).expect("finite header fixture");
    let package = crate::codegen::solve_render_fixture_tests::package(&checked)
        .expect("checked header package");
    let sources = SourceMap::new();
    let view = crate::views::algorithm_code::AlgorithmCodeView::new(&package, &sources)
        .expect("header ABI view");
    minijinja::context! {
        algorithm_code => Value::from_serialize(view),
        solve_block => solve_block_value(block, &sources).expect("typed Solve view"),
        model_name => MODEL,
    }
}

fn render(context: &Value, name: &str) -> String {
    let template = crate::templates::builtin_template_source("embedded-c-galec", name)
        .expect("built-in template");
    let env = create_environment();
    let text = env
        .template_from_str(template)
        .expect("valid template")
        .render(context.clone())
        .expect("render direct Solve fixture");
    drop_needless_void_uses(&text)
}

#[test]
fn called_real32_helper_preserves_a_nondefault_nan_payload() {
    // This target admits only binary32 blocks, and the typed program builder
    // refuses real64 constants under that profile before rendering.
    let context = context(&nan_block());
    let source = render(&context, "model.c.jinja");
    assert_eq!(
        source.matches("rumoca_galec_real32(").count(),
        2,
        "the bit-pattern helper must be defined and called:\n{source}"
    );
    assert!(!source.contains("rumoca_galec_real64("), "{source}");
    let directory = tempfile::tempdir().expect("C fixture directory");
    fs::write(directory.path().join(format!("{MODEL}.c")), &source).expect("C source");
    fs::write(
        directory.path().join(format!("{MODEL}.h")),
        render(&context, "model.h.jinja"),
    )
    .expect("C header");
    fs::write(directory.path().join("main.c"), DRIVER).expect("exact-bits driver");
    let executable = directory.path().join("nan-payload");
    let compile = Command::new("cc")
        .current_dir(directory.path())
        .args([
            "-O2",
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
        .arg(format!("{MODEL}.c"))
        .arg("main.c")
        .arg("-o")
        .arg(&executable)
        .output()
        .expect("strict C99 compiler is required");
    assert!(
        compile.status.success(),
        "{}\n{source}",
        String::from_utf8_lossy(&compile.stderr)
    );
    let output = Command::new(&executable)
        .output()
        .expect("execute C driver");
    assert!(output.status.success(), "{output:?}");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).trim(),
        format!("{PAYLOAD:08x}")
    );
}

const DRIVER: &str = r#"
#include "NanPayloadProbe.h"
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

int main(void) {
    NanPayloadProbeState state = {0};
    uint32_t bits = 0;
    if (sizeof state.y != sizeof bits) {
        return 2;
    }
    NanPayloadProbe_startup(&state);
    NanPayloadProbe_dostep(&state);
    memcpy(&bits, &state.y, sizeof bits);
    printf("%08" PRIx32 "\n", bits);
    return 0;
}
"#;
