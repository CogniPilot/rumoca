//! ME-BATCH-001: actual public rendering and independently compiled C requests.

use super::*;
use quick_xml::{Reader, events::Event};

const SOURCE: &str = include_str!("../../fixtures/fmi-batched-getters/BatchGetter.mo");
const DRIVER: &str = include_str!("../../fixtures/fmi-batched-getters/driver.c");

#[test]
fn fmi3_batched_getters_share_only_consecutive_ordinary_observations() {
    execute_control(SOURCE, false);
}

#[test]
fn fmi3_batched_getters_end_ordinary_scope_before_derivative_reads() {
    let source = SOURCE.replace("  output Real a;\n  output Real b;\nequation", "  output Real a;\n  output Real b;\n  Real state(start=0, fixed=true);\nequation\n  der(state) = 1;");
    assert_ne!(source, SOURCE, "state fixture adds a genuine derivative");
    execute_control(&source, true);
}

fn execute_control(source: &str, state: bool) {
    let compiled = rumoca::Compiler::new()
        .model("BatchGetter")
        .compile_str(source, "BatchGetter.mo")
        .expect("compile public batched getter control");
    let files = rumoca::render_target_files(&compiled, "BatchGetter", "fmi3", None)
        .expect("render actual public FMI3 files");
    let work = tempdir().expect("owned getter control directory");
    for file in &files {
        let path = work.path().join(&file.path);
        fs::create_dir_all(path.parent().expect("rendered file parent")).unwrap();
        fs::write(path, &file.content).unwrap();
    }
    let metadata = files
        .iter()
        .find(|file| file.path == "modelDescription.xml")
        .unwrap();
    fs::write(
        work.path().join("sources/references.h"),
        references(&metadata.content, state),
    )
    .unwrap();
    fs::write(work.path().join("sources/driver.c"), DRIVER).unwrap();
    for instrumented in [true, false] {
        build_and_execute(work.path(), state, instrumented);
    }
}

fn references(xml: &str, state: bool) -> String {
    let names = [
        ("x", "X"),
        ("valid", "VALID"),
        ("index", "INDEX"),
        ("a", "A"),
        ("b", "B"),
        ("time", "TIME"),
        ("_event_indicator[1]", "INDICATOR"),
        ("der(state)", "DERIVATIVE"),
    ];
    let mut reader = Reader::from_str(xml);
    let mut header = String::new();
    let mut found = std::collections::BTreeSet::new();
    loop {
        let element = match reader.read_event().expect("issued XML parses") {
            Event::Start(element) | Event::Empty(element) => element,
            Event::Eof => break,
            _ => continue,
        };
        let attributes: std::collections::BTreeMap<_, _> = element
            .attributes()
            .map(|attribute| {
                let attribute = attribute.expect("valid XML attribute");
                (
                    attribute.key.as_ref().to_vec(),
                    attribute.unescape_value().unwrap().into_owned(),
                )
            })
            .collect();
        if let Some(token) = attributes.get(b"instantiationToken".as_slice()) {
            header.push_str(&format!("#define TOKEN {token:?}\n"));
        }
        let Some(name) = attributes.get(b"name".as_slice()) else {
            continue;
        };
        let Some((_, label)) = names.iter().find(|(expected, _)| *expected == name) else {
            continue;
        };
        let reference = attributes
            .get(b"valueReference".as_slice())
            .expect("public variable has reference");
        header.push_str(&format!("#define VR_{label} {reference}\n"));
        found.insert(*label);
    }
    assert!(header.contains("#define TOKEN "));
    for (_, label) in names.into_iter().take(if state { 8 } else { 7 }) {
        assert!(found.contains(label), "canonical metadata declares {label}");
    }
    header
}

fn build_and_execute(work: &Path, state: bool, instrumented: bool) {
    let sources = work.join("sources");
    let executable = work.join(if instrumented {
        "instrumented"
    } else {
        "optimized"
    });
    let mut command = Command::new("cc");
    command.args([
        "-std=c11",
        "-Wall",
        "-Wextra",
        "-Werror",
        "-ffp-contract=off",
    ]);
    command.arg(if instrumented { "-O0" } else { "-O3" });
    if instrumented {
        // Fortify requires optimization; this counter build deliberately has none.
        // The independent optimized control retains the toolchain hardening.
        if let Ok(flags) = std::env::var("NIX_HARDENING_ENABLE") {
            let flags = flags
                .split_whitespace()
                .filter(|flag| !matches!(*flag, "fortify" | "fortify3"))
                .collect::<Vec<_>>()
                .join(" ");
            command.env("NIX_HARDENING_ENABLE", flags);
        }
        command.args([
            "-U_FORTIFY_SOURCE",
            "-finstrument-functions",
            "-fno-inline",
            "-DINSTRUMENTED",
        ]);
    }
    if state {
        command.arg("-DHAS_STATE");
    }
    command.arg("-I").arg(
        workspace_root().join("crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm/fmi3-headers"),
    );
    command.arg(sources.join("driver.c"));
    let mut units: Vec<_> = fs::read_dir(&sources)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| {
            path.extension().is_some_and(|extension| extension == "c")
                && path
                    .file_name()
                    .is_some_and(|name| name != "model.c" && name != "driver.c")
        })
        .collect();
    units.sort();
    command.args(units).arg("-lm").arg("-o").arg(&executable);
    checked_output(
        &mut command,
        "strict native generated-C getter control compilation",
    );
    let output = checked_output(
        &mut Command::new(executable),
        "generated-C getter request controls",
    );
    print!("{}", String::from_utf8(output.stdout).unwrap());
}
