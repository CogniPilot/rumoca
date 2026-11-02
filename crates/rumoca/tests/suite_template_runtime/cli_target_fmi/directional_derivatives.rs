//! Source-generated FMI 2/3 derivatives through the actual public C APIs.

use super::*;
use quick_xml::{Reader, events::Event};
use std::collections::BTreeMap;

mod profile_controls;

const COMMON: &str = include_str!("../../fixtures/fmi-partial-derivatives/api-common.h");
const DRIVER: &str = include_str!("../../fixtures/fmi-partial-derivatives/driver.c");

#[test]
fn fmi2_fmi3_directional_and_adjoint_requests_execute_the_complete_profile() {
    if !conformance_prerequisites_are_available() {
        return;
    }
    let standards = standard_roots();
    for (model, source, width) in [
        (
            "P",
            include_str!("../../fixtures/fmi-partial-derivatives/P.mo"),
            1,
        ),
        (
            "ArrayP",
            include_str!("../../fixtures/fmi-partial-derivatives/ArrayP.mo"),
            2,
        ),
    ] {
        let compiled = rumoca::Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .expect("compile derivative fixture");
        for (version, headers) in [("fmi2", &standards.0.root), ("fmi3", &standards.1.root)] {
            execute(&compiled, model, version, width, &headers.join("headers"));
        }
    }
}

fn execute(
    compiled: &rumoca::CompilationResult,
    model: &str,
    version: &str,
    width: usize,
    headers: &Path,
) {
    let work = tempdir().expect("owned derivative API directory");
    let files = rumoca::render_target_files(compiled, model, version, None)
        .expect("render the actual admitted FMI component");
    for file in &files {
        let path = work.path().join(&file.path);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, &file.content).unwrap();
    }
    let xml = &files
        .iter()
        .find(|file| file.path == "modelDescription.xml")
        .unwrap()
        .content;
    run_rendered(
        work.path(),
        headers,
        DRIVER,
        &references(xml, version, width),
    );
}

fn run_rendered(work: &Path, headers: &Path, driver: &str, references: &str) {
    fs::write(work.join("sources/references.h"), references).unwrap();
    fs::write(work.join("sources/api-common.h"), COMMON).unwrap();
    fs::write(work.join("sources/driver.c"), driver).unwrap();
    let sources = work.join("sources");
    let mut units: Vec<_> = fs::read_dir(&sources)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| {
            path.extension().is_some_and(|ext| ext == "c")
                && path
                    .file_name()
                    .is_some_and(|name| name != "model.c" && name != "driver.c")
        })
        .collect();
    units.sort();
    let binary = work.join("directional-derivative-api");
    checked_output(
        Command::new("cc")
            .args([
                "-std=c99",
                "-Wall",
                "-Wextra",
                "-Werror",
                "-ffp-contract=off",
            ])
            .arg("-I")
            .arg(headers)
            .arg(sources.join("driver.c"))
            .args(units)
            .args(["-lm", "-o"])
            .arg(&binary),
        "compile actual FMI derivative API harness",
    );
    let output = checked_output(
        &mut Command::new(binary),
        "execute actual FMI derivative API harness",
    );
    print!("{}", String::from_utf8_lossy(&output.stdout));
}

fn metadata(xml: &str) -> BTreeMap<String, String> {
    let mut reader = Reader::from_str(xml);
    let mut result = BTreeMap::new();
    loop {
        let element = match reader.read_event().expect("generated XML parses") {
            Event::Start(element) | Event::Empty(element) => element,
            Event::Eof => break,
            _ => continue,
        };
        let attrs: BTreeMap<_, _> = element
            .attributes()
            .map(|a| {
                let a = a.unwrap();
                (
                    String::from_utf8(a.key.as_ref().to_vec()).unwrap(),
                    a.unescape_value().unwrap().into_owned(),
                )
            })
            .collect();
        if let (Some(name), Some(reference)) = (attrs.get("name"), attrs.get("valueReference")) {
            assert!(result.insert(name.clone(), reference.clone()).is_none());
        }
    }
    result
}

fn references(xml: &str, version: &str, width: usize) -> String {
    let vars = metadata(xml);
    let scalarized = version == "fmi2";
    let token = lifecycle::attribute(
        xml,
        if scalarized {
            "guid"
        } else {
            "instantiationToken"
        },
    );
    let count = if scalarized { width } else { 1 };
    let vr = |name: &str| -> Vec<&str> {
        if scalarized && width > 1 {
            (1..=width)
                .map(|i| vars[&format!("{name}[{i}]")].as_str())
                .collect()
        } else {
            vec![vars[name].as_str()]
        }
    };
    let states = vr("x");
    let inputs = vr("u");
    let outputs = vr("y");
    let derivatives: Vec<_> = if scalarized && width > 1 {
        (1..=width)
            .map(|i| vars[&format!("der(x[{i}])")].as_str())
            .collect()
    } else {
        vec![vars["der(x)"].as_str()]
    };
    let knowns = states
        .into_iter()
        .chain(inputs)
        .collect::<Vec<_>>()
        .join(",");
    let unknowns = derivatives
        .into_iter()
        .chain(outputs)
        .collect::<Vec<_>>()
        .join(",");
    let capability = if scalarized {
        "providesDirectionalDerivative"
    } else {
        "providesDirectionalDerivatives"
    };
    assert_eq!(
        xml.matches(&format!("{capability}=\"true\"")).count(),
        2,
        "both interfaces advertise the exact complete profile"
    );
    if !scalarized {
        assert_eq!(
            xml.matches("providesAdjointDerivatives=\"true\"").count(),
            2
        );
    }
    format!(
        "#define FMI_VERSION {}\n#define WIDTH {width}\n#define REFS {}\n#define INPUT_REFS {count}\n#define TOKEN {token:?}\n#define NONREAL_VR {}\nstatic const Reference knowns[REFS]={{{knowns}}};\nstatic const Reference unknowns[REFS]={{{unknowns}}};\n",
        if scalarized { 2 } else { 3 },
        count * 2,
        vars["tag"]
    )
}
