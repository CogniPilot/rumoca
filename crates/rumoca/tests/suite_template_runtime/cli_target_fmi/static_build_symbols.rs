//! Every global symbol a component's translation units define carries the
//! model identifier, so components can be linked statically into one
//! importer: the FMI functions through the standard function prefix, and the
//! kernel symbols and typed functions the units share through
//! `RMC_MODEL_PREFIX`, in a shared and in a static build alike.
//!
//! Each component's FMI 2 and FMI 3 sources are compiled warning-free with
//! and without `RMC_STATIC_BUILD`, and `nm` lists the globals each object of
//! the build without it, the one an importer makes, defines; the shared
//! library linked from those objects exports the FMI functions only. A Mach-O
//! object spells a C symbol with a leading underscore, which the check
//! removes first.

use super::*;

/// A component with a typed function owner and a projection block.
const TYPED: &str = "model TypedLoop\n  function f\n    input Real u;\n    output Real y;\n  algorithm\n    y := u * u + sin(u);\n  end f;\n  Real x(start = 1, fixed = true);\n  Real z(start = 0.5);\nequation\n  der(x) = -0.5 * z;\n  z + 0.3 * sin(z) = f(x);\nend TypedLoop;\n";

/// A component whose refresh is assignments only, with no projection block.
const ASSIGN_ONLY: &str = "model AssignOnly\n  Real x(start = 1, fixed = true);\n  Real y;\n  Real z;\nequation\n  der(x) = -0.5 * y;\n  y = 2 * x + sin(x);\n  z = y * y;\nend AssignOnly;\n";

/// The standard headers of `target`, when the conformance prerequisites are
/// available.
fn target_headers(target: &str) -> Option<PathBuf> {
    if !conformance_prerequisites_are_available() {
        return None;
    }
    let (fmi2, fmi3) = standard_roots();
    let (root, header) = if target == "fmi2" {
        (fmi2.root, "fmi2Functions.h")
    } else {
        (fmi3.root, "fmi3Functions.h")
    };
    let headers = root.join("headers");
    headers.join(header).is_file().then_some(headers)
}

fn headers_available() -> Option<()> {
    target_headers("fmi2")
        .and(target_headers("fmi3"))
        .map(|_| ())
}

/// The defined globals that do not carry the model identifier, per target.
fn bare_globals(compiled: &rumoca::CompilationResult, model: &'static str) -> Vec<String> {
    let mut bare = Vec::new();
    for target in ["fmi3", "fmi2"] {
        let headers = target_headers(target).expect("checked by the caller");
        let work = tempdir().expect("static build work directory");
        let globals = defined_globals(work.path(), &headers, compiled, (model, target));
        assert!(!globals.is_empty(), "{model} {target}: no global symbol");
        bare.extend(
            globals
                .into_iter()
                .filter(|symbol| !symbol.starts_with("Rumoca_"))
                .map(|symbol| format!("{target}: {symbol}")),
        );
    }
    bare
}

#[test]
fn a_static_build_prefixes_every_shared_kernel_symbol() {
    if headers_available().is_none() {
        return;
    }
    let compiled = super::affine_promotion::compile();
    let bare = bare_globals(&compiled, super::affine_promotion::MODEL);
    assert!(
        bare.is_empty(),
        "globals without the model identifier: {bare:?}"
    );
}

/// The typed functions the units share carry the model identifier too.
#[test]
fn a_static_build_prefixes_every_shared_typed_function() {
    if headers_available().is_none() {
        return;
    }
    let compiled = rumoca::Compiler::new()
        .model("TypedLoop")
        .compile_str(TYPED, "TypedLoop.mo")
        .expect("compile the typed loop");
    let bare = bare_globals(&compiled, "TypedLoop");
    assert!(
        bare.is_empty(),
        "globals without the model identifier: {bare:?}"
    );
}

/// A component whose refresh is assignments only, with no projection block,
/// still has its dispatcher's bad-index answer and fault state, and compiles
/// warning-free as a shared library and as a static build.
#[test]
fn an_assignment_only_component_compiles_with_its_fault_state() {
    if headers_available().is_none() {
        return;
    }
    let compiled = rumoca::Compiler::new()
        .model("AssignOnly")
        .compile_str(ASSIGN_ONLY, "AssignOnly.mo")
        .expect("compile the assignment-only model");
    let bare = bare_globals(&compiled, "AssignOnly");
    assert!(
        bare.is_empty(),
        "globals without the model identifier: {bare:?}"
    );
}

/// The global symbols the component's units define when each is compiled
/// warning-free as an importer builds them, with no build macro, a leading
/// Mach-O underscore removed; after the same units compile warning-free with
/// `RMC_STATIC_BUILD` too. The symbols the shared library linked from them
/// exports that are not FMI functions follow, each marked `exported`.
fn defined_globals(
    work: &Path,
    headers: &Path,
    compiled: &rumoca::CompilationResult,
    (model, target): (&'static str, &'static str),
) -> Vec<String> {
    let fmu = build_named_fmu(work, compiled, target, model);
    let sources = fmu.root.join("sources");
    let mut units: Vec<PathBuf> = fs::read_dir(&sources)
        .expect("list the generated sources")
        .map(|entry| entry.expect("a generated source").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "c"))
        .collect();
    units.sort();
    let mut globals = Vec::new();
    let mut objects = Vec::new();
    for unit in &units {
        let object = work
            .join(unit.file_stem().expect("a unit name"))
            .with_extension("o");
        for static_build in [true, false] {
            let mut command = Command::new("cc");
            command
                .args([
                    "-std=c11",
                    "-ffp-contract=off",
                    "-fPIC",
                    "-Wall",
                    "-Wextra",
                    "-Werror",
                ])
                .arg(format!("-I{}", sources.display()))
                .arg(format!("-I{}", headers.display()))
                .arg("-c")
                .arg(unit)
                .arg("-o")
                .arg(&object);
            if static_build {
                command.arg("-DRMC_STATIC_BUILD");
            }
            checked_output(&mut command, "compile a unit");
        }
        globals.extend(symbols(
            Command::new("nm")
                .args(["-g", "--defined-only"])
                .arg(&object),
        ));
        objects.push(object);
    }
    let library = work.join(format!("{model}.so"));
    checked_output(
        Command::new("cc")
            .arg("-shared")
            .args(&objects)
            .arg("-lm")
            .arg("-o")
            .arg(&library),
        "link the shared library",
    );
    globals.extend(
        symbols(
            Command::new("nm")
                .args(["-D", "--defined-only"])
                .arg(&library),
        )
        .into_iter()
        .filter(|symbol| !symbol.contains(target))
        .map(|symbol| format!("exported {symbol}")),
    );
    globals
}

/// The symbol names `nm` lists, a leading Mach-O underscore removed.
fn symbols(command: &mut Command) -> Vec<String> {
    let output = checked_output(command, "list the symbols");
    String::from_utf8_lossy(&output.stdout)
        .lines()
        .filter_map(|line| line.split_whitespace().last())
        .map(|symbol| symbol.strip_prefix('_').unwrap_or(symbol).to_owned())
        .collect()
}
