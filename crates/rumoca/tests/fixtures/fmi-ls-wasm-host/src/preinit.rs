//! Build-time pre-initialisation of a generated component (feature `preinit`).
//!
//! Two modes:
//!   preinit-scan <component> <init-export>
//!       Static only, nothing is instantiated. Lists every construct Wizer refuses
//!       (table, element, data and GC writes in any function body; mutable
//!       reference globals), the top-level exports, and Wizer's own static
//!       instrumentation verdict. Prints `PREINIT_SCAN_OK` or fails.
//!   preinit <component> <init-export> <out>
//!       Instantiates with every import defined as a trap except one: std's
//!       hash-map seed, `wasi:random/insecure-seed`, answers a fixed recorded
//!       constant (std seeds `RandomState` from it once per thread, and model
//!       receipt builds std hash maps). So the init export can reach no clock,
//!       entropy, environment, file, stream or FMI callback; it is called once,
//!       the seed calls are counted and printed, the instance is snapshotted, the
//!       init export and writes `<out>` (never overwritten). The output's
//!       top-level exports must be the input's minus the init export, and the
//!       output must instantiate under the canonical `co-simulation-fmu`
//!       bindings, which type-check every world import and export. Wizer has
//!       removed the start functions, so that instantiation runs no guest code.
//!       The publication gate stays the compiler's own interface check.

use anyhow::{Context, Result, bail, ensure};
use std::io::Write;
use std::task::{Poll, Waker};
use wasmparser::{Encoding, Operator, Parser, Payload, ValType};
use wasmtime::component::{Component, Instance, Linker};
use wasmtime::{Config, Engine, Store, StoreContextMut};
use wasmtime_wizer::Wizer;

/// The fixed hash-map seed answered during pre-initialisation, baked into the
/// snapshot. Hash-map keys only need to be unpredictable to untrusted input,
/// which a fixed published model does not supply; no ordering may depend on them.
const PREINIT_SEED: (u64, u64) = (0x7275_6d6f_6361_0001, 0x7072_6569_6e69_7401);
const SEED_INTERFACE: &str = "wasi:random/insecure-seed@";

/// Violations printed individually; the per-operator totals are always complete.
const LISTED_VIOLATIONS: usize = 200;

pub(super) fn run_scan(component_path: &str, init_export: &str) -> Result<()> {
    let bytes = std::fs::read(component_path).context("read component")?;
    let scan = scan(&bytes)?;
    println!("modules={} functions={}", scan.modules, scan.functions);
    for export in &scan.exports {
        println!("export {} {}", export.kind, export.name);
    }
    for violation in scan.violations.iter().take(LISTED_VIOLATIONS) {
        println!(
            "refused module={} function={} {}",
            violation.module, violation.function, violation.what
        );
    }
    let mut totals = std::collections::BTreeMap::<&str, usize>::new();
    for violation in &scan.violations {
        *totals.entry(violation.what).or_default() += 1;
    }
    for (what, count) in &totals {
        println!("refused_total {what}={count}");
    }
    let has_init = scan
        .exports
        .iter()
        .any(|export| export.name == init_export && export.kind == "func");
    println!("init_export {init_export} present={has_init}");
    let wizer_verdict = Wizer::new().instrument_component(&bytes).map(|_| ());
    match &wizer_verdict {
        Ok(()) => println!("wizer_static ok"),
        Err(error) => println!("wizer_static refused: {error:#}"),
    }
    ensure!(has_init, "no top-level function export `{init_export}`");
    ensure!(
        scan.violations.is_empty(),
        "{} refused constructs",
        scan.violations.len()
    );
    ensure!(
        wizer_verdict.is_ok(),
        "Wizer refused the component statically"
    );
    println!("PREINIT_SCAN_OK");
    Ok(())
}

pub(super) fn run_preinit(component_path: &str, init_export: &str, output: &str) -> Result<()> {
    let bytes = std::fs::read(component_path).context("read component")?;
    let before = scan(&bytes)?.exports;
    ensure!(
        before
            .iter()
            .any(|export| export.name == init_export && export.kind == "func"),
        "no top-level function export `{init_export}`"
    );
    ensure!(
        !std::path::Path::new(output).exists(),
        "{output} exists; a snapshot is never overwritten"
    );

    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;
    let seed_imports: Vec<String> = Component::new(&engine, &bytes)?
        .component_type()
        .imports(&engine)
        .map(|(name, _)| name.to_owned())
        .filter(|name| name.starts_with(SEED_INTERFACE))
        .collect();
    // The store counts the seed calls the init export makes.
    let mut store = Store::new(&engine, 0u64);
    let mut wizer = Wizer::new();
    wizer.init_func(init_export).keep_init_func(false);
    let snapshot = block_on(wizer.run_component(&mut store, &bytes, instantiate_trapping))??;

    let after = scan(&snapshot)?.exports;
    let expected: Vec<_> = before
        .into_iter()
        .filter(|export| export.name != init_export)
        .collect();
    ensure!(
        after == expected,
        "snapshot exports differ from the input's minus `{init_export}`: {after:?} versus {expected:?}"
    );
    check_public_world(&engine, &snapshot)?;
    // Written only after every check, so a refused snapshot leaves no file.
    std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(output)
        .and_then(|mut file| file.write_all(&snapshot))
        .with_context(|| format!("write {output} (never overwritten)"))?;
    println!(
        "PREINIT_OK in_bytes={} out_bytes={} exports={} seed_imports={seed_imports:?} seed_calls={} seed={:#x},{:#x}",
        bytes.len(),
        snapshot.len(),
        after.len(),
        store.data(),
        PREINIT_SEED.0,
        PREINIT_SEED.1
    );
    Ok(())
}

/// Typed lookup of every import and export of the canonical world.
fn check_public_world(engine: &Engine, snapshot: &[u8]) -> Result<()> {
    let component = Component::new(engine, snapshot).context("compile snapshot")?;
    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_sync(&mut linker)?;
    super::CoSimulationFmu::add_to_linker::<
        super::HostState,
        wasmtime::component::HasSelf<super::HostState>,
    >(&mut linker, |state| state)?;
    let mut store = Store::new(
        engine,
        super::HostState {
            wasi: wasmtime_wasi::WasiCtxBuilder::new().build(),
            table: wasmtime::component::ResourceTable::new(),
            messages: Vec::new(),
        },
    );
    super::CoSimulationFmu::instantiate(&mut store, &component, &linker)
        .context("snapshot does not match the canonical co-simulation-fmu world")?;
    Ok(())
}

/// Every import traps except the hash-map seed: pre-initialisation may not read
/// clocks, entropy, the environment, files or streams, nor call the FMI callbacks.
async fn instantiate_trapping(
    store: &mut Store<u64>,
    component: &Component,
) -> wasmtime::Result<Instance> {
    let mut linker = Linker::new(store.engine());
    let seeds: Vec<String> = component
        .component_type()
        .imports(store.engine())
        .map(|(name, _)| name.to_owned())
        .filter(|name| name.starts_with(SEED_INTERFACE))
        .collect();
    for name in seeds {
        linker.instance(&name)?.func_wrap(
            "insecure-seed",
            |mut store: StoreContextMut<'_, u64>, (): ()| {
                *store.data_mut() += 1;
                Ok((PREINIT_SEED,))
            },
        )?;
    }
    linker.define_unknown_imports_as_traps(component)?;
    linker.instantiate_async(store, component).await
}

/// No host function is async, so the snapshot completes on its first poll.
fn block_on<F: Future>(future: F) -> Result<F::Output> {
    let mut future = std::pin::pin!(future);
    match future
        .as_mut()
        .poll(&mut std::task::Context::from_waker(Waker::noop()))
    {
        Poll::Ready(output) => Ok(output),
        Poll::Pending => bail!("pre-initialisation suspended without an async host"),
    }
}

#[derive(Debug, PartialEq)]
struct Export {
    name: String,
    kind: String,
}

struct Violation {
    module: usize,
    function: usize,
    what: &'static str,
}

struct Scan {
    modules: usize,
    functions: usize,
    exports: Vec<Export>,
    violations: Vec<Violation>,
}

fn scan(bytes: &[u8]) -> Result<Scan> {
    let mut scan = Scan {
        modules: 0,
        functions: 0,
        exports: Vec::new(),
        violations: Vec::new(),
    };
    let mut nesting = Vec::new();
    let mut function = 0;
    for payload in Parser::new(0).parse_all(bytes) {
        match payload? {
            Payload::Version { encoding, .. } => {
                if encoding == Encoding::Module {
                    scan.modules += 1;
                    function = 0;
                }
                nesting.push(encoding);
            }
            Payload::End(_) => {
                nesting.pop();
            }
            Payload::ComponentExportSection(exports) if nesting.len() == 1 => {
                for export in exports {
                    let export = export?;
                    scan.exports.push(Export {
                        name: export.name.0.to_string(),
                        kind: format!("{:?}", export.kind).to_lowercase(),
                    });
                }
            }
            Payload::GlobalSection(globals) => {
                for global in globals {
                    let ty = global?.ty;
                    if ty.mutable && matches!(ty.content_type, ValType::Ref(_)) {
                        scan.violations.push(Violation {
                            module: scan.modules,
                            function: usize::MAX,
                            what: "mutable reference global",
                        });
                    }
                }
            }
            Payload::CodeSectionEntry(body) => {
                scan.functions += 1;
                let mut operators = body.get_operators_reader()?;
                while !operators.eof() {
                    if let Some(what) = refused(&operators.read()?) {
                        scan.violations.push(Violation {
                            module: scan.modules,
                            function,
                            what,
                        });
                    }
                }
                function += 1;
            }
            _ => {}
        }
    }
    ensure!(nesting.is_empty(), "unbalanced component nesting");
    Ok(scan)
}

/// The operators Wizer's validation refuses in any function body.
fn refused(operator: &Operator<'_>) -> Option<&'static str> {
    Some(match operator {
        Operator::TableCopy { .. } => "table.copy",
        Operator::TableInit { .. } => "table.init",
        Operator::TableSet { .. } => "table.set",
        Operator::TableGrow { .. } => "table.grow",
        Operator::TableFill { .. } => "table.fill",
        Operator::ElemDrop { .. } => "elem.drop",
        Operator::DataDrop { .. } => "data.drop",
        Operator::StructSet { .. } => "struct.set",
        Operator::ArraySet { .. } => "array.set",
        Operator::ArrayFill { .. } => "array.fill",
        Operator::ArrayCopy { .. } => "array.copy",
        Operator::ArrayInitData { .. } => "array.init_data",
        Operator::ArrayInitElem { .. } => "array.init_elem",
        _ => return None,
    })
}

#[cfg(test)]
mod tests {
    use super::scan;
    use wasm_encoder::{
        CodeSection, ConstExpr, Function, FunctionSection, GlobalSection, GlobalType, HeapType,
        Instruction, Module, TypeSection, ValType,
    };

    fn module_with_body(instructions: &[Instruction<'_>]) -> Vec<u8> {
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types.ty().function([], []);
        module.section(&types);
        let mut functions = FunctionSection::new();
        functions.function(0);
        module.section(&functions);
        let mut code = CodeSection::new();
        let mut body = Function::new([]);
        for instruction in instructions {
            body.instruction(instruction);
        }
        body.instruction(&Instruction::End);
        code.function(&body);
        module.section(&code);
        module.finish()
    }

    #[test]
    fn every_table_and_element_write_is_listed() {
        let wasm = module_with_body(&[
            Instruction::TableGrow(0),
            Instruction::TableSet(0),
            Instruction::ElemDrop(0),
        ]);
        let found = scan(&wasm).unwrap();
        let what: Vec<_> = found.violations.iter().map(|v| v.what).collect();
        assert_eq!(what, ["table.grow", "table.set", "elem.drop"]);
        assert!(
            found
                .violations
                .iter()
                .all(|v| v.module == 1 && v.function == 0)
        );
    }

    #[test]
    fn memory_copy_and_fill_are_allowed() {
        let wasm = module_with_body(&[
            Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            },
            Instruction::MemoryFill(0),
        ]);
        let found = scan(&wasm).unwrap();
        assert!(found.violations.is_empty());
        assert_eq!((found.modules, found.functions), (1, 1));
    }

    #[test]
    fn a_mutable_reference_global_is_listed() {
        let mut module = Module::new();
        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType {
                val_type: ValType::FUNCREF,
                mutable: true,
                shared: false,
            },
            &ConstExpr::ref_null(HeapType::FUNC),
        );
        module.section(&globals);
        let found = scan(&module.finish()).unwrap();
        let what: Vec<_> = found.violations.iter().map(|v| v.what).collect();
        assert_eq!(what, ["mutable reference global"]);
    }
}
