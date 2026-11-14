//! A linked module is its browser module with only its link names changed.
use super::product::{self, BrowserModule, Sources};
use crate::emit::MathImport;
use wasmparser::{ExternalKind, Parser, Payload, TypeRef};

#[derive(Debug, Default)]
struct Shape {
    /// Every section except imports and exports: id and raw bytes.
    sections: Vec<(u8, Vec<u8>)>,
    imports: Vec<(String, String, TypeRef)>,
    exports: Vec<(String, ExternalKind, u32)>,
    /// `(minimum, maximum)` pages of each memory the module defines.
    defined_memories: Vec<(u64, Option<u64>)>,
}

fn shape(bytes: &[u8]) -> Shape {
    let mut shape = Shape::default();
    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload.unwrap();
        match &payload {
            Payload::ImportSection(reader) => {
                for import in reader.clone() {
                    let import = import.unwrap();
                    shape
                        .imports
                        .push((import.module.into(), import.name.into(), import.ty));
                }
                continue;
            }
            Payload::ExportSection(reader) => {
                for export in reader.clone() {
                    let export = export.unwrap();
                    shape
                        .exports
                        .push((export.name.into(), export.kind, export.index));
                }
                continue;
            }
            Payload::MemorySection(reader) => {
                for memory in reader.clone() {
                    let memory = memory.unwrap();
                    shape
                        .defined_memories
                        .push((memory.initial, memory.maximum));
                }
            }
            _ => {}
        }
        if let Some((id, range)) = payload.as_section() {
            shape.sections.push((id, bytes[range].to_vec()));
        }
    }
    shape
}

/// The browser modules in the order the assembler emits linked modules, each
/// with the symbol its control module's helper imports are renamed to.
fn browser_modules(product: &product::Product) -> Vec<(Vec<u8>, Option<String>)> {
    let mut modules = Vec::new();
    for module in &product.browser {
        match module {
            BrowserModule::Derivative {
                helpers,
                control,
                entries,
            } => {
                for (_, bytes) in helpers {
                    modules.push((bytes.clone(), None));
                }
                modules.push((control.clone(), Some(entries[0].0.symbol())));
            }
            BrowserModule::Kernel { bytes, .. } => modules.push((bytes.clone(), None)),
        }
    }
    modules
}

#[test]
fn every_linked_module_keeps_its_browser_sections_and_changes_only_link_names() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let linked = product.linked.modules();
    let browser = browser_modules(&product);
    assert_eq!(linked.len(), browser.len());
    for (linked, (browser, operations)) in linked.iter().zip(&browser) {
        let (linked_shape, browser_shape) = (shape(linked.module_bytes()), shape(browser));
        let name = &linked.exports()[0];
        assert_eq!(linked_shape.sections, browser_shape.sections, "{name}");
        let renamed_imports = browser_shape
            .imports
            .iter()
            .map(|(module, field, ty)| match (module.as_str(), operations) {
                ("operation", Some(operations)) => {
                    ("env".to_string(), format!("{operations}_{field}"), *ty)
                }
                _ => (module.clone(), field.clone(), *ty),
            })
            .collect::<Vec<_>>();
        assert_eq!(linked_shape.imports, renamed_imports, "{name}");
        let browser_entries = browser_shape
            .exports
            .iter()
            .filter(|(field, kind, _)| !(field == "memory" && *kind == ExternalKind::Memory))
            .collect::<Vec<_>>();
        assert_eq!(
            browser_entries.len() + 1,
            browser_shape.exports.len(),
            "{name}: the browser module re-exports its memory once"
        );
        assert_eq!(linked_shape.exports.len(), browser_entries.len(), "{name}");
        for ((field, kind, index), (_, browser_kind, browser_index)) in
            linked_shape.exports.iter().zip(browser_entries)
        {
            assert_eq!((kind, index), (browser_kind, browser_index), "{name}");
            assert!(linked.exports().contains(field), "{name}: {field}");
        }
        assert_eq!(
            linked_shape
                .exports
                .iter()
                .map(|(field, ..)| field.clone())
                .collect::<Vec<_>>(),
            linked.exports(),
        );
    }
}

/// Imports are the one shared memory, the math ABI and symbols an earlier
/// module exports; the listing of every memory is printed for the report.
#[test]
fn linked_modules_import_only_shared_memory_math_and_issued_symbols() {
    let sources = Sources::new();
    let product = product::build(&sources);
    let math = MathImport::ALL.map(MathImport::symbol);
    let mut exported = Vec::<String>::new();
    for module in product.linked.modules() {
        let shape = shape(module.module_bytes());
        let name = &module.exports()[0];
        let mut memories = 0;
        for (field_module, field, ty) in &shape.imports {
            assert_eq!(field_module, "env", "{name}");
            match ty {
                TypeRef::Memory(memory) => {
                    assert_eq!(field, "memory", "{name}");
                    assert!(!memory.memory64 && !memory.shared, "{name}");
                    memories += 1;
                }
                TypeRef::Func(_) => assert!(
                    math.contains(&field.as_str()) || exported.contains(field),
                    "{name} imports {field}, which neither the math ABI nor an earlier module provides"
                ),
                other => panic!("{name} imports {other:?}"),
            }
        }
        assert_eq!(memories, 1, "{name} imports exactly the shared memory");
        assert!(
            shape
                .exports
                .iter()
                .all(|(_, kind, _)| *kind == ExternalKind::Func),
            "{name} exports functions only"
        );
        assert!(shape.defined_memories.len() <= 1, "{name}");
        let control = module.exports().iter().any(|symbol| {
            symbol.starts_with("rumoca_k_derivative_")
                && !symbol.ends_with("values")
                && !symbol.ends_with("refresh")
        });
        if control {
            assert!(shape.defined_memories.is_empty(), "{name}");
        }
        println!(
            "memories {name}: imports env.memory; defines {:?} (pages min, max), exported none",
            shape.defined_memories
        );
        exported.extend(module.exports().iter().cloned());
    }
    let mut unique = exported.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(
        unique.len(),
        exported.len(),
        "no two modules export one symbol"
    );
}
