//! Export-only refinement of one actual checked native emitter product.

use super::NativeModelLibrary;
use wasm_encoder::{ExportKind, ExportSection, Module, RawSection};
use wasmparser::{ExternalKind, Parser, Payload};

/// Original source product retained with its linkable target export boundary.
/// It carries no static entry, allocator, execution or FMI publication authority.
/// No arbitrary byte constructor exists.
///
/// ```compile_fail
/// use rumoca_exec_wasm::{NativeLinkableModelLibrary, NativeModelLibrary};
/// fn forge(original: NativeModelLibrary, bytes: Vec<u8>) -> NativeLinkableModelLibrary {
///     NativeLinkableModelLibrary { original, bytes }
/// }
/// ```
pub struct NativeLinkableModelLibrary {
    original: NativeModelLibrary,
    bytes: Vec<u8>,
}

impl NativeLinkableModelLibrary {
    pub fn original(&self) -> &NativeModelLibrary {
        &self.original
    }

    pub fn module_bytes(&self) -> &[u8] {
        &self.bytes
    }
}

impl NativeModelLibrary {
    /// Retain this same checked source and all emitted instructions. The public
    /// memory remains an import; only its browser-facing re-export is omitted.
    pub fn into_linkable(self) -> Result<NativeLinkableModelLibrary, String> {
        self.check_linkage_contract()?;
        let original = self.compiled();
        let bytes = omit_public_memory_export(original.module_bytes())?;
        super::contract::check_linkable(
            &bytes,
            original.math_imports(),
            original.capacity_policy(),
        )?;
        Ok(NativeLinkableModelLibrary {
            original: self,
            bytes,
        })
    }
}

fn omit_public_memory_export(bytes: &[u8]) -> Result<Vec<u8>, String> {
    let mut module = Module::new();
    for payload in Parser::new(0).parse_all(bytes) {
        let payload = payload.map_err(|error| error.to_string())?;
        if let Payload::ExportSection(exports) = payload {
            let target = linkable_exports(exports)?;
            module.section(&target);
        } else if let Some((id, range)) = payload.as_section() {
            module.section(&RawSection {
                id,
                data: &bytes[range],
            });
        }
    }
    Ok(module.finish())
}

fn linkable_exports(exports: wasmparser::ExportSectionReader<'_>) -> Result<ExportSection, String> {
    let mut target = ExportSection::new();
    for export in exports {
        let export = export.map_err(|error| error.to_string())?;
        match (export.name, export.kind, export.index) {
            ("memory", ExternalKind::Memory, 0) => {}
            ("eval_assignments", ExternalKind::Func, index) => {
                target.export(export.name, ExportKind::Func, index);
            }
            _ => return Err("linkable refinement received an unchecked export".into()),
        }
    }
    Ok(target)
}
