//! One linked component from libraries in a fixed order, each symbol with
//! exactly one actual provider.

use std::collections::BTreeMap;

/// Link `libraries` (name, core module bytes) in their order into one
/// component: validated, with no stubbed function and no libdl. Every name a
/// library actually exports must be exported by no other library, so linker
/// priority never chooses between two providers of one symbol.
pub fn link_single_provider_graph(libraries: &[(&str, &[u8])]) -> Result<Vec<u8>, String> {
    let mut providers: BTreeMap<&str, &str> = BTreeMap::new();
    for &(library, bytes) in libraries {
        for name in exports(bytes).map_err(|error| format!("{library}: {error}"))? {
            if let Some(previous) = providers.insert(name, library) {
                return Err(format!(
                    "linked symbol {name} is exported by both {previous} and {library}"
                ));
            }
        }
    }
    let mut linker = wit_component::Linker::default()
        .validate(true)
        .stub_missing_functions(false)
        .use_built_in_libdl(false);
    for &(library, bytes) in libraries {
        linker = linker
            .library(library, bytes, false)
            .map_err(|error| format!("{error:#}"))?;
    }
    linker.encode().map_err(|error| format!("{error:#}"))
}

/// The export names of one core module, from its own export section.
fn exports(bytes: &[u8]) -> Result<Vec<&str>, String> {
    let mut names = Vec::new();
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        if let wasmparser::Payload::ExportSection(exports) =
            payload.map_err(|error| error.to_string())?
        {
            for export in exports {
                names.push(export.map_err(|error| error.to_string())?.name);
            }
        }
    }
    Ok(names)
}

#[cfg(test)]
mod tests;
