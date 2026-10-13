//! Structural core linkage; this product carries no FMI admission authority.

use std::collections::BTreeSet;
use wasm_encoder::{Component, ComponentSectionId, InstanceSection, ModuleArg, RawSection};

/// One import module supplied by an earlier core instance.
#[derive(Clone, Copy, Debug)]
pub struct CoreModuleArgument<'a> {
    pub import_module: &'a str,
    pub source_module: &'a str,
}

/// Borrowed module draft in dependency order, without execution admission.
#[derive(Clone, Copy, Debug)]
pub struct CoreModuleDraft<'a> {
    pub name: &'a str,
    pub bytes: &'a [u8],
    pub arguments: &'a [CoreModuleArgument<'a>],
}

/// A validated, linked core graph. It is not an executable FMI artifact.
/// No conversion into the sealed component publication result exists.
pub struct CoreModuleAssembly {
    bytes: Vec<u8>,
}

impl CoreModuleAssembly {
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
}

/// Embed actual core modules and instantiate them in their declared order.
/// Memory/function compatibility is checked on the complete linked binary.
pub fn compose_core_modules(modules: &[CoreModuleDraft<'_>]) -> Result<CoreModuleAssembly, String> {
    if modules.is_empty() {
        return Err("core assembly requires at least one module".into());
    }
    let mut names = Vec::with_capacity(modules.len());
    let mut component = Component::new();
    for module in modules {
        validate_core_module(module.bytes)?;
        if module.name.is_empty() || names.contains(&module.name) {
            return Err("core module names must be nonempty and unique".into());
        }
        let arguments = resolve_arguments(module.arguments, &names)?;
        let index = u32::try_from(names.len()).map_err(|_| "core module index overflow")?;
        component.section(&RawSection {
            id: ComponentSectionId::CoreModule.into(),
            data: module.bytes,
        });
        let mut instances = InstanceSection::new();
        instances.instantiate(index, arguments);
        component.section(&instances);
        names.push(module.name);
    }
    let bytes = component.finish();
    wasmparser::Validator::new()
        .validate_all(&bytes)
        .map_err(|error| format!("validate linked core assembly: {error}"))?;
    Ok(CoreModuleAssembly { bytes })
}

fn resolve_arguments<'a>(
    arguments: &[CoreModuleArgument<'a>],
    names: &[&str],
) -> Result<Vec<(&'a str, ModuleArg)>, String> {
    let mut seen = BTreeSet::new();
    arguments
        .iter()
        .map(|argument| {
            if argument.import_module.is_empty() || !seen.insert(argument.import_module) {
                return Err("core import module names must be nonempty and unique".into());
            }
            let index = names
                .iter()
                .position(|name| *name == argument.source_module)
                .ok_or("core import must reference an earlier module")?;
            let index = u32::try_from(index).map_err(|_| "core instance index overflow")?;
            Ok((argument.import_module, ModuleArg::Instance(index)))
        })
        .collect()
}

fn validate_core_module(bytes: &[u8]) -> Result<(), String> {
    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        match payload.map_err(|error| format!("parse core module: {error}"))? {
            wasmparser::Payload::Version { encoding, .. }
                if encoding != wasmparser::Encoding::Module =>
            {
                return Err("core assembly input is not a core module".into());
            }
            wasmparser::Payload::MemorySection(memories) => {
                for memory in memories {
                    check_memory(memory.map_err(|error| error.to_string())?)?;
                }
            }
            wasmparser::Payload::ImportSection(imports) => check_imported_memories(imports)?,
            _ => {}
        }
    }
    Ok(())
}

fn check_imported_memories(imports: wasmparser::ImportSectionReader<'_>) -> Result<(), String> {
    for import in imports {
        if let wasmparser::TypeRef::Memory(memory) = import.map_err(|error| error.to_string())?.ty {
            check_memory(memory)?;
        }
    }
    Ok(())
}

fn check_memory(memory: wasmparser::MemoryType) -> Result<(), String> {
    if memory.shared || memory.memory64 {
        return Err("core assembly requires unshared wasm32 memory".into());
    }
    Ok(())
}

#[cfg(test)]
mod tests;
