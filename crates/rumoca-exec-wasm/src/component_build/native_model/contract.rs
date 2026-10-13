//! Check the actual portable emitter contract; this does not issue an entry.

use crate::{CompiledNativeCallProgramWasm, emit::MathImport};
use wasmparser::{ExternalKind, FuncType, MemoryType, Parser, Payload, TypeRef, ValType};

pub(super) fn check(compiled: &CompiledNativeCallProgramWasm) -> Result<(), String> {
    check_profile(
        compiled.module_bytes(),
        compiled.math_imports(),
        true,
        compiled.capacity_policy(),
    )
}

#[cfg(test)]
fn check_bytes(bytes: &[u8], math: &[&str]) -> Result<(), String> {
    check_profile(
        bytes,
        math,
        true,
        crate::CheckedNativeWasmCapacityPolicy::default(),
    )
}

pub(super) fn check_linkable(
    bytes: &[u8],
    math: &[&str],
    policy: crate::CheckedNativeWasmCapacityPolicy,
) -> Result<(), String> {
    check_profile(bytes, math, false, policy)
}

fn check_profile(
    bytes: &[u8],
    math: &[&str],
    public_export: bool,
    policy: crate::CheckedNativeWasmCapacityPolicy,
) -> Result<(), String> {
    wasmparser::Validator::new()
        .validate_all(bytes)
        .map_err(|error| format!("validate native model library: {error}"))?;
    let mut contract = Contract {
        policy,
        ..Contract::default()
    };
    for payload in Parser::new(0).parse_all(bytes) {
        contract.read(payload.map_err(|error| error.to_string())?, math)?;
    }
    contract.finish(math, public_export)
}

#[derive(Default)]
struct Contract {
    policy: crate::CheckedNativeWasmCapacityPolicy,
    types: Vec<FuncType>,
    function_types: Vec<u32>,
    math: usize,
    public_memory: bool,
    private_memory: bool,
    memory_export: bool,
    entry: Option<u32>,
}

impl Contract {
    fn read(&mut self, payload: Payload<'_>, math: &[&str]) -> Result<(), String> {
        match payload {
            Payload::Version {
                encoding: wasmparser::Encoding::Module,
                ..
            }
            | Payload::CodeSectionStart { .. }
            | Payload::CodeSectionEntry(_)
            | Payload::End(_) => {}
            Payload::TypeSection(types) => {
                self.types = types
                    .into_iter_err_on_gc_types()
                    .collect::<Result<_, _>>()
                    .map_err(|error| error.to_string())?;
            }
            Payload::ImportSection(imports) => {
                for import in imports {
                    self.import(import.map_err(|error| error.to_string())?, math)?;
                }
            }
            Payload::FunctionSection(functions) => {
                for function in functions {
                    self.function_types
                        .push(function.map_err(|error| error.to_string())?);
                }
            }
            Payload::MemorySection(memories) => {
                for memory in memories {
                    self.private_memory(memory.map_err(|error| error.to_string())?)?;
                }
            }
            Payload::ExportSection(exports) => {
                for export in exports {
                    let export = export.map_err(|error| error.to_string())?;
                    match (export.name, export.kind, export.index) {
                        ("memory", ExternalKind::Memory, 0) => self.memory_export = true,
                        ("eval_assignments", ExternalKind::Func, index) => self.entry = Some(index),
                        _ => return Err("native model has an unsupported export".into()),
                    }
                }
            }
            _ => return Err("native model has an unsupported section or encoding".into()),
        }
        Ok(())
    }

    fn private_memory(&mut self, memory: MemoryType) -> Result<(), String> {
        if self.private_memory || !private_memory_with_policy(memory, self.policy) {
            return Err("native model requires one bounded private arena".into());
        }
        self.private_memory = true;
        Ok(())
    }

    fn import(&mut self, import: wasmparser::Import<'_>, math: &[&str]) -> Result<(), String> {
        if import.module != "env" {
            return Err("native model imports must come from env".into());
        }
        match import.ty {
            TypeRef::Memory(memory) => {
                if import.name != "memory" || self.public_memory || !public_memory(memory) {
                    return Err("native model requires its original public memory import".into());
                }
                self.public_memory = true;
            }
            TypeRef::Func(index) => {
                if math.get(self.math).copied() != Some(import.name) {
                    return Err("native model math import differs from emitted inventory".into());
                }
                let owner = MathImport::from_symbol(import.name)
                    .ok_or("native model math import has no emitter owner")?;
                let inputs = if owner.is_binary() { 2 } else { 1 };
                let ty = self
                    .types
                    .get(index as usize)
                    .ok_or("missing math import type")?;
                if ty.params().len() != inputs
                    || !ty.params().iter().all(|t| *t == ValType::F64)
                    || ty.results() != [ValType::F64]
                {
                    return Err("native model math signature differs from emitter owner".into());
                }
                self.function_types.push(index);
                self.math += 1;
            }
            _ => return Err("native model has an unsupported import".into()),
        }
        Ok(())
    }

    fn finish(self, math: &[&str], public_export: bool) -> Result<(), String> {
        if !self.public_memory
            || !self.private_memory
            || self.memory_export != public_export
            || self.math != math.len()
        {
            return Err("native model has an incomplete memory or import contract".into());
        }
        let entry = self
            .entry
            .ok_or("native model has no eval_assignments export")?;
        if (entry as usize) < self.math {
            return Err("native model entry must be a defined function".into());
        }
        let type_index = self
            .function_types
            .get(entry as usize)
            .ok_or("missing native entry function")?;
        let ty = self
            .types
            .get(*type_index as usize)
            .ok_or("missing native entry type")?;
        if ty.params()
            != [
                ValType::I32,
                ValType::I32,
                ValType::F64,
                ValType::I32,
                ValType::I32,
            ]
            || ty.results() != [ValType::I32]
        {
            return Err("native model has a different eval_assignments ABI".into());
        }
        Ok(())
    }
}

fn wasm32_unshared(memory: MemoryType) -> bool {
    !memory.memory64 && !memory.shared && memory.page_size_log2.is_none()
}

fn public_memory(memory: MemoryType) -> bool {
    wasm32_unshared(memory) && memory.initial == 1 && memory.maximum.is_none()
}

#[cfg(test)]
fn private_memory(memory: MemoryType) -> bool {
    private_memory_with_policy(memory, crate::CheckedNativeWasmCapacityPolicy::default())
}

fn private_memory_with_policy(
    memory: MemoryType,
    policy: crate::CheckedNativeWasmCapacityPolicy,
) -> bool {
    wasm32_unshared(memory)
        && (1..=policy.private_arena_pages()).contains(&memory.initial)
        && memory.maximum == Some(memory.initial)
}

#[cfg(test)]
mod tests;
