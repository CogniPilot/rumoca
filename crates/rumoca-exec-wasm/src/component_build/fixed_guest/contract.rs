//! Allocation and internal-slot facts of the actual source-built core.

use crate::emit::MathImport;
use std::collections::{BTreeMap, BTreeSet};
use wasmparser::{
    Dylink0Subsection, FuncType, KnownCustom, MemInfo, Parser, Payload, TypeRef, ValType,
};

pub(super) struct FixedGuestContract {
    pub(super) imports: BTreeMap<(String, String), FixedImportRole>,
    pub(super) allocation: MemInfo,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum FixedImportRole {
    Internal,
    Platform,
}

/// The linked plan slots a scalar-linked core imports beside the source slots.
const LINKED_PLAN_SLOTS: [&str; 2] = ["rumoca_linked_plan_len", "rumoca_linked_plan_copy"];

/// This private parser can refuse arbitrary bytes, but cannot issue a source core.
/// The internal slots are exact per product kind: the native core imports the
/// four source slots and the native model entry; the scalar core imports the
/// four source slots and must not import the native model entry or the linked
/// dispatch; the scalar-linked core imports the four source slots, the linked
/// dispatch and the two linked plan slots, and must not import the native
/// model entry.
pub(super) fn check(
    bytes: &[u8],
    profile: super::GuestProfile,
) -> Result<FixedGuestContract, String> {
    wasmparser::Validator::new()
        .validate_all(bytes)
        .map_err(|error| format!("validate fixed guest core: {error}"))?;
    let mut state = State::default();
    for payload in Parser::new(0).parse_all(bytes) {
        state.read(payload.map_err(|error| error.to_string())?)?;
    }
    state.finish(profile)
}

#[derive(Default)]
struct State {
    imports: BTreeMap<(String, String), FixedImportRole>,
    types: Vec<FuncType>,
    slots: BTreeSet<String>,
    function_types: Vec<u32>,
    math_exports: BTreeMap<String, u32>,
    public_memory: bool,
    allocation: Option<MemInfo>,
}

impl State {
    fn read(&mut self, payload: Payload<'_>) -> Result<(), String> {
        match payload {
            Payload::Version { encoding, .. } if encoding != wasmparser::Encoding::Module => {
                return Err("fixed guest must be a core module, not a component".into());
            }
            Payload::TypeSection(types) => {
                self.types = types
                    .into_iter_err_on_gc_types()
                    .collect::<Result<_, _>>()
                    .map_err(|error| error.to_string())?;
            }
            Payload::ImportSection(imports) => {
                for import in imports {
                    self.import(import.map_err(|error| error.to_string())?)?;
                }
            }
            Payload::FunctionSection(functions) => {
                for function in functions {
                    self.function_types
                        .push(function.map_err(|error| error.to_string())?);
                }
            }
            Payload::ExportSection(exports) => {
                self.exports(exports)?;
            }
            Payload::MemorySection(memories) if memories.count() != 0 => {
                return Err(
                    "fixed guest must use the linked public memory, not a defined replacement"
                        .into(),
                );
            }
            Payload::TableSection(tables) if tables.count() != 0 => {
                return Err(
                    "fixed guest must use the linked table, not a defined replacement".into(),
                );
            }
            Payload::CustomSection(section) => {
                self.custom_section(section)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn custom_section(
        &mut self,
        section: wasmparser::CustomSectionReader<'_>,
    ) -> Result<(), String> {
        if let KnownCustom::Dylink0(subsections) = section.as_known() {
            for subsection in subsections {
                self.allocation(subsection.map_err(|error| error.to_string())?)?;
            }
        }
        Ok(())
    }

    fn allocation(&mut self, subsection: Dylink0Subsection<'_>) -> Result<(), String> {
        match subsection {
            Dylink0Subsection::MemInfo(info) => {
                if self.allocation.is_some()
                    || info.memory_alignment > 31
                    || info.table_alignment > 31
                {
                    return Err("fixed guest has duplicate or invalid dylink allocation".into());
                }
                self.allocation = Some(info);
            }
            Dylink0Subsection::Needed(needed) if !needed.is_empty() => {
                return Err(
                    "fixed guest cannot select an external provider through dylink Needed".into(),
                );
            }
            Dylink0Subsection::Unknown { .. } | Dylink0Subsection::RuntimePath(_) => {
                return Err("fixed guest has unsupported dynamic loader metadata".into());
            }
            _ => {}
        }
        Ok(())
    }

    fn import(&mut self, import: wasmparser::Import<'_>) -> Result<(), String> {
        let identity = (import.module.to_owned(), import.name.to_owned());
        if self.imports.contains_key(&identity) {
            return Err("fixed guest has a duplicate import provider slot".into());
        }
        if let TypeRef::Func(index) = import.ty {
            self.function_types.push(index);
        }
        let role = self.import_role(import)?;
        self.imports.insert(identity, role);
        Ok(())
    }

    fn import_role(&mut self, import: wasmparser::Import<'_>) -> Result<FixedImportRole, String> {
        if import.module == "GOT.mem" {
            return match import.ty {
                TypeRef::Global(global)
                    if matches!(import.name, "__heap_base" | "__heap_end")
                        && global.content_type == ValType::I32
                        && global.mutable
                        && !global.shared =>
                {
                    Ok(FixedImportRole::Internal)
                }
                _ => Err(format!(
                    "fixed guest has unsupported linker heap address import {}",
                    import.name
                )),
            };
        }
        if import.module != "env" {
            // Exact source-built external imports are retained for the final
            // WIT checker. This is not an allowlist or admission of their provider.
            return if matches!(import.ty, TypeRef::Func(_)) {
                Ok(FixedImportRole::Platform)
            } else {
                Err("fixed guest external platform imports must be functions".into())
            };
        }
        match import.ty {
            TypeRef::Memory(memory) if import.name == "memory" => {
                if self.public_memory
                    || memory.shared
                    || memory.memory64
                    || memory.page_size_log2.is_some()
                {
                    return Err("fixed guest requires one unshared wasm32 public memory".into());
                }
                self.public_memory = true;
            }
            TypeRef::Func(index) => {
                let ty = self
                    .types
                    .get(index as usize)
                    .ok_or("fixed guest import has no type")?;
                check_slot(import.name, ty)?;
                self.slots.insert(import.name.to_owned());
            }
            TypeRef::Global(global)
                if global.content_type == ValType::I32
                    && !global.shared
                    && global.mutable == (import.name == "__stack_pointer")
                    && matches!(
                        import.name,
                        "__memory_base"
                            | "__table_base"
                            | "__stack_pointer"
                            | "__heap_base"
                            | "__heap_end"
                    ) => {}
            TypeRef::Table(table)
                if import.name == "__indirect_function_table"
                    && table.element_type == wasmparser::RefType::FUNCREF
                    && !table.table64
                    && !table.shared => {}
            _ => {
                return Err(format!(
                    "fixed guest has unsupported env import {}",
                    import.name
                ));
            }
        }
        Ok(FixedImportRole::Internal)
    }

    fn exports(&mut self, exports: wasmparser::ExportSectionReader<'_>) -> Result<(), String> {
        for export in exports {
            let export = export.map_err(|error| error.to_string())?;
            check_reserved_export(export.name)?;
            if export.kind == wasmparser::ExternalKind::Func
                && MathImport::from_symbol(export.name).is_some()
            {
                self.math_exports
                    .insert(export.name.to_owned(), export.index);
            }
        }
        Ok(())
    }

    /// The core must provide the complete math ABI that emitted models import,
    /// each symbol with its exact Binary64 signature.
    fn check_math_abi(&self) -> Result<(), String> {
        for import in MathImport::ALL {
            let symbol = import.symbol();
            let index = self
                .math_exports
                .get(symbol)
                .ok_or_else(|| format!("fixed guest does not export math ABI symbol {symbol}"))?;
            let ty = self
                .function_types
                .get(*index as usize)
                .and_then(|ty| self.types.get(*ty as usize))
                .ok_or_else(|| format!("fixed guest math ABI symbol {symbol} has no type"))?;
            let inputs = if import.is_binary() { 2 } else { 1 };
            if ty.params() != vec![ValType::F64; inputs] || ty.results() != [ValType::F64] {
                return Err(format!(
                    "fixed guest math ABI symbol {symbol} differs from the emitted signature"
                ));
            }
        }
        Ok(())
    }

    fn finish(self, profile: super::GuestProfile) -> Result<FixedGuestContract, String> {
        self.check_math_abi()?;
        let linked: Vec<&str> = [crate::LINKED_DISPATCH_EXPORT]
            .into_iter()
            .chain(LINKED_PLAN_SLOTS)
            .collect();
        let (required, forbidden) = match profile {
            super::GuestProfile::Native => (vec!["eval_assignments"], linked),
            super::GuestProfile::Scalar => {
                (Vec::new(), [vec!["eval_assignments"], linked].concat())
            }
            super::GuestProfile::ScalarLinked => (linked, vec!["eval_assignments"]),
        };
        if let Some(slot) = forbidden
            .into_iter()
            .find(|slot| self.slots.contains(*slot))
        {
            return Err(if slot == "eval_assignments" {
                "scalar fixed guest imports the native model entry eval_assignments".into()
            } else {
                format!("{} fixed guest imports {slot}", profile.tag())
            });
        }
        for slot in required.into_iter().chain([
            "rumoca_source_capsule_len",
            "rumoca_source_capsule_copy",
            "rumoca_publication_token_len",
            "rumoca_publication_token_copy",
        ]) {
            if !self.slots.contains(slot) {
                return Err(format!(
                    "fixed guest has no actual linked receiver import {slot}"
                ));
            }
        }
        if !self.public_memory {
            return Err("fixed guest has no linked public memory import".into());
        }
        let allocation = self
            .allocation
            .ok_or("fixed guest has no actual dylink allocation")?;
        Ok(FixedGuestContract {
            imports: self.imports,
            allocation,
        })
    }
}

fn check_reserved_export(name: &str) -> Result<(), String> {
    if matches!(
        name,
        "eval_assignments"
            | "rumoca_source_capsule_len"
            | "rumoca_source_capsule_copy"
            | "rumoca_publication_token_len"
            | "rumoca_publication_token_copy"
    ) || name == crate::LINKED_DISPATCH_EXPORT
        || LINKED_PLAN_SLOTS.contains(&name)
    {
        return Err(format!("fixed guest exports reserved provider slot {name}"));
    }
    Ok(())
}

/// `rumoca_dispatch`: kind, index, four key words, then the entry arguments
/// `a0 a1 t a3 a4 a5 a6 a7 a8` (the linked dispatch module's one export).
const LINKED_DISPATCH_PARAMETERS: [ValType; 15] = {
    use ValType::{F64, I32, I64};
    [
        I32, I32, I64, I64, I64, I64, I32, I32, F64, I32, I32, I32, I32, I32, I32,
    ]
};

fn check_slot(name: &str, ty: &FuncType) -> Result<(), String> {
    use ValType::{F64, I32};
    let parameters: &[ValType] = match name {
        "eval_assignments" => &[I32, I32, F64, I32, I32],
        "rumoca_source_capsule_len" | "rumoca_publication_token_len" | "rumoca_linked_plan_len" => {
            &[]
        }
        "rumoca_source_capsule_copy"
        | "rumoca_publication_token_copy"
        | "rumoca_linked_plan_copy" => &[I32, I32],
        _ if name == crate::LINKED_DISPATCH_EXPORT => &LINKED_DISPATCH_PARAMETERS,
        _ => {
            return Err(format!(
                "fixed guest has unsupported internal function {name}"
            ));
        }
    };
    if ty.params() != parameters || ty.results() != [I32] {
        return Err(format!(
            "fixed guest has a different internal ABI for {name}"
        ));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixed_core_refuses_an_actual_reserved_provider_export() {
        use wasm_encoder::{
            CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
            TypeSection,
        };
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types.ty().function([], []);
        module.section(&types);
        let mut functions = FunctionSection::new();
        functions.function(0);
        module.section(&functions);
        let mut exports = ExportSection::new();
        exports.export("eval_assignments", ExportKind::Func, 0);
        module.section(&exports);
        let mut code = CodeSection::new();
        let mut body = Function::new([]);
        body.instruction(&Instruction::End);
        code.function(&body);
        module.section(&code);
        let error = match check(&module.finish(), super::super::GuestProfile::Native) {
            Ok(_) => panic!("reserved provider must be refused"),
            Err(error) => error,
        };
        assert_eq!(
            error,
            "fixed guest exports reserved provider slot eval_assignments"
        );
    }

    fn math_module(omit: Option<&str>, wrong: Option<&str>) -> Vec<u8> {
        use wasm_encoder::{
            CodeSection, ExportKind, ExportSection, Function, FunctionSection, Instruction, Module,
            TypeSection,
        };
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types
            .ty()
            .function([wasm_encoder::ValType::F64], [wasm_encoder::ValType::F64]);
        types.ty().function(
            [wasm_encoder::ValType::F64; 2],
            [wasm_encoder::ValType::F64],
        );
        module.section(&types);
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();
        let mut code = CodeSection::new();
        for (index, import) in MathImport::ALL.into_iter().enumerate() {
            let symbol = import.symbol();
            let binary = import.is_binary() != (wrong == Some(symbol));
            functions.function(u32::from(binary));
            if omit != Some(symbol) {
                exports.export(symbol, ExportKind::Func, index as u32);
            }
            let mut body = Function::new([]);
            body.instruction(&Instruction::F64Const(0.0.into()));
            body.instruction(&Instruction::End);
            code.function(&body);
        }
        module.section(&functions);
        module.section(&exports);
        module.section(&code);
        module.finish()
    }

    fn math_state(bytes: &[u8]) -> State {
        let mut state = State::default();
        for payload in Parser::new(0).parse_all(bytes) {
            state.read(payload.unwrap()).unwrap();
        }
        state
    }

    #[test]
    fn fixed_core_must_export_the_complete_typed_math_abi() {
        math_state(&math_module(None, None))
            .check_math_abi()
            .unwrap();
        let missing = math_state(&math_module(Some("atan2"), None))
            .check_math_abi()
            .unwrap_err();
        assert_eq!(missing, "fixed guest does not export math ABI symbol atan2");
        let wrong = math_state(&math_module(None, Some("sin")))
            .check_math_abi()
            .unwrap_err();
        assert_eq!(
            wrong,
            "fixed guest math ABI symbol sin differs from the emitted signature"
        );
    }

    /// The native core must import the native model entry; the scalar core
    /// must not. Both import exactly the four source slots.
    #[test]
    fn the_model_entry_slot_is_exact_per_product_kind() {
        use super::super::GuestProfile::{Native, Scalar};
        let state = |model_entry: bool| {
            let mut state = math_state(&math_module(None, None));
            state.slots.extend(
                [
                    "rumoca_source_capsule_len",
                    "rumoca_source_capsule_copy",
                    "rumoca_publication_token_len",
                    "rumoca_publication_token_copy",
                ]
                .map(String::from),
            );
            if model_entry {
                state.slots.insert("eval_assignments".into());
            }
            state.public_memory = true;
            state.allocation = Some(MemInfo {
                memory_size: 0,
                memory_alignment: 0,
                table_size: 0,
                table_alignment: 0,
            });
            state
        };
        assert!(state(true).finish(Native).is_ok());
        assert!(state(false).finish(Scalar).is_ok());
        assert_eq!(
            state(false).finish(Native).err().unwrap(),
            "fixed guest has no actual linked receiver import eval_assignments"
        );
        assert_eq!(
            state(true).finish(Scalar).err().unwrap(),
            "scalar fixed guest imports the native model entry eval_assignments"
        );
    }

    /// Only the scalar-linked core imports the linked dispatch and plan slots,
    /// and it must import all three; no other product kind may import one.
    #[test]
    fn the_linked_slots_are_exact_per_product_kind() {
        use super::super::GuestProfile::{Native, Scalar, ScalarLinked};
        let state = |model_entry: bool, linked: &[&str]| {
            let mut state = math_state(&math_module(None, None));
            state.slots.extend(
                [
                    "rumoca_source_capsule_len",
                    "rumoca_source_capsule_copy",
                    "rumoca_publication_token_len",
                    "rumoca_publication_token_copy",
                ]
                .map(String::from),
            );
            if model_entry {
                state.slots.insert("eval_assignments".into());
            }
            state
                .slots
                .extend(linked.iter().map(|slot| slot.to_string()));
            state.public_memory = true;
            state.allocation = Some(MemInfo {
                memory_size: 0,
                memory_alignment: 0,
                table_size: 0,
                table_alignment: 0,
            });
            state
        };
        let all = [
            crate::LINKED_DISPATCH_EXPORT,
            "rumoca_linked_plan_len",
            "rumoca_linked_plan_copy",
        ];
        assert!(state(false, &all).finish(ScalarLinked).is_ok());
        for missing in all {
            let partial: Vec<&str> = all.into_iter().filter(|slot| *slot != missing).collect();
            assert_eq!(
                state(false, &partial).finish(ScalarLinked).err().unwrap(),
                format!("fixed guest has no actual linked receiver import {missing}")
            );
            assert_eq!(
                state(false, &[missing]).finish(Scalar).err().unwrap(),
                format!("scalar fixed guest imports {missing}")
            );
            assert_eq!(
                state(true, &[missing]).finish(Native).err().unwrap(),
                format!("native fixed guest imports {missing}")
            );
        }
        assert_eq!(
            state(true, &all).finish(ScalarLinked).err().unwrap(),
            "scalar fixed guest imports the native model entry eval_assignments"
        );
    }

    #[test]
    fn the_linked_slots_have_their_exact_internal_abi() {
        use ValType::{F64, I32, I64};
        let ty = |params: &[ValType]| FuncType::new(params.to_vec(), [I32]);
        assert!(
            check_slot(
                crate::LINKED_DISPATCH_EXPORT,
                &ty(&LINKED_DISPATCH_PARAMETERS)
            )
            .is_ok()
        );
        assert!(check_slot("rumoca_linked_plan_len", &ty(&[])).is_ok());
        assert!(check_slot("rumoca_linked_plan_copy", &ty(&[I32, I32])).is_ok());
        let short = &LINKED_DISPATCH_PARAMETERS[..14];
        assert!(check_slot(crate::LINKED_DISPATCH_EXPORT, &ty(short)).is_err());
        assert!(check_slot("rumoca_linked_plan_copy", &ty(&[I32, I64])).is_err());
        assert!(check_slot("rumoca_linked_plan_len", &ty(&[F64])).is_err());
        for name in [
            crate::LINKED_DISPATCH_EXPORT,
            "rumoca_linked_plan_len",
            "rumoca_linked_plan_copy",
        ] {
            assert!(check_reserved_export(name).is_err());
        }
    }

    #[test]
    fn fixed_core_parser_cannot_admit_an_empty_or_component_receiver() {
        let native = super::super::GuestProfile::Native;
        assert!(check(b"not wasm", native).is_err());
        assert!(check(&wasm_encoder::Component::new().finish(), native).is_err());
        assert!(check(&wasm_encoder::Module::new().finish(), native).is_err());
    }

    #[test]
    fn fixed_core_slot_preserves_the_fifth_typed_pointer_and_bounded_capsule_copy() {
        use ValType::{F64, I32};
        check_slot(
            "eval_assignments",
            &FuncType::new([I32, I32, F64, I32, I32], [I32]),
        )
        .unwrap();
        assert!(
            check_slot(
                "eval_assignments",
                &FuncType::new([I32, I32, F64, I32], [I32])
            )
            .is_err()
        );
        for name in [
            "rumoca_source_capsule_copy",
            "rumoca_publication_token_copy",
        ] {
            check_slot(name, &FuncType::new([I32, I32], [I32])).unwrap();
            let error = check_slot(name, &FuncType::new([I32], [I32])).unwrap_err();
            assert!(error.contains("different internal ABI") && error.contains(name));
        }
        check_slot("rumoca_publication_token_len", &FuncType::new([], [I32])).unwrap();
        assert!(check_slot("foreign", &FuncType::new([], [I32])).is_err());
    }

    fn address_import(
        module: &str,
        name: &str,
        ty: ValType,
        mutable: bool,
        shared: bool,
    ) -> Result<(), String> {
        State::default().import(wasmparser::Import {
            module,
            name,
            ty: TypeRef::Global(wasmparser::GlobalType {
                content_type: ty,
                mutable,
                shared,
            }),
        })
    }

    #[test]
    fn linker_heap_addresses_have_exact_internal_provider_contract() {
        for name in ["__heap_base", "__heap_end"] {
            assert!(address_import("GOT.mem", name, ValType::I32, true, false).is_ok());
            for (ty, mutable, shared) in [
                (ValType::I64, true, false),
                (ValType::I32, false, false),
                (ValType::I32, true, true),
            ] {
                assert!(address_import("GOT.mem", name, ty, mutable, shared).is_err());
            }
            assert!(address_import("GOT.func", name, ValType::I32, true, false).is_err());
            assert!(address_import("platform", name, ValType::I32, true, false).is_err());
        }
        for name in [
            "_CLOCK_THREAD_CPUTIME_ID",
            "_CLOCK_PROCESS_CPUTIME_ID",
            "foreign",
        ] {
            assert!(address_import("GOT.mem", name, ValType::I32, true, false).is_err());
        }
        let mut state = State::default();
        let imported = wasmparser::Import {
            module: "GOT.mem",
            name: "__heap_base",
            ty: TypeRef::Global(wasmparser::GlobalType {
                content_type: ValType::I32,
                mutable: true,
                shared: false,
            }),
        };
        state.import(imported).unwrap();
        assert!(matches!(
            state.imports.get(&("GOT.mem".into(), "__heap_base".into())),
            Some(FixedImportRole::Internal)
        ));
        assert!(state.import(imported).is_err());
        assert!(
            state.finish(super::super::GuestProfile::Native).is_err(),
            "heap imports cannot replace receiver slots"
        );
    }
}
