//! Internal browser storage mode; portable artifacts retain defined memory.
use super::*;
use wasm_encoder::GlobalType;

#[derive(Clone, Copy, PartialEq, Eq)]
pub(in crate::emit) enum ArenaStorage {
    Defined,
    Pooled,
}

impl ArenaStorage {
    pub(in crate::emit) const fn runtime() -> Self {
        if cfg!(target_arch = "wasm32") {
            Self::Pooled
        } else {
            Self::Defined
        }
    }
}

impl ArenaPlan {
    pub(in crate::emit) fn with_storage(mut self, storage: ArenaStorage) -> Self {
        self.storage = storage;
        self
    }

    pub(in crate::emit) fn pooled_bytes(self) -> Option<u32> {
        (self.storage == ArenaStorage::Pooled).then(|| self.region_bytes())
    }

    pub(in crate::emit) fn region_bytes(self) -> u32 {
        // required() already proves the register byte count is at most 64 MiB.
        ((self.registers * 8).div_ceil(65536).max(1) * 65536) as u32
    }

    pub(in crate::emit) fn add_imports(self, imports: &mut ImportSection) {
        if self.storage != ArenaStorage::Pooled {
            return;
        }
        imports.import(
            "env",
            "rumoca_private_arena",
            EntityType::Memory(MemoryType {
                minimum: u64::from(self.region_bytes() / 65536),
                maximum: None,
                memory64: false,
                shared: false,
                page_size_log2: None,
            }),
        );
        imports.import(
            "env",
            "rumoca_private_arena_base",
            EntityType::Global(GlobalType {
                val_type: ValType::I32,
                mutable: false,
                shared: false,
            }),
        );
    }
}

impl BodyEmitter<'_> {
    pub(in crate::emit) fn guard_arena_region(&mut self) -> Result<(), String> {
        let arena = self.arena_plan()?;
        if arena.storage != ArenaStorage::Pooled {
            return Ok(());
        }
        self.push(Instruction::GlobalGet(0));
        self.push(Instruction::I32Const(65535));
        self.push(Instruction::I32And);
        self.return_status_if(1);
        self.push(Instruction::GlobalGet(0));
        self.push(Instruction::I64ExtendI32U);
        self.push(Instruction::I64Const(i64::from(arena.region_bytes())));
        self.push(Instruction::I64Add);
        self.push(Instruction::MemorySize(1));
        self.push(Instruction::I64ExtendI32U);
        self.push(Instruction::I64Const(16));
        self.push(Instruction::I64Shl);
        self.push(Instruction::I64GtU);
        self.return_status_if(1);
        Ok(())
    }

    pub(super) fn relocate_arena_address(&mut self) -> Result<(), String> {
        if self.arena_plan()?.storage == ArenaStorage::Pooled {
            self.push(Instruction::GlobalGet(0));
            self.push(Instruction::I32Add);
        }
        Ok(())
    }
}
