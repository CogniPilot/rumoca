//! The one static entry a guest without a JavaScript engine reaches every
//! issued entry through. It writes no memory: an absent or different identity
//! returns the refusal before any entry runs, and an admitted call returns the
//! entry's own status unchanged.
use super::{
    LINKED_DISPATCH_EXPORT, LINKED_DISPATCH_REFUSED, LinkedEntryId, LinkedEntryKind,
    LinkedEntryPlan,
};
use wasm_encoder::{
    BlockType, CodeSection, EntityType, ExportKind, ExportSection, Function, FunctionSection,
    ImportSection, Instruction, Module, TypeSection, ValType,
};

// Parameters of `rumoca_dispatch`: kind, index, four key words, then the
// superset of entry arguments `a0 a1 t a3 a4 a5 a6 a7 a8`.
const KIND: u32 = 0;
const INDEX: u32 = 1;
const KEY: u32 = 2;
const ARGUMENTS: u32 = 6;
const COMPLETE_ARGUMENTS: u32 = 9;
const KERNEL_ARGUMENTS: u32 = 5;
const COMPLETE_TYPE: u32 = 0;
const KERNEL_TYPE: u32 = 1;
const DISPATCH_TYPE: u32 = 2;

/// `(i32 i32 f64 i32...)`: the convention every entry starts with.
fn entry_parameters(count: u32) -> Vec<ValType> {
    [ValType::I32, ValType::I32, ValType::F64]
        .into_iter()
        .chain((3..count).map(|_| ValType::I32))
        .collect()
}

/// The refusal as the i32 the entry ABI returns.
fn refuse(function: &mut Function) {
    function.instruction(&Instruction::I32Const(LINKED_DISPATCH_REFUSED as i32));
    function.instruction(&Instruction::Return);
}

/// Jump on local `selector` to the arm of its value. A value without an arm,
/// or past the last, refuses; every arm returns.
fn switch(
    function: &mut Function,
    selector: u32,
    present: &[bool],
    mut arm: impl FnMut(&mut Function, usize),
) {
    let refusal = u32::try_from(present.len()).expect("dispatch arms are counted in u32");
    function.instruction(&Instruction::Block(BlockType::Empty));
    for _ in present {
        function.instruction(&Instruction::Block(BlockType::Empty));
    }
    function.instruction(&Instruction::LocalGet(selector));
    let targets = (0..refusal)
        .zip(present)
        .map(|(depth, present)| if *present { depth } else { refusal })
        .collect::<Vec<_>>();
    function.instruction(&Instruction::BrTable(targets.into(), refusal));
    for (value, present) in present.iter().enumerate() {
        function.instruction(&Instruction::End);
        if *present {
            arm(function, value);
        } else {
            refuse(function);
        }
    }
    function.instruction(&Instruction::End);
    refuse(function);
}

/// Jump on the index to an entry of kind `code`. Only issued indices have
/// an arm: a kinematic entry has its derivative's index, so its indices may
/// have holes, and a hole refuses like an index past the last.
fn kind_arm(function: &mut Function, entries: &[(LinkedEntryId, [u64; 4])], code: usize) {
    let of_kind = (0u32..)
        .zip(entries)
        .filter(|(_, (id, _))| id.kind.code() as usize == code)
        .collect::<Vec<_>>();
    let width = of_kind
        .iter()
        .map(|(_, (id, _))| id.index as usize + 1)
        .max()
        .unwrap_or(0);
    let mut present = vec![false; width];
    for (_, (id, _)) in &of_kind {
        present[id.index as usize] = true;
    }
    switch(function, INDEX, &present, |function, index| {
        let (call, (id, key)) = of_kind
            .iter()
            .find(|(_, (id, _))| id.index as usize == index)
            .expect("present marks only issued indices");
        entry_arm(function, *call, *id, key);
    });
}

/// Refuse unless all four key words are the issued key, then call the entry
/// with exactly its own arguments and return its status unchanged.
fn entry_arm(function: &mut Function, call: u32, id: LinkedEntryId, key: &[u64; 4]) {
    for (word, value) in (KEY..).zip(key) {
        function.instruction(&Instruction::LocalGet(word));
        function.instruction(&Instruction::I64Const(*value as i64));
        function.instruction(&Instruction::I64Ne);
        if word != KEY {
            function.instruction(&Instruction::I32Or);
        }
    }
    function.instruction(&Instruction::If(BlockType::Empty));
    refuse(function);
    function.instruction(&Instruction::End);
    let count = if id.kind.complete_derivative() {
        COMPLETE_ARGUMENTS
    } else {
        KERNEL_ARGUMENTS
    };
    for argument in ARGUMENTS..ARGUMENTS + count {
        function.instruction(&Instruction::LocalGet(argument));
    }
    function.instruction(&Instruction::Call(call));
    function.instruction(&Instruction::Return);
}

impl LinkedEntryPlan {
    /// The dispatch module: it imports every issued entry by its symbol with
    /// that entry's own type, and exports only `rumoca_dispatch`.
    pub fn dispatch(&self) -> Vec<u8> {
        let entries = self.entries().collect::<Vec<_>>();
        let mut module = Module::new();
        let mut types = TypeSection::new();
        types
            .ty()
            .function(entry_parameters(COMPLETE_ARGUMENTS), [ValType::I32]);
        types
            .ty()
            .function(entry_parameters(KERNEL_ARGUMENTS), [ValType::I32]);
        let dispatch_parameters = [ValType::I32, ValType::I32]
            .into_iter()
            .chain([ValType::I64; 4])
            .chain(entry_parameters(COMPLETE_ARGUMENTS))
            .collect::<Vec<_>>();
        types.ty().function(dispatch_parameters, [ValType::I32]);
        module.section(&types);
        let mut imports = ImportSection::new();
        for (id, _) in &entries {
            let ty = if id.kind.complete_derivative() {
                COMPLETE_TYPE
            } else {
                KERNEL_TYPE
            };
            imports.import("env", &id.symbol(), EntityType::Function(ty));
        }
        module.section(&imports);
        let mut functions = FunctionSection::new();
        functions.function(DISPATCH_TYPE);
        module.section(&functions);
        let dispatch = u32::try_from(entries.len()).expect("issued entries are indexed in u32");
        let mut exports = ExportSection::new();
        exports.export(LINKED_DISPATCH_EXPORT, ExportKind::Func, dispatch);
        module.section(&exports);
        let mut function = Function::new([]);
        let kinds = (0..=LinkedEntryKind::ALL.len())
            .map(|code| {
                entries
                    .iter()
                    .any(|(id, _)| id.kind.code() as usize == code)
            })
            .collect::<Vec<_>>();
        switch(&mut function, KIND, &kinds, |function, code| {
            kind_arm(function, &entries, code);
        });
        function.instruction(&Instruction::End);
        let mut code = CodeSection::new();
        code.function(&function);
        module.section(&code);
        module.finish()
    }
}
