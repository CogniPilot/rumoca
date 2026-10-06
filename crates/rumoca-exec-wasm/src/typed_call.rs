//! Review-only typed pure-call backend. This is not a model assignment profile.
mod calls;
mod control;
mod emit;
mod layout;
mod link;
mod math;
mod matrix;
mod numbers;
pub(crate) mod program;
mod tensor;
mod views;

use rumoca_core::Span;
use rumoca_ir_solve as solve;

/// Positive return codes index this source-bound fault inventory; zero is success.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypedCallFaultKind {
    InvalidBuffer,
    InvalidInput,
    IntegerArithmetic,
    IntegerConversion,
    IndexBounds,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedCallFault {
    pub status: u32,
    pub owner: solve::SolvePureCallOwnerId,
    pub operation: Option<usize>,
    /// Enclosing (operation ordinal, branch ordinal); Fold transition is zero.
    pub region_path: Vec<(usize, usize)>,
    pub opcode: &'static str,
    pub kind: TypedCallFaultKind,
    pub provenance: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedCallCompileError {
    SiteMismatch,
    UnsupportedArithmetic,
    UnsupportedRecursiveGroup {
        owner: solve::SolvePureCallOwnerId,
        provenance: Span,
    },
    SizeLimit,
    UnsupportedOutputKind {
        owner: solve::SolvePureCallOwnerId,
        output: usize,
        kind: solve::SolvePureCallOutputKind,
        provenance: Span,
    },
    UnsupportedOperation {
        owner: solve::SolvePureCallOwnerId,
        operation: usize,
        region_path: Vec<(usize, usize)>,
        opcode: &'static str,
        provenance: Span,
    },
}

impl std::fmt::Display for TypedCallCompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "typed-call WASM compilation: {self:?}")
    }
}

impl std::error::Error for TypedCallCompileError {}

/// All pointers address disjoint, aligned eight-byte-cell spans in env.memory.
/// Input/output tuples retain the checked owner's declared value order and types.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedCallLayout {
    pub input_bytes: u32,
    pub output_bytes: u32,
    pub scratch_bytes: u32,
    pub inputs: Vec<solve::SolveValueType>,
    pub outputs: Vec<solve::SolvePureCallOutput>,
}

/// Bytes-only compilation never instantiates against the compiler's own memory.
#[derive(Debug)]
pub struct CompiledTypedCallWasm {
    module_bytes: Vec<u8>,
    layout: TypedCallLayout,
    faults: Vec<TypedCallFault>,
    math_imports: Vec<&'static str>,
}

impl CompiledTypedCallWasm {
    #[must_use]
    pub fn module_bytes(&self) -> &[u8] {
        &self.module_bytes
    }
    #[must_use]
    pub fn layout(&self) -> &TypedCallLayout {
        &self.layout
    }
    #[must_use]
    pub fn faults(&self) -> &[TypedCallFault] {
        &self.faults
    }
    /// Target math imports under `env`, in addition to the memory import.
    /// Bind `pow` to the target's Binary64 exponentiation intrinsic.
    #[must_use]
    pub fn math_imports(&self) -> &[&'static str] {
        &self.math_imports
    }
}

/// Compile one exact issued site, with atomic publication of its complete tuple.
/// Structured branches are lazy; checked finite folds snapshot carried tuples.
/// Carried tensors and calls use private whole-tuple snapshots.
/// Assertion predicate interfaces are refused until checked fatal/warning
/// continuation and pre-publication failure handling are issued by the producer.
pub fn compile_pure_call_wasm(
    table: &solve::SolvePureCallTable,
    site: &solve::SolvePureCallSite,
) -> Result<CompiledTypedCallWasm, TypedCallCompileError> {
    if !table.matches_site(site) {
        return Err(TypedCallCompileError::SiteMismatch);
    }
    if table.arithmetic().real_format() != solve::SolveRealFormat::Binary64 {
        return Err(TypedCallCompileError::UnsupportedArithmetic);
    }
    let owner = table
        .owner(site.owner())
        .ok_or(TypedCallCompileError::SiteMismatch)?;
    let linked = link::LinkedOwners::construct(table, owner.id())?;
    let plan = linked.plan(owner.id())?;
    let layout = TypedCallLayout {
        input_bytes: plan.input_bytes,
        output_bytes: plan.output_bytes,
        scratch_bytes: plan.scratch_bytes,
        inputs: owner.inputs().to_vec(),
        outputs: owner.outputs().to_vec(),
    };
    let (module_bytes, faults) = emit::compile(table, &linked, owner.id())?;
    Ok(CompiledTypedCallWasm {
        module_bytes,
        layout,
        faults,
        math_imports: linked
            .math_imports
            .iter()
            .map(|import| import.symbol())
            .collect(),
    })
}
