//! A private-constructor memory32 control program; targets only print it.
use super::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Helper {
    Static,
    Dynamic,
    Values,
    /// The values restricted to the kinematic rows.
    Kinematic,
}

/// Primitive target operations. No public raw-program constructor exists.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Instruction {
    I32Const(i32),
    I64Const(i64),
    F64Const(u64),
    LocalGet(u32),
    LocalSet(u32),
    I32Add,
    I32Mul,
    I32Eqz,
    I32GeU,
    I32GtU,
    I32And,
    I64Add,
    I64ExtendI32U,
    I64Shl,
    I64GtU,
    I64Ne,
    F64Abs,
    F64Le,
    I32Store,
    I32Load8U,
    I64Load,
    F64Load,
    MemorySize,
    MemoryCopy,
    MemoryFill,
    Block,
    Loop,
    If,
    Else,
    End,
    BrIf(u32),
    Br(u32),
    Return,
    Call(Helper),
}

/// Complete original-root control. A caller cannot change branches, storage,
/// cache domain, fault order or rollback by constructing an instruction list.
///
/// ```compile_fail
/// use rumoca_ir_solve::fmi::{FmiDerivativeControlPlan,FmiDerivativeWorkspace};
/// fn forge(workspace:FmiDerivativeWorkspace)->FmiDerivativeControlPlan {
///     FmiDerivativeControlPlan { workspace, instructions:Vec::new().into(), encoded_byte_bound:0 }
/// }
/// ```
#[derive(Debug)]
pub struct FmiDerivativeControlPlan {
    workspace: FmiDerivativeWorkspace,
    instructions: Box<[Instruction]>,
    kinematic: Option<Box<[Instruction]>>,
    encoded_byte_bound: u32,
}
impl FmiDerivativeCausalOperation<'_> {
    pub fn control_plan(
        &self,
        helper_bytes: impl IntoIterator<Item = u32>,
        capacity: crate::CheckedNativeWasmCapacityPolicy,
    ) -> Result<FmiDerivativeControlPlan, FmiDerivativeCausalRefusal> {
        let encoded_byte_bound = encoded_bound(
            self.static_targets().intervals().count(),
            self.parameter_runs().len(),
            self.kinematic()
                .map_or(0, |kinematic| kinematic.rows().intervals().count()),
        )?;
        let workspace = self.workspace(helper_bytes, capacity)?;
        let instructions = super::control_builder::instructions(self, workspace).into();
        let kinematic = self
            .kinematic()
            .map(|kinematic| super::control_builder::kinematic(kinematic, workspace).into());
        Ok(FmiDerivativeControlPlan {
            workspace,
            instructions,
            kinematic,
            encoded_byte_bound: encoded_byte_bound as u32,
        })
    }
}
impl FmiDerivativeControlPlan {
    pub fn encoded_byte_bound(&self) -> u32 {
        self.encoded_byte_bound
    }
    pub fn workspace(&self) -> FmiDerivativeWorkspace {
        self.workspace
    }
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }
    pub fn i32_locals(&self) -> u32 {
        4
    }
    /// The kinematic entry `(y, p, t, out, work) -> status`, when issued. It
    /// shares the workspace and its status words with the complete entry and
    /// never writes `y`.
    pub fn kinematic_instructions(&self) -> Option<&[Instruction]> {
        self.kinematic.as_deref()
    }
    pub fn kinematic_i32_locals(&self) -> u32 {
        2
    }
}

fn encoded_bound(
    intervals: usize,
    key_runs: usize,
    kinematic_runs: usize,
) -> Result<usize, FmiDerivativeCausalRefusal> {
    // Conservative encoded bound, checked before instruction allocation.
    // A memory32 instruction prints in at most sixteen bytes; the fixed
    // module/type/import/export envelope is bounded by 512 bytes.
    let encoded_byte_bound = intervals
        .checked_mul(240)
        .and_then(|n| key_runs.checked_mul(180).and_then(|k| n.checked_add(k)))
        .and_then(|n| {
            kinematic_runs
                .checked_mul(60)
                .and_then(|k| n.checked_add(k))
        })
        .and_then(|n| n.checked_add(4000))
        .and_then(|n| n.checked_mul(16))
        .and_then(|n| n.checked_add(512))
        .filter(|&n| n <= 8 * 1024 * 1024)
        .ok_or_else(|| {
            FmiDerivativeCausalRefusal::Storage(
                "derivative control exceeds synchronous memory32 code budget".into(),
            )
        })?;
    Ok(encoded_byte_bound)
}

#[cfg(test)]
mod tests {
    #[test]
    fn complete_control_print_budget_refuses_large_and_overflowing_sources() {
        assert!(super::encoded_bound(0, 0, 0).unwrap() < 8 * 1024 * 1024);
        assert!(super::encoded_bound(10000, 0, 0).is_err());
        assert!(super::encoded_bound(0, usize::MAX, 0).is_err());
        assert!(super::encoded_bound(usize::MAX, 0, 0).is_err());
    }
}
