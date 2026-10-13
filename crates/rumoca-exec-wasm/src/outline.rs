//! Function-size policy and the one splitter every emitted native function uses.
//!
//! Emitted state lives in memory, so a sequence of complete operations can be
//! cut at any operation boundary into consecutive functions that share the
//! same frame and return a status. Callers encode each operation once and hand
//! its instruction bytes to an [`Outliner`], which packs them under the budget.
use wasm_encoder::{Function, Instruction, ValType};

/// Largest encoded function body, locals included, any native emitter issues.
pub(crate) const FUNCTION_BYTES: usize = 1024 * 1024;
// `i32.const 0` followed by the function's end, which closes every group.
const SUCCESS_BYTES: usize = 3;

/// One indivisible operation encodes to more bytes than a function may hold.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ChunkTooLarge {
    pub bytes: usize,
    pub budget: usize,
}

pub(crate) struct Outliner {
    locals: Vec<(u32, ValType)>,
    prefix: usize,
    budget: usize,
    current: Function,
    finished: Vec<Function>,
}

impl Outliner {
    pub(crate) fn new(locals: Vec<(u32, ValType)>, budget: usize) -> Self {
        let current = Function::new(locals.clone());
        Self {
            prefix: current.byte_len(),
            locals,
            budget,
            current,
            finished: Vec::new(),
        }
    }

    /// The instruction bytes of a function declared with this outliner's locals.
    pub(crate) fn instructions(&self, function: Function) -> Vec<u8> {
        function.into_raw_body()[self.prefix..].to_vec()
    }

    /// Append one complete operation, starting a new function when it would not fit.
    pub(crate) fn push(&mut self, instructions: &[u8]) -> Result<(), ChunkTooLarge> {
        let alone = self
            .prefix
            .checked_add(instructions.len())
            .and_then(|n| n.checked_add(SUCCESS_BYTES));
        if alone.is_none_or(|n| n > self.budget) {
            return Err(ChunkTooLarge {
                bytes: alone.unwrap_or(usize::MAX),
                budget: self.budget,
            });
        }
        let together = self
            .current
            .byte_len()
            .checked_add(instructions.len())
            .and_then(|n| n.checked_add(SUCCESS_BYTES));
        if together.is_none_or(|n| n > self.budget) {
            let full = std::mem::replace(&mut self.current, Function::new(self.locals.clone()));
            self.finished.push(finish(full));
        }
        self.current.raw(instructions.iter().copied());
        Ok(())
    }

    pub(crate) fn finish(mut self) -> Vec<Function> {
        self.finished.push(finish(self.current));
        self.finished
    }
}

fn finish(mut function: Function) -> Function {
    function.instruction(&Instruction::I32Const(0));
    function.instruction(&Instruction::End);
    function
}

#[cfg(test)]
mod tests {
    use super::*;

    fn chunk(outliner: &Outliner, constants: usize) -> Vec<u8> {
        let mut function = Function::new(outliner.locals.clone());
        for _ in 0..constants {
            function.instruction(&Instruction::Nop);
        }
        outliner.instructions(function)
    }

    #[test]
    fn packs_whole_chunks_under_the_budget_and_closes_each_group() {
        let mut outliner = Outliner::new(vec![(1, ValType::I32)], 24);
        let chunk = chunk(&outliner, 6);
        for _ in 0..5 {
            outliner.push(&chunk).unwrap();
        }
        let groups = outliner.finish();
        assert!(groups.len() > 1);
        assert!(groups.iter().all(|group| group.byte_len() <= 24));
    }

    #[test]
    fn refuses_a_chunk_that_cannot_fit_alone() {
        let mut outliner = Outliner::new(Vec::new(), 8);
        let big = chunk(&outliner, 9);
        let error = outliner.push(&big).unwrap_err();
        assert_eq!(error.budget, 8);
        assert!(error.bytes > 8);
    }
}
