//! Numerical matrices for one exact point of the runtime's issued block catalog.

use super::*;
use crate::runtime::projection::{AlgebraicProjectionArgs, SeedBlockLinearization};
use std::cell::RefMut;

#[derive(Default)]
pub(super) struct SeedProjectionCache {
    time_bits: Option<u64>,
    y_bits: Vec<u64>,
    p_bits: Vec<u64>,
    blocks: Vec<Option<Rc<SeedBlockLinearization>>>,
}

impl Clone for SeedProjectionCache {
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl SeedProjectionCache {
    pub(super) fn at_point<'a>(
        cache: &'a RefCell<Self>,
        lin: AlgebraicLinearization<'_>,
        y: &[f64],
        block_count: usize,
    ) -> RefMut<'a, Self> {
        let mut point = cache.borrow_mut();
        let matches = point.time_bits == Some(lin.t.to_bits())
            && point
                .y_bits
                .iter()
                .copied()
                .eq(y.iter().map(|v| v.to_bits()))
            && point
                .p_bits
                .iter()
                .copied()
                .eq(lin.params.iter().map(|v| v.to_bits()));
        if !matches {
            point.time_bits = Some(lin.t.to_bits());
            point.y_bits.clear();
            point.y_bits.extend(y.iter().map(|v| v.to_bits()));
            point.p_bits.clear();
            point.p_bits.extend(lin.params.iter().map(|v| v.to_bits()));
            point.clear();
            point.blocks.resize_with(block_count, || None);
        }
        point
    }

    pub(super) fn clear(&mut self) {
        self.blocks.fill(None);
    }
}

impl RefreshProjectionModel<'_> {
    pub(super) fn seed_block_linearization(
        &self,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        y: &[f64],
        args: AlgebraicProjectionArgs<'_>,
    ) -> Result<Rc<SeedBlockLinearization>, RuntimeSolveError> {
        let issued_index = self.block_indices.get(block_index).copied().filter(|_| {
            self.algebraic_projection_block_structure(block_index)
                .is_some_and(solve::JacobianStructure::linearization_is_repeatable)
        });
        if let (Some(index), Some(cache)) = (issued_index, &self.seed_linearizations) {
            let mut cache = cache.borrow_mut();
            let entry = cache.blocks.get_mut(index).ok_or_else(|| {
                RuntimeSolveError::solve_ir("seed linearization has no issued block slot")
            })?;
            if let Some(linearization) = entry {
                return Ok(linearization.clone());
            }
            let linearization = Rc::new(SeedBlockLinearization::build(
                self,
                block_index,
                block,
                y,
                args,
            )?);
            *entry = Some(linearization.clone());
            return Ok(linearization);
        }
        SeedBlockLinearization::build(self, block_index, block, y, args).map(Rc::new)
    }
}
