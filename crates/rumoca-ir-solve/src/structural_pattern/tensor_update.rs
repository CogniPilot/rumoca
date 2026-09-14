use super::*;

enum FixedSubscript {
    Whole,
    Index(u32),
}

fn fixed_subscripts(subscripts: &[crate::TensorUpdateSubscript]) -> Option<Vec<FixedSubscript>> {
    subscripts
        .iter()
        .map(|subscript| match subscript {
            crate::TensorUpdateSubscript::Whole => Some(FixedSubscript::Whole),
            crate::TensorUpdateSubscript::Index(crate::TensorIndex::Constant(index)) => {
                Some(FixedSubscript::Index(*index))
            }
            crate::TensorUpdateSubscript::Index(crate::TensorIndex::Runtime(_))
            | crate::TensorUpdateSubscript::Slice { .. } => None,
        })
        .collect()
}

impl DependencyWalk<'_> {
    pub(super) fn tensor_update(
        &mut self,
        dst_start: Reg,
        base_start: Reg,
        value_start: Reg,
        dimensions: &[u32],
        subscripts: &[crate::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<(), StructuralPatternError> {
        let count = saturating_tensor_extent(dimensions);
        let Some(fixed) = fixed_subscripts(subscripts) else {
            let (value_count, selector) =
                self.tensor_update_patch(dimensions, subscripts, lanes)?;
            let patch = self.range(value_start, value_count)?.union(selector);
            for offset in 0..count.saturating_mul(lanes) {
                let dependencies = self.get(base_start + offset as Reg)?.union(patch.clone());
                self.set(dst_start + offset as Reg, dependencies);
            }
            return Ok(());
        };
        for offset in 0..count.saturating_mul(lanes) {
            let source = fixed_update_element(offset / lanes, dimensions, &fixed)
                .map_or(base_start + offset as Reg, |value| {
                    value_start + (value * lanes + offset % lanes) as Reg
                });
            self.copy(dst_start + offset as Reg, source)?;
        }
        Ok(())
    }

    /// Width of the patch value range and the dependencies of the runtime
    /// coordinates that select where the patch lands.
    fn tensor_update_patch(
        &self,
        dimensions: &[u32],
        subscripts: &[crate::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<(usize, DependencyState), StructuralPatternError> {
        let mut value_count = lanes;
        let mut selector = DependencyState::empty();
        for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
            match subscript {
                crate::TensorUpdateSubscript::Whole => {
                    value_count = value_count.saturating_mul(extent as usize);
                }
                crate::TensorUpdateSubscript::Index(crate::TensorIndex::Runtime(register_id)) => {
                    selector = selector.union(self.get(*register_id)?);
                }
                crate::TensorUpdateSubscript::Index(crate::TensorIndex::Constant(_)) => {}
                crate::TensorUpdateSubscript::Slice { start, dimensions } => {
                    let slice_count = saturating_tensor_extent(dimensions);
                    selector = selector.union(self.range(*start, slice_count)?);
                    value_count = value_count.saturating_mul(slice_count);
                }
            }
        }
        Ok((value_count, selector))
    }
}

/// The patch element replacing this base element under fixed subscripts.
/// Whole axes retain their row-major coordinate; indexed axes disappear.
fn fixed_update_element(
    mut element: usize,
    dimensions: &[u32],
    subscripts: &[FixedSubscript],
) -> Option<usize> {
    let mut value = 0;
    let mut stride = 1;
    for (&extent, subscript) in dimensions.iter().zip(subscripts).rev() {
        let coordinate = element % extent as usize;
        element /= extent as usize;
        match subscript {
            FixedSubscript::Whole => {
                value += coordinate * stride;
                stride *= extent as usize;
            }
            FixedSubscript::Index(index) => {
                if coordinate != *index as usize {
                    return None;
                }
            }
        }
    }
    Some(value)
}
