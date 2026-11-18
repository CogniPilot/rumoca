//! One parameter-static cache shared by every refresh family.
//!
//! Separate numeric and presence buffers permit a complete compiled operation
//! to borrow the same cache without assuming Rust's `Option<f64>` layout.
#[cfg(test)]
mod tests;

#[derive(Clone, Default)]
pub(super) struct StaticRefreshCache {
    pub(super) valid: bool,
    pub(super) params: Vec<f64>,
    pub(super) values: StaticRefreshValues,
}

#[derive(Clone, Default)]
pub(super) struct StaticRefreshValues {
    numbers: Vec<f64>,
    present: Vec<u8>,
}

impl StaticRefreshValues {
    pub(super) fn reset(&mut self, extent: usize) {
        self.numbers.clear();
        self.numbers.resize(extent, 0.0);
        self.present.clear();
        self.present.resize(extent, 0);
    }

    pub(super) fn get(&self, index: usize) -> Option<f64> {
        if self.present.get(index).copied()? == 0 {
            return None;
        }
        self.numbers.get(index).copied()
    }

    pub(super) fn set(&mut self, index: usize, value: f64) {
        self.numbers[index] = value;
        self.present[index] = 1;
    }
}

impl StaticRefreshCache {
    pub(super) fn prepare_operation_storage(
        &mut self,
        extent: usize,
        keys: usize,
    ) -> Result<(), String> {
        self.params
            .try_reserve_exact(keys)
            .map_err(|error| error.to_string())?;
        self.values
            .numbers
            .try_reserve_exact(extent)
            .map_err(|error| error.to_string())?;
        self.values
            .present
            .try_reserve_exact(extent)
            .map_err(|error| error.to_string())?;
        self.params.resize(keys, 0.0);
        self.values.numbers.resize(extent, 0.0);
        self.values.present.resize(extent, 0);
        Ok(())
    }

    pub(super) fn operation_view(&mut self) -> super::SolveDerivativeCacheView<'_> {
        super::SolveDerivativeCacheView {
            valid: &mut self.valid,
            key_parameters: &mut self.params,
            values: &mut self.values.numbers,
            present: &mut self.values.present,
        }
    }
}
