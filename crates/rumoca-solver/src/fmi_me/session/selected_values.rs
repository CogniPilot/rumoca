//! Reading a few named values without materializing the whole visible map.

use rustc_hash::FxHashMap;

use super::{MeSessionError, MeSimulationSession, host_state::MeHostState};

/// Declared output positions by name, in the order `output_values` returns.
///
/// Built at most once per session: the inventory is the model's visible
/// projection, which no restart, reset or chart switch replaces. The map is
/// only ever looked up, never iterated.
pub(super) struct OutputIndex {
    declared: usize,
    positions: FxHashMap<String, usize>,
}

impl OutputIndex {
    /// A repeated name keeps its last position, as the keyed map does.
    fn new(names: &[String]) -> Self {
        Self {
            declared: names.len(),
            positions: names
                .iter()
                .enumerate()
                .map(|(position, name)| (name.clone(), position))
                .collect(),
        }
    }

    fn value(&self, name: &str, values: &[f64]) -> Option<f64> {
        let position = *self.positions.get(name)?;
        values.get(position).copied()
    }
}

/// The component answered with a different number of values than it declares.
pub(super) fn output_arity_mismatch(returned: usize, declared: usize) -> MeSessionError {
    MeSessionError::Contract {
        reason: format!(
            "the component returned {returned} output values for {declared} declared outputs"
        ),
    }
}

impl MeHostState {
    /// The output index, built from the report `output_names` reads.
    ///
    /// The kernel is borrowed on every call, so a kernel that is not active
    /// refuses a selected read exactly as it refuses `output_names`.
    fn output_index(&self) -> Result<&OutputIndex, MeSessionError> {
        let kernel = self.kernel.borrow()?;
        Ok(self
            .output_index
            .get_or_init(|| OutputIndex::new(kernel.model_description().output_names)))
    }
}

impl MeSimulationSession<'_, '_> {
    /// Current visible values of `names` in request order, repeats included;
    /// `None` for a name that is neither an applied input nor an output.
    ///
    /// Each entry is what [`Self::visible_values`] holds under that name, read
    /// from one batched output observation and no keyed map.
    pub fn selected_values<N: AsRef<str>>(
        &self,
        names: &[N],
    ) -> Result<Vec<Option<f64>>, MeSessionError> {
        let index = self.host.output_index()?;
        let values = self.output_values()?;
        if index.declared != values.len() {
            return Err(output_arity_mismatch(values.len(), index.declared));
        }
        let inputs = &self.host.inputs;
        Ok(names
            .iter()
            .map(|name| {
                let name = name.as_ref();
                inputs
                    .get(name)
                    .copied()
                    .or_else(|| index.value(name, &values))
            })
            .collect())
    }
}
