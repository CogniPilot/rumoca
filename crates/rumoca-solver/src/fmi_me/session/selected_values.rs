//! Reading a few named values without evaluating or keying every output.

use std::rc::Rc;

use super::{MeSessionError, MeSimulationSession};
use crate::fmi_me::{MeError, instance::MeKernelCell};

/// What one selected read answers: the requested values, and how many values
/// the full read holds at the same observation.
type SelectedRead = (Vec<Option<f64>>, usize);

/// The component answered with a different number of values than it declares.
pub(super) fn output_arity_mismatch(returned: usize, declared: usize) -> MeSessionError {
    MeSessionError::Contract {
        reason: format!(
            "the component returned {returned} output values for {declared} declared outputs"
        ),
    }
}

/// The selected counterpart of the full output read: one observation, the
/// getter over `names` alone, and the same single warning report after it.
fn read_selected_outputs<N: AsRef<str>>(
    kernel: &Rc<MeKernelCell>,
    names: &[N],
) -> Result<SelectedRead, MeError> {
    let kernel = kernel.borrow()?;
    let observation = kernel.observe()?;
    let mut values = Vec::new();
    let returned =
        kernel.get_selected_outputs(&observation, observation.time(), names, &mut values)?;
    kernel.report_warnings(&observation)?;
    Ok((values, returned))
}

impl MeSimulationSession<'_, '_> {
    /// Current visible values of `names` in request order, repeats included;
    /// `None` for a name that is neither an applied input nor an output.
    ///
    /// Each entry is what [`Self::visible_values`] holds under that name, from
    /// one observation at the coordinate [`Self::output_values`] reads, with
    /// only the requested outputs evaluated and no keyed map.
    pub fn selected_values<N: AsRef<str>>(
        &self,
        names: &[N],
    ) -> Result<Vec<Option<f64>>, MeSessionError> {
        // Borrowed before anything is read, so a kernel that is not active
        // refuses a selected read exactly as it refuses `output_names`.
        let declared = self.host.kernel.borrow()?.declared_output_count();
        let (mut values, returned) = self.observe_selected(names)?;
        if declared != returned {
            return Err(output_arity_mismatch(returned, declared));
        }
        for (name, value) in names.iter().zip(&mut values) {
            if let Some(input) = self.host.inputs.get(name.as_ref()) {
                *value = Some(*input);
            }
        }
        Ok(values)
    }

    fn observe_selected<N: AsRef<str>>(&self, names: &[N]) -> Result<SelectedRead, MeSessionError> {
        self.require_live()?;
        let read = |kernel: &Rc<MeKernelCell>| read_selected_outputs(kernel, names);
        match self.off_point_observation() {
            Some((time, states)) => self.host.anchor().read_settled(time, states, read),
            None => Ok(read(&self.host.kernel)?),
        }
    }
}
