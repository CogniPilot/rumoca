//! Input and output sets resolved once and addressed by an integer id.
//!
//! A binding holds what a named write or read resolves on every call: the
//! value references of its inputs, or the output positions of its names and
//! which of them are declared inputs. Writes and reads through it then take
//! values alone. Ids are unique in the process, so an id this session did not
//! issue is foreign; a restart makes every binding the session issued stale.

use std::{
    rc::Rc,
    sync::atomic::{AtomicU32, Ordering},
};

use indexmap::IndexMap;

use super::{MeSessionError, MeSimulationSession, inputs::ContinuousWrite};
use crate::fmi_me::{MeValueRef, SolveMeKernel};

/// The next binding id of this process; ids start at 1.
static NEXT_BINDING: AtomicU32 = AtomicU32::new(1);

/// Why a session refused a binding id.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
pub enum MeBindingRefusal {
    /// The session did not issue this id.
    #[error("was not issued by this session")]
    Foreign,
    /// The session restarted after it issued this id.
    #[error("was issued before the session restarted")]
    Stale,
    /// An output binding was used for a write.
    #[error("binds outputs, not inputs")]
    NotInputs,
    /// An input binding was used for a read.
    #[error("binds inputs, not outputs")]
    NotOutputs,
    /// A write carried a different number of values than the binding has
    /// inputs.
    #[error("binds {bound} inputs but the write carries {given} values")]
    Arity { bound: usize, given: usize },
    /// A bound input that is no visible output has not been applied yet, so
    /// the read has no value for it.
    #[error("has no value for entry {position}: it is an input not yet applied")]
    Unapplied { position: usize },
}

/// Declared inputs resolved to their Float64 value references.
pub(super) struct BoundInputs {
    names: Box<[String]>,
    pub(super) references: Box<[MeValueRef]>,
}

impl BoundInputs {
    /// Record a committed frame as the named write records it.
    pub(super) fn record(&self, inputs: &mut IndexMap<String, f64>, values: &[f64]) {
        for (name, value) in self.names.iter().zip(values) {
            match inputs.get_mut(name.as_str()) {
                Some(recorded) => *recorded = *value,
                None => {
                    inputs.insert(name.clone(), *value);
                }
            }
        }
    }
}

/// One requested name of an output binding.
struct BoundOutput {
    name: String,
    /// Its position in the visible output index, when it is an output.
    position: Option<usize>,
    /// Whether it is a declared input, whose applied value the read reports.
    input: bool,
}

/// Requested names resolved through the visible output index, in request
/// order with repeats.
pub(super) struct BoundOutputs {
    entries: Box<[BoundOutput]>,
}

impl BoundOutputs {
    /// The resolved outputs as the kernel's resolved getter takes them.
    pub(super) fn positions(&self) -> impl ExactSizeIterator<Item = (Option<usize>, &str)> {
        self.entries
            .iter()
            .map(|entry| (entry.position, entry.name.as_str()))
    }
}

enum Bound {
    Inputs(Rc<BoundInputs>),
    Outputs(BoundOutputs),
}

/// The bindings one session issued since it last restarted, and the ids it
/// issued before.
#[derive(Default)]
pub(super) struct SessionBindings {
    live: Vec<(u32, Bound)>,
    stale: Vec<u32>,
}

impl SessionBindings {
    /// Make every binding issued so far stale.
    pub(super) fn restart(&mut self) {
        self.stale.extend(self.live.drain(..).map(|(id, _)| id));
    }

    fn issue(&mut self, bound: Bound) -> Result<u32, MeSessionError> {
        let id = NEXT_BINDING
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |id| id.checked_add(1))
            .map_err(|_| MeSessionError::Contract {
                reason: "this process has issued every binding id".to_owned(),
            })?;
        self.live.push((id, bound));
        Ok(id)
    }

    fn lookup(&self, binding: u32) -> Result<&Bound, MeSessionError> {
        if let Some((_, bound)) = self.live.iter().find(|(id, _)| *id == binding) {
            return Ok(bound);
        }
        let refusal = if self.stale.contains(&binding) {
            MeBindingRefusal::Stale
        } else {
            MeBindingRefusal::Foreign
        };
        Err(MeSessionError::Binding { binding, refusal })
    }

    pub(super) fn inputs(&self, binding: u32) -> Result<Rc<BoundInputs>, MeSessionError> {
        match self.lookup(binding)? {
            Bound::Inputs(inputs) => Ok(Rc::clone(inputs)),
            Bound::Outputs(_) => Err(MeSessionError::Binding {
                binding,
                refusal: MeBindingRefusal::NotInputs,
            }),
        }
    }

    pub(super) fn outputs(&self, binding: u32) -> Result<&BoundOutputs, MeSessionError> {
        match self.lookup(binding)? {
            Bound::Outputs(outputs) => Ok(outputs),
            Bound::Inputs(_) => Err(MeSessionError::Binding {
                binding,
                refusal: MeBindingRefusal::NotOutputs,
            }),
        }
    }
}

fn resolve_output(kernel: &SolveMeKernel, name: &str) -> Result<BoundOutput, MeSessionError> {
    let position = kernel.visible_output_position(name)?;
    let input = kernel.input_value_reference(name).is_some();
    if position.is_none() && !input {
        return Err(MeSessionError::Contract {
            reason: format!("'{name}' is neither a visible output nor a declared input"),
        });
    }
    Ok(BoundOutput {
        name: name.to_owned(),
        position,
        input,
    })
}

impl MeSimulationSession<'_, '_> {
    /// Resolve declared inputs once for [`Self::write_bound`].
    ///
    /// Each name is checked as [`Self::set_inputs`] checks it, with the same
    /// refusals. The input event reach every later write consults is derived
    /// here, so no write derives it.
    pub fn bind_inputs<N: AsRef<str>>(&mut self, names: &[N]) -> Result<u32, MeSessionError> {
        self.require_live()?;
        let references = self.input_references(names.iter().map(AsRef::as_ref))?;
        self.host.kernel.borrow()?.derive_input_event_reach();
        self.bindings.issue(Bound::Inputs(Rc::new(BoundInputs {
            names: names.iter().map(|name| name.as_ref().to_owned()).collect(),
            references: references.into(),
        })))
    }

    /// Resolve names once for [`Self::read_bound`], through the index
    /// [`Self::selected_values`] reads by, repeats preserved. A name that is
    /// neither a visible output nor a declared input is refused.
    pub fn bind_outputs<N: AsRef<str>>(&mut self, names: &[N]) -> Result<u32, MeSessionError> {
        self.require_live()?;
        let entries = {
            let kernel = self.host.kernel.borrow()?;
            names
                .iter()
                .map(|name| resolve_output(&kernel, name.as_ref()))
                .collect::<Result<Box<[_]>, _>>()?
        };
        self.bindings
            .issue(Bound::Outputs(BoundOutputs { entries }))
    }

    /// [`Self::set_inputs`] of the bound inputs with `values`, in binding
    /// order: the same validation, unchanged-frame rule and commit.
    ///
    /// A continuous write takes no restoration point where nothing after its
    /// first mutation can fail
    /// (`SolveMeKernel::write_continuous_without_rollback`).
    pub fn write_bound(&mut self, binding: u32, values: &[f64]) -> Result<(), MeSessionError> {
        self.require_live()?;
        let inputs = self.bindings.inputs(binding)?;
        if values.len() != inputs.references.len() {
            return Err(MeSessionError::Binding {
                binding,
                refusal: MeBindingRefusal::Arity {
                    bound: inputs.references.len(),
                    given: values.len(),
                },
            });
        }
        self.write_input_frame(
            &inputs.references,
            values,
            ContinuousWrite::WithoutRollbackWhereInfallible,
            |recorded| inputs.record(recorded, values),
        )
    }

    /// [`Self::selected_values`] of the bound names into `values`, bit for
    /// bit, from one observation at the same coordinate.
    pub fn read_bound(&self, binding: u32, values: &mut Vec<f64>) -> Result<(), MeSessionError> {
        let outputs = self.bindings.outputs(binding)?;
        let selected = self.observe_outputs(|kernel, observation, selected| {
            kernel.get_resolved_outputs(
                observation,
                observation.time(),
                outputs.positions(),
                selected,
            )
        })?;
        values.clear();
        for (position, (entry, value)) in outputs.entries.iter().zip(selected).enumerate() {
            let applied = if entry.input {
                self.host.inputs.get(entry.name.as_str()).copied()
            } else {
                None
            };
            values.push(applied.or(value).ok_or(MeSessionError::Binding {
                binding,
                refusal: MeBindingRefusal::Unapplied { position },
            })?);
        }
        Ok(())
    }
}
