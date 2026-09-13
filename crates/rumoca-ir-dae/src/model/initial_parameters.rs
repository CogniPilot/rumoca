//! Checked definitions of parameters determined by the initialization system.

use super::*;
use crate::{InitialParameterValueId, ParameterId, ScalarType};

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct InitialParameterValueEntry {
    pub(crate) target: u32,
    pub(crate) value: u32,
    pub(crate) provenance: DaeProvenance,
}

/// One non-Real parameter's defining equation at initialization.
#[derive(Debug, Clone, Copy)]
pub struct InitialParameterValueView<'dae> {
    target: VariableId<'dae>,
    value: ExprId<'dae>,
    provenance: DaeProvenance,
}

impl<'dae> InitialParameterValueView<'dae> {
    pub const fn target(self) -> VariableId<'dae> {
        self.target
    }
    pub const fn value(self) -> ExprId<'dae> {
        self.value
    }
    pub const fn provenance(self) -> DaeProvenance {
        self.provenance
    }
}

impl<'dae> InitializationEquations<'_, 'dae> {
    /// Define a non-Real fixed=false parameter using the initialization unknowns.
    ///
    /// This is an initialization equation, not a parameter-set binding. Solve
    /// must preserve its simultaneous relation to the other initialization rows.
    pub fn parameter_initial_value(
        &mut self,
        target: ParameterId<'dae>,
        value: ExprId<'dae>,
        owner: DaeProvenance,
    ) -> Result<InitialParameterValueId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, owner)?;
        let variable = self.storage.variable(target.index(), owner)?;
        if variable.role != VariableRole::Parameter {
            return Err(DaeConstructionError::InvalidVariableRole {
                name: variable.name.clone(),
                span: owner.span(),
            });
        }
        let attributes =
            variable
                .attributes
                .as_ref()
                .ok_or(DaeConstructionError::IncompleteDefinition {
                    kind: "parameter attributes",
                    index: target.index(),
                    span: owner.span(),
                })?;
        if attributes.fixed != Some(false) || attributes.binding.is_some() {
            return Err(DaeConstructionError::InvalidInitialParameter {
                name: variable.name.clone(),
                span: owner.span(),
            });
        }
        let declared = &self.storage.value_types[variable.value_type as usize];
        self.storage.expect_closed_expression(value, owner)?;
        let found = self.storage.expr_type(value, owner)?;
        if declared.scalar_type() == ScalarType::Real {
            return Err(DaeConstructionError::InvalidInitialParameter {
                name: variable.name.clone(),
                span: owner.span(),
            });
        }
        if found.scalar_type() != declared.scalar_type() {
            return Err(DaeConstructionError::TypeMismatch {
                expected: declared.scalar_type(),
                found: found.scalar_type(),
                span: owner.span(),
            });
        }
        if found.dimensions() != declared.dimensions() {
            return Err(DaeConstructionError::ShapeMismatch { span: owner.span() });
        }
        if self
            .storage
            .initial_parameter_value_by_variable
            .contains_key(&target.index())
        {
            return Err(duplicate("initial parameter value", target.index(), owner));
        }
        let index = checked_u32(
            self.storage.initial_parameter_values.len(),
            "initial parameter value",
            owner,
        )?;
        self.storage
            .initial_parameter_values
            .push(InitialParameterValueEntry {
                target: target.index(),
                value: value.index(),
                provenance: owner,
            });
        self.storage
            .initial_parameter_value_by_variable
            .insert(target.index(), index);
        Ok(InitialParameterValueId::from_raw(index))
    }
}

impl<'dae> DaeView<'dae> {
    /// Initial parameter definitions in construction order.
    pub fn initial_parameter_values(
        self,
    ) -> impl ExactSizeIterator<Item = InitialParameterValueView<'dae>> {
        self.dae
            .storage
            .initial_parameter_values
            .iter()
            .map(|entry| InitialParameterValueView {
                target: VariableId::from_raw(entry.target),
                value: ExprId::from_raw(entry.value),
                provenance: entry.provenance,
            })
    }
}
