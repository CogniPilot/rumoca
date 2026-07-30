use super::*;

/// Proof that one effective value type is legal for one Appendix-B coordinate role.
///
/// Only `Storage::variable_type_capability` can mint this capability. Variable
/// insertion consumes it, so the dense catalog cannot contain a role/type pair
/// that bypassed the Appendix-B classification.
pub(super) struct VariableTypeCapability<'dae> {
    value_type: ValueTypeId<'dae>,
    role: VariableRole,
    variability: ExpressionVariability,
}

impl<'dae> VariableTypeCapability<'dae> {
    pub(super) const fn value_type(&self) -> ValueTypeId<'dae> {
        self.value_type
    }

    pub(super) const fn role(&self) -> VariableRole {
        self.role
    }

    pub(super) const fn variability(&self) -> ExpressionVariability {
        self.variability
    }
}

#[derive(Clone, Copy)]
enum CoordinateTypeClass {
    Primitive,
    Real,
    DiscreteValue,
}

impl Storage {
    pub(super) fn variable_type_capability<'dae>(
        &self,
        name: &VarName,
        role: VariableRole,
        variability: ExpressionVariability,
        value_type: ValueTypeId<'dae>,
        at: DaeProvenance,
    ) -> Result<VariableTypeCapability<'dae>, DaeConstructionError> {
        let ty = self.value_type_at(value_type.index(), at)?;
        let expected = expected_coordinate_type(role, variability);
        if coordinate_type_matches(ty, expected) {
            return Ok(VariableTypeCapability {
                value_type,
                role,
                variability,
            });
        }
        Err(DaeConstructionError::InvalidVariableType {
            name: name.clone(),
            role,
            found: ty.scalar_type(),
            span: at.span(),
        })
    }
}

fn expected_coordinate_type(
    role: VariableRole,
    variability: ExpressionVariability,
) -> CoordinateTypeClass {
    match role {
        VariableRole::Parameter | VariableRole::Constant => CoordinateTypeClass::Primitive,
        VariableRole::Input if variability == ExpressionVariability::Continuous => {
            CoordinateTypeClass::Real
        }
        VariableRole::Input => CoordinateTypeClass::Primitive,
        VariableRole::State
        | VariableRole::Algebraic
        | VariableRole::Output
        | VariableRole::DiscreteReal => CoordinateTypeClass::Real,
        VariableRole::DiscreteValue => CoordinateTypeClass::DiscreteValue,
    }
}

fn coordinate_type_matches(ty: &ValueType, expected: CoordinateTypeClass) -> bool {
    match expected {
        CoordinateTypeClass::Primitive => !ty.is_record(),
        CoordinateTypeClass::Real => ty.scalar_type() == ScalarType::Real,
        CoordinateTypeClass::DiscreteValue => {
            matches!(
                ty.scalar_type(),
                ScalarType::Integer
                    | ScalarType::Enumeration
                    | ScalarType::Boolean
                    | ScalarType::String
            )
        }
    }
}
