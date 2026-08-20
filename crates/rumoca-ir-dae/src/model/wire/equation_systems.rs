use super::*;
use serde::Serialize;
use serde::ser::SerializeSeq;

pub(super) struct EquationOperationsOutput<'storage> {
    owners: &'storage [EquationOwnerEntry],
    residuals: &'storage [ResidualEquationEntry],
    families: &'storage [StructuredFamilyEntry],
    bodies: &'storage [u32],
}

impl<'storage> EquationOperationsOutput<'storage> {
    pub(super) const fn new(
        owners: &'storage [EquationOwnerEntry],
        residuals: &'storage [ResidualEquationEntry],
        families: &'storage [StructuredFamilyEntry],
        bodies: &'storage [u32],
    ) -> Self {
        Self {
            owners,
            residuals,
            families,
            bodies,
        }
    }
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
enum EquationOperationOutput<'storage> {
    Residual {
        residual: u32,
        provenance: DaeProvenance,
    },
    Structured {
        domain: u32,
        scalar_view: rumoca_core::ComprehensionScalarView,
        bodies: &'storage [u32],
        provenance: DaeProvenance,
    },
}

impl Serialize for EquationOperationsOutput<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut residual_cursor = 0usize;
        let mut family_cursor = 0usize;
        let mut sequence = serializer.serialize_seq(Some(self.owners.len()))?;
        for owner in self.owners {
            let operation = self
                .project_operation(owner, &mut residual_cursor, &mut family_cursor)
                .map_err(serde::ser::Error::custom)?;
            sequence.serialize_element(&operation)?;
        }
        if residual_cursor != self.residuals.len() || family_cursor != self.families.len() {
            return Err(serde::ser::Error::custom(
                "equation owner sequence is incomplete",
            ));
        }
        sequence.end()
    }
}

impl<'storage> EquationOperationsOutput<'storage> {
    fn project_operation(
        &self,
        owner: &EquationOwnerEntry,
        residual_cursor: &mut usize,
        family_cursor: &mut usize,
    ) -> Result<EquationOperationOutput<'storage>, &'static str> {
        match owner {
            EquationOwnerEntry::Residual(raw) => {
                let operation = self.project_residual(*raw, *residual_cursor)?;
                *residual_cursor += 1;
                Ok(operation)
            }
            EquationOwnerEntry::Structured(raw) => {
                let operation = self.project_family(*raw, *family_cursor)?;
                *family_cursor += 1;
                Ok(operation)
            }
        }
    }

    fn project_residual(
        &self,
        raw: u32,
        cursor: usize,
    ) -> Result<EquationOperationOutput<'storage>, &'static str> {
        if raw as usize != cursor {
            return Err("equation residual owner is not dense");
        }
        let equation = self
            .residuals
            .get(cursor)
            .ok_or("equation residual is missing")?;
        Ok(EquationOperationOutput::Residual {
            residual: equation.residual,
            provenance: equation.provenance,
        })
    }

    fn project_family(
        &self,
        raw: u32,
        cursor: usize,
    ) -> Result<EquationOperationOutput<'storage>, &'static str> {
        if raw as usize != cursor {
            return Err("structured equation owner is not dense");
        }
        let family = self
            .families
            .get(cursor)
            .ok_or("structured equation family is missing")?;
        let start = family.bodies.start as usize;
        let end = start
            .checked_add(family.bodies.len as usize)
            .ok_or("structured equation body range overflows")?;
        let bodies = self
            .bodies
            .get(start..end)
            .ok_or("structured equation bodies are missing")?;
        Ok(EquationOperationOutput::Structured {
            domain: family.domain,
            scalar_view: family.scalar_view,
            bodies,
            provenance: family.provenance,
        })
    }
}

pub(super) fn reconstruct_equation_systems<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    for operation in &wire.continuous_equation_operations {
        reconstruct_continuous_operation(operation, dae, ids)?;
    }
    for operation in &wire.initialization_equation_operations {
        reconstruct_initialization_operation(operation, dae, ids)?;
    }
    for equation in &wire.discrete_real_equations {
        let residual = mapped(
            &ids.expressions,
            equation.residual,
            "expression",
            equation.provenance,
        )?;
        dae.discrete(|discrete| match equation.activation {
            DiscreteRealActivationWire::Always => {
                discrete.real_equation(equation.provenance, |owner| owner.residual(residual))
            }
            DiscreteRealActivationWire::When { trigger, guard } => {
                let trigger = mapped(&ids.conditions, trigger, "condition", equation.provenance)?;
                let guard = mapped(&ids.conditions, guard, "condition", equation.provenance)?;
                discrete.when_real_equation(trigger, guard, equation.provenance, |owner| {
                    owner.residual(residual)
                })
            }
        })?;
    }
    Ok(())
}

fn reconstruct_continuous_operation<'dae>(
    operation: &EquationOperationInput,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    match operation {
        EquationOperationInput::Residual {
            residual,
            provenance,
        } => {
            let residual = mapped(&ids.expressions, *residual, "expression", *provenance)?;
            dae.continuous(|continuous| {
                continuous.equation(*provenance, |owner| owner.residual(residual))
            })?;
        }
        EquationOperationInput::Structured {
            domain,
            scalar_view,
            bodies,
            provenance,
        } => {
            let domain = mapped(&ids.domains, *domain, "domain", *provenance)?;
            let bodies = map_many(&ids.expressions, bodies, "expression", *provenance)?;
            dae.continuous(|continuous| {
                continuous.structured_family(*provenance, domain, *scalar_view, |residuals| {
                    attach_family_bodies(residuals, &bodies)
                })
            })?;
        }
    }
    Ok(())
}

fn reconstruct_initialization_operation<'dae>(
    operation: &EquationOperationInput,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    match operation {
        EquationOperationInput::Residual {
            residual,
            provenance,
        } => {
            let residual = mapped(&ids.expressions, *residual, "expression", *provenance)?;
            dae.initialization(|initialization| {
                initialization.equation(*provenance, |owner| owner.residual(residual))
            })?;
        }
        EquationOperationInput::Structured {
            domain,
            scalar_view,
            bodies,
            provenance,
        } => {
            let domain = mapped(&ids.domains, *domain, "domain", *provenance)?;
            let bodies = map_many(&ids.expressions, bodies, "expression", *provenance)?;
            dae.initialization(|initialization| {
                initialization.structured_family(*provenance, domain, *scalar_view, |residuals| {
                    attach_family_bodies(residuals, &bodies)
                })
            })?;
        }
    }
    Ok(())
}

fn attach_family_bodies<'dae>(
    residuals: &mut crate::StructuredResiduals<'_, 'dae>,
    bodies: &[ExprId<'dae>],
) -> Result<(), DaeConstructionError> {
    for body in bodies {
        residuals.body(*body)?;
    }
    Ok(())
}
