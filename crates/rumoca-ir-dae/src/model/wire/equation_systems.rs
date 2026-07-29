use super::*;

pub(super) fn reconstruct_equation_systems<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    reconstruct_continuous_owners(wire, dae, ids)?;
    reconstruct_initialization_owners(wire, dae, ids)?;
    for equation in &wire.discrete_real_equations {
        let residual = mapped_residual(ids, equation)?;
        dae.discrete(|discrete| {
            discrete.real_equation(equation.provenance, |owner| owner.residual(residual))
        })?;
    }
    for assignment in &wire.discrete_assignments {
        let variable = mapped(
            &ids.variables,
            assignment.target,
            "variable",
            assignment.provenance,
        )?;
        let target = DiscreteValueId::from_raw(variable.index());
        let value = mapped(
            &ids.expressions,
            assignment.value,
            "expression",
            assignment.provenance,
        )?;
        dae.discrete(|discrete| discrete.assignment(assignment.provenance, target, value))?;
    }
    Ok(())
}

fn reconstruct_continuous_owners<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    let mut residuals = 0usize;
    let mut families = 0usize;
    for owner in &wire.continuous_equation_owners {
        match *owner {
            EquationOwnerWire::Residual(raw) if raw as usize == residuals => {
                let equation = wire
                    .continuous_equations
                    .get(residuals)
                    .ok_or_else(|| malformed("continuous equation owner order"))?;
                let residual = mapped_residual(ids, equation)?;
                dae.continuous(|continuous| {
                    continuous.equation(equation.provenance, |owner| owner.residual(residual))
                })?;
                residuals += 1;
            }
            EquationOwnerWire::Structured(raw) if raw as usize == families => {
                let family = wire
                    .continuous_families
                    .get(families)
                    .ok_or_else(|| malformed("continuous equation owner order"))?;
                reconstruct_continuous_family(wire, dae, ids, family)?;
                families += 1;
            }
            _ => return Err(malformed("continuous equation owner order")),
        }
    }
    if residuals != wire.continuous_equations.len() || families != wire.continuous_families.len() {
        return Err(malformed("continuous equation owner order"));
    }
    Ok(())
}

fn reconstruct_initialization_owners<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    let mut residuals = 0usize;
    let mut families = 0usize;
    for owner in &wire.initialization_equation_owners {
        match *owner {
            EquationOwnerWire::Residual(raw) if raw as usize == residuals => {
                let equation = wire
                    .initialization_equations
                    .get(residuals)
                    .ok_or_else(|| malformed("initialization equation owner order"))?;
                let residual = mapped_residual(ids, equation)?;
                dae.initialization(|initialization| {
                    initialization.equation(equation.provenance, |owner| owner.residual(residual))
                })?;
                residuals += 1;
            }
            EquationOwnerWire::Structured(raw) if raw as usize == families => {
                let family = wire
                    .initialization_families
                    .get(families)
                    .ok_or_else(|| malformed("initialization equation owner order"))?;
                reconstruct_initialization_family(wire, dae, ids, family)?;
                families += 1;
            }
            _ => return Err(malformed("initialization equation owner order")),
        }
    }
    if residuals != wire.initialization_equations.len()
        || families != wire.initialization_families.len()
    {
        return Err(malformed("initialization equation owner order"));
    }
    Ok(())
}

fn reconstruct_continuous_family<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    family: &StructuredFamilyWire,
) -> Result<(), DaeConstructionError> {
    let domain = mapped(&ids.domains, family.domain, "domain", family.provenance)?;
    let bodies = mapped_family_bodies(wire, ids, family)?;
    let id = dae.continuous(|continuous| {
        continuous.structured_family(family.provenance, domain, family.scalar_view, |residuals| {
            attach_family_bodies(residuals, &bodies)
        })
    })?;
    if dae.storage.continuous_families[id.index() as usize].scalar_rows != family.scalar_rows {
        return Err(DaeConstructionError::ShapeMismatch {
            span: family.provenance.span(),
        });
    }
    Ok(())
}

fn reconstruct_initialization_family<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    family: &StructuredFamilyWire,
) -> Result<(), DaeConstructionError> {
    let domain = mapped(&ids.domains, family.domain, "domain", family.provenance)?;
    let bodies = mapped_family_bodies(wire, ids, family)?;
    let id = dae.initialization(|initialization| {
        initialization.structured_family(
            family.provenance,
            domain,
            family.scalar_view,
            |residuals| attach_family_bodies(residuals, &bodies),
        )
    })?;
    if dae.storage.initialization_families[id.index() as usize].scalar_rows != family.scalar_rows {
        return Err(DaeConstructionError::ShapeMismatch {
            span: family.provenance.span(),
        });
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

fn mapped_family_bodies<'dae>(
    wire: &StorageWire,
    ids: &WireIds<'dae>,
    family: &StructuredFamilyWire,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    let raw = wire
        .equation_family_bodies
        .get(
            family
                .bodies
                .indices()
                .ok_or(DaeConstructionError::MalformedWire {
                    column: "equation family body range",
                })?,
        )
        .ok_or_else(|| {
            unknown(
                "equation family body range",
                family.bodies.start,
                family.provenance,
            )
        })?;
    map_many(&ids.expressions, raw, "expression", family.provenance)
}

fn mapped_residual<'dae>(
    ids: &WireIds<'dae>,
    equation: &ResidualEquationWire,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    mapped(
        &ids.expressions,
        equation.residual,
        "expression",
        equation.provenance,
    )
}
