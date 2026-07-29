use super::*;

pub(super) fn reconstruct_equation_systems<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    reconstruct_equation_owners(wire, dae, ids)?;
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

#[derive(Default)]
struct OwnerReplay {
    next: usize,
    residuals: usize,
    families: usize,
}

fn reconstruct_equation_owners<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
) -> Result<(), DaeConstructionError> {
    let mut continuous = OwnerReplay::default();
    let mut initialization = OwnerReplay::default();
    let mut body_cursor = 0usize;
    while continuous.next < wire.continuous_equation_owners.len()
        || initialization.next < wire.initialization_equation_owners.len()
    {
        replay_continuous_residuals(wire, dae, ids, &mut continuous)?;
        replay_initialization_residuals(wire, dae, ids, &mut initialization)?;
        let continuous_start = next_family_start(
            &wire.continuous_equation_owners,
            &wire.continuous_families,
            &continuous,
            "continuous equation owner order",
        )?;
        let initialization_start = next_family_start(
            &wire.initialization_equation_owners,
            &wire.initialization_families,
            &initialization,
            "initialization equation owner order",
        )?;
        if continuous_start == Some(body_cursor) {
            let family = &wire.continuous_families[continuous.families];
            reconstruct_continuous_family(wire, dae, ids, family, &mut body_cursor)?;
            continuous.families += 1;
            continuous.next += 1;
        } else if initialization_start == Some(body_cursor) {
            let family = &wire.initialization_families[initialization.families];
            reconstruct_initialization_family(wire, dae, ids, family, &mut body_cursor)?;
            initialization.families += 1;
            initialization.next += 1;
        } else if continuous_start.is_some() || initialization_start.is_some() {
            return Err(malformed("equation family body order"));
        }
    }
    expect_owner_replay_consumed(wire, dae, continuous, initialization, body_cursor)
}

fn replay_continuous_residuals<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    replay: &mut OwnerReplay,
) -> Result<(), DaeConstructionError> {
    while let Some(EquationOwnerWire::Residual(raw)) =
        wire.continuous_equation_owners.get(replay.next)
    {
        if *raw as usize != replay.residuals {
            return Err(malformed("continuous equation owner order"));
        }
        let equation = wire
            .continuous_equations
            .get(replay.residuals)
            .ok_or_else(|| malformed("continuous equation owner order"))?;
        let residual = mapped_residual(ids, equation)?;
        dae.continuous(|continuous| {
            continuous.equation(equation.provenance, |owner| owner.residual(residual))
        })?;
        replay.residuals += 1;
        replay.next += 1;
    }
    Ok(())
}

fn replay_initialization_residuals<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    replay: &mut OwnerReplay,
) -> Result<(), DaeConstructionError> {
    while let Some(EquationOwnerWire::Residual(raw)) =
        wire.initialization_equation_owners.get(replay.next)
    {
        if *raw as usize != replay.residuals {
            return Err(malformed("initialization equation owner order"));
        }
        let equation = wire
            .initialization_equations
            .get(replay.residuals)
            .ok_or_else(|| malformed("initialization equation owner order"))?;
        let residual = mapped_residual(ids, equation)?;
        dae.initialization(|initialization| {
            initialization.equation(equation.provenance, |owner| owner.residual(residual))
        })?;
        replay.residuals += 1;
        replay.next += 1;
    }
    Ok(())
}

fn next_family_start(
    owners: &[EquationOwnerWire],
    families: &[StructuredFamilyWire],
    replay: &OwnerReplay,
    column: &'static str,
) -> Result<Option<usize>, DaeConstructionError> {
    let Some(owner) = owners.get(replay.next) else {
        return Ok(None);
    };
    let EquationOwnerWire::Structured(raw) = owner else {
        return Err(malformed(column));
    };
    if *raw as usize != replay.families {
        return Err(malformed(column));
    }
    let family = families
        .get(replay.families)
        .ok_or_else(|| malformed(column))?;
    Ok(Some(family.bodies.start as usize))
}

fn reconstruct_continuous_family<'dae>(
    wire: &StorageWire,
    dae: &mut DaeConstruction<'dae>,
    ids: &WireIds<'dae>,
    family: &StructuredFamilyWire,
    body_cursor: &mut usize,
) -> Result<(), DaeConstructionError> {
    let domain = mapped(&ids.domains, family.domain, "domain", family.provenance)?;
    let bodies = mapped_family_bodies(wire, ids, family, body_cursor)?;
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
    body_cursor: &mut usize,
) -> Result<(), DaeConstructionError> {
    let domain = mapped(&ids.domains, family.domain, "domain", family.provenance)?;
    let bodies = mapped_family_bodies(wire, ids, family, body_cursor)?;
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
    cursor: &mut usize,
) -> Result<Vec<ExprId<'dae>>, DaeConstructionError> {
    let indices = family
        .bodies
        .indices()
        .ok_or_else(|| malformed("equation family body range"))?;
    if indices.start != *cursor {
        return Err(malformed("equation family body order"));
    }
    let raw = wire
        .equation_family_bodies
        .get(indices.clone())
        .ok_or_else(|| {
            unknown(
                "equation family body range",
                family.bodies.start,
                family.provenance,
            )
        })?;
    let bodies = map_many(&ids.expressions, raw, "expression", family.provenance)?;
    *cursor = indices.end;
    Ok(bodies)
}

fn expect_owner_replay_consumed(
    wire: &StorageWire,
    dae: &DaeConstruction<'_>,
    continuous: OwnerReplay,
    initialization: OwnerReplay,
    body_cursor: usize,
) -> Result<(), DaeConstructionError> {
    if continuous.residuals != wire.continuous_equations.len()
        || continuous.families != wire.continuous_families.len()
        || initialization.residuals != wire.initialization_equations.len()
        || initialization.families != wire.initialization_families.len()
        || body_cursor != wire.equation_family_bodies.len()
        || dae.storage.equation_family_bodies != wire.equation_family_bodies
    {
        return Err(malformed("equation owners"));
    }
    Ok(())
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
