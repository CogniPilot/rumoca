//! Affine derivative systems that define a state derivative together.

use super::*;

#[derive(Clone, Copy)]
pub(in crate::lower) struct AffineDerivativeUnknown<'dae> {
    pub(in crate::lower) state: dae::StateId<'dae>,
    pub(in crate::lower) scalar: usize,
}

#[derive(Clone)]
pub(in crate::lower) struct AffineDerivativeSystem<'dae> {
    pub(in crate::lower) rows: Vec<AffineDerivativeRow<'dae>>,
    pub(in crate::lower) unknowns: Vec<AffineDerivativeUnknown<'dae>>,
    pub(in crate::lower) span: Span,
}

#[derive(Default)]
pub(in crate::lower) struct AffineDerivativeSystems<'dae> {
    systems: Vec<AffineDerivativeSystem<'dae>>,
    unknowns: HashMap<(u32, usize), (usize, usize)>,
}

impl<'dae> AffineDerivativeSystems<'dae> {
    pub(in crate::lower) fn construct(
        systems: Vec<AffineDerivativeSystem<'dae>>,
    ) -> Result<Self, LowerError> {
        let mut unknowns = HashMap::new();
        for (system, definition) in systems.iter().enumerate() {
            for (component, unknown) in definition.unknowns.iter().enumerate() {
                claim_affine_derivative_unknown(
                    &mut unknowns,
                    system,
                    component,
                    *unknown,
                    definition.span,
                )?;
            }
        }
        Ok(Self { systems, unknowns })
    }

    pub(in crate::lower) fn definition(
        &self,
        state: dae::StateId<'dae>,
        scalar: usize,
    ) -> Option<(usize, usize, &AffineDerivativeSystem<'dae>)> {
        let (system, component) = *self.unknowns.get(&(state.index(), scalar))?;
        Some((system, component, &self.systems[system]))
    }
}

fn claim_affine_derivative_unknown(
    unknowns: &mut HashMap<(u32, usize), (usize, usize)>,
    system: usize,
    component: usize,
    unknown: AffineDerivativeUnknown<'_>,
    span: Span,
) -> Result<(), LowerError> {
    if unknowns
        .insert((unknown.state.index(), unknown.scalar), (system, component))
        .is_some()
    {
        return Err(LowerError::contract(
            "a state derivative belongs to two affine systems",
            span,
        ));
    }
    Ok(())
}
