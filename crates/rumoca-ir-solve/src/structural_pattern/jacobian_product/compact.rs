//! Join the actual map output and opaque directions to one issued placement.
mod point;
#[cfg(test)]
mod tests;
pub use point::{StateJacobianCompactConstruction, StateJacobianCompactInput};

use super::{StateJacobianProductConstruction, StateJacobianProductValue, invalid};
use crate::{
    CompactJacobianPlacement, CompactJacobianPlacementReceipt, ProgramRegister,
    SolveProgramConstructionError, SolveScalarType, SolveValueType, StructuralPattern,
    TypedProgramBuilder,
};
use rumoca_core::{Span, StructuredIndexDomain};

type Error = SolveProgramConstructionError;

// No raw-register constructor, Clone, or wire path. The same object supplies
// the numeric map and its output-derived relation at final materialization.
pub(super) struct BodyReceipt<'program> {
    pub(super) value: ProgramRegister<'program>,
    pub(super) placement: CompactJacobianPlacement,
    pub(super) relation: StructuralPattern,
    pub(super) points: usize,
}

impl<'program> StateJacobianProductConstruction<'program, '_> {
    /// Construct and retain one original-output family. The callback receives
    /// fresh body values only; it cannot provide a raw graph, direction mask,
    /// alternate capture vector or same-shaped replacement output.
    /// ```compile_fail
    /// use rumoca_core::{Span, StructuredIndexDomain};
    /// use rumoca_ir_solve::{StateJacobianProductConstruction, CompactJacobianPlacementReceipt,
    ///     SolveProgramConstructionError, SolveFiniteCheck};
    /// fn substitute<'p,'s>(c: &mut StateJacobianProductConstruction<'p,'s>,
    ///     p: CompactJacobianPlacementReceipt<'_>, d: StructuredIndexDomain, span: Span)
    ///     -> Result<(), SolveProgramConstructionError> {
    ///     let foreign = c.constant(1.0, (span, SolveFiniteCheck::Derivative))?;
    ///     c.store_compact(p, d, 0, span, |_| Ok(foreign))
    /// }
    /// ```
    pub fn store_compact(
        &mut self,
        placement: CompactJacobianPlacementReceipt<'_>,
        domain: StructuredIndexDomain,
        program_base: u32,
        span: Span,
        build: impl for<'body, 'scope> FnOnce(
            &mut StateJacobianCompactConstruction<'body, 'scope>,
        )
            -> Result<StateJacobianProductValue<'body>, Error>,
    ) -> Result<(), Error> {
        let receipt = construct_body(
            self.builder,
            &self.inputs,
            placement,
            domain,
            (
                self.states,
                self.parameters,
                self.profile,
                program_base,
                span,
            ),
            build,
        )?;
        self.compact.push(receipt);
        Ok(())
    }
}

fn construct_body<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    inputs: &[ProgramRegister<'program>],
    placement: CompactJacobianPlacementReceipt<'_>,
    domain: StructuredIndexDomain,
    (states, parameters, profile, program_base, span): (
        usize,
        usize,
        crate::SolveArithmeticProfile,
        u32,
        Span,
    ),
    build: impl for<'body, 'scope> FnOnce(
        &mut StateJacobianCompactConstruction<'body, 'scope>,
    ) -> Result<StateJacobianProductValue<'body>, Error>,
) -> Result<BodyReceipt<'program>, Error> {
    let geometry = placement.placement();
    if geometry.states != states || inputs.len() != states + parameters + 1 {
        return Err(invalid(span));
    }
    // Ordinals are the same concrete domain the map executes, not just a
    // matching shape. Original binder identity/provenance stays in the map.
    if domain.binders.len() != geometry.extents.len()
        || domain
            .binders
            .iter()
            .zip(&geometry.extents)
            .any(|(b, &n)| b.lower != 1 || b.upper != i64::from(n) || b.step != 1)
    {
        return Err(invalid(span));
    }
    let points = geometry
        .extents
        .iter()
        .try_fold(1usize, |n, &e| n.checked_mul(e as usize))
        .ok_or_else(|| invalid(span))?;
    let count = u32::try_from(points).map_err(|_| invalid(span))?;
    program_base
        .checked_add(count)
        .ok_or_else(|| invalid(span))?;
    let real = SolveScalarType::real(profile);
    let body_type = if geometry.columns.is_empty() {
        SolveValueType::scalar(real)
    } else {
        SolveValueType::tensor(
            real,
            vec![u32::try_from(geometry.columns.len()).map_err(|_| invalid(span))?],
        )
        .map_err(|_| invalid(span))?
    };
    let input_count = u32::try_from(inputs.len()).map_err(|_| invalid(span))?;
    let vector = builder.construct_aggregate(inputs, vec![input_count], span)?;
    let mut present = None;
    let value = builder.map(
        domain,
        &[vector],
        body_type,
        span,
        |body, captures, binders, output| {
            let mut context = StateJacobianCompactConstruction::new(
                body,
                (captures[0], binders),
                geometry,
                (parameters, program_base, profile, span),
            )?;
            let result = build(&mut context)?;
            present = Some(context.finish_output(result, output, span)?);
            Ok(())
        },
    )?;
    let present = present.ok_or_else(|| invalid(span))?;
    // Only directions present at the actual original output can certify an
    // edge. Numeric zero remains Some and is deliberately retained.
    let columns = geometry
        .columns
        .iter()
        .zip(present)
        .filter_map(|(c, p)| p.then_some((c.base, c.strides.clone())))
        .collect();
    // The original issued geometry keeps its identity unchanged. A fresh
    // private geometry derives only the output-present column relation.
    let filtered = CompactJacobianPlacement::new(
        geometry.extents.clone(),
        (geometry.row.base, geometry.row.strides.clone()),
        columns,
        geometry.states,
    )
    .ok_or_else(|| invalid(span))?;
    let relation = filtered
        .derive_placement_relation(span)
        .map_err(|_| invalid(span))?;
    Ok(BodyReceipt {
        value,
        placement: geometry.clone(),
        relation,
        points,
    })
}
