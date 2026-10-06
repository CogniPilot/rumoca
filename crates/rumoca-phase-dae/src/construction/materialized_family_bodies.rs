//! Read the physical Flat row view independently of optional template eligibility.

use super::*;

pub(super) fn lower<'dae>(
    construction: &mut dae::DaeConstruction<'dae>,
    coordinates: &HashMap<VarName, Coordinate<'dae>>,
    functions: &FunctionRegistry<'_, 'dae>,
    equations: &[flat::Equation],
    family: &flat::StructuredEquationFamily,
    owner: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let extents = family
        .domain
        .extents()
        .expect("analysis validates the structured domain");
    // This is the original source storage view, even when ToDae cannot select
    // its optional symbolic body. A whole tensor remains one stored row.
    let packed_extents = match family
        .template
        .as_ref()
        .map(|template| template.scalar_view)
    {
        Some(rumoca_core::ComprehensionScalarView::RowMajorProjection) => &extents[..0],
        Some(rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count }) => {
            &extents[..usize::try_from(binder_count)
                .expect("analysis validates the source projection rank")]
        }
        Some(rumoca_core::ComprehensionScalarView::BinderSubstitution) | None => &extents,
    };
    let rows = family
        .materialized_rows()
        .expect("analysis validates the original materialized row range");
    let points = rows.len() / family.equations_per_point;
    let mut bodies = Vec::with_capacity(family.equations_per_point);
    for body_ordinal in 0..family.equations_per_point {
        let mut stored_bodies = Vec::with_capacity(points);
        for point in 0..points {
            let offset = point
                .checked_mul(family.equations_per_point)
                .and_then(|offset| offset.checked_add(body_ordinal))
                .expect("analysis validates the materialized family row range");
            let equation = &equations[rows.start + offset];
            let symbols = LoweringSymbols {
                coordinates,
                functions,
                shapes: functions.shapes.model_values(),
                function_body: None,
                values: None,
                owner_clock: None,
            };
            stored_bodies.push(lower_structured_body(
                construction,
                symbols,
                &HashMap::new(),
                &equation.residual,
                equation_generation(&equation.origin),
                equation.span,
            )?);
        }
        let provenance = dae::DaeProvenance::generated(
            dae::DaeGeneration::ArrayEquationProjection,
            owner.span(),
        )?;
        // Only the stored binder prefix is packed. Aggregate leaves retain
        // their trailing axes. The caller issues one RowMajorProjection and
        // the DAE constructor checks its full scalar cardinality once.
        bodies.push(pack_row_major_body(
            construction,
            &stored_bodies,
            packed_extents,
            provenance,
        )?);
    }
    Ok(bodies)
}
