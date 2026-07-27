//! Re-partition equation rows that array expansion has just scalarized.
//!
//! `equation_conversion::route_classified_equation` decides the partition of
//! every equation before [`super::scalarize_phantom_vector_equations`] runs.
//! An array equation whose target is a connector-array family — MSL
//! `Modelica.StateGraph.Parallel` writes `split.set = fill(inPort.set,
//! nBranches)` — names no DAE variable at that point, because the declared
//! variables are the scalarized members `split[1].set`, `split[2].set`, …, so
//! the classification finds no discrete target and the row falls through to
//! the continuous partition.
//!
//! Expansion is exactly the step that rewrites such a row onto its scalarized
//! members, so the discrete classification has to be re-run on the expanded
//! rows. MLS Appendix B.1 puts a discrete update equation in the discrete
//! partition, and `crate::balance` counts it there: left in the continuous
//! partition the row is counted by neither `f_x` (it constrains no continuous
//! unknown) nor `f_m`, and the model reports a spurious balance deficit.

use rumoca_ir_dae as dae;

use crate::analysis::discrete_partition::{
    ResidualDiscreteBucket, classify_residual_discrete_bucket,
};

/// Marker that [`super::scalarize_equation_list`] stamps into the origin of
/// every row it derived from an array equation. Only those rows are
/// reconsidered here; every other row keeps the partition
/// `route_classified_equation` gave it.
const EXPANDED_ROW_ORIGIN_MARKER: &str = " [scalarized ";

/// Move expanded discrete-valued rows out of the continuous partition.
pub(super) fn repartition_expanded_discrete_rows(dae: &mut dae::Dae) {
    let rows = std::mem::take(&mut dae.continuous.equations);
    let mut kept = Vec::with_capacity(rows.len());
    let mut moved = Vec::new();
    let mut spans = Vec::with_capacity(rows.len());
    for eq in rows {
        match expanded_discrete_valued_update(dae, &eq) {
            Some(update) => {
                spans.push((kept.len(), 0));
                moved.push(update);
            }
            None => {
                spans.push((kept.len(), 1));
                kept.push(eq);
            }
        }
    }
    dae.continuous.equations = kept;
    if moved.is_empty() {
        return;
    }
    dae.discrete.valued_updates.extend(moved);
    // Dropping rows shifts every later row, so families that index the
    // continuous block have to be re-pointed; a family whose own rows moved
    // out is dropped by the remap.
    rumoca_ir_dae::remap_structured_families_after_expansion(
        &mut dae.continuous.structured_equations,
        &spans,
    );
}

/// Rebuild an expanded `<discrete-valued scalar> - <rhs>` residual row as the
/// explicit discrete update it is, or return `None` to leave the row alone.
fn expanded_discrete_valued_update(dae: &dae::Dae, eq: &dae::Equation) -> Option<dae::Equation> {
    if eq.lhs.is_some() || eq.scalar_count != 1 || !eq.origin.contains(EXPANDED_ROW_ORIGIN_MARKER) {
        return None;
    }
    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &eq.rhs
    else {
        return None;
    };
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = lhs.as_ref()
    else {
        return None;
    };
    if !subscripts.is_empty() || !dae.variables.discrete_valued.contains_key(name.var_name()) {
        return None;
    }
    if classify_residual_discrete_bucket(dae, &eq.rhs)
        != Some(ResidualDiscreteBucket::DiscreteValued)
    {
        return None;
    }
    Some(dae::Equation::explicit(
        name.clone(),
        rhs.as_ref().clone(),
        eq.span,
        format!("explicit {}", eq.origin),
    ))
}

#[cfg(test)]
mod tests {
    use super::super::scalarize_phantom_vector_equations;
    use rumoca_ir_dae as dae;

    fn span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2)
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(name),
            subscripts: Vec::new(),
            span: span(),
        }
    }

    fn residual(lhs: &str, rhs: &str, scalar_count: usize) -> dae::Equation {
        dae::Equation::residual_array(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var_ref(lhs)),
                rhs: Box::new(var_ref(rhs)),
                span: span(),
            },
            span(),
            "equation from parallel",
            scalar_count,
        )
    }

    fn declare(map: &mut indexmap::IndexMap<rumoca_core::VarName, dae::Variable>, name: &str) {
        let key = rumoca_core::VarName::new(name);
        map.insert(key.clone(), dae::Variable::new(key, span()));
    }

    #[test]
    fn expanded_discrete_family_rows_become_discrete_updates() {
        let mut model = dae::Dae::new();
        declare(
            &mut model.variables.discrete_valued,
            "parallel.split[1].set",
        );
        declare(
            &mut model.variables.discrete_valued,
            "parallel.split[2].set",
        );
        declare(&mut model.variables.discrete_valued, "parallel.inPort.set");
        model
            .continuous
            .equations
            .push(residual("parallel.split.set", "parallel.inPort.set", 2));

        scalarize_phantom_vector_equations(&mut model).unwrap();

        assert!(
            model.continuous.equations.is_empty(),
            "expanded discrete rows must leave the continuous partition: {:?}",
            model
                .continuous
                .equations
                .iter()
                .map(|eq| eq.origin.clone())
                .collect::<Vec<_>>()
        );
        let targets = model
            .discrete
            .valued_updates
            .iter()
            .map(|eq| {
                eq.lhs
                    .as_ref()
                    .map(|lhs| lhs.var_name().as_str().to_string())
            })
            .collect::<Vec<_>>();
        assert_eq!(
            targets,
            vec![
                Some("parallel.split[1].set".to_string()),
                Some("parallel.split[2].set".to_string()),
            ]
        );
        assert!(
            model
                .discrete
                .valued_updates
                .iter()
                .all(|eq| eq.scalar_count == 1)
        );
    }

    #[test]
    fn expanded_continuous_family_rows_stay_continuous() {
        let mut model = dae::Dae::new();
        declare(&mut model.variables.algebraics, "plug.pin[1].v");
        declare(&mut model.variables.algebraics, "plug.pin[2].v");
        declare(&mut model.variables.algebraics, "source.v");
        model
            .continuous
            .equations
            .push(residual("plug.pin.v", "source.v", 2));

        scalarize_phantom_vector_equations(&mut model).unwrap();

        assert_eq!(model.continuous.equations.len(), 2);
        assert!(model.discrete.valued_updates.is_empty());
    }
}
