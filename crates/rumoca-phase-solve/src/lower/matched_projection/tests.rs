use super::*;
use crate::lower::{algebraic_projection_blocks, lower_algebraic_projection, structural_matching};
use rumoca_core::{
    ComprehensionScalarView, SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId,
    VarName,
};

fn model(count: u32) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("matched_family.mo", "Real x[n]; equation x = zeros(n);");
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 32))
        .expect("fixture source span is real");
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [count]),
                provenance,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                ty,
                provenance,
                dae::VariableAttributes::default(),
            )
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: i64::from(count),
                        step: 1,
                    }],
                },
                provenance,
            )
        })?;
        let x = model.expressions(|expressions| {
            expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Algebraic(variable))
        })?;
        model.continuous(|continuous| {
            continuous.structured_family(
                provenance,
                domain,
                ComprehensionScalarView::RowMajorProjection,
                |family| family.body(x),
            )
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn retained_family_preserves_original_singleton_order_and_targets() {
    model(9).inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let sorted = rumoca_phase_structural::sort(view).unwrap();
        let structural = structural_matching(view, &layout, Some(&sorted)).unwrap();
        assert_eq!(structural.algebraic_blocks.len(), 1);
        let MatchedProjection::Family(family) = &structural.algebraic_blocks[0] else {
            panic!("checked singleton family must retain its compact producer");
        };
        assert_eq!(family.storage_runs.len(), 1);
        let mut actual = Vec::new();
        structural.algebraic_blocks[0]
            .visit_scalar_blocks(|block| {
                assert!(block.tearing().is_none());
                block.visit_matches(|row, unknown| {
                    actual.push((row, unknown));
                    Ok(())
                })
            })
            .unwrap();
        let expected = sorted
            .matching
            .iter()
            .map(|(row, unknown)| (row.0, *unknown))
            .collect::<Vec<_>>();
        assert_eq!(actual, expected);
        let (targets, plan) =
            lower_algebraic_projection(view, &layout, &structural, &solve::ComputeBlock::default())
                .unwrap();
        assert_eq!(plan.blocks.len(), 9);
        for (row, block) in plan.blocks.scalar_blocks().iter().enumerate() {
            assert_eq!(block.rows, [row]);
            assert_eq!(block.y_indices, [row]);
            assert_eq!(targets.target_at(row), Some(solve::scalar_slot_y(row)));
        }
    });
}

#[test]
fn retained_family_storage_refusal_is_fatal_before_scalar_materialization() {
    model(9).inspect(|view| {
        let mut layout = crate::layout::lower_layout(view).unwrap();
        let sorted = rumoca_phase_structural::sort(view).unwrap();
        let rows = sorted
            .matching
            .iter()
            .map(|(row, unknown)| (row.0, *unknown))
            .collect();
        layout.solve_layout.variable_storage_runs[0].scalar_count = 8;
        assert!(algebraic_projection_blocks(view, &layout, &sorted.blocks, &rows).is_err());
        layout.solve_layout.variable_storage_runs[0].scalar_count = 9;
        layout.solve_layout.variable_storage_runs[0].role =
            solve::SolveVariableStorageRole::Parameter;
        assert!(algebraic_projection_blocks(view, &layout, &sorted.blocks, &rows).is_err());
    });
}

#[test]
fn retained_family_metadata_scales_with_bodies_not_points() {
    model(257).inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let sorted = rumoca_phase_structural::sort(view).unwrap();
        let structural = structural_matching(view, &layout, Some(&sorted)).unwrap();
        assert_eq!(structural.algebraic_blocks.len(), 1);
        let MatchedProjection::Family(family) = &structural.algebraic_blocks[0] else {
            panic!("large checked family must use the retained route");
        };
        assert_eq!(family.matching.point_count(), 257);
        assert_eq!(family.storage_runs.len(), 1);
    });
}

#[test]
fn paired_declaration_run_preserves_exact_scalar_pairs_and_singleton_order() {
    model(257).inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let sorted = rumoca_phase_structural::sort(view).unwrap();
        let structural = structural_matching(view, &layout, Some(&sorted)).unwrap();
        let MatchedProjection::Family(family) = &structural.algebraic_blocks[0] else {
            panic!("canonical checked family must retain its source");
        };
        let context = family.matching.context().unwrap();
        let [
            UnknownId::Algebraic {
                variable,
                scalar: 0,
            },
        ] = context.declaration_bases().unwrap()
        else {
            panic!("whole source declaration must retain its actual base");
        };
        let rows = family.matching.row_range();
        let run = family
            .declaration_row_run(view, &layout, context.source(), *variable, &rows)
            .unwrap()
            .unwrap();
        assert_eq!(run.rows().len(), 257);
        assert_eq!(run.y_range().len(), 257);
        for (offset, (row, target)) in run.rows().zip(run.y_range()).enumerate() {
            let (source_row, source_target) =
                family.matching.matched_declaration(offset, 0).unwrap();
            assert_eq!(source_row.0, row);
            let UnknownId::Algebraic { variable, scalar } = source_target else {
                panic!("paired source target changed kind");
            };
            let solve::ScalarSlot::Y { index, .. } = variable_scalar_slot(
                &layout,
                variable.index(),
                scalar as usize,
                family.matching.span(),
            )
            .unwrap() else {
                panic!("checked algebraic declaration changed storage");
            };
            assert_eq!(index, target);
        }
        assert_eq!(family.storage_runs.len(), 1);
    });
}

#[test]
fn paired_declaration_run_refuses_changed_rows_and_foreign_storage() {
    model(9).inspect(|view| {
        let mut layout = crate::layout::lower_layout(view).unwrap();
        let sorted = rumoca_phase_structural::sort(view).unwrap();
        let structural = structural_matching(view, &layout, Some(&sorted)).unwrap();
        let MatchedProjection::Family(family) = &structural.algebraic_blocks[0] else {
            panic!("checked family must retain its source");
        };
        let context = family.matching.context().unwrap();
        let [
            UnknownId::Algebraic {
                variable,
                scalar: 0,
            },
        ] = context.declaration_bases().unwrap()
        else {
            panic!("whole declaration base must be retained");
        };
        let rows = family.matching.row_range();
        for changed in [rows.start..rows.end - 1, rows.start + 1..rows.end + 1] {
            assert!(
                family
                    .declaration_row_run(view, &layout, context.source(), *variable, &changed)
                    .unwrap()
                    .is_none()
            );
        }
        layout.solve_layout.variable_storage_runs[variable.index() as usize].scalar_count = 8;
        assert!(
            family
                .declaration_row_run(view, &layout, context.source(), *variable, &rows)
                .is_err()
        );
    });
}

#[test]
fn paired_family_source_index_derives_only_demanded_scalar_definitions() {
    model(257).inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let sorted = rumoca_phase_structural::sort(view).unwrap();
        let structural = structural_matching(view, &layout, Some(&sorted)).unwrap();
        let compact = crate::lower::continuous_rows::index_continuous_rows(
            view,
            &structural.rows,
            &structural.algebraic_blocks,
            &layout,
        )
        .unwrap();
        let original = crate::lower::continuous_rows::index_continuous_rows(
            view,
            &structural.rows,
            &[],
            &layout,
        )
        .unwrap();
        assert_eq!(compact.storage_counts(), (0, 1));
        assert_eq!(original.storage_counts(), (257, 0));
        let MatchedProjection::Family(family) = &structural.algebraic_blocks[0] else {
            panic!("source family must remain retained");
        };
        let [UnknownId::Algebraic { variable, .. }] = family
            .matching
            .context()
            .unwrap()
            .declaration_bases()
            .unwrap()
        else {
            panic!("source family has one declaration");
        };
        for scalar in [0, 128, 256] {
            let actual = compact.algebraic_definition(*variable, scalar).unwrap();
            let expected = original.algebraic_definition(*variable, scalar).unwrap();
            assert!(matches!(actual, std::borrow::Cow::Owned(_)));
            assert_eq!(actual.expression, expected.expression);
            assert_eq!(actual.scalar, expected.scalar);
            assert_eq!(actual.domain_point, expected.domain_point);
        }
        assert!(compact.algebraic_definition(*variable, 257).is_none());
        assert_eq!(compact.storage_counts(), (0, 1));
    });
}

#[test]
fn matching_queries_retain_one_family_and_project_only_demanded_rows() {
    for count in [8, 65_536] {
        model(count).inspect(|view| {
            let layout = crate::layout::lower_layout(view).unwrap();
            let sorted = rumoca_phase_structural::sort(view).unwrap();
            let structural = structural_matching(view, &layout, Some(&sorted)).unwrap();
            assert_eq!(structural.rows.storage_counts(), (0, 1));
            assert!(structural.rows.all_algebraic(0..count as usize));
            assert!(!structural.rows.all_algebraic(0..count as usize + 1));
            for row in [0, count as usize / 2, count as usize - 1] {
                let expected = sorted
                    .matching
                    .iter()
                    .find(|(equation, _)| equation.0 == row)
                    .map(|(_, unknown)| *unknown);
                assert_eq!(structural.rows.get(&row), expected);
            }
            assert!(structural.rows.get(&(count as usize)).is_none());
        });
    }
}
