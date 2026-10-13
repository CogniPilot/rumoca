// Authored scalar labels for bounded direct-IR tests, using the sole layout owner.

use rumoca_ir_solve::{VarLayout, scalar_slot_y};

pub(crate) fn with_visible_expressions(
    mut model: rumoca_ir_solve::SolveModel,
    names: Vec<String>,
    mut rows: rumoca_ir_solve::ScalarProgramBlock,
    metadata: Vec<rumoca_ir_solve::SolveVariableMeta>,
) -> rumoca_ir_solve::SolveModel {
    use rumoca_ir_solve::{LinearOp, ScalarSlot, VisibleProjection, VisibleProjectionInput};
    if rows.row_count() == 0 && !names.is_empty() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("authored-visible-fixture.mo"),
            1,
            2,
        );
        let programs = names
            .iter()
            .map(|name| {
                let slot = model
                    .problem
                    .layout
                    .binding(name)
                    .or_else(|| {
                        crate::timeline::runtime_parameter_index(&model.problem.solve_layout, name)
                            .map(rumoca_ir_solve::scalar_slot_p)
                    })
                    .expect("authored fixture names select existing numerical storage");
                let load = match slot {
                    ScalarSlot::Y { index, .. } => LinearOp::LoadY { dst: 0, index },
                    ScalarSlot::P { index, .. } => LinearOp::LoadP { dst: 0, index },
                    ScalarSlot::Constant(value) => LinearOp::Const { dst: 0, value },
                    ScalarSlot::Time => LinearOp::LoadTime { dst: 0 },
                };
                vec![load, LinearOp::StoreOutput { src: 0 }]
            })
            .collect();
        rows = rumoca_ir_solve::ScalarProgramBlock::with_program_spans(
            programs,
            vec![span; names.len()],
        )
        .unwrap();
    }
    model.visible_projection = VisibleProjection::construct(
        &model.problem,
        vec![VisibleProjectionInput::Expression {
            names,
            rows,
            metadata: (!metadata.is_empty()).then_some(metadata),
        }],
    )
    .expect("authored expression fixture has checked visible outputs");
    model
}

pub(crate) fn named_y_layout(layout: VarLayout, names: Vec<String>) -> VarLayout {
    let y_scalars = if layout.y_scalars() == 0 {
        names.len()
    } else {
        layout.y_scalars()
    };
    let mut bindings = layout.bindings().clone();
    let mut shapes = indexmap::IndexMap::new();
    let mut spans = indexmap::IndexMap::new();
    for name in bindings.keys() {
        if let Some(shape) = layout.shape(name.as_str()) {
            shapes.insert(name.clone(), shape.to_vec());
        }
        if let Some(span) = layout.shape_span(name.as_str()) {
            spans.insert(name.clone(), span);
        }
    }
    for (index, name) in names.into_iter().enumerate() {
        let slot = scalar_slot_y(index);
        if layout.scalar_name_for_slot(slot).is_none() {
            bindings.insert(rumoca_core::VarName::intern(&name), slot);
        }
    }
    VarLayout::from_interned_parts_with_shapes_and_spans(
        bindings,
        shapes,
        spans,
        y_scalars,
        layout.p_scalars(),
    )
    .expect("authored fixture labels preserve the original checked layout")
}
