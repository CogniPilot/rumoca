use super::*;

fn real(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: Literal::Real(value),
        span: rumoca_core::Span::DUMMY,
    }
}

fn array(elements: Vec<rumoca_core::Expression>, is_matrix: bool) -> rumoca_core::Expression {
    rumoca_core::Expression::Array {
        elements,
        is_matrix,
        span: rumoca_core::Span::DUMMY,
    }
}

fn component_reference(parts: Vec<rumoca_core::ComponentRefPart>) -> rumoca_core::Reference {
    rumoca_core::Reference::from_component_reference(rumoca_core::ComponentReference {
        local: false,
        span: rumoca_core::Span::DUMMY,
        parts,
        def_id: None,
    })
}

#[test]
fn flatten_array_elements_flattens_matrix_rows() {
    let row1 = array(vec![real(1.0), real(2.0)], false);
    let row2 = array(vec![real(3.0), real(4.0)], false);

    let flattened = flatten_array_elements(&[row1, row2]);
    let values = flattened
        .iter()
        .map(|expr| match expr {
            rumoca_core::Expression::Literal {
                value: Literal::Real(value),
                ..
            } => *value,
            other => panic!("expected scalar literal, got {other:?}"),
        })
        .collect::<Vec<_>>();

    assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn scoped_single_scalar_value_has_scalar_dimensions() {
    let dae_model = dae::Dae::default();
    let structural_bindings = IndexMap::new();
    let analysis = FunctionProjectionAnalysis::new(&dae_model, &structural_bindings);
    let mut scope = FunctionProjectionScope::default();
    scope
        .scalars
        .insert("tau_inv".to_string(), vec![real(17.0)]);
    let expr = rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("tau_inv"),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(analysis.expr_dims(&expr, &scope, 0), Some(Vec::new()));
}

#[test]
fn project_reference_indices_preserves_indexed_component_parts() {
    let reference = component_reference(vec![
        rumoca_core::ComponentRefPart {
            ident: "vehicle".to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        },
        rumoca_core::ComponentRefPart {
            ident: "motor".to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
        },
        rumoca_core::ComponentRefPart {
            ident: "history".to_string(),
            span: rumoca_core::Span::DUMMY,
            subs: Vec::new(),
        },
    ]);

    let projected = project_reference_indices(&reference, &[2], rumoca_core::Span::DUMMY)
        .expect("structured reference projection should succeed");

    let component_ref = projected
        .component_ref()
        .expect("projected reference should preserve component-reference structure");
    assert_eq!(projected.as_str(), "vehicle.motor[1].history[2]");
    assert_eq!(component_ref.parts[1].ident, "motor");
    assert_eq!(component_ref.parts[1].subs.len(), 1);
    assert_eq!(component_ref.parts[2].ident, "history");
    assert_eq!(component_ref.parts[2].subs.len(), 1);
}
