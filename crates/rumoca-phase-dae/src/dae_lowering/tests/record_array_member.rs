use super::*;

#[test]
fn test_scalarize_record_array_member_index_projects_record_field() {
    let mut dae = Dae::new();

    for record_index in [1, 2, 3] {
        let name = format!("ductOut.statesFM[{record_index}].X");
        let mut var = dae::Variable::new(rumoca_core::VarName::new(&name), test_span());
        var.dims = vec![2];
        var.component_ref = Some(component_ref(&[
            ("ductOut", None),
            ("statesFM", Some(record_index)),
            ("X", None),
        ]));
        dae.variables.algebraics.insert(var.name.clone(), var);
    }

    let record_array_fields = build_record_array_field_map(&dae);
    assert_eq!(
        record_array_fields
            .get("ductOut.statesFM.X")
            .map(|entry| entry.field_dims.as_slice()),
        Some(&[2][..])
    );
    let array_dims = build_array_dims_map(&dae);
    let subscript_cases = [
        rumoca_core::Subscript::Index {
            value: 3,
            span: test_span(),
        },
        rumoca_core::Subscript::Expr {
            expr: Box::new(int_lit(3)),
            span: test_span(),
        },
        rumoca_core::Subscript::Expr {
            expr: Box::new(sub(int_lit(4), int_lit(1))),
            span: test_span(),
        },
    ];

    for subscript in subscript_cases {
        let expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::Index {
                base: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::Reference::with_component_reference(
                        "ductOut.statesFM",
                        component_ref(&[("ductOut", None), ("statesFM", None)]),
                    ),
                    subscripts: Vec::new(),
                    span: test_span(),
                }),
                subscripts: vec![subscript],
                span: test_span(),
            }),
            field: "X".to_string(),
            span: test_span(),
        };

        let projected = (0..2)
            .map(|k| {
                project_scalarized_rhs_expr_at(
                    &expr,
                    k,
                    &array_dims,
                    &record_array_fields,
                    &IndexMap::new(),
                )
                .unwrap()
            })
            .collect::<Vec<_>>();

        assert_eq!(
            projected.iter().flat_map(all_var_names).collect::<Vec<_>>(),
            vec!["ductOut.statesFM[3].X[1]", "ductOut.statesFM[3].X[2]"]
        );
    }
}
