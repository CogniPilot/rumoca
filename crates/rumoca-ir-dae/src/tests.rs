use rumoca_core::{
    ClockLattice, ClockRational, SourceId, SourceMap, Span, StructuredIndexBinder,
    StructuredIndexDomain, TypeId, VarName,
};

use crate::*;

mod temporal_wire;

struct TestSource {
    map: SourceMap,
    id: SourceId,
    text: &'static str,
}

impl TestSource {
    fn new(text: &'static str) -> Self {
        let mut map = SourceMap::new();
        let id = map.add("construction.mo", text);
        Self { map, id, text }
    }

    fn span(&self, needle: &str, occurrence: usize) -> Span {
        let start = self
            .text
            .match_indices(needle)
            .nth(occurrence)
            .map(|(start, _)| start)
            .expect("readable fixture contains requested snippet");
        Span::from_offsets(self.id, start, start + needle.len())
    }

    fn source(&self, needle: &str, occurrence: usize) -> DaeProvenance {
        DaeProvenance::source(self.span(needle, occurrence)).expect("fixture span is real")
    }
}

#[test]
fn exact_expression_provenance_resolves_through_the_source_map() {
    let source =
        TestSource::new("Real x; equation x + 2; {x, 2}; 1:3; x[1]; [x for i in 1:3]; abs(x);");
    let declaration = source.source("Real x", 0);
    let x_first = source.source("x", 1);
    let plus = source.source("+", 0);
    let two_first = source.source("2", 0);
    let array = source.source("{x, 2}", 0);
    let range = source.source("1:3", 0);
    let index = source.source("x[1]", 0);
    let subscript = source.source("1", 1);
    let comprehension = source.source("[x for i in 1:3]", 0);
    let builtin = source.source("abs(x)", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                range,
            )
        })?;
        dae.expressions(|expr| {
            let x_node = expr.at(x_first).coordinate(CoordinateInput::Algebraic(x))?;
            let two = expr.at(two_first).literal(DaeLiteral::Real(2.0))?;
            let _sum = expr.at(plus).binary(BinaryOperator::Add, x_node, two)?;
            let _array_node = expr.at(array).array([x_node, two])?;
            let range_node = expr.at(range).range(1, 1, 3)?;
            let one = expr.at(subscript).literal(DaeLiteral::Integer(1))?;
            let _index_node = expr.at(index).index(
                range_node,
                [Subscript::Index {
                    expression: one,
                    provenance: subscript,
                }],
            )?;
            let _comprehension_node = expr.at(comprehension).comprehension(domain, x_node)?;
            let _builtin_node = expr.at(builtin).builtin(PureBuiltin::Abs, [x_node])?;
            Ok(())
        })
    })
    .expect("all expression forms are checked at insertion");

    let expected = [
        "x",
        "2",
        "+",
        "{x, 2}",
        "1:3",
        "1",
        "x[1]",
        "[x for i in 1:3]",
        "abs(x)",
    ];
    dae.inspect(|view| {
        assert_eq!(view.expression_count(), expected.len());
        for (index, expected_text) in expected.iter().enumerate() {
            let expression = view
                .expression(view.expression_id(index).expect("dense expression ID"))
                .expect("expression exists");
            assert_eq!(
                view.source_text(expression.provenance()),
                Some(*expected_text)
            );
        }
        assert_eq!(
            view.source_text(view.subscript_provenance(0).unwrap()),
            Some("1")
        );
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| assert_eq!(view.expression_count(), expected.len()));
}

#[test]
fn explicitly_typed_empty_arrays_round_trip_through_checked_construction() {
    let source = TestSource::new("Real empty[0] = {};");
    let declaration = source.source("Real empty[0]", 0);
    let literal = source.source("{}", 0);
    let dae = Dae::construct(source.map, |dae| {
        let empty_real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::array(ScalarType::Real, [0]),
                declaration,
            )
        })?;
        dae.expressions(|expressions| {
            expressions.at(literal).empty_array(empty_real)?;
            Ok(())
        })
    })
    .expect("the declaration supplies the missing empty-array element type");

    dae.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(expression.value_type().dimensions(), &[0]);
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Array(elements) if elements.is_empty()
        ));
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(expression.value_type().dimensions(), &[0]);
    });
}

#[test]
fn numeric_promotion_is_derived_during_construction() {
    let source = TestSource::new("Real x; equation der(x) = 1; x + 2; if true then 3 else x;");
    let declaration = source.source("Real x", 0);
    let equation = source.source("der(x) = 1", 0);
    let addition = source.source("x + 2", 0);
    let conditional = source.source("if true then 3 else x", 0);
    let derivative_use = source.source("der(x)", 0);
    let one_use = source.source("1", 0);
    let x_use = source.source("x", 2);
    let two_use = source.source("2", 0);
    let true_use = source.source("true", 0);
    let three_use = source.source("3", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (residual, sum, branch) = dae.expressions(|expressions| {
            let derivative = expressions
                .at(derivative_use)
                .coordinate(CoordinateInput::Derivative(state))?;
            let one = expressions.at(one_use).literal(DaeLiteral::Integer(1))?;
            let residual =
                expressions
                    .at(equation)
                    .binary(BinaryOperator::Subtract, derivative, one)?;
            let value = expressions
                .at(x_use)
                .coordinate(CoordinateInput::State(state))?;
            let two = expressions.at(two_use).literal(DaeLiteral::Integer(2))?;
            let sum = expressions
                .at(addition)
                .binary(BinaryOperator::Add, value, two)?;
            let condition = expressions
                .at(true_use)
                .literal(DaeLiteral::Boolean(true))?;
            let three = expressions.at(three_use).literal(DaeLiteral::Integer(3))?;
            let branch = expressions
                .at(conditional)
                .conditional([(condition, three)], value)?;
            Ok((residual, sum, branch))
        })?;
        dae.continuous(|continuous| continuous.value_equation(equation, residual))?;
        let _ = (sum, branch);
        Ok(())
    })
    .expect("valid Modelica Integer-to-Real promotion is constructor-derived");

    dae.inspect(|view| {
        for index in [2, 5, 8] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
        }
    });
}

#[test]
fn structured_families_derive_rows_and_preserve_multidimensional_domains() {
    let source = TestSource::new("for i in 1:2, j in 1:3 loop x = a[i,j]; end for;");
    let owner = source.source("for i in 1:2, j in 1:3 loop x = a[i,j]; end for", 0);
    let indexed_owner = source.source("a[i,j]", 0);
    let i_use = source.source("i", 3);
    let j_use = source.source("j", 1);
    let domain = StructuredIndexDomain {
        binders: vec![
            StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            },
            StructuredIndexBinder {
                id: 1,
                display_name: "j".to_string(),
                lower: 1,
                upper: 3,
                step: 1,
            },
        ],
    };
    let dae = Dae::construct(source.map, |dae| {
        let domain = dae.domains(|domains| domains.structured(domain.clone(), owner))?;
        let (i, j) = dae.domains(|domains| {
            Ok((
                domains.binder(domain, 0, i_use)?,
                domains.binder(domain, 1, j_use)?,
            ))
        })?;
        let (scalar, indexed, aggregate) = dae.expressions(|expressions| {
            let scalar = expressions.at(owner).literal(DaeLiteral::Real(0.0))?;
            let row = expressions.at(owner).array([scalar, scalar, scalar])?;
            let aggregate = expressions.at(owner).array([row, row])?;
            let i = expressions.at(i_use).binder(i)?;
            let j = expressions.at(j_use).binder(j)?;
            let indexed = expressions.at(indexed_owner).index(
                aggregate,
                [
                    Subscript::Index {
                        expression: i,
                        provenance: i_use,
                    },
                    Subscript::Index {
                        expression: j,
                        provenance: j_use,
                    },
                ],
            )?;
            Ok((scalar, indexed, aggregate))
        })?;
        dae.continuous(|continuous| {
            continuous.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(indexed),
            )?;
            continuous.equation(owner, |equation| equation.residual(scalar))?;
            continuous.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                |family| family.body(aggregate),
            )?;
            Ok(())
        })?;
        dae.initialization(|initialization| {
            initialization.equation(owner, |equation| equation.residual(scalar))?;
            initialization.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                |family| family.body(aggregate),
            )?;
            Ok(())
        })
    })
    .unwrap();

    assert_structured_owner_views(&dae);
    let encoded = serde_json::to_string(&dae).unwrap();
    assert_structured_binders_round_trip_and_reject_forgery(&encoded);
}

#[test]
fn binder_prefix_projection_compacts_nested_array_families() {
    let source = TestSource::new("for i in 1:2 loop r[:] = a[i,:]; end for;");
    let owner = source.source("for i in 1:2 loop r[:] = a[i,:]; end for", 0);
    let dae = Dae::construct(source.map, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![
                        StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_string(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        },
                        StructuredIndexBinder {
                            id: 1,
                            display_name: "j".to_string(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        },
                    ],
                },
                owner,
            )
        })?;
        let row = dae.expressions(|expressions| {
            let zero = expressions.at(owner).literal(DaeLiteral::Real(0.0))?;
            expressions.at(owner).array([zero, zero, zero])
        })?;
        dae.continuous(|continuous| {
            continuous.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count: 1 },
                |family| family.body(row),
            )?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let family = view.continuous_family(0).unwrap();
        assert_eq!(family.scalar_rows(), 6);
        let projection = family.scalar_view();
        assert!(matches!(
            projection,
            rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count: 1 }
        ));
        assert_eq!(
            (0..6)
                .map(|point| projection.body_scalar(point, &[2, 3]).unwrap())
                .collect::<Vec<_>>(),
            [0, 1, 2, 0, 1, 2]
        );
    });
}

fn assert_structured_owner_views(dae: &Dae) {
    dae.inspect(|view| {
        assert_eq!(view.domain_count(), 1);
        let domain = view.domain(view.domain_id(0).unwrap()).unwrap();
        assert_eq!(domain.extents(), &[2, 3]);
        assert_eq!(domain.scalar_count(), 6);
        assert_eq!(view.continuous_family_count(), 2);
        assert_eq!(view.continuous_equation_count(), 1);
        assert_eq!(view.continuous_owner_count(), 3);
        assert_eq!(view.initialization_owner_count(), 2);
        assert!(matches!(
            view.continuous_owner(0),
            Some(ContinuousOwnerView::Structured { id, .. }) if id.index() == 0
        ));
        assert!(matches!(
            view.continuous_owner(1),
            Some(ContinuousOwnerView::Residual { id, .. }) if id.index() == 0
        ));
        assert!(matches!(
            view.continuous_owner(2),
            Some(ContinuousOwnerView::Structured { id, .. }) if id.index() == 1
        ));
        assert!(matches!(
            view.initialization_owner(0),
            Some(InitializationOwnerView::Residual { id, .. }) if id.index() == 0
        ));
        assert!(matches!(
            view.initialization_owner(1),
            Some(InitializationOwnerView::Structured { id, .. }) if id.index() == 0
        ));
        for index in 0..2 {
            let family = view.continuous_family(index).unwrap();
            assert_eq!(family.scalar_rows(), 6);
            assert_eq!(family.bodies().len(), 1);
        }
    });
}

fn assert_structured_binders_round_trip_and_reject_forgery(encoded: &str) {
    let decoded: Dae = serde_json::from_str(encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.continuous_family_count(), 2);
        assert_eq!(view.continuous_owner_count(), 3);
        assert_eq!(view.initialization_owner_count(), 2);
        let i = view.expression(view.expression_id(3).unwrap()).unwrap();
        let j = view.expression(view.expression_id(4).unwrap()).unwrap();
        assert!(matches!(
            i.operation(),
            ExpressionOperation::Coordinate(CoordinateView::Binder(binder))
                if binder.ordinal() == 0
        ));
        assert!(matches!(
            j.operation(),
            ExpressionOperation::Coordinate(CoordinateView::Binder(binder))
                if binder.ordinal() == 1
        ));
        assert_eq!(i.binder_domain(), j.binder_domain());
    });

    let mut forged: serde_json::Value = serde_json::from_str(encoded).unwrap();
    forged["storage"]["expressions"]["binder_domains"][3] = serde_json::Value::Null;
    assert!(matches!(
        serde_json::from_value::<Dae>(forged),
        Err(error) if error.to_string().contains("expression shape mismatch")
    ));

    let mut forged: serde_json::Value = serde_json::from_str(encoded).unwrap();
    forged["storage"]["continuous_equation_owners"][1] =
        forged["storage"]["continuous_equation_owners"][0].clone();
    assert!(matches!(
        serde_json::from_value::<Dae>(forged),
        Err(error) if error.to_string().contains("continuous equation owner order")
    ));
}

#[test]
fn domain_binders_cannot_cross_domains_or_escape_structured_owners() {
    let source = TestSource::new(
        "for i in 1:2 loop x[i] = 0; end for; for j in 1:2 loop y[j] = 0; end for;",
    );
    let first_owner = source.source("for i in 1:2 loop x[i] = 0; end for", 0);
    let second_owner = source.source("for j in 1:2 loop y[j] = 0; end for", 0);
    let i_use = source.source("i", 2);
    let j_use = source.source("j", 1);

    let dae = Dae::construct(source.map, |dae| {
        let domain = |id, display_name: &str| StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id,
                display_name: display_name.to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        };
        let (first, second) = dae.domains(|domains| {
            Ok((
                domains.structured(domain(0, "i"), first_owner)?,
                domains.structured(domain(1, "j"), second_owner)?,
            ))
        })?;
        let (i, j) = dae.domains(|domains| {
            Ok((
                domains.binder(first, 0, i_use)?,
                domains.binder(second, 0, j_use)?,
            ))
        })?;
        let unrelated_scope =
            dae.domains(|domains| domains.nested_in_scope([i, j], domain(2, "k"), first_owner));
        assert!(matches!(
            unrelated_scope,
            Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: Some(0),
                found_domain: 1,
                ..
            })
        ));
        let (i, j) = dae.expressions(|expressions| {
            Ok((
                expressions.at(i_use).binder(i)?,
                expressions.at(j_use).binder(j)?,
            ))
        })?;
        let crossed = dae.expressions(|expressions| {
            expressions
                .at(first_owner)
                .binary(BinaryOperator::Add, i, j)
        });
        assert!(matches!(
            crossed,
            Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: Some(0),
                found_domain: 1,
                ..
            })
        ));
        let escaped = dae.continuous(|continuous| {
            continuous.equation(first_owner, |equation| equation.residual(i))
        });
        assert!(matches!(
            escaped,
            Err(DaeConstructionError::InvalidBinderScope {
                expected_domain: None,
                found_domain: 0,
                ..
            })
        ));
        Ok(())
    })
    .expect("rejected binder uses do not create malformed owners");

    dae.inspect(|view| {
        assert_eq!(view.domain_count(), 2);
        assert_eq!(view.continuous_equation_count(), 0);
    });
}

#[test]
fn nested_comprehensions_retain_lexical_scope_provenance_and_wire_identity() {
    let source = TestSource::new("{{i + j for j in 1:3} for i in 1:2}");
    let outer_owner = source.source("{{i + j for j in 1:3} for i in 1:2}", 0);
    let inner_owner = source.source("{i + j for j in 1:3}", 0);
    let outer_range = source.source("1:2", 0);
    let inner_range = source.source("1:3", 0);
    let i_use = source.source("i", 0);
    let j_use = source.source("j", 0);
    let sum_owner = source.source("i + j", 0);
    let singleton_domain = |name: &str, upper| StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    };

    let dae = Dae::construct(source.map, |dae| {
        let outer =
            dae.domains(|domains| domains.structured(singleton_domain("i", 2), outer_range))?;
        let i = dae.domains(|domains| domains.binder(outer, 0, i_use))?;
        let inner = dae.domains(|domains| {
            domains.nested_in_scope([i], singleton_domain("j", 3), inner_range)
        })?;
        let j = dae.domains(|domains| domains.binder(inner, 0, j_use))?;
        dae.expressions(|expressions| {
            let i = expressions.at(i_use).binder(i)?;
            let j = expressions.at(j_use).binder(j)?;
            let sum = expressions
                .at(sum_owner)
                .binary(BinaryOperator::Add, i, j)?;
            let inner_expression = expressions.at(inner_owner).comprehension(inner, sum)?;
            expressions
                .at(outer_owner)
                .comprehension(outer, inner_expression)?;
            Ok(())
        })
    })
    .expect("nested lexical domains are valid by construction");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let outer = view.domain(view.domain_id(0).unwrap()).unwrap();
        let inner = view.domain(view.domain_id(1).unwrap()).unwrap();
        assert_eq!(outer.parent(), None);
        assert_eq!(inner.parent(), view.domain_id(0));
        let inner_expression = view.expression(view.expression_id(3).unwrap()).unwrap();
        let outer_expression = view.expression(view.expression_id(4).unwrap()).unwrap();
        assert_eq!(inner_expression.binder_domain(), view.domain_id(0));
        assert_eq!(outer_expression.binder_domain(), None);
        assert_eq!(inner_expression.value_type().dimensions(), &[3]);
        assert_eq!(outer_expression.value_type().dimensions(), &[2, 3]);
        assert_eq!(
            view.source_text(inner_expression.provenance()),
            Some("{i + j for j in 1:3}")
        );
        assert_eq!(
            view.source_text(outer_expression.provenance()),
            Some("{{i + j for j in 1:3} for i in 1:2}")
        );
    });
}

#[test]
fn variable_occurrences_share_declaration_identity_but_keep_use_spans() {
    let source = TestSource::new("Real x; equation x = x;");
    let declaration = source.source("Real x", 0);
    let first_use = source.source("x", 1);
    let second_use = source.source("x", 2);
    let equation_owner = source.source("x = x", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        dae.continuous(|continuous| {
            continuous.equation(equation_owner, |equation| {
                let mut expr = equation.expressions();
                let lhs = expr
                    .at(first_use)
                    .coordinate(CoordinateInput::Algebraic(x))?;
                let rhs = expr
                    .at(second_use)
                    .coordinate(CoordinateInput::Algebraic(x))?;
                equation.equal(lhs, rhs)?;
                Ok(())
            })
        })?;
        Ok(())
    })
    .expect("equation construction succeeds");

    dae.inspect(|view| {
        let lhs = view.expression(view.expression_id(0).unwrap()).unwrap();
        let rhs = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert!(matches!(
            lhs.operation(),
            ExpressionOperation::Coordinate(CoordinateView::Algebraic(_))
        ));
        assert_eq!(lhs.variable_coordinate(), rhs.variable_coordinate());
        assert_ne!(lhs.provenance(), rhs.provenance());
        assert_eq!(
            view.variable_declaration(lhs.variable_coordinate().unwrap()),
            Some(declaration)
        );

        let equation = view
            .continuous_equation(0)
            .expect("continuous equation exists");
        let residual = view.expression(equation.residual()).unwrap();
        let variable = lhs.variable_coordinate().unwrap();
        assert!(expr_contains_var(view, equation.residual(), variable));
        assert!(expr_refers_to_var(
            view,
            view.expression_id(0).unwrap(),
            variable
        ));
        assert!(!expr_refers_to_var(view, equation.residual(), variable));
        assert!(matches!(
            residual.operation(),
            ExpressionOperation::Binary {
                operator: BinaryOperator::Subtract,
                ..
            }
        ));
        assert_eq!(
            residual.provenance().origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::SyntheticResidual)
        );
        assert_eq!(residual.provenance().span(), equation_owner.span());
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        assert_eq!(view.continuous_equation_count(), 1);
    });
}

#[test]
fn variable_roles_drive_coordinate_construction_and_wire_validation() {
    let source = TestSource::new(
        "parameter Real p; constant Real c; input Real u; Real x; Real y; \
         output Real o; discrete Real z; discrete Boolean m;",
    );
    let p_at = source.source("parameter Real p", 0);
    let c_at = source.source("constant Real c", 0);
    let u_at = source.source("input Real u", 0);
    let x_at = source.source("Real x", 0);
    let y_at = source.source("Real y", 0);
    let o_at = source.source("output Real o", 0);
    let z_at = source.source("discrete Real z", 0);
    let m_at = source.source("discrete Boolean m", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), p_at)
        })?;
        let boolean = dae.types(|types| {
            types.intern(TypeId::new(1), ValueType::scalar(ScalarType::Boolean), m_at)
        })?;
        let (p, c, u, x, y, o, z, m) = dae.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    p_at,
                    VariableAttributes::default(),
                )?,
                variables.constant(VarName::new("c"), real, c_at, VariableAttributes::default())?,
                variables.input(
                    VarName::new("u"),
                    real,
                    InputVariability::Continuous,
                    u_at,
                    VariableAttributes::default(),
                )?,
                variables.state(VarName::new("x"), real, x_at, VariableAttributes::default())?,
                variables.algebraic(
                    VarName::new("y"),
                    real,
                    y_at,
                    VariableAttributes::default(),
                )?,
                variables.output(VarName::new("o"), real, o_at, VariableAttributes::default())?,
                variables.discrete_real(
                    VarName::new("z"),
                    real,
                    z_at,
                    VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("m"),
                    boolean,
                    m_at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        let clock = dae.clocks(|clocks| {
            let clock = clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                z_at,
            )?;
            clocks.own_discrete_real(clock, z, z_at)?;
            Ok(clock)
        })?;
        let previous = dae.temporal(|temporal| temporal.previous_discrete_real(clock, z, z_at))?;
        let m_value = dae.expressions(|expressions| {
            for (at, coordinate) in [
                (p_at, CoordinateInput::Parameter(p)),
                (c_at, CoordinateInput::Parameter(c)),
                (u_at, CoordinateInput::Input(u)),
                (x_at, CoordinateInput::State(x)),
                (x_at, CoordinateInput::Derivative(x)),
                (y_at, CoordinateInput::Algebraic(y)),
                (o_at, CoordinateInput::Algebraic(o)),
                (z_at, CoordinateInput::DiscreteReal(z)),
                (z_at, CoordinateInput::PreDiscreteReal(z)),
                (z_at, CoordinateInput::Previous(previous)),
                (m_at, CoordinateInput::DiscreteValue(m)),
                (m_at, CoordinateInput::PreDiscreteValue(m)),
            ] {
                expressions.at(at).coordinate(coordinate)?;
            }
            expressions.at(m_at).literal(DaeLiteral::Boolean(false))
        })?;
        dae.discrete(|discrete| discrete.assignment(m_at, m, m_value))?;
        Ok(())
    })
    .expect("role-specific coordinates are valid");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.variable_count(), 8);
        assert_eq!(view.expression_count(), 13);
        assert_coordinate_variability(view);
        assert_scalar_parameter_view(view);
    });

    assert_forged_state_role_is_rejected(&encoded);
}

fn assert_scalar_parameter_view(view: DaeView<'_>) {
    let parameter = view.variable(view.variable_id(0).unwrap()).unwrap();
    assert_eq!(parameter.scalar_count(), 1);
    assert_eq!(parameter.scalar_name(0).as_deref(), Some("p"));
    assert_eq!(parameter.scalar_name(1), None);
}

fn assert_forged_state_role_is_rejected(encoded: &str) {
    let forged = encoded.replacen("\"role\":\"state\"", "\"role\":\"algebraic\"", 1);
    assert_ne!(forged, encoded, "wire fixture contains the state role");
    let error = serde_json::from_str::<Dae>(&forged).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("variable `x` has the wrong DAE coordinate role")
    );
}

fn assert_coordinate_variability(view: DaeView<'_>) {
    let variability = (0..13)
        .map(|index| {
            view.expression(view.expression_id(index).unwrap())
                .unwrap()
                .variability()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        variability,
        [
            ExpressionVariability::Parameter,
            ExpressionVariability::Constant,
            ExpressionVariability::Continuous,
            ExpressionVariability::Continuous,
            ExpressionVariability::Continuous,
            ExpressionVariability::Continuous,
            ExpressionVariability::Continuous,
            ExpressionVariability::Discrete,
            ExpressionVariability::Discrete,
            ExpressionVariability::Discrete,
            ExpressionVariability::Discrete,
            ExpressionVariability::Discrete,
            ExpressionVariability::Constant,
        ]
    );
    let state = match view
        .variable(view.variable_id(3).unwrap())
        .unwrap()
        .identity()
    {
        VariableIdentity::State(state) => state,
        _ => panic!("the fourth fixture variable is the state"),
    };
    let derivative = view.expression_id(4).unwrap();
    assert!(expr_contains_der_of(view, derivative, state));
    assert!(expr_contains_der_of_any(view, derivative, |candidate| {
        candidate == state
    }));
}

#[test]
fn every_variable_role_can_reserve_a_header_for_forward_attributes() {
    let source = TestSource::new("Real x(start = y); Real y;");
    let x_at = source.source("Real x(start = y)", 0);
    let y_at = source.source("Real y", 0);
    let y_use = source.source("y", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), x_at)
        })?;
        let (x, x_definition) =
            dae.variables(|variables| variables.reserve_state(VarName::new("x"), real, x_at))?;
        let (y, y_definition) =
            dae.variables(|variables| variables.reserve_algebraic(VarName::new("y"), real, y_at))?;
        let start = dae.expressions(|expressions| {
            expressions
                .at(y_use)
                .coordinate(CoordinateInput::Algebraic(y))
        })?;
        dae.variables(|variables| {
            variables.define(
                x_definition,
                VariableAttributes {
                    binding: Some(start),
                    start: Some(start),
                    ..VariableAttributes::default()
                },
                x_at,
            )?;
            variables.define(y_definition, VariableAttributes::default(), y_at)
        })?;
        dae.expressions(|expressions| {
            expressions.at(x_at).coordinate(CoordinateInput::State(x))?;
            Ok(())
        })
    })
    .expect("forward variable attributes are checked after every header exists");

    dae.inspect(|view| {
        let x = view.variable(view.variable_id(0).unwrap()).unwrap();
        assert_eq!(x.binding(), view.expression_id(0));
        assert_eq!(x.start(), view.expression_id(0));
    });
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let x = view.variable(view.variable_id(0).unwrap()).unwrap();
        assert_eq!(x.binding(), view.expression_id(0));
    });
}

#[test]
fn effective_flat_type_identity_is_not_structurally_merged() {
    let source = TestSource::new("type A = Real; type B = Real;");
    let a_at = source.source("type A = Real", 0);
    let b_at = source.source("type B = Real", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            let a = types.intern(TypeId::new(10), ValueType::scalar(ScalarType::Real), a_at)?;
            let a_again =
                types.intern(TypeId::new(10), ValueType::scalar(ScalarType::Real), a_at)?;
            let b = types.intern(TypeId::new(11), ValueType::scalar(ScalarType::Real), b_at)?;
            assert_eq!(a.index(), a_again.index());
            assert_ne!(a.index(), b.index());
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        assert_eq!(view.value_type_count(), 2);
        assert_eq!(
            view.effective_flat_type(view.value_type_id(0).unwrap()),
            Some(TypeId::new(10))
        );
        assert_eq!(
            view.effective_flat_type(view.value_type_id(1).unwrap()),
            Some(TypeId::new(11))
        );
    });

    let source = TestSource::new("Real");
    let at = source.source("Real", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            types.intern(TypeId::UNKNOWN, ValueType::scalar(ScalarType::Real), at)?;
            Ok(())
        })
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::InvalidEffectiveTypeId { .. }
    ));
}

#[test]
fn every_local_b1c_target_requires_exactly_one_definition() {
    let source = TestSource::new("discrete Boolean m;");
    let declaration = source.source("discrete Boolean m", 0);
    let error = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        Ok(())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "B.1c target",
            ..
        }
    ));
}

#[test]
fn b1c_current_value_dependencies_must_be_acyclic() {
    let source = TestSource::new("discrete Boolean a; discrete Boolean b; equation a = b; b = a;");
    let a_declaration = source.source("discrete Boolean a", 0);
    let b_declaration = source.source("discrete Boolean b", 0);
    let a_assignment = source.source("a = b", 0);
    let b_assignment = source.source("b = a", 0);
    let error = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                a_declaration,
            )
        })?;
        let (a, b) = dae.variables(|variables| {
            Ok((
                variables.discrete_value(
                    VarName::new("a"),
                    boolean,
                    a_declaration,
                    VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("b"),
                    boolean,
                    b_declaration,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        let a_value = dae.expressions(|expressions| {
            expressions
                .at(a_assignment)
                .coordinate(CoordinateInput::DiscreteValue(a))
        })?;
        let b_value = dae.expressions(|expressions| {
            expressions
                .at(b_assignment)
                .coordinate(CoordinateInput::DiscreteValue(b))
        })?;
        dae.discrete(|discrete| {
            discrete.assignment(a_assignment, a, b_value)?;
            discrete.assignment(b_assignment, b, a_value)?;
            Ok(())
        })
    })
    .unwrap_err();

    assert!(matches!(
        error,
        DaeConstructionError::InvalidDiscreteDependencyCycle {
            target,
            span,
        } if target == 0 && span == a_assignment.span()
    ));
}

#[test]
fn functions_conditions_and_generated_runtime_nodes_use_the_same_arena() {
    let source =
        TestSource::new("function f input Real u; output Real y; end f; when x > 0 then end when;");
    let function_declaration = source.source("function f", 0);
    let literal_span = source.source("0", 0);
    let condition_owner = source.source("x > 0", 0);
    let clock_generated = DaeProvenance::generated(
        DaeGeneration::ClockLowering,
        source.span("when x > 0 then end when", 0),
    )
    .unwrap();
    let delay_generated =
        DaeProvenance::generated(DaeGeneration::DelayLowering, source.span("x > 0", 0)).unwrap();

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                function_declaration,
            )
        })?;
        let (function, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [real], [real], function_declaration)
        })?;
        let parameter = dae.functions(|functions| {
            functions.parameter(&reservation, VarName::new("u"), 0, function_declaration)
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, function_declaration)
        })?;
        let parameter_value =
            dae.expressions(|expr| expr.at(function_declaration).function_parameter(parameter))?;
        let literal =
            dae.expressions(|expr| expr.at(literal_span).literal(DaeLiteral::Real(0.0)))?;
        let mut body =
            dae.functions(|functions| functions.begin(reservation, function_declaration))?;
        dae.functions(|functions| {
            functions.assign(&mut body, output, parameter_value, function_declaration)
        })?;
        dae.functions(|functions| functions.define(body, function_declaration))?;

        let condition = dae.conditions(|conditions| conditions.reserve(condition_owner))?;
        let condition_value =
            dae.expressions(|expr| expr.at(condition_owner).literal(DaeLiteral::Boolean(true)))?;
        dae.conditions(|conditions| {
            conditions.define(
                condition,
                ConditionInput::Discrete(condition_value),
                condition_owner,
            )
        })?;
        let delay_time =
            dae.expressions(|expr| expr.at(delay_generated).literal(DaeLiteral::Real(1.0)))?;
        let delay = dae.temporal(|temporal| {
            let positive = temporal.positive_parameter(delay_time, 1.0, delay_generated)?;
            temporal.delay(literal, positive, delay_generated)
        })?;

        dae.expressions(|expr| {
            let _call = expr.at(function_declaration).call(function, 0, [literal])?;
            let _condition = expr
                .at(condition_owner)
                .coordinate(CoordinateInput::Condition(condition))?;
            let _clock = expr.at(clock_generated).coordinate(CoordinateInput::Time)?;
            let _delay = expr
                .at(delay_generated)
                .coordinate(CoordinateInput::Delay(delay))?;
            let duplicate_delay = expr
                .at(delay_generated)
                .coordinate(CoordinateInput::Delay(delay));
            assert!(matches!(
                duplicate_delay,
                Err(DaeConstructionError::DuplicateDefinition {
                    kind: "delay coordinate",
                    ..
                })
            ));
            Ok(())
        })
    })
    .expect("all owners share one arena");

    dae.inspect(assert_function_runtime_arena);

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.expression_count(), 8);
        assert_eq!(view.variable_count(), 0);
    });
}

fn assert_function_runtime_arena(view: DaeView<'_>) {
    assert_eq!(view.expression_count(), 8);
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    assert_eq!(function.name().as_str(), "f");
    assert_eq!(function.parameter_types().len(), 1);
    assert_eq!(function.result_types().len(), 1);
    assert_eq!(function.result_values().len(), 1);
    let result = view
        .expression(function.result_values().get(0).unwrap())
        .unwrap();
    assert_eq!(result.function_scope(), view.function_id(0));
    let condition = view.condition(view.condition_id(0).unwrap()).unwrap();
    assert!(matches!(
        condition.operation(),
        ConditionOperation::Discrete(_)
    ));
    let delay = view.delay(view.delay_id(0).unwrap()).unwrap();
    assert_eq!(delay.delay_time_evidence().unwrap().value(), 1.0);
    assert_eq!(
        view.expression(view.expression_id(6).unwrap())
            .unwrap()
            .provenance()
            .origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::ClockLowering)
    );
    assert_eq!(
        view.expression(view.expression_id(7).unwrap())
            .unwrap()
            .provenance()
            .origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::DelayLowering)
    );
}

#[test]
fn function_parameters_cannot_cross_or_escape_semantic_owners() {
    let source = TestSource::new("function f end f; function g end g;");
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let result = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let (_f, f_reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [real], [real], f_at)
        })?;
        let (_g, g_reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("g"), [real], [real], g_at)
        })?;
        let f_parameter = dae.functions(|functions| {
            functions.parameter(&f_reservation, VarName::new("u"), 0, f_at)
        })?;
        let g_parameter = dae.functions(|functions| {
            functions.parameter(&g_reservation, VarName::new("u"), 0, g_at)
        })?;
        let f_output = dae
            .functions(|functions| functions.output(&f_reservation, VarName::new("y"), 0, f_at))?;
        let g_output = dae
            .functions(|functions| functions.output(&g_reservation, VarName::new("y"), 0, g_at))?;
        let mut f_body = dae.functions(|functions| functions.begin(f_reservation, f_at))?;
        let mut g_body = dae.functions(|functions| functions.begin(g_reservation, g_at))?;
        let f_value = dae.expressions(|expr| expr.at(f_at).function_parameter(f_parameter))?;
        let g_value = dae.expressions(|expr| expr.at(g_at).function_parameter(g_parameter))?;
        let error =
            dae.expressions(|expr| expr.at(f_at).binary(BinaryOperator::Add, f_value, g_value));
        assert!(matches!(
            error,
            Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(_),
                ..
            })
        ));
        dae.functions(|functions| functions.assign(&mut f_body, f_output, f_value, f_at))?;
        dae.functions(|functions| functions.assign(&mut g_body, g_output, g_value, g_at))?;
        dae.functions(|functions| functions.define(f_body, f_at))?;
        dae.functions(|functions| functions.define(g_body, g_at))?;
        dae.continuous(|continuous| continuous.value_equation(f_at, f_value))
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: None,
            ..
        })
    ));
}

#[test]
fn function_locals_keep_ordered_statements_and_exact_use_provenance() {
    let source = TestSource::new(
        "function f\n input Real u;\n output Real y;\n protected Real z;\nalgorithm\n z := u + 1;\n y := z * 2;\nend f;",
    );
    let function_at = source.source("function f", 0);
    let parameter_at = source.source("input Real u", 0);
    let output_at = source.source("output Real y", 0);
    let local_at = source.source("Real z", 0);
    let first_assignment = source.source("z := u + 1", 0);
    let second_assignment = source.source("y := z * 2", 0);
    let parameter_use = source.source("u", 1);
    let one_at = source.source("1", 0);
    let first_rhs = source.source("u + 1", 0);
    let local_use = source.source("z", 2);
    let two_at = source.source("2", 0);
    let second_rhs = source.source("z * 2", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let (_function, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [real], [real], function_at)
        })?;
        let parameter = dae.functions(|functions| {
            functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, output_at)
        })?;
        let local = dae.functions(|functions| {
            functions.local(&reservation, VarName::new("z"), real, local_at)
        })?;
        let mut body = dae.functions(|functions| functions.begin(reservation, function_at))?;
        let parameter = dae.expressions(|expressions| {
            expressions.at(parameter_use).function_parameter(parameter)
        })?;
        let one =
            dae.expressions(|expressions| expressions.at(one_at).literal(DaeLiteral::Real(1.0)))?;
        let local_definition = dae.expressions(|expressions| {
            expressions
                .at(first_rhs)
                .binary(BinaryOperator::Add, parameter, one)
        })?;
        dae.functions(|functions| {
            functions.assign(&mut body, local, local_definition, first_assignment)
        })?;
        let local_value = dae.functions(|functions| functions.read(&body, local, local_use))?;
        let two =
            dae.expressions(|expressions| expressions.at(two_at).literal(DaeLiteral::Real(2.0)))?;
        let output_definition = dae.expressions(|expressions| {
            expressions
                .at(second_rhs)
                .binary(BinaryOperator::Multiply, local_value, two)
        })?;
        dae.functions(|functions| {
            functions.assign(&mut body, output, output_definition, second_assignment)
        })?;
        dae.functions(|functions| functions.define(body, function_at))
    })
    .expect("ordered local definitions construct a complete function");

    dae.inspect(|view| assert_function_local_body(view, local_use));
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| assert_function_local_body(view, local_use));
    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let nodes = forged["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap();
    let local_read = nodes
        .iter_mut()
        .find_map(|node| node.get_mut("function_value"))
        .expect("fixture contains a function-value read");
    local_read["definition"] = serde_json::json!(0);
    let error = serde_json::from_value::<Dae>(forged).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("function value 1 reads definition 0"),
        "wire reconstruction must reject forged function snapshots: {error}"
    );
}

fn assert_function_local_body(view: DaeView<'_>, local_use: DaeProvenance) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let parameters = function.parameters().collect::<Vec<_>>();
    assert_eq!(parameters.len(), 1);
    assert_eq!(parameters[0].name().as_str(), "u");
    assert_eq!(
        view.source_text(parameters[0].declaration()),
        Some("input Real u"),
        "function inputs retain declaration provenance"
    );
    let values = function.values().collect::<Vec<_>>();
    assert_eq!(values.len(), 2);
    assert_eq!(values[0].name().as_str(), "y");
    assert_eq!(values[0].role(), FunctionValueRole::Output);
    assert_eq!(values[1].name().as_str(), "z");
    assert_eq!(values[1].role(), FunctionValueRole::Local);
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    let result = view
        .expression(function.result_values().get(0).unwrap())
        .unwrap();
    let ExpressionOperation::Binary { lhs, .. } = result.operation() else {
        panic!("final function output must retain its checked expression");
    };
    let local = view.expression(lhs).unwrap();
    assert_eq!(local.provenance(), local_use);
    let ExpressionOperation::FunctionValue { value, definition } = local.operation() else {
        panic!("local occurrence must retain its declaration and snapshot definition");
    };
    assert_eq!(value.function(), function.id());
    assert_eq!(value.ordinal(), 1);
    assert_eq!(
        view.source_text(local.provenance()),
        Some("z"),
        "the local read keeps its source occurrence"
    );
    assert!(view.expression(definition).is_some());
}

#[test]
fn zeros_is_a_provenance_bearing_checked_array_operation() {
    let source = TestSource::new("Real z[2]; algorithm z := zeros(2);");
    let extent_at = source.source("2", 1);
    let zeros_at = source.source("zeros(2)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(2)))?;
        dae.expressions(|expressions| {
            expressions
                .at(zeros_at)
                .builtin(PureBuiltin::Zeros, [extent])
        })?;
        Ok(())
    })
    .expect("literal zeros dimensions construct a checked array");

    let assert_zeros = |view: DaeView<'_>| {
        let expression = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(expression.value_type().dimensions(), &[2]);
        assert_eq!(view.source_text(expression.provenance()), Some("zeros(2)"));
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Zeros,
                ..
            }
        ));
    };
    dae.inspect(assert_zeros);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_zeros);

    let source = TestSource::new("zeros(-1)");
    let negative_at = source.source("-1", 0);
    let invalid_zeros_at = source.source("zeros(-1)", 0);
    let error = Dae::construct(source.map, |dae| {
        let extent = dae.expressions(|expressions| {
            expressions.at(negative_at).literal(DaeLiteral::Integer(-1))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(invalid_zeros_at)
                .builtin(PureBuiltin::Zeros, [extent])
        })?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
}

#[test]
fn ones_and_fill_are_compact_typed_array_operations() {
    let source = TestSource::new("ones(2, 2); fill(0.5, 3)");
    let ones_at = source.source("ones(2, 2)", 0);
    let fill_at = source.source("fill(0.5, 3)", 0);
    let first_two_at = source.source("2", 0);
    let second_two_at = source.source("2", 1);
    let value_at = source.source("0.5", 0);
    let extent_at = source.source("3", 0);
    let dae = Dae::construct(source.map, |dae| {
        let first_two = dae.expressions(|expressions| {
            expressions.at(first_two_at).literal(DaeLiteral::Integer(2))
        })?;
        let second_two = dae.expressions(|expressions| {
            expressions
                .at(second_two_at)
                .literal(DaeLiteral::Integer(2))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(ones_at)
                .builtin(PureBuiltin::Ones, [first_two, second_two])
        })?;
        let value =
            dae.expressions(|expressions| expressions.at(value_at).literal(DaeLiteral::Real(0.5)))?;
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| {
            expressions
                .at(fill_at)
                .builtin(PureBuiltin::Fill, [value, extent])
        })?;
        Ok(())
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, dimensions, text) in [
            (2, PureBuiltin::Ones, &[2, 2][..], "ones(2, 2)"),
            (5, PureBuiltin::Fill, &[3][..], "fill(0.5, 3)"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
            assert_eq!(expression.value_type().dimensions(), dimensions);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });
}

#[test]
fn linspace_and_cross_are_checked_compact_vector_operations() {
    let source = TestSource::new("linspace(2.0, 4.0, 3); cross({1.0,2.0,3.0},{4.0,5.0,6.0})");
    let linspace_at = source.source("linspace(2.0, 4.0, 3)", 0);
    let cross_at = source.source("cross({1.0,2.0,3.0},{4.0,5.0,6.0})", 0);
    let provenances = [
        source.source("2.0", 0),
        source.source("4.0", 0),
        source.source("3", 0),
        source.source("1.0", 0),
        source.source("2.0", 1),
        source.source("3.0", 0),
        source.source("4.0", 1),
        source.source("5.0", 0),
        source.source("6.0", 0),
    ];
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions
                .at(provenances[0])
                .literal(DaeLiteral::Real(2.0))?;
            let stop = expressions
                .at(provenances[1])
                .literal(DaeLiteral::Real(4.0))?;
            let count = expressions
                .at(provenances[2])
                .literal(DaeLiteral::Integer(3))?;
            expressions
                .at(linspace_at)
                .builtin(PureBuiltin::Linspace, [start, stop, count])?;
            let lhs = provenances[3..6]
                .iter()
                .zip([1.0, 2.0, 3.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(cross_at).array(lhs)?;
            let rhs = provenances[6..]
                .iter()
                .zip([4.0, 5.0, 6.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let rhs = expressions.at(cross_at).array(rhs)?;
            expressions
                .at(cross_at)
                .builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, text) in [
            (3, PureBuiltin::Linspace, "linspace(2.0, 4.0, 3)"),
            (12, PureBuiltin::Cross, "cross({1.0,2.0,3.0},{4.0,5.0,6.0})"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().dimensions(), &[3]);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });
}

#[test]
fn enumeration_literals_are_canonical_checked_integers_and_round_trip() {
    let source = TestSource::new("E.a");
    let literal_at = source.source("E.a", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| expressions.at(literal_at).enumeration_literal(1))?;
        Ok(())
    })
    .expect("positive enumeration ordinals construct");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Integer);
        assert!(expression.value_type().dimensions().is_empty());
        assert_eq!(view.source_text(expression.provenance()), Some("E.a"));
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Literal(DaeLiteral::Enumeration(1))
        ));
    });

    let source = TestSource::new("E.invalid");
    let invalid_at = source.source("E.invalid", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| expressions.at(invalid_at).enumeration_literal(0))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::InvalidEnumerationOrdinal { ordinal: 0, .. })
    ));
}

#[test]
fn function_for_loop_is_a_compact_checked_transition() {
    let source = TestSource::new(
        "function sum3\n output Real y;\nalgorithm\n y := 0;\n for k in 1:3 loop\n  y := y + k;\n end for;\nend sum3;",
    );
    let function_at = source.source("function sum3", 0);
    let output_at = source.source("output Real y", 0);
    let initial_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:3 loop", 0);
    let update_at = source.source("y := y + k", 0);
    let y_use_at = source.source("y", 3);
    let k_use_at = source.source("k", 1);
    let update_value_at = source.source("y + k", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let (_function, reservation) = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("sum3"), [], [real], function_at)
        })?;
        let output = dae.functions(|functions| {
            functions.output(&reservation, VarName::new("y"), 0, output_at)
        })?;
        let mut body = dae.functions(|functions| functions.begin(reservation, function_at))?;
        let zero =
            dae.expressions(|expressions| expressions.at(zero_at).literal(DaeLiteral::Real(0.0)))?;
        dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "k".to_string(),
                        lower: 1,
                        upper: 3,
                        step: 1,
                    }],
                },
                loop_at,
            )
        })?;
        let binder = DomainBinderId::from_raw(domain.index(), 0);
        let mut loop_body =
            dae.functions(|functions| functions.begin_loop(&body, domain, [output], loop_at))?;
        let current =
            dae.functions(|functions| functions.read(loop_body.body(), output, y_use_at))?;
        let k = dae.expressions(|expressions| expressions.at(k_use_at).binder(binder))?;
        let update = dae.expressions(|expressions| {
            expressions
                .at(update_value_at)
                .binary(BinaryOperator::Add, current, k)
        })?;
        dae.functions(|functions| {
            functions.assign_loop(&mut loop_body, output, update, update_at)
        })?;
        dae.functions(|functions| functions.finish_loop(&mut body, loop_body, loop_at))?;
        dae.functions(|functions| functions.define(body, function_at))
    })
    .expect("loop-carried function state constructs as a checked fold");

    dae.inspect(assert_sum3_loop);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_sum3_loop);

    let mut missing_parameter: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    missing_parameter["storage"]["function_folds"][0]["parameter_values"] = serde_json::json!([]);
    assert!(
        serde_json::from_value::<Dae>(missing_parameter).is_err(),
        "wire reconstruction rejects a missing loop-transition parameter"
    );

    let mut open_initial: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let parameter = open_initial["storage"]["function_folds"][0]["parameter_values"][0].clone();
    open_initial["storage"]["function_folds"][0]["initial_values"][0] = parameter;
    let error = serde_json::from_value::<Dae>(open_initial).unwrap_err();
    assert!(
        error.to_string().contains("domain binder"),
        "wire reconstruction rejects a binder-dependent initial value: {error}"
    );
}

fn assert_sum3_loop(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    let FunctionStatementView::For {
        fold,
        statements,
        provenance,
    } = statements[1].clone()
    else {
        panic!("second function statement is the compact source loop");
    };
    assert_eq!(view.source_text(provenance), Some("for k in 1:3 loop"));
    assert_eq!(statements.count(), 1);
    let fold = view.function_fold(fold).unwrap();
    assert_eq!(fold.targets().count(), 1);
    assert_eq!(fold.initial_values().len(), 1);
    assert_eq!(fold.update_values().len(), 1);
    assert_eq!(
        view.expression(function.result_values().get(0).unwrap())
            .unwrap()
            .kind(),
        ExpressionKind::FunctionFoldOutput
    );
    let update = view
        .expression(fold.update_values().get(0).unwrap())
        .unwrap();
    assert_eq!(view.source_text(update.provenance()), Some("y + k"));
}

#[test]
fn delay_evidence_rejects_nonpositive_and_unconsumed_channels() {
    let source = TestSource::new("delay(x, 0.0)");
    let owner = source.source("delay(x, 0.0)", 0);
    let literal_at = source.source("0.0", 0);
    let error = Dae::construct(source.map, |dae| {
        let (source, delay_time) = dae.expressions(|expressions| {
            Ok((
                expressions.at(owner).literal(DaeLiteral::Real(1.0))?,
                expressions.at(literal_at).literal(DaeLiteral::Real(0.0))?,
            ))
        })?;
        dae.temporal(|temporal| {
            let rejected = temporal.positive_parameter(delay_time, 0.0, literal_at);
            assert!(matches!(
                rejected,
                Err(DaeConstructionError::InvalidPositiveParameter { .. })
            ));
            let positive = temporal.positive_parameter(source, 1.0, owner)?;
            temporal.delay(source, positive, owner)?;
            Ok(())
        })
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::IncompleteDefinition {
            kind: "delay coordinate",
            ..
        }
    ));
}

#[test]
fn roots_accept_only_closed_primitive_relations() {
    let source = TestSource::new("Real x; when x > 0 then end when;");
    let declaration = source.source("Real x", 0);
    let x_use = source.source("x", 1);
    let zero_use = source.source("0", 0);
    let relation_owner = source.source("x > 0", 0);
    let when_owner = source.source("when x > 0 then end when", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let x = dae.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (relation_expression, boolean_literal) = dae.expressions(|expressions| {
            let x = expressions
                .at(x_use)
                .coordinate(CoordinateInput::Algebraic(x))?;
            let zero = expressions.at(zero_use).literal(DaeLiteral::Real(0.0))?;
            let relation =
                expressions
                    .at(relation_owner)
                    .binary(BinaryOperator::Greater, x, zero)?;
            let literal = expressions
                .at(when_owner)
                .literal(DaeLiteral::Boolean(true))?;
            Ok((relation, literal))
        })?;

        let rejected =
            dae.conditions(|conditions| conditions.relation(boolean_literal, when_owner));
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::ExpectedPrimitiveRelation { .. })
        ));

        let (relation, activation) = dae.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        dae.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        Ok(())
    })
    .expect("the root system enforces primitive and closed inputs");

    dae.inspect(|view| {
        assert_eq!(view.relation_count(), 1);
        assert_eq!(view.condition_count(), 1);
        assert_eq!(view.root_count(), 1);
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.relation_count(), 1);
        assert_eq!(view.condition_count(), 1);
        assert_eq!(view.root_count(), 1);
    });
}

#[test]
fn event_actions_are_guarded_typed_and_keep_coincident_time_ids() {
    let source = TestSource::new(
        "Real x; when trigger then reinit(x, 1); assert(trigger, \"safe\"); end when;",
    );
    let declaration = source.source("Real x", 0);
    let guard_owner = source.source("trigger", 0);
    let action_owner = source.source("reinit(x, 1)", 0);
    let assert_owner = source.source("assert(trigger, \"safe\")", 0);
    let value_owner = source.source("1", 0);
    let message_owner = source.source("\"safe\"", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                declaration,
            )
        })?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (trigger, branch_guard) = dae.conditions(|conditions| {
            Ok((
                conditions.reserve(guard_owner)?,
                conditions.reserve(action_owner)?,
            ))
        })?;
        let (trigger_value, branch_value, value, message) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(guard_owner)
                    .literal(DaeLiteral::Boolean(true))?,
                expressions
                    .at(action_owner)
                    .literal(DaeLiteral::Boolean(false))?,
                expressions.at(value_owner).literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(message_owner)
                    .literal(DaeLiteral::String("safe".to_string()))?,
            ))
        })?;
        dae.conditions(|conditions| {
            conditions.define(
                trigger,
                ConditionInput::Discrete(trigger_value),
                guard_owner,
            )?;
            conditions.define(
                branch_guard,
                ConditionInput::Discrete(branch_value),
                action_owner,
            )
        })?;
        dae.events(|events| {
            let instant = ClockRational::new(1, 2).unwrap();
            let first = events.time_event(instant, action_owner)?;
            let second = events.time_event(instant, action_owner)?;
            assert_ne!(first.index(), second.index());
            events.reinitialize(trigger, branch_guard, state, value, action_owner)?;
            events.assert(trigger, branch_guard, message, assert_owner)?;
            Ok(())
        })
    })
    .expect("event actions are checked at their owner boundary");

    dae.inspect(|view| {
        assert_eq!(view.time_event_count(), 2);
        assert_eq!(view.event_action_count(), 2);
        assert_eq!(
            view.time_event(view.time_event_id(0).unwrap())
                .unwrap()
                .instant(),
            &ClockRational::new(1, 2).unwrap()
        );
        let reinitialize = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_ne!(reinitialize.trigger().index(), reinitialize.guard().index());
        assert!(matches!(
            reinitialize.operation(),
            EventActionOperation::Reinitialize { .. }
        ));
        let assertion = view.event_action(view.event_action_id(1).unwrap()).unwrap();
        assert!(matches!(
            assertion.operation(),
            EventActionOperation::Assert { .. }
        ));
    });
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.time_event_count(), 2);
        assert_eq!(view.event_action_count(), 2);
        let action = view.event_action(view.event_action_id(0).unwrap()).unwrap();
        assert_ne!(action.trigger().index(), action.guard().index());
    });

    assert_event_wire_requires_trigger(&encoded);
}

#[test]
fn initial_condition_is_typed_and_round_trips_through_wire_v11() {
    let source = TestSource::new("when initial() then end when;");
    let initial_at = source.source("initial()", 0);
    let dae = Dae::construct(source.map, |dae| {
        let condition = dae.conditions(|conditions| conditions.reserve(initial_at))?;
        dae.conditions(|conditions| {
            conditions.define(condition, ConditionInput::Initial, initial_at)
        })
    })
    .unwrap();

    let assert_initial = |model: &Dae| {
        model.inspect(|view| {
            let condition = view
                .condition(view.condition_id(0).unwrap())
                .expect("initial condition identity resolves");
            assert!(matches!(condition.operation(), ConditionOperation::Initial));
            assert_eq!(model.source_text(condition.provenance()), Some("initial()"));
        });
    };
    assert_initial(&dae);
    let wire = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&wire).unwrap();
    assert_initial(&decoded);
}

fn assert_event_wire_requires_trigger(encoded: &str) {
    let mut value: serde_json::Value = serde_json::from_str(encoded).unwrap();
    value["storage"]["event_actions"][0]
        .as_object_mut()
        .unwrap()
        .remove("trigger");
    let error = serde_json::from_value::<Dae>(value).unwrap_err();
    assert!(error.to_string().contains("missing field `trigger`"));
}
