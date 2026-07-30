mod derived_wire;
mod function_owners;
mod function_wire;
mod provenance;
mod range_wire;
mod runtime_owners;
mod string_conversion;
mod temporal_wire;
mod type_arena;
mod wire_buffers;

use rumoca_core::{
    ClockLattice, ClockRational, SourceId, SourceMap, Span, StructuredIndexBinder,
    StructuredIndexDomain, TypeId, VarName,
};

use crate::*;

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
fn record_layout_values_and_field_uses_round_trip_through_checked_wire() {
    let source =
        TestSource::new("record Pair Real left; Real right; end Pair; Pair(1, 2); Pair(3, 4).left");
    let real_at = source.source("Real left", 0);
    let record_at = source.source("record Pair Real left; Real right; end Pair", 0);
    let constructor_at = source.source("Pair(1, 2)", 0);
    let projection_at = source.source("Pair(3, 4).left", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), real_at))?;
        let record = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                record_at,
            )
        })?;
        dae.expressions(|expressions| {
            let one = expressions
                .at(constructor_at)
                .literal(DaeLiteral::Integer(1))?;
            let two = expressions
                .at(constructor_at)
                .literal(DaeLiteral::Integer(2))?;
            expressions.at(constructor_at).record(record, [one, two])
        })?;
        dae.expressions(|expressions| {
            let three = expressions
                .at(projection_at)
                .literal(DaeLiteral::Integer(3))?;
            let four = expressions
                .at(projection_at)
                .literal(DaeLiteral::Integer(4))?;
            let base = expressions
                .at(projection_at)
                .record(record, [three, four])?;
            expressions.at(projection_at).field(base, 0).map(|_| ())
        })
    })
    .expect("record owners construct through checked operations");

    dae.inspect(assert_record_round_trip);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_record_round_trip);

    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    forged["storage"]["value_types"][1]["record_fields"][0]["value_type"] = serde_json::json!(1);
    let error = serde_json::from_value::<Dae>(forged).unwrap_err();
    assert!(
        error.to_string().contains("value_types.record_fields"),
        "wire rejects a cyclic record base before insertion: {error}"
    );
}

fn assert_record_round_trip(view: DaeView<'_>) {
    let record_id = view.value_type_id(1).expect("record type remains dense");
    let record = view.value_type(record_id).expect("record type resolves");
    assert_eq!(record.record_name().unwrap().as_str(), "Pair");
    assert_eq!(view.record_field(record_id, 0).unwrap().0.as_str(), "left");
    assert_eq!(view.record_field(record_id, 1).unwrap().0.as_str(), "right");
    let binding = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| {
            view.source_text(expression.provenance()) == Some("Pair(1, 2)")
                && matches!(expression.operation(), ExpressionOperation::Record(_))
        })
        .expect("record value survives");
    assert!(matches!(
        binding.operation(),
        ExpressionOperation::Record(_)
    ));
    assert_eq!(view.source_text(binding.provenance()), Some("Pair(1, 2)"));
    let projection = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| {
            view.source_text(expression.provenance()) == Some("Pair(3, 4).left")
                && matches!(
                    expression.operation(),
                    ExpressionOperation::Field { field: 0, .. }
                )
        })
        .expect("field use survives");
    assert!(matches!(
        projection.operation(),
        ExpressionOperation::Field { field: 0, .. }
    ));
}

#[test]
fn record_type_rejects_unknown_provenance_before_insertion() {
    let source = TestSource::new("record Pair Real left; end Pair");
    let real_at = source.source("Real left", 0);
    let foreign_span = Span::from_offsets(SourceId::from_source_name("foreign.mo"), 0, 1);
    let foreign = DaeProvenance::source(foreign_span).expect("foreign span is non-dummy");
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), real_at))?;
        let rejected = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real)],
                foreign,
            )
        });
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::UnknownSource { span }) if span == foreign_span
        ));
        Ok(())
    })
    .expect("rejected record provenance leaves no partial owner");
    assert_eq!(dae.inspect(|view| view.value_type_count()), 1);
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
    let range_start = source.source("1", 0);
    let range_stop = source.source("3", 0);
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
            let range_start = expr.at(range_start).literal(DaeLiteral::Integer(1))?;
            let range_stop = expr.at(range_stop).literal(DaeLiteral::Integer(3))?;
            let range_node = expr.at(range).range(range_start, None, range_stop)?;
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
        "1",
        "3",
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
fn no_event_preserves_a_boolean_operand_and_exact_provenance() {
    let source = TestSource::new("Boolean quiet = noEvent(true);");
    let literal_at = source.source("true", 0);
    let no_event_at = source.source("noEvent(true)", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let value = expressions
                .at(literal_at)
                .literal(DaeLiteral::Boolean(true))?;
            expressions
                .at(no_event_at)
                .builtin(PureBuiltin::NoEvent, [value])?;
            Ok(())
        })
    })
    .expect("noEvent is a type-preserving checked expression");

    dae.inspect(|view| {
        let expression = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Boolean);
        assert_eq!(
            view.source_text(expression.provenance()),
            Some("noEvent(true)")
        );
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::NoEvent,
                ..
            }
        ));
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
    forged["storage"]["expressions"]["nodes"][3]["coordinate"]["binder"]["ordinal"] =
        serde_json::json!(2);
    assert!(serde_json::from_value::<Dae>(forged).is_err());

    let mut forged: serde_json::Value = serde_json::from_str(encoded).unwrap();
    forged["storage"]["continuous_equation_operations"][1]["residual"]["residual"] =
        serde_json::json!(u32::MAX);
    assert!(
        serde_json::from_value::<Dae>(forged).is_err(),
        "a residual operation cannot name an unknown expression"
    );
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

#[derive(Clone, Copy)]
struct VariableRoleSpans {
    parameter: DaeProvenance,
    constant: DaeProvenance,
    input: DaeProvenance,
    state: DaeProvenance,
    algebraic: DaeProvenance,
    output: DaeProvenance,
    discrete_real: DaeProvenance,
    discrete_value: DaeProvenance,
}

#[derive(Clone, Copy)]
struct VariableRoleIds<'dae> {
    parameter: ParameterId<'dae>,
    constant: ParameterId<'dae>,
    input: InputId<'dae>,
    state: StateId<'dae>,
    algebraic: AlgebraicId<'dae>,
    output: AlgebraicId<'dae>,
    discrete_real: DiscreteRealId<'dae>,
    discrete_value: DiscreteValueId<'dae>,
}

fn variable_role_spans(source: &TestSource) -> VariableRoleSpans {
    VariableRoleSpans {
        parameter: source.source("parameter Real p", 0),
        constant: source.source("constant Real c", 0),
        input: source.source("input Real u", 0),
        state: source.source("Real x", 0),
        algebraic: source.source("Real y", 0),
        output: source.source("output Real o", 0),
        discrete_real: source.source("discrete Real z", 0),
        discrete_value: source.source("discrete Boolean m", 0),
    }
}

fn define_variable_role_catalog<'dae>(
    dae: &mut DaeConstruction<'dae>,
    spans: VariableRoleSpans,
) -> Result<VariableRoleIds<'dae>, DaeConstructionError> {
    let real = dae.types(|types| {
        types.intern(
            TypeId::new(0),
            ValueType::scalar(ScalarType::Real),
            spans.parameter,
        )
    })?;
    let boolean = dae.types(|types| {
        types.intern(
            TypeId::new(1),
            ValueType::scalar(ScalarType::Boolean),
            spans.discrete_value,
        )
    })?;
    dae.variables(|variables| {
        Ok(VariableRoleIds {
            parameter: variables.parameter(
                VarName::new("p"),
                real,
                spans.parameter,
                VariableAttributes::default(),
            )?,
            constant: variables.constant(
                VarName::new("c"),
                real,
                spans.constant,
                VariableAttributes::default(),
            )?,
            input: variables.input(
                VarName::new("u"),
                real,
                InputVariability::Continuous,
                spans.input,
                VariableAttributes::default(),
            )?,
            state: variables.state(
                VarName::new("x"),
                real,
                spans.state,
                VariableAttributes::default(),
            )?,
            algebraic: variables.algebraic(
                VarName::new("y"),
                real,
                spans.algebraic,
                VariableAttributes::default(),
            )?,
            output: variables.output(
                VarName::new("o"),
                real,
                spans.output,
                VariableAttributes::default(),
            )?,
            discrete_real: variables.discrete_real(
                VarName::new("z"),
                real,
                spans.discrete_real,
                VariableAttributes::default(),
            )?,
            discrete_value: variables.discrete_value(
                VarName::new("m"),
                boolean,
                spans.discrete_value,
                VariableAttributes::default(),
            )?,
        })
    })
}

fn add_variable_role_coordinates<'dae>(
    dae: &mut DaeConstruction<'dae>,
    variables: VariableRoleIds<'dae>,
    spans: VariableRoleSpans,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    let clock = dae.clocks(|clocks| {
        let clock = clocks.periodic(
            ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
            spans.discrete_real,
        )?;
        clocks.own_discrete_real(clock, variables.discrete_real, spans.discrete_real)?;
        Ok(clock)
    })?;
    let previous = dae.temporal(|temporal| {
        temporal.previous_discrete_real(clock, variables.discrete_real, spans.discrete_real)
    })?;
    dae.expressions(|expressions| {
        for (at, coordinate) in [
            (
                spans.parameter,
                CoordinateInput::Parameter(variables.parameter),
            ),
            (
                spans.constant,
                CoordinateInput::Parameter(variables.constant),
            ),
            (spans.input, CoordinateInput::Input(variables.input)),
            (spans.state, CoordinateInput::State(variables.state)),
            (spans.state, CoordinateInput::Derivative(variables.state)),
            (
                spans.algebraic,
                CoordinateInput::Algebraic(variables.algebraic),
            ),
            (spans.output, CoordinateInput::Algebraic(variables.output)),
            (
                spans.discrete_real,
                CoordinateInput::DiscreteReal(variables.discrete_real),
            ),
            (
                spans.discrete_real,
                CoordinateInput::PreDiscreteReal(variables.discrete_real),
            ),
            (spans.discrete_real, CoordinateInput::Previous(previous)),
            (
                spans.discrete_value,
                CoordinateInput::DiscreteValue(variables.discrete_value),
            ),
            (
                spans.discrete_value,
                CoordinateInput::PreDiscreteValue(variables.discrete_value),
            ),
        ] {
            expressions.at(at).coordinate(coordinate)?;
        }
        expressions
            .at(spans.discrete_value)
            .literal(DaeLiteral::Boolean(false))
    })
}

fn construct_variable_role_dae() -> (Dae, DaeProvenance) {
    let source = TestSource::new(
        "parameter Real p; constant Real c; input Real u; Real x; Real y; \
         output Real o; discrete Real z; discrete Boolean m;",
    );
    let spans = variable_role_spans(&source);
    let dae = Dae::construct(source.map, |dae| {
        let variables = define_variable_role_catalog(dae, spans)?;
        let value = add_variable_role_coordinates(dae, variables, spans)?;
        dae.b1c([variables.discrete_value], |topology| {
            topology.owner(spans.discrete_value, [variables.discrete_value], |owner| {
                owner.always(spans.discrete_value, [(value, spans.discrete_value)])
            })?;
            Ok(())
        })
    })
    .expect("role-specific coordinates are valid");
    (dae, spans.discrete_value)
}

fn assert_variable_role_round_trip(encoded: &str, discrete_action: DaeProvenance) {
    let decoded: Dae = serde_json::from_str(encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.variable_count(), 8);
        assert_eq!(view.expression_count(), 13);
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(
            owner
                .targets()
                .iter()
                .map(DiscreteValueId::index)
                .collect::<Vec<_>>(),
            [7]
        );
        let branch = owner.branches().get(0).unwrap();
        assert!(matches!(
            branch.activation(),
            DiscreteBranchActivation::Always
        ));
        assert_eq!(branch.values().len(), 1);
        assert_eq!(branch.provenance(), discrete_action);
        assert_eq!(branch.values().get(0).unwrap().1, discrete_action);
        assert_coordinate_variability(view);
        assert_scalar_parameter_view(view);
    });
}

#[test]
fn variable_roles_drive_coordinate_construction_and_wire_validation() {
    let (dae, discrete_action) = construct_variable_role_dae();
    let encoded = serde_json::to_string(&dae).unwrap();
    assert_variable_role_round_trip(&encoded, discrete_action);

    let obsolete = encoded.replacen("\"discrete_value_owners\"", "\"discrete_assignments\"", 1);
    assert_ne!(obsolete, encoded);
    assert!(serde_json::from_str::<Dae>(&obsolete).is_err());
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
            kind: "B.1c topology",
            ..
        }
    ));
}

#[derive(Clone, Copy)]
enum PairTarget {
    A,
    B,
}

#[derive(Clone, Copy)]
enum PairValue {
    CurrentA,
    CurrentB,
    PreA,
    PreB,
}

#[derive(Clone, Copy)]
struct PairBranch {
    a: PairValue,
    b: PairValue,
}

struct PairOwnerResult {
    result: Result<Dae, DaeConstructionError>,
    first_a: Span,
    first_b: Span,
    second_b: Span,
}

fn define_pair_variables<'dae>(
    dae: &mut DaeConstruction<'dae>,
    declarations: [DaeProvenance; 2],
) -> Result<[DiscreteValueId<'dae>; 2], DaeConstructionError> {
    let boolean = dae.types(|types| {
        types.intern(
            TypeId::new(0),
            ValueType::scalar(ScalarType::Boolean),
            declarations[0],
        )
    })?;
    dae.variables(|variables| {
        Ok([
            variables.discrete_value(
                VarName::new("a"),
                boolean,
                declarations[0],
                VariableAttributes::default(),
            )?,
            variables.discrete_value(
                VarName::new("b"),
                boolean,
                declarations[1],
                VariableAttributes::default(),
            )?,
        ])
    })
}

fn define_pair_conditions<'dae>(
    dae: &mut DaeConstruction<'dae>,
    branch_count: usize,
    provenance: [DaeProvenance; 2],
) -> Result<Option<[ConditionId<'dae>; 2]>, DaeConstructionError> {
    if branch_count == 1 {
        return Ok(None);
    }
    let values = dae.expressions(|expressions| {
        Ok([
            expressions
                .at(provenance[0])
                .literal(DaeLiteral::Boolean(true))?,
            expressions
                .at(provenance[1])
                .literal(DaeLiteral::Boolean(false))?,
        ])
    })?;
    dae.conditions(|conditions| {
        let first = conditions.reserve(provenance[0])?;
        conditions.define(first, ConditionInput::Discrete(values[0]), provenance[0])?;
        let second = conditions.reserve(provenance[1])?;
        conditions.define(second, ConditionInput::Discrete(values[1]), provenance[1])?;
        Ok(Some([first, second]))
    })
}

fn pair_value_expression<'dae>(
    dae: &mut DaeConstruction<'dae>,
    value: PairValue,
    variables: [DiscreteValueId<'dae>; 2],
    provenance: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    dae.expressions(|expressions| match value {
        PairValue::CurrentA => expressions
            .at(provenance)
            .coordinate(CoordinateInput::DiscreteValue(variables[0])),
        PairValue::CurrentB => expressions
            .at(provenance)
            .coordinate(CoordinateInput::DiscreteValue(variables[1])),
        PairValue::PreA => expressions
            .at(provenance)
            .coordinate(CoordinateInput::PreDiscreteValue(variables[0])),
        PairValue::PreB => expressions
            .at(provenance)
            .coordinate(CoordinateInput::PreDiscreteValue(variables[1])),
    })
}

fn lower_pair_branches<'dae>(
    dae: &mut DaeConstruction<'dae>,
    order: [PairTarget; 2],
    branches: &[PairBranch],
    variables: [DiscreteValueId<'dae>; 2],
    action_provenance: [[DaeProvenance; 2]; 2],
) -> Result<Vec<Vec<(ExprId<'dae>, DaeProvenance)>>, DaeConstructionError> {
    let mut lowered = Vec::with_capacity(branches.len());
    for (branch_index, branch) in branches.iter().copied().enumerate() {
        let mut values = Vec::with_capacity(order.len());
        for target in order {
            let (value, provenance) = match target {
                PairTarget::A => (branch.a, action_provenance[branch_index][0]),
                PairTarget::B => (branch.b, action_provenance[branch_index][1]),
            };
            values.push((
                pair_value_expression(dae, value, variables, provenance)?,
                provenance,
            ));
        }
        lowered.push(values);
    }
    Ok(lowered)
}

fn define_pair_owner<'dae>(
    topology: &mut DiscreteValueTopology<'_, 'dae>,
    targets: [DiscreteValueId<'dae>; 2],
    conditions: Option<[ConditionId<'dae>; 2]>,
    condition_provenance: [DaeProvenance; 2],
    owner_provenance: DaeProvenance,
    branches: Vec<Vec<(ExprId<'dae>, DaeProvenance)>>,
) -> Result<(), DaeConstructionError> {
    topology.owner(owner_provenance, targets, |owner| {
        match conditions {
            Some(conditions) => {
                for (branch_index, values) in branches.into_iter().enumerate() {
                    owner.when(
                        conditions[branch_index],
                        conditions[branch_index],
                        condition_provenance[branch_index],
                        values,
                    )?;
                }
            }
            None => {
                for values in branches {
                    owner.always(owner_provenance, values)?;
                }
            }
        }
        Ok(())
    })?;
    Ok(())
}

fn construct_pair_owner(order: [PairTarget; 2], branches: &[PairBranch]) -> PairOwnerResult {
    assert!((1..=2).contains(&branches.len()));
    let source = TestSource::new(
        "discrete Boolean a; discrete Boolean b; \
         when first then a = firstA; b = firstB; \
         elsewhen second then a = secondA; b = secondB; end when;",
    );
    let a_declaration = source.source("discrete Boolean a", 0);
    let b_declaration = source.source("discrete Boolean b", 0);
    let owner_at = source.source(
        "when first then a = firstA; b = firstB; \
         elsewhen second then a = secondA; b = secondB; end when",
        0,
    );
    let first_condition = source.source("first", 1);
    let second_condition = source.source("second", 1);
    let first_a = source.source("a = firstA", 0);
    let first_b = source.source("b = firstB", 0);
    let second_a = source.source("a = secondA", 0);
    let second_b = source.source("b = secondB", 0);
    let action_provenance = [[first_a, first_b], [second_a, second_b]];
    let branches = branches.to_vec();
    let result = Dae::construct(source.map, |dae| {
        let variables = define_pair_variables(dae, [a_declaration, b_declaration])?;
        let conditions =
            define_pair_conditions(dae, branches.len(), [first_condition, second_condition])?;
        let ordered_targets = order.map(|target| match target {
            PairTarget::A => variables[0],
            PairTarget::B => variables[1],
        });
        let lowered_branches =
            lower_pair_branches(dae, order, &branches, variables, action_provenance)?;
        dae.b1c(ordered_targets, |topology| {
            define_pair_owner(
                topology,
                ordered_targets,
                conditions,
                [first_condition, second_condition],
                owner_at,
                lowered_branches,
            )
        })
    });
    PairOwnerResult {
        result,
        first_a: first_a.span(),
        first_b: first_b.span(),
        second_b: second_b.span(),
    }
}

#[test]
fn b1c_atomic_owner_accepts_current_dependency_on_issued_target_prefix() {
    let outcome = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::PreA,
            b: PairValue::CurrentA,
        }],
    );
    let dae = outcome
        .result
        .expect("the second target may read the first target in one atomic owner");
    dae.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(
            owner
                .targets()
                .iter()
                .map(DiscreteValueId::index)
                .collect::<Vec<_>>(),
            [0, 1]
        );
    });
}

#[test]
fn b1c_atomic_owner_rejects_reverse_order_until_the_producer_corrects_the_plan() {
    let reversed = construct_pair_owner(
        [PairTarget::B, PairTarget::A],
        &[PairBranch {
            a: PairValue::PreA,
            b: PairValue::CurrentA,
        }],
    );
    assert!(matches!(
        reversed.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 1,
            dependency: 0,
            span,
        }) if span == reversed.first_b
    ));

    let corrected = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::PreA,
            b: PairValue::CurrentA,
        }],
    );
    corrected
        .result
        .expect("the producer-supplied topological target order is accepted");
}

#[test]
fn b1c_atomic_owner_rejects_a_direct_current_value_self_cycle() {
    let outcome = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::CurrentA,
            b: PairValue::PreB,
        }],
    );
    assert!(matches!(
        outcome.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 0,
            dependency: 0,
            span,
        }) if span == outcome.first_a
    ));
}

#[test]
fn b1c_atomic_owner_rejects_a_two_target_current_value_cycle() {
    let outcome = construct_pair_owner(
        [PairTarget::A, PairTarget::B],
        &[PairBranch {
            a: PairValue::CurrentB,
            b: PairValue::CurrentA,
        }],
    );
    assert!(matches!(
        outcome.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 0,
            dependency: 1,
            span,
        }) if span == outcome.first_a
    ));
}

#[test]
fn b1c_atomic_owner_checks_dependencies_present_in_only_one_conditional_branch() {
    let outcome = construct_pair_owner(
        [PairTarget::B, PairTarget::A],
        &[
            PairBranch {
                a: PairValue::PreA,
                b: PairValue::PreB,
            },
            PairBranch {
                a: PairValue::PreA,
                b: PairValue::CurrentA,
            },
        ],
    );
    assert!(matches!(
        outcome.result,
        Err(DaeConstructionError::UnissuedDiscreteDependency {
            target: 1,
            dependency: 0,
            span,
        }) if span == outcome.second_b
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
        dae.b1c([a, b], |topology| {
            topology.owner(a_assignment, [a], |owner| {
                owner.always(a_assignment, [(b_value, a_assignment)])
            })?;
            topology.owner(b_assignment, [b], |owner| {
                owner.always(b_assignment, [(a_value, b_assignment)])
            })?;
            Ok(())
        })
    })
    .unwrap_err();

    assert!(matches!(
        error,
        DaeConstructionError::UnissuedDiscreteDependency {
            target,
            dependency,
            span,
        } if target == 0 && dependency == 1 && span == a_assignment.span()
    ));
}

#[test]
fn b1c_when_owner_preserves_source_priority_and_action_provenance() {
    let source = TestSource::new(
        "discrete Boolean m; when a then m = true; elsewhen b then m = false; end when;",
    );
    let declaration = source.source("discrete Boolean m", 0);
    let owner_at = source.source(
        "when a then m = true; elsewhen b then m = false; end when",
        0,
    );
    let a_at = source.source("a", 1);
    let b_at = source.source("b", 0);
    let true_at = source.source("m = true", 0);
    let false_at = source.source("m = false", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        let m = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let (a_value, b_value, true_value, false_value) = dae.expressions(|expressions| {
            Ok((
                expressions.at(a_at).literal(DaeLiteral::Boolean(true))?,
                expressions.at(b_at).literal(DaeLiteral::Boolean(false))?,
                expressions.at(true_at).literal(DaeLiteral::Boolean(true))?,
                expressions
                    .at(false_at)
                    .literal(DaeLiteral::Boolean(false))?,
            ))
        })?;
        let (a, b) = dae.conditions(|conditions| {
            let a = conditions.reserve(a_at)?;
            conditions.define(a, ConditionInput::Discrete(a_value), a_at)?;
            let b = conditions.reserve(b_at)?;
            conditions.define(b, ConditionInput::Discrete(b_value), b_at)?;
            Ok((a, b))
        })?;
        dae.b1c([m], |topology| {
            topology.owner(owner_at, [m], |owner| {
                owner.when(a, a, a_at, [(true_value, true_at)])?;
                owner.when(b, b, b_at, [(false_value, false_at)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let branches = owner.branches();
        assert_eq!(owner.provenance(), owner_at);
        assert_eq!(branches.get(0).unwrap().provenance(), a_at);
        assert_eq!(branches.get(0).unwrap().values().get(0).unwrap().1, true_at);
        assert_eq!(branches.get(1).unwrap().provenance(), b_at);
        assert_eq!(
            branches.get(1).unwrap().values().get(0).unwrap().1,
            false_at
        );
    });
}

#[test]
fn b1c_topology_error_rolls_back_earlier_owners_before_retry() {
    let source = TestSource::new(
        "discrete Boolean a; discrete Boolean b; pre(a); pre(b); first owner; second owner;",
    );
    let a_declaration = source.source("discrete Boolean a", 0);
    let b_declaration = source.source("discrete Boolean b", 0);
    let a_value_at = source.source("pre(a)", 0);
    let b_value_at = source.source("pre(b)", 0);
    let first_owner = source.source("first owner", 0);
    let second_owner = source.source("second owner", 0);
    let dae = Dae::construct(source.map, |dae| {
        let [a, b] = define_pair_variables(dae, [a_declaration, b_declaration])?;
        let [a_value, b_value] = [
            pair_value_expression(dae, PairValue::PreA, [a, b], a_value_at)?,
            pair_value_expression(dae, PairValue::PreB, [a, b], b_value_at)?,
        ];

        let rejected = dae.b1c([a, b], |topology| {
            topology.owner(first_owner, [a], |owner| {
                owner.always(first_owner, [(a_value, a_value_at)])
            })?;
            topology.owner(second_owner, [a], |_| Ok(()))?;
            Ok(())
        });
        assert!(matches!(
            rejected,
            Err(DaeConstructionError::InvalidDiscreteTargetOrder {
                expected: Some(1),
                found: Some(0),
                ..
            })
        ));

        dae.b1c([a, b], |topology| {
            topology.owner(first_owner, [a], |owner| {
                owner.always(first_owner, [(a_value, a_value_at)])
            })?;
            topology.owner(second_owner, [b], |owner| {
                owner.always(second_owner, [(b_value, b_value_at)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 2);
        let first = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let second = view
            .discrete_value_owner(view.discrete_value_owner_id(1).unwrap())
            .unwrap();
        assert_eq!(first.targets().len(), 1);
        assert_eq!(second.targets().len(), 1);
        assert_eq!(first.provenance(), first_owner);
        assert_eq!(second.provenance(), second_owner);
        assert_eq!(
            first.branches().get(0).unwrap().values().get(0).unwrap().1,
            a_value_at
        );
        assert_eq!(
            second.branches().get(0).unwrap().values().get(0).unwrap().1,
            b_value_at
        );
    });
}

#[test]
fn b1c_owner_error_rolls_back_direct_aggregate_insertion_before_retry() {
    let source = TestSource::new("discrete Boolean a; pre(a); owner;");
    let declaration = source.source("discrete Boolean a", 0);
    let value_at = source.source("pre(a)", 0);
    let owner_at = source.source("owner", 0);
    let dae = Dae::construct(source.map, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        let a = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("a"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        })?;
        let value = dae.expressions(|expressions| {
            expressions
                .at(value_at)
                .coordinate(CoordinateInput::PreDiscreteValue(a))
        })?;
        dae.b1c([a], |topology| {
            let rejected = topology.owner(owner_at, [a], |owner| {
                owner.always(owner_at, [(value, value_at)])?;
                owner.always(owner_at, [(value, value_at)])
            });
            assert!(matches!(
                rejected,
                Err(DaeConstructionError::InvalidDiscreteBranchSet { .. })
            ));
            topology.owner(owner_at, [a], |owner| {
                owner.always(owner_at, [(value, value_at)])
            })?;
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert_eq!(owner.branches().len(), 1);
    });
}

#[test]
fn empty_b1c_topology_seals_an_empty_discrete_role_view_and_round_trips() {
    let source = TestSource::new("model Empty end Empty;");
    let dae = Dae::construct(source.map, |dae| dae.b1c([], |_| Ok(()))).unwrap();
    let assert_empty = |dae: &Dae| {
        dae.inspect(|view| {
            assert_eq!(view.discrete_value_owner_count(), 0);
            assert_eq!(view.discrete_value_definition_count(), 0);
            assert_eq!(
                view.variables()
                    .filter(|(_, variable)| variable.role() == VariableRole::DiscreteValue)
                    .count(),
                0
            );
        });
    };
    assert_empty(&dae);

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    assert_empty(&decoded);
}

#[test]
fn empty_b1c_topology_capability_cannot_be_consumed_twice() {
    let source = TestSource::new("model Empty end Empty;");
    let error = Dae::construct(source.map, |dae| {
        dae.b1c([], |_| Ok(()))?;
        dae.b1c([], |_| {
            panic!("a consumed empty B.1c topology must reject before rebuilding")
        })
    })
    .unwrap_err();
    assert_eq!(
        error,
        DaeConstructionError::DuplicateTopology {
            kind: "B.1c topology",
            span: None,
        }
    );
    assert_eq!(error.source_span(), None);
}

#[test]
fn empty_b1c_topology_cannot_be_reopened_by_a_late_discrete_target() {
    let source = TestSource::new("model Empty discrete Boolean late; end Empty;");
    let declaration = source.source("discrete Boolean late", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.b1c([], |_| Ok(()))?;
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Boolean),
                declaration,
            )
        })?;
        let rejected = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("late"),
                boolean,
                declaration,
                VariableAttributes::default(),
            )
        });
        assert_eq!(
            rejected,
            Err(DaeConstructionError::DuplicateTopology {
                kind: "B.1c topology",
                span: Some(declaration.span()),
            })
        );
        Ok(())
    })
    .unwrap();
    dae.inspect(|view| assert_eq!(view.variable_count(), 0));
}
