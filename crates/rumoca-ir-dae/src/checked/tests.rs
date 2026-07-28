use rumoca_core::{SourceId, SourceMap, Span, VarName};

use super::*;

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
        let real =
            dae.types(|types| types.intern(ValueType::scalar(ScalarType::Real), declaration))?;
        let x =
            dae.variables(|variables| variables.declare(VarName::new("x"), real, declaration))?;
        let domain = dae.domains(|domains| domains.compact(3, range))?;
        dae.expressions(|expr| {
            let x_node = expr.at(x_first).coordinate(CoordinateInput::Variable(x))?;
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
fn variable_occurrences_share_declaration_identity_but_keep_use_spans() {
    let source = TestSource::new("Real x; equation x = x;");
    let declaration = source.source("Real x", 0);
    let first_use = source.source("x", 1);
    let second_use = source.source("x", 2);
    let equation_owner = source.source("x = x", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.intern(ValueType::scalar(ScalarType::Real), declaration))?;
        let x =
            dae.variables(|variables| variables.declare(VarName::new("x"), real, declaration))?;
        dae.equation(equation_owner, |equation| {
            let mut expr = equation.expressions();
            let lhs = expr
                .at(first_use)
                .coordinate(CoordinateInput::Variable(x))?;
            let rhs = expr
                .at(second_use)
                .coordinate(CoordinateInput::Variable(x))?;
            equation.equal(lhs, rhs)?;
            Ok(())
        })?;
        Ok(())
    })
    .expect("equation construction succeeds");

    dae.inspect(|view| {
        let lhs = view.expression(view.expression_id(0).unwrap()).unwrap();
        let rhs = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(lhs.variable_coordinate(), rhs.variable_coordinate());
        assert_ne!(lhs.provenance(), rhs.provenance());
        assert_eq!(
            view.variable_declaration(lhs.variable_coordinate().unwrap()),
            Some(declaration)
        );

        let equation = view.equation(0).expect("equation exists");
        let residual = view.expression(equation.residual()).unwrap();
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
        assert_eq!(view.equation_count(), 1);
    });
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
            types.intern(ValueType::scalar(ScalarType::Real), function_declaration)
        })?;
        let function = dae.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [real], [real], function_declaration)
        })?;
        let literal =
            dae.expressions(|expr| expr.at(literal_span).literal(DaeLiteral::Real(0.0)))?;
        dae.functions(|functions| functions.define(function, [literal], function_declaration))?;

        let condition = dae.conditions(|conditions| conditions.reserve(condition_owner))?;
        let condition_value =
            dae.expressions(|expr| expr.at(condition_owner).literal(DaeLiteral::Boolean(true)))?;
        dae.conditions(|conditions| {
            conditions.define(condition, condition_value, condition_owner)
        })?;

        dae.expressions(|expr| {
            let _call = expr.at(function_declaration).call(function, 0, [literal])?;
            let _condition = expr
                .at(condition_owner)
                .coordinate(CoordinateInput::Condition(condition))?;
            let _clock = expr.at(clock_generated).coordinate(CoordinateInput::Time)?;
            let _delay = expr
                .at(delay_generated)
                .coordinate(CoordinateInput::Delay(0))?;
            Ok(())
        })
    })
    .expect("all owners share one arena");

    dae.inspect(|view| {
        assert_eq!(view.expression_count(), 6);
        assert_eq!(
            view.expression(view.expression_id(4).unwrap())
                .unwrap()
                .provenance()
                .origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::ClockLowering)
        );
        assert_eq!(
            view.expression(view.expression_id(5).unwrap())
                .unwrap()
                .provenance()
                .origin(),
            DaeProvenanceOrigin::Generated(DaeGeneration::DelayLowering)
        );
    });

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.expression_count(), 6);
        assert_eq!(view.variable_count(), 0);
    });
}

#[test]
fn wire_v11_round_trip_preserves_provenance_without_inline_source_copies() {
    let source = TestSource::new("42");
    let literal = source.source("42", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expr| {
            expr.at(literal).literal(DaeLiteral::Integer(42))?;
            Ok(())
        })
    })
    .unwrap();

    let json = serde_json::to_string(&dae).unwrap();
    assert_eq!(
        json.matches("42").count(),
        2,
        "source text plus literal value"
    );
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    assert_eq!(decoded.schema_version(), CHECKED_DAE_SCHEMA_VERSION);
    decoded.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(view.source_text(expression.provenance()), Some("42"));
    });

    let legacy = json.replacen(
        &format!("\"schema_version\":{CHECKED_DAE_SCHEMA_VERSION}"),
        "\"schema_version\":10",
        1,
    );
    assert!(matches!(
        serde_json::from_str::<Dae>(&legacy),
        Err(error) if error.to_string().contains("unsupported checked DAE schema version 10")
    ));
}

#[test]
fn construction_rejects_dummy_unknown_and_out_of_range_provenance() {
    assert!(matches!(
        DaeProvenance::source(Span::DUMMY),
        Err(DaeConstructionError::MissingProvenance { .. })
    ));

    let source = TestSource::new("x");
    let unknown = DaeProvenance::source(Span::from_offsets(
        SourceId::from_source_name("missing.mo"),
        0,
        1,
    ))
    .unwrap();
    let out_of_range =
        DaeProvenance::source(Span::from_offsets(source.id, 0, source.text.len() + 1)).unwrap();

    let result = Dae::construct(source.map, |dae| {
        dae.expressions(|expr| {
            expr.at(unknown).literal(DaeLiteral::Integer(1))?;
            Ok(())
        })
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::UnknownSource { .. })
    ));

    let mut map = SourceMap::new();
    map.add("construction.mo", "x");
    let result = Dae::construct(map, |dae| {
        dae.expressions(|expr| {
            expr.at(out_of_range).literal(DaeLiteral::Integer(1))?;
            Ok(())
        })
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::InvalidSourceRange { .. })
    ));
}
