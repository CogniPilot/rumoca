use super::*;

#[test]
fn shared_model_projection_keeps_distinct_domain_points() {
    let text = "Real x[3]; sum({x[i]+x[i] for i in 1:3});";
    let mut sources = SourceMap::new();
    let source = sources.add("shared_domain.mo", text);
    let at = provenance(source, 0, text.len());
    let model = dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        let domain = model.domains(|domains| {
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
                at,
            )
        })?;
        let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
        model.expressions(|expressions| {
            let x = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let i = expressions.at(at).binder(binder)?;
            let selected = expressions.at(at).index(
                x,
                [dae::Subscript::Index {
                    expression: i,
                    provenance: at,
                }],
            )?;
            let shared = expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, selected, selected)?;
            let array = expressions.at(at).comprehension(domain, shared)?;
            expressions.at(at).builtin(dae::PureBuiltin::Sum, [array])?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let mut selected = Vec::new();
        for_each_scalar_coordinate(view, root, 0, None, |_, index| selected.push(index)).unwrap();
        assert_eq!(selected, [0, 1, 2]);
    });
}

#[test]
fn a_shared_model_dag_is_projected_once_per_query() {
    let text = "Real x[3]; equation x = x + x;";
    let mut sources = SourceMap::new();
    let source = sources.add("shared_dag.mo", text);
    let at = provenance(source, 0, text.len());
    let model = dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|expressions| {
            let mut shared = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            for _ in 0..12 {
                shared = expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, shared, shared)?;
            }
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let mut cache = ScalarCoordinateProjectionCache::default();
        for scalar in [0, 2, 0] {
            let mut callbacks = Vec::new();
            for_each_scalar_coordinate_cached(
                view,
                root,
                scalar,
                None,
                &mut cache,
                |coordinate, index| {
                    assert!(matches!(coordinate, dae::CoordinateView::Algebraic(variable) if variable.index() == 0));
                    callbacks.push(index);
                },
            )
            .unwrap();
            assert_eq!(callbacks.len(), 1, "one visit per shared expression scalar");
            assert_eq!(
                callbacks,
                [scalar],
                "shared DAGs must not cause exponential dependency visits or suppress a later query"
            );
        }
    });
}
