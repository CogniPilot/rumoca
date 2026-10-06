//! A record field of a comprehension selects that field at each domain point.
use super::*;

#[test]
fn a_record_comprehension_field_projects_each_point_to_its_own_field() {
    let text = "Real x[6]; {Pair(x[i], x[i + 3]) for i in 1:3}.second;";
    let mut sources = SourceMap::new();
    let source = sources.add("record_comprehension.mo", text);
    let at = provenance(source, 0, text.len());
    let model = dae::Dae::construct(sources, |model| {
        let (vector, pair) = model.types(|types| {
            let real = types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?;
            let fields = [
                (VarName::new("first"), real),
                (VarName::new("second"), real),
            ];
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [6]), at)?,
                types.record(VarName::new("Pair"), fields, at)?,
            ))
        })?;
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
            let i = expressions.at(at).binder(binder)?;
            let three = expressions.at(at).literal(dae::DaeLiteral::Integer(3))?;
            let shifted = expressions
                .at(at)
                .binary(dae::BinaryOperator::Add, i, three)?;
            let mut fields = Vec::new();
            for subscript in [i, shifted] {
                let x = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                fields.push(expressions.at(at).index(
                    x,
                    [dae::Subscript::Index {
                        expression: subscript,
                        provenance: at,
                    }],
                )?);
            }
            let record = expressions.at(at).record(pair, fields)?;
            let records = expressions.at(at).comprehension(domain, record)?;
            expressions.at(at).field(records, 1)?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let selected = (0..3)
            .map(|scalar| {
                let mut selected = Vec::new();
                for_each_scalar_coordinate(view, root, scalar, None, |coordinate, index| {
                    assert!(matches!(coordinate, dae::CoordinateView::Algebraic(_)));
                    selected.push(index);
                })
                .unwrap();
                selected
            })
            .collect::<Vec<_>>();
        assert_eq!(selected, [[3], [4], [5]]);
    });
}
