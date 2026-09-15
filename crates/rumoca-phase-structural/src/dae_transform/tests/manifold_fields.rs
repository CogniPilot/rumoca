use super::*;

#[test]
fn holonomic_preflight_accepts_materializable_record_fields() {
    for field in [0, 1] {
        let (model, residual, _) = field_model(field);
        model.inspect(|view| {
            assert!(
                constraints::holonomic_constraints(view)
                    .iter()
                    .any(|constraint| constraint.residual == residual)
            );
        });
    }
}

#[test]
fn retained_manifold_substitutes_the_first_matrix_field_through_nested_calls() {
    check_manifold_field(0);
}

#[test]
fn retained_manifold_substitutes_the_second_matrix_field_through_nested_calls() {
    check_manifold_field(1);
}

fn check_manifold_field(field: usize) {
    let (model, _, residual) = field_model(field);
    let candidate = model.inspect(|view| {
        constraints::direct_state_constraints(
            view,
            &constraints::DifferentiationFacts::collect(view),
        )
        .admissible
        .into_iter()
        .find(|candidate| {
            view.variable(view.variable_id(candidate.state as usize).unwrap())
                .unwrap()
                .name()
                .as_str()
                == "x"
        })
        .expect("the record field defines the demotable state")
    });
    let retained = ManifoldConstraint {
        expression: residual,
        lifted: None,
    };
    let (rebuilt, manifold) = reconstruction::rebuild_with_state_demotion_and_manifold(
        &ReductionSource::new(&model),
        candidate,
        &[retained],
    )
    .expect("record field values survive retained-manifold substitution");
    assert_eq!(manifold.len(), 1);
    assert!(manifold_is_state_only(&rebuilt, &manifold));
    rebuilt.inspect(|view| {
        crate::sort(view).expect("the demoted model is computable");
        let residual = view.expression_id(manifold[0].expression as usize).unwrap();
        let dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Subtract,
            lhs,
            rhs,
        } = view.expression(residual).unwrap().operation()
        else {
            panic!("the retained equation remains a subtraction");
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) =
            view.expression(rhs).unwrap().operation()
        else {
            panic!("the retained constraint still relates the remaining state");
        };
        assert_eq!(
            view.variable(view.variable_id(state.index() as usize).unwrap())
                .unwrap()
                .name()
                .as_str(),
            "z"
        );
        for (p, q) in [(2.0, 5.0), (-3.0, 7.0), (0.0, 0.0), (1.5, -2.25)] {
            let mut evaluator = rumoca_eval_dae::NumericEvaluator::with_overrides(
                view,
                |variable, _| match variable.name().as_str() {
                    "p" => Some(p),
                    "q" => Some(q),
                    _ => None,
                },
            );
            let expected = if field == 0 {
                [q, 2.0 * p, p, -q]
            } else {
                [p, q, 3.0 * q, p + q]
            };
            assert_eq!(evaluator.expression(lhs).unwrap(), expected);
        }
    });
}

#[derive(Clone, Copy)]
struct FieldTypes<'dae> {
    real: dae::ValueTypeId<'dae>,
    matrix: dae::ValueTypeId<'dae>,
    pair: dae::ValueTypeId<'dae>,
}

fn field_types<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    at: dae::DaeProvenance,
) -> Result<FieldTypes<'dae>, dae::DaeConstructionError> {
    model.types(|types| {
        let real = types.intern(
            TypeId::new(0),
            dae::ValueType::scalar(dae::ScalarType::Real),
            at,
        )?;
        let matrix = types.intern(
            TypeId::new(1),
            dae::ValueType::array(dae::ScalarType::Real, [2, 2]),
            at,
        )?;
        let pair = types.record(
            VarName::new("Pair"),
            [
                (VarName::new("first"), matrix),
                (VarName::new("second"), matrix),
            ],
            at,
        )?;
        Ok(FieldTypes { real, matrix, pair })
    })
}

fn field_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    types: FieldTypes<'dae>,
    at: dae::DaeProvenance,
    callee: Option<dae::FunctionId<'dae>>,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let name = if callee.is_some() {
        "swapped"
    } else {
        "makePair"
    };
    model
        .function(
            dae::FunctionSignature::new(
                VarName::new(name),
                [types.real, types.real],
                [types.pair],
                at,
            ),
            |model, reservation| {
                let (u, v, output) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("v"), 1, at)?,
                        functions.output(&reservation, VarName::new("result"), 0, at)?,
                    ))
                })?;
                let value = model.expressions(|expressions| {
                    let u = expressions.at(at).function_parameter(u)?;
                    let v = expressions.at(at).function_parameter(v)?;
                    if let Some(callee) = callee {
                        return expressions.at(at).call(callee, 0, [v, u]);
                    }
                    pair_value(expressions, types.pair, at, u, v)
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, value, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )
        .map(|(function, ())| function)
}

fn pair_value<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    pair: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    u: dae::ExprId<'dae>,
    v: dae::ExprId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
    let three = expressions.at(at).literal(dae::DaeLiteral::Real(3.0))?;
    let twice_v = expressions
        .at(at)
        .binary(dae::BinaryOperator::Multiply, two, v)?;
    let thrice_u = expressions
        .at(at)
        .binary(dae::BinaryOperator::Multiply, three, u)?;
    let negative_u = expressions.at(at).unary(dae::UnaryOperator::Negate, u)?;
    let sum = expressions.at(at).binary(dae::BinaryOperator::Add, u, v)?;
    let mut matrices = Vec::new();
    for entries in [[u, twice_v, v, negative_u], [v, u, thrice_u, sum]] {
        let first = expressions.at(at).array(entries[..2].iter().copied())?;
        let second = expressions.at(at).array(entries[2..].iter().copied())?;
        matrices.push(expressions.at(at).array([first, second])?);
    }
    expressions.at(at).record(pair, matrices)
}

fn field_source(field: usize) -> String {
    let field_name = ["first", "second"][field];
    format!(
        r#"
record Pair
  Real first[2,2];
  Real second[2,2];
end Pair;
function makePair
  input Real u;
  input Real v;
  output Pair result;
algorithm
  result := Pair({{{{u,2*v}},{{v,-u}}}},{{{{v,u}},{{3*u,u+v}}}});
end makePair;
function swapped
  input Real u;
  input Real v;
  output Pair result;
algorithm
  result := makePair(v,u);
end swapped;
model RecordFieldManifold
  parameter Real p=0 annotation(Evaluate=false);
  parameter Real q=0 annotation(Evaluate=false);
  Pair pairValue=swapped(p,q);
  Real x[2,2];
  Real z[2,2];
  Real a[2,2];
equation
  x = pairValue.{field_name};
  x = z;
  der(x) = a;
end RecordFieldManifold;
"#
    )
}

fn field_model(field: usize) -> (dae::Dae, u32, u32) {
    let text = field_source(field);
    let mut sources = SourceMap::new();
    let source = sources.add("record_field_manifold.mo", text.as_str());
    let at = source_provenance(source, &text, "equation");
    let mut position_id = 0;
    let mut retained = 0;
    let model = dae::Dae::construct(sources, |model| {
        let types = field_types(model, at)?;
        let function = field_function(model, types, at, None)?;
        let function = field_function(model, types, at, Some(function))?;
        let zero = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(0.0)))?;
        let (p, q, x, z, a) = model.variables(|variables| {
            let parameters = || dae::VariableAttributes {
                binding: Some(zero),
                is_tunable: true,
                ..Default::default()
            };
            Ok((
                variables.parameter(VarName::new("p"), types.real, at, parameters())?,
                variables.parameter(VarName::new("q"), types.real, at, parameters())?,
                variables.state(VarName::new("x"), types.matrix, at, Default::default())?,
                variables.state(VarName::new("z"), types.matrix, at, Default::default())?,
                variables.algebraic(VarName::new("a"), types.matrix, at, Default::default())?,
            ))
        })?;
        let (position, velocity, coupled_velocity, manifold) =
            model.expressions(|expressions| {
                let p = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(p))?;
                let q = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Parameter(q))?;
                let x_value = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::State(x))?;
                let x_derivative = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Derivative(x))?;
                let z_value = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::State(z))?;
                let z_derivative = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Derivative(z))?;
                let a = expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::Algebraic(a))?;
                let call = expressions.at(at).call(function, 0, [p, q])?;
                let value = expressions.at(at).field(call, field)?;
                Ok((
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Subtract, x_value, value)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Subtract, x_derivative, a)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Subtract, z_derivative, a)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Subtract, x_value, z_value)?,
                ))
            })?;
        position_id = position.index();
        retained = manifold.index();
        // One preceding index-reduction round retained x = z and replaced
        // its continuous row by der(z) = a, using der(x) = a.
        model.continuous(|continuous| {
            continuous.value_equation(at, position)?;
            continuous.value_equation(at, velocity)?;
            continuous.value_equation(at, coupled_velocity)
        })
    })
    .expect("field-projection fixture is checked DAE");
    (model, position_id, retained)
}
