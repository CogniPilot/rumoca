use super::*;

fn scalar_value(expression: &gast::Expression, locals: &HashMap<String, f64>) -> f64 {
    match expression {
        gast::Expression::Real(value) => *value,
        gast::Expression::Ref(gast::Reference::Local(part)) if part.subscripts.is_empty() => {
            *locals
                .get(part.name.lexeme())
                .expect("defined scalar temporary")
        }
        gast::Expression::Binary { op, lhs, rhs } => {
            let lhs = scalar_value(lhs, locals);
            let rhs = scalar_value(rhs, locals);
            match op {
                gast::BinaryOp::Add => lhs + rhs,
                gast::BinaryOp::Mul => lhs * rhs,
                other => panic!("unexpected reduction operator {other:?}"),
            }
        }
        other => panic!("unexpected reduction expression {other:?}"),
    }
}

/// Evaluate scalar checkpoint assignments in order, as DoStep would.
fn checkpoint_values(prefixes: Vec<gast::Spanned<gast::Statement>>) -> HashMap<String, f64> {
    let mut locals = HashMap::new();
    for statement in prefixes {
        let gast::Statement::Assignment {
            target: gast::Reference::Local(part),
            value,
        } = statement.node
        else {
            panic!("scalar reduction assignment");
        };
        assert!(part.subscripts.is_empty());
        let value = scalar_value(&value, &locals);
        locals.insert(part.name.lexeme().to_owned(), value);
    }
    locals
}

fn check_ordered_reduction(values: &[f64], builtin: dae::PureBuiltin, expected: f64) {
    let mut sources = SourceMap::new();
    let source = sources.add("ordered-reduction.mo", "sum(values)");
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 11)).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let elements = values
                .iter()
                .map(|value| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(*value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let array = expressions.at(provenance).array(elements)?;
            expressions.at(provenance).builtin(builtin, [array])?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let reduction = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .find(|id| match view.expression(*id).unwrap().operation() {
                dae::ExpressionOperation::Builtin {
                    builtin: actual, ..
                } => actual == builtin,
                _ => false,
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        // Outside an inline `if` branch every lowerer checkpoints, and the
        // checkpointed left fold has to stay bit-exact either way.
        for mut lowerer in [
            ExpressionLowerer::new(view, &definitions, &variables, &previous),
            ExpressionLowerer::with_do_step_effects(view, &definitions, &variables, &previous),
        ] {
            let result = lowerer.lower(reduction).unwrap();
            let prefixes = lowerer.take_prefix_statements();
            assert!(
                !prefixes.is_empty(),
                "must cross a materialization boundary"
            );
            let locals = checkpoint_values(prefixes);
            assert_eq!(
                scalar_value(&result.expression, &locals).to_bits(),
                expected.to_bits()
            );
        }
    });
}

#[test]
fn chunked_reductions_preserve_rounding_sensitive_left_to_right_results() {
    let sum: Vec<_> = [1e16, -1e16, 1.0].into_iter().cycle().take(201).collect();
    let expected_sum = sum.iter().copied().reduce(|a, b| a + b).unwrap();
    assert_eq!(expected_sum, 1.0);
    // Summing independently reduced triplets would incorrectly produce 67.
    check_ordered_reduction(&sum, dae::PureBuiltin::Sum, expected_sum);
    let product: Vec<_> = [1e200, 1e-200, 1e-200, 1e200]
        .into_iter()
        .cycle()
        .take(200)
        .collect();
    let expected_product = product.iter().copied().reduce(|a, b| a * b).unwrap();
    check_ordered_reduction(&product, dae::PureBuiltin::Product, expected_product);
}

fn guarded_reduction_model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "if false then sum(values) else 0.0";
    let source = sources.add("guarded-reduction.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let values = (0..200)
                .map(|_| expressions.at(at).literal(dae::DaeLiteral::Real(1.0)))
                .collect::<Result<Vec<_>, _>>()?;
            let array = expressions.at(at).array(values)?;
            let reduction = expressions.at(at).builtin(dae::PureBuiltin::Sum, [array])?;
            let condition = expressions
                .at(at)
                .literal(dae::DaeLiteral::Boolean(false))?;
            let fallback = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(at)
                .conditional([(condition, reduction)], fallback)?;
            Ok(())
        })
    })
    .unwrap()
}

fn guarded_conditional<'dae>(view: dae::DaeView<'dae>) -> dae::ExprId<'dae> {
    (0..view.expression_count())
        .filter_map(|i| view.expression_id(i))
        .find(|id| {
            matches!(
                view.expression(*id).unwrap().operation(),
                dae::ExpressionOperation::Conditional(_)
            )
        })
        .unwrap()
}

#[test]
fn reduction_checkpoints_stay_inside_their_conditional_branch() {
    let model = guarded_reduction_model();
    model.inspect(|view| {
        let conditional = guarded_conditional(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer =
            ExpressionLowerer::with_do_step_effects(view, &definitions, &variables, &previous);
        lowerer.lower(conditional).unwrap();
        let prefixes = lowerer.take_prefix_statements();
        let [statement] = prefixes.as_slice() else { panic!("no unguarded reduction prefix"); };
        let gast::Statement::If(selection) = &statement.node else { panic!("guarded scalar materialization"); };
        assert_eq!(selection.branches.len(), 1);
        assert!(matches!(selection.branches[0].condition, gast::Condition::Expression(gast::Expression::Bool(false))));
        assert!(selection.branches[0].body.len() > 1, "reduction checkpoints belong to the false branch");
        let fallback = selection.else_body.as_ref().unwrap();
        assert_eq!(fallback.len(), 1, "fallback has no reduction checkpoints");
        assert!(matches!(&fallback[0].node, gast::Statement::Assignment { value: gast::Expression::Real(value), .. } if *value == 0.0));
    });
}

#[test]
fn non_materializing_lowerers_emit_no_unguarded_reduction_checkpoints() {
    // `new` and `with_assertions` lower `if` inline without capturing branch
    // prefixes, so a checkpoint inside the branch would execute even when the
    // branch is not taken. The reduction must stay inline there instead.
    let model = guarded_reduction_model();
    model.inspect(|view| {
        let conditional = guarded_conditional(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        for mut lowerer in [
            ExpressionLowerer::new(view, &definitions, &variables, &previous),
            ExpressionLowerer::with_assertions(view, &definitions, &variables, &previous),
        ] {
            let result = lowerer.lower(conditional).unwrap();
            assert!(lowerer.take_prefix_statements().is_empty());
            assert!(matches!(result.expression, gast::Expression::If(_)));
        }
    });
}

fn small_reduction_model(
    scalar: dae::ScalarType,
    builtin: dae::PureBuiltin,
    count: usize,
) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "sum(values)";
    let source = sources.add("small-reduction.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let value = match scalar {
        dae::ScalarType::Integer => dae::DaeLiteral::Integer(7),
        _ => dae::DaeLiteral::Real(7.0),
    };
    dae::Dae::construct(sources, |dae| {
        let empty_type =
            dae.types(|types| types.derived(dae::ValueType::array(scalar, [0]), at))?;
        dae.expressions(|expressions| {
            let array = if count == 0 {
                expressions.at(at).empty_array(empty_type)?
            } else {
                let value = expressions.at(at).literal(value)?;
                expressions.at(at).array([value])?
            };
            expressions.at(at).builtin(builtin, [array])?;
            Ok(())
        })
    })
    .unwrap()
}

fn check_small_reduction(scalar: dae::ScalarType, builtin: dae::PureBuiltin, count: usize) {
    let model = small_reduction_model(scalar, builtin, count);
    model.inspect(|view| {
        let reduction = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Builtin { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::new(view, &definitions, &variables, &previous);
        let result = lowerer.lower(reduction).unwrap();
        let value = if count == 1 {
            7
        } else {
            i64::from(builtin == dae::PureBuiltin::Product)
        };
        match scalar {
            dae::ScalarType::Integer => {
                assert_eq!(result.scalar_type, gast::ScalarType::Integer);
                assert_eq!(result.expression, gast::Expression::Integer(value));
            }
            _ => {
                assert_eq!(result.scalar_type, gast::ScalarType::Real);
                assert_eq!(result.expression, gast::Expression::Real(value as f64));
            }
        }
        assert!(lowerer.take_prefix_statements().is_empty());
        assert!(lowerer.take_temporary_locals().is_empty());
    });
}

#[test]
fn empty_and_singleton_reductions_keep_numeric_type_without_temporaries() {
    for scalar in [dae::ScalarType::Real, dae::ScalarType::Integer] {
        for builtin in [dae::PureBuiltin::Sum, dae::PureBuiltin::Product] {
            for count in [0, 1] {
                check_small_reduction(scalar, builtin, count);
            }
        }
    }
}
