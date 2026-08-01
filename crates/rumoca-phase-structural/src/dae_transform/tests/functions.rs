use super::*;

#[test]
fn state_demotion_preserves_recursive_functions_folds_records_and_wire() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            record_companion: true,
            functions: true,
            ..FixtureFeatures::default()
        },
    );
    let expected_inventory = model.inspect(function_expression_inventory);
    let prepared = prepare_for_solve(&model).expect("function owners survive reconstruction");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        assert_rebuilt_functions(view);
        assert_eq!(function_expression_inventory(view), expected_inventory);
    });
    let encoded = serde_json::to_string(&transformed).expect("checked function DAE serializes");
    let decoded: dae::Dae =
        serde_json::from_str(&encoded).expect("wire rebuilds functions through checked owners");
    decoded.inspect(|view| {
        assert_rebuilt_functions(view);
        assert_eq!(function_expression_inventory(view), expected_inventory);
    });
}

#[test]
fn state_demotion_preserves_distinct_definitions_with_one_rhs() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            functions: true,
            same_rhs_definitions: true,
            ..FixtureFeatures::default()
        },
    );
    model.inspect(assert_same_rhs_definitions);
    let prepared = prepare_for_solve(&model).expect("same-RHS definitions survive reconstruction");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed { .. } => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(assert_same_rhs_definitions);
}

fn assert_same_rhs_definitions(view: dae::DaeView<'_>) {
    let function = find_function(view, "sum3");
    let assignments = function
        .statements()
        .filter_map(|statement| match statement {
            dae::FunctionStatementView::Assignment { definition } => Some(definition),
            dae::FunctionStatementView::For { .. } => None,
        })
        .collect::<Vec<_>>();
    assert!(assignments.len() >= 3);
    let first = assignments[0];
    let second = assignments[1];
    assert_ne!(first.id(), second.id());
    assert_eq!(first.target(), second.target());
    assert_eq!(first.rhs(), second.rhs());
    assert_eq!(
        view.function_definition(first.id()).unwrap().rhs(),
        first.rhs()
    );
    assert_eq!(
        view.function_definition(second.id()).unwrap().rhs(),
        second.rhs()
    );

    let dead = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| view.source_text(expression.provenance()) == Some("acc + 0"))
        .expect("definition-one occurrence remains in the arena");
    let dae::ExpressionOperation::Binary { lhs, .. } = dead.operation() else {
        panic!("retained expression remains binary");
    };
    let lhs = view.expression(lhs).expect("retained lhs resolves");
    let dae::ExpressionOperation::FunctionValue { definition, .. } = lhs.operation() else {
        panic!("retained lhs remains a function-value occurrence");
    };
    assert_eq!(definition.id(), first.id());

    let fold = function
        .statements()
        .find_map(|statement| match statement {
            dae::FunctionStatementView::For { fold, .. } => view.function_fold(fold),
            dae::FunctionStatementView::Assignment { .. } => None,
        })
        .expect("sum fixture retains its fold");
    assert_eq!(fold.initial_values().get(0).unwrap().id(), second.id());
}

#[test]
fn holonomic_reconstruction_preserves_checked_function_owners() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            record_companion: true,
            functions: true,
            holonomic: true,
            ..FixtureFeatures::default()
        },
    );
    let constraint = model
        .inspect(holonomic_constraints)
        .into_iter()
        .next()
        .expect("fixture exposes a twice-differentiable state constraint");
    let expected_inventory = model.inspect(function_expression_inventory);
    let (rebuilt, manifold) = rebuild_holonomic_constraint(&model, constraint)
        .expect("holonomic replacement reconstructs checked function owners");
    assert_eq!(manifold.len(), 2);
    rebuilt.inspect(|view| {
        assert_eq!(view.function_count(), 2);
        assert_sum_function_body(view, find_function(view, "sum3"));
        assert_record_function_body(view, find_function(view, "makePair"));
        assert_eq!(function_expression_inventory(view), expected_inventory);
        let generated = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                expression.provenance().origin()
                    == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::IndexReduction)
            })
            .collect::<Vec<_>>();
        assert!(!generated.is_empty());
        assert!(
            generated
                .iter()
                .all(|expression| { expression.provenance().span() == constraint.owner.span() })
        );
    });
}

#[test]
fn function_reconstruction_is_stack_bounded_and_deterministic() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            functions: true,
            deep_function: true,
            ..FixtureFeatures::default()
        },
    );
    let candidate = model
        .inspect(direct_state_constraints)
        .admissible
        .into_iter()
        .next()
        .expect("fixture has a direct state constraint");
    let first =
        rebuild_with_state_demotion(&model, candidate).expect("deep function DAG reconstructs");
    let second =
        rebuild_with_state_demotion(&model, candidate).expect("repeat reconstruction succeeds");
    assert_eq!(
        serde_json::to_vec(&first).expect("first DAE serializes"),
        serde_json::to_vec(&second).expect("second DAE serializes"),
        "iterative postorder reconstruction has deterministic dense insertion"
    );
    first.inspect(|view| {
        let generated = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                expression.provenance().origin()
                    == dae::DaeProvenanceOrigin::Generated(
                        dae::DaeGeneration::FunctionAggregateLowering,
                    )
            })
            .count();
        assert_eq!(generated, 4_096);
    });
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct FunctionExpressionFingerprint {
    origin: dae::DaeProvenanceOrigin,
    source: u64,
    start: usize,
    end: usize,
    operation: String,
    value_type: String,
    function_scope: Option<u32>,
    binder_domain: Option<u32>,
    edges: Vec<(u64, usize, usize)>,
}

fn function_expression_inventory(view: dae::DaeView<'_>) -> Vec<FunctionExpressionFingerprint> {
    let start = find_function(view, "sum3").declaration().span().start.0;
    let end = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| view.source_text(expression.provenance()) == Some("makePair(1).left"))
        .expect("function fixture terminator resolves")
        .provenance()
        .span()
        .end
        .0;
    let mut inventory = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .filter(|expression| {
            let provenance = expression.provenance();
            let span = provenance.span();
            span.start.0 >= start
                && span.end.0 <= end
                && matches!(
                    provenance.origin(),
                    dae::DaeProvenanceOrigin::Source
                        | dae::DaeProvenanceOrigin::Generated(
                            dae::DaeGeneration::FunctionLoopLowering
                        )
                )
        })
        .map(|expression| function_expression_fingerprint(view, expression))
        .collect::<Vec<_>>();
    inventory.sort();
    inventory
}

fn function_expression_fingerprint<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExpressionView<'dae>,
) -> FunctionExpressionFingerprint {
    let provenance = expression.provenance();
    let span = provenance.span();
    FunctionExpressionFingerprint {
        origin: provenance.origin(),
        source: span.source.0,
        start: span.start.0,
        end: span.end.0,
        operation: function_operation_fingerprint(expression.operation()),
        value_type: format!("{:?}", expression.value_type()),
        function_scope: expression.function_scope().map(dae::FunctionId::index),
        binder_domain: expression.binder_domain().map(dae::DomainId::index),
        edges: function_expression_edges(view, expression),
    }
}

fn function_expression_edges<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExpressionView<'dae>,
) -> Vec<(u64, usize, usize)> {
    let mut edges = Vec::new();
    let mut push = |id| {
        let span = view
            .expression(id)
            .expect("function expression child resolves")
            .provenance()
            .span();
        edges.push((span.source.0, span.start.0, span.end.0));
    };
    match expression.operation() {
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(_)
        | dae::ExpressionOperation::FunctionFoldParameter { .. }
        | dae::ExpressionOperation::FunctionFoldOutput { .. } => {}
        dae::ExpressionOperation::Range(range) => {
            push(range.start().expression());
            if let Some(step) = range.explicit_step() {
                push(step.expression());
            }
            push(range.stop().expression());
        }
        dae::ExpressionOperation::Unary { operand, .. } => push(operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            push(lhs);
            push(rhs);
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            push(value);
            push_string_format_edges(format, &mut push);
        }
        dae::ExpressionOperation::Conditional(operands)
        | dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Record(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        }
        | dae::ExpressionOperation::Call {
            arguments: operands,
            ..
        } => operands.iter().for_each(&mut push),
        dae::ExpressionOperation::Field { base, .. }
        | dae::ExpressionOperation::Comprehension { body: base, .. } => push(base),
        dae::ExpressionOperation::Index { base, subscripts } => {
            push(base);
            push_subscript_edges(subscripts, &mut push);
        }
        dae::ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } => {
            push(base);
            push(value);
            push_subscript_edges(subscripts, &mut push);
        }
        dae::ExpressionOperation::FunctionValue { definition, .. } => push(definition.rhs()),
    }
    edges
}

fn push_string_format_edges<'dae>(
    format: dae::StringConversionFormatView<'dae>,
    push: &mut impl FnMut(dae::ExprId<'dae>),
) {
    match format {
        dae::StringConversionFormatView::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => [minimum_length, left_justified, significant_digits]
            .into_iter()
            .flatten()
            .for_each(push),
        dae::StringConversionFormatView::Format { value } => push(value),
    }
}

fn push_subscript_edges<'dae>(
    subscripts: dae::SubscriptsView<'dae>,
    push: &mut impl FnMut(dae::ExprId<'dae>),
) {
    for subscript in subscripts.iter() {
        match subscript {
            dae::SubscriptView::Index { expression, .. }
            | dae::SubscriptView::Slice { expression, .. } => push(expression),
            dae::SubscriptView::Whole { .. } => {}
        }
    }
}

fn function_operation_fingerprint(operation: dae::ExpressionOperation<'_>) -> String {
    match operation {
        dae::ExpressionOperation::Literal(literal) => format!("literal:{literal:?}"),
        dae::ExpressionOperation::Coordinate(coordinate) => coordinate_fingerprint(coordinate),
        dae::ExpressionOperation::Unary { operator, .. } => format!("unary:{operator:?}"),
        dae::ExpressionOperation::Binary { operator, .. } => format!("binary:{operator:?}"),
        dae::ExpressionOperation::Conditional(operands) => {
            format!("conditional:{}", operands.len())
        }
        dae::ExpressionOperation::Array(operands) => format!("array:{}", operands.len()),
        dae::ExpressionOperation::Record(fields) => format!("record:{}", fields.len()),
        dae::ExpressionOperation::Field { field, .. } => format!("field:{field}"),
        dae::ExpressionOperation::Range(range) => format!(
            "range:{}:{:?}:{}",
            range.start().value(),
            range.explicit_step().map(|step| step.value()),
            range.stop().value()
        ),
        dae::ExpressionOperation::Comprehension { domain, .. } => {
            format!("comprehension:{}", domain.index())
        }
        dae::ExpressionOperation::Index { subscripts, .. } => {
            format!("index:{}", subscripts.len())
        }
        dae::ExpressionOperation::ArrayUpdate { subscripts, .. } => {
            format!("array_update:{}", subscripts.len())
        }
        dae::ExpressionOperation::Builtin { builtin, arguments } => {
            format!("builtin:{builtin:?}:{}", arguments.len())
        }
        dae::ExpressionOperation::Call {
            function,
            output,
            arguments,
        } => format!("call:{}:{output}:{}", function.index(), arguments.len()),
        dae::ExpressionOperation::StringConversion {
            declaration,
            format,
            ..
        } => format!("string_conversion:{}:{format:?}", declaration.index()),
        dae::ExpressionOperation::FunctionValue { value, definition } => format!(
            "function_value:{}:{}:{}",
            value.function().index(),
            value.ordinal(),
            definition.id().ordinal()
        ),
        dae::ExpressionOperation::FunctionFoldParameter {
            fold,
            carried,
            definition,
        } => format!(
            "fold_parameter:{}:{}:{carried}:{}",
            fold.function().index(),
            fold.ordinal(),
            definition.id().ordinal()
        ),
        dae::ExpressionOperation::FunctionFoldOutput {
            fold,
            carried,
            definition,
        } => format!(
            "fold_output:{}:{}:{carried}:{}",
            fold.function().index(),
            fold.ordinal(),
            definition.id().ordinal()
        ),
    }
}

fn coordinate_fingerprint(coordinate: dae::CoordinateView<'_>) -> String {
    match coordinate {
        dae::CoordinateView::Parameter(id) => format!("parameter:{}", id.index()),
        dae::CoordinateView::Input(id) => format!("input:{}", id.index()),
        dae::CoordinateView::State(id) => format!("state:{}", id.index()),
        dae::CoordinateView::Derivative(id) => format!("derivative:{}", id.index()),
        dae::CoordinateView::Algebraic(id) => format!("algebraic:{}", id.index()),
        dae::CoordinateView::DiscreteReal(id) => format!("discrete_real:{}", id.index()),
        dae::CoordinateView::DiscreteValue(id) => format!("discrete_value:{}", id.index()),
        dae::CoordinateView::PreDiscreteReal(id) => {
            format!("pre_discrete_real:{}", id.index())
        }
        dae::CoordinateView::PreDiscreteValue(id) => {
            format!("pre_discrete_value:{}", id.index())
        }
        dae::CoordinateView::Time => "time".to_owned(),
        dae::CoordinateView::ClockInterval(id) => format!("clock_interval:{}", id.index()),
        dae::CoordinateView::Condition(id) => format!("condition:{}", id.index()),
        dae::CoordinateView::Delay(id) => format!("delay:{}", id.index()),
        dae::CoordinateView::Previous(id) => format!("previous:{}", id.index()),
        dae::CoordinateView::Terminal(id) => format!("terminal:{}", id.index()),
        dae::CoordinateView::Binder(id) => {
            format!("binder:{}:{}", id.domain().index(), id.ordinal())
        }
        dae::CoordinateView::FunctionParameter(id) => format!(
            "function_parameter:{}:{}",
            id.function().index(),
            id.ordinal()
        ),
    }
}

fn assert_rebuilt_functions(view: dae::DaeView<'_>) {
    assert_eq!(view.function_count(), 2);
    let sum = find_function(view, "sum3");
    assert_eq!(view.source_text(sum.declaration()), Some("function sum3"));
    assert_eq!(
        view.source_text(sum.parameters().next().unwrap().declaration()),
        Some("input Real u")
    );
    let values = sum.values().collect::<Vec<_>>();
    assert_eq!(
        view.source_text(values[0].declaration()),
        Some("output Real y")
    );
    assert_eq!(
        view.source_text(values[1].declaration()),
        Some("protected Real acc")
    );
    assert_sum_function_body(view, sum);
    assert_record_function_body(view, find_function(view, "makePair"));
    assert!(sort(view).is_ok());
}

fn find_function<'dae>(view: dae::DaeView<'dae>, name: &str) -> dae::FunctionView<'dae> {
    (0..view.function_count())
        .filter_map(|index| view.function_id(index))
        .filter_map(|id| view.function(id))
        .find(|function| function.name().as_str() == name)
        .expect("named function survives")
}

fn assert_sum_function_body<'dae>(view: dae::DaeView<'dae>, function: dae::FunctionView<'dae>) {
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 3);
    let dae::FunctionStatementView::Assignment { definition } = statements[0] else {
        panic!("first sum3 statement remains an assignment");
    };
    assert_eq!(
        view.source_text(definition.provenance()),
        Some("acc := if u <= 0 then 0 else sum3(u - 1)")
    );
    let dae::FunctionStatementView::For {
        fold,
        statements: loop_statements,
        provenance,
    } = statements[1].clone()
    else {
        panic!("second sum3 statement remains a compact fold");
    };
    assert_eq!(view.source_text(provenance), Some("for k in 1:3 loop"));
    let loop_statements = loop_statements.collect::<Vec<_>>();
    let dae::FunctionStatementView::Assignment { definition } = loop_statements[0] else {
        panic!("fold body remains an assignment");
    };
    assert_eq!(
        view.source_text(definition.provenance()),
        Some("acc := acc + k")
    );
    let fold = view.function_fold(fold).expect("fold identity resolves");
    assert_eq!(
        view.source_text(fold.provenance()),
        Some("for k in 1:3 loop")
    );
    for generated in fold
        .parameter_values()
        .iter()
        .chain(fold.output_values().iter())
    {
        let generated = view
            .expression(generated.rhs())
            .expect("fold expression resolves");
        assert_eq!(
            generated.provenance().origin(),
            dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::FunctionLoopLowering)
        );
        assert_eq!(
            view.source_text(generated.provenance()),
            Some("for k in 1:3 loop")
        );
    }
    let parameter_uses = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .filter(|expression| {
            expression.function_scope() == Some(function.id())
                && matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(_))
                )
        })
        .collect::<Vec<_>>();
    assert_eq!(parameter_uses.len(), 2);
    assert_ne!(
        parameter_uses[0].provenance().span().start,
        parameter_uses[1].provenance().span().start,
        "repeated parameter occurrences retain distinct byte ranges"
    );
    for parameter_use in parameter_uses {
        assert_exact_source_expression(view, parameter_use, function.declaration(), "u");
    }
    assert_function_calls(view, function.id());
}

fn assert_function_calls<'dae>(view: dae::DaeView<'dae>, function: dae::FunctionId<'dae>) {
    let calls = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .filter(|expression| {
            matches!(
                expression.operation(),
                dae::ExpressionOperation::Call {
                    function: called,
                    ..
                } if called == function
            )
        })
        .collect::<Vec<_>>();
    assert!(calls.iter().any(|call| {
        view.source_text(call.provenance()) == Some("sum3(u - 1)")
            && call.function_scope() == Some(function)
    }));
    assert!(calls.iter().any(|call| {
        view.source_text(call.provenance()) == Some("sum3(1)") && call.function_scope().is_none()
    }));
    let dead = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| view.source_text(expression.provenance()) == Some("acc + 0"))
        .expect("discarded scoped expression remains exactly reconstructible");
    let declaration = view
        .function(function)
        .expect("called function identity resolves")
        .declaration();
    assert_exact_source_expression(view, dead, declaration, "acc + 0");
    assert_eq!(dead.function_scope(), Some(function));
    assert!(matches!(
        dead.operation(),
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            ..
        }
    ));
}

fn assert_record_function_body<'dae>(view: dae::DaeView<'dae>, function: dae::FunctionView<'dae>) {
    assert_eq!(
        view.source_text(function.declaration()),
        Some("function makePair")
    );
    let result = view
        .expression(function.result_values().rhs(0).unwrap())
        .expect("record function result resolves");
    assert!(result.value_type().is_record());
    assert!(matches!(
        result.operation(),
        dae::ExpressionOperation::Record(_)
    ));
    assert_eq!(view.source_text(result.provenance()), Some("Pair(u, u)"));
    let fields = match result.operation() {
        dae::ExpressionOperation::Record(fields) => fields,
        _ => unreachable!("record function result stays a record"),
    };
    let parameter_uses = fields
        .iter()
        .map(|field| view.expression(field).expect("record field resolves"))
        .collect::<Vec<_>>();
    assert_eq!(parameter_uses.len(), 2);
    assert_ne!(
        parameter_uses[0].provenance().span().start,
        parameter_uses[1].provenance().span().start,
        "repeated record-field references retain distinct byte ranges"
    );
    for parameter_use in parameter_uses {
        assert_exact_source_expression(view, parameter_use, function.declaration(), "u");
    }
    let projection = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| view.source_text(expression.provenance()) == Some("makePair(1).left"))
        .expect("record-valued global call projection survives");
    assert!(matches!(
        projection.operation(),
        dae::ExpressionOperation::Field { field: 0, .. }
    ));
}

fn assert_exact_source_expression(
    view: dae::DaeView<'_>,
    expression: dae::ExpressionView<'_>,
    owner: dae::DaeProvenance,
    expected: &str,
) {
    let provenance = expression.provenance();
    let span = provenance.span();
    assert_eq!(
        provenance.origin(),
        dae::DaeProvenanceOrigin::Source,
        "{expected} remains a source occurrence"
    );
    assert_eq!(span.source, owner.span().source);
    assert_eq!(span.end.0 - span.start.0, expected.len());
    assert_eq!(view.source_text(provenance), Some(expected));
}

pub(super) struct FixtureFunctionConfig<'source, 'dae> {
    pub(super) real: dae::ValueTypeId<'dae>,
    pub(super) record: Option<dae::ValueTypeId<'dae>>,
    pub(super) source: rumoca_core::SourceId,
    pub(super) text: &'source str,
    pub(super) features: FixtureFeatures,
}

pub(super) fn insert_fixture_functions<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    config: FixtureFunctionConfig<'_, 'dae>,
) -> Result<(), dae::DaeConstructionError> {
    if !config.features.functions {
        return Ok(());
    }
    insert_recursive_fold_function(
        model,
        config.real,
        config.source,
        config.text,
        config.features.deep_function,
        config.features.same_rhs_definitions,
    )?;
    if let Some(record) = config.record {
        insert_record_function(model, config.real, record, config.source, config.text)?;
    }
    Ok(())
}

struct SumFunctionOwners<'dae> {
    function: dae::FunctionId<'dae>,
    parameter: dae::FunctionParameterId<'dae>,
    output: dae::FunctionValueId<'dae>,
    accumulator: dae::FunctionValueId<'dae>,
    body: dae::FunctionBody<'dae>,
    domain: dae::DomainId<'dae>,
    binder: dae::DomainBinderId<'dae>,
}

fn declare_sum_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    reservation: dae::FunctionReservation<'_, 'dae>,
) -> Result<SumFunctionOwners<'dae>, dae::DaeConstructionError> {
    let declaration = source_provenance(source, text, "function sum3");
    let loop_at = source_provenance(source, text, "for k in 1:3 loop");
    let binder_at = nested_source_provenance(source, text, "acc := acc + k", "k", 0);
    let domain = model.domains(|domains| {
        domains.structured(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    id: 0,
                    display_name: "k".to_owned(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            loop_at,
        )
    })?;
    let binder = model.domains(|domains| domains.binder(domain, 0, binder_at))?;
    let function = reservation.function();
    let parameter = model.functions(|functions| {
        functions.parameter(
            &reservation,
            VarName::new("u"),
            0,
            source_provenance(source, text, "input Real u"),
        )
    })?;
    let output = model.functions(|functions| {
        functions.output(
            &reservation,
            VarName::new("y"),
            0,
            source_provenance(source, text, "output Real y"),
        )
    })?;
    let accumulator = model.functions(|functions| {
        functions.local(
            &reservation,
            VarName::new("acc"),
            real,
            source_provenance(source, text, "protected Real acc"),
        )
    })?;
    let body = model.functions(|functions| functions.begin(reservation, declaration))?;
    Ok(SumFunctionOwners {
        function,
        parameter,
        output,
        accumulator,
        body,
        domain,
        binder,
    })
}

fn insert_deep_chain<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    mut expression: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
    enabled: bool,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    if !enabled {
        return Ok(None);
    }
    let generated =
        dae::DaeProvenance::generated(dae::DaeGeneration::FunctionAggregateLowering, owner.span())?;
    expression = model.expressions(|expressions| {
        for _ in 0..4_096 {
            expression = expressions
                .at(generated)
                .unary(dae::UnaryOperator::Plus, expression)?;
        }
        Ok(expression)
    })?;
    Ok(Some(expression))
}

fn insert_recursive_fold_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    deep: bool,
    same_rhs_definitions: bool,
) -> Result<(), dae::DaeConstructionError> {
    let declaration = source_provenance(source, text, "function sum3");
    let loop_at = source_provenance(source, text, "for k in 1:3 loop");
    let loop_assignment = source_provenance(source, text, "acc := acc + k");
    let accumulator_use = nested_source_provenance(source, text, "acc := acc + k", "acc", 1);
    let binder_use = nested_source_provenance(source, text, "acc := acc + k", "k", 0);
    let addition_at = source_provenance(source, text, "acc + k");
    let output_assignment = source_provenance(source, text, "y := acc");
    let output_use = nested_source_provenance(source, text, "y := acc", "acc", 0);
    let global_call_at = source_provenance(source, text, "sum3(1)");
    let global_argument_at = nested_source_provenance(source, text, "sum3(1)", "1", 0);
    let signature = dae::FunctionSignature::new(VarName::new("sum3"), [real], [real], declaration);
    let (functions, ()) = model.recursive_functions(signature, [], |model, reservations| {
        let reservation = reservations
            .into_iter()
            .next()
            .expect("one recursive fixture function is reserved");
        let mut owners = declare_sum_function(model, real, source, text, reservation)?;
        initialize_sum_function(model, &mut owners, source, text, deep, same_rhs_definitions)?;
        let mut loop_body = model.functions(|functions| {
            functions.begin_loop(owners.body, owners.domain, [owners.accumulator], loop_at)
        })?;
        let accumulator_value = model.functions(|functions| {
            functions.read(loop_body.body(), owners.accumulator, accumulator_use)
        })?;
        let binder_value =
            model.expressions(|expressions| expressions.at(binder_use).binder(owners.binder))?;
        let update = model.expressions(|expressions| {
            expressions.at(addition_at).binary(
                dae::BinaryOperator::Add,
                accumulator_value,
                binder_value,
            )
        })?;
        model.functions(|functions| {
            functions.assign_loop(&mut loop_body, owners.accumulator, update, loop_assignment)
        })?;
        owners.body = model.functions(|functions| functions.finish_loop(loop_body, loop_at))?;
        let result = model
            .functions(|functions| functions.read(&owners.body, owners.accumulator, output_use))?;
        model.functions(|functions| {
            functions.assign(&mut owners.body, owners.output, result, output_assignment)
        })?;
        model.functions(|functions| functions.define(owners.body, declaration))
    })?;
    let function = functions[0];
    let argument = model.expressions(|expressions| {
        expressions
            .at(global_argument_at)
            .literal(dae::DaeLiteral::Real(1.0))
    })?;
    model
        .expressions(|expressions| expressions.at(global_call_at).call(function, 0, [argument]))
        .map(|_| ())
}

fn initialize_sum_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    owners: &mut SumFunctionOwners<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    deep: bool,
    same_rhs_definitions: bool,
) -> Result<(), dae::DaeConstructionError> {
    let condition_at = source_provenance(source, text, "u <= 0");
    let condition_parameter_at = nested_source_provenance(source, text, "u <= 0", "u", 0);
    let condition_zero_at = nested_source_provenance(source, text, "u <= 0", "0", 0);
    let branch_zero_at = nested_source_provenance(source, text, "if u <= 0 then 0 else", "0", 1);
    let decrement_at = source_provenance(source, text, "u - 1");
    let recursive_parameter_at = nested_source_provenance(source, text, "u - 1", "u", 0);
    let one_at = nested_source_provenance(source, text, "u - 1", "1", 0);
    let (condition, branch_zero, decrement) = model.expressions(|expressions| {
        let condition_parameter = expressions
            .at(condition_parameter_at)
            .function_parameter(owners.parameter)?;
        let condition_zero = expressions
            .at(condition_zero_at)
            .literal(dae::DaeLiteral::Real(0.0))?;
        let condition = expressions.at(condition_at).binary(
            dae::BinaryOperator::LessEqual,
            condition_parameter,
            condition_zero,
        )?;
        let branch_zero = expressions
            .at(branch_zero_at)
            .literal(dae::DaeLiteral::Real(0.0))?;
        let recursive_parameter = expressions
            .at(recursive_parameter_at)
            .function_parameter(owners.parameter)?;
        let one = expressions.at(one_at).literal(dae::DaeLiteral::Real(1.0))?;
        let decrement = expressions.at(decrement_at).binary(
            dae::BinaryOperator::Subtract,
            recursive_parameter,
            one,
        )?;
        Ok((condition, branch_zero, decrement))
    })?;
    let recursive = model.expressions(|expressions| {
        expressions
            .at(source_provenance(source, text, "sum3(u - 1)"))
            .call(owners.function, 0, [decrement])
    })?;
    let initial = model.expressions(|expressions| {
        expressions
            .at(source_provenance(
                source,
                text,
                "if u <= 0 then 0 else sum3(u - 1)",
            ))
            .conditional([(condition, branch_zero)], recursive)
    })?;
    model.functions(|functions| {
        functions.assign(
            &mut owners.body,
            owners.accumulator,
            initial,
            source_provenance(source, text, "acc := if u <= 0 then 0 else sum3(u - 1)"),
        )
    })?;
    let dead_expression_at = source_provenance(source, text, "acc + 0");
    let dead_accumulator = model.functions(|functions| {
        functions.read(
            &owners.body,
            owners.accumulator,
            nested_source_provenance(source, text, "acc + 0", "acc", 0),
        )
    })?;
    let dead = model.expressions(|expressions| {
        let zero = expressions
            .at(nested_source_provenance(source, text, "acc + 0", "0", 0))
            .literal(dae::DaeLiteral::Real(0.0))?;
        expressions
            .at(dead_expression_at)
            .binary(dae::BinaryOperator::Add, dead_accumulator, zero)
    })?;
    if same_rhs_definitions {
        model.functions(|functions| {
            functions.assign(
                &mut owners.body,
                owners.accumulator,
                initial,
                source_provenance_occurrence(
                    source,
                    text,
                    "acc := if u <= 0 then 0 else sum3(u - 1)",
                    1,
                ),
            )
        })?;
    }
    if let Some(deep) = insert_deep_chain(model, dead, dead_expression_at, deep)? {
        model.functions(|functions| {
            functions.assign(
                &mut owners.body,
                owners.accumulator,
                deep,
                dead_expression_at,
            )
        })?;
    }
    Ok(())
}

fn insert_record_function<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    record: dae::ValueTypeId<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
) -> Result<(), dae::DaeConstructionError> {
    let declaration = source_provenance(source, text, "function makePair");
    let parameter_at = source_provenance_occurrence(source, text, "input Real u", 1);
    let output_at = source_provenance(source, text, "output Pair p");
    let assignment_at = source_provenance(source, text, "p := Pair(u, u)");
    let constructor_at = source_provenance(source, text, "Pair(u, u)");
    let first_use = nested_source_provenance(source, text, "Pair(u, u)", "u", 0);
    let second_use = nested_source_provenance(source, text, "Pair(u, u)", "u", 1);
    let call_at = source_provenance(source, text, "makePair(1)");
    let argument_at = nested_source_provenance(source, text, "makePair(1)", "1", 0);
    let projection_at = source_provenance(source, text, "makePair(1).left");
    let signature =
        dae::FunctionSignature::new(VarName::new("makePair"), [real], [record], declaration);
    let (function, ()) = model.function(signature, |model, reservation| {
        let parameter = model.functions(|functions| {
            functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
        })?;
        let output = model.functions(|functions| {
            functions.output(&reservation, VarName::new("p"), 0, output_at)
        })?;
        let mut body = model.functions(|functions| functions.begin(reservation, declaration))?;
        let fields = model.expressions(|expressions| {
            Ok([
                expressions.at(first_use).function_parameter(parameter)?,
                expressions.at(second_use).function_parameter(parameter)?,
            ])
        })?;
        let value = model
            .expressions(|expressions| expressions.at(constructor_at).record(record, fields))?;
        model.functions(|functions| functions.assign(&mut body, output, value, assignment_at))?;
        model.functions(|functions| functions.define(body, declaration))
    })?;
    let argument = model.expressions(|expressions| {
        expressions
            .at(argument_at)
            .literal(dae::DaeLiteral::Real(1.0))
    })?;
    let call =
        model.expressions(|expressions| expressions.at(call_at).call(function, 0, [argument]))?;
    model
        .expressions(|expressions| expressions.at(projection_at).field(call, 0))
        .map(|_| ())
}

pub(super) fn fixture_function_declarations(features: FixtureFeatures) -> String {
    if !features.functions {
        return String::new();
    }
    let record_function = if features.record_companion {
        " function makePair input Real u; output Pair p; algorithm p := Pair(u, u); end makePair; makePair(1).left;"
    } else {
        ""
    };
    let repeated_assignment = if features.same_rhs_definitions {
        " acc := if u <= 0 then 0 else sum3(u - 1);"
    } else {
        ""
    };
    format!(
        " function sum3 input Real u; output Real y; protected Real acc; algorithm acc := if u <= 0 then 0 else sum3(u - 1); /* retained arena occurrence: acc + 0 */{repeated_assignment} for k in 1:3 loop acc := acc + k; end for; y := acc; end sum3; sum3(1);{record_function}"
    )
}
