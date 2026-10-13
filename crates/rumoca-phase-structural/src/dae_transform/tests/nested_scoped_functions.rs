use super::*;

const SOURCE: &str = "function nested input Boolean flag; output Integer y; protected Integer capture; algorithm y := 0; for i in 1:2 loop if flag then capture := i; else capture := -i; end if; for j in 1:3 loop y := y + capture; end for; end for; end nested;";

fn range(name: &str, upper: i64) -> rumoca_core::StructuredIndexDomain {
    rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: name.into(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

/// The function, outer loop, inner loop, branch and read spans of `SOURCE`.
fn nested_scoped_spans(source: rumoca_core::SourceId) -> [dae::DaeProvenance; 5] {
    [
        "function nested",
        "for i in 1:2 loop",
        "for j in 1:3 loop",
        "if flag then",
        "y + capture",
    ]
    .map(|needle| source_provenance(source, SOURCE, needle))
}

fn nested_scoped_model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("nested_scoped.mo", SOURCE);
    let [at, outer_at, inner_at, branch_at, read_at] = nested_scoped_spans(source);
    dae::Dae::construct(sources, |model| {
        let (integer, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        model.function(
            dae::FunctionSignature::new(VarName::new("nested"), [boolean], [integer], at),
            |model, reservation| {
                let (flag, output, capture) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("capture"), integer, at)?,
                    ))
                })?;
                let (condition, zero) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(branch_at).function_parameter(flag)?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(0))?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, zero, at))?;
                let (outer_domain, inner_domain, binder) = model.domains(|domains| {
                    let outer = domains.structured(range("i", 2), outer_at)?;
                    Ok((
                        outer,
                        domains.nested(outer, range("j", 3), inner_at)?,
                        domains.binder(outer, 0, branch_at)?,
                    ))
                })?;
                let index =
                    model.expressions(|expressions| expressions.at(branch_at).binder(binder))?;
                let outer = model.functions(|functions| {
                    functions.begin_loop_with_iteration_locals(
                        body,
                        outer_domain,
                        [output],
                        [capture],
                        outer_at,
                    )
                })?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch_loop(outer, branch_at))?;
                model.functions(|functions| {
                    functions.assign(&mut child, capture, index, branch_at)
                })?;
                let (outer, selected) =
                    model.functions(|functions| functions.finish_branch_loop(finish, child))?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch_loop(outer, branch_at))?;
                let negative = model.expressions(|expressions| {
                    expressions
                        .at(branch_at)
                        .unary(dae::UnaryOperator::Negate, index)
                })?;
                model.functions(|functions| {
                    functions.assign(&mut child, capture, negative, branch_at)
                })?;
                let (mut outer, fallback) =
                    model.functions(|functions| functions.finish_branch_loop(finish, child))?;
                model.functions(|functions| {
                    functions.finish_conditional_loop(
                        &mut outer,
                        vec![(condition, selected)],
                        fallback,
                        branch_at,
                    )
                })?;
                let mut inner = model.functions(|functions| {
                    functions.begin_nested_loop(outer, inner_domain, [output], inner_at)
                })?;
                let (current, captured) = model.functions(|functions| {
                    Ok((
                        functions.read(inner.body(), output, read_at)?,
                        functions.read(inner.body(), capture, read_at)?,
                    ))
                })?;
                let sum = model.expressions(|expressions| {
                    expressions
                        .at(read_at)
                        .binary(dae::BinaryOperator::Add, current, captured)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut inner, output, sum, read_at)
                })?;
                let outer =
                    model.functions(|functions| functions.finish_nested_loop(inner, inner_at))?;
                let body = model.functions(|functions| functions.finish_loop(outer, outer_at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn check_nested_capture(view: dae::DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let outer = view.function_fold(function.fold_id(0).unwrap()).unwrap();
    let inner = view.function_fold(function.fold_id(1).unwrap()).unwrap();
    assert_eq!(inner.enclosing_fold(), Some(outer.id()));
    let capture = outer.iteration_locals().next().unwrap();
    assert_eq!(outer.iteration_locals().count(), 1);
    assert_eq!(inner.iteration_locals().count(), 0);
    assert!(inner.targets().all(|target| target != capture));
    let dae::FunctionStatementView::For { mut statements, .. } =
        function.statements().nth(1).unwrap()
    else {
        panic!("outer loop remains compact");
    };
    let dae::FunctionStatementView::Conditional { conditional } = statements.next().unwrap() else {
        panic!("outer iteration retains its scoped conditional");
    };
    let captured = conditional.outputs().next().unwrap();
    assert_eq!(captured.target(), capture);
    assert_eq!(
        inner.body_scope().relation(captured.id()),
        Some(dae::FunctionScopeRelation::Enclosing)
    );
    let dae::FunctionStatementView::For {
        statements: mut inner_statements,
        ..
    } = statements.next().unwrap()
    else {
        panic!("inner loop remains compact");
    };
    let dae::FunctionStatementView::Assignment { definition } = inner_statements.next().unwrap()
    else {
        panic!("inner transition is one assignment");
    };
    let dae::ExpressionOperation::Binary { rhs, .. } =
        view.expression(definition.rhs()).unwrap().operation()
    else {
        panic!("inner transition retains its addition");
    };
    let expression = view.expression(rhs).unwrap();
    assert!(
        matches!(expression.operation(), dae::ExpressionOperation::FunctionValue { value, definition } if value == capture && definition.id() == captured.id())
    );
    assert_eq!(expression.binder_domain(), Some(outer.domain()));
}

#[test]
fn structural_reconstruction_preserves_outer_scoped_join_as_nested_loop_capture() {
    let model = nested_scoped_model();
    model.inspect(check_nested_capture);
    let (rebuilt, _) =
        super::super::reconstruction::rebuild_derivative_aliases(&model, &[], &[]).unwrap();
    rebuilt.inspect(check_nested_capture);
    let decoded: dae::Dae = serde_json::from_slice(&serde_json::to_vec(&rebuilt).unwrap()).unwrap();
    decoded.inspect(check_nested_capture);
}

const PARTIAL_SOURCE: &str = "function partial input Boolean flag; output Integer y; protected Integer spare; algorithm if flag then y := 1; spare := 1; else y := 2; end if; for i in 1:2 loop y := y + spare; end for; end partial;";

fn partial_local_loop_model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("partial_local_loop.mo", PARTIAL_SOURCE);
    let [at, branch_at, loop_at, read_at] = [
        "function partial",
        "if flag then",
        "for i in 1:2 loop",
        "y + spare",
    ]
    .map(|needle| source_provenance(source, PARTIAL_SOURCE, needle));
    dae::Dae::construct(sources, |model| {
        let (integer, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        model.function(
            dae::FunctionSignature::new(VarName::new("partial"), [boolean], [integer], at),
            |model, reservation| {
                let (flag, output, spare) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("flag"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("spare"), integer, at)?,
                    ))
                })?;
                let (condition, one, two) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(branch_at).function_parameter(flag)?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(2))?,
                    ))
                })?;
                let parent = model.functions(|functions| functions.begin(reservation, at))?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch(parent, branch_at))?;
                model.functions(|functions| {
                    functions.assign_all(&mut child, &[(output, one), (spare, one)], branch_at)
                })?;
                let (parent, selected) =
                    model.functions(|functions| functions.finish_branch(finish, child))?;
                let (mut child, finish) =
                    model.functions(|functions| functions.begin_branch(parent, branch_at))?;
                model
                    .functions(|functions| functions.assign(&mut child, output, two, branch_at))?;
                let (mut body, fallback) =
                    model.functions(|functions| functions.finish_branch(finish, child))?;
                model.functions(|functions| {
                    functions.finish_conditional(
                        &mut body,
                        vec![(condition, selected)],
                        fallback,
                        branch_at,
                    )
                })?;
                let domain = model.domains(|domains| domains.structured(range("i", 2), loop_at))?;
                let mut looped = model
                    .functions(|functions| functions.begin_loop(body, domain, [output], loop_at))?;
                model.functions(|functions| {
                    functions
                        .require_assigned_loop(&mut looped, spare, read_at)
                        .map(|_| ())
                })?;
                let (current, held) = model.functions(|functions| {
                    Ok((
                        functions.read(looped.body(), output, read_at)?,
                        functions.read(looped.body(), spare, read_at)?,
                    ))
                })?;
                let sum = model.expressions(|expressions| {
                    expressions
                        .at(read_at)
                        .binary(dae::BinaryOperator::Add, current, held)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut looped, output, sum, read_at)
                })?;
                let body = model.functions(|functions| functions.finish_loop(looped, loop_at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn check_loop_assigned_check(view: dae::DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let Some(dae::FunctionStatementView::For { statements, .. }) = function.statements().nth(1)
    else {
        panic!("the loop stays compact");
    };
    assert!(statements.into_iter().any(|statement| matches!(
        statement,
        dae::FunctionStatementView::RequireAssigned { .. }
    )));
}

#[test]
fn structural_reconstruction_keeps_a_loop_local_assigned_state_check() {
    let model = partial_local_loop_model();
    model.inspect(check_loop_assigned_check);
    let (rebuilt, _) =
        super::super::reconstruction::rebuild_derivative_aliases(&model, &[], &[]).unwrap();
    rebuilt.inspect(check_loop_assigned_check);
}
