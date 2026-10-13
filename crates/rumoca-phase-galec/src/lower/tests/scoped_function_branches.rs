use super::*;

fn lower_caller(model: &dae::Dae) -> Result<Vec<gast::UserFunction>, GalecTargetError> {
    model.inspect(|view| {
        let function = (0..)
            .map_while(|index| view.function_id(index))
            .find(|id| view.function(*id).unwrap().name().as_str() == "caller")
            .unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([function.index()]),
            EmissionFacts::structured(),
        )
    })
}

fn prefix_pair_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function caller if outer then (x,y):=pair(u); if inner then x:=3; else x:=4; end if; else x:=0; y:=0; end if; end caller;";
    let source = sources.add("scoped-pair.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (real, boolean) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        let pair = define_real_pair_function(dae, real, at)?;
        dae.function(
            dae::FunctionSignature::new(
                VarName::new("caller"),
                [boolean, boolean, real],
                [real, real],
                at,
            ),
            |dae, reservation| {
                let (outer, inner, u, x, y) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("outer"), 0, at)?,
                        functions.parameter(&reservation, VarName::new("inner"), 1, at)?,
                        functions.parameter(&reservation, VarName::new("u"), 2, at)?,
                        functions.output(&reservation, VarName::new("x"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 1, at)?,
                    ))
                })?;
                let (outer, inner, u, zero, three, four) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(outer)?,
                        expressions.at(at).function_parameter(inner)?,
                        expressions.at(at).function_parameter(u)?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(3.0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(4.0))?,
                    ))
                })?;
                let body = dae.functions(|functions| functions.begin(reservation, at))?;
                let (mut branch, outer_finish) =
                    dae.functions(|functions| functions.begin_branch(body, at))?;
                let results = dae.expressions(|expressions| {
                    expressions.at(at).call_results(pair, [0, 1], [u])
                })?;
                dae.functions(|functions| {
                    functions.assign_all(&mut branch, &[(x, results[0]), (y, results[1])], at)
                })?;
                let (mut selected, finish) =
                    dae.functions(|functions| functions.begin_branch(branch, at))?;
                dae.functions(|functions| functions.assign(&mut selected, x, three, at))?;
                let (branch, selected) =
                    dae.functions(|functions| functions.finish_branch(finish, selected))?;
                let (mut fallback, finish) =
                    dae.functions(|functions| functions.begin_branch(branch, at))?;
                dae.functions(|functions| functions.assign(&mut fallback, x, four, at))?;
                let (mut branch, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, fallback))?;
                dae.functions(|functions| {
                    functions.finish_conditional(&mut branch, vec![(inner, selected)], fallback, at)
                })?;
                let (body, selected) =
                    dae.functions(|functions| functions.finish_branch(outer_finish, branch))?;
                let (mut fallback, finish) =
                    dae.functions(|functions| functions.begin_branch(body, at))?;
                dae.functions(|functions| {
                    functions.assign_all(&mut fallback, &[(x, zero), (y, zero)], at)
                })?;
                let (mut body, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, fallback))?;
                dae.functions(|functions| {
                    functions.finish_conditional(&mut body, vec![(outer, selected)], fallback, at)
                })?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn scoped_function_prefix_tuple_remains_before_inner_overwrite() {
    let functions = lower_caller(&prefix_pair_fixture()).expect("complete ordered branches lower");
    let caller = functions
        .iter()
        .find(|function| function.name.lexeme() == "caller")
        .unwrap();
    assert_eq!(count_named_multi_calls(&caller.statements, "pair"), 1);
    let selection = caller
        .statements
        .iter()
        .find_map(|statement| match &statement.node {
            gast::Statement::If(selection) => Some(selection),
            _ => None,
        })
        .unwrap();
    let body = &selection.branches[0].body;
    let tuple = body
        .iter()
        .position(|statement| matches!(statement.node, gast::Statement::MultiAssignment { .. }))
        .unwrap();
    let inner = body
        .iter()
        .position(|statement| matches!(statement.node, gast::Statement::If(_)))
        .unwrap();
    assert!(tuple < inner);
    assert_eq!(
        count_named_multi_calls(selection.else_body.as_deref().unwrap(), "pair"),
        0
    );
}

#[test]
fn scoped_function_no_output_branch_retains_selected_assertion() {
    let mut sources = SourceMap::new();
    let text = "function caller input Boolean selected; output Real y; algorithm y:=0; if selected then assert(false, \"selected\"); end if; end caller;";
    let source = sources.add("scoped-effect.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let (real, boolean) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("caller"), [boolean], [real], at),
            |dae, reservation| {
                let (condition, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("selected"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let (condition, zero, fails, message) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(condition)?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Boolean(false))?,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::String("selected".to_owned()))?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, at))?;
                let (mut selected, finish) =
                    dae.functions(|functions| functions.begin_branch(body, at))?;
                dae.functions(|functions| functions.assertion(&mut selected, fails, message, at))?;
                let (body, selected) =
                    dae.functions(|functions| functions.finish_branch(finish, selected))?;
                let (fallback, finish) =
                    dae.functions(|functions| functions.begin_branch(body, at))?;
                let (mut body, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, fallback))?;
                dae.functions(|functions| {
                    functions.finish_conditional(
                        &mut body,
                        vec![(condition, selected)],
                        fallback,
                        at,
                    )
                })?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let functions = lower_caller(&model).expect("zero-output branch effect lowers");
    let caller = functions
        .iter()
        .find(|function| function.name.lexeme() == "caller")
        .unwrap();
    let selection = caller
        .statements
        .iter()
        .find_map(|statement| match &statement.node {
            gast::Statement::If(selection) => Some(selection),
            _ => None,
        })
        .expect("the selected effect retains its enclosing branch");
    assert!(!selection.branches[0].body.is_empty());
    assert!(selection.else_body.as_deref().is_none_or(<[_]>::is_empty));
}

#[test]
fn scoped_function_partial_read_has_spanful_target_refusal() {
    let mut sources = SourceMap::new();
    let text = "function caller input Boolean selected; output Real y; algorithm if selected then y:=1; end if; end caller;";
    let source = sources.add("scoped-partial.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let at = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let (real, boolean) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("caller"), [boolean], [real], at),
            |dae, reservation| {
                let (condition, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("selected"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let (condition, one) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).function_parameter(condition)?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?,
                    ))
                })?;
                let body = dae.functions(|functions| functions.begin(reservation, at))?;
                let (mut selected, finish) =
                    dae.functions(|functions| functions.begin_branch(body, at))?;
                dae.functions(|functions| functions.assign(&mut selected, output, one, at))?;
                let (body, selected) =
                    dae.functions(|functions| functions.finish_branch(finish, selected))?;
                let (fallback, finish) =
                    dae.functions(|functions| functions.begin_branch(body, at))?;
                let (mut body, fallback) =
                    dae.functions(|functions| functions.finish_branch(finish, fallback))?;
                dae.functions(|functions| {
                    functions.finish_conditional(
                        &mut body,
                        vec![(condition, selected)],
                        fallback,
                        at,
                    )
                })?;
                dae.functions(|functions| functions.require_assigned(&mut body, output, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let error = lower_caller(&model).expect_err("target does not admit absent state payloads");
    assert!(matches!(error,
        GalecTargetError::UnsupportedFeature { feature, span: actual, .. }
        if feature == "function-assigned-state" && actual == Some(span)
    ));
}
