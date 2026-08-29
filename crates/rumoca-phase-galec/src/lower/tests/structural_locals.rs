use rumoca_core::{SourceMap, Span, VarName};

use super::*;

fn fixture_provenance(text: &str) -> (SourceMap, dae::DaeProvenance) {
    let mut sources = SourceMap::new();
    let source = sources.add("structural-identity-local.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    (
        sources,
        dae::DaeProvenance::source(span).expect("fixture span is concrete"),
    )
}

fn index<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    base: dae::ExprId<'dae>,
    row: i64,
    column: i64,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        let row = expressions.at(at).literal(dae::DaeLiteral::Integer(row))?;
        let column = expressions
            .at(at)
            .literal(dae::DaeLiteral::Integer(column))?;
        expressions.at(at).index(
            base,
            [
                dae::Subscript::Index {
                    expression: row,
                    provenance: at,
                },
                dae::Subscript::Index {
                    expression: column,
                    provenance: at,
                },
            ],
        )
    })
}

fn identity<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        let extent = expressions.at(at).literal(dae::DaeLiteral::Integer(2))?;
        expressions
            .at(at)
            .builtin(dae::PureBuiltin::Identity, [extent])
    })
}

fn one_definition_identity_local() -> dae::Dae {
    let text = "function f; output Real y; protected Real I2[2,2]; algorithm assert(false, \"kept\"); I2 := identity(2); y := I2[1,1] + I2[1,2]; end f;";
    let (sources, at) = fixture_provenance(text);
    dae::Dae::construct(sources, |dae| {
        let (real, matrix) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
            ))
        })?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [real], at),
            |dae, reservation| {
                let (output, local) = dae.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("I2"), matrix, at)?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let (condition, message) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::Boolean(false))?,
                        expressions
                            .at(at)
                            .literal(dae::DaeLiteral::String("kept".to_owned()))?,
                    ))
                })?;
                dae.functions(|functions| functions.assertion(&mut body, condition, message, at))?;
                let identity = identity(dae, at)?;
                dae.functions(|functions| functions.assign(&mut body, local, identity, at))?;
                let local = dae.functions(|functions| functions.read(&body, local, at))?;
                let diagonal = index(dae, local, 1, 1, at)?;
                let off_diagonal = index(dae, local, 1, 2, at)?;
                let sum = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, diagonal, off_diagonal)
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, sum, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("checked identity-local fixture constructs")
}

fn partially_updated_identity_local() -> dae::Dae {
    let text = "function f; output Real y; protected Real I2[2,2]; algorithm I2 := identity(2); I2[1,2] := 5; y := I2[1,2]; end f;";
    let (sources, at) = fixture_provenance(text);
    dae::Dae::construct(sources, |dae| {
        let (real, matrix) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
            ))
        })?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [real], at),
            |dae, reservation| {
                let (output, local) = dae.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("I2"), matrix, at)?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let identity = identity(dae, at)?;
                dae.functions(|functions| functions.assign(&mut body, local, identity, at))?;
                let base = dae.functions(|functions| functions.read(&body, local, at))?;
                let (row, column, five) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(2))?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(5.0))?,
                    ))
                })?;
                let updated = dae.expressions(|expressions| {
                    expressions.at(at).array_update(
                        base,
                        five,
                        [
                            dae::Subscript::Index {
                                expression: row,
                                provenance: at,
                            },
                            dae::Subscript::Index {
                                expression: column,
                                provenance: at,
                            },
                        ],
                    )
                })?;
                dae.functions(|functions| functions.assign(&mut body, local, updated, at))?;
                let current = dae.functions(|functions| functions.read(&body, local, at))?;
                let selected = index(dae, current, 1, 2, at)?;
                dae.functions(|functions| functions.assign(&mut body, output, selected, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("checked partial-update fixture constructs")
}

fn conditionally_assigned_identity_local() -> dae::Dae {
    let text = "function f; input Boolean chooseIdentity; output Real y; protected Real I2[2,2]; algorithm if chooseIdentity then I2 := identity(2); else I2 := identity(2); end if; y := I2[1,1]; end f;";
    let (sources, at) = fixture_provenance(text);
    dae::Dae::construct(sources, |dae| {
        let (boolean, real, matrix) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)?,
            ))
        })?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [boolean], [real], at),
            |dae, reservation| {
                let (condition, output, local) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("chooseIdentity"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                        functions.local(&reservation, VarName::new("I2"), matrix, at)?,
                    ))
                })?;
                let condition = dae
                    .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
                let branch_identity = identity(dae, at)?;
                let fallback_identity = identity(dae, at)?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                dae.functions(|functions| {
                    functions.assign_conditional_all(
                        &mut body,
                        &[local],
                        &[condition],
                        &[vec![branch_identity]],
                        &[fallback_identity],
                        at,
                    )
                })?;
                let current = dae.functions(|functions| functions.read(&body, local, at))?;
                let selected = index(dae, current, 1, 1, at)?;
                dae.functions(|functions| functions.assign(&mut body, output, selected, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("checked conditional identity-local fixture constructs")
}

fn identity_output() -> dae::Dae {
    let text = "function f; output Real I2[2,2]; algorithm I2 := identity(2); end f;";
    let (sources, at) = fixture_provenance(text);
    dae::Dae::construct(sources, |dae| {
        let matrix = dae.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)
        })?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [matrix], at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("I2"), 0, at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let identity = identity(dae, at)?;
                dae.functions(|functions| functions.assign(&mut body, output, identity, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("checked identity-output fixture constructs")
}

fn whole_identity_local_output() -> dae::Dae {
    let text = "function f; output Real Y[2,2]; protected Real I2[2,2]; algorithm I2 := identity(2); Y := I2; end f;";
    let (sources, at) = fixture_provenance(text);
    dae::Dae::construct(sources, |dae| {
        let matrix = dae.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 2]), at)
        })?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [matrix], at),
            |dae, reservation| {
                let (output, local) = dae.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("Y"), 0, at)?,
                        functions.local(&reservation, VarName::new("I2"), matrix, at)?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let identity = identity(dae, at)?;
                dae.functions(|functions| functions.assign(&mut body, local, identity, at))?;
                let local = dae.functions(|functions| functions.read(&body, local, at))?;
                dae.functions(|functions| functions.assign(&mut body, output, local, at))?;
                dae.functions(|functions| functions.define(body, at))
            },
        )?;
        Ok(())
    })
    .expect("checked whole-output fixture constructs")
}

fn lower_only(model: &dae::Dae) -> Vec<gast::UserFunction> {
    model.inspect(|view| {
        let function = view.function_id(0).expect("fixture has one function");
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([function.index()]),
            EmissionFacts::structured(),
        )
        .expect("checked function projects")
    })
}

fn lower_only_function(model: &dae::Dae) -> gast::UserFunction {
    let mut lowered = lower_only(model);
    assert_eq!(lowered.len(), 1);
    lowered.pop().expect("one projected function")
}

fn statement_assigns_name(statement: &gast::Statement, name: &str) -> bool {
    match statement {
        gast::Statement::Assignment { target, .. } => reference_name(target) == Some(name),
        gast::Statement::For(loop_) => loop_
            .body
            .iter()
            .any(|statement| statement_assigns_name(&statement.node, name)),
        gast::Statement::If(if_) => if_
            .branches
            .iter()
            .flat_map(|branch| &branch.body)
            .chain(if_.else_body.iter().flatten())
            .any(|statement| statement_assigns_name(&statement.node, name)),
        _ => false,
    }
}

fn reference_name(reference: &gast::Reference) -> Option<&str> {
    match reference {
        gast::Reference::Local(part) => Some(part.name.lexeme()),
        gast::Reference::State(_) => None,
    }
}

fn find_assignment<'a>(
    statements: &'a [gast::Spanned<gast::Statement>],
    name: &str,
) -> Option<(&'a gast::RefPart, &'a gast::Expression)> {
    statements
        .iter()
        .find_map(|statement| match &statement.node {
            gast::Statement::Assignment {
                target: gast::Reference::Local(target),
                value,
            } if target.name.lexeme() == name => Some((target, value)),
            gast::Statement::For(loop_) => find_assignment(&loop_.body, name),
            gast::Statement::If(if_) => if_
                .branches
                .iter()
                .find_map(|branch| find_assignment(&branch.body, name))
                .or_else(|| {
                    if_.else_body
                        .as_deref()
                        .and_then(|body| find_assignment(body, name))
                }),
            _ => None,
        })
}

fn is_real_of_integer(expression: &gast::Expression, expected: i64) -> bool {
    matches!(
        expression,
        gast::Expression::Call(gast::FunctionCall { function, arguments })
            if function.lexeme() == "real"
                && arguments.as_slice() == [gast::Expression::Integer(expected)]
    )
}

fn is_dynamic_identity_projection(target: &gast::RefPart, expression: &gast::Expression) -> bool {
    let [row, column] = target.subscripts.as_slice() else {
        return false;
    };
    let gast::Expression::Call(gast::FunctionCall {
        function,
        arguments,
    }) = expression
    else {
        return false;
    };
    let [gast::Expression::If(identity)] = arguments.as_slice() else {
        return false;
    };
    let [(condition, gast::Expression::Integer(1))] = identity.branches.as_slice() else {
        return false;
    };
    let gast::Expression::Binary {
        op: gast::BinaryOp::Eq,
        lhs,
        rhs,
    } = condition
    else {
        return false;
    };
    function.lexeme() == "real"
        && matches!(identity.else_value.as_ref(), gast::Expression::Integer(0))
        && ((lhs.as_ref() == row && rhs.as_ref() == column)
            || (lhs.as_ref() == column && rhs.as_ref() == row))
}

fn contains_signal(statement: &gast::Statement) -> bool {
    match statement {
        gast::Statement::Signal(_) => true,
        gast::Statement::For(loop_) => loop_
            .body
            .iter()
            .any(|statement| contains_signal(&statement.node)),
        gast::Statement::If(if_) => if_
            .branches
            .iter()
            .flat_map(|branch| &branch.body)
            .chain(if_.else_body.iter().flatten())
            .any(|statement| contains_signal(&statement.node)),
        _ => false,
    }
}

#[test]
fn single_definition_identity_local_stays_structural_through_function_lowering() {
    let mut lowered = lower_only(&one_definition_identity_local());
    let function = lowered.pop().expect("one projected function");

    assert!(
        function
            .locals
            .iter()
            .all(|local| local.name.lexeme() != "I2"),
        "a compact identity must not acquire dense local storage"
    );
    assert!(
        function
            .statements
            .iter()
            .all(|statement| !statement_assigns_name(&statement.node, "I2")),
        "a compact identity must not acquire an elementwise initialization loop"
    );
    let (_, value) = find_assignment(&function.statements, "y")
        .expect("the observable scalar consumer remains assigned");
    assert!(matches!(
        value,
        gast::Expression::Binary {
            op: gast::BinaryOp::Add,
            lhs,
            rhs,
        } if is_real_of_integer(lhs, 1) && is_real_of_integer(rhs, 0)
    ));
    assert!(
        function
            .statements
            .iter()
            .any(|statement| contains_signal(&statement.node)),
        "eliding a pure identity assignment must not remove an adjacent assertion"
    );
    assert_eq!(
        function.statements.len(),
        2,
        "the assertion and observable output both remain"
    );
}

#[test]
fn a_whole_array_read_projects_without_reintroducing_the_elided_local() {
    let function = lower_only_function(&whole_identity_local_output());

    assert!(
        function
            .locals
            .iter()
            .all(|local| local.name.lexeme() != "I2")
    );
    assert!(
        function
            .statements
            .iter()
            .all(|statement| !statement_assigns_name(&statement.node, "I2")),
        "a whole-array consumer must project the compact definition, not reference absent storage"
    );
    assert!(
        function
            .statements
            .iter()
            .any(|statement| statement_assigns_name(&statement.node, "Y")),
        "the observable whole-array output must still be assigned"
    );
    let (target, value) = find_assignment(&function.statements, "Y")
        .expect("the whole-array output has an elementwise projection");
    assert!(
        is_dynamic_identity_projection(target, value),
        "each Y[row,column] must be real(if row == column then 1 else 0)"
    );
}

#[test]
fn a_partially_updated_identity_local_keeps_its_storage_and_initialization() {
    let function = lower_only_function(&partially_updated_identity_local());

    assert!(
        function
            .locals
            .iter()
            .any(|local| local.name.lexeme() == "I2"),
        "a later partial update needs the identity value as reaching storage"
    );
    assert!(
        function
            .statements
            .iter()
            .any(|statement| statement_assigns_name(&statement.node, "I2")),
        "the identity initialization must not be silently dropped before a partial update"
    );
}

#[test]
fn a_conditionally_assigned_identity_local_remains_materialized() {
    let function = lower_only_function(&conditionally_assigned_identity_local());

    assert!(
        function
            .locals
            .iter()
            .any(|local| local.name.lexeme() == "I2"),
        "a path-conditional identity definition needs storage for its joined value"
    );
    assert!(
        function
            .statements
            .iter()
            .any(|statement| statement_assigns_name(&statement.node, "I2")),
        "a path-conditional identity assignment must not be silently elided"
    );
}

#[test]
fn an_identity_function_output_remains_an_observable_assignment() {
    let function = lower_only_function(&identity_output());

    assert!(function.parameters.iter().any(|parameter| {
        parameter.direction == gast::Direction::Output && parameter.decl.name.lexeme() == "I2"
    }));
    assert!(
        function
            .statements
            .iter()
            .any(|statement| statement_assigns_name(&statement.node, "I2")),
        "an identity-valued output is observable and cannot be elided"
    );
}
