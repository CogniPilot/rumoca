//! Typed function-call owners inlined into the rows that use them.
//!
//! Split out of `expressions.rs` at the module seam between expression-form
//! lowering and typed call-owner issuance, to keep both files under the
//! SPEC_0021 file-size threshold. A child module so the shared row-evaluation
//! helpers stay owned by the parent.

use super::*;

/// Lower with the scalar-runtime profile a solver simulates with: a call-scoped
/// assertion owns an event root there, while the stateless native profile
/// raises it at the publication boundary and constructs no root.
fn lower_simulation_package(dae: &dae::Dae) -> crate::LoweredSolvePackage {
    let selection = crate::state_selection::prepare(dae, &std::collections::HashMap::new())
        .expect("fixture selection");
    crate::lower_selection(
        &selection,
        &std::collections::HashMap::new(),
        rumoca_ir_solve::RefreshProfile::Scalar,
    )
    .expect("fixture lowers")
}

#[test]
fn demanded_function_call_issues_one_typed_owner() {
    let source =
        TestSource::new("function f input Real u; output Real y; y := u + 1; Real z; z=f(2);");
    let function_at = source.at(0, 51);
    let variable_at = source.at(52, 59);
    let owner = source.at(60, 67);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let local = model.functions(|functions| {
                functions.local(&reservation, VarName::new("tmp"), real, function_at)
            })?;
            let local_definition = model.expressions(|expressions| {
                let parameter = expressions.at(function_at).function_parameter(parameter)?;
                let one = expressions
                    .at(function_at)
                    .literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(function_at)
                    .binary(dae::BinaryOperator::Add, parameter, one)
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model.functions(|functions| {
                functions.assign(&mut body, local, local_definition, function_at)
            })?;
            let local_value =
                model.functions(|functions| functions.read(&body, local, function_at))?;
            let result = model.expressions(|expressions| {
                let two = expressions
                    .at(function_at)
                    .literal(dae::DaeLiteral::Real(2.0))?;
                expressions
                    .at(function_at)
                    .binary(dae::BinaryOperator::Multiply, local_value, two)
            })?;
            model
                .functions(|functions| functions.assign(&mut body, output, result, function_at))?;
            model.functions(|functions| functions.define(body, function_at))
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            // A time-varying argument keeps the call a call (a constant one
            // folds to a literal at construction).
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            let call = expressions.at(owner).call(function, 0, [time])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    let [owner] = package.pure_calls.owners() else {
        panic!("one exact demanded pure-call owner expected");
    };
    assert_eq!(owner.inputs().len(), 1);
    assert_eq!(owner.outputs().len(), 1);
    assert!(owner.body().operations().len() >= 5);

    let [ComputeNode::ScalarPrograms(rows)] = package.problem.continuous.residual.nodes.as_slice()
    else {
        panic!("one scalar residual block expected");
    };
    // The production typed owner is issued above, while scalar-only consumers
    // retain the scalar body.
    assert!(
        rows.programs()[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::Binary { .. }))
    );
}

#[test]
fn clocked_function_call_projects_issued_typed_owner_without_body_inlining() {
    let source = TestSource::new(
        "function f input Real u; output Real y; y := u + 1; discrete Real z; z=f(2);",
    );
    let function_at = source.at(0, 51);
    let variable_at = source.at(52, 68);
    let clock_at = source.at(68, 69);
    let owner = source.at(69, 76);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 100).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let value = model.expressions(|expressions| {
                let parameter = expressions.at(function_at).function_parameter(parameter)?;
                let one = expressions
                    .at(function_at)
                    .literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(function_at)
                    .binary(dae::BinaryOperator::Add, parameter, one)
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, function_at))?;
            model.functions(|functions| functions.define(body, function_at))
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, owner))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let call = expressions.at(owner).call(function, 0, [two])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, call)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    assert_eq!(package.pure_calls.owners().len(), 1);
    let [row] = package.problem.discrete.rhs.programs() else {
        panic!("one clocked discrete row expected");
    };
    assert_eq!(
        row.iter()
            .filter(|operation| matches!(operation, LinearOp::PureCall { .. }))
            .count(),
        1
    );
    assert_eq!(
        row.iter()
            .filter(|operation| matches!(operation, LinearOp::Binary { .. }))
            .count(),
        0,
        "the function body is not embedded in the clocked assignment row"
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction conditional fixture enumerates capture identity and call-frame reuse"
)]
fn total_function_conditional_selects_over_the_preceding_definition_once_per_call_frame() {
    let source = TestSource::new(
        "function f input Boolean c; input Real u; output Real y; Real tmp; end f; Real z; Real z2;",
    );
    let at = source.at(0, 81);
    let model = dae::Dae::construct(source.map, |model| {
        let boolean = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let signature = dae::FunctionSignature::new(VarName::new("f"), [boolean, real], [real], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let condition = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("c"), 0, at)
            })?;
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let base = model.functions(|functions| {
                functions.local(&reservation, VarName::new("base"), real, at)
            })?;
            let conditional_local = model.functions(|functions| {
                functions.local(&reservation, VarName::new("tmp"), real, at)
            })?;
            let local_definition = model.expressions(|expressions| {
                let input = expressions.at(at).function_parameter(input)?;
                let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, input, two)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, base, local_definition, at))?;
            let local_value = model.functions(|functions| functions.read(&body, base, at))?;
            let condition = model
                .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
            let (branch, fallback) = model.expressions(|expressions| {
                let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                Ok((
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, local_value, one)?,
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, local_value, two)?,
                ))
            })?;
            body = model.functions(|functions| {
                crate::tests::scoped_assignment_body(
                    functions,
                    body,
                    &[conditional_local],
                    &[condition],
                    &[vec![branch]],
                    &[fallback],
                    at,
                )
            })?;
            let conditional_value =
                model.functions(|functions| functions.read(&body, conditional_local, at))?;
            model.functions(|functions| {
                functions.assign(&mut body, output, conditional_value, at)
            })?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (algebraic, algebraic_2) = model.variables(|variables| {
            Ok((
                variables.algebraic(
                    VarName::new("z"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("z2"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (residual, residual_2) = model.expressions(|expressions| {
            let z = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let z2 = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic_2))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, zero)?;
            let input = expressions.at(at).literal(dae::DaeLiteral::Real(3.0))?;
            let call = expressions.at(at).call(function, 0, [condition, input])?;
            Ok((
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, z, call)?,
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, z2, call)?,
            ))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, residual)?;
            continuous.value_equation(at, residual_2)
        })
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    let solve = &package.problem;
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    let sites = rows
        .programs()
        .iter()
        .map(|row| {
            row.iter()
                .find_map(|operation| match operation {
                    LinearOp::PureCall { site, .. } => Some(site),
                    _ => None,
                })
                .unwrap_or_else(|| panic!("function call remains one checked owner: {row:#?}"))
        })
        .collect::<Vec<_>>();
    assert_eq!(sites.len(), 1, "both residuals share one call invocation");
    assert_eq!(rows.output_indices(), [0, 1]);
    let owner = package.pure_calls.owner(sites[0].owner()).unwrap();
    assert!(
        owner.body().operations().iter().all(|operation| !matches!(
            operation.operation(),
            rumoca_ir_solve::SolveOperation::Conditional { .. }
        )),
        "total scalar arms select in the parent scope (SOLVE-C61)"
    );
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::Binary {
                    operator: rumoca_ir_solve::SolveBinaryOperator::Multiply,
                    ..
                }
            ))
            .count(),
        1,
        "the preceding definition is computed once in the parent"
    );
    assert_eq!(
        eval_residual_rows_with_pure_calls(rows, &package.pure_calls, &[0.0, 0.0], &[]),
        [-8.0, -8.0]
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction tensor fixture enumerates one semantic aggregate capture range"
)]
fn function_conditional_captures_tensor_definition_as_one_semantic_range() {
    let source = TestSource::new(
        "function f input Boolean c; input Real u[3]; output Real y[3]; Real base[3]; Real tmp[3]; end f; parameter Real p[3]; Real z[3];",
    );
    let at = source.at(0, 124);
    let model = dae::Dae::construct(source.map, |model| {
        let (boolean, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), [boolean, vector], [vector], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let condition = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("c"), 0, at)
            })?;
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let base = model.functions(|functions| {
                functions.local(&reservation, VarName::new("base"), vector, at)
            })?;
            let selected = model.functions(|functions| {
                functions.local(&reservation, VarName::new("tmp"), vector, at)
            })?;
            let input_value =
                model.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, base, input_value, at))?;
            let base_value = model.functions(|functions| functions.read(&body, base, at))?;
            let condition = model
                .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
            body = model.functions(|functions| {
                crate::tests::scoped_assignment_body(
                    functions,
                    body,
                    &[selected],
                    &[condition],
                    &[vec![base_value]],
                    &[base_value],
                    at,
                )
            })?;
            let selected_value =
                model.functions(|functions| functions.read(&body, selected, at))?;
            model.functions(|functions| functions.assign(&mut body, output, selected_value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("z"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, zero)?;
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            let call = expressions.at(at).call(function, 0, [condition, p])?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    let solve = &package.problem;
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one tensor residual block expected");
    };
    let site = rows.programs()[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::PureCall { site, .. } => Some(site),
            _ => None,
        })
        .expect("tensor call remains one checked owner");
    let owner = package.pure_calls.owner(site.owner()).unwrap();
    let (captures, destinations, if_true, if_false) = owner
        .body()
        .operations()
        .iter()
        .find_map(|operation| match operation.operation() {
            rumoca_ir_solve::SolveOperation::Conditional {
                captures,
                destinations,
                if_true,
                if_false,
                ..
            } => Some((captures, destinations, if_true, if_false)),
            _ => None,
        })
        .expect("typed owner retains one checked tensor conditional");
    let region_operations = if_true
        .body()
        .operations()
        .iter()
        .chain(if_false.body().operations())
        .collect::<Vec<_>>();

    assert_eq!(
        captures.len(),
        1,
        "only the used tensor definition remains one typed capture"
    );
    assert_eq!(destinations.len(), 1, "the vector is one typed result");
    assert_eq!(
        owner.body().register_types()[destinations[0].index()].dimensions(),
        &[3]
    );
    assert_eq!(
        region_operations
            .iter()
            .filter(|operation| matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::Load { .. }
            ))
            .count(),
        2,
        "both result regions load their sole typed capture exactly once"
    );
    assert!(region_operations.iter().all(|operation| !matches!(
        operation.operation(),
        rumoca_ir_solve::SolveOperation::ProjectElement { .. }
    )));
    assert_eq!(
        eval_residual_rows_with_pure_calls(rows, &package.pure_calls, &[0.0; 3], &[1.0, 2.0, 3.0]),
        [-1.0, -2.0, -3.0]
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction aggregate conditional fixture enumerates every lazy tensor result branch"
)]
fn aggregate_conditional_expression_retains_one_lazy_tensor_result_range() {
    let source = TestSource::new(
        "function choose input Boolean c; input Real u[3]; output Real y[3]; end choose; parameter Real p[3]; Real z[3];",
    );
    let at = source.at(0, 108);
    let model = dae::Dae::construct(source.map, |model| {
        let (boolean, vector) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let identity_signature =
            dae::FunctionSignature::new(VarName::new("identity"), [vector], [vector], at);
        let (identity, ()) = model.function(identity_signature, |model, reservation| {
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let input =
                model.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            let (failed, message) = model.expressions(|expressions| {
                Ok((
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::Boolean(false))?,
                    expressions.at(at).literal(dae::DaeLiteral::String(
                        "selected tensor branch executed".to_owned(),
                    ))?,
                ))
            })?;
            model.functions(|functions| functions.assertion(&mut body, failed, message, at))?;
            model.functions(|functions| functions.assign(&mut body, output, input, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let choose_signature =
            dae::FunctionSignature::new(VarName::new("choose"), [boolean, vector], [vector], at);
        let (choose, ()) = model.function(choose_signature, |model, reservation| {
            let condition = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("c"), 0, at)
            })?;
            let input = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 1, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let (condition, input) = model.expressions(|expressions| {
                Ok((
                    expressions.at(at).function_parameter(condition)?,
                    expressions.at(at).function_parameter(input)?,
                ))
            })?;
            let selected = model.expressions(|expressions| {
                let call = expressions.at(at).call(identity, 0, [input])?;
                expressions.at(at).conditional([(condition, call)], input)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, selected, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("z"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, time, one)?;
            let parameter = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            let call = expressions.at(at).call(choose, 0, [condition, parameter])?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(at, residual))
    })
    .unwrap();

    let package = lower_simulation_package(&model);
    let solve = &package.problem;
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one tensor residual block expected");
    };
    let site = rows.programs()[0]
        .iter()
        .find_map(|operation| match operation {
            LinearOp::PureCall { site, .. } => Some(site),
            _ => None,
        })
        .expect("aggregate call remains one checked owner");
    let owner = package.pure_calls.owner(site.owner()).unwrap();
    let (destinations, if_true, if_false) = owner
        .body()
        .operations()
        .iter()
        .find_map(|operation| match operation.operation() {
            rumoca_ir_solve::SolveOperation::Conditional {
                destinations,
                if_true,
                if_false,
                ..
            } => Some((destinations, if_true, if_false)),
            _ => None,
        })
        .expect("typed owner retains one checked lazy conditional");

    assert_eq!(
        destinations.len(),
        2,
        "the tensor result and branch-local assertion predicate form one correlated tuple"
    );
    assert_eq!(
        owner.body().register_types()[destinations[0].index()].dimensions(),
        &[3]
    );
    assert!(
        if_true
            .body()
            .operations()
            .iter()
            .chain(if_false.body().operations())
            .all(|operation| !matches!(
                operation.operation(),
                rumoca_ir_solve::SolveOperation::ProjectElement { .. }
            ))
    );
    assert_eq!(
        [if_true, if_false]
            .iter()
            .filter(|region| region.body().operations().iter().any(|operation| {
                let rumoca_ir_solve::SolveOperation::Store { slot, .. } = operation.operation()
                else {
                    return false;
                };
                region.body().slots()[slot.index()]
                    .value_type()
                    .dimensions()
                    == [3]
            }))
            .count(),
        2,
        "each lazy branch retains one checked tensor projection until its final output ABI"
    );
    assert_eq!(
        eval_residual_rows_with_pure_calls(rows, &package.pure_calls, &[0.0; 3], &[1.0, 2.0, 3.0]),
        [-1.0, -2.0, -3.0]
    );
    assert_eq!(solve.events.root_conditions.len(), 1);
    let mut root = [0.0];
    let parameters = vec![0.0; solve.layout.p_scalars()];
    let observations = rumoca_eval_solve::CheckedEventObservationContext::construct(
        &package.pure_calls,
        &solve.events.root_conditions,
        &solve.events.actions,
    )
    .unwrap();
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0; 3],
        &parameters,
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            event_observations: Some(&observations),
            ..Default::default()
        },
        &mut root,
    )
    .unwrap();
    assert_eq!(
        root,
        [-1.0],
        "an assertion hoisted from an inactive tensor arm retains that arm's guard"
    );
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0; 3],
        &parameters,
        2.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            event_observations: Some(&observations),
            ..Default::default()
        },
        &mut root,
    )
    .unwrap();
    assert_eq!(
        root,
        [1.0],
        "the selected tensor arm still owns its assertion"
    );
}

#[test]
fn call_scoped_assertion_constructs_guarded_root_and_action_rows() {
    let package = function_assertion_solve(true, -1.0, false, false);
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
    let mut root = [0.0];
    let observations = rumoca_eval_solve::CheckedEventObservationContext::construct(
        &package.pure_calls,
        &solve.events.root_conditions,
        &solve.events.actions,
    )
    .unwrap();
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            event_observations: Some(&observations),
            ..Default::default()
        },
        &mut root,
    )
    .unwrap();
    assert_eq!(root, [1.0], "an active failing assertion is above zero");
    let request = rumoca_eval_solve::eval_event_action_request(
        &solve.events,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            ..Default::default()
        },
    )
    .unwrap();
    assert!(matches!(
        request,
        rumoca_eval_solve::EventActionRequest::AssertionFailed { ref message }
            if message == "positive input required"
    ));
}

#[test]
fn one_typed_call_owns_all_assertion_root_and_action_outputs() {
    let package = function_two_assertions_solve();
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.programs().len(), 1);
    assert_eq!(solve.events.root_conditions.output_indices().len(), 2);
    assert_eq!(solve.events.action_conditions.programs().len(), 1);
    assert_eq!(solve.events.action_conditions.output_indices().len(), 2);
    assert_eq!(solve.events.actions.len(), 2);
    assert_eq!(
        solve.events.root_conditions.programs()[0]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::PureCallObservation { .. }))
            .count(),
        1
    );
    assert_eq!(
        solve.events.action_conditions.programs()[0]
            .iter()
            .filter(|operation| matches!(operation, LinearOp::PureCallObservation { .. }))
            .count(),
        1
    );
    let mut roots = [0.0; 2];
    let observations = rumoca_eval_solve::CheckedEventObservationContext::construct(
        &package.pure_calls,
        &solve.events.root_conditions,
        &solve.events.actions,
    )
    .unwrap();
    rumoca_eval_solve::eval_scalar_program_block_with_context(
        &solve.events.root_conditions,
        &[0.0],
        &vec![0.0; solve.layout.p_scalars()],
        0.0,
        rumoca_eval_solve::RowEvalContext {
            pure_calls: Some(&package.pure_calls),
            event_observations: Some(&observations),
            ..Default::default()
        },
        &mut roots,
    )
    .unwrap();
    assert_eq!(roots, [1.0, -1.0]);
}

#[test]
fn inactive_conditional_call_cannot_fire_its_function_assertion() {
    let package = function_assertion_solve(false, -1.0, false, false);
    let solve = &package.problem;
    assert!(
        solve.events.root_conditions.is_empty(),
        "a statically unreachable call must not construct an event root"
    );
    assert!(
        solve.events.actions.is_empty(),
        "a statically unreachable call must not construct an action"
    );
}

fn function_two_assertions_solve() -> crate::LoweredSolvePackage {
    let source = TestSource::new(
        "function f input Real u; output Real y; assert(u > 0, \"positive\"); assert(u < 10, \"bounded\"); y := u; Real z; z = f(-1);",
    );
    let function_at = source.at(0, 108);
    let first_at = source.at(42, 68);
    let second_at = source.at(69, 96);
    let variable_at = source.at(109, 116);
    let equation_at = source.at(117, 120);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let function = model
            .function(
                dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
                |model, reservation| {
                    let parameter = model.functions(|functions| {
                        functions.parameter(&reservation, VarName::new("u"), 0, function_at)
                    })?;
                    let output = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("y"), 0, function_at)
                    })?;
                    let parameter = model.expressions(|expressions| {
                        expressions.at(function_at).function_parameter(parameter)
                    })?;
                    let (zero, ten, first_message, second_message) =
                        model.expressions(|expressions| {
                            Ok((
                                expressions
                                    .at(first_at)
                                    .literal(dae::DaeLiteral::Real(0.0))?,
                                expressions
                                    .at(second_at)
                                    .literal(dae::DaeLiteral::Real(10.0))?,
                                expressions
                                    .at(first_at)
                                    .literal(dae::DaeLiteral::String("positive".to_owned()))?,
                                expressions
                                    .at(second_at)
                                    .literal(dae::DaeLiteral::String("bounded".to_owned()))?,
                            ))
                        })?;
                    let (positive, bounded) = model.expressions(|expressions| {
                        Ok((
                            expressions.at(first_at).binary(
                                dae::BinaryOperator::Greater,
                                parameter,
                                zero,
                            )?,
                            expressions.at(second_at).binary(
                                dae::BinaryOperator::Less,
                                parameter,
                                ten,
                            )?,
                        ))
                    })?;
                    let mut body =
                        model.functions(|functions| functions.begin(reservation, function_at))?;
                    model.functions(|functions| {
                        functions.assertion(&mut body, positive, first_message, first_at)?;
                        functions.assertion(&mut body, bounded, second_message, second_at)?;
                        functions.assign(&mut body, output, parameter, function_at)?;
                        functions.define(body, function_at)
                    })
                },
            )?
            .0;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let argument = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Time)?;
            let call = expressions.at(equation_at).call(function, 0, [argument])?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap();
    lower_simulation_package(&model)
}

#[test]
fn shared_call_in_branch_condition_and_value_has_one_assertion_schedule() {
    let package = function_assertion_solve(true, 1.0, true, false);
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
}

#[test]
fn nested_call_assertion_resolves_actual_argument_in_the_caller_frame() {
    let package = function_assertion_solve(true, 1.0, false, true);
    let solve = &package.problem;
    assert_eq!(solve.events.root_conditions.len(), 1);
    assert_eq!(solve.events.actions.len(), 1);
}

fn construct_asserting_identity<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    function_at: dae::DaeProvenance,
    assertion_at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at);
    model
        .function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, function_at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, function_at)
            })?;
            let parameter_value = model.expressions(|expressions| {
                expressions.at(function_at).function_parameter(parameter)
            })?;
            let assertion = model.expressions(|expressions| {
                let zero = expressions
                    .at(assertion_at)
                    .literal(dae::DaeLiteral::Real(0.0))?;
                expressions.at(assertion_at).binary(
                    dae::BinaryOperator::Greater,
                    parameter_value,
                    zero,
                )
            })?;
            let message = model.expressions(|expressions| {
                expressions
                    .at(assertion_at)
                    .literal(dae::DaeLiteral::String(
                        "positive input required".to_owned(),
                    ))
            })?;
            let mut body =
                model.functions(|functions| functions.begin(reservation, function_at))?;
            model.functions(|functions| {
                functions.assertion(&mut body, assertion, message, assertion_at)
            })?;
            model.functions(|functions| {
                functions.assign(&mut body, output, parameter_value, function_at)
            })?;
            model.functions(|functions| functions.define(body, function_at))
        })
        .map(|(function, ())| function)
}

fn construct_call_wrapper<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    called: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let signature = dae::FunctionSignature::new(VarName::new("g"), [real], [real], at);
    model
        .function(signature, |model, reservation| {
            let parameter = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("v"), 0, at)
            })?;
            let output = model
                .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, at))?;
            let parameter_value = model
                .expressions(|expressions| expressions.at(at).function_parameter(parameter))?;
            let call = model
                .expressions(|expressions| expressions.at(at).call(called, 0, [parameter_value]))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, call, at))?;
            model.functions(|functions| functions.define(body, at))
        })
        .map(|(function, ())| function)
}

fn function_assertion_solve(
    branch_active: bool,
    call_value: f64,
    call_controls_branch: bool,
    nested_call: bool,
) -> crate::LoweredSolvePackage {
    let source = TestSource::new(
        "function f input Real u; output Real y; assert(u > 0, \"positive input required\"); y := u; Real z; z = if active then f(value) else 0;",
    );
    let function_at = source.at(0, 94);
    let assertion_at = source.at(44, 84);
    let variable_at = source.at(95, 102);
    let equation_at = source.at(104, 133);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let function = construct_asserting_identity(model, real, function_at, assertion_at)?;
        let called_function = if nested_call {
            construct_call_wrapper(model, real, function, function_at)?
        } else {
            function
        };
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let active = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Boolean(branch_active))?;
            let argument = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(call_value))?;
            let call = expressions
                .at(equation_at)
                .call(called_function, 0, [argument])?;
            let fallback = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            let condition = if call_controls_branch {
                expressions
                    .at(equation_at)
                    .binary(dae::BinaryOperator::Greater, call, fallback)?
            } else {
                active
            };
            let selected = expressions
                .at(equation_at)
                .conditional([(condition, call)], fallback)?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, selected)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap();
    lower_simulation_package(&model)
}

/// The ordinary calls the continuous rows of `package` make.
fn ordinary_call_count(package: &crate::LoweredSolvePackage) -> usize {
    package
        .problem
        .continuous
        .implicit_rhs
        .nodes
        .iter()
        .filter_map(|node| match node {
            rumoca_ir_solve::ComputeNode::ScalarPrograms(block) => Some(block.programs()),
            _ => None,
        })
        .flatten()
        .flatten()
        .filter(|operation| matches!(operation, LinearOp::PureCall { .. }))
        .count()
}

#[test]
fn scalar_refresh_keeps_a_callee_holding_an_assertion_behind_its_ordinary_call() {
    let package = function_assertion_solve(true, 1.0, false, false);
    assert_eq!(
        ordinary_call_count(&package),
        1,
        "the scalar refresh calls the asserting function instead of expanding its body"
    );
}

/// `g(R(3, 0.5))` where `R` holds an Integer and a Real: the call passes the
/// literal record's Integer field as its own typed leaf.
#[test]
fn record_argument_passes_its_integer_field_as_one_typed_leaf() {
    const TEXT: &str = "function g input R r; output Real y; y := r.x; Real z; z = g(R(3, 0.5));";
    let source = TestSource::new(TEXT);
    let span_of = |needle: &str| {
        let start = TEXT.find(needle).unwrap();
        source.at(start, start + needle.len())
    };
    let function_at = span_of("function g");
    let variable_at = span_of("Real z;");
    let equation_at = span_of("z = g(R(3, 0.5));");
    let model = dae::Dae::construct(source.map, |model| {
        let (integer, real) = model.types(|types| {
            Ok((
                types.derived(
                    dae::ValueType::scalar(dae::ScalarType::Integer),
                    function_at,
                )?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)?,
            ))
        })?;
        let record = model.types(|types| {
            types.record(
                VarName::new("R"),
                [(VarName::new("n"), integer), (VarName::new("x"), real)],
                function_at,
            )
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("g"), [record], [real], function_at),
            |model, reservation| {
                let (parameter, output) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("r"), 0, function_at)?,
                        functions.output(&reservation, VarName::new("y"), 0, function_at)?,
                    ))
                })?;
                let value = model.expressions(|expressions| {
                    let parameter = expressions.at(function_at).function_parameter(parameter)?;
                    expressions.at(function_at).field(parameter, 1)
                })?;
                let mut body =
                    model.functions(|functions| functions.begin(reservation, function_at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, output, value, function_at)
                })?;
                model.functions(|functions| functions.define(body, function_at))
            },
        )?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 100).unwrap();
        let clock = model.clocks(|clocks| clocks.periodic(lattice, equation_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), variable, equation_at))?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let three = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Integer(3))?;
            let half = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.5))?;
            let argument = expressions.at(equation_at).record(record, [three, half])?;
            let call = expressions.at(equation_at).call(function, 0, [argument])?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(equation_at, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let package = lower_solve_package(&model).unwrap();
    let [owner] = package.pure_calls.owners() else {
        panic!("one exact demanded pure-call owner expected");
    };
    assert_eq!(owner.inputs().len(), 2, "one typed leaf per record field");
    assert!(matches!(
        owner.inputs()[0].element_type(),
        rumoca_ir_solve::SolveScalarType::Integer(_)
    ));
}

/// `f(u)` adds `u` up to three times, ending the loop once the sum reaches 2.
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction fixture enumerates the complete checked loop owner"
)]
fn bounded_while_sum_model() -> dae::Dae {
    const TEXT: &str = "function f input Real u; output Real y; y := 0; for i in 1:3 while y < 2 loop y := y + u; Real z; der(z) = f(z);";
    let source = TestSource::new(TEXT);
    let span_of = |needle: &str| {
        let start = TEXT.find(needle).unwrap();
        source.at(start, start + needle.len())
    };
    let function_at = span_of("function f");
    let variable_at = span_of("Real z;");
    let equation_at = span_of("der(z) = f(z);");
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
            |model, reservation| {
                let (parameter, output) = model.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, function_at)?,
                        functions.output(&reservation, VarName::new("y"), 0, function_at)?,
                    ))
                })?;
                let (u, zero, two) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(function_at).function_parameter(parameter)?,
                        expressions
                            .at(function_at)
                            .literal(dae::DaeLiteral::Real(0.0))?,
                        expressions
                            .at(function_at)
                            .literal(dae::DaeLiteral::Real(2.0))?,
                    ))
                })?;
                let mut body =
                    model.functions(|functions| functions.begin(reservation, function_at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, output, zero, function_at)
                })?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                provenance: rumoca_core::IndexBinderProvenance::Generated,
                                id: 0,
                                display_name: "i".to_owned(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        function_at,
                    )
                })?;
                let mut loop_body = model.functions(|functions| {
                    functions.begin_loop(body, domain, [output], function_at)
                })?;
                let current = model
                    .functions(|functions| functions.read(loop_body.body(), output, function_at))?;
                let (unfinished, update) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(function_at).binary(
                            dae::BinaryOperator::Less,
                            current,
                            two,
                        )?,
                        expressions
                            .at(function_at)
                            .binary(dae::BinaryOperator::Add, current, u)?,
                    ))
                })?;
                model.functions(|functions| {
                    functions.continue_loop_while(&mut loop_body, unfinished, function_at)?;
                    functions.assign_loop(&mut loop_body, output, update, function_at)
                })?;
                let body =
                    model.functions(|functions| functions.finish_loop(loop_body, function_at))?;
                model.functions(|functions| functions.define(body, function_at))
            },
        )?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let time = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let call = expressions.at(equation_at).call(function, 0, [time])?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap()
}

#[test]
fn a_bounded_while_loop_expanded_into_a_derivative_row_keeps_its_continuation() {
    let solve = lower_solve_problem(&bounded_while_sum_model()).unwrap();
    let continued = solve.continuous.derivative_rhs.nodes.iter().any(|node| match node {
        ComputeNode::ScalarPrograms(rows) => rows.programs().iter().flatten().any(|operation| {
            matches!(
                operation,
                LinearOp::FunctionFold { program, .. } | LinearOp::GuardedFunctionFold { program, .. }
                    if program.continuation.is_some()
            )
        }),
        _ => false,
    });
    assert!(
        continued,
        "the expanded fold ends at its first false predicate"
    );
}

/// `f(u)` reads a local only the `u > 0` arm assigns, through the ordered
/// assigned-state check, and holds no assertion.
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction fixture enumerates the complete checked function owner"
)]
fn partial_local_call_model() -> dae::Dae {
    const TEXT: &str = "function f input Real u; output Real y; protected Real spare; if u > 0 then y := u; spare := 1; else y := 0; end if; y := y + spare; Real z; z = f(time);";
    let source = TestSource::new(TEXT);
    let span_of = |needle: &str| {
        let start = TEXT.find(needle).unwrap();
        source.at(start, start + needle.len())
    };
    let function_at = span_of("function f");
    let branch_at = span_of("if u > 0 then");
    let variable_at = span_of("Real z;");
    let equation_at = span_of("z = f(time);");
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let function = model
            .function(
                dae::FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
                |model, reservation| {
                    let (parameter, output, spare) = model.functions(|functions| {
                        Ok((
                            functions.parameter(&reservation, VarName::new("u"), 0, function_at)?,
                            functions.output(&reservation, VarName::new("y"), 0, function_at)?,
                            functions.local(
                                &reservation,
                                VarName::new("spare"),
                                real,
                                function_at,
                            )?,
                        ))
                    })?;
                    let (u, guard, zero, one) = model.expressions(|expressions| {
                        let u = expressions.at(function_at).function_parameter(parameter)?;
                        let zero = expressions
                            .at(function_at)
                            .literal(dae::DaeLiteral::Real(0.0))?;
                        let one = expressions
                            .at(function_at)
                            .literal(dae::DaeLiteral::Real(1.0))?;
                        let guard = expressions.at(branch_at).binary(
                            dae::BinaryOperator::Greater,
                            u,
                            zero,
                        )?;
                        Ok((u, guard, zero, one))
                    })?;
                    let parent =
                        model.functions(|functions| functions.begin(reservation, function_at))?;
                    let (mut child, finish) =
                        model.functions(|functions| functions.begin_branch(parent, branch_at))?;
                    model.functions(|functions| {
                        functions.assign_all(&mut child, &[(output, u), (spare, one)], branch_at)
                    })?;
                    let (parent, selected) =
                        model.functions(|functions| functions.finish_branch(finish, child))?;
                    let (mut child, finish) =
                        model.functions(|functions| functions.begin_branch(parent, branch_at))?;
                    model.functions(|functions| {
                        functions.assign(&mut child, output, zero, branch_at)
                    })?;
                    let (mut body, fallback) =
                        model.functions(|functions| functions.finish_branch(finish, child))?;
                    model.functions(|functions| {
                        functions.finish_conditional(
                            &mut body,
                            vec![(guard, selected)],
                            fallback,
                            branch_at,
                        )
                    })?;
                    model.functions(|functions| {
                        functions
                            .require_assigned(&mut body, spare, function_at)
                            .map(|_| ())
                    })?;
                    let (current, held) = model.functions(|functions| {
                        Ok((
                            functions.read(&body, output, function_at)?,
                            functions.read(&body, spare, function_at)?,
                        ))
                    })?;
                    let sum = model.expressions(|expressions| {
                        expressions
                            .at(function_at)
                            .binary(dae::BinaryOperator::Add, current, held)
                    })?;
                    model.functions(|functions| {
                        functions.assign(&mut body, output, sum, function_at)?;
                        functions.define(body, function_at)
                    })
                },
            )?
            .0;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("z"),
                real,
                variable_at,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let time = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Time)?;
            let call = expressions.at(equation_at).call(function, 0, [time])?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .unwrap()
}

#[test]
fn a_partially_assigned_local_checked_in_a_callee_lowers() {
    let result = lower_solve_package(&partial_local_call_model());
    assert!(result.is_ok(), "{:?}", result.err());
}
