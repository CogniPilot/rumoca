//! The initialization system: its runtime flags, continuation parameter, and unknowns.
//!
//! `initial()` and `homotopy(..)` each own one dedicated runtime parameter, and a
//! `parameter` declared `fixed = false` enters the initialization projection as an
//! unknown rather than staying a checked residual.

use super::*;

#[test]
fn initial_condition_owns_a_dedicated_runtime_flag() {
    let source = TestSource::new(
        "discrete Real x; when initial() then x = 1; elsewhen false then x = 2; end when;",
    );
    let declaration = source.at(0, 15);
    let initial_at = source.at(22, 31);
    let assignment = source.at(37, 42);
    let false_at = source.at(53, 58);
    let second_assignment = source.at(64, 69);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("x"),
                rumoca_core::InstanceId::new(1),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let condition = model.conditions(|conditions| conditions.reserve(initial_at))?;
        model.conditions(|conditions| {
            conditions.define(condition, dae::ConditionInput::Initial, initial_at)
        })?;
        let false_value = model.expressions(|expressions| {
            expressions
                .at(false_at)
                .literal(dae::DaeLiteral::Boolean(false))
        })?;
        let otherwise = model.conditions(|conditions| conditions.reserve(false_at))?;
        model.conditions(|conditions| {
            conditions.define(
                otherwise,
                dae::ConditionInput::Discrete(false_value),
                false_at,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let value = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let target = expressions
                .at(assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(assignment)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        let second_residual = model.expressions(|expressions| {
            let value = expressions
                .at(second_assignment)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let target = expressions
                .at(second_assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            expressions
                .at(second_assignment)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(condition, condition, assignment, |equation| {
                equation.residual(residual)
            })?;
            discrete.when_real_equation(otherwise, otherwise, second_assignment, |equation| {
                equation.residual(second_residual)
            })?;
            Ok(())
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_grouped_initial_b1b(&solve);
}

#[test]
fn scalar_initial_coordinate_reads_the_existing_runtime_flag() {
    let source = TestSource::new("Real x; x = if initial() then 1 else 2;");
    let declaration = source.at(0, 6);
    let initial_at = source.at(15, 24);
    let owner = source.at(8, 39);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let algebraic = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                rumoca_core::InstanceId::new(2),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let condition = model.conditions(|conditions| conditions.reserve(initial_at))?;
        model.conditions(|conditions| {
            conditions.define(condition, dae::ConditionInput::Initial, initial_at)
        })?;
        let residual = model.expressions(|expressions| {
            let guard = expressions
                .at(initial_at)
                .coordinate(dae::CoordinateInput::Condition(condition))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let selected = expressions.at(owner).conditional([(guard, one)], two)?;
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, selected)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let flag = solve
        .solve_layout()
        .initial_event_parameter_index
        .expect("the initial condition reserves its established runtime flag");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().residual().nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    let program = &rows.programs()[0];
    assert!(
        program
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == flag))
    );
    assert!(
        program
            .iter()
            .any(|operation| matches!(operation, LinearOp::Select { .. }))
    );
}

fn assert_grouped_initial_b1b(solve: &rumoca_ir_solve::SolveProblem) {
    let flag = solve
        .solve_layout()
        .initial_event_parameter_index
        .expect("initial() owns one checked runtime flag");
    let guarded = solve
        .discrete()
        .guarded_assignments
        .iter()
        .filter(|program| program.role() == rumoca_ir_solve::DiscreteRowRole::Equation)
        .collect::<Vec<_>>();
    let [guarded] = guarded.as_slice() else {
        panic!("one conditional B.1b equation owner expected");
    };
    let conditional = guarded
        .program()
        .iter()
        .find_map(|operation| match operation {
            LinearOp::FunctionConditional { program, .. } => Some(program),
            _ => None,
        })
        .expect("the initial B.1b branches retain one compact owner");
    assert_eq!(conditional.arms().len(), 2);
    assert!(
        guarded
            .program()
            .iter()
            .all(|operation| !matches!(operation, LinearOp::Select { .. })),
        "the guarded assignment is not represented by scalar selections"
    );
    assert!(
        conditional
            .arms()
            .iter()
            .flat_map(|arm| arm.condition().iter())
            .any(|operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == flag))
    );
}

#[test]
fn homotopy_owns_a_dedicated_continuation_parameter() {
    let source = TestSource::new("Real x; der(x) = homotopy(x*x, x);");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 34);
    let homotopy_at = source.at(17, 33);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(0.0))
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(3),
                real,
                declaration,
                dae::VariableAttributes {
                    component_ref: None,
                    binding: None,
                    start: Some(start),
                    fixed: Some(rumoca_core::Fixity::Fixed),
                    min: None,
                    max: None,
                    nominal: None,
                    unit: None,
                    state_select: rumoca_core::StateSelect::Default,
                    description: None,
                    causality: dae::VariableCausality::Local,
                    is_tunable: false,
                    is_held: false,
                    origin: dae::VariableOrigin::Source,
                },
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let x = expressions
                .at(homotopy_at)
                .coordinate(dae::CoordinateInput::State(state))?;
            let actual = expressions
                .at(homotopy_at)
                .binary(dae::BinaryOperator::Multiply, x, x)?;
            let homotopy = expressions
                .at(homotopy_at)
                .builtin(dae::PureBuiltin::Homotopy, [actual, x])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, homotopy)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    model.inspect(|view| {
        let expression = (0..view.expression_count())
            .filter_map(|index| view.expression(view.expression_id(index)?))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Builtin {
                        builtin: dae::PureBuiltin::Homotopy,
                        ..
                    }
                )
            })
            .expect("checked DAE retains the homotopy node");
        assert_eq!(
            view.source_text(expression.provenance()),
            Some("homotopy(x*x, x)")
        );
    });
    let solve = lower_solve_problem(&model).unwrap();
    let lambda = solve
        .solve_layout()
        .initial_homotopy_parameter_index
        .expect("homotopy owns one checked continuation parameter");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous().derivative_rhs().nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert!(
        rows.programs()[0].iter().any(
            |operation| matches!(operation, LinearOp::LoadP { index, .. } if *index == lambda)
        )
    );
}

/// MLS §8.6: a `parameter` declared `fixed = false` is determined by the initialization
/// system, so it must appear there as an unknown with its own projection block. Without
/// one the residual can only be checked, which is what stalled
/// `Modelica.Electrical.Analog.Examples.ShowSaturatingInductor` on
/// `SaturatingInductor.Ipar`.
#[test]
fn fixed_false_parameter_becomes_an_initialization_projection_unknown() {
    let source = TestSource::new("parameter Real q(fixed=false); initial equation q*q=4;");
    let declaration = source.at(0, 29);
    let owner = source.at(48, 53);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(3.0))
        })?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("q"),
                rumoca_core::InstanceId::new(4),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(rumoca_core::Fixity::Free),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let residual = model.expressions(|expressions| {
            let left = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let right = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let square =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, left, right)?;
            let four = expressions.at(owner).literal(dae::DaeLiteral::Real(4.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, square, four)
        })?;
        model.initialization(|initialization| initialization.value_equation(owner, residual))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    reseal_solve_problem(&solve)
        .expect("the initialization projection satisfies the Solve shape contract");
    let ScalarSlot::P { index, .. } = solve
        .layout()
        .binding("q")
        .expect("the unsolved parameter keeps its parameter storage")
    else {
        panic!("a parameter occupies P storage");
    };
    let [block] = solve.initialization().projection_plan().blocks.as_slice() else {
        panic!(
            "one initialization projection block expected, got {:?}",
            solve.initialization().projection_plan().blocks
        );
    };
    assert_eq!(block.rows, [0]);
    assert_eq!(block.unknowns, [rumoca_ir_solve::scalar_slot_p(index)]);
    assert_eq!(
        solve.initialization().projection_unknowns(),
        [rumoca_ir_solve::scalar_slot_p(index)]
    );
}

/// A fixed algebraic's §8.6 equation refuses Solve construction until its
/// exact transitive incidence through the continuous system is computed.
///
/// This is the graph that proved assumed-universal incidence wrong: the
/// `fixed = true` row of `a` is unrelated to `q`, yet under universal
/// incidence the matching could pair it with `q`, the refresh zeroed the row,
/// and `q = 100` was silently retained with no equation determining it. The
/// declaration must refuse instead, at its own span, before any matching runs.
#[test]
fn a_fixed_algebraic_initial_equation_is_refused_until_incidence_is_exact() {
    let source = TestSource::new(
        "parameter Real q(start=100, fixed=false); Real a(start=0, fixed=true); equation a=0;",
    );
    let q_at = source.at(0, 40);
    let a_at = source.at(42, 69);
    let equation_at = source.at(80, 83);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                q_at,
            )
        })?;
        let (q_start, a_start) = model.expressions(|expressions| {
            Ok((
                expressions.at(q_at).literal(dae::DaeLiteral::Real(100.0))?,
                expressions.at(a_at).literal(dae::DaeLiteral::Real(0.0))?,
            ))
        })?;
        let (_q, a, a_reservation) = model.variables(|variables| {
            let q = variables.parameter(
                VarName::new("q"),
                rumoca_core::InstanceId::new(5),
                real,
                q_at,
                dae::VariableAttributes {
                    start: Some(q_start),
                    fixed: Some(rumoca_core::Fixity::Free),
                    ..dae::VariableAttributes::default()
                },
            )?;
            let (a, reservation) = variables.reserve_algebraic(
                VarName::new("a"),
                rumoca_core::InstanceId::new(6),
                real,
                a_at,
            )?;
            Ok((q, a, reservation))
        })?;
        model.variables(|variables| {
            variables.define(
                a_reservation,
                dae::VariableAttributes {
                    start: Some(a_start),
                    fixed: Some(rumoca_core::Fixity::Fixed),
                    ..dae::VariableAttributes::default()
                },
                a_at,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let a = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Algebraic(a))?;
            let zero = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, a, zero)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .expect("fixed algebraic fixture DAE is valid");

    let error = lower_solve_problem(&model)
        .expect_err("a fixed algebraic row must not be admitted on assumed incidence");
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, span }
                if reason.contains("transitive incidence") && *span == a_at.span()
        ),
        "the refusal names the missing incidence certificate at `a`'s declaration: {error:?}"
    );
}

/// MLS 3.6 §8.6: "All variables declared as parameter having `fixed = false` are
/// treated as unknowns during the initialization phase, i.e., there must be
/// additional equations for them — and the start-value can be used as a
/// guess-value during initialization."
///
/// The parameter set therefore holds only the guess for `q`, and the binding
/// `g = 2*q` it evaluated from that guess is a seed, not a value. The binding
/// becomes an initialization update row writing `g`'s own parameter slot, which
/// `settle_initialization_system` re-applies after the projection solves `q` —
/// without it a plausible wrong number reaches the whole trajectory.
#[test]
fn a_parameter_reading_an_initialization_unknown_is_re_applied_after_the_solve() {
    let model = dependent_parameter_model();
    let solve = lower_solve_problem(&model).unwrap();
    reseal_solve_problem(&solve)
        .expect("the dependent parameter update satisfies the Solve shape contract");
    let dependent_slot = solve
        .layout()
        .binding("g")
        .expect("the dependent parameter keeps its parameter storage");
    assert_eq!(
        solve.initialization().update_targets(),
        [dependent_slot],
        "the dependent binding is the only initialization update row"
    );
    let unsolved_slot = solve
        .layout()
        .binding("q")
        .expect("the unsolved parameter keeps its parameter storage");
    let ScalarSlot::P {
        index: unsolved_index,
        ..
    } = unsolved_slot
    else {
        panic!("a parameter occupies P storage");
    };
    assert!(
        solve.initialization().update_rhs().programs()[0]
            .iter()
            .any(|operation| matches!(
                operation,
                LinearOp::LoadP { index, .. } if *index == unsolved_index
            )),
        "the re-applied row reads the slot the projection solves"
    );
    assert!(
        !solve
            .initialization()
            .projection_unknowns()
            .contains(&dependent_slot),
        "the dependent parameter is assigned by its binding, not solved as an unknown"
    );
}

/// `parameter Real q(fixed=false); parameter Real g=2*q; initial equation q*q=4;`
fn dependent_parameter_model() -> dae::Dae {
    let source = TestSource::new(
        "parameter Real q(fixed=false); parameter Real g=2*q; initial equation q*q=4;",
    );
    let declaration = source.at(0, 29);
    let dependent = source.at(31, 50);
    let owner = source.at(69, 74);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .literal(dae::DaeLiteral::Real(3.0))
        })?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("q"),
                rumoca_core::InstanceId::new(7),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(start),
                    fixed: Some(rumoca_core::Fixity::Free),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let binding = model.expressions(|expressions| {
            let two = expressions
                .at(dependent)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let read = expressions
                .at(dependent)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            expressions
                .at(dependent)
                .binary(dae::BinaryOperator::Multiply, two, read)
        })?;
        model.variables(|variables| {
            variables.parameter(
                VarName::new("g"),
                rumoca_core::InstanceId::new(8),
                real,
                dependent,
                dae::VariableAttributes {
                    binding: Some(binding),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let residual = model.expressions(|expressions| {
            let left = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let right = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let square =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, left, right)?;
            let four = expressions.at(owner).literal(dae::DaeLiteral::Real(4.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, square, four)
        })?;
        model.initialization(|initialization| initialization.value_equation(owner, residual))?;
        Ok(())
    })
    .unwrap()
}

/// MLS 3.6 §8.6 already adds `x = startExpression` for a `fixed = true` start,
/// so an explicit `initial equation` whose every coordinate is already
/// determined is a second equation for one value: the system is overdetermined
/// and must refuse Solve construction at the row's own provenance, not surface
/// later as a runtime residual failure.
#[test]
fn an_initial_equation_over_a_fixed_start_state_is_refused_as_overdetermined() {
    let source = TestSource::new(
        "constant Boolean k = true; Real x(start=2, fixed=k); initial equation x = 3; \
         equation der(x) = 1;",
    );
    let declaration = source.at(27, 51);
    let owner = source.at(70, 75);
    let equation_at = source.at(86, 96);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let attributes = real_state_attributes(model, declaration, 2.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(9),
                real,
                declaration,
                attributes,
            )
        })?;
        let initial_residual = model.expressions(|expressions| {
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let three = expressions.at(owner).literal(dae::DaeLiteral::Real(3.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, x, three)
        })?;
        model.initialization(|initialization| {
            initialization.value_equation(owner, initial_residual)
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let one = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, derivative, one)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .expect("the overdetermination is a Solve refusal, not a DAE construction failure");

    let error = lower_solve_problem(&model)
        .expect_err("a fixed start and an initial equation are two owners for one coordinate");
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, span }
                if reason.contains("overdetermined") && *span == owner.span()
        ),
        "the refusal names MLS §8.6 overdetermination at the initial equation's own span: \
         {error:?}"
    );
}

/// MLS 3.6 §8.6: a `fixed = false` parameter is an unknown of the
/// initialization problem, so a row may read a stated `fixed = true` state as a
/// determined number and still own that parameter. The stated read contributes
/// no unknown; it must not disqualify the row.
#[test]
fn a_row_reading_a_stated_state_may_still_determine_a_fixed_false_parameter() {
    let model = stated_state_parameter_model();
    let solve = lower_solve_problem(&model)
        .expect("a row reading a stated state still owns the fixed = false parameter");
    reseal_solve_problem(&solve).expect("lowered Solve problem is valid");
    let p_slot = solve
        .layout()
        .binding("p")
        .expect("the unsolved parameter keeps its parameter storage");
    let ScalarSlot::P { .. } = p_slot else {
        panic!("a parameter occupies P storage");
    };
    assert_eq!(solve.initialization().projection_unknowns(), [p_slot]);
    let [block] = solve.initialization().projection_plan().blocks.as_slice() else {
        panic!(
            "one initialization projection block expected, got {:?}",
            solve.initialization().projection_plan().blocks
        );
    };
    assert_eq!(block.rows, [0]);
    assert_eq!(block.unknowns, [p_slot]);
    assert_eq!(
        solve.initialization().row_roles(),
        [rumoca_ir_solve::InitializationRowRole::Solved],
        "the row is matched to the parameter, not left as an unowned check"
    );
    assert_eq!(solve.initialization().row_targets(), [Some(p_slot)]);
}

fn stated_state_parameter_model() -> dae::Dae {
    let source = TestSource::new(
        "parameter Real p(start=0, fixed=false); Real x(start=2, fixed=true); \
         initial equation p = x + 1; equation der(x) = 1;",
    );
    let parameter_at = source.at(0, 38);
    let declaration = source.at(40, 67);
    let owner = source.at(86, 95);
    let equation_at = source.at(106, 116);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let guess = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(0.0))
        })?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                rumoca_core::InstanceId::new(10),
                real,
                parameter_at,
                dae::VariableAttributes {
                    start: Some(guess),
                    fixed: Some(rumoca_core::Fixity::Free),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let attributes = real_state_attributes(model, declaration, 2.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(11),
                real,
                declaration,
                attributes,
            )
        })?;
        let initial_residual = model.expressions(|expressions| {
            let p = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            let sum = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, x, one)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, p, sum)
        })?;
        model.initialization(|initialization| {
            initialization.value_equation(owner, initial_residual)
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let one = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, derivative, one)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .expect("stated-state read fixture DAE is valid")
}

/// The valid §8.6 kernel: a `fixed = true` start is the state's initialization
/// equation, the declaration seed answers it, and no residual row or projection
/// block remains. Zero rows for zero unknowns is the correct square count.
#[test]
fn a_fixed_start_state_alone_constructs_an_empty_initialization_system() {
    let source = TestSource::new("Real x(start=2, fixed=true); equation der(x) = 1;");
    let declaration = source.at(0, 27);
    let equation_at = source.at(38, 48);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let attributes = real_state_attributes(model, declaration, 2.0, true)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(12),
                real,
                declaration,
                attributes,
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let one = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, derivative, one)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .expect("kernel fixture DAE is valid");

    let solve = lower_solve_problem(&model).expect("the kernel constructs");
    reseal_solve_problem(&solve).expect("lowered Solve problem is valid");
    let initialization = solve.initialization();
    assert!(initialization.row_roles().is_empty());
    assert!(initialization.row_targets().is_empty());
    assert!(initialization.projection_unknowns().is_empty());
    assert!(initialization.projection_plan().blocks.is_empty());
    assert!(initialization.update_targets().is_empty());
}

/// A `fixed = true` state whose `start` transitively reads a coordinate the
/// projection solves is refused at its declaration. `x(start = p)` with
/// `p(fixed = false)` and `initial equation p = 1` would otherwise seed
/// `x = 0` from `p`'s guess, solve `p = 1`, and never re-apply the state's
/// §8.6 equation, silently violating `x = p`.
#[test]
fn a_fixed_start_reading_a_projection_solved_parameter_is_refused() {
    let source = TestSource::new(
        "parameter Real p(start=0, fixed=false); Real x(start=p, fixed=true); \
         initial equation p = 1; equation der(x) = 0;",
    );
    let parameter_at = source.at(0, 38);
    let declaration = source.at(40, 67);
    let owner = source.at(86, 91);
    let equation_at = source.at(102, 112);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let guess = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(0.0))
        })?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("p"),
                rumoca_core::InstanceId::new(13),
                real,
                parameter_at,
                dae::VariableAttributes {
                    start: Some(guess),
                    fixed: Some(rumoca_core::Fixity::Free),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let dependent_start = model.expressions(|expressions| {
            expressions
                .at(declaration)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(14),
                real,
                declaration,
                dae::VariableAttributes {
                    start: Some(dependent_start),
                    fixed: Some(rumoca_core::Fixity::Fixed),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let initial_residual = model.expressions(|expressions| {
            let p = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, p, one)
        })?;
        model.initialization(|initialization| {
            initialization.value_equation(owner, initial_residual)
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let zero = expressions
                .at(equation_at)
                .literal(dae::DaeLiteral::Real(0.0))?;
            expressions
                .at(equation_at)
                .binary(dae::BinaryOperator::Subtract, derivative, zero)
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_at, residual))
    })
    .expect("dependent fixed-start fixture DAE is valid");

    let error = lower_solve_problem(&model)
        .expect_err("a fixed start over a projection-solved parameter must refuse");
    assert!(
        matches!(
            &error,
            LowerError::NonComputable { reason, span }
                if reason.contains("transitively reads") && *span == declaration.span()
        ),
        "the refusal names the dependent seed at the state's declaration: {error:?}"
    );
}

/// The matching covers every unknown and every mandatory row, drawing an
/// optional stated-value check into the plan only when nothing else can own an
/// unknown. The graph: mandatory `R1` reads `{x, q}`, mandatory `R2` reads
/// `{x}`, and the carried check `C` reads `{q}`. An unknown-driven matching
/// may take `q -> R1` and then reroute `q -> C` to free `R1` for `x`, leaving
/// mandatory `R2` unmatched and refusing a valid model; the constrained
/// matching must give `R1 -> q`, `R2 -> x`, and leave only `C` unmatched as
/// the stated-value check.
#[test]
fn mandatory_rows_are_covered_before_an_optional_check_joins_the_plan() {
    let model = constrained_matching_model();
    let solve = lower_solve_problem(&model)
        .expect("both mandatory rows are covered and the carried check stays optional");
    reseal_solve_problem(&solve).expect("lowered Solve problem is valid");
    let q_slot = solve
        .layout()
        .binding("q")
        .expect("q keeps parameter storage");
    let ScalarSlot::P { .. } = q_slot else {
        panic!("q occupies parameter storage");
    };
    let x_slot = solve.layout().binding("x").expect("x keeps solver storage");
    let ScalarSlot::Y { .. } = x_slot else {
        panic!("x occupies solver storage");
    };
    assert_eq!(
        solve.initialization().row_roles(),
        [
            rumoca_ir_solve::InitializationRowRole::Solved,
            rumoca_ir_solve::InitializationRowRole::Solved,
            rumoca_ir_solve::InitializationRowRole::StatedValueCheck,
        ],
        "R1 and R2 are solved; only the carried check stands unmatched"
    );
    assert_eq!(
        solve.initialization().row_targets(),
        [Some(q_slot), Some(x_slot), None],
        "R1 determines q, R2 determines x, and the check owns nothing"
    );
    let [block] = solve.initialization().projection_plan().blocks.as_slice() else {
        panic!(
            "one initialization projection block expected, got {:?}",
            solve.initialization().projection_plan().blocks
        );
    };
    assert_eq!(block.rows, [0, 1]);
    assert_eq!(block.unknowns, [q_slot, x_slot]);
    assert_eq!(
        solve.initialization().projection_unknowns(),
        [q_slot, x_slot]
    );
}

fn constrained_matching_model() -> dae::Dae {
    let source = TestSource::new(
        "parameter Real q(start=0, fixed=false); Real s(start=3, fixed=true); \
         Real b(start=q, fixed=true); Real x(start=0, fixed=false); \
         equation der(s) = 1; b = s; der(x) = 1; initial equation x + q = 5; x = 2;",
    );
    let q_at = source.at(0, 38);
    let s_at = source.at(40, 67);
    let b_at = source.at(69, 96);
    let x_at = source.at(98, 126);
    let der_s_at = source.at(137, 147);
    let alias_at = source.at(149, 154);
    let der_x_at = source.at(156, 166);
    let r1_at = source.at(185, 194);
    let r2_at = source.at(196, 201);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                q_at,
            )
        })?;
        let q_start = model
            .expressions(|expressions| expressions.at(q_at).literal(dae::DaeLiteral::Real(0.0)))?;
        let unsolved = model.variables(|variables| {
            variables.parameter(
                VarName::new("q"),
                rumoca_core::InstanceId::new(15),
                real,
                q_at,
                dae::VariableAttributes {
                    start: Some(q_start),
                    fixed: Some(rumoca_core::Fixity::Free),
                    ..dae::VariableAttributes::default()
                },
            )
        })?;
        let stated_attributes = real_state_attributes(model, s_at, 3.0, true)?;
        let stated = model.variables(|variables| {
            variables.state(
                VarName::new("s"),
                rumoca_core::InstanceId::new(16),
                real,
                s_at,
                stated_attributes,
            )
        })?;
        let carried_start = model.expressions(|expressions| {
            expressions
                .at(b_at)
                .coordinate(dae::CoordinateInput::Parameter(unsolved))
        })?;
        let (member, member_reservation) = model.variables(|variables| {
            variables.reserve_algebraic(
                VarName::new("b"),
                rumoca_core::InstanceId::new(17),
                real,
                b_at,
            )
        })?;
        model.variables(|variables| {
            variables.define(
                member_reservation,
                dae::VariableAttributes {
                    start: Some(carried_start),
                    fixed: Some(rumoca_core::Fixity::Fixed),
                    ..dae::VariableAttributes::default()
                },
                b_at,
            )
        })?;
        let free_attributes = real_state_attributes(model, x_at, 0.0, false)?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                rumoca_core::InstanceId::new(18),
                real,
                x_at,
                free_attributes,
            )
        })?;
        unit_derivative_equation(model, der_s_at, stated)?;
        let alias = model.expressions(|expressions| {
            let b = expressions
                .at(alias_at)
                .coordinate(dae::CoordinateInput::Algebraic(member))?;
            let s = expressions
                .at(alias_at)
                .coordinate(dae::CoordinateInput::State(stated))?;
            expressions
                .at(alias_at)
                .binary(dae::BinaryOperator::Subtract, b, s)
        })?;
        model.continuous(|continuous| continuous.value_equation(alias_at, alias))?;
        unit_derivative_equation(model, der_x_at, state)?;
        mandatory_initialization_equations(model, state, unsolved, r1_at, r2_at)?;
        Ok(())
    })
    .expect("constrained-matching fixture DAE is valid")
}

fn unit_derivative_equation<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    provenance: dae::DaeProvenance,
    state: dae::StateId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let residual = model.expressions(|expressions| {
        let derivative = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::Derivative(state))?;
        let one = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Real(1.0))?;
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, derivative, one)
    })?;
    model.continuous(|continuous| continuous.value_equation(provenance, residual))
}

fn mandatory_initialization_equations<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    state: dae::StateId<'dae>,
    unsolved: dae::ParameterId<'dae>,
    r1_at: dae::DaeProvenance,
    r2_at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let first = model.expressions(|expressions| {
        let x = expressions
            .at(r1_at)
            .coordinate(dae::CoordinateInput::State(state))?;
        let q = expressions
            .at(r1_at)
            .coordinate(dae::CoordinateInput::Parameter(unsolved))?;
        let sum = expressions
            .at(r1_at)
            .binary(dae::BinaryOperator::Add, x, q)?;
        let five = expressions.at(r1_at).literal(dae::DaeLiteral::Real(5.0))?;
        expressions
            .at(r1_at)
            .binary(dae::BinaryOperator::Subtract, sum, five)
    })?;
    model.initialization(|initialization| initialization.value_equation(r1_at, first))?;
    let second = model.expressions(|expressions| {
        let x = expressions
            .at(r2_at)
            .coordinate(dae::CoordinateInput::State(state))?;
        let two = expressions.at(r2_at).literal(dae::DaeLiteral::Real(2.0))?;
        expressions
            .at(r2_at)
            .binary(dae::BinaryOperator::Subtract, x, two)
    })?;
    model.initialization(|initialization| initialization.value_equation(r2_at, second))?;
    Ok(())
}
