use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::{ComputeNode, LinearOp, ScalarSlot};

use crate::{LowerError, lower_solve_problem};

struct TestSource {
    map: SourceMap,
    source: rumoca_core::SourceId,
}

impl TestSource {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let source = map.add("solve.mo", text);
        Self { map, source }
    }

    fn at(&self, start: usize, end: usize) -> dae::DaeProvenance {
        dae::DaeProvenance::source(Span::from_offsets(self.source, start, end)).unwrap()
    }
}

fn scaled_state_model(source: TestSource, coefficient: f64) -> dae::Dae {
    let parameter_at = source.at(0, 18);
    let state_at = source.at(20, 26);
    let owner = source.at(28, 40);
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                parameter_at,
            )
        })?;
        let binding = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(coefficient))
        })?;
        let (parameter, state) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        ..dae::VariableAttributes::default()
                    },
                )?,
                variables.state(
                    VarName::new("x"),
                    real,
                    state_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let parameter = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let scaled = expressions.at(owner).binary(
                dae::BinaryOperator::Multiply,
                parameter,
                derivative,
            )?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, scaled, state)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap()
}

#[test]
fn explicit_state_equation_lowers_to_derivative_program() {
    let source = TestSource::new("Real x; der(x) = -x;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 19);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let rhs = expressions
                .at(owner)
                .unary(dae::UnaryOperator::Negate, state)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, derivative, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.solve_layout.state_scalar_count(), 1);
    assert!(solve.continuous.residual.nodes.is_empty());
    assert!(solve.continuous.implicit_rhs.nodes.is_empty());
    assert!(solve.continuous.implicit_row_targets.is_empty());
    assert!(solve.continuous.algebraic_projection_plan.is_empty());
    solve
        .validate()
        .expect("lowered explicit state system satisfies the Solve shape contract");
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    assert_eq!(rows.output_indices, [0]);
    assert_eq!(rows.program_spans, [owner.span()]);
    assert!(matches!(
        rows.programs[0][0],
        LinearOp::LoadY { index: 0, .. }
    ));
    assert!(matches!(
        rows.programs[0][1],
        LinearOp::Unary {
            op: rumoca_ir_solve::UnaryOp::Neg,
            ..
        }
    ));
}

#[test]
fn affine_state_equation_preserves_its_runtime_parameter_coefficient() {
    let source = TestSource::new("parameter Real p=2; Real x; p*der(x)-x=0;");
    let model = scaled_state_model(source, 2.0);

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one scalar derivative block expected");
    };
    let program = &rows.programs[0];
    assert!(
        program
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 0, .. })),
        "the coefficient must use the runtime parameter slot, not its declaration value"
    );
    assert!(
        program.iter().any(|operation| matches!(
            operation,
            LinearOp::Binary {
                op: rumoca_ir_solve::BinaryOp::Div,
                ..
            }
        )),
        "the proven affine coefficient must explicitly isolate the derivative"
    );
}

#[test]
fn zero_affine_derivative_coefficient_fails_before_runtime() {
    let source = TestSource::new("parameter Real p=0; Real x; p*der(x)-x=0;");
    let model = scaled_state_model(source, 0.0);

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::NonComputable { reason, .. }
            if reason.contains("zero affine coefficient")
    ));
}

#[test]
fn nested_comprehension_binders_lower_through_lexical_domain_scopes() {
    let source = TestSource::new("Real x[2,3]; equation x = {{i + j for j in 1:3} for i in 1:2};");
    let declaration = source.at(0, 11);
    let owner = source.at(22, 61);
    let outer_range = source.at(57, 60);
    let inner_range = source.at(43, 46);
    let singleton_domain = |name: &str, upper| StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: name.to_string(),
            lower: 1,
            upper,
            step: 1,
        }],
    };
    let model = dae::Dae::construct(source.map, |model| {
        let real_array = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2, 3]),
                declaration,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(
                VarName::new("x"),
                real_array,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let outer =
            model.domains(|domains| domains.structured(singleton_domain("i", 2), outer_range))?;
        let i = model.domains(|domains| domains.binder(outer, 0, owner))?;
        let inner = model.domains(|domains| {
            domains.nested_in_scope([i], singleton_domain("j", 3), inner_range)
        })?;
        let j = model.domains(|domains| domains.binder(inner, 0, owner))?;
        let residual = model.expressions(|expressions| {
            let x = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(x))?;
            let i = expressions.at(owner).binder(i)?;
            let j = expressions.at(owner).binder(j)?;
            let sum = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, i, j)?;
            let inner = expressions.at(owner).comprehension(inner, sum)?;
            let nested = expressions.at(owner).comprehension(outer, inner)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, x, nested)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.solve_layout.algebraic_scalar_count(), 6);
    assert_eq!(solve.continuous.residual.len().unwrap(), 6);
}

#[test]
fn primitive_relation_root_lowers_to_signed_event_program() {
    let source = TestSource::new("Real x; der(x) = -1; when x > 0 then end when;");
    let declaration = source.at(0, 6);
    let equation_owner = source.at(8, 19);
    let relation_owner = source.at(26, 31);
    let when_owner = source.at(21, 45);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (residual, relation_expression) = model.expressions(|expressions| {
            let derivative = expressions
                .at(equation_owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let negative_one = expressions
                .at(equation_owner)
                .literal(dae::DaeLiteral::Real(-1.0))?;
            let residual = expressions.at(equation_owner).binary(
                dae::BinaryOperator::Subtract,
                derivative,
                negative_one,
            )?;
            let state = expressions
                .at(relation_owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            let zero = expressions
                .at(relation_owner)
                .literal(dae::DaeLiteral::Real(0.0))?;
            let relation =
                expressions
                    .at(relation_owner)
                    .binary(dae::BinaryOperator::Greater, state, zero)?;
            Ok((residual, relation))
        })?;
        model.continuous(|continuous| continuous.value_equation(equation_owner, residual))?;
        let (relation, activation) = model.conditions(|conditions| {
            let relation = conditions.relation(relation_expression, relation_owner)?;
            let activation = conditions.reserve(when_owner)?;
            conditions.define(
                activation,
                dae::ConditionInput::Relation(relation),
                relation_owner,
            )?;
            Ok((relation, activation))
        })?;
        model.conditions(|conditions| conditions.root(relation, activation, when_owner))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.events.root_conditions.programs.len(), 1);
    assert_eq!(
        solve.events.root_conditions.program_spans,
        [when_owner.span()]
    );
    assert_eq!(
        solve.events.root_zero_domains,
        [rumoca_ir_solve::RootZeroDomain::Positive]
    );
    assert!(solve.events.root_relation_memory_targets[0].is_none());
}

#[test]
fn square_matrix_state_equation_lowers_to_one_checked_linear_solve() {
    let source = TestSource::new("Real omega[2]; Real J[2,2]; Real tau[2]; J * der(omega) = tau;");
    let state_at = source.at(0, 14);
    let matrix_at = source.at(16, 27);
    let rhs_at = source.at(29, 40);
    let owner = source.at(42, 62);
    let model = dae::Dae::construct(source.map, |model| {
        let (vector, matrix) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    state_at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::array(dae::ScalarType::Real, [2, 2]),
                    matrix_at,
                )?,
            ))
        })?;
        let (state, matrix_parameter, rhs_parameter) = model.variables(|variables| {
            Ok((
                variables.state(
                    VarName::new("omega"),
                    vector,
                    state_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("J"),
                    matrix,
                    matrix_at,
                    dae::VariableAttributes::default(),
                )?,
                variables.parameter(
                    VarName::new("tau"),
                    vector,
                    rhs_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let matrix = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(matrix_parameter))?;
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let product =
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Multiply, matrix, derivative)?;
            let rhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(rhs_parameter))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, product, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [
        ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            next_reg,
            span,
            ..
        },
    ] = solve.continuous.derivative_rhs.nodes.as_slice()
    else {
        panic!("one checked linear-solve node expected");
    };
    assert_eq!(*n, 2);
    assert_eq!(*span, owner.span());
    assert!(matrix_start < rhs_start);
    assert!(rhs_start < next_reg);
    assert_eq!(
        setup_ops
            .iter()
            .filter(|op| matches!(op, LinearOp::Move { .. }))
            .count(),
        6,
        "four matrix and two RHS values must be packed explicitly"
    );
}

#[test]
fn algebraic_residual_uses_checked_y_and_p_layouts() {
    let source = TestSource::new("parameter Real p = 2; Real y; y = p;");
    let parameter_at = source.at(0, 20);
    let algebraic_at = source.at(22, 28);
    let owner = source.at(30, 35);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                parameter_at,
            )
        })?;
        let two = model.expressions(|expressions| {
            expressions
                .at(parameter_at)
                .literal(dae::DaeLiteral::Real(2.0))
        })?;
        let (parameter, algebraic) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    parameter_at,
                    dae::VariableAttributes {
                        binding: Some(two),
                        ..dae::VariableAttributes::default()
                    },
                )?,
                variables.algebraic(
                    VarName::new("y"),
                    real,
                    algebraic_at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
            let rhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Parameter(parameter))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert!(matches!(
        solve.layout.binding("y"),
        Some(ScalarSlot::Y { index: 0, .. })
    ));
    assert!(matches!(
        solve.layout.binding("p"),
        Some(ScalarSlot::P { index: 0, .. })
    ));
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert_eq!(rows.output_indices, [0]);
    assert!(
        rows.programs[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadY { index: 0, .. }))
    );
    assert!(
        rows.programs[0]
            .iter()
            .any(|op| matches!(op, LinearOp::LoadP { index: 0, .. }))
    );
    let [ComputeNode::ScalarPrograms(implicit)] = solve.continuous.implicit_rhs.nodes.as_slice()
    else {
        panic!("the matched algebraic row must be executable by the runtime");
    };
    assert_eq!(implicit.output_indices, [0]);
    assert_eq!(
        solve.continuous.implicit_row_targets,
        [Some(ScalarSlot::Y {
            index: 0,
            byte_offset: 0,
        })]
    );
    assert_eq!(
        solve.continuous.algebraic_projection_plan.blocks[0].rows,
        [0]
    );
    assert_eq!(
        solve.continuous.algebraic_projection_plan.blocks[0].y_indices,
        [0]
    );
}

#[test]
fn checked_function_call_is_inlined_into_solve_program() {
    let source =
        TestSource::new("function f input Real u; output Real y; y := u + 1; Real z; z=f(2);");
    let function_at = source.at(0, 51);
    let variable_at = source.at(52, 59);
    let owner = source.at(60, 67);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), function_at)
        })?;
        let (function, reservation) = model.functions(|functions| {
            functions.reserve_recursive(VarName::new("f"), [real], [real], function_at)
        })?;
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
        let mut body = model.functions(|functions| functions.begin(reservation, function_at))?;
        model.functions(|functions| {
            functions.assign(&mut body, local, local_definition, function_at)
        })?;
        let local_value = model.functions(|functions| functions.read(&body, local, function_at))?;
        let result = model.expressions(|expressions| {
            let two = expressions
                .at(function_at)
                .literal(dae::DaeLiteral::Real(2.0))?;
            expressions
                .at(function_at)
                .binary(dae::BinaryOperator::Multiply, local_value, two)
        })?;
        model.functions(|functions| functions.assign(&mut body, output, result, function_at))?;
        model.functions(|functions| functions.define(body, function_at))?;
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
            let two = expressions.at(owner).literal(dae::DaeLiteral::Real(2.0))?;
            let call = expressions.at(owner).call(function, 0, [two])?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, z, call)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ComputeNode::ScalarPrograms(rows)] = solve.continuous.residual.nodes.as_slice() else {
        panic!("one scalar residual block expected");
    };
    assert!(
        rows.programs[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::Binary { .. }))
    );
}

#[test]
fn implicit_derivative_form_fails_at_the_equation_span() {
    let source = TestSource::new("Real x; der(x) * der(x) = x;");
    let declaration = source.at(0, 6);
    let owner = source.at(8, 28);
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let state = model.variables(|variables| {
            variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let residual = model.expressions(|expressions| {
            let derivative = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Derivative(state))?;
            let squared = expressions.at(owner).binary(
                dae::BinaryOperator::Multiply,
                derivative,
                derivative,
            )?;
            let state = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::State(state))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, squared, state)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, residual))
    })
    .unwrap();

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::NonComputable { span, .. } if span == owner.span()
    ));
}

#[test]
fn clocked_discrete_definition_lowers_with_exact_row_owner() {
    let source = TestSource::new("Real d; Clock c=Clock(0.1); d=sample(time);");
    let declaration = source.at(0, 6);
    let clock_at = source.at(16, 26);
    let owner = source.at(28, 42);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
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
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock, variable, owner))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let time = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, time)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert_eq!(solve.clocks.periodic_event_schedules.len(), 1);
    assert_eq!(solve.clocks.periodic_event_schedules[0].lattice(), lattice);
    assert_eq!(solve.discrete.clock_owners.len(), 1);
    let clock = solve.discrete.clock_owners[0].expect("row has a clock owner");
    assert_eq!(
        solve
            .clocks
            .periodic_schedule(clock)
            .expect("typed clock owner resolves")
            .lattice(),
        lattice
    );
    assert!(
        solve.discrete.rhs.programs[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadTime { .. }))
    );
}

#[test]
fn pre_discrete_value_loads_a_distinct_bound_history_lane() {
    let source = TestSource::new("discrete Integer count; count = pre(count);");
    let declaration = source.at(0, 22);
    let owner = source.at(24, 42);
    let model = dae::Dae::construct(source.map, |model| {
        let integer = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                declaration,
            )
        })?;
        let count = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("count"),
                integer,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let previous = model.expressions(|expressions| {
            expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::PreDiscreteValue(count))
        })?;
        model.discrete(|discrete| discrete.assignment(owner, count, previous))?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    assert!(matches!(
        solve.layout.binding("count"),
        Some(ScalarSlot::P { index: 0, .. })
    ));
    assert_eq!(solve.solve_layout.compiled_parameter_len, 2);
    let [binding] = solve.solve_layout.pre_param_bindings.as_slice() else {
        panic!("one exact pre-history binding expected");
    };
    assert_eq!(binding.dest_p_index, 1);
    assert!(matches!(
        binding.source,
        rumoca_ir_solve::PreParamSource::P { index: 0 }
    ));
    assert!(binding.clock_schedule.is_none());
    assert_eq!(
        solve.discrete.pre_modes,
        [rumoca_ir_solve::DiscreteEventPreMode::EventEntry]
    );
    assert!(
        solve.discrete.rhs.programs[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 1, .. }))
    );
}

#[test]
fn previous_loads_history_owned_by_its_exact_clock_schedule() {
    let source = TestSource::new(
        "discrete Real x; Clock c=Clock(0.1); when c then x=previous(x)+1; end when;",
    );
    let declaration = source.at(0, 15);
    let clock_at = source.at(25, 35);
    let condition_at = source.at(42, 43);
    let owner = source.at(49, 64);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
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
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock, variable, owner))?;
        let condition = model.conditions(|conditions| conditions.reserve(condition_at))?;
        model.conditions(|conditions| {
            conditions.define(condition, dae::ConditionInput::Clock(clock), condition_at)
        })?;
        let previous =
            model.temporal(|temporal| temporal.previous_discrete_real(clock, variable, owner))?;
        let value = model.expressions(|expressions| {
            let previous = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Previous(previous))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, previous, one)
        })?;
        model.events(|events| {
            events.assign_discrete_real(condition, condition, variable, value, owner)
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).unwrap();
    let [ordinary_pre, previous] = solve.solve_layout.pre_param_bindings.as_slice() else {
        panic!("ordinary pre and clock-owned previous history bindings expected");
    };
    assert!(ordinary_pre.clock_schedule.is_none());
    assert_eq!(
        previous
            .clock_schedule
            .as_ref()
            .expect("previous has its owning periodic schedule")
            .lattice(),
        lattice
    );
    assert!(
        solve.discrete.rhs.programs[0]
            .iter()
            .any(|operation| matches!(operation, LinearOp::LoadP { index: 2, .. }))
    );
}

#[test]
fn clocked_self_dependent_discrete_residual_fails_before_runtime() {
    let source = TestSource::new("Real d; Clock c=Clock(0.1); d=d+1;");
    let declaration = source.at(0, 6);
    let clock_at = source.at(16, 26);
    let owner = source.at(28, 34);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
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
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock, variable, owner))?;
        let residual = model.expressions(|expressions| {
            let target = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
            let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
            let value = expressions
                .at(owner)
                .binary(dae::BinaryOperator::Add, target, one)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, target, value)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .unwrap();

    let error = lower_solve_problem(&model).unwrap_err();
    assert!(matches!(
        error,
        LowerError::Unsupported { span, .. } if span == owner.span()
    ));
}
