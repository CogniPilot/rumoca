//! Red regressions for one issued call reached through several DoStep uses.
//!
//! These fixtures enter through the public package constructor, then execute
//! the emitted DoStep statement tree with an instrumented primitive-call hook.
//! The hook makes each dynamic invocation return a distinct token, so checking
//! both the invocation count and the stored values proves that consumers used
//! the one retained result. Merely counting static call statements would miss
//! a call duplicated into mutually-exclusive branches.

use std::collections::HashMap;

use super::*;

fn project_public<'inv>(
    brand: rumoca_core::TargetInvocationBrand<'inv>,
    model: &dae::Dae,
    name: &str,
) -> rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv> {
    crate::lower_to_algorithm_code(
        brand,
        &crate::GalecInput::new(model, name),
        &crate::GalecOptions::new(
            rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::construct(
                rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
            ),
        ),
    )
    .unwrap_or_else(|errors| panic!("{name} must project through the public entry: {errors:?}"))
}

#[derive(Clone, Copy)]
struct CausalProducerSpans {
    declaration: dae::DaeProvenance,
    shared: dae::DaeProvenance,
    causal: dae::DaeProvenance,
    clock: dae::DaeProvenance,
    seed: dae::DaeProvenance,
    a: dae::DaeProvenance,
    b: dae::DaeProvenance,
}

struct CausalProducerVariables<'dae> {
    copy: dae::FunctionId<'dae>,
    shared: dae::AlgebraicId<'dae>,
    seed: dae::DiscreteRealId<'dae>,
    a: dae::DiscreteRealId<'dae>,
    b: dae::DiscreteRealId<'dae>,
}

fn causal_producer_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    spans: CausalProducerSpans,
) -> Result<CausalProducerVariables<'dae>, dae::DaeConstructionError> {
    let vector = model.types(|types| {
        types.intern(
            TypeId::new(0),
            dae::ValueType::array(dae::ScalarType::Real, [2]),
            spans.declaration,
        )
    })?;
    let copy = define_array_copy(model, vector, spans.causal)?;
    let (shared, seed, a, b) = model.variables(|variables| {
        Ok((
            variables.algebraic(
                VarName::new("shared"),
                rumoca_core::InstanceId::new(1),
                vector,
                spans.shared,
                dae::VariableAttributes::default(),
            )?,
            variables.discrete_real(
                VarName::new("seed"),
                rumoca_core::InstanceId::new(2),
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )?,
            variables.discrete_real(
                VarName::new("a"),
                rumoca_core::InstanceId::new(3),
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )?,
            variables.discrete_real(
                VarName::new("b"),
                rumoca_core::InstanceId::new(4),
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )?,
        ))
    })?;
    Ok(CausalProducerVariables {
        copy,
        shared,
        seed,
        a,
        b,
    })
}

fn construct_causal_producer(
    model: &mut dae::DaeConstruction<'_>,
    spans: CausalProducerSpans,
) -> Result<(), dae::DaeConstructionError> {
    let CausalProducerVariables {
        copy,
        shared,
        seed,
        a,
        b,
    } = causal_producer_variables(model, spans)?;
    let (shared_ref, seed_ref, a_ref, b_ref, seed_value, call, residuals) =
        model.expressions(|expressions| {
            let shared_ref = expressions
                .at(spans.shared)
                .coordinate(dae::CoordinateInput::Algebraic(shared))?;
            let seed_ref = expressions
                .at(spans.declaration)
                .coordinate(dae::CoordinateInput::DiscreteReal(seed))?;
            let a_ref = expressions
                .at(spans.a)
                .coordinate(dae::CoordinateInput::DiscreteReal(a))?;
            let b_ref = expressions
                .at(spans.b)
                .coordinate(dae::CoordinateInput::DiscreteReal(b))?;
            let one = expressions
                .at(spans.seed)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let two = expressions
                .at(spans.seed)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let seed_value = expressions.at(spans.seed).array([one, two])?;
            let call = expressions.at(spans.causal).call(copy, 0, [seed_ref])?;
            let mut residuals = Vec::new();
            for ordinal in 1..=2 {
                let index = expressions
                    .at(spans.causal)
                    .literal(dae::DaeLiteral::Integer(ordinal))?;
                let subscript = dae::Subscript::Index {
                    expression: index,
                    provenance: spans.causal,
                };
                let selected_shared = expressions
                    .at(spans.causal)
                    .index(shared_ref, [subscript])?;
                let selected_call = expressions.at(spans.causal).index(call, [subscript])?;
                residuals.push(expressions.at(spans.causal).binary(
                    dae::BinaryOperator::Subtract,
                    selected_shared,
                    selected_call,
                )?);
            }
            Ok((
                shared_ref, seed_ref, a_ref, b_ref, seed_value, call, residuals,
            ))
        })?;
    let _ = (seed_ref, call);
    model.continuous(|continuous| {
        for residual in residuals {
            continuous.equation(spans.causal, |equation| equation.residual(residual))?;
        }
        Ok(())
    })?;
    let clock = periodic_clock(model, spans.clock)?;
    model.clocks(|clocks| {
        clocks.own_discrete_real(clock, seed, spans.declaration)?;
        clocks.own_discrete_real(clock, a, spans.declaration)?;
        clocks.own_discrete_real(clock, b, spans.declaration)?;
        Ok(())
    })?;
    let tick = model.conditions(|conditions| {
        let tick = conditions.reserve(spans.clock)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.clock)?;
        Ok(tick)
    })?;
    define_when_real_equation(model, tick, tick, spans.a, a_ref, shared_ref)?;
    define_when_real_equation(model, tick, tick, spans.b, b_ref, shared_ref)?;
    define_when_real_equation(model, tick, tick, spans.seed, seed_ref, seed_value)
}

fn causal_producer_reuse_model() -> dae::Dae {
    let text = "function copy input Real u[2]; output Real y[2]; algorithm y := u; end copy; \
                model CausalProducerReuse Real shared[2]; discrete Real seed[2], a[2], b[2]; \
                equation shared = copy(seed); when sample(0, 1) then seed = {1, 2}; \
                a = shared; b = shared; end when; end CausalProducerReuse;";
    let mut sources = SourceMap::new();
    let source = sources.add("CausalProducerReuse.mo", text);
    let spans = CausalProducerSpans {
        declaration: at(source, text, "discrete Real seed[2], a[2], b[2]"),
        shared: at(source, text, "Real shared[2]"),
        causal: at(source, text, "shared = copy(seed)"),
        clock: at(source, text, "sample(0, 1)"),
        seed: at(source, text, "seed = {1, 2}"),
        a: at(source, text, "a = shared"),
        b: at(source, text, "b = shared"),
    };
    dae::Dae::construct(sources, |model| construct_causal_producer(model, spans))
        .expect("checked causal-producer reuse fixture")
}

#[derive(Clone, Copy)]
struct ExclusiveVariableSpans {
    choose: dae::DaeProvenance,
    input: dae::DaeProvenance,
    shared: dae::DaeProvenance,
    output: dae::DaeProvenance,
}

struct ExclusiveVariables<'dae> {
    choose: dae::InputId<'dae>,
    input: dae::InputId<'dae>,
    shared: dae::AlgebraicId<'dae>,
    output: dae::DiscreteRealId<'dae>,
}

fn exclusive_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    spans: ExclusiveVariableSpans,
) -> Result<ExclusiveVariables<'dae>, dae::DaeConstructionError> {
    model.variables(|variables| {
        Ok(ExclusiveVariables {
            choose: variables.input(
                VarName::new("choose"),
                rumoca_core::InstanceId::new(5),
                boolean,
                dae::InputVariability::Discrete,
                spans.choose,
                dae::VariableAttributes {
                    causality: dae::VariableCausality::Input,
                    ..dae::VariableAttributes::default()
                },
            )?,
            input: variables.input(
                VarName::new("u"),
                rumoca_core::InstanceId::new(6),
                real,
                dae::InputVariability::Discrete,
                spans.input,
                dae::VariableAttributes {
                    causality: dae::VariableCausality::Input,
                    ..dae::VariableAttributes::default()
                },
            )?,
            shared: variables.algebraic(
                VarName::new("shared"),
                rumoca_core::InstanceId::new(7),
                real,
                spans.shared,
                dae::VariableAttributes::default(),
            )?,
            output: variables.discrete_real(
                VarName::new("y"),
                rumoca_core::InstanceId::new(8),
                real,
                spans.output,
                dae::VariableAttributes {
                    causality: dae::VariableCausality::Output,
                    ..dae::VariableAttributes::default()
                },
            )?,
        })
    })
}

fn mutually_exclusive_call_model() -> dae::Dae {
    let text = "function copy input Real u; output Real y; algorithm y := u; end copy; \
                model ExclusiveCall input Boolean choose; input Real u; Real shared; \
                output discrete Real y; equation shared = copy(u); when sample(0, 1) then \
                y = if choose then shared + 1 else shared - 1; end when; end ExclusiveCall;";
    let mut sources = SourceMap::new();
    let source = sources.add("ExclusiveCall.mo", text);
    let choose_at = at(source, text, "input Boolean choose");
    let input_at = at(source, text, "input Real u");
    let shared_at = at(source, text, "Real shared");
    let output_at = at(source, text, "output discrete Real y");
    let call_at = at(source, text, "shared = copy(u)");
    let clock_at = at(source, text, "sample(0, 1)");
    let assignment = at(
        source,
        text,
        "y = if choose then shared + 1 else shared - 1",
    );
    dae::Dae::construct(sources, |model| {
        let (real, boolean) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    input_at,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    choose_at,
                )?,
            ))
        })?;
        let copy = define_array_copy(model, real, call_at)?;
        let variables = exclusive_variables(
            model,
            real,
            boolean,
            ExclusiveVariableSpans {
                choose: choose_at,
                input: input_at,
                shared: shared_at,
                output: output_at,
            },
        )?;
        let (lhs, value, residual) = model.expressions(|expressions| {
            let choose = expressions
                .at(choose_at)
                .coordinate(dae::CoordinateInput::Input(variables.choose))?;
            let input = expressions
                .at(input_at)
                .coordinate(dae::CoordinateInput::Input(variables.input))?;
            let call = expressions.at(call_at).call(copy, 0, [input])?;
            let shared = expressions
                .at(shared_at)
                .coordinate(dae::CoordinateInput::Algebraic(variables.shared))?;
            let one = expressions
                .at(assignment)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let when_true =
                expressions
                    .at(assignment)
                    .binary(dae::BinaryOperator::Add, shared, one)?;
            let when_false =
                expressions
                    .at(assignment)
                    .binary(dae::BinaryOperator::Subtract, shared, one)?;
            let value = expressions
                .at(assignment)
                .conditional([(choose, when_true)], when_false)?;
            let lhs = expressions
                .at(output_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(variables.output))?;
            let residual =
                expressions
                    .at(call_at)
                    .binary(dae::BinaryOperator::Subtract, shared, call)?;
            Ok((lhs, value, residual))
        })?;
        model.continuous(|continuous| {
            continuous.equation(call_at, |equation| equation.residual(residual))?;
            Ok(())
        })?;
        let clock = periodic_clock(model, clock_at)?;
        model.clocks(|clocks| clocks.own_discrete_real(clock, variables.output, output_at))?;
        let tick = model.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            Ok(tick)
        })?;
        define_when_real_equation(model, tick, tick, assignment, lhs, value)
    })
    .expect("checked mutually-exclusive call fixture")
}

#[derive(Clone, Debug, PartialEq)]
enum TraceValue {
    Bool(bool),
    Integer(i64),
    Real(f64),
    Array(Vec<Self>),
}

impl TraceValue {
    fn bool(&self) -> bool {
        match self {
            Self::Bool(value) => *value,
            other => panic!("expected Boolean trace value, found {other:?}"),
        }
    }

    fn integer(&self) -> i64 {
        match self {
            Self::Integer(value) => *value,
            other => panic!("expected Integer trace value, found {other:?}"),
        }
    }

    fn real(&self) -> f64 {
        match self {
            Self::Real(value) => *value,
            other => panic!("expected Real trace value, found {other:?}"),
        }
    }

    fn tagged(self, tag: f64) -> Self {
        match self {
            Self::Real(value) => Self::Real(value + tag),
            Self::Array(values) => {
                Self::Array(values.into_iter().map(|value| value.tagged(tag)).collect())
            }
            other => panic!("instrumented copy accepts only Real values, found {other:?}"),
        }
    }
}

#[derive(Default)]
struct DoStepTrace {
    values: HashMap<String, TraceValue>,
    calls: usize,
}

impl DoStepTrace {
    fn set_state(&mut self, name: &str, value: TraceValue) {
        self.values.insert(format!("state:{name}"), value);
    }

    fn state(&self, name: &str) -> &TraceValue {
        self.values
            .get(&format!("state:{name}"))
            .unwrap_or_else(|| panic!("trace has no state `{name}`: {:?}", self.values))
    }

    fn execute(&mut self, statements: &[gast::Spanned<gast::Statement>]) {
        for statement in statements {
            self.statement(&statement.node);
        }
    }

    fn statement(&mut self, statement: &gast::Statement) {
        match statement {
            gast::Statement::Assignment { target, value } => {
                let value = self.expression(value);
                self.assign(target, value);
            }
            gast::Statement::MultiAssignment { targets, call } => {
                let values = self.call(call);
                assert_eq!(targets.len(), values.len(), "trace call arity");
                for (target, value) in targets.iter().zip(values) {
                    self.assign(target, value);
                }
            }
            gast::Statement::Call(call) => {
                let _ = self.call(call);
            }
            gast::Statement::If(branching) => self.if_statement(branching),
            gast::Statement::For(loop_) => {
                let start = self.expression(&loop_.start).integer();
                let step = loop_
                    .step
                    .as_ref()
                    .map_or(1, |step| self.expression(step).integer());
                let stop = self.expression(&loop_.stop).integer();
                let iterator = loop_
                    .iterator
                    .as_ref()
                    .expect("capability fixture loops have iterators")
                    .lexeme()
                    .to_owned();
                let mut value = start;
                while (step > 0 && value <= stop) || (step < 0 && value >= stop) {
                    self.values
                        .insert(format!("local:{iterator}"), TraceValue::Integer(value));
                    self.execute(&loop_.body);
                    value += step;
                }
            }
            gast::Statement::Limit(_) | gast::Statement::Signal(_) => {}
        }
    }

    fn if_statement(&mut self, branching: &gast::IfStatement) {
        for branch in &branching.branches {
            if !self.condition_matches(&branch.condition) {
                continue;
            }
            self.execute(&branch.body);
            return;
        }
        if let Some(body) = &branching.else_body {
            self.execute(body);
        }
    }

    fn condition_matches(&mut self, condition: &gast::Condition) -> bool {
        match condition {
            gast::Condition::Expression(condition) => self.expression(condition).bool(),
            gast::Condition::SignalCheck(_) => {
                panic!("capability fixture does not contain signal checks")
            }
        }
    }

    fn call(&mut self, call: &gast::FunctionCall) -> Vec<TraceValue> {
        assert_eq!(call.function.lexeme(), "copy", "unexpected trace call");
        assert_eq!(call.arguments.len(), 1, "copy has one input");
        let argument = self.expression(&call.arguments[0]);
        self.calls += 1;
        vec![argument.tagged(1000.0 * self.calls as f64)]
    }

    fn expression(&mut self, expression: &gast::Expression) -> TraceValue {
        match expression {
            gast::Expression::Bool(value) => TraceValue::Bool(*value),
            gast::Expression::Integer(value) => TraceValue::Integer(*value),
            gast::Expression::Real(value) => TraceValue::Real(*value),
            gast::Expression::Ref(reference) => self.read(reference),
            gast::Expression::Paren(value) => self.expression(value),
            gast::Expression::If(value) => self.if_expression(value),
            gast::Expression::Array(values) => {
                TraceValue::Array(values.iter().map(|value| self.expression(value)).collect())
            }
            gast::Expression::Neg(reference) => TraceValue::Real(-self.read(reference).real()),
            gast::Expression::Not(value) => TraceValue::Bool(!self.expression(value).bool()),
            gast::Expression::Binary { op, lhs, rhs } => {
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                match op {
                    gast::BinaryOp::Add => TraceValue::Real(lhs.real() + rhs.real()),
                    gast::BinaryOp::Sub => TraceValue::Real(lhs.real() - rhs.real()),
                    gast::BinaryOp::Lt => TraceValue::Bool(lhs.real() < rhs.real()),
                    gast::BinaryOp::Gt => TraceValue::Bool(lhs.real() > rhs.real()),
                    gast::BinaryOp::Le => TraceValue::Bool(lhs.real() <= rhs.real()),
                    gast::BinaryOp::Ge => TraceValue::Bool(lhs.real() >= rhs.real()),
                    gast::BinaryOp::Eq => TraceValue::Bool(lhs == rhs),
                    gast::BinaryOp::Ne => TraceValue::Bool(lhs != rhs),
                    gast::BinaryOp::And => TraceValue::Bool(lhs.bool() && rhs.bool()),
                    gast::BinaryOp::Or => TraceValue::Bool(lhs.bool() || rhs.bool()),
                    gast::BinaryOp::Mul | gast::BinaryOp::Div | gast::BinaryOp::Pow => {
                        panic!("capability trace does not need `{op:?}`")
                    }
                }
            }
            gast::Expression::Size { .. } | gast::Expression::Call(_) => {
                panic!("capability trace received an unexpected expression: {expression:?}")
            }
        }
    }

    fn if_expression(&mut self, value: &gast::IfExpression) -> TraceValue {
        for (condition, result) in &value.branches {
            if !self.expression(condition).bool() {
                continue;
            }
            return self.expression(result);
        }
        self.expression(&value.else_value)
    }

    fn key(reference: &gast::Reference) -> (String, &[gast::Expression]) {
        match reference {
            gast::Reference::Local(part) => (
                format!("local:{}", part.name.lexeme()),
                part.subscripts.as_slice(),
            ),
            gast::Reference::State(parts) => {
                let part = parts
                    .first()
                    .expect("checked capability reference has one state part");
                assert_eq!(parts.len(), 1, "capability fixture has no record reference");
                (
                    format!("state:{}", part.name.lexeme()),
                    part.subscripts.as_slice(),
                )
            }
        }
    }

    fn read(&mut self, reference: &gast::Reference) -> TraceValue {
        let (key, subscripts) = Self::key(reference);
        let mut value = self
            .values
            .get(&key)
            .unwrap_or_else(|| panic!("trace read before write of `{key}`: {:?}", self.values))
            .clone();
        for subscript in subscripts {
            let index = usize::try_from(self.expression(subscript).integer() - 1)
                .expect("GALEC subscript is positive");
            value = match value {
                TraceValue::Array(values) => values
                    .get(index)
                    .unwrap_or_else(|| panic!("trace subscript {} out of bounds", index + 1))
                    .clone(),
                other => panic!("trace subscripted non-array {other:?}"),
            };
        }
        value
    }

    fn assign(&mut self, reference: &gast::Reference, value: TraceValue) {
        let (key, subscripts) = Self::key(reference);
        if subscripts.is_empty() {
            self.values.insert(key, value);
            return;
        }
        assert_eq!(
            subscripts.len(),
            1,
            "capability fixture uses rank-one arrays"
        );
        let index = usize::try_from(self.expression(&subscripts[0]).integer() - 1)
            .expect("GALEC subscript is positive");
        let entry = self
            .values
            .entry(key)
            .or_insert_with(|| TraceValue::Array(Vec::new()));
        let TraceValue::Array(values) = entry else {
            panic!("trace assigned an element of non-array storage")
        };
        values.resize(index + 1, TraceValue::Real(f64::NAN));
        values[index] = value;
    }
}

#[test]
fn causal_producer_call_is_executed_once_and_all_consumers_reuse_its_result() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let package = project_public(brand, &causal_producer_reuse_model(), "CausalProducerReuse");
        let mut trace = DoStepTrace::default();
        trace.execute(&package.package().block().do_step.statements);

        let expected = TraceValue::Array(vec![TraceValue::Real(1001.0), TraceValue::Real(1002.0)]);
        assert_eq!(trace.calls, 1, "one issued call executes once in the tick");
        assert_eq!(
            trace.state("a"),
            &expected,
            "first consumer uses retained result"
        );
        assert_eq!(
            trace.state("b"),
            &expected,
            "second consumer uses retained result"
        );
        assert_eq!(
            trace.values.get("local:shared"),
            Some(&expected),
            "the later causal producer store reuses the clock-scheduled result"
        );
    });
}

#[test]
fn mutually_exclusive_realizations_execute_one_owner_per_selected_path() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let package = project_public(brand, &mutually_exclusive_call_model(), "ExclusiveCall");
        for (choose, expected) in [(true, 1004.0), (false, 1002.0)] {
            let mut trace = DoStepTrace::default();
            trace.set_state("choose", TraceValue::Bool(choose));
            trace.set_state("u", TraceValue::Real(3.0));
            trace.execute(&package.package().block().do_step.statements);
            assert_eq!(
                trace.calls, 1,
                "exactly one dynamic realization executes for choose={choose}"
            );
            assert_eq!(
                trace.state("y"),
                &TraceValue::Real(expected),
                "selected branch consumes the one call result"
            );
            assert_eq!(
                trace.values.get("local:shared"),
                Some(&TraceValue::Real(1003.0)),
                "the exhaustive branch join remains available to the causal store"
            );
        }
    });
}
