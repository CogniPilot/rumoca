use rumoca_core::{SourceMap, Span, TypeId, VarName};

use super::*;

/// A defining value for the algebraic `c`, built over a parameter `p` and a
/// state `x`. The closure must accept a parameter-only value and refuse any
/// value that reads the state or passes through an impure call.
#[derive(Clone, Copy)]
enum Value {
    BuiltinOverParameter,
    AlgebraicChain,
    MaxOverState,
    BuiltinOverState,
    PureCallOverParameter,
    PureCallOverState,
    PureExternalCallOverParameter,
    ImpureExternalCallOverParameter,
    TransitivelyImpureCallOverParameter,
    NestedPureCallOverInvariantAlgebraic,
}

/// The functions each fixture declares, in callee-first order.
struct Functions<'dae> {
    /// `scale(u) = 2*u`, a Modelica body.
    scale: dae::FunctionId<'dae>,
    /// `pure external "C" y = pext(u)`.
    pure_external: dae::FunctionId<'dae>,
    /// `external "C" y = ext(u)` without explicit purity: MLS §12.3 treats it
    /// as impure.
    impure_external: dae::FunctionId<'dae>,
    /// `wrap(u) = ext(u)`: a Modelica body that reaches an impure callee.
    wrap: dae::FunctionId<'dae>,
    /// `twice(u) = scale(scale(u))`: a Modelica body over a pure callee.
    twice: dae::FunctionId<'dae>,
}

fn classify(value: Value) -> (dae::Dae, u32) {
    let mut sources = SourceMap::new();
    let text = "parameter Real p; Real x; Real a; Real c; equation definitions;";
    let source = sources.add("invariance.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let mut target = 0;
    let dae = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                provenance,
            )
        })?;
        let functions = declare_functions(dae, real, provenance)?;
        let (p, x, a, c) = dae.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    real,
                    provenance,
                    dae::VariableAttributes::default(),
                )?,
                variables.state(
                    VarName::new("x"),
                    real,
                    provenance,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("a"),
                    real,
                    provenance,
                    dae::VariableAttributes::default(),
                )?,
                variables.algebraic(
                    VarName::new("c"),
                    real,
                    provenance,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        target = c.index();
        let coordinates = [
            dae::CoordinateInput::Parameter(p),
            dae::CoordinateInput::State(x),
            dae::CoordinateInput::Algebraic(a),
            dae::CoordinateInput::Algebraic(c),
        ];
        let residuals = dae.expressions(|expressions| {
            residuals(expressions, value, &functions, coordinates, provenance)
        })?;
        dae.continuous(|continuous| add_residuals(continuous, residuals, provenance))
    })
    .unwrap();
    (dae, target)
}

fn declare_functions<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<Functions<'dae>, dae::DaeConstructionError> {
    let scale = modelica_function(dae, "scale", real, at, |e, u| {
        let two = e.at(at).literal(dae::DaeLiteral::Real(2.0))?;
        e.at(at).binary(dae::BinaryOperator::Multiply, two, u)
    })?;
    let pure_external = external_function(dae, "pext", dae::FunctionPurity::Pure, real, at)?;
    let impure_external = external_function(dae, "ext", dae::FunctionPurity::Impure, real, at)?;
    let wrap = modelica_function(dae, "wrap", real, at, |e, u| {
        e.at(at).call(impure_external, 0, [u])
    })?;
    let twice = modelica_function(dae, "twice", real, at, |e, u| {
        let once = e.at(at).call(scale, 0, [u])?;
        e.at(at).call(scale, 0, [once])
    })?;
    Ok(Functions {
        scale,
        pure_external,
        impure_external,
        wrap,
        twice,
    })
}

/// One scalar Modelica function `name(u) = body(u)`.
fn modelica_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    name: &str,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    body: impl FnOnce(
        &mut dae::Expressions<'_, 'dae>,
        dae::ExprId<'dae>,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError>,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new(name), [real], [real], at),
        |dae, reservation| {
            let input = dae.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
            let output = dae.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let value = dae.expressions(|e| {
                let u = e.at(at).function_parameter(input)?;
                body(e, u)
            })?;
            let mut definition = dae.functions(|f| f.begin(reservation, at))?;
            dae.functions(|f| f.assign(&mut definition, output, value, at))?;
            dae.functions(|f| f.define(definition, at))
        },
    )?;
    Ok(function)
}

/// One scalar external C function `y = symbol(u)` with the given purity.
fn external_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    symbol: &str,
    purity: dae::FunctionPurity,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new(symbol), [real], [real], at),
        |dae, reservation| {
            let input = dae.functions(|f| f.parameter(&reservation, VarName::new("u"), 0, at))?;
            let output = dae.functions(|f| f.output(&reservation, VarName::new("y"), 0, at))?;
            let argument = dae.expressions(|e| e.at(at).function_parameter(input))?;
            let body = dae::ExternalFunctionBody::new(
                purity,
                dae::ExternalLanguage::C,
                VarName::new(symbol),
                [dae::ExternalArgument::Input(argument)],
                Some(output),
                dae::ExternalLinkage::new(Vec::<String>::new(), None, None, None),
            );
            dae.functions(|f| f.define_external(reservation, body, at))
        },
    )?;
    Ok(function)
}

type Coordinates<'dae> = [dae::CoordinateInput<'dae>; 4];

fn residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    value: Value,
    functions: &Functions<'dae>,
    coordinates: Coordinates<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let [p, x, a, c] = coordinates;
    let p_value = expressions.at(provenance).coordinate(p)?;
    let x_value = expressions.at(provenance).coordinate(x)?;
    let a_value = expressions.at(provenance).coordinate(a)?;
    let c_value = expressions.at(provenance).coordinate(c)?;
    let a_definition = expressions
        .at(provenance)
        .builtin(dae::PureBuiltin::Abs, [p_value])?;
    let call = |expressions: &mut dae::Expressions<'_, 'dae>, function, argument| {
        expressions.at(provenance).call(function, 0, [argument])
    };
    let c_definition = match value {
        Value::BuiltinOverParameter => expressions
            .at(provenance)
            .builtin(dae::PureBuiltin::Abs, [p_value])?,
        Value::AlgebraicChain => {
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Add, a_value, a_value)?
        }
        Value::MaxOverState => expressions
            .at(provenance)
            .builtin(dae::PureBuiltin::Max, [x_value, p_value])?,
        Value::BuiltinOverState => expressions
            .at(provenance)
            .builtin(dae::PureBuiltin::Abs, [x_value])?,
        Value::PureCallOverParameter => call(expressions, functions.scale, p_value)?,
        Value::PureCallOverState => call(expressions, functions.scale, x_value)?,
        Value::PureExternalCallOverParameter => {
            call(expressions, functions.pure_external, p_value)?
        }
        Value::ImpureExternalCallOverParameter => {
            call(expressions, functions.impure_external, p_value)?
        }
        Value::TransitivelyImpureCallOverParameter => call(expressions, functions.wrap, p_value)?,
        Value::NestedPureCallOverInvariantAlgebraic => call(expressions, functions.twice, a_value)?,
    };
    let a_residual =
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, a_value, a_definition)?;
    let c_residual =
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, c_value, c_definition)?;
    Ok(vec![a_residual, c_residual])
}

fn add_residuals<'dae>(
    continuous: &mut dae::ContinuousEquations<'_, 'dae>,
    residuals: Vec<dae::ExprId<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    for residual in residuals {
        continuous.equation(provenance, |equation| equation.residual(residual))?;
    }
    Ok(())
}

fn target_is_invariant(value: Value) -> bool {
    let (dae, target) = classify(value);
    dae.inspect(|view| TimeInvariance::derive(view).algebraic(target))
}

#[test]
fn a_parameter_only_algebraic_is_time_invariant() {
    assert!(target_is_invariant(Value::BuiltinOverParameter));
}

#[test]
fn an_algebraic_reached_only_through_invariant_algebraics_is_time_invariant() {
    assert!(target_is_invariant(Value::AlgebraicChain));
}

#[test]
fn a_max_over_a_state_is_not_time_invariant() {
    assert!(!target_is_invariant(Value::MaxOverState));
}

#[test]
fn a_builtin_over_a_state_is_not_time_invariant() {
    assert!(!target_is_invariant(Value::BuiltinOverState));
}

#[test]
fn a_pure_call_over_a_parameter_is_time_invariant() {
    assert!(target_is_invariant(Value::PureCallOverParameter));
}

#[test]
fn nested_pure_calls_over_an_invariant_algebraic_are_time_invariant() {
    assert!(target_is_invariant(
        Value::NestedPureCallOverInvariantAlgebraic
    ));
}

#[test]
fn an_explicitly_pure_external_call_over_a_parameter_is_time_invariant() {
    assert!(target_is_invariant(Value::PureExternalCallOverParameter));
}

#[test]
fn a_pure_call_over_a_state_is_not_time_invariant() {
    assert!(!target_is_invariant(Value::PureCallOverState));
}

#[test]
fn an_impure_external_call_over_a_parameter_is_not_time_invariant() {
    assert!(!target_is_invariant(Value::ImpureExternalCallOverParameter));
}

#[test]
fn a_call_that_reaches_an_impure_function_is_not_time_invariant() {
    assert!(!target_is_invariant(
        Value::TransitivelyImpureCallOverParameter
    ));
}

#[test]
fn purity_closure_marks_only_functions_that_reach_an_impure_callee() {
    let (dae, _) = classify(Value::BuiltinOverParameter);
    let pure = dae.inspect(|view| {
        (0..view.function_count())
            .map(|index| {
                let id = view.function_id(index).expect("dense function identity");
                let name = view
                    .function(id)
                    .expect("function resolves")
                    .name()
                    .to_string();
                (name, TimeInvariance::derive(view).pure_functions[index])
            })
            .collect::<Vec<_>>()
    });
    assert_eq!(
        pure,
        [
            ("scale".to_string(), true),
            ("pext".to_string(), true),
            ("ext".to_string(), false),
            ("wrap".to_string(), false),
            ("twice".to_string(), true),
        ]
    );
}
