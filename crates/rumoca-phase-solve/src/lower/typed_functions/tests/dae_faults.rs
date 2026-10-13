//! A DAE expression and the typed operations it lowers to are classified
//! identically (SPEC_0040 SOLVE-C61, SOLVE-C102): the DAE owner in
//! `lower::dae_faults` and the typed program's fault flow share one
//! vocabulary and one set of Integer interval rules.

use super::*;
use crate::lower::dae_faults::DaeFaults;
use dae::{
    BinaryOperator as B, DaeLiteral as L, PureBuiltin as P, ScalarType as S, UnaryOperator as U,
};
use solve::{FaultClass, FaultKind};

const X: usize = 0;
const Y: usize = 1;
const I: usize = 2;
const J: usize = 3;
const FLAG: usize = 4;
const RA: usize = 5;
const IA: usize = 6;

/// The function inputs in order: Real `x`, `y`, Integer `i`, `j`, Boolean
/// `flag`, Real `ra[3]`, Integer `ia[3]`.
const INPUTS: [(S, &[u32]); 7] = [
    (S::Real, &[]),
    (S::Real, &[]),
    (S::Integer, &[]),
    (S::Integer, &[]),
    (S::Boolean, &[]),
    (S::Real, &[3]),
    (S::Integer, &[3]),
];

type Build = for<'a, 'd> fn(
    &mut dae::Expressions<'a, 'd>,
    dae::DaeProvenance,
    &[dae::ExprId<'d>],
) -> Result<dae::ExprId<'d>, dae::DaeConstructionError>;

struct Case {
    name: &'static str,
    result: (S, &'static [u32]),
    class: FaultClass,
    /// Whether the typed program accepts the expression; an out-of-range
    /// constant subscript is refused at construction instead.
    lowers: bool,
    build: Build,
}

const ARITHMETIC: FaultClass = FaultClass::MayFault(FaultKind::IntegerArithmetic);
const CONVERSION: FaultClass = FaultClass::MayFault(FaultKind::IntegerConversion);
const BOUNDS: FaultClass = FaultClass::MayFault(FaultKind::IndexBounds);
const SAFE: FaultClass = FaultClass::CannotFault;

fn case(name: &'static str, result: (S, &'static [u32]), class: FaultClass, build: Build) -> Case {
    Case {
        name,
        result,
        class,
        lowers: true,
        build,
    }
}

fn integer<'d>(
    e: &mut dae::Expressions<'_, 'd>,
    at: dae::DaeProvenance,
    value: i64,
) -> Result<dae::ExprId<'d>, dae::DaeConstructionError> {
    e.at(at).literal(L::Integer(value))
}

fn element<'d>(
    e: &mut dae::Expressions<'_, 'd>,
    at: dae::DaeProvenance,
    array: dae::ExprId<'d>,
    index: dae::ExprId<'d>,
) -> Result<dae::ExprId<'d>, dae::DaeConstructionError> {
    e.at(at).index(
        array,
        [dae::Subscript::Index {
            expression: index,
            provenance: at,
        }],
    )
}

fn binary_cases() -> Vec<Case> {
    let real = (S::Real, &[][..]);
    let int = (S::Integer, &[][..]);
    let boolean = (S::Boolean, &[][..]);
    let mut cases = Vec::new();
    macro_rules! binary {
        ($name:literal, $result:expr, $class:expr, $op:expr, $lhs:expr, $rhs:expr) => {
            cases.push(case($name, $result, $class, |e, at, p| {
                e.at(at).binary($op, p[$lhs], p[$rhs])
            }));
        };
    }
    binary!("real add", real, SAFE, B::Add, X, Y);
    binary!("real subtract", real, SAFE, B::Subtract, X, Y);
    binary!("real multiply", real, SAFE, B::Multiply, X, Y);
    binary!("real divide", real, SAFE, B::Divide, X, Y);
    binary!("real power", real, SAFE, B::Power, X, Y);
    binary!("real elementwise add", real, SAFE, B::ElementwiseAdd, X, Y);
    binary!("mixed multiply", real, SAFE, B::Multiply, I, X);
    binary!("integer divide", real, SAFE, B::Divide, I, J);
    binary!("real less", boolean, SAFE, B::Less, X, Y);
    binary!("real equal", boolean, SAFE, B::Equal, X, Y);
    binary!(
        "integer greater equal",
        boolean,
        SAFE,
        B::GreaterEqual,
        I,
        J
    );
    binary!("boolean and", boolean, SAFE, B::And, FLAG, FLAG);
    binary!("boolean or", boolean, SAFE, B::Or, FLAG, FLAG);
    binary!("unbounded integer add", int, ARITHMETIC, B::Add, I, J);
    binary!(
        "unbounded integer subtract",
        int,
        ARITHMETIC,
        B::Subtract,
        I,
        J
    );
    binary!(
        "unbounded integer multiply",
        int,
        ARITHMETIC,
        B::Multiply,
        I,
        J
    );
    binary!("real array add", (S::Real, &[3][..]), SAFE, B::Add, RA, RA);
    binary!(
        "integer array add",
        (S::Integer, &[3][..]),
        ARITHMETIC,
        B::Add,
        IA,
        IA
    );
    cases.push(case("literal integer sum", int, SAFE, |e, at, _| {
        let (a, b) = (integer(e, at, 3)?, integer(e, at, 4)?);
        e.at(at).binary(B::Add, a, b)
    }));
    cases.push(case("literal integer product", int, SAFE, |e, at, _| {
        let (a, b) = (integer(e, at, 3)?, integer(e, at, 4)?);
        e.at(at).binary(B::Multiply, a, b)
    }));
    cases.push(case(
        "overflowing literal product",
        int,
        ARITHMETIC,
        |e, at, _| {
            let (a, b) = (integer(e, at, i64::MAX)?, integer(e, at, 2)?);
            e.at(at).binary(B::Multiply, a, b)
        },
    ));
    cases
}

fn unary_and_builtin_cases() -> Vec<Case> {
    let real = (S::Real, &[][..]);
    let int = (S::Integer, &[][..]);
    vec![
        case("real negate", real, SAFE, |e, at, p| {
            e.at(at).unary(U::Negate, p[X])
        }),
        case("not", (S::Boolean, &[]), SAFE, |e, at, p| {
            e.at(at).unary(U::Not, p[FLAG])
        }),
        case("unbounded integer negate", int, ARITHMETIC, |e, at, p| {
            e.at(at).unary(U::Negate, p[I])
        }),
        case("literal integer negate", int, SAFE, |e, at, _| {
            let value = integer(e, at, 5)?;
            e.at(at).unary(U::Negate, value)
        }),
        case("sqrt", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Sqrt, [p[X]])
        }),
        case("sin", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Sin, [p[X]])
        }),
        case("exp", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Exp, [p[X]])
        }),
        case("log", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Log, [p[X]])
        }),
        case("atan2", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Atan2, [p[X], p[Y]])
        }),
        case("floor", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Floor, [p[X]])
        }),
        case("real abs", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Abs, [p[X]])
        }),
        case("real min", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Min, [p[X], p[Y]])
        }),
        case("real max", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Max, [p[X], p[Y]])
        }),
        case("unbounded integer abs", int, ARITHMETIC, |e, at, p| {
            e.at(at).builtin(P::Abs, [p[I]])
        }),
        case("unbounded integer min", int, ARITHMETIC, |e, at, p| {
            e.at(at).builtin(P::Min, [p[I], p[J]])
        }),
        case("literal integer max", int, SAFE, |e, at, _| {
            let (a, b) = (integer(e, at, 3)?, integer(e, at, 9)?);
            e.at(at).builtin(P::Max, [a, b])
        }),
        case("real to integer", int, CONVERSION, |e, at, p| {
            e.at(at).builtin(P::Integer, [p[X]])
        }),
        case("real array sum", real, SAFE, |e, at, p| {
            e.at(at).builtin(P::Sum, [p[RA]])
        }),
        case("integer array sum", int, ARITHMETIC, |e, at, p| {
            e.at(at).builtin(P::Sum, [p[IA]])
        }),
    ]
}

fn structure_cases() -> Vec<Case> {
    let real = (S::Real, &[][..]);
    let int = (S::Integer, &[][..]);
    vec![
        case("real conditional", real, SAFE, |e, at, p| {
            e.at(at).conditional([(p[FLAG], p[X])], p[Y])
        }),
        case(
            "conditional with a faulting arm",
            int,
            ARITHMETIC,
            |e, at, p| {
                let sum = e.at(at).binary(B::Add, p[I], p[J])?;
                e.at(at).conditional([(p[FLAG], sum)], p[I])
            },
        ),
        case(
            "real array constructor",
            (S::Real, &[2]),
            SAFE,
            |e, at, p| e.at(at).array([p[X], p[Y]]),
        ),
        case("literal subscript in range", real, SAFE, |e, at, p| {
            let index = integer(e, at, 2)?;
            element(e, at, p[RA], index)
        }),
        Case {
            lowers: false,
            ..case(
                "literal subscript out of range",
                real,
                BOUNDS,
                |e, at, p| {
                    let index = integer(e, at, 4)?;
                    element(e, at, p[RA], index)
                },
            )
        },
        case("computed subscript", real, BOUNDS, |e, at, p| {
            element(e, at, p[RA], p[I])
        }),
        case("integer element read", int, SAFE, |e, at, p| {
            let index = integer(e, at, 3)?;
            element(e, at, p[IA], index)
        }),
        case("whole axis read", (S::Real, &[3]), SAFE, |e, at, p| {
            e.at(at)
                .index(p[RA], [dae::Subscript::Whole { provenance: at }])
        }),
    ]
}

fn value_type(scalar: S, shape: &[u32]) -> dae::ValueType {
    if shape.is_empty() {
        dae::ValueType::scalar(scalar)
    } else {
        dae::ValueType::array(scalar, shape.to_vec())
    }
}

fn input_variability(ordinal: usize) -> dae::InputVariability {
    if INPUTS[ordinal].0 == S::Real {
        dae::InputVariability::Continuous
    } else {
        dae::InputVariability::Discrete
    }
}

type Types<'d> = (Vec<dae::ValueTypeId<'d>>, dae::ValueTypeId<'d>);

fn declare_types<'d>(
    model: &mut dae::DaeConstruction<'d>,
    case: &Case,
    at: dae::DaeProvenance,
) -> Result<Types<'d>, dae::DaeConstructionError> {
    model.types(|types| {
        let inputs = INPUTS
            .iter()
            .map(|(scalar, shape)| types.derived(value_type(*scalar, shape), at))
            .collect::<Result<Vec<_>, _>>()?;
        let (scalar, shape) = case.result;
        Ok((inputs, types.derived(value_type(scalar, shape), at)?))
    })
}

type Parameters<'d> = (Vec<dae::FunctionParameterId<'d>>, dae::FunctionValueId<'d>);

fn declare_parameters<'d>(
    functions: &mut dae::Functions<'_, 'd>,
    reservation: &dae::FunctionReservation<'_, 'd>,
    at: dae::DaeProvenance,
) -> Result<Parameters<'d>, dae::DaeConstructionError> {
    let parameters = (0..INPUTS.len())
        .map(|ordinal| {
            functions.parameter(
                reservation,
                VarName::new(format!("p{ordinal}")),
                ordinal,
                at,
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((
        parameters,
        functions.output(reservation, VarName::new("y"), 0, at)?,
    ))
}

fn fixture(case: &Case) -> Result<(dae::Dae, u32), dae::DaeConstructionError> {
    let root = std::cell::Cell::new(0);
    let mut sources = SourceMap::new();
    let source = sources.add("faults.mo", "function f end f;");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (input_types, result_type) = declare_types(model, case, at)?;
        let signature =
            dae::FunctionSignature::new(VarName::new("f"), input_types.clone(), [result_type], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let (parameters, output) =
                model.functions(|functions| declare_parameters(functions, &reservation, at))?;
            let value = model.expressions(|expressions| {
                let reads = parameters
                    .iter()
                    .map(|parameter| expressions.at(at).function_parameter(*parameter))
                    .collect::<Result<Vec<_>, _>>()?;
                let value = (case.build)(expressions, at, &reads)?;
                root.set(value.index());
                Ok(value)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let arguments = input_types
            .iter()
            .enumerate()
            .map(|(ordinal, input_type)| {
                model.variables(|variables| {
                    variables.input(
                        VarName::new(format!("p{ordinal}")),
                        *input_type,
                        input_variability(ordinal),
                        at,
                        dae::VariableAttributes::default(),
                    )
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        model.expressions(|expressions| {
            let reads = arguments
                .iter()
                .map(|argument| {
                    expressions
                        .at(at)
                        .coordinate(dae::CoordinateInput::Input(*argument))
                })
                .collect::<Result<Vec<_>, _>>()?;
            expressions.at(at).call(function, 0, reads)?;
            Ok(())
        })
    })?;
    Ok((model, root.get()))
}

/// The class the DAE owner gives the function result expression `root`.
fn dae_class(model: &dae::Dae, root: u32) -> FaultClass {
    model.inspect(|view| {
        let root = view.expression_id(root as usize).unwrap();
        DaeFaults::new(view, &[]).class(&mut HashMap::new(), root)
    })
}

fn typed_class(model: &dae::Dae) -> Result<FaultClass, String> {
    let table = model
        .inspect(|view| {
            let call = (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .find(|expression| {
                    view.expression(*expression).is_some_and(|node| {
                        node.function_scope().is_none()
                            && matches!(node.operation(), dae::ExpressionOperation::Call { .. })
                    })
                })
                .unwrap();
            lower_exact_call(
                view,
                call,
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            )
        })
        .map_err(|error| format!("{error:?}"))?;
    let [owner] = table.owners() else {
        return Err("not one issued owner".to_owned());
    };
    Ok(owner.body().region_fault_class())
}

#[test]
fn dae_classification_equals_the_typed_programs_for_every_operator_class() {
    let cases = binary_cases()
        .into_iter()
        .chain(unary_and_builtin_cases())
        .chain(structure_cases());
    let mut disagreements = Vec::new();
    for case in cases {
        let (model, root) = match fixture(&case) {
            Ok(built) => built,
            Err(error) => {
                disagreements.push(format!("{}: construction {error:?}", case.name));
                continue;
            }
        };
        let dae_class = dae_class(&model, root);
        let typed_class = match typed_class(&model) {
            Ok(class) => class,
            Err(_) if !case.lowers => {
                if dae_class != case.class {
                    disagreements.push(format!(
                        "{}: dae {dae_class:?}, expected {:?}",
                        case.name, case.class
                    ));
                }
                continue;
            }
            Err(error) => {
                disagreements.push(format!(
                    "{}: dae {dae_class:?}, typed lowering refused: {error}",
                    case.name
                ));
                continue;
            }
        };
        if dae_class != typed_class || dae_class != case.class {
            disagreements.push(format!(
                "{}: dae {dae_class:?}, typed {typed_class:?}, expected {:?}",
                case.name, case.class
            ));
        }
    }
    assert!(disagreements.is_empty(), "{}", disagreements.join("\n"));
}
