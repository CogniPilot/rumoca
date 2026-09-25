//! The reverse, forward-dual, and typed directional derivative sites apply one
//! set of kink rules (`rumoca_eval_solve::reverse`, "Kink rules").
//!
//! Each probe applies one operation to solver coordinates and differentiates
//! it three ways: the reverse row gradient, the forward dual this module lowers,
//! and the typed directional owner of a pure call with the same body, reached
//! through the `PureCallDirectional` the forward lowering emits. At a kink, a
//! point outside the operation's domain, or an overflow edge, every site must
//! give the finite value the rule table states; at smooth points the sites
//! agree to roundoff. The generated C kernel renders these same forward and
//! directional programs, and the `derivative_kinks` harness of
//! `suite_template_runtime` pins its block Jacobians to the same values.

use rumoca_ir_solve::{
    BinaryOp, LinearOp, ScalarProgramBlock, SolveArithmeticProfile, SolveBinaryOperator,
    SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable,
    SolveRealFormat, SolveScalarType, SolveUnaryOperator, SolveValueType, UnaryOp,
};

#[derive(Clone, Copy, Debug)]
enum Operation {
    Unary(UnaryOp, Option<SolveUnaryOperator>),
    Binary(BinaryOp, Option<SolveBinaryOperator>),
    /// `pow(y0, exponent)` with a constant exponent, so the forward dual sees a
    /// statically zero exponent tangent.
    PowConstantExponent(f64),
}

impl Operation {
    fn arity(self) -> usize {
        match self {
            Self::Unary(..) | Self::PowConstantExponent(_) => 1,
            Self::Binary(..) => 2,
        }
    }
}

/// One operation at one point, with the partials the rule table states when
/// the point is a kink, lies outside the domain, or overflows.
struct Probe {
    operation: Operation,
    point: [f64; 2],
    rule: Option<[f64; 2]>,
}

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn provenance() -> rumoca_core::ProvenanceSpan {
    match span().require_provenance("kink rule fixture") {
        Ok(provenance) => provenance,
        Err(error) => panic!("fixture span is source-backed: {error:?}"),
    }
}

fn primal_row(operation: Operation) -> Vec<LinearOp> {
    let mut row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
    ];
    match operation {
        Operation::Unary(op, _) => row.push(LinearOp::Unary { dst: 2, op, arg: 0 }),
        Operation::Binary(op, _) => row.push(LinearOp::Binary {
            dst: 2,
            op,
            lhs: 0,
            rhs: 1,
        }),
        Operation::PowConstantExponent(exponent) => row.extend([
            LinearOp::Const {
                dst: 3,
                value: exponent,
            },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Pow,
                lhs: 0,
                rhs: 3,
            },
        ]),
    }
    row.push(LinearOp::StoreOutput { src: 2 });
    row
}

fn block(rows: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    match ScalarProgramBlock::with_source_span(rows, provenance()) {
        Ok(block) => block,
        Err(error) => panic!("fixture block is valid: {error:?}"),
    }
}

fn reverse_partials(operation: Operation, point: [f64; 2]) -> [f64; 2] {
    let prepared = match rumoca_eval_solve::PreparedScalarProgramBlock::new(block(vec![
        primal_row(operation),
    ])) {
        Ok(prepared) => prepared,
        Err(error) => panic!("fixture prepares: {error:?}"),
    };
    let mut gradient = [0.0; 2];
    let supported = prepared.reverse_row_y_gradient(
        0,
        &rumoca_eval_solve::reverse::ReverseInputs {
            y: &point,
            p: &[],
            t: 0.0,
            context: rumoca_eval_solve::RowEvalContext::default(),
        },
        &mut gradient,
        &mut rumoca_eval_solve::reverse::ReverseScratch::default(),
    );
    match supported {
        Ok(true) => gradient,
        other => panic!("{operation:?} has a reverse rule: {other:?}"),
    }
}

/// The probed directions: each coordinate alone, then both at once. A rule
/// that depended on which operands carry a tangent would make the combined
/// product differ from the sum of the single ones.
const DIRECTIONS: [[f64; 2]; 3] = [[1.0, 0.0], [0.0, 1.0], [1.0, 1.0]];

/// Reverse partials contracted with each of [`DIRECTIONS`].
fn contract(partials: [f64; 2]) -> [f64; 3] {
    DIRECTIONS.map(|[a, b]| partials[0] * a + partials[1] * b)
}

/// Forward products of `rows` (one primal row) along each of [`DIRECTIONS`].
fn forward_partials(
    rows: &[Vec<LinearOp>],
    pure_calls: Option<&SolvePureCallTable>,
    point: [f64; 2],
) -> [f64; 3] {
    let derived = match crate::lower_scalar_program_block_ad(rows) {
        Ok(derived) => block(derived),
        Err(error) => panic!("forward lowering succeeds: {error:?}"),
    };
    DIRECTIONS.map(|seed| {
        let mut output = [0.0];
        let evaluated = rumoca_eval_solve::eval_scalar_program_block_with_context(
            &derived,
            &point,
            &[],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(&seed),
                pure_calls,
                ..Default::default()
            },
            &mut output,
        );
        match evaluated {
            Ok(()) => output[0],
            Err(error) => panic!("forward program evaluates: {error:?}"),
        }
    })
}

fn directional_table(operation: Operation) -> Option<SolvePureCallTable> {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).ok()?,
    );
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let arity = operation.arity();
    let exponent = match operation {
        Operation::PowConstantExponent(exponent) => Some(exponent),
        _ => None,
    };
    let (unary, binary) = match operation {
        Operation::Unary(_, typed) => (Some(typed?), None),
        Operation::Binary(_, typed) => (None, Some(typed?)),
        Operation::PowConstantExponent(_) => (None, Some(SolveBinaryOperator::Power)),
    };
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::MIN),
            vec![real.clone(); arity],
            vec![SolvePureCallOutput::result(real.clone())],
            span(),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span())?;
                let value = match (unary, binary, exponent) {
                    (Some(operator), _, _) => builder.unary(operator, lhs, span())?,
                    (None, Some(operator), Some(exponent)) => {
                        let rhs = builder.constant(
                            rumoca_ir_solve::SolveValue::real(arithmetic, exponent),
                            span(),
                        )?;
                        builder.binary(operator, lhs, rhs, span())?
                    }
                    (None, Some(operator), None) => {
                        let rhs = builder.load(inputs[1], span())?;
                        builder.binary(operator, lhs, rhs, span())?
                    }
                    (None, None, _) => unreachable!("every probe names an operator"),
                };
                builder.store(outputs[0], value, span())
            },
        )?;
        Ok(())
    });
    match table {
        Ok(table) => Some(table),
        Err(error) => panic!("{operation:?} constructs a typed owner: {error:?}"),
    }
}

fn directional_partials(operation: Operation, point: [f64; 2]) -> Option<[f64; 3]> {
    let table = directional_table(operation)?;
    let owner = &table.owners()[0];
    assert!(
        owner.directional().is_some(),
        "{operation:?} has a directional owner"
    );
    let inputs = (0..operation.arity()).map(|input| input as u32);
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: inputs.collect(),
            site: owner.call_site(),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    Some(forward_partials(&[row], Some(&table), point))
}

/// Distance in units in the last place, with both zeros at distance zero.
fn ulps(lhs: f64, rhs: f64) -> u64 {
    let ordered = |value: f64| {
        let bits = value.to_bits() as i64;
        if bits < 0 { i64::MIN - bits } else { bits }
    };
    ordered(lhs).abs_diff(ordered(rhs))
}

/// Sites agree when both are NaN, or when they are equal (exactly at a kink,
/// within one unit in the last place elsewhere).
fn same(lhs: f64, rhs: f64, exact: bool) -> bool {
    if lhs.is_nan() || rhs.is_nan() {
        lhs.is_nan() && rhs.is_nan()
    } else {
        lhs == rhs || (!exact && ulps(lhs, rhs) <= 1)
    }
}

fn check(probe: &Probe) {
    let Probe {
        operation,
        point,
        rule,
    } = *probe;
    let reverse = contract(reverse_partials(operation, point));
    let forward = forward_partials(&[primal_row(operation)], None, point);
    let directional = directional_partials(operation, point);
    let mut sites = vec![("reverse", reverse), ("forward", forward)];
    sites.extend(directional.map(|products| ("directional", products)));
    sites.extend(rule.map(|rule| ("rule", contract(rule))));
    for (name, products) in &sites {
        for (direction, (value, reference)) in products.iter().zip(&reverse).enumerate() {
            // The table states a finite value at every kink it names.
            let finite = rule.is_none() || value.is_finite();
            assert!(
                finite && same(*value, *reference, rule.is_some()),
                "{operation:?} at {point:?} along {:?}: {name} = {value}, reverse = \
                 {reference} ({sites:?})",
                DIRECTIONS[direction]
            );
        }
    }
}

fn unary(op: UnaryOp, typed: SolveUnaryOperator) -> Operation {
    Operation::Unary(op, Some(typed))
}

fn binary(op: BinaryOp, typed: SolveBinaryOperator) -> Operation {
    Operation::Binary(op, Some(typed))
}

fn at(operation: Operation, x: f64, rule: Option<f64>) -> Probe {
    Probe {
        operation,
        point: [x, 0.0],
        rule: rule.map(|value| [value, 0.0]),
    }
}

fn at2(operation: Operation, point: [f64; 2], rule: Option<[f64; 2]>) -> Probe {
    Probe {
        operation,
        point,
        rule,
    }
}

const SUBNORMAL: f64 = 5e-324;

#[test]
fn unary_kinks_agree_across_sites() {
    use SolveUnaryOperator as T;
    use UnaryOp as U;
    let abs = unary(U::Abs, T::Abs);
    let sqrt = unary(U::Sqrt, T::Sqrt);
    let asin = unary(U::Asin, T::Asin);
    let acos = unary(U::Acos, T::Acos);
    let log = unary(U::Log, T::Log);
    let log10 = unary(U::Log10, T::Log10);
    let tan = unary(U::Tan, T::Tan);
    let tanh = unary(U::Tanh, T::Tanh);
    let mut probes = vec![
        at(abs, 0.0, Some(1.0)),
        at(abs, -0.0, Some(1.0)),
        at(abs, -2.0, Some(-1.0)),
        at(sqrt, 0.0, Some(0.0)),
        at(sqrt, -0.0, Some(0.0)),
        at(sqrt, -1.0, Some(0.0)),
        at(sqrt, 4.0, Some(0.25)),
        at(sqrt, SUBNORMAL, None),
        at(asin, 1.0, Some(0.0)),
        at(asin, -1.0, Some(0.0)),
        at(asin, 1.5, Some(0.0)),
        at(asin, 0.5, None),
        at(acos, 1.0, Some(0.0)),
        at(acos, -1.0, Some(0.0)),
        at(acos, -1.5, Some(0.0)),
        at(acos, 0.5, None),
        at(log, 0.0, Some(0.0)),
        at(log, -0.0, Some(0.0)),
        at(log, -2.0, Some(-0.5)),
        at(log, SUBNORMAL, Some(0.0)),
        at(log, 2.0, Some(0.5)),
        at(log10, 0.0, Some(0.0)),
        at(log10, SUBNORMAL, Some(0.0)),
        at(log10, 10.0, None),
        at(tan, std::f64::consts::FRAC_PI_2, None),
        at(tan, f64::INFINITY, None),
        at(tan, 1.0, None),
        at(tanh, 0.0, Some(1.0)),
        at(tanh, 1.0, None),
        at(tanh, 20.0, None),
        at(tanh, 800.0, Some(0.0)),
    ];
    // Piecewise-constant operations have no tangent, including at their jumps.
    for (op, typed) in [
        (U::Sign, T::Sign),
        (U::Floor, T::Floor),
        (U::Ceil, T::Ceiling),
        (U::Trunc, T::Truncate),
    ] {
        for x in [0.0, -0.0, 1.0, 0.5, -1.5] {
            probes.push(at(unary(op, typed), x, Some(0.0)));
        }
    }
    probes.push(at(Operation::Unary(U::Not, None), 0.0, Some(0.0)));
    for (op, typed) in [
        (U::Neg, T::Negate),
        (U::Sin, T::Sin),
        (U::Cos, T::Cos),
        (U::Atan, T::Atan),
        (U::Sinh, T::Sinh),
        (U::Cosh, T::Cosh),
        (U::Exp, T::Exp),
    ] {
        for x in [0.0, 0.7] {
            probes.push(at(unary(op, typed), x, None));
        }
    }
    probes.iter().for_each(check);
}

#[test]
fn binary_kinks_agree_across_sites() {
    use BinaryOp as B;
    use SolveBinaryOperator as T;
    let atan2 = binary(B::Atan2, T::Atan2);
    let pow = binary(B::Pow, T::Power);
    let div = binary(B::Div, T::Divide);
    let min = binary(B::Min, T::Min);
    let max = binary(B::Max, T::Max);
    let probes = [
        // atan2 has no derivative at the origin.
        at2(atan2, [0.0, 0.0], Some([0.0, 0.0])),
        at2(atan2, [-0.0, 0.0], Some([0.0, 0.0])),
        at2(atan2, [0.0, -0.0], Some([0.0, 0.0])),
        at2(atan2, [0.0, 1.0], Some([1.0, 0.0])),
        at2(atan2, [1.0, 1.0], None),
        // pow: the base partial r*l^(r-1) when finite, the exponent partial
        // l^r*ln(l) only for l > 0, whichever operands are seeded.
        at2(pow, [0.0, 1.0], Some([1.0, 0.0])),
        at2(pow, [-0.0, 1.0], Some([1.0, 0.0])),
        at2(pow, [0.0, 2.0], Some([0.0, 0.0])),
        at2(pow, [0.0, 0.5], Some([0.0, 0.0])),
        at2(pow, [0.0, 0.0], Some([0.0, 0.0])),
        at2(pow, [0.0, -1.0], Some([0.0, 0.0])),
        at2(pow, [-2.0, 2.0], Some([-4.0, 0.0])),
        at2(pow, [-2.0, 3.0], Some([12.0, 0.0])),
        at2(pow, [-2.0, 0.5], Some([0.0, 0.0])),
        at2(pow, [2.0, 3.0], None),
        at2(pow, [2.0, 0.5], None),
        at(Operation::PowConstantExponent(1.0), 0.0, Some(1.0)),
        at(Operation::PowConstantExponent(2.0), 0.0, Some(0.0)),
        at(Operation::PowConstantExponent(0.5), 0.0, Some(0.0)),
        at(Operation::PowConstantExponent(0.5), -2.0, Some(0.0)),
        at(Operation::PowConstantExponent(2.0), -2.0, Some(-4.0)),
        at(Operation::PowConstantExponent(0.0), 0.0, Some(0.0)),
        at(Operation::PowConstantExponent(-1.0), 0.0, Some(0.0)),
        at(Operation::PowConstantExponent(3.0), 1.5, None),
        // Division: each partial when finite, so a zero, subnormal, huge, or
        // infinite denominator zeroes the partial that does not exist.
        at2(div, [1.0, 0.0], Some([0.0, 0.0])),
        at2(div, [1.0, SUBNORMAL], Some([0.0, 0.0])),
        at2(div, [1.0, -SUBNORMAL], Some([0.0, 0.0])),
        at2(div, [1.0, 1e200], Some([1e-200, 0.0])),
        at2(div, [1.0, -1e200], Some([-1e-200, 0.0])),
        at2(div, [1.0, f64::INFINITY], Some([0.0, 0.0])),
        at2(div, [1.0, f64::NEG_INFINITY], Some([0.0, 0.0])),
        at2(div, [0.0, 0.0], Some([0.0, 0.0])),
        at2(div, [0.0, -0.0], Some([0.0, 0.0])),
        at2(div, [1.0, 2.0], None),
        // Ties route to the left operand.
        at2(min, [1.0, 1.0], Some([1.0, 0.0])),
        at2(min, [0.0, -0.0], Some([1.0, 0.0])),
        at2(min, [2.0, 1.0], Some([0.0, 1.0])),
        at2(max, [1.0, 1.0], Some([1.0, 0.0])),
        at2(max, [-0.0, 0.0], Some([1.0, 0.0])),
        at2(max, [1.0, 2.0], Some([0.0, 1.0])),
        at2(
            Operation::Binary(B::And, None),
            [1.0, 0.0],
            Some([0.0, 0.0]),
        ),
        at2(Operation::Binary(B::Or, None), [1.0, 0.0], Some([0.0, 0.0])),
        at2(binary(B::Mul, T::Multiply), [2.0, 3.0], None),
        at2(binary(B::Add, T::Add), [2.0, 3.0], None),
        at2(binary(B::Sub, T::Subtract), [2.0, 3.0], None),
    ];
    probes.iter().for_each(check);
}

/// Every probed value, including the non-finite and extreme ones at which a
/// partial overflows, underflows, or does not exist.
const VALUES: [f64; 17] = [
    f64::NAN,
    f64::INFINITY,
    f64::NEG_INFINITY,
    SUBNORMAL,
    -SUBNORMAL,
    f64::MIN_POSITIVE,
    -f64::MIN_POSITIVE,
    1e200,
    -1e200,
    0.0,
    -0.0,
    1.0,
    -1.0,
    0.5,
    -2.0,
    3.0,
    std::f64::consts::FRAC_PI_2,
];

#[test]
fn every_operation_agrees_at_extreme_operands() {
    use BinaryOp as B;
    use SolveBinaryOperator as T;
    use SolveUnaryOperator as V;
    use UnaryOp as U;
    let unary_operations = [
        (U::Neg, Some(V::Negate)),
        (U::Not, None),
        (U::Abs, Some(V::Abs)),
        (U::Sign, Some(V::Sign)),
        (U::Sqrt, Some(V::Sqrt)),
        (U::Floor, Some(V::Floor)),
        (U::Ceil, Some(V::Ceiling)),
        (U::Trunc, Some(V::Truncate)),
        (U::Sin, Some(V::Sin)),
        (U::Cos, Some(V::Cos)),
        (U::Tan, Some(V::Tan)),
        (U::Asin, Some(V::Asin)),
        (U::Acos, Some(V::Acos)),
        (U::Atan, Some(V::Atan)),
        (U::Sinh, Some(V::Sinh)),
        (U::Cosh, Some(V::Cosh)),
        (U::Tanh, Some(V::Tanh)),
        (U::Exp, Some(V::Exp)),
        (U::Log, Some(V::Log)),
        (U::Log10, Some(V::Log10)),
    ];
    let binary_operations = [
        (B::Add, Some(T::Add)),
        (B::Sub, Some(T::Subtract)),
        (B::Mul, Some(T::Multiply)),
        (B::Div, Some(T::Divide)),
        (B::Pow, Some(T::Power)),
        (B::And, None),
        (B::Or, None),
        (B::Atan2, Some(T::Atan2)),
        (B::Min, Some(T::Min)),
        (B::Max, Some(T::Max)),
    ];
    for x in VALUES {
        for (op, typed) in unary_operations {
            check(&at(Operation::Unary(op, typed), x, None));
        }
        for exponent in VALUES {
            check(&at(Operation::PowConstantExponent(exponent), x, None));
        }
        for y in VALUES {
            for (op, typed) in binary_operations {
                check(&at2(Operation::Binary(op, typed), [x, y], None));
            }
        }
    }
}
