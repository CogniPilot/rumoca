use rumoca_core::{
    ClockLattice, ClockRational, RealMatrixMultiplySemantics, SourceMap, Span, VarName,
};
use rumoca_ir_dae as dae;
use rumoca_ir_galec::ast as gast;
use rumoca_ir_galec::package::{AlgorithmCodeArithmeticProfile, AlgorithmCodePackage};
use rumoca_phase_galec::{GalecInput, GalecOptions, lower_to_algorithm_code};

fn folded_matrix_product_model() -> dae::Dae {
    let text = "function signedZero output Real y; algorithm y := [-0.0] * [+1.0]; \
                end signedZero; parameter Real folded = signedZero(); Clock c = Clock(1);";
    let mut sources = SourceMap::new();
    let source = sources.add("DependentFoldArithmetic.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).expect("fixture span is exact");

    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (function, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("signedZero"), [], [real], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let product = dae.expressions(|expressions| {
                    let negative_zero = expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(-0.0))?;
                    let positive_one = expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(1.0))?;
                    let lhs = expressions.at(provenance).array([negative_zero])?;
                    let rhs = expressions.at(provenance).array([positive_one])?;
                    expressions
                        .at(provenance)
                        .binary(dae::BinaryOperator::Multiply, lhs, rhs)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, product, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let (_, folded) = dae.variables(|variables| {
            variables.reserve_parameter(
                VarName::new("folded"),
                rumoca_core::InstanceId::new(1),
                real,
                provenance,
            )
        })?;
        let binding =
            dae.expressions(|expressions| expressions.at(provenance).call(function, 0, []))?;
        dae.variables(|variables| {
            variables.define(
                folded,
                dae::VariableAttributes {
                    binding: Some(binding),
                    causality: dae::VariableCausality::CalculatedParameter,
                    is_tunable: true,
                    ..Default::default()
                },
                provenance,
            )
        })?;
        dae.clocks(|clocks| {
            clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO)
                    .expect("fixture clock is valid"),
                provenance,
            )?;
            Ok(())
        })
    })
    .expect("checked dependent-fold fixture constructs")
}

fn project<'inv>(
    brand: rumoca_core::TargetInvocationBrand<'inv>,
    model: &dae::Dae,
    semantics: RealMatrixMultiplySemantics,
) -> rumoca_ir_galec::TracedAlgorithmCodeProduct<'inv> {
    lower_to_algorithm_code(
        brand,
        &GalecInput::new(model, "DependentFoldArithmetic"),
        &GalecOptions::new(AlgorithmCodeArithmeticProfile::construct(
            rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
            rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
            semantics,
        )),
    )
    .unwrap_or_else(|errors| panic!("signed-zero fold must project: {errors:?}"))
}

fn folded_assignments(package: &AlgorithmCodePackage) -> Vec<f64> {
    package
        .block()
        .startup
        .statements
        .iter()
        .chain(&package.block().recalibrate.statements)
        .filter_map(|statement| match &statement.node {
            gast::Statement::Assignment {
                target: gast::Reference::State(parts),
                value: gast::Expression::Real(value),
            } if parts.len() == 1 && parts[0].name.lexeme() == "folded" => Some(*value),
            _ => None,
        })
        .collect()
}

#[test]
fn dependent_function_fold_uses_the_explicit_matrix_product_profile() {
    rumoca_core::with_target_invocation_brand(|brand| {
        let model = folded_matrix_product_model();
        for (semantics, negative) in [
            (
                RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
                true,
            ),
            (
                RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
                false,
            ),
        ] {
            let product = project(brand, &model, semantics);
            let package = product.package();
            assert_eq!(
                package.arithmetic_profile().real_matrix_multiply(),
                semantics
            );
            assert_eq!(
                package.constant_folded_parameters().len(),
                1,
                "the call must be generation-time folded under {semantics:?}"
            );
            let values = folded_assignments(package);
            assert_eq!(values.len(), 2, "Startup and Recalibrate must both fold");
            for value in values {
                assert_eq!(value, 0.0);
                assert_eq!(
                    value.is_sign_negative(),
                    negative,
                    "folded signed zero must follow {semantics:?}"
                );
            }
        }
    });
}
