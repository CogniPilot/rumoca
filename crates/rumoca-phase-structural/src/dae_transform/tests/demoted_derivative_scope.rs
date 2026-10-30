//! A demoted derivative first rebuilt inside a model arm stays in that arm;
//! a use the arm does not dominate rebuilds it in its own scope.
use super::*;

const SOURCE: &str = "f = if x > 0 then der(s) else 0; y = der(s); s = x * x; der(x) = 1;";

/// Where `der(s)` occurs before its root use in `y = der(s)`.
#[derive(Clone, Copy)]
enum ArmUse {
    Then,
    BothArms,
    Nested,
}

fn derivative_of<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    state: dae::StateId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Derivative(state)))
}

fn zero<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|e| e.at(at).literal(dae::DaeLiteral::Real(0.0)))
}

fn arm_derivative_model(arm_use: ArmUse) -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add("arm_derivative.mo", SOURCE);
    let at = source_provenance(source, SOURCE, SOURCE);
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let (x, s, f, y) = dae.variables(|variables| {
            Ok((
                variables.state(VarName::new("x"), real, at, Default::default())?,
                variables.state(VarName::new("s"), real, at, Default::default())?,
                variables.algebraic(VarName::new("f"), real, at, Default::default())?,
                variables.algebraic(VarName::new("y"), real, at, Default::default())?,
            ))
        })?;
        // The arm equation is constructed before the root definition of s,
        // so the demoted derivative's operands are first rebuilt in the arm.
        let condition = dae.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            e.at(at).binary(dae::BinaryOperator::Greater, x, zero)
        })?;
        let owner = dae.begin_model_conditional(condition, at)?;
        let first = dae.model_conditional_arm(&owner, true, |dae| match arm_use {
            ArmUse::Nested => {
                let inner = dae.begin_model_conditional(condition, at)?;
                let value =
                    dae.model_conditional_arm(&inner, true, |dae| derivative_of(dae, s, at))?;
                let fallback = dae.model_conditional_arm(&inner, false, |dae| zero(dae, at))?;
                dae.finish_model_conditional(inner, [value, fallback])
            }
            ArmUse::Then | ArmUse::BothArms => derivative_of(dae, s, at),
        })?;
        let second = dae.model_conditional_arm(&owner, false, |dae| match arm_use {
            ArmUse::BothArms => derivative_of(dae, s, at),
            ArmUse::Then | ArmUse::Nested => zero(dae, at),
        })?;
        let conditional = dae.finish_model_conditional(owner, [first, second])?;
        let residuals = dae.expressions(|e| {
            let f = e.at(at).coordinate(dae::CoordinateInput::Algebraic(f))?;
            let y = e.at(at).coordinate(dae::CoordinateInput::Algebraic(y))?;
            let s_value = e.at(at).coordinate(dae::CoordinateInput::State(s))?;
            let der_s = e.at(at).coordinate(dae::CoordinateInput::Derivative(s))?;
            let x_value = e.at(at).coordinate(dae::CoordinateInput::State(x))?;
            let der_x = e.at(at).coordinate(dae::CoordinateInput::Derivative(x))?;
            let one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            let square = e
                .at(at)
                .binary(dae::BinaryOperator::Multiply, x_value, x_value)?;
            Ok([
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, f, conditional)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, y, der_s)?,
                e.at(at)
                    .binary(dae::BinaryOperator::Subtract, s_value, square)?,
                e.at(at).binary(dae::BinaryOperator::Subtract, der_x, one)?,
            ])
        })?;
        dae.continuous(|continuous| {
            for residual in residuals {
                continuous.value_equation(at, residual)?;
            }
            Ok(())
        })
    })
    .expect("checked arm-derivative fixture")
}

fn assert_demoted(arm_use: ArmUse) {
    let model = arm_derivative_model(arm_use);
    let prepared = prepare_for_solve(&model)
        .expect("the root definition's derivative is visible wherever it is used");
    assert!(matches!(prepared, PreparedDae::Transformed { .. }));
    prepared.as_dae().inspect(|view| {
        let s = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "s")
            .map(|(_, variable)| variable)
            .expect("s declaration survives reconstruction");
        assert_eq!(s.role(), dae::VariableRole::Algebraic);
        assert!(sort(view).is_ok(), "replacement DAE has a perfect matching");
    });
}

#[test]
fn a_root_definition_first_differentiated_in_an_arm_is_rebuilt_for_its_root_use() {
    assert_demoted(ArmUse::Then);
}

#[test]
fn sibling_arms_each_rebuild_the_demoted_derivative_they_use() {
    assert_demoted(ArmUse::BothArms);
}

#[test]
fn a_nested_arm_use_does_not_leak_to_the_root() {
    assert_demoted(ArmUse::Nested);
}
