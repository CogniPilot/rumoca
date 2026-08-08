use rumoca_core::{SourceMap, Span};
use rumoca_ir_dae::{
    ConditionInput, Dae, DaeConstructionError, DaeLiteral, DaeProvenance, DiscreteRealActivation,
};

#[test]
fn remaining_scalar_partition_methods_construct_and_round_trip() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("scalar_partitions.mo", "initial equation 0; equation 0;");
    let owner = DaeProvenance::source(Span::from_offsets(source, 0, 18)).expect("real source span");
    let dae = Dae::construct(source_map, |dae| {
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.initialization(|initialization| initialization.value_equation(owner, residual))?;
        dae.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(residual))
        })?;
        Ok(())
    })
    .expect("initialization value and discrete-real residual partitions construct");

    dae.inspect(|view| {
        assert_eq!(view.initialization_equation_count(), 1);
        assert_eq!(view.discrete_real_equation_count(), 1);
    });
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.initialization_equation_count(), 1);
        assert_eq!(view.discrete_real_equation_count(), 1);
    });
}

#[test]
fn conditional_discrete_real_equations_require_and_preserve_activation() {
    let mut source_map = SourceMap::new();
    let text = "when trigger then z = rhs; end when;";
    let source = source_map.add("conditional_b1b.mo", text);
    let condition_at =
        DaeProvenance::source(Span::from_offsets(source, 5, 12)).expect("condition source span");
    let equation_at =
        DaeProvenance::source(Span::from_offsets(source, 18, 25)).expect("equation source span");
    let dae = Dae::construct(source_map, |dae| {
        let (trigger, guard) = dae.conditions(|conditions| {
            Ok((
                conditions.reserve(condition_at)?,
                conditions.reserve(condition_at)?,
            ))
        })?;
        let condition = dae.expressions(|expressions| {
            expressions
                .at(condition_at)
                .literal(DaeLiteral::Boolean(true))
        })?;
        dae.conditions(|conditions| {
            conditions.define(trigger, ConditionInput::Discrete(condition), condition_at)?;
            conditions.define(guard, ConditionInput::Discrete(condition), condition_at)
        })?;
        let residual = dae.expressions(|expressions| {
            expressions.at(equation_at).literal(DaeLiteral::Real(0.0))
        })?;
        dae.discrete(|discrete| {
            discrete.when_real_equation(trigger, guard, equation_at, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .expect("defined trigger and guard construct a conditional B.1b equation");

    assert_conditional_activation(&dae, equation_at);
    let encoded = serde_json::to_string(&dae).expect("schema-v12 DAE serializes");
    let decoded: Dae = serde_json::from_str(&encoded).expect("schema-v12 DAE reconstructs");
    assert_conditional_activation(&decoded, equation_at);

    let missing = encoded.replacen("\"activation\":{\"when\"", "\"ignored\":{\"when\"", 1);
    assert_ne!(
        missing, encoded,
        "wire fixture contains required activation"
    );
    assert!(
        serde_json::from_str::<Dae>(&missing).is_err(),
        "wire-v12 cannot omit or rename B.1b activation"
    );
}

fn assert_conditional_activation(dae: &Dae, equation_at: DaeProvenance) {
    dae.inspect(|view| {
        let equation = view
            .discrete_real_equation(0)
            .expect("conditional B.1b equation resolves");
        assert_eq!(equation.provenance(), equation_at);
        assert_eq!(view.source_text(equation.provenance()), Some("z = rhs"));
        assert!(matches!(
            equation.activation(),
            DiscreteRealActivation::When { trigger, guard }
                if trigger.index() == 0 && guard.index() == 1
        ));
    });
}

#[test]
fn conditional_discrete_real_equations_reject_undefined_guards() {
    let mut source_map = SourceMap::new();
    let source = source_map.add("incomplete_b1b.mo", "when trigger then 0; end when;");
    let owner = DaeProvenance::source(Span::from_offsets(source, 0, 30)).expect("real source span");
    let error = Dae::construct(source_map, |dae| {
        let trigger = dae.conditions(|conditions| conditions.reserve(owner))?;
        let guard = dae.conditions(|conditions| conditions.reserve(owner))?;
        let residual =
            dae.expressions(|expressions| expressions.at(owner).literal(DaeLiteral::Real(0.0)))?;
        dae.discrete(|discrete| {
            discrete.when_real_equation(trigger, guard, owner, |equation| {
                equation.residual(residual)
            })
        })?;
        Ok(())
    })
    .unwrap_err();
    assert!(
        matches!(
            error,
            DaeConstructionError::IncompleteDefinition {
                kind: "discrete Real equation activation condition",
                ..
            }
        ),
        "unexpected construction error: {error:?}"
    );
}
