//! Unwritten host inputs reserve numeric storage without issuing source values.
use super::*;
use rumoca_eval_dae::{NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;
use rumoca_ir_solve::SolveDeclarationStart;

fn inputs(read_missing: bool) -> dae::Dae {
    let source = TestSource::new(
        "input Real r; input Integer i; input Boolean b; input Real z(start=0); input Integer n(start=0); input Boolean f(start=false); input Integer bound=0; input Real dependent=r;",
    );
    let at = source.at(0, 173);
    dae::Dae::construct(source.map, |model| {
        let (real, integer, boolean) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
            ))
        })?;
        let (zero, int_zero, no) = model.expressions(|e| {
            Ok((
                e.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                e.at(at).literal(dae::DaeLiteral::Integer(0))?,
                e.at(at).literal(dae::DaeLiteral::Boolean(false))?,
            ))
        })?;
        let real_input = model.variables(|v| {
            let mut first = None;
            for (name, ty, variability, start, binding) in [
                ("r", real, dae::InputVariability::Continuous, None, None),
                ("i", integer, dae::InputVariability::Discrete, None, None),
                ("b", boolean, dae::InputVariability::Discrete, None, None),
                (
                    "z",
                    real,
                    dae::InputVariability::Continuous,
                    Some(zero),
                    None,
                ),
                (
                    "n",
                    integer,
                    dae::InputVariability::Discrete,
                    Some(int_zero),
                    None,
                ),
                (
                    "f",
                    boolean,
                    dae::InputVariability::Discrete,
                    Some(no),
                    None,
                ),
                (
                    "bound",
                    integer,
                    dae::InputVariability::Discrete,
                    None,
                    Some(int_zero),
                ),
            ] {
                let id = v.input(
                    VarName::new(name),
                    ty,
                    variability,
                    at,
                    dae::VariableAttributes {
                        start,
                        binding,
                        causality: dae::VariableCausality::Input,
                        declared_causality: dae::DeclaredCausality::Input,
                        ..Default::default()
                    },
                )?;
                first.get_or_insert(id);
            }
            Ok(first.unwrap())
        })?;
        if read_missing {
            let binding = model
                .expressions(|e| e.at(at).coordinate(dae::CoordinateInput::Input(real_input)))?;
            model.variables(|v| {
                v.input(
                    VarName::new("dependent"),
                    real,
                    dae::InputVariability::Continuous,
                    at,
                    dae::VariableAttributes {
                        binding: Some(binding),
                        causality: dae::VariableCausality::Input,
                        declared_causality: dae::DeclaredCausality::Input,
                        ..Default::default()
                    },
                )
            })?;
        }
        Ok(())
    })
    .unwrap()
}

#[test]
fn host_driven_missing_inputs_reserve_storage_and_keep_exact_source_missing() {
    let model = inputs(false);
    let lowered = crate::lower_solve_model_with_input_policy(
        &model,
        &Default::default(),
        dae::InputInitializationPolicy::HostDrivenStart,
        |_| {},
    )
    .unwrap();
    let component = crate::fmi::finish_fmi_component(lowered).unwrap();
    let missing = |name| {
        component
            .variables()
            .iter()
            .find(|variable| variable.name() == name)
            .unwrap()
    };
    for name in ["r", "i", "b"] {
        assert!(matches!(
            missing(name).declaration_start(),
            SolveDeclarationStart::Missing { count: 1, .. }
        ));
    }
    assert!(
        matches!(missing("z").declaration_start(), SolveDeclarationStart::Real(values) if values.value(0) == Some(0.0))
    );
    assert!(
        matches!(missing("n").declaration_start(), SolveDeclarationStart::Integer(values) if values.value(0) == Some(&0))
    );
    assert!(
        matches!(missing("f").declaration_start(), SolveDeclarationStart::Boolean(values) if values.value(0) == Some(&false))
    );
    assert!(
        matches!(missing("bound").declaration_start(), SolveDeclarationStart::Integer(values) if values.value(0) == Some(&0))
    );
    assert!(
        crate::lower_solve_model(&model, &Default::default(), |_| {}).is_err(),
        "RequireRuntimeDriver stays strict"
    );
    model.inspect(|view| {
        let (id, variable) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "r")
            .unwrap();
        let mut evaluator = NumericEvaluator::with_input_policy(
            view,
            |_, _| None,
            dae::InputInitializationPolicy::HostDrivenStart,
        );
        let error = evaluator.initial_values(id).unwrap_err();
        assert_eq!(error.kind(), NumericEvaluationErrorKind::MissingValue);
        assert_eq!(error.span(), variable.declaration().span());
    });
}

#[test]
fn host_driven_reserved_storage_cannot_feed_reached_source_initialization() {
    let model = inputs(true);
    let expected_span = model.inspect(|view| {
        let (_, dependent) = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "dependent")
            .unwrap();
        view.expression(dependent.binding().unwrap())
            .unwrap()
            .provenance()
            .span()
    });
    let error = crate::lower_solve_model_with_input_policy(
        &model,
        &Default::default(),
        dae::InputInitializationPolicy::HostDrivenStart,
        |_| {},
    )
    .err()
    .unwrap();
    let crate::SolveModelLoweringError::RuntimeValues { message, span } = error else {
        panic!("a reached input binding must refuse in numeric preparation: {error}");
    };
    assert_eq!(
        message,
        "numeric evaluation depends on a runtime coordinate"
    );
    assert_eq!(span, Some(expected_span));
}

#[test]
fn supplied_host_values_use_original_storage_and_do_not_reauthor_missing_starts() {
    let model = inputs(false);
    let overrides = std::collections::HashMap::from([
        ("r".to_owned(), 3.0),
        ("i".to_owned(), 7.0),
        ("b".to_owned(), 1.0),
    ]);
    let lowered = crate::lower_solve_model_with_input_policy(
        &model,
        &overrides,
        dae::InputInitializationPolicy::HostDrivenStart,
        |_| {},
    )
    .unwrap();
    for (name, expected) in [("r", 3.0), ("i", 7.0), ("b", 1.0)] {
        let Some(rumoca_ir_solve::ScalarSlot::P { index, .. }) =
            lowered.model().problem.layout.binding(name)
        else {
            panic!("original P storage")
        };
        assert_eq!(lowered.model().parameters.value(index), Some(expected));
    }
    let component = crate::fmi::finish_fmi_component(lowered).unwrap();
    for name in ["r", "i", "b"] {
        assert!(
            component
                .variables()
                .iter()
                .find(|variable| variable.name() == name)
                .unwrap()
                .declaration_start()
                .is_missing()
        );
    }
}
