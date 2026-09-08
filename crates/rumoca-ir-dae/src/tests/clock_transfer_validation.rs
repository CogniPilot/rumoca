use super::*;

#[derive(Clone, Copy)]
enum SiblingCoordinate {
    State,
    Input,
    Parameter,
}

#[test]
fn clock_transfer_rejects_state_and_input_at_their_exact_use() {
    for sibling in [SiblingCoordinate::State, SiblingCoordinate::Input] {
        let (result, sibling_use) = transfer_with_sibling(sibling);
        assert!(matches!(
            result,
            Err(DaeConstructionError::InvalidClockedOperand {
                operator: "clocked value conversion",
                span,
            }) if span == sibling_use
        ));
    }
}

#[test]
fn clock_transfer_accepts_a_parameter_sibling_of_a_clocked_coordinate() {
    transfer_with_sibling(SiblingCoordinate::Parameter)
        .0
        .expect("a parameter does not introduce a continuous clock-domain coordinate");
}

fn transfer_with_sibling(sibling: SiblingCoordinate) -> (Result<Dae, DaeConstructionError>, Span) {
    let source = TestSource::new(
        "discrete Real d; Real stateValue; input Real externalValue; \
         parameter Real parameterValue; subSample(d + stateValue, 2); \
         subSample(d + externalValue, 2); subSample(d + parameterValue, 2);",
    );
    let discrete_at = source.source("discrete Real d", 0);
    let declaration_at = match sibling {
        SiblingCoordinate::State => source.source("Real stateValue", 0),
        SiblingCoordinate::Input => source.source("input Real externalValue", 0),
        SiblingCoordinate::Parameter => source.source("parameter Real parameterValue", 0),
    };
    let (sibling_use, source_at, transfer_at) = match sibling {
        SiblingCoordinate::State => (
            source.source("stateValue", 1),
            source.source("d + stateValue", 0),
            source.source("subSample(d + stateValue, 2)", 0),
        ),
        SiblingCoordinate::Input => (
            source.source("externalValue", 1),
            source.source("d + externalValue", 0),
            source.source("subSample(d + externalValue, 2)", 0),
        ),
        SiblingCoordinate::Parameter => (
            source.source("parameterValue", 1),
            source.source("d + parameterValue", 0),
            source.source("subSample(d + parameterValue, 2)", 0),
        ),
    };
    let result = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                discrete_at,
            )
        })?;
        let discrete = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("d"),
                rumoca_core::InstanceId::new(1),
                real,
                discrete_at,
                VariableAttributes::default(),
            )
        })?;
        let sibling_expression =
            build_sibling_expression(dae, sibling, real, declaration_at, sibling_use)?;
        let (source_clock, target_clock) = dae.clocks(|clocks| {
            let source_clock = clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                transfer_at,
            )?;
            let target_clock = clocks.periodic(
                ClockLattice::new(ClockRational::new(2, 1).unwrap(), ClockRational::ZERO).unwrap(),
                transfer_at,
            )?;
            clocks.own_discrete_real(source_clock.into(), discrete, discrete_at)?;
            Ok((source_clock, target_clock))
        })?;
        dae.expressions(|expressions| {
            let discrete = expressions
                .at(source_at)
                .coordinate(CoordinateInput::DiscreteReal(discrete))?;
            let source = expressions.at(source_at).binary(
                BinaryOperator::Add,
                discrete,
                sibling_expression,
            )?;
            expressions
                .at(transfer_at)
                .clock_transfer(
                    ClockTransferKind::SubSample { factor: 2 },
                    source,
                    source_clock.into(),
                    target_clock.into(),
                )
                .map(|_| ())
        })
    });
    (result, sibling_use.span())
}

fn build_sibling_expression<'dae>(
    dae: &mut DaeConstruction<'dae>,
    sibling: SiblingCoordinate,
    real: ValueTypeId<'dae>,
    declaration_at: DaeProvenance,
    sibling_use: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    match sibling {
        SiblingCoordinate::State => {
            let state = dae.variables(|variables| {
                variables.state(
                    VarName::new("stateValue"),
                    rumoca_core::InstanceId::new(2),
                    real,
                    declaration_at,
                    VariableAttributes::default(),
                )
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(sibling_use)
                    .coordinate(CoordinateInput::State(state))
            })
        }
        SiblingCoordinate::Input => {
            let input = dae.variables(|variables| {
                variables.input(
                    VarName::new("externalValue"),
                    rumoca_core::InstanceId::new(2),
                    real,
                    InputVariability::Continuous,
                    declaration_at,
                    VariableAttributes::default(),
                )
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(sibling_use)
                    .coordinate(CoordinateInput::Input(input))
            })
        }
        SiblingCoordinate::Parameter => {
            let parameter = dae.variables(|variables| {
                variables.parameter(
                    VarName::new("parameterValue"),
                    rumoca_core::InstanceId::new(2),
                    real,
                    declaration_at,
                    VariableAttributes::default(),
                )
            })?;
            dae.expressions(|expressions| {
                expressions
                    .at(sibling_use)
                    .coordinate(CoordinateInput::Parameter(parameter))
            })
        }
    }
}
