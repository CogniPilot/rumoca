//! The `sample`/`hold` boundary between a clocked partition and continuous time.
//!
//! MLS §16.5.1 defines `sample(u)` as the left limit of `u`, so lowering it as an
//! identity read is faithful exactly when the sampling tick cannot move `u`. These
//! cases fix both sides of that contract.

use super::*;

/// Shape of the continuous path between a clocked `hold` output and the value
/// a clocked row samples back.
#[derive(Clone, Copy)]
enum SampledPath {
    /// `s = sample(hold(h))`: the sampled algebraic is recomputed by the same
    /// tick that writes `h`.
    Instantaneous,
    /// `der(x) = hold(h); w = x; s = sample(w)`: integration separates the tick
    /// from the sampled value.
    ThroughState,
}

/// `der(x) = drive; w = x`: a first-order plant driven by a continuous-time
/// value, whose sensed output `w` is separated from that value by integration.
fn integrating_plant_output<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    declaration: dae::DaeProvenance,
    owner: dae::DaeProvenance,
    drive: dae::AlgebraicId<'dae>,
) -> Result<dae::AlgebraicId<'dae>, dae::DaeConstructionError> {
    let state = model.variables(|variables| {
        variables.state(
            VarName::new("x"),
            real,
            declaration,
            dae::VariableAttributes::default(),
        )
    })?;
    let plant = model.expressions(|expressions| {
        let derivative = expressions
            .at(owner)
            .coordinate(dae::CoordinateInput::Derivative(state))?;
        let drive = expressions
            .at(owner)
            .coordinate(dae::CoordinateInput::Algebraic(drive))?;
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Subtract, derivative, drive)
    })?;
    model.continuous(|continuous| continuous.value_equation(owner, plant))?;
    let sensed = model.variables(|variables| {
        variables.algebraic(
            VarName::new("w"),
            real,
            declaration,
            dae::VariableAttributes::default(),
        )
    })?;
    let sensor = model.expressions(|expressions| {
        let lhs = expressions
            .at(owner)
            .coordinate(dae::CoordinateInput::Algebraic(sensed))?;
        let rhs = expressions
            .at(owner)
            .coordinate(dae::CoordinateInput::State(state))?;
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Subtract, lhs, rhs)
    })?;
    model.continuous(|continuous| continuous.value_equation(owner, sensor))?;
    Ok(sensed)
}

/// One clock, one `hold` output, and one clocked row that samples it back.
fn clocked_hold_sample_model(source: TestSource, path: SampledPath) -> dae::Dae {
    let declaration = source.at(0, 6);
    let clock_at = source.at(8, 24);
    let hold_at = source.at(26, 34);
    let sample_at = source.at(36, 48);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let held = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("h"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let sampled = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("s"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let carrier = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), held, hold_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), sampled, sample_at))?;

        // `hold(h)`: a continuous-time value written by the clocked partition.
        let hold_residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(hold_at)
                .coordinate(dae::CoordinateInput::Algebraic(carrier))?;
            let rhs = expressions
                .at(hold_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(held))?;
            expressions
                .at(hold_at)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(hold_at, hold_residual))?;

        // The value the clocked row samples back.
        let sampled_source = match path {
            SampledPath::Instantaneous => carrier,
            SampledPath::ThroughState => {
                integrating_plant_output(model, real, declaration, hold_at, carrier)?
            }
        };

        // `s = sample(<continuous value>)`.
        let sample_residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(sample_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(sampled))?;
            let rhs = expressions
                .at(sample_at)
                .coordinate(dae::CoordinateInput::Algebraic(sampled_source))?;
            expressions
                .at(sample_at)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(sample_at, |equation| equation.residual(sample_residual))
        })?;

        // `h = 2 * s`: closes the clocked feedback that MLS's left limit delays.
        let feedback = model.expressions(|expressions| {
            let lhs = expressions
                .at(sample_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(held))?;
            let gain = expressions
                .at(sample_at)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let source = expressions
                .at(sample_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(sampled))?;
            let scaled =
                expressions
                    .at(sample_at)
                    .binary(dae::BinaryOperator::Multiply, gain, source)?;
            expressions
                .at(sample_at)
                .binary(dae::BinaryOperator::Subtract, lhs, scaled)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(sample_at, |equation| equation.residual(feedback))
        })?;
        Ok(())
    })
    .unwrap()
}

/// MLS §16.5.1: `sample(u)` is the left limit of `u`. Lowering it as an identity
/// read of `u` is only that left limit when the sampling tick cannot move `u`.
/// A clocked row sampling a value its own tick recomputes has no checked
/// schedule, so it must fail here rather than be settled as an algebraic loop.
#[test]
fn clocked_row_sampling_its_own_tick_output_is_rejected() {
    let source = TestSource::new("Real h,s; Clock c=Clock(0.1); y=hold(h); s=sample(y);");

    let error = lower_solve_problem(&clocked_hold_sample_model(
        source,
        SampledPath::Instantaneous,
    ))
    .expect_err("a clocked row may not sample a value its own tick recomputes");

    let LowerError::Unsupported { reason, span } = error else {
        panic!("the clocked schedule contract is an unsupported-semantics rejection");
    };
    assert!(
        reason.contains("left limit"),
        "the rejection must name the MLS operator semantics it protects: {reason}"
    );
    assert_eq!(
        (span.start.0, span.end.0),
        (36, 48),
        "the rejection carries the sampling row's own span"
    );
}

/// The acceptance side of the same contract: a plant driven by `hold(..)` and
/// sampled back through its states keeps its existing owner. Integration, not
/// the tick, produces the sampled value, so the identity read *is* the left
/// limit.
#[test]
fn clocked_row_sampling_across_a_state_stays_accepted() {
    let source = TestSource::new("Real h,s; Clock c=Clock(0.1); der(x)=hold(h); s=sample(x);");

    let solve = lower_solve_problem(&clocked_hold_sample_model(
        source,
        SampledPath::ThroughState,
    ))
    .expect("a sampled plant output separated by a state stays expressible");

    assert_eq!(solve.clocks.periodic_event_schedules.len(), 1);
    assert!(
        solve
            .discrete
            .clock_owners
            .iter()
            .all(|owner| owner.is_some()),
        "every clocked row keeps its periodic activation owner"
    );
}

/// A clocked row reading a continuous-time value no clock writes is untouched
/// by the contract.
#[test]
fn clocked_row_sampling_an_independent_continuous_value_stays_accepted() {
    let source = TestSource::new("Real s; Clock c=Clock(0.1); y=time; s=sample(y);");
    let declaration = source.at(0, 6);
    let clock_at = source.at(8, 26);
    let owner = source.at(28, 47);
    let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10).unwrap();
    let model = dae::Dae::construct(source.map, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let sampled = model.variables(|variables| {
            variables.discrete_real(
                VarName::new("s"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let carrier = model.variables(|variables| {
            variables.algebraic(
                VarName::new("y"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let clock = model.clocks(|clocks| clocks.periodic(lattice, clock_at))?;
        model.clocks(|clocks| clocks.own_discrete_real(clock.into(), sampled, owner))?;
        let carrier_residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(carrier))?;
            let rhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Time)?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.continuous(|continuous| continuous.value_equation(owner, carrier_residual))?;
        let sample_residual = model.expressions(|expressions| {
            let lhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::DiscreteReal(sampled))?;
            let rhs = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(carrier))?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, lhs, rhs)
        })?;
        model.discrete(|discrete| {
            discrete.real_equation(owner, |equation| equation.residual(sample_residual))
        })?;
        Ok(())
    })
    .unwrap();

    let solve = lower_solve_problem(&model).expect("an independent sample stays expressible");

    assert_eq!(solve.discrete.clock_owners.len(), 1);
    assert!(solve.discrete.clock_owners[0].is_some());
}
