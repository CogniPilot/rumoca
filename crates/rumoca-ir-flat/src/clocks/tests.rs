use super::*;
use rumoca_core::{Expression, Literal, SourceId};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
}

fn provenance(source_span: Span) -> ProvenanceSpan {
    source_span
        .require_provenance("Flat clock test")
        .expect("test span has provenance")
}

fn rational(num: i64, den: i64) -> ClockRational {
    ClockRational::new(num, den).expect("test rational must reduce")
}

fn named_span(name: &str, start: usize) -> Span {
    Span::from_offsets(SourceId::from_source_name(name), start, start + 1)
}

fn equation(source_span: Span) -> Equation {
    Equation::new(
        Expression::Literal {
            value: Literal::Integer(0),
            span: source_span,
        },
        source_span,
        crate::EquationOrigin::ComponentEquation {
            component: "clocked".into(),
        },
    )
}

#[test]
fn periodic_base_clock_stores_the_exact_rational_interval() {
    let clock = BaseClock::periodic(0.1, span()).expect("positive periodic clock");
    let interval = clock
        .base_interval()
        .expect("periodic clock has an interval");

    assert_eq!(interval.exact_period(), Ok(rational(1, 10)));
    assert_eq!(interval.seconds(), 0.1);
    assert_eq!(
        clock.lattice().map(ClockLattice::period),
        Ok(rational(1, 10))
    );
}

#[test]
fn interval_counter_base_clock_matches_the_decimal_form() {
    // MLS §16.3: Clock(1, 10) and Clock(0.1) name the same clock.
    let counted = BaseClock::rational(1, 10, span()).expect("exact rational clock");
    let decimal = BaseClock::periodic(0.1, span()).expect("positive periodic clock");

    assert!(
        counted
            .lattice()
            .expect("counted lattice")
            .is_same_clock(decimal.lattice().expect("decimal lattice"))
    );
    assert!(matches!(
        counted.kind(),
        ClockKind::Rational {
            interval_counter: 1,
            resolution: 10
        }
    ));
}

#[test]
fn inferred_clock_has_no_lattice() {
    let clock = BaseClock::inferred(span());
    let error = clock
        .lattice()
        .expect_err("an inferred clock has no period");

    assert_eq!(
        error.kind,
        ClockLatticeErrorKind::NotRationallyRepresentable
    );
    assert_eq!(error.span, span());
}

#[test]
fn super_sample_then_sub_sample_reproduces_the_base_clock_exactly() {
    let base = BaseClock::periodic(0.007, span())
        .expect("positive periodic clock")
        .lattice()
        .expect("exact base lattice");
    let up = SubClock::super_sample(3, provenance(span()))
        .expect("positive super-sample factor")
        .derive(base)
        .expect("exact super sample");
    let down = SubClock::sub_sample(3, provenance(span()))
        .expect("positive sub-sample factor")
        .derive(up)
        .expect("exact sub sample");

    assert!(down.is_same_clock(base));
    assert_eq!(down.period_seconds(), base.period_seconds());
    assert_ne!(base.period_seconds() / 3.0 * 3.0, base.period_seconds());
}

#[test]
fn sub_clock_shift_and_back_shift_cancel_exactly() {
    let base = BaseClock::periodic(0.1, span())
        .expect("positive periodic clock")
        .lattice()
        .expect("exact base lattice");
    let shifted = SubClock::shift_sample(1, 3, provenance(span()))
        .expect("nonnegative shift with positive resolution")
        .derive(base)
        .expect("exact shift sample");
    assert_eq!(shifted.phase(), rational(1, 30));

    let restored = SubClock::back_sample(1, 3, provenance(span()))
        .expect("nonnegative back shift with positive resolution")
        .derive(shifted)
        .expect("exact back sample");
    assert!(restored.is_same_clock(base));
}

#[test]
fn no_clock_sub_partitions_have_no_lattice() {
    let sub_clock = SubClock::no_clock(provenance(span()));
    let base = BaseClock::periodic(0.1, span())
        .expect("positive periodic clock")
        .lattice()
        .expect("exact base lattice");

    let error = sub_clock
        .derive(base)
        .expect_err("noClock() has no periodic lattice");
    assert_eq!(
        error.kind,
        ClockLatticeErrorKind::NotRationallyRepresentable
    );
}

#[test]
fn overflowing_sub_sample_reports_a_spanned_error() {
    let base = ClockLattice::new(
        ClockRational::integer(i128::MAX / 2 + 1),
        ClockRational::ZERO,
    )
    .expect("exact rational clock");

    let error = SubClock::sub_sample(2, provenance(span()))
        .expect("positive sub-sample factor")
        .derive(base)
        .expect_err("sub-sampling beyond i128 must not wrap");

    assert_eq!(error.kind, ClockLatticeErrorKind::IntegerOverflow);
    assert_eq!(error.span, span());
}

#[test]
fn non_positive_interval_counter_is_rejected() {
    let error = BaseClock::rational(1, 0, span()).expect_err("resolution must be positive");
    assert_eq!(error.kind, ClockLatticeErrorKind::NonPositiveFactor);
    assert_eq!(error.span, span());
}

#[test]
fn zero_interval_counter_keeps_the_inferred_period_form() {
    let clock = BaseClock::rational(0, 10, span()).expect("zero requests clock inference");
    assert!(matches!(
        clock.kind(),
        ClockKind::Rational {
            interval_counter: 0,
            resolution: 10
        }
    ));
    let error = clock
        .lattice()
        .expect_err("the period remains unknown until clock inference");
    assert_eq!(
        error.kind,
        ClockLatticeErrorKind::NotRationallyRepresentable
    );
    assert_eq!(error.span, span());
}

#[test]
fn periodic_clock_rejects_invalid_intervals_at_the_owner_span() {
    for (interval, expected) in [
        (0.0, ClockLatticeErrorKind::NonPositivePeriod),
        (-0.25, ClockLatticeErrorKind::NonPositivePeriod),
        (f64::NAN, ClockLatticeErrorKind::NonFiniteSeconds),
        (f64::INFINITY, ClockLatticeErrorKind::NonFiniteSeconds),
    ] {
        let error =
            BaseClock::periodic(interval, span()).expect_err("invalid period must be rejected");
        assert_eq!(error.kind, expected);
        assert_eq!(error.span, span());
    }
}

#[test]
fn base_clock_wire_decodes_through_checked_construction() {
    let valid = BaseClock::periodic(0.125, span()).expect("positive periodic clock");
    let encoded = serde_json::to_string(&valid).expect("clock serializes");
    let decoded: BaseClock = serde_json::from_str(&encoded).expect("clock decodes");
    assert_eq!(
        decoded.lattice().map(ClockLattice::period),
        valid.lattice().map(ClockLattice::period)
    );
    assert_eq!(decoded.source_span(), span());

    let invalid = encoded.replace("0.125", "-0.125");
    let error = serde_json::from_str::<BaseClock>(&invalid)
        .expect_err("wire decoding must reject a non-positive interval");
    assert!(error.to_string().contains("strictly positive"));
}

#[test]
fn sub_clock_rejects_invalid_arguments_at_the_operation_span() {
    let operation = Span::from_offsets(SourceId::from_source_name("sub_clock_args.mo"), 8, 19);
    for error in [
        SubClock::sub_sample(-1, provenance(operation)).expect_err("factor must be nonnegative"),
        SubClock::super_sample(-1, provenance(operation)).expect_err("factor must be nonnegative"),
        SubClock::shift_sample(-1, 1, provenance(operation))
            .expect_err("counter must be nonnegative"),
        SubClock::shift_sample(1, 0, provenance(operation))
            .expect_err("resolution must be positive"),
        SubClock::back_sample(-1, 1, provenance(operation))
            .expect_err("counter must be nonnegative"),
        SubClock::back_sample(1, 0, provenance(operation))
            .expect_err("resolution must be positive"),
    ] {
        assert_eq!(error.kind, ClockLatticeErrorKind::NonPositiveFactor);
        assert_eq!(error.span, operation);
    }
}

#[test]
fn inferred_zero_factor_is_valid_but_has_no_final_lattice() {
    let operation = Span::from_offsets(SourceId::from_source_name("inferred_factor.mo"), 12, 13);
    let base = BaseClock::periodic(0.1, span())
        .expect("positive base period")
        .lattice()
        .expect("exact base lattice");
    let clock =
        SubClock::sub_sample(0, provenance(operation)).expect("zero requests factor inference");

    let error = clock
        .derive(base)
        .expect_err("an uninferred factor is not yet computable");
    assert_eq!(
        error.kind,
        ClockLatticeErrorKind::NotRationallyRepresentable
    );
    assert_eq!(error.span, operation);
}

#[test]
fn composed_sub_clock_keeps_source_order_and_operation_provenance() {
    let base = BaseClock::periodic(0.12, span())
        .expect("positive base period")
        .lattice()
        .expect("exact base lattice");
    let shift_span = Span::from_offsets(SourceId::from_source_name("composed_clock.mo"), 3, 12);
    let sample_span = Span::from_offsets(SourceId::from_source_name("composed_clock.mo"), 15, 24);
    let clock = SubClock::identity(provenance(span()))
        .then_shift_sample(1, 3, provenance(shift_span))
        .expect("valid shift")
        .then_sub_sample(2, provenance(sample_span))
        .expect("valid sub-sample");

    let derived = clock
        .derive(base)
        .expect("composition has an exact lattice");
    assert_eq!(derived.period(), rational(6, 25));
    assert_eq!(derived.phase(), rational(1, 25));

    let failing = clock
        .then_back_sample(2, 1, provenance(sample_span))
        .expect("arguments are locally valid")
        .derive(base)
        .expect_err("back shift would precede the base clock");
    assert_eq!(
        failing.kind,
        ClockLatticeErrorKind::ClockStartsBeforeBaseClock
    );
    assert_eq!(failing.span, sample_span);
}

#[test]
fn sub_clock_wire_decodes_through_checked_operations() {
    let clock = SubClock::identity(provenance(span()))
        .then_super_sample(2, provenance(span()))
        .expect("positive factor")
        .then_shift_sample(1, 4, provenance(span()))
        .expect("valid shift");
    let encoded = serde_json::to_string(&clock).expect("sub-clock serializes");
    let decoded: SubClock = serde_json::from_str(&encoded).expect("sub-clock decodes");
    let base = BaseClock::periodic(0.2, span())
        .expect("positive period")
        .lattice()
        .expect("exact base lattice");
    assert_eq!(
        decoded.derive(base).map(ClockLattice::period),
        clock.derive(base).map(ClockLattice::period)
    );

    let invalid = encoded.replace("\"resolution\":4", "\"resolution\":0");
    let error = serde_json::from_str::<SubClock>(&invalid)
        .expect_err("wire reconstruction must reject invalid resolution");
    assert!(error.to_string().contains("strictly positive"));

    let mut missing_operation_provenance =
        serde_json::to_value(&clock).expect("sub-clock serializes to a value");
    missing_operation_provenance["operations"][0]["span"] =
        serde_json::to_value(Span::DUMMY).expect("dummy span serializes");
    let error = serde_json::from_value::<SubClock>(missing_operation_provenance)
        .expect_err("wire reconstruction must reject missing operation provenance");
    assert!(
        error
            .to_string()
            .contains("missing source provenance for Flat sub-clock operation")
    );

    let mut missing_owner_provenance =
        serde_json::to_value(&clock).expect("sub-clock serializes to a value");
    missing_owner_provenance["source_span"] =
        serde_json::to_value(Span::DUMMY).expect("dummy span serializes");
    let error = serde_json::from_value::<SubClock>(missing_owner_provenance)
        .expect_err("wire reconstruction must reject missing owner provenance");
    assert!(
        error
            .to_string()
            .contains("missing source provenance for Flat sub-clock owner")
    );
}

#[test]
fn base_clock_partition_owns_checked_sub_clock_children() {
    let owner = named_span("clock_partition.mo", 1);
    let base_variable = named_span("clock_partition.mo", 3);
    let sub_owner = named_span("clock_partition.mo", 5);
    let sub_variable = named_span("clock_partition.mo", 7);
    let equation_span = named_span("clock_partition.mo", 9);
    let discretized_span = named_span("clock_partition.mo", 11);
    let base_clock = BaseClock::periodic(0.1, owner).expect("positive base clock");
    let mut partition = BaseClockPartition::construct(4, base_clock, provenance(owner));
    partition
        .add_variable(VarName::new("base_only"), provenance(base_variable))
        .expect("unique base variable");

    let sub_clock =
        SubClock::sub_sample(2, provenance(sub_owner)).expect("positive sub-sample factor");
    let mut sub_partition = SubClockPartition::construct(2, sub_clock, provenance(sub_owner));
    sub_partition
        .add_variable(VarName::new("on_sub_clock"), provenance(sub_variable))
        .expect("unique sub-clock variable");
    sub_partition
        .add_equation(equation(equation_span))
        .expect("equation has provenance");
    partition
        .add_sub_partition(sub_partition)
        .expect("unique sub-clock owner");
    partition.mark_discretized(provenance(discretized_span));

    assert_eq!(partition.id(), 4);
    assert_eq!(partition.source_span(), owner);
    assert_eq!(partition.variables().count(), 2);
    assert_eq!(
        partition.variable_span(&VarName::new("on_sub_clock")),
        Some(sub_variable)
    );
    assert_eq!(partition.sub_partitions().len(), 1);
    assert_eq!(partition.sub_partitions()[0].equations().len(), 1);
    assert!(partition.is_discretized());
    assert_eq!(partition.discretized_span(), Some(discretized_span));
}

#[test]
fn clock_partition_rejects_duplicate_local_and_sibling_ownership() {
    let owner = named_span("clock_partition_duplicates.mo", 1);
    let first = named_span("clock_partition_duplicates.mo", 3);
    let duplicate = named_span("clock_partition_duplicates.mo", 5);
    let base_clock = BaseClock::inferred(owner);
    let mut partition = BaseClockPartition::construct(0, base_clock, provenance(owner));
    let name = VarName::new("x");
    partition
        .add_variable(name.clone(), provenance(first))
        .expect("first variable occurrence");
    let error = partition
        .add_variable(name.clone(), provenance(duplicate))
        .expect_err("duplicate variable must fail");
    assert!(matches!(
        error,
        ClockPartitionError::DuplicateVariable {
            first_span,
            duplicate_span,
            ..
        } if first_span == first && duplicate_span == duplicate
    ));

    let mut first_sub =
        SubClockPartition::construct(1, SubClock::identity(provenance(first)), provenance(first));
    first_sub
        .add_variable(name.clone(), provenance(first))
        .expect("first sub-clock ownership");
    partition
        .add_sub_partition(first_sub)
        .expect("first sub-clock partition");

    let mut sibling = SubClockPartition::construct(
        2,
        SubClock::identity(provenance(duplicate)),
        provenance(duplicate),
    );
    sibling
        .add_variable(name, provenance(duplicate))
        .expect("locally unique sibling variable");
    let error = partition
        .add_sub_partition(sibling)
        .expect_err("one variable cannot belong to sibling sub-clocks");
    assert!(matches!(
        error,
        ClockPartitionError::VariableInMultipleSubPartitions {
            first_span,
            duplicate_span,
            ..
        } if first_span == first && duplicate_span == duplicate
    ));

    let duplicate_id = SubClockPartition::construct(
        1,
        SubClock::identity(provenance(duplicate)),
        provenance(duplicate),
    );
    let error = partition
        .add_sub_partition(duplicate_id)
        .expect_err("sub-clock IDs must be unique");
    assert!(matches!(
        error,
        ClockPartitionError::DuplicateSubPartition {
            id: 1,
            first_span,
            duplicate_span,
        } if first_span == first && duplicate_span == duplicate
    ));
}

#[test]
fn base_clock_partition_wire_reconstructs_the_checked_aggregate() {
    let owner = named_span("clock_partition_wire.mo", 1);
    let variable_span = named_span("clock_partition_wire.mo", 3);
    let sub_owner = named_span("clock_partition_wire.mo", 5);
    let equation_span = named_span("clock_partition_wire.mo", 7);
    let mut partition =
        BaseClockPartition::construct(8, BaseClock::inferred(owner), provenance(owner));
    partition
        .add_variable(VarName::new("x"), provenance(variable_span))
        .expect("unique variable");
    partition
        .add_equation(equation(equation_span))
        .expect("equation has provenance");
    partition
        .add_sub_partition(SubClockPartition::construct(
            3,
            SubClock::identity(provenance(sub_owner)),
            provenance(sub_owner),
        ))
        .expect("unique sub-clock");

    let encoded = serde_json::to_value(&partition).expect("partition serializes");
    let decoded: BaseClockPartition =
        serde_json::from_value(encoded.clone()).expect("partition reconstructs");
    assert_eq!(decoded.id(), 8);
    assert_eq!(decoded.source_span(), owner);
    assert_eq!(
        decoded.variable_span(&VarName::new("x")),
        Some(variable_span)
    );
    assert_eq!(decoded.equations().len(), 1);
    assert_eq!(decoded.sub_partitions()[0].id(), 3);

    let mut missing_owner = encoded.clone();
    missing_owner["source_span"] =
        serde_json::to_value(Span::DUMMY).expect("dummy span serializes");
    let error = serde_json::from_value::<BaseClockPartition>(missing_owner)
        .expect_err("missing aggregate provenance must fail");
    assert!(
        error
            .to_string()
            .contains("missing source provenance for Flat base-clock partition owner")
    );

    let mut duplicate_sub_id = encoded.clone();
    let first_sub = duplicate_sub_id["sub_partitions"][0].clone();
    duplicate_sub_id["sub_partitions"]
        .as_array_mut()
        .expect("sub-partitions are an array")
        .push(first_sub);
    let error = serde_json::from_value::<BaseClockPartition>(duplicate_sub_id)
        .expect_err("duplicate wire sub-clock IDs must fail");
    assert!(
        error
            .to_string()
            .contains("duplicate sub-clock partition id")
    );

    let mut missing_equation_provenance = encoded.clone();
    missing_equation_provenance["equations"][0]["span"] =
        serde_json::to_value(Span::DUMMY).expect("dummy span serializes");
    let error = serde_json::from_value::<BaseClockPartition>(missing_equation_provenance)
        .expect_err("equations without provenance must fail");
    assert!(
        error
            .to_string()
            .contains("missing source provenance for Flat base-clock partition equation")
    );

    let mut legacy_shape = encoded;
    legacy_shape["is_discretized"] = serde_json::Value::Bool(false);
    let error = serde_json::from_value::<BaseClockPartition>(legacy_shape)
        .expect_err("legacy public-field wire shape must fail");
    assert!(error.to_string().contains("unknown field"));
}
