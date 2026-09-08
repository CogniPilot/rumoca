//! Direct coverage for the same-tick discrete-Real orientation and order.
//!
//! Every fixture is a checked DAE built here, so the assertions are about
//! [`CausalDiscretePlan`] itself and not about a downstream projection that
//! happens to consume it. The three behaviours this module owns are:
//!
//! * orientation — which side of `target - value = 0` is the definition, and
//!   which equations are left to a consumer;
//! * ordering — a definition that reads another same-tick discrete Real must
//!   be emitted after that producer, *independently of the order the producer
//!   was declared in*;
//! * rejection — a coupled residual and a same-tick cycle both fail closed at
//!   the offending equation's own provenance.

use rumoca_core::{InstanceId, SourceMap, Span, TypeId, VarName};

use super::*;

/// Exact provenance for `needle` inside `text`.
fn at(source: rumoca_core::SourceId, text: &str, needle: &str) -> dae::DaeProvenance {
    let start = text.find(needle).expect("fixture text contains snippet");
    dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
        .expect("fixture provenance is exact")
}

/// Which equation is declared first in the checked DAE.
///
/// The plan must produce the same order for both, which is precisely the
/// property `sampled_algorithm_clock_ownership_is_independent_of_producer_order`
/// names but never checks.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ProducerOrder {
    /// `consumer` is declared before its producer.
    ConsumerFirst,
    /// `producer` is declared before its consumer (the mirrored fixture).
    ProducerFirst,
}

/// The rejection span, or a panic naming what was produced instead.
///
/// `Result::expect_err` is unavailable here: [`CausalDiscretePlan`] is an
/// opaque evidence handle and deliberately does not implement `Debug`.
fn rejection_span(result: Result<CausalDiscretePlan<'_>, CausalDiscreteError>) -> Span {
    match result {
        Ok(plan) => panic!(
            "expected a rejection, got an order of {} targets",
            plan.discrete_real_order().len()
        ),
        Err(CausalDiscreteError::NonComputable { span }) => span,
    }
}

/// Names of the oriented targets, in scheduled order.
fn ordered_names<'dae>(view: dae::DaeView<'dae>, plan: &CausalDiscretePlan<'dae>) -> Vec<String> {
    plan.discrete_real_order()
        .iter()
        .map(|target| {
            view.variable(dae::VariableId::from(*target))
                .expect("scheduled target is branded to this DAE")
                .name()
                .to_string()
        })
        .collect()
}

fn define_real_rows<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    rows: [(dae::DaeProvenance, dae::ExprId<'dae>, dae::ExprId<'dae>); 2],
) -> Result<(), dae::DaeConstructionError> {
    dae.discrete(|discrete| {
        for (provenance, lhs, rhs) in rows {
            discrete.real_equation(provenance, |equation| {
                equation.equal(lhs, rhs)?;
                Ok(())
            })?;
        }
        Ok(())
    })
}

/// `consumer = producer + 1.0;` and `producer = 2.0;`, one declared before the
/// other according to `order`.
///
/// `consumer` is declared as the *first* variable on purpose: the scheduler
/// walks candidate targets in ascending variable index, so without a
/// dependency edge from the consumer's value to its producer the emitted order
/// would be `[consumer, producer]`. Only a real same-tick read flips it.
fn producer_consumer_model(order: ProducerOrder) -> dae::Dae {
    let text = "discrete Real consumer; discrete Real producer; \
                consumer = producer + 1.0; producer = 2.0;";
    let mut sources = SourceMap::new();
    let source = sources.add("producer-consumer.mo", text);
    let consumer_declaration = at(source, text, "discrete Real consumer");
    let producer_declaration = at(source, text, "discrete Real producer");
    let consumer_equation = at(source, text, "consumer = producer + 1.0");
    let producer_equation = at(source, text, "producer = 2.0");
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                consumer_declaration,
            )
        })?;
        let (consumer, producer) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("consumer"),
                    InstanceId::new(1),
                    real,
                    consumer_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("producer"),
                    InstanceId::new(2),
                    real,
                    producer_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (consumer_lhs, consumer_rhs, producer_lhs, producer_rhs) =
            dae.expressions(|expressions| {
                let producer_read = expressions
                    .at(consumer_equation)
                    .coordinate(dae::CoordinateInput::DiscreteReal(producer))?;
                let one = expressions
                    .at(consumer_equation)
                    .literal(dae::DaeLiteral::Real(1.0))?;
                Ok((
                    expressions
                        .at(consumer_equation)
                        .coordinate(dae::CoordinateInput::DiscreteReal(consumer))?,
                    expressions.at(consumer_equation).binary(
                        dae::BinaryOperator::Add,
                        producer_read,
                        one,
                    )?,
                    expressions
                        .at(producer_equation)
                        .coordinate(dae::CoordinateInput::DiscreteReal(producer))?,
                    expressions
                        .at(producer_equation)
                        .literal(dae::DaeLiteral::Real(2.0))?,
                ))
            })?;
        let rows = match order {
            ProducerOrder::ConsumerFirst => [
                (consumer_equation, consumer_lhs, consumer_rhs),
                (producer_equation, producer_lhs, producer_rhs),
            ],
            ProducerOrder::ProducerFirst => [
                (producer_equation, producer_lhs, producer_rhs),
                (consumer_equation, consumer_lhs, consumer_rhs),
            ],
        };
        define_real_rows(dae, rows)
    })
    .expect("checked producer/consumer fixture constructs")
}

/// `a = b + 1.0;` and `b = a + 1.0;` — each row orients to exactly one target,
/// so the failure is the *ordering* cycle rather than an ambiguous residual.
fn same_tick_cycle_model() -> dae::Dae {
    let text = "discrete Real a; discrete Real b; a = b + 1.0; b = a + 1.0;";
    let mut sources = SourceMap::new();
    let source = sources.add("same-tick-cycle.mo", text);
    let a_declaration = at(source, text, "discrete Real a");
    let b_declaration = at(source, text, "discrete Real b");
    let a_equation = at(source, text, "a = b + 1.0");
    let b_equation = at(source, text, "b = a + 1.0");
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                a_declaration,
            )
        })?;
        let (a, b) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("a"),
                    InstanceId::new(1),
                    real,
                    a_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("b"),
                    InstanceId::new(2),
                    real,
                    b_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (a_lhs, a_rhs, b_lhs, b_rhs) = dae.expressions(|expressions| {
            let b_read = expressions
                .at(a_equation)
                .coordinate(dae::CoordinateInput::DiscreteReal(b))?;
            let a_one = expressions
                .at(a_equation)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let a_read = expressions
                .at(b_equation)
                .coordinate(dae::CoordinateInput::DiscreteReal(a))?;
            let b_one = expressions
                .at(b_equation)
                .literal(dae::DaeLiteral::Real(1.0))?;
            Ok((
                expressions
                    .at(a_equation)
                    .coordinate(dae::CoordinateInput::DiscreteReal(a))?,
                expressions
                    .at(a_equation)
                    .binary(dae::BinaryOperator::Add, b_read, a_one)?,
                expressions
                    .at(b_equation)
                    .coordinate(dae::CoordinateInput::DiscreteReal(b))?,
                expressions
                    .at(b_equation)
                    .binary(dae::BinaryOperator::Add, a_read, b_one)?,
            ))
        })?;
        dae.discrete(|discrete| {
            discrete.real_equation(a_equation, |equation| {
                equation.equal(a_lhs, a_rhs)?;
                Ok(())
            })?;
            discrete.real_equation(b_equation, |equation| {
                equation.equal(b_lhs, b_rhs)?;
                Ok(())
            })?;
            Ok(())
        })
    })
    .expect("checked same-tick cycle fixture constructs")
}

/// `z + w = 1.0;` — a coupled row with no whole-coordinate side, so no
/// orientation exists at all.
fn coupled_residual_model() -> dae::Dae {
    let text = "discrete Real z; discrete Real w; z + w = 1.0;";
    let mut sources = SourceMap::new();
    let source = sources.add("coupled-residual.mo", text);
    let z_declaration = at(source, text, "discrete Real z");
    let w_declaration = at(source, text, "discrete Real w");
    let equation = at(source, text, "z + w = 1.0");
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                z_declaration,
            )
        })?;
        let (z, w) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    InstanceId::new(1),
                    real,
                    z_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("w"),
                    InstanceId::new(2),
                    real,
                    w_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            let z_read = expressions
                .at(equation)
                .coordinate(dae::CoordinateInput::DiscreteReal(z))?;
            let w_read = expressions
                .at(equation)
                .coordinate(dae::CoordinateInput::DiscreteReal(w))?;
            Ok((
                expressions
                    .at(equation)
                    .binary(dae::BinaryOperator::Add, z_read, w_read)?,
                expressions
                    .at(equation)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        dae.discrete(|discrete| {
            discrete.real_equation(equation, |eq| {
                eq.equal(lhs, rhs)?;
                Ok(())
            })?;
            Ok(())
        })
    })
    .expect("checked coupled residual fixture constructs")
}

#[test]
fn a_definition_that_reads_a_discrete_real_is_ordered_after_its_producer() {
    producer_consumer_model(ProducerOrder::ConsumerFirst).inspect(|view| {
        let plan = CausalDiscretePlan::derive(view).expect("both rows orient");

        assert_eq!(
            ordered_names(view, &plan),
            ["producer", "consumer"],
            "a same-tick read must schedule the producer first even though \
             `consumer` is the lower-indexed variable"
        );
    });
}

#[test]
fn the_scheduled_order_is_independent_of_the_producer_declaration_order() {
    let consumer_first = producer_consumer_model(ProducerOrder::ConsumerFirst)
        .inspect(|view| ordered_names(view, &CausalDiscretePlan::derive(view).expect("orients")));
    let producer_first = producer_consumer_model(ProducerOrder::ProducerFirst)
        .inspect(|view| ordered_names(view, &CausalDiscretePlan::derive(view).expect("orients")));

    assert_eq!(consumer_first, ["producer", "consumer"]);
    assert_eq!(
        consumer_first, producer_first,
        "mirroring the two equation declarations must not change the tick order"
    );
}

#[test]
fn each_row_is_oriented_onto_the_target_its_own_equation_defines() {
    for (order, consumer_row, producer_row) in [
        (ProducerOrder::ConsumerFirst, 0, 1),
        (ProducerOrder::ProducerFirst, 1, 0),
    ] {
        producer_consumer_model(order).inspect(|view| {
            let plan = CausalDiscretePlan::derive(view).expect("both rows orient");
            let name = |row: usize| {
                let definition = plan
                    .discrete_real_definition(row)
                    .expect("every non-consumed row is oriented");
                assert_eq!(
                    definition.equation() as usize,
                    row,
                    "a definition carries the ordinal of the equation it came from"
                );
                view.variable(dae::VariableId::from(definition.target()))
                    .expect("target is branded to this DAE")
                    .name()
                    .to_string()
            };

            assert_eq!(name(consumer_row), "consumer", "{order:?}");
            assert_eq!(name(producer_row), "producer", "{order:?}");
        });
    }
}

#[test]
fn a_same_tick_cycle_is_rejected_at_the_blocked_equation() {
    same_tick_cycle_model().inspect(|view| {
        let span = rejection_span(CausalDiscretePlan::derive(view));

        let blocked = view
            .discrete_real_equation(0)
            .expect("the first row resolves")
            .provenance()
            .span();
        assert_eq!(
            span, blocked,
            "rejection points at the first blocked definition, not at a synthetic span"
        );
    });
}

#[test]
fn a_coupled_residual_has_no_orientation_and_is_rejected() {
    coupled_residual_model().inspect(|view| {
        let span = rejection_span(CausalDiscretePlan::derive(view));

        assert_eq!(
            span,
            view.discrete_real_equation(0)
                .expect("the only row resolves")
                .provenance()
                .span()
        );
    });
}

#[test]
fn an_oriented_row_is_never_also_consumed_by_causal_elimination() {
    producer_consumer_model(ProducerOrder::ConsumerFirst).inspect(|view| {
        let plan = CausalDiscretePlan::derive(view).expect("both rows orient");
        let causal = plan.causal_definitions();

        for index in 0..view.discrete_real_equation_count() {
            assert_ne!(
                plan.discrete_real_definition(index).is_some(),
                causal.consumes_discrete_real_equation(index),
                "row {index} must be oriented xor consumed"
            );
        }
    });
}
