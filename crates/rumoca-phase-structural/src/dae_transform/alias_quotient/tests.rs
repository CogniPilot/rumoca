use rumoca_core::{SourceMap, Span, TypeId, VarName};

use super::*;

/// Declarations of every fixture, in ordinal order: states `x`, `y` and
/// algebraics `a`, `b`, `c`, `d`, `e`. `d` and `e` start at a nonzero guess.
const X: u32 = 0;
const Y: u32 = 1;
const A: u32 = 2;
const B: u32 = 3;
const C: u32 = 4;
const D: u32 = 5;
const E: u32 = 6;

/// One signed term of a fixture residual: `(declaration, negated)`.
type Term = (u32, bool);

/// Build a DAE whose continuous residuals are the given signed-term sums, plus
/// `der(x) = a` and `der(y) = b` so both states are ordinary.
fn fixture(residuals: &[&[Term]]) -> dae::Dae {
    fixture_with(residuals, false)
}

/// [`fixture`], with `a` and `c` declared as generated index-reduction
/// coordinates when `formal` holds, as formal-derivative construction does.
fn fixture_with(residuals: &[&[Term]], formal: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "Real x, y, a, b, c, d; equation aliases;";
    let source = sources.add("aliases.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let at = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let coordinates = declare(dae, real, at, formal)?;
        let rows = dae.expressions(|expressions| {
            let mut rows = residuals
                .iter()
                .map(|terms| signed_sum(expressions, &coordinates, terms, at))
                .collect::<Result<Vec<_>, _>>()?;
            for (state, rate) in [(X, A), (Y, B)] {
                rows.push(derivative_row(expressions, &coordinates, state, rate, at)?);
            }
            Ok(rows)
        })?;
        dae.continuous(|continuous| {
            for row in rows {
                continuous.equation(at, |equation| equation.residual(row))?;
            }
            Ok(())
        })
    })
    .unwrap()
}

/// The residual `der(state) - rate`.
fn derivative_row<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    coordinates: &[dae::CoordinateInput<'dae>],
    state: u32,
    rate: u32,
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let dae::CoordinateInput::State(state) = coordinates[state as usize] else {
        unreachable!("fixture states lead the declarations")
    };
    let derivative = expressions
        .at(at)
        .coordinate(dae::CoordinateInput::Derivative(state))?;
    let rate = expressions.at(at).coordinate(coordinates[rate as usize])?;
    expressions
        .at(at)
        .binary(dae::BinaryOperator::Subtract, derivative, rate)
}

fn declare<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    at: dae::DaeProvenance,
    formal: bool,
) -> Result<Vec<dae::CoordinateInput<'dae>>, dae::DaeConstructionError> {
    let (generated_at, generated) = if formal {
        (
            dae::DaeProvenance::generated(dae::DaeGeneration::IndexReduction, at.span())?,
            dae::VariableAttributes {
                origin: dae::VariableOrigin::Generated,
                ..Default::default()
            },
        )
    } else {
        (at, dae::VariableAttributes::default())
    };
    let seed =
        dae.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(0.5)))?;
    dae.variables(|variables| {
        let attributes = dae::VariableAttributes::default;
        Ok(vec![
            dae::CoordinateInput::State(variables.state(
                VarName::new("x"),
                real,
                at,
                attributes(),
            )?),
            dae::CoordinateInput::State(variables.state(
                VarName::new("y"),
                real,
                at,
                attributes(),
            )?),
            dae::CoordinateInput::Algebraic(variables.algebraic(
                VarName::new("a"),
                real,
                generated_at,
                generated.clone(),
            )?),
            dae::CoordinateInput::Algebraic(variables.algebraic(
                VarName::new("b"),
                real,
                at,
                attributes(),
            )?),
            dae::CoordinateInput::Algebraic(variables.algebraic(
                VarName::new("c"),
                real,
                generated_at,
                generated,
            )?),
            dae::CoordinateInput::Algebraic(variables.algebraic(
                VarName::new("d"),
                real,
                at,
                dae::VariableAttributes {
                    start: Some(seed),
                    ..attributes()
                },
            )?),
            dae::CoordinateInput::Algebraic(variables.algebraic(
                VarName::new("e"),
                real,
                at,
                dae::VariableAttributes {
                    start: Some(seed),
                    ..attributes()
                },
            )?),
        ])
    })
}

fn signed_sum<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    coordinates: &[dae::CoordinateInput<'dae>],
    terms: &[Term],
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let mut sum: Option<dae::ExprId<'dae>> = None;
    for &(declaration, negated) in terms {
        let value = expressions
            .at(at)
            .coordinate(coordinates[declaration as usize])?;
        sum = Some(match (sum, negated) {
            (None, false) => value,
            (None, true) => expressions
                .at(at)
                .unary(dae::UnaryOperator::Negate, value)?,
            (Some(sum), false) => {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, sum, value)?
            }
            (Some(sum), true) => {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, sum, value)?
            }
        });
    }
    Ok(sum.expect("fixture residual has a term"))
}

fn plan(residuals: &[&[Term]]) -> AliasPlan {
    fixture(residuals).inspect(derive_plan)
}

fn substitution(representative: u32, negated: bool) -> Option<AliasSubstitution> {
    Some(AliasSubstitution {
        representative,
        negated,
    })
}

#[test]
fn a_copy_and_a_negation_chain_compose_their_signs() {
    // a = -c and c + b = 0, i.e. c = -b: a = b.
    let plan = plan(&[&[(A, false), (C, false)], &[(C, false), (B, false)]]);
    assert_eq!(plan.substitutions[A as usize], None);
    assert_eq!(plan.substitutions[C as usize], substitution(A, true));
    assert_eq!(plan.substitutions[B as usize], substitution(A, false));
    let definitions = plan
        .definitions
        .iter()
        .map(|(owner, definition)| (*owner, definition.member))
        .collect::<Vec<_>>();
    assert_eq!(definitions, [(0, C), (1, B)]);
}

#[test]
fn a_state_is_the_representative_of_its_class() {
    let plan = plan(&[&[(C, false), (X, true)]]);
    assert_eq!(plan.substitutions[C as usize], substitution(X, false));
    assert_eq!(plan.substitutions[X as usize], None);
}

#[test]
fn a_second_state_keeps_its_role_and_its_edge() {
    // x = c and c = y: c reads x, and y stays a state tied by `x - y = 0`.
    let source = fixture(&[&[(X, false), (C, true)], &[(C, false), (Y, true)]]);
    let plan = source.inspect(derive_plan);
    assert_eq!(plan.substitutions[C as usize], substitution(X, false));
    assert_eq!(plan.substitutions[Y as usize], None);
    let quotient = quotient_aliases(&source).unwrap().expect("eligible class");
    quotient.inspect(|view| {
        let role = |ordinal: u32| {
            view.variable(view.variable_id(ordinal as usize).unwrap())
                .unwrap()
                .role()
        };
        assert_eq!(role(X), dae::VariableRole::State);
        assert_eq!(role(Y), dae::VariableRole::State);
        let reads = continuous_reads(view);
        assert_eq!(reads[0], BTreeSet::from([X, C]), "c keeps its definition");
        assert_eq!(reads[1], BTreeSet::from([X, Y]), "the state edge reads x");
        assert_eq!(reads[3], BTreeSet::from([Y, B]), "der(y) = b is unchanged");
    });
}

#[test]
fn a_nonzero_start_anchors_the_representative() {
    let plan = plan(&[&[(C, false), (D, true)]]);
    assert_eq!(plan.substitutions[C as usize], substitution(D, false));
}

#[test]
fn a_state_and_a_seeded_member_leave_the_class_unchanged() {
    let plan = plan(&[&[(X, false), (D, true)]]);
    assert!(plan.definitions.is_empty());
}

#[test]
fn a_cyclic_class_is_left_unchanged() {
    // a = c, c = b, b = -a: redundant and sign-inconsistent.
    let plan = plan(&[
        &[(A, false), (C, true)],
        &[(C, false), (B, true)],
        &[(B, false), (A, false)],
    ]);
    assert!(plan.definitions.is_empty());
}

#[test]
fn three_terms_are_not_an_alias() {
    let plan = plan(&[&[(A, false), (B, true), (C, true)]]);
    assert!(plan.definitions.is_empty());
}

#[test]
fn the_quotient_rewrites_reads_and_keeps_every_declaration() {
    // c = -a is an alias; d = c + x reads the eliminated member.
    let source = fixture(&[
        &[(C, false), (A, false)],
        &[(D, false), (C, true), (X, true)],
    ]);
    let quotient = quotient_aliases(&source)
        .unwrap()
        .expect("one class is eligible");
    source.inspect(|source| {
        quotient.inspect(|quotient| {
            assert_eq!(source.variable_count(), quotient.variable_count());
            for ((_, before), (_, after)) in source.variables().zip(quotient.variables()) {
                assert_eq!(before.name(), after.name());
                assert_eq!(before.role(), after.role());
            }
            assert_eq!(
                source.continuous_owner_count(),
                quotient.continuous_owner_count()
            );
            let reads = continuous_reads(quotient);
            assert_eq!(reads[0], BTreeSet::from([A, C]), "c keeps its definition");
            assert_eq!(reads[1], BTreeSet::from([X, A, D]), "d reads a, not c");
        })
    });
}

fn continuous_reads(view: dae::DaeView<'_>) -> Vec<BTreeSet<u32>> {
    view.continuous_owners()
        .map(|owner| {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                unreachable!("fixture owners are scalar residuals")
            };
            let mut reads = BTreeSet::new();
            dae::for_each_expression(view, equation.residual(), |_, node| {
                if let Some(variable) = node.variable_coordinate() {
                    reads.insert(variable.index());
                }
            });
            reads
        })
        .collect()
}

/// The unchanged-class records one quotient observes for `residuals`.
fn unchanged(residuals: &[&[Term]]) -> Vec<(Vec<u32>, AliasRefusal)> {
    let (result, report) = inspect_quotient_aliases(&fixture(residuals));
    assert!(result.unwrap().is_none(), "no class is quotiented");
    report
        .records
        .into_iter()
        .map(|record| match record {
            crate::ReductionRecord::AliasClassUnchanged {
                scope: QuotientScope::Source,
                members,
                reason,
            } => (members, reason),
            other => panic!("the quotient records only unchanged classes, got {other:?}"),
        })
        .collect()
}

#[test]
fn every_unquotiented_class_is_recorded_with_its_reason() {
    assert_eq!(
        unchanged(&[
            &[(A, false), (C, true)],
            &[(C, false), (B, true)],
            &[(B, false), (A, false)],
        ]),
        [(vec![A, B, C], AliasRefusal::Cycle)]
    );
    assert_eq!(
        unchanged(&[&[(D, false), (E, true)]]),
        [(vec![D, E], AliasRefusal::SeveralAnchors)]
    );
    assert_eq!(
        unchanged(&[&[(X, false), (D, true)]]),
        [(vec![X, D], AliasRefusal::AnchorIsNotState)]
    );
}

#[test]
fn an_inspected_quotient_rebuilds_and_records_no_accepted_class() {
    // a = c; the three-term row is no alias edge and reads the eliminated c.
    let (result, report) = inspect_quotient_aliases(&fixture(&[
        &[(A, false), (C, true)],
        &[(C, false), (B, false), (D, false)],
    ]));
    let quotient = match result {
        Ok(Some(quotient)) => quotient,
        Ok(None) => panic!("a = c is quotiented"),
        Err(error) => panic!("a = c is quotiented: {error}"),
    };
    assert_eq!(
        quotient.inspect(continuous_reads)[1],
        BTreeSet::from([A, B, D]),
        "the three-term row reads the representative a instead of c"
    );
    assert!(report.records.is_empty(), "{:?}", report.records);
}

fn formal_plan(residuals: &[&[Term]]) -> AliasPlan {
    fixture_with(residuals, true)
        .inspect(|view| derive_plan_observed(view, QuotientScope::FormalDerivatives, &mut ()))
}

#[test]
fn the_formal_application_composes_sign_chains_through_formal_coordinates() {
    // a = -c (formal to formal) and c + b = 0 (formal to source): b = a.
    let plan = formal_plan(&[&[(A, false), (C, false)], &[(C, false), (B, false)]]);
    assert_eq!(plan.substitutions[A as usize], None);
    assert_eq!(plan.substitutions[C as usize], substitution(A, true));
    assert_eq!(plan.substitutions[B as usize], substitution(A, false));
}

#[test]
fn the_formal_application_admits_no_edge_without_a_formal_endpoint() {
    // x = b joins a state and a source algebraic; only a = c is formal.
    let plan = formal_plan(&[&[(X, false), (B, true)], &[(A, false), (C, true)]]);
    assert_eq!(
        plan.substitutions[B as usize], None,
        "x = b stays a source edge"
    );
    assert_eq!(plan.substitutions[C as usize], substitution(A, false));
    assert_eq!(plan.definitions.len(), 1);
}

#[test]
fn the_formal_application_keeps_source_roles_and_names() {
    let source = fixture_with(
        &[&[(A, false), (C, false)], &[(C, false), (B, false)]],
        true,
    );
    let plan = source
        .inspect(|view| derive_plan_observed(view, QuotientScope::FormalDerivatives, &mut ()));
    let (quotient, _) =
        crate::dae_transform::reconstruction::rebuild_alias_quotient(&source, &plan, &[]).unwrap();
    source.inspect(|before| {
        quotient.inspect(|after| {
            for ((_, old), (_, new)) in before.variables().zip(after.variables()) {
                assert_eq!((old.name(), old.role()), (new.name(), new.role()));
            }
            let reads = continuous_reads(after);
            assert_eq!(
                reads[3],
                BTreeSet::from([Y, A]),
                "der(y) = b reads the representative a"
            );
        })
    });
}

#[test]
fn a_formal_scope_refusal_is_recorded_with_its_scope() {
    // a is a formal coordinate; d and e both carry a seed.
    let source = fixture_with(&[&[(A, false), (D, true)], &[(A, false), (E, true)]], true);
    let mut recorder = crate::dae_transform::observation::ReductionRecorder::default();
    let plan = source.inspect(|view| {
        derive_plan_observed(view, QuotientScope::FormalDerivatives, &mut recorder)
    });
    assert!(plan.is_empty());
    let records = recorder.finish(false).records;
    let [
        crate::ReductionRecord::AliasClassUnchanged {
            scope,
            members,
            reason,
        },
    ] = records.as_slice()
    else {
        panic!("one formal-scope class stays unchanged: {records:?}");
    };
    assert_eq!(*scope, QuotientScope::FormalDerivatives);
    assert_eq!(members, &[A, D, E]);
    assert_eq!(*reason, AliasRefusal::SeveralAnchors);
}

#[test]
fn a_retained_manifold_expression_is_replayed_onto_the_quotient() {
    // a = c is formal; the three-term row stands in for a retained manifold
    // constraint that reads the eliminated c.
    let source = fixture_with(
        &[
            &[(A, false), (C, true)],
            &[(C, false), (D, false), (E, false)],
        ],
        true,
    );
    let manifold = source.inspect(|view| {
        let Some(dae::ContinuousOwnerView::Residual { equation, .. }) =
            view.continuous_owners().nth(1)
        else {
            panic!("the second fixture owner is a scalar residual");
        };
        equation.residual().index()
    });
    let plan = source
        .inspect(|view| derive_plan_observed(view, QuotientScope::FormalDerivatives, &mut ()));
    assert_eq!(plan.substitutions[C as usize], substitution(A, false));
    let (quotient, replayed) =
        crate::dae_transform::reconstruction::rebuild_alias_quotient(&source, &plan, &[manifold])
            .unwrap();
    let [expression] = replayed.as_slice() else {
        panic!("one manifold expression is replayed: {replayed:?}");
    };
    quotient.inspect(|view| {
        let Some(id) = view.expression_id(*expression as usize) else {
            panic!("the replayed manifold expression resolves");
        };
        let mut reads = BTreeSet::new();
        dae::for_each_expression(view, id, |_, node| {
            if let Some(variable) = node.variable_coordinate() {
                reads.insert(variable.index());
            }
        });
        assert_eq!(
            reads,
            BTreeSet::from([A, D, E]),
            "the manifold reads the representative a instead of c"
        );
    });
}
