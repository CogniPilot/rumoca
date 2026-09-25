use rumoca_core::{SourceMap, Span, TypeId, VarName};

use super::*;

/// Declarations of every fixture, in ordinal order: states `x`, `y` and
/// algebraics `a`, `b`, `c`, `d`. `d` starts at a nonzero guess.
const X: u32 = 0;
const Y: u32 = 1;
const A: u32 = 2;
const B: u32 = 3;
const C: u32 = 4;
const D: u32 = 5;

/// One signed term of a fixture residual: `(declaration, negated)`.
type Term = (u32, bool);

/// Build a DAE whose continuous residuals are the given signed-term sums, plus
/// `der(x) = a` and `der(y) = b` so both states are ordinary.
fn fixture(residuals: &[&[Term]]) -> dae::Dae {
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
        let coordinates = declare(dae, real, at)?;
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
) -> Result<Vec<dae::CoordinateInput<'dae>>, dae::DaeConstructionError> {
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
                at,
                attributes(),
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
                at,
                attributes(),
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
