use super::*;
use rumoca_core::{SourceMap, TypeId, VarName};

fn model() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "derivative-read-roles.mo",
        "Real x; discrete Real z; Real exact; Real opaque;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 47)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (real, record) = model.types(|types| {
            let real = types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )?;
            Ok((
                real,
                types.record(VarName::new("R"), [(VarName::new("value"), real)], at)?,
            ))
        })?;
        let (x, z, exact, opaque) = model.variables(|vars| {
            let attrs = dae::VariableAttributes::default();
            Ok((
                vars.state(VarName::new("x"), real, at, attrs.clone())?,
                vars.discrete_real(VarName::new("z"), real, at, attrs.clone())?,
                vars.algebraic(VarName::new("exact"), real, at, attrs.clone())?,
                vars.algebraic(VarName::new("opaque"), real, at, attrs)?,
            ))
        })?;
        let (residual, exact_row, opaque_row) = model.expressions(|expr| {
            let z = expr
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteReal(z))?;
            let derivative = expr
                .at(at)
                .coordinate(dae::CoordinateInput::Derivative(x))?;
            expr.at(at).record(record, [z])?;
            expr.at(at).record(record, [derivative])?;
            let exact = expr
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(exact))?;
            let opaque = expr
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(opaque))?;
            Ok((
                expr.at(at)
                    .binary(dae::BinaryOperator::Subtract, derivative, z)?,
                expr.at(at)
                    .binary(dae::BinaryOperator::Subtract, exact, derivative)?,
                expr.at(at)
                    .binary(dae::BinaryOperator::Subtract, opaque, derivative)?,
            ))
        })?;
        model.continuous(|rows| {
            for value in [residual, exact_row, opaque_row, opaque_row] {
                rows.equation(at, |row| row.residual(value))?;
            }
            Ok(())
        })
    })
    .unwrap()
}

pub(super) fn derivative(view: dae::DaeView<'_>) -> (dae::ExprId<'_>, dae::StateId<'_>) {
    (0..view.expression_count())
        .filter_map(|i| view.expression_id(i))
        .find_map(|id| match view.expression(id).unwrap().operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => {
                Some((id, state))
            }
            _ => None,
        })
        .unwrap()
}

fn definitions(view: dae::DaeView<'_>) -> SameTickDefinitions<'_> {
    let (_, x) = derivative(view);
    let dae::ContinuousOwnerView::Residual { equation, .. } =
        view.continuous_owners().next().unwrap()
    else {
        panic!("scalar source");
    };
    let plan = crate::CausalDiscretePlan::derive(view).unwrap();
    SameTickDefinitions::derive_with_derivative_sources(
        view,
        &plan,
        [MatchedDerivativeSource {
            state: x,
            state_scalar: 0,
            expression: equation.residual(),
            expression_scalar: 0,
            domain_point: None,
        }],
    )
    .unwrap()
}

#[test]
fn derivative_free_record_reads_keep_the_raw_query_and_demanded_records_refuse() {
    model().inspect(|view| {
        let definitions = definitions(view);
        let records = (0..view.expression_count())
            .filter_map(|i| view.expression_id(i))
            .filter(|&id| view.expression(id).unwrap().value_type().is_record())
            .collect::<Vec<_>>();
        let z = view
            .variables()
            .find(|(_, value)| value.name().as_str() == "z")
            .unwrap()
            .0;
        let reads = expression_read_seeds(
            view,
            records[0],
            &definitions.derivative_sources,
            Some(&definitions),
            &|_| false,
        )
        .unwrap()
        .variables;
        assert_eq!(reads, BTreeSet::from([z.index()]));
        assert!(matches!(
            expression_read_seeds(
                view,
                records[1],
                &definitions.derivative_sources,
                Some(&definitions),
                &|_| false,
            ),
            Err(SameTickOrderError::UnsupportedDerivativeValue { .. })
        ));
    });
}

#[test]
fn clock_partition_derivative_boundary_covers_direct_exact_and_opaque_reads() {
    model().inspect(|view| {
        let definitions = definitions(view);
        let z = view
            .variables()
            .find(|(_, value)| value.name().as_str() == "z")
            .unwrap()
            .0
            .index();
        for name in ["exact", "opaque"] {
            let variable = view
                .variables()
                .find(|(_, value)| value.name().as_str() == name)
                .unwrap()
                .0
                .index();
            let current = definitions
                .variable_read_closure(view, BTreeSet::from([variable]))
                .unwrap();
            let clocked = definitions
                .clock_partition_variable_read_closure(view, BTreeSet::from([variable]))
                .unwrap();
            assert!(current.contains(&z), "current {name} reads z");
            assert!(!clocked.contains(&z), "clocked {name} stops at derivative");
        }
        let derivative = derivative(view).0;
        assert!(
            definitions
                .read_closure(view, &[derivative], &[])
                .unwrap()
                .contains(&z)
        );
        assert!(
            definitions
                .clock_partition_read_closure(view, &[derivative], &[])
                .unwrap()
                .is_empty()
        );
    });
}
