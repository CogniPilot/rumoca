use super::*;

/// `x = if i == k ...` inside `for i in 1:2` and a scalar `noEvent(x > 0)` guard.
fn binder_guard_fixture() -> (dae::Dae, dae::DaeProvenance, dae::DaeProvenance) {
    const TEXT: &str = "parameter Integer k; Real x; for i in 1:2 loop x = if i == k then 1 else 0; end for; x = if noEvent(x > 0) then 1 else 0;";
    let mut sources = SourceMap::new();
    let source = sources.add("binder_guards.mo", TEXT);
    let declaration = source_provenance(source, TEXT, "parameter Integer k");
    let family_owner = source_provenance(source, TEXT, "for i in 1:2 loop");
    let scalar_owner = source_provenance(source, TEXT, "x = if noEvent");

    let dae = dae::Dae::construct(sources, |model| {
        let (real, integer) = model.types(|types| {
            Ok((
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    declaration,
                )?,
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Integer),
                    declaration,
                )?,
            ))
        })?;
        let (parameter, state) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("k"),
                    integer,
                    declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.state(
                    VarName::new("x"),
                    real,
                    source_provenance(source, TEXT, "Real x"),
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let domain = model.domains(|domains| {
            domains.structured(
                rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        provenance: rumoca_core::IndexBinderProvenance::Generated,
                        id: 0,
                        display_name: "i".to_owned(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                family_owner,
            )
        })?;
        let binder = model.domains(|domains| domains.binder(domain, 0, family_owner))?;
        let (family_branch, scalar_branch) = model.expressions(|expressions| {
            Ok((
                binder_branch(expressions, family_owner, binder, parameter)?,
                state_branch(expressions, scalar_owner, state)?,
            ))
        })?;
        let residual = model.expressions(|expressions| {
            state_minus(expressions, family_owner, state, family_branch)
        })?;
        let state_residual = model.expressions(|expressions| {
            state_minus(expressions, scalar_owner, state, scalar_branch)
        })?;
        model.continuous(|continuous| {
            continuous.structured_family(
                family_owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(residual),
            )?;
            continuous.value_equation(scalar_owner, state_residual)
        })?;
        Ok(())
    })
    .expect("fixture DAE is valid");
    (dae, family_owner, scalar_owner)
}

/// `if i == k then 1 else 0`: a binder compared with a parameter.
fn binder_branch<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    owner: dae::DaeProvenance,
    binder: dae::DomainBinderId<'dae>,
    parameter: dae::ParameterId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let binder_value = expressions.at(owner).binder(binder)?;
    let parameter_value = expressions
        .at(owner)
        .coordinate(dae::CoordinateInput::Parameter(parameter))?;
    let guard =
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Equal, binder_value, parameter_value)?;
    let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
    let zero = expressions.at(owner).literal(dae::DaeLiteral::Real(0.0))?;
    expressions.at(owner).conditional([(guard, one)], zero)
}

/// `if noEvent(x > 0) then 1 else 0`: a relation that owns no event.
fn state_branch<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    owner: dae::DaeProvenance,
    state: dae::StateId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let state_value = expressions
        .at(owner)
        .coordinate(dae::CoordinateInput::State(state))?;
    let threshold = expressions.at(owner).literal(dae::DaeLiteral::Real(0.0))?;
    let relation =
        expressions
            .at(owner)
            .binary(dae::BinaryOperator::Greater, state_value, threshold)?;
    let guard = expressions
        .at(owner)
        .builtin(dae::PureBuiltin::NoEvent, [relation])?;
    let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
    let zero = expressions.at(owner).literal(dae::DaeLiteral::Real(0.0))?;
    expressions.at(owner).conditional([(guard, one)], zero)
}

fn state_minus<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    owner: dae::DaeProvenance,
    state: dae::StateId<'dae>,
    value: dae::ExprId<'dae>,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let state_value = expressions
        .at(owner)
        .coordinate(dae::CoordinateInput::State(state))?;
    expressions
        .at(owner)
        .binary(dae::BinaryOperator::Subtract, state_value, value)
}

/// The conditional expression whose source span is `span`.
fn conditional_at<'dae>(view: dae::DaeView<'dae>, span: Span) -> dae::ExprId<'dae> {
    let is_conditional = |expression: &dae::ExprId<'dae>| {
        view.expression(*expression).is_some_and(|node| {
            node.provenance().span() == span
                && matches!(node.operation(), dae::ExpressionOperation::Conditional(_))
        })
    };
    (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .find(is_conditional)
        .expect("conditional branch expression exists")
}

#[test]
fn binder_parameter_guard_is_invariant_and_no_event_state_guard_is_not() {
    let (dae, family_owner, scalar_owner) = binder_guard_fixture();
    dae.inspect(|view| {
        let context = rumoca_eval_dae::FunctionCallContext::default();
        let guard_operands = |expression| match view.expression(expression).unwrap().operation() {
            dae::ExpressionOperation::Conditional(operands) => operands,
            _ => panic!("expected conditional expression"),
        };
        assert!(super::super::parameter_conditionals::has_parameter_guards(
            view,
            &context,
            guard_operands(conditional_at(view, family_owner.span()))
        ));
        assert!(!super::super::parameter_conditionals::has_parameter_guards(
            view,
            &context,
            guard_operands(conditional_at(view, scalar_owner.span()))
        ));
    });
}
