//! An indirect current definition retains the same selected-region proof.
use super::super::*;

fn indexed_alias() -> (dae::Dae, Span) {
    let text = "Real alias, result; Integer producer; producer=3; alias={10.0,20.0}[producer]; result=if time<0 then alias else 42.0;";
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("indexed_alias.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let at = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (real, integer) = model.types(|types| {
            let real = types.intern(
                rumoca_core::TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )?;
            let integer = types.intern(
                rumoca_core::TypeId::new(1),
                dae::ValueType::scalar(dae::ScalarType::Integer),
                at,
            )?;
            Ok((real, integer))
        })?;
        let (alias, result, producer) = model.variables(|variables| {
            let alias = variables.algebraic(
                rumoca_core::VarName::new("alias"),
                real,
                at,
                dae::VariableAttributes::default(),
            )?;
            let result = variables.algebraic(
                rumoca_core::VarName::new("result"),
                real,
                at,
                dae::VariableAttributes::default(),
            )?;
            let producer = variables.discrete_value(
                rumoca_core::VarName::new("producer"),
                integer,
                at,
                dae::VariableAttributes::default(),
            )?;
            Ok((alias, result, producer))
        })?;
        let (alias_row, selected_row) = model.expressions(|expressions| {
            let a = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(alias))?;
            let r = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::Algebraic(result))?;
            let p = expressions
                .at(at)
                .coordinate(dae::CoordinateInput::DiscreteValue(producer))?;
            let ten = expressions.at(at).literal(dae::DaeLiteral::Real(10.0))?;
            let twenty = expressions.at(at).literal(dae::DaeLiteral::Real(20.0))?;
            let array = expressions.at(at).array([ten, twenty])?;
            let indexed = expressions.at(at).index(
                array,
                [dae::Subscript::Index {
                    expression: p,
                    provenance: at,
                }],
            )?;
            let alias_row = expressions
                .at(at)
                .binary(dae::BinaryOperator::Subtract, a, indexed)?;
            let time = expressions.at(at).coordinate(dae::CoordinateInput::Time)?;
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let condition = expressions
                .at(at)
                .binary(dae::BinaryOperator::Less, time, zero)?;
            let forty_two = expressions.at(at).literal(dae::DaeLiteral::Real(42.0))?;
            let selected = expressions
                .at(at)
                .conditional([(condition, a)], forty_two)?;
            let selected_row =
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Subtract, r, selected)?;
            Ok((alias_row, selected_row))
        })?;
        model.continuous(|continuous| {
            continuous.value_equation(at, alias_row)?;
            continuous.value_equation(at, selected_row)
        })?;
        define_producer(model, producer, at)
    })
    .unwrap();
    (model, span)
}

fn define_producer<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    producer: dae::DiscreteValueId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let three =
        model.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(3)))?;
    model.b1c([producer], |topology| {
        topology.owner(at, [producer], |owner| owner.always(at, [(three, at)]))?;
        Ok(())
    })
}

#[test]
fn inactive_current_alias_with_unproven_subscript_remains_lazy() {
    let (model, span) = indexed_alias();
    model.inspect(|view| {
        let dae::ContinuousOwnerView::Residual { equation, .. } =
            view.continuous_owners().nth(1).unwrap()
        else {
            panic!("selected owner");
        };
        let dae::ExpressionOperation::Binary { rhs: selected, .. } =
            view.expression(equation.residual()).unwrap().operation()
        else {
            panic!("selected definition");
        };
        let producer = view
            .variables()
            .find(|(_, v)| v.name().as_str() == "producer")
            .unwrap()
            .0
            .index();
        let plan = rumoca_phase_structural::CausalDiscretePlan::derive(view).unwrap();
        let definitions =
            Rc::new(rumoca_phase_structural::SameTickDefinitions::derive(view, &plan).unwrap());
        let layout = crate::layout::lower_layout(view).unwrap();
        let mut compiler = ScalarCompiler::new(view, &layout, None).with_current_definitions(
            DefinitionOwner::new(definitions, Rc::new(BTreeSet::from([producer]))),
        );
        compiler.project_current_definitions = true;
        compiler.enter_context(ScalarContextFrame::CurrentDefinitions {
            parent: compiler.context_id,
        });
        let dae::ExpressionOperation::Conditional(operands) =
            view.expression(selected).unwrap().operation()
        else {
            panic!("conditional owner");
        };
        assert!(!compiler.conditional_is_total(operands));
        let ops = compiler.program(selected, 0).unwrap();
        let block = solve::ScalarProgramBlock::with_source_span(
            vec![ops],
            span.require_provenance("selected alias fixture").unwrap(),
        )
        .unwrap();
        let mut actual = [f64::NAN];
        rumoca_eval_solve::eval_scalar_program_block(
            &block,
            &[f64::NAN, f64::NAN],
            &[3.0],
            0.1,
            None,
            &mut actual,
        )
        .unwrap();
        assert_eq!(actual, [42.0]);
    });
}
