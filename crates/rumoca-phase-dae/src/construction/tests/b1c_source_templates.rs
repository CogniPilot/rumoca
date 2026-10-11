//! Constructor and analysis controls for source-family ownership.

use super::super::*;
use super::support::*;

const TEXT: &str =
    "model M Boolean y[2,2]; equation for i in 1:2, j in 1:2 loop y[i,j]=i>j; end for; end M;";

fn binder(source: &TestSource, name: &str) -> Expression {
    let span = source.span("for i", 0);
    let provenance = rumoca_core::IndexBinderProvenance::Source(
        test_component_reference(name, span).target_def_id(),
    );
    Expression::VarRef {
        name: provenance.reference(name, span).unwrap(),
        subscripts: Vec::new(),
        span: source.span("for i", 0),
    }
}

fn indexed_y(source: &TestSource, subscripts: Vec<Subscript>) -> Expression {
    variable_reference(source, "y", "y[i,j]", 0, subscripts)
}

fn control(source: &TestSource) -> flat::Model {
    let mut model = test_model();
    add_primitive_variable(
        &mut model,
        source,
        "y",
        "Boolean y[2,2]",
        8,
        vec![2, 2],
        true,
    );
    let span = source.span("for i", 0);
    for i in 1..=2 {
        for j in 1..=2 {
            model.add_equation(flat::Equation::new(
                Expression::Binary {
                    op: OpBinary::Sub,
                    lhs: Box::new(indexed_y(
                        source,
                        vec![
                            Subscript::Index { value: i, span },
                            Subscript::Index { value: j, span },
                        ],
                    )),
                    rhs: Box::new(Expression::Literal {
                        value: Literal::Boolean(i > j),
                        span,
                    }),
                    span,
                },
                span,
                flat::EquationOrigin::ComponentEquation {
                    component: String::new(),
                },
            ));
        }
    }
    model.structured_equations.push(family(source));
    model
}

fn family(source: &TestSource) -> flat::StructuredEquationFamily {
    let span = source.span("for i", 0);
    flat::StructuredEquationFamily {
        domain: StructuredIndexDomain {
            binders: ["i", "j"]
                .into_iter()
                .enumerate()
                .map(|(id, name)| rumoca_core::StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Source(
                        test_component_reference(name, span).target_def_id(),
                    ),
                    id,
                    display_name: name.into(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                })
                .collect(),
        },
        first_equation_index: 0,
        equations_per_point: 1,
        span,
        origin: flat::EquationOrigin::ComponentEquation {
            component: String::new(),
        },
        regular: None,
        template: Some(rumoca_core::ComprehensionTemplate {
            body: vec![Expression::Binary {
                op: OpBinary::Sub,
                lhs: Box::new(indexed_y(
                    source,
                    ["i", "j"]
                        .into_iter()
                        .map(|name| Subscript::Expr {
                            expr: Box::new(binder(source, name)),
                            span,
                        })
                        .collect(),
                )),
                rhs: Box::new(Expression::Binary {
                    op: OpBinary::Gt,
                    lhs: Box::new(binder(source, "i")),
                    rhs: Box::new(binder(source, "j")),
                    span,
                }),
                span,
            }],
            scalar_view: rumoca_core::ComprehensionScalarView::BinderSubstitution,
        }),
        interiors: flat::FamilyInteriors::Materialized,
    }
}

fn admitted(model: &flat::Model, family: &flat::StructuredEquationFamily) -> bool {
    let analyzed = analysis::analyze(model).expect("complete original element coverage is checked");
    analyzed
        .aggregate_discrete_connections
        .source_family_assignment(
            model,
            family,
            family.template.as_ref().unwrap(),
            &analyzed.roles,
        )
        .is_some()
}

#[test]
fn full_source_family_constructs_one_scoped_scalar_body() {
    let source = TestSource::new(TEXT);
    let model = control(&source);
    assert!(admitted(&model, &model.structured_equations[0]));
    let result = construct(&model, source.map).unwrap();
    result.inspect(|view| {
        assert_eq!(view.discrete_value_owner_count(), 1);
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let structure = owner.structure().unwrap();
        assert_eq!(
            structure.scalar_view(),
            rumoca_core::ComprehensionScalarView::BinderSubstitution
        );
        let branch = owner.branches().get(0).unwrap();
        let (value, _) = branch.values().iter().next().unwrap();
        let dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Greater,
            lhs,
            rhs,
        } = view.expression(value).unwrap().operation()
        else {
            panic!("original scalar body survives");
        };
        for (ordinal, expression) in [lhs, rhs].into_iter().enumerate() {
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) =
                view.expression(expression).unwrap().operation()
            else {
                panic!("issued binder survives");
            };
            assert_eq!(binder.domain(), structure.domain());
            assert_eq!(binder.ordinal() as usize, ordinal);
        }
    });
}

#[test]
fn source_family_does_not_exchange_axes_or_foreign_source_binders() {
    let source = TestSource::new(TEXT);
    let model = control(&source);
    let mut family = model.structured_equations[0].clone();
    let Expression::Binary { lhs, .. } = &mut family.template.as_mut().unwrap().body[0] else {
        unreachable!()
    };
    let Expression::VarRef { subscripts, .. } = lhs.as_mut() else {
        unreachable!()
    };
    subscripts.swap(0, 1);
    assert!(!admitted(&model, &family), "target axis order is semantic");
    let mut family = model.structured_equations[0].clone();
    let Expression::Binary { rhs, .. } = &mut family.template.as_mut().unwrap().body[0] else {
        unreachable!()
    };
    let Expression::Binary { lhs, .. } = rhs.as_mut() else {
        unreachable!()
    };
    let Expression::VarRef { name, .. } = lhs.as_mut() else {
        unreachable!()
    };
    *name = rumoca_core::Reference::from_component_reference(test_component_reference(
        "foreign",
        source.span("for i", 0),
    ))
    .with_var_name(VarName::new("i"));
    assert!(
        !admitted(&model, &family),
        "same spelling cannot replace an issued source binder"
    );
    let mut family = model.structured_equations[0].clone();
    let Expression::Binary { lhs, .. } = &mut family.template.as_mut().unwrap().body[0] else {
        unreachable!()
    };
    let Expression::VarRef { name, .. } = lhs.as_mut() else {
        unreachable!()
    };
    *name = name.clone().with_instance_id(test_instance_id("foreign"));
    assert!(
        !admitted(&model, &family),
        "display name cannot replace target occurrence identity"
    );
}

#[test]
fn source_family_rejects_foreign_lhs_binder_even_without_rhs_binder_reads() {
    let source = TestSource::new(TEXT);
    let mut model = control(&source);
    let span = source.span("for i", 0);
    // The constant body and its complete scalar view agree. No RHS iterator
    // reference can incidentally supply the missing LHS identity check.
    for equation in &mut model.equations {
        let Expression::Binary { rhs, .. } = &mut equation.residual else {
            unreachable!()
        };
        **rhs = Expression::Literal {
            value: Literal::Boolean(false),
            span,
        };
    }
    let mut family = model.structured_equations[0].clone();
    let Expression::Binary { lhs, rhs, .. } = &mut family.template.as_mut().unwrap().body[0] else {
        unreachable!()
    };
    **rhs = Expression::Literal {
        value: Literal::Boolean(false),
        span,
    };
    let Expression::VarRef { subscripts, .. } = lhs.as_mut() else {
        unreachable!()
    };
    let Subscript::Expr { expr, .. } = &mut subscripts[0] else {
        unreachable!()
    };
    let Expression::VarRef { name, .. } = expr.as_mut() else {
        unreachable!()
    };
    let original = name.clone();
    let foreign =
        rumoca_core::Reference::from_component_reference(test_component_reference("foreign", span))
            .with_var_name(VarName::new("i"));
    assert_eq!(original.var_name(), foreign.var_name());
    assert_ne!(original.target_def_id(), foreign.target_def_id());
    model.structured_equations[0] = family.clone();
    assert!(
        admitted(&model, &family),
        "issued LHS constant-body control"
    );
    let Expression::Binary { lhs, .. } = &mut family.template.as_mut().unwrap().body[0] else {
        unreachable!()
    };
    let Expression::VarRef { subscripts, .. } = lhs.as_mut() else {
        unreachable!()
    };
    let Subscript::Expr { expr, .. } = &mut subscripts[0] else {
        unreachable!()
    };
    let Expression::VarRef { name, .. } = expr.as_mut() else {
        unreachable!()
    };
    *name = foreign;
    assert!(
        !admitted(&model, &family),
        "LHS iterator must match the family's issued declaration, even with constant RHS"
    );
}

#[test]
fn source_family_does_not_infer_source_authority_for_generated_axes() {
    let source = TestSource::new(TEXT);
    let model = control(&source);
    let mut family = model.structured_equations[0].clone();
    assert!(admitted(&model, &family));
    family.domain.binders[0].provenance = rumoca_core::IndexBinderProvenance::Generated;
    assert!(
        !admitted(&model, &family),
        "an explicitly generated axis is not an issued source declaration"
    );
}

#[test]
fn source_family_requires_the_entire_ordered_member_range() {
    let source = TestSource::new(TEXT);
    let model = control(&source);
    let mut family = model.structured_equations[0].clone();
    family.domain.binders[1].upper = 1;
    assert!(
        !admitted(&model, &family),
        "partial domain cannot reclaim complete aggregate coverage"
    );
    let mut reordered = model.clone();
    reordered.equations.swap(1, 2);
    assert!(
        !admitted(&reordered, &reordered.structured_equations[0]),
        "aggregate ordinal order must equal source family row order"
    );
}

#[test]
fn scalar_source_body_cannot_borrow_packed_backward_dependency_proof() {
    let source = TestSource::new(TEXT);
    let model = control(&source);
    let mut family = model.structured_equations[0].clone();
    let Expression::Binary { rhs, .. } = &mut family.template.as_mut().unwrap().body[0] else {
        unreachable!()
    };
    **rhs = indexed_y(
        &source,
        vec![
            Subscript::Index {
                value: 1,
                span: source.span("for i", 0),
            },
            Subscript::Index {
                value: 1,
                span: source.span("for i", 0),
            },
        ],
    );
    assert!(
        !admitted(&model, &family),
        "self reads retain the original constructor proof path"
    );
}
