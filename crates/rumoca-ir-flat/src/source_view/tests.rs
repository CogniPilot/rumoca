use super::*;
use crate::{EquationOrigin, FamilyInteriors};
use rumoca_core::{
    ComprehensionTemplate, IndexBinderProvenance, Literal, SourceId, Span, StructuredIndexDomain,
};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("source_view.mo"), 1, 2)
}

fn expression(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: span(),
    }
}

fn equation(value: i64) -> Equation {
    Equation::new(
        expression(value),
        span(),
        EquationOrigin::ComponentEquation {
            component: "test".into(),
        },
    )
}

fn family(start: usize, lower: i64, upper: i64) -> StructuredEquationFamily {
    StructuredEquationFamily {
        domain: StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower,
                upper,
                step: 1,
            }],
        },
        first_equation_index: start,
        equations_per_point: 1,
        span: span(),
        origin: EquationOrigin::ComponentEquation {
            component: "source loop".into(),
        },
        regular: None,
        template: Some(ComprehensionTemplate {
            body: vec![expression(91)],
            scalar_view: ComprehensionScalarView::BinderSubstitution,
        }),
        interiors: FamilyInteriors::ContinuousAlgebraic,
    }
}

fn model() -> Model {
    let mut model = Model::new();
    model.equations = (0..4).map(equation).collect();
    model
}

#[test]
fn ordinary_rows_borrow_original_equations_in_order() {
    let model = model();
    let view = model.source_equations().unwrap();
    assert_eq!(view.rows().len(), 4);
    for (row, expected) in view.rows().iter().zip(&model.equations) {
        let SourceEquationRow::Equation(actual) = row else {
            panic!("ordinary row")
        };
        assert!(std::ptr::eq(*actual, expected));
    }
}

#[test]
fn complete_loop_replaces_only_its_placeholders_without_cloning_body() {
    let mut model = model();
    model.structured_equations.push(family(1, 1, 2));
    let view = model.source_equations().unwrap();
    let [
        SourceEquationRow::Equation(first),
        SourceEquationRow::Loop(source_loop),
        SourceEquationRow::Equation(last),
    ] = view.rows()
    else {
        panic!("partition")
    };
    assert!(std::ptr::eq(*first, &model.equations[0]));
    assert!(std::ptr::eq(*last, &model.equations[3]));
    let family = &model.structured_equations[0];
    assert_eq!(source_loop.span(), family.span);
    assert!(std::ptr::eq(source_loop.origin(), &family.origin));
    assert!(std::ptr::eq(
        source_loop.binders(),
        family.domain.binders.as_slice()
    ));
    assert!(std::ptr::eq(
        source_loop.body(),
        family.template.as_ref().unwrap().body.as_slice()
    ));
}

#[test]
fn source_row_order_does_not_depend_on_family_metadata_order() {
    let mut model = model();
    model.structured_equations = vec![family(2, 3, 4), family(0, 1, 2)];
    let view = model.source_equations().unwrap();
    let [
        SourceEquationRow::Loop(first),
        SourceEquationRow::Loop(second),
    ] = view.rows()
    else {
        panic!("two loops")
    };
    assert_eq!(first.binders()[0].lower, 1);
    assert_eq!(second.binders()[0].lower, 3);
}

#[test]
fn materialized_family_keeps_all_original_scalar_rows() {
    let mut model = model();
    let mut family = family(0, 1, 4);
    family.interiors = FamilyInteriors::Materialized;
    model.structured_equations.push(family);
    let view = model.source_equations().unwrap();
    assert!(
        view.rows()
            .iter()
            .all(|r| matches!(r, SourceEquationRow::Equation(_)))
    );
    assert_eq!(view.rows().len(), 4);
}

#[test]
fn zero_point_loop_never_swallows_the_next_scalar_row() {
    let mut model = model();
    model.structured_equations.push(family(0, 1, 0));
    let view = model.source_equations().unwrap();
    assert_eq!(view.rows().len(), 4);
    assert!(matches!(view.rows()[0], SourceEquationRow::Equation(_)));
}

#[test]
fn unmaterialized_family_without_complete_source_template_refuses() {
    let mut model = model();
    let mut source = family(0, 1, 2);
    source.template = None;
    model.structured_equations = vec![source];
    assert_eq!(
        model.source_equations().unwrap_err(),
        SourceEquationViewError::UnsupportedFamily {
            family: 0,
            span: span()
        }
    );
    let source = &mut model.structured_equations[0];
    source.template = family(0, 1, 2).template;
    source.equations_per_point = 2;
    assert!(model.source_equations().is_err());
    let source = &mut model.structured_equations[0];
    source.equations_per_point = 1;
    source.template.as_mut().unwrap().scalar_view = ComprehensionScalarView::RowMajorProjection;
    assert!(model.source_equations().is_err());
}

#[test]
fn invalid_domain_extent_and_outside_rows_refuse_before_any_view_exists() {
    let mut model = model();
    let mut source = family(0, 1, 2);
    source.domain.binders[0].step = 0;
    model.structured_equations = vec![source];
    assert!(matches!(
        model.source_equations(),
        Err(SourceEquationViewError::InvalidRange { .. })
    ));
    model.structured_equations = vec![family(0, 1, 2_000_000)];
    assert!(matches!(
        model.source_equations(),
        Err(SourceEquationViewError::InvalidRange { .. })
    ));
    model.structured_equations = vec![family(usize::MAX, 1, 2)];
    assert!(matches!(
        model.source_equations(),
        Err(SourceEquationViewError::InvalidRange { .. })
    ));
}

#[test]
fn overlapping_placeholder_owners_refuse_instead_of_hiding_a_family() {
    let mut model = model();
    model.structured_equations = vec![family(0, 1, 3), family(2, 1, 2)];
    assert_eq!(
        model.source_equations().unwrap_err(),
        SourceEquationViewError::Overlap {
            family: 1,
            span: span()
        }
    );
}

#[test]
fn initial_and_regular_partitions_keep_their_own_equations_and_families() {
    let mut model = model();
    model.initial_equations = vec![equation(17), equation(18)];
    model.initial_structured_equations = vec![family(0, 5, 6)];
    assert_eq!(model.source_equations().unwrap().rows().len(), 4);
    let initial = model.source_initial_equations().unwrap();
    let [SourceEquationRow::Loop(source_loop)] = initial.rows() else {
        panic!("initial loop")
    };
    assert_eq!(source_loop.binders()[0].lower, 5);
    assert!(
        SourceEquationViewError::Overlap {
            family: 0,
            span: span()
        }
        .to_string()
        .contains("Overlap")
    );
}
