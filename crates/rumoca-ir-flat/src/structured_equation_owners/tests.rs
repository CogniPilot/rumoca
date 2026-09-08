use rumoca_core::{
    AffineForm, ArrayAccess, ComprehensionScalarView, ComprehensionTemplate, Expression, Literal,
    RegularForFamily, Span, StructuredIndexBinder, StructuredIndexBinderId, StructuredIndexDomain,
    VarName,
};

use super::*;
use crate::{EquationOrigin, Variable};

fn origin(label: &str) -> EquationOrigin {
    EquationOrigin::ComponentEquation {
        component: label.to_string(),
    }
}

fn expression(value: i64) -> Expression {
    Expression::Literal {
        value: Literal::Integer(value),
        span: Span::DUMMY,
    }
}

fn domain() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: StructuredIndexBinderId::new(0),
            display_name: "i".to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

fn regular() -> RegularForFamily {
    RegularForFamily {
        binders: vec!["i".to_string()],
        accesses: vec![ArrayAccess {
            var: "x".to_string(),
            subscripts: vec![AffineForm::unit_binder(0, 1)],
        }],
    }
}

fn family() -> StructuredEquationFamily {
    StructuredEquationFamily {
        domain: domain(),
        first_equation_index: 0,
        equations_per_point: 1,
        span: Span::DUMMY,
        origin: origin("family"),
        regular: Some(regular()),
        template: Some(ComprehensionTemplate {
            body: vec![expression(99)],
            scalar_view: ComprehensionScalarView::BinderSubstitution,
        }),
        interiors_materialized: false,
    }
}

fn equation(value: i64) -> Equation {
    Equation::new(expression(value), Span::DUMMY, origin("family"))
}

fn model() -> Model {
    let mut model = Model::new();
    let mut x = Variable::empty_with_span(Span::DUMMY);
    x.name = VarName::new("x");
    x.dims = vec![2];
    model.add_variable(x.name.clone(), x);
    model.equations = vec![equation(1), equation(2)];
    model.structured_equations.push(family());
    model
}

fn owner_error(model: &Model) -> StructuredEquationOwnerError {
    model
        .structured_equation_owners()
        .err()
        .expect("the mutation must reject the owner partition")
}

#[test]
fn compact_template_hides_its_scalar_views() {
    let model = model();
    let owners = model
        .structured_equation_owners()
        .expect("the exact structured owner is valid");
    let continuous = owners.continuous();
    assert_eq!(
        continuous.structured_row_indices().collect::<Vec<_>>(),
        [0, 1]
    );
    assert_eq!(continuous.standalone_rows().count(), 0);
    let roots = continuous.semantic_expression_roots().collect::<Vec<_>>();
    assert_eq!(roots.len(), 1);
    assert!(matches!(
        roots[0],
        Expression::Literal {
            value: Literal::Integer(99),
            ..
        }
    ));
    let [CheckedEquationOwner::Template(family)] = continuous.owners() else {
        panic!("one template must own the complete equation range");
    };
    assert_eq!(family.rows(), 0..2);
    assert_eq!(family.equations().len(), 2);
    assert_eq!(family.domain().scalar_count(), 2);
    let CheckedTemplateRows::RegularConstructionSample { regular } = family.row_representation()
    else {
        panic!("non-materialized rows require their exact regular proof");
    };
    assert_eq!(regular.accesses().len(), 1);
    assert_eq!(regular.accesses()[0].variable().name.as_str(), "x");
}

#[test]
fn standalone_and_template_roots_stay_in_source_row_order() {
    let mut model = model();
    model.equations.insert(
        0,
        Equation::new(expression(0), Span::DUMMY, origin("standalone")),
    );
    model.structured_equations[0].first_equation_index = 1;
    model.equations.push(Equation::new(
        expression(3),
        Span::DUMMY,
        origin("standalone"),
    ));

    let owners = model
        .structured_equation_owners()
        .expect("standalone rows and a template form one partition");
    assert_eq!(
        owners
            .continuous()
            .semantic_expression_roots()
            .map(|expression| match expression {
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } => *value,
                _ => panic!("test roots are integer literals"),
            })
            .collect::<Vec<_>>(),
        [0, 99, 3]
    );
}

#[test]
fn row_major_projection_proves_aggregate_scalar_count() {
    let mut model = model();
    model.equations.truncate(1);
    model.equations[0].scalar_count = 2;
    let family = &mut model.structured_equations[0];
    family.interiors_materialized = true;
    family.regular = None;
    family.template.as_mut().expect("test template").scalar_view =
        ComprehensionScalarView::RowMajorProjection;

    let owners = model
        .structured_equation_owners()
        .expect("one aggregate row represents the complete domain");
    let [CheckedEquationOwner::Template(family)] = owners.continuous().owners() else {
        panic!("the row-major owner remains a template family");
    };
    assert_eq!(family.rows(), 0..1);
}

#[test]
fn template_less_family_is_rejected_without_a_legacy_scalar_fallback() {
    let mut model = model();
    let family = &mut model.structured_equations[0];
    family.template = None;
    family.regular = None;
    family.interiors_materialized = true;

    assert!(matches!(
        owner_error(&model),
        StructuredEquationOwnerError::MissingCanonicalTemplate { .. }
    ));
}

#[test]
fn binder_prefix_projection_proves_both_projected_products() {
    let mut model = model();
    model.structured_equations[0]
        .domain
        .binders
        .push(StructuredIndexBinder {
            id: StructuredIndexBinderId::new(1),
            display_name: "j".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        });
    model.structured_equations[0].regular = None;
    model.structured_equations[0].interiors_materialized = true;
    model.structured_equations[0]
        .template
        .as_mut()
        .expect("test template")
        .scalar_view = ComprehensionScalarView::BinderPrefixProjection { binder_count: 1 };
    model
        .equations
        .iter_mut()
        .for_each(|row| row.scalar_count = 3);

    let owners = model
        .structured_equation_owners()
        .expect("two projected rows each own the three-axis suffix");
    assert_eq!(owners.continuous().structured_row_indices().count(), 2);
}

#[test]
fn mutations_reject_row_correlation_and_overlap() {
    let mut mismatched_origin = model();
    mismatched_origin.equations[1].origin = origin("foreign");
    assert!(matches!(
        owner_error(&mismatched_origin),
        StructuredEquationOwnerError::RowOriginMismatch { row: 1, .. }
    ));

    let mut mismatched_scalar_count = model();
    mismatched_scalar_count.equations[1].scalar_count = 2;
    assert!(matches!(
        owner_error(&mismatched_scalar_count),
        StructuredEquationOwnerError::RowScalarCountMismatch { row: 1, .. }
    ));

    let mut overlap = model();
    overlap.structured_equations.push(family());
    assert!(matches!(
        owner_error(&overlap),
        StructuredEquationOwnerError::OverlappingFamilies {
            family: 1,
            prior_family: 0,
            row: 0,
            ..
        }
    ));
}

#[test]
fn mutations_reject_domain_template_and_non_materialized_claims() {
    let mut binder = model();
    binder.structured_equations[0].domain.binders[0].id = StructuredIndexBinderId::new(4);
    assert!(matches!(
        owner_error(&binder),
        StructuredEquationOwnerError::NonCanonicalBinder { binder: 0, .. }
    ));

    let mut projection = model();
    projection.structured_equations[0]
        .template
        .as_mut()
        .expect("test template")
        .scalar_view = ComprehensionScalarView::BinderPrefixProjection { binder_count: 2 };
    projection.structured_equations[0].regular = None;
    projection.structured_equations[0].interiors_materialized = true;
    assert!(matches!(
        owner_error(&projection),
        StructuredEquationOwnerError::ProjectionOutsideDomain { .. }
    ));

    let mut missing_template = model();
    missing_template.structured_equations[0].template = None;
    assert!(matches!(
        owner_error(&missing_template),
        StructuredEquationOwnerError::MissingCanonicalTemplate { .. }
    ));

    let mut missing_regular = model();
    missing_regular.structured_equations[0].regular = None;
    assert!(matches!(
        owner_error(&missing_regular),
        StructuredEquationOwnerError::NonMaterializedWithoutRegularProof { .. }
    ));

    let mut wrong_regular_view = model();
    wrong_regular_view.structured_equations[0]
        .template
        .as_mut()
        .expect("test template")
        .scalar_view = ComprehensionScalarView::RowMajorProjection;
    wrong_regular_view.equations.truncate(1);
    wrong_regular_view.equations[0].scalar_count = 2;
    wrong_regular_view.structured_equations[0].interiors_materialized = true;
    assert!(matches!(
        owner_error(&wrong_regular_view),
        StructuredEquationOwnerError::RegularRequiresBinderSubstitution { .. }
    ));
}

#[test]
fn mutations_reject_family_cardinality_and_range_claims() {
    let mut zero_body = model();
    zero_body.structured_equations[0].equations_per_point = 0;
    assert!(matches!(
        owner_error(&zero_body),
        StructuredEquationOwnerError::ZeroEquationsPerPoint { .. }
    ));

    let mut body_count = model();
    body_count.structured_equations[0]
        .template
        .as_mut()
        .expect("test template")
        .body
        .push(expression(100));
    assert!(matches!(
        owner_error(&body_count),
        StructuredEquationOwnerError::TemplateBodyCountMismatch { .. }
    ));

    let mut range = model();
    range.structured_equations[0].first_equation_index = 1;
    assert!(matches!(
        owner_error(&range),
        StructuredEquationOwnerError::RowRangeOutsidePartition { .. }
    ));
}

#[test]
fn mutations_reject_every_regular_access_authority() {
    let mut binders = model();
    binders.structured_equations[0]
        .regular
        .as_mut()
        .expect("test regular proof")
        .binders[0] = "j".to_string();
    assert!(matches!(
        owner_error(&binders),
        StructuredEquationOwnerError::RegularBinderMismatch { .. }
    ));

    let mut coefficient_rank = model();
    coefficient_rank.structured_equations[0]
        .regular
        .as_mut()
        .expect("test regular proof")
        .accesses[0]
        .subscripts[0]
        .coeffs
        .clear();
    assert!(matches!(
        owner_error(&coefficient_rank),
        StructuredEquationOwnerError::RegularAccessCoefficientRankMismatch { .. }
    ));

    let mut missing_variable = model();
    missing_variable.structured_equations[0]
        .regular
        .as_mut()
        .expect("test regular proof")
        .accesses[0]
        .var = "y".to_string();
    assert!(matches!(
        owner_error(&missing_variable),
        StructuredEquationOwnerError::RegularAccessMissingVariable { .. }
    ));

    let mut access_rank = model();
    access_rank
        .variables
        .get_mut(&VarName::new("x"))
        .expect("x")
        .dims = vec![2, 2];
    assert!(matches!(
        owner_error(&access_rank),
        StructuredEquationOwnerError::RegularAccessRankMismatch { .. }
    ));

    let mut bounds = model();
    bounds
        .variables
        .get_mut(&VarName::new("x"))
        .expect("x")
        .dims = vec![1];
    assert!(matches!(
        owner_error(&bounds),
        StructuredEquationOwnerError::RegularAccessOutOfBounds {
            access: 0,
            axis: 0,
            ..
        }
    ));
}

#[test]
fn initial_partition_failure_cannot_be_hidden_by_a_valid_continuous_partition() {
    let mut model = model();
    model.initial_equations.push(Equation {
        scalar_count: 0,
        ..equation(10)
    });
    assert!(matches!(
        owner_error(&model),
        StructuredEquationOwnerError::ZeroScalarCount {
            partition: EquationPartitionKind::Initialization,
            row: 0,
        }
    ));
}
