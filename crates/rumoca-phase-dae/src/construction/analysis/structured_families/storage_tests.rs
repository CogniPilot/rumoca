//! Storage-range controls are independent of optional symbolic-body selection.

use super::*;

fn family(
    view: rumoca_core::ComprehensionScalarView,
    extents: &[i64],
) -> flat::StructuredEquationFamily {
    let mut sources = SourceMap::new();
    let source = sources.add("storage_range.mo", "model M equation 0=0; end M;");
    let span = Span::from_offsets(source, 17, 20);
    flat::StructuredEquationFamily {
        domain: StructuredIndexDomain {
            binders: extents
                .iter()
                .enumerate()
                .map(|(id, &upper)| rumoca_core::StructuredIndexBinder {
                    id,
                    display_name: format!("i{id}"),
                    lower: 1,
                    upper,
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
            body: vec![Expression::Literal {
                value: Literal::Real(0.0),
                span,
            }],
            scalar_view: view,
        }),
        interiors: flat::FamilyInteriors::Materialized,
    }
}

fn validate(
    families: &[flat::StructuredEquationFamily],
    count: usize,
) -> Result<HashSet<usize>, ToDaeError> {
    let first = &families[0];
    let equations = vec![
        flat::Equation::new(
            Expression::Literal {
                value: Literal::Real(0.0),
                span: first.span
            },
            first.span,
            first.origin.clone(),
        );
        count
    ];
    validate_structured_families(
        PartitionFamilies {
            // Mandatory masks all materialized optional bodies, reproducing
            // the exact policy boundary without relaxing effect admission.
            families: SelectedFamilies::mandatory(families),
            equations: &equations,
            initialization: false,
            excluded: &HashSet::new(),
        },
        &HashMap::new(),
        &HashMap::new(),
        &HashSet::new(),
        &RecordArrayFieldPlans::default(),
        &ShapeEnvironment::default(),
    )
}

#[test]
fn unselected_views_keep_their_original_physical_ranges() {
    let row = family(
        rumoca_core::ComprehensionScalarView::RowMajorProjection,
        &[8],
    );
    assert_eq!(validate(&[row], 1).unwrap(), HashSet::from([0]));
    let prefix = family(
        rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count: 1 },
        &[2, 3],
    );
    assert_eq!(validate(&[prefix], 2).unwrap(), HashSet::from([0, 1]));
    let scalar = family(
        rumoca_core::ComprehensionScalarView::BinderSubstitution,
        &[3],
    );
    assert_eq!(validate(&[scalar], 3).unwrap(), HashSet::from([0, 1, 2]));
}

#[test]
fn true_physical_overlap_remains_a_refusal() {
    let first = family(
        rumoca_core::ComprehensionScalarView::RowMajorProjection,
        &[2],
    );
    let second = first.clone();
    assert!(
        validate(&[first, second], 2)
            .unwrap_err()
            .to_string()
            .contains("two semantic owners overlap")
    );
}

#[test]
fn out_of_partition_and_invalid_prefix_remain_refusals() {
    let mut row = family(
        rumoca_core::ComprehensionScalarView::RowMajorProjection,
        &[8],
    );
    row.first_equation_index = 1;
    assert!(
        validate(&[row], 1)
            .unwrap_err()
            .to_string()
            .contains("outside the owning Flat equation partition")
    );
    let prefix = family(
        rumoca_core::ComprehensionScalarView::BinderPrefixProjection { binder_count: 3 },
        &[2, 3],
    );
    assert!(validate(&[prefix], 6).is_err());
}
