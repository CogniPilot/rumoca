use rumoca_core::{Expression, Reference, SourceMap, Span, Subscript, VarName};

use super::super::PlannedRole;
use super::super::expression_validation::validate_expression_with_record_array_fields;
use super::*;

fn projection(
    field_span: Span,
    base_span: Span,
    subscript: Subscript,
) -> (Expression, RecordArrayFieldPlans) {
    let coordinate = VarName::new("pin[1].v");
    let expression = Expression::FieldAccess {
        base: Box::new(Expression::Index {
            base: Box::new(Expression::VarRef {
                name: Reference::new("pin"),
                subscripts: Vec::new(),
                span: base_span,
            }),
            subscripts: vec![subscript.clone()],
            span: base_span,
        }),
        field: "v".to_string(),
        span: field_span,
    };
    let plans = RecordArrayFieldPlans {
        by_occurrence: HashMap::from([(
            field_span,
            RecordArrayFieldPlan {
                coordinates: vec![coordinate].into_boxed_slice(),
                subscripts: vec![subscript].into_boxed_slice(),
            },
        )]),
    };
    (expression, plans)
}

#[test]
fn dynamic_projection_subscript_fails_at_its_exact_occurrence() {
    let mut sources = SourceMap::new();
    let source = sources.add("record_projection.mo", "pin[k].v");
    let field_span = Span::from_offsets(source, 0, 8);
    let base_span = Span::from_offsets(source, 0, 6);
    let subscript_span = Span::from_offsets(source, 4, 5);
    let subscript = Subscript::Expr {
        expr: Box::new(Expression::VarRef {
            name: Reference::new("k"),
            subscripts: Vec::new(),
            span: subscript_span,
        }),
        span: subscript_span,
    };
    let (expression, plans) = projection(field_span, base_span, subscript);
    let roles = HashMap::from([(VarName::new("pin[1].v"), PlannedRole::Algebraic)]);

    let error =
        validate_expression_with_record_array_fields(&expression, &roles, &HashSet::new(), &plans)
            .expect_err("an undefined dynamic subscript must fail before DAE construction");

    assert!(matches!(
        error,
        ToDaeError::UnresolvedReference { name, span }
            if name == "k" && span == subscript_span
    ));
}

#[test]
fn absent_materialized_coordinate_fails_at_field_occurrence() {
    let mut sources = SourceMap::new();
    let source = sources.add("record_projection.mo", "pin[:].v");
    let field_span = Span::from_offsets(source, 0, 8);
    let base_span = Span::from_offsets(source, 0, 6);
    let colon_span = Span::from_offsets(source, 4, 5);
    let (expression, plans) =
        projection(field_span, base_span, Subscript::Colon { span: colon_span });

    let error = validate_expression_with_record_array_fields(
        &expression,
        &HashMap::new(),
        &HashSet::new(),
        &plans,
    )
    .expect_err("a missing materialized coordinate must invalidate its certificate");

    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics { feature, span, .. }
            if feature == "record-array member slice" && span == field_span
    ));
}
