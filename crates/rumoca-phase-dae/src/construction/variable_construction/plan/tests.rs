use super::*;
use rumoca_core::{
    ComprehensionIndex, Literal, Reference, SourceId, SourceMap, StructuredIndexDomain, TypeId,
};

macro_rules! assert_not_implemented {
    ($ty:ty, $bound:path) => {
        const _: fn() = || {
            trait AmbiguousIfImplemented<Marker> {
                fn probe() {}
            }
            impl<T> AmbiguousIfImplemented<()> for T {}
            struct Implements;
            impl<T: $bound> AmbiguousIfImplemented<Implements> for T {}
            let _ = <$ty as AmbiguousIfImplemented<_>>::probe;
        };
    };
}

assert_not_implemented!(ReservationSchedule, ::core::clone::Clone);
assert_not_implemented!(ReservationSchedule, ::core::marker::Copy);
assert_not_implemented!(VariableConstructionPlan, ::core::clone::Clone);
assert_not_implemented!(ReservationIssuer<()>, ::core::clone::Clone);
assert_not_implemented!(ReservationIssuer<()>, ::core::marker::Copy);
assert_not_implemented!(IssuedReservations<()>, ::core::clone::Clone);

#[test]
fn reservation_schedule_and_issued_capabilities_are_affine_api_products() {}

struct TestSource {
    map: SourceMap,
    id: SourceId,
    text: String,
}

impl TestSource {
    fn new(text: &str) -> Self {
        let mut map = SourceMap::new();
        let id = map.add("variable_plan.mo", text);
        Self {
            map,
            id,
            text: text.to_string(),
        }
    }

    fn span(&self, needle: &str, occurrence: usize) -> Span {
        let start = self.text.match_indices(needle).nth(occurrence).unwrap().0;
        Span::from_offsets(self.id, start, start + needle.len())
    }

    fn into_map(self) -> SourceMap {
        self.map
    }
}

fn reference(name: &str, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::new(name),
        subscripts: Vec::new(),
        span,
    }
}

fn parameter(name: &str, declaration: Span, binding: Expression) -> flat::Variable {
    let mut variable = flat::Variable::empty_with_span(declaration);
    variable.name = VarName::new(name);
    variable.type_id = TypeId::new(1);
    variable.variability = Variability::Parameter(Default::default());
    variable.is_primitive = true;
    variable.binding = Some(binding);
    variable
}

fn assert_intrinsic_attribute_does_not_reserve(
    source: &TestSource,
    ordinals: &HashMap<VarName, usize>,
    function: BuiltinFunction,
) {
    let intrinsic_attribute = parameter(
        "e",
        source.span("parameter Real e=R(1)", 0),
        Expression::BuiltinCall {
            function,
            args: vec![Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1", 1),
            }],
            span: source.span("R(1)", 0),
        },
    );
    assert!(
        !plan_variable(
            &intrinsic_attribute,
            PlannedRole::Parameter,
            None,
            None,
            ordinals
        )
        .requires_reservation(4)
    );
}

#[test]
fn only_self_later_and_runtime_function_attributes_reserve() {
    let source = TestSource::new(
        "parameter Real b=1; parameter Real a=b; parameter Real c=c; \
         parameter Real d=f(); parameter Real e=R(1);",
    );
    let ordinals = [
        (VarName::new("b"), 0),
        (VarName::new("a"), 1),
        (VarName::new("c"), 2),
        (VarName::new("d"), 3),
        (VarName::new("e"), 4),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>();

    let backward = parameter(
        "a",
        source.span("parameter Real a=b", 0),
        reference("b", source.span("b", 1)),
    );
    assert!(
        !plan_variable(&backward, PlannedRole::Parameter, None, None, &ordinals)
            .requires_reservation(1)
    );

    let self_reference = parameter(
        "c",
        source.span("parameter Real c=c", 0),
        reference("c", source.span("c", 1)),
    );
    assert!(
        plan_variable(
            &self_reference,
            PlannedRole::Parameter,
            None,
            None,
            &ordinals
        )
        .requires_reservation(2)
    );

    let runtime_call = parameter(
        "d",
        source.span("parameter Real d=f()", 0),
        Expression::FunctionCall {
            name: Reference::new("f"),
            args: Vec::new(),
            is_constructor: false,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: source.span("f()", 0),
        },
    );
    assert!(
        plan_variable(&runtime_call, PlannedRole::Parameter, None, None, &ordinals)
            .requires_reservation(3)
    );

    let record_constructor = parameter(
        "e",
        source.span("parameter Real e=R(1)", 0),
        Expression::FunctionCall {
            name: Reference::new("R"),
            args: vec![Expression::Literal {
                value: Literal::Real(1.0),
                span: source.span("1", 1),
            }],
            is_constructor: true,
            call_kind: rumoca_core::FunctionCallKind::Invocation,
            span: source.span("R(1)", 0),
        },
    );
    assert!(
        !plan_variable(
            &record_constructor,
            PlannedRole::Parameter,
            None,
            None,
            &ordinals
        )
        .requires_reservation(4)
    );
    for function in [BuiltinFunction::Previous, BuiltinFunction::Hold] {
        assert_intrinsic_attribute_does_not_reserve(&source, &ordinals, function);
    }
    let _ = source.into_map();
}

#[test]
fn later_reference_reserves_but_lexical_binders_do_not() {
    let source = TestSource::new(
        "parameter Real a=b; parameter Real b=1; parameter Real c[2]={i for i in 1:2};",
    );
    let ordinals = [
        (VarName::new("a"), 0),
        (VarName::new("b"), 1),
        (VarName::new("c"), 2),
        (VarName::new("i"), 3),
    ]
    .into_iter()
    .collect::<HashMap<_, _>>();
    let forward = parameter(
        "a",
        source.span("parameter Real a=b", 0),
        reference("b", source.span("b", 0)),
    );
    assert!(
        plan_variable(&forward, PlannedRole::Parameter, None, None, &ordinals)
            .requires_reservation(0)
    );

    let comprehension = parameter(
        "c",
        source.span("parameter Real c[2]", 0),
        Expression::ArrayComprehension {
            expr: Box::new(reference("i", source.span("i", 0))),
            indices: vec![ComprehensionIndex {
                name: "i".to_string(),
                range: Expression::Range {
                    start: Box::new(Expression::Literal {
                        value: Literal::Integer(1),
                        span: source.span("1", 1),
                    }),
                    step: None,
                    end: Box::new(Expression::Literal {
                        value: Literal::Integer(2),
                        span: source.span("2", 0),
                    }),
                    span: source.span("1:2", 0),
                },
            }],
            filter: None,
            span: source.span("{i for i in 1:2}", 0),
        },
    );
    assert!(
        !plan_variable(
            &comprehension,
            PlannedRole::Parameter,
            None,
            None,
            &ordinals
        )
        .requires_reservation(2)
    );

    let mut derived = AttributeDependencyCollector::new(&ordinals);
    derived.with_shadowed([VarName::new("i")], |collector| {
        collector.visit_expression(&reference("i", source.span("i", 1)));
    });
    assert!(derived.dependencies.is_empty());
    let _ = source.into_map();
}

#[test]
fn calculated_cycle_reports_first_internal_use_occurrence() {
    let source = TestSource::new("A=B; B=A; C=1;");
    let a_to_b = source.span("B", 0);
    let b_to_a = source.span("A", 1);
    let mut flat = flat::Model::new();
    for (name, declaration) in [
        ("A", source.span("A", 0)),
        ("B", source.span("B", 1)),
        ("C", source.span("C", 0)),
    ] {
        let mut variable = flat::Variable::empty_with_span(declaration);
        variable.name = VarName::new(name);
        flat.add_variable(variable.name.clone(), variable);
    }
    let variables = vec![
        VariablePlan {
            declaration: source.span("A", 0),
            dependencies: Box::new([DependencyUse {
                target: 1,
                span: a_to_b,
            }]),
            binding_dependencies: Box::new([DependencyUse {
                target: 1,
                span: a_to_b,
            }]),
            requires_function_ids: false,
        },
        VariablePlan {
            declaration: source.span("B", 1),
            dependencies: Box::new([DependencyUse {
                target: 0,
                span: b_to_a,
            }]),
            binding_dependencies: Box::new([DependencyUse {
                target: 0,
                span: b_to_a,
            }]),
            requires_function_ids: false,
        },
        VariablePlan {
            declaration: source.span("C", 0),
            dependencies: Box::new([]),
            binding_dependencies: Box::new([]),
            requires_function_ids: false,
        },
    ];
    let components = rumoca_core::dependency_first_sccs(&[vec![1], vec![0], vec![]]).unwrap();
    let calculated = HashSet::from([VarName::new("B")]);

    let error = reject_recursive_calculated_parameters(
        &flat,
        &calculated,
        &variables,
        &components,
        source.span("A", 0),
    )
    .unwrap_err();
    assert!(matches!(
        error,
        ToDaeError::UnsupportedFlatSemantics {
            feature,
            span,
            ..
        } if feature == "calculated parameter dependency" && span == a_to_b
    ));
    let _ = source.into_map();
}

#[test]
fn calculated_parameter_attributes_do_not_create_a_false_binding_cycle() {
    let source = TestSource::new("A(start=B)=1; B(start=A)=2;");
    let ordinals = [(VarName::new("A"), 0), (VarName::new("B"), 1)]
        .into_iter()
        .collect::<HashMap<_, _>>();
    let mut a = parameter(
        "A",
        source.span("A(start=B)=1", 0),
        Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1", 0),
        },
    );
    a.start = Some(reference("B", source.span("B", 0)));
    let derived = DerivedParameterPlan {
        domain: StructuredIndexDomain {
            binders: Vec::new(),
        },
        body: Expression::Literal {
            value: Literal::Real(1.0),
            span: source.span("1", 0),
        },
        owner: source.span("A(start=B)=1", 0),
    };
    let mut b = parameter(
        "B",
        source.span("B(start=A)=2", 0),
        Expression::Literal {
            value: Literal::Real(2.0),
            span: source.span("2", 0),
        },
    );
    b.start = Some(reference("A", source.span("A", 1)));

    let variables = vec![
        plan_variable(&a, PlannedRole::Algebraic, Some(&derived), None, &ordinals),
        plan_variable(&b, PlannedRole::Parameter, None, None, &ordinals),
    ];
    let full_components = rumoca_core::dependency_first_sccs(&[vec![1], vec![0]]).unwrap();
    assert!(full_components[0].recursive);
    assert!(
        variables
            .iter()
            .all(|variable| variable.binding_dependencies.is_empty())
    );
    let binding_components = rumoca_core::dependency_first_sccs(&[Vec::new(), Vec::new()]).unwrap();
    reject_recursive_calculated_parameters(
        &{
            let mut flat = flat::Model::new();
            flat.add_variable(VarName::new("A"), a);
            flat.add_variable(VarName::new("B"), b);
            flat
        },
        &HashSet::from([VarName::new("A")]),
        &variables,
        &binding_components,
        source.span("A", 0),
    )
    .expect("non-computational attribute cycles remain representable");
    let _ = source.into_map();
}

fn reservation_member(source: &TestSource, source_ordinal: usize) -> ReservationMember {
    ReservationMember {
        source_ordinal,
        span: source.span("member", source_ordinal),
    }
}

#[test]
fn reservation_schedule_rejects_duplicate_missing_and_foreign_members() {
    let source = TestSource::new("member member member");
    let members = vec![
        reservation_member(&source, 0),
        reservation_member(&source, 1),
    ];
    for (definition_order, detail) in [
        (vec![0, 0], "duplicate"),
        (vec![0], "missing"),
        (vec![0, 2], "foreign"),
    ] {
        let Err(error) = ReservationSchedule::checked(
            members.len(),
            members.clone(),
            definition_order,
            source.span("member", 0),
        ) else {
            panic!("a malformed reservation definition order must reject");
        };
        assert!(matches!(
            error,
            ToDaeError::UnsupportedFlatSemantics { feature, detail: message, .. }
                if feature == "variable reservation plan" && message.contains(detail)
        ));
    }
    let Err(foreign_source) = ReservationSchedule::checked(
        2,
        vec![reservation_member(&source, 2)],
        vec![2],
        source.span("member", 0),
    ) else {
        panic!("a foreign source member must reject");
    };
    assert!(matches!(
        foreign_source,
        ToDaeError::UnsupportedFlatSemantics { feature, .. }
            if feature == "variable reservation plan"
    ));
    let _ = source.into_map();
}

#[test]
fn reservation_issuer_rejects_missing_duplicate_foreign_and_long_consumption() {
    let source = TestSource::new("member member member");
    let schedule = || {
        ReservationSchedule::checked(
            2,
            vec![
                reservation_member(&source, 0),
                reservation_member(&source, 1),
            ],
            vec![1, 0],
            source.span("member", 0),
        )
        .unwrap()
    };

    let mut missing = schedule().into_issuer::<usize>();
    assert!(matches!(
        missing.next_source(0, source.span("member", 0)),
        Ok(ReservationRequirement::Reserve)
    ));
    assert!(matches!(
        missing.finish(),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));

    let mut duplicate = schedule().into_issuer::<usize>();
    assert!(matches!(
        duplicate.next_source(0, source.span("member", 0)),
        Ok(ReservationRequirement::Reserve)
    ));
    duplicate.issue(10, source.span("member", 0)).unwrap();
    assert!(matches!(
        duplicate.next_source(0, source.span("member", 1)),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));
    assert!(matches!(
        duplicate.finish(),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));

    let mut foreign = schedule().into_issuer::<usize>();
    assert!(matches!(
        foreign.next_source(2, source.span("member", 2)),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));

    let mut long = schedule().into_issuer::<usize>();
    assert!(matches!(
        long.next_source(0, source.span("member", 0)),
        Ok(ReservationRequirement::Reserve)
    ));
    long.issue(10, source.span("member", 0)).unwrap();
    assert!(matches!(
        long.next_source(1, source.span("member", 1)),
        Ok(ReservationRequirement::Reserve)
    ));
    long.issue(11, source.span("member", 1)).unwrap();
    assert!(matches!(
        long.next_source(2, source.span("member", 2)),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));
    let _ = source.into_map();
}

#[test]
fn reservation_issuer_returns_every_member_in_definition_order() {
    let source = TestSource::new("member member member");
    let schedule = ReservationSchedule::checked(
        2,
        vec![
            reservation_member(&source, 0),
            reservation_member(&source, 1),
        ],
        vec![1, 0],
        source.span("member", 0),
    )
    .unwrap();
    let mut issuer = schedule.into_issuer();
    assert!(matches!(
        issuer.next_source(0, source.span("member", 0)),
        Ok(ReservationRequirement::Reserve)
    ));
    issuer.issue("first", source.span("member", 0)).unwrap();
    assert!(matches!(
        issuer.next_source(1, source.span("member", 1)),
        Ok(ReservationRequirement::Reserve)
    ));
    issuer.issue("second", source.span("member", 1)).unwrap();
    let definitions = issuer
        .finish()
        .unwrap()
        .into_definitions()
        .collect::<Vec<_>>();
    assert_eq!(definitions, ["second", "first"]);
    let _ = source.into_map();
}

#[test]
fn reservation_issuer_consumes_unreserved_sources_without_opening_a_slot() {
    let source = TestSource::new("member member");
    let schedule =
        ReservationSchedule::checked(2, Vec::new(), Vec::new(), source.span("member", 0)).unwrap();
    let mut issuer = schedule.into_issuer::<usize>();
    for source_ordinal in 0..2 {
        assert!(matches!(
            issuer.next_source(source_ordinal, source.span("member", source_ordinal)),
            Ok(ReservationRequirement::Complete)
        ));
    }
    assert_eq!(issuer.finish().unwrap().into_definitions().count(), 0);
    let _ = source.into_map();
}

#[test]
fn populated_reservation_schedule_rejects_immediate_and_trailing_short_finish() {
    let source = TestSource::new("member member");
    let schedule = || {
        ReservationSchedule::checked(2, Vec::new(), Vec::new(), source.span("member", 0)).unwrap()
    };
    assert!(matches!(
        schedule().into_issuer::<usize>().finish(),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));
    let mut trailing_short = schedule().into_issuer::<usize>();
    assert!(matches!(
        trailing_short.next_source(0, source.span("member", 0)),
        Ok(ReservationRequirement::Complete)
    ));
    assert!(matches!(
        trailing_short.finish(),
        Err(dae::DaeConstructionError::InvalidExpressionForm { .. })
    ));
    let _ = source.into_map();
}

#[test]
fn empty_reservation_schedule_has_one_immediate_terminal_path() {
    let source = TestSource::new("owner");
    let definitions =
        ReservationSchedule::checked(0, Vec::new(), Vec::new(), source.span("owner", 0))
            .unwrap()
            .into_issuer::<usize>()
            .finish()
            .unwrap()
            .into_definitions()
            .count();
    assert_eq!(definitions, 0);
    let _ = source.into_map();
}
