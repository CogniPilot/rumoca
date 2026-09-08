//! A collected Flat function exposes exactly one source declaration, which it
//! carries as its exposure identity. These fixtures write their functions
//! directly rather than resolving a class tree, so the `63_8xx` band names
//! the declarations they write, one value per declaration.

use super::*;
use crate::construction::analysis::function_bodies::{
    exact_record_call_receivers, require_complete_record_call_receivers,
};
use rumoca_core::{
    ClassType, ComponentRefPart, ComponentReference, DefId, EffectiveType, Expression,
    FunctionParam, Literal, Reference, SourceId, TypeId,
};

fn part(name: &str, def_id: u32) -> ComponentRefPart {
    part_at(name, def_id, Span::DUMMY)
}

fn part_at(name: &str, def_id: u32, span: Span) -> ComponentRefPart {
    ComponentRefPart {
        ident: name.to_string(),
        span,
        subs: Vec::new(),
        def_id: DefId::new(def_id),
    }
}

fn test_span(start: usize, end: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("function_record_identity.mo"),
        start,
        end,
    )
}

fn assert_ed019_at(error: ToDaeError, expected_span: Span) -> String {
    let ToDaeError::UnsupportedFlatSemantics {
        feature,
        detail,
        span,
    } = error
    else {
        panic!("expected ED019 UnsupportedFlatSemantics, got {error:?}")
    };
    assert_eq!(feature, "record output assembly");
    assert_eq!(span, expected_span);
    detail
}

fn field(name: &str, def_id: u32) -> ResolvedFunctionRecordField {
    ResolvedFunctionRecordField {
        name: VarName::new(name),
        def_id: DefId::new(def_id),
    }
}

fn parameter(name: &str) -> FunctionParam {
    let effective = EffectiveType::new(TypeId::new(9), TypeId::new(9), Vec::new()).unwrap();
    FunctionParam::new(name, "Real", effective, Span::DUMMY)
}

fn resolved_root(name: &str, def_id: u32) -> Reference {
    resolved_root_at(name, def_id, Span::DUMMY)
}

fn resolved_root_at(name: &str, def_id: u32, span: Span) -> Reference {
    let component = ComponentReference::construct(false, span, vec![part_at(name, def_id, span)])
        .expect("the test root has exact identity");
    Reference::with_component_reference(name, component)
}

fn assignment(root_id: u32, field_name: &str, field_id: u32, span: Span) -> rumoca_core::Statement {
    assignment_to("y", root_id, field_name, field_id, span)
}

fn assignment_to(
    root_name: &str,
    root_id: u32,
    field_name: &str,
    field_id: u32,
    span: Span,
) -> rumoca_core::Statement {
    let component = ComponentReference::construct(
        false,
        span,
        vec![
            part_at(root_name, root_id, span),
            part_at(field_name, field_id, span),
        ],
    )
    .expect("the mutation assignment is structurally resolved");
    rumoca_core::Statement::Assignment {
        comp: component,
        value: Expression::Literal {
            value: Literal::Real(1.0),
            span,
        },
        span,
    }
}

fn receiver(
    root_id: u32,
    field_name: &str,
    field_id: u32,
    root_span: Span,
    field_span: Span,
) -> rumoca_core::ComponentReference {
    ComponentReference::construct(
        false,
        root_span,
        vec![
            part_at("y", root_id, root_span),
            part_at(field_name, field_id, field_span),
        ],
    )
    .expect("the record-call receiver is structurally resolved")
}

fn record_receiver_function() -> rumoca_core::Function {
    let mut function =
        rumoca_core::Function::new("receive", DefId::new(63_801), test_span(400, 480));
    function.add_output(
        parameter("y")
            .with_span(test_span(401, 402))
            .with_def_id(DefId::new(77))
            .with_type_class(ClassType::Record),
    );
    function
}

fn field_read(root: Reference, name: &str, def_id: u32) -> Expression {
    Expression::FieldAccess {
        base: Box::new(Expression::VarRef {
            name: root,
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }),
        field: name.to_string(),
        field_def_id: DefId::new(def_id),
        span: Span::DUMMY,
    }
}

#[test]
fn underscore_sibling_is_not_a_nested_field_path() {
    let direct = field("a", 101);
    let underscore_sibling = field("a_b", 102);
    let assignment = part("a", 101);

    assert!(assignment_part_matches_field(&assignment, &direct).unwrap());
    assert!(
        !assignment_part_matches_field(&assignment, &underscore_sibling).unwrap(),
        "`a` and `a_b` are distinct resolved siblings; spelling cannot invent a path"
    );
}

#[test]
fn matching_spelling_with_a_different_field_identity_fails_closed() {
    let error = assignment_part_matches_field(&part("a_b", 201), &field("a_b", 102))
        .expect_err("equal display spelling cannot override distinct field identities");
    assert!(error.to_string().contains("constructor identity 102"));
    assert!(error.to_string().contains("assignment identity 201"));
}

#[test]
fn matching_identity_with_a_different_field_spelling_fails_closed() {
    let error = assignment_part_matches_field(&part("forged", 102), &field("a_b", 102))
        .expect_err("one field identity cannot carry contradictory retained spellings");
    assert!(error.to_string().contains("identity 102"));
    assert!(error.to_string().contains("`a_b`"));
    assert!(error.to_string().contains("`forged`"));
}

#[test]
fn constructor_field_without_resolved_identity_fails_before_planning() {
    let field_span = test_span(100, 103);
    let declaration = parameter("a_b").with_span(field_span);
    let error = resolved_record_field("y", &declaration)
        .expect_err("record assembly cannot infer a missing field identity from its name");
    let detail = assert_ed019_at(error, field_span);
    assert!(detail.contains("no resolved field identity"));
}

#[test]
fn reserved_constructor_field_identity_fails_before_planning() {
    let field_span = test_span(110, 113);
    let declaration = parameter("a_b")
        .with_span(field_span)
        .with_def_id(DefId::new(0));
    let error = resolved_record_field("y", &declaration)
        .expect_err("DefId(0) is unresolved, not a record field identity");
    let detail = assert_ed019_at(error, field_span);
    assert!(detail.contains("no resolved field identity"));
}

#[test]
fn duplicate_constructor_field_identity_fails_before_planning() {
    let duplicate_span = test_span(120, 123);
    let mut constructor = rumoca_core::Function::new("R", DefId::new(63_802), Span::DUMMY);
    constructor.add_input(parameter("a").with_def_id(DefId::new(101)));
    constructor.add_input(
        parameter("a_b")
            .with_span(duplicate_span)
            .with_def_id(DefId::new(101)),
    );

    let error = resolved_constructor_fields("y", &constructor)
        .expect_err("one field identity cannot own two constructor positions");
    let detail = assert_ed019_at(error, duplicate_span);
    assert!(detail.contains("repeats resolved field identity 101"));
}

#[test]
fn foreign_grouped_field_fails_at_the_exact_middle_assignment() {
    let first_span = test_span(10, 13);
    let foreign_span = test_span(20, 27);
    let last_span = test_span(30, 33);
    let statements = [
        assignment(77, "a", 101, first_span),
        assignment(77, "ghost", 999, foreign_span),
        assignment(77, "b", 102, last_span),
    ];
    let mut function = rumoca_core::Function::new("build", DefId::new(63_803), test_span(0, 40));
    function.add_output(
        parameter("y")
            .with_span(test_span(1, 2))
            .with_def_id(DefId::new(77))
            .with_type_class(ClassType::Record),
    );
    let fields = [field("a", 101), field("b", 102)];

    let error = require_group_constructor_fields(&statements, &[0, 1, 2], &function, &fields)
        .expect_err("a foreign middle field cannot be silently dropped from the group");
    let detail = assert_ed019_at(error, foreign_span);
    assert!(detail.contains("y.ghost"));
    assert!(detail.contains("identity 999"));
}

#[test]
fn record_root_identity_mutations_fail_at_the_root_occurrence() {
    let root_span = test_span(50, 51);
    let mut function = rumoca_core::Function::new("build", DefId::new(63_804), test_span(40, 70));
    function.add_output(
        parameter("y")
            .with_span(test_span(41, 42))
            .with_def_id(DefId::new(77))
            .with_type_class(ClassType::Record),
    );

    for root in [part_at("y", 999, root_span), part_at("y", 0, root_span)] {
        let error = resolved_record_value(&root, &function)
            .expect_err("foreign and reserved roots cannot select a record target");
        let detail = assert_ed019_at(error, root_span);
        assert!(detail.contains("root identity"));
    }
}

#[test]
fn available_record_read_requires_exact_root_and_field_identities() {
    let available = [field("a_b", 102)];
    reject_record_self_reference(
        &field_read(resolved_root("result", 77), "a_b", 102),
        "result",
        DefId::new(77),
        &available,
        Span::DUMMY,
    )
    .expect("matching spelling and identity prove the field is available");

    let error = reject_record_self_reference(
        &field_read(resolved_root("result", 77), "a_b", 201),
        "result",
        DefId::new(77),
        &available,
        Span::DUMMY,
    )
    .expect_err("available spelling cannot authorize a different field identity");
    assert!(error.to_string().contains("identity 102"));
    assert!(error.to_string().contains("identity 201"));
}

#[test]
fn record_read_without_structured_root_identity_fails_early() {
    let available = [field("a_b", 102)];
    let error = reject_record_self_reference(
        &field_read(Reference::new("result"), "a_b", 102),
        "result",
        DefId::new(77),
        &available,
        Span::DUMMY,
    )
    .expect_err("record spelling alone cannot establish the output root");
    assert!(
        error
            .to_string()
            .contains("no structured resolved identity")
    );
}

#[test]
fn record_read_with_output_identity_and_forged_root_spelling_fails_early() {
    let available = [field("a_b", 102)];
    let error = reject_record_self_reference(
        &field_read(resolved_root("forged", 77), "a_b", 102),
        "result",
        DefId::new(77),
        &available,
        Span::DUMMY,
    )
    .expect_err("the output identity cannot be hidden behind contradictory spelling");
    assert!(error.to_string().contains("root `forged` identity 77"));
    assert!(error.to_string().contains("`result` identity 77"));
}

#[test]
fn deeper_staged_self_read_is_typed_refused_at_the_outer_field() {
    let root_span = test_span(80, 81);
    let inner_span = test_span(82, 87);
    let nested_span = test_span(88, 89);
    let inner = Expression::FieldAccess {
        base: Box::new(Expression::VarRef {
            name: resolved_root_at("result", 77, root_span),
            subscripts: Vec::new(),
            span: root_span,
        }),
        field: "inner".to_string(),
        field_def_id: DefId::new(102),
        span: inner_span,
    };
    let nested = Expression::FieldAccess {
        base: Box::new(inner),
        field: "p".to_string(),
        field_def_id: DefId::new(103),
        span: nested_span,
    };
    let error = reject_record_self_reference(
        &nested,
        "result",
        DefId::new(77),
        &[field("inner", 102)],
        test_span(75, 95),
    )
    .expect_err("a partial identity path cannot fall back to the stale whole seeded record");
    let detail = assert_ed019_at(error, nested_span);
    assert!(detail.contains("nested self-read"));
    assert!(detail.contains("complete staged record-field identity path"));
}

#[test]
fn staging_identity_keeps_same_spelling_on_distinct_targets_separate() {
    let first = FunctionRecordFieldIdentity {
        target: DefId::new(77),
        field: DefId::new(102),
    };
    let second = FunctionRecordFieldIdentity {
        target: DefId::new(88),
        field: DefId::new(102),
    };
    let identities = HashSet::from([first, second]);
    assert_eq!(identities.len(), 2);

    let mut definitions = FunctionDefinitions::new(&rumoca_core::Function::new(
        "same_spelling",
        DefId::new(63_805),
        Span::DUMMY,
    ));
    definitions.define_record_field(first);
    definitions.define_record_field(second);
    assert!(definitions.is_record_field_defined(first));
    assert!(definitions.is_record_field_defined(second));

    let display = VarName::new("result.a");
    assert_ne!(
        FunctionConditionalTarget {
            name: display.clone(),
            target_def_id: first.target,
            record_field: Some(first),
        },
        FunctionConditionalTarget {
            name: display,
            target_def_id: second.target,
            record_field: Some(second),
        },
        "conditional definedness must not merge exact fields by display spelling"
    );
}

#[test]
fn staging_groups_use_exact_target_identity_and_first_source_order() {
    let mut function =
        rumoca_core::Function::new("same_spelling", DefId::new(63_806), test_span(360, 390));
    for target in [77, 88] {
        function.add_output(
            parameter("y")
                .with_def_id(DefId::new(target))
                .with_type_class(ClassType::Record),
        );
    }
    let statements = vec![
        assignment_to("y", 88, "a", 102, test_span(361, 362)),
        assignment_to("y", 77, "a", 102, test_span(363, 364)),
        assignment_to("y", 88, "b", 103, test_span(365, 366)),
        assignment_to("y", 77, "b", 103, test_span(367, 368)),
    ];

    let groups = staged_record_assignment_groups(&statements, &function)
        .expect("resolved staging assignments group without display-name recovery");
    assert_eq!(groups.len(), 2);
    assert_eq!(groups[0].0.def_id, Some(DefId::new(88)));
    assert_eq!(groups[0].1, [0, 2]);
    assert_eq!(groups[1].0.def_id, Some(DefId::new(77)));
    assert_eq!(groups[1].1, [1, 3]);
}

#[test]
fn record_assembly_source_cannot_reintroduce_underscore_path_inference() {
    let source = include_str!("../function_record_assemblies.rs");
    let compact = source
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();

    assert!(!compact.contains(".name.strip_prefix(target.ident.as_str())"));
    assert!(!compact.contains("suffix.strip_prefix('_')"));
    assert!(!compact.contains("suffix.starts_with('_')"));

    let bodies = include_str!("../function_bodies.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let construction = include_str!("../../function_construction.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let construction_owner = include_str!("../../../construction.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let analysis_owner = include_str!("../../analysis.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let expression = include_str!("../../expression.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let record_assembly = include_str!("../../function_record_assembly.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    let definitions = include_str!("../function_definitions.rs")
        .chars()
        .filter(|character| !character.is_whitespace())
        .collect::<String>();
    assert!(compact.contains("require_group_constructor_fields("));
    assert!(bodies.contains("resolved_record_value(root,context.function)?"));
    assert!(bodies.contains("resolved_constructor_fields(&target.name,constructor)?"));
    assert!(bodies.contains("require_constructor_field("));
    assert!(!construction.contains("coordinates.insert(staging_name"));
    assert!(construction.contains("staging.insert_local(FunctionRecordFieldIdentity{"));
    assert!(definitions.contains("HashMap<FunctionDefinitionIdentity,ValueCoverage>"));
    assert!(definitions.contains("RecordField(FunctionRecordFieldIdentity)"));
    assert!(bodies.contains("definitions.define_record_field(FunctionRecordFieldIdentity{"));
    assert!(!bodies.contains("definitions.define_whole(&staged)"));
    assert!(construction_owner.contains("structFunctionRecordStagingAvailability{"));
    assert!(construction_owner.contains("if!self.available.identities.contains(&identity)"));
    assert!(construction_owner.contains("advance_function_record_staging(plan"));
    assert!(definitions.contains("Expression::FieldAccess{"));
    assert!(definitions.contains("require_direct_record_field_readable("));
    assert!(definitions.contains("join_record_field_reaching_definitions("));
    assert!(
        analysis_owner
            .contains("available.retain(|identity|identity.target!=assignment.target_def_id())")
    );
    assert!(record_assembly.contains("symbols.record_staging_scope().get(identity)"));
    assert!(
        record_assembly.contains("expressions.record_field_ordinal(record,&field.name,generated)")
    );
    assert!(!expression.contains("Option<&'symbolsFunctionRecordStagingValues"));
}

#[test]
fn record_multi_output_foreign_root_fails_at_receiver_root() {
    let root_span = test_span(500, 501);
    let function = record_receiver_function();
    let outputs = [Some(receiver(
        999,
        "a",
        101,
        root_span,
        test_span(502, 503),
    ))];
    let error = exact_record_call_receivers(
        &outputs,
        &function.outputs[0],
        DefId::new(77),
        &[field("a", 101)],
        &function,
    )
    .expect_err("a foreign receiver root cannot enter a record aggregate plan");
    assert_ed019_at(error, root_span);
}

#[test]
fn record_multi_output_duplicate_field_fails_at_second_receiver() {
    let duplicate_span = test_span(520, 521);
    let function = record_receiver_function();
    let outputs = [
        Some(receiver(
            77,
            "a",
            101,
            test_span(510, 511),
            test_span(512, 513),
        )),
        Some(receiver(77, "a", 101, test_span(518, 519), duplicate_span)),
    ];
    let error = exact_record_call_receivers(
        &outputs,
        &function.outputs[0],
        DefId::new(77),
        &[field("a", 101), field("b", 102)],
        &function,
    )
    .expect_err("one constructor field cannot receive two call results");
    assert_ed019_at(error, duplicate_span);
}

#[test]
fn record_multi_output_missing_receiver_fails_at_call_span() {
    let call_span = test_span(530, 540);
    let outputs = [
        Some(receiver(
            77,
            "a",
            101,
            test_span(531, 532),
            test_span(533, 534),
        )),
        None,
    ];
    let error = require_complete_record_call_receivers(&outputs, "y", 2, call_span)
        .expect_err("a complete record layout cannot silently omit one receiver");
    assert_ed019_at(error, call_span);
}

#[test]
fn record_multi_output_reordered_complete_layout_keeps_result_ordinals() {
    let function = record_receiver_function();
    let outputs = [
        Some(receiver(
            77,
            "b",
            102,
            test_span(550, 551),
            test_span(552, 553),
        )),
        Some(receiver(
            77,
            "a",
            101,
            test_span(554, 555),
            test_span(556, 557),
        )),
    ];
    let receivers = exact_record_call_receivers(
        &outputs,
        &function.outputs[0],
        DefId::new(77),
        &[field("a", 101), field("b", 102)],
        &function,
    )
    .expect("a reordered complete layout is exact and deterministic");
    assert_eq!(receivers[&DefId::new(101)], 1);
    assert_eq!(receivers[&DefId::new(102)], 0);
}

#[test]
fn record_multi_output_constructor_order_layout_is_admitted() {
    let function = record_receiver_function();
    let outputs = [
        Some(receiver(
            77,
            "a",
            101,
            test_span(570, 571),
            test_span(572, 573),
        )),
        Some(receiver(
            77,
            "b",
            102,
            test_span(574, 575),
            test_span(576, 577),
        )),
    ];
    require_complete_record_call_receivers(&outputs, "y", 2, test_span(570, 580))
        .expect("the complete constructor layout has every receiver");
    let receivers = exact_record_call_receivers(
        &outputs,
        &function.outputs[0],
        DefId::new(77),
        &[field("a", 101), field("b", 102)],
        &function,
    )
    .expect("the exact constructor-order layout is valid");
    assert_eq!(receivers[&DefId::new(101)], 0);
    assert_eq!(receivers[&DefId::new(102)], 1);
}
