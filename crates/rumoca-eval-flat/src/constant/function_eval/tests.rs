//! Unit tests for the user-function interpreter.

use super::*;

// Fixture-only function exposure identities in this file use the 10_000 range.

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_eval_test.mo"),
        1,
        2,
    )
}

fn function_param(
    name: &str,
    type_name: &str,
    type_id: rumoca_core::TypeId,
) -> rumoca_core::FunctionParam {
    let effective_type = rumoca_core::EffectiveType::new(type_id, type_id, Vec::new())
        .expect("fixture function type is valid");
    rumoca_core::FunctionParam::new(name, type_name, effective_type, test_span())
        .with_def_id(fixture_def_id(name))
}

fn real_param(name: &str) -> rumoca_core::FunctionParam {
    function_param(name, "Real", rumoca_core::TypeId::new(1))
}

fn integer_param(name: &str) -> rumoca_core::FunctionParam {
    function_param(name, "Integer", rumoca_core::TypeId::new(2))
}

fn component_reference(name: &str) -> rumoca_core::ComponentReference {
    component_reference_with_def_id(name, fixture_def_id(name))
}

fn fixture_def_id(name: &str) -> rumoca_core::DefId {
    let def_id = name.bytes().fold(1_u32, |hash, byte| {
        hash.wrapping_mul(16_777_619) ^ u32::from(byte)
    });
    rumoca_core::DefId::new(def_id.max(1))
}

fn fixture_reference(name: &str) -> rumoca_core::Reference {
    exact_reference(name, fixture_def_id(name))
}

fn component_reference_with_def_id(
    name: &str,
    def_id: rumoca_core::DefId,
) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        vec![rumoca_core::ComponentRefPart {
            ident: name.to_string(),
            span: test_span(),
            subs: Vec::new(),
            def_id,
        }],
    )
    .expect("fixture assignment target is exact")
}

fn exact_reference(name: &str, def_id: rumoca_core::DefId) -> rumoca_core::Reference {
    rumoca_core::Reference::with_component_reference(
        name,
        rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![rumoca_core::ComponentRefPart {
                ident: name.to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id,
            }],
        )
        .expect("fixture reference has exact identity"),
    )
}

/// A two-part qualified reference `root.member` with structured identity on
/// both parts and the rendered cache `root.member`.
fn qualified_reference(
    root: &str,
    root_id: rumoca_core::DefId,
    member: &str,
    member_id: rumoca_core::DefId,
) -> rumoca_core::Reference {
    rumoca_core::Reference::with_component_reference(
        format!("{root}.{member}"),
        rumoca_core::ComponentReference::construct(
            false,
            test_span(),
            vec![
                rumoca_core::ComponentRefPart {
                    ident: root.to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: root_id,
                },
                rumoca_core::ComponentRefPart {
                    ident: member.to_string(),
                    span: test_span(),
                    subs: Vec::new(),
                    def_id: member_id,
                },
            ],
        )
        .expect("fixture reference has exact identity"),
    )
}

fn make_simple_function() -> Function {
    // function f(input Real x) output Real y; algorithm y := x * 2; end f;
    let mut func = Function::new("test.f", rumoca_core::DefId::new(10_001), Span::DUMMY);
    func.add_input(real_param("x"));
    func.add_output(real_param("y"));
    func.pure = true;

    // y := x * 2
    func.body = vec![rumoca_core::Statement::Assignment {
        comp: component_reference("y"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::VarRef {
                name: fixture_reference("x"),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        span: rumoca_core::Span::DUMMY,
    }];

    func
}

mod arrays_and_declarations;
mod basic;
mod functions_and_defaults;
mod semantic_validation;
