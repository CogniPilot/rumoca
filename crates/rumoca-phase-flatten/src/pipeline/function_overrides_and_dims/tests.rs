//! Shared fixtures for the override rewriting tests; each submodule covers
//! one seam of the module.

use super::*;
use rumoca_core::{ClassType, ComponentPath, DefId, Span, Token};
use rumoca_ir_ast::{
    ClassDef, ClassInstanceData, ClassTree, Component, ComponentRefPart, ComponentReference,
    Extend, Name, QualifiedName, ScopeKind,
};
use std::sync::Arc;

mod component_override_map;
mod exact_call_selection;
mod function_modifier_actuals;
mod inherited_alias_rewrite;
mod package_alias_calls;
mod package_member_scope;
mod redeclare_modifier_actuals;
mod reference_capture_guards;
mod selection_diagnostics;

fn token(text: &str) -> Token {
    Token {
        text: Arc::from(text),
        ..Token::default()
    }
}

fn test_span() -> Span {
    Span::from_offsets(
        rumoca_core::SourceId::from_source_name("function_overrides_test.mo"),
        1,
        2,
    )
}

fn class(name: &str, class_type: ClassType) -> ClassDef {
    ClassDef {
        name: token(name),
        class_type,
        ..ClassDef::default()
    }
}

fn component(name: &str, type_name: &str, type_def_id: DefId) -> Component {
    Component {
        name: name.to_string(),
        type_name: Name::from_string(type_name),
        type_def_id: Some(type_def_id),
        ..Component::empty_with_span(test_span())
    }
}

fn override_target(name: &str, def_id: DefId, class_type: ClassType) -> OverrideTarget {
    override_target_with_active(name, def_id, class_type, true)
}

fn override_target_with_active(
    name: &str,
    def_id: DefId,
    class_type: ClassType,
    active: bool,
) -> OverrideTarget {
    OverrideTarget {
        alias: leaf_segment(name).to_string(),
        name: name.to_string(),
        def_id,
        class_type,
        active,
        modifier_args: Vec::new(),
    }
}

fn comp_ref(parts: &[&str]) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: parts
            .iter()
            .map(|part| ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: None,
            })
            .collect(),
        span: test_span(),
        qualified_display_name: None,
    }
}

fn resolved_comp_ref(parts: &[(&str, DefId)]) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: parts
            .iter()
            .map(|(part, def_id)| ComponentRefPart {
                ident: token(part),
                subs: None,
                def_id: Some(*def_id),
            })
            .collect(),
        span: test_span(),
        qualified_display_name: None,
    }
}

fn deferred_member_ref(receiver: (&str, DefId), member: &str) -> ComponentReference {
    ComponentReference {
        local: false,
        parts: vec![
            ComponentRefPart {
                ident: token(receiver.0),
                subs: None,
                def_id: Some(receiver.1),
            },
            ComponentRefPart {
                ident: token(member),
                subs: None,
                def_id: None,
            },
        ],
        span: test_span(),
        qualified_display_name: None,
    }
}

fn ast_var(name: &str) -> rumoca_ir_ast::Expression {
    rumoca_ir_ast::Expression::ComponentReference(comp_ref(&[name]))
}

fn resolved_ast_var(parts: &[(&str, DefId)]) -> rumoca_ir_ast::Expression {
    rumoca_ir_ast::Expression::ComponentReference(resolved_comp_ref(parts))
}

fn core_var(parts: &[(&str, DefId)]) -> Expression {
    let name = parts
        .iter()
        .map(|(name, _)| *name)
        .collect::<Vec<_>>()
        .join(".");
    Expression::VarRef {
        name: rumoca_core::Reference::with_component_reference(&name, core_comp_ref(parts)),
        subscripts: vec![],
        span: test_span(),
    }
}

fn core_comp_ref(parts: &[(&str, DefId)]) -> rumoca_core::ComponentReference {
    rumoca_core::ComponentReference::construct(
        false,
        test_span(),
        parts
            .iter()
            .map(|(part, def_id)| rumoca_core::ComponentRefPart {
                ident: (*part).to_string(),
                span: test_span(),
                subs: Vec::new(),
                def_id: *def_id,
            })
            .collect(),
    )
    .expect("test Flat reference carries exact per-segment identities")
}

fn named_arg(expr: &Expression) -> Option<(&str, &Expression)> {
    let Expression::FunctionCall { name, args, .. } = expr else {
        return None;
    };
    Some((
        name.as_str().strip_prefix("__rumoca_named_arg__.")?,
        args.first()?,
    ))
}
