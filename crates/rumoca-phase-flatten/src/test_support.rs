use rumoca_core::{EffectiveType, FunctionParam, Span, TypeId};
use rumoca_ir_ast as ast;

const REAL_TYPE: TypeId = TypeId(0x00fe_0001);
const INTEGER_TYPE: TypeId = TypeId(0x00fe_0002);
const BOOLEAN_TYPE: TypeId = TypeId(0x00fe_0003);
const AGGREGATE_TYPE: TypeId = TypeId(0x00fe_1000);
const PREDEFINED_REAL_DEF: rumoca_core::DefId = rumoca_core::DefId(0xfff0_0001);
const PREDEFINED_INTEGER_DEF: rumoca_core::DefId = rumoca_core::DefId(0xfff0_0002);
const PREDEFINED_BOOLEAN_DEF: rumoca_core::DefId = rumoca_core::DefId(0xfff0_0003);
const PREDEFINED_STRING_DEF: rumoca_core::DefId = rumoca_core::DefId(0xfff0_0004);
const PREDEFINED_CLOCK_DEF: rumoca_core::DefId = rumoca_core::DefId(0xfff0_0005);

pub(crate) fn real_param(name: &str, dimensions: Vec<i64>, span: Span) -> FunctionParam {
    param(name, "Real", REAL_TYPE, REAL_TYPE, dimensions, span)
}

pub(crate) fn integer_param(name: &str, dimensions: Vec<i64>, span: Span) -> FunctionParam {
    param(
        name,
        "Integer",
        INTEGER_TYPE,
        INTEGER_TYPE,
        dimensions,
        span,
    )
}

pub(crate) fn boolean_param(name: &str, dimensions: Vec<i64>, span: Span) -> FunctionParam {
    param(
        name,
        "Boolean",
        BOOLEAN_TYPE,
        BOOLEAN_TYPE,
        dimensions,
        span,
    )
}

pub(crate) fn named_param(
    name: &str,
    type_name: &str,
    type_id: TypeId,
    dimensions: Vec<i64>,
    span: Span,
) -> FunctionParam {
    param(name, type_name, type_id, type_id, dimensions, span)
}

pub(crate) fn aggregate_param(
    name: &str,
    type_name: &str,
    dimensions: Vec<i64>,
    span: Span,
) -> FunctionParam {
    named_param(name, type_name, AGGREGATE_TYPE, dimensions, span)
}

pub(crate) fn install_predefined_type_identities(tree: &mut ast::ClassTree) {
    for (name, def_id) in [
        ("Real", PREDEFINED_REAL_DEF),
        ("Integer", PREDEFINED_INTEGER_DEF),
        ("Boolean", PREDEFINED_BOOLEAN_DEF),
        ("String", PREDEFINED_STRING_DEF),
        ("Clock", PREDEFINED_CLOCK_DEF),
    ] {
        let path = rumoca_core::ComponentPath::from_flat_path(name);
        if tree.scope_tree.predefined_member(&path).is_none() {
            tree.scope_tree.add_predefined_member(path, def_id);
        }
    }
}

pub(crate) fn type_overlay(tree: &ast::ClassTree) -> ast::InstanceOverlay {
    let mut overlay = ast::InstanceOverlay::new();
    for (name, type_id) in [
        ("Real", tree.type_table.real()),
        ("Integer", tree.type_table.integer()),
        ("Boolean", tree.type_table.boolean()),
        ("String", tree.type_table.string()),
        (
            "Clock",
            tree.type_table
                .lookup("Clock")
                .expect("fixture tree owns predefined Clock"),
        ),
    ] {
        if let Some(def_id) = tree
            .scope_tree
            .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
        {
            overlay.type_ids_by_def_id.insert(def_id, type_id);
            overlay.type_roots.insert(type_id, type_id);
        }
    }
    for def_id in tree.def_map.keys().copied() {
        if overlay.type_ids_by_def_id.contains_key(&def_id) {
            continue;
        }
        let type_id = TypeId(0x0100_0000 | def_id.index());
        overlay.type_ids_by_def_id.insert(def_id, type_id);
        overlay.type_roots.insert(type_id, type_id);
    }
    overlay
}

fn param(
    name: &str,
    type_name: &str,
    nominal: TypeId,
    canonical: TypeId,
    dimensions: Vec<i64>,
    span: Span,
) -> FunctionParam {
    let effective_type = EffectiveType::new(nominal, canonical, dimensions)
        .expect("fixture function type is resolved");
    FunctionParam::new(name, type_name, effective_type, span)
}
