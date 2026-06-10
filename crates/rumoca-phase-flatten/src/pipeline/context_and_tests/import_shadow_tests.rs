use std::sync::Arc;

use super::*;

fn token(text: &str) -> rumoca_core::Token {
    rumoca_core::Token {
        text: Arc::from(text),
        ..Default::default()
    }
}

fn comp_ref(name: &str, def_id: rumoca_core::DefId) -> ast::Expression {
    ast::Expression::ComponentReference(ast::ComponentReference {
        local: false,
        parts: vec![ast::ComponentRefPart {
            ident: token(name),
            subs: None,
        }],
        def_id: Some(def_id),
        span: rumoca_core::Span::DUMMY,
    })
}

fn comp_ref_parts(parts: &[&str]) -> ast::ComponentReference {
    ast::ComponentReference {
        local: false,
        parts: parts
            .iter()
            .map(|part| ast::ComponentRefPart {
                ident: token(part),
                subs: None,
            })
            .collect(),
        def_id: None,
        span: rumoca_core::Span::DUMMY,
    }
}

#[test]
fn resolved_local_component_shadows_import_alias_during_equation_qualification() {
    let local_c = rumoca_core::DefId::new(1);
    let import_c = rumoca_core::DefId::new(2);
    let mut def_map = crate::ResolveDefMap::default();
    def_map.insert(local_c, "Pkg.Model.C".to_string());
    def_map.insert(import_c, "Modelica.Constants".to_string());

    let mut imports = qualify::ImportMap::default();
    imports.insert("C".to_string(), "Modelica.Constants".to_string());

    let mut prefix = QualifiedName::new();
    prefix.push("inst".to_string(), vec![]);

    let qualified = qualify_expression_imports_with_def_map(
        &comp_ref("C", local_c),
        &prefix,
        &imports,
        Some(&def_map),
    )
    .unwrap();

    let rumoca_core::Expression::VarRef { name, .. } = qualified else {
        panic!("expected VarRef");
    };
    assert_eq!(name.as_str(), "inst.C");
}

#[test]
fn resolved_import_alias_still_qualifies_to_import_target() {
    let import_c = rumoca_core::DefId::new(2);
    let mut def_map = crate::ResolveDefMap::default();
    def_map.insert(import_c, "Modelica.Constants".to_string());

    let mut imports = qualify::ImportMap::default();
    imports.insert("C".to_string(), "Modelica.Constants".to_string());

    let mut prefix = QualifiedName::new();
    prefix.push("inst".to_string(), vec![]);

    let qualified = qualify_expression_imports_with_def_map(
        &comp_ref("C", import_c),
        &prefix,
        &imports,
        Some(&def_map),
    )
    .unwrap();

    let rumoca_core::Expression::VarRef { name, .. } = qualified else {
        panic!("expected VarRef");
    };
    assert_eq!(name.as_str(), "Modelica.Constants");
}

#[test]
fn instance_component_member_shadows_import_alias_during_equation_qualification() {
    let mut imports = qualify::ImportMap::default();
    imports.insert(
        "Medium".to_string(),
        "Modelica.Media.Water.StandardWater".to_string(),
    );
    imports.insert(
        "medium".to_string(),
        "Modelica.Media.Water.StandardWater".to_string(),
    );

    let mut overlay = ast::InstanceOverlay::default();
    overlay.components.insert(
        ast::InstanceId::new(1),
        ast::InstanceData {
            qualified_name: QualifiedName::from_dotted("tank.medium.state.p"),
            ..ast::InstanceData::default()
        },
    );
    let mut ctx = Context::new();
    ctx.seed_component_member_scopes(&overlay);

    let expr = ast::Expression::FunctionCall {
        comp: comp_ref_parts(&["Medium", "dynamicViscosity"]),
        args: vec![ast::Expression::ComponentReference(comp_ref_parts(&[
            "medium", "state",
        ]))],
        span: rumoca_core::Span::DUMMY,
    };
    let prefix = QualifiedName::from_dotted("tank");

    let qualified =
        qualify_expression_imports_with_def_map_ctx(&expr, &prefix, &imports, None, &ctx).unwrap();

    let rumoca_core::Expression::FunctionCall { name, args, .. } = qualified else {
        panic!("expected function call");
    };
    assert_eq!(
        name.as_str(),
        "Modelica.Media.Water.StandardWater.dynamicViscosity"
    );
    let [rumoca_core::Expression::VarRef { name, .. }] = args.as_slice() else {
        panic!("expected one VarRef argument");
    };
    assert_eq!(name.as_str(), "tank.medium.state");
}
