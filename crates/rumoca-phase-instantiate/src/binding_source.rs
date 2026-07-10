use rumoca_core::Variability;
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

pub(crate) fn declaration_binding_source_for_flattening(
    comp: &ast::Component,
    original: &ast::Expression,
    resolved: &ast::Expression,
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
) -> Option<ast::Expression> {
    if original == resolved {
        return None;
    }
    let ast::Expression::ComponentReference(comp_ref) = original else {
        return None;
    };
    if comp_ref.parts.len() >= 2
        || single_part_source_ref_is_final_parameter_sibling(
            comp_ref,
            comp,
            effective_components,
            mod_env,
        )
    {
        return Some(original.clone());
    }
    None
}

fn single_part_source_ref_is_final_parameter_sibling(
    comp_ref: &ast::ComponentReference,
    target_component: &ast::Component,
    effective_components: &IndexMap<String, ast::Component>,
    mod_env: &ast::ModificationEnvironment,
) -> bool {
    let [part] = comp_ref.parts.as_slice() else {
        return false;
    };
    let name = part.ident.text.as_ref();
    let Some(source_component) = effective_components.get(name) else {
        return false;
    };
    if !parameter_like_component(target_component) && !parameter_like_component(source_component) {
        return false;
    }
    mod_env
        .get(&ast::QualifiedName::from_ident(name))
        .is_some_and(|modifier| modifier.final_)
}

fn parameter_like_component(component: &ast::Component) -> bool {
    matches!(
        component.variability,
        Variability::Parameter(_) | Variability::Constant(_)
    ) || component.is_structural
}
