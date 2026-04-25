use super::*;

pub(crate) fn collect_qualified_class_names(definitions: &ast::StoredDefinition) -> Vec<String> {
    let mut names = Vec::new();
    collect_qualified_class_names_recursive(&definitions.classes, "", &mut names);
    names
}

pub(crate) fn workspace_symbol_query_match_score(name: &str, query: &str) -> u8 {
    let name_lower = name.to_lowercase();
    if name_lower == query {
        0
    } else if name_lower.starts_with(query) {
        1
    } else {
        2
    }
}

pub(crate) fn class_component_members_from_tree(
    tree: &ast::ClassTree,
    class_name: &str,
) -> Vec<(String, String)> {
    let Some(class) = resolve_class_for_completion(tree, class_name) else {
        return Vec::new();
    };

    let mut members = IndexMap::<String, String>::new();
    let mut visiting = std::collections::HashSet::<DefId>::new();
    collect_class_component_members(tree, class, &mut members, &mut visiting);
    members.into_iter().collect()
}

pub(crate) fn collect_qualified_class_names_recursive(
    classes: &IndexMap<String, ast::ClassDef>,
    prefix: &str,
    names: &mut Vec<String>,
) {
    for (name, class) in classes {
        let qualified = if prefix.is_empty() {
            name.clone()
        } else {
            format!("{prefix}.{name}")
        };
        names.push(qualified.clone());
        if !class.classes.is_empty() {
            collect_qualified_class_names_recursive(&class.classes, &qualified, names);
        }
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new(SessionConfig::default())
    }
}
