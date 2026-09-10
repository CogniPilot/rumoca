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

pub(crate) fn collect_qualified_class_names_recursive(
    classes: &ast::AstIndexMap<String, ast::ClassDef>,
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
