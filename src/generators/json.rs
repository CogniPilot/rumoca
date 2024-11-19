use crate::ast;
use serde_json;

pub fn generate(def: &ast::StoredDefinition) {
    let s = serde_json::to_string_pretty(def).expect("failed to convert def to json");
    println!("{}", s);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::generators::parse_file;

    #[test]
    fn test_generate_json() {
        let def: ast::StoredDefinition = parse_file("src/model.mo").expect("failed to parse");
        generate(&def);
    }
}
