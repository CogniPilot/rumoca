//! Resolved source fixtures for Instantiate unit tests.

use rumoca_ir_ast as ast;

pub(crate) struct ResolvedFixture {
    tree: ast::ClassTree,
}

impl ResolvedFixture {
    pub(crate) fn parse(file: &str, source: &str) -> Self {
        let stored = rumoca_phase_parse::parse_to_ast(source, file)
            .unwrap_or_else(|_| panic!("fixture `{file}` must parse"));
        let mut tree = ast::ClassTree::from_parsed(stored);
        tree.source_map.add(file, source);
        let tree = rumoca_phase_resolve::resolve(ast::ParsedTree::new(tree))
            .unwrap_or_else(|_| panic!("fixture `{file}` must resolve"))
            .inner()
            .clone();
        Self { tree }
    }

    pub(crate) fn tree(&self) -> &ast::ClassTree {
        &self.tree
    }

    pub(crate) fn tree_mut(&mut self) -> &mut ast::ClassTree {
        &mut self.tree
    }

    pub(crate) fn into_tree(self) -> ast::ClassTree {
        self.tree
    }

    pub(crate) fn remove_class_def_id(&mut self, qualified_name: &str) {
        let mut parts = qualified_name.split('.');
        let first = parts
            .next()
            .unwrap_or_else(|| panic!("nonempty fixture class name"));
        let mut class = self
            .tree
            .definitions
            .classes
            .get_mut(first)
            .unwrap_or_else(|| panic!("fixture class `{qualified_name}`"));
        for part in parts {
            class = class
                .classes
                .get_mut(part)
                .unwrap_or_else(|| panic!("fixture class `{qualified_name}`"));
        }
        class.def_id = None;
    }
}

pub(crate) fn resolved_tree(file: &str, source: &str) -> ast::ClassTree {
    ResolvedFixture::parse(file, source).into_tree()
}
