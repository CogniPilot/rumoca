use super::ast::Component;
use crate::s2_analyzer::ast;
use rumoca_parser::ast::{Element, Expression};
use rumoca_parser::{ast as parse_ast, Visitable, Visitor};
use std::collections::HashMap;

#[derive(Hash, PartialEq, Eq)]
pub enum Key<'a> {
    Element(&'a Element),
    //Expression(&'a Expression),
    StoredDefinition(&'a parse_ast::StoredDefinition),
    ClassDefinition(&'a parse_ast::ClassDefinition),
    ComponentReference(&'a parse_ast::ComponentReference),
    ComponentDeclaration(&'a parse_ast::ComponentDeclaration),
}

#[allow(clippy::large_enum_variant)]
#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Empty,
    Class(ast::Class),
    Def(ast::Def),
    ComponentReference(String),
    Component(Component),
    //ComponentClause(Vec<Component>),
}

#[derive(Default)]
pub struct FlattenVisitor<'a> {
    level: usize,
    map: HashMap<Key<'a>, Value>,
    class_scope: Vec<&'a parse_ast::ClassDefinition>,
    def_scope: Vec<&'a parse_ast::StoredDefinition>,
}

impl FlattenVisitor<'_> {
    pub fn scoped_class(&mut self, up: usize) -> Option<&mut ast::Class> {
        self.class_scope
            .get(self.class_scope.len() - up - 1)
            .and_then(|key| self.map.get_mut(&Key::ClassDefinition(key)))
            .and_then(|res| {
                if let Value::Class(c) = res {
                    Some(c)
                } else {
                    None
                }
            })
    }

    pub fn scoped_def(&mut self, up: usize) -> Option<&mut ast::Def> {
        self.def_scope
            .get(self.def_scope.len() - up - 1)
            .and_then(|key| self.map.get_mut(&Key::StoredDefinition(key)))
            .and_then(|res| {
                if let Value::Def(c) = res {
                    Some(c)
                } else {
                    None
                }
            })
    }
}

impl<'a> Visitor<'a> for FlattenVisitor<'a> {
    fn enter_any(&mut self) {
        self.level += 1;
    }

    fn exit_any(&mut self) {
        self.level -= 1;
    }

    fn enter_stored_definition(&mut self, def: &'a parse_ast::StoredDefinition) {
        let flat_def = ast::Def {
            rumoca_version: env!("CARGO_PKG_VERSION").to_string(),
            rumoca_git: option_env!("GIT_VER").unwrap_or("").to_string(),
            rumoca_parser_version: def.rumoca_parser_version.clone(),
            rumoca_parser_git: def.rumoca_parser_git.clone(),
            ..Default::default()
        };

        // annotate tree
        self.map
            .insert(Key::StoredDefinition(def), Value::Def(flat_def));

        // push definition scope
        self.def_scope.push(def);
    }

    fn exit_stored_definition(&mut self, _def: &'a parse_ast::StoredDefinition) {
        self.def_scope.pop();
    }

    fn enter_class_definition(&mut self, class: &'a parse_ast::ClassDefinition) {
        // create the class definition
        let mut res = ast::Class::default();
        match &class.specifier {
            parse_ast::ClassSpecifier::Long { name, .. } => {
                res.name = name.clone();
            }
            parse_ast::ClassSpecifier::Extends { .. } => {}
        }

        // annotate tree
        let key = Key::ClassDefinition(class);
        self.map.insert(key, Value::Class(res));
        self.class_scope.push(class);
    }

    fn exit_expression(&mut self, expr: &'a Expression) {
        // TODO: flatten class references in expressions to global scope
        match expr {
            Expression::UnsignedInteger(..) => {}
            Expression::UnsignedReal(..) => {}
            Expression::Boolean(..) => {}
            Expression::Ref { .. } => {}
            Expression::Unary { .. } => {}
            Expression::Binary { .. } => {}
            Expression::If { .. } => {}
            Expression::ArrayArguments { .. } => {}
            Expression::FunctionCall { .. } => {}
            Expression::Der { .. } => {}
        }
    }

    fn exit_element(&mut self, elem: &'a Element) {
        let val = match elem {
            Element::ImportClause { .. } => Value::Empty,
            Element::ComponentClause { .. } => Value::Empty,
            Element::ClassDefinition { .. } => Value::Empty,
            Element::ExtendsClause { .. } => Value::Empty,
        };
        self.map.insert(Key::Element(elem), val);
    }

    fn exit_component_reference(&mut self, comp: &'a parse_ast::ComponentReference) {
        let mut s: String = "".to_string();
        for (index, part) in comp.parts.iter().enumerate() {
            if index != 0 || comp.local {
                s += ".";
            }
            s += &part.name;
        }

        // annotate tree
        self.map
            .insert(Key::ComponentReference(comp), Value::ComponentReference(s));
    }

    fn exit_component_declaration(&mut self, comp: &'a parse_ast::ComponentDeclaration) {
        let c = ast::Component {
            name: comp.declaration.name.clone(),
            start: comp.declaration.modification.clone(),
            array_subscripts: comp
                .declaration
                .array_subscripts
                .clone()
                .unwrap_or_default(),
        };
        let class = self.scoped_class(0).expect("failed to get class scope");
        class.components.insert(c.name.clone(), c.clone());
        self.map
            .insert(Key::ComponentDeclaration(comp), Value::Component(c));
    }

    fn exit_class_definition(&mut self, _class: &'a parse_ast::ClassDefinition) {
        let key = self.class_scope.pop().expect("failed to pop class scope");
        let value = self
            .map
            .remove(&Key::ClassDefinition(key))
            .expect("failed to remove class definition");

        if let Value::Class(c) = value {
            let def = self.scoped_def(0).expect("failed to get def scope");
            def.classes.insert(c.name.clone(), c);
        } else {
            panic!("expected Value::Class");
        }
    }
}

pub fn flatten(def: &parse_ast::StoredDefinition) -> Result<ast::Def, Box<dyn std::error::Error>> {
    let mut visitor = FlattenVisitor::default();
    def.accept(&mut visitor);
    if let Some(Value::Def(flat_def)) = visitor.map.remove(&Key::StoredDefinition(def)) {
        Ok(flat_def)
    } else {
        Err("no def found".into())
    }
}
