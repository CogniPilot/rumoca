use super::node::Node;
use super::node::NodeRef;
use super::node::Visitor;
use rumoca_parser::ast::node;
use std::collections::HashMap;

#[derive(Default)]
pub struct ReprVisitor {
    pub level: usize,
    pub repr: HashMap<usize, String>,
}

impl Visitor for ReprVisitor {
    fn enter_any(&mut self, _n: NodeRef, _parent_id: Option<usize>) {
        self.level += 1;
    }

    fn exit_any(&mut self, _n: NodeRef, _parent_id: Option<usize>) {
        self.level -= 1;
    }

    fn exit_stored_definition(&mut self, n: &node::StoredDefinition, _parent_id: Option<usize>) {
        let mut s = String::new();
        n.classes.iter().for_each(|(_name, cdef)| {
            if let Some(class_repr) = self.repr.get(&cdef.id()) {
                s += &format!("{}\n", class_repr);
            }
        });
        self.repr.insert(n.id(), s);
    }

    fn enter_class_definition(&mut self, n: &node::ClassDefinition, _parent_id: Option<usize>) {
        self.repr.insert(n.id(), format!("enter class {}", n.name));
    }

    fn enter_component_declaration(
        &mut self,
        n: &node::ComponentDeclaration,
        _parent_id: Option<usize>,
    ) {
        self.repr
            .insert(n.id(), format!("enter component declaration: {}", n.name));
    }

    fn exit_component_reference(
        &mut self,
        n: &node::ComponentReference,
        _parent_id: Option<usize>,
    ) {
        let mut s = String::new();
        if n.local {
            s += ".";
        }
        n.parts.iter().for_each(|p| {
            s += p.name.as_str();
            if !p.array_subscripts.is_empty() {
                s += "[";
                p.array_subscripts.iter().for_each(|a| {
                    s += &format!("{:?}", a);
                });
                s += "]";
            }
        });
        self.repr
            .insert(n.id(), format!("enter component reference: {}", s));
    }

    fn enter_subscript(&mut self, n: &node::Subscript, _parent_id: Option<usize>) {
        self.repr.insert(n.id(), format!("{:?}", n));
    }
}
