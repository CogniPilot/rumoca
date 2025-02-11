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

    fn exit_class_definition(&mut self, n: &node::ClassDefinition, _parent_id: Option<usize>) {
        let mut s = format!("class {}\n", n.name);
        for comp in n.components.values() {
            let repr = self
                .repr
                .get(&comp.id())
                .expect(&format!("no repr for {}", comp.id()));
            s += &format!("    {}\n", repr);
        }
        s += &format!("equations\n");
        for eq in n.equations.iter() {
            let repr = self
                .repr
                .get(&eq.id())
                .expect(&format!("no repr for {}", eq.id()));
            s += &format!("    {}\n", repr);
        }
        s += &format!("alorithms\n");
        s += &format!("end {};\n", n.name);
        self.repr.insert(n.id(), s);
    }

    fn exit_equation_simple(&mut self, n: &node::EquationSimple, parent_id: Option<usize>) {
        self.repr.insert(
            parent_id.expect("no parent"),
            format!("{} = {}", self.repr[&n.lhs.id()], self.repr[&n.rhs.id()]),
        );
    }

    fn exit_expression(&mut self, n: &node::Expression, _parent_id: Option<usize>) {
        self.repr.insert(n.id(), "expr".to_string());
    }

    fn exit_component_declaration(
        &mut self,
        n: &node::ComponentDeclaration,
        _parent_id: Option<usize>,
    ) {
        self.repr
            .insert(n.id(), format!("{:?} {};", n.type_specifier, n.name));
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
