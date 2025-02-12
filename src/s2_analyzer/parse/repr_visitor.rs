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
            if let Some(class_repr) = self.repr.remove(&cdef.id()) {
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
                .remove(&comp.id())
                .expect(&format!("no repr for {}", comp.id()));
            s += &format!("    {}\n", repr);
        }
        s += &format!("equations\n");
        for eq in n.equations.iter() {
            let repr = self
                .repr
                .remove(&eq.id())
                .expect(&format!("no repr for {}", eq.id()));
            s += &format!("    {}\n", repr);
        }
        s += &format!("alorithms\n");
        s += &format!("end {};\n", n.name);
        self.repr.insert(n.id(), s);
    }

    fn exit_equation_simple(&mut self, n: &node::EquationSimple, _parent_id: Option<usize>) {
        self.repr.insert(
            n.id(),
            format!("{} = {};", self.repr[&n.lhs.id()], self.repr[&n.rhs.id()]),
        );
    }

    fn exit_equation_connect(&mut self, n: &node::EquationConnect, _parent_id: Option<usize>) {
        self.repr.insert(
            n.id(),
            format!(
                "connect({}, {});",
                self.repr[&n.lhs.id()],
                self.repr[&n.rhs.id()]
            ),
        );
    }

    fn exit_expression(&mut self, n: &node::Expression, _parent_id: Option<usize>) {
        self.repr.insert(n.id(), "expr".to_string());
    }

    fn exit_equation(&mut self, n: &node::Equation, _parent_id: Option<usize>) {
        let eq = self
            .repr
            .remove(
                &(match n {
                    node::Equation::Simple(v) => v.id(),
                    node::Equation::If(v) => v.id(),
                    node::Equation::Connect(v) => v.id(),
                    node::Equation::For(v) => v.id(),
                    node::Equation::Empty => panic!("empty equation"),
                }),
            )
            .expect(&format!("equation not found {}: {:?}", n.id(), n));
        self.repr.insert(n.id(), eq);
    }

    fn exit_component_declaration(
        &mut self,
        n: &node::ComponentDeclaration,
        _parent_id: Option<usize>,
    ) {
        self.repr
            .insert(n.id(), format!("{:?} {};", n.type_specifier, n.name));
    }

    fn exit_ref_part(&mut self, n: &node::RefPart, parent_id: Option<usize>) {
        let mut s = String::new();
        s += n.name.as_str();
        if !n.array_subscripts.is_empty() {
            s += "[";
            n.array_subscripts.iter().for_each(|a| {
                s += &format!("{:?}", a);
            });
            s += "]";
        }
        self.repr
            .insert(parent_id.expect("no parent"), format!("{}", s));
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
        let parts: Vec<String> = n.parts.iter().map(|p| self.repr[&p.id()].clone()).collect();
        s += &parts.join(".");
        self.repr.insert(n.id(), s);
    }

    fn enter_subscript(&mut self, n: &node::Subscript, _parent_id: Option<usize>) {
        self.repr.insert(n.id(), format!("{:?}", n));
    }
}
