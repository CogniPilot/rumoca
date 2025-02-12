use super::node::Node;
use super::node::NodeRef;
use super::node::Visitor;
use rumoca_parser::ast::node;
use rumoca_parser::ast::part::{BinaryOp, UnaryOp};
use std::collections::HashMap;

#[derive(Default)]
pub struct ReprVisitor {
    pub level: usize,
    priv_repr: HashMap<usize, String>,
}

impl ReprVisitor {
    pub fn repr_get(&self, id: usize) -> &str {
        self.priv_repr
            .get(&id)
            .unwrap_or_else(|| panic!("no repr for {}", id))
    }
    pub fn repr_insert(&mut self, id: usize, val: String) {
        if self.priv_repr.contains_key(&id) {
            panic!("repr already exists for {}", id);
        }
        self.priv_repr.insert(id, val);
    }
    pub fn repr_remove(&mut self, id: usize) -> String {
        self.priv_repr
            .remove(&id)
            .unwrap_or_else(|| panic!("no repr for {}", id))
    }
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
            s += &format!("{}\n", self.repr_remove(cdef.id()));
        });
        self.repr_insert(n.id(), s);
    }

    fn exit_class_definition(&mut self, n: &node::ClassDefinition, _parent_id: Option<usize>) {
        let mut s = format!("class {}\n", n.name);
        for comp in n.components.values() {
            s += &format!("    {}\n", self.repr_remove(comp.id()));
        }
        if !n.equations.is_empty() {
            s += "equation\n";
            for eq in n.equations.iter() {
                s += &format!("    {}\n", self.repr_remove(eq.id()));
            }
        }
        if !n.algorithms.is_empty() {
            for alg in n.algorithms.iter() {
                s += "algorithm\n";
                for stmt in alg.iter() {
                    s += &format!("    {}\n", self.repr_remove(stmt.id()));
                }
            }
        }
        if !n.initial_equations.is_empty() {
            s += "initial equation\n";
            for eq in n.initial_equations.iter() {
                s += &format!("    {}\n", self.repr_remove(eq.id()));
            }
        }
        if !n.initial_algorithms.is_empty() {
            for alg in n.initial_algorithms.iter() {
                s += "initial algorithm\n";
                for stmt in alg.iter() {
                    s += &format!("    {}\n", self.repr_remove(stmt.id()));
                }
            }
        }
        s += &format!("end {};\n", n.name);
        self.repr_insert(n.id(), s);
    }

    fn exit_equation_simple(&mut self, n: &node::EquationSimple, _parent_id: Option<usize>) {
        let lhs_repr = self.repr_remove(n.lhs.id());
        let rhs_repr = self.repr_remove(n.rhs.id());
        self.repr_insert(n.id(), format!("{} = {};", lhs_repr, rhs_repr));
    }

    fn exit_equation_connect(&mut self, n: &node::EquationConnect, _parent_id: Option<usize>) {
        let lhs_repr = self.repr_remove(n.lhs.id());
        let rhs_repr = self.repr_remove(n.rhs.id());
        self.repr_insert(n.id(), format!("connect({}, {});", lhs_repr, rhs_repr));
    }

    fn exit_binary(&mut self, n: &node::Binary, _parent_id: Option<usize>) {
        let lhs_repr = self.repr_remove(n.lhs.id());
        let rhs_repr = self.repr_remove(n.rhs.id());
        let s = match n.op {
            BinaryOp::Add => &format!("{} + {}", lhs_repr, rhs_repr),
            BinaryOp::Sub => &format!("{} - {}", lhs_repr, rhs_repr),
            BinaryOp::Mul => &format!("{}*{}", lhs_repr, rhs_repr),
            BinaryOp::Div => &format!("{}/{}", lhs_repr, rhs_repr),
            BinaryOp::ElemAdd => &format!("{} .+ {}", lhs_repr, rhs_repr),
            BinaryOp::ElemSub => &format!("{} .- {}", lhs_repr, rhs_repr),
            BinaryOp::ElemMul => &format!("{}.*{}", lhs_repr, rhs_repr),
            BinaryOp::ElemDiv => &format!("{}./{}", lhs_repr, rhs_repr),
            BinaryOp::And => &format!("{} && {}", lhs_repr, rhs_repr),
            BinaryOp::Or => &format!("{} || {}", lhs_repr, rhs_repr),
            BinaryOp::Exp => &format!("{}^{}", lhs_repr, rhs_repr),
            BinaryOp::ElemExp => &format!("{}.^{}", lhs_repr, rhs_repr),
            BinaryOp::Equal => &format!("{} == {}", lhs_repr, rhs_repr),
            BinaryOp::GreaterThan => &format!("{} > {}", lhs_repr, rhs_repr),
            BinaryOp::GreaterThanOrEqual => &format!("{} >= {}", lhs_repr, rhs_repr),
            BinaryOp::LessThan => &format!("{} < {}", lhs_repr, rhs_repr),
            BinaryOp::LessThanOrEqual => &format!("{} <= {}", lhs_repr, rhs_repr),
            BinaryOp::NotEqual => &format!("{} != {}", lhs_repr, rhs_repr),
            BinaryOp::Range => &format!("{}:{}", lhs_repr, rhs_repr),
            BinaryOp::Not => panic!("Not not a binary op"),
            BinaryOp::Paren => panic!("Parent not a binary op"),
            BinaryOp::Empty => panic!("Empty binary op"),
        };
        self.repr_insert(n.id(), s.to_string());
    }

    fn exit_unary(&mut self, n: &node::Unary, _parent_id: Option<usize>) {
        let rhs_repr = self.repr_remove(n.rhs.id());
        let s = match n.op {
            UnaryOp::ElemNegative => &format!(".-{}", rhs_repr),
            UnaryOp::ElemPositive => &format!(".+{}", rhs_repr),
            UnaryOp::Negative => &format!("-{}", rhs_repr),
            UnaryOp::Positive => &format!("+{}", rhs_repr),
            UnaryOp::Not => &format!("!{}", rhs_repr),
            UnaryOp::Paren => &format!("({})", rhs_repr),
            UnaryOp::Empty => panic!("Empty unary op"),
        };
        self.repr_insert(n.id(), s.to_string());
    }

    fn exit_unsigned_integer(&mut self, n: &node::UnsignedInteger, _parent_id: Option<usize>) {
        self.repr_insert(n.id(), n.val.to_string());
    }

    fn exit_unsigned_real(&mut self, n: &node::UnsignedReal, _parent_id: Option<usize>) {
        self.repr_insert(n.id(), n.val.to_string());
    }

    fn exit_boolean(&mut self, n: &node::Boolean, _parent_id: Option<usize>) {
        self.repr_insert(n.id(), n.val.to_string());
    }

    fn exit_function_call(&mut self, n: &node::FunctionCall, _parent_id: Option<usize>) {
        let mut s = self.repr_remove(n.comp.id());
        s += "(";
        let args: Vec<String> = n
            .args
            .iter()
            .map(|arg| self.repr_remove(arg.id()).to_string())
            .collect();
        s += &args.join(", ");
        s += ")";
        self.repr_insert(n.id(), s.to_string());
    }

    fn exit_component_declaration(
        &mut self,
        n: &node::ComponentDeclaration,
        _parent_id: Option<usize>,
    ) {
        self.repr_insert(n.id(), format!("{:?} {};", n.type_specifier, n.name));
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
        self.repr_insert(parent_id.expect("no parent"), s.to_string());
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
        let parts: Vec<String> = n
            .parts
            .iter()
            .map(|p| self.repr_remove(p.id()).clone())
            .collect();
        s += &parts.join(".");
        self.repr_insert(n.id(), s);
    }

    fn exit_subscript(&mut self, n: &node::Subscript, _parent_id: Option<usize>) {
        self.repr_insert(n.id(), format!("{:?}", n));
    }
}
