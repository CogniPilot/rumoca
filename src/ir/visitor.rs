use crate::ir;

#[allow(unused)]
pub trait Visitor {
    fn enter_stored_definition(&mut self, _node: &mut ir::ast::StoredDefinition) {}
    fn exit_stored_definition(&mut self, _node: &mut ir::ast::StoredDefinition) {}

    fn enter_class_definition(&mut self, _node: &mut ir::ast::ClassDefinition) {}
    fn exit_class_definition(&mut self, _node: &mut ir::ast::ClassDefinition) {}

    fn enter_equation(&mut self, _node: &mut ir::ast::Equation) {}
    fn exit_equation(&mut self, _node: &mut ir::ast::Equation) {}

    fn enter_expression(&mut self, _node: &mut ir::ast::Expression) {}
    fn exit_expression(&mut self, _node: &mut ir::ast::Expression) {}

    fn enter_component(&mut self, _node: &mut ir::ast::Component) {}
    fn exit_component(&mut self, _node: &mut ir::ast::Component) {}

    fn enter_component_reference(&mut self, _node: &mut ir::ast::ComponentReference) {}
    fn exit_component_reference(&mut self, _node: &mut ir::ast::ComponentReference) {}
}

pub trait Visitable {
    fn accept<V: Visitor>(&mut self, visitor: &mut V);
}

#[allow(unused)]
impl Visitable for ir::ast::StoredDefinition {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_stored_definition(self);
        for (_name, class) in &mut self.class_list {
            class.accept(visitor);
        }
        visitor.exit_stored_definition(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::ClassDefinition {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_class_definition(self);
        for eq in &mut self.equations {
            eq.accept(visitor);
        }
        visitor.exit_class_definition(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::Equation {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_equation(self);
        match self {
            ir::ast::Equation::Simple { lhs, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            ir::ast::Equation::FunctionCall { comp, args } => {
                comp.accept(visitor);
                for arg in args {
                    arg.accept(visitor);
                }
            }
            _ => {}
        }
        visitor.exit_equation(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::Expression {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_expression(self);
        match self {
            ir::ast::Expression::Unary { op, rhs } => {
                rhs.accept(visitor);
            }
            ir::ast::Expression::Binary { lhs, op, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            ir::ast::Expression::ComponentReference(cref) => {
                cref.accept(visitor);
            }
            ir::ast::Expression::FunctionCall { comp, args } => {
                comp.accept(visitor);
                for arg in args {
                    arg.accept(visitor);
                }
            }
            _ => {}
        }
        visitor.exit_expression(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::Component {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_component(self);
        visitor.exit_component(self);
    }
}

#[allow(unused)]
impl Visitable for ir::ast::ComponentReference {
    fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_component_reference(self);
        visitor.exit_component_reference(self);
    }
}
