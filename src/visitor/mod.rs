use crate::ir;
pub mod sub_comp_namer;

#[allow(unused)]
pub trait Visitor {
    fn enter_stored_definition(&mut self, _node: &mut ir::StoredDefinition) {}
    fn exit_stored_definition(&mut self, _node: &mut ir::StoredDefinition) {}

    fn enter_class_definition(&mut self, _node: &mut ir::ClassDefinition) {}
    fn exit_class_definition(&mut self, _node: &mut ir::ClassDefinition) {}

    fn enter_equation(&mut self, _node: &mut ir::Equation) {}
    fn exit_equation(&mut self, _node: &mut ir::Equation) {}

    fn enter_expression(&mut self, _node: &mut ir::Expression) {}
    fn exit_expression(&mut self, _node: &mut ir::Expression) {}

    fn enter_component(&mut self, _node: &mut ir::Component) {}
    fn exit_component(&mut self, _node: &mut ir::Component) {}

    fn enter_component_reference(&mut self, _node: &mut ir::ComponentReference) {}
    fn exit_component_reference(&mut self, _node: &mut ir::ComponentReference) {}
}

#[allow(unused)]
impl ir::StoredDefinition {
    pub fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_stored_definition(self);
        for (_name, class) in &mut self.class_list {
            class.accept(visitor);
        }
        visitor.exit_stored_definition(self);
    }
}

#[allow(unused)]
impl ir::ClassDefinition {
    pub fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_class_definition(self);
        for eq in &mut self.equations {
            eq.accept(visitor);
        }
        visitor.exit_class_definition(self);
    }
}

#[allow(unused)]
impl ir::Equation {
    pub fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_equation(self);
        match self {
            ir::Equation::Simple { lhs, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            _ => {}
        }
        visitor.exit_equation(self);
    }
}

#[allow(unused)]
impl ir::Expression {
    pub fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_expression(self);
        match self {
            ir::Expression::Unary { op, rhs } => {
                rhs.accept(visitor);
            }
            ir::Expression::Binary { lhs, op, rhs } => {
                lhs.accept(visitor);
                rhs.accept(visitor);
            }
            ir::Expression::ComponentReference(cref) => {
                cref.accept(visitor);
            }
            _ => {}
        }
        visitor.exit_expression(self);
    }
}

#[allow(unused)]
impl ir::Component {
    pub fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_component(self);
        visitor.exit_component(self);
    }
}

#[allow(unused)]
impl ir::ComponentReference {
    pub fn accept<V: Visitor>(&mut self, visitor: &mut V) {
        visitor.enter_component_reference(self);
        visitor.exit_component_reference(self);
    }
}
