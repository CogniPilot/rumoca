use paste::paste;
use rumoca_parser::s1_parser::ast::node;
use rumoca_parser::s1_parser::ast::part::NodeData;

pub trait Node {
    fn children(&self) -> Vec<NodeRef> {
        Vec::new()
    }
    fn node_data(&self) -> &NodeData;
    fn id(&self) -> usize {
        self.node_data().id
    }
    fn span(&self) -> (usize, usize) {
        self.node_data().span
    }
}

macro_rules! node_data_impl_basic {
    () => {
        fn node_data(&self) -> &NodeData {
            &self.node_data
        }
    };
}

macro_rules! node_macros {
    ($($name:ident),*) => {
        paste! {

            pub trait Visitable {
                fn accept<V: Visitor + ?Sized>(&self, visitor: &mut V, parent: Option<usize>);
            }

            #[derive(Clone, Debug, PartialEq, Eq, Default)]
            pub enum NodeRef<'a> {
                #[default]
                Empty,
                $(
                    $name(&'a node::$name),
                )*
            }

            impl <'a> Node for NodeRef<'a> {

                fn node_data(&self) -> &NodeData {
                    match self {
                        NodeRef::Empty => panic!("empty node"),
                        $(
                            NodeRef::$name(node) => &node.node_data(),
                        )*
                    }
                }

                fn id(&self) -> usize {
                    self.node_data().id
                }

                fn span(&self) -> (usize, usize) {
                    self.node_data().span
                }

                fn children(&self) -> Vec<NodeRef> {
                    match self {
                        NodeRef::Empty => vec![],
                        $(
                            NodeRef::$name(node) => node.children(),
                        )*
                    }
                }
            }

            $(
                impl <'a> From<NodeRef<'a>>  for &'a node::$name {
                    fn from(value: NodeRef<'a>) -> Self {
                        if let NodeRef::$name(inner) = value {
                            inner
                        } else {
                            panic!("Expected NodeRef::{}", stringify!($name));
                        }
                    }
                }

                impl <'a> From<&'a node::$name> for NodeRef<'a> {
                    fn from(value: &'a node::$name) -> Self {
                        NodeRef::$name(value)
                    }
                }
            )*

            $(
                impl Visitable for node::$name {
                    fn accept<V: Visitor + ?Sized>(& self, visitor: &mut V, parent_id: Option<usize>) {
                        visitor.enter_any(NodeRef::$name(self), parent_id);
                        visitor.[<enter_ $name:snake>](self, parent_id);
                        let parent_id = Some(self.node_data().id);
                        let children = self.children();
                        children.iter().for_each(|child| child.accept(visitor, parent_id));
                        visitor.[<exit_ $name:snake>](self, parent_id);
                        visitor.exit_any(NodeRef::$name(self), parent_id)
                    }
                }
            )*

            impl <'a> Visitable for NodeRef<'a> {
                fn accept<V: Visitor + ?Sized>(& self, visitor: &mut V, _parent_id: Option<usize>) {
                    let id = Some(self.node_data().id);
                    match self {
                        $(
                            NodeRef::$name(node) => node.accept(visitor, id),
                        )*
                        NodeRef::Empty => {},
                    }
                }
            }

            #[allow(unused_variables)]
            pub trait Visitor {
                fn enter_any(&mut self, n: NodeRef, parent_id: Option<usize>) {}
                fn exit_any(&mut self, n: NodeRef, parent_id: Option<usize>) {}
                $(
                    fn [<enter_ $name:snake>](&mut self, n: & node::$name, parent_id: Option<usize>) {}
                    fn [<exit_ $name:snake>](&mut self, n: & node::$name, parent_id: Option<usize>) {}
                )*
            }
        }
    };
}

node_macros!(
    // File Level Nodes
    StoredDefinition,
    // Class Level Nodes
    ClassDefinition,
    ImportClause,
    ExtendsClause,
    ComponentDeclaration,
    // Equations
    Equation,
    EquationSimple,
    EquationIf,
    EquationFor,
    EquationConnect,
    EquationIfBlock,
    // Statements
    Statement,
    StatementAssignment,
    StatementIf,
    StatementFor,
    StatementWhile,
    StatementBreak,
    StatementReturn,
    StatementIfBlock,
    // Expressions
    Expression,
    Unary,
    Binary,
    ExpressionIf,
    FunctionCall,
    ExpressionIfBlock,
    UnsignedInteger,
    UnsignedReal,
    Boolean,
    ComponentReference,
    RefPart,
    Array,
    Subscript,
    SubscriptRange,
    // Modification
    Argument,
    ArgumentModification,
    ArgumentRedeclaration,
    ArgumentReplaceable,
    Modification,
    ModificationClass,
    // Common
    Description,
    ForIndex
);

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// File Level Nodes

impl Node for node::StoredDefinition {
    fn children(&self) -> Vec<NodeRef> {
        self.classes.iter().map(|(_k, v)| v.into()).collect()
    }
    node_data_impl_basic!();
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Class Level Node

impl Node for node::ClassDefinition {
    fn children(&self) -> Vec<NodeRef> {
        self.components
            .values()
            .map(|comp| comp.into())
            .chain(self.equations.iter().map(|eq| eq.into()))
            .chain(self.initial_equations.iter().map(|eq| eq.into()))
            .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::ImportClause {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
    }
    node_data_impl_basic!();
}

impl Node for node::ExtendsClause {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
    }
    node_data_impl_basic!();
}

impl Node for node::ComponentDeclaration {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
    }
    node_data_impl_basic!();
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Equation Nodes

impl Node for node::Equation {
    fn children(&self) -> Vec<NodeRef> {
        match self {
            node::Equation::Empty => Vec::new(),
            node::Equation::Connect(v) => vec![v.into()],
            node::Equation::For(v) => vec![v.into()],
            node::Equation::If(v) => vec![v.into()],
            node::Equation::Simple(v) => vec![v.into()],
        }
    }
    fn node_data(&self) -> &NodeData {
        match self {
            node::Equation::Empty => panic!("equation empty"),
            node::Equation::Connect(v) => v.node_data(),
            node::Equation::For(v) => v.node_data(),
            node::Equation::If(v) => v.node_data(),
            node::Equation::Simple(v) => v.node_data(),
        }
    }
}

impl Node for node::EquationSimple {
    fn children(&self) -> Vec<NodeRef> {
        vec![(&self.lhs).into(), (&self.rhs).into()]
    }
    node_data_impl_basic!();
}

impl Node for node::EquationIf {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
        // self.if_blocks
        //     .iter()
        //     .flat_map(|block| {
        //         std::iter::once((&block.cond).into()).chain(block.eqs.iter().map(|eq| eq.into()))
        //     })
        //     .chain(self.else_eqs.iter().map(|eq| eq.into()))
        //     .chain(std::iter::once((&self.description).into()))
        //     .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::EquationFor {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
        // self.indices
        //     .iter()
        //     .map(|index| index.into())
        //     .chain(self.eqs.iter().map(|eq| eq.into()))
        //     .chain(std::iter::once((&self.description).into()))
        //     .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::EquationConnect {
    fn children(&self) -> Vec<NodeRef> {
        vec![(&self.lhs).into(), (&self.rhs).into()]
    }
    node_data_impl_basic!();
}

impl Node for node::EquationIfBlock {
    fn children(&self) -> Vec<NodeRef> {
        std::iter::once((&self.cond).into())
            .chain(self.eqs.iter().map(|eq| eq.into()))
            .collect()
    }
    node_data_impl_basic!();
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Statement Nodes

impl Node for node::Statement {
    fn children(&self) -> Vec<NodeRef> {
        match self {
            node::Statement::Empty => Vec::new(),
            node::Statement::Assignment(v) => vec![v.into()],
            node::Statement::Break(v) => vec![v.into()],
            node::Statement::For(v) => vec![v.into()],
            node::Statement::If(v) => vec![v.into()],
            node::Statement::Return(v) => vec![v.into()],
            node::Statement::While(v) => vec![v.into()],
        }
    }
    fn node_data(&self) -> &NodeData {
        match self {
            node::Statement::Empty => panic!("statement empty"),
            node::Statement::Assignment(v) => v.node_data(),
            node::Statement::Break(v) => v.node_data(),
            node::Statement::For(v) => v.node_data(),
            node::Statement::If(v) => v.node_data(),
            node::Statement::Return(v) => v.node_data(),
            node::Statement::While(v) => v.node_data(),
        }
    }
}

impl Node for node::StatementAssignment {
    fn children(&self) -> Vec<NodeRef> {
        vec![(&self.comp).into(), (&self.rhs).into()]
    }
    node_data_impl_basic!();
}

impl Node for node::StatementIf {
    fn children(&self) -> Vec<NodeRef> {
        self.if_blocks
            .iter()
            .flat_map(|block| {
                std::iter::once((&block.cond).into())
                    .chain(block.stmts.iter().map(|stmt| stmt.into()))
            })
            .chain(self.else_stmts.iter().map(|stmt| stmt.into()))
            .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::StatementFor {
    fn children(&self) -> Vec<NodeRef> {
        self.stmts.iter().map(|stmt| stmt.into()).collect()
    }
    node_data_impl_basic!();
}

impl Node for node::StatementWhile {
    fn children(&self) -> Vec<NodeRef> {
        std::iter::once((&self.cond).into())
            .chain(self.stmts.iter().map(|stmt| stmt.into()))
            .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::StatementIfBlock {
    fn children(&self) -> Vec<NodeRef> {
        std::iter::once((&self.cond).into())
            .chain(self.stmts.iter().map(|stmt| stmt.into()))
            .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::StatementBreak {
    node_data_impl_basic!();
}

impl Node for node::StatementReturn {
    node_data_impl_basic!();
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Expression Nodes

impl Node for node::Expression {
    fn children(&self) -> Vec<NodeRef> {
        match &self {
            node::Expression::Ref(v) => vec![v.into()],
            node::Expression::Unary(v) => vec![v.into()],
            node::Expression::Binary(v) => vec![v.into()],
            node::Expression::If(v) => vec![v.into()],
            node::Expression::UnsignedInteger(v) => vec![v.into()],
            node::Expression::UnsignedReal(v) => vec![v.into()],
            node::Expression::Boolean(v) => vec![v.into()],
            node::Expression::Array(v) => vec![v.into()],
            node::Expression::FunctionCall(v) => vec![v.into()],
            node::Expression::Empty => Vec::new(),
        }
    }
    fn node_data(&self) -> &NodeData {
        match &self {
            node::Expression::Ref(v) => v.node_data(),
            node::Expression::Unary(v) => v.node_data(),
            node::Expression::Binary(v) => v.node_data(),
            node::Expression::If(v) => v.node_data(),
            node::Expression::UnsignedInteger(v) => v.node_data(),
            node::Expression::UnsignedReal(v) => v.node_data(),
            node::Expression::Boolean(v) => v.node_data(),
            node::Expression::Array(v) => v.node_data(),
            node::Expression::FunctionCall(v) => v.node_data(),
            node::Expression::Empty => panic!("expression empty"),
        }
    }
}

impl Node for node::Unary {
    node_data_impl_basic!();
}

impl Node for node::Binary {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
        // vec![&*(self.lhs).into(), &*(self.rhs).into()]
    }
    node_data_impl_basic!();
}

impl Node for node::ExpressionIf {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
        // self.if_blocks
        //     .iter()
        //     .flat_map(|block| vec![(&block.cond).into(), (&block.expr).into()])
        //     .chain(self.else_expr.iter().map(|expr| expr.as_ref().into()))
        //     .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::FunctionCall {
    fn children(&self) -> Vec<NodeRef> {
        std::iter::once((&self.comp).into())
            .chain(self.args.iter().map(|arg| arg.into()))
            .collect()
    }
    node_data_impl_basic!();
}

impl Node for node::ExpressionIfBlock {
    fn children(&self) -> Vec<NodeRef> {
        vec![(&self.cond).into(), (&self.expr).into()]
    }
    node_data_impl_basic!();
}

impl Node for node::UnsignedInteger {
    node_data_impl_basic!();
}

impl Node for node::UnsignedReal {
    node_data_impl_basic!();
}

impl Node for node::Boolean {
    node_data_impl_basic!();
}

impl Node for node::ComponentReference {
    fn children(&self) -> Vec<NodeRef> {
        self.parts.iter().map(|part| part.into()).collect()
    }
    node_data_impl_basic!();
}

impl Node for node::RefPart {
    fn children(&self) -> Vec<NodeRef> {
        self.array_subscripts.iter().map(|sub| sub.into()).collect()
    }
    node_data_impl_basic!();
}

impl Node for node::Array {
    fn children(&self) -> Vec<NodeRef> {
        vec![]
        //self.subscripts.iter().map(|sub| sub.into()).collect()
    }
    node_data_impl_basic!();
}

impl Node for node::Subscript {
    fn children(&self) -> Vec<NodeRef> {
        match self {
            node::Subscript::Expression(v) => vec![v.into()],
            node::Subscript::Range(v) => vec![v.into()],
            node::Subscript::Empty => Vec::new(),
        }
    }
    fn node_data(&self) -> &NodeData {
        match self {
            node::Subscript::Expression(v) => v.node_data(),
            node::Subscript::Range(v) => v.node_data(),
            node::Subscript::Empty => panic!("subscript empty"),
        }
    }
}

impl Node for node::SubscriptRange {
    node_data_impl_basic!();
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Modification Nodes

impl Node for node::Argument {
    fn children(&self) -> Vec<NodeRef> {
        match &self {
            node::Argument::Empty => Vec::new(),
            node::Argument::Modification(v) => v.children(),
            node::Argument::Redeclaration(v) => v.children(),
            node::Argument::Replaceable(v) => v.children(),
        }
    }
    fn node_data(&self) -> &NodeData {
        match &self {
            node::Argument::Empty => panic!("argument empty"),
            node::Argument::Modification(v) => v.node_data(),
            node::Argument::Redeclaration(v) => v.node_data(),
            node::Argument::Replaceable(v) => v.node_data(),
        }
    }
}

impl Node for node::ArgumentModification {
    node_data_impl_basic!();
}

impl Node for node::ArgumentRedeclaration {
    node_data_impl_basic!();
}

impl Node for node::ArgumentReplaceable {
    node_data_impl_basic!();
}

impl Node for node::Modification {
    fn children(&self) -> Vec<NodeRef> {
        match &self {
            node::Modification::Empty => Vec::new(),
            node::Modification::Class(v) => v.children(),
            node::Modification::Expression(v) => v.children(),
        }
    }
    fn node_data(&self) -> &NodeData {
        match &self {
            node::Modification::Empty => panic!("argument empty"),
            node::Modification::Class(v) => v.node_data(),
            node::Modification::Expression(v) => v.node_data(),
        }
    }
}

impl Node for node::ModificationClass {
    node_data_impl_basic!();
}

impl Node for node::ModExpr {
    fn children(&self) -> Vec<NodeRef> {
        match &self {
            node::ModExpr::Empty => Vec::new(),
            node::ModExpr::Break(v) => v.children(),
            node::ModExpr::Expression(v) => v.children(),
        }
    }
    fn node_data(&self) -> &NodeData {
        match &self {
            node::ModExpr::Empty => panic!("argument empty"),
            node::ModExpr::Break(v) => v.node_data(),
            node::ModExpr::Expression(v) => v.node_data(),
        }
    }
}

impl Node for node::ModExprBreak {
    node_data_impl_basic!();
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Common Nodes

impl Node for node::Description {
    fn children(&self) -> Vec<NodeRef> {
        self.annotation.iter().map(|arg| arg.into()).collect()
    }
    node_data_impl_basic!();
}

impl Node for node::ForIndex {
    fn children(&self) -> Vec<NodeRef> {
        self.in_expr.iter().map(|expr| expr.into()).collect()
    }
    node_data_impl_basic!();
}
