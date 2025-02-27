use crate::modelica_grammar_trait;
#[allow(unused_imports)]
use parol_runtime::{Location, Result, Span, ToSpan, Token};
use std::fmt::{Debug, Display, Error, Formatter};

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct OwnedToken {
    pub text: String,
    pub span: Span,
}

impl ToSpan for OwnedToken {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&Token<'_>> for OwnedToken {
    type Error = anyhow::Error;

    fn try_from(value: &Token<'_>) -> std::result::Result<Self, Self::Error> {
        Ok(OwnedToken {
            text: value.text().to_string(),
            span: Span::new(value.location.start as usize, value.location.end as usize),
        })
    }
}

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct StoredDefinition {
//     pub class_list: IndexMap<String, ClassDefinition>,
//     pub within: Option<Name>,
//     pub span: Span,
// }

// impl ToSpan for StoredDefinition {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::StoredDefinition> for StoredDefinition {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::StoredDefinition,
//     ) -> std::result::Result<Self, Self::Error> {
//         let mut def = StoredDefinition {
//             class_list: IndexMap::new(),
//             span: ast.span().clone(),
//             ..Default::default()
//         };
//         for class in &ast.stored_definition_list {
//             def.class_list.insert(
//                 class.class_definition.name.clone(),
//                 class.class_definition.clone(),
//             );
//         }
//         def.within = match &ast.stored_definition_opt {
//             Some(within) => match &within.stored_definition_opt1 {
//                 Some(within) => Some(within.name.clone()),
//                 None => None,
//             },
//             None => None,
//         };
//         Ok(def)
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ClassDefinition {
//     pub name: String,
//     pub encapsulated: bool,
//     pub equations: Vec<Equation>,
//     pub span: Span,
// }

// impl ToSpan for ClassDefinition {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ClassDefinition> for ClassDefinition {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ClassDefinition,
//     ) -> std::result::Result<Self, Self::Error> {
//         let mut def = ClassDefinition {
//             span: ast.span().clone(),
//             encapsulated: ast.class_definition_opt.is_some(),
//             ..Default::default()
//         };
//         match &ast.class_specifier {
//             ClassSpecifier::Long { name, .. } => {
//                 def.name = name.clone();
//             }
//             ClassSpecifier::Empty => {}
//         }

//         Ok(def)
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ClassPrefixes {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for ClassPrefixes {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ClassPrefixes> for ClassPrefixes {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ClassPrefixes,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ClassPrefixes {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub enum ClassSpecifier {
//     #[default]
//     Empty,
//     Long {
//         name: String,
//         composition: Vec<Composition>,
//         span: Span,
//     },
// }

// impl ToSpan for ClassSpecifier {
//     fn span(&self) -> Span {
//         match self {
//             ClassSpecifier::Empty => Span::default(),
//             ClassSpecifier::Long { span, .. } => span.clone(),
//         }
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ClassSpecifier> for ClassSpecifier {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ClassSpecifier,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ast.long_class_specifier.clone())
//     }
// }

// impl TryFrom<&modelica_grammar_trait::LongClassSpecifier> for ClassSpecifier {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::LongClassSpecifier,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ClassSpecifier::Long {
//             name: ast.name.name.clone(),
//             composition: vec![],
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Composition {
//     pub equations: Vec<Equation>,
//     pub span: Span,
// }

// impl ToSpan for Composition {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Composition> for Composition {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::Composition,
//     ) -> std::result::Result<Self, Self::Error> {
//         for comp in &ast.composition_list {
//             for eq in &comp.equation_section.equations {
//                 match eq {
//                     Equation::Assignment { .. } => {}
//                     Equation::Empty => {}
//                 }
//             }
//         }
//         Ok(Composition {
//             equations: vec![],
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ElementList {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for ElementList {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ElementList> for ElementList {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ElementList,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ElementList {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Element {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for Element {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Element> for Element {
//     type Error = anyhow::Error;

//     fn try_from(ast: &modelica_grammar_trait::Element) -> std::result::Result<Self, Self::Error> {
//         Ok(Element {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ComponentClause {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for ComponentClause {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ComponentClause> for ComponentClause {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ComponentClause,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ComponentClause {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ComponentList {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for ComponentList {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ComponentList> for ComponentList {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ComponentList,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ComponentList {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ComponentDeclaration {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for ComponentDeclaration {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ComponentDeclaration> for ComponentDeclaration {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ComponentDeclaration,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ComponentDeclaration {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Declaration {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for Declaration {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Declaration> for Declaration {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::Declaration,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(Declaration {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct EquationSection {
//     pub name: String,
//     pub private: bool,
//     pub equations: Vec<Equation>,
//     pub span: Span,
// }

// impl ToSpan for EquationSection {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::EquationSection> for EquationSection {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::EquationSection,
//     ) -> std::result::Result<Self, Self::Error> {
//         let mut def = EquationSection {
//             name: "".to_string(),
//             private: false,
//             equations: vec![],
//             span: ast.span().clone(),
//         };
//         for eq in &ast.equation_section_list {
//             def.equations.push(eq.some_equation.clone());
//         }
//         Ok(def)
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct TypePrefix {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for TypePrefix {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::TypePrefix> for TypePrefix {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::TypePrefix,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(TypePrefix {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct TypeSpecifier {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for TypeSpecifier {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::TypeSpecifier> for TypeSpecifier {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::TypeSpecifier,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(TypeSpecifier {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Ident {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for Ident {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Ident> for Ident {
//     type Error = anyhow::Error;

//     fn try_from(ast: &modelica_grammar_trait::Ident) -> std::result::Result<Self, Self::Error> {
//         Ok(Ident {
//             name: ast.ident.text().to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub enum Equation {
//     #[default]
//     Empty,
//     Assignment {
//         name: String,
//         span: Span,
//     },
// }

// impl ToSpan for Equation {
//     fn span(&self) -> Span {
//         match self {
//             Equation::Empty => Span::default(),
//             Equation::Assignment { span, .. } => span.clone(),
//         }
//     }
// }

// impl TryFrom<&modelica_grammar_trait::SomeEquation> for Equation {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::SomeEquation,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(Equation::Assignment {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

#[derive(Debug, Default, Clone)]
pub enum OpBinary {
    #[default]
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Eq,
    Neq,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    Exp,
    AddElem,
    SubElem,
    MulElem,
    DivElem,
}

#[derive(Debug, Default, Clone)]
pub enum OpUnary {
    #[default]
    Plus,
    Minus,
    Not,
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum Expression {
    #[default]
    Empty,
    Range {
        start: Box<Expression>,
        step: Box<Expression>,
        end: Box<Expression>,
        span: Span,
    },
    Unary {
        op: OpUnary,
        rhs: Box<Expression>,
        span: Span,
    },
    Binary {
        op: OpBinary,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
        span: Span,
    },
    UnsignedInteger {
        value: String,
        span: Span,
    },
    ComponentReference {
        comp: String,
        span: Span,
    },
}

impl ToSpan for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Range { span, .. } => span.clone(),
            Expression::Unary { span, .. } => span.clone(),
            Expression::Binary { span, .. } => span.clone(),
            Expression::UnsignedInteger { span, .. } => span.clone(),
            Expression::ComponentReference { span, .. } => span.clone(),
            Expression::Empty => Span::default(),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Primary> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        match ast {
            &modelica_grammar_trait::Primary::ComponentReference(ref comp_ref) => {
                Ok(comp_ref.component_reference.clone())
            }
            &modelica_grammar_trait::Primary::UnsignedInteger(ref unsigned_int) => {
                Ok(Expression::UnsignedInteger {
                    value: unsigned_int.unsigned_integer.unsigned_integer.text.clone(),
                    span: unsigned_int.span().clone(),
                })
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentReference> for Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentReference,
    ) -> std::result::Result<Self, Self::Error> {
        let mut comp = "".to_string();
        if ast.component_reference_opt.is_some() {
            comp += ".";
        }
        comp += &ast.ident.ident.text;
        for ident in &ast.component_reference_list {
            comp += &ident.ident.ident.text;
        }
        Ok(Expression::ComponentReference {
            comp,
            span: ast.span().clone(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::Factor> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Factor) -> std::result::Result<Self, Self::Error> {
        if ast.factor_list.is_empty() {
            return Ok(ast.primary.clone());
        } else {
            Ok(Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(ast.primary.clone().try_into()?),
                rhs: Box::new(ast.factor_list[0].primary.clone().try_into()?),
                span: ast.span().clone(),
            })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Term> for Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
        if ast.term_list.is_empty() {
            return Ok(ast.factor.clone());
        } else {
            let mut lhs = ast.factor.clone();
            for factor in &ast.term_list {
                lhs = Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match factor.mul_operator {
                        modelica_grammar_trait::MulOperator::Star(..) => OpBinary::Mul,
                        modelica_grammar_trait::MulOperator::Slash(..) => OpBinary::Div,
                        modelica_grammar_trait::MulOperator::DotSlash(..) => OpBinary::DivElem,
                        modelica_grammar_trait::MulOperator::DotStar(..) => OpBinary::MulElem,
                    },
                    rhs: Box::new(factor.factor.clone()),
                    span: ast.span().clone(),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArithmeticExpression,
    ) -> std::result::Result<Self, Self::Error> {
        // TODO unary term
        if ast.arithmetic_expression_list.is_empty() {
            return Ok(ast.term.clone());
        } else {
            let mut lhs = ast.term.clone();
            for term in &ast.arithmetic_expression_list {
                lhs = Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match term.add_operator {
                        modelica_grammar_trait::AddOperator::Plus(..) => OpBinary::Add,
                        modelica_grammar_trait::AddOperator::Minus(..) => OpBinary::Sub,
                        modelica_grammar_trait::AddOperator::DotPlus(..) => OpBinary::AddElem,
                        modelica_grammar_trait::AddOperator::DotMinus(..) => OpBinary::SubElem,
                    },
                    rhs: Box::new(term.term.clone()),
                    span: ast.span().clone(),
                };
            }
            Ok(lhs)
        }
    }
}

// impl TryFrom<&modelica_grammar_trait::SimpleExpression> for Expression {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::SimpleExpression,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(Expression::Assignment {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

// impl TryFrom<&modelica_grammar_trait::LogicalExpression> for Expression {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::LogicalExpression,
//     ) -> std::result::Result<Self, Self::Error> {
//         if ast.logical_expression_list.is_empty() {
//             return Ok(ast.logical_term.clone());
//         }
//         Ok(Expression::Or {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct LogicalExpression {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for LogicalExpression {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::LogicalExpression> for LogicalExpression {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::LogicalExpression,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(LogicalExpression {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct LogicalTerm {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for LogicalTerm {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::LogicalTerm> for LogicalTerm {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::LogicalTerm,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(LogicalTerm {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct LogicalFactor {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for LogicalFactor {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::LogicalFactor> for LogicalFactor {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::LogicalFactor,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(LogicalFactor {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Relation {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for Relation {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Relation> for Relation {
//     type Error = anyhow::Error;

//     fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
//         Ok(Relation {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct RelationalOperator {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for RelationalOperator {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::RelationalOperator> for RelationalOperator {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::RelationalOperator,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(RelationalOperator {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ArithmeticExpression {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for ArithmeticExpression {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for ArithmeticExpression {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ArithmeticExpression,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(ArithmeticExpression {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct AddOperator {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for AddOperator {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::AddOperator> for AddOperator {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::AddOperator,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(AddOperator {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Term {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for Term {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Term> for Term {
//     type Error = anyhow::Error;

//     fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
//         Ok(Term {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct MulOperator {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for MulOperator {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::MulOperator> for MulOperator {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::MulOperator,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(MulOperator {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Primary {
//     pub name: String,
//     pub span: Span,
// }

// impl ToSpan for Primary {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::Primary> for Primary {
//     type Error = anyhow::Error;

//     fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
//         Ok(Primary {
//             name: "".to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ComponentReference {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ComponentReference {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentReference> for ComponentReference {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentReference,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ComponentReference {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct UnsignedInteger {
//     pub value: String,
//     pub span: Span,
// }

// impl ToSpan for UnsignedInteger {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::UnsignedInteger> for UnsignedInteger {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::UnsignedInteger,
//     ) -> std::result::Result<Self, Self::Error> {
//         Ok(UnsignedInteger {
//             value: ast.unsigned_integer.text().to_string(),
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Name {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Name {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Name> for Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Name) -> std::result::Result<Self, Self::Error> {
        let mut name = ast.ident.ident.text.clone();
        for ident in &ast.name_list {
            name += ".";
            name += &ident.ident.ident.text;
        }
        Ok(Name {
            name,
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
///
/// Data structure that implements the semantic actions for our Modelica grammar
/// !Change this type as needed!
///
#[derive(Debug, Default)]
pub struct ModelicaGrammar<'t> {
    pub modelica: Option<modelica_grammar_trait::StoredDefinition>,
    _phantom: std::marker::PhantomData<&'t str>,
}

impl ModelicaGrammar<'_> {
    pub fn new() -> Self {
        ModelicaGrammar::default()
    }
}

impl<'t> Display for modelica_grammar_trait::StoredDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

impl Display for ModelicaGrammar<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        match &self.modelica {
            Some(modelica) => writeln!(f, "{}", modelica),
            None => write!(f, "No parse result"),
        }
    }
}

impl<'t> modelica_grammar_trait::ModelicaGrammarTrait for ModelicaGrammar<'t> {
    // !Adjust your implementation as needed!

    /// Semantic action for non-terminal 'Modelica'
    fn stored_definition(&mut self, arg: &modelica_grammar_trait::StoredDefinition) -> Result<()> {
        self.modelica = Some(arg.clone());
        Ok(())
    }
}
