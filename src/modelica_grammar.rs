use crate::ir;
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
// impl ToSpan for ir::StoredDefinition {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::StoredDefinition> for ir::StoredDefinition {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::StoredDefinition,
//     ) -> std::result::Result<Self, Self::Error> {
//         let mut def = ir::StoredDefinition {
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
// impl ToSpan for ir::ClassDefinition {
//     fn span(&self) -> Span {
//         self.span.clone()
//     }
// }

// impl TryFrom<&modelica_grammar_trait::ClassDefinition> for ir::ClassDefinition {
//     type Error = anyhow::Error;

//     fn try_from(
//         ast: &modelica_grammar_trait::ClassDefinition,
//     ) -> std::result::Result<Self, Self::Error> {
//         let mut def = ir::ClassDefinition {
//             span: ast.span().clone(),
//             encapsulated: ast.class_definition_opt.is_some(),
//             ..Default::default()
//         };
//         match &ast.class_specifier {
//             ClassSpecifier::Empty => {}
//             ClassSpecifier::Long {
//                 name, composition, ..
//             } => {
//                 def.name = name.clone();
//                 for comp in composition {
//                     def.equations.append(&mut comp.equations.clone());
//                 }
//             }
//         }
//         Ok(def)
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
//             name: ast.name.ident.text.clone(),
//             composition: vec![],
//             span: ast.span().clone(),
//         })
//     }
// }

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Composition {
//     pub equations: Vec<ir::Equation>,
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
//                     ir::Equation::Assignment { .. } => {}
//                     ir::Equation::Empty => {}
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
impl ToSpan for ir::EquationSection {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::EquationSection> for ir::EquationSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationSection,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = ir::EquationSection {
            private: false,
            equations: vec![],
            span: ast.span().clone(),
        };
        for eq in &ast.equation_section_list {
            def.equations.push(eq.some_equation.clone());
        }
        Ok(def)
    }
}

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

impl ToSpan for ir::Equation {
    fn span(&self) -> Span {
        match self {
            ir::Equation::Empty => Span::default(),
            ir::Equation::Assignment { span, .. } => span.clone(),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::SomeEquation> for ir::Equation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SomeEquation,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::Equation::Assignment {
            lhs: ast.simple_expression.clone(),
            rhs: ast.expression.clone(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------

impl ToSpan for ir::Expression {
    fn span(&self) -> Span {
        match self {
            ir::Expression::Range { span, .. } => span.clone(),
            ir::Expression::Unary { span, .. } => span.clone(),
            ir::Expression::Binary { span, .. } => span.clone(),
            ir::Expression::UnsignedInteger { span, .. } => span.clone(),
            ir::Expression::UnsignedReal { span, .. } => span.clone(),
            ir::Expression::ComponentReference { span, .. } => span.clone(),
            ir::Expression::Empty => Span::default(),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Primary> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        match ast {
            &modelica_grammar_trait::Primary::ComponentReference(ref comp_ref) => {
                Ok(comp_ref.component_reference.clone())
            }
            &modelica_grammar_trait::Primary::UnsignedNumber(ref unsigned_num) => {
                match unsigned_num.unsigned_number {
                    modelica_grammar_trait::UnsignedNumber::UnsignedInteger(ref unsigned_int) => {
                        Ok(ir::Expression::UnsignedInteger {
                            value: unsigned_int.unsigned_integer.unsigned_integer.text.clone(),
                            span: unsigned_int.span().clone(),
                        })
                    }
                    modelica_grammar_trait::UnsignedNumber::UnsignedReal(ref unsigned_real) => {
                        Ok(ir::Expression::UnsignedReal {
                            value: match unsigned_real.unsigned_real {
                                modelica_grammar_trait::UnsignedReal::Decimal(ref num) => {
                                    num.decimal.text.clone()
                                }
                                modelica_grammar_trait::UnsignedReal::Scientific(ref num) => {
                                    num.scientific.text.clone()
                                }
                                modelica_grammar_trait::UnsignedReal::Scientific2(ref num) => {
                                    num.scientific2.text.clone()
                                }
                            },
                            span: unsigned_real.span().clone(),
                        })
                    }
                }
                // Ok(ir::Expression::UnsignedInteger {
                //     value: unsigned_int.unsigned_integer.unsigned_integer.text.clone(),
                //     span: unsigned_int.span().clone(),
                // })
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentReference> for ir::Expression {
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
        Ok(ir::Expression::ComponentReference {
            comp,
            span: ast.span().clone(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::Factor> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Factor) -> std::result::Result<Self, Self::Error> {
        if ast.factor_list.is_empty() {
            return Ok(ast.primary.clone());
        } else {
            Ok(ir::Expression::Binary {
                op: ir::OpBinary::Mul,
                lhs: Box::new(ast.primary.clone()),
                rhs: Box::new(ast.factor_list[0].primary.clone()),
                span: ast.span().clone(),
            })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Term> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
        if ast.term_list.is_empty() {
            return Ok(ast.factor.clone());
        } else {
            let mut lhs = ast.factor.clone();
            for factor in &ast.term_list {
                lhs = ir::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match factor.mul_operator {
                        modelica_grammar_trait::MulOperator::Star(..) => ir::OpBinary::Mul,
                        modelica_grammar_trait::MulOperator::Slash(..) => ir::OpBinary::Div,
                        modelica_grammar_trait::MulOperator::DotSlash(..) => ir::OpBinary::DivElem,
                        modelica_grammar_trait::MulOperator::DotStar(..) => ir::OpBinary::MulElem,
                    },
                    rhs: Box::new(factor.factor.clone()),
                    span: ast.span().clone(),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for ir::Expression {
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
                lhs = ir::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match term.add_operator {
                        modelica_grammar_trait::AddOperator::Plus(..) => ir::OpBinary::Add,
                        modelica_grammar_trait::AddOperator::Minus(..) => ir::OpBinary::Sub,
                        modelica_grammar_trait::AddOperator::DotPlus(..) => ir::OpBinary::AddElem,
                        modelica_grammar_trait::AddOperator::DotMinus(..) => ir::OpBinary::SubElem,
                    },
                    rhs: Box::new(term.term.clone()),
                    span: ast.span().clone(),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Relation> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
        match &ast.relation_opt {
            Some(relation) => Ok(ir::Expression::Binary {
                lhs: Box::new(ast.arithmetic_expression.clone()),
                op: match relation.relational_operator {
                    modelica_grammar_trait::RelationalOperator::EquEqu(..) => ir::OpBinary::Eq,
                    modelica_grammar_trait::RelationalOperator::GT(..) => ir::OpBinary::Gt,
                    modelica_grammar_trait::RelationalOperator::LT(..) => ir::OpBinary::Lt,
                    modelica_grammar_trait::RelationalOperator::GTEqu(..) => ir::OpBinary::Ge,
                    modelica_grammar_trait::RelationalOperator::LTEqu(..) => ir::OpBinary::Le,
                    modelica_grammar_trait::RelationalOperator::LTGT(..) => ir::OpBinary::Neq,
                },
                rhs: Box::new(relation.arithmetic_expression.clone()),
                span: ast.span().clone(),
            }),
            None => Ok(ast.arithmetic_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalFactor> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalFactor,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_factor_opt.is_some() {
            Ok(ir::Expression::Unary {
                op: ir::OpUnary::Not,
                rhs: Box::new(ast.relation.clone()),
                span: ast.span().clone(),
            })
        } else {
            Ok(ast.relation.clone())
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalTerm> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalTerm,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_term_list.is_empty() {
            return Ok(ast.logical_factor.clone());
        } else {
            let mut lhs = ast.logical_factor.clone();
            for term in &ast.logical_term_list {
                lhs = ir::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: ir::OpBinary::And,
                    rhs: Box::new(term.logical_factor.clone()),
                    span: ast.span().clone(),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalExpression> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalExpression,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_expression_list.is_empty() {
            return Ok(ast.logical_term.clone());
        } else {
            let mut lhs = ast.logical_term.clone();
            for term in &ast.logical_expression_list {
                lhs = ir::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: ir::OpBinary::Or,
                    rhs: Box::new(term.logical_term.clone()),
                    span: ast.span().clone(),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::SimpleExpression> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SimpleExpression,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.simple_expression_opt {
            Some(opt) => match &opt.simple_expression_opt0 {
                Some(opt0) => Ok(ir::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: Some(Box::new(opt.logical_expression.clone())),
                    end: Box::new(opt0.logical_expression.clone()),
                    span: ast.span(),
                }),
                None => Ok(ir::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: None,
                    end: Box::new(opt.logical_expression.clone()),
                    span: ast.span(),
                }),
            },
            None => Ok(ast.logical_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Expression> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Expression,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ast.simple_expression.clone())
    }
}

//-----------------------------------------------------------------------------
impl ToSpan for ir::ComponentReference {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentReference> for ir::ComponentReference {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentReference,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ComponentReference {
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
impl ToSpan for ir::Name {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Name> for ir::Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Name) -> std::result::Result<Self, Self::Error> {
        let mut name = ast.ident.ident.text.clone();
        for ident in &ast.name_list {
            name += ".";
            name += &ident.ident.ident.text;
        }
        Ok(ir::Name {
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
