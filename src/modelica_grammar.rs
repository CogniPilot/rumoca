use crate::ir;
use crate::modelica_grammar_trait;
#[allow(unused_imports)]
use parol_runtime::{Location, Result, Span, ToSpan, Token};
use std::fmt::{Debug, Display, Error, Formatter};

//-----------------------------------------------------------------------------
impl TryFrom<&Token<'_>> for ir::Token {
    type Error = anyhow::Error;

    fn try_from(value: &Token<'_>) -> std::result::Result<Self, Self::Error> {
        Ok(ir::Token {
            text: value.text().to_string(),
            location: value.location.clone(),
            token_number: value.token_number,
            token_type: value.token_type,
        })
    }
}

//-----------------------------------------------------------------------------
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
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Composition {
    pub elements: Vec<modelica_grammar_trait::Element>,
    pub protected_elements: Vec<modelica_grammar_trait::Element>,
    pub equations: Vec<ir::Equation>,
    pub initial_equations: Vec<ir::Equation>,
    pub algorithms: Vec<Vec<ir::Statement>>,
    pub initial_algorithms: Vec<Vec<ir::Statement>>,
}

impl TryFrom<&modelica_grammar_trait::Composition> for Composition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Composition,
    ) -> std::result::Result<Self, Self::Error> {
        let mut comp = Composition {
            ..Default::default()
        };
        for comp_list in &ast.composition_list {
            match comp_list.composition_list_group {
                modelica_grammar_trait::CompositionListGroup::PublicElementList(ref elem_list) => {
                    for _elem in &elem_list.element_list.elements {}
                }
                modelica_grammar_trait::CompositionListGroup::ProtectedElementList(
                    ref _elem_list,
                ) => {}
                modelica_grammar_trait::CompositionListGroup::EquationSection(ref eq_sec) => {
                    let sec = &eq_sec.equation_section;
                    for eq in &sec.equations {
                        if sec.initial {
                            comp.initial_equations.push(eq.clone());
                        } else {
                            comp.equations.push(eq.clone());
                        }
                    }
                }
                modelica_grammar_trait::CompositionListGroup::AlgorithmSection(ref alg_sec) => {
                    let sec = &alg_sec.algorithm_section;
                    let mut algo = vec![];
                    for stmt in &sec.statements {
                        algo.push(stmt.clone());
                    }
                    if sec.initial {
                        comp.initial_algorithms.push(algo);
                    } else {
                        comp.algorithms.push(algo);
                    }
                }
            }
        }
        Ok(comp)
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ElementList {
    pub elements: Vec<modelica_grammar_trait::Element>,
}

impl TryFrom<&modelica_grammar_trait::ElementList> for ElementList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ElementList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut elements = Vec::new();
        for elem in &ast.element_list_list {
            elements.push(elem.element.clone());
        }
        Ok(ElementList { elements })
    }
}

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct Element {
//     pub name: String,
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
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ComponentList {
    pub components: Vec<modelica_grammar_trait::ComponentDeclaration>,
}

impl TryFrom<&modelica_grammar_trait::ComponentList> for ComponentList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut components = vec![ast.component_declaration.clone()];
        for comp in &ast.component_list_list {
            components.push(comp.component_declaration.clone());
        }
        Ok(ComponentList { components })
    }
}

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct ComponentDeclaration {
//     pub name: String,
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
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct EquationSection {
    pub initial: bool,
    pub equations: Vec<ir::Equation>,
}

impl TryFrom<&modelica_grammar_trait::EquationSection> for EquationSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationSection,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = EquationSection {
            initial: ast.equation_section_opt.is_some(),
            equations: vec![],
        };
        for eq in &ast.equation_section_list {
            def.equations.push(eq.some_equation.clone());
        }
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct AlgorithmSection {
    pub initial: bool,
    pub statements: Vec<ir::Statement>,
}

impl TryFrom<&modelica_grammar_trait::AlgorithmSection> for AlgorithmSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AlgorithmSection,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = AlgorithmSection {
            initial: ast.algorithm_section_opt.is_some(),
            statements: vec![],
        };
        for alg in &ast.algorithm_section_list {
            def.statements.push(alg.statement.clone());
        }
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct TypePrefix {
//     pub name: String,
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
impl TryFrom<&modelica_grammar_trait::Ident> for ir::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Ident) -> std::result::Result<Self, Self::Error> {
        Ok(ir::Token {
            location: ast.ident.location.clone(),
            text: ast.ident.text.clone(),
            token_number: ast.ident.token_number,
            token_type: ast.ident.token_type,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::UnsignedInteger> for ir::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedInteger,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::Token {
            location: ast.unsigned_integer.location.clone(),
            text: ast.unsigned_integer.text.clone(),
            token_number: ast.unsigned_integer.token_number,
            token_type: ast.unsigned_integer.token_type,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::UnsignedReal> for ir::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedReal,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::UnsignedReal::Decimal(num) => Ok(num.decimal.clone()),
            modelica_grammar_trait::UnsignedReal::Scientific(num) => Ok(num.scientific.clone()),
            modelica_grammar_trait::UnsignedReal::Scientific2(num) => Ok(num.scientific2.clone()),
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::SomeEquation> for ir::Equation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SomeEquation,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::Equation::Simple {
            lhs: ast.simple_expression.clone(),
            rhs: ast.expression.clone(),
            node: ir::NodeData::new(),
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Statement> for ir::Statement {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Statement) -> std::result::Result<Self, Self::Error> {
        Ok(ir::Statement::Assignment {
            comp: ast.component_reference.clone(),
            value: ast.expression.clone(),
            node: ir::NodeData::new(),
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Primary> for ir::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::Primary::ComponentReference(comp_ref) => Ok(
                ir::Expression::ComponentReference(comp_ref.component_reference.clone()),
            ),
            modelica_grammar_trait::Primary::UnsignedNumber(unsigned_num) => {
                match &unsigned_num.unsigned_number {
                    modelica_grammar_trait::UnsignedNumber::UnsignedInteger(unsigned_int) => {
                        Ok(ir::Expression::UnsignedInteger {
                            value: unsigned_int.unsigned_integer.clone(),
                            node: ir::NodeData::new(),
                        })
                    }
                    modelica_grammar_trait::UnsignedNumber::UnsignedReal(unsigned_real) => {
                        Ok(ir::Expression::UnsignedReal {
                            value: unsigned_real.unsigned_real.clone(),
                            node: ir::NodeData::new(),
                        })
                    }
                }
            }
            modelica_grammar_trait::Primary::String(string) => Ok(ir::Expression::String {
                value: string.string.string.clone(),
                node: ir::NodeData::new(),
            }),
            modelica_grammar_trait::Primary::True(bool) => Ok(ir::Expression::Bool {
                value: bool.r#true.r#true.clone(),
                node: ir::NodeData::new(),
            }),
            modelica_grammar_trait::Primary::False(bool) => Ok(ir::Expression::Bool {
                value: bool.r#false.r#false.clone(),
                node: ir::NodeData::new(),
            }),
            modelica_grammar_trait::Primary::End(end) => Ok(ir::Expression::End {
                value: end.end.end.clone(),
                node: ir::NodeData::new(),
            }),
        }
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
                node: ir::NodeData::new(),
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
                    node: ir::NodeData::new(),
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
                    node: ir::NodeData::new(),
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
                node: ir::NodeData::new(),
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
                node: ir::NodeData::new(),
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
                    node: ir::NodeData::new(),
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
                    node: ir::NodeData::new(),
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
                    node: ir::NodeData::new(),
                }),
                None => Ok(ir::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: None,
                    end: Box::new(opt.logical_expression.clone()),
                    node: ir::NodeData::new(),
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
impl TryFrom<&modelica_grammar_trait::ComponentReference> for ir::ComponentReference {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentReference,
    ) -> std::result::Result<Self, Self::Error> {
        let mut comp = Vec::new();
        comp.push(ast.ident.clone());
        for comp_ref in &ast.component_reference_list {
            comp.push(comp_ref.ident.clone());
        }
        Ok(ir::ComponentReference {
            name: comp,
            node: ir::NodeData::new(),
        })
    }
}

//-----------------------------------------------------------------------------
// #[derive(Debug, Default, Clone)]
// #[allow(unused)]
// pub struct UnsignedInteger {
//     pub value: String,
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
impl TryFrom<&modelica_grammar_trait::Name> for ir::Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Name) -> std::result::Result<Self, Self::Error> {
        let mut name = vec![ast.ident.clone()];
        for ident in &ast.name_list {
            name.push(ident.ident.clone());
        }
        Ok(ir::Name {
            name,
            node: ir::NodeData::new(),
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
