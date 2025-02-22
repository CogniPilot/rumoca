use crate::modelica_grammar_trait;
use indexmap::IndexMap;
#[allow(unused_imports)]
use parol_runtime::{Location, Result, Span, ToSpan, Token};
use std::fmt::{Debug, Display, Error, Formatter};

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct StoredDefinition {
    pub class_list: IndexMap<String, ClassDefinition>,
    pub within: Option<String>,
    pub span: Span,
}

impl ToSpan for StoredDefinition {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::StoredDefinition> for StoredDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::StoredDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = StoredDefinition {
            class_list: IndexMap::new(),
            span: ast.span().clone(),
            ..Default::default()
        };
        for class in &ast.stored_definition_list {
            def.class_list.insert(
                class.class_definition.name.clone(),
                class.class_definition.clone(),
            );
        }
        def.within = match &ast.stored_definition_opt {
            Some(within) => match &within.stored_definition_opt0 {
                Some(within) => Some(within.name.name.clone()),
                None => None,
            },
            None => None,
        };
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassDefinition {
    pub name: String,
    pub encapsulated: bool,
    pub span: Span,
}

impl ToSpan for ClassDefinition {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ClassDefinition<'_>> for ClassDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ClassDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = ClassDefinition {
            name: "".to_string(),
            span: ast.span().clone(),
            encapsulated: ast.class_definition_opt.is_some(),
        };
        match &ast.class_specifier {
            ClassSpecifier::Long { name, .. } => {
                def.name = name.clone();
            }
            ClassSpecifier::Empty => {}
        }

        Ok(def)
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassPrefixes {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ClassPrefixes {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ClassPrefixes<'_>> for ClassPrefixes {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ClassPrefixes,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ClassPrefixes {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassType {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ClassType {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ClassType<'_>> for ClassType {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::ClassType) -> std::result::Result<Self, Self::Error> {
        Ok(ClassType {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub enum ClassSpecifier {
    #[default]
    Empty,
    Long {
        name: String,
        span: Span,
    },
}

impl ToSpan for ClassSpecifier {
    fn span(&self) -> Span {
        match self {
            ClassSpecifier::Empty => Span::default(),
            ClassSpecifier::Long { span, .. } => span.clone(),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ClassSpecifier> for ClassSpecifier {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ClassSpecifier,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ClassSpecifier::Long {
            name: ast.long_class_specifier.ident.name.clone(),
            span: ast.span().clone(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::LongClassSpecifier> for ClassSpecifier {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LongClassSpecifier,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ClassSpecifier::Long {
            name: ast.ident.name.clone(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Composition {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Composition {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Composition> for Composition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Composition,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(Composition {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ElementList {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ElementList {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ElementList> for ElementList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ElementList,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ElementList {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Element {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Element {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Element> for Element {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Element) -> std::result::Result<Self, Self::Error> {
        Ok(Element {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ComponentClause {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ComponentClause {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentClause> for ComponentClause {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentClause,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ComponentClause {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ComponentList {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ComponentList {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentList> for ComponentList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentList,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ComponentList {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ComponentDeclaration {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ComponentDeclaration {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ComponentDeclaration> for ComponentDeclaration {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentDeclaration,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ComponentDeclaration {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Declaration {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Declaration {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Declaration> for Declaration {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Declaration,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(Declaration {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct EquationSection {
    pub name: String,
    pub span: Span,
}

impl ToSpan for EquationSection {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::EquationSection> for EquationSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationSection,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(EquationSection {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct TypePrefix {
    pub name: String,
    pub span: Span,
}

impl ToSpan for TypePrefix {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::TypePrefix<'_>> for TypePrefix {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::TypePrefix,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(TypePrefix {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct TypeSpecifier {
    pub name: String,
    pub span: Span,
}

impl ToSpan for TypeSpecifier {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::TypeSpecifier<'_>> for TypeSpecifier {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::TypeSpecifier,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(TypeSpecifier {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Ident {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Ident<'_>> for Ident {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Ident) -> std::result::Result<Self, Self::Error> {
        Ok(Ident {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct SomeEquation {
    pub name: String,
    pub span: Span,
}

impl ToSpan for SomeEquation {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::SomeEquation> for SomeEquation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SomeEquation,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(SomeEquation {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct SimpleExpression {
    pub name: String,
    pub span: Span,
}

impl ToSpan for SimpleExpression {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::SimpleExpression> for SimpleExpression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SimpleExpression,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(SimpleExpression {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct LogicalExpression {
    pub name: String,
    pub span: Span,
}

impl ToSpan for LogicalExpression {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalExpression> for LogicalExpression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalExpression,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(LogicalExpression {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct LogicalTerm {
    pub name: String,
    pub span: Span,
}

impl ToSpan for LogicalTerm {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalTerm> for LogicalTerm {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalTerm,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(LogicalTerm {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct LogicalFactor {
    pub name: String,
    pub span: Span,
}

impl ToSpan for LogicalFactor {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalFactor<'_>> for LogicalFactor {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalFactor,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(LogicalFactor {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Relation {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Relation {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Relation> for Relation {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
        Ok(Relation {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct RelationalOperator {
    pub name: String,
    pub span: Span,
}

impl ToSpan for RelationalOperator {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::RelationalOperator<'_>> for RelationalOperator {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::RelationalOperator,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(RelationalOperator {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ArithmeticExpression {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ArithmeticExpression {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for ArithmeticExpression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArithmeticExpression,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ArithmeticExpression {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct AddOperator {
    pub name: String,
    pub span: Span,
}

impl ToSpan for AddOperator {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::AddOperator<'_>> for AddOperator {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AddOperator,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(AddOperator {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Term {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Term {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Term> for Term {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
        Ok(Term {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct MulOperator {
    pub name: String,
    pub span: Span,
}

impl ToSpan for MulOperator {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::MulOperator<'_>> for MulOperator {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::MulOperator,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(MulOperator {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Factor {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Factor {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Factor<'_>> for Factor {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Factor) -> std::result::Result<Self, Self::Error> {
        Ok(Factor {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Primary {
    pub name: String,
    pub span: Span,
}

impl ToSpan for Primary {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::Primary> for Primary {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        Ok(Primary {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

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

impl TryFrom<&modelica_grammar_trait::ComponentReference<'_>> for ComponentReference {
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
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct UnsignedInteger {
    pub name: String,
    pub span: Span,
}

impl ToSpan for UnsignedInteger {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::UnsignedInteger<'_>> for UnsignedInteger {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedInteger,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(UnsignedInteger {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

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
        Ok(Name {
            name: ast.ident.name.clone(),
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
    pub modelica: Option<StoredDefinition>,
    _phantom: std::marker::PhantomData<&'t str>,
}

impl ModelicaGrammar<'_> {
    pub fn new() -> Self {
        ModelicaGrammar::default()
    }
}

impl<'t> Display for StoredDefinition {
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

impl<'t> modelica_grammar_trait::ModelicaGrammarTrait<'t> for ModelicaGrammar<'t> {
    // !Adjust your implementation as needed!

    /// Semantic action for non-terminal 'Modelica'
    fn stored_definition(&mut self, arg: &modelica_grammar_trait::StoredDefinition) -> Result<()> {
        self.modelica = Some(arg.try_into()?);
        Ok(())
    }
}
