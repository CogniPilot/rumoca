use crate::modelica_grammar_trait;
#[allow(unused_imports)]
use parol_runtime::{Location, Result, Span, ToSpan, Token};
use std::fmt::{Debug, Display, Error, Formatter};

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct StoredDefinition {
    pub class_list: String,
    pub span: Span,
}

impl ToSpan for StoredDefinition {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::AutoStoredDefinition> for StoredDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoStoredDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(StoredDefinition {
            class_list: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassDefinition {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ClassDefinition {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::AutoClassDefinition<'_>> for ClassDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoClassDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        // if let Some(encapsulted) = &ast.auto_class_definition_opt {
        //     encapsulted.encapsulated
        // }
        Ok(ClassDefinition {
            name: "".to_string(),
            span: ast.span().clone(),
        })
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

impl TryFrom<&modelica_grammar_trait::AutoClassPrefixes<'_>> for ClassPrefixes {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoClassPrefixes,
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

impl TryFrom<&modelica_grammar_trait::AutoClassType<'_>> for ClassType {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoClassType,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ClassType {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ClassSpecifier {
    pub name: String,
    pub span: Span,
}

impl ToSpan for ClassSpecifier {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::AutoClassSpecifier> for ClassSpecifier {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoClassSpecifier,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ClassSpecifier {
            name: "".to_string(),
            span: ast.span().clone(),
        })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct LongClassSpecifier {
    pub name: String,
    pub span: Span,
}

impl ToSpan for LongClassSpecifier {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl TryFrom<&modelica_grammar_trait::AutoLongClassSpecifier> for LongClassSpecifier {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoLongClassSpecifier,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(LongClassSpecifier {
            name: "".to_string(),
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

impl TryFrom<&modelica_grammar_trait::AutoComposition> for Composition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoComposition,
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

impl TryFrom<&modelica_grammar_trait::AutoElementList> for ElementList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoElementList,
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

impl TryFrom<&modelica_grammar_trait::AutoElement> for Element {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoElement,
    ) -> std::result::Result<Self, Self::Error> {
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

impl TryFrom<&modelica_grammar_trait::AutoComponentClause> for ComponentClause {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoComponentClause,
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

impl TryFrom<&modelica_grammar_trait::AutoComponentList> for ComponentList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoComponentList,
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

impl TryFrom<&modelica_grammar_trait::AutoComponentDeclaration> for ComponentDeclaration {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoComponentDeclaration,
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

impl TryFrom<&modelica_grammar_trait::AutoDeclaration> for Declaration {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoDeclaration,
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

impl TryFrom<&modelica_grammar_trait::AutoEquationSection> for EquationSection {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoEquationSection,
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

impl TryFrom<&modelica_grammar_trait::AutoTypePrefix<'_>> for TypePrefix {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoTypePrefix,
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

impl TryFrom<&modelica_grammar_trait::AutoTypeSpecifier<'_>> for TypeSpecifier {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoTypeSpecifier,
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

impl TryFrom<&modelica_grammar_trait::AutoIdent<'_>> for Ident {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::AutoIdent) -> std::result::Result<Self, Self::Error> {
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

impl TryFrom<&modelica_grammar_trait::AutoSomeEquation> for SomeEquation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoSomeEquation,
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

impl TryFrom<&modelica_grammar_trait::AutoSimpleExpression> for SimpleExpression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoSimpleExpression,
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

impl TryFrom<&modelica_grammar_trait::AutoLogicalExpression> for LogicalExpression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoLogicalExpression,
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

impl TryFrom<&modelica_grammar_trait::AutoLogicalTerm> for LogicalTerm {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoLogicalTerm,
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

impl TryFrom<&modelica_grammar_trait::AutoLogicalFactor<'_>> for LogicalFactor {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoLogicalFactor,
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

impl TryFrom<&modelica_grammar_trait::AutoRelation> for Relation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoRelation,
    ) -> std::result::Result<Self, Self::Error> {
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

impl TryFrom<&modelica_grammar_trait::AutoRelationalOperator<'_>> for RelationalOperator {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoRelationalOperator,
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

impl TryFrom<&modelica_grammar_trait::AutoArithmeticExpression> for ArithmeticExpression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoArithmeticExpression,
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

impl TryFrom<&modelica_grammar_trait::AutoAddOperator<'_>> for AddOperator {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoAddOperator,
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

impl TryFrom<&modelica_grammar_trait::AutoTerm> for Term {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::AutoTerm) -> std::result::Result<Self, Self::Error> {
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

impl TryFrom<&modelica_grammar_trait::AutoMulOperator<'_>> for MulOperator {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoMulOperator,
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

impl TryFrom<&modelica_grammar_trait::AutoFactor<'_>> for Factor {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoFactor,
    ) -> std::result::Result<Self, Self::Error> {
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

impl TryFrom<&modelica_grammar_trait::AutoPrimary> for Primary {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoPrimary,
    ) -> std::result::Result<Self, Self::Error> {
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

impl TryFrom<&modelica_grammar_trait::AutoComponentReference<'_>> for ComponentReference {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoComponentReference,
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

impl TryFrom<&modelica_grammar_trait::AutoUnsignedInteger<'_>> for UnsignedInteger {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::AutoUnsignedInteger,
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

impl TryFrom<&modelica_grammar_trait::AutoName> for Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::AutoName) -> std::result::Result<Self, Self::Error> {
        Ok(Name {
            name: "".to_string(),
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
        self.modelica = Some(arg.auto_stored_definition.clone());
        Ok(())
    }
}
