//! This module provides implementations for converting the automatic Parol AST
//! (Abstract Syntax Tree) into the internal AST representation (`crate::ir::ast`).
//!
//! The module includes various `TryFrom` implementations for converting between
//! the `modelica_grammar_trait` types and the internal `ir::ast` types. These
//! conversions handle different Modelica constructs such as stored definitions,
//! class definitions, equations, statements, expressions, and more.
//!
//! # Key Features
//!
//! - **StoredDefinition Conversion**: Converts a `StoredDefinition` from the
//!   `modelica_grammar_trait` to the internal `ir::ast::StoredDefinition`.
//! - **Token Conversion**: Converts tokens from the Parol runtime to the internal
//!   `ir::ast::Token` representation.
//! - **ClassDefinition Conversion**: Handles the conversion of Modelica class
//!   definitions, including long and short class specifiers.
//! - **Composition and ElementList**: Converts Modelica compositions and element
//!   lists into their internal representations.
//! - **Equation and Algorithm Sections**: Converts Modelica equation and algorithm
//!   sections, including initial and non-initial variants.
//! - **Expressions and Statements**: Provides detailed conversions for Modelica
//!   expressions and statements, including binary, unary, and function call expressions.
//! - **Component References**: Converts Modelica component references and their parts
//!   into the internal representation.
//!
//! # Notes
//!
//! - Some features, such as `extends`, `der`, `enum class specifier`, and others,
//!   are marked as `todo!` and are not yet implemented.
//! - The module uses `anyhow::Error` for error handling during conversions.
//! - Default values are provided for certain constructs, such as default start values
//!   for components based on their type.
//!
//! # Example Usage
//!
//! This module is primarily used internally by the `ModelicaGrammar` struct, which
//! implements the `modelica_grammar_trait::ModelicaGrammarTrait` trait. The `stored_definition`
//! method is used to parse and store the converted Modelica AST.

// Disable clippy warnings that can result from auto-generated Display implementations
#![allow(clippy::extra_unused_lifetimes)]

use crate::ir;
use crate::modelica_grammar_trait;
use indexmap::IndexMap;
use parol_runtime::{Result, Token};
use std::fmt::{Debug, Display, Error, Formatter};

/// Helper to format location info from a token for error messages
fn loc_info(token: &ir::ast::Token) -> String {
    let loc = &token.location;
    format!(
        " at {}:{}:{}",
        loc.file_name, loc.start_line, loc.start_column
    )
}

/// Helper to format location info from an expression
fn expr_loc_info(expr: &ir::ast::Expression) -> String {
    expr.get_location()
        .map(|loc| {
            format!(
                " at {}:{}:{}",
                loc.file_name, loc.start_line, loc.start_column
            )
        })
        .unwrap_or_default()
}

/// Helper to collect elements from array_arguments into a Vec<Expression>
/// Handles both simple arrays like {1, 2, 3} and nested arrays like {{1, 2}, {3, 4}}
fn collect_array_elements(
    args: &modelica_grammar_trait::ArrayArguments,
) -> anyhow::Result<Vec<ir::ast::Expression>> {
    let mut elements = Vec::new();

    // First element
    elements.push((*args.expression).clone());

    // Collect remaining elements from the optional chain
    if let Some(opt) = &args.array_arguments_opt {
        match &opt.array_arguments_opt_group {
            modelica_grammar_trait::ArrayArgumentsOptGroup::CommaArrayArgumentsNonFirst(
                comma_args,
            ) => {
                collect_array_non_first(&comma_args.array_arguments_non_first, &mut elements);
            }
            modelica_grammar_trait::ArrayArgumentsOptGroup::ForForIndices(_for_indices) => {
                // Array comprehension like {i for i in 1:10} - not yet supported
                anyhow::bail!(
                    "Array comprehension with 'for' is not yet supported{}",
                    expr_loc_info(&args.expression)
                );
            }
        }
    }

    Ok(elements)
}

/// Helper to recursively collect elements from array_arguments_non_first chain
fn collect_array_non_first(
    args: &modelica_grammar_trait::ArrayArgumentsNonFirst,
    elements: &mut Vec<ir::ast::Expression>,
) {
    // Add current element
    elements.push(args.expression.clone());

    // Recursively collect remaining elements
    if let Some(opt) = &args.array_arguments_non_first_opt {
        collect_array_non_first(&opt.array_arguments_non_first, elements);
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::StoredDefinition> for ir::ast::StoredDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::StoredDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = ir::ast::StoredDefinition {
            class_list: IndexMap::new(),
            ..Default::default()
        };
        for class in &ast.stored_definition_list {
            def.class_list.insert(
                class.class_definition.name.text.clone(),
                class.class_definition.clone(),
            );
        }
        def.within = match &ast.stored_definition_opt {
            Some(within) => within
                .stored_definition_opt1
                .as_ref()
                .map(|within| within.name.clone()),
            None => None,
        };
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&Token<'_>> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(value: &Token<'_>) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::Token {
            text: value.text().to_string(),
            location: ir::ast::Location {
                start_line: value.location.start_line,
                start_column: value.location.start_column,
                end_line: value.location.end_line,
                end_column: value.location.end_column,
                start: value.location.start,
                end: value.location.end,
                file_name: value
                    .location
                    .file_name
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
            },
            token_number: value.token_number,
            token_type: value.token_type,
        })
    }
}

/// Convert grammar ClassType to IR ClassType
fn convert_class_type(class_type: &modelica_grammar_trait::ClassType) -> ir::ast::ClassType {
    match class_type {
        modelica_grammar_trait::ClassType::Class(_) => ir::ast::ClassType::Class,
        modelica_grammar_trait::ClassType::Model(_) => ir::ast::ClassType::Model,
        modelica_grammar_trait::ClassType::ClassTypeOptRecord(_) => ir::ast::ClassType::Record,
        modelica_grammar_trait::ClassType::Block(_) => ir::ast::ClassType::Block,
        modelica_grammar_trait::ClassType::ClassTypeOpt0Connector(_) => {
            ir::ast::ClassType::Connector
        }
        modelica_grammar_trait::ClassType::Type(_) => ir::ast::ClassType::Type,
        modelica_grammar_trait::ClassType::Package(_) => ir::ast::ClassType::Package,
        modelica_grammar_trait::ClassType::ClassTypeOpt1ClassTypeOpt2Function(_) => {
            ir::ast::ClassType::Function
        }
        modelica_grammar_trait::ClassType::Operator(_) => ir::ast::ClassType::Operator,
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ClassDefinition> for ir::ast::ClassDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ClassDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let class_type = convert_class_type(&ast.class_prefixes.class_type);
        match &ast.class_specifier {
            modelica_grammar_trait::ClassSpecifier::LongClassSpecifier(long) => {
                match &long.long_class_specifier {
                    modelica_grammar_trait::LongClassSpecifier::StandardClassSpecifier(
                        class_specifier,
                    ) => {
                        let spec = &class_specifier.standard_class_specifier;
                        Ok(ir::ast::ClassDefinition {
                            name: spec.name.clone(),
                            class_type,
                            extends: spec.composition.extends.clone(),
                            imports: spec.composition.imports.clone(),
                            classes: spec.composition.classes.clone(),
                            equations: spec.composition.equations.clone(),
                            algorithms: spec.composition.algorithms.clone(),
                            initial_equations: spec.composition.initial_equations.clone(),
                            initial_algorithms: spec.composition.initial_algorithms.clone(),
                            components: spec.composition.components.clone(),
                            encapsulated: ast.class_definition_opt.is_some(),
                        })
                    }
                    modelica_grammar_trait::LongClassSpecifier::ExtendsClassSpecifier(ext) => {
                        anyhow::bail!(
                            "'extends' class specifier is not yet supported{}",
                            loc_info(&ext.extends_class_specifier.ident)
                        )
                    }
                }
            }
            modelica_grammar_trait::ClassSpecifier::DerClassSpecifier(spec) => {
                anyhow::bail!(
                    "'der' class specifier is not yet supported{}",
                    loc_info(&spec.der_class_specifier.ident)
                )
            }
            modelica_grammar_trait::ClassSpecifier::ShortClassSpecifier(short) => {
                match &short.short_class_specifier {
                    modelica_grammar_trait::ShortClassSpecifier::EnumClassSpecifier(spec) => {
                        anyhow::bail!(
                            "'enumeration' class specifier is not yet supported{}",
                            loc_info(&spec.enum_class_specifier.ident)
                        )
                    }
                    modelica_grammar_trait::ShortClassSpecifier::TypeClassSpecifier(spec) => {
                        // type MyType = BaseType "description";
                        // Creates a class that extends the base type
                        let type_spec = &spec.type_class_specifier;
                        let base_type_name = type_spec.type_specifier.name.clone();

                        // Create an Extend clause for the base type
                        let extend = ir::ast::Extend {
                            comp: base_type_name,
                        };

                        Ok(ir::ast::ClassDefinition {
                            name: type_spec.ident.clone(),
                            class_type,
                            extends: vec![extend],
                            imports: vec![],
                            classes: IndexMap::new(),
                            equations: vec![],
                            algorithms: vec![],
                            initial_equations: vec![],
                            initial_algorithms: vec![],
                            components: IndexMap::new(),
                            encapsulated: ast.class_definition_opt.is_some(),
                        })
                    }
                }
            }
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct Composition {
    pub extends: Vec<ir::ast::Extend>,
    pub imports: Vec<ir::ast::Import>,
    pub components: IndexMap<String, ir::ast::Component>,
    pub classes: IndexMap<String, ir::ast::ClassDefinition>,
    pub equations: Vec<ir::ast::Equation>,
    pub initial_equations: Vec<ir::ast::Equation>,
    pub algorithms: Vec<Vec<ir::ast::Statement>>,
    pub initial_algorithms: Vec<Vec<ir::ast::Statement>>,
}

impl TryFrom<&modelica_grammar_trait::Composition> for Composition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Composition,
    ) -> std::result::Result<Self, Self::Error> {
        let mut comp = Composition {
            ..Default::default()
        };

        comp.components = ast.element_list.components.clone();
        comp.classes = ast.element_list.classes.clone();
        comp.extends = ast.element_list.extends.clone();
        comp.imports = ast.element_list.imports.clone();

        for comp_list in &ast.composition_list {
            match &comp_list.composition_list_group {
                modelica_grammar_trait::CompositionListGroup::PublicElementList(elem_list) => {
                    anyhow::bail!(
                        "'public' element list is not yet supported{}",
                        loc_info(&elem_list.public.public)
                    )
                }
                modelica_grammar_trait::CompositionListGroup::ProtectedElementList(elem_list) => {
                    anyhow::bail!(
                        "'protected' element list is not yet supported{}",
                        loc_info(&elem_list.protected.protected)
                    )
                }
                modelica_grammar_trait::CompositionListGroup::EquationSection(eq_sec) => {
                    let sec = &eq_sec.equation_section;
                    for eq in &sec.equations {
                        if sec.initial {
                            comp.initial_equations.push(eq.clone());
                        } else {
                            comp.equations.push(eq.clone());
                        }
                    }
                }
                modelica_grammar_trait::CompositionListGroup::AlgorithmSection(alg_sec) => {
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
    pub components: IndexMap<String, ir::ast::Component>,
    pub classes: IndexMap<String, ir::ast::ClassDefinition>,
    pub imports: Vec<ir::ast::Import>,
    pub extends: Vec<ir::ast::Extend>,
}

impl TryFrom<&modelica_grammar_trait::ElementList> for ElementList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ElementList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = ElementList {
            components: IndexMap::new(),
            ..Default::default()
        };
        for elem_list in &ast.element_list_list {
            match &elem_list.element {
                modelica_grammar_trait::Element::ElementDefinition(edef) => {
                    match &edef.element_definition.element_definition_group {
                        modelica_grammar_trait::ElementDefinitionGroup::ClassDefinition(class) => {
                            let nested_class = class.class_definition.clone();
                            let name = nested_class.name.text.clone();
                            def.classes.insert(name, nested_class);
                        }
                        modelica_grammar_trait::ElementDefinitionGroup::ComponentClause(clause) => {
                            let connection =
                                match &clause.component_clause.type_prefix.type_prefix_opt {
                                    Some(opt) => match &opt.type_prefix_opt_group {
                                        modelica_grammar_trait::TypePrefixOptGroup::Flow(flow) => {
                                            ir::ast::Connection::Flow(flow.flow.flow.clone())
                                        }
                                        modelica_grammar_trait::TypePrefixOptGroup::Stream(
                                            stream,
                                        ) => ir::ast::Connection::Stream(
                                            stream.stream.stream.clone(),
                                        ),
                                    },
                                    None => ir::ast::Connection::Empty,
                                };

                            let variability = match &clause
                                .component_clause
                                .type_prefix
                                .type_prefix_opt0
                            {
                                Some(opt) => match &opt.type_prefix_opt0_group {
                                    modelica_grammar_trait::TypePrefixOpt0Group::Constant(c) => {
                                        ir::ast::Variability::Constant(c.constant.constant.clone())
                                    }
                                    modelica_grammar_trait::TypePrefixOpt0Group::Discrete(c) => {
                                        ir::ast::Variability::Discrete(c.discrete.discrete.clone())
                                    }
                                    modelica_grammar_trait::TypePrefixOpt0Group::Parameter(c) => {
                                        ir::ast::Variability::Parameter(
                                            c.parameter.parameter.clone(),
                                        )
                                    }
                                },
                                None => ir::ast::Variability::Empty,
                            };

                            let causality =
                                match &clause.component_clause.type_prefix.type_prefix_opt1 {
                                    Some(opt) => match &opt.type_prefix_opt1_group {
                                        modelica_grammar_trait::TypePrefixOpt1Group::Input(c) => {
                                            ir::ast::Causality::Input(c.input.input.clone())
                                        }
                                        modelica_grammar_trait::TypePrefixOpt1Group::Output(c) => {
                                            ir::ast::Causality::Output(c.output.output.clone())
                                        }
                                    },
                                    None => ir::ast::Causality::Empty,
                                };

                            for c in &clause.component_clause.component_list.components {
                                // Extract annotation arguments if present
                                let annotation =
                                    if let Some(desc_opt) = &c.description.description_opt {
                                        if let Some(class_mod_opt) = &desc_opt
                                            .annotation_clause
                                            .class_modification
                                            .class_modification_opt
                                        {
                                            class_mod_opt.argument_list.args.clone()
                                        } else {
                                            Vec::new()
                                        }
                                    } else {
                                        Vec::new()
                                    };

                                let mut value = ir::ast::Component {
                                    name: c.declaration.ident.text.clone(),
                                    type_name: clause.component_clause.type_specifier.name.clone(),
                                    variability: variability.clone(),
                                    causality: causality.clone(),
                                    connection: connection.clone(),
                                    description: c.description.description_string.tokens.clone(),
                                    start: ir::ast::Expression::Terminal {
                                        terminal_type: ir::ast::TerminalType::UnsignedReal,
                                        token: ir::ast::Token {
                                            text: "0.0".to_string(),
                                            ..Default::default()
                                        },
                                    },
                                    shape: Vec::new(), // Scalar by default, populated from array subscripts
                                    annotation,
                                };

                                // set default start value
                                value.start = match value.type_name.to_string().as_str() {
                                    "Real" => ir::ast::Expression::Terminal {
                                        terminal_type: ir::ast::TerminalType::UnsignedReal,
                                        token: ir::ast::Token {
                                            text: "0.0".to_string(),
                                            ..Default::default()
                                        },
                                    },
                                    "Integer" => ir::ast::Expression::Terminal {
                                        terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                        token: ir::ast::Token {
                                            text: "0".to_string(),
                                            ..Default::default()
                                        },
                                    },
                                    "Bool" => ir::ast::Expression::Terminal {
                                        terminal_type: ir::ast::TerminalType::Bool,
                                        token: ir::ast::Token {
                                            text: "0".to_string(),
                                            ..Default::default()
                                        },
                                    },
                                    _ => ir::ast::Expression::Empty {},
                                };

                                // Extract array dimensions from declaration subscripts (e.g., Real[2,3] or Real[2][3])
                                if let Some(decl_opt) = &c.declaration.declaration_opt {
                                    for subscript in &decl_opt.array_subscripts.subscripts {
                                        // Extract integer dimension from subscript expression
                                        if let ir::ast::Subscript::Expression(
                                            ir::ast::Expression::Terminal {
                                                token,
                                                terminal_type:
                                                    ir::ast::TerminalType::UnsignedInteger,
                                            },
                                        ) = subscript
                                        {
                                            if let Ok(dim) = token.text.parse::<usize>() {
                                                value.shape.push(dim);
                                            }
                                        }
                                    }
                                }

                                // handle for component modification
                                if let Some(modif) = &c.declaration.declaration_opt0 {
                                    match &modif.modification {
                                        modelica_grammar_trait::Modification::ClassModificationModificationOpt(
                                            class_mod,
                                        ) => {
                                            let modif = &*(class_mod.class_modification);
                                            if let Some(opt) = &modif.class_modification_opt {
                                                // Look for start= and shape= in the modifier arguments
                                                for arg in &opt.argument_list.args {
                                                    if let ir::ast::Expression::Binary { op, lhs, rhs } = arg {
                                                        if matches!(op, ir::ast::OpBinary::Eq(_)) {
                                                            // This is a named argument like start=2.5 or shape=(3)
                                                            if let ir::ast::Expression::ComponentReference(comp) = &**lhs {
                                                                match comp.to_string().as_str() {
                                                                    "start" => {
                                                                        value.start = (**rhs).clone();
                                                                    }
                                                                    "shape" => {
                                                                        // Extract shape from expression like (3) or {3, 2}
                                                                        match &**rhs {
                                                                            // Handle shape=(3) - single dimension
                                                                            ir::ast::Expression::Terminal {
                                                                                token,
                                                                                terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                                                            } => {
                                                                                if let Ok(dim) = token.text.parse::<usize>() {
                                                                                    value.shape = vec![dim];
                                                                                }
                                                                            }
                                                                            // Handle shape={3, 2} - multi-dimensional
                                                                            ir::ast::Expression::Array { elements } => {
                                                                                value.shape.clear();
                                                                                for elem in elements {
                                                                                    if let ir::ast::Expression::Terminal {
                                                                                        token,
                                                                                        terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                                                                    } = elem {
                                                                                        if let Ok(dim) = token.text.parse::<usize>() {
                                                                                            value.shape.push(dim);
                                                                                        }
                                                                                    }
                                                                                }
                                                                            }
                                                                            _ => {}
                                                                        }
                                                                    }
                                                                    _ => {}
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                        modelica_grammar_trait::Modification::EquModificationExpression(
                                            eq_mod,
                                        ) => {
                                            match &eq_mod.modification_expression {
                                                modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                                                    value.start = expr.expression.clone();
                                                }
                                                modelica_grammar_trait::ModificationExpression::Break(brk) => {
                                                    anyhow::bail!(
                                                        "'break' in modification expression is not yet supported{}",
                                                        loc_info(&brk.r#break.r#break)
                                                    )
                                                }
                                            }
                                        }
                                    }
                                }

                                def.components
                                    .insert(c.declaration.ident.text.clone(), value);
                            }
                        }
                    }
                }
                modelica_grammar_trait::Element::ImportClause(import_elem) => {
                    let import_clause = &import_elem.import_clause;
                    let parsed_import = match &import_clause.import_clause_group {
                        // import D = A.B.C; (renamed import)
                        modelica_grammar_trait::ImportClauseGroup::IdentEquName(renamed) => {
                            ir::ast::Import::Renamed {
                                alias: renamed.ident.clone(),
                                path: renamed.name.clone(),
                            }
                        }
                        // import A.B.C; or import A.B.*; or import A.B.{C, D};
                        modelica_grammar_trait::ImportClauseGroup::NameImportClauseOpt(
                            name_opt,
                        ) => {
                            let path = name_opt.name.clone();
                            match &name_opt.import_clause_opt {
                                None => {
                                    // import A.B.C; (qualified import)
                                    ir::ast::Import::Qualified { path }
                                }
                                Some(opt) => {
                                    match &opt.import_clause_opt_group {
                                        // import A.B.*;
                                        modelica_grammar_trait::ImportClauseOptGroup::DotStar(
                                            _,
                                        ) => ir::ast::Import::Unqualified { path },
                                        // import A.B.* or import A.B.{C, D}
                                        modelica_grammar_trait::ImportClauseOptGroup::DotImportClauseOptGroupGroup(dot_group) => {
                                            match &dot_group.import_clause_opt_group_group {
                                                // import A.B.*
                                                modelica_grammar_trait::ImportClauseOptGroupGroup::Star(_) => {
                                                    ir::ast::Import::Unqualified { path }
                                                }
                                                // import A.B.{C, D, E}
                                                modelica_grammar_trait::ImportClauseOptGroupGroup::LBraceImportListRBrace(list) => {
                                                    let mut names = vec![list.import_list.ident.clone()];
                                                    for item in &list.import_list.import_list_list {
                                                        names.push(item.ident.clone());
                                                    }
                                                    ir::ast::Import::Selective { path, names }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    };
                    def.imports.push(parsed_import);
                }
                modelica_grammar_trait::Element::ExtendsClause(clause) => {
                    let type_loc = clause
                        .extends_clause
                        .type_specifier
                        .name
                        .name
                        .first()
                        .map(loc_info)
                        .unwrap_or_default();
                    if clause.extends_clause.extends_clause_opt.is_some() {
                        anyhow::bail!(
                            "Class or inheritance modification in 'extends' is not yet supported{}",
                            type_loc
                        )
                    }
                    if clause.extends_clause.extends_clause_opt0.is_some() {
                        anyhow::bail!(
                            "Annotation in 'extends' clause is not yet supported{}",
                            type_loc
                        )
                    }
                    def.extends.push(ir::ast::Extend {
                        comp: clause.extends_clause.type_specifier.name.clone(),
                    });
                }
                modelica_grammar_trait::Element::ElementReplaceableDefinition(repl) => {
                    anyhow::bail!(
                        "'replaceable' element definition is not yet supported{}",
                        loc_info(&repl.element_replaceable_definition.replaceable.replaceable)
                    )
                }
            }
        }
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::String> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::String) -> std::result::Result<Self, Self::Error> {
        let mut tok = ast.string.clone();
        // remove quotes from string text
        tok.text = tok.text[1..tok.text.len() - 1].to_string();
        Ok(tok)
    }
}

//-----------------------------------------------------------------------------
#[derive(Default, Clone, Debug, PartialEq)]
#[allow(unused)]
pub struct TokenList {
    pub tokens: Vec<ir::ast::Token>,
}

impl TryFrom<&modelica_grammar_trait::DescriptionString> for TokenList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::DescriptionString,
    ) -> std::result::Result<Self, Self::Error> {
        let mut tokens = Vec::new();
        if let Some(opt) = &ast.description_string_opt {
            tokens.push(opt.string.clone());
            for string in &opt.description_string_opt_list {
                tokens.push(string.string.clone());
            }
        }
        Ok(TokenList { tokens })
    }
}

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
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct EquationSection {
    pub initial: bool,
    pub equations: Vec<ir::ast::Equation>,
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
    pub statements: Vec<ir::ast::Statement>,
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
impl TryFrom<&modelica_grammar_trait::Ident> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Ident) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Ident::BasicIdent(tok) => Ok(ir::ast::Token {
                location: tok.basic_ident.location.clone(),
                text: tok.basic_ident.text.clone(),
                token_number: tok.basic_ident.token_number,
                token_type: tok.basic_ident.token_type,
            }),
            modelica_grammar_trait::Ident::QIdent(tok) => Ok(ir::ast::Token {
                location: tok.q_ident.location.clone(),
                text: tok.q_ident.text.clone(),
                token_number: tok.q_ident.token_number,
                token_type: tok.q_ident.token_type,
            }),
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::UnsignedInteger> for ir::ast::Token {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::UnsignedInteger,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::Token {
            location: ast.unsigned_integer.location.clone(),
            text: ast.unsigned_integer.text.clone(),
            token_number: ast.unsigned_integer.token_number,
            token_type: ast.unsigned_integer.token_type,
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::UnsignedReal> for ir::ast::Token {
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
impl TryFrom<&modelica_grammar_trait::EquationBlock> for ir::ast::EquationBlock {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::EquationBlock,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::EquationBlock {
            cond: ast.expression.clone(),
            eqs: ast
                .equation_block_list
                .iter()
                .map(|x| x.some_equation.clone())
                .collect(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::StatementBlock> for ir::ast::StatementBlock {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::StatementBlock,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::StatementBlock {
            cond: ast.expression.clone(),
            stmts: ast
                .statement_block_list
                .iter()
                .map(|x| x.statement.clone())
                .collect(),
        })
    }
}

impl TryFrom<&modelica_grammar_trait::SomeEquation> for ir::ast::Equation {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SomeEquation,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.some_equation_option {
            modelica_grammar_trait::SomeEquationOption::SimpleEquation(eq) => {
                match &eq.simple_equation.simple_equation_opt {
                    Some(rhs) => Ok(ir::ast::Equation::Simple {
                        lhs: eq.simple_equation.simple_expression.clone(),
                        rhs: rhs.expression.clone(),
                    }),
                    None => {
                        // this is a function call eq (reinit, assert, terminate, etc.)
                        // see 8.3.6-8.3.8
                        match &eq.simple_equation.simple_expression {
                            ir::ast::Expression::FunctionCall { comp, args } => {
                                Ok(ir::ast::Equation::FunctionCall {
                                    comp: comp.clone(),
                                    args: args.clone(),
                                })
                            }
                            _ => Err(anyhow::anyhow!(
                                "Modelica only allows functional call statement as equation: {:?}",
                                ast
                            )),
                        }
                    }
                }
            }
            modelica_grammar_trait::SomeEquationOption::ConnectEquation(eq) => {
                Ok(ir::ast::Equation::Connect {
                    lhs: eq.connect_equation.component_reference.clone(),
                    rhs: eq.connect_equation.component_reference0.clone(),
                })
            }
            modelica_grammar_trait::SomeEquationOption::ForEquation(eq) => {
                // Convert for indices
                let mut indices = Vec::new();

                // First index
                let first_idx = &eq.for_equation.for_indices.for_index;
                let range = first_idx
                    .for_index_opt
                    .as_ref()
                    .map(|opt| opt.expression.clone())
                    .unwrap_or_default();
                indices.push(ir::ast::ForIndex {
                    ident: first_idx.ident.clone(),
                    range,
                });

                // Additional indices
                for idx_item in &eq.for_equation.for_indices.for_indices_list {
                    let idx = &idx_item.for_index;
                    let range = idx
                        .for_index_opt
                        .as_ref()
                        .map(|opt| opt.expression.clone())
                        .unwrap_or_default();
                    indices.push(ir::ast::ForIndex {
                        ident: idx.ident.clone(),
                        range,
                    });
                }

                // Convert equations in the loop body
                let equations: Vec<ir::ast::Equation> = eq
                    .for_equation
                    .for_equation_list
                    .iter()
                    .map(|eq_item| eq_item.some_equation.clone())
                    .collect();

                Ok(ir::ast::Equation::For { indices, equations })
            }
            modelica_grammar_trait::SomeEquationOption::IfEquation(eq) => {
                let mut blocks = vec![eq.if_equation.if0.clone()];
                for when in &eq.if_equation.if_equation_list {
                    blocks.push(when.elseif0.clone());
                }
                Ok(ir::ast::Equation::If {
                    cond_blocks: blocks,
                    else_block: eq.if_equation.if_equation_opt.as_ref().map(|opt| {
                        opt.if_equation_opt_list
                            .iter()
                            .map(|x| x.some_equation.clone())
                            .collect()
                    }),
                })
            }
            modelica_grammar_trait::SomeEquationOption::WhenEquation(eq) => {
                let mut cond_blocks = vec![eq.when_equation.when0.clone()];
                for when in &eq.when_equation.when_equation_list {
                    cond_blocks.push(when.elsewhen0.clone());
                }
                Ok(ir::ast::Equation::When(cond_blocks))
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Statement> for ir::ast::Statement {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Statement) -> std::result::Result<Self, Self::Error> {
        match &ast.statement_option {
            modelica_grammar_trait::StatementOption::ComponentStatement(stmt) => {
                match &stmt.component_statement.component_statement_group {
                    modelica_grammar_trait::ComponentStatementGroup::ColonEquExpression(assign) => {
                        Ok(ir::ast::Statement::Assignment {
                            comp: stmt.component_statement.component_reference.clone(),
                            value: assign.expression.clone(),
                        })
                    }
                    modelica_grammar_trait::ComponentStatementGroup::FunctionCallArgs(args) => {
                        Ok(ir::ast::Statement::FunctionCall {
                            comp: stmt.component_statement.component_reference.clone(),
                            args: args.function_call_args.args.clone(),
                        })
                    }
                }
            }
            modelica_grammar_trait::StatementOption::Break(tok) => Ok(ir::ast::Statement::Break {
                token: tok.r#break.r#break.clone(),
            }),
            modelica_grammar_trait::StatementOption::Return(tok) => {
                Ok(ir::ast::Statement::Return {
                    token: tok.r#return.r#return.clone(),
                })
            }
            modelica_grammar_trait::StatementOption::ForStatement(..) => {
                Ok(ir::ast::Statement::For {
                    indices: vec![], // todo
                    equations: vec![],
                })
            }
            modelica_grammar_trait::StatementOption::IfStatement(stmt) => {
                anyhow::bail!(
                    "'if' statement is not yet supported{}",
                    expr_loc_info(&stmt.if_statement.r#if0.cond)
                )
            }
            modelica_grammar_trait::StatementOption::WhenStatement(stmt) => {
                anyhow::bail!(
                    "'when' statement is not yet supported{}",
                    expr_loc_info(&stmt.when_statement.when0.cond)
                )
            }
            modelica_grammar_trait::StatementOption::WhileStatement(stmt) => {
                anyhow::bail!(
                    "'while' statement is not yet supported{}",
                    expr_loc_info(&stmt.while_statement.expression)
                )
            }
            modelica_grammar_trait::StatementOption::FunctionCallOutputStatement(stmt) => {
                let loc = stmt
                    .function_call_output_statement
                    .component_reference
                    .parts
                    .first()
                    .map(|p| loc_info(&p.ident))
                    .unwrap_or_default();
                anyhow::bail!(
                    "Function call with output list like '(a, b) = func()' is not yet supported{}",
                    loc
                )
            }
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ArraySubscripts {
    pub subscripts: Vec<ir::ast::Subscript>,
}

impl TryFrom<&modelica_grammar_trait::ArraySubscripts> for ArraySubscripts {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArraySubscripts,
    ) -> std::result::Result<Self, Self::Error> {
        let mut subscripts = vec![ast.subscript.as_ref().clone()];
        for subscript in &ast.array_subscripts_list {
            subscripts.push(subscript.subscript.clone());
        }
        Ok(ArraySubscripts { subscripts })
    }
}

impl TryFrom<&modelica_grammar_trait::Subscript> for ir::ast::Subscript {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Subscript) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Subscript::Colon(tok) => Ok(ir::ast::Subscript::Range {
                token: tok.colon.clone(),
            }),
            modelica_grammar_trait::Subscript::Expression(expr) => Ok(
                ir::ast::Subscript::Expression(expr.expression.as_ref().clone()),
            ),
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]
#[allow(unused)]
pub struct ExpressionList {
    pub args: Vec<ir::ast::Expression>,
}

impl TryFrom<&modelica_grammar_trait::FunctionArgument> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArgument,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArgument::Expression(expr) => {
                Ok(expr.expression.as_ref().clone())
            }
            modelica_grammar_trait::FunctionArgument::FunctionPartialApplication(fpa) => {
                let loc = &fpa.function_partial_application.function.function.location;
                anyhow::bail!(
                    "Function partial application is not supported at line {}, column {}. \
                     This may indicate a syntax error in your Modelica code - \
                     check for stray text or missing semicolons near function calls.",
                    loc.start_line,
                    loc.start_column
                )
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionArguments> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArguments,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArguments::ExpressionFunctionArgumentsOpt(def) => {
                let mut args = vec![*def.expression.clone()];
                if let Some(opt) = &def.function_arguments_opt {
                    match &opt.function_arguments_opt_group {
                        modelica_grammar_trait::FunctionArgumentsOptGroup::CommaFunctionArgumentsNonFirst(
                            expr,
                        ) => {
                            args.append(&mut expr.function_arguments_non_first.args.clone());
                        }
                        modelica_grammar_trait::FunctionArgumentsOptGroup::ForForIndices(..) => {
                            anyhow::bail!(
                                "Array comprehensions with 'for' are not yet supported."
                            )
                        }
                    }
                }
                Ok(ExpressionList { args })
            }
            modelica_grammar_trait::FunctionArguments::FunctionPartialApplicationFunctionArgumentsOpt0(fpa) => {
                let loc = &fpa.function_partial_application.function.function.location;
                anyhow::bail!(
                    "Function partial application is not supported at line {}, column {}. \
                     This may indicate a syntax error in your Modelica code - \
                     check for stray text or missing semicolons near function calls.",
                    loc.start_line, loc.start_column
                )
            }
            modelica_grammar_trait::FunctionArguments::NamedArguments(..) => {
                anyhow::bail!(
                    "Named function arguments are not yet supported. \
                     Use positional arguments instead."
                )
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionArgumentsNonFirst> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArgumentsNonFirst,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArgumentsNonFirst::FunctionArgumentFunctionArgumentsNonFirstOpt(expr) => {
                let mut args = vec![expr.function_argument.clone()];
                if let Some(opt) = &expr.function_arguments_non_first_opt {
                    args.append(&mut opt.function_arguments_non_first.args.clone());
                }
                Ok(ExpressionList { args })
            }
            modelica_grammar_trait::FunctionArgumentsNonFirst::NamedArguments(args) => {
                anyhow::bail!(
                    "Named arguments like 'func(x=1, y=2)' are not yet supported{}. \
                     Use positional arguments instead.",
                    loc_info(&args.named_arguments.named_argument.ident)
                )
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ArgumentList> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArgumentList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut args = vec![(*ast.argument).clone()];
        for arg in &ast.argument_list_list {
            args.push(arg.argument.clone())
        }
        Ok(ExpressionList { args })
    }
}

impl TryFrom<&modelica_grammar_trait::Argument> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Argument) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Argument::ElementModificationOrReplaceable(modif) => {
                match &modif.element_modification_or_replaceable.element_modification_or_replaceable_group {
                    modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementModification(elem) => {
                        let name_loc = elem
                            .element_modification
                            .name
                            .name
                            .first()
                            .map(loc_info)
                            .unwrap_or_default();
                        match &elem.element_modification.element_modification_opt {
                            Some(opt) => {
                                match &opt.modification {
                                    modelica_grammar_trait::Modification::ClassModificationModificationOpt(_modif) => {
                                        anyhow::bail!(
                                            "Class modification in argument is not yet supported{}",
                                            name_loc
                                        )
                                    }
                                    modelica_grammar_trait::Modification::EquModificationExpression(modif) => {
                                        match &modif.modification_expression {
                                            modelica_grammar_trait::ModificationExpression::Break(brk) => {
                                                anyhow::bail!(
                                                    "'break' in modification expression is not yet supported{}",
                                                    loc_info(&brk.r#break.r#break)
                                                )
                                            }
                                            modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                                                // Create a Binary expression to preserve the name=value structure
                                                // LHS = name (as ComponentReference), RHS = value
                                                let name = &elem.element_modification.name;
                                                let parts = name.name.iter().map(|token| {
                                                    ir::ast::ComponentRefPart {
                                                        ident: token.clone(),
                                                        subs: None,
                                                    }
                                                }).collect();
                                                let name_expr = ir::ast::Expression::ComponentReference(
                                                    ir::ast::ComponentReference {
                                                        local: false,
                                                        parts,
                                                    }
                                                );
                                                Ok(ir::ast::Expression::Binary {
                                                    op: ir::ast::OpBinary::Eq(ir::ast::Token::default()),
                                                    lhs: Box::new(name_expr),
                                                    rhs: Box::new(expr.expression.clone()),
                                                })
                                            }
                                        }
                                    }
                                }
                            }
                            None => {
                                Ok(ir::ast::Expression::Empty)
                            }
                        }
                    }
                    modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementReplaceable(repl) => {
                        anyhow::bail!(
                            "'replaceable' element in modification is not yet supported{}",
                            loc_info(&repl.element_replaceable.replaceable.replaceable)
                        )
                    }
                }
            }
            modelica_grammar_trait::Argument::ElementRedeclaration(redcl) => {
                anyhow::bail!(
                    "'redeclare' in argument is not yet supported{}",
                    loc_info(&redcl.element_redeclaration.redeclare.redeclare)
                )
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::OutputExpressionList> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::OutputExpressionList,
    ) -> std::result::Result<Self, Self::Error> {
        let mut v = Vec::new();
        if let Some(opt) = &ast.output_expression_list_opt {
            v.push(opt.expression.clone());
        }
        for expr in &ast.output_expression_list_list {
            if let Some(opt) = &expr.output_expression_list_opt0 {
                v.push(opt.expression.clone());
            }
        }
        Ok(ExpressionList { args: v })
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionCallArgs> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionCallArgs,
    ) -> std::result::Result<Self, Self::Error> {
        if let Some(opt) = &ast.function_call_args_opt {
            Ok(ExpressionList {
                args: opt.function_arguments.args.clone(),
            })
        } else {
            Ok(ExpressionList { args: vec![] })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Primary> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::Primary::ComponentPrimary(comp) => {
                match &comp.component_primary.component_primary_opt {
                    Some(args) => Ok(ir::ast::Expression::FunctionCall {
                        comp: (*comp.component_primary.component_reference).clone(),
                        args: args.function_call_args.args.clone(),
                    }),
                    None => Ok(ir::ast::Expression::ComponentReference(
                        comp.component_primary.component_reference.as_ref().clone(),
                    )),
                }
            }
            modelica_grammar_trait::Primary::UnsignedNumber(unsigned_num) => {
                match &unsigned_num.unsigned_number {
                    modelica_grammar_trait::UnsignedNumber::UnsignedInteger(unsigned_int) => {
                        Ok(ir::ast::Expression::Terminal {
                            terminal_type: ir::ast::TerminalType::UnsignedInteger,
                            token: unsigned_int.unsigned_integer.clone(),
                        })
                    }
                    modelica_grammar_trait::UnsignedNumber::UnsignedReal(unsigned_real) => {
                        Ok(ir::ast::Expression::Terminal {
                            terminal_type: ir::ast::TerminalType::UnsignedReal,
                            token: unsigned_real.unsigned_real.clone(),
                        })
                    }
                }
            }
            modelica_grammar_trait::Primary::String(string) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::String,
                token: string.string.clone(),
            }),
            modelica_grammar_trait::Primary::True(bool) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::Bool,
                token: bool.r#true.r#true.clone(),
            }),
            modelica_grammar_trait::Primary::False(bool) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::Bool,
                token: bool.r#false.r#false.clone(),
            }),
            modelica_grammar_trait::Primary::End(end) => Ok(ir::ast::Expression::Terminal {
                terminal_type: ir::ast::TerminalType::End,
                token: end.end.end.clone(),
            }),
            modelica_grammar_trait::Primary::ArrayPrimary(arr) => {
                let elements = collect_array_elements(&arr.array_primary.array_arguments)?;
                Ok(ir::ast::Expression::Array { elements })
            }
            modelica_grammar_trait::Primary::RangePrimary(range) => {
                anyhow::bail!(
                    "Range primary like '{{1:10}}' is not yet supported{}",
                    expr_loc_info(&range.range_primary.expression_list.expression)
                )
            }
            modelica_grammar_trait::Primary::OutputPrimary(output) => {
                let primary = &output.output_primary;
                let location_info = primary
                    .output_expression_list
                    .args
                    .first()
                    .and_then(|e| e.get_location())
                    .map(|loc| {
                        format!(
                            " at {}:{}:{}",
                            loc.file_name, loc.start_line, loc.start_column
                        )
                    })
                    .unwrap_or_default();

                if primary.output_primary_opt.is_some() {
                    anyhow::bail!(
                        "Output primary with array subscripts or identifiers is not yet supported{}. \
                         This may indicate a syntax error - check for stray text near parenthesized expressions.",
                        location_info
                    );
                };
                if primary.output_expression_list.args.len() > 1 {
                    // Multiple outputs like (a, b) = func() - create a Tuple
                    Ok(ir::ast::Expression::Tuple {
                        elements: primary.output_expression_list.args.clone(),
                    })
                } else if primary.output_expression_list.args.len() == 1 {
                    Ok(primary.output_expression_list.args[0].clone())
                } else {
                    // Empty parentheses - return Empty expression
                    Ok(ir::ast::Expression::Empty)
                }
            }
            modelica_grammar_trait::Primary::GlobalFunctionCall(expr) => {
                let tok = match &expr.global_function_call.global_function_call_group {
                    modelica_grammar_trait::GlobalFunctionCallGroup::Der(expr) => {
                        expr.der.der.clone()
                    }
                    modelica_grammar_trait::GlobalFunctionCallGroup::Initial(expr) => {
                        expr.initial.initial.clone()
                    }
                    modelica_grammar_trait::GlobalFunctionCallGroup::Pure(expr) => {
                        expr.pure.pure.clone()
                    }
                };
                let part = ir::ast::ComponentRefPart {
                    ident: tok,
                    subs: None,
                };
                Ok(ir::ast::Expression::FunctionCall {
                    comp: ir::ast::ComponentReference {
                        local: false,
                        parts: vec![part],
                    },
                    args: expr.global_function_call.function_call_args.args.clone(),
                })
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Factor> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Factor) -> std::result::Result<Self, Self::Error> {
        if ast.factor_list.is_empty() {
            Ok(ast.primary.as_ref().clone())
        } else {
            Ok(ir::ast::Expression::Binary {
                op: ir::ast::OpBinary::Exp(ir::ast::Token::default()),
                lhs: Box::new(ast.primary.as_ref().clone()),
                rhs: Box::new(ast.factor_list[0].primary.clone()),
            })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Term> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
        if ast.term_list.is_empty() {
            Ok(ast.factor.clone())
        } else {
            let mut lhs = ast.factor.clone();
            for factor in &ast.term_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match &factor.mul_operator {
                        modelica_grammar_trait::MulOperator::Star(op) => {
                            ir::ast::OpBinary::Mul(op.star.clone())
                        }
                        modelica_grammar_trait::MulOperator::Slash(op) => {
                            ir::ast::OpBinary::Div(op.slash.clone())
                        }
                        modelica_grammar_trait::MulOperator::DotSlash(op) => {
                            ir::ast::OpBinary::DivElem(op.dot_slash.clone())
                        }
                        modelica_grammar_trait::MulOperator::DotStar(op) => {
                            ir::ast::OpBinary::MulElem(op.dot_star.clone())
                        }
                    },
                    rhs: Box::new(factor.factor.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArithmeticExpression,
    ) -> std::result::Result<Self, Self::Error> {
        // handle first term
        let mut lhs = match &ast.arithmetic_expression_opt {
            Some(opt) => ir::ast::Expression::Unary {
                op: match &opt.add_operator {
                    modelica_grammar_trait::AddOperator::Minus(tok) => {
                        ir::ast::OpUnary::Minus(tok.minus.clone())
                    }
                    modelica_grammar_trait::AddOperator::Plus(tok) => {
                        ir::ast::OpUnary::Plus(tok.plus.clone())
                    }
                    modelica_grammar_trait::AddOperator::DotMinus(tok) => {
                        ir::ast::OpUnary::DotMinus(tok.dot_minus.clone())
                    }
                    modelica_grammar_trait::AddOperator::DotPlus(tok) => {
                        ir::ast::OpUnary::DotPlus(tok.dot_plus.clone())
                    }
                },
                rhs: Box::new(ast.term.as_ref().clone()),
            },
            None => ast.term.as_ref().clone(),
        };

        // if has term list, process expressions
        if !ast.arithmetic_expression_list.is_empty() {
            for term in &ast.arithmetic_expression_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: match &term.add_operator {
                        modelica_grammar_trait::AddOperator::Plus(tok) => {
                            ir::ast::OpBinary::Add(tok.plus.clone())
                        }
                        modelica_grammar_trait::AddOperator::Minus(tok) => {
                            ir::ast::OpBinary::Sub(tok.minus.clone())
                        }
                        modelica_grammar_trait::AddOperator::DotPlus(tok) => {
                            ir::ast::OpBinary::AddElem(tok.dot_plus.clone())
                        }
                        modelica_grammar_trait::AddOperator::DotMinus(tok) => {
                            ir::ast::OpBinary::SubElem(tok.dot_minus.clone())
                        }
                    },
                    rhs: Box::new(term.term.clone()),
                };
            }
        }
        Ok(lhs)
    }
}

impl TryFrom<&modelica_grammar_trait::Relation> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
        match &ast.relation_opt {
            Some(relation) => Ok(ir::ast::Expression::Binary {
                lhs: Box::new(ast.arithmetic_expression.as_ref().clone()),
                op: match &relation.relational_operator {
                    modelica_grammar_trait::RelationalOperator::EquEqu(tok) => {
                        ir::ast::OpBinary::Eq(tok.equ_equ.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::GT(tok) => {
                        ir::ast::OpBinary::Gt(tok.g_t.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::LT(tok) => {
                        ir::ast::OpBinary::Lt(tok.l_t.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::GTEqu(tok) => {
                        ir::ast::OpBinary::Ge(tok.g_t_equ.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::LTEqu(tok) => {
                        ir::ast::OpBinary::Le(tok.l_t_equ.clone())
                    }
                    modelica_grammar_trait::RelationalOperator::LTGT(tok) => {
                        ir::ast::OpBinary::Neq(tok.l_t_g_t.clone())
                    }
                },
                rhs: Box::new(relation.arithmetic_expression.clone()),
            }),
            None => Ok(ast.arithmetic_expression.as_ref().clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalFactor> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalFactor,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.logical_factor_opt {
            Some(opt) => {
                let not_tok = opt.not.not.clone();
                Ok(ir::ast::Expression::Unary {
                    op: ir::ast::OpUnary::Not(not_tok),
                    rhs: Box::new(ast.relation.as_ref().clone()),
                })
            }
            None => Ok(ast.relation.as_ref().clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalTerm> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalTerm,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_term_list.is_empty() {
            Ok(ast.logical_factor.as_ref().clone())
        } else {
            let mut lhs = ast.logical_factor.as_ref().clone();
            for term in &ast.logical_term_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: ir::ast::OpBinary::And(ir::ast::Token::default()),
                    rhs: Box::new(term.logical_factor.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalExpression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalExpression,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_expression_list.is_empty() {
            Ok(ast.logical_term.as_ref().clone())
        } else {
            let mut lhs = ast.logical_term.as_ref().clone();
            for term in &ast.logical_expression_list {
                lhs = ir::ast::Expression::Binary {
                    lhs: Box::new(lhs),
                    op: ir::ast::OpBinary::Or(ir::ast::Token::default()),
                    rhs: Box::new(term.logical_term.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::SimpleExpression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SimpleExpression,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.simple_expression_opt {
            Some(opt) => match &opt.simple_expression_opt0 {
                Some(opt0) => Ok(ir::ast::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: Some(Box::new(opt.logical_expression.clone())),
                    end: Box::new(opt0.logical_expression.clone()),
                }),
                None => Ok(ir::ast::Expression::Range {
                    start: Box::new(ast.logical_expression.clone()),
                    step: None,
                    end: Box::new(opt.logical_expression.clone()),
                }),
            },
            None => Ok(ast.logical_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Expression> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::Expression,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::Expression::SimpleExpression(simple_expression) => {
                Ok(simple_expression.simple_expression.as_ref().clone())
            }
            modelica_grammar_trait::Expression::IfExpression(expr) => {
                let if_expr = &expr.if_expression;

                // Build the branches: first the main if, then any elseifs
                let mut branches = Vec::new();

                // The main if branch: condition is expression, result is expression0
                let condition = (*if_expr.expression).clone();
                let then_expr = if_expr.expression0.clone();
                branches.push((condition, then_expr));

                // Add any elseif branches from the list
                for elseif in &if_expr.if_expression_list {
                    let elseif_cond = elseif.expression.clone();
                    let elseif_expr = elseif.expression0.clone();
                    branches.push((elseif_cond, elseif_expr));
                }

                // The else branch is expression1
                let else_branch = Box::new(if_expr.expression1.clone());

                Ok(ir::ast::Expression::If {
                    branches,
                    else_branch,
                })
            }
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ComponentReference> for ir::ast::ComponentReference {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentReference,
    ) -> std::result::Result<Self, Self::Error> {
        let mut parts = Vec::new();

        // Handle subscripts for the first part (e.g., x[i] in component_reference_opt0)
        let first_subs = ast
            .component_reference_opt0
            .as_ref()
            .map(|opt| opt.array_subscripts.subscripts.clone());

        parts.push(ir::ast::ComponentRefPart {
            ident: ast.ident.clone(),
            subs: first_subs,
        });
        for comp_ref in &ast.component_reference_list {
            parts.push(comp_ref.component_ref_part.clone());
        }
        Ok(ir::ast::ComponentReference {
            local: ast.component_reference_opt.is_some(),
            parts,
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ComponentRefPart> for ir::ast::ComponentRefPart {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ComponentRefPart,
    ) -> std::result::Result<Self, Self::Error> {
        Ok(ir::ast::ComponentRefPart {
            ident: ast.ident.clone(),
            subs: ast
                .component_ref_part_opt
                .as_ref()
                .map(|subs| subs.array_subscripts.subscripts.clone()),
        })
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Name> for ir::ast::Name {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Name) -> std::result::Result<Self, Self::Error> {
        let mut name = vec![ast.ident.clone()];
        for ident in &ast.name_list {
            name.push(ident.ident.clone());
        }
        Ok(ir::ast::Name { name })
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default)]
pub struct ModelicaGrammar<'t> {
    pub modelica: Option<ir::ast::StoredDefinition>,
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
            Some(modelica) => writeln!(f, "{:#?}", modelica),
            None => write!(f, "No parse result"),
        }
    }
}

impl<'t> modelica_grammar_trait::ModelicaGrammarTrait for ModelicaGrammar<'t> {
    fn stored_definition(&mut self, arg: &modelica_grammar_trait::StoredDefinition) -> Result<()> {
        self.modelica = Some(arg.try_into()?);
        Ok(())
    }
}
