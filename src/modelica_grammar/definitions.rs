//! Conversion for class definitions and composition structures.

use super::helpers::{loc_info, span_location};
use crate::ir;
use crate::modelica_grammar_trait;
use indexmap::IndexMap;
use parol_runtime::Token;

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
        def.within = ast.stored_definition_opt.as_ref().map(|within_clause| {
            within_clause
                .stored_definition_opt1
                .as_ref()
                .map(|w| w.name.clone())
                .unwrap_or_else(|| ir::ast::Name { name: vec![] })
        });
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

/// Extract the keyword token from grammar ClassType for semantic highlighting
fn get_class_type_token(class_type: &modelica_grammar_trait::ClassType) -> ir::ast::Token {
    match class_type {
        modelica_grammar_trait::ClassType::Class(c) => c.class.class.clone(),
        modelica_grammar_trait::ClassType::Model(m) => m.model.model.clone(),
        modelica_grammar_trait::ClassType::ClassTypeOptRecord(r) => r.record.record.clone(),
        modelica_grammar_trait::ClassType::Block(b) => b.block.block.clone(),
        modelica_grammar_trait::ClassType::ClassTypeOpt0Connector(c) => {
            c.connector.connector.clone()
        }
        modelica_grammar_trait::ClassType::Type(t) => t.r#type.r#type.clone(),
        modelica_grammar_trait::ClassType::Package(p) => p.package.package.clone(),
        modelica_grammar_trait::ClassType::ClassTypeOpt1ClassTypeOpt2Function(f) => {
            f.function.function.clone()
        }
        modelica_grammar_trait::ClassType::Operator(o) => o.operator.operator.clone(),
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ClassDefinition> for ir::ast::ClassDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ClassDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let class_type = convert_class_type(&ast.class_prefixes.class_type);
        let class_type_token = get_class_type_token(&ast.class_prefixes.class_type);
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
                            class_type_token,
                            description: spec.description_string.tokens.clone(),
                            location: span_location(&spec.name, &spec.ident),
                            extends: spec.composition.extends.clone(),
                            imports: spec.composition.imports.clone(),
                            classes: spec.composition.classes.clone(),
                            equations: spec.composition.equations.clone(),
                            algorithms: spec.composition.algorithms.clone(),
                            initial_equations: spec.composition.initial_equations.clone(),
                            initial_algorithms: spec.composition.initial_algorithms.clone(),
                            components: spec.composition.components.clone(),
                            encapsulated: ast.class_definition_opt.is_some(),
                            partial: ast.class_prefixes.class_prefixes_opt.is_some(),
                            causality: ir::ast::Causality::Empty,
                            equation_keyword: spec.composition.equation_keyword.clone(),
                            initial_equation_keyword: spec
                                .composition
                                .initial_equation_keyword
                                .clone(),
                            algorithm_keyword: spec.composition.algorithm_keyword.clone(),
                            initial_algorithm_keyword: spec
                                .composition
                                .initial_algorithm_keyword
                                .clone(),
                            end_name_token: Some(spec.ident.clone()),
                            enum_literals: vec![],
                            annotation: spec.composition.annotation.clone(),
                        })
                    }
                    modelica_grammar_trait::LongClassSpecifier::ExtendsClassSpecifier(ext) => {
                        // Handle 'extends IDENT [class_modification] description composition end IDENT'
                        // This is a class that extends an inherited class by the same name
                        let spec = &ext.extends_class_specifier;

                        // Create an extends clause for the inherited class
                        let extends_modifiers =
                            if let Some(class_mod) = &spec.extends_class_specifier_opt {
                                if let Some(arg_list) =
                                    &class_mod.class_modification.class_modification_opt
                                {
                                    arg_list.argument_list.args.clone()
                                } else {
                                    vec![]
                                }
                            } else {
                                vec![]
                            };

                        let extends_name = ir::ast::Name {
                            name: vec![spec.ident.clone()],
                        };

                        let inherited_extends = ir::ast::Extend {
                            comp: extends_name,
                            location: spec.ident.location.clone(),
                            modifications: extends_modifiers,
                        };

                        // Combine inherited extends with composition extends
                        let mut all_extends = vec![inherited_extends];
                        all_extends.extend(spec.composition.extends.clone());

                        Ok(ir::ast::ClassDefinition {
                            name: spec.ident.clone(),
                            class_type,
                            class_type_token,
                            description: spec.description_string.tokens.clone(),
                            location: span_location(&spec.ident, &spec.ident0),
                            extends: all_extends,
                            imports: spec.composition.imports.clone(),
                            classes: spec.composition.classes.clone(),
                            equations: spec.composition.equations.clone(),
                            algorithms: spec.composition.algorithms.clone(),
                            initial_equations: spec.composition.initial_equations.clone(),
                            initial_algorithms: spec.composition.initial_algorithms.clone(),
                            components: spec.composition.components.clone(),
                            encapsulated: ast.class_definition_opt.is_some(),
                            partial: ast.class_prefixes.class_prefixes_opt.is_some(),
                            causality: ir::ast::Causality::Empty,
                            equation_keyword: spec.composition.equation_keyword.clone(),
                            initial_equation_keyword: spec
                                .composition
                                .initial_equation_keyword
                                .clone(),
                            algorithm_keyword: spec.composition.algorithm_keyword.clone(),
                            initial_algorithm_keyword: spec
                                .composition
                                .initial_algorithm_keyword
                                .clone(),
                            end_name_token: Some(spec.ident0.clone()),
                            enum_literals: vec![],
                            annotation: spec.composition.annotation.clone(),
                        })
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
                        let enum_spec = &spec.enum_class_specifier;

                        // Extract enumeration literals with their descriptions
                        let enum_literals = match &enum_spec.enum_class_specifier_group {
                            modelica_grammar_trait::EnumClassSpecifierGroup::EnumClassSpecifierOpt(opt) => {
                                if let Some(list_opt) = &opt.enum_class_specifier_opt {
                                    let list = &list_opt.enum_list;
                                    let mut literals = vec![ir::ast::EnumLiteral {
                                        ident: list.enumeration_literal.ident.clone(),
                                        description: list
                                            .enumeration_literal
                                            .description
                                            .description_string
                                            .tokens
                                            .clone(),
                                    }];
                                    for item in &list.enum_list_list {
                                        literals.push(ir::ast::EnumLiteral {
                                            ident: item.enumeration_literal.ident.clone(),
                                            description: item
                                                .enumeration_literal
                                                .description
                                                .description_string
                                                .tokens
                                                .clone(),
                                        });
                                    }
                                    literals
                                } else {
                                    vec![]
                                }
                            }
                            modelica_grammar_trait::EnumClassSpecifierGroup::Colon(_) => {
                                // enumeration(:) - represents an extensible enumeration
                                vec![]
                            }
                        };

                        Ok(ir::ast::ClassDefinition {
                            name: enum_spec.ident.clone(),
                            class_type: ir::ast::ClassType::Type,
                            class_type_token,
                            description: vec![],
                            location: enum_spec.ident.location.clone(),
                            extends: vec![],
                            imports: vec![],
                            classes: IndexMap::new(),
                            equations: vec![],
                            algorithms: vec![],
                            initial_equations: vec![],
                            initial_algorithms: vec![],
                            components: IndexMap::new(),
                            encapsulated: ast.class_definition_opt.is_some(),
                            partial: ast.class_prefixes.class_prefixes_opt.is_some(),
                            causality: ir::ast::Causality::Empty,
                            equation_keyword: None,
                            initial_equation_keyword: None,
                            algorithm_keyword: None,
                            initial_algorithm_keyword: None,
                            end_name_token: None,
                            enum_literals,
                            annotation: vec![],
                        })
                    }
                    modelica_grammar_trait::ShortClassSpecifier::TypeClassSpecifier(spec) => {
                        // type MyType = [input|output] BaseType(mods) "description";
                        // Creates a class that extends the base type with optional causality
                        // e.g., type Time = Real(unit="s");
                        //       connector RealInput = input Real;
                        let type_spec = &spec.type_class_specifier;
                        let base_type_name = type_spec.type_specifier.name.clone();

                        // Extract causality from base_prefix (input/output keyword)
                        let causality = match &type_spec.base_prefix.base_prefix_opt {
                            Some(opt) => match &opt.base_prefix_opt_group {
                                modelica_grammar_trait::BasePrefixOptGroup::Input(inp) => {
                                    ir::ast::Causality::Input(inp.input.input.clone())
                                }
                                modelica_grammar_trait::BasePrefixOptGroup::Output(out) => {
                                    ir::ast::Causality::Output(out.output.output.clone())
                                }
                            },
                            None => ir::ast::Causality::Empty,
                        };

                        // Extract modifications from class_modification if present
                        // e.g., Real(unit="s") -> modifications = [unit = "s"]
                        let modifications =
                            if let Some(class_mod_opt0) = &type_spec.type_class_specifier_opt0 {
                                if let Some(arg_list) =
                                    &class_mod_opt0.class_modification.class_modification_opt
                                {
                                    arg_list.argument_list.args.clone()
                                } else {
                                    vec![]
                                }
                            } else {
                                vec![]
                            };

                        // Create an Extend clause for the base type
                        // For short class specifiers, use ident location for both start and end
                        let extend = ir::ast::Extend {
                            comp: base_type_name,
                            location: type_spec.ident.location.clone(),
                            modifications,
                        };

                        Ok(ir::ast::ClassDefinition {
                            name: type_spec.ident.clone(),
                            class_type,
                            class_type_token,
                            description: vec![],
                            location: type_spec.ident.location.clone(),
                            extends: vec![extend],
                            imports: vec![],
                            classes: IndexMap::new(),
                            equations: vec![],
                            algorithms: vec![],
                            initial_equations: vec![],
                            initial_algorithms: vec![],
                            components: IndexMap::new(),
                            encapsulated: ast.class_definition_opt.is_some(),
                            partial: ast.class_prefixes.class_prefixes_opt.is_some(),
                            causality,
                            equation_keyword: None,
                            initial_equation_keyword: None,
                            algorithm_keyword: None,
                            initial_algorithm_keyword: None,
                            end_name_token: None, // Short class specifiers don't have "end Name"
                            enum_literals: vec![],
                            annotation: vec![],
                        })
                    }
                }
            }
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct Composition {
    pub extends: Vec<ir::ast::Extend>,
    pub imports: Vec<ir::ast::Import>,
    pub components: IndexMap<String, ir::ast::Component>,
    pub classes: IndexMap<String, ir::ast::ClassDefinition>,
    pub equations: Vec<ir::ast::Equation>,
    pub initial_equations: Vec<ir::ast::Equation>,
    pub algorithms: Vec<Vec<ir::ast::Statement>>,
    pub initial_algorithms: Vec<Vec<ir::ast::Statement>>,
    /// Token for "equation" keyword (if present)
    pub equation_keyword: Option<ir::ast::Token>,
    /// Token for "initial equation" keyword (if present)
    pub initial_equation_keyword: Option<ir::ast::Token>,
    /// Token for "algorithm" keyword (if present)
    pub algorithm_keyword: Option<ir::ast::Token>,
    /// Token for "initial algorithm" keyword (if present)
    pub initial_algorithm_keyword: Option<ir::ast::Token>,
    /// Annotation clause for this class
    pub annotation: Vec<ir::ast::Expression>,
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
                    // Merge public elements into composition
                    // Note: 'public' is the default visibility, so we just add them
                    comp.components
                        .extend(elem_list.element_list.components.clone());
                    comp.classes.extend(elem_list.element_list.classes.clone());
                    comp.extends.extend(elem_list.element_list.extends.clone());
                    comp.imports.extend(elem_list.element_list.imports.clone());
                }
                modelica_grammar_trait::CompositionListGroup::ProtectedElementList(elem_list) => {
                    // Merge protected elements into composition, marking them as protected
                    for (name, mut component) in elem_list.element_list.components.clone() {
                        component.is_protected = true;
                        comp.components.insert(name, component);
                    }
                    comp.classes.extend(elem_list.element_list.classes.clone());
                    comp.extends.extend(elem_list.element_list.extends.clone());
                    comp.imports.extend(elem_list.element_list.imports.clone());
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
                    // Store keyword tokens (only store the first occurrence)
                    if sec.initial {
                        if comp.initial_equation_keyword.is_none() {
                            // Use the initial keyword if present, otherwise use the equation keyword
                            comp.initial_equation_keyword = Some(
                                sec.initial_keyword
                                    .clone()
                                    .unwrap_or(sec.equation_keyword.clone()),
                            );
                        }
                    } else if comp.equation_keyword.is_none() {
                        comp.equation_keyword = Some(sec.equation_keyword.clone());
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
                        // Store keyword token (only store the first occurrence)
                        if comp.initial_algorithm_keyword.is_none() {
                            comp.initial_algorithm_keyword = Some(
                                sec.initial_keyword
                                    .clone()
                                    .unwrap_or(sec.algorithm_keyword.clone()),
                            );
                        }
                    } else {
                        comp.algorithms.push(algo);
                        if comp.algorithm_keyword.is_none() {
                            comp.algorithm_keyword = Some(sec.algorithm_keyword.clone());
                        }
                    }
                }
            }
        }

        // Extract annotation from composition_opt0
        if let Some(annotation_opt) = &ast.composition_opt0
            && let Some(class_mod_opt) = &annotation_opt
                .annotation_clause
                .class_modification
                .class_modification_opt
        {
            comp.annotation = class_mod_opt.argument_list.args.clone();
        }

        Ok(comp)
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

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
                            // Extract inner/outer flags from element definition
                            let is_inner = edef.element_definition.element_definition_opt1.is_some();
                            let is_outer = edef.element_definition.element_definition_opt2.is_some();

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

                            // Extract type-level array subscripts (e.g., Real[3] z)
                            // These apply to all components in this clause
                            let mut type_level_shape = Vec::new();
                            if let Some(clause_opt) = &clause.component_clause.component_clause_opt
                            {
                                for subscript in &clause_opt.array_subscripts.subscripts {
                                    if let ir::ast::Subscript::Expression(
                                        ir::ast::Expression::Terminal {
                                            token,
                                            terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                        },
                                    ) = subscript
                                        && let Ok(dim) = token.text.parse::<usize>() {
                                            type_level_shape.push(dim);
                                        }
                                }
                            }

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

                                // Compute location spanning from type_specifier to declaration ident
                                let comp_location = clause
                                    .component_clause
                                    .type_specifier
                                    .name
                                    .name
                                    .first()
                                    .map(|start_tok| span_location(start_tok, &c.declaration.ident))
                                    .unwrap_or_else(|| c.declaration.ident.location.clone());

                                // Extract condition attribute (e.g., `if use_reset`)
                                let condition = c
                                    .component_declaration_opt
                                    .as_ref()
                                    .map(|opt| opt.condition_attribute.expression.clone());

                                let mut value = ir::ast::Component {
                                    name: c.declaration.ident.text.clone(),
                                    name_token: c.declaration.ident.clone(),
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
                                    start_is_modification: false,
                                    start_has_each: false,
                                    shape: type_level_shape.clone(), // Start with type-level subscripts (Real[3] z)
                                    shape_expr: Vec::new(), // Raw dimension expressions for parameter-dependent sizes
                                    shape_is_modification: false,
                                    annotation,
                                    modifications: indexmap::IndexMap::new(),
                                    location: comp_location,
                                    condition,
                                    inner: is_inner,
                                    outer: is_outer,
                                    final_attributes: std::collections::HashSet::new(),
                                    is_protected: false,
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

                                // Append declaration-level subscripts (e.g., Real z[2] or Real z[n] or Real a[:]) to type-level shape
                                if let Some(decl_opt) = &c.declaration.declaration_opt {
                                    for subscript in &decl_opt.array_subscripts.subscripts {
                                        // Store the full subscript (Expression or Range) for formatting
                                        value.shape_expr.push(subscript.clone());
                                        // Also try to extract integer dimension if it's a literal expression
                                        if let ir::ast::Subscript::Expression(ir::ast::Expression::Terminal {
                                            token,
                                            terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                        }) = subscript
                                            && let Ok(dim) = token.text.parse::<usize>() {
                                                value.shape.push(dim);
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
                                                // Look for start=, shape=, and other parameter modifications
                                                for (idx, arg) in opt.argument_list.args.iter().enumerate() {
                                                    // Check for sub-modifications on builtin attributes like start(y=1)
                                                    // These are parsed as FunctionCall expressions
                                                    if let ir::ast::Expression::FunctionCall { comp, args: _ } = arg {
                                                        let func_name = comp.to_string();
                                                        // Valid built-in type attributes that cannot have sub-modifications
                                                        const ALL_BUILTIN_ATTRS: &[&str] = &[
                                                            "start", "fixed", "min", "max", "nominal",
                                                            "unit", "displayUnit", "quantity", "stateSelect",
                                                            "unbounded"
                                                        ];
                                                        let type_name_str = value.type_name.to_string();
                                                        let is_builtin = matches!(
                                                            type_name_str.as_str(),
                                                            "Real" | "Integer" | "Boolean" | "String"
                                                        );
                                                        if is_builtin && ALL_BUILTIN_ATTRS.contains(&func_name.as_str()) {
                                                            // Get location info for error message
                                                            let loc = if let Some(first) = comp.parts.first() {
                                                                loc_info(&first.ident)
                                                            } else {
                                                                String::new()
                                                            };
                                                            anyhow::bail!(
                                                                "Modified element {}.y not found in class {}{}",
                                                                func_name,
                                                                type_name_str,
                                                                loc
                                                            );
                                                        }
                                                    } else if let ir::ast::Expression::Binary { op, lhs, rhs } = arg
                                                        && matches!(op, ir::ast::OpBinary::Assign(_)) {
                                                            // This is a named argument like start=2.5, shape=(3), or R=10
                                                            if let ir::ast::Expression::ComponentReference(comp) = &**lhs {
                                                                let param_name = comp.to_string();
                                                                // Check if this argument has the `each` modifier
                                                                let has_each = opt.argument_list.each_flags.get(idx).copied().unwrap_or(false);
                                                                // Check if this argument has the `final` modifier
                                                                let has_final = opt.argument_list.final_flags.get(idx).copied().unwrap_or(false);
                                                                match param_name.as_str() {
                                                                    "start" => {
                                                                        value.start = (**rhs).clone();
                                                                        value.start_is_modification = true;
                                                                        value.start_has_each = has_each;
                                                                        if has_final {
                                                                            value.final_attributes.insert("start".to_string());
                                                                        }
                                                                    }
                                                                    "shape" => {
                                                                        // Extract shape from expression like (3) or {3, 2}
                                                                        match &**rhs {
                                                                            // Handle shape=3 - single dimension without parens
                                                                            ir::ast::Expression::Terminal {
                                                                                token,
                                                                                terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                                                            } => {
                                                                                if let Ok(dim) = token.text.parse::<usize>() {
                                                                                    value.shape = vec![dim];
                                                                                }
                                                                            }
                                                                            // Handle shape=(3) - single dimension with parens
                                                                            ir::ast::Expression::Parenthesized { inner } => {
                                                                                if let ir::ast::Expression::Terminal {
                                                                                    token,
                                                                                    terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                                                                } = &**inner
                                                                                    && let Ok(dim) = token.text.parse::<usize>() {
                                                                                        value.shape = vec![dim];
                                                                                    }
                                                                            }
                                                                            // Handle shape={3, 2} - multi-dimensional with array syntax
                                                                            ir::ast::Expression::Array { elements, .. } => {
                                                                                value.shape.clear();
                                                                                for elem in elements {
                                                                                    if let ir::ast::Expression::Terminal {
                                                                                        token,
                                                                                        terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                                                                    } = elem
                                                                                        && let Ok(dim) = token.text.parse::<usize>() {
                                                                                            value.shape.push(dim);
                                                                                        }
                                                                                }
                                                                            }
                                                                            // Handle shape=(3, 1) - multi-dimensional with tuple syntax
                                                                            ir::ast::Expression::Tuple { elements } => {
                                                                                value.shape.clear();
                                                                                for elem in elements {
                                                                                    if let ir::ast::Expression::Terminal {
                                                                                        token,
                                                                                        terminal_type: ir::ast::TerminalType::UnsignedInteger,
                                                                                    } = elem
                                                                                        && let Ok(dim) = token.text.parse::<usize>() {
                                                                                            value.shape.push(dim);
                                                                                        }
                                                                                }
                                                                            }
                                                                            _ => {}
                                                                        }
                                                                    }
                                                                    _ => {
                                                                        // Valid built-in type attributes
                                                                        const REAL_ATTRS: &[&str] = &[
                                                                            "start", "fixed", "min", "max", "nominal",
                                                                            "unit", "displayUnit", "quantity", "stateSelect",
                                                                            "unbounded", "each", "final"
                                                                        ];
                                                                        const INTEGER_ATTRS: &[&str] = &[
                                                                            "start", "fixed", "min", "max", "quantity",
                                                                            "each", "final"
                                                                        ];
                                                                        const BOOLEAN_ATTRS: &[&str] = &[
                                                                            "start", "fixed", "quantity", "each", "final"
                                                                        ];
                                                                        const STRING_ATTRS: &[&str] = &[
                                                                            "start", "fixed", "quantity", "each", "final"
                                                                        ];

                                                                        let type_name_str = value.type_name.to_string();
                                                                        let is_builtin = matches!(
                                                                            type_name_str.as_str(),
                                                                            "Real" | "Integer" | "Boolean" | "String"
                                                                        );

                                                                        if is_builtin {
                                                                            let valid_attrs = match type_name_str.as_str() {
                                                                                "Real" => REAL_ATTRS,
                                                                                "Integer" => INTEGER_ATTRS,
                                                                                "Boolean" => BOOLEAN_ATTRS,
                                                                                "String" => STRING_ATTRS,
                                                                                _ => &[],
                                                                            };

                                                                            if !valid_attrs.contains(&param_name.as_str()) {
                                                                                // Get location info for error message
                                                                                let loc = if let Some(first) = comp.parts.first() {
                                                                                    format!(
                                                                                        " at line {}, column {}",
                                                                                        first.ident.location.start_line,
                                                                                        first.ident.location.start_column
                                                                                    )
                                                                                } else {
                                                                                    String::new()
                                                                                };
                                                                                anyhow::bail!(
                                                                                    "Invalid modification '{}' for type '{}'{}\nValid attributes are: {}",
                                                                                    param_name,
                                                                                    type_name_str,
                                                                                    loc,
                                                                                    valid_attrs.join(", ")
                                                                                );
                                                                            }
                                                                        }

                                                                        // Store modification (for user-defined types or valid built-in attrs)
                                                                        value.modifications.insert(param_name.clone(), (**rhs).clone());
                                                                        // Track if this attribute is marked as final
                                                                        if has_final {
                                                                            value.final_attributes.insert(param_name);
                                                                        }
                                                                    }
                                                                }
                                                            }
                                                        }
                                                }
                                            }
                                            // Also check for binding expression after class modification
                                            // e.g., parameter Integer m(min=1) = 3
                                            // The "= 3" part is in modification_opt
                                            if let Some(mod_opt) = &class_mod.modification_opt {
                                                match &mod_opt.modification_expression {
                                                    modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                                                        value.start = expr.expression.clone();
                                                    }
                                                    modelica_grammar_trait::ModificationExpression::Break(_) => {
                                                        // 'break' means remove any inherited binding - do nothing
                                                        // The variable will have no binding equation
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
                                                modelica_grammar_trait::ModificationExpression::Break(_) => {
                                                    // 'break' means remove any inherited binding - do nothing
                                                    // The variable will have no binding equation
                                                }
                                            }
                                        }
                                    }
                                }

                                def.components
                                    .insert(c.declaration.ident.text.clone(), value);
                            }
                        }
                        modelica_grammar_trait::ElementDefinitionGroup::ReplaceableElementDefinitionGroupGroupElementDefinitionOpt3(repl) => {
                            // Handle replaceable ( class_definition | component_clause )
                            // Extract inner/outer flags from element definition
                            let is_inner_repl = edef.element_definition.element_definition_opt1.is_some();
                            let is_outer_repl = edef.element_definition.element_definition_opt2.is_some();

                            match &repl.element_definition_group_group {
                                modelica_grammar_trait::ElementDefinitionGroupGroup::ClassDefinition(class) => {
                                    let nested_class = class.class_definition.clone();
                                    let name = nested_class.name.text.clone();
                                    def.classes.insert(name, nested_class);
                                }
                                modelica_grammar_trait::ElementDefinitionGroupGroup::ComponentClause(clause) => {
                                    // Process replaceable component clause - simplified handling
                                    for c in &clause.component_clause.component_list.components {
                                        let comp_location = clause
                                            .component_clause
                                            .type_specifier
                                            .name
                                            .name
                                            .first()
                                            .map(|start_tok| span_location(start_tok, &c.declaration.ident))
                                            .unwrap_or_else(|| c.declaration.ident.location.clone());

                                        // Extract condition attribute for replaceable components
                                        let condition = c
                                            .component_declaration_opt
                                            .as_ref()
                                            .map(|opt| opt.condition_attribute.expression.clone());

                                        let value = ir::ast::Component {
                                            name: c.declaration.ident.text.clone(),
                                            name_token: c.declaration.ident.clone(),
                                            type_name: clause.component_clause.type_specifier.name.clone(),
                                            variability: ir::ast::Variability::Empty,
                                            causality: ir::ast::Causality::Empty,
                                            connection: ir::ast::Connection::Empty,
                                            description: c.description.description_string.tokens.clone(),
                                            start: ir::ast::Expression::Empty,
                                            start_is_modification: false,
                                            start_has_each: false,
                                            shape: Vec::new(),
                                            shape_expr: Vec::new(),
                                            shape_is_modification: false,
                                            annotation: Vec::new(),
                                            modifications: indexmap::IndexMap::new(),
                                            location: comp_location,
                                            condition,
                                            inner: is_inner_repl,
                                            outer: is_outer_repl,
                                            final_attributes: std::collections::HashSet::new(),
                                            is_protected: false,
                                        };

                                        def.components.insert(c.declaration.ident.text.clone(), value);
                                    }
                                }
                            }
                        }
                    }
                }
                modelica_grammar_trait::Element::ImportClause(import_elem) => {
                    let import_clause = &import_elem.import_clause;
                    // Get the location from the import keyword token
                    let location = import_clause.import.import.location.clone();
                    let parsed_import = match &import_clause.import_clause_group {
                        // import D = A.B.C; (renamed import)
                        modelica_grammar_trait::ImportClauseGroup::IdentEquName(renamed) => {
                            ir::ast::Import::Renamed {
                                alias: renamed.ident.clone(),
                                path: renamed.name.clone(),
                                location,
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
                                    ir::ast::Import::Qualified { path, location }
                                }
                                Some(opt) => {
                                    match &opt.import_clause_opt_group {
                                        // import A.B.*;
                                        modelica_grammar_trait::ImportClauseOptGroup::DotStar(
                                            _,
                                        ) => ir::ast::Import::Unqualified { path, location },
                                        // import A.B.* or import A.B.{C, D}
                                        modelica_grammar_trait::ImportClauseOptGroup::DotImportClauseOptGroupGroup(dot_group) => {
                                            match &dot_group.import_clause_opt_group_group {
                                                // import A.B.*
                                                modelica_grammar_trait::ImportClauseOptGroupGroup::Star(_) => {
                                                    ir::ast::Import::Unqualified { path, location }
                                                }
                                                // import A.B.{C, D, E}
                                                modelica_grammar_trait::ImportClauseOptGroupGroup::LBraceImportListRBrace(list) => {
                                                    let mut names = vec![list.import_list.ident.clone()];
                                                    for item in &list.import_list.import_list_list {
                                                        names.push(item.ident.clone());
                                                    }
                                                    ir::ast::Import::Selective { path, names, location }
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
                    // Compute location spanning from 'extends' keyword to end of type_specifier
                    let extend_location = clause
                        .extends_clause
                        .type_specifier
                        .name
                        .name
                        .last()
                        .map(|end_tok| {
                            span_location(&clause.extends_clause.extends.extends, end_tok)
                        })
                        .unwrap_or_else(|| clause.extends_clause.extends.extends.location.clone());

                    // Extract modifications from extends clause if present
                    let modifications = if let Some(ext_opt) =
                        &clause.extends_clause.extends_clause_opt
                    {
                        // class_or_inheritance_modification contains the modifications
                        if let Some(mod_opt) = &ext_opt
                            .class_or_inheritance_modification
                            .class_or_inheritance_modification_opt
                        {
                            // Extract all arguments from the modification list
                            let list = &mod_opt.argument_or_inheritance_modification_list;
                            let mut mods = Vec::new();

                            // First item
                            match &list.argument_or_inheritance_modification_list_group {
                                modelica_grammar_trait::ArgumentOrInheritanceModificationListGroup::Argument(arg) => {
                                    mods.push(arg.argument.expression.clone());
                                }
                                modelica_grammar_trait::ArgumentOrInheritanceModificationListGroup::InheritanceModification(_) => {
                                    // Inheritance modifications (break/connect) not yet supported
                                }
                            }

                            // Remaining items
                            for item in &list.argument_or_inheritance_modification_list_list {
                                match &item.argument_or_inheritance_modification_list_list_group {
                                    modelica_grammar_trait::ArgumentOrInheritanceModificationListListGroup::Argument(arg) => {
                                        mods.push(arg.argument.expression.clone());
                                    }
                                    modelica_grammar_trait::ArgumentOrInheritanceModificationListListGroup::InheritanceModification(_) => {
                                        // Inheritance modifications (break/connect) not yet supported
                                    }
                                }
                            }

                            mods
                        } else {
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    };

                    // Note: Annotations in extends clauses are currently ignored
                    // if clause.extends_clause.extends_clause_opt0.is_some() { ... }

                    def.extends.push(ir::ast::Extend {
                        comp: clause.extends_clause.type_specifier.name.clone(),
                        location: extend_location,
                        modifications,
                    });
                }
            }
        }
        Ok(def)
    }
}
