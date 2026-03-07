//! Conversion for class definitions and composition structures.

use super::expressions::ExpressionList;
use super::helpers::span_location;
use super::sections::{AlgorithmSection, EquationSection};
use crate::errors::semantic_error_from_token;
use crate::generated::modelica_grammar_trait;
use indexmap::IndexMap;
use std::sync::Arc;

//-----------------------------------------------------------------------------
// Helper functions to reduce nesting in conversion code.

/// Extract modifiers from an optional extends class specifier.
fn extract_extends_modifiers(
    opt: &Option<modelica_grammar_trait::ExtendsClassSpecifierOpt>,
) -> Vec<rumoca_ir_ast::ExtendModification> {
    let Some(class_mod) = opt else {
        return vec![];
    };
    let Some(arg_list) = &class_mod.class_modification.class_modification_opt else {
        return vec![];
    };
    // Zip the three parallel arrays into ExtendModification structs
    arg_list
        .argument_list
        .args
        .iter()
        .zip(arg_list.argument_list.each_flags.iter())
        .zip(arg_list.argument_list.redeclare_flags.iter())
        .map(
            |((expr, &each), &redeclare)| rumoca_ir_ast::ExtendModification {
                expr: expr.clone(),
                each,
                redeclare,
            },
        )
        .collect()
}

/// Extract enumeration literals from an EnumClassSpecifierGroup.
fn extract_enum_literals(
    group: &modelica_grammar_trait::EnumClassSpecifierGroup,
) -> Vec<rumoca_ir_ast::EnumLiteral> {
    let modelica_grammar_trait::EnumClassSpecifierGroup::EnumClassSpecifierOpt(opt) = group else {
        return vec![]; // Colon case: enumeration(:)
    };
    let Some(list_opt) = &opt.enum_class_specifier_opt else {
        return vec![];
    };
    let list = &list_opt.enum_list;

    let mut literals = vec![make_enum_literal(&list.enumeration_literal)];
    for item in &list.enum_list_list {
        literals.push(make_enum_literal(&item.enumeration_literal));
    }
    literals
}

/// Create an EnumLiteral from an EnumerationLiteral grammar node.
fn make_enum_literal(
    lit: &modelica_grammar_trait::EnumerationLiteral,
) -> rumoca_ir_ast::EnumLiteral {
    rumoca_ir_ast::EnumLiteral {
        ident: lit.ident.clone(),
        description: lit.description.description_string.tokens.clone(),
    }
}

/// Extract causality from a BasePrefix.
fn extract_causality_from_base_prefix(
    base_prefix: &modelica_grammar_trait::BasePrefix,
) -> rumoca_ir_core::Causality {
    let Some(opt) = &base_prefix.base_prefix_opt else {
        return rumoca_ir_core::Causality::Empty;
    };
    match &opt.base_prefix_opt_group {
        modelica_grammar_trait::BasePrefixOptGroup::Input(inp) => {
            rumoca_ir_core::Causality::Input(inp.input.input.clone().into())
        }
        modelica_grammar_trait::BasePrefixOptGroup::Output(out) => {
            rumoca_ir_core::Causality::Output(out.output.output.clone().into())
        }
    }
}

/// Extract array subscripts from TypeClassSpecifierOpt.
fn extract_type_array_subscripts(
    opt: &Option<modelica_grammar_trait::TypeClassSpecifierOpt>,
) -> Vec<rumoca_ir_ast::Subscript> {
    opt.as_ref()
        .map(|o| o.array_subscripts.subscripts.clone())
        .unwrap_or_default()
}

/// Extract modifications from TypeClassSpecifierOpt0.
fn extract_type_class_mods(
    opt: &Option<modelica_grammar_trait::TypeClassSpecifierOpt0>,
) -> Vec<rumoca_ir_ast::ExtendModification> {
    let Some(class_mod_opt0) = opt else {
        return vec![];
    };
    let Some(arg_list) = &class_mod_opt0.class_modification.class_modification_opt else {
        return vec![];
    };
    // Zip the three parallel arrays into ExtendModification structs
    arg_list
        .argument_list
        .args
        .iter()
        .zip(arg_list.argument_list.each_flags.iter())
        .zip(arg_list.argument_list.redeclare_flags.iter())
        .map(
            |((expr, &each), &redeclare)| rumoca_ir_ast::ExtendModification {
                expr: expr.clone(),
                each,
                redeclare,
            },
        )
        .collect()
}

/// Collect named arguments recursively for partial function applications (MLS §12.4.2.1).
fn collect_named_args(
    named_args: &modelica_grammar_trait::NamedArguments,
    mods: &mut Vec<rumoca_ir_ast::Expression>,
) {
    let arg = &named_args.named_argument;
    let lhs = rumoca_ir_ast::Expression::ComponentReference(rumoca_ir_ast::ComponentReference {
        parts: vec![rumoca_ir_ast::ComponentRefPart {
            ident: arg.ident.clone(),
            subs: None,
        }],
        local: false,
        ..Default::default()
    });
    let rhs = arg.function_argument.clone();
    mods.push(rumoca_ir_ast::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Assign(arg.ident.clone()),
        lhs: Arc::new(lhs),
        rhs: Arc::new(rhs),
    });
    if let Some(opt) = &named_args.named_arguments_opt {
        collect_named_args(&opt.named_arguments, mods);
    }
}

/// Extract partial application modifications from FunctionPartialApplicationOpt.
fn extract_partial_app_mods(
    opt: &Option<modelica_grammar_trait::FunctionPartialApplicationOpt>,
) -> Vec<rumoca_ir_ast::Expression> {
    let Some(args_opt) = opt else {
        return Vec::new();
    };
    let mut modifications = Vec::new();
    collect_named_args(&args_opt.named_arguments, &mut modifications);
    modifications
}

/// Merge components into composition, checking for duplicates.
/// Returns Err if a duplicate is found.
fn merge_components(
    target: &mut IndexMap<String, rumoca_ir_ast::Component>,
    source: IndexMap<String, rumoca_ir_ast::Component>,
    set_protected: bool,
) -> Result<(), anyhow::Error> {
    for (name, mut component) in source {
        if let Some(existing) = target.get(&name) {
            return Err(semantic_error_from_token(
                format!(
                    "Duplicate declaration of '{}' at line {} (first declared at line {})",
                    name,
                    component.name_token.location.start_line,
                    existing.name_token.location.start_line
                ),
                &component.name_token,
            ));
        }
        if set_protected {
            component.is_protected = true;
        }
        target.insert(name, component);
    }
    Ok(())
}

/// Merge classes into composition, optionally marking as protected.
fn merge_classes(
    target: &mut IndexMap<String, rumoca_ir_ast::ClassDef>,
    source: IndexMap<String, rumoca_ir_ast::ClassDef>,
    set_protected: bool,
) {
    for (name, mut class) in source {
        if set_protected {
            class.is_protected = true;
        }
        target.insert(name, class);
    }
}

/// Process equation section, adding equations to appropriate list.
fn process_equation_section(comp: &mut Composition, sec: &EquationSection) {
    for eq in &sec.equations {
        if sec.initial {
            comp.initial_equations.push(eq.clone());
        } else {
            comp.equations.push(eq.clone());
        }
    }
    // Store keyword tokens (only store the first occurrence)
    let keyword = sec
        .initial_keyword
        .clone()
        .unwrap_or(sec.equation_keyword.clone());
    if sec.initial && comp.initial_equation_keyword.is_none() {
        comp.initial_equation_keyword = Some(keyword);
    } else if !sec.initial && comp.equation_keyword.is_none() {
        comp.equation_keyword = Some(sec.equation_keyword.clone());
    }
}

/// Process algorithm section, adding algorithms to appropriate list.
fn process_algorithm_section(comp: &mut Composition, sec: &AlgorithmSection) {
    let algo = sec.statements.to_vec();
    let keyword = sec
        .initial_keyword
        .clone()
        .unwrap_or(sec.algorithm_keyword.clone());
    if sec.initial {
        comp.initial_algorithms.push(algo);
        if comp.initial_algorithm_keyword.is_none() {
            comp.initial_algorithm_keyword = Some(keyword);
        }
        return;
    }
    comp.algorithms.push(algo);
    if comp.algorithm_keyword.is_none() {
        comp.algorithm_keyword = Some(sec.algorithm_keyword.clone());
    }
}

/// Validate annotation modifiers - 'each' and 'final' are forbidden in annotations (MLS §18.1).
fn validate_annotation_modifiers(
    arg_list: &ExpressionList,
    annotation_token: &rumoca_ir_core::Token,
) -> Result<(), anyhow::Error> {
    for each in &arg_list.each_flags {
        if *each {
            return Err(semantic_error_from_token(
                "MLS §18.1: 'each' modifier is not allowed in annotations",
                annotation_token,
            ));
        }
    }
    for is_final in &arg_list.final_flags {
        if *is_final {
            return Err(semantic_error_from_token(
                "MLS §18.1: 'final' modifier is not allowed in annotations",
                annotation_token,
            ));
        }
    }
    Ok(())
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::StoredDefinition> for rumoca_ir_ast::StoredDefinition {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::StoredDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let mut def = rumoca_ir_ast::StoredDefinition {
            classes: IndexMap::new(),
            ..Default::default()
        };

        // Predefined types that cannot be redeclared (MLS §4.8)
        const PREDEFINED_TYPES: &[&str] = &["Real", "Integer", "Boolean", "String"];

        for class in &ast.stored_definition_list {
            let class_name = &class.class_definition.name.text;

            // Check for redeclaration of predefined types
            if PREDEFINED_TYPES.contains(&&**class_name) {
                return Err(semantic_error_from_token(
                    format!("Cannot redeclare predefined type '{class_name}'"),
                    &class.class_definition.name,
                ));
            }

            def.classes
                .insert(class_name.to_string(), class.class_definition.clone());
        }
        def.within = ast.stored_definition_opt.as_ref().map(|within_clause| {
            within_clause
                .stored_definition_opt1
                .as_ref()
                .map(|w| w.name.clone())
                .unwrap_or_else(|| rumoca_ir_ast::Name {
                    name: vec![],
                    def_id: None,
                })
        });
        Ok(def)
    }
}

//-----------------------------------------------------------------------------
/// Validate class-specific restrictions per Modelica spec
///
/// - Connectors cannot have equation/algorithm sections or protected elements
/// - Packages cannot have equation/algorithm sections or non-constant components
/// - Records cannot have equation/algorithm sections
/// - Functions cannot have equation sections (only algorithm sections)
fn validate_class_restrictions(class_def: &rumoca_ir_ast::ClassDef) -> anyhow::Result<()> {
    match class_def.class_type {
        rumoca_ir_ast::ClassType::Connector => validate_connector_restrictions(class_def)?,
        rumoca_ir_ast::ClassType::Package => validate_package_restrictions(class_def)?,
        rumoca_ir_ast::ClassType::Record => validate_record_restrictions(class_def)?,
        rumoca_ir_ast::ClassType::Function => validate_function_restrictions(class_def)?,
        _ => {}
    }

    // Check that the end name matches the class name
    if let Some(end_name) = &class_def.end_name_token
        && end_name.text != class_def.name.text
    {
        return Err(semantic_error_from_token(
            format!(
                "End name '{}' does not match class name '{}' (line {})",
                end_name.text, class_def.name.text, end_name.location.start_line
            ),
            end_name,
        ));
    }

    Ok(())
}

fn class_restriction_error(class_def: &rumoca_ir_ast::ClassDef, message: String) -> anyhow::Error {
    semantic_error_from_token(message, &class_def.name)
}

fn validate_connector_restrictions(class_def: &rumoca_ir_ast::ClassDef) -> anyhow::Result<()> {
    if !class_def.equations.is_empty() || !class_def.initial_equations.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Connector '{}' cannot have equation sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    if !class_def.algorithms.is_empty() || !class_def.initial_algorithms.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Connector '{}' cannot have algorithm sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    if class_def.components.values().any(|c| c.is_protected) {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Connector '{}' cannot have protected elements (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    Ok(())
}

fn validate_package_restrictions(class_def: &rumoca_ir_ast::ClassDef) -> anyhow::Result<()> {
    if !class_def.equations.is_empty() || !class_def.initial_equations.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Package '{}' cannot have equation sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    if !class_def.algorithms.is_empty() || !class_def.initial_algorithms.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Package '{}' cannot have algorithm sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    for (name, comp) in &class_def.components {
        if !matches!(comp.variability, rumoca_ir_core::Variability::Constant(_)) {
            return Err(class_restriction_error(
                class_def,
                format!(
                    "Package '{}' can only contain constants, not '{}' (line {})",
                    class_def.name.text, name, class_def.location.start_line
                ),
            ));
        }
    }
    Ok(())
}

fn validate_record_restrictions(class_def: &rumoca_ir_ast::ClassDef) -> anyhow::Result<()> {
    if !class_def.equations.is_empty() || !class_def.initial_equations.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Record '{}' cannot have equation sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    if !class_def.algorithms.is_empty() || !class_def.initial_algorithms.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Record '{}' cannot have algorithm sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    Ok(())
}

fn validate_function_restrictions(class_def: &rumoca_ir_ast::ClassDef) -> anyhow::Result<()> {
    if !class_def.equations.is_empty() || !class_def.initial_equations.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Function '{}' cannot have equation sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    if !class_def.initial_algorithms.is_empty() {
        return Err(class_restriction_error(
            class_def,
            format!(
                "Function '{}' cannot have initial algorithm sections (line {})",
                class_def.name.text, class_def.location.start_line
            ),
        ));
    }
    Ok(())
}

/// Convert grammar ClassType to IR ClassType
fn convert_class_type(class_type: &modelica_grammar_trait::ClassType) -> rumoca_ir_ast::ClassType {
    match class_type {
        modelica_grammar_trait::ClassType::Class(_) => rumoca_ir_ast::ClassType::Class,
        modelica_grammar_trait::ClassType::Model(_) => rumoca_ir_ast::ClassType::Model,
        modelica_grammar_trait::ClassType::ClassTypeOptRecord(_) => {
            rumoca_ir_ast::ClassType::Record
        }
        modelica_grammar_trait::ClassType::Block(_) => rumoca_ir_ast::ClassType::Block,
        modelica_grammar_trait::ClassType::ClassTypeOpt0Connector(_) => {
            rumoca_ir_ast::ClassType::Connector
        }
        modelica_grammar_trait::ClassType::Type(_) => rumoca_ir_ast::ClassType::Type,
        modelica_grammar_trait::ClassType::Package(_) => rumoca_ir_ast::ClassType::Package,
        modelica_grammar_trait::ClassType::ClassTypeOpt1ClassTypeOpt2Function(_) => {
            rumoca_ir_ast::ClassType::Function
        }
        modelica_grammar_trait::ClassType::Operator(_) => rumoca_ir_ast::ClassType::Operator,
    }
}

/// Check if the class type is an expandable connector (MLS §9.1.3)
fn is_expandable_connector(class_type: &modelica_grammar_trait::ClassType) -> bool {
    if let modelica_grammar_trait::ClassType::ClassTypeOpt0Connector(c) = class_type {
        c.class_type_opt0.is_some()
    } else {
        false
    }
}

/// Check if the class type is an operator record (MLS §14)
fn is_operator_record(class_type: &modelica_grammar_trait::ClassType) -> bool {
    if let modelica_grammar_trait::ClassType::ClassTypeOptRecord(r) = class_type {
        r.class_type_opt.is_some()
    } else {
        false
    }
}

/// Check if a function is pure (MLS §12.3).
/// Functions are pure by default. Returns false only when declared with `impure`.
fn is_pure_function(class_type: &modelica_grammar_trait::ClassType) -> bool {
    if let modelica_grammar_trait::ClassType::ClassTypeOpt1ClassTypeOpt2Function(f) = class_type {
        // Check if impure keyword is present
        if let Some(ref opt1) = f.class_type_opt1 {
            // If it's Impure, return false; if Pure or absent, return true
            !matches!(
                opt1.class_type_opt1_group,
                modelica_grammar_trait::ClassTypeOpt1Group::Impure(_)
            )
        } else {
            // No pure/impure keyword means pure by default
            true
        }
    } else {
        // Non-function classes - default to true (doesn't really matter)
        true
    }
}

/// Extract the keyword token from grammar ClassType for semantic highlighting
fn get_class_type_token(class_type: &modelica_grammar_trait::ClassType) -> rumoca_ir_core::Token {
    match class_type {
        modelica_grammar_trait::ClassType::Class(c) => c.class.class.clone().into(),
        modelica_grammar_trait::ClassType::Model(m) => m.model.model.clone().into(),
        modelica_grammar_trait::ClassType::ClassTypeOptRecord(r) => r.record.record.clone().into(),
        modelica_grammar_trait::ClassType::Block(b) => b.block.block.clone().into(),
        modelica_grammar_trait::ClassType::ClassTypeOpt0Connector(c) => {
            c.connector.connector.clone().into()
        }
        modelica_grammar_trait::ClassType::Type(t) => t.r#type.r#type.clone().into(),
        modelica_grammar_trait::ClassType::Package(p) => p.package.package.clone().into(),
        modelica_grammar_trait::ClassType::ClassTypeOpt1ClassTypeOpt2Function(f) => {
            f.function.function.clone().into()
        }
        modelica_grammar_trait::ClassType::Operator(o) => o.operator.operator.clone().into(),
    }
}

//-----------------------------------------------------------------------------
// Helper functions for converting class specifiers to reduce nesting

/// Context for class conversion - common fields from ClassDefinition
struct ClassConversionContext {
    class_type: rumoca_ir_ast::ClassType,
    class_type_token: rumoca_ir_core::Token,
    encapsulated: bool,
    partial: bool,
    expandable: bool,
    operator_record: bool,
    /// True if the function is pure (MLS §12.3). Functions are pure by default.
    pure: bool,
}

impl ClassConversionContext {
    fn from_ast(ast: &modelica_grammar_trait::ClassDefinition) -> Self {
        Self {
            class_type: convert_class_type(&ast.class_prefixes.class_type),
            class_type_token: get_class_type_token(&ast.class_prefixes.class_type),
            encapsulated: ast.class_definition_opt.is_some(),
            partial: ast.class_prefixes.class_prefixes_opt.is_some(),
            expandable: is_expandable_connector(&ast.class_prefixes.class_type),
            operator_record: is_operator_record(&ast.class_prefixes.class_type),
            pure: is_pure_function(&ast.class_prefixes.class_type),
        }
    }
}

/// Convert a standard class specifier to ClassDef.
fn convert_standard_class_specifier(
    spec: &modelica_grammar_trait::StandardClassSpecifier,
    ctx: &ClassConversionContext,
) -> Result<rumoca_ir_ast::ClassDef, anyhow::Error> {
    let class_def = rumoca_ir_ast::ClassDef {
        def_id: None,
        scope_id: None,
        name: spec.name.clone(),
        class_type: ctx.class_type.clone(),
        class_type_token: ctx.class_type_token.clone(),
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
        encapsulated: ctx.encapsulated,
        partial: ctx.partial,
        expandable: ctx.expandable,
        operator_record: ctx.operator_record,
        pure: ctx.pure,
        causality: rumoca_ir_core::Causality::Empty,
        equation_keyword: spec.composition.equation_keyword.clone(),
        initial_equation_keyword: spec.composition.initial_equation_keyword.clone(),
        algorithm_keyword: spec.composition.algorithm_keyword.clone(),
        initial_algorithm_keyword: spec.composition.initial_algorithm_keyword.clone(),
        end_name_token: Some(spec.ident.clone()),
        enum_literals: vec![],
        annotation: spec.composition.annotation.clone(),
        is_protected: false,
        is_final: false,
        is_replaceable: false,
        constrainedby: None,
        array_subscripts: vec![],
        external: spec.composition.external.clone(),
    };
    validate_class_restrictions(&class_def)?;
    Ok(class_def)
}

/// Convert an extends class specifier to ClassDef.
fn convert_extends_class_specifier(
    spec: &modelica_grammar_trait::ExtendsClassSpecifier,
    ctx: &ClassConversionContext,
) -> Result<rumoca_ir_ast::ClassDef, anyhow::Error> {
    // Create an extends clause for the inherited class
    let extends_modifiers = extract_extends_modifiers(&spec.extends_class_specifier_opt);
    let extends_name = rumoca_ir_ast::Name {
        name: vec![spec.ident.clone()],
        def_id: None,
    };
    let inherited_extends = rumoca_ir_ast::Extend {
        base_name: extends_name,
        base_def_id: None,
        location: spec.ident.location.clone(),
        modifications: extends_modifiers,
        break_names: vec![],
        is_protected: false,
        annotation: vec![],
    };

    // Combine inherited extends with composition extends
    let mut all_extends = vec![inherited_extends];
    all_extends.extend(spec.composition.extends.clone());

    let class_def = rumoca_ir_ast::ClassDef {
        def_id: None,
        scope_id: None,
        name: spec.ident.clone(),
        class_type: ctx.class_type.clone(),
        class_type_token: ctx.class_type_token.clone(),
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
        encapsulated: ctx.encapsulated,
        partial: ctx.partial,
        expandable: ctx.expandable,
        operator_record: ctx.operator_record,
        pure: ctx.pure,
        causality: rumoca_ir_core::Causality::Empty,
        equation_keyword: spec.composition.equation_keyword.clone(),
        initial_equation_keyword: spec.composition.initial_equation_keyword.clone(),
        algorithm_keyword: spec.composition.algorithm_keyword.clone(),
        initial_algorithm_keyword: spec.composition.initial_algorithm_keyword.clone(),
        end_name_token: Some(spec.ident0.clone()),
        enum_literals: vec![],
        annotation: spec.composition.annotation.clone(),
        is_protected: false,
        is_final: false,
        is_replaceable: false,
        constrainedby: None,
        array_subscripts: vec![],
        external: spec.composition.external.clone(),
    };
    validate_class_restrictions(&class_def)?;
    Ok(class_def)
}

/// Convert an enum class specifier to ClassDef.
fn convert_enum_class_specifier(
    enum_spec: &modelica_grammar_trait::EnumClassSpecifier,
    ctx: &ClassConversionContext,
) -> rumoca_ir_ast::ClassDef {
    let enum_literals = extract_enum_literals(&enum_spec.enum_class_specifier_group);
    rumoca_ir_ast::ClassDef {
        def_id: None,
        scope_id: None,
        name: enum_spec.ident.clone(),
        class_type: rumoca_ir_ast::ClassType::Type,
        class_type_token: ctx.class_type_token.clone(),
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
        encapsulated: ctx.encapsulated,
        partial: ctx.partial,
        expandable: false,
        operator_record: false,
        pure: true, // Enums are not functions
        causality: rumoca_ir_core::Causality::Empty,
        equation_keyword: None,
        initial_equation_keyword: None,
        algorithm_keyword: None,
        initial_algorithm_keyword: None,
        end_name_token: None,
        enum_literals,
        annotation: vec![],
        is_protected: false,
        is_final: false,
        is_replaceable: false,
        constrainedby: None,
        array_subscripts: vec![],
        external: None, // Enums don't have external declarations
    }
}

/// Convert a type class specifier to ClassDef.
fn convert_type_class_specifier(
    type_spec: &modelica_grammar_trait::TypeClassSpecifier,
    ctx: &ClassConversionContext,
) -> rumoca_ir_ast::ClassDef {
    let base_type_name = type_spec.type_specifier.name.clone();
    let causality = extract_causality_from_base_prefix(&type_spec.base_prefix);
    let array_subscripts = extract_type_array_subscripts(&type_spec.type_class_specifier_opt);
    let modifications = extract_type_class_mods(&type_spec.type_class_specifier_opt0);

    let extend = rumoca_ir_ast::Extend {
        base_name: base_type_name,
        base_def_id: None,
        location: type_spec.ident.location.clone(),
        modifications,
        break_names: vec![],
        is_protected: false,
        annotation: vec![],
    };

    rumoca_ir_ast::ClassDef {
        def_id: None,
        scope_id: None,
        name: type_spec.ident.clone(),
        class_type: ctx.class_type.clone(),
        class_type_token: ctx.class_type_token.clone(),
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
        encapsulated: ctx.encapsulated,
        partial: ctx.partial,
        expandable: ctx.expandable,
        operator_record: ctx.operator_record,
        pure: ctx.pure,
        causality,
        equation_keyword: None,
        initial_equation_keyword: None,
        algorithm_keyword: None,
        initial_algorithm_keyword: None,
        end_name_token: None,
        enum_literals: vec![],
        annotation: vec![],
        is_protected: false,
        is_final: false,
        is_replaceable: false,
        constrainedby: None,
        array_subscripts,
        external: None, // Type aliases don't have external declarations
    }
}

/// Convert a function partial class specifier to ClassDef.
fn convert_function_partial_class_specifier(
    partial_spec: &modelica_grammar_trait::FunctionPartialClassSpecifier,
    ctx: &ClassConversionContext,
) -> rumoca_ir_ast::ClassDef {
    let base_func_name = partial_spec
        .function_partial_application
        .type_specifier
        .name
        .clone();

    // Extract named argument modifications and convert to ExtendModification
    let raw_mods = extract_partial_app_mods(
        &partial_spec
            .function_partial_application
            .function_partial_application_opt,
    );
    let modifications: Vec<rumoca_ir_ast::ExtendModification> = raw_mods
        .into_iter()
        .map(|expr| rumoca_ir_ast::ExtendModification {
            expr,
            each: false,
            redeclare: false,
        })
        .collect();

    let extend = rumoca_ir_ast::Extend {
        base_name: base_func_name,
        base_def_id: None,
        location: partial_spec.ident.location.clone(),
        modifications,
        break_names: vec![],
        is_protected: false,
        annotation: vec![],
    };

    rumoca_ir_ast::ClassDef {
        def_id: None,
        scope_id: None,
        name: partial_spec.ident.clone(),
        class_type: ctx.class_type.clone(),
        class_type_token: ctx.class_type_token.clone(),
        description: vec![],
        location: partial_spec.ident.location.clone(),
        extends: vec![extend],
        imports: vec![],
        classes: IndexMap::new(),
        equations: vec![],
        algorithms: vec![],
        initial_equations: vec![],
        initial_algorithms: vec![],
        components: IndexMap::new(),
        encapsulated: ctx.encapsulated,
        partial: ctx.partial,
        expandable: false,
        operator_record: false,
        pure: ctx.pure,
        causality: rumoca_ir_core::Causality::Empty,
        equation_keyword: None,
        initial_equation_keyword: None,
        algorithm_keyword: None,
        initial_algorithm_keyword: None,
        end_name_token: None,
        enum_literals: vec![],
        annotation: vec![],
        is_protected: false,
        is_final: false,
        is_replaceable: false,
        constrainedby: None,
        array_subscripts: vec![],
        external: None, // Function partial applications don't have external declarations
    }
}

/// Convert a der class specifier to ClassDef.
///
/// Modelica short form:
/// `function f_der = der(f, x, y);`
///
/// This is represented as a short-form class extending the referenced base function.
/// The derivative variable list is accepted by the parser and retained in source,
/// but is not yet lowered into dedicated derivative metadata in ClassDef.
fn convert_der_class_specifier(
    der_spec: &modelica_grammar_trait::DerClassSpecifier,
    ctx: &ClassConversionContext,
) -> rumoca_ir_ast::ClassDef {
    let extend = rumoca_ir_ast::Extend {
        base_name: der_spec.type_specifier.name.clone(),
        base_def_id: None,
        location: der_spec.ident.location.clone(),
        modifications: vec![],
        break_names: vec![],
        is_protected: false,
        annotation: vec![],
    };

    rumoca_ir_ast::ClassDef {
        def_id: None,
        scope_id: None,
        name: der_spec.ident.clone(),
        class_type: ctx.class_type.clone(),
        class_type_token: ctx.class_type_token.clone(),
        description: der_spec.description.description_string.tokens.clone(),
        location: der_spec.ident.location.clone(),
        extends: vec![extend],
        imports: vec![],
        classes: IndexMap::new(),
        equations: vec![],
        algorithms: vec![],
        initial_equations: vec![],
        initial_algorithms: vec![],
        components: IndexMap::new(),
        encapsulated: ctx.encapsulated,
        partial: ctx.partial,
        expandable: false,
        operator_record: false,
        pure: ctx.pure,
        causality: rumoca_ir_core::Causality::Empty,
        equation_keyword: None,
        initial_equation_keyword: None,
        algorithm_keyword: None,
        initial_algorithm_keyword: None,
        end_name_token: None,
        enum_literals: vec![],
        annotation: vec![],
        is_protected: false,
        is_final: false,
        is_replaceable: false,
        constrainedby: None,
        array_subscripts: vec![],
        external: None,
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ClassDefinition> for rumoca_ir_ast::ClassDef {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ClassDefinition,
    ) -> std::result::Result<Self, Self::Error> {
        let ctx = ClassConversionContext::from_ast(ast);

        match &ast.class_specifier {
            modelica_grammar_trait::ClassSpecifier::LongClassSpecifier(long) => {
                match &long.long_class_specifier {
                    modelica_grammar_trait::LongClassSpecifier::StandardClassSpecifier(spec) => {
                        convert_standard_class_specifier(&spec.standard_class_specifier, &ctx)
                    }
                    modelica_grammar_trait::LongClassSpecifier::ExtendsClassSpecifier(ext) => {
                        convert_extends_class_specifier(&ext.extends_class_specifier, &ctx)
                    }
                }
            }
            modelica_grammar_trait::ClassSpecifier::DerClassSpecifier(spec) => {
                Ok(convert_der_class_specifier(&spec.der_class_specifier, &ctx))
            }
            modelica_grammar_trait::ClassSpecifier::ShortClassSpecifier(short) => {
                match &short.short_class_specifier {
                    modelica_grammar_trait::ShortClassSpecifier::EnumClassSpecifier(spec) => Ok(
                        convert_enum_class_specifier(&spec.enum_class_specifier, &ctx),
                    ),
                    modelica_grammar_trait::ShortClassSpecifier::TypeClassSpecifier(spec) => Ok(
                        convert_type_class_specifier(&spec.type_class_specifier, &ctx),
                    ),
                    modelica_grammar_trait::ShortClassSpecifier::FunctionPartialClassSpecifier(
                        spec,
                    ) => Ok(convert_function_partial_class_specifier(
                        &spec.function_partial_class_specifier,
                        &ctx,
                    )),
                }
            }
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct Composition {
    pub extends: Vec<rumoca_ir_ast::Extend>,
    pub imports: Vec<rumoca_ir_ast::Import>,
    pub components: IndexMap<String, rumoca_ir_ast::Component>,
    pub classes: IndexMap<String, rumoca_ir_ast::ClassDef>,
    pub equations: Vec<rumoca_ir_ast::Equation>,
    pub initial_equations: Vec<rumoca_ir_ast::Equation>,
    pub algorithms: Vec<Vec<rumoca_ir_ast::Statement>>,
    pub initial_algorithms: Vec<Vec<rumoca_ir_ast::Statement>>,
    /// Token for "equation" keyword (if present)
    pub equation_keyword: Option<rumoca_ir_core::Token>,
    /// Token for "initial equation" keyword (if present)
    pub initial_equation_keyword: Option<rumoca_ir_core::Token>,
    /// Token for "algorithm" keyword (if present)
    pub algorithm_keyword: Option<rumoca_ir_core::Token>,
    /// Token for "initial algorithm" keyword (if present)
    pub initial_algorithm_keyword: Option<rumoca_ir_core::Token>,
    /// Annotation clause for this class
    pub annotation: Vec<rumoca_ir_ast::Expression>,
    /// External function declaration (MLS §12.9)
    pub external: Option<rumoca_ir_ast::ExternalFunction>,
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
                    // Merge public elements into composition (default visibility)
                    merge_components(
                        &mut comp.components,
                        elem_list.element_list.components.clone(),
                        false,
                    )?;
                    comp.classes.extend(elem_list.element_list.classes.clone());
                    comp.extends.extend(elem_list.element_list.extends.clone());
                    comp.imports.extend(elem_list.element_list.imports.clone());
                }
                modelica_grammar_trait::CompositionListGroup::ProtectedElementList(elem_list) => {
                    // Merge protected elements into composition
                    merge_components(
                        &mut comp.components,
                        elem_list.element_list.components.clone(),
                        true,
                    )?;
                    merge_classes(
                        &mut comp.classes,
                        elem_list.element_list.classes.clone(),
                        true,
                    );
                    comp.extends.extend(elem_list.element_list.extends.clone());
                    comp.imports.extend(elem_list.element_list.imports.clone());
                }
                modelica_grammar_trait::CompositionListGroup::EquationSection(eq_sec) => {
                    process_equation_section(&mut comp, &eq_sec.equation_section);
                }
                modelica_grammar_trait::CompositionListGroup::AlgorithmSection(alg_sec) => {
                    process_algorithm_section(&mut comp, &alg_sec.algorithm_section);
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
            validate_annotation_modifiers(
                &class_mod_opt.argument_list,
                &annotation_opt.annotation_clause.annotation.annotation,
            )?;
            comp.annotation = class_mod_opt.argument_list.args.clone();
        }

        // Extract external function declaration (MLS §12.9)
        if let Some(external_opt) = &ast.composition_opt {
            comp.external = Some(extract_external_function(external_opt));
        }

        Ok(comp)
    }
}

/// Extract external function information from the composition.
fn extract_external_function(
    external_opt: &modelica_grammar_trait::CompositionOpt,
) -> rumoca_ir_ast::ExternalFunction {
    let mut external = rumoca_ir_ast::ExternalFunction::default();

    // Extract language specification (e.g., "C")
    if let Some(lang_spec) = &external_opt.composition_opt1 {
        // The language_specification is a string token
        external.language = Some(
            lang_spec
                .language_specification
                .string
                .text
                .trim_matches('"')
                .to_string(),
        );
    }

    // Extract external function call (name and arguments)
    if let Some(func_call) = &external_opt.composition_opt2 {
        let ext_call = &func_call.external_function_call;

        // Get the external function name
        external.function_name = Some(ext_call.ident.clone());

        // Get the output assignment (if any): result = external_func(...)
        if let Some(output_opt) = &ext_call.external_function_call_opt {
            external.output = Some(output_opt.component_reference.clone());
        }

        // Get the arguments - first expression + additional expressions from list
        if let Some(args_opt) = &ext_call.external_function_call_opt0 {
            let expr_list = &args_opt.expression_list;
            let mut args = vec![expr_list.expression.clone()];
            for item in &expr_list.expression_list_list {
                args.push(item.expression.clone());
            }
            external.args = args;
        }
    }

    external
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct ElementList {
    pub components: IndexMap<String, rumoca_ir_ast::Component>,
    pub classes: IndexMap<String, rumoca_ir_ast::ClassDef>,
    pub imports: Vec<rumoca_ir_ast::Import>,
    pub extends: Vec<rumoca_ir_ast::Extend>,
}
