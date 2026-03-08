//! Element list conversion for the parse phase.
//!
//! This module contains the TryFrom implementation for ElementList,
//! which handles conversion from parser AST to rumoca_ir::ast.

use super::definitions::ElementList;
use super::helpers::{loc_info, span_location};
use crate::errors::{semantic_error_from_component_reference, semantic_error_from_token};
use crate::generated::modelica_grammar_trait;
use indexmap::IndexMap;

//-----------------------------------------------------------------------------
// Helper functions for extracting type prefix attributes

/// Extract connection type (flow/stream) from type prefix.
fn extract_connection(
    type_prefix: &modelica_grammar_trait::TypePrefix,
) -> rumoca_ir_ast::Connection {
    let Some(opt) = &type_prefix.type_prefix_opt else {
        return rumoca_ir_ast::Connection::Empty;
    };
    match &opt.type_prefix_opt_group {
        modelica_grammar_trait::TypePrefixOptGroup::Flow(flow) => {
            rumoca_ir_ast::Connection::Flow(flow.flow.flow.clone().into())
        }
        modelica_grammar_trait::TypePrefixOptGroup::Stream(stream) => {
            rumoca_ir_ast::Connection::Stream(stream.stream.stream.clone().into())
        }
    }
}

/// Extract variability from type prefix.
fn extract_variability(
    type_prefix: &modelica_grammar_trait::TypePrefix,
) -> rumoca_ir_core::Variability {
    let Some(opt) = &type_prefix.type_prefix_opt0 else {
        return rumoca_ir_core::Variability::Empty;
    };
    match &opt.type_prefix_opt0_group {
        modelica_grammar_trait::TypePrefixOpt0Group::Constant(c) => {
            rumoca_ir_core::Variability::Constant(c.constant.constant.clone().into())
        }
        modelica_grammar_trait::TypePrefixOpt0Group::Discrete(c) => {
            rumoca_ir_core::Variability::Discrete(c.discrete.discrete.clone().into())
        }
        modelica_grammar_trait::TypePrefixOpt0Group::Parameter(c) => {
            rumoca_ir_core::Variability::Parameter(c.parameter.parameter.clone().into())
        }
    }
}

/// Extract causality from type prefix.
fn extract_causality(
    type_prefix: &modelica_grammar_trait::TypePrefix,
) -> rumoca_ir_core::Causality {
    let Some(opt) = &type_prefix.type_prefix_opt1 else {
        return rumoca_ir_core::Causality::Empty;
    };
    match &opt.type_prefix_opt1_group {
        modelica_grammar_trait::TypePrefixOpt1Group::Input(c) => {
            rumoca_ir_core::Causality::Input(c.input.input.clone().into())
        }
        modelica_grammar_trait::TypePrefixOpt1Group::Output(c) => {
            rumoca_ir_core::Causality::Output(c.output.output.clone().into())
        }
    }
}

/// Try to extract a dimension from a subscript.
/// Returns Some(dim) if the subscript is an integer literal or Boolean type.
fn try_extract_dimension(subscript: &rumoca_ir_ast::Subscript) -> Option<usize> {
    match subscript {
        rumoca_ir_ast::Subscript::Expression(rumoca_ir_ast::Expression::Terminal {
            token,
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        }) => token.text.parse::<usize>().ok(),
        rumoca_ir_ast::Subscript::Expression(rumoca_ir_ast::Expression::ComponentReference(
            comp_ref,
        )) => {
            // Check for Boolean type as dimension (MLS §10.5)
            if comp_ref.parts.len() == 1
                && &*comp_ref.parts[0].ident.text == "Boolean"
                && comp_ref.parts[0].subs.is_none()
            {
                Some(2)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Extract type-level array shape from component clause.
fn extract_type_level_shape(
    clause_opt: &Option<modelica_grammar_trait::ComponentClauseOpt>,
) -> (Vec<usize>, Vec<rumoca_ir_ast::Subscript>) {
    let Some(opt) = clause_opt else {
        return (Vec::new(), Vec::new());
    };
    let mut shape = Vec::new();
    let mut shape_expr = Vec::new();
    for subscript in &opt.array_subscripts.subscripts {
        shape_expr.push(subscript.clone());
        if let Some(dim) = try_extract_dimension(subscript) {
            shape.push(dim);
        }
    }
    (shape, shape_expr)
}

/// Get default start value for a type name.
fn default_start_value(type_name: &str) -> rumoca_ir_ast::Expression {
    use std::sync::Arc;
    match type_name {
        "Real" => rumoca_ir_ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
            token: rumoca_ir_core::Token {
                text: Arc::from("0.0"),
                ..Default::default()
            },
        },
        "Integer" => rumoca_ir_ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_ir_core::Token {
                text: Arc::from("0"),
                ..Default::default()
            },
        },
        "Boolean" => rumoca_ir_ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::Bool,
            token: rumoca_ir_core::Token {
                text: Arc::from("false"),
                ..Default::default()
            },
        },
        _ => rumoca_ir_ast::Expression::Empty {},
    }
}

/// Extract annotation arguments from a component description.
fn extract_annotation(
    desc: &modelica_grammar_trait::Description,
) -> Vec<rumoca_ir_ast::Expression> {
    let Some(desc_opt) = &desc.description_opt else {
        return Vec::new();
    };
    let Some(class_mod_opt) = &desc_opt
        .annotation_clause
        .class_modification
        .class_modification_opt
    else {
        return Vec::new();
    };
    class_mod_opt.argument_list.args.clone()
}

/// Check if an outer component has illegal bindings or modifications.
fn check_outer_component_restrictions(
    value: &rumoca_ir_ast::Component,
    ident: &rumoca_ir_core::Token,
) -> anyhow::Result<()> {
    // Check for binding equation (= expr)
    let has_real_binding = if matches!(value.start, rumoca_ir_ast::Expression::Empty) {
        false
    } else if let Some(loc) = value.start.get_location() {
        !(loc.start_line == 0 && loc.start_column == 0 && loc.file_name.is_empty())
    } else {
        false
    };

    if has_real_binding {
        return Err(semantic_error_from_token(
            format!(
                "Outer component '{}' shall not have a binding equation at line {}",
                ident.text, ident.location.start_line
            ),
            ident,
        ));
    }

    // Check for modifications (start=, fixed=, etc.)
    if !value.modifications.is_empty() || value.start_is_modification {
        return Err(semantic_error_from_token(
            format!(
                "Outer component '{}' shall not have modifications at line {}",
                ident.text, ident.location.start_line
            ),
            ident,
        ));
    }

    Ok(())
}

/// Check for duplicate component declaration.
fn check_duplicate_component(
    def: &ElementList,
    comp_name: &str,
    ident: &rumoca_ir_core::Token,
) -> anyhow::Result<()> {
    if let Some(existing) = def.components.get(comp_name) {
        return Err(semantic_error_from_token(
            format!(
                "Duplicate declaration of '{}' at line {} (first declared at line {})",
                comp_name, ident.location.start_line, existing.name_token.location.start_line
            ),
            ident,
        ));
    }
    if let Some(existing_class) = def.classes.get(comp_name) {
        return Err(semantic_error_from_token(
            format!(
                "Component '{}' at line {} conflicts with class of the same name (declared at line {})",
                comp_name, ident.location.start_line, existing_class.location.start_line
            ),
            ident,
        ));
    }
    Ok(())
}

/// Process an import clause and return the parsed Import.
fn process_import_clause(
    import_clause: &modelica_grammar_trait::ImportClause,
) -> rumoca_ir_ast::Import {
    let location = import_clause_location(import_clause);
    match &import_clause.import_clause_group {
        modelica_grammar_trait::ImportClauseGroup::IdentEquImportClauseOptName(renamed) => {
            let global_scope = renamed.import_clause_opt.is_some();
            rumoca_ir_ast::Import::Renamed {
                alias: renamed.ident.clone(),
                path: renamed.name.clone(),
                location,
                global_scope,
            }
        }
        modelica_grammar_trait::ImportClauseGroup::ImportClauseOpt0NameImportClauseOpt1(
            name_opt,
        ) => {
            let path = name_opt.name.clone();
            let global_scope = name_opt.import_clause_opt0.is_some();
            process_import_suffix(&name_opt.import_clause_opt1, path, location, global_scope)
        }
    }
}

fn import_clause_location(
    import_clause: &modelica_grammar_trait::ImportClause,
) -> rumoca_ir_core::Location {
    let mut location = import_clause.import.import.location.clone();
    match &import_clause.import_clause_group {
        modelica_grammar_trait::ImportClauseGroup::IdentEquImportClauseOptName(renamed) => {
            extend_location_end_with_name(&mut location, &renamed.name);
            extend_location_end_with_token(&mut location, &renamed.ident);
        }
        modelica_grammar_trait::ImportClauseGroup::ImportClauseOpt0NameImportClauseOpt1(
            name_opt,
        ) => {
            extend_location_end_with_name(&mut location, &name_opt.name);
            if let Some(suffix) = &name_opt.import_clause_opt1 {
                match &suffix.import_clause_opt1_group {
                    modelica_grammar_trait::ImportClauseOpt1Group::DotStar(dot_star) => {
                        extend_location_end_with_token(&mut location, &dot_star.dot_star);
                    }
                    modelica_grammar_trait::ImportClauseOpt1Group::DotImportClauseOpt1GroupGroup(
                        dot_group,
                    ) => match &dot_group.import_clause_opt1_group_group {
                        modelica_grammar_trait::ImportClauseOpt1GroupGroup::Star(star) => {
                            extend_location_end_with_token(&mut location, &star.star);
                        }
                        modelica_grammar_trait::ImportClauseOpt1GroupGroup::LBraceImportListRBrace(
                            list,
                        ) => {
                            let last_name = list
                                .import_list
                                .import_list_list
                                .last()
                                .map(|item| &item.ident)
                                .unwrap_or(&list.import_list.ident);
                            extend_location_end_with_token(&mut location, last_name);
                        }
                    },
                }
            }
        }
    }
    location
}

fn extend_location_end_with_name(
    location: &mut rumoca_ir_core::Location,
    name: &rumoca_ir_ast::Name,
) {
    if let Some(last) = name.name.last() {
        extend_location_end_with_token(location, last);
    }
}

fn extend_location_end_with_token(
    location: &mut rumoca_ir_core::Location,
    token: &rumoca_ir_core::Token,
) {
    let token_loc = &token.location;
    if token_loc.end > location.end
        || (token_loc.end == location.end
            && (token_loc.end_line, token_loc.end_column)
                > (location.end_line, location.end_column))
    {
        location.end_line = token_loc.end_line;
        location.end_column = token_loc.end_column;
        location.end = token_loc.end;
    }
}

/// Process the optional import suffix (wildcard or selective).
fn process_import_suffix(
    opt: &Option<modelica_grammar_trait::ImportClauseOpt1>,
    path: rumoca_ir_ast::Name,
    location: rumoca_ir_core::Location,
    global_scope: bool,
) -> rumoca_ir_ast::Import {
    let Some(suffix) = opt else {
        return rumoca_ir_ast::Import::Qualified {
            path,
            location,
            global_scope,
        };
    };
    match &suffix.import_clause_opt1_group {
        modelica_grammar_trait::ImportClauseOpt1Group::DotStar(_) => {
            rumoca_ir_ast::Import::Unqualified {
                path,
                location,
                global_scope,
            }
        }
        modelica_grammar_trait::ImportClauseOpt1Group::DotImportClauseOpt1GroupGroup(dot_group) => {
            match &dot_group.import_clause_opt1_group_group {
                modelica_grammar_trait::ImportClauseOpt1GroupGroup::Star(_) => {
                    rumoca_ir_ast::Import::Unqualified {
                        path,
                        location,
                        global_scope,
                    }
                }
                modelica_grammar_trait::ImportClauseOpt1GroupGroup::LBraceImportListRBrace(
                    list,
                ) => {
                    let mut names = vec![list.import_list.ident.clone()];
                    for item in &list.import_list.import_list_list {
                        names.push(item.ident.clone());
                    }
                    rumoca_ir_ast::Import::Selective {
                        path,
                        names,
                        location,
                        global_scope,
                    }
                }
            }
        }
    }
}

/// Helper to extract break name from inheritance modification.
fn extract_break_name(im: &modelica_grammar_trait::InheritanceModification) -> Option<String> {
    match &im.inheritance_modification_group {
        modelica_grammar_trait::InheritanceModificationGroup::Ident(ident_group) => {
            Some(ident_group.ident.text.to_string())
        }
        modelica_grammar_trait::InheritanceModificationGroup::ConnectEquation(_) => None,
    }
}

/// Extract modifications from an extends clause.
fn extract_extends_mods(
    ext_opt: &Option<modelica_grammar_trait::ExtendsClauseOpt>,
) -> (Vec<rumoca_ir_ast::ExtendModification>, Vec<String>) {
    let Some(opt) = ext_opt else {
        return (Vec::new(), Vec::new());
    };
    let Some(mod_opt) = &opt
        .class_or_inheritance_modification
        .class_or_inheritance_modification_opt
    else {
        return (Vec::new(), Vec::new());
    };

    let list = &mod_opt.argument_or_inheritance_modification_list;
    let mut mods = Vec::new();
    let mut break_names = Vec::new();

    // Process first item
    match &list.argument_or_inheritance_modification_list_group {
        modelica_grammar_trait::ArgumentOrInheritanceModificationListGroup::Argument(arg) => {
            mods.push(rumoca_ir_ast::ExtendModification {
                expr: arg.argument.expression.clone(),
                each: arg.argument.each,
                redeclare: arg.argument.redeclare,
            });
        }
        modelica_grammar_trait::ArgumentOrInheritanceModificationListGroup::InheritanceModification(
            im,
        ) => {
            if let Some(name) = extract_break_name(&im.inheritance_modification) {
                break_names.push(name);
            }
        }
    }

    // Process remaining items
    for item in &list.argument_or_inheritance_modification_list_list {
        match &item.argument_or_inheritance_modification_list_list_group {
            modelica_grammar_trait::ArgumentOrInheritanceModificationListListGroup::Argument(
                arg,
            ) => {
                mods.push(rumoca_ir_ast::ExtendModification {
                    expr: arg.argument.expression.clone(),
                    each: arg.argument.each,
                    redeclare: arg.argument.redeclare,
                });
            }
            modelica_grammar_trait::ArgumentOrInheritanceModificationListListGroup::InheritanceModification(im) => {
                if let Some(name) = extract_break_name(&im.inheritance_modification) {
                    break_names.push(name);
                }
            }
        }
    }

    (mods, break_names)
}

/// Process a nested class definition element.
fn process_class_definition(
    def: &mut ElementList,
    class: &modelica_grammar_trait::ElementDefinitionGroupClassDefinition,
    is_final: bool,
) -> anyhow::Result<()> {
    let mut nested_class = class.class_definition.clone();
    nested_class.is_final = is_final;
    nested_class.is_replaceable = false;

    let name = nested_class.name.text.to_string();
    if let Some(existing_comp) = def.components.get(&name) {
        return Err(semantic_error_from_token(
            format!(
                "Class '{}' at line {} conflicts with component of the same name (declared at line {})",
                name,
                nested_class.location.start_line,
                existing_comp.name_token.location.start_line
            ),
            &nested_class.name,
        ));
    }
    def.classes.insert(name, nested_class);
    Ok(())
}

/// Context for component processing.
struct ComponentContext {
    variability: rumoca_ir_core::Variability,
    causality: rumoca_ir_core::Causality,
    connection: rumoca_ir_ast::Connection,
    type_name: rumoca_ir_ast::Name,
    type_level_shape: Vec<usize>,
    type_level_shape_expr: Vec<rumoca_ir_ast::Subscript>,
    is_final: bool,
    is_inner: bool,
    is_outer: bool,
    is_replaceable: bool,
    constrainedby: Option<rumoca_ir_ast::Name>,
}

/// Process a single component declaration.
fn process_single_component(
    def: &mut ElementList,
    c: &modelica_grammar_trait::ComponentDeclaration,
    ctx: &ComponentContext,
    type_spec: &modelica_grammar_trait::TypeSpecifier,
) -> anyhow::Result<()> {
    let annotation = extract_annotation(&c.description);
    let comp_location = type_spec
        .name
        .name
        .first()
        .map(|start_tok| span_location(start_tok, &c.declaration.ident))
        .unwrap_or_else(|| c.declaration.ident.location.clone());
    let condition = c
        .component_declaration_opt
        .as_ref()
        .map(|opt| opt.condition_attribute.expression.clone());

    let mut value = rumoca_ir_ast::Component {
        def_id: None,
        type_id: None,
        type_def_id: None,
        name: c.declaration.ident.text.to_string(),
        name_token: c.declaration.ident.clone(),
        type_name: ctx.type_name.clone(),
        variability: ctx.variability.clone(),
        causality: ctx.causality.clone(),
        connection: ctx.connection.clone(),
        description: c.description.description_string.tokens.clone(),
        start: default_start_value(&ctx.type_name.to_string()),
        start_is_modification: false,
        start_has_each: false,
        has_explicit_binding: false,
        binding: None,
        shape: ctx.type_level_shape.clone(),
        shape_expr: ctx.type_level_shape_expr.clone(),
        shape_is_modification: false,
        annotation,
        modifications: indexmap::IndexMap::new(),
        location: comp_location,
        condition,
        inner: ctx.is_inner,
        outer: ctx.is_outer,
        final_attributes: std::collections::HashSet::new(),
        each_modifications: std::collections::HashSet::new(),
        is_protected: false,
        is_final: ctx.is_final,
        is_replaceable: ctx.is_replaceable,
        constrainedby: ctx.constrainedby.clone(),
        is_structural: false,
    };

    // Append declaration-level subscripts
    if let Some(decl_opt) = &c.declaration.declaration_opt {
        for subscript in &decl_opt.array_subscripts.subscripts {
            value.shape_expr.push(subscript.clone());
            if let Some(dim) = try_extract_dimension(subscript) {
                value.shape.push(dim);
            }
        }
    }

    // Handle component modification
    if let Some(modif) = &c.declaration.declaration_opt0 {
        process_component_modification(&mut value, modif)?;
    }

    // Check outer component restrictions (MLS §5.4)
    if ctx.is_outer && !ctx.is_inner {
        check_outer_component_restrictions(&value, &c.declaration.ident)?;
    }

    let comp_name = c.declaration.ident.text.to_string();
    check_duplicate_component(def, &comp_name, &c.declaration.ident)?;
    def.components.insert(comp_name, value);
    Ok(())
}

/// Process a component clause element.
fn process_component_clause(
    def: &mut ElementList,
    clause: &modelica_grammar_trait::ElementDefinitionGroupComponentClause,
    is_final: bool,
    is_inner: bool,
    is_outer: bool,
) -> anyhow::Result<()> {
    let type_prefix = &clause.component_clause.type_prefix;
    let ctx = ComponentContext {
        variability: extract_variability(type_prefix),
        causality: extract_causality(type_prefix),
        connection: extract_connection(type_prefix),
        type_name: clause.component_clause.type_specifier.name.clone(),
        type_level_shape: extract_type_level_shape(&clause.component_clause.component_clause_opt).0,
        type_level_shape_expr: extract_type_level_shape(
            &clause.component_clause.component_clause_opt,
        )
        .1,
        is_final,
        is_inner,
        is_outer,
        is_replaceable: false,
        constrainedby: None,
    };

    for c in &clause.component_clause.component_list.components {
        process_single_component(def, c, &ctx, &clause.component_clause.type_specifier)?;
    }
    Ok(())
}

/// Process a replaceable element.
fn process_replaceable_element(
    def: &mut ElementList,
    repl: &modelica_grammar_trait::ElementDefinitionGroupReplaceableElementDefinitionGroupGroupElementDefinitionOpt3,
    is_final: bool,
    is_inner: bool,
    is_outer: bool,
) -> anyhow::Result<()> {
    let constrainedby = repl
        .element_definition_opt3
        .as_ref()
        .map(|opt3| opt3.constraining_clause.type_specifier.name.clone());
    let constrainedby_mods = repl
        .element_definition_opt3
        .as_ref()
        .and_then(|opt3| opt3.constraining_clause.constraining_clause_opt.as_ref());

    match &repl.element_definition_group_group {
        modelica_grammar_trait::ElementDefinitionGroupGroup::ClassDefinition(class) => {
            let mut nested_class = class.class_definition.clone();
            nested_class.is_final = is_final;
            nested_class.is_replaceable = true;
            nested_class.constrainedby = constrainedby;
            let name = nested_class.name.text.to_string();
            def.classes.insert(name, nested_class);
        }
        modelica_grammar_trait::ElementDefinitionGroupGroup::ComponentClause(clause) => {
            let (type_level_shape, type_level_shape_expr) =
                extract_type_level_shape(&clause.component_clause.component_clause_opt);
            let type_prefix = &clause.component_clause.type_prefix;
            let ctx = ComponentContext {
                variability: extract_variability(type_prefix),
                causality: extract_causality(type_prefix),
                connection: extract_connection(type_prefix),
                type_name: clause.component_clause.type_specifier.name.clone(),
                type_level_shape,
                type_level_shape_expr,
                is_final,
                is_inner,
                is_outer,
                is_replaceable: true,
                constrainedby: constrainedby.clone(),
            };

            for c in &clause.component_clause.component_list.components {
                process_single_component(def, c, &ctx, &clause.component_clause.type_specifier)?;

                let comp_name = c.declaration.ident.text.to_string();
                if let Some(value) = def.components.get_mut(&comp_name) {
                    // MLS §7.3.2: Preserve constraining-clause class modifications on
                    // replaceable components (e.g., constrainedby C(n=n)) so they apply
                    // after redeclaration as defaults for the replacement type.
                    merge_constraining_clause_modifications(value, constrainedby_mods)?;
                }
            }
        }
    }
    Ok(())
}

/// Merge constraining-clause class modifications into a replaceable component.
///
/// For declarations like:
/// `replaceable C c constrainedby C(n=n)`
/// keep `n=n` on the component so it survives redeclare and can configure the
/// replacement type. Existing declaration-level modifications take precedence.
fn merge_constraining_clause_modifications(
    value: &mut rumoca_ir_ast::Component,
    constrainedby_mods: Option<&modelica_grammar_trait::ConstrainingClauseOpt>,
) -> anyhow::Result<()> {
    let Some(constrainedby_mods) = constrainedby_mods else {
        return Ok(());
    };
    let Some(class_mod_opt) = &constrainedby_mods.class_modification.class_modification_opt else {
        return Ok(());
    };

    for (idx, arg) in class_mod_opt.argument_list.args.iter().enumerate() {
        let has_each = class_mod_opt
            .argument_list
            .each_flags
            .get(idx)
            .copied()
            .unwrap_or(false);
        let has_final = class_mod_opt
            .argument_list
            .final_flags
            .get(idx)
            .copied()
            .unwrap_or(false);

        let Some(target_name) = constraining_arg_target_name(arg) else {
            continue;
        };
        // Keep built-in attribute handling in declaration modifications only.
        // For constrainedby mods we stash component-target defaults and apply them
        // when a redeclare actually replaces the component type.
        if is_builtin_attribute_name(&target_name) {
            continue;
        }
        let Some(stored_value) = normalized_constraining_arg_value(arg) else {
            continue;
        };

        let key = format!("__constrainedby__.{target_name}");
        if value.modifications.contains_key(&key) {
            continue;
        }
        value.modifications.insert(key.clone(), stored_value);
        if has_each {
            value.each_modifications.insert(key.clone());
        }
        if has_final {
            value.final_attributes.insert(key);
        }
    }

    Ok(())
}

fn constraining_arg_target_name(arg: &rumoca_ir_ast::Expression) -> Option<String> {
    match arg {
        rumoca_ir_ast::Expression::Modification { target, .. }
        | rumoca_ir_ast::Expression::ClassModification { target, .. } => Some(target.to_string()),
        rumoca_ir_ast::Expression::Binary { op, lhs, .. }
            if matches!(op, rumoca_ir_core::OpBinary::Assign(_))
                && matches!(
                    lhs.as_ref(),
                    rumoca_ir_ast::Expression::ClassModification { .. }
                ) =>
        {
            if let rumoca_ir_ast::Expression::ClassModification { target, .. } = lhs.as_ref() {
                Some(target.to_string())
            } else {
                None
            }
        }
        _ => None,
    }
}

fn is_builtin_attribute_name(name: &str) -> bool {
    matches!(
        name,
        "start" | "fixed" | "min" | "max" | "nominal" | "unit" | "stateSelect"
    )
}

fn normalized_constraining_arg_value(
    arg: &rumoca_ir_ast::Expression,
) -> Option<rumoca_ir_ast::Expression> {
    match arg {
        // Same storage shape as regular component modifications map:
        //   param = expr  -> store expr under key "param"
        rumoca_ir_ast::Expression::Modification { value, .. } => Some(value.as_ref().clone()),
        // Nested component mods/bindings are stored as full expression.
        rumoca_ir_ast::Expression::ClassModification { .. } => Some(arg.clone()),
        rumoca_ir_ast::Expression::Binary { op, lhs, .. }
            if matches!(op, rumoca_ir_core::OpBinary::Assign(_))
                && matches!(
                    lhs.as_ref(),
                    rumoca_ir_ast::Expression::ClassModification { .. }
                ) =>
        {
            Some(arg.clone())
        }
        _ => None,
    }
}

/// Process an extends clause element.
fn process_extends_clause(
    def: &mut ElementList,
    clause: &modelica_grammar_trait::ElementExtendsClause,
) {
    let extend_location = clause
        .extends_clause
        .type_specifier
        .name
        .name
        .last()
        .map(|end_tok| span_location(&clause.extends_clause.extends.extends, end_tok))
        .unwrap_or_else(|| clause.extends_clause.extends.extends.location.clone());

    let (modifications, break_names) =
        extract_extends_mods(&clause.extends_clause.extends_clause_opt);

    def.extends.push(rumoca_ir_ast::Extend {
        base_name: clause.extends_clause.type_specifier.name.clone(),
        base_def_id: None,
        location: extend_location,
        modifications,
        break_names,
        is_protected: false,
        annotation: vec![],
    });
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
            process_element(&mut def, &elem_list.element)?;
        }
        Ok(def)
    }
}

/// Process a single element from the element list.
fn process_element(
    def: &mut ElementList,
    element: &modelica_grammar_trait::Element,
) -> anyhow::Result<()> {
    match element {
        modelica_grammar_trait::Element::ElementDefinition(edef) => {
            process_element_definition(def, edef)?;
        }
        modelica_grammar_trait::Element::ImportClause(import_elem) => {
            def.imports
                .push(process_import_clause(&import_elem.import_clause));
        }
        modelica_grammar_trait::Element::ExtendsClause(clause) => {
            process_extends_clause(def, clause);
        }
    }
    Ok(())
}

/// Process an element definition (class, component, or replaceable).
fn process_element_definition(
    def: &mut ElementList,
    edef: &modelica_grammar_trait::ElementElementDefinition,
) -> anyhow::Result<()> {
    let is_final = edef.element_definition.element_definition_opt0.is_some();
    let is_inner = edef.element_definition.element_definition_opt1.is_some();
    let is_outer = edef.element_definition.element_definition_opt2.is_some();

    match &edef.element_definition.element_definition_group {
        modelica_grammar_trait::ElementDefinitionGroup::ClassDefinition(class) => {
            process_class_definition(def, class, is_final)?;
        }
        modelica_grammar_trait::ElementDefinitionGroup::ComponentClause(clause) => {
            process_component_clause(def, clause, is_final, is_inner, is_outer)?;
        }
        modelica_grammar_trait::ElementDefinitionGroup::ReplaceableElementDefinitionGroupGroupElementDefinitionOpt3(repl) => {
            process_replaceable_element(def, repl, is_final, is_inner, is_outer)?;
        }
    }
    Ok(())
}

//-----------------------------------------------------------------------------
// Helper functions for component modification processing

/// Valid built-in type attributes that cannot have sub-modifications.
const ALL_BUILTIN_ATTRS: &[&str] = &[
    "start",
    "fixed",
    "min",
    "max",
    "nominal",
    "unit",
    "displayUnit",
    "quantity",
    "stateSelect",
    "unbounded",
];

/// Valid attributes for each builtin type.
const REAL_ATTRS: &[&str] = &[
    "start",
    "fixed",
    "min",
    "max",
    "nominal",
    "unit",
    "displayUnit",
    "quantity",
    "stateSelect",
    "unbounded",
    "each",
    "final",
];
const INTEGER_ATTRS: &[&str] = &["start", "fixed", "min", "max", "quantity", "each", "final"];
const BOOLEAN_ATTRS: &[&str] = &["start", "fixed", "quantity", "each", "final"];
const STRING_ATTRS: &[&str] = &["start", "fixed", "quantity", "each", "final"];

/// Check if a type is a builtin type.
fn is_builtin_type(type_name: &str) -> bool {
    matches!(type_name, "Real" | "Integer" | "Boolean" | "String")
}

fn duplicate_modification_error(
    param_name: &str,
    target: &rumoca_ir_ast::ComponentReference,
) -> anyhow::Error {
    let message = format!("duplicate modification of element '{param_name}'");
    semantic_error_from_component_reference(message, target)
}

/// Get valid attributes for a builtin type.
fn get_valid_attrs(type_name: &str) -> &'static [&'static str] {
    match type_name {
        "Real" => REAL_ATTRS,
        "Integer" => INTEGER_ATTRS,
        "Boolean" => BOOLEAN_ATTRS,
        "String" => STRING_ATTRS,
        _ => &[],
    }
}

/// Check for invalid sub-modifications on builtin attributes.
fn check_builtin_submod(
    comp: &rumoca_ir_ast::ComponentReference,
    type_name: &str,
) -> anyhow::Result<()> {
    if !is_builtin_type(type_name) {
        return Ok(());
    }
    let func_name = comp.to_string();
    if ALL_BUILTIN_ATTRS.contains(&func_name.as_str()) {
        let loc = comp
            .parts
            .first()
            .map_or(String::new(), |f| loc_info(&f.ident));
        let message = format!("Modified element {func_name}.y not found in class {type_name}{loc}");
        return Err(semantic_error_from_component_reference(message, comp));
    }
    Ok(())
}

/// Extract shape from an expression.
fn extract_shape(expr: &rumoca_ir_ast::Expression) -> Vec<usize> {
    match expr {
        rumoca_ir_ast::Expression::Terminal {
            token,
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
        } => token.text.parse().map(|d| vec![d]).unwrap_or_default(),
        rumoca_ir_ast::Expression::Parenthesized { inner } => {
            if let rumoca_ir_ast::Expression::Terminal {
                token,
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            } = &**inner
            {
                return token.text.parse().map(|d| vec![d]).unwrap_or_default();
            }
            Vec::new()
        }
        rumoca_ir_ast::Expression::Array { elements, .. }
        | rumoca_ir_ast::Expression::Tuple { elements } => elements
            .iter()
            .filter_map(|e| {
                if let rumoca_ir_ast::Expression::Terminal {
                    token,
                    terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                } = e
                {
                    token.text.parse().ok()
                } else {
                    None
                }
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Validate that a modification is valid for a builtin type.
fn validate_builtin_mod(
    param_name: &str,
    type_name: &str,
    comp: &rumoca_ir_ast::ComponentReference,
) -> anyhow::Result<()> {
    if !is_builtin_type(type_name) {
        return Ok(());
    }
    let valid_attrs = get_valid_attrs(type_name);
    if !valid_attrs.contains(&param_name) {
        let loc = comp.parts.first().map_or(String::new(), |f| {
            format!(
                " at line {}, column {}",
                f.ident.location.start_line, f.ident.location.start_column
            )
        });
        let message = format!(
            "Invalid modification '{}' for type '{}'{}\nValid attributes are: {}",
            param_name,
            type_name,
            loc,
            valid_attrs.join(", ")
        );
        return Err(semantic_error_from_component_reference(message, comp));
    }
    Ok(())
}

/// Process a named argument (start=..., shape=..., etc.).
fn process_named_arg(
    value: &mut rumoca_ir_ast::Component,
    param_name: &str,
    rhs: &rumoca_ir_ast::Expression,
    has_each: bool,
    has_final: bool,
    comp: &rumoca_ir_ast::ComponentReference,
) -> anyhow::Result<()> {
    let type_name = value.type_name.to_string();
    match param_name {
        "start" => {
            value.start = rhs.clone();
            value.start_is_modification = true;
            value.start_has_each = has_each;
            if has_final {
                value.final_attributes.insert("start".to_string());
            }
        }
        "shape" => {
            value.shape = extract_shape(rhs);
        }
        _ => {
            validate_builtin_mod(param_name, &type_name, comp)?;
            if value.modifications.contains_key(param_name) {
                return Err(duplicate_modification_error(param_name, comp));
            }
            value
                .modifications
                .insert(param_name.to_string(), rhs.clone());
            if has_each {
                value.each_modifications.insert(param_name.to_string());
            }
            if has_final {
                value.final_attributes.insert(param_name.to_string());
            }
        }
    }
    Ok(())
}

/// Process a single argument from the class modification.
fn process_mod_arg(
    value: &mut rumoca_ir_ast::Component,
    arg: &rumoca_ir_ast::Expression,
    has_each: bool,
    has_final: bool,
) -> anyhow::Result<()> {
    let type_name = value.type_name.to_string();

    // Check for invalid sub-modifications on builtin attributes
    if let rumoca_ir_ast::Expression::FunctionCall { comp, .. } = arg {
        check_builtin_submod(comp, &type_name)?;
    }

    // Handle named argument (param = value)
    if let rumoca_ir_ast::Expression::Binary { op, lhs, rhs } = arg
        && matches!(op, rumoca_ir_core::OpBinary::Assign(_))
        && let rumoca_ir_ast::Expression::ComponentReference(comp) = &**lhs
    {
        let param_name = comp.to_string();
        process_named_arg(value, &param_name, rhs, has_each, has_final, comp)?;
    }

    // Handle Expression::Modification variant
    if let rumoca_ir_ast::Expression::Modification {
        target,
        value: mod_value,
    } = arg
    {
        let param_name = target.to_string();
        if param_name == "start" {
            value.start = (**mod_value).clone();
            value.start_is_modification = true;
            value.start_has_each = has_each;
            if has_final {
                value.final_attributes.insert("start".to_string());
            }
        } else {
            if value.modifications.contains_key(&param_name) {
                return Err(duplicate_modification_error(&param_name, target));
            }
            value
                .modifications
                .insert(param_name.clone(), (**mod_value).clone());
            if has_each {
                value.each_modifications.insert(param_name.clone());
            }
            if has_final {
                value.final_attributes.insert(param_name);
            }
        }
    }

    // Handle nested modifications (with optional binding)
    // MLS §7.2: field(start=X) or field(start=X)=expr
    // The parser produces ClassModification for the first form,
    // and Binary { Assign, ClassModification, rhs } for the second.
    if let rumoca_ir_ast::Expression::ClassModification { target, .. } = arg {
        let param_name = target.to_string();
        if value.modifications.contains_key(&param_name) {
            return Err(duplicate_modification_error(&param_name, target));
        }
        value.modifications.insert(param_name.clone(), arg.clone());
        if has_each {
            value.each_modifications.insert(param_name.clone());
        }
        if has_final {
            value.final_attributes.insert(param_name);
        }
    }

    // Handle nested modifications WITH binding: field(start=X)=expr
    // The parser produces Binary { Assign, ClassModification { target, mods }, rhs }
    if let rumoca_ir_ast::Expression::Binary { op, lhs, rhs: _ } = arg
        && matches!(op, rumoca_ir_core::OpBinary::Assign(_))
        && let rumoca_ir_ast::Expression::ClassModification { target, .. } = &**lhs
    {
        let param_name = target.to_string();
        if value.modifications.contains_key(&param_name) {
            return Err(duplicate_modification_error(&param_name, target));
        }
        // Store the whole Binary expression so both nested mods and binding
        // are available during instantiation
        value.modifications.insert(param_name.clone(), arg.clone());
        if has_each {
            value.each_modifications.insert(param_name.clone());
        }
        if has_final {
            value.final_attributes.insert(param_name);
        }
    }

    Ok(())
}

/// Process modification expression (binding).
fn process_mod_expr(
    value: &mut rumoca_ir_ast::Component,
    mod_expr: &modelica_grammar_trait::ModificationExpression,
) {
    if let modelica_grammar_trait::ModificationExpression::Expression(expr) = mod_expr {
        // Store the binding expression in the dedicated binding field
        value.binding = Some(expr.expression.clone());
        value.has_explicit_binding = true;
        // Also set start as fallback (used when there is no explicit start= modifier)
        // This will be overwritten if there's an explicit start= modifier
        if !value.start_is_modification {
            value.start = expr.expression.clone();
        }
    }
    // 'break' means remove any inherited binding - do nothing
}

/// Process component modification (start=, fixed=, binding expression, etc.)
fn process_component_modification(
    value: &mut rumoca_ir_ast::Component,
    modif: &modelica_grammar_trait::DeclarationOpt0,
) -> anyhow::Result<()> {
    match &modif.modification {
        modelica_grammar_trait::Modification::ClassModificationModificationOpt(class_mod) => {
            if let Some(opt) = &class_mod.class_modification.class_modification_opt {
                for (idx, arg) in opt.argument_list.args.iter().enumerate() {
                    let has_each = opt
                        .argument_list
                        .each_flags
                        .get(idx)
                        .copied()
                        .unwrap_or(false);
                    let has_final = opt
                        .argument_list
                        .final_flags
                        .get(idx)
                        .copied()
                        .unwrap_or(false);
                    process_mod_arg(value, arg, has_each, has_final)?;
                }
            }
            if let Some(mod_opt) = &class_mod.modification_opt {
                process_mod_expr(value, &mod_opt.modification_expression);
            }
        }
        modelica_grammar_trait::Modification::EquModificationExpression(eq_mod) => {
            process_mod_expr(value, &eq_mod.modification_expression);
        }
    }
    Ok(())
}
