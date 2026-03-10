//! Conversion for expressions.

use super::helpers::{collect_array_elements, convert_for_indices, loc_info};
use crate::errors::semantic_error_from_token;
use crate::generated::modelica_grammar_trait;
use std::sync::Arc;

//-----------------------------------------------------------------------------
// Helper functions to reduce nesting in operator conversions.

/// Create an ExpressionList from args with default flags.
fn make_expr_list(args: Vec<rumoca_ir_ast::Expression>) -> ExpressionList {
    let len = args.len();
    ExpressionList {
        args,
        each_flags: vec![false; len],
        final_flags: vec![false; len],
        redeclare_flags: vec![false; len],
        replaceable_flags: vec![false; len],
    }
}

/// Process function arguments that have a for-comprehension.
fn process_for_comprehension(
    base_expr: &rumoca_ir_ast::Expression,
    for_clause: &modelica_grammar_trait::FunctionArgumentsOptGroupForForIndices,
) -> ExpressionList {
    let indices = convert_for_indices(&for_clause.for_indices);
    let comprehension = rumoca_ir_ast::Expression::ArrayComprehension {
        expr: Arc::new(base_expr.clone()),
        indices,
        filter: None,
    };
    make_expr_list(vec![comprehension])
}

/// Extend args from function arguments non-first group.
fn extend_from_non_first(
    args: &mut Vec<rumoca_ir_ast::Expression>,
    group: &modelica_grammar_trait::FunctionArgumentsOptGroup,
) {
    if let modelica_grammar_trait::FunctionArgumentsOptGroup::CommaFunctionArgumentsNonFirst(expr) =
        group
    {
        args.append(&mut expr.function_arguments_non_first.args.clone());
    }
}

/// Extract for-indices from function arguments if present.
fn extract_for_indices(
    opt: &Option<modelica_grammar_trait::FunctionArgumentsOpt>,
) -> Option<&modelica_grammar_trait::FunctionArgumentsOptGroupForForIndices> {
    opt.as_ref()
        .and_then(|o| match &o.function_arguments_opt_group {
            modelica_grammar_trait::FunctionArgumentsOptGroup::ForForIndices(f) => Some(f),
            _ => None,
        })
}

/// Convert expression function arguments to ExpressionList.
fn convert_expression_function_args(
    def: &modelica_grammar_trait::FunctionArgumentsExpressionFunctionArgumentsOpt,
) -> anyhow::Result<ExpressionList> {
    // Check for for-comprehension first
    if let Some(for_clause) = extract_for_indices(&def.function_arguments_opt) {
        return Ok(process_for_comprehension(&def.expression, for_clause));
    }

    // Regular function arguments
    let mut args = vec![def.expression.clone()];
    if let Some(opt) = &def.function_arguments_opt {
        extend_from_non_first(&mut args, &opt.function_arguments_opt_group);
    }
    Ok(make_expr_list(args))
}

/// Convert a MulOperator to OpBinary.
fn mul_op_to_binary(op: &modelica_grammar_trait::MulOperator) -> rumoca_ir_core::OpBinary {
    match op {
        modelica_grammar_trait::MulOperator::Star(o) => {
            rumoca_ir_core::OpBinary::Mul(o.star.clone().into())
        }
        modelica_grammar_trait::MulOperator::Slash(o) => {
            rumoca_ir_core::OpBinary::Div(o.slash.clone().into())
        }
        modelica_grammar_trait::MulOperator::DotSlash(o) => {
            rumoca_ir_core::OpBinary::DivElem(o.dot_slash.clone().into())
        }
        modelica_grammar_trait::MulOperator::DotStar(o) => {
            rumoca_ir_core::OpBinary::MulElem(o.dot_star.clone().into())
        }
    }
}

/// Convert an AddOperator to OpBinary.
fn add_op_to_binary(op: &modelica_grammar_trait::AddOperator) -> rumoca_ir_core::OpBinary {
    match op {
        modelica_grammar_trait::AddOperator::Plus(t) => {
            rumoca_ir_core::OpBinary::Add(t.plus.clone().into())
        }
        modelica_grammar_trait::AddOperator::Minus(t) => {
            rumoca_ir_core::OpBinary::Sub(t.minus.clone().into())
        }
        modelica_grammar_trait::AddOperator::DotPlus(t) => {
            rumoca_ir_core::OpBinary::AddElem(t.dot_plus.clone().into())
        }
        modelica_grammar_trait::AddOperator::DotMinus(t) => {
            rumoca_ir_core::OpBinary::SubElem(t.dot_minus.clone().into())
        }
    }
}

/// Convert an AddOperator to OpUnary.
fn add_op_to_unary(op: &modelica_grammar_trait::AddOperator) -> rumoca_ir_core::OpUnary {
    match op {
        modelica_grammar_trait::AddOperator::Minus(t) => {
            rumoca_ir_core::OpUnary::Minus(t.minus.clone().into())
        }
        modelica_grammar_trait::AddOperator::Plus(t) => {
            rumoca_ir_core::OpUnary::Plus(t.plus.clone().into())
        }
        modelica_grammar_trait::AddOperator::DotMinus(t) => {
            rumoca_ir_core::OpUnary::DotMinus(t.dot_minus.clone().into())
        }
        modelica_grammar_trait::AddOperator::DotPlus(t) => {
            rumoca_ir_core::OpUnary::DotPlus(t.dot_plus.clone().into())
        }
    }
}

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct ArraySubscripts {
    pub subscripts: Vec<rumoca_ir_ast::Subscript>,
}

impl TryFrom<&modelica_grammar_trait::ArraySubscripts> for ArraySubscripts {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArraySubscripts,
    ) -> std::result::Result<Self, Self::Error> {
        let mut subscripts = vec![ast.subscript.clone()];
        for subscript in &ast.array_subscripts_list {
            subscripts.push(subscript.subscript.clone());
        }
        Ok(ArraySubscripts { subscripts })
    }
}

impl TryFrom<&modelica_grammar_trait::Subscript> for rumoca_ir_ast::Subscript {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Subscript) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Subscript::Colon(tok) => Ok(rumoca_ir_ast::Subscript::Range {
                token: tok.colon.clone().into(),
            }),
            modelica_grammar_trait::Subscript::Expression(expr) => Ok(
                rumoca_ir_ast::Subscript::Expression(expr.expression.clone()),
            ),
        }
    }
}

//-----------------------------------------------------------------------------
/// Represents a modification argument with optional `each`, `final`, `redeclare`,
/// and `replaceable` prefixes.
#[derive(Debug, Default, Clone)]
pub struct ModificationArg {
    pub expression: rumoca_ir_ast::Expression,
    /// True if this argument has `each` prefix (for array modifications)
    pub each: bool,
    /// True if this argument has `final` prefix
    pub r#final: bool,
    /// True if this argument is a redeclaration (MLS §7.3)
    pub redeclare: bool,
    /// True if this argument is an element replaceable declaration (MLS §7.3)
    pub replaceable: bool,
}

#[derive(Debug, Default, Clone)]

pub struct ExpressionList {
    pub args: Vec<rumoca_ir_ast::Expression>,
    /// Parallel to args - true if the corresponding arg has `each` modifier prefix
    pub each_flags: Vec<bool>,
    /// Parallel to args - true if the corresponding arg has `final` modifier prefix
    pub final_flags: Vec<bool>,
    /// Parallel to args - true if the corresponding arg is a redeclaration
    pub redeclare_flags: Vec<bool>,
    /// Parallel to args - true if the corresponding arg is an element replaceable declaration
    pub replaceable_flags: Vec<bool>,
}

/// Convert a NamedArgument to a NamedArgument expression
/// This preserves the parameter name token with source location for better error messages.
fn named_argument_to_expr(
    named_arg: &modelica_grammar_trait::NamedArgument,
) -> rumoca_ir_ast::Expression {
    // Create a NamedArgument expression that preserves the parameter name token
    rumoca_ir_ast::Expression::NamedArgument {
        name: named_arg.ident.clone(),
        value: Arc::new(named_arg.function_argument.clone()),
    }
}

/// Collect all named arguments from a NamedArguments structure
fn collect_named_arguments(
    named_args: &modelica_grammar_trait::NamedArguments,
) -> Vec<rumoca_ir_ast::Expression> {
    let mut args = vec![named_argument_to_expr(&named_args.named_argument)];

    // Recursively collect additional named arguments
    let mut current_opt = &named_args.named_arguments_opt;
    while let Some(opt) = current_opt {
        args.push(named_argument_to_expr(&opt.named_arguments.named_argument));
        current_opt = &opt.named_arguments.named_arguments_opt;
    }

    args
}

impl TryFrom<&modelica_grammar_trait::FunctionArgument> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionArgument,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::FunctionArgument::Expression(expr) => {
                Ok(expr.expression.clone())
            }
            modelica_grammar_trait::FunctionArgument::FunctionPartialApplication(fpa) => {
                // Convert 'function Foo.Bar(arg=val)' to a function call expression
                let partial_app = &fpa.function_partial_application;

                // Convert Name to ComponentReference
                let parts: Vec<rumoca_ir_ast::ComponentRefPart> = partial_app
                    .type_specifier
                    .name
                    .name
                    .iter()
                    .map(|token| rumoca_ir_ast::ComponentRefPart {
                        ident: token.clone(),
                        subs: None,
                    })
                    .collect();

                let comp = rumoca_ir_ast::ComponentReference {
                    local: partial_app.type_specifier.type_specifier_opt.is_some(),
                    parts,
                    ..Default::default()
                };

                // Get named arguments if present
                let args = if let Some(opt) = &partial_app.function_partial_application_opt {
                    collect_named_arguments(&opt.named_arguments)
                } else {
                    vec![]
                };

                Ok(rumoca_ir_ast::Expression::FunctionCall { comp, args })
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
                convert_expression_function_args(def)
            }
            modelica_grammar_trait::FunctionArguments::FunctionPartialApplicationFunctionArgumentsOpt0(fpa) => {
                // Convert 'function Foo.Bar(arg=val)' to a function call expression
                let partial_app = &fpa.function_partial_application;

                // Convert Name to ComponentReference
                let parts: Vec<rumoca_ir_ast::ComponentRefPart> = partial_app
                    .type_specifier
                    .name
                    .name
                    .iter()
                    .map(|token| rumoca_ir_ast::ComponentRefPart {
                        ident: token.clone(),
                        subs: None,
                    })
                    .collect();

                let comp = rumoca_ir_ast::ComponentReference {
                    local: partial_app.type_specifier.type_specifier_opt.is_some(),
                    parts,
                    ..Default::default()
                };

                // Get named arguments if present
                let func_args = if let Some(opt) = &partial_app.function_partial_application_opt {
                    collect_named_arguments(&opt.named_arguments)
                } else {
                    vec![]
                };

                let func_call_expr = rumoca_ir_ast::Expression::FunctionCall { comp, args: func_args };

                // Start with the partial application as the first arg
                let mut args = vec![func_call_expr];

                // Collect additional arguments if present
                if let Some(opt0) = &fpa.function_arguments_opt0 {
                    args.append(&mut opt0.function_arguments_non_first.args.clone());
                }

                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                let redeclare_flags = vec![false; args.len()];
                let replaceable_flags = vec![false; args.len()];
                Ok(ExpressionList {
                    args,
                    each_flags,
                    final_flags,
                    redeclare_flags,
                    replaceable_flags,
                })
            }
            modelica_grammar_trait::FunctionArguments::NamedArguments(named) => {
                let args = collect_named_arguments(&named.named_arguments);
                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                let redeclare_flags = vec![false; args.len()];
                let replaceable_flags = vec![false; args.len()];
                Ok(ExpressionList {
                    args,
                    each_flags,
                    final_flags,
                    redeclare_flags,
                    replaceable_flags,
                })
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
                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                let redeclare_flags = vec![false; args.len()];
                let replaceable_flags = vec![false; args.len()];
                Ok(ExpressionList {
                    args,
                    each_flags,
                    final_flags,
                    redeclare_flags,
                    replaceable_flags,
                })
            }
            modelica_grammar_trait::FunctionArgumentsNonFirst::NamedArguments(named) => {
                let args = collect_named_arguments(&named.named_arguments);
                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                let redeclare_flags = vec![false; args.len()];
                let replaceable_flags = vec![false; args.len()];
                Ok(ExpressionList {
                    args,
                    each_flags,
                    final_flags,
                    redeclare_flags,
                    replaceable_flags,
                })
            }
        }
    }
}

//-----------------------------------------------------------------------------
// Helper functions for Argument conversion to reduce function complexity.

/// Build a ComponentReference from an AST Name (already converted).
/// AST Name has: name: Vec<Token> - a flat list of identifier tokens.
fn ast_name_to_comp_ref(name: &rumoca_ir_ast::Name) -> rumoca_ir_ast::ComponentReference {
    let parts: Vec<rumoca_ir_ast::ComponentRefPart> = name
        .name
        .iter()
        .map(|token| rumoca_ir_ast::ComponentRefPart {
            ident: token.clone(),
            subs: None,
        })
        .collect();
    rumoca_ir_ast::ComponentReference {
        local: false,
        parts,
        ..Default::default()
    }
}

/// Build a single-part ComponentReference from an identifier token.
fn ident_to_comp_ref(ident: &rumoca_ir_core::Token) -> rumoca_ir_ast::ComponentReference {
    rumoca_ir_ast::ComponentReference {
        local: false,
        parts: vec![rumoca_ir_ast::ComponentRefPart {
            ident: ident.clone(),
            subs: None,
        }],
        ..Default::default()
    }
}

/// Extract arguments from a class modification, if present.
fn extract_class_mod_args(
    class_mod: &modelica_grammar_trait::ClassModification,
) -> Vec<rumoca_ir_ast::Expression> {
    class_mod
        .class_modification_opt
        .as_ref()
        .map(|opt| opt.argument_list.args.clone())
        .unwrap_or_default()
}

/// Convert a TypeClassSpecifier (inner type) to a Modification expression.
fn convert_type_class_specifier_inner(
    tcs: &modelica_grammar_trait::TypeClassSpecifier,
) -> rumoca_ir_ast::Expression {
    let name_ref = ident_to_comp_ref(&tcs.ident);
    let new_type_ref = ast_name_to_comp_ref(&tcs.type_specifier.name);
    let args = tcs
        .type_class_specifier_opt0
        .as_ref()
        .map(|class_mod| extract_class_mod_args(&class_mod.class_modification))
        .unwrap_or_default();

    rumoca_ir_ast::Expression::Modification {
        target: name_ref,
        value: Arc::new(rumoca_ir_ast::Expression::ClassModification {
            target: new_type_ref,
            modifications: args,
        }),
    }
}

/// Convert an EnumClassSpecifier (inner type) to a FunctionCall expression.
fn convert_enum_class_specifier_inner(
    ecs: &modelica_grammar_trait::EnumClassSpecifier,
) -> rumoca_ir_ast::Expression {
    let name_ref = ident_to_comp_ref(&ecs.ident);
    rumoca_ir_ast::Expression::FunctionCall {
        comp: name_ref,
        args: vec![],
    }
}

/// Collect named arguments from a NamedArguments structure recursively.
fn collect_partial_app_args(
    named_args: &modelica_grammar_trait::NamedArguments,
) -> Vec<rumoca_ir_ast::Expression> {
    let mut args = vec![rumoca_ir_ast::Expression::NamedArgument {
        name: named_args.named_argument.ident.clone(),
        value: Arc::new(named_args.named_argument.function_argument.clone()),
    }];
    if let Some(opt) = &named_args.named_arguments_opt {
        args.extend(collect_partial_app_args(&opt.named_arguments));
    }
    args
}

/// Convert a FunctionPartialClassSpecifier (inner type) to a Modification expression.
fn convert_function_partial_specifier_inner(
    fpcs: &modelica_grammar_trait::FunctionPartialClassSpecifier,
) -> rumoca_ir_ast::Expression {
    let name_ref = ident_to_comp_ref(&fpcs.ident);
    let base_func_ref =
        ast_name_to_comp_ref(&fpcs.function_partial_application.type_specifier.name);

    let args = fpcs
        .function_partial_application
        .function_partial_application_opt
        .as_ref()
        .map(|args_opt| collect_partial_app_args(&args_opt.named_arguments))
        .unwrap_or_default();

    rumoca_ir_ast::Expression::Modification {
        target: name_ref,
        value: Arc::new(rumoca_ir_ast::Expression::FunctionCall {
            comp: base_func_ref,
            args,
        }),
    }
}

/// Extract modification arguments from a component declaration.
fn extract_component_mod_args(
    decl: &modelica_grammar_trait::Declaration,
    _name_ref: &rumoca_ir_ast::ComponentReference,
) -> Result<
    (
        Vec<rumoca_ir_ast::Expression>,
        Option<rumoca_ir_ast::Expression>,
    ),
    anyhow::Error,
> {
    if let Some(modif) = &decl.declaration_opt0 {
        match &modif.modification {
            modelica_grammar_trait::Modification::EquModificationExpression(eq_mod) => {
                match &eq_mod.modification_expression {
                    modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                        // Return value modification as a direct assignment
                        Ok((vec![], Some(expr.expression.clone())))
                    }
                    modelica_grammar_trait::ModificationExpression::Break(_) => Ok((vec![], None)),
                }
            }
            modelica_grammar_trait::Modification::ClassModificationModificationOpt(class_mod) => {
                Ok((extract_class_mod_args(&class_mod.class_modification), None))
            }
        }
    } else {
        Ok((vec![], None))
    }
}

/// Convert a ComponentClause1 (inner type) redeclaration to a Modification expression.
fn convert_component_clause_redecl_inner(
    cc1: &modelica_grammar_trait::ComponentClause1,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    let decl = &cc1.component_declaration1.declaration;

    // Check if trying to redeclare a builtin attribute
    let redecl_name = &decl.ident.text;
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
    if ALL_BUILTIN_ATTRS.contains(&&**redecl_name) {
        return Err(semantic_error_from_token(
            format!(
                "Invalid redeclaration of {}, attributes of basic types may not be redeclared{}",
                redecl_name,
                loc_info(&decl.ident)
            ),
            &decl.ident,
        ));
    }

    let name_ref = ident_to_comp_ref(&decl.ident);
    let new_type_ref = ast_name_to_comp_ref(&cc1.type_specifier.name);

    let (args, direct_value) = extract_component_mod_args(decl, &name_ref)?;

    // If there's a direct value assignment, return Modification with that value
    if let Some(value) = direct_value {
        return Ok(rumoca_ir_ast::Expression::Modification {
            target: name_ref,
            value: Arc::new(rumoca_ir_ast::Expression::ClassModification {
                target: new_type_ref,
                modifications: vec![value],
            }),
        });
    }

    Ok(rumoca_ir_ast::Expression::Modification {
        target: name_ref,
        value: Arc::new(rumoca_ir_ast::Expression::ClassModification {
            target: new_type_ref,
            modifications: args,
        }),
    })
}

/// Convert a ShortClassSpecifier to an expression (for redeclarations).
fn convert_short_class_specifier(
    short_spec: &modelica_grammar_trait::ShortClassSpecifier,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    match short_spec {
        modelica_grammar_trait::ShortClassSpecifier::TypeClassSpecifier(type_spec) => {
            // Unwrap the enum variant to get the inner TypeClassSpecifier
            Ok(convert_type_class_specifier_inner(
                &type_spec.type_class_specifier,
            ))
        }
        modelica_grammar_trait::ShortClassSpecifier::EnumClassSpecifier(enum_spec) => {
            // Unwrap the enum variant to get the inner EnumClassSpecifier
            Ok(convert_enum_class_specifier_inner(
                &enum_spec.enum_class_specifier,
            ))
        }
        modelica_grammar_trait::ShortClassSpecifier::FunctionPartialClassSpecifier(
            partial_spec,
        ) => {
            // FunctionPartialClassSpecifier is already the inner type
            Ok(convert_function_partial_specifier_inner(
                &partial_spec.function_partial_class_specifier,
            ))
        }
    }
}

/// Convert a replaceable component clause (inner type) to a Modification expression.
fn convert_replaceable_component_clause_inner(
    cc1: &modelica_grammar_trait::ComponentClause1,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    let decl = &cc1.component_declaration1.declaration;
    let name_ref = ident_to_comp_ref(&decl.ident);
    let new_type_ref = ast_name_to_comp_ref(&cc1.type_specifier.name);

    if let Some(modif) = &decl.declaration_opt0 {
        match &modif.modification {
            modelica_grammar_trait::Modification::EquModificationExpression(eq_mod) => {
                match &eq_mod.modification_expression {
                    modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                        return Ok(rumoca_ir_ast::Expression::Modification {
                            target: name_ref,
                            value: Arc::new(expr.expression.clone()),
                        });
                    }
                    modelica_grammar_trait::ModificationExpression::Break(_) => {}
                }
            }
            modelica_grammar_trait::Modification::ClassModificationModificationOpt(class_mod) => {
                let args = extract_class_mod_args(&class_mod.class_modification);
                return Ok(rumoca_ir_ast::Expression::Modification {
                    target: name_ref,
                    value: Arc::new(rumoca_ir_ast::Expression::ClassModification {
                        target: new_type_ref,
                        modifications: args,
                    }),
                });
            }
        }
    }

    Ok(rumoca_ir_ast::Expression::Modification {
        target: name_ref,
        value: Arc::new(rumoca_ir_ast::Expression::ClassModification {
            target: new_type_ref,
            modifications: vec![],
        }),
    })
}

/// Convert an ElementReplaceable redeclaration to an expression.
/// The input is the inner ElementReplaceable type directly.
fn convert_element_replaceable_redecl(
    repl: &modelica_grammar_trait::ElementReplaceable,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    match &repl.element_replaceable_group {
        modelica_grammar_trait::ElementReplaceableGroup::ShortClassDefinition(short_def) => {
            convert_short_class_specifier(&short_def.short_class_definition.short_class_specifier)
        }
        modelica_grammar_trait::ElementReplaceableGroup::ComponentClause1(comp_clause) => {
            convert_replaceable_component_clause_inner(&comp_clause.component_clause1)
        }
    }
}

/// Convert an ElementRedeclaration to an expression.
/// The input is the inner ElementRedeclaration type directly.
fn convert_element_redeclaration(
    redecl: &modelica_grammar_trait::ElementRedeclaration,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    match &redecl.element_redeclaration_group {
        modelica_grammar_trait::ElementRedeclarationGroup::ShortClassDefinition(short_def) => {
            convert_short_class_specifier(&short_def.short_class_definition.short_class_specifier)
        }
        modelica_grammar_trait::ElementRedeclarationGroup::ComponentClause1(comp_clause) => {
            convert_component_clause_redecl_inner(&comp_clause.component_clause1)
        }
        modelica_grammar_trait::ElementRedeclarationGroup::ElementReplaceable(repl) => {
            convert_element_replaceable_redecl(&repl.element_replaceable)
        }
    }
}

/// Convert an ElementModification to an expression.
/// The input is the inner ElementModification type directly.
fn convert_element_modification(
    elem: &modelica_grammar_trait::ElementModification,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    // ElementModification has: name (AST Name), element_modification_opt
    let target = ast_name_to_comp_ref(&elem.name);

    match &elem.element_modification_opt {
        Some(opt) => match &opt.modification {
            modelica_grammar_trait::Modification::ClassModificationModificationOpt(class_modif) => {
                let args = extract_class_mod_args(&class_modif.class_modification);
                let mod_expr = rumoca_ir_ast::Expression::ClassModification {
                    target: target.clone(),
                    modifications: args,
                };

                // If there's also a value assignment (name(mods) = value), wrap in binary
                if let Some(mod_opt) = &class_modif.modification_opt {
                    match &mod_opt.modification_expression {
                        modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                            Ok(rumoca_ir_ast::Expression::Binary {
                                op: rumoca_ir_core::OpBinary::Assign(
                                    rumoca_ir_core::Token::default(),
                                ),
                                lhs: Arc::new(mod_expr),
                                rhs: Arc::new(expr.expression.clone()),
                            })
                        }
                        modelica_grammar_trait::ModificationExpression::Break(_) => Ok(mod_expr),
                    }
                } else {
                    Ok(mod_expr)
                }
            }
            modelica_grammar_trait::Modification::EquModificationExpression(modif) => {
                match &modif.modification_expression {
                    modelica_grammar_trait::ModificationExpression::Break(_) => {
                        Ok(rumoca_ir_ast::Expression::Empty)
                    }
                    modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                        Ok(rumoca_ir_ast::Expression::Modification {
                            target,
                            value: Arc::new(expr.expression.clone()),
                        })
                    }
                }
            }
        },
        None => Ok(rumoca_ir_ast::Expression::Empty),
    }
}

/// Convert an ElementModificationOrReplaceable to an expression.
/// The input is the inner ElementModificationOrReplaceable type directly.
fn convert_element_mod_or_replaceable(
    modif: &modelica_grammar_trait::ElementModificationOrReplaceable,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    match &modif.element_modification_or_replaceable_group {
        modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementModification(
            elem,
        ) => {
            // Unwrap to get inner ElementModification
            convert_element_modification(&elem.element_modification)
        }
        modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementReplaceable(repl) => {
            convert_element_replaceable_redecl(&repl.element_replaceable)
        }
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::ArgumentList> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArgumentList,
    ) -> std::result::Result<Self, Self::Error> {
        // After grammar change, ast.argument is ModificationArg
        // Extract expressions, modifier flags, and redeclaration metadata from ModificationArgs.
        let mut args = vec![ast.argument.expression.clone()];
        let mut each_flags = vec![ast.argument.each];
        let mut final_flags = vec![ast.argument.r#final];
        let mut redeclare_flags = vec![ast.argument.redeclare];
        let mut replaceable_flags = vec![ast.argument.replaceable];
        for arg in &ast.argument_list_list {
            args.push(arg.argument.expression.clone());
            each_flags.push(arg.argument.each);
            final_flags.push(arg.argument.r#final);
            redeclare_flags.push(arg.argument.redeclare);
            replaceable_flags.push(arg.argument.replaceable);
        }
        Ok(ExpressionList {
            args,
            each_flags,
            final_flags,
            redeclare_flags,
            replaceable_flags,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::Argument> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Argument) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Argument::ElementModificationOrReplaceable(modif) => {
                // Unwrap the wrapper to get the inner ElementModificationOrReplaceable
                convert_element_mod_or_replaceable(&modif.element_modification_or_replaceable)
            }
            modelica_grammar_trait::Argument::ElementRedeclaration(redcl) => {
                // Unwrap the wrapper to get the inner ElementRedeclaration
                convert_element_redeclaration(&redcl.element_redeclaration)
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Argument> for ModificationArg {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Argument) -> std::result::Result<Self, Self::Error> {
        // Extract the modifier flags from the argument structure.
        let (each, r#final, redeclare, replaceable) = match ast {
            modelica_grammar_trait::Argument::ElementModificationOrReplaceable(modif) => {
                let emor = &modif.element_modification_or_replaceable;
                let each = emor.element_modification_or_replaceable_opt.is_some();
                let r#final = emor.element_modification_or_replaceable_opt0.is_some();
                let replaceable = matches!(
                    emor.element_modification_or_replaceable_group,
                    modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementReplaceable(_)
                );
                (each, r#final, false, replaceable)
            }
            modelica_grammar_trait::Argument::ElementRedeclaration(redecl) => {
                // Redeclarations have their own each/final flags inside element_redeclaration
                let each = redecl
                    .element_redeclaration
                    .element_redeclaration_opt
                    .is_some();
                let r#final = redecl
                    .element_redeclaration
                    .element_redeclaration_opt0
                    .is_some();
                (each, r#final, true, false)
            }
        };

        // Use the existing conversion to get the expression
        let expression: rumoca_ir_ast::Expression = ast.try_into()?;

        Ok(ModificationArg {
            expression,
            each,
            r#final,
            redeclare,
            replaceable,
        })
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
        let each_flags = vec![false; v.len()];
        let final_flags = vec![false; v.len()];
        let redeclare_flags = vec![false; v.len()];
        let replaceable_flags = vec![false; v.len()];
        Ok(ExpressionList {
            args: v,
            each_flags,
            final_flags,
            redeclare_flags,
            replaceable_flags,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::FunctionCallArgs> for ExpressionList {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::FunctionCallArgs,
    ) -> std::result::Result<Self, Self::Error> {
        if let Some(opt) = &ast.function_call_args_opt {
            let args = opt.function_arguments.args.clone();
            let each_flags = opt.function_arguments.each_flags.clone();
            let final_flags = opt.function_arguments.final_flags.clone();
            let redeclare_flags = opt.function_arguments.redeclare_flags.clone();
            let replaceable_flags = opt.function_arguments.replaceable_flags.clone();
            Ok(ExpressionList {
                args,
                each_flags,
                final_flags,
                redeclare_flags,
                replaceable_flags,
            })
        } else {
            Ok(ExpressionList {
                args: vec![],
                each_flags: vec![],
                final_flags: vec![],
                redeclare_flags: vec![],
                replaceable_flags: vec![],
            })
        }
    }
}

//-----------------------------------------------------------------------------
// Helper functions for Primary conversion to reduce function complexity.

/// Convert ExpressionList to Vec<Expression>.
fn expr_list_to_vec(el: &modelica_grammar_trait::ExpressionList) -> Vec<rumoca_ir_ast::Expression> {
    let mut elements = vec![el.expression.clone()];
    for item in &el.expression_list_list {
        elements.push(item.expression.clone());
    }
    elements
}

/// Convert a RangePrimary (matrix literal) to an Expression.
fn convert_range_primary(rp: &modelica_grammar_trait::RangePrimary) -> rumoca_ir_ast::Expression {
    // First row
    let first_row = expr_list_to_vec(&rp.expression_list);

    // Check if there are additional rows (semicolons)
    if rp.range_primary_list.is_empty() {
        // Single row: [1, 2, 3] - just return as matrix Array
        rumoca_ir_ast::Expression::Array {
            elements: first_row,
            is_matrix: true,
        }
    } else {
        // Multiple rows: [1, 2; 3, 4] - create array of row arrays
        let mut rows = vec![rumoca_ir_ast::Expression::Array {
            elements: first_row,
            is_matrix: true,
        }];
        for row_item in &rp.range_primary_list {
            let row = expr_list_to_vec(&row_item.expression_list);
            rows.push(rumoca_ir_ast::Expression::Array {
                elements: row,
                is_matrix: true,
            });
        }
        rumoca_ir_ast::Expression::Array {
            elements: rows,
            is_matrix: true,
        }
    }
}

/// Convert an OutputPrimary (parenthesized expression or tuple) to an Expression.
fn convert_output_primary(
    primary: &modelica_grammar_trait::OutputPrimary,
) -> Result<rumoca_ir_ast::Expression, anyhow::Error> {
    let base = if primary.output_expression_list.args.len() > 1 {
        // Multiple outputs like (a, b) = func() - create a Tuple
        rumoca_ir_ast::Expression::Tuple {
            elements: primary.output_expression_list.args.clone(),
        }
    } else if primary.output_expression_list.args.len() == 1 {
        // Single expression in parentheses - preserve with Parenthesized wrapper
        rumoca_ir_ast::Expression::Parenthesized {
            inner: Arc::new(primary.output_expression_list.args[0].clone()),
        }
    } else {
        // Empty parentheses - return Empty expression
        rumoca_ir_ast::Expression::Empty
    };

    if let Some(opt) = &primary.output_primary_opt {
        match &opt.output_primary_opt_group {
            modelica_grammar_trait::OutputPrimaryOptGroup::ArraySubscripts(subs) => {
                Ok(rumoca_ir_ast::Expression::ArrayIndex {
                    base: Arc::new(base),
                    subscripts: subs.array_subscripts.subscripts.clone(),
                })
            }
            modelica_grammar_trait::OutputPrimaryOptGroup::DotIdent(field) => {
                Ok(rumoca_ir_ast::Expression::FieldAccess {
                    base: Arc::new(base),
                    field: field.ident.text.to_string(),
                })
            }
        }
    } else {
        Ok(base)
    }
}

/// Convert a GlobalFunctionCall (der, initial, pure) to an Expression.
///
/// Handles optional array subscripts after the function call: `der(x)[i]`
fn convert_global_function_call(
    gfc: &modelica_grammar_trait::GlobalFunctionCall,
) -> rumoca_ir_ast::Expression {
    let tok = match &gfc.global_function_call_group {
        modelica_grammar_trait::GlobalFunctionCallGroup::Der(expr) => expr.der.der.clone(),
        modelica_grammar_trait::GlobalFunctionCallGroup::Initial(expr) => {
            expr.initial.initial.clone()
        }
        modelica_grammar_trait::GlobalFunctionCallGroup::Pure(expr) => expr.pure.pure.clone(),
    };
    let part = rumoca_ir_ast::ComponentRefPart {
        ident: tok.into(),
        subs: None,
    };
    let func_call = rumoca_ir_ast::Expression::FunctionCall {
        comp: rumoca_ir_ast::ComponentReference {
            local: false,
            parts: vec![part],
            ..Default::default()
        },
        args: gfc.function_call_args.args.clone(),
    };

    // If there are subscripts, wrap in ArrayIndex
    match &gfc.global_function_call_opt {
        Some(opt) => rumoca_ir_ast::Expression::ArrayIndex {
            base: Arc::new(func_call),
            subscripts: opt.array_subscripts.subscripts.clone(),
        },
        None => func_call,
    }
}

//-----------------------------------------------------------------------------
impl TryFrom<&modelica_grammar_trait::Primary> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Primary) -> std::result::Result<Self, Self::Error> {
        match &ast {
            modelica_grammar_trait::Primary::ComponentPrimary(comp) => {
                match &comp.component_primary.component_primary_opt {
                    Some(args) => {
                        // Function call: f(x) or f(x)[i]
                        let func_call = rumoca_ir_ast::Expression::FunctionCall {
                            comp: comp.component_primary.component_reference.clone(),
                            args: args.function_call_args.args.clone(),
                        };
                        // Check for optional subscripts: f(x)[i]
                        match &args.component_primary_opt0 {
                            Some(subs) => Ok(rumoca_ir_ast::Expression::ArrayIndex {
                                base: Arc::new(func_call),
                                subscripts: subs.array_subscripts.subscripts.clone(),
                            }),
                            None => Ok(func_call),
                        }
                    }
                    None => Ok(rumoca_ir_ast::Expression::ComponentReference(
                        comp.component_primary.component_reference.clone(),
                    )),
                }
            }
            modelica_grammar_trait::Primary::UnsignedNumber(unsigned_num) => {
                match &unsigned_num.unsigned_number {
                    modelica_grammar_trait::UnsignedNumber::UnsignedInteger(unsigned_int) => {
                        Ok(rumoca_ir_ast::Expression::Terminal {
                            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                            token: unsigned_int.unsigned_integer.clone(),
                        })
                    }
                    modelica_grammar_trait::UnsignedNumber::UnsignedReal(unsigned_real) => {
                        Ok(rumoca_ir_ast::Expression::Terminal {
                            terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                            token: unsigned_real.unsigned_real.clone(),
                        })
                    }
                }
            }
            modelica_grammar_trait::Primary::String(string) => {
                Ok(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::String,
                    token: string.string.clone(),
                })
            }
            modelica_grammar_trait::Primary::True(bool) => {
                Ok(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::Bool,
                    token: bool.r#true.r#true.clone().into(),
                })
            }
            modelica_grammar_trait::Primary::False(bool) => {
                Ok(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::Bool,
                    token: bool.r#false.r#false.clone().into(),
                })
            }
            modelica_grammar_trait::Primary::End(end) => Ok(rumoca_ir_ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::End,
                token: end.end.end.clone().into(),
            }),
            modelica_grammar_trait::Primary::ArrayPrimary(arr) => {
                match &arr.array_primary.array_primary_opt {
                    Some(opt) => collect_array_elements(&opt.array_arguments),
                    None => Ok(rumoca_ir_ast::Expression::Array {
                        elements: vec![],
                        is_matrix: false,
                    }),
                }
            }
            modelica_grammar_trait::Primary::RangePrimary(range) => {
                Ok(convert_range_primary(&range.range_primary))
            }
            modelica_grammar_trait::Primary::OutputPrimary(output) => {
                convert_output_primary(&output.output_primary)
            }
            modelica_grammar_trait::Primary::GlobalFunctionCall(expr) => {
                Ok(convert_global_function_call(&expr.global_function_call))
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Factor> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Factor) -> std::result::Result<Self, Self::Error> {
        if ast.factor_list.is_empty() {
            Ok(ast.primary.clone())
        } else {
            let op = match &ast.factor_list[0].factor_list_group {
                modelica_grammar_trait::FactorListGroup::Circumflex(c) => {
                    rumoca_ir_core::OpBinary::Exp(c.circumflex.clone().into())
                }
                modelica_grammar_trait::FactorListGroup::DotCircumflex(dc) => {
                    rumoca_ir_core::OpBinary::ExpElem(dc.dot_circumflex.clone().into())
                }
            };
            Ok(rumoca_ir_ast::Expression::Binary {
                op,
                lhs: Arc::new(ast.primary.clone()),
                rhs: Arc::new(ast.factor_list[0].primary.clone()),
            })
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Term> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Term) -> std::result::Result<Self, Self::Error> {
        if ast.term_list.is_empty() {
            return Ok(ast.factor.clone());
        }
        let mut lhs = ast.factor.clone();
        for factor in &ast.term_list {
            lhs = rumoca_ir_ast::Expression::Binary {
                lhs: Arc::new(lhs),
                op: mul_op_to_binary(&factor.mul_operator),
                rhs: Arc::new(factor.factor.clone()),
            };
        }
        Ok(lhs)
    }
}

impl TryFrom<&modelica_grammar_trait::ArithmeticExpression> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::ArithmeticExpression,
    ) -> std::result::Result<Self, Self::Error> {
        // handle first term
        let mut lhs = match &ast.arithmetic_expression_opt {
            Some(opt) => rumoca_ir_ast::Expression::Unary {
                op: add_op_to_unary(&opt.add_operator),
                rhs: Arc::new(ast.term.clone()),
            },
            None => ast.term.clone(),
        };

        // if has term list, process expressions
        for term in &ast.arithmetic_expression_list {
            lhs = rumoca_ir_ast::Expression::Binary {
                lhs: Arc::new(lhs),
                op: add_op_to_binary(&term.add_operator),
                rhs: Arc::new(term.term.clone()),
            };
        }
        Ok(lhs)
    }
}

impl TryFrom<&modelica_grammar_trait::Relation> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
        match &ast.relation_opt {
            Some(relation) => Ok(rumoca_ir_ast::Expression::Binary {
                lhs: Arc::new(ast.arithmetic_expression.clone()),
                op: match &relation.relational_operator {
                    modelica_grammar_trait::RelationalOperator::EquEqu(tok) => {
                        rumoca_ir_core::OpBinary::Eq(tok.equ_equ.clone().into())
                    }
                    modelica_grammar_trait::RelationalOperator::GT(tok) => {
                        rumoca_ir_core::OpBinary::Gt(tok.g_t.clone().into())
                    }
                    modelica_grammar_trait::RelationalOperator::LT(tok) => {
                        rumoca_ir_core::OpBinary::Lt(tok.l_t.clone().into())
                    }
                    modelica_grammar_trait::RelationalOperator::GTEqu(tok) => {
                        rumoca_ir_core::OpBinary::Ge(tok.g_t_equ.clone().into())
                    }
                    modelica_grammar_trait::RelationalOperator::LTEqu(tok) => {
                        rumoca_ir_core::OpBinary::Le(tok.l_t_equ.clone().into())
                    }
                    modelica_grammar_trait::RelationalOperator::LTGT(tok) => {
                        rumoca_ir_core::OpBinary::Neq(tok.l_t_g_t.clone().into())
                    }
                },
                rhs: Arc::new(relation.arithmetic_expression.clone()),
            }),
            None => Ok(ast.arithmetic_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalFactor> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalFactor,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.logical_factor_opt {
            Some(opt) => {
                let not_tok = opt.not.not.clone().into();
                Ok(rumoca_ir_ast::Expression::Unary {
                    op: rumoca_ir_core::OpUnary::Not(not_tok),
                    rhs: Arc::new(ast.relation.clone()),
                })
            }
            None => Ok(ast.relation.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalTerm> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalTerm,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_term_list.is_empty() {
            Ok(ast.logical_factor.clone())
        } else {
            let mut lhs = ast.logical_factor.clone();
            for term in &ast.logical_term_list {
                lhs = rumoca_ir_ast::Expression::Binary {
                    lhs: Arc::new(lhs),
                    op: rumoca_ir_core::OpBinary::And(rumoca_ir_core::Token::default()),
                    rhs: Arc::new(term.logical_factor.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalExpression> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalExpression,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_expression_list.is_empty() {
            Ok(ast.logical_term.clone())
        } else {
            let mut lhs = ast.logical_term.clone();
            for term in &ast.logical_expression_list {
                lhs = rumoca_ir_ast::Expression::Binary {
                    lhs: Arc::new(lhs),
                    op: rumoca_ir_core::OpBinary::Or(rumoca_ir_core::Token::default()),
                    rhs: Arc::new(term.logical_term.clone()),
                };
            }
            Ok(lhs)
        }
    }
}

impl TryFrom<&modelica_grammar_trait::SimpleExpression> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::SimpleExpression,
    ) -> std::result::Result<Self, Self::Error> {
        match &ast.simple_expression_opt {
            Some(opt) => match &opt.simple_expression_opt0 {
                Some(opt0) => Ok(rumoca_ir_ast::Expression::Range {
                    start: Arc::new(ast.logical_expression.clone()),
                    step: Some(Arc::new(opt.logical_expression.clone())),
                    end: Arc::new(opt0.logical_expression.clone()),
                }),
                None => Ok(rumoca_ir_ast::Expression::Range {
                    start: Arc::new(ast.logical_expression.clone()),
                    step: None,
                    end: Arc::new(opt.logical_expression.clone()),
                }),
            },
            None => Ok(ast.logical_expression.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Expression> for rumoca_ir_ast::Expression {
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
                let condition = if_expr.expression.clone();
                let then_expr = if_expr.expression0.clone();
                branches.push((condition, then_expr));

                // Add any elseif branches from the list
                for elseif in &if_expr.if_expression_list {
                    let elseif_cond = elseif.expression.clone();
                    let elseif_expr = elseif.expression0.clone();
                    branches.push((elseif_cond, elseif_expr));
                }

                // The else branch is expression1
                let else_branch = Arc::new(if_expr.expression1.clone());

                Ok(rumoca_ir_ast::Expression::If {
                    branches,
                    else_branch,
                })
            }
        }
    }
}
