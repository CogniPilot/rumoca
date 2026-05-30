//! Conversion for expressions.

use super::helpers::{
    collect_array_elements, convert_for_indices, expression_list_span, loc_info, merge_spans,
    token_span,
};
use crate::errors::semantic_error_from_token;
use crate::generated::modelica_grammar_trait;
use rumoca_core::Span;
use std::sync::Arc;

//-----------------------------------------------------------------------------
// Helper functions to reduce nesting in operator conversions.

fn empty_expr() -> rumoca_ir_ast::Expression {
    rumoca_ir_ast::Expression::Empty { span: Span::DUMMY }
}

fn call_span(comp: &rumoca_ir_ast::ComponentReference, args: &[rumoca_ir_ast::Expression]) -> Span {
    args.last()
        .map_or(comp.span, |last| merge_spans(comp.span, last.span()))
}

fn binary_span(lhs: &rumoca_ir_ast::Expression, rhs: &rumoca_ir_ast::Expression) -> Span {
    merge_spans(lhs.span(), rhs.span())
}

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
        span: base_expr.span(),
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
fn mul_op_to_binary(op: &modelica_grammar_trait::MulOperator) -> rumoca_core::OpBinary {
    match op {
        modelica_grammar_trait::MulOperator::Star(_) => rumoca_core::OpBinary::Mul,
        modelica_grammar_trait::MulOperator::Slash(_) => rumoca_core::OpBinary::Div,
        modelica_grammar_trait::MulOperator::DotSlash(_) => rumoca_core::OpBinary::DivElem,
        modelica_grammar_trait::MulOperator::DotStar(_) => rumoca_core::OpBinary::MulElem,
    }
}

/// Convert an AddOperator to OpBinary.
fn add_op_to_binary(op: &modelica_grammar_trait::AddOperator) -> rumoca_core::OpBinary {
    match op {
        modelica_grammar_trait::AddOperator::Plus(_) => rumoca_core::OpBinary::Add,
        modelica_grammar_trait::AddOperator::Minus(_) => rumoca_core::OpBinary::Sub,
        modelica_grammar_trait::AddOperator::DotPlus(_) => rumoca_core::OpBinary::AddElem,
        modelica_grammar_trait::AddOperator::DotMinus(_) => rumoca_core::OpBinary::SubElem,
    }
}

/// Convert an AddOperator to OpUnary.
fn add_op_to_unary(op: &modelica_grammar_trait::AddOperator) -> rumoca_core::OpUnary {
    match op {
        modelica_grammar_trait::AddOperator::Minus(_) => rumoca_core::OpUnary::Minus,
        modelica_grammar_trait::AddOperator::Plus(_) => rumoca_core::OpUnary::Plus,
        modelica_grammar_trait::AddOperator::DotMinus(_) => rumoca_core::OpUnary::DotMinus,
        modelica_grammar_trait::AddOperator::DotPlus(_) => rumoca_core::OpUnary::DotPlus,
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
        span: token_span(&named_arg.ident),
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
                    span: parts
                        .first()
                        .map(|part| token_span(&part.ident))
                        .unwrap_or(Span::DUMMY),
                    parts,
                    ..Default::default()
                };

                // Get named arguments if present
                let args = if let Some(opt) = &partial_app.function_partial_application_opt {
                    collect_named_arguments(&opt.named_arguments)
                } else {
                    vec![]
                };

                let span = call_span(&comp, &args);
                Ok(rumoca_ir_ast::Expression::FunctionCall { comp, args, span })
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
                    span: parts
                        .first()
                        .map(|part| token_span(&part.ident))
                        .unwrap_or(Span::DUMMY),
                    parts,
                    ..Default::default()
                };

                // Get named arguments if present
                let func_args = if let Some(opt) = &partial_app.function_partial_application_opt {
                    collect_named_arguments(&opt.named_arguments)
                } else {
                    vec![]
                };

                let func_call_expr = rumoca_ir_ast::Expression::FunctionCall {
                    span: call_span(&comp, &func_args),
                    comp,
                    args: func_args,
                };

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
        span: parts
            .first()
            .map(|part| token_span(&part.ident))
            .unwrap_or(Span::DUMMY),
        parts,
        ..Default::default()
    }
}

/// Build a single-part ComponentReference from an identifier token.
fn ident_to_comp_ref(ident: &rumoca_core::Token) -> rumoca_ir_ast::ComponentReference {
    rumoca_ir_ast::ComponentReference {
        local: false,
        span: token_span(ident),
        parts: vec![rumoca_ir_ast::ComponentRefPart {
            ident: ident.clone(),
            subs: None,
        }],
        ..Default::default()
    }
}

fn extract_class_mod_arg_list(
    class_mod: &modelica_grammar_trait::ClassModification,
) -> ExpressionList {
    class_mod
        .class_modification_opt
        .as_ref()
        .map(|opt| opt.argument_list.clone())
        .unwrap_or_default()
}

/// Extract arguments from a class modification, if present.
fn extract_class_mod_args(
    class_mod: &modelica_grammar_trait::ClassModification,
) -> Vec<rumoca_ir_ast::Expression> {
    extract_class_mod_arg_list(class_mod).args
}

/// Convert a TypeClassSpecifier (inner type) to a Modification expression.
fn convert_type_class_specifier_inner(
    tcs: &modelica_grammar_trait::TypeClassSpecifier,
) -> rumoca_ir_ast::Expression {
    let name_ref = ident_to_comp_ref(&tcs.ident);
    let new_type_ref = ast_name_to_comp_ref(&tcs.type_specifier.name);
    let arg_list = tcs
        .type_class_specifier_opt0
        .as_ref()
        .map(|class_mod| extract_class_mod_arg_list(&class_mod.class_modification))
        .unwrap_or_default();

    let class_mod = rumoca_ir_ast::Expression::ClassModification {
        span: new_type_ref.span,
        target: new_type_ref,
        modifications: arg_list.args,
        each_flags: arg_list.each_flags,
        final_flags: arg_list.final_flags,
        redeclare_flags: arg_list.redeclare_flags,
    };
    rumoca_ir_ast::Expression::Modification {
        span: merge_spans(name_ref.span, class_mod.span()),
        target: name_ref,
        value: Arc::new(class_mod),
    }
}

/// Convert an EnumClassSpecifier (inner type) to a FunctionCall expression.
fn convert_enum_class_specifier_inner(
    ecs: &modelica_grammar_trait::EnumClassSpecifier,
) -> rumoca_ir_ast::Expression {
    let name_ref = ident_to_comp_ref(&ecs.ident);
    rumoca_ir_ast::Expression::FunctionCall {
        span: name_ref.span,
        comp: name_ref,
        args: vec![],
    }
}

/// Collect named arguments from a NamedArguments structure recursively.
fn collect_partial_app_args(
    named_args: &modelica_grammar_trait::NamedArguments,
) -> Vec<rumoca_ir_ast::Expression> {
    let mut args = vec![rumoca_ir_ast::Expression::NamedArgument {
        span: token_span(&named_args.named_argument.ident),
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

    let function_call = rumoca_ir_ast::Expression::FunctionCall {
        span: call_span(&base_func_ref, &args),
        comp: base_func_ref,
        args,
    };
    rumoca_ir_ast::Expression::Modification {
        span: merge_spans(name_ref.span, function_call.span()),
        target: name_ref,
        value: Arc::new(function_call),
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
        let class_mod = rumoca_ir_ast::Expression::ClassModification {
            span: merge_spans(new_type_ref.span, value.span()),
            target: new_type_ref,
            modifications: vec![value],
            each_flags: vec![false],
            final_flags: vec![false],
            redeclare_flags: vec![false],
        };
        return Ok(rumoca_ir_ast::Expression::Modification {
            span: merge_spans(name_ref.span, class_mod.span()),
            target: name_ref,
            value: Arc::new(class_mod),
        });
    }

    let class_mod = rumoca_ir_ast::Expression::ClassModification {
        span: merge_spans(new_type_ref.span, expression_list_span(&args)),
        target: new_type_ref,
        modifications: args,
        each_flags: Vec::new(),
        final_flags: Vec::new(),
        redeclare_flags: Vec::new(),
    };
    Ok(rumoca_ir_ast::Expression::Modification {
        span: merge_spans(name_ref.span, class_mod.span()),
        target: name_ref,
        value: Arc::new(class_mod),
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
                            span: merge_spans(name_ref.span, expr.expression.span()),
                            target: name_ref,
                            value: Arc::new(expr.expression.clone()),
                        });
                    }
                    modelica_grammar_trait::ModificationExpression::Break(_) => {}
                }
            }
            modelica_grammar_trait::Modification::ClassModificationModificationOpt(class_mod) => {
                let arg_list = extract_class_mod_arg_list(&class_mod.class_modification);
                let class_mod = rumoca_ir_ast::Expression::ClassModification {
                    span: merge_spans(new_type_ref.span, expression_list_span(&arg_list.args)),
                    target: new_type_ref,
                    modifications: arg_list.args,
                    each_flags: arg_list.each_flags,
                    final_flags: arg_list.final_flags,
                    redeclare_flags: arg_list.redeclare_flags,
                };
                return Ok(rumoca_ir_ast::Expression::Modification {
                    span: merge_spans(name_ref.span, class_mod.span()),
                    target: name_ref,
                    value: Arc::new(class_mod),
                });
            }
        }
    }

    let class_mod = rumoca_ir_ast::Expression::ClassModification {
        span: new_type_ref.span,
        target: new_type_ref,
        modifications: vec![],
        each_flags: vec![],
        final_flags: vec![],
        redeclare_flags: vec![],
    };
    Ok(rumoca_ir_ast::Expression::Modification {
        span: merge_spans(name_ref.span, class_mod.span()),
        target: name_ref,
        value: Arc::new(class_mod),
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
                let arg_list = extract_class_mod_arg_list(&class_modif.class_modification);
                let mod_expr = rumoca_ir_ast::Expression::ClassModification {
                    span: merge_spans(target.span, expression_list_span(&arg_list.args)),
                    target: target.clone(),
                    modifications: arg_list.args,
                    each_flags: arg_list.each_flags,
                    final_flags: arg_list.final_flags,
                    redeclare_flags: arg_list.redeclare_flags,
                };

                // If there's also a value assignment (name(mods) = value), wrap in binary
                if let Some(mod_opt) = &class_modif.modification_opt {
                    match &mod_opt.modification_expression {
                        modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                            Ok(rumoca_ir_ast::Expression::Binary {
                                op: rumoca_core::OpBinary::Assign,
                                span: binary_span(&mod_expr, &expr.expression),
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
                    modelica_grammar_trait::ModificationExpression::Break(_) => Ok(empty_expr()),
                    modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                        Ok(rumoca_ir_ast::Expression::Modification {
                            span: merge_spans(target.span, expr.expression.span()),
                            target,
                            value: Arc::new(expr.expression.clone()),
                        })
                    }
                }
            }
        },
        None => Ok(empty_expr()),
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
            span: expression_list_span(&first_row),
            elements: first_row,
            is_matrix: true,
        }
    } else {
        // Multiple rows: [1, 2; 3, 4] - create array of row arrays
        let mut rows = vec![rumoca_ir_ast::Expression::Array {
            span: expression_list_span(&first_row),
            elements: first_row,
            is_matrix: true,
        }];
        for row_item in &rp.range_primary_list {
            let row = expr_list_to_vec(&row_item.expression_list);
            rows.push(rumoca_ir_ast::Expression::Array {
                span: expression_list_span(&row),
                elements: row,
                is_matrix: true,
            });
        }
        rumoca_ir_ast::Expression::Array {
            span: expression_list_span(&rows),
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
            span: expression_list_span(&primary.output_expression_list.args),
            elements: primary.output_expression_list.args.clone(),
        }
    } else if primary.output_expression_list.args.len() == 1 {
        // Single expression in parentheses - preserve with Parenthesized wrapper
        rumoca_ir_ast::Expression::Parenthesized {
            span: primary.output_expression_list.args[0].span(),
            inner: Arc::new(primary.output_expression_list.args[0].clone()),
        }
    } else {
        // Empty parentheses - return Empty expression
        empty_expr()
    };

    if let Some(opt) = &primary.output_primary_opt {
        match &opt.output_primary_opt_group {
            modelica_grammar_trait::OutputPrimaryOptGroup::ArraySubscripts(subs) => {
                Ok(rumoca_ir_ast::Expression::ArrayIndex {
                    span: base.span(),
                    base: Arc::new(base),
                    subscripts: subs.array_subscripts.subscripts.clone(),
                })
            }
            modelica_grammar_trait::OutputPrimaryOptGroup::DotIdent(field) => {
                Ok(rumoca_ir_ast::Expression::FieldAccess {
                    span: merge_spans(base.span(), token_span(&field.ident)),
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
        ident: tok.clone().into(),
        subs: None,
    };
    let comp = rumoca_ir_ast::ComponentReference {
        local: false,
        span: token_span(&tok.into()),
        parts: vec![part],
        ..Default::default()
    };
    let args = gfc.function_call_args.args.clone();
    let func_call = rumoca_ir_ast::Expression::FunctionCall {
        span: call_span(&comp, &args),
        comp,
        args,
    };

    // If there are subscripts, wrap in ArrayIndex
    match &gfc.global_function_call_opt {
        Some(opt) => rumoca_ir_ast::Expression::ArrayIndex {
            span: func_call.span(),
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
                        let comp_ref = comp.component_primary.component_reference.clone();
                        let args_vec = args.function_call_args.args.clone();
                        let func_call = rumoca_ir_ast::Expression::FunctionCall {
                            span: call_span(&comp_ref, &args_vec),
                            comp: comp_ref,
                            args: args_vec,
                        };
                        // Check for optional subscripts: f(x)[i]
                        match &args.component_primary_opt0 {
                            Some(subs) => Ok(rumoca_ir_ast::Expression::ArrayIndex {
                                span: func_call.span(),
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
                            span: token_span(&unsigned_int.unsigned_integer),
                        })
                    }
                    modelica_grammar_trait::UnsignedNumber::UnsignedReal(unsigned_real) => {
                        Ok(rumoca_ir_ast::Expression::Terminal {
                            terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
                            token: unsigned_real.unsigned_real.clone(),
                            span: token_span(&unsigned_real.unsigned_real),
                        })
                    }
                }
            }
            modelica_grammar_trait::Primary::String(string) => {
                Ok(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::String,
                    token: string.string.clone(),
                    span: token_span(&string.string),
                })
            }
            modelica_grammar_trait::Primary::True(bool) => {
                Ok(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::Bool,
                    token: bool.r#true.r#true.clone().into(),
                    span: token_span(&bool.r#true.r#true.clone().into()),
                })
            }
            modelica_grammar_trait::Primary::False(bool) => {
                Ok(rumoca_ir_ast::Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::Bool,
                    token: bool.r#false.r#false.clone().into(),
                    span: token_span(&bool.r#false.r#false.clone().into()),
                })
            }
            modelica_grammar_trait::Primary::End(end) => Ok(rumoca_ir_ast::Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::End,
                token: end.end.end.clone().into(),
                span: token_span(&end.end.end.clone().into()),
            }),
            modelica_grammar_trait::Primary::ArrayPrimary(arr) => {
                match &arr.array_primary.array_primary_opt {
                    Some(opt) => collect_array_elements(&opt.array_arguments),
                    None => Ok(rumoca_ir_ast::Expression::Array {
                        span: Span::DUMMY,
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
                modelica_grammar_trait::FactorListGroup::Circumflex(_) => {
                    rumoca_core::OpBinary::Exp
                }
                modelica_grammar_trait::FactorListGroup::DotCircumflex(_) => {
                    rumoca_core::OpBinary::ExpElem
                }
            };
            let rhs = ast.factor_list[0].primary.clone();
            Ok(rumoca_ir_ast::Expression::Binary {
                op,
                span: binary_span(&ast.primary, &rhs),
                lhs: Arc::new(ast.primary.clone()),
                rhs: Arc::new(rhs),
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
            let rhs = factor.factor.clone();
            let span = binary_span(&lhs, &rhs);
            lhs = rumoca_ir_ast::Expression::Binary {
                span,
                lhs: Arc::new(lhs),
                op: mul_op_to_binary(&factor.mul_operator),
                rhs: Arc::new(rhs),
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
                span: ast.term.span(),
                rhs: Arc::new(ast.term.clone()),
            },
            None => ast.term.clone(),
        };

        // if has term list, process expressions
        for term in &ast.arithmetic_expression_list {
            let rhs = term.term.clone();
            let span = binary_span(&lhs, &rhs);
            lhs = rumoca_ir_ast::Expression::Binary {
                span,
                lhs: Arc::new(lhs),
                op: add_op_to_binary(&term.add_operator),
                rhs: Arc::new(rhs),
            };
        }
        Ok(lhs)
    }
}

impl TryFrom<&modelica_grammar_trait::Relation> for rumoca_ir_ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Relation) -> std::result::Result<Self, Self::Error> {
        match &ast.relation_opt {
            Some(relation) => {
                let rhs = relation.arithmetic_expression.clone();
                Ok(rumoca_ir_ast::Expression::Binary {
                    span: binary_span(&ast.arithmetic_expression, &rhs),
                    lhs: Arc::new(ast.arithmetic_expression.clone()),
                    op: match &relation.relational_operator {
                        modelica_grammar_trait::RelationalOperator::EquEqu(_) => {
                            rumoca_core::OpBinary::Eq
                        }
                        modelica_grammar_trait::RelationalOperator::GT(_) => {
                            rumoca_core::OpBinary::Gt
                        }
                        modelica_grammar_trait::RelationalOperator::LT(_) => {
                            rumoca_core::OpBinary::Lt
                        }
                        modelica_grammar_trait::RelationalOperator::GTEqu(_) => {
                            rumoca_core::OpBinary::Ge
                        }
                        modelica_grammar_trait::RelationalOperator::LTEqu(_) => {
                            rumoca_core::OpBinary::Le
                        }
                        modelica_grammar_trait::RelationalOperator::LTGT(_) => {
                            rumoca_core::OpBinary::Neq
                        }
                    },
                    rhs: Arc::new(rhs),
                })
            }
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
            Some(_) => Ok(rumoca_ir_ast::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                span: ast.relation.span(),
                rhs: Arc::new(ast.relation.clone()),
            }),
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
                let rhs = term.logical_factor.clone();
                let span = binary_span(&lhs, &rhs);
                lhs = rumoca_ir_ast::Expression::Binary {
                    span,
                    lhs: Arc::new(lhs),
                    op: rumoca_core::OpBinary::And,
                    rhs: Arc::new(rhs),
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
                let rhs = term.logical_term.clone();
                let span = binary_span(&lhs, &rhs);
                lhs = rumoca_ir_ast::Expression::Binary {
                    span,
                    lhs: Arc::new(lhs),
                    op: rumoca_core::OpBinary::Or,
                    rhs: Arc::new(rhs),
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
                    span: merge_spans(
                        ast.logical_expression.span(),
                        opt0.logical_expression.span(),
                    ),
                    start: Arc::new(ast.logical_expression.clone()),
                    step: Some(Arc::new(opt.logical_expression.clone())),
                    end: Arc::new(opt0.logical_expression.clone()),
                }),
                None => Ok(rumoca_ir_ast::Expression::Range {
                    span: merge_spans(ast.logical_expression.span(), opt.logical_expression.span()),
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
                    span: merge_spans(
                        branches
                            .first()
                            .map(|(condition, _)| condition.span())
                            .unwrap_or(Span::DUMMY),
                        else_branch.span(),
                    ),
                    branches,
                    else_branch,
                })
            }
        }
    }
}
