//! Conversion for expressions.

use super::helpers::{collect_array_elements, loc_info};
use crate::ir;
use crate::modelica_grammar_trait;

//-----------------------------------------------------------------------------
#[derive(Debug, Default, Clone)]

pub struct ArraySubscripts {
    pub subscripts: Vec<ir::ast::Subscript>,
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

impl TryFrom<&modelica_grammar_trait::Subscript> for ir::ast::Subscript {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Subscript) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Subscript::Colon(tok) => Ok(ir::ast::Subscript::Range {
                token: tok.colon.clone(),
            }),
            modelica_grammar_trait::Subscript::Expression(expr) => {
                Ok(ir::ast::Subscript::Expression(expr.expression.clone()))
            }
        }
    }
}

//-----------------------------------------------------------------------------
/// Represents a modification argument with optional `each` and `final` prefixes
#[derive(Debug, Default, Clone)]
pub struct ModificationArg {
    pub expression: ir::ast::Expression,
    /// True if this argument has `each` prefix (for array modifications)
    pub each: bool,
    /// True if this argument has `final` prefix
    pub r#final: bool,
}

#[derive(Debug, Default, Clone)]

pub struct ExpressionList {
    pub args: Vec<ir::ast::Expression>,
    /// Parallel to args - true if the corresponding arg has `each` modifier prefix
    pub each_flags: Vec<bool>,
    /// Parallel to args - true if the corresponding arg has `final` modifier prefix
    pub final_flags: Vec<bool>,
}

/// Convert a NamedArgument to an Expression representing `name = value`
fn named_argument_to_expr(
    named_arg: &modelica_grammar_trait::NamedArgument,
) -> ir::ast::Expression {
    // Create a component reference for the argument name
    let name_expr = ir::ast::Expression::ComponentReference(ir::ast::ComponentReference {
        local: false,
        parts: vec![ir::ast::ComponentRefPart {
            ident: named_arg.ident.clone(),
            subs: None,
        }],
    });

    // Create a binary expression: name = value
    // Use Assign (not Eq) because this is a modification assignment, not equality comparison
    ir::ast::Expression::Binary {
        op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
        lhs: Box::new(name_expr),
        rhs: Box::new(named_arg.function_argument.clone()),
    }
}

/// Collect all named arguments from a NamedArguments structure
fn collect_named_arguments(
    named_args: &modelica_grammar_trait::NamedArguments,
) -> Vec<ir::ast::Expression> {
    let mut args = vec![named_argument_to_expr(&named_args.named_argument)];

    // Recursively collect additional named arguments
    let mut current_opt = &named_args.named_arguments_opt;
    while let Some(opt) = current_opt {
        args.push(named_argument_to_expr(&opt.named_arguments.named_argument));
        current_opt = &opt.named_arguments.named_arguments_opt;
    }

    args
}

impl TryFrom<&modelica_grammar_trait::FunctionArgument> for ir::ast::Expression {
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
                let parts: Vec<ir::ast::ComponentRefPart> = partial_app
                    .type_specifier
                    .name
                    .name
                    .iter()
                    .map(|token| ir::ast::ComponentRefPart {
                        ident: token.clone(),
                        subs: None,
                    })
                    .collect();

                let comp = ir::ast::ComponentReference {
                    local: partial_app.type_specifier.type_specifier_opt.is_some(),
                    parts,
                };

                // Get named arguments if present
                let args = if let Some(opt) = &partial_app.function_partial_application_opt {
                    collect_named_arguments(&opt.named_arguments)
                } else {
                    vec![]
                };

                Ok(ir::ast::Expression::FunctionCall { comp, args })
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
                let mut args = vec![def.expression.clone()];
                if let Some(opt) = &def.function_arguments_opt {
                    match &opt.function_arguments_opt_group {
                        modelica_grammar_trait::FunctionArgumentsOptGroup::CommaFunctionArgumentsNonFirst(
                            expr,
                        ) => {
                            args.append(&mut expr.function_arguments_non_first.args.clone());
                        }
                        modelica_grammar_trait::FunctionArgumentsOptGroup::ForForIndices(
                            for_clause,
                        ) => {
                            // Iterator expression like max(expr for i in 1:10)
                            // Convert to an array comprehension expression
                            use super::helpers::convert_for_indices;
                            let indices = convert_for_indices(&for_clause.for_indices);
                            let comprehension = ir::ast::Expression::ArrayComprehension {
                                expr: Box::new(def.expression.clone()),
                                indices,
                            };
                            return Ok(ExpressionList {
                                args: vec![comprehension],
                                each_flags: vec![false],
                                final_flags: vec![false],
                            });
                        }
                    }
                }
                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                Ok(ExpressionList { args, each_flags, final_flags })
            }
            modelica_grammar_trait::FunctionArguments::FunctionPartialApplicationFunctionArgumentsOpt0(fpa) => {
                // Convert 'function Foo.Bar(arg=val)' to a function call expression
                let partial_app = &fpa.function_partial_application;

                // Convert Name to ComponentReference
                let parts: Vec<ir::ast::ComponentRefPart> = partial_app
                    .type_specifier
                    .name
                    .name
                    .iter()
                    .map(|token| ir::ast::ComponentRefPart {
                        ident: token.clone(),
                        subs: None,
                    })
                    .collect();

                let comp = ir::ast::ComponentReference {
                    local: partial_app.type_specifier.type_specifier_opt.is_some(),
                    parts,
                };

                // Get named arguments if present
                let func_args = if let Some(opt) = &partial_app.function_partial_application_opt {
                    collect_named_arguments(&opt.named_arguments)
                } else {
                    vec![]
                };

                let func_call_expr = ir::ast::Expression::FunctionCall { comp, args: func_args };

                // Start with the partial application as the first arg
                let mut args = vec![func_call_expr];

                // Collect additional arguments if present
                if let Some(opt0) = &fpa.function_arguments_opt0 {
                    args.append(&mut opt0.function_arguments_non_first.args.clone());
                }

                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                Ok(ExpressionList { args, each_flags, final_flags })
            }
            modelica_grammar_trait::FunctionArguments::NamedArguments(named) => {
                let args = collect_named_arguments(&named.named_arguments);
                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                Ok(ExpressionList { args, each_flags, final_flags })
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
                Ok(ExpressionList { args, each_flags, final_flags })
            }
            modelica_grammar_trait::FunctionArgumentsNonFirst::NamedArguments(named) => {
                let args = collect_named_arguments(&named.named_arguments);
                let each_flags = vec![false; args.len()];
                let final_flags = vec![false; args.len()];
                Ok(ExpressionList { args, each_flags, final_flags })
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
        // After grammar change, ast.argument is ModificationArg
        // Extract expressions, each_flags, and final_flags from ModificationArgs
        let mut args = vec![ast.argument.expression.clone()];
        let mut each_flags = vec![ast.argument.each];
        let mut final_flags = vec![ast.argument.r#final];
        for arg in &ast.argument_list_list {
            args.push(arg.argument.expression.clone());
            each_flags.push(arg.argument.each);
            final_flags.push(arg.argument.r#final);
        }
        Ok(ExpressionList {
            args,
            each_flags,
            final_flags,
        })
    }
}

impl TryFrom<&modelica_grammar_trait::Argument> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Argument) -> std::result::Result<Self, Self::Error> {
        match ast {
            modelica_grammar_trait::Argument::ElementModificationOrReplaceable(modif) => {
                match &modif.element_modification_or_replaceable.element_modification_or_replaceable_group {
                    modelica_grammar_trait::ElementModificationOrReplaceableGroup::ElementModification(elem) => {
                        let _name_loc = elem
                            .element_modification
                            .name
                            .name
                            .first()
                            .map(loc_info)
                            .unwrap_or_default();
                        match &elem.element_modification.element_modification_opt {
                            Some(opt) => {
                                match &opt.modification {
                                    modelica_grammar_trait::Modification::ClassModificationModificationOpt(class_modif) => {
                                        // Handle class modification like: name(submod=value) or name(a, b)
                                        // Represent as a function call expression

                                        // Build the function name from the element name
                                        let name = &elem.element_modification.name;
                                        let parts: Vec<ir::ast::ComponentRefPart> = name.name.iter().map(|token| {
                                            ir::ast::ComponentRefPart {
                                                ident: token.clone(),
                                                subs: None,
                                            }
                                        }).collect();
                                        let func_ref = ir::ast::ComponentReference {
                                            local: false,
                                            parts,
                                        };

                                        // Get the arguments from the class modification
                                        let args = if let Some(opt) = &class_modif.class_modification.class_modification_opt {
                                            opt.argument_list.args.clone()
                                        } else {
                                            vec![]
                                        };

                                        // Create the function call expression
                                        let call_expr = ir::ast::Expression::FunctionCall {
                                            comp: func_ref,
                                            args,
                                        };

                                        // If there's also a value assignment (name(mods) = value), wrap in binary
                                        if let Some(mod_opt) = &class_modif.modification_opt {
                                            match &mod_opt.modification_expression {
                                                modelica_grammar_trait::ModificationExpression::Expression(expr) => {
                                                    Ok(ir::ast::Expression::Binary {
                                                        op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                                                        lhs: Box::new(call_expr),
                                                        rhs: Box::new(expr.expression.clone()),
                                                    })
                                                }
                                                modelica_grammar_trait::ModificationExpression::Break(_) => {
                                                    // 'break' means remove inherited binding - return just the call without assignment
                                                    Ok(call_expr)
                                                }
                                            }
                                        } else {
                                            Ok(call_expr)
                                        }
                                    }
                                    modelica_grammar_trait::Modification::EquModificationExpression(modif) => {
                                        match &modif.modification_expression {
                                            modelica_grammar_trait::ModificationExpression::Break(_) => {
                                                // 'break' means remove inherited binding - return Empty to skip this modification
                                                Ok(ir::ast::Expression::Empty)
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
                                                    op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
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
                let redecl = &redcl.element_redeclaration;
                match &redecl.element_redeclaration_group {
                    modelica_grammar_trait::ElementRedeclarationGroup::ShortClassDefinition(
                        short_def,
                    ) => {
                        // Handle short class definition redeclaration
                        // e.g., redeclare Modelica.Blocks.Sources.Step signalSource(final height=I)
                        match &short_def.short_class_definition.short_class_specifier {
                            modelica_grammar_trait::ShortClassSpecifier::TypeClassSpecifier(
                                type_spec,
                            ) => {
                                // Build component reference from the declared name (what's being redeclared)
                                // e.g., for "redeclare package Medium = SomePackage", this is "Medium"
                                let name_ref = ir::ast::ComponentReference {
                                    local: false,
                                    parts: vec![ir::ast::ComponentRefPart {
                                        ident: type_spec.type_class_specifier.ident.clone(),
                                        subs: None,
                                    }],
                                };

                                // Build component reference from the type_specifier (the new type)
                                // e.g., for "redeclare package Medium = SomePackage", this is "SomePackage"
                                let new_type_ref = ir::ast::ComponentReference {
                                    local: false,
                                    parts: type_spec
                                        .type_class_specifier
                                        .type_specifier
                                        .name
                                        .name
                                        .iter()
                                        .map(|tok| ir::ast::ComponentRefPart {
                                            ident: tok.clone(),
                                            subs: None,
                                        })
                                        .collect(),
                                };

                                // Get class modification arguments if present
                                let args = if let Some(class_mod) =
                                    &type_spec.type_class_specifier.type_class_specifier_opt0
                                {
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

                                // Create Binary expression: name = NewType(args)
                                // This stores the redeclaration as a proper key-value pair
                                Ok(ir::ast::Expression::Binary {
                                    op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                                    lhs: Box::new(ir::ast::Expression::ComponentReference(
                                        name_ref,
                                    )),
                                    rhs: Box::new(ir::ast::Expression::FunctionCall {
                                        comp: new_type_ref,
                                        args,
                                    }),
                                })
                            }
                            modelica_grammar_trait::ShortClassSpecifier::EnumClassSpecifier(
                                enum_spec,
                            ) => {
                                // Handle enum redeclaration
                                let name_ref = ir::ast::ComponentReference {
                                    local: false,
                                    parts: vec![ir::ast::ComponentRefPart {
                                        ident: enum_spec.enum_class_specifier.ident.clone(),
                                        subs: None,
                                    }],
                                };
                                Ok(ir::ast::Expression::FunctionCall {
                                    comp: name_ref,
                                    args: vec![],
                                })
                            }
                        }
                    }
                    modelica_grammar_trait::ElementRedeclarationGroup::ComponentClause1(
                        comp_clause,
                    ) => {
                        // Handle component clause redeclaration
                        // e.g., redeclare NewType componentName(modifications)
                        // e.g., extends VoltageSource(redeclare Modelica.Blocks.Sources.Sine signalSource(f=f))
                        let decl = &comp_clause
                            .component_clause1
                            .component_declaration1
                            .declaration;

                        // Check if trying to redeclare a builtin attribute
                        // Builtin attributes (start, fixed, etc.) cannot be redeclared
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
                        if ALL_BUILTIN_ATTRS.contains(&redecl_name.as_str()) {
                            anyhow::bail!(
                                "Invalid redeclaration of {}, attributes of basic types may not be redeclared{}",
                                redecl_name,
                                loc_info(&decl.ident)
                            );
                        }

                        // Build component reference for the component name being redeclared
                        let name_ref = ir::ast::ComponentReference {
                            local: false,
                            parts: vec![ir::ast::ComponentRefPart {
                                ident: decl.ident.clone(),
                                subs: None,
                            }],
                        };

                        // Build component reference for the new type from type_specifier
                        let new_type_ref = ir::ast::ComponentReference {
                            local: false,
                            parts: comp_clause
                                .component_clause1
                                .type_specifier
                                .name
                                .name
                                .iter()
                                .map(|tok| ir::ast::ComponentRefPart {
                                    ident: tok.clone(),
                                    subs: None,
                                })
                                .collect(),
                        };

                        // Get modification arguments if present
                        let args = if let Some(modif) = &decl.declaration_opt0 {
                            match &modif.modification {
                                modelica_grammar_trait::Modification::EquModificationExpression(
                                    eq_mod,
                                ) => {
                                    // For simple value modifications like = expr
                                    // This is unusual for type redeclaration, but handle it
                                    match &eq_mod.modification_expression {
                                        modelica_grammar_trait::ModificationExpression::Expression(
                                            expr,
                                        ) => {
                                            // Return as Binary expression for simple value assignment
                                            return Ok(ir::ast::Expression::Binary {
                                                op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                                                lhs: Box::new(ir::ast::Expression::ComponentReference(
                                                    name_ref,
                                                )),
                                                rhs: Box::new(expr.expression.clone()),
                                            });
                                        }
                                        modelica_grammar_trait::ModificationExpression::Break(_) => {
                                            vec![]
                                        }
                                    }
                                }
                                modelica_grammar_trait::Modification::ClassModificationModificationOpt(
                                    class_mod,
                                ) => {
                                    // Get args from class modification (e.g., (f=f, amplitude=V))
                                    if let Some(arg_list) =
                                        &class_mod.class_modification.class_modification_opt
                                    {
                                        arg_list.argument_list.args.clone()
                                    } else {
                                        vec![]
                                    }
                                }
                            }
                        } else {
                            vec![]
                        };

                        // Create Binary expression: componentName = NewType(args)
                        Ok(ir::ast::Expression::Binary {
                            op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                            lhs: Box::new(ir::ast::Expression::ComponentReference(name_ref)),
                            rhs: Box::new(ir::ast::Expression::FunctionCall {
                                comp: new_type_ref,
                                args,
                            }),
                        })
                    }
                    modelica_grammar_trait::ElementRedeclarationGroup::ElementReplaceable(repl) => {
                        // Handle 'redeclare replaceable ...'
                        // This is like the other cases but wrapped in element_replaceable
                        match &repl.element_replaceable.element_replaceable_group {
                            modelica_grammar_trait::ElementReplaceableGroup::ShortClassDefinition(
                                short_def,
                            ) => {
                                // Handle short class definition: redeclare replaceable package Medium = ...
                                match &short_def.short_class_definition.short_class_specifier {
                                    modelica_grammar_trait::ShortClassSpecifier::TypeClassSpecifier(
                                        type_spec,
                                    ) => {
                                        // Build component reference from the declared name (what's being redeclared)
                                        let name_ref = ir::ast::ComponentReference {
                                            local: false,
                                            parts: vec![ir::ast::ComponentRefPart {
                                                ident: type_spec.type_class_specifier.ident.clone(),
                                                subs: None,
                                            }],
                                        };

                                        // Build component reference from the type_specifier (the new type)
                                        let new_type_ref = ir::ast::ComponentReference {
                                            local: false,
                                            parts: type_spec
                                                .type_class_specifier
                                                .type_specifier
                                                .name
                                                .name
                                                .iter()
                                                .map(|tok| ir::ast::ComponentRefPart {
                                                    ident: tok.clone(),
                                                    subs: None,
                                                })
                                                .collect(),
                                        };

                                        let args = if let Some(class_mod) =
                                            &type_spec.type_class_specifier.type_class_specifier_opt0
                                        {
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

                                        // Create Binary expression: name = NewType(args)
                                        Ok(ir::ast::Expression::Binary {
                                            op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                                            lhs: Box::new(ir::ast::Expression::ComponentReference(
                                                name_ref,
                                            )),
                                            rhs: Box::new(ir::ast::Expression::FunctionCall {
                                                comp: new_type_ref,
                                                args,
                                            }),
                                        })
                                    }
                                    modelica_grammar_trait::ShortClassSpecifier::EnumClassSpecifier(
                                        enum_spec,
                                    ) => {
                                        let name_ref = ir::ast::ComponentReference {
                                            local: false,
                                            parts: vec![ir::ast::ComponentRefPart {
                                                ident: enum_spec.enum_class_specifier.ident.clone(),
                                                subs: None,
                                            }],
                                        };
                                        Ok(ir::ast::Expression::FunctionCall {
                                            comp: name_ref,
                                            args: vec![],
                                        })
                                    }
                                }
                            }
                            modelica_grammar_trait::ElementReplaceableGroup::ComponentClause1(
                                comp_clause,
                            ) => {
                                // Handle component clause: redeclare replaceable NewType componentName(...)
                                let decl =
                                    &comp_clause.component_clause1.component_declaration1.declaration;

                                // Build component reference for the component name being redeclared
                                let name_ref = ir::ast::ComponentReference {
                                    local: false,
                                    parts: vec![ir::ast::ComponentRefPart {
                                        ident: decl.ident.clone(),
                                        subs: None,
                                    }],
                                };

                                // Build component reference for the new type from type_specifier
                                let new_type_ref = ir::ast::ComponentReference {
                                    local: false,
                                    parts: comp_clause
                                        .component_clause1
                                        .type_specifier
                                        .name
                                        .name
                                        .iter()
                                        .map(|tok| ir::ast::ComponentRefPart {
                                            ident: tok.clone(),
                                            subs: None,
                                        })
                                        .collect(),
                                };

                                // Get modification arguments if present
                                let args = if let Some(modif) = &decl.declaration_opt0 {
                                    match &modif.modification {
                                        modelica_grammar_trait::Modification::EquModificationExpression(
                                            eq_mod,
                                        ) => match &eq_mod.modification_expression {
                                            modelica_grammar_trait::ModificationExpression::Expression(
                                                expr,
                                            ) => {
                                                // Return as Binary expression for simple value assignment
                                                return Ok(ir::ast::Expression::Binary {
                                                    op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                                                    lhs: Box::new(ir::ast::Expression::ComponentReference(
                                                        name_ref,
                                                    )),
                                                    rhs: Box::new(expr.expression.clone()),
                                                });
                                            }
                                            modelica_grammar_trait::ModificationExpression::Break(_) => {
                                                vec![]
                                            }
                                        },
                                        modelica_grammar_trait::Modification::ClassModificationModificationOpt(
                                            class_mod,
                                        ) => {
                                            if let Some(arg_list) =
                                                &class_mod.class_modification.class_modification_opt
                                            {
                                                arg_list.argument_list.args.clone()
                                            } else {
                                                vec![]
                                            }
                                        }
                                    }
                                } else {
                                    vec![]
                                };

                                // Create Binary expression: componentName = NewType(args)
                                Ok(ir::ast::Expression::Binary {
                                    op: ir::ast::OpBinary::Assign(ir::ast::Token::default()),
                                    lhs: Box::new(ir::ast::Expression::ComponentReference(name_ref)),
                                    rhs: Box::new(ir::ast::Expression::FunctionCall {
                                        comp: new_type_ref,
                                        args,
                                    }),
                                })
                            }
                        }
                    }
                }
            }
        }
    }
}

impl TryFrom<&modelica_grammar_trait::Argument> for ModificationArg {
    type Error = anyhow::Error;

    fn try_from(ast: &modelica_grammar_trait::Argument) -> std::result::Result<Self, Self::Error> {
        // Extract the `each` and `final` flags from the argument structure
        let (each, r#final) = match ast {
            modelica_grammar_trait::Argument::ElementModificationOrReplaceable(modif) => {
                let emor = &modif.element_modification_or_replaceable;
                let each = emor.element_modification_or_replaceable_opt.is_some();
                let r#final = emor.element_modification_or_replaceable_opt0.is_some();
                (each, r#final)
            }
            modelica_grammar_trait::Argument::ElementRedeclaration(_) => {
                // Redeclarations don't have each/final in the same way
                (false, false)
            }
        };

        // Use the existing conversion to get the expression
        let expression: ir::ast::Expression = ast.try_into()?;

        Ok(ModificationArg {
            expression,
            each,
            r#final,
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
        Ok(ExpressionList {
            args: v,
            each_flags,
            final_flags,
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
            Ok(ExpressionList {
                args,
                each_flags,
                final_flags,
            })
        } else {
            Ok(ExpressionList {
                args: vec![],
                each_flags: vec![],
                final_flags: vec![],
            })
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
                        comp: comp.component_primary.component_reference.clone(),
                        args: args.function_call_args.args.clone(),
                    }),
                    None => Ok(ir::ast::Expression::ComponentReference(
                        comp.component_primary.component_reference.clone(),
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
                collect_array_elements(&arr.array_primary.array_arguments)
            }
            modelica_grammar_trait::Primary::RangePrimary(range) => {
                // range_primary is: '[' expression_list { ';' expression_list } ']'
                // This creates matrix literals like [1, 2; 3, 4]
                let rp = &range.range_primary;

                // Helper to convert ExpressionList to Vec<Expression>
                fn expr_list_to_vec(
                    el: &modelica_grammar_trait::ExpressionList,
                ) -> Vec<ir::ast::Expression> {
                    let mut elements = vec![el.expression.clone()];
                    for item in &el.expression_list_list {
                        elements.push(item.expression.clone());
                    }
                    elements
                }

                // First row
                let first_row = expr_list_to_vec(&rp.expression_list);

                // Check if there are additional rows (semicolons)
                if rp.range_primary_list.is_empty() {
                    // Single row: [1, 2, 3] - just return as matrix Array
                    Ok(ir::ast::Expression::Array {
                        elements: first_row,
                        is_matrix: true,
                    })
                } else {
                    // Multiple rows: [1, 2; 3, 4] - create array of row arrays
                    let mut rows = vec![ir::ast::Expression::Array {
                        elements: first_row,
                        is_matrix: true,
                    }];
                    for row_item in &rp.range_primary_list {
                        let row = expr_list_to_vec(&row_item.expression_list);
                        rows.push(ir::ast::Expression::Array {
                            elements: row,
                            is_matrix: true,
                        });
                    }
                    Ok(ir::ast::Expression::Array {
                        elements: rows,
                        is_matrix: true,
                    })
                }
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
                    // Single expression in parentheses - preserve with Parenthesized wrapper
                    Ok(ir::ast::Expression::Parenthesized {
                        inner: Box::new(primary.output_expression_list.args[0].clone()),
                    })
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
            Ok(ast.primary.clone())
        } else {
            Ok(ir::ast::Expression::Binary {
                op: ir::ast::OpBinary::Exp(ir::ast::Token::default()),
                lhs: Box::new(ast.primary.clone()),
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
                rhs: Box::new(ast.term.clone()),
            },
            None => ast.term.clone(),
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
                lhs: Box::new(ast.arithmetic_expression.clone()),
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
            None => Ok(ast.arithmetic_expression.clone()),
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
                    rhs: Box::new(ast.relation.clone()),
                })
            }
            None => Ok(ast.relation.clone()),
        }
    }
}

impl TryFrom<&modelica_grammar_trait::LogicalTerm> for ir::ast::Expression {
    type Error = anyhow::Error;

    fn try_from(
        ast: &modelica_grammar_trait::LogicalTerm,
    ) -> std::result::Result<Self, Self::Error> {
        if ast.logical_term_list.is_empty() {
            Ok(ast.logical_factor.clone())
        } else {
            let mut lhs = ast.logical_factor.clone();
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
            Ok(ast.logical_term.clone())
        } else {
            let mut lhs = ast.logical_term.clone();
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
                let else_branch = Box::new(if_expr.expression1.clone());

                Ok(ir::ast::Expression::If {
                    branches,
                    else_branch,
                })
            }
        }
    }
}
