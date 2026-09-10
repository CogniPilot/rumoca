//! Finding and rewriting `jacobian` call sites.
//!
//! The surface is an ordinary call spelling, so the grammar is untouched: a
//! parsed program that never writes `jacobian` is bit-identical before and
//! after this pass. Recognition is by name and shape after parsing, and the
//! rewrite replaces the call with a call to the minted wrapper, keeping the
//! original span so later diagnostics still point at what the author wrote.

use rumoca_core::{Location, Span};
use rumoca_ir_ast as ast;

use crate::refusal::{Refusable, Refusal, Rule, Site};

/// The recognized surface spelling.
pub(crate) const SURFACE: &str = "jacobian";

/// A recognized `jacobian(f(a, b), a)` call.
#[derive(Debug, Clone)]
pub(crate) struct CallSite {
    /// Class path from the file's top level to the class holding the call.
    pub(crate) owner: Vec<String>,
    /// Span of the whole `jacobian(…)` call.
    pub(crate) span: Span,
    /// Location of the call, for diagnostics.
    pub(crate) location: Location,
    /// Path parts naming the differentiated function.
    pub(crate) callee: Vec<String>,
    /// Rendered actual arguments of the differentiated call.
    pub(crate) arguments: Vec<String>,
    /// The actual argument expressions as parsed, which is what states the
    /// rank each one carries across the call boundary (JAC-R10).
    pub(crate) actuals: Vec<ast::Expression>,
    /// Rendered text of the argument to differentiate with respect to.
    pub(crate) argument: String,
}

/// Every recognized call site in `definition`, in source order.
///
/// A file that declares a class of its own by this name opts out entirely: a
/// declared name wins over the recognized construct, which is Modelica's own
/// rule and keeps a model that already calls its own `jacobian` compiling.
pub(crate) fn collect(definition: &ast::StoredDefinition, file: &str) -> Refusable<Vec<CallSite>> {
    let mut found = Vec::new();
    if definition.classes.values().any(declares_surface) || definition.classes.contains_key(SURFACE)
    {
        return Ok(found);
    }
    for (name, class) in &definition.classes {
        collect_in_class(class, &mut vec![name.clone()], file, &mut found)?;
    }
    Ok(found)
}

/// Whether a class, or any class nested in it, declares the surface name.
fn declares_surface(class: &ast::ClassDef) -> bool {
    class.classes.contains_key(SURFACE) || class.classes.values().any(declares_surface)
}

fn collect_in_class(
    class: &ast::ClassDef,
    path: &mut Vec<String>,
    file: &str,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    for component in class.components.values() {
        if let Some(binding) = &component.binding {
            collect_in_expression(binding, path, file, found)?;
        }
    }
    for equation in class.equations.iter().chain(&class.initial_equations) {
        collect_in_equation(equation, path, file, found)?;
    }
    for statement in class
        .algorithms
        .iter()
        .chain(&class.initial_algorithms)
        .flatten()
    {
        collect_in_statement(statement, path, file, found)?;
    }
    for (name, nested) in &class.classes {
        path.push(name.clone());
        collect_in_class(nested, path, file, found)?;
        path.pop();
    }
    Ok(())
}

fn collect_in_equation(
    equation: &ast::Equation,
    path: &mut Vec<String>,
    file: &str,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    match equation {
        ast::Equation::Simple { lhs, rhs } => {
            collect_in_expression(lhs, path, file, found)?;
            collect_in_expression(rhs, path, file, found)
        }
        ast::Equation::For { equations, .. } => {
            for nested in equations {
                collect_in_equation(nested, path, file, found)?;
            }
            Ok(())
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_in_expression(&block.cond, path, file, found)?;
                for nested in &block.eqs {
                    collect_in_equation(nested, path, file, found)?;
                }
            }
            Ok(())
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_in_expression(&block.cond, path, file, found)?;
                for nested in &block.eqs {
                    collect_in_equation(nested, path, file, found)?;
                }
            }
            for nested in else_block.iter().flatten() {
                collect_in_equation(nested, path, file, found)?;
            }
            Ok(())
        }
        ast::Equation::FunctionCall { args, .. } => {
            for argument in args {
                collect_in_expression(argument, path, file, found)?;
            }
            Ok(())
        }
        ast::Equation::Empty | ast::Equation::Connect { .. } | ast::Equation::Assert { .. } => {
            Ok(())
        }
    }
}

fn collect_in_statement(
    statement: &ast::Statement,
    path: &mut Vec<String>,
    file: &str,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    match statement {
        ast::Statement::Assignment { value, .. } => collect_in_expression(value, path, file, found),
        ast::Statement::For { equations, .. } => {
            for nested in equations {
                collect_in_statement(nested, path, file, found)?;
            }
            Ok(())
        }
        ast::Statement::While(block) => {
            for nested in &block.stmts {
                collect_in_statement(nested, path, file, found)?;
            }
            Ok(())
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                for nested in &block.stmts {
                    collect_in_statement(nested, path, file, found)?;
                }
            }
            for nested in else_block.iter().flatten() {
                collect_in_statement(nested, path, file, found)?;
            }
            Ok(())
        }
        ast::Statement::When(blocks) => {
            for block in blocks {
                for nested in &block.stmts {
                    collect_in_statement(nested, path, file, found)?;
                }
            }
            Ok(())
        }
        ast::Statement::FunctionCall { args, .. } => {
            for argument in args {
                collect_in_expression(argument, path, file, found)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn collect_in_expression(
    expression: &ast::Expression,
    path: &mut Vec<String>,
    file: &str,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    if let ast::Expression::FunctionCall {
        comp, args, span, ..
    } = expression
        && is_surface_call(comp)
    {
        found.push(read_call_site(path, comp, args, *span, file)?);
        return Ok(());
    }
    for child in children(expression) {
        collect_in_expression(child, path, file, found)?;
    }
    Ok(())
}

/// Whether a call names the surface construct.
pub(crate) fn is_surface_call(comp: &ast::ComponentReference) -> bool {
    matches!(comp.parts.as_slice(), [part] if part.ident.text.as_ref() == SURFACE)
}

fn read_call_site(
    path: &[String],
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    span: Span,
    file: &str,
) -> Refusable<CallSite> {
    let location = comp
        .get_location()
        .cloned()
        .unwrap_or_else(Location::default);
    let site = Site::at(file, Some(&location));
    let [
        ast::Expression::FunctionCall {
            comp: callee,
            args: callee_args,
            is_partial_application: false,
            ..
        },
        differentiated,
    ] = args
    else {
        return Err(Refusal::new(
            Rule::CallForm,
            site,
            "the recognized form is jacobian(f(a, b), a): a call and one of its arguments",
        ));
    };
    let argument = differentiated.to_string();
    let arguments: Vec<String> = callee_args.iter().map(ToString::to_string).collect();
    let matches = arguments.iter().filter(|text| **text == argument).count();
    if matches != 1 {
        return Err(Refusal::new(
            Rule::CallForm,
            site,
            format!(
                "`{argument}` appears {matches} times among the arguments of `{callee}`; it must \
                 appear exactly once"
            ),
        ));
    }
    if !matches!(differentiated, ast::Expression::ComponentReference(_)) {
        return Err(Refusal::new(
            Rule::CallForm,
            site,
            format!("`{argument}` is not a plain argument reference"),
        ));
    }
    Ok(CallSite {
        owner: path.to_vec(),
        span,
        location,
        callee: callee
            .parts
            .iter()
            .map(|part| part.ident.text.to_string())
            .collect(),
        arguments,
        actuals: callee_args.to_vec(),
        argument,
    })
}

/// The immediate subexpressions of `expression`.
pub(crate) fn children(expression: &ast::Expression) -> Vec<&ast::Expression> {
    match expression {
        ast::Expression::Range {
            start, step, end, ..
        } => {
            let mut children = vec![start.as_ref()];
            children.extend(step.as_deref());
            children.push(end.as_ref());
            children
        }
        ast::Expression::Unary { rhs, .. } => vec![rhs.as_ref()],
        ast::Expression::Binary { lhs, rhs, .. } => vec![lhs.as_ref(), rhs.as_ref()],
        ast::Expression::FunctionCall { args, .. }
        | ast::Expression::Array { elements: args, .. }
        | ast::Expression::Tuple { elements: args, .. }
        | ast::Expression::ClassModification {
            modifications: args,
            ..
        } => args.iter().collect(),
        ast::Expression::NamedArgument { value, .. } => vec![value.as_ref()],
        ast::Expression::Modification { value, .. } => vec![value.as_ref()],
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            let mut children: Vec<&ast::Expression> = Vec::new();
            for (condition, value) in branches {
                children.push(condition);
                children.push(value);
            }
            children.push(else_branch.as_ref());
            children
        }
        ast::Expression::Parenthesized { inner, .. } => vec![inner.as_ref()],
        ast::Expression::ArrayComprehension { expr, filter, .. } => {
            let mut children = vec![expr.as_ref()];
            children.extend(filter.as_deref());
            children
        }
        ast::Expression::ArrayIndex { base, .. } => vec![base.as_ref()],
        ast::Expression::FieldAccess { base, .. } => vec![base.as_ref()],
        ast::Expression::Empty { .. }
        | ast::Expression::Terminal { .. }
        | ast::Expression::ComponentReference(_) => Vec::new(),
    }
}
