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
    let top_level_lookup = if definition.classes.contains_key(SURFACE) {
        SurfaceLookup::Shadowed
    } else {
        SurfaceLookup::Recognize
    };
    for (name, class) in &definition.classes {
        collect_in_class(
            class,
            &mut vec![name.clone()],
            file,
            top_level_lookup,
            &mut found,
        )?;
    }
    Ok(found)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SurfaceLookup {
    Recognize,
    Shadowed,
    InheritedUnknown,
}

impl SurfaceLookup {
    fn enter(self, class: &ast::ClassDef) -> Self {
        if scope_binds_surface(class) {
            return Self::Shadowed;
        }
        match self {
            Self::Shadowed => Self::Shadowed,
            Self::InheritedUnknown => Self::InheritedUnknown,
            Self::Recognize if !class.extends.is_empty() => Self::InheritedUnknown,
            Self::Recognize => Self::Recognize,
        }
    }
}

/// Whether this exact lexical scope binds the recognized spelling.
fn scope_binds_surface(class: &ast::ClassDef) -> bool {
    class.classes.contains_key(SURFACE)
        || class.components.contains_key(SURFACE)
        || class.imports.iter().any(import_binds_surface)
}

fn import_binds_surface(import: &ast::Import) -> bool {
    match import {
        ast::Import::Qualified { path, .. } => path
            .name
            .last()
            .is_some_and(|part| part.text.as_ref() == SURFACE),
        ast::Import::Renamed { alias, .. } => alias.text.as_ref() == SURFACE,
        ast::Import::Selective { names, .. } => {
            names.iter().any(|name| name.text.as_ref() == SURFACE)
        }
        // A wildcard's introduced names are unknowable before lookup. Refuse
        // recognition rather than stealing a possibly imported ordinary call.
        ast::Import::Unqualified { .. } => true,
    }
}

fn recognizes_surface_call(
    lookup: SurfaceLookup,
    component: &ast::ComponentReference,
    file: &str,
) -> Refusable<bool> {
    if !is_surface_call(component) {
        return Ok(false);
    }
    match lookup {
        SurfaceLookup::Recognize => Ok(true),
        SurfaceLookup::Shadowed => Ok(false),
        SurfaceLookup::InheritedUnknown => Err(Refusal::new(
            Rule::CalleeLookup,
            Site::at(file, component.get_location()),
            "cannot decide before resolution whether an extends clause supplies `jacobian`; synthesis refuses rather than stealing an inherited call",
        )),
    }
}

fn collect_in_class(
    class: &ast::ClassDef,
    path: &mut Vec<String>,
    file: &str,
    inherited_lookup: SurfaceLookup,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    // Recovery equations/statements do not carry their own source location.
    // Keep the nearest honest owner as the refusal site instead of inventing
    // one or allowing the recovery node to disappear during expansion.
    let recovery_owner = Site::at(file, Some(&class.location));
    let recognize_surface = inherited_lookup.enter(class);
    for subscript in &class.array_subscripts {
        collect_in_declaration_subscript(
            subscript,
            path,
            recognize_surface,
            &recovery_owner,
            found,
        )?;
    }
    for component in class.components.values() {
        collect_in_component(
            component,
            path,
            file,
            recognize_surface,
            &recovery_owner,
            found,
        )?;
    }
    for equation in class.equations.iter().chain(&class.initial_equations) {
        if !matches!(equation, ast::Equation::Empty)
            && let Some(violation) = ast::equation_required_value_violation(equation)
        {
            return Err(value_expression_refusal(
                file,
                &recovery_owner,
                violation,
                "equation",
            ));
        }
        collect_in_equation(
            equation,
            path,
            file,
            recognize_surface,
            &recovery_owner,
            found,
        )?;
    }
    for statement in class
        .algorithms
        .iter()
        .chain(&class.initial_algorithms)
        .flatten()
    {
        if !matches!(statement, ast::Statement::Empty)
            && let Some(violation) = ast::statement_required_value_violation(statement)
        {
            return Err(value_expression_refusal(
                file,
                &recovery_owner,
                violation,
                "algorithm statement",
            ));
        }
        collect_in_statement(
            statement,
            path,
            file,
            recognize_surface,
            &recovery_owner,
            found,
        )?;
    }
    for extend in &class.extends {
        for modification in &extend.modifications {
            reject_modifier_violation(&modification.expr, &recovery_owner, "extends modification")?;
            collect_in_expression(&modification.expr, path, file, recognize_surface, found)?;
        }
        for annotation in &extend.annotation {
            reject_modifier_violation(annotation, &recovery_owner, "extends annotation")?;
            collect_in_expression(annotation, path, file, recognize_surface, found)?;
        }
    }
    for annotation in &class.annotation {
        reject_modifier_violation(annotation, &recovery_owner, "class annotation")?;
        collect_in_expression(annotation, path, file, recognize_surface, found)?;
    }
    if let Some(external) = &class.external {
        if let Some(output) = &external.output {
            collect_in_reference(output, path, file, recognize_surface, found)?;
        }
        for argument in &external.args {
            reject_expression_violation(argument, &recovery_owner, "external argument")?;
            collect_in_expression(argument, path, file, recognize_surface, found)?;
        }
        for annotation in &external.annotation {
            reject_modifier_violation(annotation, &recovery_owner, "external annotation")?;
            collect_in_expression(annotation, path, file, recognize_surface, found)?;
        }
    }
    for (name, nested) in &class.classes {
        path.push(name.clone());
        collect_in_class(nested, path, file, recognize_surface, found)?;
        path.pop();
    }
    Ok(())
}

fn collect_in_component(
    component: &ast::Component,
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    if component.has_explicit_binding != component.binding.is_some() {
        return Err(Refusal::new(
            Rule::ExpressionForm,
            Site::at(file, Some(&component.location)),
            format!(
                "component `{}` has an inconsistent explicit-binding marker and payload",
                component.name
            ),
        ));
    }
    for subscript in &component.shape_expr {
        collect_in_declaration_subscript(
            subscript,
            path,
            recognize_surface,
            recovery_owner,
            found,
        )?;
    }
    if !matches!(component.start, ast::Expression::Empty { .. }) {
        reject_expression_violation(&component.start, recovery_owner, "component start")?;
        collect_in_expression(&component.start, path, file, recognize_surface, found)?;
    }
    if let Some(binding) = &component.binding {
        reject_expression_violation(binding, recovery_owner, "component binding")?;
        collect_in_expression(binding, path, file, recognize_surface, found)?;
    }
    let modifications: Vec<&ast::Expression> = if component.source_modifications.is_empty() {
        component.modifications.values().collect()
    } else {
        component.source_modifications.iter().collect()
    };
    for modification in modifications {
        reject_modifier_violation(modification, recovery_owner, "component modification")?;
        collect_in_expression(modification, path, file, recognize_surface, found)?;
    }
    if let Some(condition) = &component.condition {
        reject_expression_violation(condition, recovery_owner, "component condition")?;
        collect_in_expression(condition, path, file, recognize_surface, found)?;
    }
    for annotation in &component.annotation {
        reject_modifier_violation(annotation, recovery_owner, "component annotation")?;
        collect_in_expression(annotation, path, file, recognize_surface, found)?;
    }
    Ok(())
}

fn reject_expression_violation(
    expression: &ast::Expression,
    recovery_owner: &Site,
    context: &str,
) -> Refusable<()> {
    if let Some(violation) = ast::expression_required_value_violation(expression) {
        return Err(value_expression_refusal(
            &recovery_owner.file,
            recovery_owner,
            violation,
            context,
        ));
    }
    Ok(())
}

fn reject_modifier_violation(
    expression: &ast::Expression,
    recovery_owner: &Site,
    context: &str,
) -> Refusable<()> {
    if let Some(violation) = ast::modifier_required_value_violation(expression) {
        return Err(value_expression_refusal(
            &recovery_owner.file,
            recovery_owner,
            violation,
            context,
        ));
    }
    Ok(())
}

fn value_expression_refusal(
    file: &str,
    recovery_owner: &Site,
    violation: ast::RequiredValueViolation,
    context: &str,
) -> Refusal {
    let site = violation.span.map_or_else(
        || recovery_owner.clone(),
        |span| Site {
            line: recovery_owner.line,
            column: recovery_owner.column,
            ..Site::with_span(file, None, span)
        },
    );
    Refusal::new(
        Rule::ExpressionForm,
        site,
        format!("{context} contains {}", violation.kind.description()),
    )
}

fn collect_in_equation(
    equation: &ast::Equation,
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    match equation {
        ast::Equation::Simple { lhs, rhs } => {
            collect_in_expression(lhs, path, file, recognize_surface, found)?;
            collect_in_expression(rhs, path, file, recognize_surface, found)
        }
        ast::Equation::For { indices, equations } => {
            for index in indices {
                collect_in_expression(&index.range, path, file, recognize_surface, found)?;
            }
            for nested in equations {
                collect_in_equation(nested, path, file, recognize_surface, recovery_owner, found)?;
            }
            Ok(())
        }
        ast::Equation::When(blocks) => {
            for block in blocks {
                collect_in_expression(&block.cond, path, file, recognize_surface, found)?;
                for nested in &block.eqs {
                    collect_in_equation(
                        nested,
                        path,
                        file,
                        recognize_surface,
                        recovery_owner,
                        found,
                    )?;
                }
            }
            Ok(())
        }
        ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_in_expression(&block.cond, path, file, recognize_surface, found)?;
                for nested in &block.eqs {
                    collect_in_equation(
                        nested,
                        path,
                        file,
                        recognize_surface,
                        recovery_owner,
                        found,
                    )?;
                }
            }
            for nested in else_block.iter().flatten() {
                collect_in_equation(nested, path, file, recognize_surface, recovery_owner, found)?;
            }
            Ok(())
        }
        ast::Equation::FunctionCall { comp, args, .. } => {
            if recognizes_surface_call(recognize_surface, comp, file)? {
                return Err(Refusal::new(
                    Rule::CallForm,
                    Site::at(file, comp.get_location()),
                    "jacobian surface syntax is value-producing and cannot be used as a standalone equation call",
                ));
            }
            collect_in_reference(comp, path, file, recognize_surface, found)?;
            for argument in args {
                collect_in_expression(argument, path, file, recognize_surface, found)?;
            }
            Ok(())
        }
        ast::Equation::Empty => Err(Refusal::new(
            Rule::StatementForm,
            recovery_owner.clone(),
            "an empty equation node is parser recovery; an empty equation section is an empty list",
        )),
        ast::Equation::Connect { lhs, rhs } => {
            collect_in_reference(lhs, path, file, recognize_surface, found)?;
            collect_in_reference(rhs, path, file, recognize_surface, found)
        }
        ast::Equation::Assert {
            condition,
            message,
            level,
        } => {
            collect_in_expression(condition, path, file, recognize_surface, found)?;
            collect_in_expression(message, path, file, recognize_surface, found)?;
            if let Some(level) = level {
                collect_in_expression(level, path, file, recognize_surface, found)?;
            }
            Ok(())
        }
    }
}

fn collect_in_statement(
    statement: &ast::Statement,
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    match statement {
        ast::Statement::Assignment { comp, value } => {
            collect_in_reference(comp, path, file, recognize_surface, found)?;
            collect_in_expression(value, path, file, recognize_surface, found)
        }
        ast::Statement::For { indices, equations } => {
            for index in indices {
                collect_in_expression(&index.range, path, file, recognize_surface, found)?;
            }
            collect_in_statements(
                equations,
                path,
                file,
                recognize_surface,
                recovery_owner,
                found,
            )
        }
        ast::Statement::While(block) => {
            collect_in_expression(&block.cond, path, file, recognize_surface, found)?;
            collect_in_statements(
                &block.stmts,
                path,
                file,
                recognize_surface,
                recovery_owner,
                found,
            )
        }
        ast::Statement::If {
            cond_blocks,
            else_block,
        } => collect_in_statement_blocks(
            cond_blocks,
            else_block.as_deref(),
            path,
            file,
            recognize_surface,
            recovery_owner,
            found,
        ),
        ast::Statement::When(blocks) => collect_in_statement_blocks(
            blocks,
            None,
            path,
            file,
            recognize_surface,
            recovery_owner,
            found,
        ),
        ast::Statement::Empty => Err(Refusal::new(
            Rule::StatementForm,
            recovery_owner.clone(),
            "an empty statement node is parser recovery; an empty algorithm is an empty list",
        )),
        ast::Statement::FunctionCall {
            comp,
            args,
            outputs,
        } => {
            if recognizes_surface_call(recognize_surface, comp, file)? {
                return Err(Refusal::new(
                    Rule::CallForm,
                    Site::at(file, comp.get_location()),
                    "jacobian surface syntax must occur in a value expression, not a call statement",
                ));
            }
            collect_in_reference(comp, path, file, recognize_surface, found)?;
            for argument in args {
                collect_in_expression(argument, path, file, recognize_surface, found)?;
            }
            for output in outputs {
                if !matches!(output, ast::Expression::Empty { .. }) {
                    collect_in_expression(output, path, file, recognize_surface, found)?;
                }
            }
            Ok(())
        }
        ast::Statement::Reinit { variable, value } => {
            collect_in_reference(variable, path, file, recognize_surface, found)?;
            collect_in_expression(value, path, file, recognize_surface, found)
        }
        ast::Statement::Assert {
            condition,
            message,
            level,
        } => {
            collect_in_expression(condition, path, file, recognize_surface, found)?;
            collect_in_expression(message, path, file, recognize_surface, found)?;
            if let Some(level) = level {
                collect_in_expression(level, path, file, recognize_surface, found)?;
            }
            Ok(())
        }
        ast::Statement::Return { .. } | ast::Statement::Break { .. } => Ok(()),
    }
}

fn collect_in_statement_blocks(
    blocks: &[ast::StatementBlock],
    else_block: Option<&[ast::Statement]>,
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    for block in blocks {
        collect_in_expression(&block.cond, path, file, recognize_surface, found)?;
        collect_in_statements(
            &block.stmts,
            path,
            file,
            recognize_surface,
            recovery_owner,
            found,
        )?;
    }
    if let Some(else_block) = else_block {
        collect_in_statements(
            else_block,
            path,
            file,
            recognize_surface,
            recovery_owner,
            found,
        )?;
    }
    Ok(())
}

fn collect_in_statements(
    statements: &[ast::Statement],
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    for statement in statements {
        collect_in_statement(
            statement,
            path,
            file,
            recognize_surface,
            recovery_owner,
            found,
        )?;
    }
    Ok(())
}

fn collect_in_expression(
    expression: &ast::Expression,
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    if let ast::Expression::FunctionCall {
        comp,
        args,
        is_partial_application: false,
        span,
    } = expression
        && recognizes_surface_call(recognize_surface, comp, file)?
    {
        if args.iter().any(|argument| {
            ast::contains_function_call(argument, |nested, _| is_surface_call(nested))
        }) {
            return Err(Refusal::new(
                Rule::CallForm,
                Site::at(file, comp.get_location()),
                "a jacobian surface call may not contain another jacobian surface call; nested rewrites would overlap",
            ));
        }
        found.push(read_call_site(path, comp, args, *span, file)?);
        return Ok(());
    }
    for child in children(expression) {
        collect_in_expression(child, path, file, recognize_surface, found)?;
    }
    Ok(())
}

fn collect_in_reference(
    reference: &ast::ComponentReference,
    path: &mut Vec<String>,
    file: &str,
    recognize_surface: SurfaceLookup,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    let owner = Site::at(file, reference.get_location());
    for part in &reference.parts {
        for subscript in part.subs.iter().flatten() {
            collect_in_subscript(subscript, path, recognize_surface, &owner, found)?;
        }
    }
    Ok(())
}

fn collect_in_subscript(
    subscript: &ast::Subscript,
    path: &mut Vec<String>,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    collect_in_validated_subscript(
        subscript,
        path,
        recognize_surface,
        recovery_owner,
        found,
        ast::subscript_required_value_violation(subscript),
    )
}

fn collect_in_declaration_subscript(
    subscript: &ast::Subscript,
    path: &mut Vec<String>,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
) -> Refusable<()> {
    collect_in_validated_subscript(
        subscript,
        path,
        recognize_surface,
        recovery_owner,
        found,
        ast::declaration_subscript_required_value_violation(subscript),
    )
}

fn collect_in_validated_subscript(
    subscript: &ast::Subscript,
    path: &mut Vec<String>,
    recognize_surface: SurfaceLookup,
    recovery_owner: &Site,
    found: &mut Vec<CallSite>,
    violation: Option<ast::RequiredValueViolation>,
) -> Refusable<()> {
    if let Some(violation) = violation {
        return Err(value_expression_refusal(
            &recovery_owner.file,
            recovery_owner,
            violation,
            "array subscript",
        ));
    }
    match subscript {
        ast::Subscript::Empty => unreachable!("recovery subscript rejected above"),
        ast::Subscript::Range { .. } => Ok(()),
        ast::Subscript::Expression(expression) => collect_in_expression(
            expression,
            path,
            &recovery_owner.file,
            recognize_surface,
            found,
        ),
    }
}

/// Whether a call names the surface construct.
pub(crate) fn is_surface_call(comp: &ast::ComponentReference) -> bool {
    matches!(comp.parts.as_slice(), [part]
        if part.ident.text.as_ref() == SURFACE
            && part.subs.as_ref().is_none_or(Vec::is_empty))
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
    fn append_reference_subscripts<'a>(
        reference: &'a ast::ComponentReference,
        children: &mut Vec<&'a ast::Expression>,
    ) {
        for subscript in reference
            .parts
            .iter()
            .flat_map(|part| part.subs.iter().flatten())
        {
            if let ast::Subscript::Expression(expression) = subscript {
                children.push(expression);
            }
        }
    }

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
        ast::Expression::FunctionCall { comp, args, .. } => {
            let mut children = Vec::new();
            append_reference_subscripts(comp, &mut children);
            children.extend(args.iter());
            children
        }
        ast::Expression::DerivativeCall { args, .. } => args.iter().collect(),
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
            elements.iter().collect()
        }
        ast::Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            let mut children = Vec::new();
            append_reference_subscripts(target, &mut children);
            children.extend(modifications.iter());
            children
        }
        ast::Expression::NamedArgument { value, .. } => vec![value.as_ref()],
        ast::Expression::Modification { target, value, .. } => {
            let mut children = Vec::new();
            append_reference_subscripts(target, &mut children);
            children.extend(value.as_deref());
            children
        }
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
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            let mut children: Vec<&ast::Expression> =
                indices.iter().map(|index| &index.range).collect();
            children.push(expr.as_ref());
            children.extend(filter.as_deref());
            children
        }
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            let mut children = vec![base.as_ref()];
            children.extend(subscripts.iter().filter_map(|subscript| match subscript {
                ast::Subscript::Expression(expression) => Some(expression),
                ast::Subscript::Range { .. } | ast::Subscript::Empty => None,
            }));
            children
        }
        ast::Expression::FieldAccess { base, .. } => vec![base.as_ref()],
        ast::Expression::ComponentReference(reference) => {
            let mut children = Vec::new();
            append_reference_subscripts(reference, &mut children);
            children
        }
        ast::Expression::Empty { .. } | ast::Expression::Terminal { .. } => Vec::new(),
    }
}
