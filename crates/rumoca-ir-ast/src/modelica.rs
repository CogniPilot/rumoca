//! Modelica output for AST (parse phase debugging).
//!
//! This module provides `to_modelica()` implementations for the parsed AST,
//! allowing the compiler's parse output to be viewed as Modelica code.
//! The output should be nearly identical to the source, except for whitespace.

use crate::{
    Causality, ClassDef, Component, Connection, EnumLiteral, Expression, Extend,
    ExtendModification, Import, Statement, StatementBlock, StoredDefinition, Variability,
};
use std::fmt::Write;

/// Convert an enum literal to Modelica string representation.
fn enum_literal_to_string(lit: &EnumLiteral) -> String {
    if lit.description.is_empty() {
        lit.ident.text.to_string()
    } else {
        let desc: Vec<_> = lit.description.iter().map(|t| t.text.to_string()).collect();
        format!("{} \"{}\"", lit.ident.text, desc.join(""))
    }
}

/// Write equations with proper indentation for multi-line equations.
fn write_equations(out: &mut String, equations: &[crate::Equation], inner_indent: &str) {
    for eq in equations {
        let eq_str = format!("{};", eq);
        for line in eq_str.lines() {
            writeln!(out, "{}  {}", inner_indent, line).expect("write to String never fails");
        }
    }
}

/// Convert an extend modification to Modelica string representation.
fn extend_modification_to_string(m: &ExtendModification) -> String {
    let each_prefix = if m.each { "each " } else { "" };
    let final_prefix = if m.final_ { "final " } else { "" };
    if m.redeclare
        && let Some(redeclare_expr) = redeclare_assignment_to_string(&m.expr)
    {
        return format!(
            "redeclare {}{}{}",
            final_prefix, each_prefix, redeclare_expr
        );
    }
    let redeclare_prefix = if m.redeclare { "redeclare " } else { "" };
    format!(
        "{}{}{}{}",
        redeclare_prefix, final_prefix, each_prefix, m.expr
    )
}

fn expression_list_to_string(exprs: &[Expression]) -> String {
    exprs
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

fn class_mod_target_with_mods(expr: &Expression) -> Option<(String, Option<String>)> {
    match expr {
        Expression::ComponentReference(comp) => Some((comp.to_string(), None)),
        Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            let target_name = target.to_string();
            if modifications.is_empty() {
                Some((target_name, None))
            } else {
                Some((target_name, Some(expression_list_to_string(modifications))))
            }
        }
        _ => None,
    }
}

fn starts_with_uppercase_ident(name: &str) -> bool {
    name.chars().next().is_some_and(char::is_uppercase)
}

fn redeclare_assignment_to_string(expr: &Expression) -> Option<String> {
    let (instance_name, lhs_mods, rhs_expr, was_modification_expr) = match expr {
        Expression::Binary {
            op: rumoca_core::OpBinary::Assign,
            lhs,
            rhs,
            ..
        } => {
            let (instance_name, lhs_mods) = class_mod_target_with_mods(lhs)?;
            (instance_name, lhs_mods, rhs.as_ref(), false)
        }
        Expression::Modification { target, value, .. } => {
            (target.to_string(), None, value.as_ref(), true)
        }
        _ => return None,
    };

    match rhs_expr {
        Expression::FunctionCall { comp, args, .. } => {
            let type_name = comp.to_string();
            let rhs_mods = if args.is_empty() {
                lhs_mods.map(|mods| format!("({mods})")).unwrap_or_default()
            } else {
                format!("({})", expression_list_to_string(args))
            };
            Some(format!("{type_name} {instance_name}{rhs_mods}"))
        }
        Expression::ComponentReference(comp) => {
            let type_name = comp.to_string();
            let suffix = lhs_mods.map(|mods| format!("({mods})")).unwrap_or_default();
            Some(format!("{type_name} {instance_name}{suffix}"))
        }
        Expression::ClassModification {
            target,
            modifications,
            ..
        } => {
            let type_name = target.to_string();
            if was_modification_expr
                && modifications.is_empty()
                && starts_with_uppercase_ident(&instance_name)
            {
                // Preserve short class/package redeclare assignment form in round-trippable output:
                // `redeclare package Medium = Modelica.Media.Water.StandardWater`
                return Some(format!("package {instance_name} = {type_name}"));
            }
            let rhs_mods = if modifications.is_empty() {
                lhs_mods.map(|mods| format!("({mods})")).unwrap_or_default()
            } else {
                format!("({})", expression_list_to_string(modifications))
            };
            Some(format!("{type_name} {instance_name}{rhs_mods}"))
        }
        _ => None,
    }
}

/// Format a component modification with optional `each` prefix.
fn format_modification(
    name: &str,
    expr: &Expression,
    each_mods: &indexmap::IndexSet<String>,
) -> String {
    let prefix = if each_mods.contains(name) {
        "each "
    } else {
        ""
    };
    format!("{}{} = {}", prefix, name, expr)
}

/// Write a block of statements with a given header.
fn write_statement_block(
    out: &mut String,
    header: &str,
    cond: &Expression,
    stmts: &[Statement],
    indent: &str,
    inner_indent: &str,
) {
    writeln!(out, "{}{} {} then", indent, header, cond).expect("write to String never fails");
    for stmt in stmts {
        write!(out, "{}", stmt.to_modelica(inner_indent)).expect("write to String never fails");
    }
}

/// Write an if/when block (handles first vs subsequent blocks).
fn write_conditional_block(
    out: &mut String,
    block: &StatementBlock,
    is_first: bool,
    keyword: &str,
    else_keyword: &str,
    indent: &str,
    inner_indent: &str,
) {
    let header = if is_first { keyword } else { else_keyword };
    write_statement_block(out, header, &block.cond, &block.stmts, indent, inner_indent);
}

/// Write a list of statements with proper indentation.
fn write_statements(out: &mut String, stmts: &[Statement], indent: &str) {
    for stmt in stmts {
        write!(out, "{}", stmt.to_modelica(indent)).expect("write to String never fails");
    }
}

impl StoredDefinition {
    /// Convert the parsed AST to Modelica syntax for debugging.
    /// This should produce output nearly identical to the source.
    pub fn to_modelica(&self) -> String {
        let mut out = String::new();

        // Within clause
        if let Some(within) = &self.within {
            writeln!(out, "within {};", within).expect("write to String never fails");
            writeln!(out).expect("write to String never fails");
        }

        // Class definitions
        for (_, class) in &self.classes {
            write!(out, "{}", class.to_modelica("")).expect("write to String never fails");
        }

        out
    }
}

impl ClassDef {
    /// Convert a class definition to Modelica syntax.
    pub fn to_modelica(&self, indent: &str) -> String {
        let mut out = String::new();
        let inner_indent = format!("{}  ", indent);

        // Class header with prefixes
        if self.encapsulated {
            write!(out, "{}encapsulated ", indent).expect("write to String never fails");
        } else {
            write!(out, "{}", indent).expect("write to String never fails");
        }
        if self.partial {
            write!(out, "partial ").expect("write to String never fails");
        }
        if self.expandable {
            write!(out, "expandable ").expect("write to String never fails");
        }
        if self.operator_record {
            write!(out, "operator ").expect("write to String never fails");
        }
        write!(out, "{} {}", self.class_type.as_str(), self.name.text)
            .expect("write to String never fails");

        // Description string
        if !self.description.is_empty() {
            let desc: Vec<_> = self
                .description
                .iter()
                .map(|t| t.text.to_string())
                .collect();
            write!(out, " \"{}\"", desc.join("")).expect("write to String never fails");
        }

        writeln!(out).expect("write to String never fails");

        // Imports
        for import in &self.imports {
            writeln!(out, "{}{};", inner_indent, import.to_modelica())
                .expect("write to String never fails");
        }

        // Extends
        for ext in &self.extends {
            writeln!(out, "{}{}", inner_indent, ext.to_modelica())
                .expect("write to String never fails");
        }

        // Enumeration literals
        if !self.enum_literals.is_empty() {
            let literals: Vec<_> = self
                .enum_literals
                .iter()
                .map(enum_literal_to_string)
                .collect();
            writeln!(out, "{}enumeration({});", inner_indent, literals.join(", "))
                .expect("write to String never fails");
        }

        // Components (public section)
        let public_comps: Vec<_> = self
            .components
            .values()
            .filter(|c| !c.is_protected)
            .collect();
        for comp in &public_comps {
            writeln!(out, "{}{}", inner_indent, comp.to_modelica())
                .expect("write to String never fails");
        }

        // Protected section
        let protected_comps: Vec<_> = self
            .components
            .values()
            .filter(|c| c.is_protected)
            .collect();
        if !protected_comps.is_empty() {
            writeln!(out, "{}protected", inner_indent).expect("write to String never fails");
            for comp in &protected_comps {
                writeln!(out, "{}{}", inner_indent, comp.to_modelica())
                    .expect("write to String never fails");
            }
        }

        // Nested classes
        for (_, class) in &self.classes {
            write!(out, "{}", class.to_modelica(&inner_indent))
                .expect("write to String never fails");
        }

        // Initial equations
        if !self.initial_equations.is_empty() {
            writeln!(out, "{}initial equation", inner_indent).expect("write to String never fails");
            write_equations(&mut out, &self.initial_equations, &inner_indent);
        }

        // Equations
        // Equations
        if !self.equations.is_empty() {
            writeln!(out, "{}equation", inner_indent).expect("write to String never fails");
            write_equations(&mut out, &self.equations, &inner_indent);
        }

        // Initial algorithms
        for algo in &self.initial_algorithms {
            writeln!(out, "{}initial algorithm", inner_indent)
                .expect("write to String never fails");
            for stmt in algo {
                write!(out, "{}", stmt.to_modelica(&format!("{}  ", inner_indent)))
                    .expect("write to String never fails");
            }
        }

        // Algorithms
        for algo in &self.algorithms {
            writeln!(out, "{}algorithm", inner_indent).expect("write to String never fails");
            for stmt in algo {
                write!(out, "{}", stmt.to_modelica(&format!("{}  ", inner_indent)))
                    .expect("write to String never fails");
            }
        }

        // Annotation
        if !self.annotation.is_empty() {
            let anno_strs: Vec<_> = self.annotation.iter().map(|e| e.to_string()).collect();
            writeln!(out, "{}annotation({});", inner_indent, anno_strs.join(", "))
                .expect("write to String never fails");
        }

        writeln!(out, "{}end {};", indent, self.name.text).expect("write to String never fails");

        out
    }
}

impl Component {
    /// Convert a component to Modelica syntax.
    pub fn to_modelica(&self) -> String {
        let mut out = String::new();

        // Prefixes
        if self.is_final {
            write!(out, "final ").expect("write to String never fails");
        }
        if self.is_replaceable {
            write!(out, "replaceable ").expect("write to String never fails");
        }
        if self.inner {
            write!(out, "inner ").expect("write to String never fails");
        }
        if self.outer {
            write!(out, "outer ").expect("write to String never fails");
        }
        match &self.connection {
            Connection::Flow(_) => write!(out, "flow ").expect("write to String never fails"),
            Connection::Stream(_) => write!(out, "stream ").expect("write to String never fails"),
            Connection::Empty => {}
        }
        match &self.causality {
            Causality::Input(_) => write!(out, "input ").expect("write to String never fails"),
            Causality::Output(_) => write!(out, "output ").expect("write to String never fails"),
            Causality::Empty => {}
        }
        match &self.variability {
            Variability::Constant(_) => {
                write!(out, "constant ").expect("write to String never fails")
            }
            Variability::Parameter(_) => {
                write!(out, "parameter ").expect("write to String never fails")
            }
            Variability::Discrete(_) => {
                write!(out, "discrete ").expect("write to String never fails")
            }
            Variability::Continuous(_) => {}
            Variability::Empty => {}
        }

        // Type
        write!(out, "{}", self.type_name).expect("write to String never fails");

        // Array dimensions from subscripts
        if !self.shape_expr.is_empty() {
            let dims: Vec<_> = self.shape_expr.iter().map(|s| s.to_string()).collect();
            write!(out, "[{}]", dims.join(", ")).expect("write to String never fails");
        }

        // Name
        write!(out, " {}", self.name).expect("write to String never fails");

        // Modifications
        if !self.modifications.is_empty() {
            let each_mods = &self.each_modifications;
            let mods: Vec<_> = self
                .modifications
                .iter()
                .map(|(name, expr)| format_modification(name, expr, each_mods))
                .collect();
            write!(out, "({})", mods.join(", ")).expect("write to String never fails");
        }

        // Binding expression
        if self.has_explicit_binding {
            write!(out, " = {}", self.start).expect("write to String never fails");
        } else if !matches!(self.start, Expression::Empty { .. }) && self.start_is_modification {
            // Start value from modification
            if self.start_has_each {
                write!(out, "(each start = {})", self.start).expect("write to String never fails");
            } else {
                write!(out, "(start = {})", self.start).expect("write to String never fails");
            }
        }

        // Condition
        if let Some(cond) = &self.condition {
            write!(out, " if {}", cond).expect("write to String never fails");
        }

        // Constraining clause
        if let Some(constrainedby) = &self.constrainedby {
            write!(out, " constrainedby {}", constrainedby).expect("write to String never fails");
        }

        // Description
        if !self.description.is_empty() {
            let desc: Vec<_> = self
                .description
                .iter()
                .map(|t| t.text.to_string())
                .collect();
            write!(out, " \"{}\"", desc.join("")).expect("write to String never fails");
        }

        // Annotation
        if !self.annotation.is_empty() {
            let anno_strs: Vec<_> = self.annotation.iter().map(|e| e.to_string()).collect();
            write!(out, " annotation({})", anno_strs.join(", "))
                .expect("write to String never fails");
        }

        write!(out, ";").expect("write to String never fails");

        out
    }
}

impl Extend {
    /// Convert an extends clause to Modelica syntax.
    pub fn to_modelica(&self) -> String {
        let mut out = format!("extends {}", self.base_name);

        if !self.modifications.is_empty() || !self.break_names.is_empty() {
            let mods: Vec<_> = self
                .modifications
                .iter()
                .map(extend_modification_to_string)
                .collect();

            // Add break names
            let break_strs: Vec<_> = self
                .break_names
                .iter()
                .map(|n| format!("break {}", n))
                .collect();

            let all_mods: Vec<_> = mods.into_iter().chain(break_strs).collect();
            write!(out, "({})", all_mods.join(", ")).expect("write to String never fails");
        }

        // Add annotation if present
        if !self.annotation.is_empty() {
            let annot_strs: Vec<_> = self.annotation.iter().map(|e| e.to_string()).collect();
            write!(out, " annotation({})", annot_strs.join(", "))
                .expect("write to String never fails");
        }

        write!(out, ";").expect("write to String never fails");
        out
    }
}

impl Import {
    /// Convert an import to Modelica syntax.
    pub fn to_modelica(&self) -> String {
        match self {
            Import::Qualified {
                path, global_scope, ..
            } => {
                if *global_scope {
                    format!("import .{}", path)
                } else {
                    format!("import {}", path)
                }
            }
            Import::Renamed {
                alias,
                path,
                global_scope,
                ..
            } => {
                if *global_scope {
                    format!("import {} = .{}", alias.text, path)
                } else {
                    format!("import {} = {}", alias.text, path)
                }
            }
            Import::Unqualified {
                path, global_scope, ..
            } => {
                if *global_scope {
                    format!("import .{}.*", path)
                } else {
                    format!("import {}.*", path)
                }
            }
            Import::Selective {
                path,
                names,
                global_scope,
                ..
            } => {
                let name_strs: Vec<_> = names.iter().map(|t| t.text.to_string()).collect();
                if *global_scope {
                    format!("import .{}.{{{}}}", path, name_strs.join(", "))
                } else {
                    format!("import {}.{{{}}}", path, name_strs.join(", "))
                }
            }
        }
    }
}

/// Format a for-statement to Modelica syntax.
fn format_for_statement(
    indices: &[crate::ForIndex],
    equations: &[Statement],
    indent: &str,
) -> String {
    let mut out = String::new();
    let indices_str: Vec<_> = indices
        .iter()
        .map(|i| format!("{} in {}", i.ident.text, i.range))
        .collect();
    writeln!(out, "{}for {} loop", indent, indices_str.join(", "))
        .expect("write to String never fails");
    let inner_indent = format!("{}  ", indent);
    for stmt in equations {
        write!(out, "{}", stmt.to_modelica(&inner_indent)).expect("write to String never fails");
    }
    writeln!(out, "{}end for;", indent).expect("write to String never fails");
    out
}

/// Format a while-statement to Modelica syntax.
fn format_while_statement(block: &StatementBlock, indent: &str) -> String {
    let mut out = String::new();
    writeln!(out, "{}while {} loop", indent, block.cond).expect("write to String never fails");
    let inner_indent = format!("{}  ", indent);
    for stmt in &block.stmts {
        write!(out, "{}", stmt.to_modelica(&inner_indent)).expect("write to String never fails");
    }
    writeln!(out, "{}end while;", indent).expect("write to String never fails");
    out
}

/// Format a function call statement to Modelica syntax.
fn format_function_call(
    comp: &crate::ComponentReference,
    args: &[Expression],
    outputs: &[Expression],
    indent: &str,
) -> String {
    if outputs.is_empty() {
        let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        format!("{}{}({});\n", indent, comp, args_str.join(", "))
    } else {
        let outputs_str: Vec<_> = outputs.iter().map(|o| o.to_string()).collect();
        let args_str: Vec<_> = args.iter().map(|a| a.to_string()).collect();
        format!(
            "{}({}) := {}({});\n",
            indent,
            outputs_str.join(", "),
            comp,
            args_str.join(", ")
        )
    }
}

impl Statement {
    /// Convert a statement to Modelica syntax.
    pub fn to_modelica(&self, indent: &str) -> String {
        match self {
            Statement::Empty => String::new(),
            Statement::Assignment { comp, value } => {
                format!("{}{} := {};\n", indent, comp, value)
            }
            Statement::Return { .. } => format!("{}return;\n", indent),
            Statement::Break { .. } => format!("{}break;\n", indent),
            Statement::For { indices, equations } => {
                format_for_statement(indices, equations, indent)
            }
            Statement::While(block) => format_while_statement(block, indent),
            Statement::If {
                cond_blocks,
                else_block,
            } => {
                let mut out = String::new();
                let inner_indent = format!("{}  ", indent);
                for (i, block) in cond_blocks.iter().enumerate() {
                    write_conditional_block(
                        &mut out,
                        block,
                        i == 0,
                        "if",
                        "elseif",
                        indent,
                        &inner_indent,
                    );
                }
                if let Some(else_stmts) = else_block {
                    writeln!(out, "{}else", indent).expect("write to String never fails");
                    write_statements(&mut out, else_stmts, &inner_indent);
                }
                writeln!(out, "{}end if;", indent).expect("write to String never fails");
                out
            }
            Statement::When(blocks) => {
                let mut out = String::new();
                let inner_indent = format!("{}  ", indent);
                for (i, block) in blocks.iter().enumerate() {
                    write_conditional_block(
                        &mut out,
                        block,
                        i == 0,
                        "when",
                        "elsewhen",
                        indent,
                        &inner_indent,
                    );
                }
                writeln!(out, "{}end when;", indent).expect("write to String never fails");
                out
            }
            Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => format_function_call(comp, args, outputs, indent),
            Statement::Reinit { variable, value } => {
                format!("{}reinit({}, {});\n", indent, variable, value)
            }
            Statement::Assert {
                condition,
                message,
                level,
            } => {
                if let Some(lvl) = level {
                    format!("{}assert({}, {}, {});\n", indent, condition, message, lvl)
                } else {
                    format!("{}assert({}, {});\n", indent, condition, message)
                }
            }
        }
    }
}
