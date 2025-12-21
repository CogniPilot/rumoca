//! Function inliner visitor
//!
//! This visitor inlines user-defined function calls by substituting
//! the function body with actual arguments.

use crate::ir::ast::{Causality, ClassDefinition, ClassType, Expression, Statement};
use crate::ir::transform::constants::is_builtin_function;
use crate::ir::visitor::MutVisitor;
use indexmap::IndexMap;

/// Visitor that inlines user-defined function calls
/// Uses references to avoid cloning ClassDefinition objects
pub struct FunctionInliner<'a> {
    /// Map of function names to references to their definitions
    functions: IndexMap<String, &'a ClassDefinition>,
}

impl<'a> FunctionInliner<'a> {
    /// Create a new function inliner with the given function definitions
    pub fn new(functions: IndexMap<String, &'a ClassDefinition>) -> Self {
        Self { functions }
    }

    /// Create from a class list, extracting functions recursively (including nested)
    pub fn from_class_list(class_list: &'a IndexMap<String, ClassDefinition>) -> Self {
        let mut functions: IndexMap<String, &'a ClassDefinition> = IndexMap::new();
        for (_name, class) in class_list {
            Self::collect_functions_recursive(class, "", &mut functions);
        }
        Self { functions }
    }

    /// Check if a function name is known (for debugging)
    pub fn has_function(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Get all function names containing a pattern (for debugging)
    pub fn functions_matching(&self, pattern: &str) -> Vec<String> {
        self.functions
            .keys()
            .filter(|k| k.contains(pattern))
            .cloned()
            .collect()
    }

    /// Recursively collect functions from a class and its nested classes
    fn collect_functions_recursive(
        class: &'a ClassDefinition,
        prefix: &str,
        functions: &mut IndexMap<String, &'a ClassDefinition>,
    ) {
        let full_name = if prefix.is_empty() {
            class.name.text.clone()
        } else {
            format!("{}.{}", prefix, class.name.text)
        };

        // If this is a function, add it with its full path
        if matches!(class.class_type, ClassType::Function) {
            functions.insert(full_name.clone(), class);
            // Also add the short name for calls within the same package
            functions.insert(class.name.text.clone(), class);
        }

        // Recursively process nested classes
        for (_name, nested_class) in &class.classes {
            Self::collect_functions_recursive(nested_class, &full_name, functions);
        }

        // For packages, also add relative paths for their children
        // This allows Package.function to be called from sibling classes
        if matches!(class.class_type, ClassType::Package) {
            Self::collect_functions_with_relative_paths(class, &class.name.text, functions);
        }
    }

    /// Collect functions with relative paths from a given package root
    /// This allows functions to be called with package-relative names
    fn collect_functions_with_relative_paths(
        class: &'a ClassDefinition,
        relative_prefix: &str,
        functions: &mut IndexMap<String, &'a ClassDefinition>,
    ) {
        for (_name, nested_class) in &class.classes {
            let relative_name = format!("{}.{}", relative_prefix, nested_class.name.text);

            if matches!(nested_class.class_type, ClassType::Function) {
                functions.insert(relative_name.clone(), nested_class);
            }

            // Recursively process nested packages
            if matches!(nested_class.class_type, ClassType::Package) {
                Self::collect_functions_with_relative_paths(
                    nested_class,
                    &relative_name,
                    functions,
                );
            }
        }
    }

    /// Inline a function call, returning the substituted expression
    /// For single-output functions, returns the single expression
    /// For multi-output functions, returns a Tuple of expressions
    fn inline_call(&self, func_name: &str, args: &[Expression]) -> Option<Expression> {
        // Don't inline built-in functions (like abs, sqrt, sin, cos, etc.)
        // These should be preserved and handled by the backend
        // Also extract the simple function name (last part) for checking
        let simple_name = func_name.rsplit('.').next().unwrap_or(func_name);
        if is_builtin_function(simple_name) {
            return None;
        }

        let func = self.functions.get(func_name)?;

        // Get input and output parameters from function components
        let inputs: Vec<(&String, &crate::ir::ast::Component)> = func
            .components
            .iter()
            .filter(|(_, comp)| matches!(comp.causality, Causality::Input(_)))
            .collect();

        let outputs: Vec<(&String, &crate::ir::ast::Component)> = func
            .components
            .iter()
            .filter(|(_, comp)| matches!(comp.causality, Causality::Output(_)))
            .collect();

        if outputs.is_empty() {
            return None;
        }

        // Check argument count matches input count
        if args.len() != inputs.len() {
            return None;
        }

        // Build substitution map: input_name -> actual_arg
        let mut substitutions: IndexMap<String, Expression> = IndexMap::new();
        for (i, (input_name, _)) in inputs.iter().enumerate() {
            substitutions.insert((*input_name).clone(), args[i].clone());
        }

        // Build a map of output_name -> expression from algorithm assignments
        let mut output_exprs: IndexMap<String, Expression> = IndexMap::new();
        for algo in &func.algorithms {
            for stmt in algo {
                if let Statement::Assignment { comp, value } = stmt {
                    let comp_name = comp.to_string();
                    // Check if this is an output variable assignment
                    if outputs.iter().any(|(name, _)| **name == comp_name) {
                        output_exprs.insert(comp_name, substitute_vars(value, &substitutions));
                    }
                }
            }
        }

        // Return based on number of outputs
        if outputs.len() == 1 {
            // Single output - return the expression directly
            let output_name = outputs[0].0;
            output_exprs.get(output_name).cloned()
        } else {
            // Multi-output - return a Tuple in the same order as outputs are declared
            let mut elements = Vec::new();
            for (output_name, _) in &outputs {
                if let Some(expr) = output_exprs.get(*output_name) {
                    elements.push(expr.clone());
                } else {
                    // Output not assigned - can't inline this function
                    return None;
                }
            }
            Some(Expression::Tuple { elements })
        }
    }
}

/// Substitute variable references in an expression with their replacements
fn substitute_vars(expr: &Expression, substitutions: &IndexMap<String, Expression>) -> Expression {
    match expr {
        Expression::ComponentReference(comp_ref) => {
            let var_name = comp_ref.to_string();
            // First try exact match
            if let Some(replacement) = substitutions.get(&var_name) {
                return replacement.clone();
            }

            // If no exact match, try to substitute the base component
            // e.g., if substituting c1 -> u1, then c1.re should become u1.re
            if !comp_ref.parts.is_empty() {
                let base_name = &comp_ref.parts[0].ident.text;
                if let Some(Expression::ComponentReference(repl_ref)) = substitutions.get(base_name)
                    && comp_ref.parts.len() > 1
                {
                    // Append the remaining parts (field accesses) to the replacement
                    let mut new_parts = repl_ref.parts.clone();
                    new_parts.extend(comp_ref.parts[1..].iter().cloned());
                    return Expression::ComponentReference(crate::ir::ast::ComponentReference {
                        local: repl_ref.local,
                        parts: new_parts,
                    });
                }
            }

            expr.clone()
        }
        Expression::Binary { op, lhs, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_vars(lhs, substitutions)),
            rhs: Box::new(substitute_vars(rhs, substitutions)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_vars(rhs, substitutions)),
        },
        Expression::FunctionCall { comp, args } => Expression::FunctionCall {
            comp: comp.clone(),
            args: args
                .iter()
                .map(|a| substitute_vars(a, substitutions))
                .collect(),
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_vars(e, substitutions))
                .collect(),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: elements
                .iter()
                .map(|e| substitute_vars(e, substitutions))
                .collect(),
        },
        // Terminal expressions and other types don't need substitution
        _ => expr.clone(),
    }
}

impl<'a> MutVisitor for FunctionInliner<'a> {
    fn exit_expression(&mut self, expr: &mut Expression) {
        if let Expression::FunctionCall { comp, args } = expr {
            let func_name = comp.to_string();
            if let Some(inlined) = self.inline_call(&func_name, args) {
                *expr = inlined;
            }
        }
    }
}
