//! Core compilation pipeline.
//!
//! This module contains the main compilation pipeline that transforms
//! a Modelica AST into a DAE representation.

use super::error_handling::{SyntaxError, UndefinedVariableError, extract_line_col_from_error};
use super::function_collector::collect_all_functions;
use super::result::CompilationResult;
use crate::ir::analysis::balance_check::check_dae_balance;
use crate::ir::analysis::var_validator::VarValidator;
use crate::ir::ast::{ClassType, StoredDefinition};
use crate::ir::structural::create_dae::create_dae;
use crate::ir::transform::flatten::flatten;
use crate::ir::transform::function_inliner::FunctionInliner;
use crate::ir::transform::import_resolver::ImportResolver;
use crate::ir::transform::tuple_expander::expand_tuple_equations;
use crate::ir::visitor::MutVisitable;
use anyhow::Result;
use miette::SourceSpan;
use std::time::Instant;

/// Run the compilation pipeline on a parsed AST.
///
/// This function takes a parsed StoredDefinition and performs:
/// 1. Flattening - resolve class hierarchy
/// 2. Import resolution - rewrite short names to fully qualified
/// 3. Variable validation - check for undefined variables
/// 4. Function inlining - inline user-defined functions
/// 5. Tuple expansion - expand tuple equations
/// 6. DAE creation - create the final DAE representation
/// 7. Balance checking - verify equations match unknowns
pub fn compile_from_ast(
    def: StoredDefinition,
    source: &str,
    model_name: Option<&str>,
    model_hash: String,
    parse_time: std::time::Duration,
    verbose: bool,
) -> Result<CompilationResult> {
    // Flatten
    let flatten_start = Instant::now();
    let fclass_result = flatten(&def, model_name);

    // Handle flatten errors with proper source location
    let mut fclass = match fclass_result {
        Ok(fc) => fc,
        Err(e) => {
            let error_msg = e.to_string();

            // Try to extract line/column from error message like "at line X, column Y"
            let (line, col) = extract_line_col_from_error(&error_msg).unwrap_or((1, 1));

            // Calculate byte offset for the line/column
            let mut byte_offset = 0;
            for (i, src_line) in source.lines().enumerate() {
                if i + 1 == line {
                    byte_offset += col.saturating_sub(1);
                    break;
                }
                byte_offset += src_line.len() + 1;
            }

            let span = SourceSpan::new(byte_offset.into(), 1_usize);
            let diagnostic = SyntaxError {
                src: source.to_string(),
                span,
                message: error_msg,
            };
            let report = miette::Report::new(diagnostic);
            return Err(anyhow::anyhow!("{:?}", report));
        }
    };
    let flatten_time = flatten_start.elapsed();

    if verbose {
        println!("Flattening took {} ms", flatten_time.as_millis());
        println!("Flattened class:\n{:#?}\n", fclass);
    }

    // Resolve imports - rewrite short function names to fully qualified names
    // This must happen before validation so imported names are recognized
    let mut import_resolver = ImportResolver::new(&fclass, &def);
    fclass.accept_mut(&mut import_resolver);

    // Collect all function names from the stored definition (including nested)
    let function_names = collect_all_functions(&def);

    // Skip validation for packages and classes with nested functions
    // (function parameters aren't yet properly scoped)
    let has_nested_functions = fclass
        .classes
        .values()
        .any(|c| c.class_type == ClassType::Function);
    let should_validate = !matches!(fclass.class_type, ClassType::Package) && !has_nested_functions;

    if should_validate {
        // Validate variable references (passing function names so they're recognized)
        let mut validator = VarValidator::with_functions(&fclass, &function_names);
        fclass.accept_mut(&mut validator);

        if !validator.undefined_vars.is_empty() {
            // Just report the first undefined variable with miette for now
            let (var_name, _context) = &validator.undefined_vars[0];

            // Find the first occurrence of this variable in the source
            let mut byte_offset = 0;
            let mut found = false;
            for line in source.lines() {
                if let Some(col) = line.find(var_name) {
                    byte_offset += col;
                    found = true;
                    break;
                }
                byte_offset += line.len() + 1;
            }

            let span = if found {
                SourceSpan::new(byte_offset.into(), var_name.len())
            } else {
                SourceSpan::new(0.into(), 1_usize)
            };

            let diagnostic = UndefinedVariableError {
                src: source.to_string(),
                var_name: var_name.clone(),
                span,
            };

            let report = miette::Report::new(diagnostic);
            return Err(anyhow::anyhow!("{:?}", report));
        }
    }

    // Inline user-defined function calls
    let mut inliner = FunctionInliner::from_class_list(&def.class_list);
    fclass.accept_mut(&mut inliner);

    // Expand tuple equations like (a, b) = (expr1, expr2) into separate equations
    expand_tuple_equations(&mut fclass);

    if verbose {
        println!(
            "After function inlining and tuple expansion:\n{:#?}\n",
            fclass
        );
    }

    // Create DAE
    let dae_start = Instant::now();
    let mut dae = create_dae(&mut fclass)?;
    dae.model_hash = model_hash.clone();
    let dae_time = dae_start.elapsed();

    if verbose {
        println!("DAE creation took {} ms", dae_time.as_millis());
        println!("DAE:\n{:#?}\n", dae);
    }

    // Check model balance
    let balance = check_dae_balance(&dae);

    if verbose {
        println!("{}", balance.status_message());
    }

    Ok(CompilationResult {
        dae,
        def,
        parse_time,
        flatten_time,
        dae_time,
        model_hash,
        balance,
    })
}
