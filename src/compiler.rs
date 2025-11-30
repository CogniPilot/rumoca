//! High-level API for compiling Modelica models to DAE representations.
//!
//! This module provides a clean, ergonomic interface for using rumoca as a library.
//! The main entry point is the [`Compiler`] struct, which uses a builder pattern
//! for configuration.
//!
//! # Examples
//!
//! Basic usage:
//!
//! ```no_run
//! use rumoca::Compiler;
//!
//! let result = Compiler::new()
//!     .model("MyModel")
//!     .compile_file("model.mo")?;
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! With verbose output and template rendering:
//!
//! ```no_run
//! use rumoca::Compiler;
//!
//! let output = Compiler::new()
//!     .model("MyModel")
//!     .verbose(true)
//!     .compile_file("model.mo")?
//!     .render_template("template.j2")?;
//! # Ok::<(), anyhow::Error>(())
//! ```
//!
//! Compiling from a string:
//!
//! ```no_run
//! use rumoca::Compiler;
//!
//! let modelica_code = r#"
//!     model Integrator
//!         Real x(start=0);
//!     equation
//!         der(x) = 1;
//!     end Integrator;
//! "#;
//!
//! let result = Compiler::new()
//!     .model("Integrator")
//!     .compile_str(modelica_code, "Integrator.mo")?;
//! # Ok::<(), anyhow::Error>(())
//! ```

use crate::dae::ast::Dae;
use crate::errors::{SyntaxError, UndefinedVariableError};
use crate::ir::ast::StoredDefinition;
use crate::ir::balance_check::{BalanceCheckResult, check_dae_balance};
use crate::ir::create_dae::create_dae;
use crate::ir::flatten::flatten;
use crate::ir::visitor::Visitable;
use crate::ir::visitors::function_inliner::FunctionInliner;
use crate::ir::visitors::import_resolver::ImportResolver;
use crate::ir::visitors::tuple_expander::expand_tuple_equations;
use crate::ir::visitors::var_validator::VarValidator;
use crate::modelica_grammar::ModelicaGrammar;
use crate::modelica_parser::parse;
use anyhow::{Context, Result};
use miette::SourceSpan;
use parol_runtime::ParolError;
use std::fs;
use std::time::Instant;

/// Create a syntax error diagnostic from a parse error
fn create_syntax_error(error: &ParolError, source: &str) -> SyntaxError {
    let error_debug = format!("{:?}", error);
    let error_display = format!("{}", error);

    // Try to extract location from ParolError debug format first
    let location = extract_location_from_debug(&error_debug)
        // Then try from "at line X, column Y" format in error message
        .or_else(|| extract_line_col_from_error(&error_display))
        .or_else(|| extract_line_col_from_error(&error_debug));

    // Extract location and message
    let (span, message) = if let Some((line_num, col_num)) = location {
        // Calculate byte offset for the error location
        let mut byte_offset = 0;
        for (i, line) in source.lines().enumerate() {
            if i + 1 == line_num {
                byte_offset += col_num.saturating_sub(1);
                break;
            }
            byte_offset += line.len() + 1; // +1 for newline
        }

        // Extract the message - try cause first, then fall back to display
        let msg = extract_error_cause(&error_debug).unwrap_or_else(|| error_display.clone());

        // Create a span of reasonable length (10 chars or to end of line)
        let remaining = source.len().saturating_sub(byte_offset);
        let span_len = remaining.min(10);

        (SourceSpan::new(byte_offset.into(), span_len), msg)
    } else {
        // Fallback: highlight the start of the file
        (SourceSpan::new(0.into(), 1_usize), error_display)
    };

    SyntaxError {
        src: source.to_string(),
        span,
        message,
    }
}

/// Extract location from ParolError debug output
/// Looks for pattern like "error_location: Location { start_line: 2, start_column: 21, ..."
fn extract_location_from_debug(debug_str: &str) -> Option<(usize, usize)> {
    // Find the first occurrence of "error_location: Location {"
    if let Some(pos) = debug_str.find("error_location: Location {") {
        let after_location = &debug_str[pos..];

        // Extract start_line
        let line_num = if let Some(line_pos) = after_location.find("start_line:") {
            let after_line = &after_location[line_pos + 11..];
            // Find the number (digits before the next comma)
            after_line
                .split(',')
                .next()
                .and_then(|s| s.trim().parse::<usize>().ok())
        } else {
            None
        };

        // Extract start_column
        let col_num = if let Some(col_pos) = after_location.find("start_column:") {
            let after_col = &after_location[col_pos + 13..];
            // Find the number (digits before the next comma)
            after_col
                .split(',')
                .next()
                .and_then(|s| s.trim().parse::<usize>().ok())
        } else {
            None
        };

        match (line_num, col_num) {
            (Some(line), Some(col)) => Some((line, col)),
            _ => None,
        }
    } else {
        None
    }
}

/// Extract the error cause from ParolError debug output
/// Looks for pattern like 'cause: "..."'
fn extract_error_cause(debug_str: &str) -> Option<String> {
    // Find the first occurrence of 'cause: "'
    if let Some(pos) = debug_str.find("cause: \"") {
        let after_cause = &debug_str[pos + 8..];
        // Find the closing quote (but handle escaped quotes)
        let mut in_escape = false;
        let mut cause_end = 0;

        for (i, ch) in after_cause.chars().enumerate() {
            if in_escape {
                in_escape = false;
                continue;
            }
            if ch == '\\' {
                in_escape = true;
                continue;
            }
            if ch == '"' {
                cause_end = i;
                break;
            }
        }

        if cause_end > 0 {
            let cause = &after_cause[..cause_end];
            // Clean up the cause message - extract just the first line
            let first_line = cause.lines().next().unwrap_or(cause);
            return Some(first_line.to_string());
        }
    }
    None
}

/// Extract line and column numbers from error messages like "at line X, column Y"
fn extract_line_col_from_error(error_msg: &str) -> Option<(usize, usize)> {
    // Look for pattern "at line X, column Y" or "line X, column Y"
    let patterns = ["at line ", "line "];

    for pattern in patterns {
        if let Some(pos) = error_msg.find(pattern) {
            let after_pattern = &error_msg[pos + pattern.len()..];

            // Parse line number
            let line_end = after_pattern
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(after_pattern.len());
            let line: usize = after_pattern[..line_end].parse().ok()?;

            // Look for column
            let col_pattern = ", column ";
            if let Some(col_pos) = after_pattern.find(col_pattern) {
                let after_col = &after_pattern[col_pos + col_pattern.len()..];
                let col_end = after_col
                    .find(|c: char| !c.is_ascii_digit())
                    .unwrap_or(after_col.len());
                let col: usize = after_col[..col_end].parse().ok()?;
                return Some((line, col));
            }
        }
    }
    None
}

/// A high-level compiler for Modelica models.
///
/// This struct provides a builder-pattern interface for configuring and executing
/// the compilation pipeline from Modelica source code to DAE representation.
///
/// # Examples
///
/// ```no_run
/// use rumoca::Compiler;
///
/// let result = Compiler::new()
///     .model("MyModel")
///     .verbose(true)
///     .compile_file("model.mo")?;
/// # Ok::<(), anyhow::Error>(())
/// ```
#[derive(Debug, Default, Clone)]
pub struct Compiler {
    verbose: bool,
    /// Main model/class name to simulate (required)
    model_name: Option<String>,
    /// Additional source files to include in compilation
    additional_files: Vec<String>,
}

impl Compiler {
    /// Creates a new compiler with default settings.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// let compiler = Compiler::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    /// Enables or disables verbose output during compilation.
    ///
    /// When enabled, the compiler will print timing information and intermediate
    /// representations to stdout.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// let compiler = Compiler::new().verbose(true);
    /// ```
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Sets the main model/class name to simulate (required).
    ///
    /// According to the Modelica specification, the user must specify which
    /// class (of specialized class `model` or `block`) to simulate.
    ///
    /// # Examples
    ///
    /// ```
    /// use rumoca::Compiler;
    ///
    /// let compiler = Compiler::new().model("MyModel");
    /// ```
    pub fn model(mut self, name: &str) -> Self {
        self.model_name = Some(name.to_string());
        self
    }

    /// Adds an additional source file to include in compilation.
    ///
    /// Use this to include library files, package definitions, or other
    /// dependencies that the main model requires.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .include("library/utils.mo")
    ///     .include("library/types.mo")
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include(mut self, path: &str) -> Self {
        self.additional_files.push(path.to_string());
        self
    }

    /// Adds multiple source files to include in compilation.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .include_all(&["lib1.mo", "lib2.mo"])
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include_all(mut self, paths: &[&str]) -> Self {
        for path in paths {
            self.additional_files.push((*path).to_string());
        }
        self
    }

    /// Includes a Modelica package directory in compilation.
    ///
    /// This method discovers all Modelica files in a package directory structure,
    /// following Modelica Spec 13.4 conventions:
    /// - Directories with `package.mo` are treated as packages
    /// - `package.order` files specify the order of nested entities
    /// - Single `.mo` files define classes
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyPackage.MyModel")
    ///     .include_package("path/to/MyPackage")?
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include_package(mut self, path: &str) -> Result<Self> {
        use crate::ir::multi_file::discover_modelica_files;

        let package_path = std::path::Path::new(path);
        let files = discover_modelica_files(package_path)?;

        for file in files {
            self.additional_files
                .push(file.to_string_lossy().to_string());
        }

        Ok(self)
    }

    /// Includes a package from MODELICAPATH by name.
    ///
    /// This method searches the MODELICAPATH environment variable for a package
    /// with the given name and includes all its files.
    ///
    /// According to Modelica Spec 13.3, MODELICAPATH is an ordered list of library
    /// root directories, separated by `:` on Unix or `;` on Windows.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// // Set MODELICAPATH=/path/to/libs before running
    /// let result = Compiler::new()
    ///     .model("Modelica.Mechanics.Rotational.Examples.First")
    ///     .include_from_modelica_path("Modelica")?
    ///     .compile_file("model.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn include_from_modelica_path(self, package_name: &str) -> Result<Self> {
        use crate::ir::multi_file::find_package_in_modelica_path;

        let package_path = find_package_in_modelica_path(package_name).ok_or_else(|| {
            anyhow::anyhow!(
                "Package '{}' not found in MODELICAPATH. Current MODELICAPATH: {:?}",
                package_name,
                std::env::var("MODELICAPATH").unwrap_or_default()
            )
        })?;

        self.include_package(&package_path.to_string_lossy())
    }

    /// Compiles a Modelica package directory directly.
    ///
    /// This method discovers all files in a package directory structure and
    /// compiles them together. The main model to simulate is specified via `.model()`.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyPackage.MyModel")
    ///     .compile_package("path/to/MyPackage")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_package(&self, path: &str) -> Result<CompilationResult> {
        use crate::ir::multi_file::discover_modelica_files;

        let package_path = std::path::Path::new(path);
        let files = discover_modelica_files(package_path)?;

        if files.is_empty() {
            anyhow::bail!("No Modelica files found in package: {}", path);
        }

        let file_strs: Vec<&str> = files.iter().map(|p| p.to_str().unwrap()).collect();
        self.compile_files(&file_strs)
    }

    /// Compiles a Modelica file to a DAE representation.
    ///
    /// This method performs the full compilation pipeline:
    /// 1. Reads the file from disk
    /// 2. Parses the Modelica code into an AST
    /// 3. Flattens the hierarchical class structure
    /// 4. Converts to DAE representation
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the Modelica file to compile
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be read
    /// - The Modelica code contains syntax errors
    /// - The model contains unsupported features (e.g., unexpanded connection equations)
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// println!("Model has {} states", result.dae.x.len());
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_file(&self, path: &str) -> Result<CompilationResult> {
        // Parse additional files first
        let mut all_definitions = Vec::new();

        for additional_path in &self.additional_files {
            let additional_source = fs::read_to_string(additional_path)
                .with_context(|| format!("Failed to read file: {}", additional_path))?;

            let def = self.parse_source(&additional_source, additional_path)?;
            all_definitions.push((additional_path.clone(), def));
        }

        // Parse main file
        let input =
            fs::read_to_string(path).with_context(|| format!("Failed to read file: {}", path))?;

        let main_def = self.parse_source(&input, path)?;
        all_definitions.push((path.to_string(), main_def));

        // Compile with all definitions
        self.compile_definitions(all_definitions, &input, path)
    }

    /// Compiles multiple Modelica files together.
    ///
    /// This method compiles multiple files, merging their class definitions
    /// before flattening. The main model to simulate is specified via `.model()`.
    ///
    /// # Arguments
    ///
    /// * `paths` - Paths to the Modelica files to compile
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyPackage.MyModel")
    ///     .compile_files(&["library.mo", "model.mo"])?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_files(&self, paths: &[&str]) -> Result<CompilationResult> {
        if paths.is_empty() {
            anyhow::bail!("At least one file must be provided");
        }

        let mut all_definitions = Vec::new();
        let mut all_sources = Vec::new();

        for path in paths {
            let source = fs::read_to_string(path)
                .with_context(|| format!("Failed to read file: {}", path))?;

            let def = self.parse_source(&source, path)?;
            all_definitions.push((path.to_string(), def));
            all_sources.push((path.to_string(), source));
        }

        // Use last file as the "main" for error reporting
        let (main_path, main_source) = all_sources.last().unwrap();
        self.compile_definitions(all_definitions, main_source, main_path)
    }

    /// Parse a source file and return the StoredDefinition
    fn parse_source(&self, source: &str, file_name: &str) -> Result<StoredDefinition> {
        let mut grammar = ModelicaGrammar::new();
        if let Err(e) = parse(source, file_name, &mut grammar) {
            let diagnostic = create_syntax_error(&e, source);
            let report = miette::Report::new(diagnostic);
            return Err(anyhow::anyhow!("{:?}", report));
        }

        grammar.modelica.ok_or_else(|| {
            anyhow::anyhow!("Parser succeeded but produced no AST for {}", file_name)
        })
    }

    /// Compile from pre-parsed definitions
    fn compile_definitions(
        &self,
        definitions: Vec<(String, StoredDefinition)>,
        main_source: &str,
        main_file_name: &str,
    ) -> Result<CompilationResult> {
        use crate::ir::multi_file::merge_stored_definitions;

        let start = Instant::now();

        // Merge all definitions
        let def = if definitions.len() == 1 {
            definitions.into_iter().next().unwrap().1
        } else {
            if self.verbose {
                println!("Merging {} files...", definitions.len());
            }
            merge_stored_definitions(definitions)?
        };

        let model_hash = format!("{:x}", chksum_md5::hash(main_source));
        let parse_time = start.elapsed();

        if self.verbose {
            println!("Parsing took {} ms", parse_time.as_millis());
            println!("AST:\n{:#?}\n", def);
        }

        // Continue with the rest of the compilation pipeline
        self.compile_from_ast(def, main_source, main_file_name, model_hash, parse_time)
    }

    /// Internal: compile from a parsed AST
    fn compile_from_ast(
        &self,
        def: StoredDefinition,
        source: &str,
        _file_name: &str,
        model_hash: String,
        parse_time: std::time::Duration,
    ) -> Result<CompilationResult> {
        // Flatten
        let flatten_start = Instant::now();
        let fclass_result = flatten(&def, self.model_name.as_deref());

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

        if self.verbose {
            println!("Flattening took {} ms", flatten_time.as_millis());
            println!("Flattened class:\n{:#?}\n", fclass);
        }

        // Resolve imports - rewrite short function names to fully qualified names
        // This must happen before validation so imported names are recognized
        let mut import_resolver = ImportResolver::new(&fclass, &def);
        fclass.accept(&mut import_resolver);

        // Collect all function names from the stored definition (including nested)
        let function_names = collect_all_functions(&def);

        // Validate variable references (passing function names so they're recognized)
        let mut validator = VarValidator::with_functions(&fclass, &function_names);
        fclass.accept(&mut validator);

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

        // Inline user-defined function calls
        let mut inliner = FunctionInliner::from_class_list(&def.class_list);
        fclass.accept(&mut inliner);

        // Expand tuple equations like (a, b) = (expr1, expr2) into separate equations
        expand_tuple_equations(&mut fclass);

        if self.verbose {
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

        if self.verbose {
            println!("DAE creation took {} ms", dae_time.as_millis());
            println!("DAE:\n{:#?}\n", dae);
        }

        // Check model balance
        let balance = check_dae_balance(&dae);

        if self.verbose {
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

    /// Compiles Modelica source code from a string to a DAE representation.
    ///
    /// This method performs the full compilation pipeline on the provided source code.
    ///
    /// # Arguments
    ///
    /// * `source` - The Modelica source code to compile
    /// * `file_name` - A name to use for error reporting (can be anything)
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The Modelica code contains syntax errors
    /// - The model contains unsupported features
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let code = "model Test\n  Real x;\nequation\n  der(x) = 1;\nend Test;";
    /// let result = Compiler::new()
    ///     .model("Test")
    ///     .compile_str(code, "test.mo")?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_str(&self, source: &str, file_name: &str) -> Result<CompilationResult> {
        let def = self.parse_source(source, file_name)?;
        let definitions = vec![(file_name.to_string(), def)];
        self.compile_definitions(definitions, source, file_name)
    }
}

/// The result of a successful compilation.
///
/// Contains the compiled DAE representation along with timing information
/// and intermediate representations.
#[derive(Debug)]
pub struct CompilationResult {
    /// The compiled DAE representation
    pub dae: Dae,

    /// The parsed AST (before flattening)
    pub def: StoredDefinition,

    /// Time spent parsing
    pub parse_time: std::time::Duration,

    /// Time spent flattening
    pub flatten_time: std::time::Duration,

    /// Time spent creating DAE
    pub dae_time: std::time::Duration,

    /// MD5 hash of the source model
    pub model_hash: String,

    /// Balance check result
    pub balance: BalanceCheckResult,
}

impl CompilationResult {
    /// Returns the total compilation time.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// println!("Compiled in {} ms", result.total_time().as_millis());
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn total_time(&self) -> std::time::Duration {
        self.parse_time + self.flatten_time + self.dae_time
    }

    /// Renders the DAE using a Jinja2 template file.
    ///
    /// # Arguments
    ///
    /// * `template_path` - Path to the Jinja2 template file
    ///
    /// # Returns
    ///
    /// The rendered template as a string
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The template file cannot be read
    /// - The template contains syntax errors
    /// - The template references non-existent DAE fields
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let mut result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// result.render_template("template.j2")?; // Prints to stdout
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn render_template(&mut self, template_path: &str) -> Result<()> {
        let template_content = fs::read_to_string(template_path)
            .with_context(|| format!("Failed to read template file: {}", template_path))?;

        let template_hash = format!("{:x}", chksum_md5::hash(&template_content));
        self.dae.template_hash = template_hash;

        crate::dae::jinja::render_template(self.dae.clone(), template_path)
    }

    /// Renders the DAE using a Jinja2 template file and returns the result as a string.
    ///
    /// # Arguments
    ///
    /// * `template_path` - Path to the Jinja2 template file
    ///
    /// # Returns
    ///
    /// The rendered template as a string
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The template file cannot be read
    /// - The template contains syntax errors
    /// - The template references non-existent DAE fields
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let mut result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// let code = result.render_template_to_string("template.j2")?;
    /// println!("Generated code:\n{}", code);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn render_template_to_string(&mut self, template_path: &str) -> Result<String> {
        use minijinja::{Environment, context};

        let template_content = fs::read_to_string(template_path)
            .with_context(|| format!("Failed to read template file: {}", template_path))?;

        let template_hash = format!("{:x}", chksum_md5::hash(&template_content));
        self.dae.template_hash = template_hash.clone();

        // Use minijinja to render the template
        let mut env = Environment::new();
        env.add_function("panic", crate::dae::jinja::panic);
        env.add_function("warn", crate::dae::jinja::warn);
        env.add_template("template", &template_content)?;
        let tmpl = env.get_template("template")?;
        let output = tmpl.render(context!(dae => self.dae.clone()))?;

        Ok(output)
    }

    /// Returns a reference to the compiled DAE.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// let dae = result.dae();
    /// println!("States: {:?}", dae.x.keys());
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn dae(&self) -> &Dae {
        &self.dae
    }

    /// Returns a mutable reference to the compiled DAE.
    pub fn dae_mut(&mut self) -> &mut Dae {
        &mut self.dae
    }

    /// Returns whether the model is balanced (equations == unknowns).
    ///
    /// A balanced model has exactly as many equations as unknown variables.
    /// Models that are not balanced cannot be simulated.
    pub fn is_balanced(&self) -> bool {
        self.balance.is_balanced
    }

    /// Returns a human-readable description of the model's balance status.
    ///
    /// This includes counts of equations, unknowns, states, and algebraic variables.
    pub fn balance_status(&self) -> String {
        self.balance.status_message()
    }

    /// Exports the DAE to Base Modelica JSON using native serialization (recommended).
    ///
    /// This method provides fast, type-safe serialization to the Base Modelica IR format
    /// (MCP-0031) using Rust's serde_json library. This is the recommended approach for
    /// Base Modelica export.
    ///
    /// # Returns
    ///
    /// A pretty-printed JSON string conforming to the Base Modelica IR specification.
    ///
    /// # Errors
    ///
    /// Returns an error if serialization fails.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::Compiler;
    ///
    /// let result = Compiler::new()
    ///     .model("MyModel")
    ///     .compile_file("model.mo")?;
    /// let json = result.to_base_modelica_json()?;
    /// println!("{}", json);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn to_base_modelica_json(&self) -> Result<String> {
        self.dae
            .to_base_modelica_json()
            .context("Failed to serialize DAE to Base Modelica JSON")
    }
}

/// Recursively collects all function names from a class and its nested classes.
///
/// Returns a tuple of (full_path, ClassDefinition) for each function found.
fn collect_functions_from_class(
    class: &crate::ir::ast::ClassDefinition,
    prefix: &str,
    functions: &mut indexmap::IndexMap<String, crate::ir::ast::ClassDefinition>,
) {
    // Build the full path for this class
    let full_name = if prefix.is_empty() {
        class.name.text.clone()
    } else {
        format!("{}.{}", prefix, class.name.text)
    };

    // If this is a function, add it with full path
    if matches!(class.class_type, crate::ir::ast::ClassType::Function) {
        functions.insert(full_name.clone(), class.clone());
        // Also add short name for calls within the same package
        functions.insert(class.name.text.clone(), class.clone());
    }

    // Also register package names so they're valid in function call paths
    if matches!(class.class_type, crate::ir::ast::ClassType::Package) {
        functions.insert(full_name.clone(), class.clone());
        // Also add the short name
        functions.insert(class.name.text.clone(), class.clone());
    }

    // Recursively process nested classes
    for (_name, nested_class) in &class.classes {
        collect_functions_from_class(nested_class, &full_name, functions);
    }

    // For packages, also add relative paths for their children
    // This allows Package.function to be called from sibling classes
    if matches!(class.class_type, crate::ir::ast::ClassType::Package) {
        collect_functions_with_relative_paths(class, &class.name.text, functions);
    }
}

/// Collect functions with relative paths from a given package root.
/// This allows functions to be called with package-relative names.
fn collect_functions_with_relative_paths(
    class: &crate::ir::ast::ClassDefinition,
    relative_prefix: &str,
    functions: &mut indexmap::IndexMap<String, crate::ir::ast::ClassDefinition>,
) {
    for (_name, nested_class) in &class.classes {
        let relative_name = format!("{}.{}", relative_prefix, nested_class.name.text);

        if matches!(nested_class.class_type, crate::ir::ast::ClassType::Function) {
            functions.insert(relative_name.clone(), nested_class.clone());
        }

        // Recursively process nested packages
        if matches!(nested_class.class_type, crate::ir::ast::ClassType::Package) {
            collect_functions_with_relative_paths(nested_class, &relative_name, functions);
        }
    }
}

/// Collects all function definitions from a stored definition.
///
/// Returns a vector of function names (with their full paths for nested functions)
/// and an IndexMap mapping function paths to their definitions.
fn collect_all_functions(def: &StoredDefinition) -> Vec<String> {
    let mut functions = indexmap::IndexMap::new();
    for (_class_name, class) in &def.class_list {
        collect_functions_from_class(class, "", &mut functions);
    }
    functions.keys().cloned().collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_default() {
        let compiler = Compiler::new();
        assert!(!compiler.verbose);
    }

    #[test]
    fn test_compiler_verbose() {
        let compiler = Compiler::new().verbose(true);
        assert!(compiler.verbose);
    }

    #[test]
    fn test_compile_simple_model() {
        let source = r#"
model Integrator
    Real x(start=0);
equation
    der(x) = 1;
end Integrator;
"#;

        let result = Compiler::new()
            .model("Integrator")
            .compile_str(source, "test.mo");
        assert!(result.is_ok(), "Failed to compile: {:?}", result.err());

        let result = result.unwrap();
        assert!(!result.dae.x.is_empty(), "Should have state variables");
        assert_eq!(result.dae.x.len(), 1, "Should have exactly one state");
    }

    #[test]
    fn test_compile_requires_model_name() {
        let source = r#"
model Test
    Real x;
equation
    der(x) = 1;
end Test;
"#;

        let result = Compiler::new().compile_str(source, "test.mo");
        assert!(result.is_err(), "Should error when model name not provided");
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Model name is required"),
            "Error should mention model name is required: {}",
            err_msg
        );
    }

    #[test]
    fn test_compilation_result_total_time() {
        let source = r#"
model Test
    Real x;
equation
    der(x) = 1;
end Test;
"#;

        let result = Compiler::new()
            .model("Test")
            .compile_str(source, "test.mo")
            .unwrap();
        let total = result.total_time();
        assert!(total > std::time::Duration::from_nanos(0));
        assert_eq!(
            total,
            result.parse_time + result.flatten_time + result.dae_time
        );
    }
}
