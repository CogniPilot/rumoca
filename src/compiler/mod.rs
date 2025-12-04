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

mod error_handling;
mod function_collector;
mod pipeline;
mod result;

pub use result::CompilationResult;

use crate::ir::ast::StoredDefinition;
use crate::modelica_grammar::ModelicaGrammar;
use crate::modelica_parser::parse;
use anyhow::{Context, Result};
use error_handling::create_syntax_error;
use std::fs;
use std::time::Instant;

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
        use crate::ir::transform::multi_file::discover_modelica_files;

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
        use crate::ir::transform::multi_file::find_package_in_modelica_path;

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
        use crate::ir::transform::multi_file::discover_modelica_files;

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
        _main_file_name: &str,
    ) -> Result<CompilationResult> {
        use crate::ir::transform::multi_file::merge_stored_definitions;

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

        // Run the compilation pipeline
        pipeline::compile_from_ast(
            def,
            main_source,
            self.model_name.as_deref(),
            model_hash,
            parse_time,
            self.verbose,
        )
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

    /// Compiles from a pre-parsed StoredDefinition.
    ///
    /// This method is useful when you have already parsed the Modelica code
    /// and want to avoid re-parsing it. This is especially beneficial when
    /// compiling multiple models from the same source files (e.g., testing).
    ///
    /// # Arguments
    ///
    /// * `def` - The pre-parsed StoredDefinition
    /// * `source` - The original source code (for error reporting)
    ///
    /// # Returns
    ///
    /// A [`CompilationResult`] containing the DAE and metadata
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use rumoca::{Compiler, ir::ast::StoredDefinition};
    /// use rumoca::modelica_grammar::ModelicaGrammar;
    /// use rumoca::modelica_parser::parse;
    ///
    /// // Parse once
    /// let source = "model Test\n  Real x;\nequation\n  der(x) = 1;\nend Test;";
    /// let mut grammar = ModelicaGrammar::new();
    /// parse(source, "test.mo", &mut grammar).unwrap();
    /// let def = grammar.modelica.unwrap();
    ///
    /// // Compile multiple times without re-parsing
    /// let result = Compiler::new()
    ///     .model("Test")
    ///     .compile_parsed(def.clone(), source)?;
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn compile_parsed(&self, def: StoredDefinition, source: &str) -> Result<CompilationResult> {
        let model_hash = format!("{:x}", chksum_md5::hash(source));
        pipeline::compile_from_ast(
            def,
            source,
            self.model_name.as_deref(),
            model_hash,
            std::time::Duration::ZERO, // No parse time for pre-parsed
            self.verbose,
        )
    }

    /// Compiles from a reference to a pre-parsed StoredDefinition.
    ///
    /// This is more efficient than `compile_parsed` when compiling many models
    /// from the same AST because it avoids cloning the StoredDefinition during
    /// the compilation process. The def is only cloned once at the end.
    pub fn compile_parsed_ref(
        &self,
        def: &StoredDefinition,
        source: &str,
    ) -> Result<CompilationResult> {
        let model_hash = format!("{:x}", chksum_md5::hash(source));
        pipeline::compile_from_ast_ref(
            def,
            source,
            self.model_name.as_deref(),
            model_hash,
            std::time::Duration::ZERO,
            self.verbose,
        )
    }

    /// Performs a lightweight balance check only, without full compilation.
    ///
    /// This is much faster than full compilation when you only need to check
    /// if a model is balanced. It skips the StoredDefinition cloning entirely.
    pub fn check_balance(
        &self,
        def: &StoredDefinition,
    ) -> Result<crate::ir::analysis::balance_check::BalanceCheckResult> {
        pipeline::check_balance_only(def, self.model_name.as_deref())
    }
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
