//! Compilation result type and template rendering.
//!
//! This module contains the [`CompilationResult`] struct which holds
//! the output of a successful compilation, including the DAE representation
//! and timing information.

use crate::dae::ast::Dae;
use crate::dae::balance::BalanceResult;
use crate::ir::ast::{ClassDefinition, StoredDefinition};
use anyhow::{Context, Result};
use std::fs;

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

    /// The expanded class (after flattening and import resolution, before DAE creation)
    /// Used for semantic analysis (undefined variables, unused variables, etc.)
    pub expanded_class: ClassDefinition,

    /// Time spent parsing
    pub parse_time: std::time::Duration,

    /// Time spent flattening
    pub flatten_time: std::time::Duration,

    /// Time spent creating DAE
    pub dae_time: std::time::Duration,

    /// MD5 hash of the source model
    pub model_hash: String,

    /// Balance check result
    pub balance: BalanceResult,
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

        crate::dae::jinja::render_template(&self.dae, template_path)
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
        let output = tmpl.render(context!(dae => &self.dae))?;

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
        self.balance.is_balanced()
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
    /// let json = result.to_dae_ir_json()?;
    /// println!("{}", json);
    /// # Ok::<(), anyhow::Error>(())
    /// ```
    pub fn to_dae_ir_json(&self) -> Result<String> {
        self.dae
            .to_dae_ir_json()
            .context("Failed to serialize DAE to DAE IR JSON")
    }
}
