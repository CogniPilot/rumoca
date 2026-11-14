//! The inputs a scalar FMU is packaged from besides its main document: the
//! source roots it is compiled with, and the host's options document. The
//! document is read by the interactive options owner the browser session
//! uses, and its solver by the canonical solver-name owner; every runtime
//! choice becomes the component's own step plan or is refused by name.

use anyhow::{Context, Result, bail};
use rumoca_ir_solve::fmi::FmiComponent;
use rumoca_sim::interactive_options::InteractiveOptions;
use rumoca_sim::{SimExecutionPolicy, SimOptions, SimSolverMode};
use std::{collections::BTreeSet, fs, path::Path};

/// The source roots listed in the JSON array at `path`: each an existing
/// absolute path, listed once after canonicalisation.
pub fn read_source_roots(path: &Path) -> Result<Vec<String>> {
    let text = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
    let roots: Vec<String> = serde_json::from_str(&text)
        .with_context(|| format!("{} is not a JSON array of paths", path.display()))?;
    let mut seen = BTreeSet::new();
    for root in &roots {
        let root_path = Path::new(root);
        if !root_path.is_absolute() || !root_path.exists() {
            bail!("source root `{root}` is not an existing absolute path");
        }
        if !seen.insert(fs::canonicalize(root_path)?) {
            bail!("source root `{root}` is listed twice");
        }
    }
    Ok(roots)
}

/// One checked options document for a scalar package.
pub struct ScalarPackageOptions {
    options: InteractiveOptions,
    sim: SimOptions,
}

impl ScalarPackageOptions {
    /// Parse `json` with the interactive options owner. The solver must be a
    /// canonical solver name; no family alias selects one.
    pub fn parse(json: &str) -> Result<Self> {
        let options = InteractiveOptions::parse(json)?;
        let sim = rumoca_sim::scalar_package_sim_options(&options)?;
        Ok(Self { options, sim })
    }

    /// The options lowering reads: parameter overrides and initial inputs.
    pub fn sim_options(&self) -> &SimOptions {
        &self.sim
    }

    /// Bind the runtime choices to `component` as its step plan: the solver,
    /// its configuration and both tolerances, through the backend that owns
    /// them. Refused by name: an absent tolerance or fixed-step
    /// configuration (none is defaulted), a policy that requires compiled
    /// code (the guest is INTERPRETED), and any request no step plan carries.
    pub fn bind_runtime_choices(&self, component: FmiComponent) -> Result<FmiComponent> {
        if !(self.options.atol > 0.0 && self.options.rtol > 0.0) {
            bail!("the options must state a positive atol and rtol; none is defaulted");
        }
        if self.sim.execution_policy == SimExecutionPolicy::NativeRequired {
            bail!("execution policy `native_required` is refused: this guest is INTERPRETED");
        }
        if self.sim.solver_mode == SimSolverMode::FixedStep && self.options.fixed_step.is_none() {
            bail!("solver `fixed-step` needs its explicit fixed_step configuration");
        }
        let plan = rumoca_sim::co_simulation_plan(&self.sim)?;
        Ok(component.with_co_simulation(plan)?)
    }

    /// [`Self::bind_runtime_choices`] for the compiled (scalar-linked)
    /// product, whose guest admits only compiled execution: the interpreter
    /// policy is refused instead of `native_required`.
    pub fn bind_linked_runtime_choices(&self, component: FmiComponent) -> Result<FmiComponent> {
        if !(self.options.atol > 0.0 && self.options.rtol > 0.0) {
            bail!("the options must state a positive atol and rtol; none is defaulted");
        }
        if self.sim.execution_policy == SimExecutionPolicy::Interpreter {
            bail!("execution policy `interpreter` is refused: this guest is COMPILED");
        }
        if self.sim.solver_mode == SimSolverMode::FixedStep && self.options.fixed_step.is_none() {
            bail!("solver `fixed-step` needs its explicit fixed_step configuration");
        }
        let plan = rumoca_sim::co_simulation_plan(&self.sim)?;
        Ok(component.with_co_simulation(plan)?)
    }
}
