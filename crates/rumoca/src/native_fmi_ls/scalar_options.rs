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

/// The model a scalar-linked package lowers, with the number of companion
/// documents. `sources_json` must be the SDK export's own workspace source set
/// (the JSON object of companion documents): it is compiled with `source` as
/// `input.mo` through the export's compile, so every document keeps the name,
/// and so every span its source id, that the saved product's component was
/// lowered with. Source roots (a JSON array) are refused by name.
pub fn compile_linked_package_source(
    source: &str,
    model: &str,
    sources_json: &str,
) -> Result<(std::sync::Arc<rumoca_compile::compile::Dae>, usize)> {
    if sources_json.trim_start().starts_with('[') {
        bail!(
            "a linked package compiles the SDK export's workspace source set: \
             SOURCES must be that JSON object, not source roots"
        );
    }
    let documents =
        serde_json::from_str::<std::collections::BTreeMap<String, String>>(sources_json)
            .context("the linked package's workspace source set")?
            .len();
    let compiled = rumoca_compile::compile::compile_source_set(source, model, sources_json)
        .map_err(anyhow::Error::msg)?;
    Ok((compiled.dae, documents))
}

/// One checked options document for a scalar package.
pub struct ScalarPackageOptions {
    options: InteractiveOptions,
    sim: SimOptions,
}

impl ScalarPackageOptions {
    /// Parse `json` with the interactive options owner. The solver must be a
    /// canonical solver name; no family alias selects one. The lowering
    /// options come from the one scalar package construction the linked
    /// export also uses, so both lower the same model the same way.
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
        self.bind(component, |policy| {
            (policy == SimExecutionPolicy::NativeRequired).then_some(
                "execution policy `native_required` is refused: this guest is INTERPRETED",
            )
        })
    }

    /// [`Self::bind_runtime_choices`] for the scalar-linked product, whose
    /// guest constructs with compiled linked entries only and admits no
    /// interpreter. An options document that asks for interpretation is
    /// refused by name; the guest's own construction, not this document,
    /// requires every operation to be compiled.
    pub fn bind_linked_runtime_choices(&self, component: FmiComponent) -> Result<FmiComponent> {
        self.bind(component, |policy| {
            (policy == SimExecutionPolicy::Interpreter).then_some(
                "execution policy `interpreter` is refused: the scalar-linked guest executes only compiled entries",
            )
        })
    }

    fn bind(
        &self,
        component: FmiComponent,
        refused_policy: impl FnOnce(SimExecutionPolicy) -> Option<&'static str>,
    ) -> Result<FmiComponent> {
        if !(self.options.atol > 0.0 && self.options.rtol > 0.0) {
            bail!("the options must state a positive atol and rtol; none is defaulted");
        }
        if let Some(refusal) = refused_policy(self.sim.execution_policy) {
            bail!("{refusal}");
        }
        if self.sim.solver_mode == SimSolverMode::FixedStep && self.options.fixed_step.is_none() {
            bail!("solver `fixed-step` needs its explicit fixed_step configuration");
        }
        let plan = rumoca_sim::co_simulation_plan(&self.sim)?;
        Ok(component.with_co_simulation(plan)?)
    }
}
