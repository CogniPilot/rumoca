//! Typed, immutable source-generation invocations.
//!
//! A bound argument owns both its canonical identity and its exact runtime
//! value. The normalized profile digest and the executed argv are projections
//! of the same retained invocation, so a manifest value cannot be rebound
//! after the row has been authenticated.

use super::update_field;
use crate::verify_cmd::embedded_head_to_head::manifest::{
    ComparatorVersionArg, Entry, ModelArg, TargetArg,
};
use crate::verify_cmd::embedded_head_to_head::process;
use crate::verify_cmd::embedded_head_to_head::typed_path::{
    CasadiEmissionInclude, ComparatorGenerator, CompilerExecutable, ExecutableRole, FixtureSource,
    PythonExecutable, Role, RolePath, RumocaCacheDirectory, RumocaEmissionInclude,
};
use anyhow::{Result, ensure};
use sha2::Sha256;
use std::ffi::OsString;
use std::marker::PhantomData;

trait GenerationSlot {
    const LABEL: &'static str;
    const ID: &'static str;
}

trait ManifestGenerationSlot: GenerationSlot {
    type Value;
    fn encode(value: Self::Value) -> OsString;
}

impl<R: Role> GenerationSlot for R {
    const LABEL: &'static str = "path-role";
    const ID: &'static str = R::ID;
}

macro_rules! manifest_slots {
    ($($name:ident => $id:literal),+ $(,)?) => {
        $(
            enum $name {}
            impl GenerationSlot for $name {
                const LABEL: &'static str = "manifest-literal";
                const ID: &'static str = $id;
            }
        )+
    };
}

manifest_slots! {
    ModelSlot => "model",
    TargetSlot => "target",
    ComparatorVersionSlot => "comparator-version",
}

impl ManifestGenerationSlot for ModelSlot {
    type Value = ModelArg;

    fn encode(value: Self::Value) -> OsString {
        value.into_string().into()
    }
}

impl ManifestGenerationSlot for TargetSlot {
    type Value = TargetArg;

    fn encode(value: Self::Value) -> OsString {
        value.into_string().into()
    }
}

impl ManifestGenerationSlot for ComparatorVersionSlot {
    type Value = ComparatorVersionArg;

    fn encode(value: Self::Value) -> OsString {
        value.into_string().into()
    }
}

struct BoundValue<R: GenerationSlot> {
    actual: OsString,
    role: PhantomData<R>,
}

impl<R: GenerationSlot> BoundValue<R> {
    fn encoded(actual: OsString) -> Self {
        Self {
            actual,
            role: PhantomData,
        }
    }

    fn normalized() -> Self {
        Self::encoded(OsString::new())
    }

    fn erase(self) -> BoundGenerationArg {
        BoundGenerationArg {
            canonical_label: R::LABEL,
            canonical_value: R::ID,
            actual: self.actual,
        }
    }
}

impl<R: ManifestGenerationSlot> BoundValue<R> {
    fn manifest(value: R::Value) -> Self {
        Self::encoded(R::encode(value))
    }
}

#[derive(Clone)]
struct BoundGenerationArg {
    canonical_label: &'static str,
    canonical_value: &'static str,
    actual: OsString,
}

impl BoundGenerationArg {
    fn literal(value: &'static str) -> Self {
        Self {
            canonical_label: "literal",
            canonical_value: value,
            actual: value.into(),
        }
    }
}

#[derive(Clone)]
struct BoundGenerationInvocation {
    id: &'static str,
    tool_role: &'static str,
    args: Vec<BoundGenerationArg>,
}

pub(in crate::verify_cmd::embedded_head_to_head::cross) struct BoundGenerationPlan {
    rumoca: BoundGenerationInvocation,
    casadi: BoundGenerationInvocation,
}

impl BoundGenerationInvocation {
    fn bind_path<R: Role>(&mut self, value: &RolePath<R>) -> Result<()> {
        let mut matches = self
            .args
            .iter_mut()
            .filter(|arg| arg.canonical_label == "path-role" && arg.canonical_value == R::ID)
            .collect::<Vec<_>>();
        ensure!(
            matches.len() == 1,
            "{} has {} bindings for dynamic role {}",
            self.id,
            matches.len(),
            R::ID,
        );
        matches[0].actual = value.as_path().as_os_str().to_owned();
        Ok(())
    }

    fn update_digest(&self, digest: &mut Sha256) {
        update_field(digest, "invocation", self.id);
        update_field(digest, "tool-role", self.tool_role);
        for arg in &self.args {
            update_field(digest, arg.canonical_label, arg.canonical_value);
            if arg.canonical_label == "manifest-literal" {
                update_field(
                    digest,
                    "manifest-value",
                    arg.actual
                        .to_str()
                        .expect("validated manifest literals are UTF-8"),
                );
            }
        }
    }

    fn command<R: ExecutableRole>(
        self,
        tool: &RolePath<R>,
        runtime_environment: Option<&process::CompilerRuntimeEnvironment>,
    ) -> Result<process::HermeticCommand> {
        ensure!(
            self.tool_role == R::ID,
            "{} tool role {} was rebound to {}",
            self.id,
            self.tool_role,
            R::ID
        );
        ensure!(
            self.args.iter().all(|arg| {
                arg.canonical_label != "path-role" || !arg.actual.as_encoded_bytes().is_empty()
            }),
            "{} retains an unbound dynamic path role",
            self.id
        );
        let mut command = process::HermeticCommand::new(tool);
        if let Some(environment) = runtime_environment {
            command.compiler_runtime_environment(environment);
        }
        command.args(self.args.iter().map(|arg| &arg.actual));
        Ok(command)
    }
}

impl BoundGenerationPlan {
    pub(super) fn normalized(entry: &Entry) -> Result<Self> {
        Ok(Self {
            rumoca: normalized_rumoca_generation(<CompilerExecutable as Role>::ID, entry)?,
            casadi: normalized_casadi_generation(entry)?,
        })
    }

    pub(super) fn update_digest(&self, digest: &mut Sha256) {
        self.rumoca.update_digest(digest);
        self.casadi.update_digest(digest);
    }

    pub(super) fn rumoca_command(
        &self,
        compiler: &RolePath<CompilerExecutable>,
        runtime_environment: &process::CompilerRuntimeEnvironment,
        fixture: &RolePath<FixtureSource>,
        cache: &RolePath<RumocaCacheDirectory>,
        output: &RolePath<RumocaEmissionInclude>,
    ) -> Result<process::HermeticCommand> {
        let mut invocation = self.rumoca.clone();
        invocation.bind_path(fixture)?;
        invocation.bind_path(cache)?;
        invocation.bind_path(output)?;
        invocation.command(compiler, Some(runtime_environment))
    }

    pub(super) fn casadi_command(
        &self,
        python: &RolePath<PythonExecutable>,
        generator: &RolePath<ComparatorGenerator>,
        output: &RolePath<CasadiEmissionInclude>,
    ) -> Result<process::HermeticCommand> {
        let mut invocation = self.casadi.clone();
        invocation.bind_path(generator)?;
        invocation.bind_path(output)?;
        invocation.command(python, None)
    }

    #[cfg(test)]
    pub(super) fn with_compiler_role(entry: &Entry, compiler_role: &'static str) -> Self {
        let mut plan = Self::normalized(entry).expect("executable test row");
        plan.rumoca.tool_role = compiler_role;
        plan
    }

    #[cfg(test)]
    pub(super) fn mutated(entry: &Entry, mutation: TestMutation) -> Self {
        let mut plan = Self::normalized(entry).expect("executable test row");
        match mutation {
            TestMutation::ReorderRumoca => plan.rumoca.args.swap(0, 1),
            TestMutation::OmitCasadi => {
                plan.casadi.args.remove(1);
            }
            TestMutation::RebindRumocaTarget => {
                plan.rumoca.args[5].actual = "galec".into();
            }
        }
        plan
    }
}

fn rumoca_generation_invocation(
    compiler_role: &'static str,
    fixture: BoundValue<FixtureSource>,
    model: BoundValue<ModelSlot>,
    target: BoundValue<TargetSlot>,
    cache: BoundValue<RumocaCacheDirectory>,
    output: BoundValue<RumocaEmissionInclude>,
) -> BoundGenerationInvocation {
    BoundGenerationInvocation {
        id: "Rumoca emission",
        tool_role: compiler_role,
        args: vec![
            BoundGenerationArg::literal("compile"),
            fixture.erase(),
            BoundGenerationArg::literal("--model"),
            model.erase(),
            BoundGenerationArg::literal("--target"),
            target.erase(),
            BoundGenerationArg::literal("--cache-dir"),
            cache.erase(),
            BoundGenerationArg::literal("--output"),
            output.erase(),
        ],
    }
}

fn casadi_generation_invocation(
    generator: BoundValue<ComparatorGenerator>,
    output: BoundValue<CasadiEmissionInclude>,
    expected_version: BoundValue<ComparatorVersionSlot>,
) -> BoundGenerationInvocation {
    BoundGenerationInvocation {
        id: "CasADi comparator generation",
        tool_role: <PythonExecutable as Role>::ID,
        args: vec![
            generator.erase(),
            BoundGenerationArg::literal("--out"),
            output.erase(),
            BoundGenerationArg::literal("--expected-version"),
            expected_version.erase(),
        ],
    }
}

fn normalized_rumoca_generation(
    compiler_role: &'static str,
    entry: &Entry,
) -> Result<BoundGenerationInvocation> {
    let bindings = entry.generation_bindings()?;
    Ok(rumoca_generation_invocation(
        compiler_role,
        BoundValue::normalized(),
        BoundValue::manifest(bindings.model),
        BoundValue::manifest(bindings.target),
        BoundValue::normalized(),
        BoundValue::normalized(),
    ))
}

fn normalized_casadi_generation(entry: &Entry) -> Result<BoundGenerationInvocation> {
    let bindings = entry.generation_bindings()?;
    Ok(casadi_generation_invocation(
        BoundValue::normalized(),
        BoundValue::normalized(),
        BoundValue::manifest(bindings.comparator_version),
    ))
}

#[cfg(test)]
#[derive(Clone, Copy)]
pub(super) enum TestMutation {
    ReorderRumoca,
    OmitCasadi,
    RebindRumocaTarget,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn checked_entry() -> Entry {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        let raw = std::fs::read_to_string(
            crate::verify_cmd::embedded_head_to_head::manifest::path(&root),
        )
        .unwrap();
        let manifest: serde_json::Value = serde_json::from_str(&raw).unwrap();
        serde_json::from_value(manifest["entries"][0].clone()).unwrap()
    }

    #[test]
    fn command_argv_contains_only_current_compiler_inputs() {
        let entry = checked_entry();
        let plan = BoundGenerationPlan::normalized(&entry).unwrap();
        let environment = process::CompilerRuntimeEnvironment::capture();
        let command = plan
            .rumoca_command(
                &RolePath::normalized(),
                &environment,
                &RolePath::normalized(),
                &RolePath::normalized(),
                &RolePath::normalized(),
            )
            .unwrap();
        let args = command
            .get_args()
            .map(|arg| arg.to_string_lossy().into_owned())
            .collect::<Vec<_>>();
        assert_eq!(args.len(), 10);
        for expected in [["--model", "ExpMixedStep"], ["--target", "efmu"]] {
            assert!(args.windows(2).any(|window| window == expected));
        }
    }

    #[test]
    fn omitted_dynamic_path_binding_cannot_become_argv() {
        let plan = BoundGenerationPlan::normalized(&checked_entry()).unwrap();
        let mut invocation = plan.rumoca.clone();
        invocation
            .bind_path(&RolePath::<FixtureSource>::normalized())
            .unwrap();
        invocation
            .bind_path(&RolePath::<RumocaCacheDirectory>::normalized())
            .unwrap();
        let environment = process::CompilerRuntimeEnvironment::capture();
        let error = invocation
            .command(
                &RolePath::<CompilerExecutable>::normalized(),
                Some(&environment),
            )
            .err()
            .expect("unbound output path must prevent command construction");
        assert!(error.to_string().contains("unbound dynamic path role"));
    }
}
