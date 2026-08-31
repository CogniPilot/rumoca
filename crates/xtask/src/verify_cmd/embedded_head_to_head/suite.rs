use std::time::Instant;

use super::*;

pub(super) fn measure_row(root: &Path, row: &BoundRow<'_>, artifact_root: &Path) -> Verdict {
    let started = Instant::now();
    match measure(root, row, artifact_root) {
        Ok(evidence) => judge(row.entry, evidence, started.elapsed().as_secs_f64()),
        Err(failure) => Verdict::unmeasured(row.entry, failure, started.elapsed().as_secs_f64()),
    }
}

fn measure(
    root: &Path,
    bound: &BoundRow<'_>,
    artifact_root: &Path,
) -> std::result::Result<Evidence, SuiteFailure> {
    let mut state = SuiteState::new(root, bound, artifact_root);
    let mut cursor = cross::start_suite(&bound.plan);
    loop {
        match cursor.next() {
            cross::SuiteProgress::Pending(pending) => {
                match cross::execute_suite_step(pending, |step| state.execute(step)) {
                    Ok((next, executed, ())) => {
                        state.completed_steps.push(executed);
                        cursor = next;
                    }
                    Err(error) => return Err(state.fail(error)),
                }
            }
            cross::SuiteProgress::Complete(proof) => {
                let partial = state.partial();
                return state
                    .complete(proof)
                    .and_then(|suite| Evidence::from_completed(bound.entry, suite))
                    .map_err(|error| SuiteFailure { error, partial });
            }
        }
    }
}

pub(super) struct SuiteFailure {
    pub(super) error: anyhow::Error,
    pub(super) partial: PartialSuiteEvidence,
}

impl SuiteFailure {
    pub(super) fn from_partial(error: anyhow::Error, mut partial: PartialSuiteEvidence) -> Self {
        partial
            .commands
            .extend_from_slice(process::attempted_receipts(&error));
        Self { error, partial }
    }
}

#[derive(Clone, Serialize)]
pub(super) struct PartialSuiteEvidence {
    pub(super) completed_steps: Vec<cross::ExecutedSuiteStep>,
    pub(super) commands: Vec<process::CommandReceipt>,
}

struct SuiteState<'a> {
    root: &'a Path,
    entry: &'a Entry,
    plan: &'a cross::BoundExecutionPlan,
    row: PathBuf,
    artifact_root: PathBuf,
    bundle_destination: PathBuf,
    inputs: Option<snapshot::AuthenticatedInputs>,
    oracle: Option<RolePath<OracleInclude>>,
    oracle_guard: Option<artifact_guard::FrozenArtifactSet>,
    rumoca: Option<emit::RumocaEmission>,
    casadi: Option<emit::CasadiEmission>,
    rumoca_link: Option<cross::LinkedArtifact>,
    casadi_link: Option<cross::LinkedArtifact>,
    rumoca_trace: Option<qemu::TraceMeasurement>,
    casadi_trace: Option<qemu::TraceMeasurement>,
    correctness_output_lines: Vec<String>,
    completed_steps: Vec<cross::ExecutedSuiteStep>,
    commands: Vec<process::CommandReceipt>,
}

pub(super) struct CompletedSuite {
    pub(super) _proof: cross::SuiteCompletionProof,
    pub(super) suite_sha256: String,
    pub(super) artifact_root: PathBuf,
    pub(super) bundle_destination: PathBuf,
    pub(super) inputs: snapshot::AuthenticatedInputs,
    pub(super) oracle: RolePath<OracleInclude>,
    pub(super) oracle_guard: artifact_guard::FrozenArtifactSet,
    pub(super) rumoca: emit::RumocaEmission,
    pub(super) casadi: emit::CasadiEmission,
    pub(super) rumoca_link: cross::LinkedArtifact,
    pub(super) casadi_link: cross::LinkedArtifact,
    pub(super) rumoca_trace: qemu::TraceMeasurement,
    pub(super) casadi_trace: qemu::TraceMeasurement,
    pub(super) correctness_output_lines: Vec<String>,
    pub(super) commands: Vec<process::CommandReceipt>,
}

macro_rules! execute_bound_suite_operation {
    ($state:ident, $step:ident; $($variant:ident => $id:literal => $executor:ident),+ $(,)?) => {
        match $step {
            $(cross::SuiteStep::$variant => $state.$executor()),+
        }
    };
}

impl<'a> SuiteState<'a> {
    fn new(root: &'a Path, bound: &'a BoundRow<'_>, artifact_root: &Path) -> Self {
        Self {
            root,
            entry: bound.entry,
            plan: &bound.plan,
            row: artifact_root.join("work/rows").join(&bound.entry.id),
            artifact_root: artifact_root.to_path_buf(),
            bundle_destination: artifact_root
                .join("evidence-bundle/rows")
                .join(&bound.entry.id),
            inputs: None,
            oracle: None,
            oracle_guard: None,
            rumoca: None,
            casadi: None,
            rumoca_link: None,
            casadi_link: None,
            rumoca_trace: None,
            casadi_trace: None,
            correctness_output_lines: Vec::new(),
            completed_steps: Vec::new(),
            commands: Vec::new(),
        }
    }

    fn partial(&self) -> PartialSuiteEvidence {
        PartialSuiteEvidence {
            completed_steps: self.completed_steps.clone(),
            commands: self.commands.clone(),
        }
    }

    fn fail(&self, error: anyhow::Error) -> SuiteFailure {
        SuiteFailure::from_partial(error, self.partial())
    }

    fn execute(&mut self, step: cross::SuiteStep) -> Result<()> {
        embedded_suite_operations!(execute_bound_suite_operation, self, step)
    }

    fn context(&self) -> Result<BuildContext<'_>> {
        Ok(BuildContext {
            plan: self.plan,
            harness: self
                .inputs
                .as_ref()
                .context("suite used inputs before staging")?
                .harness(),
            oracle: self
                .oracle
                .as_ref()
                .context("suite used oracle before construction")?,
            oracle_guard: self
                .oracle_guard
                .as_ref()
                .context("suite used oracle guard before construction")?,
        })
    }

    fn stage_inputs(&mut self) -> Result<()> {
        self.inputs = Some(snapshot::stage(
            self.root,
            self.entry,
            &self.row.join("authenticated-inputs"),
        )?);
        Ok(())
    }

    fn write_oracle(&mut self) -> Result<()> {
        let oracle = self.row.join("oracle");
        fs::create_dir_all(&oracle)?;
        write_oracle(&oracle, &self.entry.correctness_cases)?;
        self.oracle_guard = Some(artifact_guard::FrozenArtifactSet::capture_tree(&oracle)?);
        self.oracle = Some(RolePath::checked(oracle)?);
        Ok(())
    }

    fn generate_rumoca(&mut self) -> Result<()> {
        let cache = RolePath::<RumocaCacheDirectory>::checked(self.row.join("cache"))?;
        let emission = emit::emit_rumoca(
            self.inputs
                .as_ref()
                .context("Rumoca generation lacks inputs")?,
            self.plan,
            &self.row.join("emitted/rumoca"),
            &cache,
        )?;
        self.commands.push(emission.command.clone());
        self.rumoca = Some(emission);
        Ok(())
    }

    fn generate_casadi(&mut self) -> Result<()> {
        let emission = emit::emit_casadi(
            self.inputs
                .as_ref()
                .context("CasADi generation lacks inputs")?,
            self.plan,
            &self.row.join("emitted/casadi"),
        )?;
        self.commands.push(emission.command.clone());
        self.casadi = Some(emission);
        Ok(())
    }

    fn validate_counter(
        &mut self,
        recipe: cross::CounterRecipe,
        expected: u64,
        stem: &str,
    ) -> Result<()> {
        let linked = cross::build_fixture(recipe, &self.row.join(stem), &self.context()?)?;
        self.commands.extend(linked.commands.clone());
        self.commands.push(qemu::validate_counter(
            self.plan, &linked, expected, &self.row,
        )?);
        Ok(())
    }

    fn counter_straight(&mut self) -> Result<()> {
        self.validate_counter(cross::CounterRecipe::Straight, 5, "acceptance-straight")
    }

    fn counter_called_leaf(&mut self) -> Result<()> {
        self.validate_counter(
            cross::CounterRecipe::CalledLeaf,
            7,
            "acceptance-called-leaf",
        )
    }

    fn validate_exit_status(&mut self) -> Result<()> {
        let linked = cross::build_fixture(
            cross::CounterRecipe::ExitStatus,
            &self.row.join("acceptance-exit-status"),
            &self.context()?,
        )?;
        self.commands.extend(linked.commands.clone());
        self.commands.push(qemu::validate_exit_status(
            self.plan, &linked, 7, &self.row,
        )?);
        Ok(())
    }

    fn mutation_deleted_call(&mut self) -> Result<()> {
        let commands = validate_measured_call_required(
            self.plan,
            &self.context()?,
            &self.row,
            self.rumoca
                .as_ref()
                .context("deleted-call mutation lacks Rumoca emission")?,
            &self.entry.correctness_cases,
        )?;
        self.commands.extend(commands);
        Ok(())
    }

    fn mutation_constant_output(&mut self) -> Result<()> {
        let commands = validate_constant_output_mutation(
            self.plan,
            &self.context()?,
            &self.row,
            self.rumoca
                .as_ref()
                .context("constant-output mutation lacks Rumoca emission")?,
            &self.entry.correctness_cases,
        )?;
        self.commands.extend(commands);
        Ok(())
    }

    fn mutation_closed_branches(&mut self) -> Result<()> {
        let commands = validate_closed_branch_mutation(
            self.plan,
            &self.context()?,
            &self.row,
            self.rumoca
                .as_ref()
                .context("closed-branch mutation lacks Rumoca emission")?,
            &self.entry.correctness_cases,
        )?;
        self.commands.extend(commands);
        Ok(())
    }

    fn mutation_casadi_row_major(&mut self) -> Result<()> {
        let commands = validate_casadi_layout_mutation(
            self.plan,
            &self.context()?,
            &self.row,
            self.casadi
                .as_ref()
                .context("row-major mutation lacks CasADi emission")?,
            &self.entry.correctness_cases,
        )?;
        self.commands.extend(commands);
        Ok(())
    }

    fn correctness_rumoca(&mut self) -> Result<()> {
        let linked = cross::build_rumoca_correctness(
            self.rumoca
                .as_ref()
                .context("Rumoca correctness lacks emission")?,
            &self.row.join("build-correctness-rumoca"),
            &self.context()?,
        )?;
        self.commands.extend(linked.commands.clone());
        let trace = cross::measure_guest_instructions(
            self.plan,
            &linked,
            "rumoca_exp_mixed",
            &self.entry.correctness_cases,
            &self.row,
        )?;
        self.commands.push(trace.command.clone());
        self.correctness_output_lines.extend(trace.output_lines);
        Ok(())
    }

    fn correctness_casadi(&mut self) -> Result<()> {
        let linked = cross::build_casadi_correctness(
            self.casadi
                .as_ref()
                .context("CasADi correctness lacks emission")?,
            &self.row.join("build-correctness-casadi"),
            &self.context()?,
        )?;
        self.commands.extend(linked.commands.clone());
        let trace = cross::measure_guest_instructions(
            self.plan,
            &linked,
            "casadi_exp_mixed",
            &self.entry.correctness_cases,
            &self.row,
        )?;
        self.commands.push(trace.command.clone());
        self.correctness_output_lines.extend(trace.output_lines);
        Ok(())
    }

    fn build_rumoca_measured(&mut self) -> Result<()> {
        self.rumoca_link = Some(cross::build_rumoca(
            self.rumoca
                .as_ref()
                .context("Rumoca measured build lacks emission")?,
            &self.row.join("build-rumoca"),
            &self.context()?,
        )?);
        self.commands.extend(
            self.rumoca_link
                .as_ref()
                .expect("just stored Rumoca link")
                .commands
                .clone(),
        );
        Ok(())
    }

    fn build_casadi_measured(&mut self) -> Result<()> {
        self.casadi_link = Some(cross::build_casadi(
            self.casadi
                .as_ref()
                .context("CasADi measured build lacks emission")?,
            &self.row.join("build-casadi"),
            &self.context()?,
        )?);
        self.commands.extend(
            self.casadi_link
                .as_ref()
                .expect("just stored CasADi link")
                .commands
                .clone(),
        );
        Ok(())
    }

    fn measure_rumoca(&mut self) -> Result<()> {
        let trace = cross::measure_guest_instructions(
            self.plan,
            self.rumoca_link
                .as_ref()
                .context("Rumoca measurement lacks linked artifact")?,
            "rumoca_exp_mixed",
            &self.entry.correctness_cases[..1],
            &self.row,
        )?;
        self.commands.push(trace.command.clone());
        self.rumoca_trace = Some(trace);
        Ok(())
    }

    fn measure_casadi(&mut self) -> Result<()> {
        let trace = cross::measure_guest_instructions(
            self.plan,
            self.casadi_link
                .as_ref()
                .context("CasADi measurement lacks linked artifact")?,
            "casadi_exp_mixed",
            &self.entry.correctness_cases[..1],
            &self.row,
        )?;
        self.commands.push(trace.command.clone());
        self.casadi_trace = Some(trace);
        Ok(())
    }

    fn complete(self, proof: cross::SuiteCompletionProof) -> Result<CompletedSuite> {
        let suite_sha256 = proof.digest().to_string();
        Ok(CompletedSuite {
            _proof: proof,
            suite_sha256,
            artifact_root: self.artifact_root,
            bundle_destination: self.bundle_destination,
            inputs: self.inputs.context("completed suite lacks staged inputs")?,
            oracle: self.oracle.context("completed suite lacks oracle")?,
            oracle_guard: self
                .oracle_guard
                .context("completed suite lacks oracle guard")?,
            rumoca: self
                .rumoca
                .context("completed suite lacks Rumoca emission")?,
            casadi: self
                .casadi
                .context("completed suite lacks CasADi emission")?,
            rumoca_link: self
                .rumoca_link
                .context("completed suite lacks Rumoca measured link")?,
            casadi_link: self
                .casadi_link
                .context("completed suite lacks CasADi measured link")?,
            rumoca_trace: self
                .rumoca_trace
                .context("completed suite lacks Rumoca measurement")?,
            casadi_trace: self
                .casadi_trace
                .context("completed suite lacks CasADi measurement")?,
            correctness_output_lines: self.correctness_output_lines,
            commands: self.commands,
        })
    }
}
