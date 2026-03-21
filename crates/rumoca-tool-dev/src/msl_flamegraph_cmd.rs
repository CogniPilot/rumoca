use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, ensure};
use clap::{Args, ValueEnum};

use crate::{command_exists, common, exe_name, run_status};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub(crate) enum MslFlamegraphMode {
    Compile,
    Simulate,
}

impl MslFlamegraphMode {
    fn as_arg(self) -> &'static str {
        match self {
            Self::Compile => "compile",
            Self::Simulate => "simulate",
        }
    }
}

#[derive(Debug, Args, Clone)]
pub(crate) struct MslFlamegraphArgs {
    /// Fully qualified model name to profile
    #[arg(long)]
    pub(crate) model: String,

    /// Focused execution mode to profile
    #[arg(long, value_enum, default_value_t = MslFlamegraphMode::Compile)]
    pub(crate) mode: MslFlamegraphMode,

    /// Root directory of the extracted MSL release
    #[arg(long)]
    pub(crate) source_root: Option<PathBuf>,

    /// Output SVG path
    #[arg(long)]
    pub(crate) output: Option<PathBuf>,

    /// Sampling frequency in Hz
    #[arg(long, default_value_t = 99)]
    pub(crate) freq: u32,

    /// Disable inline expansion in perf script post-processing
    #[arg(long)]
    pub(crate) no_inline: bool,

    /// Optional stop time override when mode=simulate
    #[arg(long)]
    pub(crate) stop_time: Option<f64>,
}

fn sanitize_model_name(model: &str) -> String {
    model
        .chars()
        .map(|ch| match ch {
            'A'..='Z' | 'a'..='z' | '0'..='9' => ch,
            _ => '_',
        })
        .collect()
}

fn default_source_root_path() -> PathBuf {
    common::MslPaths::current().msl_dir
}

fn default_output_path(repo_root: &Path, model: &str, mode: MslFlamegraphMode) -> PathBuf {
    repo_root
        .join("target/msl/results/flamegraph")
        .join(format!(
            "{}-{}.svg",
            sanitize_model_name(model),
            mode.as_arg()
        ))
}

fn build_profile_helper(repo_root: &Path) -> Command {
    let mut build = Command::new("cargo");
    build
        .current_dir(repo_root)
        .arg("build")
        .arg("--release")
        .arg("--bin")
        .arg("rumoca-msl-profile");
    build
}

fn profile_helper_binary_path(repo_root: &Path) -> PathBuf {
    repo_root
        .join("target/release")
        .join(exe_name("rumoca-msl-profile"))
}

fn build_flamegraph_command(
    repo_root: &Path,
    helper_bin: &Path,
    source_root: &Path,
    args: &MslFlamegraphArgs,
    output: &Path,
) -> Command {
    let mut flamegraph = Command::new("flamegraph");
    flamegraph
        .current_dir(repo_root)
        .arg("--ignore-status")
        .arg("--freq")
        .arg(args.freq.to_string())
        .arg("--output")
        .arg(output);
    if args.no_inline {
        flamegraph.arg("--no-inline");
    }
    flamegraph
        .arg("--")
        .arg(helper_bin)
        .arg("--source-root")
        .arg(source_root)
        .arg("--model")
        .arg(&args.model)
        .arg("--mode")
        .arg(args.mode.as_arg());
    if let Some(stop_time) = args.stop_time {
        flamegraph.arg("--stop-time").arg(stop_time.to_string());
    }
    flamegraph
}

pub(crate) fn run(args: MslFlamegraphArgs, repo_root: &Path) -> Result<()> {
    ensure!(
        command_exists("flamegraph"),
        "flamegraph is not installed. Install cargo-flamegraph first."
    );

    let source_root = args
        .source_root
        .clone()
        .unwrap_or_else(default_source_root_path);
    ensure!(
        source_root.is_dir(),
        "MSL source-root directory does not exist: {}",
        source_root.display()
    );

    let output = args
        .output
        .clone()
        .unwrap_or_else(|| default_output_path(repo_root, &args.model, args.mode));
    if let Some(parent) = output.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }

    run_status(build_profile_helper(repo_root))?;

    let profile_bin = profile_helper_binary_path(repo_root);
    ensure!(
        profile_bin.is_file(),
        "profile helper was not built: {}",
        profile_bin.display()
    );

    run_status(build_flamegraph_command(
        repo_root,
        &profile_bin,
        &source_root,
        &args,
        &output,
    ))?;
    println!("Flamegraph written to {}", output.display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn args_to_strings(command: &Command) -> Vec<String> {
        command
            .get_args()
            .map(|arg| arg.to_string_lossy().into_owned())
            .collect()
    }

    #[test]
    fn default_output_path_uses_sanitized_model_name() {
        let repo_root = Path::new("/tmp/rumoca");
        let output = default_output_path(
            repo_root,
            "Modelica.Electrical.Digital.Examples.DFFREG",
            MslFlamegraphMode::Simulate,
        );
        assert_eq!(
            output,
            repo_root
                .join("target/msl/results/flamegraph")
                .join("Modelica_Electrical_Digital_Examples_DFFREG-simulate.svg")
        );
    }

    #[test]
    fn build_profile_helper_targets_release_helper_binary() {
        let repo_root = Path::new("/tmp/rumoca");
        let command = build_profile_helper(repo_root);
        assert_eq!(command.get_program(), "cargo");
        assert_eq!(
            args_to_strings(&command),
            vec!["build", "--release", "--bin", "rumoca-msl-profile"]
        );
        assert_eq!(
            profile_helper_binary_path(repo_root),
            repo_root
                .join("target/release")
                .join(exe_name("rumoca-msl-profile"))
        );
    }

    #[test]
    fn build_flamegraph_command_passes_profile_helper_arguments() {
        let repo_root = Path::new("/tmp/rumoca");
        let helper = repo_root.join("target/release/rumoca-msl-profile");
        let source_root = repo_root.join("target/msl/ModelicaStandardLibrary-4.1.0");
        let output = PathBuf::from("/tmp/out.svg");
        let args = MslFlamegraphArgs {
            model: "Modelica.Electrical.Digital.Examples.DFFREG".to_string(),
            mode: MslFlamegraphMode::Simulate,
            source_root: None,
            output: None,
            freq: 49,
            no_inline: true,
            stop_time: Some(0.2),
        };
        let command = build_flamegraph_command(repo_root, &helper, &source_root, &args, &output);
        let helper_arg = helper.to_string_lossy().to_string();
        let source_root_arg = source_root.to_string_lossy().to_string();
        assert_eq!(command.get_program(), "flamegraph");
        assert_eq!(
            args_to_strings(&command),
            vec![
                "--ignore-status",
                "--freq",
                "49",
                "--output",
                "/tmp/out.svg",
                "--no-inline",
                "--",
                &helper_arg,
                "--source-root",
                &source_root_arg,
                "--model",
                "Modelica.Electrical.Digital.Examples.DFFREG",
                "--mode",
                "simulate",
                "--stop-time",
                "0.2",
            ]
        );
    }
}
