use super::{
    Cli, Commands, CoverageCommand, PythonCommand, RepoCliCommand, RepoCommand, RepoHooksCommand,
    RepoMslCommand, RepoPolicyCommand, VscodeCommand, WasmCommand, classify_candidate,
};
use crate::repo_cli_cmd::{
    ShellKind, cargo_install_rum_args, completion_install_plan, detect_shell_kind,
    path_var_contains_dir, shell_path_update_guidance, shell_profile_update,
};
use crate::verify_cmd::VerifyCommand;
use clap::CommandFactory;
use clap::Parser;
use clap::error::ErrorKind;
use std::env;
use std::ffi::OsStr;
use std::path::Path;

#[test]
fn classify_keeps_single_use_private_helper() {
    let label = classify_candidate("private", false, Some(2), 2);
    assert_eq!(label, "single_use_helper_keep");
}

#[test]
fn classify_marks_zero_callsite_private_as_dead_likely() {
    let label = classify_candidate("private", false, Some(1), 1);
    assert_eq!(label, "dead_likely");
}

#[test]
fn cli_parses_verify_quick_job() {
    let cli = Cli::try_parse_from(["rum", "verify", "quick"]).expect("parse verify quick");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::Quick),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_full_job() {
    let cli = Cli::try_parse_from(["rum", "verify", "full"]).expect("parse verify full");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::Full),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_rejects_removed_ci_command() {
    let error = Cli::try_parse_from(["rum", "ci", "verify"]).expect_err("ci should be removed");
    assert_eq!(error.kind(), ErrorKind::InvalidSubcommand);
}

#[test]
fn cli_parses_verify_workspace_job() {
    let cli = Cli::try_parse_from(["rum", "verify", "workspace"]).expect("parse verify workspace");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::Workspace),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_template_runtimes_job() {
    let cli = Cli::try_parse_from(["rum", "verify", "template-runtimes"])
        .expect("parse verify template-runtimes");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::TemplateRuntimes),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_msl_parity_job() {
    let cli =
        Cli::try_parse_from(["rum", "verify", "msl-parity"]).expect("parse verify msl-parity");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::MslParity),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_vscode_build() {
    let cli = Cli::try_parse_from(["rum", "vscode", "build"]).expect("parse vscode build");
    match cli.command {
        Commands::Vscode(args) => match args.command {
            VscodeCommand::Build(_) => {}
            other => panic!("expected vscode build, got {other:?}"),
        },
        other => panic!("expected vscode command, got {other:?}"),
    }
}

#[test]
fn cli_parses_vscode_package() {
    let cli = Cli::try_parse_from(["rum", "vscode", "package", "--target", "linux-x64"])
        .expect("parse vscode package");
    match cli.command {
        Commands::Vscode(args) => match args.command {
            VscodeCommand::Package(args) => {
                assert_eq!(
                    args.target,
                    crate::vscode_cmd::VscodePackageTarget::LinuxX64
                );
            }
            other => panic!("expected vscode package, got {other:?}"),
        },
        other => panic!("expected vscode command, got {other:?}"),
    }
}

#[test]
fn cli_parses_vscode_test() {
    let cli = Cli::try_parse_from(["rum", "vscode", "test"]).expect("parse vscode test");
    match cli.command {
        Commands::Vscode(args) => match args.command {
            VscodeCommand::Test => {}
            other => panic!("expected vscode test, got {other:?}"),
        },
        other => panic!("expected vscode command, got {other:?}"),
    }
}

#[test]
fn cli_parses_vscode_edit() {
    let cli = Cli::try_parse_from(["rum", "vscode", "edit"]).expect("parse vscode edit");
    match cli.command {
        Commands::Vscode(args) => match args.command {
            VscodeCommand::Edit(_) => {}
            other => panic!("expected vscode edit, got {other:?}"),
        },
        other => panic!("expected vscode command, got {other:?}"),
    }
}

#[test]
fn cli_parses_wasm_build() {
    let cli = Cli::try_parse_from(["rum", "wasm", "build"]).expect("parse wasm build");
    match cli.command {
        Commands::Wasm(args) => match args.command {
            WasmCommand::Build => {}
            other => panic!("expected wasm build, got {other:?}"),
        },
        other => panic!("expected wasm command, got {other:?}"),
    }
}

#[test]
fn cli_parses_wasm_test() {
    let cli = Cli::try_parse_from(["rum", "wasm", "test"]).expect("parse wasm test");
    match cli.command {
        Commands::Wasm(args) => match args.command {
            WasmCommand::Test => {}
            other => panic!("expected wasm test, got {other:?}"),
        },
        other => panic!("expected wasm command, got {other:?}"),
    }
}

#[test]
fn cli_parses_wasm_edit() {
    let cli =
        Cli::try_parse_from(["rum", "wasm", "edit", "--port", "9001"]).expect("parse wasm edit");
    match cli.command {
        Commands::Wasm(args) => match args.command {
            WasmCommand::Edit(args) => assert_eq!(args.port, Some(9001)),
            other => panic!("expected wasm edit, got {other:?}"),
        },
        other => panic!("expected wasm command, got {other:?}"),
    }
}

#[test]
fn cli_parses_python_build() {
    let cli = Cli::try_parse_from(["rum", "python", "build"]).expect("parse python build");
    match cli.command {
        Commands::Python(args) => match args.command {
            PythonCommand::Build(_) => {}
        },
        other => panic!("expected python command, got {other:?}"),
    }
}

#[test]
fn cli_parses_coverage_report() {
    let cli = Cli::try_parse_from(["rum", "coverage", "report"]).expect("parse coverage report");
    match cli.command {
        Commands::Coverage(args) => match args.command {
            CoverageCommand::Report(_) => {}
            other => panic!("expected coverage report, got {other:?}"),
        },
        other => panic!("expected coverage command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_msl_omc_reference() {
    let cli = Cli::try_parse_from(["rum", "repo", "msl", "omc-reference"]).expect("parse repo msl");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Msl(args) => match args.command {
                RepoMslCommand::OmcReference(_) => {}
                other => panic!("expected repo msl omc-reference, got {other:?}"),
            },
            other => panic!("expected repo msl, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_msl_flamegraph() {
    let cli = Cli::try_parse_from([
        "rum",
        "repo",
        "msl",
        "flamegraph",
        "--model",
        "Modelica.Electrical.Digital.Examples.DFFREG",
        "--mode",
        "simulate",
    ])
    .expect("parse repo msl flamegraph");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Msl(args) => match args.command {
                RepoMslCommand::Flamegraph(args) => {
                    assert_eq!(args.model, "Modelica.Electrical.Digital.Examples.DFFREG");
                    assert_eq!(
                        args.mode,
                        crate::msl_flamegraph_cmd::MslFlamegraphMode::Simulate
                    );
                }
                other => panic!("expected repo msl flamegraph, got {other:?}"),
            },
            other => panic!("expected repo msl, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_rejects_removed_modelica_command() {
    let err = Cli::try_parse_from(["rum", "modelica", "harness", "omc-reference"])
        .expect_err("modelica command should be removed");
    assert_eq!(err.kind(), ErrorKind::InvalidSubcommand);
}

#[test]
fn cli_parses_repo_hooks_install() {
    let cli = Cli::try_parse_from(["rum", "repo", "hooks", "install"]).expect("parse repo hooks");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Hooks(args) => match args.command {
                RepoHooksCommand::Install => {}
            },
            other => panic!("expected repo hooks, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_cli_install() {
    let cli =
        Cli::try_parse_from(["rum", "repo", "cli", "install", "--path"]).expect("parse repo cli");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Cli(args) => match args.command {
                RepoCliCommand::Install(args) => assert!(args.path),
            },
            other => panic!("expected repo cli, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_policy_rust_file_lines() {
    let cli = Cli::try_parse_from(["rum", "repo", "policy", "rust-file-lines", "--all-files"])
        .expect("parse repo policy rust-file-lines");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Policy(args) => match args.command {
                RepoPolicyCommand::RustFileLines(args) => assert!(args.all_files),
            },
            other => panic!("expected repo policy, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_rejects_removed_legacy_entrypoints() {
    for argv in [
        ["rum", "test"].as_slice(),
        ["rum", "dev"].as_slice(),
        ["rum", "msl"].as_slice(),
        ["rum", "coverage-report"].as_slice(),
        ["rum", "coverage-gate"].as_slice(),
        ["rum", "build-python"].as_slice(),
        ["rum", "crate-dag"].as_slice(),
        ["rum", "install-git-hooks"].as_slice(),
    ] {
        let result = Cli::try_parse_from(argv.iter().copied());
        assert!(result.is_err(), "expected parse failure for {:?}", argv);
    }
}

#[test]
fn cli_parses_ver_command() {
    let cli = Cli::try_parse_from(["rum", "ver"]).expect("parse ver");
    match cli.command {
        Commands::Ver => {}
        other => panic!("expected ver command, got {other:?}"),
    }
}

#[test]
fn cli_top_level_help_flag_is_removed() {
    let error =
        Cli::try_parse_from(["rum", "--help"]).expect_err("top-level --help should not parse");
    assert_eq!(error.kind(), ErrorKind::UnknownArgument);
}

#[test]
fn cli_top_level_version_flag_is_removed() {
    let error = Cli::try_parse_from(["rum", "--version"])
        .expect_err("top-level --version should not parse");
    assert_eq!(error.kind(), ErrorKind::UnknownArgument);
}

#[test]
fn cargo_install_rum_args_target_the_rum_binary() {
    let root = Path::new("/tmp/rumoca");
    let args = cargo_install_rum_args(root);
    assert_eq!(
        args,
        vec![
            "install".to_string(),
            "--path".to_string(),
            root.join("crates/rumoca-tool-dev").display().to_string(),
            "--locked".to_string(),
            "--bin".to_string(),
            "rum".to_string(),
            "--force".to_string(),
        ]
    );
}

#[test]
fn detect_shell_kind_matches_bash_and_fish() {
    assert_eq!(
        detect_shell_kind(Some(OsStr::new("/bin/bash"))),
        ShellKind::Bash
    );
    assert_eq!(detect_shell_kind(Some(OsStr::new("fish"))), ShellKind::Fish);
}

#[test]
fn path_var_contains_dir_matches_existing_entry() {
    let joined =
        env::join_paths([Path::new("/tmp/bin"), Path::new("/tmp/rumoca/.cargo/bin")]).unwrap();
    assert!(path_var_contains_dir(
        Some(joined.as_os_str()),
        Path::new("/tmp/rumoca/.cargo/bin")
    ));
}

#[test]
fn bash_path_guidance_mentions_bashrc() {
    let guidance = shell_path_update_guidance(ShellKind::Bash, Path::new("/tmp/.cargo/bin"));
    assert!(guidance.current_command.contains("export PATH="));
    assert!(guidance.persist_intro.contains(".bashrc"));
    assert_eq!(guidance.reload_hint.as_deref(), Some("source ~/.bashrc"));
}

#[test]
fn shell_profile_update_for_bash_targets_bashrc() {
    let update = shell_profile_update(
        ShellKind::Bash,
        Path::new("/home/tester"),
        Path::new("/home/tester/.cargo/bin"),
    )
    .expect("bash profile update");
    assert_eq!(update.path, Path::new("/home/tester/.bashrc"));
    assert!(update.snippet.contains("export PATH="));
}

#[test]
fn completion_install_plan_for_bash_targets_user_completion_dir() {
    let plan =
        completion_install_plan(ShellKind::Bash, Path::new("/home/tester")).expect("bash plan");
    assert_eq!(
        plan.script_path,
        Path::new("/home/tester/.local/share/bash-completion/completions/rum")
    );
    let profile_update = plan.profile_update.expect("bash profile update");
    assert_eq!(profile_update.path, Path::new("/home/tester/.bashrc"));
    assert!(plan.script_contents.contains("complete -F"));
}

#[test]
fn completion_install_plan_for_fish_needs_no_profile_update() {
    let plan =
        completion_install_plan(ShellKind::Fish, Path::new("/home/tester")).expect("fish plan");
    assert_eq!(
        plan.script_path,
        Path::new("/home/tester/.config/fish/completions/rum.fish")
    );
    assert!(plan.profile_update.is_none());
    assert!(plan.script_contents.contains("complete -c rum"));
}

#[test]
fn generated_completions_include_repo_graph_crates_flags_and_repo_msl_commands() {
    let mut command = Cli::command();
    let script =
        crate::completion_cmd::render(crate::completion_cmd::ShellKind::Bash, &mut command)
            .expect("render bash completions");
    assert!(script.contains("graph"));
    assert!(script.contains("crates"));
    assert!(script.contains("--format"));
    assert!(script.contains("rum__repo__msl"));
    assert!(script.contains("promote-quality-baseline"));
    assert!(script.contains("--baseline"));
}
