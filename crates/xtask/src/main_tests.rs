use super::{
    Cli, Commands, CoverageCommand, PlaygroundCommand, PythonCommand, RepoCliCommand, RepoCommand,
    RepoCompletionsCommand, RepoGraphCommand, RepoHooksCommand, RepoPolicyCommand,
    RepoUbuntuCommand, VscodeCommand, WebCommand, classify_candidate,
    docs_wasm_package_is_up_to_date, is_line_count_excluded_rust_file, rust_target_is_installed,
};
use crate::docs_cmd::{DocsBook, DocsCommand};
use crate::modelica_dependency_cache::{CmmCommand, ModelicaDepsCommand};
use crate::repo_cli_cmd::{
    ShellKind, completion_install_plan, detect_shell_kind, path_var_contains_dir,
    shell_path_update_guidance, shell_profile_update, xtask_cli_launcher_contents,
    xtask_cli_launcher_path,
};
use crate::verify_cmd::{TemplateRuntimeBackend, VerifyCommand};
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
fn rust_line_count_policy_only_excludes_generated_files() {
    assert!(!is_line_count_excluded_rust_file(
        "crates/rumoca/tests/architecture_hardening_test.rs"
    ));
    assert!(!is_line_count_excluded_rust_file(
        "crates/foo/src/lower/tests.rs"
    ));
    assert!(!is_line_count_excluded_rust_file(
        "crates/foo/src/lower/runtime.rs"
    ));
    assert!(is_line_count_excluded_rust_file(
        "crates/foo/src/generated/parser.rs"
    ));
}

#[test]
fn cli_parses_verify_quick_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "quick"]).expect("parse verify quick");
    match cli.command {
        Commands::Verify(args) => {
            assert!(matches!(args.command, VerifyCommand::Quick(_)))
        }
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_full_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "full"]).expect("parse verify full");
    match cli.command {
        Commands::Verify(args) => {
            assert!(matches!(args.command, VerifyCommand::Full(_)))
        }
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_rejects_removed_ci_command() {
    let error = Cli::try_parse_from(["xtask", "ci", "verify"]).expect_err("ci should be removed");
    assert_eq!(error.kind(), ErrorKind::InvalidSubcommand);
}

#[test]
fn cli_parses_docs_build_user_guide() {
    let cli = Cli::try_parse_from(["xtask", "docs", "build", "--book", "user"])
        .expect("parse docs build");
    match cli.command {
        Commands::Docs(args) => match args.command {
            DocsCommand::Build(args) => assert_eq!(args.book, DocsBook::User),
            other => panic!("expected docs build, got {other:?}"),
        },
        other => panic!("expected docs command, got {other:?}"),
    }
}

#[test]
fn cli_parses_docs_serve_dev_guide() {
    let cli = Cli::try_parse_from([
        "xtask",
        "docs",
        "serve",
        "--book",
        "dev",
        "--port",
        "9000",
        "--skip-build",
        "--skip-wasm-build",
    ])
    .expect("parse docs serve");
    match cli.command {
        Commands::Docs(args) => match args.command {
            DocsCommand::Serve(args) => {
                assert_eq!(args.book, DocsBook::Dev);
                assert_eq!(args.port, Some(9000));
                assert!(args.skip_build);
                assert!(args.skip_wasm_build);
            }
            other => panic!("expected docs serve, got {other:?}"),
        },
        other => panic!("expected docs command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_workspace_job() {
    let cli =
        Cli::try_parse_from(["xtask", "verify", "workspace"]).expect("parse verify workspace");
    match cli.command {
        Commands::Verify(args) => assert!(matches!(args.command, VerifyCommand::Workspace(_))),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_template_runtimes_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "template-runtimes"])
        .expect("parse verify template-runtimes");
    match cli.command {
        Commands::Verify(args) => match args.command {
            VerifyCommand::TemplateRuntimes(args) => {
                assert_eq!(args.backend, TemplateRuntimeBackend::All);
            }
            other => panic!("expected template runtimes command, got {other:?}"),
        },
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_template_runtimes_backend() {
    let cli = Cli::try_parse_from([
        "xtask",
        "verify",
        "template-runtimes",
        "--backend",
        "casadi",
    ])
    .expect("parse verify template-runtimes --backend casadi");
    match cli.command {
        Commands::Verify(args) => match args.command {
            VerifyCommand::TemplateRuntimes(args) => {
                assert_eq!(args.backend, TemplateRuntimeBackend::Casadi);
            }
            other => panic!("expected template runtimes command, got {other:?}"),
        },
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_examples_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "examples"]).expect("parse verify examples");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::Examples),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_editor_msl_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "editor-msl", "--install-prereqs"])
        .expect("parse verify editor-msl");
    match cli.command {
        Commands::Verify(args) => match args.command {
            VerifyCommand::EditorMsl(args) => assert!(args.install_prereqs),
            other => panic!("expected editor-msl args, got {other:?}"),
        },
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_vscode_msl_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "vscode-msl", "--install-prereqs"])
        .expect("parse verify vscode-msl");
    match cli.command {
        Commands::Verify(args) => match args.command {
            VerifyCommand::VscodeMsl(args) => assert!(args.install_prereqs),
            other => panic!("expected vscode-msl args, got {other:?}"),
        },
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_lsp_msl_completion_timings_job() {
    let cli = Cli::try_parse_from([
        "xtask",
        "verify",
        "lsp-msl-completion-timings",
        "--install-prereqs",
    ])
    .expect("parse verify lsp-msl-completion-timings");
    match cli.command {
        Commands::Verify(args) => match args.command {
            VerifyCommand::LspMslCompletionTimings(args) => assert!(args.install_prereqs),
            other => panic!("expected lsp-msl-completion-timings args, got {other:?}"),
        },
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_wasm_editor_msl_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "wasm-editor-msl"])
        .expect("parse verify wasm-editor-msl");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::WasmEditorMsl),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_msl_parity_job() {
    let cli =
        Cli::try_parse_from(["xtask", "verify", "msl-parity"]).expect("parse verify msl-parity");
    match cli.command {
        Commands::Verify(args) => {
            assert!(matches!(args.command, VerifyCommand::MslParity(_)))
        }
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_msl_parity_quality_baseline_flags() {
    let cli = Cli::try_parse_from([
        "xtask",
        "verify",
        "msl-parity",
        "--quality-baseline",
        "baseline.json",
        "--no-remote-quality-baseline",
    ])
    .expect("parse verify msl-parity baseline flags");
    match cli.command {
        Commands::Verify(args) => {
            assert!(matches!(args.command, VerifyCommand::MslParity(_)))
        }
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn verify_msl_parity_shard_parses_and_validates() {
    let parse = |arg: &str| {
        let cli = Cli::try_parse_from(["xtask", "verify", "msl-parity", "--shard", arg])
            .expect("parse verify msl-parity --shard");
        match cli.command {
            Commands::Verify(args) => match args.command {
                VerifyCommand::MslParity(msl) => msl.parse_shard(),
                other => panic!("expected msl-parity, got {other:?}"),
            },
            other => panic!("expected verify command, got {other:?}"),
        }
    };
    assert_eq!(parse("2/4").expect("valid shard"), Some((2, 4)));
    assert_eq!(parse("1/1").expect("valid shard"), Some((1, 1)));
    // No --shard => None.
    let bare = Cli::try_parse_from(["xtask", "verify", "msl-parity"]).expect("parse bare");
    let Commands::Verify(bare_args) = bare.command else {
        panic!("expected verify command");
    };
    let VerifyCommand::MslParity(bare_msl) = bare_args.command else {
        panic!("expected msl-parity");
    };
    assert_eq!(bare_msl.parse_shard().expect("no shard"), None);
    // Malformed pairs are rejected (never a silent full-set or empty-shard run).
    for bad in ["0/4", "5/4", "3", "abc", "1/0", "2/x"] {
        assert!(parse(bad).is_err(), "expected '{bad}' to be rejected");
    }
}

#[test]
fn cli_parses_verify_msl_parity_prebuilt_workers() {
    let cli = Cli::try_parse_from([
        "xtask",
        "verify",
        "msl-parity",
        "--prebuilt-test-binary",
        "result-msl-test/bin/msl_tests",
        "--prebuilt-model-worker",
        "result-msl-test/bin/rumoca-worker",
        "--prebuilt-sim-worker",
        "result-sim-worker/bin/rumoca-sim-worker",
    ])
    .expect("parse verify msl-parity prebuilt workers");
    match cli.command {
        Commands::Verify(args) => match args.command {
            VerifyCommand::MslParity(_) => {}
            other => panic!("expected msl-parity, got {other:?}"),
        },
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_verify_msl_hotspots_job() {
    let cli = Cli::try_parse_from(["xtask", "verify", "msl-hotspots"])
        .expect("parse verify msl-hotspots");
    match cli.command {
        Commands::Verify(args) => assert_eq!(args.command, VerifyCommand::MslHotspots),
        other => panic!("expected verify command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_cmm_ensure() {
    let cli = Cli::try_parse_from(["xtask", "repo", "cmm", "ensure"]).expect("parse repo cmm");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Cmm(args) => match args.command {
                CmmCommand::Ensure(args) => assert!(!args.force),
                other => panic!("expected cmm ensure, got {other:?}"),
            },
            other => panic!("expected repo cmm, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_modelica_deps_ensure() {
    let cli = Cli::try_parse_from(["xtask", "repo", "modelica-deps", "ensure"])
        .expect("parse repo modelica-deps");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::ModelicaDeps(args) => match args.command {
                ModelicaDepsCommand::Ensure(args) => assert!(!args.force),
                other => panic!("expected modelica-deps ensure, got {other:?}"),
            },
            other => panic!("expected repo modelica-deps, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_graph_crates_dot_artifact() {
    let cli = Cli::try_parse_from(["xtask", "repo", "graph", "crates", "--format", "dot"])
        .expect("parse repo graph crates");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Graph(args) => match args.command {
                RepoGraphCommand::Crates(_) => {}
            },
            other => panic!("expected repo graph, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_review_scan() {
    let cli = Cli::try_parse_from([
        "xtask",
        "repo",
        "review-scan",
        "--base",
        "origin/main",
        "--head",
        "HEAD",
        "--out",
        "target/review/findings.json",
        "--fail-on-high",
        "--fail-on-forbidden",
    ])
    .expect("parse repo review-scan");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::ReviewScan(_) => {}
            other => panic!("expected repo review-scan, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_review_packet() {
    let cli = Cli::try_parse_from([
        "xtask",
        "repo",
        "review-packet",
        "--base",
        "origin/main",
        "--head",
        "HEAD",
        "--phase",
        "phase-0",
        "--out",
        "target/review/packet.md",
    ])
    .expect("parse repo review-packet");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::ReviewPacket(_) => {}
            other => panic!("expected repo review-packet, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_vscode_build() {
    let cli = Cli::try_parse_from(["xtask", "vscode", "build"]).expect("parse vscode build");
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
    let cli = Cli::try_parse_from(["xtask", "vscode", "package", "--target", "linux-x64"])
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
fn cli_parses_vscode_install_check() {
    let cli = Cli::try_parse_from([
        "xtask",
        "vscode",
        "install-check",
        "--no-build",
        "--no-open",
        "--document",
        "examples/models/Ball.mo",
    ])
    .expect("parse vscode install-check");
    match cli.command {
        Commands::Vscode(args) => match args.command {
            VscodeCommand::InstallCheck(args) => {
                assert!(args.no_build);
                assert!(args.no_open);
                assert_eq!(
                    args.document.as_deref(),
                    Some(Path::new("examples/models/Ball.mo"))
                );
            }
            other => panic!("expected vscode install-check, got {other:?}"),
        },
        other => panic!("expected vscode command, got {other:?}"),
    }
}

#[test]
fn cli_parses_vscode_test() {
    let cli = Cli::try_parse_from(["xtask", "vscode", "test"]).expect("parse vscode test");
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
    let cli = Cli::try_parse_from(["xtask", "vscode", "edit"]).expect("parse vscode edit");
    match cli.command {
        Commands::Vscode(args) => match args.command {
            VscodeCommand::Edit(_) => {}
            other => panic!("expected vscode edit, got {other:?}"),
        },
        other => panic!("expected vscode command, got {other:?}"),
    }
}

#[test]
fn cli_parses_web_build() {
    let cli = Cli::try_parse_from(["xtask", "web", "build"]).expect("parse web build");
    match cli.command {
        Commands::Web(args) => match args.command {
            WebCommand::Build => {}
        },
        other => panic!("expected web command, got {other:?}"),
    }
}

#[test]
fn cli_parses_playground_build() {
    let cli =
        Cli::try_parse_from(["xtask", "playground", "build"]).expect("parse playground build");
    match cli.command {
        Commands::Playground(args) => match args.command {
            PlaygroundCommand::Build(args) => assert!(!args.dev),
            other => panic!("expected playground build, got {other:?}"),
        },
        other => panic!("expected playground command, got {other:?}"),
    }
}

#[test]
fn cli_parses_playground_build_dev() {
    let cli = Cli::try_parse_from(["xtask", "playground", "build", "--dev"])
        .expect("parse playground build --dev");
    match cli.command {
        Commands::Playground(args) => match args.command {
            PlaygroundCommand::Build(args) => assert!(args.dev),
            other => panic!("expected playground build --dev, got {other:?}"),
        },
        other => panic!("expected playground command, got {other:?}"),
    }
}

#[test]
fn cli_parses_playground_test() {
    let cli = Cli::try_parse_from(["xtask", "playground", "test"]).expect("parse playground test");
    match cli.command {
        Commands::Playground(args) => match args.command {
            PlaygroundCommand::Test => {}
            other => panic!("expected playground test, got {other:?}"),
        },
        other => panic!("expected playground command, got {other:?}"),
    }
}

#[test]
fn cli_parses_playground_edit() {
    let cli = Cli::try_parse_from(["xtask", "playground", "edit", "--port", "9001"])
        .expect("parse playground edit");
    match cli.command {
        Commands::Playground(args) => match args.command {
            PlaygroundCommand::Edit(args) => {
                assert_eq!(args.port, Some(9001));
                assert!(!args.rayon);
                assert!(!args.skip_build);
            }
            other => panic!("expected playground edit, got {other:?}"),
        },
        other => panic!("expected playground command, got {other:?}"),
    }
}

#[test]
fn cli_parses_playground_edit_skip_build() {
    let cli = Cli::try_parse_from([
        "xtask",
        "playground",
        "edit",
        "--skip-build",
        "--port",
        "9002",
    ])
    .expect("parse playground edit --skip-build");
    match cli.command {
        Commands::Playground(args) => match args.command {
            PlaygroundCommand::Edit(args) => {
                assert_eq!(args.port, Some(9002));
                assert!(args.skip_build);
            }
            other => panic!("expected playground edit, got {other:?}"),
        },
        other => panic!("expected playground command, got {other:?}"),
    }
}

#[test]
fn cli_parses_playground_edit_rayon() {
    let cli = Cli::try_parse_from(["xtask", "playground", "edit", "--rayon"])
        .expect("parse playground edit --rayon");
    match cli.command {
        Commands::Playground(args) => match args.command {
            PlaygroundCommand::Edit(args) => assert!(args.rayon),
            other => panic!("expected playground edit --rayon, got {other:?}"),
        },
        other => panic!("expected playground command, got {other:?}"),
    }
}

#[test]
fn cli_parses_python_build() {
    let cli = Cli::try_parse_from(["xtask", "python", "build"]).expect("parse python build");
    match cli.command {
        Commands::Python(args) => match args.command {
            PythonCommand::Build(_) => {}
        },
        other => panic!("expected python command, got {other:?}"),
    }
}

#[test]
fn cli_parses_coverage_report() {
    let cli = Cli::try_parse_from(["xtask", "coverage", "report"]).expect("parse coverage report");
    match cli.command {
        Commands::Coverage(args) => match args.command {
            CoverageCommand::Report(_) => {}
            other => panic!("expected coverage report, got {other:?}"),
        },
        other => panic!("expected coverage command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_msl_passthrough() {
    // `repo msl` is a thin passthrough: the typed subcommand surface lives in the
    // `rumoca-msl-tools` bin (rumoca-test-msl). xtask must capture the subcommand
    // name and every following flag verbatim (allow_hyphen_values), so it can
    // forward them unchanged.
    let cli = Cli::try_parse_from([
        "xtask",
        "repo",
        "msl",
        "flamegraph",
        "--model",
        "Modelica.Electrical.Digital.Examples.DFFREG",
        "--mode",
        "simulate",
    ])
    .expect("parse repo msl passthrough");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Msl(args) => {
                let expected: Vec<String> = [
                    "flamegraph",
                    "--model",
                    "Modelica.Electrical.Digital.Examples.DFFREG",
                    "--mode",
                    "simulate",
                ]
                .iter()
                .map(|s| (*s).to_string())
                .collect();
                assert_eq!(args.forwarded, expected);
            }
            other => panic!("expected repo msl, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_rejects_removed_modelica_command() {
    let err = Cli::try_parse_from(["xtask", "modelica", "harness", "omc-reference"])
        .expect_err("modelica command should be removed");
    assert_eq!(err.kind(), ErrorKind::InvalidSubcommand);
}

#[test]
fn cli_parses_repo_hooks_install() {
    let cli = Cli::try_parse_from(["xtask", "repo", "hooks", "install"]).expect("parse repo hooks");
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
        Cli::try_parse_from(["xtask", "repo", "cli", "install", "--path"]).expect("parse repo cli");
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
fn cli_parses_repo_ubuntu_install_vscode_smoke_prereqs() {
    let cli = Cli::try_parse_from([
        "xtask",
        "repo",
        "ubuntu",
        "install-vscode-smoke-prereqs",
        "--no-update",
    ])
    .expect("parse repo ubuntu install-vscode-smoke-prereqs");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Ubuntu(args) => match args.command {
                RepoUbuntuCommand::InstallVscodeSmokePrereqs(args) => assert!(args.no_update),
            },
            other => panic!("expected repo ubuntu, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_completions_print() {
    let cli = Cli::try_parse_from(["xtask", "repo", "completions", "print", "bash"])
        .expect("parse repo completions print");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Completions(args) => match args.command {
                RepoCompletionsCommand::Print(args) => {
                    assert_eq!(args.shell, crate::completion_cmd::ShellKind::Bash);
                }
                other => panic!("expected repo completions print, got {other:?}"),
            },
            other => panic!("expected repo completions, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_completions_install_without_explicit_shell() {
    let cli = Cli::try_parse_from(["xtask", "repo", "completions", "install"])
        .expect("parse repo completions install");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Completions(args) => match args.command {
                RepoCompletionsCommand::Install(args) => assert!(args.shell.is_none()),
                other => panic!("expected repo completions install, got {other:?}"),
            },
            other => panic!("expected repo completions, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_parses_repo_completions_install_with_explicit_shell() {
    let cli = Cli::try_parse_from(["xtask", "repo", "completions", "install", "fish"])
        .expect("parse repo completions install fish");
    match cli.command {
        Commands::Repo(args) => match args.command {
            RepoCommand::Completions(args) => match args.command {
                RepoCompletionsCommand::Install(args) => {
                    assert_eq!(args.shell, Some(crate::completion_cmd::ShellKind::Fish));
                }
                other => panic!("expected repo completions install, got {other:?}"),
            },
            other => panic!("expected repo completions, got {other:?}"),
        },
        other => panic!("expected repo command, got {other:?}"),
    }
}

#[test]
fn cli_repo_completions_requires_subcommand() {
    let error = Cli::try_parse_from(["xtask", "repo", "completions"])
        .expect_err("repo completions should require a subcommand");
    let rendered = error.to_string();
    assert!(rendered.contains("Usage: xtask repo completions <COMMAND>"));
    assert!(rendered.contains("print"));
    assert!(rendered.contains("install"));
}

#[test]
fn cli_parses_repo_policy_rust_file_lines() {
    let cli = Cli::try_parse_from(["xtask", "repo", "policy", "rust-file-lines", "--all-files"])
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
fn cli_top_level_help_flag_is_supported() {
    let error = Cli::try_parse_from(["xtask", "--help"]).expect_err("top-level --help should exit");
    assert_eq!(error.kind(), ErrorKind::DisplayHelp);
}

#[test]
fn cli_top_level_version_flag_is_supported() {
    let error =
        Cli::try_parse_from(["xtask", "--version"]).expect_err("top-level --version should exit");
    assert_eq!(error.kind(), ErrorKind::DisplayVersion);
}

#[test]
fn cli_accepts_help_subcommand() {
    // `xtask help` should behave like `rumoca help` (clap's built-in help
    // subcommand) for cross-binary parity.
    let error = Cli::try_parse_from(["xtask", "help"]).expect_err("help subcommand should display");
    assert_eq!(error.kind(), ErrorKind::DisplayHelp);
}

#[test]
fn xtask_cli_launcher_runs_repo_via_cargo_run() {
    let root = Path::new("/tmp/rumoca");
    let bin_dir = Path::new("/tmp/bin");
    let launcher_path = xtask_cli_launcher_path(bin_dir);
    let contents = xtask_cli_launcher_contents(root);

    #[cfg(windows)]
    assert_eq!(launcher_path, bin_dir.join("xtask.cmd"));
    #[cfg(not(windows))]
    assert_eq!(launcher_path, bin_dir.join("xtask"));

    #[cfg(windows)]
    {
        assert!(contents.contains("cargo run -q -p xtask --bin xtask --"));
        assert!(contents.contains("%*"));
    }
    #[cfg(not(windows))]
    {
        assert!(contents.contains("cargo build -p xtask --bin xtask"));
        assert!(contents.contains("tail -n +1 -f"));
        assert!(contents.contains("exec \"$xtask_bin\" \"$@\""));
        assert!(!contents.contains("cargo run"));
        assert!(contents.contains("\"$@\""));
    }
    assert!(contents.contains(&root.display().to_string()));
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
        Path::new("/home/tester/.local/share/bash-completion/completions/xtask")
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
        Path::new("/home/tester/.config/fish/completions/xtask.fish")
    );
    assert!(plan.profile_update.is_none());
    assert!(plan.script_contents.contains("complete -c xtask"));
}

#[test]
fn generated_completions_include_repo_graph_crates_flags_and_repo_msl_passthrough() {
    let mut command = Cli::command();
    let script =
        crate::completion_cmd::render(crate::completion_cmd::ShellKind::Bash, &mut command)
            .expect("render bash completions");
    assert!(script.contains("graph"));
    assert!(script.contains("crates"));
    assert!(script.contains("ubuntu"));
    assert!(script.contains("install-vscode-smoke-prereqs"));
    assert!(script.contains("--install-prereqs"));
    assert!(script.contains("--format"));
    // The repo subcommand word-list still offers graph/msl/cmm — `msl` is a
    // completed repo subcommand (this is the user-facing behavior; the exact
    // clap_complete helper-function naming is an internal detail we don't assert).
    assert!(script.contains("graph msl cmm"));
    // `repo msl` is now a passthrough to the `rumoca-msl-tools` bin, so its former
    // subcommands (and their flags) live in that bin and are intentionally absent
    // from xtask's own completions.
    assert!(!script.contains("promote-quality-baseline"));
}

/// Set a file's modification time to `seconds` after the Unix epoch, so the
/// freshness gate can be exercised without flaky wall-clock sleeps.
fn set_mtime(path: &Path, seconds: u64) {
    let when = std::time::UNIX_EPOCH + std::time::Duration::from_secs(seconds);
    std::fs::File::options()
        .write(true)
        .open(path)
        .expect("open file to set mtime")
        .set_modified(when)
        .expect("set mtime");
}

#[test]
fn docs_wasm_freshness_gate_tracks_source_edits() {
    let temp = tempfile::tempdir().expect("temp dir");
    let root = temp.path();

    // A crate source file (a WASM build input) and a compiled wasm artifact.
    let src = root.join("crates/demo/src/lib.rs");
    std::fs::create_dir_all(src.parent().expect("src parent")).expect("create crate dirs");
    std::fs::write(&src, "// demo").expect("write source");

    let pkg = root.join("packages/rumoca/dist/release-full-web");
    std::fs::create_dir_all(&pkg).expect("create pkg dir");
    let wasm = pkg.join("rumoca_bind_wasm_bg.wasm");
    std::fs::write(&wasm, b"\0asm").expect("write wasm");

    // Artifact newer than the source -> up to date (no rebuild).
    set_mtime(&src, 1_000);
    set_mtime(&wasm, 2_000);
    assert!(docs_wasm_package_is_up_to_date(root, &pkg).expect("freshness check"));

    // Source edited after the artifact was built -> stale (rebuild).
    set_mtime(&src, 3_000);
    assert!(!docs_wasm_package_is_up_to_date(root, &pkg).expect("freshness check"));

    // An embedded template edit must also count as a build input, even though it
    // is not a `.rs` file: the gate considers every file under `crates/`.
    let template = root.join("crates/demo/src/templates/dae-modelica/model.mo.jinja");
    std::fs::create_dir_all(template.parent().expect("template parent")).expect("template dirs");
    std::fs::write(&template, "{{ x }}").expect("write template");
    set_mtime(&src, 1_000);
    set_mtime(&template, 5_000);
    assert!(!docs_wasm_package_is_up_to_date(root, &pkg).expect("freshness check"));
}

#[test]
fn docs_wasm_freshness_gate_rebuilds_when_no_artifact_present() {
    let temp = tempfile::tempdir().expect("temp dir");
    let root = temp.path();
    std::fs::create_dir_all(root.join("crates")).expect("create crates dir");

    // Package directory exists but holds no compiled `.wasm` output: cannot be
    // confirmed up to date, so the gate asks for a rebuild rather than serving it.
    let pkg = root.join("packages/rumoca/dist/release-full-web");
    std::fs::create_dir_all(&pkg).expect("create pkg dir");
    std::fs::write(pkg.join("rumoca_bind_wasm.js"), "// glue").expect("write glue");
    assert!(!docs_wasm_package_is_up_to_date(root, &pkg).expect("freshness check"));
}

#[test]
fn rust_target_detection_uses_active_toolchain_sysroot() {
    let temp = tempfile::tempdir().expect("temp dir");
    assert!(!rust_target_is_installed(
        temp.path(),
        "wasm32-unknown-unknown"
    ));

    let target_lib = temp.path().join("lib/rustlib/wasm32-unknown-unknown/lib");
    std::fs::create_dir_all(target_lib).expect("create target lib");
    assert!(rust_target_is_installed(
        temp.path(),
        "wasm32-unknown-unknown"
    ));
}
