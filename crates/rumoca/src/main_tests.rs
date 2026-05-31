use super::*;
use clap::CommandFactory;
use rumoca_compile::compile::{
    ModelFailureDiagnostic,
    core::{Label, SourceMap},
};
use rumoca_core::{SourceId, Span};
use std::io::Write;
use tempfile::NamedTempFile;

#[test]
fn compile_failure_report_handles_missing_primary_label() {
    let failure = ModelFailureDiagnostic {
        model_name: "Broken".to_string(),
        phase: None,
        error_code: Some("EX999".to_string()),
        error: "phase failed before labeling source".to_string(),
        primary_label: None,
    };
    let report = build_compile_failure_report(&failure, &SourceMap::new());
    let rendered = format!("{report:?}");

    assert!(rendered.contains("EX999"));
    assert!(rendered.contains("missing a primary source label"));
}

#[test]
fn compile_failure_report_handles_missing_label_source() {
    let failure = ModelFailureDiagnostic {
        model_name: "Broken".to_string(),
        phase: None,
        error_code: Some("EX998".to_string()),
        error: "phase emitted stale source id".to_string(),
        primary_label: Some(Label::primary(Span::from_offsets(SourceId(99), 0, 1))),
    };
    let report = build_compile_failure_report(&failure, &SourceMap::new());
    let rendered = format!("{report:?}");

    assert!(rendered.contains("EX998"));
    assert!(rendered.contains("references a missing source file"));
}

#[test]
fn parse_eval_at_spec_reads_named_state_and_time() {
    let (overrides, t) = parse_eval_at_spec("x=1.0,inductor.i=-3@0.5").expect("parse");
    assert_eq!(
        overrides,
        vec![("x".to_string(), 1.0), ("inductor.i".to_string(), -3.0)]
    );
    assert_eq!(t, 0.5);
}

#[test]
fn parse_eval_at_spec_defaults_time_to_zero_and_allows_no_assignments() {
    let (overrides, t) = parse_eval_at_spec("x=1,y=2").expect("parse without time");
    assert_eq!(
        overrides,
        vec![("x".to_string(), 1.0), ("y".to_string(), 2.0)]
    );
    assert_eq!(t, 0.0);

    // Discovery run: no assignments, just a time — lists state names.
    let (empty, t_only) = parse_eval_at_spec("@1.5").expect("parse time-only");
    assert!(empty.is_empty());
    assert_eq!(t_only, 1.5);
}

#[test]
fn parse_eval_at_spec_rejects_positional_and_bad_values() {
    // Positional (bare number) is rejected: states must be named.
    assert!(parse_eval_at_spec("1.0,2.0@0.5").is_err());
    assert!(parse_eval_at_spec("x=oops@0.5").is_err());
    assert!(parse_eval_at_spec("=1.0@0.5").is_err());
    assert!(parse_eval_at_spec("x=1.0@later").is_err());
}

#[test]
fn expand_trace_filter_expands_short_phase_aliases() {
    assert_eq!(expand_trace_filter("dae"), "rumoca_phase_dae=debug");
    assert_eq!(
        expand_trace_filter("dae:trace,structural"),
        "rumoca_phase_dae=trace,rumoca_phase_structural=debug"
    );
    assert_eq!(
        expand_trace_filter("resolve:debug,solve:info"),
        "rumoca_phase_resolve=debug,rumoca_phase_solve=info"
    );
}

#[test]
fn expand_trace_filter_passes_through_unknown_and_raw_directives() {
    // Full EnvFilter directives (with `=` or `::`) are not aliases -> unchanged.
    assert_eq!(
        expand_trace_filter("rumoca_phase_dae::profile=debug"),
        "rumoca_phase_dae::profile=debug"
    );
    assert_eq!(expand_trace_filter("info"), "info");
    // Mixed alias + raw directive.
    assert_eq!(
        expand_trace_filter("dae:debug,rumoca_phase_dae::profile=debug"),
        "rumoca_phase_dae=debug,rumoca_phase_dae::profile=debug"
    );
}

#[test]
fn cli_parses_sim_inspect_flags() {
    let cli = Cli::try_parse_from([
        "rumoca",
        "sim",
        "Ball.mo",
        "--model",
        "Ball",
        "--inspect",
        "eval",
        "--at",
        "v=1.0@0.5",
    ])
    .expect("parse sim --inspect");
    match cli.command {
        Commands::Sim(args) => {
            assert_eq!(args.inspect, Some(InspectKind::Eval));
            assert_eq!(args.at.as_deref(), Some("v=1.0@0.5"));
        }
        other => panic!("expected sim command, got {other:?}"),
    }
}

#[test]
fn cli_parses_configured_sim_command() {
    let cli = Cli::try_parse_from([
        "rumoca",
        "sim",
        "--config",
        "examples/interactive/quadrotor/rum.acro.toml",
    ])
    .expect("parse sim --config");
    match cli.command {
        Commands::Sim(args) => {
            assert_eq!(
                args.config.as_deref(),
                Some("examples/interactive/quadrotor/rum.acro.toml")
            );
            assert!(args.command.is_none());
        }
        other => panic!("expected sim command, got {other:?}"),
    }
}

#[test]
fn cli_parses_direct_sim_command() {
    let cli = Cli::try_parse_from([
        "rumoca", "sim", "Ball.mo", "--model", "Ball", "--t-end", "10", "--dt", "0.01",
    ])
    .expect("parse direct sim");
    match cli.command {
        Commands::Sim(args) => {
            assert_eq!(args.model_file.as_deref(), Some("Ball.mo"));
            assert_eq!(args.model_options.model.as_deref(), Some("Ball"));
            assert_eq!(args.t_end, Some(10.0));
            assert_eq!(args.dt, Some(0.01));
            assert!(args.config.is_none());
        }
        other => panic!("expected sim command, got {other:?}"),
    }
}

#[cfg(feature = "runner")]
#[test]
fn configured_sim_model_name_prefers_cli_override_without_config_model() {
    let model_path = std::path::Path::new("Fallback.mo");

    assert_eq!(
        configured_model_name(Some("Some.Model"), None, model_path),
        "Some.Model"
    );
}

#[cfg(feature = "runner")]
#[test]
fn configured_sim_model_name_uses_config_before_file_stem() {
    let config_model = rumoca_sim::runner::config::ModelConfig {
        file: "Configured.mo".to_string(),
        name: "Configured.Model".to_string(),
    };
    let model_path = std::path::Path::new("Fallback.mo");

    assert_eq!(
        configured_model_name(None, Some(&config_model), model_path),
        "Configured.Model"
    );
}

#[test]
fn cli_parses_sim_init_command() {
    let cli = Cli::try_parse_from(["rumoca", "sim", "init"]).expect("parse sim init");
    match cli.command {
        Commands::Sim(args) => match args.command {
            Some(SimSubcommand::Init) => {}
            other => panic!("expected sim init, got {other:?}"),
        },
        other => panic!("expected sim command, got {other:?}"),
    }
}

#[test]
fn cli_parses_sim_bench_command() {
    let cli = Cli::try_parse_from([
        "rumoca",
        "sim",
        "bench",
        "--config",
        "examples/interactive/quadrotor/rum.acro.toml",
        "--iterations",
        "3",
        "--solver",
        "bdf",
    ])
    .expect("parse sim bench");
    match cli.command {
        Commands::Sim(args) => match args.command {
            Some(SimSubcommand::Bench(bench)) => {
                assert_eq!(
                    bench.config.as_deref(),
                    Some("examples/interactive/quadrotor/rum.acro.toml")
                );
                assert_eq!(bench.iterations, 3);
                assert_eq!(bench.solver, Some(sim_bench::BenchSolverMode::Bdf));
            }
            other => panic!("expected sim bench, got {other:?}"),
        },
        other => panic!("expected sim command, got {other:?}"),
    }
}

#[test]
fn completions_are_generated_from_clap_and_cover_all_subcommands() {
    // clap_complete generates from the live command tree, so every subcommand —
    // including ones the old hand-maintained spec dropped (`targets`, `cache`) —
    // must appear, and stay in sync automatically.
    let fish = completion_script(CompletionShell::Fish);
    for subcommand in ["compile", "sim", "fmt", "lint", "targets", "cache"] {
        assert!(
            fish.contains(subcommand),
            "fish completions should mention `{subcommand}`"
        );
    }
    // The `--inspect` flag (sim) should be completed too.
    assert!(fish.contains("inspect"));

    // Other shells generate without panicking and are non-empty.
    for shell in [
        CompletionShell::Bash,
        CompletionShell::Zsh,
        CompletionShell::PowerShell,
    ] {
        assert!(!completion_script(shell).is_empty());
    }
}

#[test]
fn cli_parses_targets_command() {
    let cli = Cli::try_parse_from(["rumoca", "targets", "--json"]).expect("parse targets --json");
    match cli.command {
        Commands::Targets(args) => assert!(args.json),
        other => panic!("expected targets command, got {other:?}"),
    }
}

#[test]
fn compile_help_unifies_outputs_under_target() {
    let mut command = Cli::command();
    let compile = command
        .find_subcommand_mut("compile")
        .expect("compile subcommand");
    let help = compile.render_long_help().to_string();

    assert!(!help.contains("--backend"));
    assert!(!help.contains("--template-file"));
    assert!(!help.contains("--template-ir"));
    // Three separated concerns: --emit (dumps), --target (codegen), --phase (.jinja IR).
    assert!(help.contains("--emit"));
    assert!(help.contains("--target"));
    assert!(help.contains("--phase"));
    assert!(help.contains("a built-in target"));
    // --emit enumerates every stage/format possibility (clap [possible values]).
    for value in ["dae-mo", "dae-json", "solve-json"] {
        assert!(help.contains(value), "--emit help should list {value}");
    }
    // The review's #1 fix: --target points users at `rumoca targets`.
    assert!(help.contains("rumoca targets"));
}

#[test]
fn compile_phase_maps_to_template_ir() {
    assert_eq!(TemplateIr::from(CompilePhase::Solve), TemplateIr::Solve);
    assert_eq!(TemplateIr::from(CompilePhase::Dae), TemplateIr::Dae);
    assert_eq!(TemplateIr::from(CompilePhase::Flat), TemplateIr::Flat);
    assert_eq!(TemplateIr::from(CompilePhase::Ast), TemplateIr::Ast);
}

#[test]
fn cli_parses_compile_manifest_target() {
    let cli = Cli::try_parse_from([
        "rumoca",
        "compile",
        "model.mo",
        "--model",
        "M",
        "--target",
        "embedded-c",
        "--output",
        "out",
    ])
    .expect("parse compile target");
    match cli.command {
        Commands::Compile(args) => {
            assert_eq!(args.input.options.model.as_deref(), Some("M"));
            assert_eq!(args.target.as_deref(), Some("embedded-c"));
            assert_eq!(args.output, Some(PathBuf::from("out")));
        }
        other => panic!("expected compile command, got {other:?}"),
    }
}

#[test]
fn cli_parses_compile_builtin_manifest_target() {
    let cli = Cli::try_parse_from([
        "rumoca", "compile", "model.mo", "--model", "M", "--target", "sympy", "--output",
        "model.py",
    ])
    .expect("parse compile template target");
    match cli.command {
        Commands::Compile(args) => {
            assert_eq!(args.input.options.model.as_deref(), Some("M"));
            assert_eq!(args.target.as_deref(), Some("sympy"));
            assert_eq!(args.output, Some(PathBuf::from("model.py")));
        }
        other => panic!("expected compile command, got {other:?}"),
    }
}

#[test]
fn cli_parses_compile_emit_dump() {
    let cli = Cli::try_parse_from([
        "rumoca",
        "compile",
        "model.mo",
        "--model",
        "M",
        "--emit",
        "solve-json",
        "--output",
        "solve.json",
    ])
    .expect("parse compile --emit dump");
    match cli.command {
        Commands::Compile(args) => {
            assert_eq!(args.input.options.model.as_deref(), Some("M"));
            assert_eq!(args.emit, Some(EmitTarget::SolveJson));
            assert_eq!(args.target, None);
            assert_eq!(args.output, Some(PathBuf::from("solve.json")));
        }
        other => panic!("expected compile command, got {other:?}"),
    }
}

#[test]
fn compile_flag_separation_is_enforced() {
    // --emit and --target are mutually exclusive (don't overload --target).
    assert!(
        Cli::try_parse_from([
            "rumoca", "compile", "m.mo", "--emit", "dae-mo", "--target", "sympy"
        ])
        .is_err()
    );
    // --phase requires --target (it only picks the IR fed to a .jinja).
    assert!(Cli::try_parse_from(["rumoca", "compile", "m.mo", "--phase", "flat"]).is_err());
    // --phase + a .jinja --target parses.
    assert!(
        Cli::try_parse_from([
            "rumoca", "compile", "m.mo", "--phase", "flat", "--target", "t.jinja"
        ])
        .is_ok()
    );
    // every <stage>-<format> dump value is accepted.
    for value in [
        "ast-mo",
        "ast-json",
        "flat-mo",
        "flat-json",
        "dae-mo",
        "dae-json",
        "solve-json",
    ] {
        assert!(
            Cli::try_parse_from(["rumoca", "compile", "m.mo", "--emit", value]).is_ok(),
            "--emit {value} should parse"
        );
    }
    // solve-mo is intentionally not a value (no Modelica form).
    assert!(Cli::try_parse_from(["rumoca", "compile", "m.mo", "--emit", "solve-mo"]).is_err());
}

#[test]
fn cli_rejects_compile_backend_option() {
    let err = Cli::try_parse_from(["rumoca", "compile", "model.mo", "--backend", "sympy"])
        .expect_err("backend option was unified into target");
    assert!(
        err.to_string().contains("unexpected argument '--backend'"),
        "old backend option should not parse: {err}"
    );
}

#[test]
fn compile_rejects_bare_json_flag() {
    // The old `--json` boolean is gone; JSON is selected via `--emit <stage>-json`.
    assert!(Cli::try_parse_from(["rumoca", "compile", "model.mo", "--json"]).is_err());
}

#[test]
fn fmi3_build_uses_fmi3_platform_tuple() {
    let (fmi2_platform, _) = crate::fmu::fmu_binary_platform(Some("fmi2")).expect("fmi2 platform");
    let (fmi3_platform, _) = crate::fmu::fmu_binary_platform(Some("fmi3")).expect("fmi3 platform");

    #[cfg(target_os = "linux")]
    {
        assert_eq!(fmi2_platform, "linux64");
        assert_eq!(fmi3_platform, "x86_64-linux");
    }
    #[cfg(target_os = "macos")]
    {
        assert_eq!(fmi2_platform, "darwin64");
        assert!(matches!(fmi3_platform, "aarch64-darwin" | "x86_64-darwin"));
    }
    #[cfg(target_os = "windows")]
    {
        assert_eq!(fmi2_platform, "win64");
        assert_eq!(fmi3_platform, "x86_64-windows");
    }
}

#[test]
fn cli_rejects_old_export_commands() {
    for command in ["export-fmu", "export-embedded-c"] {
        let err = Cli::try_parse_from(["rumoca", command]).expect_err("export command removed");
        assert!(
            err.to_string().contains("unrecognized subcommand"),
            "old command should not parse: {err}"
        );
    }
}

#[test]
fn infer_model_from_single_top_level_class() {
    let mut file = NamedTempFile::new().expect("temp file");
    writeln!(
        file,
        "model OnlyOne\n  Real x;\nequation\n  der(x)=1;\nend OnlyOne;"
    )
    .expect("write");
    let model = infer_model_name(file.path().to_str().expect("utf8 path")).expect("infer");
    assert_eq!(model, "OnlyOne");
}

#[test]
fn infer_model_by_file_stem_when_multiple_candidates() {
    let dir = tempfile::tempdir().expect("temp dir");
    let path = dir.path().join("Preferred.mo");
    std::fs::write(
        &path,
        "model Alternate\n  Real x;\nend Alternate;\nmodel Preferred\n  Real y;\nend Preferred;",
    )
    .expect("write");
    let model = infer_model_name(path.to_str().expect("utf8 path")).expect("infer");
    assert_eq!(model, "Preferred");
}

#[test]
fn infer_model_errors_when_ambiguous() {
    let mut file = NamedTempFile::new().expect("temp file");
    writeln!(
        file,
        "model Alpha\n  Real x;\nend Alpha;\nmodel Beta\n  Real y;\nend Beta;"
    )
    .expect("write");
    let error = infer_model_name(file.path().to_str().expect("utf8 path"))
        .expect_err("should fail without explicit model");
    assert!(
        error.to_string().contains("Pass --model <NAME>"),
        "unexpected error: {error}"
    );
}

#[test]
fn infer_model_from_recovered_parse_broken_file() {
    let mut file = NamedTempFile::new().expect("temp file");
    writeln!(file, "model Broken").expect("write");
    writeln!(file, "  Real x").expect("write");
    writeln!(file, "end Broken;").expect("write");
    let model = infer_model_name(file.path().to_str().expect("utf8 path"))
        .expect("recovery parser should preserve top-level model name");
    assert_eq!(model, "Broken");
}

#[test]
fn split_path_list_parses_multiple_entries() {
    let joined =
        std::env::join_paths([PathBuf::from("libA"), PathBuf::from("libB")]).expect("join paths");
    let parsed = split_path_list(Some(joined));
    assert_eq!(parsed, vec!["libA".to_string(), "libB".to_string()]);
}

#[test]
fn merged_source_root_paths_prefers_cli_then_env_and_dedups() {
    let cli = vec!["/tmp/rootA".to_string(), "/tmp/rootA".to_string()];
    let env_modelica = vec!["/tmp/rootB".to_string(), "/tmp/rootA".to_string()];
    let merged = merge_source_root_path_sources(&cli, &env_modelica);
    assert_eq!(
        merged,
        vec!["/tmp/rootA".to_string(), "/tmp/rootB".to_string()]
    );
}

#[test]
fn cli_error_report_preserves_compiler_diagnostics() {
    let error = anyhow::Error::new(CompilerError::ParseError("bad package layout".to_string()));
    let report = build_cli_error_report(&error);
    let rendered = format!("{report:?}");
    assert!(
        rendered.contains("E004"),
        "compiler errors should render via miette with their diagnostic code: {rendered}"
    );
    assert!(
        rendered.contains("bad package layout"),
        "compiler error message should be preserved: {rendered}"
    );
}

#[test]
fn cli_error_report_wraps_generic_errors_in_miette() {
    let error = anyhow::anyhow!("plain failure");
    let report = build_cli_error_report(&error);
    let rendered = format!("{report:?}");
    assert!(
        rendered.contains("plain failure"),
        "generic errors should still render through miette: {rendered}"
    );
}

#[test]
fn compile_target_flat_modelica_uses_flat_template_context() {
    let source = r#"
        model Test
          Real x(start = 0);
        equation
          der(x) = 1;
        end Test;
    "#;
    let result = Compiler::new()
        .model("Test")
        .compile_str(source, "Test.mo")
        .expect("model should compile");
    let output = tempfile::tempdir().expect("temp output dir");

    crate::target_manifest::compile_target(
        &result,
        "Test",
        "flat-modelica",
        Some(output.path().to_path_buf()),
        None,
    )
    .expect("flat-modelica target should render");

    let rendered =
        std::fs::read_to_string(output.path().join("Test_flat.mo")).expect("read output");
    assert!(
        rendered.contains("class Test"),
        "flat-modelica output should contain the rendered flat model: {rendered}"
    );
}
