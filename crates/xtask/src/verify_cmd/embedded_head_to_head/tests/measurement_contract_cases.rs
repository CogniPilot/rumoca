use super::*;

#[cfg(unix)]
#[test]
fn relative_and_symlinked_executables_retain_role_basenames_as_absolute_paths() {
    use std::os::unix::fs::symlink;

    let temporary = tempfile::tempdir().unwrap();
    let bin = temporary.path().join("bin");
    fs::create_dir_all(&bin).unwrap();
    symlink("/bin/sh", bin.join("python")).unwrap();
    let absolute = super::super::absolute_from(temporary.path(), Path::new("bin/python"));
    assert!(absolute.is_absolute());
    assert!(absolute.is_file());
    let typed =
        super::super::typed_path::RolePath::<super::super::typed_path::PythonExecutable>::checked(
            absolute.clone(),
        )
        .unwrap();
    assert_eq!(typed.as_path(), absolute);
    assert_ne!(fs::canonicalize(typed.as_path()).unwrap(), typed.as_path());
}

#[test]
fn staged_compiler_and_pre_execution_hash_survive_source_mutation() {
    let temporary = tempfile::tempdir().unwrap();
    let shared = temporary.path().join("target/debug/rumoca");
    let artifacts = temporary.path().join("target/verification/gate");
    fs::create_dir_all(shared.parent().unwrap()).unwrap();
    fs::write(&shared, "compiler bytes selected after successful build").unwrap();

    let closure = super::super::tool_closure::AuthenticatedToolClosure::for_test(temporary.path());
    let staged = super::super::cross::stage_compiler(&shared, &artifacts, &closure).unwrap();
    let selected_hash = staged.sha256().to_string();
    fs::write(&shared, "concurrent in-place cargo mutation").unwrap();

    assert_eq!(
        fs::read_to_string(staged.path().as_path()).unwrap(),
        "compiler bytes selected after successful build"
    );
    assert_eq!(
        super::super::sha256_file(staged.path().as_path()).unwrap(),
        selected_hash
    );
    assert_eq!(staged.sha256(), selected_hash);
    assert_ne!(super::super::sha256_file(&shared).unwrap(), selected_hash);
}

#[cfg(unix)]
#[test]
fn staged_compiler_mutation_is_rejected_at_the_execution_boundary() {
    use std::os::unix::fs::PermissionsExt;

    let temporary = tempfile::tempdir().unwrap();
    let row = entry();
    let root = fake_execution_root(temporary.path(), &row);
    let plan = super::super::cross::bind_execution_plan(&root, &row).unwrap();
    let compiler = root.compiler_path().as_path();
    let mut permissions = fs::metadata(compiler).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(compiler, permissions).unwrap();
    fs::write(compiler, "mutated staged compiler").unwrap();

    let error = super::super::cross::rumoca_generation_command(
        &plan,
        &super::super::typed_path::RolePath::normalized(),
        &super::super::typed_path::RolePath::normalized(),
        &super::super::typed_path::RolePath::normalized(),
    )
    .err()
    .expect("changed staged compiler must fail before command construction");
    assert!(
        error
            .to_string()
            .contains("digest changed before execution")
    );
}

#[test]
fn setup_receipts_preserve_git_probe_and_build_execution_order() {
    let baseline = [
        "01-git-version",
        "02-rev-parse",
        "03-ls-tree",
        "04-cat-file",
    ]
    .map(super::super::process::test_receipt);
    let probes = ["05-gcc", "06-nm", "07-qemu", "08-python", "09-prlimit"]
        .map(super::super::process::test_receipt);
    let compiler_source = [
        "10-source-head-pre",
        "11-source-roster-pre",
        "12-source-deleted-pre",
        "13-source-status-pre",
        "14-source-head-post",
        "15-source-roster-post",
        "16-source-deleted-post",
        "17-source-status-post",
    ]
    .map(super::super::process::test_receipt);
    let mut ledger = super::super::SetupLedger::new();
    ledger.commands.extend(baseline);
    ledger.commands.extend(probes);
    ledger.commands.extend(compiler_source);
    ledger
        .commands
        .push(super::super::process::test_receipt("18-compiler-build"));
    let commands = ledger.commands;
    assert_eq!(
        commands
            .iter()
            .map(super::super::process::CommandReceipt::display_only)
            .collect::<Vec<_>>(),
        [
            "01-git-version",
            "02-rev-parse",
            "03-ls-tree",
            "04-cat-file",
            "05-gcc",
            "06-nm",
            "07-qemu",
            "08-python",
            "09-prlimit",
            "10-source-head-pre",
            "11-source-roster-pre",
            "12-source-deleted-pre",
            "13-source-status-pre",
            "14-source-head-post",
            "15-source-roster-post",
            "16-source-deleted-post",
            "17-source-status-post",
            "18-compiler-build",
        ]
    );
}

#[test]
fn setup_evidence_retains_mandatory_authenticated_git_and_source_identity() {
    fn setup(git_tool: super::super::GitToolEvidence) -> super::super::SetupEvidence {
        super::super::SetupEvidence {
            commands: Vec::new(),
            versions: super::super::ToolVersions {
                arm_gcc: "gcc".into(),
                arm_nm: "nm".into(),
                qemu: "qemu".into(),
                python: "python".into(),
                prlimit: "prlimit".into(),
            },
            tool_sha256: super::super::ToolHashes {
                arm_gcc: "1".repeat(64),
                arm_nm: "2".repeat(64),
                qemu: "3".repeat(64),
                python: "4".repeat(64),
                prlimit: "5".repeat(64),
            },
            git_tool,
            compiler_source: super::super::compiler_source::CompilerSourceEvidence {
                head_commit: "a".repeat(40),
                workspace_dirty: true,
                workspace_status_sha256: "9".repeat(64),
                closure_sha256: "a".repeat(64),
                roster_count: 42,
            },
            compiler_dependencies: super::super::compiler_deps::DependencyEvidence {
                closure_sha256: "c".repeat(64),
                registry_packages: 7,
                full_registry_packages: 6,
                resolution_manifest_packages: 1,
                local_packages: 3,
                build_dependency_edges: 2,
                proc_macro_packages: 1,
                manifest_artifact: super::super::artifact_bundle::FileEvidence {
                    relative_path: "evidence-bundle/setup/compiler-dependencies.json".into(),
                    sha256: "d".repeat(64),
                    bytes: 456,
                },
            },
            compiler_toolchain: super::super::CompilerToolchainEvidence {
                cargo_version: "cargo reviewed".into(),
                cargo_sha256: "e".repeat(64),
                rustc_version: "rustc reviewed".into(),
                rustc_sha256: "f".repeat(64),
                host_target: super::super::process::COMPILER_HOST_TARGET.into(),
                rust_sysroot: "/nix/store/rust".into(),
                rust_sysroot_tree_sha256: "0".repeat(64),
                nix_path_sha256: "1".repeat(64),
                nix_store_input_roots: vec!["/nix/store/reviewed-toolchain".into()],
                resolver_cargo_home: "/source/cargo/home".into(),
            },
            tool_closure: super::super::tool_closure::ToolClosureEvidence {
                sha256: "8".repeat(64),
                roots: vec![super::super::tool_closure::ToolClosureRootEvidence {
                    store_root: "/nix/store/test".into(),
                    nar_hash: "sha256:test".into(),
                }],
            },
            compiler_build_environment_sha256: "b".repeat(64),
            compiler_sha256: "6".repeat(64),
            compiler_artifact: super::super::artifact_bundle::FileEvidence {
                relative_path: "evidence-bundle/setup/compiler/rumoca".into(),
                sha256: "6".repeat(64),
                bytes: 123,
            },
            compiler_runtime_environment_sha256: "7".repeat(64),
            suite_implementation_sha256: "2".repeat(64),
            suite_implementation_history_sha256: "3".repeat(64),
        }
    }

    let enabled = serde_json::to_value(setup(super::super::GitToolEvidence {
        version: "git version reviewed".into(),
        sha256: "8".repeat(64),
    }))
    .unwrap();
    assert_eq!(enabled["git_tool"]["version"], "git version reviewed");
    assert_eq!(enabled["git_tool"]["sha256"], "8".repeat(64));
    assert_eq!(enabled["compiler_source"]["roster_count"], 42);
    assert_eq!(enabled["compiler_source"]["workspace_dirty"], true);
    assert_eq!(enabled["compiler_build_environment_sha256"], "b".repeat(64));
    assert_eq!(enabled["suite_implementation_sha256"], "2".repeat(64));
    assert_eq!(
        enabled["suite_implementation_history_sha256"],
        "3".repeat(64)
    );
}

#[test]
fn checked_input_header_must_be_the_header_resolved_by_the_drivers() {
    let temporary = tempfile::tempdir().unwrap();
    let mut row = entry();
    row.fixture = "case/fixture.mo".into();
    row.input_header = "authenticated-but-unused/inputs.h".into();
    let error =
        super::super::snapshot::stage(temporary.path(), &row, &temporary.path().join("snapshot"))
            .err()
            .expect("an authenticated but uncompiled header must be rejected");
    assert!(format!("{error:#}").contains("is not the compiled driver header"));
}

#[test]
fn authenticated_snapshot_is_immune_to_later_workspace_mutation() {
    let root = repository_root();
    let row = real_entry(&root);
    let temporary = tempfile::tempdir().unwrap();
    let workspace = temporary.path().join("workspace");
    copy_checked_workspace(&root, &row, &workspace);
    let inputs = super::super::snapshot::stage(
        &workspace,
        &row,
        &temporary.path().join("gate/authenticated-inputs"),
    )
    .unwrap();
    let before = inputs
        .staged_paths()
        .into_iter()
        .map(|path| (path.to_path_buf(), fs::read(path).unwrap()))
        .collect::<Vec<_>>();

    for relative in [
        &row.fixture,
        &row.comparator_generator,
        &row.comparator_wrapper,
        &row.input_header,
    ] {
        fs::write(workspace.join(relative), b"mutated after snapshot\n").unwrap();
    }
    let harness = workspace
        .join(&row.fixture)
        .parent()
        .unwrap()
        .join("harness");
    fs::write(
        harness.join("startup.S"),
        b"mutated harness after snapshot\n",
    )
    .unwrap();

    for (path, bytes) in before {
        assert!(path.starts_with(inputs.root()));
        assert_eq!(fs::read(path).unwrap(), bytes);
    }
    assert!(inputs.fixture().as_path().starts_with(inputs.root()));
    assert!(inputs.generator().as_path().starts_with(inputs.root()));
    assert!(inputs.wrapper().as_path().starts_with(inputs.root()));

    assert_generation_bindings_use_snapshot(temporary.path(), &workspace, &row, &inputs);
    assert_build_bindings_use_snapshot(temporary.path(), &workspace, &row, &inputs);
}

#[test]
fn authenticated_snapshot_rejects_extra_and_missing_harness_members() {
    let root = repository_root();
    let row = real_entry(&root);
    for mutation in ["extra", "missing"] {
        let temporary = tempfile::tempdir().unwrap();
        let workspace = temporary.path().join("workspace");
        copy_checked_workspace(&root, &row, &workspace);
        let harness = workspace
            .join(&row.fixture)
            .parent()
            .unwrap()
            .join("harness");
        if mutation == "extra" {
            fs::write(harness.join("unreviewed.c"), "unexpected\n").unwrap();
        } else {
            fs::remove_file(harness.join("startup.S")).unwrap();
        }
        let error =
            super::super::snapshot::stage(&workspace, &row, &temporary.path().join("snapshot"))
                .err()
                .expect("a non-exact harness roster must fail closed");
        assert!(format!("{error:#}").contains("harness roster"));
    }
}

#[cfg(unix)]
#[test]
fn adjacent_stale_cannot_override_fresh_casadi_emission() {
    use std::os::unix::fs::PermissionsExt;

    let temporary = tempfile::tempdir().unwrap();
    let output = temporary.path().join("emitted/casadi");
    let fresh_c = [
        "  if (sz_arg) *sz_arg = 4;",
        "  if (sz_res) *sz_res = 1;",
        "  if (sz_iw) *sz_iw = 0;",
        "  if (sz_w) *sz_w = 0;",
        "  if (sz_arg) *sz_arg = 4*sizeof(const casadi_real*);",
        "  if (sz_res) *sz_res = 1*sizeof(casadi_real*);",
        "  if (sz_iw) *sz_iw = 0*sizeof(casadi_int);",
        "  if (sz_w) *sz_w = 0*sizeof(casadi_real);",
    ]
    .join("\n")
        + "\n";
    let fresh_h = [
        "#define exp_mixed_full_SZ_ARG 4",
        "#define exp_mixed_full_SZ_RES 1",
        "#define exp_mixed_full_SZ_IW 0",
        "#define exp_mixed_full_SZ_W 0",
    ]
    .join("\n")
        + "\n";
    let c_arguments = fresh_c
        .lines()
        .map(|line| format!("'{line}'"))
        .collect::<Vec<_>>()
        .join(" ");
    let h_arguments = fresh_h
        .lines()
        .map(|line| format!("'{line}'"))
        .collect::<Vec<_>>()
        .join(" ");
    let generator = format!(
        "while [ \"$1\" != \"--out\" ]; do shift; done\nshift\nout=$1\nprintf '%s\\n' {c_arguments} > \"$out/casadi_exp_mixed.c\"\nprintf '%s\\n' {h_arguments} > \"$out/casadi_exp_mixed.h\"\n"
    );
    let wrapper = "#include \"casadi_exp_mixed.c\"\n";
    let workspace = temporary.path().join("workspace");
    let root = repository_root();
    let mut row = real_entry(&root);
    copy_checked_workspace(&root, &row, &workspace);
    let fixture_root = workspace.join(&row.fixture).parent().unwrap().to_path_buf();
    fs::write(workspace.join(&row.comparator_generator), &generator).unwrap();
    fs::write(workspace.join(&row.comparator_wrapper), wrapper).unwrap();
    fs::write(
        fixture_root.join("casadi/casadi_exp_mixed.c"),
        "stale adjacent c\n",
    )
    .unwrap();
    row.comparator_generator_sha256 = format!("{:x}", Sha256::digest(generator.as_bytes()));
    row.comparator_wrapper_sha256 = format!("{:x}", Sha256::digest(wrapper.as_bytes()));
    let inputs = super::super::snapshot::stage(
        &workspace,
        &row,
        &temporary.path().join("authenticated-inputs"),
    )
    .unwrap();
    let python_path = temporary.path().join("bin/python");
    fs::create_dir_all(python_path.parent().unwrap()).unwrap();
    fs::write(&python_path, "#!/bin/sh\nexec /bin/sh \"$@\"\n").unwrap();
    let mut permissions = fs::metadata(&python_path).unwrap().permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&python_path, permissions).unwrap();
    let pins = ComparatorOutputPins {
        casadi_c_sha256: format!("{:x}", Sha256::digest(fresh_c.as_bytes())),
        casadi_h_sha256: format!("{:x}", Sha256::digest(fresh_h.as_bytes())),
    };
    let plan = fake_bound_plan_with_pins(temporary.path(), &row, pins);

    let emission = emit::emit_casadi(&inputs, &plan, &output).unwrap();
    assert!(emission.wrapper_c.as_path().starts_with(inputs.root()));
    assert_eq!(
        fs::read_to_string(emission.wrapper_c.as_path()).unwrap(),
        "#include \"casadi_exp_mixed.c\"\n"
    );
    assert_eq!(
        fs::read_to_string(emission.generated_c.as_path()).unwrap(),
        fresh_c
    );
}

#[cfg(unix)]
#[test]
fn stale_hand_edited_winner_cannot_be_measured() {
    let temporary = tempfile::tempdir().unwrap();
    let (row, inputs) = staged_real_inputs(temporary.path());
    let output = temporary.path().join("emitted/rumoca");
    fs::create_dir_all(&output).unwrap();
    let stale = output.join("ExpMixedStep.c");
    fs::write(&stale, "void hand_edited_winner(void) {}\n").unwrap();
    for name in [
        ".clang-format",
        "ExpMixedStep.h",
        "rumoca_galec_kernels.c",
        "rumoca_galec_kernels.h",
    ] {
        fs::write(output.join(name), "hand edited\n").unwrap();
    }
    let plan = fake_bound_plan(temporary.path(), &row);
    let cache =
        super::super::typed_path::RolePath::checked(temporary.path().join("cache")).unwrap();
    let result = emit::emit_rumoca(&inputs, &plan, &output, &cache);
    let error = result.err().expect("zero-exit fake must remain unmeasured");
    assert!(
        !stale.exists(),
        "fresh emission must delete the hand-edited C"
    );
    assert!(format!("{error:#}").contains("emission roster"));
}

#[test]
fn rejected_measurement_has_no_comparison_outcome() {
    let mut row = entry();
    let evidence = super::super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: 1251,
        comparator_instructions: 1084,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    };
    let verdict = judge(&row, evidence, 0.0);
    rejected_findings(&verdict);
    let wire = serde_json::to_value(&verdict).unwrap();
    assert_eq!(wire["status"], "rejected");
    assert!(wire.get("delta").is_none());
    assert!(wire.get("comparison_outcome").is_none());

    row.max_delta = Some(0);
    let BaselineMeasurement::Accepted(measured) = &mut row.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    measured.rumoca = measured.comparator;
    assert!(super::super::manifest::validate(&manifest(vec![row])).is_ok());

    let mut dominant = entry();
    dominant.max_delta = Some(-1);
    let BaselineMeasurement::Accepted(measured) = &mut dominant.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    measured.rumoca = measured.comparator - 1;
    assert!(super::super::manifest::validate(&manifest(vec![dominant])).is_ok());
}

#[test]
fn improvements_require_explicit_promotion_and_promoted_rows_reject_old_counts() {
    let mut row = entry();
    let BaselineMeasurement::Accepted(measured) = &row.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let improvement = measured.rumoca - 1;
    let verdict = judge(
        &row,
        instruction_evidence(improvement, measured.comparator),
        0.0,
    );
    assert!(
        rejected_findings(&verdict)
            .iter()
            .any(|finding| finding.contains("explicitly promote measured.rumoca and max_delta"))
    );

    let BaselineMeasurement::Accepted(measured) = &mut row.measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    let old = measured.rumoca;
    measured.rumoca = improvement;
    let comparator = measured.comparator;
    *row.max_delta.as_mut().unwrap() -= 1;
    super::super::manifest::validate(&manifest(vec![row.clone()])).unwrap();
    let old_count = judge(&row, instruction_evidence(old, comparator), 0.0);
    assert!(
        rejected_findings(&old_count)
            .iter()
            .any(|finding| finding.contains("Rumoca instructions regressed"))
    );
}

#[test]
fn pending_schema_nine_row_cannot_import_a_coordinated_baseline_edit() {
    let prior = manifest(vec![entry()]);
    let mut current = prior.clone();
    let BaselineMeasurement::Accepted(measured) = &mut current.entries[0].measured else {
        unreachable!("entry fixture always carries an accepted baseline");
    };
    measured.rumoca += 1;
    *current.entries[0].max_delta.as_mut().unwrap() += 1;

    assert!(
        super::super::manifest::validate(&current).is_ok(),
        "the ordinary self-consistency check intentionally cannot authenticate history"
    );
    let error = super::super::manifest::validate_non_relaxation(&current, &prior)
        .expect_err("schema nine must reject coordinated baseline edits");
    assert!(
        error.to_string().contains("benchmark identity changed"),
        "{error:#}"
    );
}

#[test]
fn reviewed_row_cannot_be_removed_or_redefined_under_the_same_id() {
    let prior = manifest(vec![entry()]);
    let removed = manifest(Vec::new());
    assert!(super::super::manifest::validate_non_relaxation(&removed, &prior).is_err());

    let mut redefined = prior.clone();
    redefined.entries[0].fixture_sha256 = "1".repeat(64);
    let error = super::super::manifest::validate_non_relaxation(&redefined, &prior)
        .expect_err("an easier fixture needs a new benchmark identity");
    assert!(error.to_string().contains("benchmark identity changed"));
}

#[test]
fn reviewed_row_cannot_hide_cross_flag_drift_behind_the_same_profile_name() {
    let prior = manifest(vec![entry()]);
    let mut redefined = prior.clone();
    redefined.entries[0].normalized_profile_sha256 = "1".repeat(64);
    let error = super::super::manifest::validate_non_relaxation(&redefined, &prior)
        .expect_err("cross flag drift needs a new benchmark identity");
    assert!(
        format!("{error:#}").contains("benchmark identity changed"),
        "{error:#}"
    );
}

#[test]
fn historical_ratchet_rejects_tool_and_generated_output_pin_migration() {
    let prior = manifest(vec![entry()]);
    let mut tool_migration = prior.clone();
    tool_migration.measured_tools.arm_gcc_sha256 = "1".repeat(64);
    assert!(
        super::super::manifest::validate_non_relaxation(&tool_migration, &prior)
            .unwrap_err()
            .to_string()
            .contains("measured-tool SHA-256 pins changed")
    );

    let mut output_migration = prior.clone();
    output_migration.comparator_outputs.casadi_c_sha256 = "2".repeat(64);
    assert!(
        super::super::manifest::validate_non_relaxation(&output_migration, &prior)
            .unwrap_err()
            .to_string()
            .contains("comparator-output SHA-256 pins changed")
    );
}

#[test]
fn historical_suite_identity_is_self_contained_and_replacements_are_append_only() {
    let current_digest = super::super::suite_identity::sha256();
    let prior_digest = "1".repeat(64);
    let mut prior_wire = serde_json::to_value(manifest(vec![entry()])).unwrap();
    prior_wire["suite_implementation_history"]["initial_sha256"] = serde_json::json!(prior_digest);
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let historical = super::super::manifest::parse_historical(
        &serde_json::to_string(&prior).unwrap(),
        "historical-runner",
    )
    .expect("historical validation must not compare against today's runner bytes");

    let mut current_wire = serde_json::to_value(&prior).unwrap();
    current_wire["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": current_digest,
        "rationale": "reviewed runner-only replacement"
    }]);
    let current: Manifest = serde_json::from_value(current_wire).unwrap();
    super::super::manifest::validate_historical_non_relaxation(
        &checked_current(&current),
        &historical,
    )
    .expect("an appended runner replacement must preserve benchmark row identity");
    assert_eq!(
        current.entries[0].normalized_profile_sha256, prior.entries[0].normalized_profile_sha256,
        "runner identity must not contaminate immutable benchmark protocol identity"
    );

    let mut rewritten = serde_json::to_value(&current).unwrap();
    rewritten["suite_implementation_history"]["initial_sha256"] = serde_json::json!("2".repeat(64));
    let rewritten: Manifest = serde_json::from_value(rewritten).unwrap();
    let error = super::super::manifest::validate_non_relaxation(&rewritten, &prior)
        .expect_err("runner history cannot be rewritten");
    assert!(format!("{error:#}").contains("history was rewritten"));
}

#[test]
fn checked_in_suite_replacement_binds_current_bytes_and_rejects_history_mutations() {
    let root = repository_root();
    let current = super::super::manifest::load(&super::super::manifest::path(&root))
        .expect("the checked-in replacement must bind the current trusted byte closure");
    let current_wire = serde_json::to_value(&current).unwrap();
    let replacements = current_wire["suite_implementation_history"]["replacements"]
        .as_array()
        .unwrap();
    assert_eq!(
        replacements.len(),
        1,
        "this comparison authenticates exactly one implementation replacement"
    );
    assert_eq!(
        replacements[0]["implementation_sha256"],
        super::super::suite_identity::sha256()
    );

    let mut stale_wire = current_wire.clone();
    stale_wire["suite_implementation_history"]["replacements"][0]["implementation_sha256"] =
        serde_json::json!("f".repeat(64));
    let stale: Manifest = serde_json::from_value(stale_wire).unwrap();
    let error = super::super::manifest::validate(&stale)
        .expect_err("a stale active replacement must not authenticate current runner bytes");
    assert!(
        format!("{error:#}").contains("trusted benchmark implementation identity changed"),
        "{error:#}"
    );

    let mut prior_wire = current_wire.clone();
    prior_wire["suite_implementation_history"]["replacements"] = serde_json::json!([]);
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let mut rewritten_wire = current_wire;
    rewritten_wire["suite_implementation_history"]["initial_sha256"] =
        serde_json::json!("e".repeat(64));
    let rewritten: Manifest = serde_json::from_value(rewritten_wire).unwrap();
    let error = super::super::manifest::validate_non_relaxation(&rewritten, &prior)
        .expect_err("an active replacement must not conceal rewritten predecessor history");
    assert!(
        format!("{error:#}").contains("history was rewritten"),
        "{error:#}"
    );
}

#[test]
fn suite_implementation_history_rejects_duplicates_and_semantic_authority() {
    let current_digest = super::super::suite_identity::sha256();
    let mut duplicate = serde_json::to_value(manifest(vec![entry()])).unwrap();
    duplicate["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": current_digest,
        "rationale": "invalid reversion"
    }]);
    let duplicate: Manifest = serde_json::from_value(duplicate).unwrap();
    assert!(super::super::manifest::validate(&duplicate).is_err());

    let prior_digest = "1".repeat(64);
    let mut prior_wire = serde_json::to_value(manifest(vec![entry()])).unwrap();
    prior_wire["suite_implementation_history"]["initial_sha256"] = serde_json::json!(prior_digest);
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let mut current_wire = serde_json::to_value(&prior).unwrap();
    current_wire["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": super::super::suite_identity::sha256(),
        "rationale": "reviewed runner-only replacement"
    }]);
    current_wire["measured_tools"]["qemu_sha256"] = serde_json::json!("3".repeat(64));
    let current: Manifest = serde_json::from_value(current_wire).unwrap();
    let error = super::super::manifest::validate_non_relaxation(&current, &prior)
        .expect_err("a runner replacement cannot authorize a tool migration");
    assert!(format!("{error:#}").contains("measured-tool SHA-256 pins changed"));
}

#[test]
fn schema_nine_forbids_tool_and_existing_row_identity_migration() {
    let prior = manifest(vec![entry()]);
    let historical = historical_current(&prior);

    let mut changed_tool = prior.clone();
    changed_tool.measured_tools.compiler_dependencies_sha256 = "2".repeat(64);
    let error = super::super::manifest::validate_historical_non_relaxation(
        &checked_current(&changed_tool),
        &historical,
    )
    .expect_err("schema nine must not authenticate caller-supplied migration evidence");
    assert!(format!("{error:#}").contains("schema 9 has no migration authority"));

    let mut changed_policy = prior.clone();
    changed_policy.ratchet_policy = "relaxed policy".into();
    let error = super::super::manifest::validate_non_relaxation(&changed_policy, &prior)
        .expect_err("runner changes cannot authorize benchmark-protocol drift");
    assert!(format!("{error:#}").contains("ratchet policy changed"));

    let mut changed_identity = prior;
    changed_identity.entries[0].normalized_profile_sha256 = "3".repeat(64);
    let error = super::super::manifest::validate_historical_non_relaxation(
        &checked_current(&changed_identity),
        &historical,
    )
    .expect_err("an existing row identity is immutable");
    assert!(format!("{error:#}").contains("add a new row instead"));

    let mut added_row = changed_identity;
    added_row.entries[0] = entry();
    let mut second = entry();
    second.id = "second-schema-nine-row".into();
    added_row.entries.push(second);
    let error = super::super::manifest::validate_historical_non_relaxation(
        &checked_current(&added_row),
        &historical,
    )
    .expect_err("schema nine cannot add another competitor row");
    assert!(format!("{error:#}").contains("no authority to add"));
}

#[test]
fn historical_manifest_rejects_uncommitted_schema_lineage() {
    let mut value = serde_json::to_value(manifest(vec![entry()])).unwrap();
    value["schema_version"] = serde_json::json!(7);
    let text = serde_json::to_string(&value).unwrap();
    let error = super::super::manifest::parse_historical(&text, "test-schema-seven")
        .expect_err("the superseded schema must not acquire a compatibility reader");
    assert!(format!("{error:#}").contains("unsupported; expected 9"));
}

fn historical_current(manifest: &Manifest) -> super::super::manifest::HistoricalManifest {
    let text = serde_json::to_string(manifest).unwrap();
    super::super::manifest::parse_historical(&text, "test-current").unwrap()
}

#[test]
fn pending_schema_nine_ci_job_is_suspended_but_retains_baseline_contract() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let workflow = fs::read_to_string(root.join(".github/workflows/ci.yml")).unwrap();
    let job = workflow
        .split("\n  embedded-head-to-head:\n")
        .nth(1)
        .and_then(|tail| tail.split("\n  kani:\n").next())
        .expect("CI owns one embedded-head-to-head job before kani");
    for required in [
        "if: ${{ false }}",
        "fetch-depth: 0",
        "github.event.pull_request.base.sha || github.event.before",
        "--baseline-git-revision \"$EMBEDDED_BASE_SHA\"",
    ] {
        assert!(
            job.contains(required),
            "the PR instruction ratchet lost historical authority `{required}`"
        );
    }
    for forbidden in [
        "git cat-file",
        "git show",
        "baseline_manifest",
        "baseline_args",
    ] {
        assert!(
            !job.contains(forbidden),
            "workflow must delegate Git baseline authority instead of containing `{forbidden}`"
        );
    }
}

#[test]
fn ci_and_nightly_publish_the_same_fail_closed_authenticated_bundle_contract() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for (workflow, next_job, baseline) in [
        (
            "ci.yml",
            "kani",
            "github.event.pull_request.base.sha || github.event.before",
        ),
        ("nightly.yml", "parser-fuzz", "EMBEDDED_BASE_SHA: HEAD^"),
    ] {
        let text = fs::read_to_string(root.join(".github/workflows").join(workflow)).unwrap();
        let job = text
            .split("\n  embedded-head-to-head:\n")
            .nth(1)
            .and_then(|tail| tail.split(&format!("\n  {next_job}:\n")).next())
            .unwrap();
        for required in [
            "fetch-depth: 0",
            "--arm-toolchain \"$arm_root\"",
            "--qemu \"$qemu\"",
            "--python \"$python\"",
            "--prlimit \"$prlimit\"",
            "--baseline-git-revision \"$EMBEDDED_BASE_SHA\"",
            baseline,
            "Require embedded head-to-head evidence on success",
            "if: always()",
            "target/verification/embedded-head-to-head-summary.json",
            "target/verification/embedded-head-to-head/evidence-bundle/**",
            "if-no-files-found: error",
        ] {
            assert!(job.contains(required), "{workflow} lost `{required}`");
        }
        assert!(!job.contains("if-no-files-found: warn"));
    }
}

#[test]
fn git_baseline_loader_reads_the_exact_present_blob() {
    let baseline = manifest(vec![entry()]);
    let repository = git_repository(Some(&baseline));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);
    commit_file(
        repository.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );
    let git = git_executable();
    let loaded = super::super::manifest::authenticate_git_history(
        &checked_current(&baseline),
        repository.path(),
        &git,
        predecessor.trim(),
    )
    .unwrap();
    assert!(!loaded.evidence().is_bootstrap());
    assert!(loaded.evidence().predecessor_manifest_sha256().is_some());
    assert_eq!(loaded.commands().len(), 7);
    for (receipt, expected) in loaded.commands().iter().zip([
        "rev-parse",
        "rev-parse",
        "merge-base",
        "is-shallow-repository",
        "rev-list",
        "ls-tree",
        "cat-file",
    ]) {
        assert!(receipt.display_only().contains(expected));
        assert!(receipt.display_only().contains("--no-replace-objects"));
        assert!(receipt.is_hermetic());
        for forbidden in [
            "GIT_DIR",
            "GIT_WORK_TREE",
            "GIT_OBJECT_DIRECTORY",
            "GIT_ALTERNATE_OBJECT_DIRECTORIES",
        ] {
            assert!(!receipt.has_environment_name(forbidden));
        }
        assert!(receipt.has_environment_name("GIT_NO_REPLACE_OBJECTS"));
        assert!(receipt.has_environment_name("GIT_CONFIG_NOSYSTEM"));
    }
}

#[test]
fn ambient_fake_git_candidate_is_rejected_by_reviewed_sha_pin() {
    let temporary = tempfile::tempdir().unwrap();
    let fake = temporary.path().join("git");
    fs::write(&fake, "#!/bin/sh\necho forged git\n").unwrap();
    let pins = manifest(vec![entry()]).measured_tools;
    let error = super::super::authenticate_git_candidate(fake, &pins)
        .err()
        .expect("fake Git must not authenticate");
    assert!(error.to_string().contains("SHA-256 changed before use"));
}

#[test]
fn git_baseline_loader_ignores_replace_refs() {
    let baseline = manifest(vec![entry()]);
    let repository = git_repository(Some(&baseline));
    let original = git(repository.path(), &["rev-parse", "HEAD"]);
    fs::remove_file(
        repository
            .path()
            .join(super::super::manifest::MANIFEST_PATH),
    )
    .unwrap();
    fs::write(
        repository.path().join("replacement.txt"),
        "replacement tree\n",
    )
    .unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "replacement",
        ],
    );
    let replacement = git(repository.path(), &["rev-parse", "HEAD"]);
    git(
        repository.path(),
        &["replace", original.trim(), replacement.trim()],
    );

    let git = git_executable();
    let loaded = super::super::manifest::authenticate_git_history(
        &checked_current(&baseline),
        repository.path(),
        &git,
        original.trim(),
    )
    .unwrap();
    assert_eq!(loaded.evidence().predecessor_commit(), original.trim());
    assert!(!loaded.evidence().is_bootstrap());
}

#[test]
fn git_baseline_loader_binds_a_worktree_git_directory_explicitly() {
    let baseline = manifest(vec![entry()]);
    let repository = git_repository(Some(&baseline));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);
    commit_file(
        repository.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );
    let worktree_parent = tempfile::tempdir().unwrap();
    let worktree = worktree_parent.path().join("checked-out-worktree");
    git(
        repository.path(),
        &[
            "worktree",
            "add",
            "--quiet",
            "--detach",
            worktree.to_str().unwrap(),
            "HEAD",
        ],
    );
    assert!(worktree.join(".git").is_file());

    let git = git_executable();
    let loaded = super::super::manifest::authenticate_git_history(
        &checked_current(&baseline),
        &worktree,
        &git,
        predecessor.trim(),
    )
    .unwrap();
    assert!(!loaded.evidence().is_bootstrap());
    assert!(
        loaded
            .commands()
            .iter()
            .all(|receipt| receipt.is_hermetic())
    );
}

#[test]
fn git_baseline_loader_bootstraps_only_on_successful_exact_absence() {
    let repository = git_repository(None);
    let git = git_executable();
    let current = manifest(vec![entry()]);
    let loaded = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        "HEAD",
    )
    .unwrap();
    assert!(loaded.evidence().is_bootstrap());
    assert!(loaded.evidence().predecessor_manifest_sha256().is_none());
    assert_eq!(loaded.commands().len(), 5);
    assert!(loaded.commands()[0].display_only().contains("rev-parse"));
    assert!(loaded.commands()[2].display_only().contains("merge-base"));
    assert!(
        loaded.commands()[3]
            .display_only()
            .contains("is-shallow-repository")
    );
    assert!(loaded.commands()[4].display_only().contains("rev-list"));
    assert!(
        loaded.commands()[4]
            .display_only()
            .contains(super::super::manifest::MANIFEST_PATH)
    );
    assert!(
        serde_json::to_value(manifest(vec![entry()]))
            .unwrap()
            .get("migration_ledger")
            .is_none()
    );
}

#[test]
fn bootstrap_cannot_introduce_selected_efmu_artifact_history() {
    let mut row = pending_entry();
    row.rumoca_artifact_history = efmu_entry().rumoca_artifact_history;
    let error = super::super::manifest::validate(&manifest(vec![row])).unwrap_err();
    assert!(format!("{error:#}").contains("cannot carry artifact history"));
}

#[test]
fn committed_first_introduction_binds_the_exact_head_blob() {
    let current = manifest(vec![entry()]);
    let repository = git_repository(Some(&current));
    let loaded = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        "HEAD",
    )
    .unwrap();
    assert!(loaded.evidence().is_bootstrap());
    assert!(
        loaded
            .commands()
            .iter()
            .any(|receipt| receipt.display_only().contains("cat-file blob"))
    );

    let mut mismatched = current;
    mismatched.ratchet_policy.push_str(" changed");
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&mismatched),
        repository.path(),
        &git_executable(),
        "HEAD",
    )
    .err()
    .expect("HEAD-only bootstrap must bind the exact checked manifest bytes");
    assert!(
        format!("{error:#}").contains("differ from the checked current manifest"),
        "{error:#}"
    );
}

#[test]
fn pre_manifest_baseline_cannot_reenter_bootstrap_after_history_exists() {
    let repository = git_repository(None);
    let pre_manifest = git(repository.path(), &["rev-parse", "HEAD"]);
    let current = manifest(vec![entry()]);
    let manifest_path = repository
        .path()
        .join(super::super::manifest::MANIFEST_PATH);
    fs::create_dir_all(manifest_path.parent().unwrap()).unwrap();
    fs::write(&manifest_path, serde_json::to_vec_pretty(&current).unwrap()).unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "introduce manifest",
        ],
    );
    commit_file(
        repository.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );

    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        pre_manifest.trim(),
    )
    .err()
    .expect("caller-selected point absence must not authorize bootstrap");
    assert!(
        format!("{error:#}").contains("strict-ancestor history does"),
        "{error:#}"
    );
}

#[test]
fn shallow_history_cannot_authorize_bootstrap_or_predecessor_evidence() {
    let current = manifest(vec![entry()]);
    let origin = git_repository(Some(&current));
    commit_file(
        origin.path(),
        "after.txt",
        "later compiler change\n",
        "later compiler change",
    );
    let clone_parent = tempfile::tempdir().unwrap();
    let clone = clone_parent.path().join("shallow");
    let output = Command::new("git")
        .args(["clone", "--quiet", "--depth", "1"])
        .arg(format!("file://{}", origin.path().display()))
        .arg(&clone)
        .output()
        .unwrap();
    assert!(output.status.success());
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        &clone,
        &git_executable(),
        "HEAD",
    )
    .err()
    .expect("a shallow repository cannot prove global manifest history");
    assert!(format!("{error:#}").contains("require complete Git history"));
}

#[test]
fn git_baseline_loader_rejects_an_invalid_revision() {
    let repository = git_repository(None);
    let git = git_executable();
    let current = manifest(vec![entry()]);
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        "not-a-revision",
    )
    .err()
    .expect("an unauthenticated revision must never bootstrap");
    assert!(
        format!("{error:#}").contains("authenticate embedded head-to-head baseline commit"),
        "{error:#}"
    );
}

#[test]
fn git_baseline_loader_rejects_a_missing_tree_object() {
    let repository = git_repository(None);
    let tree = git(repository.path(), &["rev-parse", "HEAD^{tree}"]);
    let tree = tree.trim();
    let object = repository
        .path()
        .join(".git/objects")
        .join(&tree[..2])
        .join(&tree[2..]);
    assert!(object.is_file(), "test tree object must be loose");
    fs::remove_file(object).unwrap();

    let git = git_executable();
    let current = manifest(vec![entry()]);
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        "HEAD",
    )
    .err()
    .expect("a missing tree object must fail instead of bootstrap");
    assert!(
        format!("{error:#}").contains("authenticate embedded head-to-head manifest history"),
        "{error:#}"
    );
}

#[test]
fn authenticated_history_rejects_a_nonancestor_commit_before_bootstrap() {
    let repository = git_repository(None);
    let unrelated = git(repository.path(), &["rev-parse", "HEAD"]);
    git(
        repository.path(),
        &["checkout", "--quiet", "--orphan", "other"],
    );
    fs::write(repository.path().join("other.txt"), "unrelated history\n").unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "unrelated",
        ],
    );
    let current = manifest(vec![entry()]);
    let git = git_executable();
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git,
        unrelated.trim(),
    )
    .err()
    .expect("a nonancestor must never authorize bootstrap");
    assert!(
        format!("{error:#}").contains("is not an ancestor"),
        "{error:#}"
    );
}

#[test]
fn intermediate_commit_cannot_authorize_schema_nine_semantic_migration() {
    let prior = manifest(vec![entry()]);
    let repository = git_repository(Some(&prior));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);

    fs::write(
        repository.path().join("reviewed-evidence.txt"),
        "independent migration review\n",
    )
    .unwrap();
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "review migration evidence",
        ],
    );
    let mut current = prior;
    current.measured_tools.compiler_dependencies_sha256 = "2".repeat(64);
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        predecessor.trim(),
    )
    .err()
    .expect("an intermediate commit cannot authenticate a schema-eight migration");
    assert!(
        format!("{error:#}").contains("schema 9 has no migration authority"),
        "{error:#}"
    );
}

#[test]
fn authenticated_runner_replacement_is_operable_without_semantic_migration() {
    let mut prior_wire = serde_json::to_value(manifest(vec![entry()])).unwrap();
    prior_wire["suite_implementation_history"]["initial_sha256"] =
        serde_json::json!("1".repeat(64));
    let prior: Manifest = serde_json::from_value(prior_wire).unwrap();
    let repository = git_repository(Some(&prior));
    let predecessor = git(repository.path(), &["rev-parse", "HEAD"]);
    commit_file(
        repository.path(),
        "runner-equivalence.txt",
        "reviewed identical protocol and exact observations\n",
        "review runner replacement",
    );
    let head_commit = git(repository.path(), &["rev-parse", "HEAD"]);

    let mut current_wire = serde_json::to_value(&prior).unwrap();
    current_wire["suite_implementation_history"]["replacements"] = serde_json::json!([{
        "implementation_sha256": super::super::suite_identity::sha256(),
        "rationale": "reviewed runner-only replacement with unchanged protocol"
    }]);
    let current: Manifest = serde_json::from_value(current_wire).unwrap();
    let authenticated = super::super::manifest::authenticate_git_history(
        &checked_current(&current),
        repository.path(),
        &git_executable(),
        predecessor.trim(),
    )
    .unwrap();
    let wire = serde_json::to_value(authenticated.evidence()).unwrap();
    assert_eq!(
        wire["kind"],
        "authenticated-predecessor-with-runner-replacement"
    );
    assert_eq!(wire["head_commit"], head_commit.trim());
    assert_eq!(wire["previous_implementation_sha256"], "1".repeat(64));
    assert_eq!(
        wire["current_implementation_sha256"],
        super::super::suite_identity::sha256()
    );

    let mut multiple_wire = serde_json::to_value(&prior).unwrap();
    multiple_wire["suite_implementation_history"]["replacements"] = serde_json::json!([
        {
            "implementation_sha256": "2".repeat(64),
            "rationale": "first replacement"
        },
        {
            "implementation_sha256": super::super::suite_identity::sha256(),
            "rationale": "second replacement"
        }
    ]);
    let multiple: Manifest = serde_json::from_value(multiple_wire).unwrap();
    let error = super::super::manifest::authenticate_git_history(
        &checked_current(&multiple),
        repository.path(),
        &git_executable(),
        predecessor.trim(),
    )
    .err()
    .expect("one comparison cannot launder multiple runner replacements");
    assert!(format!("{error:#}").contains("cannot append multiple"));
}

fn git_repository(manifest: Option<&Manifest>) -> tempfile::TempDir {
    let repository = tempfile::tempdir().unwrap();
    git(repository.path(), &["init", "--quiet"]);
    match manifest {
        Some(manifest) => {
            let path = repository
                .path()
                .join(super::super::manifest::MANIFEST_PATH);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, serde_json::to_vec_pretty(manifest).unwrap()).unwrap();
        }
        None => {
            fs::write(
                repository.path().join("README.md"),
                "baseline without manifest\n",
            )
            .unwrap();
        }
    }
    git(repository.path(), &["add", "--all"]);
    git(
        repository.path(),
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "baseline",
        ],
    );
    repository
}

fn checked_current(manifest: &Manifest) -> super::super::manifest::CheckedCurrentManifest {
    super::super::manifest::CheckedCurrentManifest::for_test(manifest.clone())
}

fn commit_file(root: &Path, name: &str, contents: &str, message: &str) {
    fs::write(root.join(name), contents).unwrap();
    git(root, &["add", "--all"]);
    git(
        root,
        &[
            "-c",
            "user.name=Rumoca Test",
            "-c",
            "user.email=rumoca@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            message,
        ],
    );
}

pub(super) fn git(root: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .arg("-C")
        .arg(root)
        .args(args)
        .output()
        .unwrap();
    assert!(
        output.status.success(),
        "git {args:?} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap()
}

fn git_executable()
-> super::super::typed_path::AuthenticatedExecutable<super::super::typed_path::GitExecutable> {
    let path = std::env::split_paths(&std::env::var_os("PATH").unwrap())
        .map(|directory| directory.join("git"))
        .find(|candidate| candidate.is_file())
        .unwrap()
        .canonicalize()
        .unwrap();
    let digest = format!("{:x}", Sha256::digest(fs::read(&path).unwrap()));
    let path = super::super::typed_path::RolePath::checked(path).unwrap();
    super::super::typed_path::AuthenticatedExecutable::checked(path, &digest).unwrap()
}

fn assert_generation_bindings_use_snapshot(
    root: &Path,
    workspace: &Path,
    row: &Entry,
    inputs: &super::super::snapshot::AuthenticatedInputs,
) {
    let plan = fake_bound_plan(root, row);
    let cache = super::super::typed_path::RolePath::checked(root.join("gate/cache")).unwrap();
    let rumoca_output =
        super::super::typed_path::RolePath::checked(root.join("gate/emitted/rumoca")).unwrap();
    let rumoca = super::super::cross::rumoca_generation_command(
        &plan,
        inputs.fixture(),
        &cache,
        &rumoca_output,
    )
    .unwrap();
    let rumoca_args = rumoca.get_args().map(PathBuf::from).collect::<Vec<_>>();
    assert!(rumoca_args.contains(&inputs.fixture().as_path().to_path_buf()));
    assert!(!rumoca_args.iter().any(|path| path.starts_with(workspace)));

    let casadi_output =
        super::super::typed_path::RolePath::checked(root.join("gate/emitted/casadi")).unwrap();
    let casadi =
        super::super::cross::casadi_generation_command(&plan, inputs.generator(), &casadi_output)
            .unwrap();
    let casadi_args = casadi.get_args().map(PathBuf::from).collect::<Vec<_>>();
    assert!(casadi_args.contains(&inputs.generator().as_path().to_path_buf()));
    assert!(!casadi_args.iter().any(|path| path.starts_with(workspace)));
}

fn assert_build_bindings_use_snapshot(
    root: &Path,
    workspace: &Path,
    row: &Entry,
    inputs: &super::super::snapshot::AuthenticatedInputs,
) {
    let emission_root = root.join("gate/emitted/casadi");
    fs::create_dir_all(&emission_root).unwrap();
    fs::write(emission_root.join("casadi_exp_mixed.c"), "test source\n").unwrap();
    fs::write(emission_root.join("casadi_exp_mixed.h"), "test header\n").unwrap();
    let emission = emit::CasadiEmission {
        frozen: super::super::artifact_guard::FrozenArtifactSet::capture_tree(&emission_root)
            .unwrap(),
        include: super::super::typed_path::RolePath::checked(emission_root.clone()).unwrap(),
        generated_c: super::super::typed_path::RolePath::checked(
            emission_root.join("casadi_exp_mixed.c"),
        )
        .unwrap(),
        generated_h: super::super::typed_path::RolePath::checked(
            emission_root.join("casadi_exp_mixed.h"),
        )
        .unwrap(),
        wrapper_c: inputs.wrapper().clone(),
        command: super::super::process::test_receipt("casadi-generation"),
    };
    let plan = fake_bound_plan(root, row);
    let oracle_root = root.join("gate/oracle");
    fs::create_dir_all(&oracle_root).unwrap();
    fs::write(oracle_root.join("expected_output.h"), "test oracle\n").unwrap();
    let oracle_guard =
        super::super::artifact_guard::FrozenArtifactSet::capture_tree(&oracle_root).unwrap();
    let oracle = super::super::typed_path::RolePath::checked(oracle_root).unwrap();
    let context = super::super::cross::BuildContext {
        plan: &plan,
        harness: inputs.harness(),
        oracle: &oracle,
        oracle_guard: &oracle_guard,
    };
    let paths = super::super::cross::casadi_measured_bound_paths(
        &emission,
        &root.join("gate/build-casadi"),
        &context,
    )
    .unwrap();
    assert!(paths.contains(&inputs.wrapper().as_path().to_path_buf()));
    assert!(!paths.iter().any(|path| path.starts_with(workspace)));
}

fn fake_bound_plan(root: &Path, row: &Entry) -> super::super::cross::BoundExecutionPlan {
    fake_bound_plan_with_pins(root, row, comparator_output_pins())
}

fn fake_bound_plan_with_pins(
    root: &Path,
    row: &Entry,
    pins: ComparatorOutputPins,
) -> super::super::cross::BoundExecutionPlan {
    let execution_root = fake_execution_root_with_pins(root, row, pins);
    super::super::cross::bind_execution_plan(&execution_root, row).unwrap()
}

pub(super) fn fake_execution_root(
    root: &Path,
    row: &Entry,
) -> std::sync::Arc<super::super::cross::AuthenticatedExecutionRoot> {
    fake_execution_root_with_pins(root, row, comparator_output_pins())
}

fn fake_execution_root_with_pins(
    root: &Path,
    row: &Entry,
    comparator_outputs: ComparatorOutputPins,
) -> std::sync::Arc<super::super::cross::AuthenticatedExecutionRoot> {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static NEXT_PLAN: AtomicUsize = AtomicUsize::new(0);
    let sequence = NEXT_PLAN.fetch_add(1, Ordering::Relaxed);
    let gcc = super::super::typed_path::RolePath::checked(
        root.join("gate/toolchain/bin/arm-none-eabi-gcc"),
    )
    .unwrap();
    let nm = super::super::typed_path::RolePath::checked(
        root.join("gate/toolchain/bin/arm-none-eabi-nm"),
    )
    .unwrap();
    let qemu =
        super::super::typed_path::RolePath::checked(root.join("gate/bin/qemu-system-arm")).unwrap();
    let python = super::super::typed_path::RolePath::checked(root.join("bin/python"))
        .or_else(|_| super::super::typed_path::RolePath::checked(root.join("gate/bin/python")))
        .unwrap();
    let prlimit =
        super::super::typed_path::RolePath::checked(root.join("gate/bin/prlimit")).unwrap();
    for path in [
        gcc.as_path(),
        nm.as_path(),
        qemu.as_path(),
        python.as_path(),
        prlimit.as_path(),
    ] {
        if !path.exists() {
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, "#!/bin/sh\nexit 0\n").unwrap();
        }
    }
    let source = root.join(format!("fake-compiler-{sequence}/rumoca"));
    fs::create_dir_all(source.parent().unwrap()).unwrap();
    fs::write(&source, "#!/bin/sh\nexit 0\n").unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut permissions = fs::metadata(&source).unwrap().permissions();
        permissions.set_mode(0o755);
        fs::set_permissions(&source, permissions).unwrap();
    }
    let closure = super::super::tool_closure::AuthenticatedToolClosure::for_test(root);
    let compiler = super::super::cross::stage_compiler(
        &source,
        &root.join(format!("gate-plan-{sequence}")),
        &closure,
    )
    .unwrap();
    let gcc_sha256 = super::super::sha256_file(gcc.as_path()).unwrap();
    let nm_sha256 = super::super::sha256_file(nm.as_path()).unwrap();
    let qemu_sha256 = super::super::sha256_file(qemu.as_path()).unwrap();
    let python_sha256 = super::super::sha256_file(python.as_path()).unwrap();
    let prlimit_sha256 = super::super::sha256_file(prlimit.as_path()).unwrap();
    let gcc = super::super::typed_path::AuthenticatedExecutable::checked(gcc, &gcc_sha256).unwrap();
    let nm = super::super::typed_path::AuthenticatedExecutable::checked(nm, &nm_sha256).unwrap();
    let qemu =
        super::super::typed_path::AuthenticatedExecutable::checked(qemu, &qemu_sha256).unwrap();
    let python =
        super::super::typed_path::AuthenticatedExecutable::checked(python, &python_sha256).unwrap();
    let prlimit =
        super::super::typed_path::AuthenticatedExecutable::checked(prlimit, &prlimit_sha256)
            .unwrap();
    super::super::cross::authenticate_execution_root(super::super::cross::ExecutionRootInputs {
        gcc,
        nm,
        qemu,
        python,
        prlimit,
        gcc_version: super::super::cross::expected_gcc_version(row.cross_profile).into(),
        nm_version: super::super::cross::expected_nm_version(row.cross_profile).into(),
        qemu_version: super::super::cross::expected_qemu_version(row.cross_profile).into(),
        python_version: super::super::cross::expected_python_version(row.cross_profile).into(),
        prlimit_version: super::super::cross::expected_prlimit_version(row.cross_profile).into(),
        compiler,
        comparator_outputs,
        tool_closure: closure,
    })
    .unwrap()
}

pub(super) fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn real_entry(root: &Path) -> Entry {
    super::super::manifest::load(&super::super::manifest::path(root))
        .unwrap()
        .entries
        .into_iter()
        .next()
        .unwrap()
}

fn staged_real_inputs(destination: &Path) -> (Entry, super::super::snapshot::AuthenticatedInputs) {
    let root = repository_root();
    let row = real_entry(&root);
    let inputs =
        super::super::snapshot::stage(&root, &row, &destination.join("authenticated-inputs"))
            .unwrap();
    (row, inputs)
}

fn copy_checked_workspace(source: &Path, row: &Entry, destination: &Path) {
    for relative in [
        &row.fixture,
        &row.comparator_generator,
        &row.comparator_wrapper,
    ] {
        let target = destination.join(relative);
        fs::create_dir_all(target.parent().unwrap()).unwrap();
        fs::copy(source.join(relative), target).unwrap();
    }
    let source_harness = source.join(&row.fixture).parent().unwrap().join("harness");
    let target_harness = destination
        .join(&row.fixture)
        .parent()
        .unwrap()
        .join("harness");
    fs::create_dir_all(&target_harness).unwrap();
    for member in fs::read_dir(source_harness).unwrap() {
        let member = member.unwrap();
        fs::copy(member.path(), target_harness.join(member.file_name())).unwrap();
    }
}

#[test]
fn slower_comparator_cannot_make_the_delta_ratchet_pass() {
    let row = entry();
    let evidence = super::super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: 1250,
        comparator_instructions: 1200,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    };
    let verdict = judge(&row, evidence, 0.0);
    assert!(rejected_findings(&verdict)[0].contains("comparator instructions changed"));
}

#[test]
fn zero_instruction_rumoca_call_has_no_comparison_outcome() {
    let row = entry();
    let evidence = super::super::Evidence {
        entry_sha256: digest(),
        suite_implementation_sha256: super::super::suite_identity::sha256(),
        normalized_profile_sha256: digest(),
        completed_suite_sha256: digest(),
        authenticated_inputs_sha256: digest(),
        rumoca_instructions: 0,
        comparator_instructions: 1084,
        output_lines: Vec::new(),
        artifact_bundle: artifact_bundle(),
        commands: Vec::new(),
    };
    let verdict = judge(&row, evidence, 0.0);
    assert!(
        rejected_findings(&verdict)
            .iter()
            .any(|finding| finding.contains("zero-instruction"))
    );
}
