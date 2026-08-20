use super::*;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslCompilerContractMigration {
    pub(super) from_contract: String,
    pub(super) to_contract: String,
    pub(super) evidence_git_commit: String,
    pub(super) sim_target_models: usize,
    pub(super) stage_counts_before: MslCompilerContractStageCounts,
    pub(super) stage_counts_after: MslCompilerContractStageCounts,
    pub(super) phase_failure_counts_after: IndexMap<String, usize>,
    pub(super) error_code_counts_after: IndexMap<String, usize>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(super) struct MslCompilerContractStageCounts {
    pub(super) parse_models: usize,
    pub(super) flatten_models: usize,
    pub(super) dae_models: usize,
    pub(super) compiled_models: usize,
    pub(super) solve_models: usize,
    pub(super) balanced_models: usize,
    pub(super) unbalanced_models: usize,
    pub(super) partial_models: usize,
    pub(super) balance_denominator: usize,
    pub(super) initial_balanced_models: usize,
    pub(super) initial_unbalanced_models: usize,
    pub(super) sim_attempted: usize,
    pub(super) ic_attempted: usize,
    pub(super) ic_ok: usize,
    pub(super) ic_solver_fail: usize,
    pub(super) sim_ok: usize,
}

pub(super) fn checked_dae_compiler_contract_migration() -> MslCompilerContractMigration {
    MslCompilerContractMigration {
        from_contract: "permissive-dae-v1".to_string(),
        to_contract: "checked-dae-v1".to_string(),
        evidence_git_commit: "3fc9a6cb9c60e1137eb6151f29cb87e9ad35064b".to_string(),
        sim_target_models: 566,
        stage_counts_before: compiler_contract_stage_counts_before(),
        stage_counts_after: compiler_contract_stage_counts_after(),
        phase_failure_counts_after: [
            ("Flatten", 82),
            ("Instantiate", 9),
            ("Resolve", 25),
            ("ToDae", 216),
            ("Typecheck", 6),
        ]
        .into_iter()
        .map(|(phase, count)| (phase.to_string(), count))
        .collect(),
        error_code_counts_after: [
            ("ED001", 24),
            ("ED008", 7),
            ("ED009", 3),
            ("ED010", 14),
            ("ED013", 22),
            ("ED018", 29),
            ("ED019", 111),
            ("ED020", 1),
            ("ED021", 5),
            ("EF004", 24),
            ("EF005", 11),
            ("EF016", 16),
            ("EF020", 1),
            ("EF024", 16),
            ("EF025", 12),
            ("EI007", 2),
            ("EI012", 6),
            ("EI027", 1),
            ("EL005", 60),
            ("EMSL_TIMEOUT_MODEL_ATTEMPT", 11),
            ("ER066", 23),
            ("ER130", 2),
            ("ET000", 1),
            ("ET004", 4),
            ("EX001", 6),
            ("EX002", 13),
        ]
        .into_iter()
        .map(|(code, count)| (code.to_string(), count))
        .collect(),
    }
}

fn compiler_contract_stage_counts_before() -> MslCompilerContractStageCounts {
    MslCompilerContractStageCounts {
        parse_models: 566,
        flatten_models: 555,
        dae_models: 545,
        compiled_models: 545,
        solve_models: 446,
        balanced_models: 532,
        unbalanced_models: 0,
        partial_models: 13,
        balance_denominator: 532,
        initial_balanced_models: 532,
        initial_unbalanced_models: 0,
        sim_attempted: 496,
        ic_attempted: 267,
        ic_ok: 252,
        ic_solver_fail: 15,
        sim_ok: 207,
    }
}

fn compiler_contract_stage_counts_after() -> MslCompilerContractStageCounts {
    MslCompilerContractStageCounts {
        parse_models: 566,
        flatten_models: 444,
        dae_models: 228,
        compiled_models: 228,
        solve_models: 202,
        balanced_models: 217,
        unbalanced_models: 0,
        partial_models: 11,
        balance_denominator: 217,
        initial_balanced_models: 217,
        initial_unbalanced_models: 0,
        sim_attempted: 210,
        ic_attempted: 150,
        ic_ok: 146,
        ic_solver_fail: 4,
        sim_ok: 122,
    }
}
