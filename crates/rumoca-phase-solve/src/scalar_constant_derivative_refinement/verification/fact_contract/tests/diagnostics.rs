//! Frozen pre-refactor diagnostic text, exercised through the whole checker.
use super::*;
use crate::NonemptyDerivativePatternKind;

type MetadataCase = (&'static str, fn(&mut SolveMetadataFacts));

const METADATA_CASES: [MetadataCase; 46] = [
    ("pure call owners", |facts| facts.pure_call_owners = 7),
    ("implicit row targets", |facts| {
        facts.implicit_row_targets = 7
    }),
    ("algebraic projection blocks", |facts| {
        facts.algebraic_projection_blocks = 7
    }),
    ("manifold projection blocks", |facts| {
        facts.manifold_projection_blocks = 7
    }),
    ("initialization projection unknowns", |facts| {
        facts.initialization_projection_unknowns = 7
    }),
    ("initialization projection blocks", |facts| {
        facts.initialization_projection_blocks = 7
    }),
    ("initialization update targets", |facts| {
        facts.initialization_update_targets = 7
    }),
    ("continuous refresh rows", |facts| {
        facts.continuous_refresh_rows = 7
    }),
    ("continuous refresh static parameters", |facts| {
        facts.continuous_refresh_static_parameters = 7
    }),
    ("structural implicit", |facts| facts.structural_implicit = 7),
    ("structural algebraic projection blocks", |facts| {
        facts.structural_algebraic_projection_blocks = 7
    }),
    ("structural manifold", |facts| facts.structural_manifold = 7),
    ("structural manifold projection blocks", |facts| {
        facts.structural_manifold_projection_blocks = 7
    }),
    ("initialization structural residual", |facts| {
        facts.initialization_structural_residual = 7
    }),
    ("initialization structural projection blocks", |facts| {
        facts.initialization_structural_projection_blocks = 7
    }),
    ("discrete update targets", |facts| {
        facts.discrete_update_targets = 7
    }),
    ("discrete event iteration runs", |facts| {
        facts.discrete_event_iteration_runs = 7
    }),
    ("discrete runtime assignment targets", |facts| {
        facts.discrete_runtime_assignment_targets = 7
    }),
    ("discrete runtime assignment roles", |facts| {
        facts.discrete_runtime_assignment_roles = 7
    }),
    ("discrete post-commit targets", |facts| {
        facts.discrete_post_commit_targets = 7
    }),
    ("discrete post-commit runtime rows", |facts| {
        facts.discrete_post_commit_runtime_rows = 7
    }),
    ("discrete row roles", |facts| facts.discrete_row_roles = 7),
    ("discrete pre modes", |facts| facts.discrete_pre_modes = 7),
    ("discrete observation refresh", |facts| {
        facts.discrete_observation_refresh = 7
    }),
    ("discrete observation reads Y", |facts| {
        facts.discrete_observation_refresh_reads_y = 7
    }),
    ("discrete history effects", |facts| {
        facts.discrete_integrator_history_effects = 7
    }),
    ("discrete clock owners", |facts| {
        facts.discrete_clock_owners = 7
    }),
    ("discrete structured updates", |facts| {
        facts.discrete_structured_updates = 7
    }),
    ("discrete guarded assignments", |facts| {
        facts.discrete_guarded_assignments = 7
    }),
    ("discrete event transactions", |facts| {
        facts.discrete_event_transactions = 7
    }),
    ("discrete clock partition order", |facts| {
        facts.discrete_clock_partition_order = 7
    }),
    ("discrete clock intermediate targets", |facts| {
        facts.discrete_clock_intermediate_targets = 7
    }),
    ("discrete clock intermediate clocks", |facts| {
        facts.discrete_clock_intermediate_clocks = 7
    }),
    ("event root memory targets", |facts| {
        facts.event_root_memory_targets = 7
    }),
    ("event root zero domains", |facts| {
        facts.event_root_zero_domains = 7
    }),
    ("event root refresh roles", |facts| {
        facts.event_root_refresh_roles = 7
    }),
    ("event condition memories", |facts| {
        facts.event_condition_memories = 7
    }),
    ("event scheduled roots", |facts| {
        facts.event_scheduled_roots = 7
    }),
    ("event scheduled times", |facts| {
        facts.event_scheduled_times = 7
    }),
    ("event dynamic time names", |facts| {
        facts.event_dynamic_time_names = 7
    }),
    ("event actions", |facts| facts.event_actions = 7),
    ("event terminal flag", |facts| facts.event_has_terminal = 7),
    ("event delay targets", |facts| facts.event_delay_targets = 7),
    ("event delay flags", |facts| {
        facts.event_delay_discrete_flags = 7
    }),
    ("clock schedules", |facts| facts.clock_schedules = 7),
    ("clock activation parameters", |facts| {
        facts.clock_activation_parameters = 7
    }),
];

fn refusal(facts: &SolveFacts) -> String {
    check_scalar_constant_derivative_refinement(&profile(0, 0), facts)
        .err()
        .expect("changed facts must refuse")
        .to_string()
}

#[test]
fn metadata_diagnostic_fields_and_first_error_order() {
    for (index, (label, mutate)) in METADATA_CASES.iter().enumerate() {
        let expected = format!("Solve metadata `{label}` has count 7, expected 0");
        let mut isolated = conforming_facts(0, 0);
        mutate(&mut isolated.metadata);
        assert_eq!(refusal(&isolated), expected);
        let mut suffix = conforming_facts(0, 0);
        for (_, later) in &METADATA_CASES[index..] {
            later(&mut suffix.metadata);
        }
        assert_eq!(refusal(&suffix), expected);
    }
}

#[test]
fn owner_diagnostic_labels_and_first_error_order() {
    let labels = [
        "continuous implicit",
        "continuous residual",
        "continuous manifold",
        "derivative kernel",
        "initialization residual",
        "structured discrete",
        "implicit tensor JVP",
        "manifold JVP",
        "initialization JVP",
        "initialization updates",
        "runtime assignments",
        "post-commit assignments",
        "discrete rows",
        "clock intermediates",
        "root conditions",
        "dynamic time events",
        "action conditions",
        "delay sources",
        "delay times",
        "delay maxima",
        "exact refresh programs",
        "full derivative JVP",
        "implicit scalar JVP",
        "visible rows",
        "guarded assignments",
        "event message programs",
        "compact tensor setup",
        "event transaction programs",
    ];
    for (index, label) in labels.iter().enumerate() {
        let mut facts = conforming_facts(0, 0);
        let expected = facts.owners[index];
        facts.owners[index] = 7;
        let message = format!("Solve executable owner `{label}` has count 7, expected {expected}");
        assert_eq!(refusal(&facts), message);
        for count in &mut facts.owners[index..] {
            *count = 7;
        }
        facts.metadata.event_actions = 7;
        assert_eq!(refusal(&facts), message);
    }
}

#[test]
fn nonidentity_matrix_diagnostic_labels() {
    for (matrix, label) in [
        (MassMatrixFact::Diagonal { entry_count: 0 }, "Diagonal"),
        (MassMatrixFact::Sparse { entry_count: 7 }, "Sparse"),
    ] {
        let mut facts = conforming_facts(0, 0);
        facts.metadata.mass_matrix = matrix;
        let entry_count = match matrix {
            MassMatrixFact::Diagonal { entry_count } | MassMatrixFact::Sparse { entry_count } => {
                entry_count
            }
            MassMatrixFact::Identity => unreachable!("test only constructs nonidentity matrices"),
        };
        assert_eq!(
            refusal(&facts),
            format!(
                "Solve mass matrix must be Identity; found {label} with {entry_count} stored entries"
            )
        );
    }
}

#[test]
fn derivative_pattern_diagnostic_labels() {
    let cases = [
        (DerivativePatternFact::Absent, "absent", 0, 0),
        (
            DerivativePatternFact::Empty {
                rows: 2,
                columns: 3,
            },
            "empty",
            2,
            3,
        ),
        (
            DerivativePatternFact::Other {
                kind: NonemptyDerivativePatternKind::Full,
                rows: 2,
                columns: 3,
            },
            "full",
            2,
            3,
        ),
        (
            DerivativePatternFact::Other {
                kind: NonemptyDerivativePatternKind::Diagonal,
                rows: 2,
                columns: 3,
            },
            "diagonal",
            2,
            3,
        ),
        (
            DerivativePatternFact::Other {
                kind: NonemptyDerivativePatternKind::Banded,
                rows: 2,
                columns: 3,
            },
            "banded",
            2,
            3,
        ),
        (
            DerivativePatternFact::Other {
                kind: NonemptyDerivativePatternKind::Csr,
                rows: 2,
                columns: 3,
            },
            "csr",
            2,
            3,
        ),
        (
            DerivativePatternFact::Other {
                kind: NonemptyDerivativePatternKind::Affine,
                rows: 2,
                columns: 3,
            },
            "affine",
            2,
            3,
        ),
    ];
    for (pattern, label, rows, columns) in cases {
        let mut facts = conforming_facts(0, 0);
        facts.metadata.structural_derivative = pattern;
        assert_eq!(
            refusal(&facts),
            format!(
                "Solve derivative structural pattern must be empty 1x1; found {label} {rows}x{columns}"
            )
        );
    }
}
