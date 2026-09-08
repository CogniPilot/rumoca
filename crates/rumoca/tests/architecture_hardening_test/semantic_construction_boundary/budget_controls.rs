//! Exact fail-closed resource-budget controls excluded from debt.
//!
//! These owners have a reviewed typed terminal error. Pinning that architecture
//! prevents the debt ledger from accidentally classifying an explicit named
//! implementation limit as a fail-open semantic cutoff.

use crate::architecture_hardening_support::workspace_root;

use super::MIGRATION_NOTICE;
use super::owner_scan::{OwnerDigest, collect_workspace_owner_tokens};

struct BudgetControl {
    owner: OwnerDigest,
    required_terminal_tokens: &'static [&'static str],
}

const FAIL_CLOSED_RESOURCE_BUDGETS: &[BudgetControl] = &[
    BudgetControl {
        owner: OwnerDigest {
            identity: "rumoca-phase-instantiate::InstantiateContext::validate_depth_limit",
            normalized_len: 567,
            blake3: "0cf338b1891f22b36088b157d223872c1238fec2e56c52f352a0e89766f5f738",
        },
        required_terminal_tokens: &[
            "-> InstantiateResult < () >",
            "Err (Box :: new (InstantiateError :: instantiation_depth_limit",
        ],
    },
    BudgetControl {
        owner: OwnerDigest {
            identity: "rumoca-phase-instantiate::connections::extract_connections_from_for_equation",
            normalized_len: 2597,
            blake3: "68f5a4f75ed1f7efa5775d357ef02cf0465b44279137001b3eb563954dc102fb",
        },
        required_terminal_tokens: &[
            "-> InstantiateResult < () >",
            "ForRangeExpansion :: MaterializationLimit",
            "return connection_materialization_limit_error",
        ],
    },
    BudgetControl {
        owner: OwnerDigest {
            identity: "rumoca-phase-instantiate::connections::connection_materialization_limit_error",
            normalized_len: 612,
            blake3: "720d137d4489315b36d6ff643c494cf2583a3304ed4068c779b7126677f8e207",
        },
        required_terminal_tokens: &[
            "-> InstantiateResult < () >",
            "Err (Box :: new (InstantiateError :: structural_param_error",
            "MAX_MATERIALIZED_CONNECTION_ITERATIONS",
        ],
    },
    BudgetControl {
        owner: OwnerDigest {
            identity: "rumoca-phase-instantiate::connections::require_materialized_connection_budget",
            normalized_len: 971,
            blake3: "2e813d7de2c334a0d04bd024e6666b91ae41bfefd5f3e8ebbb317b3a9147ec1f",
        },
        required_terminal_tokens: &["-> InstantiateResult < () >", "return Err"],
    },
    BudgetControl {
        owner: OwnerDigest {
            identity: "rumoca-phase-flatten::pipeline::flatten_pipeline::collect_rewritten_functions_to_fixed_point",
            normalized_len: 1099,
            blake3: "69204c49fae5039f7f05aca2befc529e543a4c534402b8d20c8fec8794fe3931",
        },
        required_terminal_tokens: &[
            "-> Result < bool , FlattenError >",
            "Err (FlattenError :: function_rewrite_no_converge",
        ],
    },
];

#[test]
fn test_named_resource_budgets_keep_their_typed_terminal_errors() {
    let workspace = workspace_root();
    let crate_roots = [
        (
            "rumoca-phase-instantiate",
            workspace.join("crates/rumoca-phase-instantiate"),
        ),
        (
            "rumoca-phase-flatten",
            workspace.join("crates/rumoca-phase-flatten"),
        ),
    ];
    let measured = collect_workspace_owner_tokens(&crate_roots, &workspace);
    let mut differences = measured.differences;
    for control in FAIL_CLOSED_RESOURCE_BUDGETS {
        let Some(tokens) = measured.tokens.get(control.owner.identity) else {
            differences.push(format!(
                "  stale/deleted/renamed fail-closed budget owner `{}`",
                control.owner.identity
            ));
            continue;
        };
        let digest = blake3::hash(tokens.as_bytes()).to_hex().to_string();
        if tokens.len() != control.owner.normalized_len || digest != control.owner.blake3 {
            differences.push(format!(
                "  changed fail-closed budget owner `{}`: expected len {} blake3 {}, measured len {} blake3 {digest}",
                control.owner.identity,
                control.owner.normalized_len,
                control.owner.blake3,
                tokens.len()
            ));
        }
        for required in control.required_terminal_tokens {
            if !tokens.contains(required) {
                differences.push(format!(
                    "  `{}` lost reviewed typed-terminal tokens `{required}`",
                    control.owner.identity
                ));
            }
        }
    }
    assert!(
        differences.is_empty(),
        "fail-closed resource-budget controls changed:\n{}\n\n{MIGRATION_NOTICE}",
        differences.join("\n")
    );
}
