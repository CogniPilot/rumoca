//! Current worker-facing wire records are closed, explicit-null schemas.
//!
//! This deliberately discovers every `Option<_>` field in each current worker
//! DTO. A catalog of field names would let a newly added bare option bypass the
//! required-key check before its runtime fixture happened to exercise `None`.

use std::fs;

use crate::architecture_hardening_support::workspace_root;

const WORKER_WIRE_STRUCTS: &[(&str, &[&str])] = &[
    (
        "crates/rumoca-worker/src/lib.rs",
        &[
            "ModelWorkerRequest",
            "ModelWorkerResponse",
            "WorkerProgressEvent",
            "WorkerMemorySnapshot",
            "WorkerModelResult",
        ],
    ),
    (
        "crates/rumoca-sim/src/sim_trace_compare.rs",
        &["SimTrace", "SimTraceVariableMeta"],
    ),
];

fn serde_tokens(attributes: &[syn::Attribute]) -> Vec<String> {
    attributes
        .iter()
        .filter(|attribute| attribute.path().is_ident("serde"))
        .filter_map(|attribute| match &attribute.meta {
            syn::Meta::List(list) => Some(list.tokens.to_string()),
            syn::Meta::Path(_) | syn::Meta::NameValue(_) => None,
        })
        .collect()
}

fn tokens_contain_word(tokens: &str, expected: &str) -> bool {
    tokens
        .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
        .any(|word| word == expected)
}

fn type_is_option(ty: &syn::Type) -> bool {
    matches!(ty, syn::Type::Path(path) if path.path.segments.last().is_some_and(|segment| segment.ident == "Option"))
}

fn option_type_aliases(syntax: &syn::File) -> std::collections::BTreeSet<String> {
    syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Type(alias) if type_is_option(&alias.ty) => Some(alias.ident.to_string()),
            _ => None,
        })
        .collect()
}

fn type_is_option_or_alias(
    ty: &syn::Type,
    option_aliases: &std::collections::BTreeSet<String>,
) -> bool {
    type_is_option(ty)
        || matches!(ty, syn::Type::Path(path) if path.path.segments.last().is_some_and(|segment| option_aliases.contains(&segment.ident.to_string())))
}

fn worker_wire_option_offenders(source: &str, struct_name: &str) -> Vec<String> {
    let syntax = syn::parse_file(source).expect("worker wire source parses as Rust");
    let option_aliases = option_type_aliases(&syntax);
    let item = syntax
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Struct(item) if item.ident == struct_name => Some(item),
            _ => None,
        })
        .unwrap_or_else(|| panic!("missing current worker wire struct `{struct_name}`"));
    let container_serde = serde_tokens(&item.attrs);
    let container_denies_unknown = container_serde
        .iter()
        .any(|tokens| tokens_contain_word(tokens, "deny_unknown_fields"));
    let container_defaults = container_serde
        .iter()
        .any(|tokens| tokens_contain_word(tokens, "default"));

    let mut offenders = Vec::new();
    if !container_denies_unknown {
        offenders.push(format!("{struct_name}: missing deny_unknown_fields"));
    }
    for field in item
        .fields
        .iter()
        .filter(|field| type_is_option_or_alias(&field.ty, &option_aliases))
    {
        let field_name = field
            .ident
            .as_ref()
            .expect("worker wire structs use named fields");
        let serde = serde_tokens(&field.attrs);
        let required_decoder = serde.iter().any(|tokens| {
            tokens_contain_word(tokens, "deserialize_with")
                && tokens.contains("deserialize_required")
        });
        let uses_default = container_defaults
            || serde
                .iter()
                .any(|tokens| tokens_contain_word(tokens, "default"));
        let skips_null = serde
            .iter()
            .any(|tokens| tokens_contain_word(tokens, "skip_serializing_if"));
        if !required_decoder || uses_default || skips_null {
            offenders.push(format!("{struct_name}.{field_name}"));
        }
    }
    offenders
}

#[test]
fn current_worker_dtos_require_explicit_null_option_keys() {
    for (relative_path, struct_names) in WORKER_WIRE_STRUCTS {
        let path = workspace_root().join(relative_path);
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        for struct_name in *struct_names {
            let offenders = worker_wire_option_offenders(&source, struct_name);
            assert!(
                offenders.is_empty(),
                "current worker DTO `{struct_name}` in {} must reject omitted option keys, serialize explicit nulls, and reject unknown keys: {offenders:?}",
                path.display()
            );
        }
    }
}

#[test]
fn worker_option_gate_discovers_unlisted_bare_option_fields() {
    let source = r#"
        #[serde(deny_unknown_fields)]
        struct CurrentWorkerWire {
            #[serde(deserialize_with = "deserialize_required_option")]
            checked: Option<String>,
            newly_added: Option<String>,
        }
    "#;
    assert_eq!(
        worker_wire_option_offenders(source, "CurrentWorkerWire"),
        ["CurrentWorkerWire.newly_added"]
    );
}

#[test]
fn worker_option_gate_rejects_an_option_type_alias_evasion() {
    let source = r#"
        type Maybe<T> = Option<T>;

        #[serde(deny_unknown_fields)]
        struct CurrentWorkerWire {
            hidden: Maybe<String>,
        }
    "#;
    assert_eq!(
        worker_wire_option_offenders(source, "CurrentWorkerWire"),
        ["CurrentWorkerWire.hidden"]
    );
}

#[test]
fn worker_option_gate_rejects_default_and_null_skip_paths() {
    let source = r#"
        #[serde(default)]
        struct CurrentWorkerWire {
            #[serde(
                deserialize_with = "deserialize_required_option",
                skip_serializing_if = "Option::is_none"
            )]
            optional: Option<String>,
        }
    "#;
    assert_eq!(
        worker_wire_option_offenders(source, "CurrentWorkerWire"),
        [
            "CurrentWorkerWire: missing deny_unknown_fields",
            "CurrentWorkerWire.optional",
        ]
    );
}
