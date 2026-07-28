//! SPEC_0008 diagnostic-code hardening gates.
//!
//! Four properties are enforced by text scan (no new crate dependency, so
//! `crates/rumoca/Cargo.toml` stays untouched):
//!
//! 1. **Span-freedom is justified.** Every `LowerError` / `StructuralError`
//!    variant with no span-bearing field must document *why* no honest source
//!    span exists, per SPEC_0008 "Source Traceability". Only the two delegating
//!    wrappers (`Spanned`, `WithContext`) are exempt.
//! 2. **The spec table does not go stale.** Every `E<PREFIX>0xx` range minted by
//!    a phase diagnostic registry must have a row in
//!    `spec/SPEC_0008_PHASE_ERRORS.md`.
//! 3. **Diagnostic identities are globally unique.** A mnemonic belongs to
//!    exactly one phase/target owner.
//! 4. **Placeholders stay dead.** The pre-SPEC_0008 free-form simulation codes
//!    (`"lowering"` / `"simulation"` / `"override"`) must not come back.

use super::*;
use std::collections::{BTreeMap, BTreeSet};

const SPAN_FREE_MARKER: &str = "Span-free:";

/// Variants that legitimately have neither a span-bearing field nor a
/// `Span-free:` note: they wrap another error and delegate span/code lookups.
const SPAN_DELEGATING_VARIANTS: &[&str] = &["Spanned", "WithContext"];

const MANDATED_PHASE_ERROR_IMPLS: &[(&str, &[&str])] = &[
    ("crates/rumoca-phase-parse/src/errors.rs", &["ParseError"]),
    (
        "crates/rumoca-phase-resolve/src/errors.rs",
        &["ResolveError"],
    ),
    (
        "crates/rumoca-phase-typecheck/src/lib.rs",
        &["TypeCheckError"],
    ),
    (
        "crates/rumoca-phase-instantiate/src/errors.rs",
        &["InstantiateError"],
    ),
    (
        "crates/rumoca-phase-flatten/src/errors.rs",
        &["FlattenError"],
    ),
    ("crates/rumoca-phase-dae/src/errors.rs", &["ToDaeError"]),
    (
        "crates/rumoca-phase-structural/src/types.rs",
        &["StructuralError"],
    ),
    (
        "crates/rumoca-phase-solve/src/lower/error.rs",
        &["LowerError"],
    ),
    (
        "crates/rumoca-phase-solve/src/solve_model.rs",
        &["SolveModelLowerError", "ParameterOverrideError"],
    ),
];

#[test]
fn test_semantic_phase_errors_implement_phase_error() {
    let root = workspace_root();
    let mut missing = Vec::new();

    for (relative_path, error_types) in MANDATED_PHASE_ERROR_IMPLS {
        let content =
            fs::read_to_string(root.join(relative_path)).expect("read mandated phase error source");
        for error_type in *error_types {
            let marker = format!("PhaseError for {error_type}");
            if !content.contains(&marker) {
                missing.push(format!("{relative_path}: {error_type}"));
            }
        }
    }

    assert!(
        missing.is_empty(),
        "SPEC_0008 `Phase Error Pattern` requires every semantic phase-local user-facing error \
enum to implement rumoca_core::PhaseError (codegen is the explicit exception); missing: \
{missing:#?}"
    );
}

#[test]
fn test_miette_phase_errors_retain_source_identified_spans() {
    let root = workspace_root();
    let paths = [
        "crates/rumoca-phase-resolve/src/errors.rs",
        "crates/rumoca-phase-instantiate/src/errors.rs",
        "crates/rumoca-phase-flatten/src/errors.rs",
        "crates/rumoca-phase-dae/src/errors.rs",
    ];
    let mut offenders = Vec::new();

    for relative_path in paths {
        let content =
            fs::read_to_string(root.join(relative_path)).expect("read miette phase error source");
        for (line_index, line) in content.lines().enumerate() {
            if line.contains("span: SourceSpan") {
                offenders.push(format!("{relative_path}:{}", line_index + 1));
            }
        }
    }

    assert!(
        offenders.is_empty(),
        "SPEC_0008 source identity cannot be reconstructed from miette::SourceSpan byte offsets; \
phase-local errors must retain rumoca_core::Span: {offenders:#?}"
    );
}

struct Variant {
    name: String,
    docs: String,
    fields: String,
}

/// Body of `pub enum <name> { … }`, brace-matched from the header.
fn enum_body<'a>(content: &'a str, enum_name: &str) -> &'a str {
    let header = format!("pub enum {enum_name} {{");
    let start = content
        .find(&header)
        .expect("scanned file must declare the enum")
        + header.len();
    let rest = &content[start..];
    let mut depth = 0usize;
    for (offset, ch) in rest.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' if depth == 0 => return &rest[..offset],
            '}' => depth -= 1,
            _ => {}
        }
    }
    rest
}

/// Field-name text of a variant, with inner doc comments stripped so a doc line
/// mentioning "span" cannot masquerade as a span-bearing field.
fn field_names(block: &str) -> String {
    block
        .lines()
        .map(str::trim)
        .filter(|line| !line.starts_with("//"))
        .collect::<Vec<_>>()
        .join("\n")
}

/// Split an enum body into variants with their preceding doc block. Multi-line
/// `#[...]` attributes are skipped by bracket depth.
fn parse_variants(body: &str) -> Vec<Variant> {
    let mut variants = Vec::new();
    let mut docs = String::new();
    let mut attr_depth = 0usize;
    let mut field_depth = 0usize;
    let mut pending: Option<(String, String)> = None;

    for line in body.lines() {
        let trimmed = line.trim();
        if attr_depth > 0 || trimmed.starts_with('#') {
            attr_depth += trimmed.matches('[').count();
            attr_depth = attr_depth.saturating_sub(trimmed.matches(']').count());
            continue;
        }
        if field_depth > 0 {
            if let Some((_, fields)) = pending.as_mut() {
                fields.push_str(trimmed);
                fields.push('\n');
            }
            field_depth += trimmed.matches('{').count();
            field_depth = field_depth.saturating_sub(trimmed.matches('}').count());
            if field_depth == 0
                && let Some((name, fields)) = pending.take()
            {
                variants.push(Variant {
                    name,
                    docs: std::mem::take(&mut docs),
                    fields,
                });
            }
            continue;
        }
        if trimmed.starts_with("///") {
            docs.push_str(trimmed);
            docs.push('\n');
            continue;
        }
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }
        let name_end = trimmed
            .find(|c: char| !(c.is_alphanumeric() || c == '_'))
            .unwrap_or(trimmed.len());
        let name = trimmed[..name_end].to_string();
        if name.is_empty() {
            continue;
        }
        let rest = &trimmed[name_end..];
        if rest.trim_start().starts_with('{') {
            // A one-line variant such as `InvalidIcPlanUnknown { name: String },`
            // opens and closes its field block on the same line.
            field_depth = rest
                .matches('{')
                .count()
                .saturating_sub(rest.matches('}').count());
            let fields = format!("{rest}\n");
            if field_depth == 0 {
                variants.push(Variant {
                    name,
                    docs: std::mem::take(&mut docs),
                    fields,
                });
            } else {
                pending = Some((name, fields));
            }
        } else {
            // Unit variant such as `DynamicSubscript,` or `EmptySystem,`.
            variants.push(Variant {
                name,
                docs: std::mem::take(&mut docs),
                fields: String::new(),
            });
        }
    }

    variants
}

fn span_free_offenders(content: &str, enum_name: &str) -> Vec<String> {
    parse_variants(enum_body(content, enum_name))
        .into_iter()
        .filter(|variant| {
            !field_names(&variant.fields).contains("span")
                && !SPAN_DELEGATING_VARIANTS.contains(&variant.name.as_str())
                && !variant.docs.contains(SPAN_FREE_MARKER)
        })
        .map(|variant| format!("{enum_name}::{}", variant.name))
        .collect()
}

#[test]
fn test_structural_and_solve_error_variants_document_span_freedom() {
    let root = workspace_root();
    let cases = [
        ("crates/rumoca-phase-solve/src/lower/error.rs", "LowerError"),
        (
            "crates/rumoca-phase-structural/src/types.rs",
            "StructuralError",
        ),
    ];

    let mut offenders = Vec::new();
    let mut scanned = 0usize;
    for (rel, enum_name) in cases {
        let content = fs::read_to_string(root.join(rel)).expect("read scanned error enum");
        scanned += parse_variants(enum_body(&content, enum_name)).len();
        offenders.extend(span_free_offenders(&content, enum_name));
    }

    assert!(
        scanned >= 20,
        "the enum scan found only {scanned} variants; the parser is out of sync with the source"
    );
    assert!(
        offenders.is_empty(),
        "SPEC_0008 Source Traceability: every span-less error variant must document why no honest \
source span exists with a `{SPAN_FREE_MARKER}` doc line (only span-delegating wrappers \
{SPAN_DELEGATING_VARIANTS:?} are exempt); undocumented: {offenders:#?}"
    );
}

/// Split `"EL001"` into its `EL` prefix and `001` number, if it is a mnemonic.
fn split_code(literal: &str) -> Option<(&str, &str)> {
    let rest = literal.strip_prefix('E')?;
    let letters = rest.chars().take_while(char::is_ascii_uppercase).count();
    if letters == 0 {
        return None;
    }
    let digits = &rest[letters..];
    if digits.len() != 3 || !digits.chars().all(|c| c.is_ascii_digit()) {
        return None;
    }
    Some((&literal[..1 + letters], digits))
}

/// Exact mnemonics minted by a diagnostic registry.
///
/// Phase registries use either bare string literals (`"ET001"`) or miette
/// attributes (`code(rumoca::typecheck::ET001)`). Restricting the scan to
/// those two production forms avoids treating explanatory cross-phase
/// comments as ownership.
fn minted_codes(content: &str) -> BTreeSet<String> {
    let mut codes: BTreeSet<String> = content
        .split('"')
        .skip(1)
        .step_by(2)
        .filter(|literal| split_code(literal).is_some())
        .map(str::to_string)
        .collect();

    for tail in content.split("code(rumoca::").skip(1) {
        let Some(attribute) = tail.split(')').next() else {
            continue;
        };
        let Some(code) = attribute.rsplit("::").next().map(str::trim) else {
            continue;
        };
        if split_code(code).is_some() {
            codes.insert(code.to_string());
        }
    }
    codes
}

fn minted_ranges(codes: &BTreeSet<String>) -> BTreeSet<String> {
    codes
        .iter()
        .filter_map(|code| split_code(code).map(|(prefix, _)| format!("{prefix}0xx")))
        .collect()
}

struct DiagnosticRegistry {
    owner: &'static str,
    paths: &'static [&'static str],
}

const DIAGNOSTIC_REGISTRIES: &[DiagnosticRegistry] = &[
    DiagnosticRegistry {
        owner: "parse",
        paths: &["crates/rumoca-phase-parse/src/errors.rs"],
    },
    DiagnosticRegistry {
        owner: "resolve",
        paths: &["crates/rumoca-phase-resolve/src"],
    },
    DiagnosticRegistry {
        owner: "typecheck",
        paths: &["crates/rumoca-phase-typecheck/src"],
    },
    DiagnosticRegistry {
        owner: "instantiate",
        paths: &["crates/rumoca-phase-instantiate/src/errors.rs"],
    },
    DiagnosticRegistry {
        owner: "flatten",
        paths: &["crates/rumoca-phase-flatten/src/errors.rs"],
    },
    DiagnosticRegistry {
        owner: "todae",
        paths: &["crates/rumoca-phase-dae/src/errors.rs"],
    },
    DiagnosticRegistry {
        owner: "codegen",
        paths: &["crates/rumoca-phase-codegen/src/errors.rs"],
    },
    DiagnosticRegistry {
        owner: "class merge",
        paths: &["crates/rumoca-compile/src/session/diagnostic_adapters.rs"],
    },
    DiagnosticRegistry {
        owner: "structural",
        paths: &["crates/rumoca-phase-structural/src/diagnostic_codes.rs"],
    },
    DiagnosticRegistry {
        owner: "solve lowering",
        paths: &["crates/rumoca-phase-solve/src/diagnostic_codes.rs"],
    },
    DiagnosticRegistry {
        owner: "simulation runtime",
        paths: &["crates/rumoca-sim/src/solve_lowering/diagnostics.rs"],
    },
    DiagnosticRegistry {
        owner: "GALEC IR",
        paths: &[
            "crates/rumoca-ir-galec/src/diagnostic.rs",
            "crates/rumoca-ir-galec/src/parse/errors.rs",
        ],
    },
    DiagnosticRegistry {
        owner: "GALEC target projection",
        paths: &["crates/rumoca-galec-codegen/src/diagnostic.rs"],
    },
    DiagnosticRegistry {
        owner: "eFMI packaging",
        paths: &["crates/rumoca-galec-codegen/src/manifest_context/diagnostic.rs"],
    },
];

fn registry_codes(root: &Path, registry: &DiagnosticRegistry) -> BTreeSet<String> {
    let mut codes = BTreeSet::new();
    for rel in registry.paths {
        let path = root.join(rel);
        let mut files = Vec::new();
        if path.is_dir() {
            collect_rs_files(&path, &mut files);
        } else {
            files.push(path);
        }
        for file in files {
            let content = fs::read_to_string(&file).unwrap_or_else(|error| {
                panic!("read diagnostic registry {}: {error}", file.display())
            });
            let production = content.split("#[cfg(test)]").next().unwrap_or(&content);
            codes.extend(minted_codes(production));
        }
    }
    assert!(
        !codes.is_empty(),
        "{} diagnostic registry did not mint any codes",
        registry.owner
    );
    codes
}

#[test]
fn test_phase_error_codes_are_registered_in_spec_0008() {
    let root = workspace_root();
    let mut ranges = BTreeSet::new();
    for registry in DIAGNOSTIC_REGISTRIES {
        ranges.extend(minted_ranges(&registry_codes(&root, registry)));
    }

    for expected in [
        "EP0xx", "ER0xx", "ET0xx", "EI0xx", "EF0xx", "ED0xx", "EC0xx", "EM0xx", "ES0xx", "EL0xx",
        "EX0xx", "EG0xx", "EGT0xx", "EFM0xx",
    ] {
        assert!(
            ranges.contains(expected),
            "expected a phase registry to mint {expected}; found {ranges:?}"
        );
    }

    let spec =
        fs::read_to_string(root.join("spec/SPEC_0008_PHASE_ERRORS.md")).expect("read SPEC_0008");
    let missing: Vec<&String> = ranges
        .iter()
        .filter(|range| !spec.contains(&format!("| {range} |")))
        .collect();

    assert!(
        missing.is_empty(),
        "SPEC_0008 `Error Code Ranges` is missing a row for live diagnostic ranges {missing:#?}; \
add `| <RANGE> | <phase> | <mnemonic> | <description> |` instead of shipping unregistered codes"
    );
}

#[test]
fn test_diagnostic_mnemonics_have_one_global_owner() {
    let root = workspace_root();
    let mut owners_by_code: BTreeMap<String, BTreeSet<&str>> = BTreeMap::new();
    for registry in DIAGNOSTIC_REGISTRIES {
        for code in registry_codes(&root, registry) {
            owners_by_code
                .entry(code)
                .or_default()
                .insert(registry.owner);
        }
    }

    let collisions: Vec<String> = owners_by_code
        .into_iter()
        .filter(|(_, owners)| owners.len() > 1)
        .map(|(code, owners)| format!("{code}: {owners:?}"))
        .collect();
    assert!(
        collisions.is_empty(),
        "SPEC_0008 diagnostic mnemonics must have one global owner; collisions: {collisions:#?}"
    );
}

#[test]
fn test_simulation_diagnostics_do_not_use_placeholder_codes() {
    let path = workspace_root().join("crates/rumoca-sim/src/solve_lowering/diagnostics.rs");
    let content = fs::read_to_string(&path).expect("read sim diagnostics");
    let production = content
        .split("#[cfg(test)]")
        .next()
        .expect("sim diagnostics has a production section");

    let offenders: Vec<&str> = ["\"lowering\"", "\"simulation\"", "\"override\""]
        .into_iter()
        .filter(|placeholder| production.contains(placeholder))
        .collect();

    assert!(
        offenders.is_empty(),
        "simulation diagnostics must emit SPEC_0008 codes, not free-form placeholders: {offenders:?}"
    );
}

/// Is `name` a bare mnemonic like `ES001` / `WS002`?
fn is_mnemonic(name: &str) -> bool {
    let letters = name.chars().take_while(char::is_ascii_uppercase).count();
    let digits = &name[letters..];
    letters > 0 && digits.len() == 3 && digits.chars().all(|c| c.is_ascii_digit())
}

/// Codes handed to `structural_warning(...)`, read off the constant name
/// (`ES001_STRUCTURAL_SINGULARITY` -> `ES001`).
fn structural_warning_codes(production: &str) -> BTreeSet<String> {
    let mut codes = BTreeSet::new();
    for tail in production.split("structural_warning(").skip(1) {
        let Some(first_arg) = tail.split(',').next() else {
            continue;
        };
        let Some(name) = first_arg.trim().split('_').next() else {
            continue;
        };
        if is_mnemonic(name) {
            codes.insert(name.to_string());
        }
    }
    codes
}

/// SPEC_0008 "Severity prefix": `E<phase>` is error severity, `W<phase>` is
/// warning severity, and any live violation must be written down under "Known
/// drift" so a consumer bucketing by prefix is not silently misled. The
/// structural phase currently emits `ES001`/`ES002` as warnings, so the spec
/// must record exactly those.
#[test]
fn test_warning_severity_codes_are_spec_registered() {
    let root = workspace_root();
    let content =
        fs::read_to_string(root.join("crates/rumoca-phase-structural/src/diagnostics.rs"))
            .expect("read structural diagnostics");
    let production = content
        .split("#[cfg(test)]")
        .next()
        .expect("structural diagnostics has a production section");

    let warning_codes = structural_warning_codes(production);
    assert!(
        !warning_codes.is_empty(),
        "structural diagnostics must still emit warning-severity codes"
    );

    let spec =
        fs::read_to_string(root.join("spec/SPEC_0008_PHASE_ERRORS.md")).expect("read SPEC_0008");
    let drift: String = spec.split("**Known drift**").skip(1).collect();
    let unrecorded: Vec<&String> = warning_codes
        .iter()
        .filter(|code| code.starts_with('E'))
        .filter(|code| !drift.contains(code.as_str()))
        .collect();

    assert!(
        unrecorded.is_empty(),
        "warning-severity codes {unrecorded:#?} live in an `E` (error) range; either rename them \
to the `W` convention or record them under SPEC_0008 `Known drift` so prefix-bucketing consumers \
are warned"
    );
}
