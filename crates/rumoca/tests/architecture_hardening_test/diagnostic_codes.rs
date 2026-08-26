//! SPEC_0008 diagnostic-code hardening gates.
//!
//! Six properties are enforced by text scan (no new crate dependency, so
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
//!    exactly one phase/target owner, and no registry declares one twice.
//! 4. **Severity matches the prefix.** SPEC_0008 states that a warning MUST NOT
//!    be minted in an `E` range. That is checked over every crate's shipped
//!    source rather than over a hand-kept list of files, because a warning
//!    minted in a phase nobody remembered to list is exactly the violation the
//!    property exists to catch.
//! 5. **Placeholders stay dead.** The pre-SPEC_0008 free-form simulation codes
//!    (`"lowering"` / `"simulation"` / `"override"`) must not come back.
//! 6. **The scans still see.** A text scan that quietly stops matching reports
//!    a clean workspace while a violation ships, which is worse than no scan at
//!    all. Canaries assert that each recognized spelling is still recognized,
//!    that the workspace scan still finds mnemonics the shipped tree really
//!    mints, and that a comment or a `#[cfg(test)]` body answers nothing.

use super::*;
use crate::totality_debt::gate::production_source_files_under;
use crate::totality_debt::scan::production_code;
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

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
        &["InstantiateError", "InstantiateWarning"],
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
    ("crates/rumoca-phase-solve/src/error.rs", &["LowerError"]),
    (
        "crates/rumoca-phase-galec/src/diagnostic.rs",
        &["GalecTargetError"],
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
        ("crates/rumoca-phase-solve/src/error.rs", "LowerError"),
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
        scanned >= 8,
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

fn miette_attribute_code_occurrences(content: &str) -> Vec<String> {
    content
        .split("code(rumoca::")
        .skip(1)
        .filter_map(|tail| tail.split(')').next())
        .filter_map(|attribute| attribute.rsplit("::").next().map(str::trim))
        .filter(|code| split_code(code).is_some())
        .map(str::to_string)
        .collect()
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
            "crates/rumoca-phase-parse-galec/src/parse/errors.rs",
        ],
    },
    DiagnosticRegistry {
        owner: "GALEC target projection",
        paths: &["crates/rumoca-phase-galec/src/diagnostic.rs"],
    },
];

/// Mnemonics that one registry deliberately declares more than once, with the
/// reason. `EI001` has two presentation variants for the same ModelNotFound
/// semantic identity: one source-free and one span-bearing.
const SHARED_PRESENTATION_MNEMONICS: &[&str] = &["EI001"];

/// Every source file a registry mints from.
fn registry_files(root: &Path, registry: &DiagnosticRegistry) -> Vec<PathBuf> {
    let mut files = Vec::new();
    for rel in registry.paths {
        let path = root.join(rel);
        if path.is_dir() {
            collect_rs_files(&path, &mut files);
        } else {
            files.push(path);
        }
    }
    files.sort();
    files
}

fn registry_codes(root: &Path, registry: &DiagnosticRegistry) -> BTreeSet<String> {
    let mut codes = BTreeSet::new();
    for file in registry_files(root, registry) {
        let content = fs::read_to_string(&file)
            .unwrap_or_else(|error| panic!("read diagnostic registry {}: {error}", file.display()));
        codes.extend(minted_codes(&production_code(&content)));
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
        "EX0xx", "EG0xx", "EGT0xx",
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
fn test_no_registry_declares_a_mnemonic_twice() {
    let root = workspace_root();
    let mut unexpected = Vec::new();
    for registry in DIAGNOSTIC_REGISTRIES {
        let mut counts = BTreeMap::<String, usize>::new();
        for file in registry_files(&root, registry) {
            let content = fs::read_to_string(&file)
                .unwrap_or_else(|error| panic!("read {}: {error}", file.display()));
            for code in miette_attribute_code_occurrences(&production_code(&content)) {
                *counts.entry(code).or_default() += 1;
            }
        }
        unexpected.extend(
            counts
                .into_iter()
                .filter(|(code, count)| {
                    *count > 1 && !SHARED_PRESENTATION_MNEMONICS.contains(&code.as_str())
                })
                .map(|(code, count)| format!("{}: {code} declared {count} times", registry.owner)),
        );
    }
    assert!(
        unexpected.is_empty(),
        "SPEC_0008 makes a mnemonic one diagnostic identity, so no registry may declare one \
twice: {unexpected:#?}"
    );
}

#[test]
fn test_simulation_diagnostics_do_not_use_placeholder_codes() {
    let path = workspace_root().join("crates/rumoca-sim/src/solve_lowering/diagnostics.rs");
    let content = fs::read_to_string(&path).expect("read sim diagnostics");
    let production = production_code(&content);

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

/// The call suffix every warning-minting constructor and helper ends in:
/// `CommonDiagnostic::warning(`, `Diagnostic::global_warning(`,
/// `structural_warning(`, `singular_warning(`, `ctx.emit_warning(`.
///
/// Matching on the suffix rather than on a list of exact spellings is the whole
/// point: the previous scan named two spellings, so the phase that mints
/// through a third was invisible to it and shipped two warnings in an `E`
/// range. A helper the workspace adds tomorrow is recognized the moment it is
/// named for what it does.
const WARNING_MINT_SUFFIX: &str = "warning(";

/// Every warning-severity mnemonic minted by this shipped source.
///
/// The mnemonic is read from the call's first argument, which is written either
/// as the code itself (`"WT006"`) or as the constant that holds it
/// (`WT006_INVALID_INT_COERCION`). A first argument that is neither -- a
/// forwarding helper's `code` parameter, say -- names no mnemonic here and is
/// counted at the call site that does name one.
fn warning_codes(production: &str) -> BTreeSet<String> {
    let mut codes = BTreeSet::new();
    for tail in production.split(WARNING_MINT_SUFFIX).skip(1) {
        let first_argument = tail.split([',', ')']).next().unwrap_or_default().trim();
        let named = match first_argument.strip_prefix('"') {
            Some(literal) => literal.split('"').next().unwrap_or_default(),
            None => first_argument.split('_').next().unwrap_or_default(),
        };
        if is_mnemonic(named) {
            codes.insert(named.to_string());
        }
    }
    codes
}

/// Every shipped source file of the workspace, as
/// `(workspace-relative path, production code)`.
///
/// The corpus is every crate's `src` tree, not a hand-kept list: a SPEC_0008
/// property holds of the workspace, and a list of files to check can only ever
/// be as complete as the last person to remember it. Each file is read through
/// the shared production scan, so a comment about a diagnostic and a
/// `#[cfg(test)]` fixture that mints one answer nothing about shipped code.
fn workspace_production_code(root: &Path) -> Vec<(String, String)> {
    let mut sources = Vec::new();
    let crates = fs::read_dir(root.join("crates")).expect("read crates directory");
    for entry in crates.flatten() {
        for file in production_source_files_under(&entry.path().join("src")) {
            let content = fs::read_to_string(&file)
                .unwrap_or_else(|error| panic!("read {}: {error}", file.display()));
            let display = file
                .strip_prefix(root)
                .unwrap_or(&file)
                .display()
                .to_string();
            sources.push((display, production_code(&content)));
        }
    }
    sources
}

/// Every warning mnemonic the workspace's shipped source mints, with the files
/// that mint each.
fn workspace_warning_codes(root: &Path) -> BTreeMap<String, BTreeSet<String>> {
    let mut minted: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    for (display, production) in workspace_production_code(root) {
        for code in warning_codes(&production) {
            minted.entry(code).or_default().insert(display.clone());
        }
    }
    minted
}

/// SPEC_0008 "Severity prefix": `E<phase>` is error severity, `W<phase>` is
/// warning severity, and any live violation must be written down under "Known
/// drift" so a consumer bucketing by prefix is not silently misled. The
/// structural phase emits `ES001`/`ES002` as warnings, so the spec must record
/// exactly those and nothing else may join them unrecorded.
#[test]
fn test_warning_severity_codes_are_spec_registered() {
    let root = workspace_root();
    let minted = workspace_warning_codes(&root);

    let spec =
        fs::read_to_string(root.join("spec/SPEC_0008_PHASE_ERRORS.md")).expect("read SPEC_0008");
    let drift: String = spec.split("**Known drift**").skip(1).collect();
    let unrecorded: Vec<String> = minted
        .iter()
        .filter(|(code, _)| code.starts_with('E'))
        .filter(|(code, _)| !drift.contains(code.as_str()))
        .map(|(code, files)| format!("{code} minted by {files:?}"))
        .collect();

    assert!(
        unrecorded.is_empty(),
        "warning-severity codes live in an `E` (error) range: {unrecorded:#?}; either rename them \
to the `W` convention or record them under SPEC_0008 `Known drift` so prefix-bucketing consumers \
are warned"
    );
}

/// A constant whose name starts with a mnemonic must hold that mnemonic.
///
/// The severity scan reads the mnemonic off the call's first argument, and for
/// a named constant that is the constant's name. `const WT006_X: &str = "ET006"`
/// would therefore report a `W` code while shipping an `E` one, so the two
/// spellings are held to agree instead of being trusted to.
#[test]
fn test_a_mnemonic_named_constant_holds_the_code_it_names() {
    let mismatched: Vec<String> = workspace_production_code(&workspace_root())
        .into_iter()
        .flat_map(|(display, production)| {
            mnemonic_named_string_constants(&production)
                .into_iter()
                .filter(|(name, value)| name != value)
                .map(move |(name, value)| format!("{display}: {name}_… holds \"{value}\""))
        })
        .collect();
    assert!(
        mismatched.is_empty(),
        "a constant named for a mnemonic must hold that mnemonic, or the severity scan reads one \
code and the compiler ships another: {mismatched:#?}"
    );
}

/// `(mnemonic in the name, mnemonic in the value)` for every
/// `const ES001_…: &str = "ES001";` in this shipped source.
fn mnemonic_named_string_constants(production: &str) -> Vec<(String, String)> {
    let mut pairs = Vec::new();
    for tail in production.split("const ").skip(1) {
        let Some((declaration, rest)) = tail.split_once('=') else {
            continue;
        };
        let Some(name) = declaration.split(':').next().map(str::trim) else {
            continue;
        };
        let Some(mnemonic) = name.split('_').next().filter(|head| is_mnemonic(head)) else {
            continue;
        };
        let Some(value) = rest.trim_start().strip_prefix('"') else {
            continue;
        };
        pairs.push((
            mnemonic.to_string(),
            value.split('"').next().unwrap_or_default().to_string(),
        ));
    }
    pairs
}

/// Mnemonics the shipped tree really mints as warnings, one per recognized
/// spelling, asserted so the workspace scan cannot go quiet.
///
/// A scan that matches nothing passes every gate it feeds while the property it
/// checks is unenforced, which is strictly worse than having no scan: the
/// dashboard is green and nobody is looking. Each row names the mnemonic and
/// the phase whose spelling proves that spelling is still seen. A row that
/// stops holding is a real change -- the code was renamed or retired -- and the
/// answer is to update the row, never to drop the assertion.
const WORKSPACE_WARNING_CANARIES: &[(&str, &str)] = &[
    (
        "WT003",
        "typecheck, minted as a `CommonDiagnostic::warning` literal",
    ),
    (
        "WT006",
        "AST constant evaluation, minted through `ctx.emit_warning`",
    ),
    (
        "WT007",
        "AST constant evaluation, minted through `ctx.emit_warning`",
    ),
    ("ES001", "structural, minted through `singular_warning`"),
];

#[test]
fn test_workspace_warning_scan_still_sees_the_codes_it_is_meant_to_see() {
    let root = workspace_root();
    let minted = workspace_warning_codes(&root);
    let missing: Vec<String> = WORKSPACE_WARNING_CANARIES
        .iter()
        .filter(|(code, _)| !minted.contains_key(*code))
        .map(|(code, provenance)| format!("{code} ({provenance})"))
        .collect();
    assert!(
        missing.is_empty(),
        "the workspace warning scan no longer sees {missing:#?}; a scan that quietly matches \
nothing reports a clean severity table while a violation ships, so fix the scan or update the \
canary to the code's new spelling"
    );
}

/// One source fixture per recognized warning-mint spelling, with the mnemonic
/// the scan must read out of it.
const WARNING_MINT_SPELLING_CANARIES: &[(&str, &str)] = &[
    (
        r#"CommonDiagnostic::warning("WT003", message, label)"#,
        "WT003",
    ),
    (
        r#"CommonDiagnostic::global_warning("WR001", message)"#,
        "WR001",
    ),
    (
        "diagnostics::singular_warning(\n    ES001_STRUCTURAL_SINGULARITY,\n    span,\n)",
        "ES001",
    ),
    (
        "ctx.emit_warning(\n    WT006_INVALID_INT_COERCION,\n    message,\n    span,\n)",
        "WT006",
    ),
];

#[test]
fn test_every_recognized_warning_mint_spelling_is_still_read() {
    let unread: Vec<&str> = WARNING_MINT_SPELLING_CANARIES
        .iter()
        .filter(|(source, code)| !warning_codes(source).contains(*code))
        .map(|(source, _)| *source)
        .collect();
    assert!(
        unread.is_empty(),
        "the warning-mint recognizer no longer reads {unread:#?}"
    );
}

#[test]
fn test_a_comment_or_a_test_body_mints_nothing() {
    let source = "\
// CommonDiagnostic::warning(\"EX999\", message, label)
/// Renders like `CommonDiagnostic::warning(\"EX998\", ..)`.
pub fn render() {}

#[cfg(test)]
mod tests {
    fn fixture() {
        CommonDiagnostic::warning(\"EX997\", message, label);
    }
}
";
    assert!(
        warning_codes(&production_code(source)).is_empty(),
        "prose about a mint and a `#[cfg(test)]` fixture do not ship, so neither may answer a \
question about shipped code"
    );
}
