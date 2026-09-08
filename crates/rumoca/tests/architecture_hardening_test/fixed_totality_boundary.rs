//! The effective MLS 3.6 §4.8.1 `fixed` attribute is total from DAE
//! construction onward.
//!
//! Modelica source spells `fixed` optionally, and the default depends on the
//! variable's role: `true` for parameters and constants, `false` for every
//! other variable. Products before the checked DAE (instantiate, flatten)
//! carry only the explicit source spelling; DAE variable definition decides
//! the role default exactly once, and everything downstream copies the total
//! `rumoca_core::Fixity` value. `semantic_default_boundary` proves at compile
//! time that `Fixity` has no `Default` impl; this gate proves nothing
//! downstream of the decision reopens the question, and that the owner file
//! itself stays single-source:
//!
//! * no production type in a DAE-or-later crate wraps `Fixity` in `Option`
//!   outside the one declared-spelling construction input,
//! * no production expression in those crates applies an `Option` adapter to
//!   a `fixed` value, which is how a second, silent default would come back,
//! * no function anywhere in these crates maps a `VariableRole` to a bare
//!   `Fixity` outside the decision join, regardless of visibility, so a
//!   separately callable defaulting operation cannot reappear,
//! * no expression outside the decision file calls a function named like the
//!   decision join, and
//! * the owner file carries **exactly** one spelling-input field, exactly one
//!   role-defaulting signature, and exactly the two sanctioned
//!   `Option<Fixity>` type occurrences. Exact counts, not lower bounds: a
//!   second decider or second spelling input inside the owner is as much a
//!   violation as one downstream.
//!
//! Layer strengths differ and are stated as such. The `Option<Fixity>` and
//! role-to-`Fixity` signature checks are keyed on types, so a rename of the
//! function or field does not evade them (a rename of `Fixity` itself trips
//! the anti-vacuity control below). The `fixed`-receiver adapter check and
//! the decision-name call check are vocabulary layers, defeasible by rename;
//! they exist to catch the common regression cheaply, not to carry the
//! guarantee. The rename-resistant guarantees are the type-level checks here
//! plus the compile-time no-`Default` probe in `semantic_default_boundary`.
//!
//! Scope: production-capable targets of the listed crates, per the shared
//! module-graph helper. Flat-tier crates are deliberately absent: their
//! `Option<bool>` is the source spelling itself, not a decision deferred.

use std::collections::BTreeSet;
use std::fs;

use syn::visit::Visit;

use crate::architecture_hardening_support::{production_rust_sources, workspace_root};

/// Crates at or after the checked DAE, where `fixed` must be total.
const DAE_AND_LATER_CRATES: &[&str] = &[
    "rumoca-ir-dae",
    "rumoca-phase-dae",
    "rumoca-phase-structural",
    "rumoca-phase-solve",
    "rumoca-ir-solve",
    "rumoca-phase-codegen",
    "rumoca-phase-galec",
    "rumoca-ir-galec",
    "rumoca-eval-dae",
    "rumoca-eval-solve",
    "rumoca-opt",
    "rumoca-exec-cranelift",
    "rumoca-exec-mlir",
    "rumoca-sim",
    "rumoca-solver",
    "rumoca-compile",
    "rumoca-bind-python",
    "rumoca-bind-wasm",
];

/// The one file allowed to hold `Option<Fixity>` and the defaulting join.
const DECLARED_SPELLING_CHANNEL: &str = "crates/rumoca-ir-dae/src/model.rs";

/// Exactly one `Option<Fixity>` struct field: the declared-spelling
/// construction input on `VariableAttributes`.
const SANCTIONED_CHANNEL_OPTION_FIELDS: usize = 1;

/// Exactly one signature mapping a `VariableRole` to a bare `Fixity`: the
/// private decision join.
const SANCTIONED_CHANNEL_DECISION_SIGNATURES: usize = 1;

/// Exactly two `Option<Fixity>` type occurrences in the owner: the
/// construction-input field and the join's own `declared` parameter. Any
/// third occurrence is a new absence channel, wherever in the file it hides.
const SANCTIONED_CHANNEL_OPTION_OCCURRENCES: usize = 2;

/// `Option` adapters that would reinterpret an absent `fixed`. None of these
/// exist on `Fixity` itself, so any hit against a `fixed` receiver means an
/// `Option` has been reintroduced around the attribute.
const OPTION_ADAPTERS: &[&str] = &[
    "unwrap",
    "unwrap_or",
    "unwrap_or_else",
    "unwrap_or_default",
    "expect",
    "map_or",
    "map_or_else",
    "or_else",
    "or_default",
    "get_or_insert",
    "get_or_insert_with",
    "is_some",
    "is_none",
    "transpose",
    "flatten",
];

/// Names of the decision join. This is the vocabulary layer of the
/// defaulting-call check: it catches a downstream call to the join (or to a
/// resurrected `default_fixity`) by name, and a rename evades it. The
/// signature check below carries the rename-resistant half.
const DECISION_NAMES: &[&str] = &["effective_fixity", "default_fixity"];

#[derive(Default)]
struct FixityOptionVisitor {
    option_of_fixity: usize,
    option_of_fixity_fields: usize,
    fixed_option_fields: Vec<String>,
    fixed_absence_adapters: Vec<String>,
    role_defaulting_signatures: Vec<String>,
    decision_name_calls: Vec<String>,
}

impl FixityOptionVisitor {
    fn type_is_option_of_fixity(ty: &syn::Type) -> bool {
        let syn::Type::Path(path) = ty else {
            return false;
        };
        let Some(last) = path.path.segments.last() else {
            return false;
        };
        if last.ident != "Option" {
            return false;
        }
        let syn::PathArguments::AngleBracketed(arguments) = &last.arguments else {
            return false;
        };
        arguments.args.iter().any(|argument| {
            let syn::GenericArgument::Type(syn::Type::Path(inner)) = argument else {
                return false;
            };
            inner
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "Fixity")
        })
    }

    fn type_mentions_option(ty: &syn::Type) -> bool {
        tokens_contain_word(&quote_tokens(ty), "Option")
    }

    /// Whether a type is (after stripping references) a path ending in
    /// `ident` with no generic arguments, so `Fixity` matches while
    /// `Option<Fixity>` does not.
    fn type_is_bare(ty: &syn::Type, ident: &str) -> bool {
        let mut ty = ty;
        while let syn::Type::Reference(reference) = ty {
            ty = &reference.elem;
        }
        let syn::Type::Path(path) = ty else {
            return false;
        };
        path.path.segments.last().is_some_and(|segment| {
            segment.ident == ident && matches!(segment.arguments, syn::PathArguments::None)
        })
    }

    /// Type-keyed check for a callable defaulting operation: a signature
    /// producing a bare `Fixity` from a `VariableRole` input. `self_is_role`
    /// covers methods on `impl VariableRole`, whose receiver carries the role
    /// without spelling the type.
    fn signature_maps_role_to_fixity(signature: &syn::Signature, self_is_role: bool) -> bool {
        let syn::ReturnType::Type(_, output) = &signature.output else {
            return false;
        };
        if !Self::type_is_bare(output, "Fixity") {
            return false;
        }
        signature.inputs.iter().any(|input| match input {
            syn::FnArg::Typed(pattern) => Self::type_is_bare(&pattern.ty, "VariableRole"),
            syn::FnArg::Receiver(_) => self_is_role,
        })
    }

    fn record_role_defaulting_signature(&mut self, signature: &syn::Signature, self_is_role: bool) {
        if Self::signature_maps_role_to_fixity(signature, self_is_role) {
            self.role_defaulting_signatures.push(format!(
                "fn `{}` maps a VariableRole to a Fixity",
                signature.ident
            ));
        }
    }
}

impl<'ast> Visit<'ast> for FixityOptionVisitor {
    fn visit_type(&mut self, ty: &'ast syn::Type) {
        if Self::type_is_option_of_fixity(ty) {
            self.option_of_fixity += 1;
        }
        syn::visit::visit_type(self, ty);
    }

    fn visit_field(&mut self, field: &'ast syn::Field) {
        if Self::type_is_option_of_fixity(&field.ty) {
            self.option_of_fixity_fields += 1;
        }
        if field.ident.as_ref().is_some_and(|ident| ident == "fixed")
            && Self::type_mentions_option(&field.ty)
        {
            self.fixed_option_fields.push(format!(
                "field `fixed: {}`",
                quote_tokens(&field.ty).replace(' ', "")
            ));
        }
        syn::visit::visit_field(self, field);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        let method = call.method.to_string();
        if OPTION_ADAPTERS.contains(&method.as_str())
            && tokens_contain_word(&quote_tokens(&call.receiver), "fixed")
        {
            self.fixed_absence_adapters
                .push(format!("`{}` applied to a `fixed` receiver", method));
        }
        if DECISION_NAMES.contains(&method.as_str()) {
            self.decision_name_calls
                .push(format!("method call `{method}`"));
        }
        syn::visit::visit_expr_method_call(self, call);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref() {
            let named_decision = path.path.segments.last().is_some_and(|segment| {
                DECISION_NAMES.contains(&segment.ident.to_string().as_str())
            });
            if named_decision {
                let segment = path
                    .path
                    .segments
                    .last()
                    .expect("a matched call path has a final segment");
                self.decision_name_calls
                    .push(format!("call to `{}`", segment.ident));
            }
        }
        syn::visit::visit_expr_call(self, call);
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.record_role_defaulting_signature(&item.sig, false);
        syn::visit::visit_item_fn(self, item);
    }

    fn visit_trait_item_fn(&mut self, item: &'ast syn::TraitItemFn) {
        self.record_role_defaulting_signature(&item.sig, false);
        syn::visit::visit_trait_item_fn(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        // A method on `impl VariableRole` receives the role as `self`;
        // visibility is irrelevant on purpose, because trait-impl methods are
        // `Inherited` and a private helper is still a second decider.
        let self_is_role = Self::type_is_bare(&item.self_ty, "VariableRole");
        for member in &item.items {
            if let syn::ImplItem::Fn(method) = member {
                self.record_role_defaulting_signature(&method.sig, self_is_role);
            }
        }
        syn::visit::visit_item_impl(self, item);
    }
}

fn quote_tokens<T: quote::ToTokens>(value: &T) -> String {
    value.to_token_stream().to_string()
}

fn tokens_contain_word(tokens: &str, expected: &str) -> bool {
    tokens
        .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
        .any(|word| word == expected)
}

/// The whole-scan state, separated from file collection so the mutation
/// tests below can run the identical analysis over mutated sources.
#[derive(Default)]
struct GateScan {
    offenders: BTreeSet<String>,
    channel_option_occurrences: usize,
    channel_option_fields: usize,
    channel_decision_signatures: usize,
    files_mentioning_fixity: usize,
    scanned: usize,
}

impl GateScan {
    fn scan(&mut self, display: &str, source: &str) {
        self.scanned += 1;
        let Ok(syntax) = syn::parse_file(source) else {
            panic!("parse {display} for the fixed-totality gate");
        };
        if tokens_contain_word(source, "Fixity") {
            self.files_mentioning_fixity += 1;
        }
        let mut visitor = FixityOptionVisitor::default();
        visitor.visit_file(&syntax);
        if display == DECLARED_SPELLING_CHANNEL {
            // The owner is not exempted; it is counted exactly. The main
            // test pins these totals so a second decider, second spelling
            // field, or stray `Option<Fixity>` inside the owner fails just
            // like one downstream would. Only the offender listing treats
            // the owner's sanctioned occurrences as non-violations.
            self.channel_option_occurrences = visitor.option_of_fixity;
            self.channel_option_fields = visitor.option_of_fixity_fields;
            self.channel_decision_signatures = visitor.role_defaulting_signatures.len();
            visitor.option_of_fixity = 0;
            visitor.role_defaulting_signatures.clear();
            visitor.decision_name_calls.clear();
        }
        if visitor.option_of_fixity > 0 {
            self.offenders.insert(format!(
                "{display}: `Option<Fixity>` outside the declared-spelling channel"
            ));
        }
        if display != DECLARED_SPELLING_CHANNEL {
            for finding in visitor.fixed_option_fields {
                self.offenders.insert(format!("{display}: {finding}"));
            }
        }
        for finding in visitor.fixed_absence_adapters {
            self.offenders.insert(format!("{display}: {finding}"));
        }
        for finding in visitor.role_defaulting_signatures {
            self.offenders
                .insert(format!("{display}: {finding} outside the decision join"));
        }
        for finding in visitor.decision_name_calls {
            self.offenders
                .insert(format!("{display}: {finding} outside the decision file"));
        }
    }
}

#[test]
fn fixed_is_total_downstream_of_dae_construction() {
    let root = workspace_root();
    let mut scan = GateScan::default();

    for crate_name in DAE_AND_LATER_CRATES {
        let crate_root = root.join("crates").join(crate_name);
        for (path, source) in production_rust_sources(&crate_root, &root) {
            let display = path
                .to_string_lossy()
                .replace(std::path::MAIN_SEPARATOR, "/");
            scan.scan(&display, &source);
        }
    }

    // Anti-vacuity and single-source controls. Exact counts on the owner:
    // fewer than sanctioned means the detector rotted or the channel moved,
    // and this gate is no longer checking anything; more than sanctioned
    // means the owner itself acquired a second decider or a second spelling
    // input, which is the very thing this gate exists to prevent.
    assert!(
        scan.scanned > 100,
        "production scan collapsed to {} files",
        scan.scanned
    );
    assert_eq!(
        scan.channel_option_fields, SANCTIONED_CHANNEL_OPTION_FIELDS,
        "{DECLARED_SPELLING_CHANNEL} must carry exactly one `Option<Fixity>` field, the \
         declared-spelling construction input; zero means the field detector rotted or the \
         channel moved (move this gate's allowlist with it), more than one means a second \
         spelling input appeared inside the owner"
    );
    assert_eq!(
        scan.channel_decision_signatures, SANCTIONED_CHANNEL_DECISION_SIGNATURES,
        "{DECLARED_SPELLING_CHANNEL} must carry exactly one signature mapping a VariableRole \
         to a Fixity, the private decision join; zero means the signature detector rotted or \
         the join moved, more than one means a second defaulting join appeared inside the owner"
    );
    assert_eq!(
        scan.channel_option_occurrences, SANCTIONED_CHANNEL_OPTION_OCCURRENCES,
        "{DECLARED_SPELLING_CHANNEL} must carry exactly two `Option<Fixity>` type occurrences, \
         the construction-input field and the join's `declared` parameter; fewer means the \
         detector rotted or the channel moved, more means a new absence channel appeared \
         inside the owner"
    );
    assert!(
        scan.files_mentioning_fixity >= 4,
        "only {} scanned files mention `Fixity`; the total type was renamed or removed, so \
         this gate is no longer checking anything",
        scan.files_mentioning_fixity
    );
    assert!(
        scan.offenders.is_empty(),
        "`fixed` must stay total downstream of DAE construction; migrate these sites instead \
         of reinterpreting absence:\n{}",
        scan.offenders
            .iter()
            .cloned()
            .collect::<Vec<_>>()
            .join("\n")
    );
}

/// The real owner source, read from the working tree so every mutation below
/// is applied to the bytes the main gate actually scans.
fn channel_source() -> String {
    let path = workspace_root().join(DECLARED_SPELLING_CHANNEL);
    fs::read_to_string(&path)
        .unwrap_or_else(|error| panic!("read {DECLARED_SPELLING_CHANNEL}: {error}"))
}

/// Replace a needle that must occur exactly once, so a mutation that no
/// longer matches the owner's bytes fails loudly instead of silently testing
/// nothing.
fn replace_once(source: &str, needle: &str, replacement: &str) -> String {
    assert_eq!(
        source.matches(needle).count(),
        1,
        "the owner must contain `{needle}` exactly once for this mutation to mean anything"
    );
    source.replacen(needle, replacement, 1)
}

/// The join's signature line as it appears in the owner. The mutations below
/// key on it through [`replace_once`], so a drift in the owner is reported as
/// a stale mutation rather than passing vacuously.
const JOIN_SIGNATURE: &str =
    "const fn effective_fixity(role: VariableRole, declared: Option<Fixity>) -> Fixity {";

/// A duplicated defaulting join inside the owner must overshoot the exact
/// signature count, so the main gate's `== 1` control fails.
///
/// Mutated sources only need to parse, not compile, so the shadow join can
/// delegate to the real one.
#[test]
fn a_second_defaulting_join_inside_the_owner_trips_the_exact_count() {
    let mut mutated = channel_source();
    mutated.push_str(concat!(
        "\nconst fn effective_fixity_shadow(",
        "role: VariableRole, declared: Option<Fixity>) -> Fixity {\n",
        "    effective_fixity(role, declared)\n",
        "}\n",
    ));
    let mut scan = GateScan::default();
    scan.scan(DECLARED_SPELLING_CHANNEL, &mutated);
    assert_eq!(
        scan.channel_decision_signatures, 2,
        "a second join inside the owner must be counted, not cleared; the main gate's exact \
         count then rejects it"
    );
    assert_eq!(
        scan.channel_option_occurrences, 3,
        "the shadow join's `Option<Fixity>` parameter must also overshoot the occurrence count"
    );
}

/// A join whose return type no longer names `Fixity` is a join the signature
/// detector must stop counting, driving the exact count to zero: the
/// zero-direction of the control, distinguishable from the overshoot.
#[test]
fn a_missing_join_inside_the_owner_trips_the_exact_count_from_below() {
    let mutated = replace_once(
        &channel_source(),
        JOIN_SIGNATURE,
        "const fn effective_fixity(role: VariableRole, declared: Option<Fixity>) -> FixityGone {",
    );
    let mut scan = GateScan::default();
    scan.scan(DECLARED_SPELLING_CHANNEL, &mutated);
    assert_eq!(
        scan.channel_decision_signatures, 0,
        "a join that no longer produces `Fixity` must not satisfy the signature control"
    );
}

/// Renaming the join's identifier changes nothing for the structural layer:
/// the signature is still counted, so the exact-count control stays
/// satisfied. The vocabulary layer (`DECISION_NAMES`) is what goes stale on
/// a rename, and that is a documented weakness, not a guarantee.
#[test]
fn renaming_the_join_keeps_the_type_keyed_count_at_one() {
    let mutated = replace_once(
        &channel_source(),
        JOIN_SIGNATURE,
        "const fn decide_fixity(role: VariableRole, declared: Option<Fixity>) -> Fixity {",
    );
    let mut scan = GateScan::default();
    scan.scan(DECLARED_SPELLING_CHANNEL, &mutated);
    assert_eq!(
        scan.channel_decision_signatures, 1,
        "the structural control tracks the operation's types, not its name"
    );
}

/// A second `Option<Fixity>` field inside the owner, under any field name,
/// must overshoot both the field count and the occurrence count.
#[test]
fn a_second_spelling_input_field_inside_the_owner_trips_the_exact_count() {
    let mut mutated = channel_source();
    mutated.push_str("\nstruct ShadowSpellingInput {\n    declared: Option<Fixity>,\n}\n");
    let mut scan = GateScan::default();
    scan.scan(DECLARED_SPELLING_CHANNEL, &mutated);
    assert_eq!(
        scan.channel_option_fields, 2,
        "a second `Option<Fixity>` field must be counted whatever it is named"
    );
    assert_eq!(
        scan.channel_option_occurrences, 3,
        "the second field must also overshoot the occurrence count"
    );
}

/// The join moved out of the owner is a downstream defaulting operation: the
/// same bytes that are sanctioned inside the channel are an offender under
/// any other path, and the channel's own count goes to zero.
#[test]
fn the_join_outside_the_owner_is_an_offender_not_a_relocation() {
    let mut scan = GateScan::default();
    scan.scan(
        "crates/rumoca-phase-solve/src/relocated_join.rs",
        concat!(
            "use rumoca_core::Fixity;\n",
            "use rumoca_ir_dae::VariableRole;\n",
            "const fn effective_fixity(role: VariableRole, declared: Option<Fixity>)",
            " -> Fixity {\n",
            "    match declared {\n",
            "        Some(fixity) => fixity,\n",
            "        None => Fixity::Fixed,\n",
            "    }\n",
            "}\n",
        ),
    );
    assert!(
        scan.offenders
            .iter()
            .any(|offender| offender.contains("maps a VariableRole to a Fixity")),
        "a defaulting join outside the owner must be listed as an offender, got {:?}",
        scan.offenders
    );
    assert_eq!(
        scan.channel_decision_signatures, 0,
        "a relocated join must not satisfy the owner's exact count"
    );
}
