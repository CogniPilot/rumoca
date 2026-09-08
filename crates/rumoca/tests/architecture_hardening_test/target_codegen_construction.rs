//! Construction gates for target selection and checksum rendering.

use super::*;
use quote::ToTokens;

fn source(relative: &str) -> String {
    fs::read_to_string(workspace_root().join(relative))
        .unwrap_or_else(|error| panic!("read {relative}: {error}"))
}

/// Census of one call path with the identifier and signature of each
/// enclosing function, so route properties are asserted against a call
/// site's typed inputs rather than against function names alone. An `owner`
/// of "" matches only single-segment call paths.
struct CallSiteCensus<'a> {
    owner: &'a str,
    name: &'a str,
    enclosing: Vec<(String, String)>,
    fn_stack: Vec<(String, String)>,
}

impl<'ast> syn::visit::Visit<'ast> for CallSiteCensus<'_> {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.fn_stack.push((
            item.sig.ident.to_string(),
            item.sig.to_token_stream().to_string(),
        ));
        syn::visit::visit_item_fn(self, item);
        self.fn_stack.pop();
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.fn_stack.push((
            item.sig.ident.to_string(),
            item.sig.to_token_stream().to_string(),
        ));
        syn::visit::visit_impl_item_fn(self, item);
        self.fn_stack.pop();
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref() {
            let segments = &path.path.segments;
            let name_matches = segments
                .last()
                .is_some_and(|segment| segment.ident == self.name);
            let owner_matches = if self.owner.is_empty() {
                segments.len() == 1
            } else {
                segments.len() >= 2 && segments[segments.len() - 2].ident == self.owner
            };
            if name_matches && owner_matches {
                self.enclosing
                    .push(self.fn_stack.last().cloned().unwrap_or_default());
            }
        }
        syn::visit::visit_expr_call(self, call);
    }
}

fn call_sites(files: &[&syn::File], owner: &str, name: &str) -> Vec<(String, String)> {
    use syn::visit::Visit as _;
    let mut census = CallSiteCensus {
        owner,
        name,
        enclosing: Vec::new(),
        fn_stack: Vec::new(),
    };
    for file in files {
        census.visit_file(file);
    }
    census.enclosing
}

/// Census of struct-literal constructions of one type, resolving `Self`
/// literals through the enclosing impl block.
struct MintCensus<'a> {
    type_name: &'a str,
    mints: Vec<(String, String)>,
    fn_stack: Vec<(String, String)>,
    impl_stack: Vec<String>,
}

impl<'ast> syn::visit::Visit<'ast> for MintCensus<'_> {
    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let self_ty = match item.self_ty.as_ref() {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        };
        self.impl_stack.push(self_ty);
        syn::visit::visit_item_impl(self, item);
        self.impl_stack.pop();
    }

    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.fn_stack.push((
            item.sig.ident.to_string(),
            item.sig.to_token_stream().to_string(),
        ));
        syn::visit::visit_item_fn(self, item);
        self.fn_stack.pop();
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.fn_stack.push((
            item.sig.ident.to_string(),
            item.sig.to_token_stream().to_string(),
        ));
        syn::visit::visit_impl_item_fn(self, item);
        self.fn_stack.pop();
    }

    fn visit_expr_struct(&mut self, expr: &'ast syn::ExprStruct) {
        let last = expr
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
            .unwrap_or_default();
        let names_type = last == self.type_name
            || (last == "Self"
                && self
                    .impl_stack
                    .last()
                    .is_some_and(|self_ty| self_ty == self.type_name));
        if names_type {
            self.mints
                .push(self.fn_stack.last().cloned().unwrap_or_default());
        }
        syn::visit::visit_expr_struct(self, expr);
    }
}

fn admitted_mints(file: &syn::File, type_name: &str) -> Vec<(String, String)> {
    use syn::visit::Visit as _;
    let mut census = MintCensus {
        type_name,
        mints: Vec::new(),
        fn_stack: Vec::new(),
        impl_stack: Vec::new(),
    };
    census.visit_file(file);
    census.mints
}

fn assert_affine_admitted_type(rendering: &syn::File, name: &str) {
    let mut declared = false;
    for item in &rendering.items {
        let (vis, attrs, fields): (&syn::Visibility, &[syn::Attribute], Option<&syn::Fields>) =
            match item {
                syn::Item::Struct(item) if item.ident == name => {
                    (&item.vis, &item.attrs, Some(&item.fields))
                }
                syn::Item::Enum(item) if item.ident == name => (&item.vis, &item.attrs, None),
                _ => continue,
            };
        declared = true;
        assert!(
            matches!(vis, syn::Visibility::Inherited),
            "admitted type {name} must stay private to target rendering"
        );
        if let Some(fields) = fields {
            assert!(
                fields
                    .iter()
                    .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
                "admitted type {name} fields must stay private so no part can be re-paired"
            );
        }
        for forbidden in ["Clone", "Copy", "Default"] {
            assert!(
                !attrs.iter().any(|attribute| {
                    attribute.path().is_ident("derive")
                        && attribute
                            .meta
                            .to_token_stream()
                            .to_string()
                            .contains(forbidden)
                }),
                "affine admitted type {name} must not derive {forbidden}"
            );
        }
    }
    assert!(
        declared,
        "target rendering must declare admitted type {name}"
    );

    for item in &rendering.items {
        let syn::Item::Impl(block) = item else {
            continue;
        };
        let is_target = match block.self_ty.as_ref() {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == name),
            _ => false,
        };
        if !is_target {
            continue;
        }
        if let Some((_, trait_path, _)) = &block.trait_ {
            let trait_name = trait_path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default();
            assert!(
                !matches!(
                    trait_name.as_str(),
                    "Clone" | "Copy" | "Default" | "Deref" | "DerefMut" | "From" | "Into"
                ),
                "affine admitted type {name} must not implement escape trait {trait_name}"
            );
            continue;
        }
        for method in &block.items {
            let syn::ImplItem::Fn(method) = method else {
                continue;
            };
            if let Some(receiver) = method.sig.receiver() {
                assert!(
                    receiver.reference.is_none(),
                    "admitted type {name} method {} must consume self; a borrowing \
                     accessor would let parts escape for independent re-pairing",
                    method.sig.ident
                );
            }
        }
    }
}

#[test]
fn algorithm_code_lowering_requires_an_affine_admitted_authority() {
    let rendering_source = source(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs",
    );
    let rendering = syn::parse_file(&rendering_source).expect("parse target rendering");

    for name in [
        "AdmittedAlgorithmCodeLowering",
        "AdmittedPackagedOperation",
        "AdmittedUnpackagedOperation",
        "AdmittedTargetOperation",
        "PreparedTargetEmission",
    ] {
        assert_affine_admitted_type(&rendering, name);
    }

    let mints = admitted_mints(&rendering, "AdmittedAlgorithmCodeLowering");
    let [(mint_fn, mint_signature)] = mints.as_slice() else {
        panic!(
            "exactly one mint may construct the admitted lowering authority, found {}",
            mints.len()
        );
    };
    assert!(
        mint_signature.contains("CheckedTargetCapabilityContract")
            && mint_signature.contains("TargetAlgorithmCodeArithmetic"),
        "the sole mint must consume the capability contract and arithmetic by value: \
         {mint_signature}"
    );
    let validations = call_sites(&[&rendering], "", "validate_dae_render_capability_contract");
    assert!(
        validations.iter().any(|(function, _)| function == mint_fn),
        "the sole mint must validate the capability contract it consumes"
    );

    let lowerings = call_sites(
        &[&rendering],
        "rumoca_phase_galec",
        "lower_to_algorithm_code",
    );
    let [(lowering_fn, _)] = lowerings.as_slice() else {
        panic!(
            "GALEC lowering must have exactly one call site, found {}",
            lowerings.len()
        );
    };
    let consuming_route = rendering
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item) => Some(item),
            _ => None,
        })
        .filter(|block| {
            matches!(block.self_ty.as_ref(), syn::Type::Path(path)
                if path.path.segments.last().is_some_and(
                    |segment| segment.ident == "AdmittedAlgorithmCodeLowering"))
        })
        .flat_map(|block| block.items.iter())
        .find_map(|item| match item {
            syn::ImplItem::Fn(item) if item.sig.ident == *lowering_fn => Some(item),
            _ => None,
        })
        .expect("the sole GALEC lowering call site must be a method of the admitted authority");
    assert!(
        consuming_route.sig.inputs.iter().any(|input| matches!(
            input,
            syn::FnArg::Receiver(receiver) if receiver.reference.is_none()
        )),
        "the sole lowering route must consume the admitted authority by value"
    );

    for (path, text) in non_test_files("crates/rumoca-compile/src") {
        let is_rendering =
            path.ends_with("codegen_target/checked_plan/target_artifact/rendering.rs");
        assert!(
            is_rendering || !text.contains("lower_to_algorithm_code"),
            "{} must not open a second GALEC lowering route",
            path.display()
        );
        assert!(
            !text.contains("DaeCapabilityAdmission"),
            "{} resurrects the retired free-floating DAE admission token",
            path.display()
        );
    }

    let retired_seam = rendering
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item) => Some(item),
            _ => None,
        })
        .flat_map(|item| item.items.iter())
        .any(|item| {
            matches!(item, syn::ImplItem::Fn(item)
            if item.sig.ident == "lower_algorithm_code")
        });
    assert!(
        !retired_seam,
        "the separate-arithmetic lowering seam must stay deleted"
    );
}

#[test]
fn target_admission_binds_metadata_session_and_operation_atomically() {
    let artifact_root_source =
        source("crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact.rs");
    let artifact_root = syn::parse_file(&artifact_root_source).expect("parse target artifact root");
    let rendering_source = source(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs",
    );
    let rendering = syn::parse_file(&rendering_source).expect("parse target rendering");

    // Two-target substitution gate: correlated target facts may only be
    // opened by the private aggregate constructor. Local exhaustive
    // destructuring there is permitted; invariant-bearing tuple seams are not.
    assert_no_separable_authority_parameters(
        &artifact_root,
        &[
            ("ArtifactSession", "construct"),
            ("CompletedPresentation", "construct"),
        ],
    );
    assert_no_separable_authority_parameters(&rendering, &[]);
    assert_single_consumed_target_bundle(&rendering);

    // Metadata, prepared operation, and session join exactly once from one
    // checked target, strict compilation, invocation brand, and session input.
    let mints = admitted_mints(&rendering, "PreparedTargetEmission");
    let [(_, mint_signature)] = mints.as_slice() else {
        panic!(
            "exactly one mint may join target, session, and operation, found {}",
            mints.len()
        );
    };
    assert!(
        mint_signature.contains("brand : TargetInvocationBrand < 'inv >")
            && mint_signature.contains("compilation : & StrictCompilation")
            && mint_signature.contains("target : CheckedTargetBundle")
            && mint_signature.contains("input : ArtifactSessionInput"),
        "the sole emission mint must consume the complete correlated inputs: {mint_signature}"
    );

    // The session is minted in the aggregate constructor; completion
    // presentation is minted only after consuming that prepared aggregate.
    let sessions = call_sites(
        &[&artifact_root, &rendering],
        "ArtifactSession",
        "construct",
    );
    let [(session_fn, _)] = sessions.as_slice() else {
        panic!(
            "artifact-session construction must have exactly one site, found {}",
            sessions.len()
        );
    };
    let presentations = call_sites(
        &[&artifact_root, &rendering],
        "CompletedPresentation",
        "construct",
    );
    let [(presentation_fn, _)] = presentations.as_slice() else {
        panic!(
            "completion-presentation construction must have exactly one site, found {}",
            presentations.len()
        );
    };
    assert_eq!(session_fn, "construct");
    assert_eq!(presentation_fn, "emit");
    let session_site = rendering
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item) => Some(item),
            _ => None,
        })
        .filter(|block| {
            matches!(block.self_ty.as_ref(), syn::Type::Path(path)
                if path.path.segments.last().is_some_and(
                    |segment| segment.ident == "PreparedTargetEmission"))
        })
        .flat_map(|block| block.items.iter())
        .find_map(|item| match item {
            syn::ImplItem::Fn(item) if item.sig.ident == *session_fn => Some(item),
            _ => None,
        })
        .expect("the sole session site must be the prepared-emission constructor");
    assert!(
        session_site
            .sig
            .inputs
            .iter()
            .all(|input| !matches!(input, syn::FnArg::Receiver(_))),
        "the prepared-emission constructor must be an associated function"
    );

    // The superseded pre-admission routes stay deleted.
    let bundle_impls = rendering
        .items
        .iter()
        .filter(|item| {
            matches!(item, syn::Item::Impl(item)
                if matches!(item.self_ty.as_ref(), syn::Type::Path(path)
                    if path.path.segments.last().is_some_and(
                        |segment| segment.ident == "CheckedTargetBundle")))
        })
        .count();
    assert_eq!(
        bundle_impls, 0,
        "CheckedTargetBundle must expose no plan-opening impl in rendering"
    );
    assert!(
        !rendering_source.contains("TargetRenderOperation"),
        "the session-first render operation carrier must stay deleted"
    );
}

struct ParameterGate<'a> {
    allowed: &'a [(&'a str, &'a str)],
    impl_stack: Vec<String>,
    offenders: Vec<String>,
}

impl ParameterGate<'_> {
    fn record_parameter(&mut self, owner: &str, ident: &str, input: &syn::FnArg) {
        let syn::FnArg::Typed(input) = input else {
            return;
        };
        let tokens = input.ty.to_token_stream().to_string();
        for forbidden in [
            "CheckedTargetMetadata",
            "AdmittedTargetOperation",
            "AdmittedPackagedOperation",
            "AdmittedUnpackagedOperation",
            "CompletedPresentation",
        ] {
            if tokens.contains(forbidden) {
                self.offenders.push(format!("{owner}::{ident}: {tokens}"));
            }
        }
        // An owned or mutable presentation authority is what a session mint
        // consumes. Shared borrows dispatch rendering and cannot re-enter it.
        let is_shared_borrow = tokens.starts_with("& ") && !tokens.starts_with("& mut");
        if tokens.contains("StrictTargetPresentationAuthority") && !is_shared_borrow {
            self.offenders.push(format!("{owner}::{ident}: {tokens}"));
        }
    }
}

impl<'ast> syn::visit::Visit<'ast> for ParameterGate<'_> {
    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let self_ty = match item.self_ty.as_ref() {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        };
        self.impl_stack.push(self_ty);
        syn::visit::visit_item_impl(self, item);
        self.impl_stack.pop();
    }

    fn visit_signature(&mut self, signature: &'ast syn::Signature) {
        let owner = self.impl_stack.last().cloned().unwrap_or_default();
        let ident = signature.ident.to_string();
        let is_allowed = self.allowed.iter().any(|(allowed_owner, allowed_ident)| {
            *allowed_owner == owner && *allowed_ident == ident
        });
        if !is_allowed {
            for input in &signature.inputs {
                self.record_parameter(&owner, &ident, input);
            }
        }
        syn::visit::visit_signature(self, signature);
    }
}

fn assert_no_separable_authority_parameters(file: &syn::File, allowed: &[(&str, &str)]) {
    use syn::visit::Visit as _;
    let mut gate = ParameterGate {
        allowed,
        impl_stack: Vec::new(),
        offenders: Vec::new(),
    };
    gate.visit_file(file);
    assert!(
        gate.offenders.is_empty(),
        "session-scoped authorities must never travel as independent parameters; a foreign \
         metadata/presentation/operation pairing is representable at: {:?}",
        gate.offenders
    );
}

/// Census of one method-call name with the enclosing function identifier and
/// the receiver's plain identifier (empty when the receiver is not a bare
/// path), so plan-opening calls can be tied to the exact consumed parameter.
struct MethodCallCensus<'a> {
    name: &'a str,
    calls: Vec<(String, String)>,
    fn_stack: Vec<String>,
}

impl<'ast> syn::visit::Visit<'ast> for MethodCallCensus<'_> {
    fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
        self.fn_stack.push(item.sig.ident.to_string());
        syn::visit::visit_item_fn(self, item);
        self.fn_stack.pop();
    }

    fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
        self.fn_stack.push(item.sig.ident.to_string());
        syn::visit::visit_impl_item_fn(self, item);
        self.fn_stack.pop();
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        if call.method == self.name {
            let receiver = match call.receiver.as_ref() {
                syn::Expr::Path(path) => path
                    .path
                    .get_ident()
                    .map(|ident| ident.to_string())
                    .unwrap_or_default(),
                _ => String::new(),
            };
            self.calls
                .push((self.fn_stack.last().cloned().unwrap_or_default(), receiver));
        }
        syn::visit::visit_expr_method_call(self, call);
    }
}

fn method_calls(file: &syn::File, name: &str) -> Vec<(String, String)> {
    use syn::visit::Visit as _;
    let mut census = MethodCallCensus {
        name,
        calls: Vec::new(),
        fn_stack: Vec::new(),
    };
    census.visit_file(file);
    census.calls
}

const PLAN_AUTHORITY_TYPES: [&str; 10] = [
    "CheckedTargetRenderPlan",
    "CheckedTargetPackagePlan",
    "CheckedUnpackagedTargetPlan",
    "CheckedTargetPackageProductPlan",
    "CheckedUnpackagedTargetProductPlan",
    "CheckedOtherTargetPackagePlan",
    "CheckedOtherUnpackagedTargetPlan",
    "CheckedAlgorithmCodePackagePlan",
    "CheckedSolveAlgorithmPackagePlan",
    "CheckedUnpackagedAlgorithmCodePlan",
];

struct SignatureAuthorityCensus {
    impl_stack: Vec<String>,
    bundle_parameters: Vec<(String, String, String, String)>,
    plan_parameters: Vec<String>,
    plan_returns: Vec<String>,
}

impl<'ast> syn::visit::Visit<'ast> for SignatureAuthorityCensus {
    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        let self_ty = match item.self_ty.as_ref() {
            syn::Type::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string())
                .unwrap_or_default(),
            _ => String::new(),
        };
        self.impl_stack.push(self_ty);
        syn::visit::visit_item_impl(self, item);
        self.impl_stack.pop();
    }

    fn visit_signature(&mut self, signature: &'ast syn::Signature) {
        let owner = self.impl_stack.last().cloned().unwrap_or_default();
        let ident = signature.ident.to_string();
        for input in &signature.inputs {
            let syn::FnArg::Typed(input) = input else {
                continue;
            };
            let tokens = input.ty.to_token_stream().to_string();
            if tokens.contains("CheckedTargetBundle") {
                let name = match input.pat.as_ref() {
                    syn::Pat::Ident(pat) => pat.ident.to_string(),
                    _ => String::new(),
                };
                self.bundle_parameters
                    .push((owner.clone(), ident.clone(), name, tokens.clone()));
            }
            if PLAN_AUTHORITY_TYPES
                .iter()
                .any(|plan| tokens.contains(plan))
            {
                self.plan_parameters.push(format!("{owner}::{ident}"));
            }
        }
        let output = &signature.output;
        let output = quote::quote!(#output).to_string();
        if PLAN_AUTHORITY_TYPES
            .iter()
            .any(|plan| output.contains(plan))
        {
            self.plan_returns.push(format!("{owner}::{ident}"));
        }
        syn::visit::visit_signature(self, signature);
    }
}

struct PlanConstructionCensus {
    found: Vec<String>,
}

impl PlanConstructionCensus {
    fn record(&mut self, path: &syn::Path) {
        let spelling = path.to_token_stream().to_string();
        if PLAN_AUTHORITY_TYPES
            .iter()
            .any(|plan| spelling.contains(plan))
        {
            self.found.push(spelling);
        }
    }
}

impl<'ast> syn::visit::Visit<'ast> for PlanConstructionCensus {
    fn visit_expr_struct(&mut self, expr: &'ast syn::ExprStruct) {
        self.record(&expr.path);
        syn::visit::visit_expr_struct(self, expr);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(path) = call.func.as_ref() {
            self.record(&path.path);
        }
        syn::visit::visit_expr_call(self, call);
    }
}

/// The atomic-consumption half of the two-target substitution gate: exactly
/// one checked target is consumed, its render plan is opened exactly once
/// through that exact parameter inside the sole mint, and no render- or
/// product-plan authority can enter, leave, or be fabricated across any
/// other function boundary in target rendering.
fn assert_single_consumed_target_bundle(rendering: &syn::File) {
    use syn::visit::Visit as _;

    let mut census = SignatureAuthorityCensus {
        impl_stack: Vec::new(),
        bundle_parameters: Vec::new(),
        plan_parameters: Vec::new(),
        plan_returns: Vec::new(),
    };
    census.visit_file(rendering);

    let mut bundle_parameters = census.bundle_parameters;
    bundle_parameters.sort();
    assert_eq!(
        bundle_parameters
            .iter()
            .map(|(owner, function, name, tokens)| {
                (
                    owner.as_str(),
                    function.as_str(),
                    name.as_str(),
                    tokens.as_str(),
                )
            })
            .collect::<Vec<_>>(),
        [
            (
                "PreparedTargetEmission",
                "construct",
                "target",
                "CheckedTargetBundle"
            ),
            (
                "StrictCompilation",
                "render_target",
                "target",
                "CheckedTargetBundle"
            ),
        ],
        "exactly one checked target may be consumed: one by-value mint \
         parameter fed by one by-value forwarding entry"
    );
    assert!(
        census.plan_parameters.is_empty(),
        "render- and product-plan authorities must never cross a function \
         boundary as parameters: {:?}",
        census.plan_parameters
    );
    assert!(
        census.plan_returns.is_empty(),
        "target rendering must not produce render- or product-plan \
         authorities: {:?}",
        census.plan_returns
    );

    let retired_tuple_calls = method_calls(rendering, "into_render_plan");
    assert!(
        retired_tuple_calls.is_empty(),
        "the metadata/plan/authority tuple seam must stay deleted"
    );
    let render_plan_calls = method_calls(rendering, "into_plan");
    assert_eq!(
        render_plan_calls
            .iter()
            .map(|(function, receiver)| (function.as_str(), receiver.as_str()))
            .collect::<Vec<_>>(),
        [("construct", "render_authority")],
        "the sole render-plan opening must consume the authority destructured \
         locally from the mint's checked target"
    );
    let product_plan_calls = method_calls(rendering, "into_product_plan");
    assert_eq!(
        product_plan_calls
            .iter()
            .map(|(function, receiver)| (function.as_str(), receiver.as_str()))
            .collect::<Vec<_>>(),
        [("construct", "plan"), ("construct", "plan")],
        "both product-plan refinements must stay inside the sole mint, on \
         the plan opened from its consumed target"
    );

    let mut constructions = PlanConstructionCensus { found: Vec::new() };
    constructions.visit_file(rendering);
    assert!(
        constructions.found.is_empty(),
        "target rendering must not fabricate render- or product-plan values \
         in place: {:?}",
        constructions.found
    );

    assert_own_target_forwarding(rendering);
}

fn assert_own_target_forwarding(rendering: &syn::File) {
    let forwarding = rendering
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item) => Some(item),
            _ => None,
        })
        .filter(|block| {
            matches!(block.self_ty.as_ref(), syn::Type::Path(path)
                if path.path.segments.last().is_some_and(
                    |segment| segment.ident == "StrictCompilation"))
        })
        .flat_map(|block| block.items.iter())
        .find_map(|item| match item {
            syn::ImplItem::Fn(item) if item.sig.ident == "render_target" => Some(item),
            _ => None,
        })
        .expect("StrictCompilation must retain the sole forwarding entry");
    let body = forwarding.block.to_token_stream().to_string();
    let body: String = body.split_whitespace().collect();
    assert!(
        body.contains("PreparedTargetEmission::construct(brand,self,target,input)?.emit()"),
        "the forwarding entry must hand its own consumed target to the sole \
         mint verbatim: {body}"
    );
}

#[test]
fn substitution_gate_detects_an_injected_second_target() {
    let rendering_source = source(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs",
    );

    // The gate must accept the real source first, so a gate that rejects
    // everything cannot make the mutant below pass vacuously.
    let rendering = syn::parse_file(&rendering_source).expect("parse target rendering");
    assert_single_consumed_target_bundle(&rendering);

    // One realizable private-module A/B substitution: metadata is taken from
    // target A while the render authority is taken from target B. Rust privacy
    // prevents this outside the crate; this mutation pins the sole private
    // constructor that carries the same-invocation correlation obligation.
    let mut mutant = rendering_source.clone();
    for (anchor, replacement) in [
        (
            "        target: CheckedTargetBundle,\n        input: ArtifactSessionInput,\n    ) -> Result<Self> {",
            "        target: CheckedTargetBundle,\n        foreign: CheckedTargetBundle,\n        input: ArtifactSessionInput,\n    ) -> Result<Self> {",
        ),
        (
            "        let CheckedTargetBundle {\n            metadata,\n            render_authority,\n        } = target;",
            "        let CheckedTargetBundle { metadata, .. } = target;\n        let CheckedTargetBundle { render_authority, .. } = foreign;",
        ),
        (
            "            PreparedTargetEmission::construct(brand, self, target, input)?.emit()",
            "            let foreign = super::super::TargetBundle::load(\"galec\")?.check()?;\n            PreparedTargetEmission::construct(brand, self, target, foreign, input)?.emit()",
        ),
    ] {
        let next = mutant.replacen(anchor, replacement, 1);
        assert_ne!(
            next, mutant,
            "substitution anchor must match the mint bytes: {anchor}"
        );
        mutant = next;
    }
    let mutant = syn::parse_file(&mutant).expect("parse the two-target substitution mutant");
    assert!(
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            assert_single_consumed_target_bundle(&mutant);
        }))
        .is_err(),
        "the gate must reject the realizable two-target substitution"
    );
}

/// Fallible semantic preparation and capability validation may run only in
/// admission scope. Any function whose signature includes an artifact
/// session is a render path and must contain none of these calls; the
/// preparation entries live inside `PreparedTargetEmission::construct`.
const ADMISSION_SCOPED_CALLS: [&str; 14] = [
    "prepare_flat_rendering",
    "prepare_dae_rendering",
    "prepare_solve_model_rendering",
    "prepare_fmi_component_rendering",
    "admit_fmi_component_rendering",
    "prepare_algorithm_code_package",
    "prepare_solve_algorithm_production",
    "lower_to_algorithm_code",
    "lower_solve_algorithm_product",
    "lower_solve_model",
    "lower_fmi_component",
    "validate_dae_render_capability_contract",
    "validate_solve_capability_contract",
    "validate_fmi_capability_contract",
];

fn assert_session_scoped_functions_render_only(rendering: &syn::File) {
    use syn::visit::Visit as _;

    struct Gate {
        fn_stack: Vec<(String, bool)>,
        session_scoped: usize,
        offenders: Vec<String>,
    }

    impl Gate {
        fn record(&mut self, name: &str) {
            if ADMISSION_SCOPED_CALLS.contains(&name)
                && let Some((ident, true)) = self.fn_stack.last()
            {
                self.offenders.push(format!("{ident} calls {name}"));
            }
        }
    }

    impl<'ast> syn::visit::Visit<'ast> for Gate {
        fn visit_item_fn(&mut self, item: &'ast syn::ItemFn) {
            let scoped = item
                .sig
                .to_token_stream()
                .to_string()
                .contains("ArtifactSession <");
            self.session_scoped += usize::from(scoped);
            self.fn_stack.push((item.sig.ident.to_string(), scoped));
            syn::visit::visit_item_fn(self, item);
            self.fn_stack.pop();
        }

        fn visit_impl_item_fn(&mut self, item: &'ast syn::ImplItemFn) {
            let scoped = item
                .sig
                .to_token_stream()
                .to_string()
                .contains("ArtifactSession <");
            self.session_scoped += usize::from(scoped);
            self.fn_stack.push((item.sig.ident.to_string(), scoped));
            syn::visit::visit_impl_item_fn(self, item);
            self.fn_stack.pop();
        }

        fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
            if let syn::Expr::Path(path) = call.func.as_ref()
                && let Some(segment) = path.path.segments.last()
            {
                self.record(&segment.ident.to_string());
            }
            syn::visit::visit_expr_call(self, call);
        }

        fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
            self.record(&call.method.to_string());
            syn::visit::visit_expr_method_call(self, call);
        }
    }

    let mut gate = Gate {
        fn_stack: Vec::new(),
        session_scoped: 0,
        offenders: Vec::new(),
    };
    gate.visit_file(rendering);
    assert!(
        gate.session_scoped >= 3,
        "the render-path census lost its subjects; the gate would be vacuous"
    );
    assert!(
        gate.offenders.is_empty(),
        "fallible semantic preparation must precede the artifact session; it moved into a \
         session-scoped render path: {:?}",
        gate.offenders
    );
}

/// Enumerate phase-codegen's direct semantic render entries and
/// require each to consume a prepared carrier rather than a raw semantic
/// root. This is deliberately an exhaustive family enumeration over the
/// authority's own signatures, so a macro-generated render path in the
/// compile crate cannot reintroduce post-session preparation that a
/// rendering.rs census cannot see.
fn assert_direct_render_entries_consume_prepared_carriers(codegen: &syn::File) {
    let mut methods = std::collections::BTreeMap::new();
    for item in &codegen.items {
        let syn::Item::Fn(function) = item else {
            continue;
        };
        let name = function.sig.ident.to_string();
        if ![
            "render_ast_template_",
            "render_flat_template_",
            "render_dae_template_",
        ]
        .iter()
        .any(|prefix| name.starts_with(prefix))
        {
            continue;
        }
        let parameters = function
            .sig
            .inputs
            .iter()
            .filter_map(|input| match input {
                syn::FnArg::Typed(input) => Some(input.ty.to_token_stream().to_string()),
                syn::FnArg::Receiver(_) => None,
            })
            .collect::<Vec<_>>()
            .join(", ");
        assert!(
            methods.insert(name, parameters).is_none(),
            "a direct render entry must have one definition"
        );
    }
    let expected = [
        ("render_ast_template_content", "PreparedAstRendering"),
        ("render_dae_template_content", "PreparedDaeRendering"),
        ("render_flat_template_content", "PreparedFlatRendering"),
    ];
    assert_eq!(
        methods.keys().map(String::as_str).collect::<Vec<_>>(),
        expected.map(|(name, _)| name),
        "phase-codegen must expose exactly the prepared body-render family"
    );
    for (name, prepared) in expected {
        let parameters = &methods[name];
        assert!(
            parameters.contains(prepared)
                && !["ast :: ClassTree", "Model", "dae :: Dae"]
                    .iter()
                    .any(|raw| parameters.contains(raw)),
            "render entry {name} must consume its prepared carrier, not a raw \
             semantic root: {parameters}"
        );
    }
}

fn target_rendering_syntax() -> syn::File {
    syn::parse_file(&source(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs",
    ))
    .expect("parse target rendering")
}

fn named_mutation_subject<'syntax>(
    syntax: &'syntax mut syn::File,
    name: &str,
) -> &'syntax mut syn::Item {
    let mut subjects = syntax.items.iter_mut().filter(|item| {
        let ident = match item {
            syn::Item::Fn(item) => &item.sig.ident,
            syn::Item::Enum(item) => &item.ident,
            syn::Item::Struct(item) => &item.ident,
            _ => return false,
        };
        ident == name
    });
    let subject = subjects
        .next()
        .unwrap_or_else(|| panic!("mutation subject {name} must exist"));
    assert!(
        subjects.next().is_none(),
        "mutation subject {name} must have exactly one definition"
    );
    subject
}

fn assert_syntax_gate_rejects(syntax: &syn::File, gate: fn(&syn::File), expected: &str) {
    let failure = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| gate(syntax)))
        .expect_err("syntax gate must reject the mutation");
    let diagnostic = failure
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| failure.downcast_ref::<&str>().copied())
        .expect("syntax gate panic must carry a string diagnostic");
    assert!(
        diagnostic.contains(expected),
        "unexpected syntax-gate rejection: expected {expected:?}, got {diagnostic:?}"
    );
}

#[test]
#[should_panic(expected = "unexpected syntax-gate rejection")]
fn syntax_rejection_control_does_not_accept_an_unrelated_panic() {
    assert_syntax_gate_rejects(
        &syn::parse_quote! {},
        |_| panic!("unrelated gate failure"),
        "the intended rejection",
    );
}

#[test]
fn direct_product_preparation_is_admission_scoped() {
    let rendering = target_rendering_syntax();
    assert_session_scoped_functions_render_only(&rendering);
    assert_admitted_flat_dae_carriers_are_prepared_only(&rendering);

    let codegen = syn::parse_file(&source("crates/rumoca-phase-codegen/src/codegen/mod.rs"))
        .expect("parse phase-codegen template module");
    assert_direct_render_entries_consume_prepared_carriers(&codegen);
    let artifact =
        source("crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact.rs");
    assert!(
        !artifact.contains("StrictTargetPreparationAuthority")
            && !artifact.contains("StrictTargetPresentationAuthority"),
        "ArtifactSession must not retain a forgeable presentation token"
    );
    assert!(
        !workspace_root()
            .join("crates/rumoca-phase-codegen/src/strict_target_presentation.rs")
            .exists(),
        "the forgeable target-authority module must stay deleted"
    );
}

#[test]
fn direct_render_census_rejects_retired_file_renderers() {
    let codegen = syn::parse_file(&source("crates/rumoca-phase-codegen/src/codegen/mod.rs"))
        .expect("parse phase-codegen template module");
    assert_direct_render_entries_consume_prepared_carriers(&codegen);
    for family in ["ast", "flat", "dae"] {
        let mut mutant = codegen.clone();
        let syn::Item::Fn(body) =
            named_mutation_subject(&mut mutant, &format!("render_{family}_template_content"))
        else {
            panic!("body renderer must be a function");
        };
        let mut retired = body.clone();
        retired.sig.ident = syn::parse_str(&format!("render_{family}_template_file"))
            .expect("retired function identifier");
        mutant.items.push(syn::Item::Fn(retired));
        assert_eq!(mutant.items.len(), codegen.items.len() + 1);
        assert_syntax_gate_rejects(
            &mutant,
            assert_direct_render_entries_consume_prepared_carriers,
            "phase-codegen must expose exactly the prepared body-render family",
        );
    }
}

// These controls mutate the declarations inspected by the syntax gates.
// They do not establish that a complete production mutant compiles or runs.
#[test]
fn preparation_gate_detects_post_session_preparation() {
    let mut rendering = target_rendering_syntax();
    assert_session_scoped_functions_render_only(&rendering);
    let item_count = rendering.items.len();
    rendering.items.push(syn::parse_quote! {
        fn misplaced_preparation(
            session: &ArtifactSession<'_>,
            flat: &rumoca_ir_flat::Model,
        ) {
            let _prepared = rumoca_phase_codegen::prepare_flat_rendering(flat);
        }
    });
    assert_eq!(rendering.items.len(), item_count + 1);
    assert_syntax_gate_rejects(
        &rendering,
        assert_session_scoped_functions_render_only,
        "fallible semantic preparation must precede the artifact session",
    );
}

#[test]
fn preparation_gate_detects_duplicate_flat_preparation() {
    use syn::visit::Visit as _;

    let mut rendering = target_rendering_syntax();
    let mut baseline = PreparationCallCounter::default();
    baseline.visit_file(&rendering);
    assert_eq!((baseline.flat, baseline.dae), (1, 1));
    let syn::Item::Fn(helper) = named_mutation_subject(&mut rendering, "prepare_flat_direct")
    else {
        panic!("Flat preparation helper must be a function");
    };
    let statement_count = helper.block.stmts.len();
    helper.block.stmts.insert(
        0,
        syn::parse_quote! {
            let _duplicate =
                rumoca_phase_codegen::prepare_flat_rendering(&compilation.result().flat)?;
        },
    );
    assert_eq!(helper.block.stmts.len(), statement_count + 1);
    let mut mutant = PreparationCallCounter::default();
    mutant.visit_file(&rendering);
    assert_eq!((mutant.flat, mutant.dae), (2, 1));
}

#[test]
fn preparation_gate_detects_foreign_raw_root() {
    for (name, argument_count) in [("prepare_flat_direct", 1), ("prepare_dae_direct", 2)] {
        let mut rendering = target_rendering_syntax();
        assert_admitted_flat_dae_carriers_are_prepared_only(&rendering);
        let syn::Item::Fn(helper) = named_mutation_subject(&mut rendering, name) else {
            panic!("preparation helper must be a function");
        };
        assert_eq!(helper.sig.inputs.len(), argument_count);
        helper.sig.inputs.push(syn::parse_quote! {
            foreign: &StrictCompilation
        });
        assert_eq!(helper.sig.inputs.len(), argument_count + 1);
        assert_syntax_gate_rejects(
            &rendering,
            assert_admitted_flat_dae_carriers_are_prepared_only,
            &format!("{name} has 2 raw-root inputs"),
        );
    }
}

#[test]
fn preparation_gate_detects_raw_root_in_admitted_carriers() {
    for name in ["AdmittedPackagedOperation", "AdmittedUnpackagedOperation"] {
        let mut rendering = target_rendering_syntax();
        assert_admitted_flat_dae_carriers_are_prepared_only(&rendering);
        let syn::Item::Enum(operation) = named_mutation_subject(&mut rendering, name) else {
            panic!("admitted operation must be an enum");
        };
        let variant = operation
            .variants
            .iter_mut()
            .find(|variant| variant.ident == "Flat")
            .expect("admitted operation has a Flat variant");
        let syn::Fields::Named(fields) = &mut variant.fields else {
            panic!("Flat operation must have named fields");
        };
        let field_count = fields.named.len();
        fields.named.push(syn::parse_quote! {
            raw: *const StrictCompilation
        });
        assert_eq!(fields.named.len(), field_count + 1);
        assert_syntax_gate_rejects(
            &rendering,
            assert_admitted_flat_dae_carriers_are_prepared_only,
            &format!("admitted carrier {name} retained raw-root authority"),
        );
    }
}

#[test]
fn preparation_gate_detects_aliased_raw_root_in_prepared_product() {
    let mut rendering = target_rendering_syntax();
    assert_admitted_flat_dae_carriers_are_prepared_only(&rendering);
    let syn::Item::Struct(product) = named_mutation_subject(&mut rendering, "FlatDirectProduct")
    else {
        panic!("Flat product must be a struct");
    };
    let syn::Fields::Unnamed(fields) = &mut product.fields else {
        panic!("Flat product must wrap its prepared product");
    };
    assert_eq!(fields.unnamed.len(), 1);
    let mut foreign = fields.unnamed[0].clone();
    foreign.ty = syn::parse_quote! { *const RawCompilation };
    fields.unnamed.push(foreign);
    assert_eq!(fields.unnamed.len(), 2);
    rendering.items.push(syn::parse_quote! {
        type RawCompilation = StrictCompilation;
    });
    assert_syntax_gate_rejects(
        &rendering,
        assert_admitted_flat_dae_carriers_are_prepared_only,
        "admitted carrier FlatDirectProduct retained raw-root authority",
    );
}

#[derive(Default)]
struct PreparationCallCounter {
    flat: usize,
    dae: usize,
}

impl<'ast> syn::visit::Visit<'ast> for PreparationCallCounter {
    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        let name = match call.func.as_ref() {
            syn::Expr::Path(path) => path
                .path
                .segments
                .last()
                .map(|segment| segment.ident.to_string()),
            _ => None,
        };
        match name.as_deref() {
            Some("prepare_flat_rendering") => self.flat += 1,
            Some("prepare_dae_rendering") => self.dae += 1,
            _ => {}
        }
        syn::visit::visit_expr_call(self, call);
    }
}

struct PreparedCarrierSignatureGate<'a, F> {
    owner: Vec<String>,
    forbidden: &'a F,
    offenders: Vec<String>,
}
impl<'ast, F: Fn(&str) -> bool> syn::visit::Visit<'ast> for PreparedCarrierSignatureGate<'_, F> {
    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        self.owner.push(item.self_ty.to_token_stream().to_string());
        syn::visit::visit_item_impl(self, item);
        self.owner.pop();
    }

    fn visit_signature(&mut self, signature: &'ast syn::Signature) {
        let tokens = signature.to_token_stream().to_string();
        let direct_product = self.owner.last().is_some_and(|owner| {
            owner.contains("FlatDirectProduct") || owner.contains("DaeDirectProduct")
        });
        let session_scoped = tokens.contains("ArtifactSession <");
        let preparation_helper = matches!(
            signature.ident.to_string().as_str(),
            "prepare_flat_direct" | "prepare_dae_direct"
        );
        if (direct_product || session_scoped) && (self.forbidden)(&tokens) {
            self.offenders.push(signature.ident.to_string());
        }
        if preparation_helper {
            let compilation_inputs = signature
                .inputs
                .iter()
                .filter(|input| match input {
                    syn::FnArg::Typed(input) => {
                        (self.forbidden)(&input.ty.to_token_stream().to_string())
                    }
                    syn::FnArg::Receiver(_) => false,
                })
                .count();
            if compilation_inputs != 1 {
                self.offenders.push(format!(
                    "{} has {compilation_inputs} raw-root inputs",
                    signature.ident
                ));
            }
        }
        syn::visit::visit_signature(self, signature);
    }
}
fn assert_admitted_flat_dae_carriers_are_prepared_only(file: &syn::File) {
    use quote::ToTokens as _;
    use syn::visit::Visit as _;

    let aliases = file
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Type(alias) => Some((
                alias.ident.to_string(),
                alias.ty.to_token_stream().to_string(),
            )),
            _ => None,
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    let forbidden = |tokens: &str| {
        tokens.contains("StrictCompilation")
            || tokens.contains("flat :: Model")
            || tokens.contains("dae :: Dae")
            || aliases.iter().any(|(alias, target)| {
                tokens
                    .split(|character: char| !character.is_ascii_alphanumeric() && character != '_')
                    .any(|word| word == alias)
                    && (target.contains("StrictCompilation")
                        || target.contains("flat :: Model")
                        || target.contains("dae :: Dae"))
            })
    };

    for item in &file.items {
        match item {
            syn::Item::Enum(item)
                if matches!(
                    item.ident.to_string().as_str(),
                    "AdmittedPackagedOperation"
                        | "AdmittedUnpackagedOperation"
                        | "AdmittedTargetOperation"
                ) =>
            {
                assert!(
                    !forbidden(&item.to_token_stream().to_string()),
                    "admitted carrier {} retained raw-root authority",
                    item.ident
                )
            }
            syn::Item::Struct(item)
                if matches!(
                    item.ident.to_string().as_str(),
                    "FlatDirectProduct" | "DaeDirectProduct" | "PreparedTargetEmission"
                ) =>
            {
                assert!(
                    !forbidden(&item.to_token_stream().to_string()),
                    "admitted carrier {} retained raw-root authority",
                    item.ident
                )
            }
            _ => {}
        }
    }

    let mut gate = PreparedCarrierSignatureGate {
        owner: Vec::new(),
        forbidden: &forbidden,
        offenders: Vec::new(),
    };
    gate.visit_file(file);
    assert!(
        gate.offenders.is_empty(),
        "admitted Flat/DAE carriers or post-session helpers regained raw-root authority: {:?}",
        gate.offenders
    );
}

#[test]
fn solve_algorithm_refinement_cannot_accept_a_second_arithmetic_authority() {
    use syn::visit::Visit as _;

    struct TypeNameVisitor<'a> {
        forbidden: &'a str,
        found: bool,
    }

    impl<'ast> syn::visit::Visit<'ast> for TypeNameVisitor<'_> {
        fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
            self.found |= segment.ident == self.forbidden;
            syn::visit::visit_path_segment(self, segment);
        }
    }

    fn signature_contains_type(signature: &syn::Signature, forbidden: &str) -> bool {
        let mut visitor = TypeNameVisitor {
            forbidden,
            found: false,
        };
        for input in &signature.inputs {
            if let syn::FnArg::Typed(input) = input {
                visitor.visit_type(&input.ty);
            }
        }
        visitor.found
    }

    let phase_source = source("crates/rumoca-phase-solve/src/algorithm.rs");
    let phase =
        syn::parse_file(&phase_source).expect("parse phase-solve Algorithm Code refinement");
    let lower = phase
        .items
        .iter()
        .find_map(|item| match item {
            syn::Item::Fn(item) if item.sig.ident == "lower_solve_algorithm_product" => Some(item),
            _ => None,
        })
        .expect("phase-solve owns the Algorithm Code refinement entry point");
    assert_eq!(
        lower.sig.inputs.len(),
        1,
        "phase-solve must accept only the package-rooted product; its numeric profile is retained by that package"
    );
    assert!(
        !signature_contains_type(&lower.sig, "SolveArithmeticProfile"),
        "phase-solve reintroduced an independently supplied Solve arithmetic profile"
    );

    let root_source = source("crates/rumoca-ir-solve/src/algorithm_block/root.rs");
    let root = syn::parse_file(&root_source).expect("parse Solve Algorithm Block construction");
    let construct = root
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Impl(item) => Some(item),
            _ => None,
        })
        .flat_map(|item| item.items.iter())
        .find_map(|item| match item {
            syn::ImplItem::Fn(item) if item.sig.ident == "construct" => Some(item),
            _ => None,
        })
        .expect("SolveAlgorithmBlock owns one checked constructor");
    assert!(
        !signature_contains_type(&construct.sig, "SolveArithmeticProfile"),
        "SolveAlgorithmBlock construction must derive arithmetic from AlgorithmCodePackage"
    );

    let error_source = source("crates/rumoca-ir-solve/src/algorithm_block/root/error.rs");
    let errors = syn::parse_file(&error_source).expect("parse Algorithm Block construction errors");
    let has_profile_mismatch = errors.items.iter().any(|item| match item {
        syn::Item::Enum(item) if item.ident == "SolveAlgorithmBlockConstructionError" => item
            .variants
            .iter()
            .any(|variant| variant.ident == "ArithmeticProfileMismatch"),
        _ => false,
    });
    assert!(
        !has_profile_mismatch,
        "a late arithmetic mismatch state means a second profile authority is still representable"
    );
}

#[test]
fn standalone_templates_and_invocation_selected_template_ir_stay_absent() {
    let cli = source("crates/rumoca/src/cli.rs");
    let selectors = source("crates/rumoca/src/cli/compile_selectors.rs");
    let targets = source("crates/rumoca/src/target_manifest.rs");
    let compiler = source("crates/rumoca/src/compiler.rs");
    let lsp = source("crates/rumoca-tool-lsp/src/server/scenario_commands.rs");

    for (surface, text, forbidden) in [
        ("CLI arguments", cli.as_str(), "pub phase:"),
        ("CLI selectors", selectors.as_str(), "enum CompilePhase"),
        (
            "target orchestration",
            targets.as_str(),
            "raw_template_target",
        ),
        (
            "target orchestration",
            targets.as_str(),
            "render_raw_template",
        ),
        ("compiler API", compiler.as_str(), "pub fn render_template"),
        ("LSP target orchestration", lsp.as_str(), "raw_jinja_target"),
        (
            "LSP target orchestration",
            lsp.as_str(),
            "render_raw_jinja_target",
        ),
    ] {
        assert!(
            !text.contains(forbidden),
            "{surface} reintroduced untyped target authority `{forbidden}`; every rendered file must come from a checked target manifest"
        );
    }
}

#[test]
fn algorithm_code_emission_stays_a_passive_checked_syntax_spelling() {
    let root = workspace_root();
    let views = root.join("crates/rumoca-phase-codegen/src/views");
    for retired in [
        "algorithm_code_typed.rs",
        "algorithm_code_arena.rs",
        "algorithm_code_bound_equalization.rs",
        "algorithm_code_overlay.rs",
        "algorithm_code_scopes.rs",
        "algorithm_code_slot_overlay.rs",
    ] {
        assert!(
            !views.join(retired).exists(),
            "Algorithm Code emission must not regain target strategy `{retired}`; GALEC syntax is already checked before phase-codegen"
        );
    }
    assert!(
        !views.join("algorithm_code_typed").exists(),
        "Algorithm Code marshalling/destination rewriting must not return under phase-codegen"
    );
    assert!(
        !root
            .join("crates/rumoca-phase-codegen/src/codegen/symbol_alloc.rs")
            .exists(),
        "target-side symbol allocation must remain upstream of passive codegen"
    );

    let view = source("crates/rumoca-phase-codegen/src/views/algorithm_code.rs");
    for forbidden in [
        "TypedBlockView",
        "ShapeEvidence",
        "kernelize(",
        "ScratchLayoutView",
        "CallGraph::prove",
        "require_acyclic_calls",
        "marshalling::",
        "destination::plan",
        "rumoca_ir_galec::builtins",
        "find_builtin",
        "find_lifted_base",
        "reference_shape",
        "call_shape",
        "HashMap",
        "HashSet",
    ] {
        assert!(
            !view.contains(forbidden),
            "the `.alg` view reintroduced semantic inference or C strategy `{forbidden}`"
        );
    }
    for required in [
        "block: &'a ast::Block",
        "record_statement_spans",
        "package.block()",
    ] {
        assert!(
            view.contains(required),
            "the `.alg` view must borrow the checked package syntax and preserve provenance; missing `{required}`"
        );
    }

    let mut phase_sources = Vec::new();
    collect_rs_files(
        &root.join("crates/rumoca-phase-codegen/src"),
        &mut phase_sources,
    );
    for path in phase_sources {
        let text = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        for forbidden in [
            "algorithm_code_typed",
            "algorithm_code_arena",
            "algorithm_code_bound_equalization",
            "algorithm_code_overlay",
            "algorithm_code_scopes",
            "algorithm_code_slot_overlay",
            "algorithm_code_symbols",
            "rumoca_ir_galec::builtins",
            "CallGraph::prove",
            "require_acyclic_calls",
            "destination::plan",
            "marshalling::",
            "find_lifted_base",
            "allocate_symbols_function",
            "binary32_bound_filter",
            "int32_bound_filter",
        ] {
            assert!(
                !text.contains(forbidden),
                "phase-codegen reintroduced GALEC semantic resolution or C strategy `{forbidden}` in {}",
                path.display()
            );
        }
    }
}

#[test]
fn matrix_multiply_semantics_cannot_return_to_codegen_or_obsolete_owner_vocabulary() {
    let root = workspace_root();
    let retired_owner = concat!("Row", "Contraction");
    let mut rust_sources = Vec::new();
    for relative in [
        "crates/rumoca-ir-galec/src",
        "crates/rumoca-phase-galec/src",
        "crates/rumoca-phase-codegen/src",
    ] {
        let mut paths = Vec::new();
        collect_rs_files(&root.join(relative), &mut paths);
        for path in paths {
            let relative_path = path.strip_prefix(&root).expect("workspace source");
            if relative_path.to_string_lossy().contains("/tests/")
                || relative_path.to_string_lossy().ends_with("tests.rs")
            {
                continue;
            }
            rust_sources.push((
                relative_path.to_path_buf(),
                fs::read_to_string(&path)
                    .unwrap_or_else(|error| panic!("read {}: {error}", path.display())),
            ));
        }
    }
    for (path, text) in &rust_sources {
        for forbidden in [
            retired_owner,
            "kernelize(",
            "fill_scaled_add",
            "scaled_add_fill",
        ] {
            assert!(
                !text.contains(forbidden),
                "{} restored obsolete matrix-product semantic owner `{forbidden}`",
                path.display()
            );
        }
    }

    let templates = non_test_files("crates/rumoca-phase-codegen/src/templates");
    for (path, text) in templates {
        assert!(
            !text.contains("MatrixMultiply"),
            "{} recognizes MatrixMultiply in target syntax; a passive target may consume only a future prepared spelling/receipt",
            path.display()
        );
    }
}

fn non_test_files(relative: &str) -> Vec<(std::path::PathBuf, String)> {
    fn collect(path: &std::path::Path, files: &mut Vec<std::path::PathBuf>) {
        let mut entries = fs::read_dir(path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
            .map(|entry| entry.expect("template entry").path())
            .collect::<Vec<_>>();
        entries.sort();
        for entry in entries {
            if entry.is_dir() {
                collect(&entry, files);
            } else if entry.is_file() {
                files.push(entry);
            }
        }
    }
    let root = workspace_root();
    let mut files = Vec::new();
    collect(&root.join(relative), &mut files);
    files
        .into_iter()
        .map(|path| {
            let text = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            (path, text)
        })
        .collect()
}

#[test]
fn arbitrary_raw_template_renderers_stay_non_public() {
    let codegen = source("crates/rumoca-phase-codegen/src/codegen/mod.rs");
    for forbidden in [
        "pub fn render_template(",
        "pub fn render_template_with_name(",
        "pub fn render_template_for_input(",
        "pub fn render_template_with_name_for_input(",
        "pub fn render_flat_template_with_name(",
        "pub fn render_solve_template_with_name(",
        "pub fn render_ast_template_with_name(",
    ] {
        assert!(
            !codegen.contains(forbidden),
            "phase-codegen reintroduced arbitrary public template authority `{forbidden}`"
        );
    }
}

#[test]
fn forgeable_strict_target_authority_stays_deleted() {
    let root = workspace_root();
    let mut paths = Vec::new();
    collect_rs_files(&root.join("crates"), &mut paths);
    let mut offenders = Vec::new();
    for path in paths {
        let relative = path
            .strip_prefix(&root)
            .expect("workspace source")
            .to_string_lossy()
            .replace('\\', "/");
        if relative.contains("/tests/") || relative.ends_with("/tests.rs") {
            continue;
        }
        let text =
            fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let production_text = text
            .lines()
            .filter(|line| !line.trim_start().starts_with("///"))
            .collect::<String>();
        if [
            "StrictTargetPreparationAuthority",
            "StrictTargetPresentationAuthority",
            "issue_strict_target_preparation_authority",
        ]
        .iter()
        .any(|forbidden| production_text.contains(forbidden))
        {
            offenders.push(relative);
        }
    }
    assert!(
        offenders.is_empty(),
        "forgeable strict target tokens/issuer returned in production sources: {offenders:?}"
    );
    assert!(
        !root
            .join("crates/rumoca-phase-codegen/src/strict_target_presentation.rs")
            .exists(),
        "obsolete authority module must stay deleted"
    );
}

#[test]
fn generic_semantic_input_and_file_pairing_stays_unrepresentable() {
    let root = workspace_root();
    let mut sources = Vec::new();
    collect_rs_files(&root.join("crates/rumoca-phase-codegen/src"), &mut sources);
    let mut offenders = Vec::new();
    for path in sources {
        let relative = path
            .strip_prefix(&root)
            .expect("phase-codegen source beneath workspace")
            .to_string_lossy()
            .replace('\\', "/");
        if relative.contains("/tests/") || relative.ends_with("/tests.rs") {
            continue;
        }
        let source =
            fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let file =
            syn::parse_file(&source).unwrap_or_else(|error| panic!("parse {relative}: {error}"));
        collect_generic_semantic_file_joins(&file.items, &relative, &mut offenders);
        assert!(
            !source.contains("does not match supplied canonical"),
            "{relative} restored runtime repair of a foreign semantic input/file pair"
        );
    }
    assert!(
        offenders.is_empty(),
        "generic CodegenInput + SemanticTemplateFile joins are not correct by construction; use product-specific prepared file capabilities: {offenders:#?}"
    );
}

#[test]
fn legacy_rendered_target_dto_and_client_side_phase_rendering_stay_deleted() {
    let schema = source("crates/rumoca-compile/src/codegen_target.rs");
    let compile_facade = source("crates/rumoca-compile/src/lib.rs");
    let wasm = non_test_rust_sources("crates/rumoca-bind-wasm/src");
    let lsp = non_test_rust_sources("crates/rumoca-tool-lsp/src");

    assert!(
        !schema.contains("pub struct RenderedTargetFile"),
        "the raw path/content RenderedTargetFile compatibility DTO must remain deleted"
    );
    assert!(
        !compile_facade
            .split(|ch: char| !ch.is_ascii_alphanumeric())
            .any(|word| word == "RenderedTargetFile"),
        "rumoca-compile must not reexport the retired RenderedTargetFile DTO"
    );
    for forbidden in [
        "RenderedTargetFile",
        "render_checked_dae_target_files",
        "rumoca_phase_codegen",
        "AlgorithmCodeTemplateRenderer",
        "SolveTemplateRenderer",
        "lower_to_algorithm_code",
    ] {
        assert!(
            !wasm.contains(forbidden),
            "bind-wasm restored retired client-side target rendering `{forbidden}`"
        );
    }
    for forbidden in [
        "RenderedTargetFile",
        "rumoca_phase_codegen",
        "rumoca_phase_galec",
        "rumoca_sim::lower_solve_model",
        "AlgorithmCodeTemplateRenderer",
        "SolveTemplateRenderer",
    ] {
        assert!(
            !lsp.contains(forbidden),
            "LSP restored retired client-side target rendering `{forbidden}`; use StrictCompilation::render_target"
        );
    }
}

fn non_test_rust_sources(relative: &str) -> String {
    let root = workspace_root();
    let mut paths = Vec::new();
    collect_rs_files(&root.join(relative), &mut paths);
    paths.sort();
    paths
        .into_iter()
        .filter(|path| {
            let relative = path
                .strip_prefix(&root)
                .expect("source beneath workspace")
                .to_string_lossy();
            !relative.contains("/tests/") && !relative.ends_with("/tests.rs")
        })
        .map(|path| {
            fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[test]
fn compile_owned_target_artifact_closes_private_issuers_without_generic_folds() {
    let schema = source("crates/rumoca-compile/src/codegen_target/checked_plan.rs");
    let artifact_root =
        source("crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact.rs");
    let artifact_rendering = source(
        "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs",
    );
    let artifact = format!("{artifact_root}\n{artifact_rendering}");
    let root_rendering = source("crates/rumoca/src/target_manifest.rs");
    let syntax =
        syn::parse_file(&artifact_rendering).expect("parse compile-owned target rendering source");

    assert_compile_private_plan_boundary(&schema);
    assert_compile_owned_close_surface(&artifact);
    assert_private_consuming_issuers(&syntax);
    assert_private_completion_carriers(&syntax);
    assert_fixed_direct_completion_schema(&artifact);
    assert_member_brand_signatures_are_closed(&syntax);
    assert_sole_completed_artifact_producer();
    assert_root_is_a_thin_target_facade(&root_rendering);
}

fn assert_fixed_direct_completion_schema(artifact: &str) {
    for required in [
        "AstPackageIssuer",
        "AstPackageCompletion",
        "FlatPackageIssuer",
        "FlatPackageCompletion",
        "DaePackageIssuer",
        "DaePackageCompletion",
        "SolveDirectPackageIssuer",
        "SolveDirectPackageCompletion",
        "FmiPackageIssuer",
        "FmiPackageCompletion",
        "AstUnpackagedIssuer",
        "AstUnpackagedCompletion",
        "FlatUnpackagedIssuer",
        "FlatUnpackagedCompletion",
        "DaeUnpackagedIssuer",
        "DaeUnpackagedCompletion",
        "SolveDirectUnpackagedIssuer",
        "SolveDirectUnpackagedCompletion",
        "FmiUnpackagedIssuer",
        "FmiUnpackagedCompletion",
        "struct $issuer<'member, 'operation, 'inv>",
        "struct $completion<'member>",
        "brand: MemberBrand<'member>",
        "Result<$completion<'member>>",
        "impl $completion<'_>",
        "fn erase(self)",
    ] {
        assert!(
            artifact.contains(required),
            "fixed direct member closure lost concrete schema/invocation `{required}`"
        );
    }
    assert_eq!(
        artifact.matches("define_direct_package_close!(").count(),
        5,
        "every direct packaged product must use the one fixed close schema"
    );
    assert_eq!(
        artifact.matches("define_direct_unpacked_close!(").count(),
        5,
        "every direct unpackaged product must use the one fixed close schema"
    );
}

#[test]
fn completed_artifact_producer_census_resolves_alias_self_and_from() {
    let fixture = syn::parse_file(
        r#"
        pub struct CompletedTargetArtifact;
        pub type CompletionAlias = CompletedTargetArtifact;
        impl CompletionAlias {
            pub fn from_parts() -> Self { unimplemented!() }
        }
        impl From<u8> for CompletionAlias {
            fn from(_: u8) -> Self { unimplemented!() }
        }
        pub fn expose() -> CompletionAlias { unimplemented!() }
        "#,
    )
    .expect("parse producer-census adversary");
    let aliases = completed_artifact_aliases(&fixture.items);
    let mut producers = Vec::new();
    collect_public_completed_artifact_producers(
        &fixture.items,
        "fixture.rs",
        &aliases,
        &mut producers,
    );
    producers.sort();
    assert_eq!(
        producers,
        [
            "fixture.rs::expose",
            "fixture.rs::from_parts",
            "fixture.rs::impl:From",
            "fixture.rs::type:CompletionAlias",
        ],
        "producer census must resolve public aliases, `Self`, and From-style construction"
    );
}

fn assert_compile_private_plan_boundary(schema: &str) {
    for required in [
        "mod target_artifact;",
        "fn into_plan(self) -> CheckedTargetRenderPlan",
        "fn into_product_plan(self) -> CheckedTargetPackageProductPlan",
    ] {
        assert!(
            schema.contains(required),
            "checked target construction lost its compile-private render-plan boundary `{required}`"
        );
    }
    for forbidden in [
        "pub fn render_plan(",
        "pub fn into_plan(",
        "pub enum CheckedTargetPackageMember",
        "pub struct CheckedTargetPackageFold",
        "pub struct CheckedPackagedTargetFileIssuer",
        "pub struct CheckedPackagedTargetFileCompletion",
        "pub fn members(&self) -> impl ExactSizeIterator<Item = CheckedTargetRenderStep",
        "pub fn members(&self) -> impl ExactSizeIterator<Item = CheckedTargetPackageMember",
        "pub fn algorithm_code_source(self) -> CheckedTargetPackageFileMemberRef",
    ] {
        assert!(
            !schema.contains(forbidden),
            "checked plans must not expose ambient sibling/member authority `{forbidden}`"
        );
    }
}

fn assert_compile_owned_close_surface(artifact: &str) {
    for required in [
        "struct MemberBrand<'member>",
        "struct MemberScope;",
        "PhantomData<fn(&'member mut ()) -> &'member mut ()>",
        "fn mint(_scope: &'member mut MemberScope) -> Self",
        "struct PackagedAlgorithmCodeIssuer<'member, 'operation, 'inv>",
        "struct SolveAlgorithmIssuer<'member, 'operation, 'inv>",
        "macro_rules! define_direct_package_close",
        "struct AlgorithmCodeSourceIssuer<'member, 'operation, 'inv>",
        "macro_rules! define_direct_unpacked_close",
        "fn close_rendered(\n        self,",
        "pub fn render_target(\n        &self,\n        target: CheckedTargetBundle,\n        input: ArtifactSessionInput,",
        "Result<CompletedTargetArtifact>",
        "with_target_invocation_brand(|brand|",
        "CompletedTargetArtifact::Packaged",
        "CompletedTargetArtifact::Unpackaged",
    ] {
        assert!(
            artifact.contains(required),
            "compile-owned target closure lost required authority `{required}`"
        );
    }
    assert_eq!(
        artifact.matches("let mut scope = MemberScope;").count(),
        2,
        "each package/unpackaged fold closer must create one fresh local member scope"
    );
    assert_eq!(
        artifact.matches("MemberBrand::mint(&mut scope)").count(),
        2,
        "the two fold closers must mint their brand only from their fresh local scope"
    );
    assert_eq!(
        artifact.matches("define_direct_package_close!(").count(),
        5,
        "the fixed direct package close macro must be instantiated once for each canonical direct product"
    );
    assert_eq!(
        artifact.matches("define_direct_unpacked_close!(").count(),
        5,
        "the fixed direct unpackaged close macro must be instantiated once for each canonical direct product"
    );
    for forbidden in [
        "pub struct PackagedAlgorithmCodeIssuer",
        "pub struct SolveAlgorithmIssuer",
        "pub struct DirectPackageIssuer",
        "pub struct AlgorithmCodeSourceIssuer",
        "pub struct DirectUnpackagedIssuer",
        "CheckedTargetPackageFold",
        "CheckedPackagedTargetFileIssuer",
        "CheckedPackagedTargetFileCompletion",
        "SealedRenderedTargetFile",
        "RenderedTargetFile",
        "fn render_fold<",
        "fn map<P",
        "FnMut(MemberBrand",
        "fn with_member_brand<R>(",
        "FnOnce(MemberBrand<'member>) -> R",
        "let _ = (self.step, self.brand);",
        "let _ = (core, mode, self.brand);",
        "let _ = (core, self.brand);",
        "MemberBrand {",
        "struct DirectPackageIssuer",
        "struct DirectPackageCompletion",
        "struct DirectUnpackagedIssuer",
        "struct DirectUnpackagedCompletion",
        "trait DirectProduct",
        "P: DirectProduct",
    ] {
        assert!(
            !artifact.contains(forbidden),
            "compile-owned target closure reintroduced a public/generic byte-admission bridge `{forbidden}`"
        );
    }
}

fn assert_private_consuming_issuers(syntax: &syn::File) {
    for name in [
        "PackagedAlgorithmCodeIssuer",
        "SolveAlgorithmIssuer",
        "AlgorithmCodeSourceIssuer",
    ] {
        let issuer = syntax
            .items
            .iter()
            .find_map(|item| match item {
                syn::Item::Struct(item) if item.ident == name => Some(item),
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing compile-owned issuer {name}"));
        assert!(
            matches!(issuer.vis, syn::Visibility::Inherited),
            "{name} must remain private to compile-owned target closure"
        );
        assert!(
            issuer
                .fields
                .iter()
                .all(|field| matches!(field.vis, syn::Visibility::Inherited)),
            "{name} member authority must not expose writable fields"
        );
        assert!(
            issuer
                .generics
                .params
                .iter()
                .all(|parameter| matches!(parameter, syn::GenericParam::Lifetime(_))),
            "{name} must not reopen the fixed member protocol over a generic payload/product/error type"
        );
        let derives = issuer
            .attrs
            .iter()
            .filter(|attribute| attribute.path().is_ident("derive"))
            .map(|attribute| quote::quote!(#attribute).to_string())
            .collect::<String>();
        for forbidden in ["Clone", "Copy", "Default", "Serialize", "Deserialize"] {
            assert!(
                !derives
                    .split(|ch: char| !ch.is_ascii_alphanumeric())
                    .any(|word| word == forbidden),
                "{name} must be affine and non-wire; found derive {forbidden}"
            );
        }
        assert_no_forbidden_trait_impl(syntax, name);

        let close = syntax
            .items
            .iter()
            .filter_map(|item| match item {
                syn::Item::Impl(item)
                    if matches!(
                        item.self_ty.as_ref(),
                        syn::Type::Path(path)
                            if path.path.segments.last().is_some_and(|segment| segment.ident == name)
                    ) => Some(item),
                _ => None,
            })
            .flat_map(|item| &item.items)
            .find_map(|item| match item {
                syn::ImplItem::Fn(function) if function.sig.ident == "close_rendered" => {
                    Some(function)
                }
                _ => None,
            })
            .unwrap_or_else(|| panic!("missing consuming closer for {name}"));
        assert!(
            matches!(close.vis, syn::Visibility::Inherited),
            "{name} closer must remain private"
        );
        assert!(
            matches!(
                close.sig.inputs.first(),
                Some(syn::FnArg::Receiver(receiver)) if receiver.reference.is_none()
            ),
            "{name} closer must consume its fresh issuer"
        );
    }
}

fn assert_private_completion_carriers(syntax: &syn::File) {
    let mut count = 0;
    for item in &syntax.items {
        let (name, visibility, attributes, generics) = match item {
            syn::Item::Struct(item) if item.ident.to_string().ends_with("Completion") => (
                item.ident.to_string(),
                &item.vis,
                item.attrs.as_slice(),
                &item.generics,
            ),
            syn::Item::Enum(item) if item.ident.to_string().ends_with("Completion") => (
                item.ident.to_string(),
                &item.vis,
                item.attrs.as_slice(),
                &item.generics,
            ),
            _ => continue,
        };
        count += 1;
        assert!(
            matches!(visibility, syn::Visibility::Inherited),
            "{name} must remain private to the compile-owned fold"
        );
        assert!(
            generics
                .params
                .iter()
                .all(|parameter| matches!(parameter, syn::GenericParam::Lifetime(_))),
            "{name} must be a fixed concrete completion, not a generic payload/product/error protocol"
        );
        let derives = attributes
            .iter()
            .filter(|attribute| attribute.path().is_ident("derive"))
            .map(|attribute| quote::quote!(#attribute).to_string())
            .collect::<String>();
        for forbidden in ["Clone", "Copy", "Default", "Serialize", "Deserialize"] {
            assert!(
                !derives
                    .split(|ch: char| !ch.is_ascii_alphanumeric())
                    .any(|word| word == forbidden),
                "{name} must be affine and non-wire; found derive {forbidden}"
            );
        }
        assert_no_forbidden_trait_impl(syntax, &name);
    }
    assert!(count > 0, "the member fold must retain branded completions");
}

fn assert_member_brand_signatures_are_closed(syntax: &syn::File) {
    let mut signatures = Vec::new();
    collect_signatures(&syntax.items, &mut signatures);
    for signature in signatures {
        let tokens = quote::quote!(#signature).to_string();
        let output = &signature.output;
        let output = quote::quote!(#output).to_string();
        if !tokens.contains("MemberBrand") {
            continue;
        }
        assert!(
            signature
                .generics
                .params
                .iter()
                .all(|parameter| matches!(parameter, syn::GenericParam::Lifetime(_))),
            "member-brand operations must not choose a caller-controlled result/error/payload type: {tokens}"
        );
        assert!(
            !tokens.contains("FnOnce") && !tokens.contains("FnMut") && !tokens.contains("Fn <"),
            "member-brand operations must use one private fixed completion protocol, not a generic callback: {tokens}"
        );
        if signature.ident == "close" {
            assert!(
                output.contains("Completion")
                    && !output.contains("Closed")
                    && !output.contains("Completed"),
                "a branded close operation must return its fixed lifetime-carrying completion; only the private fold eraser may produce an unbranded closed member: {tokens}"
            );
        }
    }
    assert_closed_file_producer_boundaries(syntax);
}

fn record_closed_file_function(
    function: &syn::ItemFn,
    fold_erasers: &mut Vec<String>,
    erase_callers: &mut Vec<String>,
) {
    let name = function.sig.ident.to_string();
    if signature_returns_closed_file(&function.sig) {
        assert!(
            matches!(function.vis, syn::Visibility::Inherited)
                && ["close_package_member", "close_unpackaged_member"].contains(&name.as_str()),
            "only the two private fold closers may return an unbranded closed file: {name}"
        );
        fold_erasers.push(name.clone());
    }
    let body = &function.block;
    if quote::quote!(#body).to_string().contains(". erase (") {
        erase_callers.push(name);
    }
}

fn record_closed_file_impl(block: &syn::ItemImpl, erase_callers: &mut Vec<String>) {
    let owner = type_path_name(&block.self_ty).unwrap_or_default();
    for item in &block.items {
        let syn::ImplItem::Fn(function) = item else {
            continue;
        };
        if signature_returns_closed_file(&function.sig) {
            assert!(
                owner.ends_with("Completion")
                    && function.sig.ident == "erase"
                    && matches!(function.vis, syn::Visibility::Inherited)
                    && matches!(
                        function.sig.inputs.first(),
                        Some(syn::FnArg::Receiver(receiver)) if receiver.reference.is_none()
                    ),
                "closed files may be produced only by a private consuming completion eraser: {owner}::{}",
                function.sig.ident
            );
        }
        let body = &function.block;
        if quote::quote!(#body).to_string().contains(". erase (") {
            erase_callers.push(format!("{owner}::{}", function.sig.ident));
        }
    }
}

fn assert_closed_file_producer_boundaries(syntax: &syn::File) {
    let mut fold_erasers = Vec::new();
    let mut erase_callers = Vec::new();
    for item in &syntax.items {
        match item {
            syn::Item::Fn(function) => {
                record_closed_file_function(function, &mut fold_erasers, &mut erase_callers);
            }
            syn::Item::Impl(block) => {
                record_closed_file_impl(block, &mut erase_callers);
            }
            _ => {}
        }
    }
    fold_erasers.sort();
    assert_eq!(
        fold_erasers,
        ["close_package_member", "close_unpackaged_member"],
        "only the two private product-family fold erasers may remove member brands into closed files"
    );
    erase_callers.sort();
    assert_eq!(
        erase_callers,
        [
            "PackageMemberCompletion::erase",
            "UnpackagedMemberCompletion::erase",
            "close_package_member",
            "close_unpackaged_member",
        ],
        "completion erasure may occur only in the two exhaustive completion enums and their two fold closers"
    );
}

fn signature_returns_closed_file(signature: &syn::Signature) -> bool {
    let output = &signature.output;
    let output = quote::quote!(#output).to_string();
    output.contains("ClosedPackageFile") || output.contains("ClosedUnpackagedFile")
}

fn assert_sole_completed_artifact_producer() {
    let root = workspace_root();
    let mut compile_sources = Vec::new();
    collect_rs_files(
        &root.join("crates/rumoca-compile/src"),
        &mut compile_sources,
    );
    let mut public_artifact_results = Vec::new();
    for path in compile_sources {
        let relative = path
            .strip_prefix(&root)
            .expect("compile source beneath workspace")
            .to_string_lossy()
            .replace('\\', "/");
        if relative.contains("/tests/") || relative.ends_with("/tests.rs") {
            continue;
        }
        let source =
            fs::read_to_string(&path).unwrap_or_else(|error| panic!("read {relative}: {error}"));
        let file =
            syn::parse_file(&source).unwrap_or_else(|error| panic!("parse {relative}: {error}"));
        let aliases = completed_artifact_aliases(&file.items);
        collect_public_completed_artifact_producers(
            &file.items,
            &relative,
            &aliases,
            &mut public_artifact_results,
        );
    }
    public_artifact_results.sort();
    assert_eq!(
        public_artifact_results,
        [
            "crates/rumoca-compile/src/codegen_target/checked_plan/target_artifact/rendering.rs::render_target"
        ],
        "StrictCompilation::render_target must be the sole public operation producing a completed target artifact"
    );
}

fn assert_root_is_a_thin_target_facade(root_rendering: &str) {
    assert!(
        root_rendering.contains("result.strict().render_target(target, artifact_input)"),
        "the root facade must delegate the complete checked target to StrictCompilation::render_target"
    );
    for forbidden in [
        "struct RootPackagedTargetMemberIssuer",
        "struct RootUnpackagedTargetMemberIssuer",
        "ArtifactSession::construct(",
        "with_member_brand(",
        "close_rendered(",
        "render_web(",
        "render_and_package(",
    ] {
        assert!(
            !root_rendering.contains(forbidden),
            "root orchestration reintroduced compile-owned target authority `{forbidden}`"
        );
    }
}

fn collect_public_completed_artifact_producers(
    items: &[syn::Item],
    path: &str,
    aliases: &BTreeSet<String>,
    producers: &mut Vec<String>,
) {
    for item in items {
        match item {
            syn::Item::Fn(function)
                if matches!(function.vis, syn::Visibility::Public(_))
                    && signature_returns_completed_artifact(&function.sig, false, aliases) =>
            {
                producers.push(format!("{path}::{}", function.sig.ident));
            }
            syn::Item::Impl(block) => {
                collect_public_completed_artifact_impl(block, path, aliases, producers);
            }
            syn::Item::Mod(module) => {
                if let Some((_, items)) = &module.content {
                    collect_public_completed_artifact_producers(items, path, aliases, producers);
                }
            }
            syn::Item::Type(alias)
                if matches!(alias.vis, syn::Visibility::Public(_))
                    && aliases.contains(&alias.ident.to_string()) =>
            {
                producers.push(format!("{path}::type:{}", alias.ident));
            }
            _ => {}
        }
    }
}

fn collect_public_completed_artifact_impl(
    block: &syn::ItemImpl,
    path: &str,
    aliases: &BTreeSet<String>,
    producers: &mut Vec<String>,
) {
    let self_is_completed =
        type_path_name(&block.self_ty).is_some_and(|name| aliases.contains(&name));
    record_completed_artifact_trait_impl(block, path, aliases, self_is_completed, producers);
    for item in &block.items {
        let syn::ImplItem::Fn(function) = item else {
            continue;
        };
        if !matches!(function.vis, syn::Visibility::Public(_)) {
            continue;
        }
        if !signature_returns_completed_artifact(&function.sig, self_is_completed, aliases) {
            continue;
        }
        producers.push(format!("{path}::{}", function.sig.ident));
    }
}

fn record_completed_artifact_trait_impl(
    block: &syn::ItemImpl,
    path: &str,
    aliases: &BTreeSet<String>,
    self_is_completed: bool,
    producers: &mut Vec<String>,
) {
    let Some((_, trait_path, _)) = &block.trait_ else {
        return;
    };
    let trait_name = trait_path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
        .unwrap_or_default();
    let trait_spelling = quote::quote!(#trait_path).to_string();
    let constructs_completed =
        self_is_completed && ["From", "TryFrom", "FromIterator"].contains(&trait_name.as_str());
    let consumes_completed = trait_name == "Into" && tokens_name_one_of(&trait_spelling, aliases);
    if constructs_completed || consumes_completed {
        producers.push(format!("{path}::impl:{trait_name}"));
    }
}

fn collect_generic_semantic_file_joins(
    items: &[syn::Item],
    path: &str,
    offenders: &mut Vec<String>,
) {
    let mut signatures = Vec::new();
    collect_signatures(items, &mut signatures);
    for signature in signatures {
        let inputs = signature
            .inputs
            .iter()
            .map(|input| quote::quote!(#input).to_string())
            .collect::<String>();
        if inputs.contains("CodegenInput") && inputs.contains("SemanticTemplateFile") {
            offenders.push(format!("{path}::{}", signature.ident));
        }
    }
}

fn collect_signatures<'a>(items: &'a [syn::Item], signatures: &mut Vec<&'a syn::Signature>) {
    for item in items {
        match item {
            syn::Item::Fn(function) => signatures.push(&function.sig),
            syn::Item::Impl(block) => {
                signatures.extend(block.items.iter().filter_map(|item| match item {
                    syn::ImplItem::Fn(function) => Some(&function.sig),
                    _ => None,
                }));
            }
            syn::Item::Trait(block) => {
                signatures.extend(block.items.iter().filter_map(|item| match item {
                    syn::TraitItem::Fn(function) => Some(&function.sig),
                    _ => None,
                }));
            }
            syn::Item::Mod(module) => {
                if let Some((_, items)) = &module.content {
                    collect_signatures(items, signatures);
                }
            }
            _ => {}
        }
    }
}

fn signature_returns_completed_artifact(
    signature: &syn::Signature,
    self_is_completed: bool,
    aliases: &BTreeSet<String>,
) -> bool {
    let output = &signature.output;
    let output = quote::quote!(#output).to_string();
    tokens_name_one_of(&output, aliases)
        || (self_is_completed
            && output
                .split(|ch: char| !ch.is_ascii_alphanumeric())
                .any(|token| token == "Self"))
}

fn completed_artifact_aliases(items: &[syn::Item]) -> BTreeSet<String> {
    let mut aliases = BTreeSet::from(["CompletedTargetArtifact".to_owned()]);
    loop {
        let previous_len = aliases.len();
        collect_completed_artifact_aliases(items, &mut aliases);
        if aliases.len() == previous_len {
            return aliases;
        }
    }
}

fn collect_completed_artifact_aliases(items: &[syn::Item], aliases: &mut BTreeSet<String>) {
    for item in items {
        match item {
            syn::Item::Type(alias)
                if type_path_name(&alias.ty).is_some_and(|target| aliases.contains(&target)) =>
            {
                aliases.insert(alias.ident.to_string());
            }
            syn::Item::Use(item) => collect_completed_artifact_use_aliases(&item.tree, aliases),
            syn::Item::Mod(module) => {
                if let Some((_, nested)) = &module.content {
                    collect_completed_artifact_aliases(nested, aliases);
                }
            }
            _ => {}
        }
    }
}

fn collect_completed_artifact_use_aliases(tree: &syn::UseTree, aliases: &mut BTreeSet<String>) {
    match tree {
        syn::UseTree::Path(path) => {
            collect_completed_artifact_use_aliases(&path.tree, aliases);
        }
        syn::UseTree::Rename(rename) if aliases.contains(&rename.ident.to_string()) => {
            aliases.insert(rename.rename.to_string());
        }
        syn::UseTree::Group(group) => {
            for item in &group.items {
                collect_completed_artifact_use_aliases(item, aliases);
            }
        }
        syn::UseTree::Name(_) | syn::UseTree::Rename(_) | syn::UseTree::Glob(_) => {}
    }
}

fn tokens_name_one_of(tokens: &str, names: &BTreeSet<String>) -> bool {
    tokens
        .split(|ch: char| !ch.is_ascii_alphanumeric() && ch != '_')
        .any(|token| names.contains(token))
}

fn assert_no_forbidden_trait_impl(syntax: &syn::File, target: &str) {
    let type_aliases = syntax
        .items
        .iter()
        .filter_map(|item| match item {
            syn::Item::Type(alias) => {
                type_path_name(&alias.ty).map(|resolved| (alias.ident.to_string(), resolved))
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    let mut trait_aliases = Vec::new();
    for item in &syntax.items {
        if let syn::Item::Use(item) = item {
            collect_renamed_imports(&item.tree, &mut trait_aliases);
        }
    }
    for item in &syntax.items {
        let syn::Item::Impl(item) = item else {
            continue;
        };
        let Some((_, trait_path, _)) = &item.trait_ else {
            continue;
        };
        let Some(mut trait_name) = trait_path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
        else {
            continue;
        };
        if let Some((_, original)) = trait_aliases.iter().find(|(alias, _)| alias == &trait_name) {
            trait_name.clone_from(original);
        }
        if !["Clone", "Copy", "Default", "Serialize", "Deserialize"].contains(&trait_name.as_str())
        {
            continue;
        }
        let Some(mut self_name) = type_path_name(&item.self_ty) else {
            continue;
        };
        for _ in 0..=type_aliases.len() {
            let Some((_, resolved)) = type_aliases.iter().find(|(alias, _)| alias == &self_name)
            else {
                break;
            };
            self_name.clone_from(resolved);
        }
        assert_ne!(
            self_name, target,
            "{target} must not gain a handwritten or aliased {trait_name} implementation"
        );
    }
}

fn type_path_name(ty: &syn::Type) -> Option<String> {
    match ty {
        syn::Type::Path(path) => path
            .path
            .segments
            .last()
            .map(|segment| segment.ident.to_string()),
        _ => None,
    }
}

fn collect_renamed_imports(tree: &syn::UseTree, aliases: &mut Vec<(String, String)>) {
    match tree {
        syn::UseTree::Path(path) => collect_renamed_imports(&path.tree, aliases),
        syn::UseTree::Rename(rename) => {
            aliases.push((rename.rename.to_string(), rename.ident.to_string()))
        }
        syn::UseTree::Group(group) => {
            for tree in &group.items {
                collect_renamed_imports(tree, aliases);
            }
        }
        syn::UseTree::Name(_) | syn::UseTree::Glob(_) => {}
    }
}
