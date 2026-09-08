//! Syntax-aware construction boundary for the FMI continuous-state domain.
//!
//! This gate inventories Rust syntax; it does not infer types or claim semantic
//! correctness. Rust privacy and type checking prove the inventoried calls,
//! while mutation controls prove that an added raw-count route changes this
//! review boundary instead of hiding behind an unchanged preferred spelling.

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;

use syn::visit::{self, Visit};

use super::architecture_hardening_support::{
    attributes_require_test, production_rust_sources, workspace_root,
};

const TRACKED_CONSTRUCTORS: [&str; 10] = [
    "FmiContinuousStateWidth",
    "MeContinuousStateDomain",
    "MeSolverTolerances",
    "RootScanStateWidth",
    "RootScanIndicatorWidth",
    "RootScanShape",
    "RootScanWorkspace",
    "MeHostComponent",
    "MeRootSearchPolicy",
    "MeNumericalSetup",
];

const PROTECTED_AGGREGATES: [&str; 22] = [
    "MeBackendProbe",
    "MeContinuousStateDomain",
    "KernelDerivatives",
    "PreparedMeInstantiation",
    "MeKernelBody",
    "MeRetainedComponent",
    "MeHostState",
    "InitializationOutcome",
    "InitializationStatus",
    "MeComponentHost",
    "MeSimulationSession",
    "MeHostComponent",
    "MeRootSearchPolicy",
    "MeRootSearchState",
    "RootScanShape",
    "RootScanWorkspace",
    "ScanSample",
    "MeContinuousPoint",
    "MeAdvanceRequest",
    "MeStepProposal",
    "MeAcceptedStep",
    "MeNumericalSetup",
];

const LINKED_WIDTH_CONSTRUCTOR: &str =
    "crates/rumoca-ir-solve/src/fmi/linked_runtime.rs::FmiContinuousStateWidth::new";
const LINKED_WIDTH_ISSUER: &str =
    "crates/rumoca-ir-solve/src/fmi/linked_runtime.rs::FmiLinkedRuntimeFacts::construct";
const STATE_DOMAIN_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me.rs::MeContinuousStateDomain::from_linked";
const VERIFICATION_DOMAIN_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me.rs::MeContinuousStateDomain::from_verification_rates";
const STATE_DOMAIN_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/kernel/component/instantiation.rs::prepare_me_instantiation";
const VERIFICATION_DOMAIN_MINT: &str =
    "crates/rumoca-solver/src/fmi_me/backend_test_support.rs::MeBackendProbe::linear";
const VERIFICATION_DOMAIN_FIELD: &str =
    "crates/rumoca-solver/src/fmi_me/backend_test_support.rs::MeBackendProbe::state_domain";
const ROOT_SHAPE_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me/kernel.rs::RootScanShape::issue";
const ROOT_STATE_WIDTH_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me/kernel.rs::RootScanStateWidth::issue";
const ROOT_INDICATOR_WIDTH_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me/kernel.rs::RootScanIndicatorWidth::issue";
const ROOT_SHAPE_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/kernel.rs::SolveMeKernel::root_scan_shape";
const ROOT_SEARCH_OWNER: &str = "crates/rumoca-solver/src/fmi_me/root.rs::MeRootSearchState::new";
const KERNEL_ROOT_OWNER: &str =
    "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::new";
const HOST_COMPONENT_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::build_restored_host_state";
const ROOT_POLICY_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me/root.rs::MeRootSearchPolicy::new";
const ROOT_POLICY_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/root.rs::MeRootSearchState::configure_active";
const NUMERICAL_SETUP_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me/integrator.rs::MeNumericalSetup::from_checked_host";
const HOST_NUMERICAL_SETUP_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeComponentHost::numerical_setup";
const VERIFICATION_NUMERICAL_SETUP_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/backend_test_support.rs::MeBackendProbe::numerical_setup";
const TOLERANCE_CONSTRUCTOR: &str = "crates/rumoca-solver/src/fmi_me.rs::MeSolverTolerances::check";
const OPTIONS_TOLERANCE_ISSUER: &str =
    "crates/rumoca-solver/src/fmi_me/session/options.rs::MeSessionOptions::new";
const ROOT_WORKSPACE_CONSTRUCTOR: &str =
    "crates/rumoca-solver/src/fmi_me/root.rs::RootScanWorkspace::new";
const COMPONENT_ACCEPT_OWNER: &str = "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::accept_proposal";
const COMPONENT_SCAN_OWNER: &str = "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::scan_accepted_interval";
const SESSION_ACCEPT_OWNER: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeSimulationSession::accept_proposal";
const SESSION_ACCEPT_CALLER: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeSimulationSession::consume_accepted_step";
const SESSION_SCAN_OWNER: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeSimulationSession::scan_interval";
const COMPONENT_RETAINED_REFRESH_OWNER: &str = "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::refresh_retained_indicators";
const SESSION_RETAINED_REFRESH_OWNER: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeSimulationSession::refresh_retained_indicators";
const SESSION_RETAINED_REFRESH_START: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeSimulationSession::restart_plugin_history";
const SESSION_RETAINED_REFRESH_STEP: &str =
    "crates/rumoca-solver/src/fmi_me/session.rs::MeSimulationSession::commit_accepted_endpoint";
#[derive(Clone)]
struct RustSource {
    path: PathBuf,
    source: String,
}

struct Inventory {
    constructor_definitions: BTreeMap<String, BTreeSet<String>>,
    direct_mints: BTreeMap<String, BTreeSet<String>>,
    associated_calls: BTreeMap<String, BTreeSet<String>>,
    owner_method_calls: BTreeMap<String, BTreeSet<String>>,
    forbidden_traits_or_derives: BTreeSet<String>,
    root_shape_uses: BTreeSet<String>,
    linked_width_extractions: BTreeSet<String>,
    linked_width_inputs: BTreeSet<String>,
    verification_domain_fields: BTreeSet<String>,
    verification_domain_rates_inputs: BTreeSet<String>,
    verification_domain_mint_provenance: BTreeSet<String>,
    raw_dimension_fields: BTreeSet<String>,
    raw_dimension_constructor_inputs: BTreeSet<String>,
    exposed_kernel_root_fields: BTreeSet<String>,
    exposed_component_owner_handles: BTreeSet<String>,
    exposed_protected_capabilities: BTreeSet<String>,
    parallel_host_component_fields: BTreeSet<String>,
    workspace_new_visibilities: BTreeSet<String>,
}

impl Inventory {
    fn new() -> Self {
        Self {
            constructor_definitions: BTreeMap::new(),
            direct_mints: BTreeMap::new(),
            associated_calls: BTreeMap::new(),
            owner_method_calls: BTreeMap::new(),
            forbidden_traits_or_derives: BTreeSet::new(),
            root_shape_uses: BTreeSet::new(),
            linked_width_extractions: BTreeSet::new(),
            linked_width_inputs: BTreeSet::new(),
            verification_domain_fields: BTreeSet::new(),
            verification_domain_rates_inputs: BTreeSet::new(),
            verification_domain_mint_provenance: BTreeSet::new(),
            raw_dimension_fields: BTreeSet::new(),
            raw_dimension_constructor_inputs: BTreeSet::new(),
            exposed_kernel_root_fields: BTreeSet::new(),
            exposed_component_owner_handles: BTreeSet::new(),
            exposed_protected_capabilities: BTreeSet::new(),
            parallel_host_component_fields: BTreeSet::new(),
            workspace_new_visibilities: BTreeSet::new(),
        }
    }

    fn insert_constructor(&mut self, type_name: &str, route: String) {
        insert_map_route(&mut self.constructor_definitions, type_name, route);
    }

    fn insert_direct_mint(&mut self, type_name: &str, route: String) {
        insert_map_route(&mut self.direct_mints, type_name, route);
    }

    fn insert_associated_call(&mut self, call: String, route: String) {
        insert_map_route(&mut self.associated_calls, &call, route);
    }

    fn insert_owner_method_call(&mut self, method: &str, route: String) {
        insert_map_route(&mut self.owner_method_calls, method, route);
    }
}

fn insert_map_route(map: &mut BTreeMap<String, BTreeSet<String>>, key: &str, route: String) {
    match map.entry(key.to_owned()) {
        std::collections::btree_map::Entry::Vacant(entry) => {
            entry.insert(BTreeSet::from([route]));
        }
        std::collections::btree_map::Entry::Occupied(mut entry) => {
            entry.get_mut().insert(route);
        }
    }
}

struct TypeNames {
    names: BTreeSet<String>,
}

impl TypeNames {
    fn in_type(ty: &syn::Type) -> BTreeSet<String> {
        let mut visitor = Self {
            names: BTreeSet::new(),
        };
        visitor.visit_type(ty);
        visitor.names
    }
}

impl<'ast> Visit<'ast> for TypeNames {
    fn visit_path_segment(&mut self, segment: &'ast syn::PathSegment) {
        self.names.insert(segment.ident.to_string());
        visit::visit_path_segment(self, segment);
    }
}

struct PatternNames {
    names: BTreeSet<String>,
}

impl PatternNames {
    fn in_pattern(pattern: &syn::Pat) -> BTreeSet<String> {
        let mut visitor = Self {
            names: BTreeSet::new(),
        };
        visitor.visit_pat(pattern);
        visitor.names
    }
}

impl<'ast> Visit<'ast> for PatternNames {
    fn visit_pat_ident(&mut self, pattern: &'ast syn::PatIdent) {
        self.names.insert(pattern.ident.to_string());
        visit::visit_pat_ident(self, pattern);
    }
}

struct BoundaryVisitor<'a> {
    file: String,
    current_impl: Option<String>,
    current_trait: Option<String>,
    current_function: Option<String>,
    inventory: &'a mut Inventory,
}

impl BoundaryVisitor<'_> {
    fn route(&self) -> String {
        self.current_function
            .clone()
            .unwrap_or_else(|| format!("{}::<module>", self.file))
    }

    fn resolved_type_name(&self, written: &str) -> Option<String> {
        if written == "Self" {
            self.current_impl.clone()
        } else {
            Some(written.to_owned())
        }
    }

    fn inspect_signature(&mut self, signature: &syn::Signature, visibility: &syn::Visibility) {
        let returns = return_type_names(&signature.output);
        let constructs_impl = self
            .current_impl
            .as_ref()
            .is_some_and(|owner| returns.contains("Self") || returns.contains(owner));
        self.record_tracked_constructor(signature, constructs_impl, visibility);
        self.record_protected_constructor_inputs(signature, &returns, constructs_impl);
        self.record_linked_width_input(signature);
        self.record_verification_domain_rates_input(signature);
        self.record_component_owner_handle(signature);
        self.record_protected_capability_escape(signature);
    }

    fn record_tracked_constructor(
        &mut self,
        signature: &syn::Signature,
        constructs_impl: bool,
        visibility: &syn::Visibility,
    ) {
        let Some(owner) = self.current_impl.clone() else {
            return;
        };
        let route = self.route();
        if self.current_trait.is_none()
            && constructs_impl
            && TRACKED_CONSTRUCTORS.contains(&owner.as_str())
        {
            self.inventory.insert_constructor(&owner, route.clone());
        }
        if owner == "RootScanWorkspace" && signature.ident == "new" {
            self.inventory.workspace_new_visibilities.insert(format!(
                "{}:{}",
                route,
                visibility_name(visibility)
            ));
        }
    }

    fn record_protected_constructor_inputs(
        &mut self,
        signature: &syn::Signature,
        returns: &BTreeSet<String>,
        constructs_impl: bool,
    ) {
        let constructs_protected = (constructs_impl
            && self
                .current_impl
                .as_deref()
                .is_some_and(|owner| PROTECTED_AGGREGATES.contains(&owner)))
            || PROTECTED_AGGREGATES
                .iter()
                .any(|name| returns.contains(*name));
        if !constructs_protected {
            return;
        }
        let route = self.route();
        for input in raw_dimension_inputs(signature) {
            self.inventory
                .raw_dimension_constructor_inputs
                .insert(format!("{route}:{input}"));
        }
    }

    fn record_linked_width_input(&mut self, signature: &syn::Signature) {
        if typed_inputs(signature)
            .iter()
            .any(|(_, names)| names.contains("FmiContinuousStateWidth"))
        {
            let route = self.route();
            self.inventory.linked_width_inputs.insert(route);
        }
    }

    fn record_verification_domain_rates_input(&mut self, signature: &syn::Signature) {
        let route = self.route();
        if route != VERIFICATION_DOMAIN_MINT {
            return;
        }
        let has_rates = typed_inputs(signature).iter().any(|(patterns, types)| {
            patterns.contains("rates") && types.contains("Vec") && types.contains("f64")
        });
        if has_rates {
            self.inventory
                .verification_domain_rates_inputs
                .insert(route);
        }
    }

    fn record_component_owner_handle(&mut self, signature: &syn::Signature) {
        if self.current_impl.as_deref() != Some("MeHostComponent")
            || signature.ident == "new"
            || !return_type_names(&signature.output).contains("Rc")
        {
            return;
        }
        self.inventory
            .exposed_component_owner_handles
            .insert(self.route());
    }

    fn record_protected_capability_escape(&mut self, signature: &syn::Signature) {
        if !self
            .current_impl
            .as_deref()
            .is_some_and(|owner| PROTECTED_AGGREGATES.contains(&owner))
            || signature.ident == "new"
            || !returns_protected_capability(signature)
        {
            return;
        }
        self.inventory
            .exposed_protected_capabilities
            .insert(self.route());
    }

    fn record_associated_call(&mut self, path: &syn::Path) {
        let segments = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        let Some((written_type, method)) = associated_pair(&segments) else {
            return;
        };
        let Some(type_name) = self.resolved_type_name(written_type) else {
            return;
        };
        let call = format!("{type_name}::{method}");
        let route = self.route();
        if tracked_associated_call(&call) {
            self.inventory.insert_associated_call(call, route.clone());
        }
        if method == "root_scan_shape" {
            self.inventory.root_shape_uses.insert(route.clone());
        }
        if method == "continuous_state_width" {
            self.inventory.linked_width_extractions.insert(route);
        }
    }

    fn record_direct_call_mint(&mut self, path: &syn::Path) {
        let segments = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        let Some(written_type) = segments.last() else {
            return;
        };
        let Some(type_name) = self.resolved_type_name(written_type) else {
            return;
        };
        if TRACKED_CONSTRUCTORS.contains(&type_name.as_str()) {
            let route = self.route();
            self.inventory.insert_direct_mint(&type_name, route);
        }
    }

    fn record_direct_struct_mint(&mut self, path: &syn::Path) {
        let Some(last) = path.segments.last() else {
            return;
        };
        let Some(type_name) = self.resolved_type_name(&last.ident.to_string()) else {
            return;
        };
        if TRACKED_CONSTRUCTORS.contains(&type_name.as_str()) {
            let route = self.route();
            self.inventory.insert_direct_mint(&type_name, route);
        }
    }

    fn record_raw_fields(&mut self, owner: &str, fields: &syn::Fields) {
        if !PROTECTED_AGGREGATES.contains(&owner) {
            return;
        }
        for (index, field) in fields.iter().enumerate() {
            if is_bare_usize(&field.ty) {
                let name = field
                    .ident
                    .as_ref()
                    .map(ToString::to_string)
                    .unwrap_or_else(|| format!("#{index}"));
                let route = format!("{}::{owner}::{name}", self.file);
                self.inventory.raw_dimension_fields.insert(route);
            }
        }
    }

    fn record_component_owner_fields(&mut self, owner: &str, fields: &syn::Fields) {
        for (index, field) in fields.iter().enumerate() {
            let name = field
                .ident
                .as_ref()
                .map(ToString::to_string)
                .unwrap_or_else(|| format!("#{index}"));
            if owner == "MeHostComponent" && !matches!(&field.vis, syn::Visibility::Inherited) {
                self.inventory.exposed_kernel_root_fields.insert(format!(
                    "{}::MeHostComponent::{name}:{}",
                    self.file,
                    visibility_name(&field.vis)
                ));
            }
            if owner == "MeHostState" && is_parallel_component_authority(&field.ty) {
                self.inventory
                    .parallel_host_component_fields
                    .insert(format!("{}::MeHostState::{name}", self.file));
            }
            if owner == "MeBackendProbe"
                && name == "state_domain"
                && TypeNames::in_type(&field.ty).contains("MeContinuousStateDomain")
            {
                self.inventory
                    .verification_domain_fields
                    .insert(format!("{}::MeBackendProbe::{name}", self.file));
            }
        }
    }

    fn record_forbidden_derives(&mut self, owner: &str, attributes: &[syn::Attribute]) {
        if !TRACKED_CONSTRUCTORS.contains(&owner) {
            return;
        }
        for attribute in attributes
            .iter()
            .filter(|attribute| attribute.path().is_ident("derive"))
        {
            attribute
                .parse_nested_meta(|meta| {
                    self.record_forbidden_derive_name(owner, &meta.path);
                    Ok(())
                })
                .expect("derive syntax is parsed");
        }
    }

    /// Record one derive name when it is forbidden for `owner`.
    ///
    /// Split out of the `parse_nested_meta` callback so the name test is not
    /// nested inside the attribute loop and the callback; the recorded key and
    /// the forbidden-name predicate are unchanged.
    fn record_forbidden_derive_name(&mut self, owner: &str, path: &syn::Path) {
        let Some(name) = path
            .segments
            .last()
            .map(|segment| segment.ident.to_string())
        else {
            return;
        };
        if is_forbidden_trait_or_derive(owner, &name) {
            self.inventory
                .forbidden_traits_or_derives
                .insert(format!("{}::{owner}::{name}", self.file));
        }
    }

    fn record_verification_domain_mint_provenance(&mut self, call: &syn::ExprCall) {
        if self.route() != VERIFICATION_DOMAIN_MINT
            || !call_path_ends_with(call, "from_verification_rates")
            || call.args.len() != 1
            || !call.args.first().is_some_and(is_rates_borrow)
        {
            return;
        }
        self.inventory
            .verification_domain_mint_provenance
            .insert(VERIFICATION_DOMAIN_MINT.to_owned());
    }
}

impl<'ast> Visit<'ast> for BoundaryVisitor<'_> {
    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if !attributes_require_test(&module.attrs) {
            visit::visit_item_mod(self, module);
        }
    }

    fn visit_item_struct(&mut self, item: &'ast syn::ItemStruct) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        let owner = item.ident.to_string();
        self.record_forbidden_derives(&owner, &item.attrs);
        self.record_raw_fields(&owner, &item.fields);
        self.record_component_owner_fields(&owner, &item.fields);
        visit::visit_item_struct(self, item);
    }

    fn visit_item_enum(&mut self, item: &'ast syn::ItemEnum) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        let owner = item.ident.to_string();
        self.record_forbidden_derives(&owner, &item.attrs);
        for variant in &item.variants {
            self.record_raw_fields(&owner, &variant.fields);
        }
        visit::visit_item_enum(self, item);
    }

    fn visit_item_impl(&mut self, item: &'ast syn::ItemImpl) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        let saved_impl = self.current_impl.clone();
        let saved_trait = self.current_trait.clone();
        self.current_impl = self_type_name(&item.self_ty);
        self.current_trait = item
            .trait_
            .as_ref()
            .and_then(|(_, path, _)| path.segments.last())
            .map(|segment| segment.ident.to_string());
        if let (Some(owner), Some(trait_name)) = (&self.current_impl, &self.current_trait)
            && TRACKED_CONSTRUCTORS.contains(&owner.as_str())
            && is_forbidden_trait_or_derive(owner, trait_name)
        {
            self.inventory
                .forbidden_traits_or_derives
                .insert(format!("{}::{owner}::{trait_name}", self.file));
        }
        visit::visit_item_impl(self, item);
        self.current_impl = saved_impl;
        self.current_trait = saved_trait;
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if attributes_require_test(&function.attrs) {
            return;
        }
        let saved_impl = self.current_impl.take();
        let saved_trait = self.current_trait.take();
        let saved_function = self
            .current_function
            .replace(format!("{}::{}", self.file, function.sig.ident));
        self.inspect_signature(&function.sig, &function.vis);
        visit::visit_item_fn(self, function);
        self.current_function = saved_function;
        self.current_impl = saved_impl;
        self.current_trait = saved_trait;
    }

    fn visit_impl_item_fn(&mut self, function: &'ast syn::ImplItemFn) {
        if attributes_require_test(&function.attrs) {
            return;
        }
        let owner = self
            .current_impl
            .clone()
            .unwrap_or_else(|| "<unknown-impl>".to_owned());
        let saved_function = self
            .current_function
            .replace(format!("{}::{owner}::{}", self.file, function.sig.ident));
        self.inspect_signature(&function.sig, &function.vis);
        visit::visit_impl_item_fn(self, function);
        self.current_function = saved_function;
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if let syn::Expr::Path(function) = call.func.as_ref() {
            self.record_associated_call(&function.path);
            self.record_direct_call_mint(&function.path);
        }
        self.record_verification_domain_mint_provenance(call);
        visit::visit_expr_call(self, call);
    }

    fn visit_expr_path(&mut self, expression: &'ast syn::ExprPath) {
        self.record_associated_call(&expression.path);
        self.record_direct_call_mint(&expression.path);
        visit::visit_expr_path(self, expression);
    }

    fn visit_expr_struct(&mut self, expression: &'ast syn::ExprStruct) {
        self.record_direct_struct_mint(&expression.path);
        visit::visit_expr_struct(self, expression);
    }

    fn visit_expr_method_call(&mut self, call: &'ast syn::ExprMethodCall) {
        let method = call.method.to_string();
        let route = self.route();
        if matches!(
            method.as_str(),
            "accept_step"
                | "accept_proposal"
                | "scan_accepted_interval"
                | "refresh_retained_indicators"
        ) {
            self.inventory
                .insert_owner_method_call(&method, route.clone());
        }
        if method == "root_scan_shape" {
            self.inventory.root_shape_uses.insert(route.clone());
        }
        if method == "continuous_state_width" {
            self.inventory.linked_width_extractions.insert(route);
        }
        visit::visit_expr_method_call(self, call);
    }
}

fn visibility_name(visibility: &syn::Visibility) -> &'static str {
    match visibility {
        syn::Visibility::Inherited => "private",
        syn::Visibility::Public(_) => "public",
        syn::Visibility::Restricted(_) => "restricted",
    }
}

fn self_type_name(ty: &syn::Type) -> Option<String> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    path.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

fn return_type_names(output: &syn::ReturnType) -> BTreeSet<String> {
    match output {
        syn::ReturnType::Default => BTreeSet::new(),
        syn::ReturnType::Type(_, ty) => TypeNames::in_type(ty),
    }
}

fn returns_protected_capability(signature: &syn::Signature) -> bool {
    let syn::ReturnType::Type(_, ty) = &signature.output else {
        return false;
    };
    let names = TypeNames::in_type(ty);
    if !names
        .iter()
        .any(|name| PROTECTED_AGGREGATES.contains(&name.as_str()))
    {
        return false;
    }
    let carries_capability = names.iter().any(|name| {
        matches!(
            name.as_str(),
            "Cell" | "RefCell" | "RefMut" | "Rc" | "Arc" | "FnMut"
        )
    });
    carries_capability || type_contains_mutable_reference(ty)
}

fn type_contains_mutable_reference(ty: &syn::Type) -> bool {
    struct MutableReference(bool);
    impl<'ast> Visit<'ast> for MutableReference {
        fn visit_type_reference(&mut self, reference: &'ast syn::TypeReference) {
            self.0 |= reference.mutability.is_some();
            visit::visit_type_reference(self, reference);
        }
    }
    let mut visitor = MutableReference(false);
    visitor.visit_type(ty);
    visitor.0
}

fn typed_inputs(signature: &syn::Signature) -> Vec<(BTreeSet<String>, BTreeSet<String>)> {
    signature
        .inputs
        .iter()
        .filter_map(|argument| match argument {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(argument) => Some((
                PatternNames::in_pattern(&argument.pat),
                TypeNames::in_type(&argument.ty),
            )),
        })
        .collect()
}

fn raw_dimension_inputs(signature: &syn::Signature) -> BTreeSet<String> {
    signature
        .inputs
        .iter()
        .enumerate()
        .filter_map(|(index, argument)| match argument {
            syn::FnArg::Receiver(_) => None,
            syn::FnArg::Typed(argument) if is_bare_usize(&argument.ty) => {
                let names = PatternNames::in_pattern(&argument.pat);
                if names.is_empty() {
                    Some(BTreeSet::from([format!("#{index}")]))
                } else {
                    Some(names)
                }
            }
            syn::FnArg::Typed(_) => None,
        })
        .flatten()
        .collect()
}

fn is_bare_usize(ty: &syn::Type) -> bool {
    let syn::Type::Path(path) = ty else {
        return false;
    };
    path.qself.is_none()
        && path.path.segments.last().is_some_and(|segment| {
            segment.ident == "usize" && matches!(&segment.arguments, syn::PathArguments::None)
        })
}

fn associated_pair(segments: &[String]) -> Option<(&str, &str)> {
    let [.., type_name, method] = segments else {
        return None;
    };
    Some((type_name, method))
}

fn tracked_associated_call(call: &str) -> bool {
    matches!(
        call,
        "FmiContinuousStateWidth::new"
            | "MeContinuousStateDomain::from_linked"
            | "MeContinuousStateDomain::from_verification_rates"
            | "MeSolverTolerances::check"
            | "RootScanStateWidth::issue"
            | "RootScanIndicatorWidth::issue"
            | "RootScanShape::issue"
            | "RootScanWorkspace::new"
            | "MeRootSearchState::new"
            | "MeHostComponent::new"
            | "MeRootSearchPolicy::new"
            | "MeNumericalSetup::from_checked_host"
    )
}

fn call_path_ends_with(call: &syn::ExprCall, name: &str) -> bool {
    let syn::Expr::Path(function) = call.func.as_ref() else {
        return false;
    };
    function
        .path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == name)
}

fn is_rates_borrow(expression: &syn::Expr) -> bool {
    let syn::Expr::Reference(reference) = expression else {
        return false;
    };
    matches!(reference.expr.as_ref(), syn::Expr::Path(path) if path.path.is_ident("rates"))
}

fn is_parallel_component_authority(ty: &syn::Type) -> bool {
    let names = TypeNames::in_type(ty);
    names.contains("SolveMeKernel")
        || names.contains("MeRootSearchState")
        || names.contains("MeDerivativeController")
        || names.contains("MeValueRef")
}

fn is_forbidden_trait_or_derive(owner: &str, name: &str) -> bool {
    matches!(name, "Default" | "Serialize" | "Deserialize")
        || matches!(
            owner,
            "MeHostComponent" | "RootScanShape" | "RootScanWorkspace"
        ) && matches!(name, "Clone" | "Copy")
}

fn production_sources() -> Vec<RustSource> {
    let root = workspace_root();
    let mut sources = production_rust_sources(&root.join("crates/rumoca-ir-solve"), &root);
    sources.extend(production_rust_sources(
        &root.join("crates/rumoca-solver"),
        &root,
    ));
    sources
        .into_iter()
        .map(|(path, source)| RustSource { path, source })
        .collect()
}

fn inventory(sources: &[RustSource]) -> Inventory {
    let mut inventory = Inventory::new();
    for source in sources {
        let syntax = syn::parse_file(&source.source)
            .unwrap_or_else(|error| panic!("parse {}: {error}", source.path.display()));
        let mut visitor = BoundaryVisitor {
            file: source.path.display().to_string(),
            current_impl: None,
            current_trait: None,
            current_function: None,
            inventory: &mut inventory,
        };
        visitor.visit_file(&syntax);
    }
    inventory
}

fn expected(items: &[&str]) -> BTreeSet<String> {
    items.iter().map(|item| (*item).to_owned()).collect()
}

fn map_entries(map: &BTreeMap<String, BTreeSet<String>>, key: &str) -> BTreeSet<String> {
    match map.get(key) {
        Some(routes) => routes.clone(),
        None => BTreeSet::new(),
    }
}

fn record_delta(
    actual: &BTreeSet<String>,
    expected: &BTreeSet<String>,
    label: &str,
    violations: &mut BTreeSet<String>,
) {
    for route in actual.difference(expected) {
        violations.insert(format!("unexpected-{label}:{route}"));
    }
    for route in expected.difference(actual) {
        violations.insert(format!("missing-{label}:{route}"));
    }
}

fn validate_constructor_graph(inventory: &Inventory, violations: &mut BTreeSet<String>) {
    for (type_name, constructor) in [
        ("FmiContinuousStateWidth", LINKED_WIDTH_CONSTRUCTOR),
        ("MeSolverTolerances", TOLERANCE_CONSTRUCTOR),
        ("RootScanStateWidth", ROOT_STATE_WIDTH_CONSTRUCTOR),
        ("RootScanIndicatorWidth", ROOT_INDICATOR_WIDTH_CONSTRUCTOR),
        ("RootScanShape", ROOT_SHAPE_CONSTRUCTOR),
        ("RootScanWorkspace", ROOT_WORKSPACE_CONSTRUCTOR),
        ("MeHostComponent", KERNEL_ROOT_OWNER),
        ("MeRootSearchPolicy", ROOT_POLICY_CONSTRUCTOR),
        ("MeNumericalSetup", NUMERICAL_SETUP_CONSTRUCTOR),
    ] {
        record_delta(
            &map_entries(&inventory.constructor_definitions, type_name),
            &expected(&[constructor]),
            &format!("{type_name}-constructor"),
            violations,
        );
        record_delta(
            &map_entries(&inventory.direct_mints, type_name),
            &expected(&[constructor]),
            &format!("{type_name}-direct-mint"),
            violations,
        );
    }
    let domain_constructors =
        expected(&[STATE_DOMAIN_CONSTRUCTOR, VERIFICATION_DOMAIN_CONSTRUCTOR]);
    record_delta(
        &map_entries(
            &inventory.constructor_definitions,
            "MeContinuousStateDomain",
        ),
        &domain_constructors,
        "MeContinuousStateDomain-constructor",
        violations,
    );
    record_delta(
        &map_entries(&inventory.direct_mints, "MeContinuousStateDomain"),
        &domain_constructors,
        "MeContinuousStateDomain-direct-mint",
        violations,
    );
}

fn validate_call_graph(inventory: &Inventory, violations: &mut BTreeSet<String>) {
    for (call, owner) in [
        ("FmiContinuousStateWidth::new", LINKED_WIDTH_ISSUER),
        ("MeContinuousStateDomain::from_linked", STATE_DOMAIN_ISSUER),
        (
            "MeContinuousStateDomain::from_verification_rates",
            VERIFICATION_DOMAIN_MINT,
        ),
        ("RootScanStateWidth::issue", ROOT_SHAPE_ISSUER),
        ("RootScanIndicatorWidth::issue", ROOT_SHAPE_ISSUER),
        ("RootScanShape::issue", ROOT_SHAPE_ISSUER),
        ("RootScanWorkspace::new", ROOT_SEARCH_OWNER),
        ("MeRootSearchState::new", KERNEL_ROOT_OWNER),
        ("MeHostComponent::new", HOST_COMPONENT_ISSUER),
        ("MeRootSearchPolicy::new", ROOT_POLICY_ISSUER),
    ] {
        record_delta(
            &map_entries(&inventory.associated_calls, call),
            &expected(&[owner]),
            call,
            violations,
        );
    }
    record_delta(
        &map_entries(
            &inventory.associated_calls,
            "MeNumericalSetup::from_checked_host",
        ),
        &expected(&[
            HOST_NUMERICAL_SETUP_ISSUER,
            VERIFICATION_NUMERICAL_SETUP_ISSUER,
        ]),
        "MeNumericalSetup::from_checked_host",
        violations,
    );
    record_delta(
        &map_entries(&inventory.associated_calls, "MeSolverTolerances::check"),
        &expected(&[
            OPTIONS_TOLERANCE_ISSUER,
            VERIFICATION_NUMERICAL_SETUP_ISSUER,
        ]),
        "MeSolverTolerances::check",
        violations,
    );
    record_delta(
        &inventory.root_shape_uses,
        &expected(&[ROOT_SEARCH_OWNER]),
        "root-scan-shape-use",
        violations,
    );
    record_delta(
        &inventory.linked_width_extractions,
        &expected(&[STATE_DOMAIN_ISSUER]),
        "linked-width-extraction",
        violations,
    );
    record_delta(
        &inventory.linked_width_inputs,
        &expected(&[STATE_DOMAIN_CONSTRUCTOR]),
        "linked-width-input",
        violations,
    );
    for (method, owners) in [
        ("accept_step", &[COMPONENT_ACCEPT_OWNER][..]),
        (
            "accept_proposal",
            &[SESSION_ACCEPT_OWNER, SESSION_ACCEPT_CALLER][..],
        ),
        (
            "scan_accepted_interval",
            &[COMPONENT_SCAN_OWNER, SESSION_SCAN_OWNER][..],
        ),
        (
            "refresh_retained_indicators",
            &[
                COMPONENT_RETAINED_REFRESH_OWNER,
                SESSION_RETAINED_REFRESH_OWNER,
                SESSION_RETAINED_REFRESH_START,
                SESSION_RETAINED_REFRESH_STEP,
            ][..],
        ),
    ] {
        record_delta(
            &map_entries(&inventory.owner_method_calls, method),
            &expected(owners),
            &format!("{method}-owner"),
            violations,
        );
    }
}

fn validate_surfaces(inventory: &Inventory, violations: &mut BTreeSet<String>) {
    for field in &inventory.raw_dimension_fields {
        violations.insert(format!("raw-dimension-field:{field}"));
    }
    for input in &inventory.raw_dimension_constructor_inputs {
        violations.insert(format!("raw-dimension-constructor-input:{input}"));
    }
    for field in &inventory.exposed_kernel_root_fields {
        violations.insert(format!("exposed-MeHostComponent-field:{field}"));
    }
    for route in &inventory.exposed_component_owner_handles {
        violations.insert(format!("exposed-MeHostComponent-owner-handle:{route}"));
    }
    for route in &inventory.exposed_protected_capabilities {
        violations.insert(format!("exposed-protected-capability:{route}"));
    }
    for field in &inventory.parallel_host_component_fields {
        violations.insert(format!("parallel-MeHostState-component-authority:{field}"));
    }
    record_delta(
        &inventory.verification_domain_fields,
        &expected(&[VERIFICATION_DOMAIN_FIELD]),
        "verification-domain-field",
        violations,
    );
    record_delta(
        &inventory.verification_domain_rates_inputs,
        &expected(&[VERIFICATION_DOMAIN_MINT]),
        "verification-domain-rates-input",
        violations,
    );
    record_delta(
        &inventory.verification_domain_mint_provenance,
        &expected(&[VERIFICATION_DOMAIN_MINT]),
        "verification-domain-mint-provenance",
        violations,
    );
    record_delta(
        &inventory.workspace_new_visibilities,
        &expected(&[&format!("{ROOT_WORKSPACE_CONSTRUCTOR}:private")]),
        "RootScanWorkspace-new-visibility",
        violations,
    );
    for route in &inventory.forbidden_traits_or_derives {
        violations.insert(format!("forbidden-semantic-trait-or-derive:{route}"));
    }
}

fn violations(sources: &[RustSource]) -> BTreeSet<String> {
    let inventory = inventory(sources);
    let mut violations = BTreeSet::new();
    validate_constructor_graph(&inventory, &mut violations);
    validate_call_graph(&inventory, &mut violations);
    validate_surfaces(&inventory, &mut violations);
    violations
}

fn append_mutation(sources: &[RustSource], suffix: &str, addition: &str) -> Vec<RustSource> {
    let mut mutation = sources.to_vec();
    let source = mutation
        .iter_mut()
        .find(|source| source.path.ends_with(suffix))
        .unwrap_or_else(|| panic!("missing mutation owner {suffix}"));
    source.source.push_str(addition);
    mutation
}

fn replace_mutation(
    sources: &[RustSource],
    suffix: &str,
    before: &str,
    after: &str,
) -> Vec<RustSource> {
    let mut mutation = sources.to_vec();
    let source = mutation
        .iter_mut()
        .find(|source| source.path.ends_with(suffix))
        .unwrap_or_else(|| panic!("missing mutation owner {suffix}"));
    assert_eq!(
        source.source.matches(before).count(),
        1,
        "mutation anchor must be unique"
    );
    source.source = source.source.replacen(before, after, 1);
    mutation
}

fn assert_violation(sources: &[RustSource], expected: &str) {
    let found = violations(sources);
    assert!(
        found.contains(expected),
        "mutation must produce {expected:?}; found {found:#?}"
    );
}

#[test]
fn fmi_continuous_state_domain_has_one_typed_construction_graph() {
    let found = violations(&production_sources());
    assert!(
        found.is_empty(),
        "FMI continuous-state-domain boundary violations: {found:#?}"
    );
}

#[test]
fn mutations_detect_alternate_width_and_domain_mints() {
    let sources = production_sources();
    let alternate_width = append_mutation(
        &sources,
        "crates/rumoca-ir-solve/src/fmi/linked_runtime.rs",
        "\nimpl FmiContinuousStateWidth { fn from_count(len: usize) -> Self { Self(len) } }\n",
    );
    assert_violation(
        &alternate_width,
        "unexpected-FmiContinuousStateWidth-constructor:crates/rumoca-ir-solve/src/fmi/linked_runtime.rs::FmiContinuousStateWidth::from_count",
    );

    let alternate_domain = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me.rs",
        "\nimpl MeContinuousStateDomain { fn from_count(len: usize) -> Self { Self(len) } }\n",
    );
    assert_violation(
        &alternate_domain,
        "unexpected-MeContinuousStateDomain-constructor:crates/rumoca-solver/src/fmi_me.rs::MeContinuousStateDomain::from_count",
    );

    let foreign_width_consumer = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me.rs",
        "\nfn retain_linked_width(width: rumoca_ir_solve::fmi::FmiContinuousStateWidth) { drop(width); }\n",
    );
    assert_violation(
        &foreign_width_consumer,
        "unexpected-linked-width-input:crates/rumoca-solver/src/fmi_me.rs::retain_linked_width",
    );
}

#[test]
fn mutations_detect_foreign_width_shape_and_workspace_issuers() {
    let sources = production_sources();
    let foreign_width = append_mutation(
        &sources,
        "crates/rumoca-ir-solve/src/fmi/linked_runtime.rs",
        "\nfn foreign_width(len: usize) -> FmiContinuousStateWidth { FmiContinuousStateWidth::new(len) }\n",
    );
    assert_violation(
        &foreign_width,
        "unexpected-FmiContinuousStateWidth::new:crates/rumoca-ir-solve/src/fmi/linked_runtime.rs::foreign_width",
    );

    let foreign_shape = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/kernel.rs",
        "\nimpl SolveMeKernel { fn foreign_shape(&self) -> RootScanShape { RootScanShape::issue(todo!(), todo!()) } }\n",
    );
    assert_violation(
        &foreign_shape,
        "unexpected-RootScanShape::issue:crates/rumoca-solver/src/fmi_me/kernel.rs::SolveMeKernel::foreign_shape",
    );

    let state_width_from_indicator_count = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/kernel.rs",
        "\nimpl RootScanStateWidth { fn from_indicator_count(n_indicators: usize) -> Self { Self(n_indicators) } }\n",
    );
    assert_violation(
        &state_width_from_indicator_count,
        "unexpected-RootScanStateWidth-constructor:crates/rumoca-solver/src/fmi_me/kernel.rs::RootScanStateWidth::from_indicator_count",
    );

    let foreign_workspace = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/root.rs",
        "\nfn foreign_workspace(kernel: &SolveMeKernel) -> Result<RootScanWorkspace, MeSessionError> { RootScanWorkspace::new(kernel.root_scan_shape()) }\n",
    );
    assert_violation(
        &foreign_workspace,
        "unexpected-RootScanWorkspace::new:crates/rumoca-solver/src/fmi_me/root.rs::foreign_workspace",
    );
    assert_violation(
        &foreign_workspace,
        "unexpected-root-scan-shape-use:crates/rumoca-solver/src/fmi_me/root.rs::foreign_workspace",
    );
}

#[test]
fn mutations_detect_raw_dimensions_and_public_numerical_setup_construction() {
    let sources = production_sources();
    let raw_field = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/root.rs",
        "\nstruct MeRootSearchState { n_states: usize }\n",
    );
    assert_violation(
        &raw_field,
        "raw-dimension-field:crates/rumoca-solver/src/fmi_me/root.rs::MeRootSearchState::n_states",
    );

    let raw_constructor = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/integrator.rs",
        "\nimpl MeContinuousPoint { fn from_count(n_states: usize) -> Self { todo!() } }\n",
    );
    assert_violation(
        &raw_constructor,
        "raw-dimension-constructor-input:crates/rumoca-solver/src/fmi_me/integrator.rs::MeContinuousPoint::from_count:n_states",
    );

    let exposed_workspace = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/root.rs",
        "impl RootScanWorkspace {\n    fn new(",
        "impl RootScanWorkspace {\n    pub(super) fn new(",
    );
    assert_violation(
        &exposed_workspace,
        "unexpected-RootScanWorkspace-new-visibility:crates/rumoca-solver/src/fmi_me/root.rs::RootScanWorkspace::new:restricted",
    );

    let public_setup = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/integrator.rs",
        "\nimpl MeNumericalSetup { pub fn from_parts() -> Self { todo!() } }\n",
    );
    assert_violation(
        &public_setup,
        "unexpected-MeNumericalSetup-constructor:crates/rumoca-solver/src/fmi_me/integrator.rs::MeNumericalSetup::from_parts",
    );

    let setup_default = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/integrator.rs",
        "\nimpl Default for MeNumericalSetup { fn default() -> Self { todo!() } }\n",
    );
    assert_violation(
        &setup_default,
        "forbidden-semantic-trait-or-derive:crates/rumoca-solver/src/fmi_me/integrator.rs::MeNumericalSetup::Default",
    );

    let setup_serde_derives = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/integrator.rs",
        "#[derive(Debug, Clone)]\npub struct MeNumericalSetup",
        "#[derive(Debug, Clone, Serialize, Deserialize)]\npub struct MeNumericalSetup",
    );
    assert_violation(
        &setup_serde_derives,
        "forbidden-semantic-trait-or-derive:crates/rumoca-solver/src/fmi_me/integrator.rs::MeNumericalSetup::Serialize",
    );
    assert_violation(
        &setup_serde_derives,
        "forbidden-semantic-trait-or-derive:crates/rumoca-solver/src/fmi_me/integrator.rs::MeNumericalSetup::Deserialize",
    );
}

#[test]
fn mutations_detect_split_kernel_root_ownership() {
    let sources = production_sources();
    let exposed_field = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs",
        "pub(in crate::fmi_me::session) struct MeHostComponent {\n    kernel: Rc<RefCell<SolveMeKernel>>,",
        "pub(in crate::fmi_me::session) struct MeHostComponent {\n    pub(super) kernel: Rc<RefCell<SolveMeKernel>>,",
    );
    assert_violation(
        &exposed_field,
        "exposed-MeHostComponent-field:crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::kernel:restricted",
    );

    let owner_handle = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs",
        "pub(in crate::fmi_me::session) fn kernel(&self) -> &RefCell<SolveMeKernel> {\n        self.kernel.as_ref()",
        "pub(in crate::fmi_me::session) fn kernel(&self) -> &Rc<RefCell<SolveMeKernel>> {\n        &self.kernel",
    );
    assert_violation(
        &owner_handle,
        "exposed-MeHostComponent-owner-handle:crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::kernel",
    );

    let mutable_root_search = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs",
        "\nimpl MeHostComponent { fn root_search_mut(&mut self) -> &mut MeRootSearchState { &mut self.root_search } }\n",
    );
    assert_violation(
        &mutable_root_search,
        "exposed-protected-capability:crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::root_search_mut",
    );

    let scan_workspace = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/root.rs",
        "\nimpl MeRootSearchState { fn scan_workspace(&self) -> std::cell::RefMut<'_, RootScanWorkspace> { self.workspace.borrow_mut() } }\n",
    );
    assert_violation(
        &scan_workspace,
        "exposed-protected-capability:crates/rumoca-solver/src/fmi_me/root.rs::MeRootSearchState::scan_workspace",
    );

    let crossed_components = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs",
        "\nimpl MeHostComponent { fn from_crossed(left: Self, right: Self) -> Self { Self { kernel: left.kernel, derivatives: left.derivatives, root_search: right.root_search, max_step_duration_reference: right.max_step_duration_reference } } }\n",
    );
    assert_violation(
        &crossed_components,
        "unexpected-MeHostComponent-constructor:crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::from_crossed",
    );
    assert_violation(
        &crossed_components,
        "unexpected-MeHostComponent-direct-mint:crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::MeHostComponent::from_crossed",
    );

    let foreign_root_search = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session/host_state/component.rs",
        "\nfn foreign_root_search(kernel: &SolveMeKernel) -> Result<MeRootSearchState, MeSessionError> { MeRootSearchState::new(kernel) }\n",
    );
    assert_violation(
        &foreign_root_search,
        "unexpected-MeRootSearchState::new:crates/rumoca-solver/src/fmi_me/session/host_state/component.rs::foreign_root_search",
    );

    let foreign_scan_call = append_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session.rs",
        "\nfn cross_pair_scan(root: &MeRootSearchState, target: &mut impl RootScanTarget, step: &MeAcceptedStep) -> Result<Option<MeRootApplication>, MeSessionError> { root.scan_accepted_interval(target, step) }\n",
    );
    assert_violation(
        &foreign_scan_call,
        "unexpected-scan_accepted_interval-owner:crates/rumoca-solver/src/fmi_me/session.rs::cross_pair_scan",
    );

    let parallel_kernel = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/session/host_state.rs",
        "pub(super) struct MeHostState {",
        "pub(super) struct MeHostState {\n    foreign_kernel: Rc<RefCell<SolveMeKernel>>,",
    );
    assert_violation(
        &parallel_kernel,
        "parallel-MeHostState-component-authority:crates/rumoca-solver/src/fmi_me/session/host_state.rs::MeHostState::foreign_kernel",
    );
}

#[test]
fn mutations_detect_untyped_verification_domain_construction() {
    let sources = production_sources();
    let unrelated_count = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/backend_test_support.rs",
        "super::MeContinuousStateDomain::from_verification_rates(&rates)",
        "super::MeContinuousStateDomain::from_verification_rates(&[])",
    );
    assert_violation(
        &unrelated_count,
        "missing-verification-domain-mint-provenance:crates/rumoca-solver/src/fmi_me/backend_test_support.rs::MeBackendProbe::linear",
    );

    let parallel_count = replace_mutation(
        &sources,
        "crates/rumoca-solver/src/fmi_me/backend_test_support.rs",
        "    state_domain: super::MeContinuousStateDomain,",
        "    n_states: usize,",
    );
    assert_violation(
        &parallel_count,
        "raw-dimension-field:crates/rumoca-solver/src/fmi_me/backend_test_support.rs::MeBackendProbe::n_states",
    );
    assert_violation(
        &parallel_count,
        "missing-verification-domain-field:crates/rumoca-solver/src/fmi_me/backend_test_support.rs::MeBackendProbe::state_domain",
    );
}
