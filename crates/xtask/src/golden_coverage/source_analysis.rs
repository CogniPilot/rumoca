use std::collections::BTreeSet;
use std::fs;
use std::ops::RangeInclusive;
use std::path::{Component, Path};

use anyhow::{Context as _, Result};
use proc_macro2::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::visit::Visit as _;
use syn::{Meta, Token};

#[cfg(test)]
use super::schema::{RustItemId, RustItemKind, RustItemScope, RustModuleId};

const NON_PRODUCT_CRATE_DIRS: [&str; 2] = ["rumoca-test-msl", "xtask"];

#[derive(Default)]
pub(crate) struct SourceAnalysis {
    pub(crate) test_lines: BTreeSet<u32>,
    pub(crate) macro_lines: BTreeSet<u32>,
    pub(crate) generic_lines: BTreeSet<u32>,
}

pub(crate) fn analyze_source(path: &Path) -> Result<SourceAnalysis> {
    let source = fs::read_to_string(path)
        .with_context(|| format!("failed to read production source {}", path.display()))?;
    let syntax = syn::parse_file(&source)
        .with_context(|| format!("failed to parse production source {}", path.display()))?;
    let mut collector = RangeCollector::default();
    collector.visit_file(&syntax);
    Ok(SourceAnalysis {
        test_lines: expand_ranges(collector.test_ranges),
        macro_lines: expand_ranges(collector.macro_ranges),
        generic_lines: expand_ranges(collector.generic_ranges),
    })
}

pub(crate) fn production_relative_path(root: &Path, path: &Path) -> Option<String> {
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        root.join(path)
    };
    let relative = absolute.strip_prefix(root).ok()?;
    let components: Vec<_> = relative.components().collect();
    let [
        Component::Normal(crates),
        Component::Normal(package),
        Component::Normal(src),
        rest @ ..,
    ] = components.as_slice()
    else {
        return None;
    };
    if *crates != "crates"
        || *src != "src"
        || NON_PRODUCT_CRATE_DIRS.contains(&package.to_string_lossy().as_ref())
        || rest.is_empty()
        || relative.extension().and_then(|value| value.to_str()) != Some("rs")
        || is_test_source(relative)
    {
        return None;
    }
    Some(relative.to_string_lossy().replace('\\', "/"))
}

#[cfg(test)]
pub(super) fn validate_rust_item_capture(
    root: &Path,
    relative: &str,
    item: &RustItemId,
    definition_line: u32,
) -> Result<()> {
    let catalog = rust_item_catalog(root, relative)?;
    let matches: Vec<_> = catalog.iter().filter(|entry| entry.item == *item).collect();
    anyhow::ensure!(
        matches.len() == 1,
        "typed Rust item identity resolves to {} source items instead of exactly one",
        matches.len()
    );
    anyhow::ensure!(
        matches[0].definition_line == definition_line,
        "typed Rust item has foreign definition-line evidence"
    );
    Ok(())
}

#[cfg(test)]
pub(super) fn resolve_rust_item_capture(
    root: &Path,
    relative: &str,
    definition_line: u32,
) -> Result<RustItemId> {
    let catalog = rust_item_catalog(root, relative)?;
    let matches: Vec<_> = catalog
        .iter()
        .filter(|entry| entry.definition_line == definition_line)
        .collect();
    anyhow::ensure!(
        matches.len() == 1,
        "source line resolves to {} uniquely catalogued Rust items instead of one",
        matches.len()
    );
    Ok(matches[0].item.clone())
}

#[cfg(test)]
struct CatalogItem {
    item: RustItemId,
    definition_line: u32,
}

#[cfg(test)]
fn rust_item_catalog(root: &Path, relative: &str) -> Result<Vec<CatalogItem>> {
    let relative_path = Path::new(relative);
    let (package_dir, file_modules) = rust_source_identity(relative_path)?;
    let manifest = fs::read_to_string(root.join("crates").join(package_dir).join("Cargo.toml"))?;
    let package = parse_package_name(&manifest)?;
    let source = fs::read_to_string(root.join(relative_path))?;
    let syntax = syn::parse_file(&source)?;
    let mut catalog = Vec::new();
    collect_items_at_line(&syntax.items, &package, &file_modules, &mut catalog);
    let mut identities = BTreeSet::new();
    for entry in &catalog {
        anyhow::ensure!(
            identities.insert(&entry.item),
            "source contains a duplicate typed Rust item identity"
        );
    }
    Ok(catalog)
}

#[cfg(test)]
fn rust_source_identity(path: &Path) -> Result<(String, Vec<String>)> {
    let components: Vec<_> = path.components().collect();
    let [
        Component::Normal(crates),
        Component::Normal(package),
        Component::Normal(src),
        rest @ ..,
    ] = components.as_slice()
    else {
        anyhow::bail!("Rust item source is outside a crate src directory");
    };
    anyhow::ensure!(
        *crates == "crates" && *src == "src",
        "invalid Rust item source path"
    );
    let mut modules: Vec<String> = rest
        .iter()
        .filter_map(|component| match component {
            Component::Normal(value) => value.to_str().map(str::to_owned),
            _ => None,
        })
        .collect();
    let file = modules.pop().context("Rust item source has no file")?;
    let stem = file
        .strip_suffix(".rs")
        .context("Rust item source is not Rust")?;
    if !matches!(stem, "lib" | "main" | "mod") {
        modules.push(stem.to_string());
    }
    Ok((package.to_string_lossy().into_owned(), modules))
}

#[cfg(test)]
#[derive(serde::Deserialize)]
struct CargoManifestIdentity {
    package: CargoPackageIdentity,
}

#[cfg(test)]
#[derive(serde::Deserialize)]
struct CargoPackageIdentity {
    name: String,
}

#[cfg(test)]
fn parse_package_name(source: &str) -> Result<String> {
    Ok(toml::from_str::<CargoManifestIdentity>(source)?
        .package
        .name)
}

#[cfg(test)]
fn collect_items_at_line(
    items: &[syn::Item],
    package: &str,
    modules: &[String],
    catalog: &mut Vec<CatalogItem>,
) {
    for item in items {
        if item_attributes(item).is_some_and(has_test_attribute) {
            continue;
        }
        match item {
            syn::Item::Fn(function) => {
                if !function.sig.generics.params.is_empty() {
                    continue;
                }
                catalog.push(CatalogItem {
                    item: RustItemId {
                        package: package.to_string(),
                        module: module_id(modules),
                        scope: RustItemScope::Module,
                        kind: RustItemKind::Function,
                        item_name: function.sig.ident.to_string(),
                    },
                    definition_line: function.span().start().line as u32,
                });
            }
            syn::Item::Impl(item) => collect_impl_items(item, package, modules, catalog),
            syn::Item::Mod(module) => {
                if let Some((_, nested)) = &module.content {
                    let mut nested_modules = modules.to_vec();
                    nested_modules.push(module.ident.to_string());
                    collect_items_at_line(nested, package, &nested_modules, catalog);
                }
            }
            _ => {}
        }
    }
}

#[cfg(test)]
fn collect_impl_items(
    item: &syn::ItemImpl,
    package: &str,
    modules: &[String],
    catalog: &mut Vec<CatalogItem>,
) {
    if !item.generics.params.is_empty() {
        return;
    }
    let Some(type_path) = simple_type_path(&item.self_ty) else {
        return;
    };
    let scope = match &item.trait_ {
        Some((_, trait_path, _)) => {
            let Some(trait_path) = simple_path(trait_path, true) else {
                return;
            };
            RustItemScope::TraitImpl {
                trait_path,
                type_path,
            }
        }
        None => RustItemScope::Impl { type_path },
    };
    for member in &item.items {
        let syn::ImplItem::Fn(function) = member else {
            continue;
        };
        if has_test_attribute(&function.attrs) || !function.sig.generics.params.is_empty() {
            continue;
        }
        catalog.push(CatalogItem {
            item: RustItemId {
                package: package.to_string(),
                module: module_id(modules),
                scope: scope.clone(),
                kind: RustItemKind::Method,
                item_name: function.sig.ident.to_string(),
            },
            definition_line: function.span().start().line as u32,
        });
    }
}

#[cfg(test)]
fn module_id(modules: &[String]) -> RustModuleId {
    if modules.is_empty() {
        RustModuleId::CrateRoot
    } else {
        RustModuleId::Path(modules.to_vec())
    }
}

#[cfg(test)]
fn simple_type_path(value: &syn::Type) -> Option<Vec<String>> {
    let syn::Type::Path(path) = value else {
        return None;
    };
    simple_path(&path.path, path.qself.is_none())
}

#[cfg(test)]
fn simple_path(path: &syn::Path, no_qself: bool) -> Option<Vec<String>> {
    if !no_qself
        || path.segments.is_empty()
        || path
            .segments
            .iter()
            .any(|segment| !matches!(segment.arguments, syn::PathArguments::None))
    {
        return None;
    }
    Some(
        path.segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect(),
    )
}

#[derive(Default)]
struct RangeCollector {
    test_ranges: Vec<RangeInclusive<u32>>,
    macro_ranges: Vec<RangeInclusive<u32>>,
    generic_ranges: Vec<RangeInclusive<u32>>,
}

impl RangeCollector {
    fn is_test_only(&mut self, attributes: &[syn::Attribute], span: Span) -> bool {
        if has_test_attribute(attributes) {
            self.test_ranges.push(span_lines(span));
            true
        } else {
            false
        }
    }
}

impl<'ast> syn::visit::Visit<'ast> for RangeCollector {
    fn visit_item(&mut self, node: &'ast syn::Item) {
        if !item_attributes(node).is_some_and(|attrs| self.is_test_only(attrs, node.span())) {
            syn::visit::visit_item(self, node);
        }
    }

    fn visit_impl_item(&mut self, node: &'ast syn::ImplItem) {
        if !impl_item_attributes(node).is_some_and(|attrs| self.is_test_only(attrs, node.span())) {
            syn::visit::visit_impl_item(self, node);
        }
    }

    fn visit_trait_item(&mut self, node: &'ast syn::TraitItem) {
        if !trait_item_attributes(node).is_some_and(|attrs| self.is_test_only(attrs, node.span())) {
            syn::visit::visit_trait_item(self, node);
        }
    }

    fn visit_foreign_item(&mut self, node: &'ast syn::ForeignItem) {
        if !foreign_item_attributes(node).is_some_and(|attrs| self.is_test_only(attrs, node.span()))
        {
            syn::visit::visit_foreign_item(self, node);
        }
    }

    fn visit_field(&mut self, node: &'ast syn::Field) {
        if !self.is_test_only(&node.attrs, node.span()) {
            syn::visit::visit_field(self, node);
        }
    }

    fn visit_variant(&mut self, node: &'ast syn::Variant) {
        if !self.is_test_only(&node.attrs, node.span()) {
            syn::visit::visit_variant(self, node);
        }
    }

    fn visit_item_macro(&mut self, node: &'ast syn::ItemMacro) {
        self.macro_ranges.push(span_lines(node.span()));
        syn::visit::visit_item_macro(self, node);
    }

    fn visit_expr_macro(&mut self, node: &'ast syn::ExprMacro) {
        self.macro_ranges.push(span_lines(node.span()));
        syn::visit::visit_expr_macro(self, node);
    }

    fn visit_stmt_macro(&mut self, node: &'ast syn::StmtMacro) {
        self.macro_ranges.push(span_lines(node.span()));
        syn::visit::visit_stmt_macro(self, node);
    }

    fn visit_item_fn(&mut self, node: &'ast syn::ItemFn) {
        if !node.sig.generics.params.is_empty() {
            self.generic_ranges.push(span_lines(node.span()));
        }
        syn::visit::visit_item_fn(self, node);
    }

    fn visit_item_impl(&mut self, node: &'ast syn::ItemImpl) {
        if !node.generics.params.is_empty() {
            self.generic_ranges.push(span_lines(node.span()));
        }
        syn::visit::visit_item_impl(self, node);
    }

    fn visit_impl_item_fn(&mut self, node: &'ast syn::ImplItemFn) {
        if !node.sig.generics.params.is_empty() {
            self.generic_ranges.push(span_lines(node.span()));
        }
        syn::visit::visit_impl_item_fn(self, node);
    }

    fn visit_trait_item_fn(&mut self, node: &'ast syn::TraitItemFn) {
        if !node.sig.generics.params.is_empty() {
            self.generic_ranges.push(span_lines(node.span()));
        }
        syn::visit::visit_trait_item_fn(self, node);
    }
}

fn item_attributes(item: &syn::Item) -> Option<&[syn::Attribute]> {
    match item {
        syn::Item::Const(item) => Some(&item.attrs),
        syn::Item::Enum(item) => Some(&item.attrs),
        syn::Item::ExternCrate(item) => Some(&item.attrs),
        syn::Item::Fn(item) => Some(&item.attrs),
        syn::Item::ForeignMod(item) => Some(&item.attrs),
        syn::Item::Impl(item) => Some(&item.attrs),
        syn::Item::Macro(item) => Some(&item.attrs),
        syn::Item::Mod(item) => Some(&item.attrs),
        syn::Item::Static(item) => Some(&item.attrs),
        syn::Item::Struct(item) => Some(&item.attrs),
        syn::Item::Trait(item) => Some(&item.attrs),
        syn::Item::TraitAlias(item) => Some(&item.attrs),
        syn::Item::Type(item) => Some(&item.attrs),
        syn::Item::Union(item) => Some(&item.attrs),
        syn::Item::Use(item) => Some(&item.attrs),
        syn::Item::Verbatim(_) => None,
        _ => None,
    }
}

fn impl_item_attributes(item: &syn::ImplItem) -> Option<&[syn::Attribute]> {
    match item {
        syn::ImplItem::Const(item) => Some(&item.attrs),
        syn::ImplItem::Fn(item) => Some(&item.attrs),
        syn::ImplItem::Type(item) => Some(&item.attrs),
        syn::ImplItem::Macro(item) => Some(&item.attrs),
        syn::ImplItem::Verbatim(_) => None,
        _ => None,
    }
}

fn trait_item_attributes(item: &syn::TraitItem) -> Option<&[syn::Attribute]> {
    match item {
        syn::TraitItem::Const(item) => Some(&item.attrs),
        syn::TraitItem::Fn(item) => Some(&item.attrs),
        syn::TraitItem::Type(item) => Some(&item.attrs),
        syn::TraitItem::Macro(item) => Some(&item.attrs),
        syn::TraitItem::Verbatim(_) => None,
        _ => None,
    }
}

fn foreign_item_attributes(item: &syn::ForeignItem) -> Option<&[syn::Attribute]> {
    match item {
        syn::ForeignItem::Fn(item) => Some(&item.attrs),
        syn::ForeignItem::Static(item) => Some(&item.attrs),
        syn::ForeignItem::Type(item) => Some(&item.attrs),
        syn::ForeignItem::Macro(item) => Some(&item.attrs),
        syn::ForeignItem::Verbatim(_) => None,
        _ => None,
    }
}

fn has_test_attribute(attributes: &[syn::Attribute]) -> bool {
    attributes.iter().any(|attribute| {
        attribute.path().is_ident("test")
            || match &attribute.meta {
                Meta::List(list) if list.path.is_ident("cfg") => list
                    .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
                    .is_ok_and(|terms| terms.iter().any(cfg_requires_test)),
                _ => false,
            }
    })
}

fn cfg_requires_test(meta: &Meta) -> bool {
    match meta {
        Meta::Path(path) => path.is_ident("test"),
        Meta::List(list) if list.path.is_ident("all") => list
            .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
            .is_ok_and(|terms| terms.iter().any(cfg_requires_test)),
        Meta::List(list) if list.path.is_ident("any") => list
            .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)
            .is_ok_and(|terms| !terms.is_empty() && terms.iter().all(cfg_requires_test)),
        Meta::List(_) | Meta::NameValue(_) => false,
    }
}

fn span_lines(span: Span) -> RangeInclusive<u32> {
    span.start().line as u32..=span.end().line as u32
}

fn expand_ranges(ranges: Vec<RangeInclusive<u32>>) -> BTreeSet<u32> {
    ranges.into_iter().flatten().collect()
}

fn is_test_source(path: &Path) -> bool {
    path.components()
        .any(|component| component.as_os_str() == "tests")
        || matches!(
            path.file_name().and_then(|name| name.to_str()),
            Some("tests.rs" | "test.rs" | "test_support.rs" | "test_utils.rs")
        )
}
