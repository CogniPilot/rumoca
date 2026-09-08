//! Type, syntax, and source-analysis helpers for the Rust boundary gate.

use super::super::super::super::architecture_hardening_support::attributes_require_test;
use std::collections::{BTreeMap, BTreeSet};
use syn::visit::{self, Visit};

#[derive(Clone)]
struct ImportBinding {
    defining_module: Vec<String>,
    source: Vec<String>,
}

#[derive(Clone)]
struct SemanticDeclaration {
    kind: DeclarationKind,
    module: Vec<String>,
    name: String,
    fields: Vec<syn::Type>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum DeclarationKind {
    Aggregate,
    Alias,
}

/// Qualified semantic type identities for the complete production module set.
///
/// Bare names are never shared between modules. Imports resolve to their
/// declaration or namespace identity before carrier status is consulted, so an
/// out-of-line impl sees the carrier it imported without tainting an unrelated
/// sibling declaration with the same spelling.
pub(super) struct SemanticTypeIndex {
    external_crates: BTreeMap<(String, String), String>,
    declarations: BTreeMap<String, SemanticDeclaration>,
    globs: BTreeMap<String, Vec<ImportBinding>>,
    imports: BTreeMap<(String, String), ImportBinding>,
    modules: BTreeSet<String>,
    owned: BTreeSet<String>,
    semantic: BTreeSet<String>,
    text: BTreeSet<String>,
}

pub(super) struct ParsedRustSource {
    pub(super) external_crates: BTreeMap<String, String>,
    pub(super) file: String,
    pub(super) module: Vec<String>,
    pub(super) syntax: syn::File,
}

impl SemanticTypeIndex {
    pub(super) fn new(sources: &[ParsedRustSource]) -> Self {
        let mut index = Self {
            external_crates: BTreeMap::new(),
            declarations: BTreeMap::new(),
            globs: BTreeMap::new(),
            imports: BTreeMap::new(),
            modules: BTreeSet::from(["crate".to_string()]),
            owned: BTreeSet::new(),
            semantic: BTreeSet::new(),
            text: BTreeSet::new(),
        };
        for source in sources {
            index.insert_module_and_parents(&source.module);
            let target = source
                .module
                .first()
                .expect("analyzed source has a target identity")
                .clone();
            for (alias, canonical) in &source.external_crates {
                index
                    .external_crates
                    .insert((target.clone(), alias.clone()), canonical.clone());
            }
            index.collect_items(&source.syntax.items, &source.module);
        }
        index.seed_named_semantic_declarations();
        index.propagate_semantic_declarations();
        index
    }

    pub(super) fn signature_has_semantic_typed_input(
        &self,
        module: &[String],
        signature: &syn::Signature,
    ) -> bool {
        signature.inputs.iter().any(|argument| {
            matches!(argument, syn::FnArg::Typed(argument)
                if self.type_mentions_semantic(module, &argument.ty))
        })
    }

    pub(super) fn pattern_mentions_semantic_type(
        &self,
        module: &[String],
        pattern: &syn::Pat,
    ) -> bool {
        match pattern {
            syn::Pat::Type(typed) => self.type_mentions_semantic(module, &typed.ty),
            syn::Pat::Reference(reference) => {
                self.pattern_mentions_semantic_type(module, &reference.pat)
            }
            syn::Pat::Tuple(tuple) => tuple
                .elems
                .iter()
                .any(|pattern| self.pattern_mentions_semantic_type(module, pattern)),
            _ => false,
        }
    }

    pub(super) fn type_mentions_semantic(&self, module: &[String], ty: &syn::Type) -> bool {
        struct SemanticVisitor<'a> {
            found: bool,
            index: &'a SemanticTypeIndex,
            module: &'a [String],
        }

        impl<'ast> Visit<'ast> for SemanticVisitor<'_> {
            fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
                self.found |= self.index.path_is_semantic(self.module, &path.path);
                visit::visit_type_path(self, path);
            }
        }

        let mut visitor = SemanticVisitor {
            found: false,
            index: self,
            module,
        };
        visitor.visit_type(ty);
        visitor.found
    }

    pub(super) fn type_contains_owned_semantic_ir(
        &self,
        module: &[String],
        ty: &syn::Type,
    ) -> bool {
        struct OwnedVisitor<'a> {
            found: bool,
            index: &'a SemanticTypeIndex,
            module: &'a [String],
        }

        impl<'ast> Visit<'ast> for OwnedVisitor<'_> {
            fn visit_type_reference(&mut self, _reference: &'ast syn::TypeReference) {
                // A reference, including one nested in an alias, is not an
                // owned semantic result.
            }

            fn visit_type_path(&mut self, path: &'ast syn::TypePath) {
                self.found |= self.index.path_is_owned(self.module, &path.path);
                visit::visit_type_path(self, path);
            }
        }

        let mut visitor = OwnedVisitor {
            found: false,
            index: self,
            module,
        };
        visitor.visit_type(ty);
        visitor.found
    }

    pub(super) fn type_contains_text(&self, module: &[String], ty: &syn::Type) -> bool {
        struct TextVisitor<'a> {
            found: bool,
            index: &'a SemanticTypeIndex,
            module: &'a [String],
        }

        impl<'ast> Visit<'ast> for TextVisitor<'_> {
            fn visit_type(&mut self, ty: &'ast syn::Type) {
                self.found |= self.index.type_is_text_carrier(self.module, ty);
                visit::visit_type(self, ty);
            }
        }

        let mut visitor = TextVisitor {
            found: false,
            index: self,
            module,
        };
        visitor.visit_type(ty);
        visitor.found
    }

    pub(super) fn path_is_text(&self, module: &[String], path: &syn::Path) -> bool {
        let resolved = self.resolve_path(module, path);
        self.resolved_path_is_text(&resolved)
    }

    pub(super) fn expression_path_text_sink(
        &self,
        module: &[String],
        path: &syn::ExprPath,
    ) -> Option<String> {
        let name = path.path.segments.last()?.ident.to_string();
        if !matches!(
            name.as_str(),
            "add"
                | "add_assign"
                | "extend"
                | "extend_from_slice"
                | "insert"
                | "insert_str"
                | "push"
                | "push_str"
                | "replace_range"
                | "write_all"
                | "write_char"
                | "write_fmt"
                | "write_str"
        ) {
            return None;
        }
        let mut owner = path.path.clone();
        owner.segments.pop();
        let path_owner_is_text = !owner.segments.is_empty()
            && (self.path_is_text(module, &owner)
                || self.resolved_path_is_text_sink_owner(&self.resolve_path(module, &owner)));
        let qself_owner_is_text = path.qself.as_ref().is_some_and(|qself| {
            self.type_is_text_carrier(module, &qself.ty)
                || matches!(qself.ty.as_ref(), syn::Type::Path(owner)
                if self.resolved_path_is_text_sink_owner(
                    &self.resolve_path(module, &owner.path)
                ))
        });
        (path_owner_is_text || qself_owner_is_text).then_some(name)
    }

    pub(super) fn semantic_field_requires_review(&self, module: &[String], ty: &syn::Type) -> bool {
        if !self.type_mentions_semantic(module, ty) {
            return false;
        }
        !matches!(ty, syn::Type::Reference(reference)
            if reference.mutability.is_none()
                && self.type_is_direct_semantic(module, &reference.elem))
    }

    pub(super) fn semantic_import_aliases(
        &self,
        module: &[String],
        tree: &syn::UseTree,
    ) -> Vec<String> {
        let mut imports = Vec::new();
        flatten_use_tree(tree, &mut Vec::new(), &mut imports, &mut Vec::new());
        imports
            .into_iter()
            .filter(|import| import.renamed && self.segments_are_semantic(module, &import.source))
            .map(|import| format!("{} as {}", import.source.join("::"), import.local))
            .collect()
    }

    fn collect_items(&mut self, items: &[syn::Item], module: &[String]) {
        for item in items {
            if attributes_require_test(item_attributes(item)) {
                continue;
            }
            match item {
                syn::Item::Type(item) => self.insert_declaration(
                    module,
                    &item.ident.to_string(),
                    DeclarationKind::Alias,
                    vec![item.ty.as_ref().clone()],
                ),
                syn::Item::Struct(item) => self.insert_declaration(
                    module,
                    &item.ident.to_string(),
                    DeclarationKind::Aggregate,
                    item.fields.iter().map(|field| field.ty.clone()).collect(),
                ),
                syn::Item::Enum(item) => self.insert_declaration(
                    module,
                    &item.ident.to_string(),
                    DeclarationKind::Aggregate,
                    item.variants
                        .iter()
                        .flat_map(|variant| variant.fields.iter())
                        .map(|field| field.ty.clone())
                        .collect(),
                ),
                syn::Item::Union(item) => self.insert_declaration(
                    module,
                    &item.ident.to_string(),
                    DeclarationKind::Aggregate,
                    item.fields
                        .named
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect(),
                ),
                syn::Item::Use(item) => self.insert_imports(module, &item.tree),
                syn::Item::ExternCrate(item) => {
                    self.insert_extern_crate(module, item);
                }
                syn::Item::Mod(item) => self.collect_module_item(module, item),
                _ => {}
            }
        }
    }

    fn collect_module_item(&mut self, module: &[String], item: &syn::ItemMod) {
        let mut nested_module = module.to_vec();
        nested_module.push(item.ident.to_string());
        self.insert_module_and_parents(&nested_module);
        if let Some((_, nested)) = &item.content {
            self.collect_items(nested, &nested_module);
        }
    }

    fn insert_declaration(
        &mut self,
        module: &[String],
        name: &str,
        kind: DeclarationKind,
        fields: Vec<syn::Type>,
    ) {
        let identity = qualified_name(module, name);
        self.declarations.insert(
            identity,
            SemanticDeclaration {
                kind,
                module: module.to_vec(),
                name: name.to_string(),
                fields,
            },
        );
    }

    fn insert_extern_crate(&mut self, module: &[String], item: &syn::ItemExternCrate) {
        let source = item.ident.to_string();
        let local = item
            .rename
            .as_ref()
            .map_or_else(|| source.clone(), |(_, rename)| rename.to_string());
        self.imports.insert(
            (module_key(module), local),
            ImportBinding {
                defining_module: module.to_vec(),
                source: vec!["<extern>".to_string(), source],
            },
        );
    }

    fn insert_imports(&mut self, module: &[String], tree: &syn::UseTree) {
        let mut imports = Vec::new();
        let mut globs = Vec::new();
        flatten_use_tree(tree, &mut Vec::new(), &mut imports, &mut globs);
        let module_key = module_key(module);
        for import in imports {
            self.imports.insert(
                (module_key.clone(), import.local),
                ImportBinding {
                    defining_module: module.to_vec(),
                    source: import.source,
                },
            );
        }
        self.globs
            .entry(module_key)
            .or_default()
            .extend(globs.into_iter().map(|source| ImportBinding {
                defining_module: module.to_vec(),
                source,
            }));
    }

    fn insert_module_and_parents(&mut self, module: &[String]) {
        for length in 0..=module.len() {
            self.modules.insert(module_key(&module[..length]));
        }
    }

    fn seed_named_semantic_declarations(&mut self) {
        for (identity, declaration) in &self.declarations {
            if is_semantic_type_name(&declaration.name) {
                self.semantic.insert(identity.clone());
            }
            if is_owned_semantic_ir_name(&declaration.name) {
                self.owned.insert(identity.clone());
            }
        }
    }

    fn propagate_semantic_declarations(&mut self) {
        loop {
            let mut semantic_additions = Vec::new();
            let mut owned_additions = Vec::new();
            let mut text_additions = Vec::new();
            for (identity, declaration) in &self.declarations {
                self.collect_declaration_additions(
                    identity,
                    declaration,
                    &mut semantic_additions,
                    &mut owned_additions,
                    &mut text_additions,
                );
            }
            if semantic_additions.is_empty()
                && owned_additions.is_empty()
                && text_additions.is_empty()
            {
                return;
            }
            self.semantic.extend(semantic_additions);
            self.owned.extend(owned_additions);
            self.text.extend(text_additions);
        }
    }

    fn collect_declaration_additions(
        &self,
        identity: &str,
        declaration: &SemanticDeclaration,
        semantic: &mut Vec<String>,
        owned: &mut Vec<String>,
        text: &mut Vec<String>,
    ) {
        if !self.semantic.contains(identity) && self.declaration_mentions_semantic(declaration) {
            semantic.push(identity.to_string());
        }
        if !self.owned.contains(identity)
            && declaration.kind == DeclarationKind::Alias
            && self.declaration_contains_owned_semantic(declaration)
        {
            owned.push(identity.to_string());
        }
        if !self.text.contains(identity)
            && declaration.kind == DeclarationKind::Alias
            && self.declaration_contains_text(declaration)
        {
            text.push(identity.to_string());
        }
    }

    fn declaration_mentions_semantic(&self, declaration: &SemanticDeclaration) -> bool {
        declaration
            .fields
            .iter()
            .any(|field| self.type_mentions_semantic(&declaration.module, field))
    }

    fn declaration_contains_owned_semantic(&self, declaration: &SemanticDeclaration) -> bool {
        declaration
            .fields
            .iter()
            .any(|field| self.type_contains_owned_semantic_ir(&declaration.module, field))
    }

    fn declaration_contains_text(&self, declaration: &SemanticDeclaration) -> bool {
        declaration
            .fields
            .iter()
            .any(|field| self.type_contains_text(&declaration.module, field))
    }

    fn type_is_text_carrier(&self, module: &[String], ty: &syn::Type) -> bool {
        match ty {
            syn::Type::Path(path) => {
                self.path_is_text(module, &path.path)
                    || path.path.segments.last().is_some_and(vec_segment_is_bytes)
            }
            syn::Type::Reference(reference) => self.type_is_text_carrier(module, &reference.elem),
            syn::Type::Slice(slice) => {
                matches!(slice.elem.as_ref(), syn::Type::Path(byte) if byte.path.is_ident("u8"))
            }
            syn::Type::Array(array) => {
                matches!(array.elem.as_ref(), syn::Type::Path(byte) if byte.path.is_ident("u8"))
            }
            syn::Type::Group(group) => self.type_is_text_carrier(module, &group.elem),
            syn::Type::Paren(paren) => self.type_is_text_carrier(module, &paren.elem),
            _ => false,
        }
    }

    fn resolved_path_is_text(&self, resolved: &[String]) -> bool {
        let identity = resolved.join("::");
        self.text.contains(&identity)
            || (resolved.len() == 1
                && resolved
                    .first()
                    .is_some_and(|name| is_builtin_text_type_name(name)))
            || (resolved
                .first()
                .is_some_and(|root| matches!(root.as_str(), "std" | "core" | "alloc"))
                && resolved
                    .last()
                    .is_some_and(|name| is_builtin_text_type_name(name)))
    }

    fn resolved_path_is_text_sink_owner(&self, resolved: &[String]) -> bool {
        self.resolved_path_is_text(resolved)
            || (resolved.len() == 1
                && resolved
                    .first()
                    .is_some_and(|name| is_builtin_text_sink_owner(name)))
            || (resolved
                .first()
                .is_some_and(|root| matches!(root.as_str(), "std" | "core" | "alloc"))
                && resolved
                    .last()
                    .is_some_and(|name| is_builtin_text_sink_owner(name)))
    }

    fn type_is_direct_semantic(&self, module: &[String], ty: &syn::Type) -> bool {
        match ty {
            syn::Type::Path(path) => self.path_is_semantic(module, &path.path),
            syn::Type::Group(group) => self.type_is_direct_semantic(module, &group.elem),
            syn::Type::Paren(paren) => self.type_is_direct_semantic(module, &paren.elem),
            _ => false,
        }
    }

    fn path_is_semantic(&self, module: &[String], path: &syn::Path) -> bool {
        self.resolved_path_is_semantic(&self.resolve_path(module, path))
    }

    fn path_is_owned(&self, module: &[String], path: &syn::Path) -> bool {
        let resolved = self.resolve_path(module, path);
        resolved
            .first()
            .is_some_and(|root| root.starts_with("rumoca_ir_"))
            || self.owned.contains(&resolved.join("::"))
            || (resolved.len() == 1
                && resolved
                    .last()
                    .is_some_and(|name| is_owned_semantic_ir_name(name)))
    }

    fn segments_are_semantic(&self, module: &[String], segments: &[String]) -> bool {
        let resolved = self.resolve_segments(module, segments, &mut BTreeSet::new());
        self.resolved_path_is_semantic(&resolved)
            || self.semantic.iter().any(|identity| {
                identity == &resolved.join("::")
                    || identity.starts_with(&format!("{}::", resolved.join("::")))
            })
    }

    fn resolved_path_is_semantic(&self, resolved: &[String]) -> bool {
        resolved
            .first()
            .is_some_and(|root| root.starts_with("rumoca_ir_"))
            || (resolved.first().is_some_and(|root| root == "minijinja")
                && resolved.last().is_some_and(|name| name == "Value"))
            || self.semantic.contains(&resolved.join("::"))
            || (resolved.len() == 1
                && resolved
                    .last()
                    .is_some_and(|name| is_semantic_type_name(name)))
    }

    fn resolve_path(&self, module: &[String], path: &syn::Path) -> Vec<String> {
        let segments = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        self.resolve_segments(module, &segments, &mut BTreeSet::new())
    }

    fn resolve_segments(
        &self,
        module: &[String],
        segments: &[String],
        visited: &mut BTreeSet<String>,
    ) -> Vec<String> {
        if segments.is_empty() {
            return vec!["crate".to_string()];
        }
        let state = format!("{}|{}", module_key(module), segments.join("::"));
        if !visited.insert(state) {
            return segments.to_vec();
        }
        match segments[0].as_str() {
            "crate" => self.resolve_absolute(module, &segments[1..], visited),
            "self" => self.resolve_local_absolute(module, &segments[1..], visited),
            "super" => {
                let mut parent = module.to_vec();
                let mut offset = 0;
                while segments
                    .get(offset)
                    .is_some_and(|segment| segment == "super")
                {
                    parent.truncate(parent.len().saturating_sub(1).max(1));
                    offset += 1;
                }
                self.resolve_local_absolute(&parent, &segments[offset..], visited)
            }
            local => {
                let key = (module_key(module), local.to_string());
                if let Some(binding) = self.imports.get(&key) {
                    return self.resolve_binding_with_suffix(binding, &segments[1..], visited);
                }
                let local_identity = qualified_name(module, local);
                let local_module = child_module_key(module, local);
                if self.declarations.contains_key(&local_identity)
                    || self.modules.contains(&local_module)
                {
                    return self.resolve_local_absolute(module, segments, visited);
                }
                if let Some(canonical) = module.first().and_then(|target| {
                    self.external_crates
                        .get(&(target.clone(), local.to_string()))
                }) {
                    return std::iter::once(canonical.clone())
                        .chain(segments[1..].iter().cloned())
                        .collect();
                }
                if local.starts_with("rumoca_ir_") {
                    return segments.to_vec();
                }
                if let Some(resolved) = self.resolve_from_globs(module, segments, visited) {
                    return resolved;
                }
                segments.to_vec()
            }
        }
    }

    fn resolve_absolute(
        &self,
        module: &[String],
        segments: &[String],
        visited: &mut BTreeSet<String>,
    ) -> Vec<String> {
        let mut absolute = module.first().cloned().into_iter().collect::<Vec<_>>();
        absolute.extend_from_slice(segments);
        self.resolve_local_absolute(&[], &absolute, visited)
    }

    fn resolve_local_absolute(
        &self,
        base: &[String],
        segments: &[String],
        visited: &mut BTreeSet<String>,
    ) -> Vec<String> {
        let mut prefix = base.to_vec();
        for (index, segment) in segments.iter().enumerate() {
            let key = (module_key(&prefix), segment.clone());
            if let Some(binding) = self.imports.get(&key) {
                return self.resolve_binding_with_suffix(binding, &segments[index + 1..], visited);
            }
            prefix.push(segment.clone());
        }
        std::iter::once("crate".to_string()).chain(prefix).collect()
    }

    fn resolve_binding_with_suffix(
        &self,
        binding: &ImportBinding,
        suffix: &[String],
        visited: &mut BTreeSet<String>,
    ) -> Vec<String> {
        let mut resolved = if binding
            .source
            .first()
            .is_some_and(|root| root == "<extern>")
        {
            let external = &binding.source[1..];
            let root = external.first().cloned().unwrap_or_default();
            let canonical = binding
                .defining_module
                .first()
                .and_then(|target| self.external_crates.get(&(target.clone(), root.clone())))
                .cloned()
                .unwrap_or(root);
            std::iter::once(canonical)
                .chain(external[1..].iter().cloned())
                .collect()
        } else {
            self.resolve_segments(&binding.defining_module, &binding.source, visited)
        };
        resolved.extend_from_slice(suffix);
        if resolved.first().is_some_and(|root| root == "crate") {
            self.resolve_local_absolute(&[], &resolved[1..], visited)
        } else {
            resolved
        }
    }

    fn resolve_from_globs(
        &self,
        module: &[String],
        segments: &[String],
        visited: &mut BTreeSet<String>,
    ) -> Option<Vec<String>> {
        let mut candidates = self
            .globs
            .get(&module_key(module))?
            .iter()
            .filter_map(|binding| {
                let mut candidate_visited = visited.clone();
                let resolved =
                    self.resolve_binding_with_suffix(binding, segments, &mut candidate_visited);
                let identity = resolved.join("::");
                let namespace = format!("{identity}::");
                (resolved
                    .first()
                    .is_some_and(|root| root.starts_with("rumoca_ir_"))
                    || self.declarations.contains_key(&identity)
                    || self.modules.contains(&identity)
                    || self.imports.keys().any(|(scope, _)| scope == &identity)
                    || self
                        .semantic
                        .iter()
                        .any(|candidate| candidate.starts_with(&namespace)))
                .then_some(resolved)
            })
            .collect::<Vec<_>>();
        candidates.sort();
        candidates.dedup();
        (candidates.len() == 1).then(|| candidates.remove(0))
    }
}

#[derive(Clone)]
struct FlatImport {
    local: String,
    renamed: bool,
    source: Vec<String>,
}

fn flatten_use_tree(
    tree: &syn::UseTree,
    prefix: &mut Vec<String>,
    imports: &mut Vec<FlatImport>,
    globs: &mut Vec<Vec<String>>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            flatten_use_tree(&path.tree, prefix, imports, globs);
            prefix.pop();
        }
        syn::UseTree::Name(name) if name.ident == "self" => {
            if let Some(local) = prefix.last() {
                imports.push(FlatImport {
                    local: local.clone(),
                    renamed: false,
                    source: prefix.clone(),
                });
            }
        }
        syn::UseTree::Name(name) => {
            let local = name.ident.to_string();
            let mut source = prefix.clone();
            source.push(local.clone());
            imports.push(FlatImport {
                local,
                renamed: false,
                source,
            });
        }
        syn::UseTree::Rename(rename) => {
            let mut source = prefix.clone();
            if rename.ident != "self" {
                source.push(rename.ident.to_string());
            }
            imports.push(FlatImport {
                local: rename.rename.to_string(),
                renamed: true,
                source,
            });
        }
        syn::UseTree::Glob(_) => globs.push(prefix.clone()),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                flatten_use_tree(item, prefix, imports, globs);
            }
        }
    }
}

fn qualified_name(module: &[String], name: &str) -> String {
    if module.is_empty() {
        format!("crate::{name}")
    } else {
        format!("crate::{}::{name}", module.join("::"))
    }
}

fn module_key(module: &[String]) -> String {
    if module.is_empty() {
        "crate".to_string()
    } else {
        format!("crate::{}", module.join("::"))
    }
}

fn child_module_key(module: &[String], child: &str) -> String {
    format!("{}::{child}", module_key(module))
}

pub(super) fn item_attributes(item: &syn::Item) -> &[syn::Attribute] {
    match item {
        syn::Item::Const(item) => &item.attrs,
        syn::Item::Enum(item) => &item.attrs,
        syn::Item::ExternCrate(item) => &item.attrs,
        syn::Item::Fn(item) => &item.attrs,
        syn::Item::ForeignMod(item) => &item.attrs,
        syn::Item::Impl(item) => &item.attrs,
        syn::Item::Macro(item) => &item.attrs,
        syn::Item::Mod(item) => &item.attrs,
        syn::Item::Static(item) => &item.attrs,
        syn::Item::Struct(item) => &item.attrs,
        syn::Item::Trait(item) => &item.attrs,
        syn::Item::TraitAlias(item) => &item.attrs,
        syn::Item::Type(item) => &item.attrs,
        syn::Item::Union(item) => &item.attrs,
        syn::Item::Use(item) => &item.attrs,
        _ => &[],
    }
}

pub(super) fn is_owned_semantic_ir_name(name: &str) -> bool {
    matches!(
        name,
        "Expression"
            | "Statement"
            | "Equation"
            | "Model"
            | "Dae"
            | "SolveProblem"
            | "SolveArtifacts"
            | "AlgorithmCodePackage"
            | "ClassTree"
            | "ComputeBlock"
            | "ComputeNode"
            | "ScalarProgram"
            | "ScalarProgramBlock"
            | "LinearOp"
            | "BinaryOp"
            | "UnaryOp"
            | "CompareOp"
            | "BuiltinType"
    ) || name.ends_with("Expression")
        || name.ends_with("Statement")
        || name.ends_with("Equation")
}

fn is_builtin_text_type_name(name: &str) -> bool {
    matches!(
        name,
        "OsStr" | "OsString" | "Path" | "PathBuf" | "String" | "char" | "str"
    )
}

fn is_builtin_text_sink_owner(name: &str) -> bool {
    matches!(name, "Add" | "AddAssign" | "Extend" | "Write") || is_builtin_text_type_name(name)
}

fn vec_segment_is_bytes(segment: &syn::PathSegment) -> bool {
    segment.ident == "Vec"
        && matches!(&segment.arguments, syn::PathArguments::AngleBracketed(arguments)
        if arguments.args.iter().any(|argument| {
            matches!(argument, syn::GenericArgument::Type(syn::Type::Path(byte))
                if byte.path.is_ident("u8"))
        }))
}

pub(super) fn expression_is_string_value(
    expression: &syn::Expr,
    strings: &BTreeSet<String>,
) -> bool {
    matches!(expression, syn::Expr::Lit(literal)
        if matches!(literal.lit, syn::Lit::Str(_) | syn::Lit::Char(_) | syn::Lit::Byte(_) | syn::Lit::ByteStr(_)))
        || expression_root_name(expression).is_some_and(|root| strings.contains(&root))
}

pub(super) fn expression_constructs_string(
    expression: &syn::Expr,
    strings: &BTreeSet<String>,
    semantic_types: &SemanticTypeIndex,
    semantic_module: &[String],
) -> bool {
    if expression_is_string_value(expression, strings) {
        return true;
    }
    match expression {
        syn::Expr::Call(call) => match call.func.as_ref() {
            syn::Expr::Path(path) => {
                let mut owner = path.path.clone();
                owner.segments.pop();
                semantic_types.path_is_text(semantic_module, &owner)
            }
            _ => false,
        },
        syn::Expr::Macro(item) => macro_name(&item.mac)
            .is_some_and(|name| matches!(name.as_str(), "format" | "format_args" | "concat")),
        syn::Expr::MethodCall(call) => {
            matches!(
                call.method.to_string().as_str(),
                "to_string" | "to_owned" | "replace" | "join" | "collect"
            ) || (call.method == "into"
                && expression_constructs_string(
                    &call.receiver,
                    strings,
                    semantic_types,
                    semantic_module,
                ))
        }
        syn::Expr::Paren(paren) => {
            expression_constructs_string(&paren.expr, strings, semantic_types, semantic_module)
        }
        syn::Expr::Group(group) => {
            expression_constructs_string(&group.expr, strings, semantic_types, semantic_module)
        }
        _ => false,
    }
}

pub(super) fn is_semantic_type_name(name: &str) -> bool {
    matches!(
        name,
        "Value"
            | "Expression"
            | "Expr"
            | "Statement"
            | "Equation"
            | "Model"
            | "Dae"
            | "SolveProblem"
            | "SolveArtifacts"
            | "AlgorithmCodePackage"
            | "ClassTree"
            | "CodegenInput"
            | "ComputeBlock"
            | "ComputeNode"
            | "ScalarProgram"
            | "ScalarProgramBlock"
            | "LinearOp"
            | "BinaryOp"
            | "UnaryOp"
            | "CompareOp"
            | "BuiltinType"
            | "DiscreteSolveSystem"
            | "StructuralPattern"
            | "TensorInputKind"
            | "TensorOutputMap"
            | "TensorUpdateSubscript"
            | "AffineStencilConstStride"
            | "AffineStencilConstStrideTerm"
            | "AffineStencilIndexStrideTerm"
            | "AffineStencilLoadStride"
    ) || name.ends_with("Expression")
        || name.ends_with("Statement")
        || name.ends_with("Equation")
        || name.ends_with("View")
        || name.ends_with("SemanticView")
        || name.ends_with("TemplateView")
}

pub(super) fn type_is_mutable_reference(ty: &syn::Type) -> bool {
    matches!(ty, syn::Type::Reference(reference) if reference.mutability.is_some())
}

pub(super) fn pattern_names(pattern: &syn::Pat, output: &mut BTreeSet<String>) {
    match pattern {
        syn::Pat::Ident(ident) => {
            output.insert(ident.ident.to_string());
        }
        syn::Pat::Reference(reference) => pattern_names(&reference.pat, output),
        syn::Pat::Tuple(tuple) => {
            for element in &tuple.elems {
                pattern_names(element, output);
            }
        }
        syn::Pat::Type(typed) => pattern_names(&typed.pat, output),
        _ => {}
    }
}

pub(super) fn expression_root_name(expression: &syn::Expr) -> Option<String> {
    match expression {
        syn::Expr::Path(path) if path.path.segments.len() == 1 => path
            .path
            .segments
            .first()
            .map(|segment| segment.ident.to_string()),
        syn::Expr::Field(field) => expression_root_name(&field.base),
        syn::Expr::Index(index) => expression_root_name(&index.expr),
        syn::Expr::Paren(paren) => expression_root_name(&paren.expr),
        syn::Expr::Group(group) => expression_root_name(&group.expr),
        syn::Expr::Reference(reference) => expression_root_name(&reference.expr),
        _ => None,
    }
}

pub(super) fn is_mutator_name(name: &str) -> bool {
    matches!(
        name,
        "insert"
            | "push"
            | "push_str"
            | "remove"
            | "retain"
            | "clear"
            | "sort"
            | "sort_by"
            | "truncate"
            | "extend"
            | "append"
            | "swap"
    )
}

pub(super) fn is_assignment_operator(operator: &syn::BinOp) -> bool {
    matches!(
        operator,
        syn::BinOp::AddAssign(_)
            | syn::BinOp::SubAssign(_)
            | syn::BinOp::MulAssign(_)
            | syn::BinOp::DivAssign(_)
            | syn::BinOp::RemAssign(_)
            | syn::BinOp::BitXorAssign(_)
            | syn::BinOp::BitAndAssign(_)
            | syn::BinOp::BitOrAssign(_)
            | syn::BinOp::ShlAssign(_)
            | syn::BinOp::ShrAssign(_)
    )
}

pub(super) fn is_semantic_transform_name(name: &str) -> bool {
    let name = name.to_ascii_lowercase();
    name == "eval"
        || name.starts_with("eval_")
        || name.starts_with("evaluate")
        || name == "fold"
        || name.contains("constant_fold")
        || name == "lower"
        || name.starts_with("lower_")
        || name.ends_with("_lowering")
        || name.starts_with("unroll")
        || name.starts_with("expand")
        || name.starts_with("rewrite")
        || name.starts_with("project")
        || name.starts_with("compile")
        || name.starts_with("translate")
        || name.starts_with("encode")
        || name.starts_with("normalize")
        || name.starts_with("specialize")
        || name.starts_with("materialize")
        || name.starts_with("plan")
        || name.starts_with("classify")
        || name.starts_with("canonicalize")
        || name.starts_with("scalarize")
        || name.starts_with("to_scalar")
}

pub(super) fn macro_name(item: &syn::Macro) -> Option<String> {
    item.path
        .segments
        .last()
        .map(|segment| segment.ident.to_string())
}

pub(super) fn flattened_use_paths(tree: &syn::UseTree) -> Vec<Vec<String>> {
    fn walk(tree: &syn::UseTree, prefix: &mut Vec<String>, output: &mut Vec<Vec<String>>) {
        match tree {
            syn::UseTree::Path(path) => {
                prefix.push(path.ident.to_string());
                walk(&path.tree, prefix, output);
                prefix.pop();
            }
            syn::UseTree::Name(name) => {
                let mut path = prefix.clone();
                path.push(name.ident.to_string());
                output.push(path);
            }
            syn::UseTree::Rename(rename) => {
                let mut path = prefix.clone();
                path.push(rename.ident.to_string());
                output.push(path);
            }
            syn::UseTree::Group(group) => {
                for item in &group.items {
                    walk(item, prefix, output);
                }
            }
            syn::UseTree::Glob(_) => output.push(prefix.clone()),
        }
    }
    let mut output = Vec::new();
    walk(tree, &mut Vec::new(), &mut output);
    output
}

pub(super) fn returns_bare_string_literal(block: &syn::Block) -> bool {
    block.stmts.last().is_some_and(|statement| match statement {
        syn::Stmt::Expr(expression, _) => expression_is_string_literal_result(expression),
        _ => false,
    })
}

pub(super) fn expression_is_string_literal_result(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Lit(literal) => matches!(literal.lit, syn::Lit::Str(_)),
        syn::Expr::Return(returned) => returned
            .expr
            .as_deref()
            .is_some_and(expression_is_string_literal_result),
        syn::Expr::Paren(paren) => expression_is_string_literal_result(&paren.expr),
        syn::Expr::Group(group) => expression_is_string_literal_result(&group.expr),
        syn::Expr::If(item) => {
            returns_bare_string_literal(&item.then_branch)
                || item
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, branch)| expression_is_string_literal_result(branch))
        }
        syn::Expr::Match(item) => item
            .arms
            .iter()
            .any(|arm| expression_is_string_literal_result(&arm.body)),
        syn::Expr::Block(item) => returns_bare_string_literal(&item.block),
        _ => false,
    }
}
