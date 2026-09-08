//! Guard explicit AST recovery nodes against successful semantic consumption.

use std::collections::{BTreeMap, BTreeSet};

use super::semantic_owner_bindings::{
    SemanticSourceIdentity, canonical_local_binding, semantic_module_bindings,
    semantic_module_member,
};
use super::*;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum AstBinding {
    AstModule,
    CoreModule,
    Equation,
    Statement,
    AstExpression,
    AstSubscript,
    TerminalType,
    CoreExpression,
    CoreStatement,
    OpUnary,
    OpBinary,
    Other,
}

impl AstBinding {
    fn is_ast_recovery(self) -> bool {
        matches!(self, Self::Equation | Self::Statement)
    }

    fn is_semantic_empty_owner(self) -> bool {
        matches!(
            self,
            Self::Equation
                | Self::Statement
                | Self::AstExpression
                | Self::AstSubscript
                | Self::TerminalType
                | Self::CoreExpression
                | Self::CoreStatement
                | Self::OpUnary
                | Self::OpBinary
        )
    }

    fn is_empty_operator(self) -> bool {
        matches!(self, Self::OpUnary | Self::OpBinary)
    }

    fn is_recovery_sentinel_owner(self) -> bool {
        matches!(
            self,
            Self::Equation
                | Self::Statement
                | Self::AstExpression
                | Self::AstSubscript
                | Self::TerminalType
                | Self::CoreExpression
                | Self::CoreStatement
        )
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ScanKind {
    AstRecoverySuccess,
    SemanticZeroFabrication,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct ScopedBinding {
    target: AstBinding,
    priority: u8,
}

#[derive(Default)]
struct ScopeFrame {
    names: BTreeMap<String, ScopedBinding>,
    module_boundary: bool,
}

impl ScopeFrame {
    fn insert(&mut self, name: String, target: AstBinding, priority: u8) -> bool {
        if self
            .names
            .get(&name)
            .is_some_and(|existing| existing.priority > priority)
        {
            return false;
        }
        let replacement = ScopedBinding { target, priority };
        if self.names.get(&name) == Some(&replacement) {
            return false;
        }
        self.names.insert(name, replacement);
        true
    }
}

#[derive(Debug)]
struct ImportSpec {
    path: Vec<String>,
    alias: Option<String>,
    glob: bool,
}

struct AstRecoverySuccessVisitor {
    offenders: BTreeSet<String>,
    scopes: Vec<ScopeFrame>,
    source_identity: SemanticSourceIdentity,
    scan_kind: ScanKind,
}

impl AstRecoverySuccessVisitor {
    fn new(source_identity: SemanticSourceIdentity, scan_kind: ScanKind) -> Self {
        Self {
            offenders: BTreeSet::new(),
            scopes: Vec::new(),
            source_identity,
            scan_kind,
        }
    }

    fn record_offender(&mut self) {
        let finding = match self.scan_kind {
            ScanKind::AstRecoverySuccess => {
                "explicit AST Equation::Empty or Statement::Empty returns Ok/Some"
            }
            ScanKind::SemanticZeroFabrication => {
                "Empty semantic match arm returns a successful value"
            }
        };
        self.offenders.insert(finding.to_string());
    }

    fn visit_items_in_scope(&mut self, items: &[syn::Item], module_boundary: bool) {
        self.scopes.push(ScopeFrame {
            names: BTreeMap::new(),
            module_boundary,
        });
        self.populate_scope(items.iter(), module_boundary);
        for item in items {
            self.visit_item(item);
        }
        self.scopes.pop();
    }

    fn visit_block_in_scope(&mut self, block: &syn::Block) {
        let items = block.stmts.iter().filter_map(|statement| match statement {
            syn::Stmt::Item(item) => Some(item),
            _ => None,
        });
        self.scopes.push(ScopeFrame {
            names: BTreeMap::new(),
            module_boundary: false,
        });
        self.populate_scope(items, false);
        for statement in &block.stmts {
            self.visit_stmt(statement);
        }
        self.scopes.pop();
    }

    fn populate_scope<'a>(
        &mut self,
        items: impl Iterator<Item = &'a syn::Item> + Clone,
        module_boundary: bool,
    ) {
        for item in items.clone() {
            self.record_local_item(item, module_boundary);
        }
        let imports = self.collect_imports(items);
        // Imports are order-independent. Iterate to resolve chained aliases,
        // such as `use syntax::Equation as AstEquation`.
        for _ in 0..=imports.len() {
            let mut changed = false;
            for import in &imports {
                changed |= self.apply_import(import);
            }
            if !changed {
                break;
            }
        }
        // An unresolved explicit import still shadows a glob import.
        for import in &imports {
            if !import.glob
                && let Some(alias) = &import.alias
                && self.resolve_path_segments(&import.path).is_none()
            {
                self.current_scope_mut()
                    .insert(alias.clone(), AstBinding::Other, 2);
            }
        }
    }

    fn collect_imports<'a>(
        &mut self,
        items: impl Iterator<Item = &'a syn::Item>,
    ) -> Vec<ImportSpec> {
        let mut imports = Vec::new();
        for item in items {
            match item {
                syn::Item::Use(item_use) if !attributes_require_test(&item_use.attrs) => {
                    flatten_import_tree(&item_use.tree, Vec::new(), &mut imports);
                }
                syn::Item::ExternCrate(item) if !attributes_require_test(&item.attrs) => {
                    let (alias, target) = extern_crate_binding(item, self.source_identity);
                    self.current_scope_mut().insert(alias, target, 2);
                }
                _ => {}
            }
        }
        imports
    }

    fn record_local_item(&mut self, item: &syn::Item, module_boundary: bool) {
        let (name, canonical_owner) =
            local_item_name(item).map_or((None, None), |(name, attrs)| {
                if attributes_require_test(attrs) {
                    return (None, None);
                }
                let canonical_scope = module_boundary
                    && self
                        .scopes
                        .iter()
                        .filter(|scope| scope.module_boundary)
                        .count()
                        == 1
                    && matches!(item, syn::Item::Enum(_));
                let canonical_owner = canonical_scope
                    .then(|| canonical_local_binding(&name, self.source_identity))
                    .flatten();
                (Some(name), canonical_owner)
            });
        let Some(name) = name else {
            return;
        };
        let target = canonical_owner.unwrap_or(AstBinding::Other);
        self.current_scope_mut().insert(name, target, 3);
    }

    fn apply_import(&mut self, import: &ImportSpec) -> bool {
        if import.glob {
            let bindings = self.glob_bindings(&import.path);
            let mut changed = false;
            for (name, target) in bindings {
                changed |= self.current_scope_mut().insert(name, target, 1);
            }
            return changed;
        }
        let Some(alias) = &import.alias else {
            return false;
        };
        let Some(target) = self.resolve_path_segments(&import.path) else {
            return false;
        };
        self.current_scope_mut().insert(alias.clone(), target, 2)
    }

    fn glob_bindings(&self, path: &[String]) -> Vec<(String, AstBinding)> {
        if let Some(module @ (AstBinding::AstModule | AstBinding::CoreModule)) =
            self.resolve_path_segments(path)
        {
            return semantic_module_bindings(module);
        }
        let Some(frame) = self.relative_module_frame(path) else {
            return Vec::new();
        };
        frame
            .names
            .iter()
            .map(|(name, binding)| (name.clone(), binding.target))
            .collect()
    }

    fn pattern_mentions_ast_recovery(&self, pattern: &syn::Pat) -> bool {
        struct RecoveryPathVisitor<'a> {
            owner: &'a AstRecoverySuccessVisitor,
            found: bool,
        }
        impl<'ast> Visit<'ast> for RecoveryPathVisitor<'_> {
            fn visit_path(&mut self, path: &'ast syn::Path) {
                self.found |= self.owner.path_is_ast_recovery(path);
                visit::visit_path(self, path);
            }
        }

        let mut visitor = RecoveryPathVisitor {
            owner: self,
            found: false,
        };
        visitor.visit_pat(pattern);
        visitor.found
    }

    fn pattern_mentions_semantic_empty(&self, pattern: &syn::Pat) -> bool {
        self.pattern_has_path(pattern, Self::path_is_semantic_empty)
    }

    fn pattern_mentions_empty_operator(&self, pattern: &syn::Pat) -> bool {
        self.pattern_has_path(pattern, Self::path_is_empty_operator)
    }

    fn pattern_has_path(
        &self,
        pattern: &syn::Pat,
        predicate: fn(&Self, &syn::Path) -> bool,
    ) -> bool {
        struct PathVisitor<'a> {
            owner: &'a AstRecoverySuccessVisitor,
            predicate: fn(&AstRecoverySuccessVisitor, &syn::Path) -> bool,
            found: bool,
        }
        impl<'ast> Visit<'ast> for PathVisitor<'_> {
            fn visit_path(&mut self, path: &'ast syn::Path) {
                self.found |= (self.predicate)(self.owner, path);
                visit::visit_path(self, path);
            }
        }

        let mut visitor = PathVisitor {
            owner: self,
            predicate,
            found: false,
        };
        visitor.visit_pat(pattern);
        visitor.found
    }

    fn expression_mentions_recovery_sentinel(&self, expression: &syn::Expr) -> bool {
        struct PathVisitor<'a> {
            owner: &'a AstRecoverySuccessVisitor,
            found: bool,
        }
        impl<'ast> Visit<'ast> for PathVisitor<'_> {
            fn visit_path(&mut self, path: &'ast syn::Path) {
                self.found |= self.owner.path_is_recovery_sentinel(path);
                visit::visit_path(self, path);
            }
        }

        let mut visitor = PathVisitor {
            owner: self,
            found: false,
        };
        visitor.visit_expr(expression);
        visitor.found
    }

    fn condition_mentions_missing_semantic_data(&self, condition: &syn::Expr) -> bool {
        if condition_proves_semantic_empty(condition, self) {
            return true;
        }
        match condition {
            syn::Expr::MethodCall(call) => {
                call.method == "is_none"
                    && super::semantic_zero::expression_looks_semantic(&call.receiver)
            }
            syn::Expr::Group(group) => self.condition_mentions_missing_semantic_data(&group.expr),
            syn::Expr::Paren(paren) => self.condition_mentions_missing_semantic_data(&paren.expr),
            _ => false,
        }
    }

    fn path_is_ast_recovery(&self, path: &syn::Path) -> bool {
        let mut segments = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        if segments.pop().as_deref() != Some("Empty") {
            return false;
        }
        self.resolve_path_segments(&segments)
            .is_some_and(AstBinding::is_ast_recovery)
    }

    fn path_is_semantic_empty(&self, path: &syn::Path) -> bool {
        self.empty_path_owner(path)
            .is_some_and(AstBinding::is_semantic_empty_owner)
    }

    fn path_is_empty_operator(&self, path: &syn::Path) -> bool {
        self.empty_path_owner(path)
            .is_some_and(AstBinding::is_empty_operator)
    }

    fn path_is_recovery_sentinel(&self, path: &syn::Path) -> bool {
        self.empty_path_owner(path)
            .is_some_and(AstBinding::is_recovery_sentinel_owner)
    }

    fn empty_path_owner(&self, path: &syn::Path) -> Option<AstBinding> {
        let mut segments = path
            .segments
            .iter()
            .map(|segment| segment.ident.to_string())
            .collect::<Vec<_>>();
        (segments.pop().as_deref() == Some("Empty"))
            .then(|| self.resolve_path_segments(&segments))
            .flatten()
    }

    fn resolve_path_segments(&self, segments: &[String]) -> Option<AstBinding> {
        let first = segments.first().map(String::as_str)?;
        let (mut target, consumed) = match first {
            // A lexical item/import with the dependency's spelling wins in
            // Rust's type namespace. Only an unshadowed extern-prelude name
            // proves the canonical AST crate.
            "rumoca_ir_ast" => (self.lookup_bare(first).unwrap_or(AstBinding::AstModule), 1),
            "rumoca_core" => (self.lookup_bare(first).unwrap_or(AstBinding::CoreModule), 1),
            // Inside rumoca-ir-ast, nodes is the canonical owner module. Do
            // not generalize this to arbitrary crate modules: a local
            // crate::other::Equation carries no AST identity proof.
            "crate"
                if self.source_identity.is_ast_crate()
                    && segments.get(1).is_some_and(|segment| segment == "nodes") =>
            {
                (AstBinding::AstModule, 2)
            }
            "crate" if self.source_identity.is_ast_crate() => (AstBinding::AstModule, 1),
            "crate" if self.source_identity.is_core_crate() => (AstBinding::CoreModule, 1),
            "self" => {
                let name = segments.get(1)?;
                (self.lookup_in_current_module(name)?, 2)
            }
            "super" => {
                let supers = segments.iter().take_while(|part| *part == "super").count();
                let name = segments.get(supers)?;
                (self.lookup_in_ancestor_module(supers, name)?, supers + 1)
            }
            _ => (self.lookup_bare(first)?, 1),
        };
        for segment in &segments[consumed..] {
            target = semantic_module_member(target, segment)?;
        }
        Some(target)
    }

    fn lookup_bare(&self, name: &str) -> Option<AstBinding> {
        for frame in self.scopes.iter().rev() {
            if let Some(binding) = frame.names.get(name) {
                return Some(binding.target);
            }
            if frame.module_boundary {
                break;
            }
        }
        None
    }

    fn lookup_in_current_module(&self, name: &str) -> Option<AstBinding> {
        self.scopes
            .iter()
            .rev()
            .find(|frame| frame.module_boundary)
            .and_then(|frame| frame.names.get(name))
            .map(|binding| binding.target)
    }

    fn lookup_in_ancestor_module(&self, supers: usize, name: &str) -> Option<AstBinding> {
        self.scopes
            .iter()
            .rev()
            .filter(|frame| frame.module_boundary)
            .nth(supers)
            .and_then(|frame| frame.names.get(name))
            .map(|binding| binding.target)
    }

    fn relative_module_frame(&self, path: &[String]) -> Option<&ScopeFrame> {
        if path.is_empty() || !path.iter().all(|part| part == "super") {
            return None;
        }
        self.scopes
            .iter()
            .rev()
            .filter(|frame| frame.module_boundary)
            .nth(path.len())
    }

    fn current_scope_mut(&mut self) -> &mut ScopeFrame {
        self.scopes.last_mut().expect("visitor has a lexical scope")
    }
}

impl<'ast> Visit<'ast> for AstRecoverySuccessVisitor {
    fn visit_file(&mut self, file: &'ast syn::File) {
        self.visit_items_in_scope(&file.items, true);
    }

    fn visit_item_mod(&mut self, module: &'ast syn::ItemMod) {
        if !attributes_require_test(&module.attrs)
            && let Some((_, items)) = &module.content
        {
            self.visit_items_in_scope(items, true);
        }
    }

    fn visit_item_fn(&mut self, function: &'ast syn::ItemFn) {
        if !attributes_require_test(&function.attrs) {
            visit::visit_item_fn(self, function);
        }
    }

    fn visit_item_impl(&mut self, implementation: &'ast syn::ItemImpl) {
        if !attributes_require_test(&implementation.attrs) {
            visit::visit_item_impl(self, implementation);
        }
    }

    fn visit_item_use(&mut self, _item_use: &'ast syn::ItemUse) {}

    fn visit_block(&mut self, block: &'ast syn::Block) {
        self.visit_block_in_scope(block);
    }

    fn visit_arm(&mut self, arm: &'ast syn::Arm) {
        match self.scan_kind {
            ScanKind::AstRecoverySuccess => {
                if self.pattern_mentions_ast_recovery(&arm.pat)
                    && expression_returns_success_result_deep(&arm.body)
                {
                    self.record_offender();
                }
            }
            ScanKind::SemanticZeroFabrication => {
                let empty = self.pattern_mentions_semantic_empty(&arm.pat);
                let operator_success = self.pattern_mentions_empty_operator(&arm.pat)
                    && super::semantic_zero::expression_returns_ok_result_deep(&arm.body);
                if empty
                    && (super::semantic_zero::expression_returns_successful_zero(&arm.body)
                        || operator_success)
                {
                    self.record_offender();
                }
            }
        }
        visit::visit_arm(self, arm);
    }

    fn visit_expr_if(&mut self, expression: &'ast syn::ExprIf) {
        match self.scan_kind {
            ScanKind::AstRecoverySuccess => {
                if condition_proves_ast_recovery(&expression.cond, self)
                    && block_returns_success_result_deep(&expression.then_branch)
                {
                    self.record_offender();
                }
            }
            ScanKind::SemanticZeroFabrication => {
                if self.condition_mentions_missing_semantic_data(&expression.cond)
                    && super::semantic_zero::block_contains_successful_zero(&expression.then_branch)
                {
                    self.record_offender();
                }
            }
        }
        visit::visit_expr_if(self, expression);
    }

    fn visit_expr_call(&mut self, call: &'ast syn::ExprCall) {
        if self.scan_kind == ScanKind::SemanticZeroFabrication
            && path_expression_ends_with(call.func.as_ref(), "replace")
            && call
                .args
                .iter()
                .nth(1)
                .is_some_and(|argument| self.expression_mentions_recovery_sentinel(argument))
        {
            self.offenders
                .insert("rewrite installs an Empty recovery sentinel as a placeholder".to_string());
        }
        visit::visit_expr_call(self, call);
    }
}

fn extern_crate_binding(
    item: &syn::ItemExternCrate,
    source_identity: SemanticSourceIdentity,
) -> (String, AstBinding) {
    let source = item.ident.to_string();
    let alias = item
        .rename
        .as_ref()
        .map_or_else(|| source.clone(), |(_, alias)| alias.to_string());
    let target = match source.as_str() {
        "rumoca_ir_ast" => AstBinding::AstModule,
        "rumoca_core" => AstBinding::CoreModule,
        "self" if source_identity.is_ast_crate() => AstBinding::AstModule,
        "self" if source_identity.is_core_crate() => AstBinding::CoreModule,
        _ => AstBinding::Other,
    };
    (alias, target)
}

fn local_item_name(item: &syn::Item) -> Option<(String, &[syn::Attribute])> {
    match item {
        syn::Item::Enum(item) => Some((item.ident.to_string(), &item.attrs)),
        syn::Item::Mod(item) => Some((item.ident.to_string(), &item.attrs)),
        syn::Item::Struct(item) => Some((item.ident.to_string(), &item.attrs)),
        syn::Item::Trait(item) => Some((item.ident.to_string(), &item.attrs)),
        syn::Item::Type(item) => Some((item.ident.to_string(), &item.attrs)),
        syn::Item::Union(item) => Some((item.ident.to_string(), &item.attrs)),
        _ => None,
    }
}

fn flatten_import_tree(
    tree: &syn::UseTree,
    mut prefix: Vec<String>,
    imports: &mut Vec<ImportSpec>,
) {
    match tree {
        syn::UseTree::Path(path) => {
            prefix.push(path.ident.to_string());
            flatten_import_tree(&path.tree, prefix, imports);
        }
        syn::UseTree::Name(name) => {
            let ident = name.ident.to_string();
            if ident == "self" {
                let alias = prefix.last().cloned();
                imports.push(ImportSpec {
                    path: prefix,
                    alias,
                    glob: false,
                });
            } else {
                prefix.push(ident.clone());
                imports.push(ImportSpec {
                    path: prefix,
                    alias: Some(ident),
                    glob: false,
                });
            }
        }
        syn::UseTree::Rename(rename) => {
            if rename.ident != "self" {
                prefix.push(rename.ident.to_string());
            }
            imports.push(ImportSpec {
                path: prefix,
                alias: Some(rename.rename.to_string()),
                glob: false,
            });
        }
        syn::UseTree::Glob(_) => imports.push(ImportSpec {
            path: prefix,
            alias: None,
            glob: true,
        }),
        syn::UseTree::Group(group) => {
            for item in &group.items {
                flatten_import_tree(item, prefix.clone(), imports);
            }
        }
    }
}

fn condition_proves_ast_recovery(
    condition: &syn::Expr,
    visitor: &AstRecoverySuccessVisitor,
) -> bool {
    match condition {
        syn::Expr::Macro(invocation)
            if invocation
                .mac
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "matches") =>
        {
            syn::parse2::<MatchesInput>(invocation.mac.tokens.clone())
                .is_ok_and(|input| visitor.pattern_mentions_ast_recovery(&input.pattern))
        }
        syn::Expr::Let(expression) => visitor.pattern_mentions_ast_recovery(&expression.pat),
        syn::Expr::Binary(expression) if matches!(expression.op, syn::BinOp::And(_)) => {
            condition_proves_ast_recovery(&expression.left, visitor)
                || condition_proves_ast_recovery(&expression.right, visitor)
        }
        syn::Expr::Group(expression) => condition_proves_ast_recovery(&expression.expr, visitor),
        syn::Expr::Paren(expression) => condition_proves_ast_recovery(&expression.expr, visitor),
        _ => false,
    }
}

fn condition_proves_semantic_empty(
    condition: &syn::Expr,
    visitor: &AstRecoverySuccessVisitor,
) -> bool {
    match condition {
        syn::Expr::Macro(invocation)
            if invocation
                .mac
                .path
                .segments
                .last()
                .is_some_and(|segment| segment.ident == "matches") =>
        {
            syn::parse2::<MatchesInput>(invocation.mac.tokens.clone())
                .is_ok_and(|input| visitor.pattern_mentions_semantic_empty(&input.pattern))
        }
        syn::Expr::Let(expression) => visitor.pattern_mentions_semantic_empty(&expression.pat),
        syn::Expr::Binary(expression) if matches!(expression.op, syn::BinOp::And(_)) => {
            condition_proves_semantic_empty(&expression.left, visitor)
                || condition_proves_semantic_empty(&expression.right, visitor)
        }
        syn::Expr::Group(expression) => condition_proves_semantic_empty(&expression.expr, visitor),
        syn::Expr::Paren(expression) => condition_proves_semantic_empty(&expression.expr, visitor),
        _ => false,
    }
}

struct MatchesInput {
    pattern: syn::Pat,
}

impl syn::parse::Parse for MatchesInput {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let _: syn::Expr = input.parse()?;
        input.parse::<syn::Token![,]>()?;
        let pattern = syn::Pat::parse_multi_with_leading_vert(input)?;
        if input.peek(syn::Token![if]) {
            input.parse::<syn::Token![if]>()?;
            let _: syn::Expr = input.parse()?;
        }
        if input.peek(syn::Token![,]) {
            input.parse::<syn::Token![,]>()?;
        }
        if !input.is_empty() {
            return Err(input.error("unexpected tokens after matches! pattern"));
        }
        Ok(Self { pattern })
    }
}

fn expression_returns_success_result_deep(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Call(call) => {
            path_expression_ends_with(call.func.as_ref(), "Ok")
                || path_expression_ends_with(call.func.as_ref(), "Some")
        }
        syn::Expr::Return(expression) => expression
            .expr
            .as_deref()
            .is_some_and(expression_returns_success_result_deep),
        syn::Expr::Block(expression) => block_returns_success_result_deep(&expression.block),
        syn::Expr::Group(expression) => expression_returns_success_result_deep(&expression.expr),
        syn::Expr::Paren(expression) => expression_returns_success_result_deep(&expression.expr),
        syn::Expr::If(expression) => {
            block_returns_success_result_deep(&expression.then_branch)
                || expression
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, branch)| expression_returns_success_result_deep(branch))
        }
        syn::Expr::Match(expression) => expression
            .arms
            .iter()
            .any(|arm| expression_returns_success_result_deep(&arm.body)),
        _ => false,
    }
}

fn expression_contains_success_return(expression: &syn::Expr) -> bool {
    match expression {
        syn::Expr::Return(_) => expression_returns_success_result_deep(expression),
        syn::Expr::Block(expression) => block_contains_success_return(&expression.block),
        syn::Expr::Group(expression) => expression_contains_success_return(&expression.expr),
        syn::Expr::Paren(expression) => expression_contains_success_return(&expression.expr),
        syn::Expr::If(expression) => {
            block_contains_success_return(&expression.then_branch)
                || expression
                    .else_branch
                    .as_ref()
                    .is_some_and(|(_, branch)| expression_contains_success_return(branch))
        }
        syn::Expr::Match(expression) => expression
            .arms
            .iter()
            .any(|arm| expression_contains_success_return(&arm.body)),
        syn::Expr::Loop(expression) => block_contains_success_return(&expression.body),
        syn::Expr::While(expression) => block_contains_success_return(&expression.body),
        syn::Expr::ForLoop(expression) => block_contains_success_return(&expression.body),
        _ => false,
    }
}

fn block_returns_success_result_deep(block: &syn::Block) -> bool {
    if block_contains_success_return(block) {
        return true;
    }
    block.stmts.last().is_some_and(|statement| {
        matches!(statement, syn::Stmt::Expr(expression, None) if expression_returns_success_result_deep(expression))
    })
}

fn block_contains_success_return(block: &syn::Block) -> bool {
    block.stmts.iter().any(|statement| match statement {
        syn::Stmt::Expr(expression, _) => expression_contains_success_return(expression),
        syn::Stmt::Local(local) => local
            .init
            .as_ref()
            .is_some_and(|init| expression_contains_success_return(&init.expr)),
        syn::Stmt::Item(_) | syn::Stmt::Macro(_) => false,
    })
}

fn ast_recovery_successes(
    source: &str,
    source_identity: SemanticSourceIdentity,
) -> Result<Vec<String>, syn::Error> {
    let syntax = syn::parse_file(source)?;
    let mut visitor = AstRecoverySuccessVisitor::new(source_identity, ScanKind::AstRecoverySuccess);
    visitor.visit_file(&syntax);
    Ok(visitor.offenders.into_iter().collect())
}

pub(super) fn semantic_zero_fabrications(
    source: &str,
    source_identity: SemanticSourceIdentity,
) -> Result<Vec<String>, syn::Error> {
    let syntax = syn::parse_file(source)?;
    let mut visitor =
        AstRecoverySuccessVisitor::new(source_identity, ScanKind::SemanticZeroFabrication);
    visitor.visit_file(&syntax);
    Ok(visitor.offenders.into_iter().collect())
}

#[test]
fn explicit_ast_recovery_nodes_cannot_return_semantic_success() {
    let root = workspace_root();
    let mut files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut files);
    files.sort();

    let mut offenders = Vec::new();
    for path in files {
        let relative = normalized_rel_path(path.strip_prefix(&root).unwrap_or(&path));
        if is_test_or_example_path(Path::new(&relative)) {
            continue;
        }
        let source = fs::read_to_string(&path).expect("read production Rust source");
        if !source.contains("::Empty") {
            continue;
        }
        let findings = ast_recovery_successes(
            &source,
            SemanticSourceIdentity::from_relative_path(&relative),
        )
        .unwrap_or_else(|error| panic!("parse {relative} for AST recovery success: {error}"));
        offenders.extend(
            findings
                .into_iter()
                .map(|finding| format!("{relative}: {finding}")),
        );
    }

    assert!(
        offenders.is_empty(),
        "explicit AST recovery nodes must fail closed rather than become successful semantic no-ops: {offenders:#?}"
    );
}

fn assert_positive_fixtures(fixtures: &[&str]) {
    for source in fixtures {
        assert_eq!(
            ast_recovery_successes(source, SemanticSourceIdentity::External)
                .expect("valid positive fixture")
                .len(),
            1,
            "gate missed explicit AST recovery success: {source}"
        );
    }
}

fn assert_negative_fixtures(fixtures: &[&str]) {
    for source in fixtures {
        assert!(
            ast_recovery_successes(source, SemanticSourceIdentity::External)
                .expect("valid negative fixture")
                .is_empty(),
            "gate rejected a legal non-AST-success form: {source}"
        );
    }
}

#[test]
fn explicit_ast_recovery_success_gate_has_positive_and_negative_fixtures() {
    assert_positive_fixtures(&[
        "fn f(x: E) -> R { match x { rumoca_ir_ast::Equation::Empty => Ok(Rows::default()), _ => Err(()) } }",
        "extern crate rumoca_ir_ast as syntax; fn f(x: E) -> R { match x { syntax::Equation::Empty => Ok(()), _ => Err(()) } }",
        "use rumoca_ir_ast as syntax; fn f(x: E) -> R { match x { syntax::Statement::Empty => Some(Flow::Continue), _ => None } }",
        "use rumoca_ir_ast::{Equation, Statement as AstStatement}; fn f(x: E) -> R { match x { Equation::Empty | AstStatement::Empty => Ok(()), _ => Err(()) } }",
        "use rumoca_ir_ast::*; fn f(x: Equation) -> R { match x { Equation::Empty => Ok(()), _ => Err(()) } }",
        "use rumoca_ir_ast as syntax; use syntax::Equation as AstEquation; fn f(x: AstEquation) -> R { match x { AstEquation::Empty => Ok(()), _ => Err(()) } }",
        "fn f(x: E) -> R { use rumoca_ir_ast::{self as syntax}; match x { syntax::Equation::Empty => Ok(()), _ => Err(()) } }",
        "mod outer { use rumoca_ir_ast::Equation; mod inner { use super::*; fn f(x: Equation) -> R { match x { Equation::Empty => Some(()), _ => None } } } }",
        "use rumoca_ir_ast as ast; fn f(x: E) -> R { match x { ast::Equation::Empty => { if ready() { return Ok(()); } Err(()) }, _ => Err(()) } }",
        "use rumoca_ir_ast as ast; fn f(eq: E) -> R { if matches!(eq, ast::Equation::Empty) { return Some(Flow::Continue); } None }",
        "use rumoca_ir_ast as syntax; mod nested { use super::syntax::Statement; fn f(x: Statement) -> R { if let Statement::Empty = x { return Ok(()); } Err(()) } }",
    ]);
    assert_negative_fixtures(&[
        "fn f(x: E) -> R { match x { ast::Equation::Empty => Ok(()), _ => Err(()) } }",
        "mod rumoca_ir_ast { pub enum Equation { Empty } } fn f(x: E) -> R { match x { rumoca_ir_ast::Equation::Empty => Ok(()), _ => Err(()) } }",
        "extern crate other as rumoca_ir_ast; fn f(x: E) -> R { match x { rumoca_ir_ast::Statement::Empty => Some(()), _ => None } }",
        "fn left() { use rumoca_ir_ast as syntax; consume(syntax::Equation::Empty); } fn right(x: E) -> R { match x { syntax::Equation::Empty => Ok(()), _ => Err(()) } }",
        "mod left { use rumoca_ir_ast as syntax; } mod right { use rumoca_core as syntax; fn f(x: E) -> R { match x { syntax::Statement::Empty { .. } => Ok(()), _ => Err(()) } } }",
        "use rumoca_ir_ast::*; use rumoca_core::Statement; fn f(x: Statement) -> R { match x { Statement::Empty { .. } => Ok(()), _ => Err(()) } }",
        "fn f(x: E) -> R { match x { rumoca_core::Statement::Empty { .. } => Ok(()), _ => Err(()) } }",
        "use rumoca_ir_ast as ast; fn f(x: E) -> R { match x { ast::Equation::Empty => Err(E::recovery()), _ => Ok(()) } }",
        "use rumoca_ir_ast as ast; fn f(x: E) -> E { match x { ast::Statement::Empty => ast::Statement::Empty, other => other } }",
        "use rumoca_ir_ast as ast; fn f(eq: E) -> R { if !matches!(eq, ast::Equation::Empty) { return Ok(()); } Err(()) }",
        "fn f(x: E) -> R { match x { foo::ast::Equation::Empty => Ok(()), _ => Err(()) } }",
    ]);
    assert_eq!(
        ast_recovery_successes(
            "enum Equation { Empty } fn f(x: Equation) -> R { match x { Equation::Empty => Ok(()), } }",
            SemanticSourceIdentity::AstNodes,
        )
        .expect("valid AST-crate fixture")
        .len(),
        1,
    );
    assert_eq!(
        ast_recovery_successes(
            "fn f(x: E) -> R { match x { crate::nodes::Equation::Empty => Ok(()), _ => Err(()) } }",
            SemanticSourceIdentity::AstOther,
        )
        .expect("valid canonical AST nodes fixture")
        .len(),
        1,
    );
    assert_eq!(
        ast_recovery_successes(
            "use crate::nodes::Statement as AstStatement; fn f(x: E) -> R { match x { AstStatement::Empty => Some(()), _ => None } }",
            SemanticSourceIdentity::AstOther,
        )
        .expect("valid imported canonical AST nodes fixture")
        .len(),
        1,
    );
    assert!(
        ast_recovery_successes(
            "mod local { enum Statement { Empty } fn f(x: Statement) -> R { match x { Statement::Empty => Ok(()), } } }",
            SemanticSourceIdentity::AstOther,
        )
        .expect("valid nested AST-crate collision fixture")
        .is_empty()
    );
    assert!(
        ast_recovery_successes(
            "enum Equation { Empty } fn f(x: Equation) -> R { match x { Equation::Empty => Ok(()), } }",
            SemanticSourceIdentity::AstOther,
        )
        .expect("valid out-of-line AST-crate collision fixture")
        .is_empty()
    );
    assert!(
        ast_recovery_successes(
            "fn f(x: E) -> R { match x { crate::other::Equation::Empty => Ok(()), _ => Err(()) } }",
            SemanticSourceIdentity::AstOther,
        )
        .expect("valid noncanonical AST-crate module fixture")
        .is_empty()
    );
}
