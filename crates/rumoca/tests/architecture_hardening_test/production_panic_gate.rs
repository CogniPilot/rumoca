//! Production panic-surface scanner and adversarial controls.

use super::*;

fn guaranteed_panic_evasions(source: &str) -> Vec<&'static str> {
    let syntax =
        syn::parse_file(source).expect("production Rust must parse for panic-evasion scan");
    let mut visitor = GuaranteedPanicVisitor::default();
    syn::visit::Visit::visit_file(&mut visitor, &syntax);
    visitor.findings
}

#[derive(Default)]
struct GuaranteedPanicVisitor {
    findings: Vec<&'static str>,
}

impl syn::visit::Visit<'_> for GuaranteedPanicVisitor {
    fn visit_stmt(&mut self, statement: &syn::Stmt) {
        let attributes: &[syn::Attribute] = match statement {
            syn::Stmt::Local(local) => &local.attrs,
            syn::Stmt::Macro(invocation) => &invocation.attrs,
            syn::Stmt::Item(_) | syn::Stmt::Expr(_, _) => &[],
        };
        if attributes_require_test(attributes) {
            return;
        }
        syn::visit::visit_stmt(self, statement);
    }

    fn visit_expr(&mut self, expression: &syn::Expr) {
        if attributes_require_test(expression_attributes(expression)) {
            return;
        }
        syn::visit::visit_expr(self, expression);
    }

    fn visit_arm(&mut self, arm: &syn::Arm) {
        if attributes_require_test(&arm.attrs) {
            return;
        }
        syn::visit::visit_arm(self, arm);
    }

    fn visit_field_value(&mut self, field: &syn::FieldValue) {
        if attributes_require_test(&field.attrs) {
            return;
        }
        syn::visit::visit_field_value(self, field);
    }

    fn visit_item_mod(&mut self, item: &syn::ItemMod) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_item_mod(self, item);
    }

    fn visit_item_fn(&mut self, item: &syn::ItemFn) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_item_fn(self, item);
    }

    fn visit_item_impl(&mut self, item: &syn::ItemImpl) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_item_impl(self, item);
    }

    fn visit_item_const(&mut self, item: &syn::ItemConst) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_item_const(self, item);
    }

    fn visit_item_static(&mut self, item: &syn::ItemStatic) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_item_static(self, item);
    }

    fn visit_item_trait(&mut self, item: &syn::ItemTrait) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_item_trait(self, item);
    }

    fn visit_impl_item_fn(&mut self, item: &syn::ImplItemFn) {
        if attributes_require_test(&item.attrs) {
            return;
        }
        syn::visit::visit_impl_item_fn(self, item);
    }

    fn visit_expr_method_call(&mut self, expression: &syn::ExprMethodCall) {
        if expression.method == "expect" || expression.method == "unwrap" {
            if explicit_none_expression(&expression.receiver) {
                self.findings.push("explicit None asserted present");
            }
            if empty_iterator_next_expression(&expression.receiver) {
                self.findings
                    .push("empty iterator asserted present with expect/unwrap");
            }
            if explicit_result_constructor(&expression.receiver, "Err") {
                self.findings.push("explicit Err asserted successful");
            }
        }
        if (expression.method == "expect_err" || expression.method == "unwrap_err")
            && explicit_result_constructor(&expression.receiver, "Ok")
        {
            self.findings.push("explicit Ok asserted erroneous");
        }
        syn::visit::visit_expr_method_call(self, expression);
    }

    fn visit_expr_call(&mut self, expression: &syn::ExprCall) {
        if let Some(finding) = associated_guaranteed_failure(expression) {
            self.findings.push(finding);
        }
        syn::visit::visit_expr_call(self, expression);
    }

    fn visit_macro(&mut self, invocation: &syn::Macro) {
        for (name, finding) in [
            ("panic", "panic macro invocation"),
            ("todo", "todo macro invocation"),
            ("unimplemented", "unimplemented macro invocation"),
        ] {
            if panic_path_ends_with(&invocation.path, &[name]) {
                self.findings.push(finding);
            }
        }
        syn::visit::visit_macro(self, invocation);
    }
}

fn associated_guaranteed_failure(call: &syn::ExprCall) -> Option<&'static str> {
    let syn::Expr::Path(callee) = peel_grouped_expression(&call.func) else {
        return None;
    };
    let argument = call.args.first()?;
    if (canonical_associated_method(&callee.path, "Option", "option", "expect")
        || canonical_associated_method(&callee.path, "Option", "option", "unwrap"))
        && explicit_none_expression(argument)
    {
        return Some("explicit None asserted present");
    }
    if (canonical_associated_method(&callee.path, "Result", "result", "expect")
        || canonical_associated_method(&callee.path, "Result", "result", "unwrap"))
        && explicit_result_constructor(argument, "Err")
    {
        return Some("explicit Err asserted successful");
    }
    if (canonical_associated_method(&callee.path, "Result", "result", "expect_err")
        || canonical_associated_method(&callee.path, "Result", "result", "unwrap_err"))
        && explicit_result_constructor(argument, "Ok")
    {
        return Some("explicit Ok asserted erroneous");
    }
    None
}

fn canonical_associated_method(path: &syn::Path, owner: &str, module: &str, method: &str) -> bool {
    path_is(path, &[owner, method])
        || path_is(path, &["std", module, owner, method])
        || path_is(path, &["core", module, owner, method])
}

fn expression_attributes(expression: &syn::Expr) -> &[syn::Attribute] {
    match expression {
        syn::Expr::Array(value) => &value.attrs,
        syn::Expr::Assign(value) => &value.attrs,
        syn::Expr::Async(value) => &value.attrs,
        syn::Expr::Await(value) => &value.attrs,
        syn::Expr::Binary(value) => &value.attrs,
        syn::Expr::Block(value) => &value.attrs,
        syn::Expr::Break(value) => &value.attrs,
        syn::Expr::Call(value) => &value.attrs,
        syn::Expr::Cast(value) => &value.attrs,
        syn::Expr::Closure(value) => &value.attrs,
        syn::Expr::Const(value) => &value.attrs,
        syn::Expr::Continue(value) => &value.attrs,
        syn::Expr::Field(value) => &value.attrs,
        syn::Expr::ForLoop(value) => &value.attrs,
        syn::Expr::Group(value) => &value.attrs,
        syn::Expr::If(value) => &value.attrs,
        syn::Expr::Index(value) => &value.attrs,
        syn::Expr::Infer(value) => &value.attrs,
        syn::Expr::Let(value) => &value.attrs,
        syn::Expr::Lit(value) => &value.attrs,
        syn::Expr::Loop(value) => &value.attrs,
        syn::Expr::Macro(value) => &value.attrs,
        syn::Expr::Match(value) => &value.attrs,
        syn::Expr::MethodCall(value) => &value.attrs,
        syn::Expr::Paren(value) => &value.attrs,
        syn::Expr::Path(value) => &value.attrs,
        syn::Expr::Range(value) => &value.attrs,
        syn::Expr::RawAddr(value) => &value.attrs,
        syn::Expr::Reference(value) => &value.attrs,
        syn::Expr::Repeat(value) => &value.attrs,
        syn::Expr::Return(value) => &value.attrs,
        syn::Expr::Struct(value) => &value.attrs,
        syn::Expr::Try(value) => &value.attrs,
        syn::Expr::TryBlock(value) => &value.attrs,
        syn::Expr::Tuple(value) => &value.attrs,
        syn::Expr::Unary(value) => &value.attrs,
        syn::Expr::Unsafe(value) => &value.attrs,
        syn::Expr::While(value) => &value.attrs,
        syn::Expr::Yield(value) => &value.attrs,
        _ => &[],
    }
}

fn explicit_result_constructor(expression: &syn::Expr, constructor: &str) -> bool {
    let syn::Expr::Call(call) = peel_grouped_expression(expression) else {
        return false;
    };
    let syn::Expr::Path(path) = peel_grouped_expression(&call.func) else {
        return false;
    };
    path.path
        .segments
        .last()
        .is_some_and(|segment| segment.ident == constructor)
}

fn explicit_none_expression(expression: &syn::Expr) -> bool {
    let syn::Expr::Path(path) = peel_grouped_expression(expression) else {
        return false;
    };
    path.qself.is_none()
        && path
            .path
            .segments
            .last()
            .is_some_and(|segment| segment.ident == "None")
}

fn empty_iterator_next_expression(expression: &syn::Expr) -> bool {
    let syn::Expr::MethodCall(next) = peel_grouped_expression(expression) else {
        return false;
    };
    if next.method != "next" || !next.args.is_empty() {
        return false;
    }
    let syn::Expr::Call(empty) = peel_grouped_expression(&next.receiver) else {
        return false;
    };
    let syn::Expr::Path(path) = peel_grouped_expression(&empty.func) else {
        return false;
    };
    panic_path_ends_with(&path.path, &["std", "iter", "empty"])
        || panic_path_ends_with(&path.path, &["core", "iter", "empty"])
}

fn peel_grouped_expression(mut expression: &syn::Expr) -> &syn::Expr {
    loop {
        expression = match expression {
            syn::Expr::Group(group) => &group.expr,
            syn::Expr::Paren(parenthesized) => &parenthesized.expr,
            _ => return expression,
        };
    }
}

fn panic_path_ends_with(path: &syn::Path, expected: &[&str]) -> bool {
    path.segments.len() >= expected.len()
        && path
            .segments
            .iter()
            .rev()
            .zip(expected.iter().rev())
            .all(|(segment, expected)| segment.ident == expected)
}

fn path_is(path: &syn::Path, expected: &[&str]) -> bool {
    path.segments.len() == expected.len()
        && path
            .segments
            .iter()
            .zip(expected)
            .all(|(segment, expected)| segment.ident == expected)
}

pub(super) fn assert_guaranteed_failure_mutation_controls() {
    for mutation in [
        "fn evade() { std::iter::empty::<u8>().next().expect(\"always absent\"); }",
        "fn evade() { std::iter::empty::<u8>().next().unwrap(); }",
        "fn evade() { core :: iter :: empty :: <u8>().next().unwrap(); }",
        "fn evade() { None::<&mut u8>.expect(\"always absent\"); }",
        "fn evade() { None::<\n &mut u8\n >\n .expect(\"always absent\"); }",
        "fn evade() { (None :: <u8>).unwrap(); }",
        "fn evade() { panic ! (\"whitespace cannot hide a panic\"); }",
        "fn evade() { std::todo ! (); }",
        "fn evade() { unimplemented ! (); }",
        "fn evade() { Err::<(), _>(\"failure\").expect(\"always an error\"); }",
        "fn evade() { Ok::<_, ()>(1).unwrap_err(); }",
        "fn evade() { Option::unwrap(None::<u8>); }",
        "fn evade() { std::option::Option::expect(None::<u8>, \"always absent\"); }",
        "fn evade() { Result::expect(Err::<(), _>(\"failure\"), \"always an error\"); }",
        "fn evade() { core::result::Result::unwrap_err(Ok::<_, ()>(1)); }",
    ] {
        assert!(
            !guaranteed_panic_evasions(mutation).is_empty(),
            "production panic gate missed guaranteed-failure mutation `{mutation}`"
        );
    }
    assert!(
        guaranteed_panic_evasions(
            "fn checked(value: Option<u8>) -> u8 { value.expect(\"construction proved presence\") }"
        )
        .is_empty()
    );
    for control in [
        r#"fn text() { let _ = "std::iter::empty::<u8>().next().unwrap()"; }"#,
        "fn comment() { /* None::<u8>.unwrap() */ }",
        "#[cfg(test)] mod tests { fn assertion() { None::<u8>.unwrap(); } }",
        "#[cfg(test)] impl Example { fn assertion() { None::<u8>.unwrap(); } }",
        "#[cfg(test)] const ABSENT: u8 = None::<u8>.unwrap();",
        "fn scoped() { #[cfg(test)] { None::<u8>.unwrap(); } }",
        "fn scoped() { #[cfg(all(test, feature = \"fmi\"))] let absent = None::<u8>.unwrap(); }",
        "fn scoped() { #[cfg(test)] panic!(\"test-only statement macro\"); }",
        "fn scoped(value: bool) { match value { #[cfg(test)] true => None::<u8>.unwrap(), _ => 0 } }",
        "fn scoped() { let _ = Example { #[cfg(test)] value: None::<u8>.unwrap() }; }",
        "fn custom() { Wrapper::expect(Err::<(), _>(\"not a Result owner\")); }",
        "fn custom() { custom::Option::unwrap(None::<u8>); }",
        "fn custom() { custom::Result::expect(Err::<(), _>(\"failure\"), \"message\"); }",
    ] {
        assert!(
            guaranteed_panic_evasions(control).is_empty(),
            "production panic gate fabricated a finding from `{control}`"
        );
    }
    assert!(!guaranteed_panic_evasions(
        "#[cfg(any(test, feature = \"fmi\"))] mod live { fn assertion() { None::<u8>.unwrap(); } }"
    )
    .is_empty());
    assert!(
        !guaranteed_panic_evasions(
            "fn live() { #[cfg(any(test, feature = \"fmi\"))] { None::<u8>.unwrap(); } }"
        )
        .is_empty()
    );
}

pub(super) fn assert_production_has_no_guaranteed_panics() {
    let root = workspace_root();
    let mut rs_files = Vec::new();
    collect_rs_files(&root.join("crates"), &mut rs_files);

    let mut offenders = Vec::new();
    for path in rs_files {
        let rel = path.strip_prefix(&root).unwrap_or(&path);
        if is_test_or_example_path(rel) {
            continue;
        }
        let content = fs::read_to_string(&path).expect("read Rust source");
        offenders.extend(
            guaranteed_panic_evasions(&content)
                .into_iter()
                .map(|evasion| format!("{} contains {evasion}", path.display())),
        );
    }

    assert!(
        offenders.is_empty(),
        "production code must not use or disguise panic!/todo!/unimplemented!; return a typed \
error at the true trust boundary or carry construction-issued proof instead: {offenders:#?}"
    );
}
