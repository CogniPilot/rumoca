use std::cell::RefCell;

use crate::component_base_name;
use indexmap::IndexSet;
use rumoca_core::ExpressionVisitor;
use rumoca_core::{
    BuiltinFunction, Expression, Literal, Reference, Subscript, VarName, VarNameId,
    split_last_top_level,
};

pub fn parse_embedded_subscripts(name: &str) -> Option<Vec<i64>> {
    rumoca_core::parse_scalar_name(name).map(|scalar| scalar.indices)
}

pub fn subscripts_all_one(subscripts: &[Subscript]) -> bool {
    !subscripts.is_empty()
        && subscripts.iter().all(|sub| match sub {
            Subscript::Index { value, .. } => *value == 1,
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(i),
                    ..
                } => *i == 1,
                _ => false,
            },
            Subscript::Colon { .. } => false,
        })
}

pub fn embedded_subscripts_all_one(name: &str) -> bool {
    parse_embedded_subscripts(name)
        .is_some_and(|indices| !indices.is_empty() && indices.iter().all(|i| *i == 1))
}

pub fn subscripts_match_indices(subscripts: &[Subscript], expected: &[i64]) -> bool {
    if subscripts.len() != expected.len() || subscripts.is_empty() {
        return false;
    }
    subscripts
        .iter()
        .zip(expected.iter())
        .all(|(sub, expected_idx)| match sub {
            Subscript::Index { value, .. } => *value == *expected_idx,
            Subscript::Expr { expr, .. } => match expr.as_ref() {
                Expression::Literal {
                    value: Literal::Integer(i),
                    ..
                } => *i == *expected_idx,
                _ => false,
            },
            Subscript::Colon { .. } => false,
        })
}

pub fn split_complex_field_suffix(name: &str) -> Option<(&str, &str)> {
    let (base, field) = split_last_top_level(name)?;
    matches!(field, "re" | "im").then_some((base, field))
}

pub fn complex_base_alias_match(base_or_field: &str, other: &str) -> bool {
    if !has_complex_field_suffix_candidate(base_or_field)
        && !has_complex_field_suffix_candidate(other)
    {
        return false;
    }
    split_complex_field_suffix(base_or_field).is_some_and(|(base, _)| base == other)
        || split_complex_field_suffix(other).is_some_and(|(base, _)| base == base_or_field)
}

fn has_complex_field_suffix_candidate(name: &str) -> bool {
    name.ends_with(".re") || name.ends_with(".im")
}

fn segment_may_be_complex_field(segment: &str) -> bool {
    component_base_name(segment).is_some_and(|base| matches!(base.as_str(), "re" | "im"))
}

fn base_name_for_valid_component(name: &str) -> Option<String> {
    component_base_name(name)
}

fn valid_component_has_embedded_subscripts(name: &str) -> Option<bool> {
    base_name_for_valid_component(name).map(|base| base != name)
}

fn valid_unsubscripted_component_id(name: &VarName) -> Option<VarNameId> {
    base_name_for_valid_component(name.as_str())
        .and_then(|base| (base == name.as_str()).then_some(name.id()))
}

fn reference_base_id(name: &Reference) -> Option<VarNameId> {
    cached_base_id(name.var_name())
}

fn var_name_base_id(name: &VarName) -> Option<VarNameId> {
    cached_base_id(name)
}

thread_local! {
    static BASE_NAME_ID_CACHE: RefCell<indexmap::IndexMap<VarNameId, Option<VarNameId>>> =
        RefCell::new(indexmap::IndexMap::new());
}

fn cached_base_id(name: &VarName) -> Option<VarNameId> {
    let base_name = base_name_for_valid_component(name.as_str())?;
    if base_name == name.as_str() {
        return Some(name.id());
    }
    BASE_NAME_ID_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        if let Some(base_id) = cache.get(&name.id()) {
            return *base_id;
        }
        let base_id = Some(VarName::new(base_name).id());
        cache.insert(name.id(), base_id);
        base_id
    })
}

/// Check if a `VarRef` expression with `name` and `subscripts` matches `unknown`.
pub fn var_ref_matches_unknown(
    name: &Reference,
    subscripts: &[Subscript],
    unknown: &VarName,
) -> bool {
    if name.var_name().id() == unknown.id() {
        return subscripts.is_empty() || subscripts_all_one(subscripts);
    }
    if subscripts.is_empty() && complex_base_alias_match(name.as_str(), unknown.as_str()) {
        return true;
    }
    let Some(name_base_id) = reference_base_id(name) else {
        return false;
    };
    let Some(unknown_base_id) = var_name_base_id(unknown) else {
        return false;
    };
    if name_base_id != unknown_base_id {
        if !segment_may_be_complex_field(name.last_segment())
            && !segment_may_be_complex_field(unknown.last_segment())
        {
            return false;
        }
        let name_base = component_base_name(name.as_str());
        let unknown_base = component_base_name(unknown.as_str());
        if name_base
            .as_deref()
            .zip(unknown_base.as_deref())
            .is_some_and(|(name_base, unknown_base)| {
                complex_base_alias_match(name_base, unknown_base)
            })
        {
            return true;
        }
        return false;
    }
    if !subscripts.is_empty() {
        if let Some(indices) = parse_embedded_subscripts(unknown.as_str())
            && subscripts_match_indices(subscripts, &indices)
        {
            return true;
        }
        return false;
    }

    let Some(name_has_embedded) = valid_component_has_embedded_subscripts(name.as_str()) else {
        return false;
    };
    let Some(unknown_has_embedded) = valid_component_has_embedded_subscripts(unknown.as_str())
    else {
        return false;
    };
    if name_has_embedded != unknown_has_embedded {
        let embedded_name = if name_has_embedded {
            name.as_str()
        } else {
            unknown.as_str()
        };
        if embedded_subscripts_all_one(embedded_name) {
            return true;
        }
        return false;
    }
    if name_has_embedded {
        return name.as_str() == unknown.as_str();
    }
    true
}

/// Check if an expression references a variable (by base name).
pub fn expr_contains_var(expr: &Expression, var: &VarName) -> bool {
    let mut checker = ContainsVarChecker { var, found: false };
    checker.visit_expression(expr);
    checker.found
}

struct ContainsVarChecker<'a> {
    var: &'a VarName,
    found: bool,
}

impl ExpressionVisitor for ContainsVarChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        if var_ref_matches_unknown(name, subscripts, self.var) {
            self.found = true;
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

fn scalar_subscript_index(sub: &Subscript) -> Option<usize> {
    match sub {
        Subscript::Index { value, .. } => usize::try_from(*value).ok().filter(|index| *index > 0),
        Subscript::Expr { expr, .. } => match expr.as_ref() {
            Expression::Literal {
                value: Literal::Integer(i),
                ..
            } => usize::try_from(*i).ok().filter(|index| *index > 0),
            Expression::Literal {
                value: Literal::Real(v),
                ..
            } if v.is_finite() && v.fract() == 0.0 => {
                usize::try_from(*v as i64).ok().filter(|index| *index > 0)
            }
            _ => None,
        },
        _ => None,
    }
}

fn append_subscripts(base: String, subscripts: &[Subscript]) -> Option<String> {
    if subscripts.is_empty() {
        return Some(base);
    }
    let mut indices = Vec::with_capacity(subscripts.len());
    for sub in subscripts {
        indices.push(scalar_subscript_index(sub)?);
    }
    Some(crate::format_subscript_key(&base, &indices))
}

fn expr_exact_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => append_subscripts(name.as_str().to_string(), subscripts),
        Expression::Index {
            base, subscripts, ..
        } => {
            let base_name = expr_exact_name(base)?;
            append_subscripts(base_name, subscripts)
        }
        Expression::FieldAccess { base, field, .. } => {
            let base_name = expr_exact_name(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

fn expr_base_name_inner(expr: &Expression) -> Option<String> {
    match expr {
        Expression::VarRef { name, .. } => component_base_name(name.as_str()),
        Expression::Index { base, .. } => expr_base_name_inner(base),
        Expression::FieldAccess { base, field, .. } => {
            let base_name = expr_base_name_inner(base)?;
            Some(format!("{base_name}.{field}"))
        }
        _ => None,
    }
}

/// Returns true if `expr` is a reference to `var_name` (exact or by base component).
pub fn expr_refers_to_var(expr: &Expression, var_name: &VarName) -> bool {
    if let Some(expr_exact) = expr_exact_name(expr)
        && expr_exact == var_name.as_str()
    {
        return true;
    }
    // For indexed targets, require exact index/path match to avoid cross-index collisions.
    if valid_component_has_embedded_subscripts(var_name.as_str()).unwrap_or(false) {
        return false;
    }
    let Some(expr_base) = expr_base_name_inner(expr) else {
        return false;
    };
    let Some(var_base) = component_base_name(var_name.as_str()) else {
        return false;
    };
    expr_base == var_base
}

#[derive(Debug, Clone)]
pub struct DerivativeNameMatcher {
    exact_names: IndexSet<VarNameId>,
    base_names: IndexSet<VarNameId>,
}

impl DerivativeNameMatcher {
    pub fn from_var_names<'a>(names: impl IntoIterator<Item = &'a VarName>) -> Self {
        let mut exact_names = IndexSet::new();
        let mut base_names = IndexSet::new();
        for name in names {
            exact_names.insert(name.id());
            if valid_unsubscripted_component_id(name).is_some() {
                base_names.insert(name.id());
            }
        }
        Self {
            exact_names,
            base_names,
        }
    }

    pub fn expression_refers_to_match(&self, expr: &Expression) -> bool {
        if let Expression::VarRef {
            name, subscripts, ..
        } = expr
        {
            return self.var_ref_matches(name, subscripts);
        }
        if let Some(exact_name) = expr_exact_name(expr)
            && self.exact_names.contains(&VarName::new(exact_name).id())
        {
            return true;
        }
        let Some(base_name) = expr_base_name_inner(expr) else {
            return false;
        };
        self.base_names.contains(&VarName::new(base_name).id())
    }

    fn var_ref_matches(&self, name: &Reference, subscripts: &[Subscript]) -> bool {
        if subscripts.is_empty() && self.exact_names.contains(&name.var_name().id()) {
            return true;
        }
        if !subscripts.is_empty()
            && let Some(exact_name) = append_subscripts(name.as_str().to_string(), subscripts)
            && self.exact_names.contains(&VarName::new(exact_name).id())
        {
            return true;
        }
        self.base_name_matches(name)
    }

    fn base_name_matches(&self, name: &Reference) -> bool {
        let Some(has_embedded_subscripts) = valid_component_has_embedded_subscripts(name.as_str())
        else {
            return false;
        };
        if !has_embedded_subscripts {
            return self.base_names.contains(&name.var_name().id());
        }
        component_base_name(name.as_str())
            .is_some_and(|base_name| self.base_names.contains(&VarName::new(base_name).id()))
    }
}

#[derive(Debug, Clone, Copy)]
struct SingleDerivativeNameMatcher {
    exact_name: VarNameId,
    base_name: Option<VarNameId>,
}

impl SingleDerivativeNameMatcher {
    fn from_var_name(name: &VarName) -> Self {
        let base_name = valid_unsubscripted_component_id(name);
        Self {
            exact_name: name.id(),
            base_name,
        }
    }

    fn expression_refers_to_match(&self, expr: &Expression) -> bool {
        if let Expression::VarRef {
            name, subscripts, ..
        } = expr
        {
            return self.var_ref_matches(name, subscripts);
        }
        if let Some(exact_name) = expr_exact_name(expr)
            && VarName::new(exact_name).id() == self.exact_name
        {
            return true;
        }
        let Some(base_name) = expr_base_name_inner(expr) else {
            return false;
        };
        self.base_name
            .is_some_and(|base_id| VarName::new(base_name).id() == base_id)
    }

    fn var_ref_matches(&self, name: &Reference, subscripts: &[Subscript]) -> bool {
        if subscripts.is_empty() && name.var_name().id() == self.exact_name {
            return true;
        }
        if !subscripts.is_empty()
            && let Some(exact_name) = append_subscripts(name.as_str().to_string(), subscripts)
            && VarName::new(exact_name).id() == self.exact_name
        {
            return true;
        }
        self.base_name_matches(name)
    }

    fn base_name_matches(&self, name: &Reference) -> bool {
        let Some(base_id) = self.base_name else {
            return false;
        };
        let Some(has_embedded_subscripts) = valid_component_has_embedded_subscripts(name.as_str())
        else {
            return false;
        };
        if !has_embedded_subscripts {
            return name.var_name().id() == base_id;
        }
        component_base_name(name.as_str())
            .is_some_and(|base_name| VarName::new(base_name).id() == base_id)
    }
}

/// Returns true if `expr` contains `der(var_name)` anywhere in its tree.
pub fn expr_contains_der_of(expr: &Expression, var_name: &VarName) -> bool {
    let matcher = SingleDerivativeNameMatcher::from_var_name(var_name);
    let mut checker = ContainsSingleDerOfChecker {
        matcher,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

pub fn expr_contains_der_of_any(expr: &Expression, matcher: &DerivativeNameMatcher) -> bool {
    let mut checker = ContainsDerOfChecker {
        matcher,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct ContainsSingleDerOfChecker {
    matcher: SingleDerivativeNameMatcher,
    found: bool,
}

impl ExpressionVisitor for ContainsSingleDerOfChecker {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && args
                .first()
                .is_some_and(|arg| self.matcher.expression_refers_to_match(arg))
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

struct ContainsDerOfChecker<'a> {
    matcher: &'a DerivativeNameMatcher,
    found: bool,
}

impl ExpressionVisitor for ContainsDerOfChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && args
                .first()
                .is_some_and(|arg| self.matcher.expression_refers_to_match(arg))
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn embedded_subscripts_reject_float_syntax() {
        assert_eq!(parse_embedded_subscripts("x[1]"), Some(vec![1]));
        assert_eq!(parse_embedded_subscripts("x[1.0]"), None);
    }

    #[test]
    fn subscript_match_rejects_real_literal_indices() {
        let subscript = Subscript::generated_expr(Box::new(Expression::Literal {
            value: Literal::Real(1.0),
            span: rumoca_core::Span::DUMMY,
        }));
        assert!(!subscripts_all_one(std::slice::from_ref(&subscript)));
        assert!(!subscripts_match_indices(&[subscript], &[1]));
    }

    #[test]
    fn derivative_name_matcher_matches_indexed_and_embedded_state_refs() {
        let matcher = DerivativeNameMatcher::from_var_names([
            &VarName::new("x"),
            &VarName::new("support.phi"),
        ]);
        let indexed = Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: Reference::new("x"),
                subscripts: vec![Subscript::generated_index(2, rumoca_core::Span::DUMMY)],
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        };
        let embedded = Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: Reference::new("support[2].phi"),
                subscripts: Vec::new(),
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        };

        assert!(expr_contains_der_of_any(&indexed, &matcher));
        assert!(expr_contains_der_of_any(&embedded, &matcher));
    }

    #[test]
    fn derivative_name_matcher_requires_exact_indexed_state_refs() {
        let matcher = DerivativeNameMatcher::from_var_names([&VarName::new("x[2]")]);
        let other_index = Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![Expression::VarRef {
                name: Reference::new("x"),
                subscripts: vec![Subscript::generated_index(3, rumoca_core::Span::DUMMY)],
                span: rumoca_core::Span::DUMMY,
            }],
            span: rumoca_core::Span::DUMMY,
        };

        assert!(!expr_contains_der_of_any(&other_index, &matcher));
    }
}
