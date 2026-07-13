use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

pub(super) fn canonicalize_record_alias_expr(
    expr: &mut rumoca_core::Expression,
    ctx: &Context,
    known_variables: &HashSet<String>,
) {
    canonicalize_record_alias_expr_in_owner(expr, ctx, known_variables, None);
}

pub(super) fn canonicalize_record_alias_expr_in_owner(
    expr: &mut rumoca_core::Expression,
    ctx: &Context,
    known_variables: &HashSet<String>,
    owner: Option<&rumoca_core::ComponentPath>,
) {
    let mut rewriter = RecordAliasCanonicalizer {
        ctx,
        known_variables,
        owner,
    };
    *expr = rewriter.rewrite_expression(expr);
}

pub(super) fn canonicalize_record_alias_opt_expr_in_owner(
    expr: &mut Option<rumoca_core::Expression>,
    ctx: &Context,
    known_variables: &HashSet<String>,
    owner: &rumoca_core::ComponentPath,
) {
    if let Some(expr) = expr {
        canonicalize_record_alias_expr_in_owner(expr, ctx, known_variables, Some(owner));
    }
}

pub(super) fn canonicalize_record_alias_statements(
    statements: &mut [rumoca_core::Statement],
    ctx: &Context,
    known_variables: &HashSet<String>,
) {
    let mut rewriter = RecordAliasCanonicalizer {
        ctx,
        known_variables,
        owner: None,
    };
    for statement in statements {
        *statement = rewriter.rewrite_statement(statement);
    }
}

pub(super) fn canonicalize_record_alias_when_equations(
    equations: &mut [flat::WhenEquation],
    ctx: &Context,
    known_variables: &HashSet<String>,
) {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { value, .. } | flat::WhenEquation::Reinit { value, .. } => {
                canonicalize_record_alias_expr(value, ctx, known_variables);
            }
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                canonicalize_record_alias_expr(condition, ctx, known_variables);
                canonicalize_record_alias_expr(message, ctx, known_variables);
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (condition, branch_equations) in branches {
                    canonicalize_record_alias_expr(condition, ctx, known_variables);
                    canonicalize_record_alias_when_equations(
                        branch_equations,
                        ctx,
                        known_variables,
                    );
                }
                canonicalize_record_alias_when_equations(else_branch, ctx, known_variables);
            }
            flat::WhenEquation::FunctionCallOutputs { function, .. } => {
                canonicalize_record_alias_expr(function, ctx, known_variables);
            }
            flat::WhenEquation::Terminate { message, .. } => {
                canonicalize_record_alias_expr(message, ctx, known_variables);
            }
        }
    }
}

struct RecordAliasCanonicalizer<'a> {
    ctx: &'a Context,
    known_variables: &'a HashSet<String>,
    owner: Option<&'a rumoca_core::ComponentPath>,
}

impl ExpressionRewriter for RecordAliasCanonicalizer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if let rumoca_core::Expression::FieldAccess { base, field, span } = expr
            && let Some(rewritten) = self.rewrite_record_alias_field_access(base, field, *span)
        {
            return rewritten;
        }
        self.walk_expression(expr)
    }

    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
    ) -> rumoca_core::Expression {
        let rewritten_name = if subscripts.is_empty() {
            record_alias_rewrite_name(name.as_str(), self.ctx, self.known_variables, self.owner)
                .map(rumoca_core::Reference::new)
                .unwrap_or_else(|| name.clone())
        } else {
            name.clone()
        };
        rumoca_core::Expression::VarRef {
            name: rewritten_name,
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl RecordAliasCanonicalizer<'_> {
    fn rewrite_record_alias_field_access(
        &mut self,
        base: &rumoca_core::Expression,
        field: &str,
        span: rumoca_core::Span,
    ) -> Option<rumoca_core::Expression> {
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = base
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        let field_path = format!("{}.{}", name.as_str(), field);
        record_alias_rewrite_name(&field_path, self.ctx, self.known_variables, self.owner)
            .or_else(|| {
                owner_projected_record_field_candidate(
                    name.as_str(),
                    field,
                    self.owner,
                    self.known_variables,
                )
            })
            .map(|rewritten| rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(rewritten),
                subscripts: vec![],
                span,
            })
    }
}

impl StatementRewriter for RecordAliasCanonicalizer<'_> {}

pub(super) fn rewrite_name(
    name: &str,
    ctx: &Context,
    known_variables: &HashSet<String>,
    owner: Option<&rumoca_core::ComponentPath>,
) -> Option<String> {
    let name_path = rumoca_core::ComponentPath::from_flat_path(name);
    ctx.record_aliases.iter().find_map(|(alias, target)| {
        if !name_path.starts_with(alias) || name_path.len() == alias.len() {
            return None;
        }
        let suffix = name_path
            .suffix_from(alias.len())
            .expect("suffix index is in range");
        record_alias_candidate(target, alias, &suffix, known_variables, owner)
    })
}

fn owner_projected_record_field_candidate(
    base_name: &str,
    field: &str,
    owner: Option<&rumoca_core::ComponentPath>,
    known_variables: &HashSet<String>,
) -> Option<String> {
    let owner = owner?;
    let base = rumoca_core::ComponentPath::from_flat_path(base_name);
    let owner_parts = owner.parts();
    let (indexed_pos, subscript) =
        owner_parts
            .iter()
            .enumerate()
            .rev()
            .find_map(|(idx, part)| {
                component_part_subscript_suffix(part)
                    .filter(|suffix| !suffix.is_empty())
                    .map(|suffix| (idx, suffix))
            })?;
    if owner_parts.len() < 2 {
        return None;
    }
    if owner_parts.last().map(String::as_str) != Some(field) {
        return None;
    }
    let projected_leaf_pos = owner_parts.len() - 2;
    if projected_leaf_pos <= indexed_pos {
        return None;
    }
    let shared_prefix = owner.prefix(indexed_pos)?;
    if !base.starts_with(&shared_prefix) {
        return None;
    }
    let owner_sibling_candidate = rumoca_core::ComponentPath::from_parts(
        owner_parts[..=indexed_pos]
            .iter()
            .chain(owner_parts[projected_leaf_pos..].iter())
            .cloned(),
    )
    .to_flat_string();
    if owner_sibling_candidate != owner.as_str()
        && known_variables.contains(&owner_sibling_candidate)
    {
        return Some(owner_sibling_candidate);
    }
    let mut candidate_parts = owner_parts[..indexed_pos].to_vec();
    candidate_parts.push(format!("{}{}", owner_parts[projected_leaf_pos], subscript));
    if projected_leaf_pos + 1 < owner_parts.len() {
        candidate_parts.extend(owner_parts[projected_leaf_pos + 1..].iter().cloned());
    } else {
        candidate_parts.push(field.to_string());
    }
    let candidate = rumoca_core::ComponentPath::from_parts(candidate_parts).to_flat_string();
    if candidate == owner.as_str() {
        return None;
    }
    known_variables.contains(&candidate).then_some(candidate)
}

fn record_alias_candidate(
    target: &rumoca_core::ComponentPath,
    alias: &rumoca_core::ComponentPath,
    suffix: &rumoca_core::ComponentPath,
    known_variables: &HashSet<String>,
    owner: Option<&rumoca_core::ComponentPath>,
) -> Option<String> {
    let direct = target.join(suffix).to_flat_string();
    if known_variables.contains(&direct) {
        return Some(direct);
    }
    let alias_indexed_target = target_with_projected_alias_index(target, alias);
    if let Some(indexed_target) = alias_indexed_target {
        let indexed = indexed_target.join(suffix).to_flat_string();
        if known_variables.contains(&indexed) {
            return Some(indexed);
        }
    }
    let owner_indexed_target = target_with_owner_projected_index(target, owner?)?;
    let indexed = owner_indexed_target.join(suffix).to_flat_string();
    known_variables.contains(&indexed).then_some(indexed)
}

fn target_with_owner_projected_index(
    target: &rumoca_core::ComponentPath,
    owner: &rumoca_core::ComponentPath,
) -> Option<rumoca_core::ComponentPath> {
    target_with_projected_index(target, owner.parts())
}

fn target_with_projected_alias_index(
    target: &rumoca_core::ComponentPath,
    alias: &rumoca_core::ComponentPath,
) -> Option<rumoca_core::ComponentPath> {
    target_with_projected_index(target, alias.parts())
}

fn target_with_projected_index(
    target: &rumoca_core::ComponentPath,
    indexed_parts: &[String],
) -> Option<rumoca_core::ComponentPath> {
    let target_parts = target.parts();
    let last_target = target_parts.last()?;
    if component_part_has_subscript(last_target) {
        return None;
    }
    let subscript = indexed_parts.iter().rev().find_map(|part| {
        component_part_subscript_suffix(part).filter(|suffix| !suffix.is_empty())
    })?;
    let mut projected_parts = target_parts.to_vec();
    let last = projected_parts.last_mut()?;
    last.push_str(subscript);
    Some(rumoca_core::ComponentPath::from_parts(projected_parts))
}

fn component_part_has_subscript(part: &str) -> bool {
    component_part_subscript_suffix(part).is_some()
}

fn component_part_subscript_suffix(part: &str) -> Option<&str> {
    let start = part.find('[')?;
    part.ends_with(']').then_some(&part[start..])
}
