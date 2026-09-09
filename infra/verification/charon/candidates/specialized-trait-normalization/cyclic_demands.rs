//! Shorten associated-type paths only across equal, source-constrained predicates.
use super::{AssocTypePath, BaseClause, TypeConstraintSet};
use crate::ast::*;
use derive_generic_visitor::Visitor;
use rustc_hash::FxHashSet;

fn base_ref(root: &TraitDecl, base: BaseClause) -> Option<TraitRef> {
    match base {
        BaseClause::SelfClause => Some(TraitRef::new(
            TraitRefKind::SelfId,
            RegionBinder::empty(TraitDeclRef {
                id: root.def_id,
                generics: Box::new(root.generics.identity_args()),
            }),
        )),
        BaseClause::Local(var) => Some(
            root.generics
                .trait_clauses
                .get(var.bound_at_depth(DeBruijnId::zero())?)?
                .identity_tref(),
        ),
    }
}

fn repeats_trait(
    krate: &TranslatedCrate,
    mut id: TraitDeclId,
    path: &[TraitClauseId],
) -> Option<bool> {
    let mut seen = FxHashSet::default();
    seen.insert(id);
    for clause in path {
        id = krate
            .trait_decls
            .get(id)?
            .implied_clauses
            .get(*clause)?
            .trait_
            .skip_binder
            .id;
        if !seen.insert(id) {
            return Some(true);
        }
    }
    Some(false)
}

fn source_constraints(params: &GenericParams) -> TypeConstraintSet {
    // Do not use the existing lifetime-erasing approximation to justify equality.
    let constraints = params
        .trait_type_constraints
        .iter()
        .filter(|constraint| constraint.regions.is_empty())
        .cloned()
        .collect();
    TypeConstraintSet::from_constraints(&constraints)
}

fn add_source_constraints(
    constraints: &mut TypeConstraintSet,
    declaration: &TraitDecl,
    tref: &TraitRef,
) {
    let inherited = declaration
        .generics
        .trait_type_constraints
        .iter()
        .filter(|constraint| constraint.regions.is_empty())
        .cloned()
        .map(|constraint| constraint.substitute_with_tref(tref))
        .collect();
    for (path, ty) in TypeConstraintSet::from_constraints(&inherited).iter() {
        constraints.insert_path(&path, ty);
    }
}

#[derive(Visitor)]
struct NormalizePredicate<'a> {
    constraints: &'a TypeConstraintSet,
    depth: DeBruijnId,
    active: FxHashSet<(TraitRef, AssocTypeId)>,
}

impl VisitorWithBinderDepth for NormalizePredicate<'_> {
    fn binder_depth_mut(&mut self) -> &mut DeBruijnId {
        &mut self.depth
    }
}

impl VisitAstMut for NormalizePredicate<'_> {
    fn visit<T: AstVisitable>(&mut self, value: &mut T) -> ControlFlow<Self::Break> {
        VisitWithBinderDepth::new(self).visit(value)
    }

    fn visit_ty(&mut self, ty: &mut Ty) -> ControlFlow<Self::Break> {
        if let TyKind::TraitType(tref, type_id, args) = ty.kind()
            && args.is_empty()
            && let Some(tref) = tref.clone().move_from_under_binders(self.depth)
            && let Some(path) = tref.to_path()
            && let Some(replacement) = self.constraints.find(&AssocTypePath {
                tref: path,
                type_id: *type_id,
            })
        {
            let replacement = replacement.clone().move_under_binders(self.depth);
            let original = ty.clone();
            let key = (tref, *type_id);
            if replacement != original && self.active.insert(key.clone()) {
                *ty = replacement;
                let result = self.visit(ty);
                self.active.remove(&key);
                return result;
            }
            return ControlFlow::Continue(());
        }
        self.visit_inner(ty)
    }
}

fn normalized_predicate(
    tref: &TraitRef,
    constraints: &TypeConstraintSet,
) -> Option<PolyTraitDeclRef> {
    if !tref.trait_decl_ref.regions.is_empty() {
        return None;
    }
    let mut predicate = tref.trait_decl_ref.clone();
    let _ = NormalizePredicate {
        constraints,
        depth: DeBruijnId::zero(),
        active: FxHashSet::default(),
    }
    .visit(&mut predicate);
    Some(predicate)
}

/// `None` means an unresolved cycle, not a proof that the path is infinite.
/// Every successful rewrite deletes a nonempty segment with equal endpoint
/// predicates. Exact predicate equality uses declared equations and preserves
/// trait identity, arguments and binders; it does not assume an inverse Try law.
pub(super) fn normalize_path(
    krate: &TranslatedCrate,
    root: &TraitDecl,
    path: &AssocTypePath,
) -> Option<AssocTypePath> {
    let base = base_ref(root, path.tref.base)?;
    let mut path = path.clone();
    while repeats_trait(krate, base.trait_id(), &path.tref.parent_path)? {
        let mut constraints = source_constraints(&root.generics);
        let mut ancestors = vec![base.clone()];
        let mut shortening = None;
        for (index, clause_id) in path.tref.parent_path.iter().enumerate() {
            let current = ancestors.last()?;
            let declaration = krate.trait_decls.get(current.trait_id())?;
            add_source_constraints(&mut constraints, declaration, current);
            let clause = declaration.implied_clauses.get(*clause_id)?;
            let next = TraitRef::new(
                TraitRefKind::ParentClause(Box::new(current.clone()), *clause_id),
                clause
                    .trait_
                    .clone()
                    .try_substitute_with_tref(current)
                    .ok()?,
            );
            let predicate = normalized_predicate(&next, &constraints)?;
            for (ancestor_index, ancestor) in ancestors.iter().enumerate() {
                if normalized_predicate(ancestor, &constraints)? == predicate {
                    shortening = Some((ancestor_index, index + 1));
                    break;
                }
            }
            if shortening.is_some() {
                break;
            }
            ancestors.push(next);
        }
        let (start, end) = shortening?;
        path.tref.parent_path.drain(start..end);
    }
    Some(path)
}

pub(super) fn path_type(
    krate: &TranslatedCrate,
    root: &TraitDecl,
    path: &AssocTypePath,
) -> Option<Ty> {
    let base = base_ref(root, path.tref.base)?;
    let mut relative = path.clone();
    relative.tref.base = BaseClause::SelfClause;
    relative.on_real_tref(krate, base)
}
