//! The rank each actual argument carries into a differentiated call.
//!
//! Modelica vectorizes a call whose actual has more dimensions than its formal
//! declares (MLS 12.4.6): the call is then evaluated once per element of the
//! extra dimensions and its result gains them. A Jacobian wrapper states one
//! shape, `Real[m, n]` with `n` from the differentiated input and `m` from the
//! output (JAC-S3), and both halves are read from *declarations*. A vectorized
//! call therefore has a result the wrapper cannot state, and stating it anyway
//! mints a function whose declared output shape its own body does not produce.
//!
//! JAC-E1 runs expansion on document text before any tree is resolved, so the
//! only ranks available here are the ones a declaration in the call's own
//! lexical chain states. A rank this pass cannot read is refused rather than
//! assumed, which is JAC-T1 applied at the call boundary: this is the one
//! place a shape decision is made without a rank already in hand.

use rumoca_ir_ast as ast;

use crate::model::{FunctionModel, PortRole};
use crate::refusal::{Refusable, Refusal, Rule, Site};
use crate::sites::CallSite;

/// Refuse unless every actual argument has the rank its formal declares.
pub(crate) fn check_actual_ranks(
    definition: &ast::StoredDefinition,
    site: &CallSite,
    model: &FunctionModel,
    at: &Site,
) -> Refusable<()> {
    let chain = declaring_chain(definition, &site.owner);
    for (formal, actual) in model.ports_with(PortRole::Input).zip(&site.actuals) {
        let declared = formal.rank();
        let Some(carried) = rank(&chain, actual) else {
            return Err(Refusal::new(
                Rule::ActualShape,
                at.clone(),
                format!(
                    "`{actual}` is passed to `{}`, which is declared with rank {declared}, and \
                     this expansion cannot read the actual's own rank at the call site; an \
                     actual with more dimensions than its formal vectorizes the call \
                     (MLS 12.4.6), and the wrapper would state a shape the call's result does \
                     not have",
                    formal.name
                ),
            ));
        };
        if carried != declared {
            return Err(Refusal::new(
                Rule::ActualShape,
                at.clone(),
                format!(
                    "`{actual}` has rank {carried} but `{}` is declared with rank {declared}; \
                     Modelica vectorizes such a call (MLS 12.4.6), so its result is not the \
                     `Real[m, n]` JAC-S3 states and no wrapper can name that shape",
                    formal.name
                ),
            ));
        }
    }
    Ok(())
}

/// The classes that can declare a name at the call site, innermost first.
fn declaring_chain<'a>(
    definition: &'a ast::StoredDefinition,
    owner: &[String],
) -> Vec<&'a ast::ClassDef> {
    let mut chain = Vec::new();
    let mut classes = &definition.classes;
    for name in owner {
        let Some(class) = classes.get(name) else {
            break;
        };
        chain.push(class);
        classes = &class.classes;
    }
    chain.reverse();
    chain
}

/// The rank an actual argument carries, when the call site's own declarations
/// state one.
///
/// [`None`] is every form this pass cannot take apart from text alone, which
/// is a refusal and never a guess.
fn rank(chain: &[&ast::ClassDef], expression: &ast::Expression) -> Option<usize> {
    match expression {
        ast::Expression::Terminal { .. } => Some(0),
        ast::Expression::Parenthesized { inner, .. } => rank(chain, inner),
        ast::Expression::Unary { rhs, .. } => rank(chain, rhs),
        ast::Expression::ComponentReference(comp) => reference_rank(chain, comp),
        ast::Expression::Array { elements, .. } => {
            let held = rank(chain, elements.first()?)?;
            elements
                .iter()
                .all(|element| rank(chain, element) == Some(held))
                .then_some(held + 1)
        }
        _ => None,
    }
}

/// The rank a reference leaves after its subscripts.
fn reference_rank(chain: &[&ast::ClassDef], comp: &ast::ComponentReference) -> Option<usize> {
    let [part] = comp.parts.as_slice() else {
        return None;
    };
    let declared = declared_rank(chain, part.ident.text.as_ref())?;
    let subscripts = part.subs.as_deref().unwrap_or_default();
    for subscript in subscripts {
        // A subscript that is not one index keeps a dimension whose extent
        // this pass does not track, so the rank it leaves is not stated.
        match subscript {
            ast::Subscript::Expression(index) if rank(chain, index) == Some(0) => {}
            _ => return None,
        }
    }
    declared.checked_sub(subscripts.len())
}

/// The declared rank of a name, taking the innermost declaration of it.
fn declared_rank(chain: &[&ast::ClassDef], name: &str) -> Option<usize> {
    chain
        .iter()
        .find_map(|class| class.components.get(name))
        .map(|component| component.shape_expr.len())
}
