//! Print unmaterialized `for` families of a Flat model as `for` loops.
//!
//! Flatten may keep a source `for` equation as a structured family whose
//! interior rows are placeholders (`FamilyInteriors` other than
//! `Materialized`); only its comprehension template states the body. A Flat
//! template that reads [`CONTEXT_KEY`] gets the equation partitions as rows in
//! which each such family is one `for` loop over its template body, so the
//! family's placeholder rows are never read. Templates that read only
//! `flat.equations` keep the materialized-view requirement.

use rumoca_core::ComprehensionScalarView;
use rumoca_ir_flat as flat;
use serde_json::{Value, json};

/// The render-context key of the loop-aware equation rows.
pub(super) const CONTEXT_KEY: &str = "flat_equation_rows";

/// Whether `family` can be printed as one `for` loop: its template body is the
/// scalar body of each domain point, written over the domain's binders.
pub(super) fn printable(family: &flat::StructuredEquationFamily) -> bool {
    family.template.as_ref().is_some_and(|template| {
        template.scalar_view == ComprehensionScalarView::BinderSubstitution
            && template.body.len() == family.equations_per_point
            && !family.domain.binders.is_empty()
    })
}

/// `{"regular": rows, "initial": rows}` for the two equation partitions.
pub(super) fn context_value(model: &flat::Model) -> Value {
    json!({
        "regular": rows(&model.equations, &model.structured_equations),
        "initial": rows(&model.initial_equations, &model.initial_structured_equations),
    })
}

/// The partition in order: `{"equation": eq}` for each row outside an
/// unmaterialized printable family, and one `{"loop": ...}` in place of each
/// such family's rows.
fn rows(equations: &[flat::Equation], families: &[flat::StructuredEquationFamily]) -> Vec<Value> {
    let mut loops = families
        .iter()
        .filter(|family| !family.interiors_materialized() && printable(family))
        .filter_map(|family| Some((family.materialized_rows()?, family)))
        .collect::<Vec<_>>();
    loops.sort_by_key(|(range, _)| range.start);
    let mut loops = loops.into_iter().peekable();
    let mut rows = Vec::with_capacity(equations.len());
    let mut index = 0;
    while index < equations.len() {
        if let Some((range, family)) = loops.next_if(|(range, _)| range.start == index) {
            rows.push(json!({ "loop": loop_value(family) }));
            index = range.end.max(index + 1);
            continue;
        }
        rows.push(json!({ "equation": equations[index] }));
        index += 1;
    }
    rows
}

/// One `for i in lower:step:upper loop` header per binder (outermost first)
/// and the template body as residual equations.
fn loop_value(family: &flat::StructuredEquationFamily) -> Value {
    let headers = family
        .domain
        .binders
        .iter()
        .map(|binder| {
            let range = match binder.step {
                1 => format!("{}:{}", binder.lower, binder.upper),
                step => format!("{}:{}:{}", binder.lower, step, binder.upper),
            };
            format!("for {} in {range} loop", binder.display_name)
        })
        .collect::<Vec<_>>();
    let equations = family
        .template
        .iter()
        .flat_map(|template| template.body.iter())
        .map(|residual| flat::Equation::new(residual.clone(), family.span, family.origin.clone()))
        .collect::<Vec<_>>();
    json!({ "headers": headers, "equations": equations })
}
