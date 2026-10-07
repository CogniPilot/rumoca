use rumoca_ir_ast as ast;

/// The component's own `annotation(Evaluate = ...)` literal, if it writes one.
///
/// MLS §18.6: `Evaluate = true` asks for the parameter's value to be used
/// during symbolic processing; `Evaluate = false` forbids it. An explicit
/// `false` therefore outranks every default that would otherwise evaluate the
/// parameter, `final` and an enclosing `Evaluate = true` included.
pub(crate) fn evaluate_annotation(comp: &ast::Component) -> Option<bool> {
    comp.annotation.iter().find_map(evaluate_literal)
}

fn evaluate_literal(anno_expr: &ast::Expression) -> Option<bool> {
    let (name_text, value) = match anno_expr {
        ast::Expression::NamedArgument { name, value, .. } => (name.text.as_ref(), value.as_ref()),
        ast::Expression::Modification { target, value, .. } => {
            (target.parts.first()?.ident.text.as_ref(), value.as_ref())
        }
        _ => return None,
    };
    if name_text != "Evaluate" {
        return None;
    }
    match value {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        },
        _ => None,
    }
}

/// A component's resolved MLS §18.6 `Evaluate` decision.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum EvaluateMark {
    /// Neither the component nor an enclosing one decides.
    #[default]
    Unmarked,
    /// Evaluated during translation: `Evaluate = true`, `final`, or an
    /// enclosing component evaluated.
    Evaluate,
    /// Never evaluated during translation: `Evaluate = false` on the
    /// component or on an enclosing one (a record parameter's fields).
    Refused,
}

impl EvaluateMark {
    /// The decision for a component from its own annotation, whether it is
    /// `final`, and the decision of the innermost enclosing component. Its
    /// own annotation wins; an enclosing refusal outranks `final`, as an
    /// explicit `Evaluate = false` does.
    pub(crate) fn resolve(own: Option<bool>, is_final: bool, inherited: Self) -> Self {
        match (own, inherited) {
            (Some(true), _) => Self::Evaluate,
            (Some(false), _) | (None, Self::Refused) => Self::Refused,
            (None, _) if is_final => Self::Evaluate,
            (None, inherited) => inherited,
        }
    }

    pub(crate) fn evaluates(self) -> bool {
        self == Self::Evaluate
    }

    pub(crate) fn refused(self) -> bool {
        self == Self::Refused
    }
}

#[cfg(test)]
mod tests {
    use super::EvaluateMark::{Evaluate, Refused, Unmarked};
    use super::*;

    #[test]
    fn the_innermost_decision_wins_and_a_refusal_outranks_final() {
        assert_eq!(EvaluateMark::resolve(None, false, Unmarked), Unmarked);
        assert_eq!(EvaluateMark::resolve(None, true, Unmarked), Evaluate);
        assert_eq!(EvaluateMark::resolve(None, false, Evaluate), Evaluate);
        // A field of an `Evaluate = false` record is refused, final or not.
        assert_eq!(EvaluateMark::resolve(None, false, Refused), Refused);
        assert_eq!(EvaluateMark::resolve(None, true, Refused), Refused);
        // The component's own annotation outranks every inherited decision.
        assert_eq!(EvaluateMark::resolve(Some(false), true, Evaluate), Refused);
        assert_eq!(EvaluateMark::resolve(Some(true), false, Refused), Evaluate);
    }
}
