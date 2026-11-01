//! MLS §7.2.3: a lower-precedence modification merged below a stated one.
use super::*;
use rumoca_core::OpBinary;
use std::sync::Arc;

impl Expression {
    /// The element a modification argument names: `x = e`, `x(...)` or
    /// `x(...) = e`.
    pub fn modification_target(&self) -> Option<&ComponentReference> {
        match self {
            Self::Modification { target, .. } | Self::ClassModification { target, .. } => {
                Some(target)
            }
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                ..
            } => match lhs.as_ref() {
                Self::ClassModification { target, .. } => Some(target),
                _ => None,
            },
            _ => None,
        }
    }

    /// Merge `default`, a lower-precedence modification of the same element,
    /// below this one: what this modification states wins, nested arguments
    /// merge argument by argument, and a binding only `default` states is kept.
    /// A binding this one states replaces the whole value, so it supersedes
    /// `default`'s element modifications (`h(p = R(5, 4))` over `p(b = 3)`
    /// keeps `p.b = 4`); `default`'s attributes (`start`, `unit`, ...) are
    /// not part of the value and still merge below it.
    ///
    /// Both use one storage shape: a full argument (`x = e`, `x(...)`,
    /// `x(...) = e`) or, in a component's modification map, its value
    /// (`e`, `x(...)`, `x(...) = e`).
    ///
    /// MLS §7.2.6: an element `default` makes `final` cannot be modified
    /// again. If this modification states one, or states a binding over a
    /// final element of the value, the merge fails with that
    /// element's path below the merged one (`a`, `p.a`); `self` is then
    /// partly merged and must not be used.
    pub fn merge_modification_below(&mut self, default: &Self) -> Result<(), String> {
        if let Some(lower) = default.nested_modification() {
            if let Some(stated) = self.nested_modification_mut() {
                merge_arguments_below(stated, lower)?;
            } else if self.modification_binding().is_some() {
                return self.bind_over(lower);
            }
        }
        if self.modification_binding().is_none()
            && let Some(binding) = default.modification_binding()
            && let Some(stated) = self.nested_modification()
        {
            let merged = bound(stated, binding, self.span());
            *self = merged;
        }
        Ok(())
    }

    /// A stated binding replaces the whole value, so it modifies every
    /// element of `lower`, final ones included, and supersedes the rest.
    /// Only `lower`'s attributes remain to merge below it.
    fn bind_over(&mut self, lower: &Self) -> Result<(), String> {
        first_final(&arguments_where(lower, |name| !is_attribute(name))).map_or(Ok(()), Err)?;
        let attributes = arguments_where(lower, is_attribute);
        let Some(binding) = self
            .modification_binding()
            .filter(|_| has_arguments(&attributes))
        else {
            return Ok(());
        };
        let merged = bound(&attributes, binding, self.span());
        *self = merged;
        Ok(())
    }

    /// The `x(...)` part of a modification, if it has one.
    fn nested_modification(&self) -> Option<&Self> {
        match self {
            Self::ClassModification { .. } => Some(self),
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                ..
            } if matches!(lhs.as_ref(), Self::ClassModification { .. }) => Some(lhs),
            Self::Modification { value, .. }
                if matches!(value.as_ref(), Self::ClassModification { .. }) =>
            {
                Some(value)
            }
            _ => None,
        }
    }

    fn nested_modification_mut(&mut self) -> Option<&mut Self> {
        match self {
            Self::ClassModification { .. } => Some(self),
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                ..
            } if matches!(lhs.as_ref(), Self::ClassModification { .. }) => Some(Arc::make_mut(lhs)),
            Self::Modification { value, .. }
                if matches!(value.as_ref(), Self::ClassModification { .. }) =>
            {
                Some(Arc::make_mut(value))
            }
            _ => None,
        }
    }

    /// The value a modification binds, if it binds one.
    fn modification_binding(&self) -> Option<&Self> {
        match self {
            Self::ClassModification { .. } => None,
            Self::Binary {
                op: OpBinary::Assign,
                lhs,
                rhs,
                ..
            } if matches!(lhs.as_ref(), Self::ClassModification { .. }) => Some(rhs),
            Self::Modification { value, .. } => {
                (!matches!(value.as_ref(), Self::ClassModification { .. })).then_some(value)
            }
            value => Some(value),
        }
    }
}

/// The path of the first element a class modification makes `final`, at any
/// depth of its nested arguments.
fn first_final(modification: &Expression) -> Option<String> {
    let Expression::ClassModification {
        modifications,
        final_flags,
        ..
    } = modification
    else {
        return None;
    };
    modifications
        .iter()
        .enumerate()
        .find_map(|(index, argument)| {
            let name = argument.modification_target()?.to_string();
            if final_flags.get(index).copied().unwrap_or(false) {
                return Some(name);
            }
            let inner = first_final(argument.nested_modification()?)?;
            Some(format!("{name}.{inner}"))
        })
}

/// A predefined attribute of a primitive type (MLS §4.9): metadata of a
/// value, not an element of it.
fn is_attribute(name: &str) -> bool {
    rumoca_core::PREDEFINED_COMPONENT_ATTRIBUTES.contains(&name)
}

/// The arguments of a class modification whose element name `keep` accepts,
/// with their flags.
fn arguments_where(modification: &Expression, keep: impl Fn(&str) -> bool) -> Expression {
    let mut kept = modification.clone();
    if let Expression::ClassModification {
        modifications,
        each_flags,
        final_flags,
        redeclare_flags,
        ..
    } = &mut kept
    {
        let selected: Vec<bool> = modifications
            .iter()
            .map(|argument| {
                argument
                    .modification_target()
                    .is_some_and(|target| keep(&target.to_string()))
            })
            .collect();
        retain_selected(modifications, &selected);
        for flags in [each_flags, final_flags, redeclare_flags] {
            retain_selected(flags, &selected);
        }
    }
    kept
}

fn retain_selected<T>(items: &mut Vec<T>, selected: &[bool]) {
    let mut index = 0;
    items.retain(|_| {
        index += 1;
        selected.get(index - 1).copied().unwrap_or(false)
    });
}

fn has_arguments(modification: &Expression) -> bool {
    matches!(modification, Expression::ClassModification { modifications, .. } if !modifications.is_empty())
}

/// `nested = binding`, the stored form of `x(...) = e`.
fn bound(nested: &Expression, binding: &Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Assign,
        lhs: Arc::new(nested.clone()),
        rhs: Arc::new(binding.clone()),
        span,
    }
}

/// Append each argument of `lower` whose element `stated` does not modify,
/// and merge the ones it does, refusing one that `lower` makes `final`.
fn merge_arguments_below(stated: &mut Expression, lower: &Expression) -> Result<(), String> {
    let (
        Expression::ClassModification {
            modifications,
            each_flags,
            final_flags,
            redeclare_flags,
            ..
        },
        Expression::ClassModification {
            modifications: lower_modifications,
            each_flags: lower_each,
            final_flags: lower_final,
            redeclare_flags: lower_redeclare,
            ..
        },
    ) = (stated, lower)
    else {
        return Ok(());
    };
    for (index, argument) in lower_modifications.iter().enumerate() {
        let target = argument.modification_target().map(ToString::to_string);
        let existing = target.as_ref().and_then(|target| {
            modifications.iter_mut().find(|stated| {
                stated
                    .modification_target()
                    .map(ToString::to_string)
                    .as_ref()
                    == Some(target)
            })
        });
        let name = target.unwrap_or_default();
        match existing {
            Some(_) if lower_final.get(index).copied().unwrap_or(false) => return Err(name),
            Some(stated) => stated
                .merge_modification_below(argument)
                .map_err(|inner| format!("{name}.{inner}"))?,
            None => {
                let count = modifications.len();
                for flags in [&mut *each_flags, &mut *final_flags, &mut *redeclare_flags] {
                    flags.resize(count, false);
                }
                modifications.push(argument.clone());
                each_flags.push(lower_each.get(index).copied().unwrap_or(false));
                final_flags.push(lower_final.get(index).copied().unwrap_or(false));
                redeclare_flags.push(lower_redeclare.get(index).copied().unwrap_or(false));
            }
        }
    }
    Ok(())
}
