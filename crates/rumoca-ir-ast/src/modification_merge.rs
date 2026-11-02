//! MLS §7.2.3: a lower-precedence modification merged below a stated one.
use super::*;
use rumoca_core::OpBinary;
use std::sync::Arc;

/// What the nested arguments of a modified element are. Only the element's
/// declared type decides it (MLS 4.9): a primitive value takes attributes
/// (`start`, `unit`, ...), a structured one takes elements, whatever they are
/// named.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NestedArguments {
    Attributes,
    Elements,
}

/// Why a lower modification cannot be merged below a stated one. Each holds
/// the element's path below the merged one (`a`, `p.a`; empty for the merged
/// element itself).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MergeRefusal {
    /// The stated modification modifies an element the lower one makes
    /// `final` (MLS 7.2.6).
    Final(String),
    /// A stated binding meets nested arguments below it, and the element's
    /// type is not known, so neither is whether the binding supersedes them.
    Unclassified(String),
}

impl MergeRefusal {
    /// The path as seen from the element that contains `name`.
    #[must_use]
    pub fn within(self, name: &str) -> Self {
        let join = |inner: String| {
            if inner.is_empty() {
                name.to_string()
            } else {
                format!("{name}.{inner}")
            }
        };
        match self {
            Self::Final(inner) => Self::Final(join(inner)),
            Self::Unclassified(inner) => Self::Unclassified(join(inner)),
        }
    }

    fn within_path(self, path: &str) -> Self {
        if path.is_empty() {
            self
        } else {
            self.within(path)
        }
    }
}

/// What a merge asks about the elements it binds: their path below the
/// merged element (empty for the merged element itself).
pub type ClassifyNested<'a> = dyn Fn(&[String]) -> Option<NestedArguments> + 'a;

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
    ///
    /// A binding this one states replaces the whole value (MLS 7.2.3). Over a
    /// structured element it supersedes `default`'s modifications of the
    /// elements this one does not restate (`h(p = R(5, 4))` over `p(b = 3)`
    /// keeps `p.b = 4`). Over a primitive one, `default`'s attributes are not
    /// part of the value and still merge below it. `classify` says which an
    /// element is; where it cannot, the merge is refused.
    ///
    /// Both use one storage shape: a full argument (`x = e`, `x(...)`,
    /// `x(...) = e`) or, in a component's modification map, its value
    /// (`e`, `x(...)`, `x(...) = e`).
    ///
    /// MLS §7.2.6: an element `default` makes `final` cannot be modified
    /// again, by a modification of it or by a binding that replaces it. On
    /// any refusal `self` is partly merged and must not be used.
    pub fn merge_modification_below(
        &mut self,
        default: &Self,
        classify: &ClassifyNested<'_>,
    ) -> Result<(), MergeRefusal> {
        self.merge_below_at(default, &mut Vec::new(), classify)
    }

    fn merge_below_at(
        &mut self,
        default: &Self,
        path: &mut Vec<String>,
        classify: &ClassifyNested<'_>,
    ) -> Result<(), MergeRefusal> {
        if let Some(lower) = default.nested_modification() {
            if self.modification_binding().is_some() {
                return self.bind_over(lower, path, classify);
            }
            if let Some(stated) = self.nested_modification_mut() {
                merge_arguments_below(stated, lower, path, classify)?;
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

    /// `lower`'s nested arguments below this modification's binding, which
    /// replaces the whole value: a structured element's lower elements are
    /// superseded unless this modification restates them, and refused if
    /// final; a primitive element's attributes merge below as usual.
    fn bind_over(
        &mut self,
        lower: &Self,
        path: &mut Vec<String>,
        classify: &ClassifyNested<'_>,
    ) -> Result<(), MergeRefusal> {
        let at = || path.join(".");
        match classify(path) {
            None => Err(MergeRefusal::Unclassified(at())),
            Some(NestedArguments::Elements) => {
                let restated = self
                    .nested_modification()
                    .map(argument_names)
                    .unwrap_or_default();
                let superseded =
                    arguments_where(lower, |name| !restated.iter().any(|stated| stated == name));
                if let Some(element) = first_final(&superseded) {
                    return Err(MergeRefusal::Final(element).within_path(&at()));
                }
                let kept =
                    arguments_where(lower, |name| restated.iter().any(|stated| stated == name));
                match self.nested_modification_mut() {
                    Some(stated) => merge_arguments_below(stated, &kept, path, classify),
                    None => Ok(()),
                }
            }
            Some(NestedArguments::Attributes) => {
                if let Some(stated) = self.nested_modification_mut() {
                    return merge_arguments_below(stated, lower, path, classify);
                }
                let Some(binding) = self.modification_binding().filter(|_| has_arguments(lower))
                else {
                    return Ok(());
                };
                let merged = bound(lower, binding, self.span());
                *self = merged;
                Ok(())
            }
        }
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

/// The element names a class modification's arguments modify.
fn argument_names(modification: &Expression) -> Vec<String> {
    match modification {
        Expression::ClassModification { modifications, .. } => modifications
            .iter()
            .filter_map(|argument| argument.modification_target().map(ToString::to_string))
            .collect(),
        _ => Vec::new(),
    }
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
fn merge_arguments_below(
    stated: &mut Expression,
    lower: &Expression,
    path: &mut Vec<String>,
    classify: &ClassifyNested<'_>,
) -> Result<(), MergeRefusal> {
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
            Some(_) if lower_final.get(index).copied().unwrap_or(false) => {
                return Err(MergeRefusal::Final(name).within_path(&path.join(".")));
            }
            Some(stated) => {
                path.push(name);
                let merged = stated.merge_below_at(argument, path, classify);
                path.pop();
                merged?;
            }
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
