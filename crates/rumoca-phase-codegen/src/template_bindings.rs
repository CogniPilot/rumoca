//! Target-neutral template bindings.
//!
//! Phase codegen receives a set of named bindings from the compiler and proves
//! only three structural facts about them: every name is a template identifier,
//! the names are unique, and no name collides with a semantic root at render
//! time. It assigns the names no meaning. Binding a name to a checked artifact
//! fact, an artifact identity, or an output path is the compiler's exclusive
//! responsibility; this crate can neither construct nor recover any of them.

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;

use minijinja::Value;
use rumoca_core::TargetInvocationBrand;

/// One immutable value bound to a template name.
///
/// A binding is either flat text or a nested object of further bindings. There
/// is no sequence or numeric leaf: the compiler flattens every scalar fact to
/// text, and the only structured value is the immutable artifact projection.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateBindingValue {
    /// A flat text leaf.
    Text(String),
    /// A nested map of further named bindings.
    Object(BTreeMap<String, TemplateBindingValue>),
}

impl TemplateBindingValue {
    fn to_render_value(&self) -> Value {
        match self {
            Self::Text(text) => Value::from(text.as_str()),
            Self::Object(entries) => entries
                .iter()
                .map(|(name, value)| (name.clone(), value.to_render_value()))
                .collect(),
        }
    }
}

/// Failure to bind one name into a [`TemplateBindings`] set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateBindingError {
    /// The name is not a template identifier `[A-Za-z_][A-Za-z0-9_]*`.
    NonIdentifierName(String),
    /// A binding with this name already exists in the set.
    DuplicateName(String),
}

impl fmt::Display for TemplateBindingError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonIdentifierName(name) => write!(
                formatter,
                "template binding name '{name}' is not a template identifier"
            ),
            Self::DuplicateName(name) => {
                write!(formatter, "template binding name '{name}' is already bound")
            }
        }
    }
}

impl std::error::Error for TemplateBindingError {}

/// Target-neutral named bindings supplied to one template render.
///
/// This carrier is deliberately not serializable: a template reads a bound name
/// only after [`render_context`](Self::render_context) has joined it onto the
/// disjoint semantic roots, and nothing here enumerates the whole set as one
/// value.
///
/// The crate path resolves for the neutral carrier:
///
/// ```
/// use rumoca_phase_codegen::TemplateBindings;
/// ```
///
/// The target-aware carrier and its identity grammar are gone, with no shim:
///
/// ```compile_fail
/// use rumoca_phase_codegen::TemplateArtifactFacts;
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::template_artifact_facts;
/// ```
///
/// ```compile_fail
/// use rumoca_phase_codegen::artifact_identity_template_name;
/// ```
#[derive(Debug)]
pub struct TemplateBindings<'inv> {
    _brand: TargetInvocationBrand<'inv>,
    entries: BTreeMap<String, TemplateBindingValue>,
}

impl<'inv> TemplateBindings<'inv> {
    /// Begin an empty binding set for one branded target invocation.
    #[must_use]
    pub fn construct(brand: TargetInvocationBrand<'inv>) -> Self {
        Self {
            _brand: brand,
            entries: BTreeMap::new(),
        }
    }

    /// Bind one value to a template name.
    ///
    /// The name must be a template identifier and must not already be bound. No
    /// other property of the name is interpreted here.
    pub fn bind(
        &mut self,
        name: impl Into<String>,
        value: TemplateBindingValue,
    ) -> Result<(), TemplateBindingError> {
        let name = name.into();
        if !is_template_identifier(&name) {
            return Err(TemplateBindingError::NonIdentifierName(name));
        }
        match self.entries.entry(name) {
            Entry::Occupied(entry) => Err(TemplateBindingError::DuplicateName(entry.key().clone())),
            Entry::Vacant(entry) => {
                entry.insert(value);
                Ok(())
            }
        }
    }

    /// Join the bound names onto the disjoint semantic roots.
    ///
    /// Every bound name is proven disjoint from every semantic root: a semantic
    /// root that reuses a bound name is rejected, so a binding can never shadow
    /// a semantic value and a semantic value can never forge a binding. Bound
    /// names are already unique among themselves, so the second insertion pass
    /// never overwrites a semantic value.
    pub(crate) fn render_context(&self, semantic: Value) -> Result<Value, minijinja::Error> {
        let mut context: BTreeMap<String, Value> = BTreeMap::new();
        for key in semantic.try_iter()? {
            let Some(name) = key.as_str() else {
                return Err(minijinja::Error::new(
                    minijinja::ErrorKind::InvalidOperation,
                    "target template semantic context contains a non-string top-level key",
                ));
            };
            if self.entries.contains_key(name) {
                return Err(minijinja::Error::new(
                    minijinja::ErrorKind::InvalidOperation,
                    format!(
                        "target template semantic context collides with reserved top-level name '{name}'"
                    ),
                ));
            }
            context.insert(name.to_owned(), semantic.get_item(&key)?);
        }
        for (name, value) in &self.entries {
            context.insert(name.clone(), value.to_render_value());
        }
        Ok(context.into_iter().collect())
    }
}

fn is_template_identifier(name: &str) -> bool {
    let mut bytes = name.bytes();
    matches!(bytes.next(), Some(b'A'..=b'Z' | b'a'..=b'z' | b'_'))
        && bytes.all(|byte| matches!(byte, b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'_'))
}

#[cfg(test)]
mod tests;
