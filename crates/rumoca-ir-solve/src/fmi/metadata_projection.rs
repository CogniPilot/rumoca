//! Canonical borrowed template projections of the checked FMI inventory.

use super::metadata::{FmiNumericAttribute, FmiVariable};
use crate::SolveInitialValues;
use serde::Serialize;
use serde::ser::{SerializeMap, SerializeSeq, Serializer};

/// One borrowed inventory encoder, available only through admitted codegen views.
pub(super) struct SerializedFmiVariables<'inventory> {
    variables: &'inventory [FmiVariable],
}

impl<'inventory> SerializedFmiVariables<'inventory> {
    pub(super) const fn borrowing(variables: &'inventory [FmiVariable]) -> Self {
        Self { variables }
    }
}

impl Serialize for SerializedFmiVariables<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_seq(self.variables.iter().map(SerializedFmiVariable))
    }
}

/// One entry of the encoding above, in the inventory's own order.
///
/// Rendering reads the same entry the checked views expose: the derived scalar
/// identities rather than the stored ones, the typed backing owner, and a
/// `start` key only where the entry has one to give.
pub(super) struct SerializedFmiVariable<'entry>(pub(super) &'entry FmiVariable);

impl Serialize for SerializedFmiVariable<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut entry = serializer.serialize_map(None)?;
        self.serialize_entries(&mut entry)?;
        entry.end()
    }
}

impl SerializedFmiVariable<'_> {
    pub(super) fn serialize_entries<M: SerializeMap>(&self, entry: &mut M) -> Result<(), M::Error> {
        let variable = self.0;
        entry.serialize_entry("name", &variable.name)?;
        entry.serialize_entry("scalar_names", &ScalarNames(variable))?;
        entry.serialize_entry("value_kind", &variable.value_kind)?;
        entry.serialize_entry("fmi3_type", variable.fmi3_type())?;
        entry.serialize_entry("fmi2_integer_admitted", &variable.fmi2_integer_admitted())?;
        entry.serialize_entry("dimensions", &variable.dimensions)?;
        variable.backing.serialize_into(entry)?;
        for attribute in FmiNumericAttribute::ALL {
            if matches!(
                attribute,
                FmiNumericAttribute::Start | FmiNumericAttribute::StartFmi3
            ) {
                self.serialize_start(entry, attribute)?;
                continue;
            }
            self.serialize_numeric(entry, attribute)?;
        }
        entry.serialize_entry(
            "minimum_fmi3",
            &variable.minimum().and_then(|values| {
                source_numeric_values(values).reduce(|a, b| if b < a { b } else { a })
            }),
        )?;
        entry.serialize_entry(
            "maximum_fmi3",
            &variable.maximum().and_then(|values| {
                source_numeric_values(values).reduce(|a, b| if b > a { b } else { a })
            }),
        )?;
        entry.serialize_entry(
            "nominal_fmi3",
            &variable.nominal().and_then(uniform_numeric_value),
        )?;
        entry.serialize_entry("text_start", &variable.text_start)?;
        entry.serialize_entry("enumeration", &variable.enumeration)?;
        entry.serialize_entry("unit", &variable.unit)?;
        entry.serialize_entry("description", &variable.description)?;
        entry.serialize_entry("causality", &variable.causality)?;
        entry.serialize_entry("declared_causality", &variable.declared_causality)?;
        entry.serialize_entry("variability", &variable.variability)?;
        entry.serialize_entry("initial", &variable.initial)?;
        entry.serialize_entry("initial_unknown", &variable.is_initial_unknown())?;
        entry.serialize_entry("tunable", &variable.tunable)?;
        entry.serialize_entry("declaration", &variable.declaration)?;
        entry.serialize_entry("value_reference_fmi3", &variable.value_reference_fmi3)?;
        Ok(())
    }

    fn serialize_numeric<M: SerializeMap>(
        &self,
        entry: &mut M,
        attribute: FmiNumericAttribute,
    ) -> Result<(), M::Error> {
        let variable = self.0;
        let values = match variable.numeric_attribute(attribute) {
            Some(values) => {
                let count = variable.numeric_attribute_count(attribute).ok_or_else(|| {
                    <M::Error as serde::ser::Error>::custom("numeric attribute has no extent")
                })?;
                Some(NumericValues(values, count))
            }
            None => None,
        };
        entry.serialize_entry(attribute.name(), &values)
    }

    fn serialize_start<M: SerializeMap>(
        &self,
        entry: &mut M,
        attribute: FmiNumericAttribute,
    ) -> Result<(), M::Error> {
        let variable = self.0;
        if let Some(values) = variable.typed_start() {
            return entry.serialize_entry(
                attribute.name(),
                &StartValues(
                    values,
                    variable.numeric_attribute_count(attribute).ok_or_else(|| {
                        <M::Error as serde::ser::Error>::custom("start has no extent")
                    })?,
                ),
            );
        }
        if variable.value_kind() == crate::SolveVariableValueKind::String
            && variable.initial() != Some(super::FmiInitial::Calculated)
            && variable.causality() != super::FmiCausality::Independent
            && let Some(values) = variable.text_start()
        {
            entry.serialize_entry(attribute.name(), values)?;
        }
        Ok(())
    }
}

struct ScalarNames<'a>(&'a FmiVariable);

impl Serialize for ScalarNames<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.collect_seq(self.0.scalar_names())
    }
}

struct NumericValues<'a>(&'a SolveInitialValues, usize);

struct StartValues<'a>(&'a crate::SolveDeclarationStart, usize);
impl Serialize for StartValues<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self.0 {
            crate::SolveDeclarationStart::Real(values) => {
                NumericValues(values, self.1).serialize(serializer)
            }
            crate::SolveDeclarationStart::Integer(values)
            | crate::SolveDeclarationStart::Enumeration(values) => {
                serialize_values(serializer, self.1, |ordinal| values.value(ordinal))
            }
            crate::SolveDeclarationStart::Boolean(values) => {
                serialize_values(serializer, self.1, |ordinal| values.value(ordinal))
            }
            crate::SolveDeclarationStart::Missing { .. } => serializer.serialize_none(),
        }
    }
}

impl Serialize for NumericValues<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serialize_values(serializer, self.1, |ordinal| self.0.value(ordinal))
    }
}

fn serialize_values<S: Serializer, T: Serialize>(
    serializer: S,
    count: usize,
    value: impl Fn(usize) -> Option<T>,
) -> Result<S::Ok, S::Error> {
    let mut sequence = serializer.serialize_seq(Some(count))?;
    for ordinal in 0..count {
        let value = value(ordinal).ok_or_else(|| {
            serde::ser::Error::custom("FMI value extent exceeds its source owner")
        })?;
        sequence.serialize_element(&value)?;
    }
    sequence.end()
}

fn source_numeric_values(values: &SolveInitialValues) -> impl Iterator<Item = f64> + '_ {
    values.runs().flat_map(|run| {
        run.repeated_bits()
            .into_iter()
            .chain(run.literal_bits().unwrap_or(&[]).iter().copied())
            .map(f64::from_bits)
    })
}

fn uniform_numeric_value(values: &SolveInitialValues) -> Option<f64> {
    let mut source = source_numeric_values(values);
    let first = source.next()?;
    source.all(|value| value == first).then_some(first)
}

#[cfg(test)]
mod tests {
    use super::{NumericValues, SolveInitialValues};

    #[test]
    fn numeric_projection_refuses_extent_beyond_the_source_owner() {
        let values = SolveInitialValues::repeat(-0.0, 2).unwrap();
        assert_eq!(
            serde_json::to_string(&NumericValues(&values, 2)).unwrap(),
            "[-0.0,-0.0]"
        );
        let error = serde_json::to_string(&NumericValues(&values, 3)).unwrap_err();
        assert!(
            error
                .to_string()
                .contains("FMI value extent exceeds its source owner")
        );
    }
}
