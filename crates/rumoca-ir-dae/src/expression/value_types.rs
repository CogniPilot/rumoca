use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ScalarType {
    Real,
    Integer,
    Enumeration,
    Boolean,
    String,
    Record,
}

impl std::hash::Hash for ScalarType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let tag = match self {
            Self::Real => 0,
            Self::Integer => 1,
            Self::Enumeration => 2,
            Self::Boolean => 3,
            Self::String => 4,
            Self::Record => 5,
        };
        state.write_u8(tag);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ExpressionVariability {
    Constant,
    Parameter,
    Discrete,
    Continuous,
}

impl ScalarType {
    pub(crate) fn is_numeric(self) -> bool {
        matches!(self, Self::Real | Self::Integer)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(deny_unknown_fields)]
pub struct ValueType {
    scalar: ScalarType,
    dimensions: Box<[u32]>,
    record_name: Option<rumoca_core::VarName>,
    record_fields: Box<[RecordFieldType]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct RecordFieldType {
    name: rumoca_core::VarName,
    value_type: u32,
}

impl ValueType {
    pub fn scalar(scalar: ScalarType) -> Self {
        Self {
            scalar,
            dimensions: Box::new([]),
            record_name: None,
            record_fields: Box::new([]),
        }
    }

    pub fn array(scalar: ScalarType, dimensions: impl Into<Box<[u32]>>) -> Self {
        Self {
            scalar,
            dimensions: dimensions.into(),
            record_name: None,
            record_fields: Box::new([]),
        }
    }

    pub(crate) fn record(
        name: rumoca_core::VarName,
        fields: impl Into<Box<[RecordFieldType]>>,
    ) -> Self {
        Self {
            scalar: ScalarType::Record,
            dimensions: Box::new([]),
            record_name: Some(name),
            record_fields: fields.into(),
        }
    }

    pub const fn scalar_type(&self) -> ScalarType {
        self.scalar
    }

    pub fn dimensions(&self) -> &[u32] {
        &self.dimensions
    }

    pub fn is_scalar(&self) -> bool {
        self.scalar != ScalarType::Record && self.dimensions.is_empty()
    }

    pub fn scalar_count(&self) -> Option<usize> {
        if self.scalar == ScalarType::Record {
            return None;
        }
        self.dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
    }

    pub fn scalar_subscripts(&self, flat_index: usize) -> Option<Vec<u32>> {
        if self.scalar == ScalarType::Record {
            return None;
        }
        if self.dimensions.is_empty() {
            return (flat_index == 0).then(Vec::new);
        }
        let scalar_count = self.scalar_count()?;
        if flat_index >= scalar_count || self.dimensions.contains(&0) {
            return None;
        }
        let mut remainder = flat_index;
        let mut subscripts = Vec::with_capacity(self.dimensions.len());
        for extent in self.dimensions.iter().rev() {
            subscripts.push(u32::try_from(remainder % *extent as usize).ok()? + 1);
            remainder /= *extent as usize;
        }
        subscripts.reverse();
        Some(subscripts)
    }

    pub fn is_record(&self) -> bool {
        self.scalar == ScalarType::Record
    }

    pub fn record_name(&self) -> Option<&rumoca_core::VarName> {
        self.record_name.as_ref()
    }

    pub fn record_field_count(&self) -> usize {
        self.record_fields.len()
    }

    pub fn record_field_name(&self, ordinal: usize) -> Option<&rumoca_core::VarName> {
        self.record_fields.get(ordinal).map(|field| &field.name)
    }

    pub(crate) fn record_field_type(&self, ordinal: usize) -> Option<u32> {
        self.record_fields
            .get(ordinal)
            .map(|field| field.value_type)
    }
}

impl RecordFieldType {
    pub(crate) fn new(name: rumoca_core::VarName, value_type: u32) -> Self {
        Self { name, value_type }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DaeLiteral {
    Real(f64),
    Integer(i64),
    Enumeration(i64),
    Boolean(bool),
    String(String),
}

impl DaeLiteral {
    pub(super) fn scalar_type(&self) -> ScalarType {
        match self {
            Self::Real(_) => ScalarType::Real,
            Self::Integer(_) => ScalarType::Integer,
            Self::Enumeration(_) => ScalarType::Enumeration,
            Self::Boolean(_) => ScalarType::Boolean,
            Self::String(_) => ScalarType::String,
        }
    }
}
