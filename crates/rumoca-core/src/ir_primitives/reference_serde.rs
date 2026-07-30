use serde::{Deserialize, Deserializer, Serialize, Serializer};

use super::{ComponentReference, InstanceId, Reference, ResolvedFunctionReference, VarName};

#[derive(Serialize, Deserialize)]
struct ReferenceWire {
    name: VarName,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    component_ref: Option<ComponentReference>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    resolved_function: Option<ResolvedFunctionReference>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    instance_id: Option<InstanceId>,
    #[serde(default, skip_serializing_if = "is_false")]
    generated: bool,
}

fn is_false(value: &bool) -> bool {
    !*value
}

impl Serialize for Reference {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        if serializer.is_human_readable()
            && self.component_ref.is_none()
            && self.resolved_function.is_none()
            && self.instance_id.is_none()
            && !self.generated
        {
            return self.name.serialize(serializer);
        }

        if !serializer.is_human_readable() {
            return (
                &self.name,
                &self.component_ref,
                &self.resolved_function,
                &self.instance_id,
                &self.generated,
            )
                .serialize(serializer);
        }

        ReferenceWire {
            name: self.name.clone(),
            component_ref: self.component_ref.clone(),
            resolved_function: self.resolved_function,
            instance_id: self.instance_id,
            generated: self.generated,
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Reference {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        if !deserializer.is_human_readable() {
            let (name, component_ref, resolved_function, instance_id, generated) =
                <(
                    VarName,
                    Option<ComponentReference>,
                    Option<ResolvedFunctionReference>,
                    Option<InstanceId>,
                    bool,
                )>::deserialize(deserializer)?;
            return Ok(Self {
                name,
                component_ref,
                resolved_function,
                instance_id,
                generated,
            });
        }

        #[derive(Deserialize)]
        #[serde(untagged)]
        enum HumanReference {
            Name(VarName),
            Wire(ReferenceWire),
        }

        match HumanReference::deserialize(deserializer)? {
            HumanReference::Name(name) => Ok(Self {
                name,
                component_ref: None,
                resolved_function: None,
                instance_id: None,
                generated: false,
            }),
            HumanReference::Wire(wire) => Ok(Self {
                name: wire.name,
                component_ref: wire.component_ref,
                resolved_function: wire.resolved_function,
                instance_id: wire.instance_id,
                generated: wire.generated,
            }),
        }
    }
}
