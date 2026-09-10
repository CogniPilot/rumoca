use rumoca_core::{ClassType, DefId, Location};

use crate::{ClassDef, ClassDefIndex};

/// Language-defined role of one function owned by an ExternalObject class.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExternalObjectLifecycleRole {
    Constructor,
    Destructor,
}

impl ExternalObjectLifecycleRole {
    pub const fn source_name(self) -> &'static str {
        match self {
            Self::Constructor => "constructor",
            Self::Destructor => "destructor",
        }
    }
}

/// Exact declarations forming one resolved ExternalObject lifecycle.
///
/// Lifecycle facts can only be minted by [`ClassDefIndex::external_object_lifecycle`].
///
/// ```compile_fail
/// fn forge<'tree>(
///     owner: &'tree rumoca_ir_ast::ClassDef,
///     owner_def_id: rumoca_core::DefId,
/// ) -> rumoca_ir_ast::ExternalObjectLifecycle<'tree> {
///     rumoca_ir_ast::ExternalObjectLifecycle {
///         owner_def_id,
///         owner,
///         constructor_def_id: owner_def_id,
///         constructor: owner,
///         destructor_def_id: owner_def_id,
///         destructor: owner,
///     }
/// }
/// ```
#[derive(Debug, Clone, Copy)]
pub struct ExternalObjectLifecycle<'tree> {
    owner_def_id: DefId,
    owner: &'tree ClassDef,
    constructor_def_id: DefId,
    constructor: &'tree ClassDef,
    destructor_def_id: DefId,
    destructor: &'tree ClassDef,
}

impl<'tree> ExternalObjectLifecycle<'tree> {
    pub const fn owner_def_id(&self) -> DefId {
        self.owner_def_id
    }

    pub const fn owner(&self) -> &'tree ClassDef {
        self.owner
    }

    pub const fn constructor_def_id(&self) -> DefId {
        self.constructor_def_id
    }

    pub const fn constructor(&self) -> &'tree ClassDef {
        self.constructor
    }

    pub const fn destructor_def_id(&self) -> DefId {
        self.destructor_def_id
    }

    pub const fn destructor(&self) -> &'tree ClassDef {
        self.destructor
    }
}

/// Structural failure while deriving an ExternalObject lifecycle fact.
#[derive(Debug, Clone, Copy)]
pub enum ExternalObjectLifecycleError<'tree> {
    MissingBuiltinIdentity {
        owner_def_id: DefId,
    },
    MissingOwner {
        owner_def_id: DefId,
    },
    UnresolvedBaseIdentity {
        owner_def_id: DefId,
        owner: &'tree ClassDef,
    },
    MissingBase {
        owner_def_id: DefId,
        owner: &'tree ClassDef,
        base_def_id: DefId,
    },
    IndirectExternalObjectExtension {
        owner_def_id: DefId,
        owner: &'tree ClassDef,
        direct_base_def_id: DefId,
    },
    MissingLifecycleFunction {
        owner_def_id: DefId,
        owner: &'tree ClassDef,
        role: ExternalObjectLifecycleRole,
    },
    LifecycleMemberIsNotFunction {
        owner_def_id: DefId,
        owner: &'tree ClassDef,
        role: ExternalObjectLifecycleRole,
        member: &'tree ClassDef,
    },
    MissingLifecycleIdentity {
        owner_def_id: DefId,
        owner: &'tree ClassDef,
        role: ExternalObjectLifecycleRole,
        member: &'tree ClassDef,
    },
}

impl ExternalObjectLifecycleError<'_> {
    /// Name the exact resolved fact that could not be constructed.
    pub const fn required_fact(&self) -> &'static str {
        match self {
            Self::MissingBuiltinIdentity { .. } => "predefined ExternalObject identity",
            Self::MissingOwner { .. } => "ExternalObject owner declaration",
            Self::UnresolvedBaseIdentity { .. } => "ExternalObject base identity",
            Self::MissingBase { .. } => "ExternalObject base declaration",
            Self::IndirectExternalObjectExtension { .. } => "direct ExternalObject inheritance",
            Self::MissingLifecycleFunction { role, .. } => match role {
                ExternalObjectLifecycleRole::Constructor => "ExternalObject constructor function",
                ExternalObjectLifecycleRole::Destructor => "ExternalObject destructor function",
            },
            Self::LifecycleMemberIsNotFunction { role, .. } => match role {
                ExternalObjectLifecycleRole::Constructor => {
                    "ExternalObject constructor function kind"
                }
                ExternalObjectLifecycleRole::Destructor => {
                    "ExternalObject destructor function kind"
                }
            },
            Self::MissingLifecycleIdentity { role, .. } => match role {
                ExternalObjectLifecycleRole::Constructor => "ExternalObject constructor identity",
                ExternalObjectLifecycleRole::Destructor => "ExternalObject destructor identity",
            },
        }
    }

    /// Return the exact source declaration responsible for this failed fact.
    pub fn declaration_location(&self) -> Option<&Location> {
        match self {
            Self::MissingBuiltinIdentity { .. } | Self::MissingOwner { .. } => None,
            Self::UnresolvedBaseIdentity { owner, .. } => owner
                .extends
                .iter()
                .find(|extend| extend.base_def_id.is_none())
                .map_or(Some(&owner.location), |extend| Some(&extend.location)),
            Self::MissingBase {
                owner, base_def_id, ..
            }
            | Self::IndirectExternalObjectExtension {
                owner,
                direct_base_def_id: base_def_id,
                ..
            } => owner
                .extends
                .iter()
                .find(|extend| extend.base_def_id == Some(*base_def_id))
                .map_or(Some(&owner.location), |extend| Some(&extend.location)),
            Self::MissingLifecycleFunction { owner, .. } => Some(&owner.location),
            Self::LifecycleMemberIsNotFunction { member, .. }
            | Self::MissingLifecycleIdentity { member, .. } => Some(&member.location),
        }
    }
}

impl<'tree> ClassDefIndex<'tree> {
    /// Derive the required lifecycle declarations owned by one resolved
    /// ExternalObject class.
    ///
    /// The predefined `ExternalObject` symbol is resolved once when this index
    /// is built. Every subsequent decision uses only exact `DefId` inheritance
    /// and direct child ownership. The language-defined lifecycle names select
    /// roles inside that owner; qualified/display names never define identity.
    pub fn external_object_lifecycle(
        &self,
        owner_def_id: DefId,
    ) -> Result<Option<ExternalObjectLifecycle<'tree>>, ExternalObjectLifecycleError<'tree>> {
        let external_object_def_id = self
            .external_object_def_id
            .ok_or(ExternalObjectLifecycleError::MissingBuiltinIdentity { owner_def_id })?;
        let owner = self
            .get(owner_def_id)
            .ok_or(ExternalObjectLifecycleError::MissingOwner { owner_def_id })?;

        let mut directly_extends_external_object = false;
        let mut indirect_external_object_base = None;
        for extend in &owner.extends {
            let base_def_id =
                extend
                    .base_def_id
                    .ok_or(ExternalObjectLifecycleError::UnresolvedBaseIdentity {
                        owner_def_id,
                        owner,
                    })?;
            if base_def_id == external_object_def_id {
                directly_extends_external_object = true;
                continue;
            }
            if self.external_object_owner_def_ids.contains(&base_def_id) {
                indirect_external_object_base = Some(base_def_id);
                continue;
            }
            if !self.builtin_def_ids.contains(&base_def_id) && self.get(base_def_id).is_none() {
                return Err(ExternalObjectLifecycleError::MissingBase {
                    owner_def_id,
                    owner,
                    base_def_id,
                });
            }
        }

        if directly_extends_external_object {
            return self.external_object_lifecycle_declarations(owner_def_id, owner);
        }
        if let Some(direct_base_def_id) = indirect_external_object_base {
            return Err(
                ExternalObjectLifecycleError::IndirectExternalObjectExtension {
                    owner_def_id,
                    owner,
                    direct_base_def_id,
                },
            );
        }
        Ok(None)
    }

    fn external_object_lifecycle_declarations(
        &self,
        owner_def_id: DefId,
        owner: &'tree ClassDef,
    ) -> Result<Option<ExternalObjectLifecycle<'tree>>, ExternalObjectLifecycleError<'tree>> {
        let (constructor_def_id, constructor) = lifecycle_member(
            owner_def_id,
            owner,
            ExternalObjectLifecycleRole::Constructor,
        )?;
        let (destructor_def_id, destructor) =
            lifecycle_member(owner_def_id, owner, ExternalObjectLifecycleRole::Destructor)?;
        Ok(Some(ExternalObjectLifecycle {
            owner_def_id,
            owner,
            constructor_def_id,
            constructor,
            destructor_def_id,
            destructor,
        }))
    }
}

fn lifecycle_member(
    owner_def_id: DefId,
    owner: &ClassDef,
    role: ExternalObjectLifecycleRole,
) -> Result<(DefId, &ClassDef), ExternalObjectLifecycleError<'_>> {
    let member = owner.classes.get(role.source_name()).ok_or(
        ExternalObjectLifecycleError::MissingLifecycleFunction {
            owner_def_id,
            owner,
            role,
        },
    )?;
    if member.class_type != ClassType::Function {
        return Err(ExternalObjectLifecycleError::LifecycleMemberIsNotFunction {
            owner_def_id,
            owner,
            role,
            member,
        });
    }
    let member_def_id =
        member
            .def_id
            .ok_or(ExternalObjectLifecycleError::MissingLifecycleIdentity {
                owner_def_id,
                owner,
                role,
                member,
            })?;
    Ok((member_def_id, member))
}

#[cfg(test)]
mod tests {
    use rumoca_core::{ComponentPath, Token};

    use super::*;
    use crate::{AstIndexMap, ClassTree, Extend, StoredDefinition};

    const EXTERNAL_OBJECT: DefId = DefId(1);
    const REAL: DefId = DefId(2);

    fn class(
        def_id: DefId,
        name: &str,
        class_type: ClassType,
        bases: &[DefId],
        classes: AstIndexMap<String, ClassDef>,
    ) -> ClassDef {
        ClassDef {
            def_id: Some(def_id),
            name: Token {
                text: name.into(),
                ..Default::default()
            },
            class_type,
            extends: bases
                .iter()
                .map(|base_def_id| Extend {
                    base_def_id: Some(*base_def_id),
                    ..Default::default()
                })
                .collect(),
            classes,
            ..Default::default()
        }
    }

    fn function(def_id: DefId, name: &str) -> ClassDef {
        class(
            def_id,
            name,
            ClassType::Function,
            &[],
            AstIndexMap::default(),
        )
    }

    fn lifecycle(constructor: DefId, destructor: Option<DefId>) -> AstIndexMap<String, ClassDef> {
        let mut classes = AstIndexMap::default();
        classes.insert(
            "constructor".to_string(),
            function(constructor, "constructor"),
        );
        if let Some(destructor) = destructor {
            classes.insert("destructor".to_string(), function(destructor, "destructor"));
        }
        classes
    }

    fn tree(classes: impl IntoIterator<Item = ClassDef>) -> ClassTree {
        let mut definitions = StoredDefinition::default();
        let mut name_map = AstIndexMap::default();
        name_map.insert("ExternalObject".to_string(), EXTERNAL_OBJECT);
        name_map.insert("Real".to_string(), REAL);
        for class in classes {
            let name = class.name.text.to_string();
            name_map.insert(name.clone(), class.def_id.expect("test class DefId"));
            for child in class.classes.values() {
                name_map.insert(
                    format!("{name}.{}", child.name.text),
                    child.def_id.expect("test child DefId"),
                );
            }
            definitions.classes.insert(name, class);
        }
        let mut scope_tree = crate::ScopeTree::new();
        scope_tree.add_predefined_member(
            ComponentPath::from_flat_path("ExternalObject"),
            EXTERNAL_OBJECT,
        );
        scope_tree.add_predefined_member(ComponentPath::from_flat_path("Real"), REAL);
        ClassTree {
            definitions,
            scope_tree,
            name_map,
            ..ClassTree::default()
        }
    }

    #[test]
    fn ordinary_class_has_no_external_object_lifecycle() {
        let tree = tree([class(
            DefId(10),
            "Ordinary",
            ClassType::Class,
            &[],
            AstIndexMap::default(),
        )]);
        let index = ClassDefIndex::from_tree(&tree);
        assert!(matches!(
            index.external_object_lifecycle(DefId(10)),
            Ok(None)
        ));
    }

    #[test]
    fn builtin_real_extension_is_not_an_external_object() {
        let tree = tree([class(
            DefId(10),
            "T",
            ClassType::Type,
            &[REAL],
            AstIndexMap::default(),
        )]);
        let index = ClassDefIndex::from_tree(&tree);
        assert!(matches!(
            index.external_object_lifecycle(DefId(10)),
            Ok(None)
        ));
    }

    #[test]
    fn direct_external_object_returns_exact_lifecycle_identities() {
        let tree = tree([class(
            DefId(10),
            "Handle",
            ClassType::Class,
            &[EXTERNAL_OBJECT],
            lifecycle(DefId(11), Some(DefId(12))),
        )]);
        let index = ClassDefIndex::from_tree(&tree);
        let lifecycle = index
            .external_object_lifecycle(DefId(10))
            .expect("resolved lifecycle query")
            .expect("direct ExternalObject lifecycle");
        assert_eq!(lifecycle.owner_def_id(), DefId(10));
        assert_eq!(lifecycle.constructor_def_id(), DefId(11));
        assert_eq!(lifecycle.destructor_def_id(), DefId(12));
        assert_eq!(lifecycle.owner().name.text.as_ref(), "Handle");
        assert_eq!(lifecycle.constructor().name.text.as_ref(), "constructor");
        assert_eq!(lifecycle.destructor().name.text.as_ref(), "destructor");
    }

    #[test]
    fn predefined_external_object_identity_ignores_name_map_shadowing() {
        let mut tree = tree([class(
            DefId(10),
            "Handle",
            ClassType::Class,
            &[EXTERNAL_OBJECT],
            lifecycle(DefId(11), Some(DefId(12))),
        )]);
        tree.name_map
            .insert("ExternalObject".to_string(), DefId(99));

        let index = ClassDefIndex::from_tree(&tree);
        let lifecycle = index
            .external_object_lifecycle(DefId(10))
            .expect("shadowing cannot corrupt the lifecycle query")
            .expect("Handle directly extends the predefined ExternalObject");
        assert_eq!(lifecycle.owner_def_id(), DefId(10));
    }

    #[test]
    fn indirect_external_object_extension_is_typed_error() {
        let tree = tree([
            class(
                DefId(10),
                "Handle",
                ClassType::Class,
                &[EXTERNAL_OBJECT],
                lifecycle(DefId(11), Some(DefId(12))),
            ),
            class(
                DefId(20),
                "Derived",
                ClassType::Class,
                &[DefId(10)],
                AstIndexMap::default(),
            ),
        ]);
        let index = ClassDefIndex::from_tree(&tree);
        let error = index
            .external_object_lifecycle(DefId(20))
            .expect_err("indirect ExternalObject extension must fail");
        assert!(matches!(
            error,
            ExternalObjectLifecycleError::IndirectExternalObjectExtension {
                direct_base_def_id: DefId(10),
                ..
            }
        ));
        assert_eq!(error.required_fact(), "direct ExternalObject inheritance");
        let derived = index.get(DefId(20)).expect("derived class");
        assert!(std::ptr::eq(
            error.declaration_location().expect("extends location"),
            &derived.extends[0].location
        ));
    }

    #[test]
    fn transitive_external_object_extension_is_typed_error() {
        let tree = tree([
            class(
                DefId(10),
                "Handle",
                ClassType::Class,
                &[EXTERNAL_OBJECT],
                lifecycle(DefId(11), Some(DefId(12))),
            ),
            class(
                DefId(20),
                "Derived",
                ClassType::Class,
                &[DefId(10)],
                AstIndexMap::default(),
            ),
            class(
                DefId(30),
                "DerivedAgain",
                ClassType::Class,
                &[DefId(20)],
                AstIndexMap::default(),
            ),
        ]);
        let index = ClassDefIndex::from_tree(&tree);
        assert!(matches!(
            index.external_object_lifecycle(DefId(30)),
            Err(
                ExternalObjectLifecycleError::IndirectExternalObjectExtension {
                    direct_base_def_id: DefId(20),
                    ..
                }
            )
        ));
    }

    #[test]
    fn non_external_object_diamond_is_not_a_cycle() {
        let tree = tree([
            class(
                DefId(10),
                "Base",
                ClassType::Class,
                &[],
                AstIndexMap::default(),
            ),
            class(
                DefId(11),
                "Left",
                ClassType::Class,
                &[DefId(10)],
                AstIndexMap::default(),
            ),
            class(
                DefId(12),
                "Right",
                ClassType::Class,
                &[DefId(10)],
                AstIndexMap::default(),
            ),
            class(
                DefId(13),
                "Diamond",
                ClassType::Class,
                &[DefId(11), DefId(12)],
                AstIndexMap::default(),
            ),
        ]);
        let index = ClassDefIndex::from_tree(&tree);
        assert!(matches!(
            index.external_object_lifecycle(DefId(13)),
            Ok(None)
        ));
    }

    #[test]
    fn missing_lifecycle_member_is_typed_error() {
        let tree = tree([class(
            DefId(10),
            "Handle",
            ClassType::Class,
            &[EXTERNAL_OBJECT],
            lifecycle(DefId(11), None),
        )]);
        let index = ClassDefIndex::from_tree(&tree);
        let error = index
            .external_object_lifecycle(DefId(10))
            .expect_err("missing destructor must fail");
        assert!(matches!(
            error,
            ExternalObjectLifecycleError::MissingLifecycleFunction {
                role: ExternalObjectLifecycleRole::Destructor,
                ..
            }
        ));
        assert_eq!(error.required_fact(), "ExternalObject destructor function");
        let owner = index.get(DefId(10)).expect("ExternalObject owner");
        assert!(std::ptr::eq(
            error.declaration_location().expect("owner location"),
            &owner.location
        ));
    }

    #[test]
    fn wrong_lifecycle_kind_reports_member_fact_and_location() {
        let mut members = lifecycle(DefId(11), Some(DefId(12)));
        members
            .get_mut("constructor")
            .expect("constructor")
            .class_type = ClassType::Model;
        let tree = tree([class(
            DefId(10),
            "Handle",
            ClassType::Class,
            &[EXTERNAL_OBJECT],
            members,
        )]);
        let index = ClassDefIndex::from_tree(&tree);
        let error = index
            .external_object_lifecycle(DefId(10))
            .expect_err("non-function constructor must fail");
        assert_eq!(
            error.required_fact(),
            "ExternalObject constructor function kind"
        );
        let constructor = index.get(DefId(11)).expect("constructor member");
        assert!(std::ptr::eq(
            error.declaration_location().expect("member location"),
            &constructor.location
        ));
    }
}
