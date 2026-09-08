//! Checked static member layout for one correlated eFMI package.

use std::collections::BTreeSet;
use std::fmt;

use serde::Serialize;

/// Closed role of one member in the correlated eFMI archive.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ProductionArtifactRole {
    PackageManifest,
    AlgorithmCodeManifest,
    AlgorithmCodeSource,
    ProductionManifest,
    ProductionHeader,
    ProductionSource,
    Schema,
}

/// One portable, archive-relative member path.
///
/// Construction admits ASCII only, which is a strict subset of NFC and makes
/// the case-collision proof independent of host locale and filesystem.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
pub struct ProductionPortableMemberPath(Box<str>);

impl ProductionPortableMemberPath {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Checked top-level eFMI model-representation directory name.
#[derive(Debug, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct ProductionRepresentationName(Box<str>);

impl ProductionRepresentationName {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Checked representation-relative file basename.
#[derive(Debug, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct ProductionRepresentationFileName(Box<str>);

impl ProductionRepresentationFileName {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Checked representation-relative directory spelling required by eFMI File.
#[derive(Debug, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct ProductionRepresentationFilePath(Box<str>);

impl ProductionRepresentationFilePath {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Correlated whole-member and representation-relative file identities.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ProductionArtifactRepresentationFile {
    representation_name: ProductionRepresentationName,
    file_name: ProductionRepresentationFileName,
    file_path: ProductionRepresentationFilePath,
}

impl ProductionArtifactRepresentationFile {
    #[must_use]
    pub const fn representation_name(&self) -> &ProductionRepresentationName {
        &self.representation_name
    }

    #[must_use]
    pub const fn file_name(&self) -> &ProductionRepresentationFileName {
        &self.file_name
    }

    #[must_use]
    pub const fn file_path(&self) -> &ProductionRepresentationFilePath {
        &self.file_path
    }
}

/// One checked role/path correlation issued by the whole-layout constructor.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ProductionArtifactLayoutMember {
    role: ProductionArtifactRole,
    member_path: ProductionPortableMemberPath,
    representation_file: Option<ProductionArtifactRepresentationFile>,
}

impl ProductionArtifactLayoutMember {
    #[must_use]
    pub const fn role(&self) -> ProductionArtifactRole {
        self.role
    }

    #[must_use]
    pub const fn member_path(&self) -> &ProductionPortableMemberPath {
        &self.member_path
    }

    /// Representation-relative facts for AC/PC members. Root/schema members
    /// have no representation-local interpretation.
    #[must_use]
    pub const fn representation_file(&self) -> Option<&ProductionArtifactRepresentationFile> {
        self.representation_file.as_ref()
    }
}

/// Source-relative header spelling proven by the same layout that owns both
/// C members. This is a path, not a C identifier or arbitrary template text.
#[derive(Debug, PartialEq, Eq, Serialize)]
#[serde(transparent)]
pub struct ProductionHeaderInclude(Box<str>);

impl ProductionHeaderInclude {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Complete checked static archive-member set for one correlated product.
///
/// The type is deliberately non-`Clone`, non-`Default`, and non-deserializable.
/// Preparation consumes it and the presentation plan retains the sole owner.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ProductionArtifactLayout {
    members: Box<[ProductionArtifactLayoutMember]>,
    package_manifest: usize,
    algorithm_code_manifest: usize,
    algorithm_code_source: usize,
    production_manifest: usize,
    production_header: usize,
    production_source: usize,
    schema_start: usize,
    header_include: ProductionHeaderInclude,
}

/// Invocation-independent checked correlated-product layout retained by
/// target construction. Rendering binds this already-proved layout to the
/// invocation without revalidating paths, cardinalities, or correlations.
#[derive(Debug, PartialEq, Eq)]
pub struct ProductionArtifactLayoutSpec(ProductionArtifactLayout);

impl ProductionArtifactLayoutSpec {
    pub fn construct(
        drafts: Vec<(ProductionArtifactRole, Box<str>)>,
    ) -> Result<Self, ProductionArtifactLayoutError> {
        ProductionArtifactLayout::construct(drafts).map(Self)
    }

    #[must_use]
    pub fn members(&self) -> &[ProductionArtifactLayoutMember] {
        self.0.members()
    }

    #[must_use]
    pub fn into_layout(self) -> ProductionArtifactLayout {
        self.0
    }
}

impl ProductionArtifactLayout {
    /// Issue the sealed layout from a complete target member inventory.
    ///
    /// This constructor is public because target-manifest orchestration lives
    /// in the facade crate. It cannot mint arbitrary authority: every role,
    /// singleton path, representation relation, and whole-set collision is
    /// proved here before the non-cloneable value can enter preparation.
    pub fn construct<I, P>(drafts: I) -> Result<Self, ProductionArtifactLayoutError>
    where
        I: IntoIterator<Item = (ProductionArtifactRole, P)>,
        P: Into<Box<str>>,
    {
        let mut members = drafts
            .into_iter()
            .map(|(role, member_path)| {
                let member_path = member_path.into();
                validate_member_path(&member_path)?;
                validate_role_extension(role, &member_path)?;
                Ok(ProductionArtifactLayoutMember {
                    role,
                    member_path: ProductionPortableMemberPath(member_path),
                    representation_file: None,
                })
            })
            .collect::<Result<Vec<_>, ProductionArtifactLayoutError>>()?;
        prove_case_distinct(&members)?;

        let package_manifest = singleton_index(&members, ProductionArtifactRole::PackageManifest)?;
        let algorithm_code_manifest =
            singleton_index(&members, ProductionArtifactRole::AlgorithmCodeManifest)?;
        let algorithm_code_source =
            singleton_index(&members, ProductionArtifactRole::AlgorithmCodeSource)?;
        let production_manifest =
            singleton_index(&members, ProductionArtifactRole::ProductionManifest)?;
        let production_header =
            singleton_index(&members, ProductionArtifactRole::ProductionHeader)?;
        let production_source =
            singleton_index(&members, ProductionArtifactRole::ProductionSource)?;
        if !members
            .iter()
            .any(|member| member.role == ProductionArtifactRole::Schema)
        {
            return Err(ProductionArtifactLayoutError::MissingSchema);
        }

        let schema_start = members
            .iter()
            .position(|member| member.role == ProductionArtifactRole::Schema)
            .expect("the schema role was proven inhabited");
        if let Some(member) = members[schema_start..]
            .iter()
            .find(|member| member.role != ProductionArtifactRole::Schema)
        {
            return Err(ProductionArtifactLayoutError::NonSchemaAfterSchema { role: member.role });
        }

        require_exact_path(&members[package_manifest], "__content.xml")?;
        require_exact_path(
            &members[algorithm_code_manifest],
            "AlgorithmCode/manifest.xml",
        )?;
        require_exact_path(&members[production_manifest], "ProductionCode/manifest.xml")?;
        require_exact_path(&members[algorithm_code_source], "AlgorithmCode/model.alg")?;
        require_exact_path(
            &members[production_header],
            "ProductionCode/sources/production.h",
        )?;
        require_exact_path(
            &members[production_source],
            "ProductionCode/sources/production.c",
        )?;

        for (index, representation) in [
            (algorithm_code_manifest, "AlgorithmCode"),
            (algorithm_code_source, "AlgorithmCode"),
            (production_manifest, "ProductionCode"),
            (production_header, "ProductionCode"),
            (production_source, "ProductionCode"),
        ] {
            members[index].representation_file = Some(representation_file(
                members[index].member_path.as_str(),
                representation,
            )?);
        }

        let header_path = members[production_header].member_path.as_str();
        let source_path = members[production_source].member_path.as_str();
        let (header_parent, header_name) = split_parent(header_path);
        let (source_parent, _) = split_parent(source_path);
        if header_parent != source_parent {
            return Err(ProductionArtifactLayoutError::CSourceHeaderDirectoryMismatch);
        }
        let header_include = ProductionHeaderInclude(header_name.into());
        Ok(Self {
            members: members.into_boxed_slice(),
            package_manifest,
            algorithm_code_manifest,
            algorithm_code_source,
            production_manifest,
            production_header,
            production_source,
            schema_start,
            header_include,
        })
    }

    #[must_use]
    pub fn members(&self) -> &[ProductionArtifactLayoutMember] {
        &self.members
    }

    #[must_use]
    pub const fn package_manifest(&self) -> &ProductionArtifactLayoutMember {
        &self.members[self.package_manifest]
    }

    #[must_use]
    pub const fn algorithm_code_manifest(&self) -> &ProductionArtifactLayoutMember {
        &self.members[self.algorithm_code_manifest]
    }

    #[must_use]
    pub const fn algorithm_code_source(&self) -> &ProductionArtifactLayoutMember {
        &self.members[self.algorithm_code_source]
    }

    #[must_use]
    pub const fn production_manifest(&self) -> &ProductionArtifactLayoutMember {
        &self.members[self.production_manifest]
    }

    #[must_use]
    pub const fn production_header(&self) -> &ProductionArtifactLayoutMember {
        &self.members[self.production_header]
    }

    #[must_use]
    pub const fn production_source(&self) -> &ProductionArtifactLayoutMember {
        &self.members[self.production_source]
    }

    #[must_use]
    pub fn algorithm_code_manifest_file(&self) -> &ProductionArtifactRepresentationFile {
        representation_fact(self.algorithm_code_manifest())
    }

    #[must_use]
    pub fn algorithm_code_source_file(&self) -> &ProductionArtifactRepresentationFile {
        representation_fact(self.algorithm_code_source())
    }

    #[must_use]
    pub fn production_manifest_file(&self) -> &ProductionArtifactRepresentationFile {
        representation_fact(self.production_manifest())
    }

    #[must_use]
    pub fn production_header_file(&self) -> &ProductionArtifactRepresentationFile {
        representation_fact(self.production_header())
    }

    #[must_use]
    pub fn production_source_file(&self) -> &ProductionArtifactRepresentationFile {
        representation_fact(self.production_source())
    }

    #[must_use]
    pub fn schemas(&self) -> &[ProductionArtifactLayoutMember] {
        &self.members[self.schema_start..]
    }

    #[must_use]
    pub const fn header_include(&self) -> &ProductionHeaderInclude {
        &self.header_include
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProductionArtifactLayoutError {
    NonPortablePath {
        path: Box<str>,
    },
    RoleExtensionMismatch {
        role: ProductionArtifactRole,
        path: Box<str>,
    },
    CaseCollision {
        path: Box<str>,
    },
    MissingRole {
        role: ProductionArtifactRole,
    },
    DuplicateRole {
        role: ProductionArtifactRole,
    },
    MissingSchema,
    NonSchemaAfterSchema {
        role: ProductionArtifactRole,
    },
    CSourceHeaderDirectoryMismatch,
    StandardMemberPathMismatch {
        role: ProductionArtifactRole,
        expected: &'static str,
        actual: Box<str>,
    },
    RepresentationPathMismatch {
        path: Box<str>,
    },
}

impl fmt::Display for ProductionArtifactLayoutError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonPortablePath { path } => {
                write!(formatter, "non-portable eFMI member path `{path}`")
            }
            Self::RoleExtensionMismatch { role, path } => {
                write!(
                    formatter,
                    "eFMI member path `{path}` disagrees with role {role:?}"
                )
            }
            Self::CaseCollision { path } => {
                write!(formatter, "case-colliding eFMI member path `{path}`")
            }
            Self::MissingRole { role } => write!(formatter, "eFMI layout is missing {role:?}"),
            Self::DuplicateRole { role } => {
                write!(formatter, "eFMI layout contains more than one {role:?}")
            }
            Self::MissingSchema => formatter.write_str("eFMI layout contains no schema member"),
            Self::NonSchemaAfterSchema { role } => write!(
                formatter,
                "eFMI role {role:?} appears after the schema suffix begins"
            ),
            Self::CSourceHeaderDirectoryMismatch => formatter
                .write_str("Production C source and header must occupy the same member directory"),
            Self::StandardMemberPathMismatch {
                role,
                expected,
                actual,
            } => write!(
                formatter,
                "eFMI {role:?} member must be `{expected}`, not `{actual}`"
            ),
            Self::RepresentationPathMismatch { path } => {
                write!(
                    formatter,
                    "invalid representation-local eFMI member `{path}`"
                )
            }
        }
    }
}

impl std::error::Error for ProductionArtifactLayoutError {}

fn singleton_index(
    members: &[ProductionArtifactLayoutMember],
    role: ProductionArtifactRole,
) -> Result<usize, ProductionArtifactLayoutError> {
    let mut indices = members
        .iter()
        .enumerate()
        .filter_map(|(index, member)| (member.role == role).then_some(index));
    let first = indices
        .next()
        .ok_or(ProductionArtifactLayoutError::MissingRole { role })?;
    if indices.next().is_some() {
        return Err(ProductionArtifactLayoutError::DuplicateRole { role });
    }
    Ok(first)
}

fn representation_fact(
    member: &ProductionArtifactLayoutMember,
) -> &ProductionArtifactRepresentationFile {
    member
        .representation_file
        .as_ref()
        .expect("required AC/PC members receive representation facts at construction")
}

fn require_exact_path(
    member: &ProductionArtifactLayoutMember,
    expected: &'static str,
) -> Result<(), ProductionArtifactLayoutError> {
    let actual = member.member_path.as_str();
    if actual != expected {
        return Err(ProductionArtifactLayoutError::StandardMemberPathMismatch {
            role: member.role,
            expected,
            actual: actual.into(),
        });
    }
    Ok(())
}

fn representation_file(
    member_path: &str,
    representation: &'static str,
) -> Result<ProductionArtifactRepresentationFile, ProductionArtifactLayoutError> {
    let relative = member_path
        .strip_prefix(representation)
        .and_then(|path| path.strip_prefix('/'))
        .ok_or_else(
            || ProductionArtifactLayoutError::RepresentationPathMismatch {
                path: member_path.into(),
            },
        )?;
    let (parent, file_name) = split_parent(relative);
    if file_name.is_empty() {
        return Err(ProductionArtifactLayoutError::RepresentationPathMismatch {
            path: member_path.into(),
        });
    }
    let file_path = if parent.is_empty() {
        "./".into()
    } else {
        format!("./{parent}/").into_boxed_str()
    };
    Ok(ProductionArtifactRepresentationFile {
        representation_name: ProductionRepresentationName(representation.into()),
        file_name: ProductionRepresentationFileName(file_name.into()),
        file_path: ProductionRepresentationFilePath(file_path),
    })
}

fn validate_member_path(path: &str) -> Result<(), ProductionArtifactLayoutError> {
    let portable_characters = path
        .bytes()
        .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'/' | b'.' | b'_' | b'-'));
    let segments_are_portable = !path.is_empty()
        && !path.starts_with('/')
        && !path.ends_with('/')
        && path
            .split('/')
            .all(|segment| !segment.is_empty() && segment != "." && segment != "..");
    let drive_or_template = path
        .as_bytes()
        .get(1)
        .is_some_and(|separator| *separator == b':')
        || path.contains("{{")
        || path.contains("}}")
        || path.contains('\\');
    if !portable_characters || !segments_are_portable || drive_or_template {
        return Err(ProductionArtifactLayoutError::NonPortablePath { path: path.into() });
    }
    Ok(())
}

fn validate_role_extension(
    role: ProductionArtifactRole,
    path: &str,
) -> Result<(), ProductionArtifactLayoutError> {
    let admitted = match role {
        ProductionArtifactRole::PackageManifest
        | ProductionArtifactRole::AlgorithmCodeManifest
        | ProductionArtifactRole::ProductionManifest => path.ends_with(".xml"),
        ProductionArtifactRole::AlgorithmCodeSource => path.ends_with(".alg"),
        ProductionArtifactRole::ProductionHeader => path.ends_with(".h"),
        ProductionArtifactRole::ProductionSource => path.ends_with(".c"),
        ProductionArtifactRole::Schema => path.starts_with("schemas/"),
    };
    if !admitted {
        return Err(ProductionArtifactLayoutError::RoleExtensionMismatch {
            role,
            path: path.into(),
        });
    }
    Ok(())
}

fn prove_case_distinct(
    members: &[ProductionArtifactLayoutMember],
) -> Result<(), ProductionArtifactLayoutError> {
    let mut folded = BTreeSet::new();
    for member in members {
        let path = member.member_path.as_str();
        if !folded.insert(path.to_ascii_lowercase()) {
            return Err(ProductionArtifactLayoutError::CaseCollision { path: path.into() });
        }
    }
    Ok(())
}

fn split_parent(path: &str) -> (&str, &str) {
    path.rsplit_once('/')
        .map_or(("", path), |(parent, name)| (parent, name))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn complete_layout() -> Vec<(ProductionArtifactRole, &'static str)> {
        vec![
            (
                ProductionArtifactRole::AlgorithmCodeSource,
                "AlgorithmCode/model.alg",
            ),
            (
                ProductionArtifactRole::AlgorithmCodeManifest,
                "AlgorithmCode/manifest.xml",
            ),
            (
                ProductionArtifactRole::ProductionHeader,
                "ProductionCode/sources/production.h",
            ),
            (
                ProductionArtifactRole::ProductionSource,
                "ProductionCode/sources/production.c",
            ),
            (
                ProductionArtifactRole::ProductionManifest,
                "ProductionCode/manifest.xml",
            ),
            (ProductionArtifactRole::PackageManifest, "__content.xml"),
            (
                ProductionArtifactRole::Schema,
                "schemas/ProductionCode/schema.xsd",
            ),
        ]
    }

    #[test]
    fn complete_static_member_set_is_deterministic() {
        let left = ProductionArtifactLayout::construct(complete_layout())
            .expect("the complete layout is checked");
        let right = ProductionArtifactLayout::construct(complete_layout())
            .expect("the same complete layout is checked");
        assert_eq!(left, right);
        assert_eq!(left.header_include().as_str(), "production.h");
        assert_eq!(
            left.package_manifest().member_path().as_str(),
            "__content.xml"
        );
        assert_eq!(
            left.algorithm_code_source_file()
                .representation_name()
                .as_str(),
            "AlgorithmCode"
        );
        assert_eq!(
            left.algorithm_code_source_file().file_name().as_str(),
            "model.alg"
        );
        assert_eq!(left.algorithm_code_source_file().file_path().as_str(), "./");
        assert_eq!(
            left.production_header_file().file_path().as_str(),
            "./sources/"
        );
        assert_eq!(left.schemas().len(), 1);
        assert_eq!(
            left.members()
                .iter()
                .filter_map(|member| member.representation_file().map(|_| member.role()))
                .collect::<Vec<_>>(),
            vec![
                ProductionArtifactRole::AlgorithmCodeSource,
                ProductionArtifactRole::AlgorithmCodeManifest,
                ProductionArtifactRole::ProductionHeader,
                ProductionArtifactRole::ProductionSource,
                ProductionArtifactRole::ProductionManifest,
            ],
            "every representation-bearing role receives exactly one correlated file fact",
        );
        assert_eq!(
            left.members()
                .iter()
                .map(ProductionArtifactLayoutMember::role)
                .collect::<Vec<_>>(),
            vec![
                ProductionArtifactRole::AlgorithmCodeSource,
                ProductionArtifactRole::AlgorithmCodeManifest,
                ProductionArtifactRole::ProductionHeader,
                ProductionArtifactRole::ProductionSource,
                ProductionArtifactRole::ProductionManifest,
                ProductionArtifactRole::PackageManifest,
                ProductionArtifactRole::Schema,
            ],
            "layout must retain the target-issued producer-first sequence",
        );
        assert_eq!(
            include_str!("artifact_layout.rs")
                .matches(concat!("(production_source, ", "\"ProductionCode\")"))
                .count(),
            1,
            "the representation assignment catalog must list Production source exactly once",
        );
    }

    #[test]
    fn traversal_templates_and_case_collisions_are_rejected_once() {
        for path in [
            "../production.h",
            "/production.h",
            "C:/production.h",
            "{{ name }}.h",
        ] {
            let mut drafts = complete_layout();
            drafts[2] = (ProductionArtifactRole::ProductionHeader, path);
            assert!(ProductionArtifactLayout::construct(drafts).is_err());
        }
        let mut drafts = complete_layout();
        drafts.push((
            ProductionArtifactRole::Schema,
            "schemas/ProductionCode/SCHEMA.XSD",
        ));
        assert!(matches!(
            ProductionArtifactLayout::construct(drafts),
            Err(ProductionArtifactLayoutError::CaseCollision { .. })
        ));
    }

    #[test]
    fn singleton_paths_and_schema_tree_are_exact() {
        let mut drafts = complete_layout();
        drafts[5] = (ProductionArtifactRole::PackageManifest, "content.xml");
        assert!(matches!(
            ProductionArtifactLayout::construct(drafts),
            Err(ProductionArtifactLayoutError::StandardMemberPathMismatch {
                role: ProductionArtifactRole::PackageManifest,
                ..
            })
        ));

        let mut drafts = complete_layout();
        drafts[1] = (
            ProductionArtifactRole::AlgorithmCodeManifest,
            "AlgorithmCode/ac.xml",
        );
        assert!(matches!(
            ProductionArtifactLayout::construct(drafts),
            Err(ProductionArtifactLayoutError::StandardMemberPathMismatch {
                role: ProductionArtifactRole::AlgorithmCodeManifest,
                ..
            })
        ));

        let mut drafts = complete_layout();
        drafts.push((ProductionArtifactRole::Schema, "schemas/VERSION"));
        drafts.push((ProductionArtifactRole::Schema, "schemas/LICENSE"));
        let layout = ProductionArtifactLayout::construct(drafts)
            .expect("non-XSD files in the borrowed schema tree remain package members");
        assert_eq!(layout.schemas().len(), 3);

        let mut drafts = complete_layout();
        let schema = drafts.pop().expect("one schema member");
        drafts.insert(0, schema);
        assert!(matches!(
            ProductionArtifactLayout::construct(drafts),
            Err(ProductionArtifactLayoutError::NonSchemaAfterSchema { .. })
        ));

        let mut drafts = complete_layout();
        drafts[6] = (ProductionArtifactRole::Schema, "LICENSE");
        assert!(matches!(
            ProductionArtifactLayout::construct(drafts),
            Err(ProductionArtifactLayoutError::RoleExtensionMismatch {
                role: ProductionArtifactRole::Schema,
                ..
            })
        ));
    }
}
