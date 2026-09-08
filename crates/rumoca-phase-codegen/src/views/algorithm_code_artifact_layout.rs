//! Checked static member layout for one packaged Algorithm Code eFMU.

use std::collections::BTreeSet;
use std::fmt;

use rumoca_core::TargetInvocationBrand;
use rumoca_ir_galec::TracedAlgorithmCodeProduct;
use serde::Serialize;

/// Closed role of one rendered member in a packaged Algorithm Code eFMU.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AlgorithmCodeArtifactRole {
    PackageManifest,
    AlgorithmCodeManifest,
    AlgorithmCodeSource,
}

impl AlgorithmCodeArtifactRole {
    /// Complete rendered-member vocabulary for this product.
    pub const ALL: &'static [Self] = &[
        Self::PackageManifest,
        Self::AlgorithmCodeManifest,
        Self::AlgorithmCodeSource,
    ];
}

/// One portable, archive-relative member path.
///
/// Construction admits ASCII only, which is a strict subset of NFC and makes
/// the case-collision proof independent of host locale and filesystem.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
pub struct AlgorithmCodePortableMemberPath(Box<str>);

impl AlgorithmCodePortableMemberPath {
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Checked representation-relative Algorithm Code source identity.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AlgorithmCodeRepresentationFile {
    file_name: Box<str>,
    file_path: Box<str>,
}

impl AlgorithmCodeRepresentationFile {
    #[must_use]
    pub fn file_name(&self) -> &str {
        &self.file_name
    }

    #[must_use]
    pub fn file_path(&self) -> &str {
        &self.file_path
    }
}

/// One checked role/path correlation issued by whole-layout construction.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AlgorithmCodeArtifactLayoutMember {
    role: AlgorithmCodeArtifactRole,
    member_path: AlgorithmCodePortableMemberPath,
}

impl AlgorithmCodeArtifactLayoutMember {
    #[must_use]
    pub const fn role(&self) -> AlgorithmCodeArtifactRole {
        self.role
    }

    #[must_use]
    pub const fn member_path(&self) -> &AlgorithmCodePortableMemberPath {
        &self.member_path
    }
}

/// Complete checked rendered-member family for one packaged Algorithm Code
/// product.
///
/// The type is deliberately non-`Clone`, non-`Default`, and
/// non-deserializable. Target-manifest orchestration constructs it from its
/// already-closed role inventory and the packaged renderer retains the sole
/// owner.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AlgorithmCodeArtifactLayout {
    members: Box<[AlgorithmCodeArtifactLayoutMember]>,
    package_manifest: usize,
    algorithm_code_manifest: usize,
    algorithm_code_source: usize,
    algorithm_code_source_file: AlgorithmCodeRepresentationFile,
}

/// Invocation-independent checked layout retained by target construction.
///
/// All fallible path/cardinality/collision admission happens while issuing
/// this specification. Binding it to the one target invocation is
/// infallible, so strict rendering cannot rediscover an invalid package.
#[derive(Debug, PartialEq, Eq)]
pub struct AlgorithmCodeArtifactLayoutSpec(AlgorithmCodeArtifactLayout);

impl AlgorithmCodeArtifactLayoutSpec {
    pub fn construct(
        drafts: Vec<(AlgorithmCodeArtifactRole, Box<str>)>,
    ) -> Result<Self, AlgorithmCodeArtifactLayoutError> {
        AlgorithmCodeArtifactLayout::construct(drafts).map(Self)
    }

    #[must_use]
    pub fn members(&self) -> &[AlgorithmCodeArtifactLayoutMember] {
        self.0.members()
    }

    #[must_use]
    pub fn into_layout(self) -> AlgorithmCodeArtifactLayout {
        self.0
    }
}

/// Sole owner joining one checked Algorithm Code package to its complete
/// target-issued package-member layout.
///
/// This value is deliberately non-`Clone`, non-`Default`, and
/// non-deserializable. Packaged template carriers can borrow only this joined
/// owner, so a semantic package and an unrelated member layout cannot be
/// supplied independently at render time.
#[derive(Debug)]
pub struct PreparedAlgorithmCodePackage<'inv> {
    product: TracedAlgorithmCodeProduct<'inv>,
    artifact_layout: AlgorithmCodeArtifactLayout,
}

impl<'inv> PreparedAlgorithmCodePackage<'inv> {
    pub(crate) const fn brand(&self) -> TargetInvocationBrand<'inv> {
        self.product.brand()
    }

    pub(crate) const fn traced_product(&self) -> &TracedAlgorithmCodeProduct<'inv> {
        &self.product
    }

    pub(crate) const fn artifact_layout(&self) -> &AlgorithmCodeArtifactLayout {
        &self.artifact_layout
    }
}

/// Consume the checked semantic package and target-issued layout into their
/// one render authority.
#[must_use]
pub const fn prepare_algorithm_code_package<'inv>(
    product: TracedAlgorithmCodeProduct<'inv>,
    artifact_layout: AlgorithmCodeArtifactLayout,
) -> PreparedAlgorithmCodePackage<'inv> {
    PreparedAlgorithmCodePackage {
        product,
        artifact_layout,
    }
}

impl AlgorithmCodeArtifactLayout {
    /// Issue a sealed layout from one complete target role inventory.
    ///
    /// Although orchestration calls this public constructor, it cannot mint a
    /// partial or relabelled package: exact role cardinality, singleton paths,
    /// portability, and case distinction are all prerequisites to obtaining
    /// the value.
    pub fn construct<I, P>(drafts: I) -> Result<Self, AlgorithmCodeArtifactLayoutError>
    where
        I: IntoIterator<Item = (AlgorithmCodeArtifactRole, P)>,
        P: Into<Box<str>>,
    {
        let members = drafts
            .into_iter()
            .map(|(role, member_path)| {
                let member_path = member_path.into();
                validate_member_path(&member_path)?;
                validate_role_extension(role, &member_path)?;
                Ok(AlgorithmCodeArtifactLayoutMember {
                    role,
                    member_path: AlgorithmCodePortableMemberPath(member_path),
                })
            })
            .collect::<Result<Vec<_>, AlgorithmCodeArtifactLayoutError>>()?;
        prove_case_distinct(&members)?;

        let package_manifest =
            singleton_index(&members, AlgorithmCodeArtifactRole::PackageManifest)?;
        let algorithm_code_manifest =
            singleton_index(&members, AlgorithmCodeArtifactRole::AlgorithmCodeManifest)?;
        let algorithm_code_source =
            singleton_index(&members, AlgorithmCodeArtifactRole::AlgorithmCodeSource)?;

        require_exact_path(&members[package_manifest], "__content.xml")?;
        require_exact_path(
            &members[algorithm_code_manifest],
            "AlgorithmCode/manifest.xml",
        )?;
        require_exact_path(&members[algorithm_code_source], "AlgorithmCode/model.alg")?;
        let algorithm_code_source_file =
            representation_file(members[algorithm_code_source].member_path().as_str())?;

        Ok(Self {
            members: members.into_boxed_slice(),
            package_manifest,
            algorithm_code_manifest,
            algorithm_code_source,
            algorithm_code_source_file,
        })
    }

    #[must_use]
    pub fn members(&self) -> &[AlgorithmCodeArtifactLayoutMember] {
        &self.members
    }

    #[must_use]
    pub const fn package_manifest(&self) -> &AlgorithmCodeArtifactLayoutMember {
        &self.members[self.package_manifest]
    }

    #[must_use]
    pub const fn algorithm_code_manifest(&self) -> &AlgorithmCodeArtifactLayoutMember {
        &self.members[self.algorithm_code_manifest]
    }

    #[must_use]
    pub const fn algorithm_code_source(&self) -> &AlgorithmCodeArtifactLayoutMember {
        &self.members[self.algorithm_code_source]
    }

    #[must_use]
    pub const fn algorithm_code_source_file(&self) -> &AlgorithmCodeRepresentationFile {
        &self.algorithm_code_source_file
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AlgorithmCodeArtifactLayoutError {
    NonPortablePath {
        path: Box<str>,
    },
    RoleExtensionMismatch {
        role: AlgorithmCodeArtifactRole,
        path: Box<str>,
    },
    CaseCollision {
        path: Box<str>,
    },
    MissingRole {
        role: AlgorithmCodeArtifactRole,
    },
    DuplicateRole {
        role: AlgorithmCodeArtifactRole,
    },
    StandardMemberPathMismatch {
        role: AlgorithmCodeArtifactRole,
        expected: &'static str,
        actual: Box<str>,
    },
    RepresentationPathMismatch {
        path: Box<str>,
    },
}

impl fmt::Display for AlgorithmCodeArtifactLayoutError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonPortablePath { path } => {
                write!(
                    formatter,
                    "non-portable Algorithm Code package member path `{path}`"
                )
            }
            Self::RoleExtensionMismatch { role, path } => write!(
                formatter,
                "Algorithm Code package member path `{path}` disagrees with role {role:?}"
            ),
            Self::CaseCollision { path } => {
                write!(
                    formatter,
                    "case-colliding Algorithm Code package member path `{path}`"
                )
            }
            Self::MissingRole { role } => {
                write!(
                    formatter,
                    "Algorithm Code package layout is missing {role:?}"
                )
            }
            Self::DuplicateRole { role } => write!(
                formatter,
                "Algorithm Code package layout contains more than one {role:?}"
            ),
            Self::StandardMemberPathMismatch {
                role,
                expected,
                actual,
            } => write!(
                formatter,
                "Algorithm Code package role {role:?} requires `{expected}`, not `{actual}`"
            ),
            Self::RepresentationPathMismatch { path } => write!(
                formatter,
                "Algorithm Code member `{path}` is not representation-relative"
            ),
        }
    }
}

impl std::error::Error for AlgorithmCodeArtifactLayoutError {}

fn singleton_index(
    members: &[AlgorithmCodeArtifactLayoutMember],
    role: AlgorithmCodeArtifactRole,
) -> Result<usize, AlgorithmCodeArtifactLayoutError> {
    let mut indices = members
        .iter()
        .enumerate()
        .filter_map(|(index, member)| (member.role == role).then_some(index));
    let index = indices
        .next()
        .ok_or(AlgorithmCodeArtifactLayoutError::MissingRole { role })?;
    if indices.next().is_some() {
        return Err(AlgorithmCodeArtifactLayoutError::DuplicateRole { role });
    }
    Ok(index)
}

fn require_exact_path(
    member: &AlgorithmCodeArtifactLayoutMember,
    expected: &'static str,
) -> Result<(), AlgorithmCodeArtifactLayoutError> {
    let actual = member.member_path.as_str();
    if actual != expected {
        return Err(
            AlgorithmCodeArtifactLayoutError::StandardMemberPathMismatch {
                role: member.role,
                expected,
                actual: actual.into(),
            },
        );
    }
    Ok(())
}

fn representation_file(
    member_path: &str,
) -> Result<AlgorithmCodeRepresentationFile, AlgorithmCodeArtifactLayoutError> {
    let relative = member_path.strip_prefix("AlgorithmCode/").ok_or_else(|| {
        AlgorithmCodeArtifactLayoutError::RepresentationPathMismatch {
            path: member_path.into(),
        }
    })?;
    let (parent, file_name) = split_parent(relative);
    if file_name.is_empty() {
        return Err(
            AlgorithmCodeArtifactLayoutError::RepresentationPathMismatch {
                path: member_path.into(),
            },
        );
    }
    let file_path = if parent.is_empty() {
        "./".into()
    } else {
        format!("./{parent}/").into_boxed_str()
    };
    Ok(AlgorithmCodeRepresentationFile {
        file_name: file_name.into(),
        file_path,
    })
}

fn validate_member_path(path: &str) -> Result<(), AlgorithmCodeArtifactLayoutError> {
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
        return Err(AlgorithmCodeArtifactLayoutError::NonPortablePath { path: path.into() });
    }
    Ok(())
}

fn validate_role_extension(
    role: AlgorithmCodeArtifactRole,
    path: &str,
) -> Result<(), AlgorithmCodeArtifactLayoutError> {
    let admitted = match role {
        AlgorithmCodeArtifactRole::PackageManifest
        | AlgorithmCodeArtifactRole::AlgorithmCodeManifest => path.ends_with(".xml"),
        AlgorithmCodeArtifactRole::AlgorithmCodeSource => path.ends_with(".alg"),
    };
    if !admitted {
        return Err(AlgorithmCodeArtifactLayoutError::RoleExtensionMismatch {
            role,
            path: path.into(),
        });
    }
    Ok(())
}

fn prove_case_distinct(
    members: &[AlgorithmCodeArtifactLayoutMember],
) -> Result<(), AlgorithmCodeArtifactLayoutError> {
    let mut folded = BTreeSet::new();
    for member in members {
        let path = member.member_path.as_str();
        if !folded.insert(path.to_ascii_lowercase()) {
            return Err(AlgorithmCodeArtifactLayoutError::CaseCollision { path: path.into() });
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

    fn complete_layout() -> Vec<(AlgorithmCodeArtifactRole, &'static str)> {
        vec![
            (
                AlgorithmCodeArtifactRole::AlgorithmCodeSource,
                "AlgorithmCode/model.alg",
            ),
            (
                AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                "AlgorithmCode/manifest.xml",
            ),
            (AlgorithmCodeArtifactRole::PackageManifest, "__content.xml"),
        ]
    }

    fn with_algorithm_code_source_path(
        role: AlgorithmCodeArtifactRole,
        original: &'static str,
        source_path: &'static str,
    ) -> (AlgorithmCodeArtifactRole, &'static str) {
        let path = if role == AlgorithmCodeArtifactRole::AlgorithmCodeSource {
            source_path
        } else {
            original
        };
        (role, path)
    }

    #[test]
    fn complete_layout_issues_total_exact_members_and_source_relation() {
        let layout = AlgorithmCodeArtifactLayout::construct(complete_layout()).unwrap();
        assert_eq!(layout.members().len(), 3);
        assert_eq!(
            layout
                .members()
                .iter()
                .map(AlgorithmCodeArtifactLayoutMember::role)
                .collect::<Vec<_>>(),
            vec![
                AlgorithmCodeArtifactRole::AlgorithmCodeSource,
                AlgorithmCodeArtifactRole::AlgorithmCodeManifest,
                AlgorithmCodeArtifactRole::PackageManifest,
            ],
            "layout must retain the target-issued producer-first sequence",
        );
        assert_eq!(
            layout.package_manifest().member_path().as_str(),
            "__content.xml"
        );
        assert_eq!(
            layout.algorithm_code_manifest().member_path().as_str(),
            "AlgorithmCode/manifest.xml"
        );
        assert_eq!(
            layout.algorithm_code_source().member_path().as_str(),
            "AlgorithmCode/model.alg"
        );
        assert_eq!(layout.algorithm_code_source_file().file_name(), "model.alg");
        assert_eq!(layout.algorithm_code_source_file().file_path(), "./");
    }

    #[test]
    fn incomplete_duplicate_and_relabelled_layouts_never_exist() {
        for removed_role in AlgorithmCodeArtifactRole::ALL {
            let drafts = complete_layout()
                .into_iter()
                .filter(|(role, _)| role != removed_role)
                .collect::<Vec<_>>();
            assert!(AlgorithmCodeArtifactLayout::construct(drafts).is_err());
        }

        let mut duplicate = complete_layout();
        duplicate.push((
            AlgorithmCodeArtifactRole::AlgorithmCodeSource,
            "AlgorithmCode/alternate.alg",
        ));
        assert!(matches!(
            AlgorithmCodeArtifactLayout::construct(duplicate),
            Err(AlgorithmCodeArtifactLayoutError::DuplicateRole {
                role: AlgorithmCodeArtifactRole::AlgorithmCodeSource
            })
        ));

        let relabelled = complete_layout().into_iter().map(|(role, path)| {
            if role == AlgorithmCodeArtifactRole::AlgorithmCodeSource {
                (role, "AlgorithmCode/other.alg")
            } else {
                (role, path)
            }
        });
        assert!(matches!(
            AlgorithmCodeArtifactLayout::construct(relabelled),
            Err(
                AlgorithmCodeArtifactLayoutError::StandardMemberPathMismatch {
                    role: AlgorithmCodeArtifactRole::AlgorithmCodeSource,
                    ..
                }
            )
        ));
    }

    #[test]
    fn dynamic_nonportable_and_case_colliding_members_never_exist() {
        for path in [
            "AlgorithmCode/{{ model_name }}.alg",
            "AlgorithmCode/../model.alg",
            "/AlgorithmCode/model.alg",
            "AlgorithmCode\\model.alg",
        ] {
            let drafts = complete_layout()
                .into_iter()
                .map(|(role, original)| with_algorithm_code_source_path(role, original, path));
            assert!(matches!(
                AlgorithmCodeArtifactLayout::construct(drafts),
                Err(AlgorithmCodeArtifactLayoutError::NonPortablePath { .. })
            ));
        }

        let mut collision = complete_layout();
        collision.push((
            AlgorithmCodeArtifactRole::AlgorithmCodeSource,
            "algorithmcode/MODEL.alg",
        ));
        assert!(matches!(
            AlgorithmCodeArtifactLayout::construct(collision),
            Err(AlgorithmCodeArtifactLayoutError::CaseCollision { .. })
        ));
    }
}
