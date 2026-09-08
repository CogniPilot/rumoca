use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use anyhow::{Result, ensure};

use super::{
    CandidateClaim, CandidateConeNode, CapturePlan, CaptureScenario, ContentBinding, Endpoint,
    GoldenModelRecord, GoldenProfile, GoldenRegistry, ModelClaim, OwnerId, ReviewedClaim,
    TerminalDisposition, checked_repo_path, ensure_exact_test_identity, ensure_safe_atom,
    ensure_safe_id, scenario_identity, sha256_file, validate_compliance_instant,
};

type ProfileConeEdge = (OwnerId, &'static [OwnerId]);

/// The only checked product schema 2 can construct today. It borrows the
/// exact registry record whose source, review record, scenarios, cone, and
/// optional capture footprint were checked. There is no reviewed/golden
/// counterpart until semantic evidence and a non-self-referential production
/// snapshot have an implemented admission checker.
pub struct CheckedCandidateCapture<'record> {
    id: &'record str,
    source: &'record ContentBinding,
    scenarios: &'record [CaptureScenario],
}

impl<'record> CheckedCandidateCapture<'record> {
    pub fn id(&self) -> &str {
        self.id
    }

    pub fn source(&self) -> &ContentBinding {
        self.source
    }

    pub fn scenarios(&self) -> &[CaptureScenario] {
        self.scenarios
    }
}

/// Typed refusal for the deliberately absent reviewed-admission checker.
/// Callers and tests can downcast this through `anyhow::Error`; matching the
/// text is never the authority for deciding whether admission is implemented.
#[derive(Debug)]
pub struct ReviewedAdmissionNotImplemented {
    model_id: String,
}

impl fmt::Display for ReviewedAdmissionNotImplemented {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "reviewed golden-model admission is not implemented for `{}`",
            self.model_id
        )
    }
}

impl Error for ReviewedAdmissionNotImplemented {}

/// Linear capability proving that one owned registry contained only checked
/// candidates. It has no Clone, Default, or wire construction route.
pub struct CandidateOnlyRegistryAdmission {
    _sealed: (),
}

/// Validate an entire registry for the candidate-only aggregate. The return
/// type intentionally carries no model or coverage data: successful checking
/// proves only that every record is a well-formed candidate, so the aggregate
/// has no representable numerator source.
pub fn check_candidate_only_registry(
    root: &Path,
    registry: GoldenRegistry,
) -> Result<CandidateOnlyRegistryAdmission> {
    for model in &registry.models {
        let _checked_candidate = check_candidate_capture(root, model)?;
    }
    Ok(CandidateOnlyRegistryAdmission { _sealed: () })
}

/// Validate one registry record for candidate capture. Candidates are the
/// only constructible result and therefore can never enter the official
/// aggregate. A well-formed reviewed record receives the typed refusal above;
/// malformed common evidence is rejected before that refusal is issued.
pub fn check_candidate_capture<'record>(
    root: &Path,
    model: &'record GoldenModelRecord,
) -> Result<CheckedCandidateCapture<'record>> {
    ensure_safe_id(&model.id)?;
    match &model.claim {
        ModelClaim::Candidate(candidate) => {
            validate_candidate_claim(root, model.profile, candidate)?;
            Ok(CheckedCandidateCapture {
                id: &model.id,
                source: &candidate.capture.source,
                scenarios: &candidate.capture.scenarios,
            })
        }
        ModelClaim::Reviewed(reviewed) => {
            validate_reviewed_record_shape(root, model.profile, reviewed)?;
            Err(ReviewedAdmissionNotImplemented {
                model_id: model.id.clone(),
            }
            .into())
        }
    }
}

fn validate_candidate_claim(
    root: &Path,
    profile: GoldenProfile,
    candidate: &CandidateClaim,
) -> Result<()> {
    validate_capture_plan(
        root,
        profile,
        &candidate.reason,
        &candidate.last_compliance_check,
        &candidate.capture,
    )?;
    if let Some(footprint) = &candidate.coverage_footprint {
        validate_content_binding(root, footprint)?;
    }
    Ok(())
}

fn validate_reviewed_record_shape(
    root: &Path,
    profile: GoldenProfile,
    reviewed: &ReviewedClaim,
) -> Result<()> {
    let dispositions = validate_capture_plan(
        root,
        profile,
        &reviewed.reason,
        &reviewed.last_compliance_check,
        &reviewed.capture,
    )?;
    let mut outstanding = BTreeSet::new();
    for receipt in &reviewed.outstanding {
        ensure!(
            !receipt.reason.trim().is_empty(),
            "outstanding entry for {:?} has no reason",
            receipt.endpoint
        );
        ensure!(
            outstanding.insert(receipt.endpoint),
            "outstanding entry for {:?} is repeated",
            receipt.endpoint
        );
        ensure!(
            matches!(
                dispositions.get(&receipt.endpoint),
                Some(TerminalDisposition::Blocked | TerminalDisposition::Unmet)
            ),
            "outstanding entry for {:?} names an endpoint that is not blocked",
            receipt.endpoint
        );
    }
    for (endpoint, disposition) in &dispositions {
        if matches!(
            disposition,
            TerminalDisposition::Blocked | TerminalDisposition::Unmet
        ) {
            ensure!(
                outstanding.contains(endpoint),
                "blocked endpoint {endpoint:?} has no outstanding entry"
            );
        }
    }
    validate_content_binding(root, &reviewed.coverage_footprint)?;
    Ok(())
}

fn validate_capture_plan(
    root: &Path,
    profile: GoldenProfile,
    reason: &str,
    last_compliance_check: &str,
    capture: &CapturePlan,
) -> Result<BTreeMap<Endpoint, TerminalDisposition>> {
    ensure!(!reason.trim().is_empty(), "claim has no reason");
    validate_compliance_instant(last_compliance_check)?;
    validate_content_binding(root, &capture.source)?;
    validate_content_binding(root, &capture.review_record)?;
    ensure!(!capture.scenarios.is_empty(), "capture has no scenarios");
    let owners = validate_capture_cone(profile, &capture.cone)?;
    let mut ids = BTreeSet::new();
    let mut tests = BTreeSet::new();
    let mut dispositions = BTreeMap::new();
    for scenario in &capture.scenarios {
        validate_capture_scenario(scenario)?;
        ensure!(ids.insert(&scenario.id), "duplicate capture scenario id");
        ensure!(
            tests.insert(scenario_identity(&scenario.test)?),
            "duplicate capture exact test"
        );
        validate_scenario_requests(scenario, &owners, &mut dispositions)?;
    }
    let requested: BTreeSet<Endpoint> = dispositions.keys().copied().collect();
    ensure!(
        requested == profile_endpoints(profile),
        "capture scenarios do not exactly request the selected profile's endpoints"
    );
    Ok(dispositions)
}

fn profile_endpoints(profile: GoldenProfile) -> BTreeSet<Endpoint> {
    let endpoints: &[Endpoint] = match profile {
        GoldenProfile::UnitDerivativeV1 => &[
            Endpoint::DaeSolveProductionRefinement,
            Endpoint::NativeInterpreterRkTrace,
            Endpoint::Fmi3ModelDescriptionXml,
            Endpoint::Fmi3BuildDescriptionXml,
            Endpoint::Fmi3ModelSourceCc,
            Endpoint::Fmi3ArchiveMembership,
            Endpoint::Fmi3MeDerivativeStateLifecycle,
            Endpoint::Fmi3CsDoStepLifecycle,
            Endpoint::DirectGalecContinuousRefusal,
            Endpoint::RegisteredGalecContinuousRefusal,
            Endpoint::RegisteredEfmuContinuousRefusal,
        ],
        GoldenProfile::ParameterDecayV1 => &[
            Endpoint::NativeInterpreterRkTrace,
            Endpoint::DirectGalecContinuousRefusal,
            Endpoint::RegisteredGalecContinuousRefusal,
            Endpoint::RegisteredEfmuContinuousRefusal,
        ],
    };
    endpoints.iter().copied().collect()
}

fn validate_capture_cone(
    profile: GoldenProfile,
    cone: &[CandidateConeNode],
) -> Result<BTreeSet<OwnerId>> {
    let expected = profile_cone(profile);
    ensure!(
        cone.len() == expected.len(),
        "capture cone does not have the selected profile's exact owner set"
    );
    for (node, (owner, predecessors)) in cone.iter().zip(expected) {
        ensure!(
            node.owner == *owner && node.predecessors.as_slice() == *predecessors,
            "capture cone does not have the selected profile's exact ordered edges"
        );
    }
    Ok(expected.iter().map(|(owner, _)| *owner).collect())
}

/// The exact cone is part of the closed profile, not user-extensible evidence.
/// A registry record can select a profile but cannot redraw its compiler path.
pub(super) fn profile_cone(profile: GoldenProfile) -> &'static [ProfileConeEdge] {
    match profile {
        GoldenProfile::UnitDerivativeV1 => &[
            (OwnerId::CanonicalSource, &[]),
            (OwnerId::StrictParse, &[OwnerId::CanonicalSource]),
            (OwnerId::ResolvedAst, &[OwnerId::StrictParse]),
            (OwnerId::FlatIr, &[OwnerId::ResolvedAst]),
            (OwnerId::DaeIr, &[OwnerId::FlatIr]),
            (OwnerId::PreparedDae, &[OwnerId::DaeIr]),
            (OwnerId::SolveIr, &[OwnerId::PreparedDae]),
            (
                OwnerId::DaeSolveVariableRefinement,
                &[OwnerId::DaeIr, OwnerId::SolveIr],
            ),
            (
                OwnerId::DaeSolveEquationRefinement,
                &[
                    OwnerId::DaeIr,
                    OwnerId::SolveIr,
                    OwnerId::DaeSolveVariableRefinement,
                ],
            ),
            (
                OwnerId::RuntimeFmiComponent,
                &[OwnerId::SolveIr, OwnerId::DaeSolveVariableRefinement],
            ),
            (OwnerId::NativeMeHost, &[OwnerId::RuntimeFmiComponent]),
            (OwnerId::NativeTrace, &[OwnerId::NativeMeHost]),
            (OwnerId::Fmi3TargetAdmission, &[OwnerId::SolveIr]),
            (OwnerId::Fmi3Component, &[OwnerId::Fmi3TargetAdmission]),
            (OwnerId::Fmi3PreparedEmission, &[OwnerId::Fmi3Component]),
            (
                OwnerId::Fmi3ModelDescription,
                &[OwnerId::Fmi3PreparedEmission],
            ),
            (
                OwnerId::Fmi3BuildDescription,
                &[OwnerId::Fmi3PreparedEmission],
            ),
            (OwnerId::Fmi3ModelSource, &[OwnerId::Fmi3PreparedEmission]),
            (
                OwnerId::Fmi3Archive,
                &[
                    OwnerId::Fmi3ModelDescription,
                    OwnerId::Fmi3BuildDescription,
                    OwnerId::Fmi3ModelSource,
                ],
            ),
            (
                OwnerId::Fmi3MeLifecycle,
                &[OwnerId::Fmi3ModelDescription, OwnerId::Fmi3ModelSource],
            ),
            (
                OwnerId::Fmi3CsLifecycle,
                &[OwnerId::Fmi3ModelDescription, OwnerId::Fmi3ModelSource],
            ),
            (OwnerId::DirectGalecRefusal, &[OwnerId::DaeIr]),
            (OwnerId::RegisteredGalecRefusal, &[OwnerId::DaeIr]),
            (OwnerId::RegisteredEfmuRefusal, &[OwnerId::DaeIr]),
        ],
        GoldenProfile::ParameterDecayV1 => &[
            (OwnerId::CanonicalSource, &[]),
            (OwnerId::StrictParse, &[OwnerId::CanonicalSource]),
            (OwnerId::ResolvedAst, &[OwnerId::StrictParse]),
            (OwnerId::FlatIr, &[OwnerId::ResolvedAst]),
            (OwnerId::DaeIr, &[OwnerId::FlatIr]),
            (OwnerId::PreparedDae, &[OwnerId::DaeIr]),
            (OwnerId::SolveIr, &[OwnerId::PreparedDae]),
            (
                OwnerId::DaeSolveVariableRefinement,
                &[OwnerId::DaeIr, OwnerId::SolveIr],
            ),
            (
                OwnerId::RuntimeFmiComponent,
                &[OwnerId::SolveIr, OwnerId::DaeSolveVariableRefinement],
            ),
            (OwnerId::NativeMeHost, &[OwnerId::RuntimeFmiComponent]),
            (OwnerId::NativeTrace, &[OwnerId::NativeMeHost]),
            (OwnerId::DirectGalecRefusal, &[OwnerId::DaeIr]),
            (OwnerId::RegisteredGalecRefusal, &[OwnerId::DaeIr]),
            (OwnerId::RegisteredEfmuRefusal, &[OwnerId::DaeIr]),
        ],
    }
}

fn validate_scenario_requests(
    scenario: &CaptureScenario,
    owners: &BTreeSet<OwnerId>,
    dispositions: &mut BTreeMap<Endpoint, TerminalDisposition>,
) -> Result<()> {
    ensure!(
        !scenario.requests.is_empty(),
        "capture scenario has no endpoint request"
    );
    for request in &scenario.requests {
        let owner = endpoint_owner(request.endpoint);
        ensure!(
            owners.contains(&owner),
            "endpoint owner is outside capture cone"
        );
        ensure!(
            dispositions
                .insert(request.endpoint, request.disposition)
                .is_none(),
            "endpoint is requested more than once"
        );
        let refusal_endpoint = matches!(
            request.endpoint,
            Endpoint::DirectGalecContinuousRefusal
                | Endpoint::RegisteredGalecContinuousRefusal
                | Endpoint::RegisteredEfmuContinuousRefusal
        );
        let completed_as_refusal = match request.disposition {
            TerminalDisposition::Observed => Some(false),
            TerminalDisposition::TypedRefusal => Some(true),
            TerminalDisposition::Blocked | TerminalDisposition::Unmet => None,
        };
        if let Some(completed_as_refusal) = completed_as_refusal {
            ensure!(
                refusal_endpoint == completed_as_refusal,
                "endpoint has a contradictory terminal disposition"
            );
        }
    }
    Ok(())
}

fn endpoint_owner(endpoint: Endpoint) -> OwnerId {
    match endpoint {
        Endpoint::DaeSolveProductionRefinement => OwnerId::DaeSolveEquationRefinement,
        Endpoint::NativeInterpreterRkTrace => OwnerId::NativeTrace,
        Endpoint::Fmi3ModelDescriptionXml => OwnerId::Fmi3ModelDescription,
        Endpoint::Fmi3BuildDescriptionXml => OwnerId::Fmi3BuildDescription,
        Endpoint::Fmi3ModelSourceCc => OwnerId::Fmi3ModelSource,
        Endpoint::Fmi3ArchiveMembership => OwnerId::Fmi3Archive,
        Endpoint::Fmi3MeDerivativeStateLifecycle => OwnerId::Fmi3MeLifecycle,
        Endpoint::Fmi3CsDoStepLifecycle => OwnerId::Fmi3CsLifecycle,
        Endpoint::DirectGalecContinuousRefusal => OwnerId::DirectGalecRefusal,
        Endpoint::RegisteredGalecContinuousRefusal => OwnerId::RegisteredGalecRefusal,
        Endpoint::RegisteredEfmuContinuousRefusal => OwnerId::RegisteredEfmuRefusal,
    }
}

fn validate_content_binding(root: &Path, binding: &ContentBinding) -> Result<PathBuf> {
    let path = validate_recorded_binding(root, binding)?;
    ensure!(
        sha256_file(&path)? == binding.sha256,
        "stale content binding for `{}`",
        binding.path
    );
    Ok(path)
}

/// The shape half of a content binding: a well-formed digest over a
/// repository-relative path that exists. The digest is not compared.
fn validate_recorded_binding(root: &Path, binding: &ContentBinding) -> Result<PathBuf> {
    ensure!(
        binding.sha256.len() == 64
            && binding
                .sha256
                .bytes()
                .all(|byte| byte.is_ascii_hexdigit() && !byte.is_ascii_uppercase()),
        "invalid SHA-256 binding `{}`",
        binding.sha256
    );
    checked_repo_path(root, &binding.path)
}

fn validate_capture_scenario(scenario: &CaptureScenario) -> Result<()> {
    ensure_safe_id(&scenario.id)?;
    ensure_safe_atom("package", &scenario.test.package)?;
    ensure_safe_atom("test target", &scenario.test.test_target)?;
    ensure_exact_test_identity(&scenario.test.test_name)?;
    let mut prior = None;
    for feature in &scenario.test.features {
        ensure_safe_atom("feature", feature)?;
        ensure!(
            prior.is_none_or(|value| value < feature),
            "scenario `{}` features are not strictly sorted and unique",
            scenario.id
        );
        prior = Some(feature);
    }
    Ok(())
}
