//! Formal-statement registry: which formal statement justifies a pinned behavior.
//!
//! [`super`]'s contract registry and SPEC_0022 answer *what* the compiler
//! implements: one row per MLS requirement, with a status. That is not enough to
//! re-litigate a semantics decision, because it never says where the decision's
//! authority came from. A reader who finds `when true then` deleted, or a
//! counter frozen at its start value, cannot tell from the contract row whether
//! the behavior is what the Modelica Language Specification requires or merely
//! what OpenModelica happens to do.
//!
//! This registry answers that second question. Every row states one formal
//! statement in one sentence, names the tier the statement comes from, and
//! points at the place in the tree that holds the compiler to it.
//!
//! # The three tiers
//!
//! * [`StatementTier::SpecSourced`] — the statement is normative. It carries the
//!   MLS edition, the section, and the fragment of MLS text it rests on. A
//!   reader who disagrees with the row has to argue with the specification.
//! * [`StatementTier::OracleImplied`] — the specification does not decide the
//!   question, or decides it only up to an explicit freedom, and the behavior is
//!   pinned to what an oracle does instead. The row names the oracle, an
//!   evidence pointer, and a latitude note saying what the specification left
//!   open. A reader who disagrees with the row is free to argue for a different
//!   choice, and only has to re-measure the oracle to do it.
//! * [`StatementTier::SpecSilent`] — the specification does not decide the
//!   question and *no oracle can settle it either*, so the compiler chooses and
//!   says so. The row carries the rationale establishing the silence, the
//!   alternatives that were considered, and — when one was tried — which oracle
//!   was consulted and why it did not answer.
//!
//! The tiers are not a quality ranking. An `OracleImplied` row is often the more
//! load-bearing of the two named before it, and a `SpecSilent` row is the most
//! fragile of the three: nothing outside this registry records that the choice
//! was ever made.
//!
//! `SpecSilent` exists because the alternative is worse. A choice with no
//! authority behind it was previously filed as `OracleImplied` with an oracle
//! field reading "none", which is a contradiction in the schema and reads to a
//! later maintainer as though an oracle had agreed.
//!
//! # Quotes are grounded, never composed
//!
//! A fabricated specification quote is worse than no quote, so a `SpecSourced`
//! row may not introduce one. Its `quote` must already appear, verbatim, in the
//! compiler source or tests named by `quote_source` — the place where the
//! quotation was written and reviewed alongside the code it governs.
//! `tests/formal_statement_invariants.rs` checks this mechanically, comparing
//! the two with comment markers stripped and whitespace collapsed so a doc
//! comment may wrap the quotation across lines.
//!
//! Not every rule the compiler leans on is quoted verbatim somewhere in the
//! tree; plenty reach the source as the compiler's own reading of a section.
//! [`QuoteKind`] says which of the two a row carries, so a reader can tell an
//! MLS sentence from an interpretation of one without opening the file.
//!
//! # Pins, and which way a pin points
//!
//! Every row points at exactly one place, through [`StatementPin`]:
//!
//! * `Test` — a test function that a change to the pinned behavior makes fail.
//! * `Site` — the construction or enforcement site in production source.
//! * `Record` — a written record (a module header, a diagnostic) for a statement
//!   the compiler does not currently satisfy.
//!
//! A `Test` pin does not say by itself *which way* it points, and the difference
//! matters enough to be a field. A test under an `Enforced` row asserts the
//! statement, so it fails when the statement stops holding. A test under a
//! [`EnforcementStatus::RecordedDivergence`] row asserts what the compiler does
//! *instead*, so it fails when the statement starts holding — that is, when the
//! divergence is fixed. Reading the second as the first is how a divergence
//! record silently becomes a claim of compliance, so [`PinPolarity`] states it
//! and the invariants check it against the status.
//!
//! An [`EnforcementStatus::Enforced`] row must pin a `Test` or a `Site`: a
//! statement whose only evidence is prose is not enforced, whatever the prose
//! says.
//!
//! # Adding rows
//!
//! Rows live in `data/formal_statements.toml` and are hand written. Nothing
//! generates them, because the classification each row makes — which tier, and
//! which way its pin points — is a judgment, and a generator would launder it.
//!
//! Nothing here checks a section *number*. The invariants prove that a quote is
//! grounded in the tree and that a section is well formed; they cannot prove the
//! section is the right one, and a wrong number is invisible to every guard in
//! this crate. Attribution is a reviewer's job.

use serde::{Deserialize, Serialize};

use super::{ContractCategory, ContractId};

/// The Modelica Language Specification edition a statement is quoted from.
///
/// The compiler cites both editions on purpose: it targets 3.7, and 3.6 still
/// states some rules more precisely (and, for purity, differently). A row that
/// did not say which edition it quoted would be unre-checkable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MlsEdition {
    /// MLS 3.6.
    #[serde(rename = "3.6")]
    V3_6,
    /// MLS 3.7 — the edition the compiler declares as its target.
    #[serde(rename = "3.7")]
    V3_7,
}

impl std::fmt::Display for MlsEdition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            MlsEdition::V3_6 => "3.6",
            MlsEdition::V3_7 => "3.7",
        })
    }
}

/// Whether a `SpecSourced` row quotes the specification or reads it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum QuoteKind {
    /// The `quote` is MLS text, word for word.
    Verbatim,
    /// The `quote` is the compiler's own statement of what the section means.
    ///
    /// A paraphrase is still grounded — it is quoted from the tree, where it was
    /// written beside the code it governs — but it is an interpretation, and a
    /// reader re-litigating the row should read the section itself.
    Paraphrase,
}

/// Where a formal statement's authority comes from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementTier {
    /// The Modelica Language Specification states it.
    SpecSourced {
        /// Which edition the quotation is taken from.
        edition: MlsEdition,
        /// The MLS section, spelled with the section sign (`§8.3.5.1`).
        section: String,
        /// Whether `quote` is MLS text or the compiler's reading of it.
        quote_kind: QuoteKind,
        /// The fragment the statement rests on.
        quote: String,
        /// Where that fragment already appears in the tree.
        quote_source: String,
    },
    /// The specification leaves it open; an oracle decides it.
    OracleImplied {
        /// The oracle and the run configuration that produced the evidence.
        oracle: String,
        /// Where the evidence is recorded in the tree.
        evidence: String,
        /// What the specification leaves open, and why this is a choice.
        latitude_note: String,
    },
    /// The specification is silent and no oracle settles it; the compiler chose.
    SpecSilent {
        /// Why the silence is *established* rather than assumed.
        ///
        /// A rationale that only says the specification does not mention the
        /// case is not enough: it has to say where the reader looked and what
        /// the section it looked at does scope, so a later maintainer can tell a
        /// searched-for silence from an unread one.
        rationale: String,
        /// The other choices that were available, and what distinguishes them.
        alternatives_considered: String,
        /// Which oracle was consulted, when one was, and why it did not answer.
        oracle_consulted: Option<String>,
    },
}

impl StatementTier {
    /// The short tier label used in reports.
    pub fn label(&self) -> &'static str {
        match self {
            StatementTier::SpecSourced { .. } => "SpecSourced",
            StatementTier::OracleImplied { .. } => "OracleImplied",
            StatementTier::SpecSilent { .. } => "SpecSilent",
        }
    }
}

/// Which way a [`StatementPin`] points.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PinPolarity {
    /// The pin asserts the statement: it fails when the statement stops holding.
    Asserts,
    /// The pin asserts what the compiler does *instead* of the statement.
    ///
    /// Such a pin fails when the divergence is *fixed*, which is the point: the
    /// change that fixes it is forced through this row rather than past it.
    PinsDivergence,
}

/// The place that holds the compiler to a statement.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StatementPin {
    /// A `#[test]` function that fails when the statement stops holding.
    Test {
        /// Repository-relative path of the file holding the test.
        file: String,
        /// The test function's name, without `fn` or parentheses.
        function: String,
    },
    /// The construction or enforcement site in production source.
    Site {
        /// Repository-relative path of the file holding the site.
        file: String,
        /// An item name or other exact spelling that occurs in that file.
        symbol: String,
    },
    /// A written record of a statement the compiler does not satisfy.
    Record {
        /// Repository-relative path of the file holding the record.
        file: String,
        /// A phrase from the record, verbatim, so rewording revisits this row.
        anchor: String,
    },
}

impl StatementPin {
    /// The repository-relative file this pin names.
    pub fn file(&self) -> &str {
        match self {
            StatementPin::Test { file, .. }
            | StatementPin::Site { file, .. }
            | StatementPin::Record { file, .. } => file,
        }
    }

    /// The exact text that must occur in [`StatementPin::file`].
    pub fn needle(&self) -> String {
        match self {
            StatementPin::Test { function, .. } => format!("fn {function}("),
            StatementPin::Site { symbol, .. } => symbol.clone(),
            StatementPin::Record { anchor, .. } => anchor.clone(),
        }
    }

    /// Whether this pin is executable evidence rather than prose.
    pub fn is_executable(&self) -> bool {
        matches!(self, StatementPin::Test { .. } | StatementPin::Site { .. })
    }
}

/// What the compiler currently does about a statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EnforcementStatus {
    /// A test or construction site holds the compiler to it.
    Enforced,
    /// The compiler does not satisfy it, and the divergence is written down.
    RecordedDivergence,
    /// The statement is admitted but nothing in the compiler implements it yet.
    Unimplemented,
}

/// One formal statement, its tier, its pin, and its status.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormalStatement {
    /// Stable identifier, `FS-<CATEGORY>-<NNN>`.
    pub id: String,
    /// The SPEC_0022 category the identifier's middle segment names.
    pub category: ContractCategory,
    /// The formal statement itself, in one precise sentence.
    pub statement: String,
    /// Where the statement's authority comes from.
    pub tier: StatementTier,
    /// The place that holds the compiler to it.
    pub pin: StatementPin,
    /// Which way [`FormalStatement::pin`] points.
    pub pin_polarity: PinPolarity,
    /// What the compiler currently does about it.
    pub status: EnforcementStatus,
    /// SPEC_0022 contracts this statement refines, if any.
    pub contracts: Vec<ContractId>,
    /// Further MLS sections the statement touches without quoting.
    pub related_sections: Vec<String>,
    /// Anything a reader needs that the statement itself cannot carry.
    pub note: Option<String>,
}

impl FormalStatement {
    /// The identifier's category segment, e.g. `EQN` for `FS-EQN-001`.
    pub fn category_prefix(&self) -> &'static str {
        self.category.prefix()
    }
}

/// Why a row in `data/formal_statements.toml` is not a [`FormalStatement`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormalStatementError {
    /// The file is not valid TOML, or a row is missing a mandatory field.
    Toml(String),
    /// A row's identifier does not read `FS-<CATEGORY>-<NNN>`.
    MalformedId(String),
    /// A row's identifier names a category no SPEC_0022 prefix spells.
    UnknownCategory {
        /// The offending identifier.
        id: String,
        /// The category segment that matched no prefix.
        prefix: String,
    },
    /// A row's `tier` names neither tier.
    UnknownTier {
        /// The offending identifier.
        id: String,
        /// The `tier` value that matched neither variant.
        tier: String,
    },
    /// A row omits a field the tier it declares requires.
    MissingTierField {
        /// The offending identifier.
        id: String,
        /// The declared tier.
        tier: String,
        /// The field that tier requires.
        field: &'static str,
    },
    /// A row carries a field belonging to the tier it did not declare.
    ForeignTierField {
        /// The offending identifier.
        id: String,
        /// The declared tier.
        tier: String,
        /// The field that belongs to the other tier.
        field: &'static str,
    },
    /// A row omits a field every row requires, or leaves it blank.
    MissingField {
        /// The offending identifier.
        id: String,
        /// The blank or absent field.
        field: &'static str,
    },
    /// A row's pin points the wrong way for the status it declares.
    PinPolarityMismatch {
        /// The offending identifier.
        id: String,
        /// The declared status.
        status: &'static str,
        /// The polarity that status requires.
        required: &'static str,
    },
}

impl std::fmt::Display for FormalStatementError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FormalStatementError::Toml(message) => {
                write!(f, "formal_statements.toml is not loadable: {message}")
            }
            FormalStatementError::MalformedId(id) => {
                write!(f, "`{id}` is not spelled FS-<CATEGORY>-<NNN>")
            }
            FormalStatementError::UnknownCategory { id, prefix } => {
                write!(
                    f,
                    "`{id}` names category `{prefix}`, which SPEC_0022 has no prefix for"
                )
            }
            FormalStatementError::UnknownTier { id, tier } => {
                write!(
                    f,
                    "`{id}` declares tier `{tier}`, which is none of SpecSourced, OracleImplied, \
                     SpecSilent"
                )
            }
            FormalStatementError::MissingTierField { id, tier, field } => {
                write!(f, "`{id}` is {tier} but carries no `{field}`")
            }
            FormalStatementError::ForeignTierField { id, tier, field } => {
                write!(
                    f,
                    "`{id}` is {tier} but carries `{field}`, which belongs to another tier"
                )
            }
            FormalStatementError::MissingField { id, field } => {
                write!(f, "`{id}` carries no `{field}`")
            }
            FormalStatementError::PinPolarityMismatch {
                id,
                status,
                required,
            } => {
                write!(
                    f,
                    "`{id}` is {status}, so its pin must be `{required}`; reading a pin the wrong \
                     way turns a divergence record into a claim of compliance"
                )
            }
        }
    }
}

impl std::error::Error for FormalStatementError {}

/// One row of `data/formal_statements.toml`, before it is checked.
///
/// The TOML surface is deliberately flat, the way `contracts.toml` and
/// `contract_cases.toml` are: a row is readable and diffable without knowing the
/// Rust types. [`FormalStatement`]'s enums are recovered by [`check_row`], so a
/// row that names a tier without its fields never becomes a statement at all.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
struct RawStatement {
    id: String,
    statement: String,
    tier: String,
    #[serde(default)]
    edition: Option<MlsEdition>,
    #[serde(default)]
    section: Option<String>,
    #[serde(default)]
    quote_kind: Option<QuoteKind>,
    #[serde(default)]
    quote: Option<String>,
    #[serde(default)]
    quote_source: Option<String>,
    #[serde(default)]
    oracle: Option<String>,
    #[serde(default)]
    evidence: Option<String>,
    #[serde(default)]
    latitude_note: Option<String>,
    #[serde(default)]
    rationale: Option<String>,
    #[serde(default)]
    alternatives_considered: Option<String>,
    #[serde(default)]
    oracle_consulted: Option<String>,
    pin_kind: String,
    pin_file: String,
    pin_anchor: String,
    pin_polarity: PinPolarity,
    status: EnforcementStatus,
    #[serde(default)]
    contracts: Vec<String>,
    #[serde(default)]
    related_sections: Vec<String>,
    #[serde(default)]
    note: Option<String>,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct StatementsToml {
    statements: Vec<RawStatement>,
}

/// The category a `FS-<CATEGORY>-<NNN>` identifier names, and its ordinal.
fn split_id(id: &str) -> Option<(&str, &str)> {
    let rest = id.strip_prefix("FS-")?;
    let (prefix, digits) = rest.rsplit_once('-')?;
    let well_formed = !prefix.is_empty()
        && prefix.chars().all(|c| c.is_ascii_uppercase())
        && digits.len() == 3
        && digits.chars().all(|c| c.is_ascii_digit());
    well_formed.then_some((prefix, digits))
}

/// Which optional fields each tier owns.
///
/// A field present on a row whose tier does not own it is a
/// [`FormalStatementError::ForeignTierField`], which is what stops a row from
/// carrying an oracle *and* a quotation and leaving a reader to guess which one
/// the classification rests on.
const TIER_FIELDS: [(&str, &[&str]); 3] = [
    (
        "SpecSourced",
        &["edition", "section", "quote_kind", "quote", "quote_source"],
    ),
    ("OracleImplied", &["oracle", "evidence", "latitude_note"]),
    (
        "SpecSilent",
        &["rationale", "alternatives_considered", "oracle_consulted"],
    ),
];

/// Reject any field present on `row` that the declared tier does not own.
fn reject_foreign_tier_fields(row: &RawStatement) -> Result<(), FormalStatementError> {
    let present: [(&str, bool); 11] = [
        ("edition", row.edition.is_some()),
        ("section", row.section.is_some()),
        ("quote_kind", row.quote_kind.is_some()),
        ("quote", row.quote.is_some()),
        ("quote_source", row.quote_source.is_some()),
        ("oracle", row.oracle.is_some()),
        ("evidence", row.evidence.is_some()),
        ("latitude_note", row.latitude_note.is_some()),
        ("rationale", row.rationale.is_some()),
        (
            "alternatives_considered",
            row.alternatives_considered.is_some(),
        ),
        ("oracle_consulted", row.oracle_consulted.is_some()),
    ];
    let owned = TIER_FIELDS
        .iter()
        .find(|(tier, _)| *tier == row.tier)
        .map(|(_, fields)| *fields)
        .ok_or_else(|| FormalStatementError::UnknownTier {
            id: row.id.clone(),
            tier: row.tier.clone(),
        })?;
    for (field, is_present) in present {
        if is_present && !owned.contains(&field) {
            return Err(FormalStatementError::ForeignTierField {
                id: row.id.clone(),
                tier: row.tier.clone(),
                field: TIER_FIELDS
                    .iter()
                    .flat_map(|(_, fields)| fields.iter())
                    .find(|owned_field| **owned_field == field)
                    .copied()
                    .unwrap_or("unknown"),
            });
        }
    }
    Ok(())
}

/// Recover the tier a row declares, or say exactly which field is wrong.
fn check_tier(row: &RawStatement) -> Result<StatementTier, FormalStatementError> {
    reject_foreign_tier_fields(row)?;
    let id = row.id.clone();
    let tier = row.tier.clone();
    let missing = move |field| FormalStatementError::MissingTierField {
        id: id.clone(),
        tier: tier.clone(),
        field,
    };
    match row.tier.as_str() {
        "SpecSourced" => Ok(StatementTier::SpecSourced {
            edition: row.edition.ok_or_else(|| missing("edition"))?,
            section: non_blank(row.section.as_deref(), || missing("section"))?,
            quote_kind: row.quote_kind.ok_or_else(|| missing("quote_kind"))?,
            quote: non_blank(row.quote.as_deref(), || missing("quote"))?,
            quote_source: non_blank(row.quote_source.as_deref(), || missing("quote_source"))?,
        }),
        "OracleImplied" => Ok(StatementTier::OracleImplied {
            oracle: non_blank(row.oracle.as_deref(), || missing("oracle"))?,
            evidence: non_blank(row.evidence.as_deref(), || missing("evidence"))?,
            latitude_note: non_blank(row.latitude_note.as_deref(), || missing("latitude_note"))?,
        }),
        "SpecSilent" => Ok(StatementTier::SpecSilent {
            rationale: non_blank(row.rationale.as_deref(), || missing("rationale"))?,
            alternatives_considered: non_blank(row.alternatives_considered.as_deref(), || {
                missing("alternatives_considered")
            })?,
            oracle_consulted: row.oracle_consulted.clone(),
        }),
        other => Err(FormalStatementError::UnknownTier {
            id: row.id.clone(),
            tier: other.to_string(),
        }),
    }
}

fn non_blank(
    value: Option<&str>,
    error: impl Fn() -> FormalStatementError,
) -> Result<String, FormalStatementError> {
    match value {
        Some(text) if !text.trim().is_empty() => Ok(text.to_string()),
        _ => Err(error()),
    }
}

/// Recover the pin a row declares.
fn check_pin(row: &RawStatement) -> Result<StatementPin, FormalStatementError> {
    let file = row.pin_file.clone();
    let anchor = row.pin_anchor.clone();
    for (value, field) in [(&file, "pin_file"), (&anchor, "pin_anchor")] {
        if value.trim().is_empty() {
            return Err(FormalStatementError::MissingField {
                id: row.id.clone(),
                field,
            });
        }
    }
    match row.pin_kind.as_str() {
        "Test" => Ok(StatementPin::Test {
            file,
            function: anchor,
        }),
        "Site" => Ok(StatementPin::Site {
            file,
            symbol: anchor,
        }),
        "Record" => Ok(StatementPin::Record { file, anchor }),
        _ => Err(FormalStatementError::MissingField {
            id: row.id.clone(),
            field: "pin_kind",
        }),
    }
}

/// Reject a pin that points the wrong way for the status it sits under.
///
/// `Unimplemented` is deliberately unconstrained: such a row may pin the
/// abstention the compiler makes instead, or a record of what is missing, and
/// neither reading is wrong enough to legislate here.
fn check_pin_polarity(row: &RawStatement) -> Result<(), FormalStatementError> {
    let required = match row.status {
        EnforcementStatus::Enforced => Some((PinPolarity::Asserts, "Enforced", "Asserts")),
        EnforcementStatus::RecordedDivergence => Some((
            PinPolarity::PinsDivergence,
            "RecordedDivergence",
            "PinsDivergence",
        )),
        EnforcementStatus::Unimplemented => None,
    };
    match required {
        Some((polarity, status, name)) if row.pin_polarity != polarity => {
            Err(FormalStatementError::PinPolarityMismatch {
                id: row.id.clone(),
                status,
                required: name,
            })
        }
        _ => Ok(()),
    }
}

/// Turn one checked row into a statement, or say why it is not one.
fn check_row(row: RawStatement) -> Result<FormalStatement, FormalStatementError> {
    let (prefix, _) =
        split_id(&row.id).ok_or_else(|| FormalStatementError::MalformedId(row.id.clone()))?;
    let category = ContractCategory::from_prefix(prefix).ok_or_else(|| {
        FormalStatementError::UnknownCategory {
            id: row.id.clone(),
            prefix: prefix.to_string(),
        }
    })?;
    if row.statement.trim().is_empty() {
        return Err(FormalStatementError::MissingField {
            id: row.id.clone(),
            field: "statement",
        });
    }
    let tier = check_tier(&row)?;
    let pin = check_pin(&row)?;
    check_pin_polarity(&row)?;
    Ok(FormalStatement {
        category,
        statement: row.statement,
        tier,
        pin,
        pin_polarity: row.pin_polarity,
        status: row.status,
        contracts: row.contracts.iter().map(ContractId::new).collect(),
        related_sections: row.related_sections,
        note: row.note,
        id: row.id,
    })
}

/// Parse and check a formal-statement table.
///
/// Exposed so a test can assert that a malformed row is *refused*: the check is
/// the registry's guarantee, and a guarantee nothing exercises is a comment.
pub fn parse_formal_statements(raw: &str) -> Result<Vec<FormalStatement>, FormalStatementError> {
    let parsed: StatementsToml =
        toml::from_str(raw).map_err(|error| FormalStatementError::Toml(error.to_string()))?;
    parsed.statements.into_iter().map(check_row).collect()
}

/// Load every formal statement from the embedded TOML data file.
///
/// # Panics
///
/// Panics when the embedded table is malformed. The table ships inside the
/// binary and the invariant tests parse it on every run, so a failure here is
/// a build-time defect, not a runtime condition.
pub fn load_all_formal_statements() -> Vec<FormalStatement> {
    parse_formal_statements(include_str!("../../data/formal_statements.toml"))
        .expect("formal_statements.toml ships in the binary and is gated by formal_statement_invariants; a malformed table is a build-time defect")
}
