//! Compliance reporting for contract testing.

use crate::registry::{ContractCategory, ContractStatus};
use crate::runner::TestRunner;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

/// Compliance report for MLS contract testing.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceReport {
    /// Overall statistics.
    pub summary: ReportSummary,
    /// Per-category statistics.
    pub categories: IndexMap<ContractCategory, CategoryStats>,
    /// Per-tier statistics.
    pub tiers: TierStats,
    /// Individual contract results.
    pub results: Vec<ContractResultEntry>,
}

/// Overall compliance summary.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReportSummary {
    /// Total number of contracts.
    pub total_contracts: usize,
    /// Number of implemented contracts.
    pub implemented: usize,
    /// Number of passing tests.
    pub passing: usize,
    /// Number of failing tests.
    pub failing: usize,
    /// Number of not implemented contracts.
    pub not_implemented: usize,
    /// Number of not applicable contracts.
    pub not_applicable: usize,
    /// Overall compliance percentage.
    pub compliance_percent: f64,
}

/// Statistics for a single category.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CategoryStats {
    /// Category name.
    pub category: ContractCategory,
    /// Total contracts in this category.
    pub total: usize,
    /// Implemented contracts.
    pub implemented: usize,
    /// Passing tests.
    pub passing: usize,
    /// Failing tests.
    pub failing: usize,
    /// Compliance percentage for this category.
    pub compliance_percent: f64,
}

/// Statistics by implementation tier.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TierStats {
    /// Tier 1 (MVP) statistics.
    pub tier1: TierSummary,
    /// Tier 2 (Standard) statistics.
    pub tier2: TierSummary,
    /// Tier 3 (Advanced) statistics.
    pub tier3: TierSummary,
}

/// Summary for a single tier.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TierSummary {
    /// Total contracts in this tier.
    pub total: usize,
    /// Implemented contracts.
    pub implemented: usize,
    /// Passing tests.
    pub passing: usize,
    /// Compliance percentage.
    pub compliance_percent: f64,
}

/// Individual contract result entry.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContractResultEntry {
    /// Contract ID.
    pub id: String,
    /// Contract description.
    pub description: String,
    /// Category.
    pub category: ContractCategory,
    /// Implementation status.
    pub status: ContractStatus,
    /// Test result (if run).
    pub test_passed: Option<bool>,
    /// Error message (if failed).
    pub error: Option<String>,
    /// Execution time in milliseconds.
    pub duration_ms: Option<u64>,
}

impl ComplianceReport {
    /// Generate a compliance report from test results.
    pub fn generate(runner: &TestRunner) -> Self {
        let registry = runner.registry();
        let results = runner.results();

        // Build per-category stats
        let mut categories = IndexMap::new();
        for category in all_categories() {
            let contracts: Vec<_> = registry.by_category(category).collect();
            let total = contracts.len();
            let implemented = contracts
                .iter()
                .filter(|c| c.status == ContractStatus::Implemented)
                .count();

            let passing = contracts
                .iter()
                .filter(|c| results.get(&c.id).map(|r| r.passed).unwrap_or(false))
                .count();

            let failing = contracts
                .iter()
                .filter(|c| results.get(&c.id).map(|r| !r.passed).unwrap_or(false))
                .count();

            let compliance_percent = if total > 0 {
                (passing as f64 / total as f64) * 100.0
            } else {
                0.0
            };

            categories.insert(
                category,
                CategoryStats {
                    category,
                    total,
                    implemented,
                    passing,
                    failing,
                    compliance_percent,
                },
            );
        }

        // Build summary
        let total_contracts = registry.len();
        let implemented = registry
            .all()
            .filter(|c| c.status == ContractStatus::Implemented)
            .count();
        let not_implemented = registry
            .all()
            .filter(|c| c.status == ContractStatus::NotImplemented)
            .count();
        let not_applicable = registry
            .all()
            .filter(|c| c.status == ContractStatus::NotApplicable)
            .count();

        let passing = results.values().filter(|r| r.passed).count();
        let failing = results.values().filter(|r| !r.passed).count();

        let compliance_percent = if total_contracts > 0 {
            (passing as f64 / total_contracts as f64) * 100.0
        } else {
            0.0
        };

        let summary = ReportSummary {
            total_contracts,
            implemented,
            passing,
            failing,
            not_implemented,
            not_applicable,
            compliance_percent,
        };

        // Build tier stats (placeholder - would need tier assignment in contracts)
        let tiers = TierStats::default();

        // Build individual results
        let mut result_entries = Vec::new();
        for contract in registry.all() {
            let test_result = results.get(&contract.id);
            result_entries.push(ContractResultEntry {
                id: contract.id.to_string(),
                description: contract.requirement.clone(),
                category: contract.category,
                status: contract.status,
                test_passed: test_result.map(|r| r.passed),
                error: test_result.and_then(|r| r.error.clone()),
                duration_ms: test_result.map(|r| r.duration_ms),
            });
        }

        Self {
            summary,
            categories,
            tiers,
            results: result_entries,
        }
    }

    /// Generate a markdown report.
    pub fn to_markdown(&self) -> String {
        let mut md = String::new();

        md.push_str("# MLS Compliance Report\n\n");

        // Summary section
        md.push_str("## Summary\n\n");
        md.push_str("| Metric | Value |\n|--------|-------|\n");
        md.push_str(&format!(
            "| Total Contracts | {} |\n",
            self.summary.total_contracts
        ));
        md.push_str(&format!("| Implemented | {} |\n", self.summary.implemented));
        md.push_str(&format!("| Passing | {} |\n", self.summary.passing));
        md.push_str(&format!("| Failing | {} |\n", self.summary.failing));
        md.push_str(&format!(
            "| Not Implemented | {} |\n",
            self.summary.not_implemented
        ));
        md.push_str(&format!(
            "| **Compliance** | **{:.1}%** |\n\n",
            self.summary.compliance_percent
        ));

        // Per-category breakdown
        md.push_str("## Category Breakdown\n\n");
        md.push_str("| Category | Total | Implemented | Passing | Compliance |\n");
        md.push_str("|----------|-------|-------------|---------|------------|\n");

        for (category, stats) in &self.categories {
            md.push_str(&format!(
                "| {:?} | {} | {} | {} | {:.1}% |\n",
                category, stats.total, stats.implemented, stats.passing, stats.compliance_percent
            ));
        }

        md.push('\n');

        // Failed tests section (if any)
        let failed: Vec<_> = self
            .results
            .iter()
            .filter(|r| r.test_passed == Some(false))
            .collect();

        if !failed.is_empty() {
            md.push_str("## Failed Tests\n\n");
            for result in failed {
                write_failed_result(&mut md, result);
            }
        }

        md
    }

    /// Generate a JSON report.
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
}

/// Write a single failed result entry to markdown.
fn write_failed_result(md: &mut String, result: &ContractResultEntry) {
    md.push_str(&format!("### {}\n\n", result.id));
    md.push_str(&format!("**Description:** {}\n\n", result.description));
    md.push_str(&format!("**Category:** {:?}\n\n", result.category));
    if let Some(ref error) = result.error {
        md.push_str(&format!("**Error:** {}\n\n", error));
    }
}

/// Get all contract categories.
fn all_categories() -> Vec<ContractCategory> {
    vec![
        ContractCategory::Lexical,
        ContractCategory::Declaration,
        ContractCategory::Instantiation,
        ContractCategory::Expression,
        ContractCategory::Equation,
        ContractCategory::Algorithm,
        ContractCategory::Connection,
        ContractCategory::Function,
        ContractCategory::Type,
        ContractCategory::Array,
        ContractCategory::Package,
        ContractCategory::OperatorRecord,
        ContractCategory::Simulation,
        ContractCategory::Clock,
        ContractCategory::Stream,
        ContractCategory::StateMachine,
        ContractCategory::Annotation,
        ContractCategory::Unit,
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::{Contract, ContractCategory, ContractRegistry, ContractStatus};
    use crate::runner::TestRunner;

    fn sample_runner() -> TestRunner {
        let mut registry = ContractRegistry::new();
        registry.register(
            Contract::new(
                "LEX-001",
                ContractCategory::Lexical,
                "ASCII identifiers",
                "§2.3",
                "ASCII-only identifiers",
            )
            .with_status(ContractStatus::Implemented),
        );
        registry.register(
            Contract::new(
                "DECL-001",
                ContractCategory::Declaration,
                "Name uniqueness",
                "§4.4",
                "No duplicate declarations",
            )
            .with_status(ContractStatus::Implemented),
        );
        registry.register(
            Contract::new(
                "SIM-001",
                ContractCategory::Simulation,
                "Initialization",
                "App B",
                "Consistent initialization",
            )
            .with_status(ContractStatus::NotImplemented),
        );

        let mut runner = TestRunner::new(registry);
        runner.register_test("LEX-001", || Ok(()));
        runner.register_test("DECL-001", || Err("duplicate declaration".to_string()));
        runner.run_all();
        runner
    }

    #[test]
    fn test_generate_report_summary_and_category_counts() {
        let runner = sample_runner();
        let report = ComplianceReport::generate(&runner);

        assert_eq!(report.summary.total_contracts, 3);
        assert_eq!(report.summary.implemented, 2);
        assert_eq!(report.summary.not_implemented, 1);
        assert_eq!(report.summary.passing, 1);
        assert_eq!(report.summary.failing, 1);
        assert!((report.summary.compliance_percent - 33.333).abs() < 0.01);

        assert_eq!(report.categories.len(), 18);
        let lex = report
            .categories
            .get(&ContractCategory::Lexical)
            .expect("missing lexical stats");
        assert_eq!(lex.total, 1);
        assert_eq!(lex.implemented, 1);
        assert_eq!(lex.passing, 1);
        assert_eq!(lex.failing, 0);
    }

    #[test]
    fn test_markdown_generation_includes_failure_details() {
        let runner = sample_runner();
        let report = ComplianceReport::generate(&runner);
        let md = report.to_markdown();

        assert!(md.contains("# MLS Compliance Report"));
        assert!(md.contains("## Summary"));
        assert!(md.contains("## Category Breakdown"));
        assert!(md.contains("## Failed Tests"));
        assert!(md.contains("DECL-001"));
        assert!(md.contains("duplicate declaration"));
    }

    #[test]
    fn test_json_generation_roundtrip() {
        let runner = sample_runner();
        let report = ComplianceReport::generate(&runner);
        let json = report.to_json().expect("json generation failed");
        let parsed: ComplianceReport = serde_json::from_str(&json).expect("json parse failed");
        assert_eq!(parsed.summary.total_contracts, 3);
        assert_eq!(parsed.summary.failing, 1);
    }
}
