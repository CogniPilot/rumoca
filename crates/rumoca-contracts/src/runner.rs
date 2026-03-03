//! Test runner for contract testing.

use crate::registry::{ContractId, ContractRegistry};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

/// Result of running a contract test.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContractResult {
    /// Contract ID.
    pub id: ContractId,
    /// Whether the test passed.
    pub passed: bool,
    /// Error message if failed.
    pub error: Option<String>,
    /// Execution time in milliseconds.
    pub duration_ms: u64,
}

impl ContractResult {
    /// Create a passing result.
    pub fn pass(id: ContractId, duration_ms: u64) -> Self {
        Self {
            id,
            passed: true,
            error: None,
            duration_ms,
        }
    }

    /// Create a failing result.
    pub fn fail(id: ContractId, error: impl Into<String>, duration_ms: u64) -> Self {
        Self {
            id,
            passed: false,
            error: Some(error.into()),
            duration_ms,
        }
    }

    /// Create a skipped result.
    pub fn skip(id: ContractId) -> Self {
        Self {
            id,
            passed: false,
            error: Some("skipped".to_string()),
            duration_ms: 0,
        }
    }
}

/// Test runner for contract tests.
pub struct TestRunner {
    /// The contract registry.
    registry: ContractRegistry,
    /// Results from test runs.
    results: IndexMap<ContractId, ContractResult>,
    /// Test functions by contract ID.
    tests: IndexMap<ContractId, Box<dyn Fn() -> Result<(), String> + Send + Sync>>,
}

impl TestRunner {
    /// Create a new test runner.
    pub fn new(registry: ContractRegistry) -> Self {
        Self {
            registry,
            results: IndexMap::new(),
            tests: IndexMap::new(),
        }
    }

    /// Register a test function for a contract.
    pub fn register_test<F>(&mut self, id: &str, test: F)
    where
        F: Fn() -> Result<(), String> + Send + Sync + 'static,
    {
        self.tests.insert(ContractId::new(id), Box::new(test));
    }

    /// Run all registered tests.
    pub fn run_all(&mut self) {
        for (id, test) in &self.tests {
            let start = std::time::Instant::now();
            let result = match test() {
                Ok(()) => ContractResult::pass(id.clone(), start.elapsed().as_millis() as u64),
                Err(e) => ContractResult::fail(id.clone(), e, start.elapsed().as_millis() as u64),
            };
            self.results.insert(id.clone(), result);
        }
    }

    /// Run tests for a specific category.
    pub fn run_category(&mut self, category: crate::registry::ContractCategory) {
        for contract in self.registry.by_category(category) {
            let Some(test) = self.tests.get(&contract.id) else {
                continue;
            };
            let result = self.run_single_test(&contract.id, test);
            self.results.insert(contract.id.clone(), result);
        }
    }

    /// Run a single test and return the result.
    fn run_single_test(
        &self,
        id: &ContractId,
        test: &(dyn Fn() -> Result<(), String> + Send + Sync),
    ) -> ContractResult {
        let start = std::time::Instant::now();
        match test() {
            Ok(()) => ContractResult::pass(id.clone(), start.elapsed().as_millis() as u64),
            Err(e) => ContractResult::fail(id.clone(), e, start.elapsed().as_millis() as u64),
        }
    }

    /// Get test results.
    pub fn results(&self) -> &IndexMap<ContractId, ContractResult> {
        &self.results
    }

    /// Get the registry.
    pub fn registry(&self) -> &ContractRegistry {
        &self.registry
    }

    /// Count passed tests.
    pub fn passed_count(&self) -> usize {
        self.results.values().filter(|r| r.passed).count()
    }

    /// Count failed tests.
    pub fn failed_count(&self) -> usize {
        self.results.values().filter(|r| !r.passed).count()
    }

    /// Count registered tests.
    pub fn test_count(&self) -> usize {
        self.tests.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::registry::{Contract, ContractCategory, ContractStatus};

    fn sample_registry() -> ContractRegistry {
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
        registry
    }

    #[test]
    fn run_all_records_pass_and_fail_results() {
        let mut runner = TestRunner::new(sample_registry());
        runner.register_test("LEX-001", || Ok(()));
        runner.register_test("DECL-001", || Err("duplicate name".to_string()));

        runner.run_all();

        assert_eq!(runner.test_count(), 2);
        assert_eq!(runner.results().len(), 2);
        assert_eq!(runner.passed_count(), 1);
        assert_eq!(runner.failed_count(), 1);

        let fail = runner
            .results()
            .get(&ContractId::new("DECL-001"))
            .expect("missing failing result");
        assert!(!fail.passed);
        assert_eq!(fail.error.as_deref(), Some("duplicate name"));
    }

    #[test]
    fn run_category_executes_only_matching_contracts() {
        let mut runner = TestRunner::new(sample_registry());
        runner.register_test("LEX-001", || Ok(()));
        runner.register_test("DECL-001", || Ok(()));

        runner.run_category(ContractCategory::Lexical);

        assert_eq!(runner.results().len(), 1);
        assert!(runner.results().contains_key(&ContractId::new("LEX-001")));
        assert!(!runner.results().contains_key(&ContractId::new("DECL-001")));
    }

    #[test]
    fn duplicate_registration_replaces_previous_test_fn() {
        let mut runner = TestRunner::new(sample_registry());
        runner.register_test("LEX-001", || Err("first".to_string()));
        runner.register_test("LEX-001", || Ok(()));

        runner.run_all();

        let result = runner
            .results()
            .get(&ContractId::new("LEX-001"))
            .expect("missing LEX-001 result");
        assert!(result.passed, "latest registration should win");
    }
}
