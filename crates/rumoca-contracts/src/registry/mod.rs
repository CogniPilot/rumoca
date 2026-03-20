//! Contract Registry backed by a compile-time static contract table.

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

/// Unique identifier for a contract.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ContractId(pub String);

impl ContractId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
}

impl std::fmt::Display for ContractId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Contract categories matching SPEC_0022.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ContractCategory {
    /// LEX - Lexical contracts (§2)
    Lexical,
    /// DECL - Declaration contracts (§4)
    Declaration,
    /// INST - Instantiation contracts (§5, §7)
    Instantiation,
    /// EXPR - Expression/Operator contracts (§3)
    Expression,
    /// EQN - Equation contracts (§8)
    Equation,
    /// ALG - Algorithm contracts (§11)
    Algorithm,
    /// CONN - Connection contracts (§9)
    Connection,
    /// FUNC - Function contracts (§12)
    Function,
    /// TYPE - Type/Interface contracts (§6)
    Type,
    /// ARR - Array contracts (§10)
    Array,
    /// PKG - Package/Import contracts (§13)
    Package,
    /// OPREC - Operator Record contracts (§14)
    OperatorRecord,
    /// SIM - Simulation contracts (§8.6, App B)
    Simulation,
    /// CLK - Clock/Synchronous contracts (§16)
    Clock,
    /// STRM - Stream Connector contracts (§15)
    Stream,
    /// SM - State Machine contracts (§17)
    StateMachine,
    /// ANN - Annotation contracts (§18)
    Annotation,
    /// UNIT - Unit Expression contracts (§19)
    Unit,
}

impl ContractCategory {
    /// Get the prefix for this category.
    pub fn prefix(&self) -> &'static str {
        match self {
            ContractCategory::Lexical => "LEX",
            ContractCategory::Declaration => "DECL",
            ContractCategory::Instantiation => "INST",
            ContractCategory::Expression => "EXPR",
            ContractCategory::Equation => "EQN",
            ContractCategory::Algorithm => "ALG",
            ContractCategory::Connection => "CONN",
            ContractCategory::Function => "FUNC",
            ContractCategory::Type => "TYPE",
            ContractCategory::Array => "ARR",
            ContractCategory::Package => "PKG",
            ContractCategory::OperatorRecord => "OPREC",
            ContractCategory::Simulation => "SIM",
            ContractCategory::Clock => "CLK",
            ContractCategory::Stream => "STRM",
            ContractCategory::StateMachine => "SM",
            ContractCategory::Annotation => "ANN",
            ContractCategory::Unit => "UNIT",
        }
    }

    /// Get the MLS section reference.
    pub fn mls_section(&self) -> &'static str {
        match self {
            ContractCategory::Lexical => "§2",
            ContractCategory::Declaration => "§4",
            ContractCategory::Instantiation => "§5, §7",
            ContractCategory::Expression => "§3",
            ContractCategory::Equation => "§8",
            ContractCategory::Algorithm => "§11",
            ContractCategory::Connection => "§9",
            ContractCategory::Function => "§12",
            ContractCategory::Type => "§6",
            ContractCategory::Array => "§10",
            ContractCategory::Package => "§13",
            ContractCategory::OperatorRecord => "§14",
            ContractCategory::Simulation => "§8.6, App B",
            ContractCategory::Clock => "§16",
            ContractCategory::Stream => "§15",
            ContractCategory::StateMachine => "§17",
            ContractCategory::Annotation => "§18",
            ContractCategory::Unit => "§19",
        }
    }
}

/// Implementation status of a contract.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum ContractStatus {
    /// Not yet implemented.
    #[default]
    NotImplemented,
    /// Partially implemented (some cases work).
    Partial,
    /// Fully implemented and tested.
    Implemented,
    /// Not applicable to this compiler (e.g., GUI annotations).
    NotApplicable,
    /// Deferred to a later tier.
    Deferred,
}

/// Runtime contract record used by reports/test runner.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Contract {
    /// Unique contract ID (e.g., "LEX-001").
    pub id: ContractId,
    /// Contract category.
    pub category: ContractCategory,
    /// Short name/title.
    pub name: String,
    /// MLS section reference.
    pub mls_ref: String,
    /// The requirement text from MLS.
    pub requirement: String,
    /// Implementation status.
    pub status: ContractStatus,
    /// Implementation notes.
    #[serde(default)]
    pub notes: Option<String>,
    /// Which tier this belongs to (1=MVP, 2=Standard, 3=Advanced).
    pub tier: u8,
}

impl Contract {
    /// Create a new contract.
    pub fn new(
        id: impl Into<String>,
        category: ContractCategory,
        name: impl Into<String>,
        mls_ref: impl Into<String>,
        requirement: impl Into<String>,
    ) -> Self {
        Self {
            id: ContractId::new(id),
            category,
            name: name.into(),
            mls_ref: mls_ref.into(),
            requirement: requirement.into(),
            status: ContractStatus::NotImplemented,
            notes: None,
            tier: 1,
        }
    }

    /// Set the status for this contract.
    pub fn with_status(mut self, status: ContractStatus) -> Self {
        self.status = status;
        self
    }
}

/// Registry of all MLS contracts.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ContractRegistry {
    contracts: IndexMap<ContractId, Contract>,
}

impl ContractRegistry {
    /// Create a new empty registry.
    pub fn new() -> Self {
        Self::default()
    }

    /// Build a registry from a list of contracts.
    pub fn from_contracts(list: Vec<Contract>) -> Self {
        let contracts = list.into_iter().map(|c| (c.id.clone(), c)).collect();
        Self { contracts }
    }

    /// Register a contract.
    pub fn register(&mut self, contract: Contract) {
        self.contracts.insert(contract.id.clone(), contract);
    }

    /// Get a contract by ID.
    pub fn get(&self, id: &str) -> Option<&Contract> {
        self.contracts.get(&ContractId::new(id))
    }

    /// Get all contracts.
    pub fn all(&self) -> impl Iterator<Item = &Contract> {
        self.contracts.values()
    }

    /// Get contracts by category.
    pub fn by_category(&self, category: ContractCategory) -> impl Iterator<Item = &Contract> {
        self.contracts
            .values()
            .filter(move |c| c.category == category)
    }

    /// Get contracts by status.
    pub fn by_status(&self, status: ContractStatus) -> impl Iterator<Item = &Contract> {
        self.contracts.values().filter(move |c| c.status == status)
    }

    /// Count contracts by category.
    pub fn count_by_category(&self, category: ContractCategory) -> usize {
        self.by_category(category).count()
    }

    /// Count contracts by status.
    pub fn count_by_status(&self, status: ContractStatus) -> usize {
        self.by_status(status).count()
    }

    /// Get total number of contracts.
    pub fn len(&self) -> usize {
        self.contracts.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.contracts.is_empty()
    }

    /// Update status for a contract.
    pub fn set_status(&mut self, id: &str, status: ContractStatus) {
        if let Some(contract) = self.contracts.get_mut(&ContractId::new(id)) {
            contract.status = status;
        }
    }
}

/// Deserializer shim for the contracts TOML file.
#[derive(serde::Deserialize)]
struct ContractsToml {
    contracts: Vec<Contract>,
}

/// Load all contracts from the embedded TOML data file.
pub fn load_all_contracts() -> Vec<Contract> {
    let raw = include_str!("../../data/contracts.toml");
    let parsed: ContractsToml = toml::from_str(raw).expect("contracts.toml must be valid TOML");
    parsed.contracts
}
