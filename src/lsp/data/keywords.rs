//! Shared Modelica keyword data for LSP features.
//!
//! Provides keyword information used by hover, completion, and other LSP handlers.

use lsp_types::{CompletionItem, CompletionItemKind};

/// Information about a Modelica keyword
pub struct KeywordInfo {
    /// The keyword name
    pub name: &'static str,
    /// Short description for completion
    pub detail: &'static str,
    /// Longer description for hover (markdown)
    pub description: &'static str,
    /// Completion item kind
    pub kind: CompletionItemKind,
}

/// All Modelica keywords with their descriptions
pub static MODELICA_KEYWORDS: &[KeywordInfo] = &[
    // Class types
    KeywordInfo {
        name: "model",
        detail: "model declaration",
        description: "**model**\n\nA model is a class that can contain equations and may be instantiated.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "class",
        detail: "class declaration",
        description: "**class**\n\nA general class definition. Models, connectors, and other specialized classes inherit from class.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "connector",
        detail: "connector declaration",
        description: "**connector**\n\nA connector defines the interface for connections between components.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "package",
        detail: "package declaration",
        description: "**package**\n\nA package is a namespace for organizing classes and models.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "function",
        detail: "function declaration",
        description: "**function**\n\nA function is a class that computes outputs from inputs using algorithms.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "record",
        detail: "record declaration",
        description: "**record**\n\nA record is a class used as a data structure without equations.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "block",
        detail: "block declaration",
        description: "**block**\n\nA block is a class with fixed causality for inputs and outputs.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "type",
        detail: "type declaration",
        description: "**type**\n\nDefines a type alias or derived type.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Variability prefixes
    KeywordInfo {
        name: "parameter",
        detail: "parameter variable",
        description: "**parameter**\n\nA parameter is a variable that remains constant during simulation but can be changed between simulations.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "constant",
        detail: "constant variable",
        description: "**constant**\n\nA constant is a variable whose value cannot be changed.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "discrete",
        detail: "discrete variable",
        description: "**discrete**\n\nA discrete variable that only changes at events.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Causality prefixes
    KeywordInfo {
        name: "input",
        detail: "input connector",
        description: "**input**\n\nDeclares an input connector variable.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "output",
        detail: "output connector",
        description: "**output**\n\nDeclares an output connector variable.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Connection prefixes
    KeywordInfo {
        name: "flow",
        detail: "flow variable",
        description: "**flow**\n\nDeclares a flow variable (summed to zero in connections).",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "stream",
        detail: "stream variable",
        description: "**stream**\n\nDeclares a stream variable for bidirectional flow.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Built-in types
    KeywordInfo {
        name: "Real",
        detail: "Real number type",
        description: "**Real**\n\nFloating-point number type.\n\nAttributes: `unit`, `displayUnit`, `min`, `max`, `start`, `fixed`, `nominal`, `stateSelect`",
        kind: CompletionItemKind::TYPE_PARAMETER,
    },
    KeywordInfo {
        name: "Integer",
        detail: "Integer type",
        description: "**Integer**\n\nInteger number type.\n\nAttributes: `min`, `max`, `start`, `fixed`",
        kind: CompletionItemKind::TYPE_PARAMETER,
    },
    KeywordInfo {
        name: "Boolean",
        detail: "Boolean type",
        description: "**Boolean**\n\nBoolean type with values `true` and `false`.\n\nAttributes: `start`, `fixed`",
        kind: CompletionItemKind::TYPE_PARAMETER,
    },
    KeywordInfo {
        name: "String",
        detail: "String type",
        description: "**String**\n\nString type for text.\n\nAttributes: `start`",
        kind: CompletionItemKind::TYPE_PARAMETER,
    },
    // Sections
    KeywordInfo {
        name: "equation",
        detail: "equation section",
        description: "**equation**\n\nSection containing equations that define the mathematical relationships.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "algorithm",
        detail: "algorithm section",
        description: "**algorithm**\n\nSection containing sequential assignment statements.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "initial equation",
        detail: "initial equation section",
        description: "**initial equation**\n\nSection containing equations for initialization.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "initial algorithm",
        detail: "initial algorithm section",
        description: "**initial algorithm**\n\nSection containing algorithm statements for initialization.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "protected",
        detail: "protected section",
        description: "**protected**\n\nSection for declaring protected (non-public) elements.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "public",
        detail: "public section",
        description: "**public**\n\nSection for declaring public elements (default).",
        kind: CompletionItemKind::KEYWORD,
    },
    // Control flow
    KeywordInfo {
        name: "if",
        detail: "if statement",
        description: "**if**\n\nConditional expression or statement.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "then",
        detail: "then clause",
        description: "**then**\n\nFollows a condition in if/when statements.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "else",
        detail: "else clause",
        description: "**else**\n\nAlternative branch in if statements.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "elseif",
        detail: "elseif clause",
        description: "**elseif**\n\nAdditional conditional branch in if statements.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "for",
        detail: "for loop",
        description: "**for**\n\nLoop construct for iteration.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "loop",
        detail: "loop keyword",
        description: "**loop**\n\nMarks the body of a for or while loop.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "while",
        detail: "while loop",
        description: "**while**\n\nLoop that continues while a condition is true.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "when",
        detail: "when statement",
        description: "**when**\n\nEvent-triggered section. Equations inside are active only when condition becomes true.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "end",
        detail: "end keyword",
        description: "**end**\n\nMarks the end of a class, loop, or conditional.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Inheritance and imports
    KeywordInfo {
        name: "extends",
        detail: "inheritance",
        description: "**extends**\n\nInheritance from a base class.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "import",
        detail: "import statement",
        description: "**import**\n\nImports classes from other packages.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "within",
        detail: "within statement",
        description: "**within**\n\nSpecifies the package this file belongs to.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Modifiers
    KeywordInfo {
        name: "final",
        detail: "final modifier",
        description: "**final**\n\nPrevents further modification of the element.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "partial",
        detail: "partial class",
        description: "**partial**\n\nA partial class that cannot be instantiated directly.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "replaceable",
        detail: "replaceable element",
        description: "**replaceable**\n\nAn element that can be replaced in derived classes.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "redeclare",
        detail: "redeclare element",
        description: "**redeclare**\n\nReplaces a replaceable element.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "inner",
        detail: "inner element",
        description: "**inner**\n\nDeclares an element that can be accessed by outer references.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "outer",
        detail: "outer element",
        description: "**outer**\n\nReferences an inner element from an enclosing scope.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "encapsulated",
        detail: "encapsulated class",
        description: "**encapsulated**\n\nA class that doesn't inherit from its enclosing scope.",
        kind: CompletionItemKind::KEYWORD,
    },
    // Annotations and misc
    KeywordInfo {
        name: "annotation",
        detail: "annotation",
        description: "**annotation**\n\nMetadata for documentation, icons, experiments, etc.",
        kind: CompletionItemKind::KEYWORD,
    },
    KeywordInfo {
        name: "time",
        detail: "simulation time",
        description: "**time**\n\nBuilt-in variable representing simulation time.",
        kind: CompletionItemKind::VARIABLE,
    },
    KeywordInfo {
        name: "true",
        detail: "boolean true",
        description: "**true**\n\nBoolean literal for true.",
        kind: CompletionItemKind::VALUE,
    },
    KeywordInfo {
        name: "false",
        detail: "boolean false",
        description: "**false**\n\nBoolean literal for false.",
        kind: CompletionItemKind::VALUE,
    },
];

/// Get keyword info by name
pub fn get_keyword_info(name: &str) -> Option<&'static KeywordInfo> {
    MODELICA_KEYWORDS.iter().find(|k| k.name == name)
}

/// Get hover text for a keyword (returns None if not a keyword)
pub fn get_keyword_hover(name: &str) -> Option<String> {
    get_keyword_info(name).map(|k| k.description.to_string())
}

/// Get all keyword completion items
pub fn get_keyword_completions() -> Vec<CompletionItem> {
    MODELICA_KEYWORDS
        .iter()
        .map(|k| CompletionItem {
            label: k.name.to_string(),
            kind: Some(k.kind),
            detail: Some(k.detail.to_string()),
            ..Default::default()
        })
        .collect()
}
