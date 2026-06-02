/// Per-category formatter coverage over classified AST trivia gaps.
#[derive(Debug, Clone, PartialEq)]
pub struct FormatCoverageReport {
    /// Total eligible single-line trivia gaps.
    pub eligible_gaps: usize,
    /// Eligible gaps controlled by the active formatter options.
    pub covered_gaps: usize,
    /// Coverage split by whitespace category.
    pub categories: Vec<FormatCoverageCategoryReport>,
    /// First few unclassified gap examples for diagnosis.
    pub unclassified_examples: Vec<String>,
}

impl FormatCoverageReport {
    pub(super) fn new() -> Self {
        Self {
            eligible_gaps: 0,
            covered_gaps: 0,
            categories: Vec::new(),
            unclassified_examples: Vec::new(),
        }
    }

    pub(super) fn record(&mut self, category: FormatCoverageCategory, covered: bool) {
        self.eligible_gaps += 1;
        if covered {
            self.covered_gaps += 1;
        }
        if let Some(entry) = self
            .categories
            .iter_mut()
            .find(|entry| entry.category == category)
        {
            entry.eligible_gaps += 1;
            if covered {
                entry.covered_gaps += 1;
            }
            return;
        }
        self.categories.push(FormatCoverageCategoryReport {
            category,
            eligible_gaps: 1,
            covered_gaps: usize::from(covered),
        });
    }

    pub(super) fn record_unclassified_example(&mut self, example: String) {
        if self.unclassified_examples.len() < 8
            && !self
                .unclassified_examples
                .iter()
                .any(|existing| existing == &example)
        {
            self.unclassified_examples.push(example);
        }
    }

    /// Coverage as a percentage, where an empty report is considered complete.
    pub fn coverage_percent(&self) -> f64 {
        if self.eligible_gaps == 0 {
            return 100.0;
        }
        self.covered_gaps as f64 * 100.0 / self.eligible_gaps as f64
    }
}

/// Coverage for one formatter whitespace category.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatCoverageCategoryReport {
    /// Whitespace category.
    pub category: FormatCoverageCategory,
    /// Eligible single-line trivia gaps in this category.
    pub eligible_gaps: usize,
    /// Gaps controlled by the active formatter options.
    pub covered_gaps: usize,
}

/// Formatter-controlled AST trivia category.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatCoverageCategory {
    ClassPrefix,
    ClassKeywordName,
    ClassEndName,
    TypeAliasAssignment,
    QualifiedNameDot,
    RenamedImportAssignment,
    SelectiveImportComma,
    ComponentPrefix,
    ComponentTypeName,
    ComponentDeclarationAlignment,
    ComponentBindingAssignment,
    ArraySubscriptComma,
    SimpleEquationAssignment,
    StatementAssignment,
    StatementCallAssignment,
    StatementSeparator,
    ArgumentAssignment,
    OpeningParenthesisPadding,
    ExpressionListComma,
    ConnectComma,
    ExtendsBase,
    ConstrainedBy,
    ForIndexIn,
    BinaryOperator,
    Unclassified,
}

impl FormatCoverageCategory {
    /// Stable label used in coverage reports.
    pub fn label(&self) -> &'static str {
        match self {
            Self::ClassPrefix => "class-prefix",
            Self::ClassKeywordName => "class-keyword-name",
            Self::ClassEndName => "class-end-name",
            Self::TypeAliasAssignment => "type-alias-assignment",
            Self::QualifiedNameDot => "qualified-name-dot",
            Self::RenamedImportAssignment => "renamed-import-assignment",
            Self::SelectiveImportComma => "selective-import-comma",
            Self::ComponentPrefix => "component-prefix",
            Self::ComponentTypeName => "component-type-name",
            Self::ComponentDeclarationAlignment => "component-declaration-alignment",
            Self::ComponentBindingAssignment => "component-binding-assignment",
            Self::ArraySubscriptComma => "array-subscript-comma",
            Self::SimpleEquationAssignment => "simple-equation-assignment",
            Self::StatementAssignment => "statement-assignment",
            Self::StatementCallAssignment => "statement-call-assignment",
            Self::StatementSeparator => "statement-separator",
            Self::ArgumentAssignment => "argument-assignment",
            Self::OpeningParenthesisPadding => "opening-parenthesis-padding",
            Self::ExpressionListComma => "expression-list-comma",
            Self::ConnectComma => "connect-comma",
            Self::ExtendsBase => "extends-base",
            Self::ConstrainedBy => "constrainedby",
            Self::ForIndexIn => "for-index-in",
            Self::BinaryOperator => "binary-operator",
            Self::Unclassified => "unclassified",
        }
    }
}
