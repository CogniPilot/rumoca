use super::*;

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]

pub struct Name {
    pub name: Vec<Token>,
    /// Resolved definition ID (populated by resolve phase).
    /// For simple type names, this is the DefId of the class being referenced.
    /// For qualified type names (e.g., `Medium.AbsolutePressure`), this may be
    /// the DefId of the first part if full resolution fails (partial resolution).
    /// Partial resolution indicates the name is valid but depends on a replaceable
    /// type parameter that will be resolved during instantiation (MLS §7.3).
    #[serde(skip)]
    pub def_id: Option<DefId>,
}

impl Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut s = Vec::new();
        for n in &self.name {
            s.push(n.text.clone());
        }
        write!(f, "{}", s.join("."))
    }
}

impl Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = Vec::new();
        for n in &self.name {
            s.push(n.text.clone());
        }
        write!(f, "{:?}", s.join("."))
    }
}

impl Name {
    /// Create a Name from a dotted string (e.g., "Modelica.Blocks.Continuous.FirstOrder")
    pub fn from_string(s: &str) -> Self {
        let name: Vec<Token> = rumoca_core::ComponentPath::from_flat_path(s)
            .into_parts()
            .into_iter()
            .map(|part| Token {
                text: Arc::from(part.as_str()),
                location: Location::default(),
                token_number: 0,
                token_type: 0,
            })
            .collect();

        Name { name, def_id: None }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub struct StoredDefinition {
    pub classes: AstIndexMap<String, ClassDef>,
    pub within: Option<Name>,
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]

pub struct Component {
    /// Unique identifier assigned during semantic analysis.
    /// None when first parsed, populated during name resolution.
    pub def_id: Option<DefId>,
    /// Type of this component, resolved during type checking.
    /// None when first parsed, populated during type checking.
    pub type_id: Option<TypeId>,
    /// DefId of the component's type class (e.g., for `RealInput u`, this is the DefId of RealInput).
    /// None for built-in types (Real, Integer, Boolean, String).
    /// Populated during name resolution, used by instantiate phase for O(1) type lookup.
    pub type_def_id: Option<DefId>,
    pub name: String,
    /// The token for the component name with exact source location
    pub name_token: Token,
    pub type_name: Name,
    pub variability: Variability,
    pub causality: Causality,
    pub connection: Connection,
    pub description: Vec<Token>,
    pub start: Expression,
    /// True if start value is from a modification (start=x), false if from binding (= x)
    pub start_is_modification: bool,
    /// True if the start modifier has `each` prefix (for array components)
    pub start_has_each: bool,
    /// True if an explicit binding equation (= value) was provided in the declaration
    /// This distinguishes `constant Real c = 0;` (has_explicit_binding=true) from
    /// `constant Real c;` (has_explicit_binding=false, even though start may have a default)
    pub has_explicit_binding: bool,
    /// The binding expression from `= expr` in declaration (e.g., `Real v = p.v - n.v`)
    /// Separate from `start` to preserve binding when both binding and start modifier are present.
    pub binding: Option<Expression>,
    /// Array dimensions - empty for scalars, e.g., [2, 3] for a 2x3 matrix
    /// Only populated when dimensions are known literal integers
    pub shape: Vec<usize>,
    /// Raw array dimension subscripts (e.g., `n` in `Real x[n]` or `:` in `Real a[:]`)
    /// Used for parameter-dependent dimensions that need runtime evaluation
    /// Can be Subscript::Expression for explicit dimensions or Subscript::Range for `:`
    pub shape_expr: Vec<Subscript>,
    /// True if shape is from a modification (shape=x), false if from subscript \[x\]
    pub shape_is_modification: bool,
    /// Annotation arguments (e.g., from `annotation(Icon(...), Dialog(...))`)
    pub annotation: Vec<Expression>,
    /// Ordered source-form modifier arguments from the component declaration.
    ///
    /// These preserve the syntax inside `x(...)` before semantic component
    /// attributes are split into `start`, `binding`, `shape`, and
    /// `modifications`.
    pub source_modifications: Vec<Expression>,
    /// Parallel to `source_modifications` - true if the source modifier has an `each` prefix.
    pub source_modification_each_flags: Vec<bool>,
    /// Parallel to `source_modifications` - true if the source modifier has a `final` prefix.
    pub source_modification_final_flags: Vec<bool>,
    /// Parallel to `source_modifications` - true if the source modifier is a redeclaration.
    pub source_modification_redeclare_flags: Vec<bool>,
    /// Component modifications (e.g., R=10 in `Resistor R1(R=10)`)
    /// Maps parameter name to its modified value expression
    pub modifications: AstIndexMap<String, Expression>,
    /// Full source location for the component declaration
    pub location: Location,
    /// Conditional component expression (e.g., `if use_reset` in `BooleanInput reset if use_reset`)
    /// None means the component is unconditional (always present)
    pub condition: Option<Expression>,
    /// True if declared with 'inner' prefix (provides instance to outer references)
    pub inner: bool,
    /// True if declared with 'outer' prefix (references an inner instance from enclosing scope)
    pub outer: bool,
    /// Set of attribute names that are marked as final (e.g., "start" if `final start = 1.0`)
    /// When a derived class tries to override these attributes, an error should be raised
    pub final_attributes: IndexSet<String>,
    /// Set of modification names that have the `each` prefix (MLS §7.2.5).
    /// When applied to array components, `each`-prefixed modifications apply the same
    /// value to every element, while non-`each` modifications distribute array values.
    pub each_modifications: IndexSet<String>,
    /// True if this component is declared in a protected section
    pub is_protected: bool,
    /// True if declared with 'final' prefix (cannot be overridden in derived classes)
    pub is_final: bool,
    /// True if declared with 'replaceable' prefix (can be redeclared in derived classes)
    pub is_replaceable: bool,
    /// True if declared with element-level `redeclare` prefix.
    pub is_redeclare: bool,
    /// Constraining type for replaceable components (MLS §7.3.2)
    /// If set, redeclarations must be subtypes of this type
    pub constrainedby: Option<Name>,
    /// True if this is a structural parameter (MLS §18.3)
    /// Structural parameters affect array sizes, for-loop ranges, or if-equation conditions
    /// and must be evaluable at translation time.
    pub is_structural: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComponentShapeContractError {
    NumericShapeWithoutShapeExpr {
        component: String,
    },
    LiteralShapeMismatch {
        component: String,
        shape: Vec<usize>,
        shape_expr: Vec<usize>,
    },
}

impl Component {
    pub fn validate_shape_contract(&self) -> Result<(), ComponentShapeContractError> {
        if !self.shape.is_empty() && self.shape_expr.is_empty() {
            return Err(ComponentShapeContractError::NumericShapeWithoutShapeExpr {
                component: self.name.clone(),
            });
        }

        let Some(shape_expr) = literal_shape_expr(&self.shape_expr) else {
            return Ok(());
        };
        if !self.shape.is_empty() && self.shape != shape_expr {
            return Err(ComponentShapeContractError::LiteralShapeMismatch {
                component: self.name.clone(),
                shape: self.shape.clone(),
                shape_expr,
            });
        }
        Ok(())
    }
}

fn literal_shape_expr(shape_expr: &[Subscript]) -> Option<Vec<usize>> {
    let mut dims = Vec::with_capacity(shape_expr.len());
    for subscript in shape_expr {
        let Subscript::Expression(Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
            ..
        }) = subscript
        else {
            return None;
        };
        let Ok(dim) = token.text.parse::<usize>() else {
            return None;
        };
        dims.push(dim);
    }
    Some(dims)
}

impl Default for Component {
    fn default() -> Self {
        Self {
            def_id: None,
            type_id: None,
            type_def_id: None,
            name: String::new(),
            name_token: Token::default(),
            type_name: Name::default(),
            variability: Variability::Empty,
            causality: Causality::Empty,
            connection: Connection::default(),
            description: Vec::new(),
            start: Expression::default(),
            start_is_modification: false,
            start_has_each: false,
            has_explicit_binding: false,
            binding: None,
            shape: Vec::new(),
            shape_expr: Vec::new(),
            shape_is_modification: false,
            annotation: Vec::new(),
            source_modifications: Vec::new(),
            source_modification_each_flags: Vec::new(),
            source_modification_final_flags: Vec::new(),
            source_modification_redeclare_flags: Vec::new(),
            modifications: AstIndexMap::default(),
            location: Location::default(),
            condition: None,
            inner: false,
            outer: false,
            final_attributes: IndexSet::new(),
            each_modifications: IndexSet::new(),
            is_protected: false,
            is_final: false,
            is_replaceable: false,
            is_redeclare: false,
            constrainedby: None,
            is_structural: false,
        }
    }
}

#[cfg(test)]
mod component_shape_contract_tests {
    use super::*;

    fn int_dim(value: usize) -> Subscript {
        Subscript::Expression(Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: Arc::from(value.to_string()),
                ..Default::default()
            },
            span: Span::DUMMY,
        })
    }

    #[test]
    fn component_shape_contract_accepts_matching_literal_shape_expr() {
        let component = Component {
            name: "x".to_string(),
            shape: vec![2, 3],
            shape_expr: vec![int_dim(2), int_dim(3)],
            ..Default::default()
        };

        assert_eq!(component.validate_shape_contract(), Ok(()));
    }

    #[test]
    fn component_shape_contract_rejects_numeric_shape_without_shape_expr() {
        let component = Component {
            name: "x".to_string(),
            shape: vec![2],
            ..Default::default()
        };

        assert_eq!(
            component.validate_shape_contract(),
            Err(ComponentShapeContractError::NumericShapeWithoutShapeExpr {
                component: "x".to_string()
            })
        );
    }

    #[test]
    fn component_shape_contract_rejects_divergent_literal_shape_expr() {
        let component = Component {
            name: "x".to_string(),
            shape: vec![2],
            shape_expr: vec![int_dim(3)],
            ..Default::default()
        };

        assert_eq!(
            component.validate_shape_contract(),
            Err(ComponentShapeContractError::LiteralShapeMismatch {
                component: "x".to_string(),
                shape: vec![2],
                shape_expr: vec![3],
            })
        );
    }
}

impl Debug for Component {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("Component");
        builder
            .field("name", &self.name)
            .field("type_name", &self.type_name);
        if self.variability != Variability::Empty {
            builder.field("variability", &self.variability);
        }
        if self.causality != Causality::Empty {
            builder.field("causality", &self.causality);
        }
        if self.connection != Connection::Empty {
            builder.field("connection", &self.connection);
        }
        if !self.description.is_empty() {
            builder.field("description", &self.description);
        }
        if !self.shape.is_empty() {
            builder.field("shape", &self.shape);
        }
        if !self.shape_expr.is_empty() {
            builder.field("shape_expr", &self.shape_expr);
        }
        if !self.annotation.is_empty() {
            builder.field("annotation", &self.annotation);
        }
        if !self.modifications.is_empty() {
            builder.field("modifications", &self.modifications);
        }
        if self.condition.is_some() {
            builder.field("condition", &self.condition);
        }
        if self.inner {
            builder.field("inner", &self.inner);
        }
        if self.outer {
            builder.field("outer", &self.outer);
        }
        if !self.final_attributes.is_empty() {
            builder.field("final_attributes", &self.final_attributes);
        }
        if !self.each_modifications.is_empty() {
            builder.field("each_modifications", &self.each_modifications);
        }
        if self.is_protected {
            builder.field("is_protected", &self.is_protected);
        }
        if self.is_final {
            builder.field("is_final", &self.is_final);
        }
        builder.finish()
    }
}

/// Enumeration literal with optional description
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct EnumLiteral {
    /// The literal identifier (e.g., 'U' or Red)
    pub ident: Token,
    /// Optional description strings (e.g., "Uninitialized")
    pub description: Vec<Token>,
}

/// External function declaration (MLS §12.9).
///
/// Functions can be declared as external to call C code:
/// ```modelica
/// function myFunc
///   input Real x;
///   output Real y;
///   external "C" y = my_c_func(x);
/// end myFunc;
/// ```
#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExternalFunction {
    /// Language specification (e.g., "C", "FORTRAN 77"). Default is "C".
    pub language: Option<String>,
    /// External function name (defaults to Modelica function name if not specified).
    pub function_name: Option<Token>,
    /// Output variable that receives the return value (if any).
    pub output: Option<ComponentReference>,
    /// Arguments passed to the external function.
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct ClassDef {
    /// Unique identifier assigned during semantic analysis.
    /// None when first parsed, populated during name resolution.
    pub def_id: Option<DefId>,
    /// Scope created by this class for name lookup.
    /// None when first parsed, populated during name resolution.
    pub scope_id: Option<ScopeId>,
    pub name: Token,
    pub class_type: ClassType,
    /// Token for the class type keyword (model, class, function, etc.)
    pub class_type_token: Token,
    pub encapsulated: bool,
    /// True if the class is declared with the `partial` keyword
    pub partial: bool,
    /// True if the class is declared with the `expandable` keyword (MLS §9.1.3)
    /// Only meaningful for connector classes
    pub expandable: bool,
    /// True if the class is declared with the `operator` keyword (MLS §14)
    /// Only meaningful for record classes (operator record)
    pub operator_record: bool,
    /// True if the function is pure (no side effects). MLS §12.3.
    /// Only meaningful for function classes. Functions are pure by default.
    /// Set to false when declared with `impure` keyword.
    pub pure: bool,
    /// True when the source declared `pure` or `impure` explicitly
    /// (MLS §12.3; external functions without an explicit declaration are
    /// deprecated, FUNC-032).
    pub purity_declared: bool,
    /// Causality from type alias definition (e.g., `connector RealInput = input Real`)
    /// Components of this type inherit this causality
    pub causality: Causality,
    /// Description string for this class (e.g., "A test model")
    pub description: Vec<Token>,
    /// Full source location spanning from class keyword to end statement
    pub location: Location,
    pub extends: Vec<Extend>,
    pub imports: Vec<Import>,
    /// Nested class definitions (functions, models, packages, etc.)
    pub classes: AstIndexMap<String, ClassDef>,
    pub components: AstIndexMap<String, Component>,
    pub equations: Vec<Equation>,
    pub initial_equations: Vec<Equation>,
    pub algorithms: Vec<Vec<Statement>>,
    pub initial_algorithms: Vec<Vec<Statement>>,
    /// Token for "equation" keyword (if present)
    pub equation_keyword: Option<Token>,
    /// Token for "initial equation" keyword (if present)
    pub initial_equation_keyword: Option<Token>,
    /// Token for "algorithm" keyword (if present)
    pub algorithm_keyword: Option<Token>,
    /// Token for "initial algorithm" keyword (if present)
    pub initial_algorithm_keyword: Option<Token>,
    /// Token for the class name in "end ClassName;" (for rename support)
    pub end_name_token: Option<Token>,
    /// Enumeration literals for enum types (e.g., `type MyEnum = enumeration(A "desc", B, C)`)
    pub enum_literals: Vec<EnumLiteral>,
    /// Annotation clause for this class (e.g., Documentation, Icon, Diagram)
    pub annotation: Vec<Expression>,
    /// True if this class is declared in a protected section
    pub is_protected: bool,
    /// True if declared with 'final' prefix (cannot be redeclared in derived classes)
    /// MLS §7.2.6: "A final declaration in an element modification prevents modification"
    pub is_final: bool,
    /// True if declared with 'inner' prefix.
    pub is_inner: bool,
    /// True if declared with 'outer' prefix.
    pub is_outer: bool,
    /// True if declared with 'replaceable' prefix (can be redeclared in derived classes)
    /// MLS §7.3: "A replaceable element is an element that can be replaced by a different element"
    pub is_replaceable: bool,
    /// True if declared with element-level `redeclare` prefix.
    pub is_redeclare: bool,
    /// Constraining type for replaceable classes (MLS §7.3.2)
    /// If set, redeclarations must be subtypes of this type
    pub constrainedby: Option<Name>,
    /// Array subscripts for type alias definitions (e.g., `type Vector3 = Real[3]`)
    /// Stores the `[3]` part as subscripts for later use in type resolution
    pub array_subscripts: Vec<Subscript>,
    /// External function declaration (MLS §12.9).
    /// Only meaningful for function classes.
    pub external: Option<ExternalFunction>,
}

impl Default for ClassDef {
    fn default() -> Self {
        Self {
            def_id: None,
            scope_id: None,
            name: Token::default(),
            class_type: ClassType::default(),
            class_type_token: Token::default(),
            encapsulated: false,
            partial: false,
            expandable: false,
            operator_record: false,
            pure: false,
            purity_declared: false,
            causality: Causality::Empty,
            description: Vec::new(),
            location: Location::default(),
            extends: Vec::new(),
            imports: Vec::new(),
            classes: AstIndexMap::default(),
            components: AstIndexMap::default(),
            equations: Vec::new(),
            initial_equations: Vec::new(),
            algorithms: Vec::new(),
            initial_algorithms: Vec::new(),
            equation_keyword: None,
            initial_equation_keyword: None,
            algorithm_keyword: None,
            initial_algorithm_keyword: None,
            end_name_token: None,
            enum_literals: Vec::new(),
            annotation: Vec::new(),
            is_protected: false,
            is_final: false,
            is_inner: false,
            is_outer: false,
            is_replaceable: false,
            is_redeclare: false,
            constrainedby: None,
            array_subscripts: Vec::new(),
            external: None,
        }
    }
}

impl ClassDef {
    /// Iterate over all component declarations with their names.
    ///
    /// This provides a convenient way to iterate over components without
    /// directly accessing the `components` field.
    pub fn iter_components(&self) -> impl Iterator<Item = (&str, &Component)> {
        self.components
            .iter()
            .map(|(name, comp)| (name.as_str(), comp))
    }

    /// Iterate over all nested class definitions with their names.
    ///
    /// This includes functions, models, packages, types, etc. defined within this class.
    pub fn iter_classes(&self) -> impl Iterator<Item = (&str, &ClassDef)> {
        self.classes
            .iter()
            .map(|(name, class)| (name.as_str(), class))
    }

    /// Iterate over all equations (regular + initial).
    ///
    /// This chains `equations` and `initial_equations` into a single iterator.
    pub fn iter_all_equations(&self) -> impl Iterator<Item = &Equation> {
        self.equations.iter().chain(self.initial_equations.iter())
    }

    /// Iterate over all statements from all algorithm sections (regular + initial).
    ///
    /// This flattens all algorithm blocks and chains regular and initial algorithms.
    pub fn iter_all_statements(&self) -> impl Iterator<Item = &Statement> {
        self.algorithms
            .iter()
            .flatten()
            .chain(self.initial_algorithms.iter().flatten())
    }
}

/// MLS §7.1: Extends clause for inheritance.
#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Extend {
    /// The base class name (syntactic).
    pub base_name: Name,
    /// Resolved reference to the base class definition.
    /// None when first parsed, populated during name resolution.
    pub base_def_id: Option<DefId>,
    /// Source location of the extends clause.
    pub location: Location,
    /// Modifications applied to the extends clause (e.g., extends Foo(bar=1)).
    pub modifications: Vec<ExtendModification>,
    /// Component names that are deselected via `break` (MLS §7.4 Selective Model Extension).
    /// e.g., extends Base(break x, break y) would have break_names = ["x", "y"]
    pub break_names: Vec<String>,
    /// True if this is a protected extends (MLS §7.1.2).
    /// Protected extends: `protected extends Base;`
    pub is_protected: bool,
    /// Optional annotation clause.
    pub annotation: Vec<Expression>,
}

/// A modification item in an extends clause.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExtendModification {
    /// The modification expression.
    pub expr: Expression,
    /// True if the modification has `each` prefix (MLS §7.2.5).
    pub each: bool,
    /// True if the modification has `final` prefix (MLS §7.2.6).
    pub final_: bool,
    /// True if the modification is a redeclaration (MLS §7.3).
    pub redeclare: bool,
}

impl Default for ExtendModification {
    fn default() -> Self {
        Self {
            expr: Expression::Empty {
                span: rumoca_core::Span::DUMMY,
            },
            each: false,
            final_: false,
            redeclare: false,
        }
    }
}

/// Import clause for bringing names into scope
/// Modelica supports several import styles:
/// - `import A.B.C;` - qualified import (use as C)
/// - `import D = A.B.C;` - renamed import (use as D)
/// - `import A.B.*;` - unqualified import (all names from A.B)
/// - `import A.B.{C, D, E};` - selective import (specific names)
/// - `import .A.B.C;` - global scope import (leading dot)
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Import {
    /// Qualified import: `import A.B.C;` - imports C, accessed as C
    Qualified {
        path: Name,
        location: Location,
        /// True if import uses global scope (leading dot: `import .A.B.C;`)
        global_scope: bool,
    },
    /// Renamed import: `import D = A.B.C;` - imports C, accessed as D
    Renamed {
        alias: Token,
        path: Name,
        location: Location,
        /// True if import uses global scope (leading dot: `import D = .A.B.C;`)
        global_scope: bool,
    },
    /// Unqualified import: `import A.B.*;` - imports all from A.B
    Unqualified {
        path: Name,
        location: Location,
        /// True if import uses global scope (leading dot: `import .A.B.*;`)
        global_scope: bool,
    },
    /// Selective import: `import A.B.{C, D};` - imports specific names
    Selective {
        path: Name,
        names: Vec<Token>,
        location: Location,
        /// True if import uses global scope (leading dot: `import .A.B.{C, D};`)
        global_scope: bool,
    },
}

impl Import {
    /// Get the base path for this import
    pub fn base_path(&self) -> &Name {
        match self {
            Import::Qualified { path, .. } => path,
            Import::Renamed { path, .. } => path,
            Import::Unqualified { path, .. } => path,
            Import::Selective { path, .. } => path,
        }
    }

    /// Get the source location of this import
    pub fn location(&self) -> &Location {
        match self {
            Import::Qualified { location, .. } => location,
            Import::Renamed { location, .. } => location,
            Import::Unqualified { location, .. } => location,
            Import::Selective { location, .. } => location,
        }
    }

    /// Returns true if this import uses global scope (leading dot)
    pub fn is_global_scope(&self) -> bool {
        match self {
            Import::Qualified { global_scope, .. } => *global_scope,
            Import::Renamed { global_scope, .. } => *global_scope,
            Import::Unqualified { global_scope, .. } => *global_scope,
            Import::Selective { global_scope, .. } => *global_scope,
        }
    }
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]

pub struct ComponentRefPart {
    pub ident: Token,
    pub subs: Option<Vec<Subscript>>,
}

impl Debug for ComponentRefPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use Display for debug to keep formatting consistent
        write!(f, "{}", self)
    }
}

impl Display for ComponentRefPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident.text)?;
        if let Some(subs) = &self.subs {
            write!(f, "[{}]", format_subscripts(subs))?;
        }
        Ok(())
    }
}

/// Format subscripts as comma-separated string (no spaces).
fn format_subscripts(subs: &[Subscript]) -> String {
    subs.iter()
        .map(|s| s.to_string())
        .collect::<Vec<_>>()
        .join(",")
}

#[derive(Default, Clone, PartialEq, Serialize, Deserialize)]

pub struct ComponentReference {
    /// Whether this reference starts with a `.` (local lookup).
    pub local: bool,
    /// The parts of the reference (e.g., `a.b.c` has 3 parts).
    pub parts: Vec<ComponentRefPart>,
    /// Source span for this component reference.
    pub span: Span,
    /// Resolved definition ID for the first part (populated by resolve phase).
    /// For `a.b.c`, this resolves `a` to its DefId.
    #[serde(skip)]
    pub def_id: Option<DefId>,
}

impl Display for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, part) in self.parts.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            write!(f, "{}", part)?;
        }
        Ok(())
    }
}

impl Debug for ComponentReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use Display for debug to keep formatting consistent
        write!(f, "{}", self)
    }
}

impl ComponentReference {
    /// Get the source location of the first token in this component reference.
    pub fn get_location(&self) -> Option<&Location> {
        self.parts.first().map(|part| &part.ident.location)
    }
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct EquationBlock {
    pub cond: Expression,
    pub eqs: Vec<Equation>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct StatementBlock {
    pub cond: Expression,
    pub stmts: Vec<Statement>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]

pub struct ForIndex {
    pub ident: Token,
    pub range: Expression,
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub enum Equation {
    #[default]
    Empty,
    Simple {
        lhs: Expression,
        rhs: Expression,
    },
    Connect {
        lhs: ComponentReference,
        rhs: ComponentReference,
    },
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Equation>,
    },
    When(Vec<EquationBlock>),
    If {
        cond_blocks: Vec<EquationBlock>,
        else_block: Option<Vec<Equation>>,
    },
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
    },
    /// MLS §8.3.7: assert(condition, message, level)
    Assert {
        /// The Boolean condition to check.
        condition: Expression,
        /// The error message string.
        message: Expression,
        /// Optional assertion level (AssertionLevel.error or .warning).
        level: Option<Expression>,
    },
}

impl Equation {
    /// Get the source location of the first token in this equation.
    /// Returns None for Empty equations.
    pub fn get_location(&self) -> Option<&Location> {
        match self {
            Equation::Empty => None,
            Equation::Simple { lhs, .. } => lhs.get_location(),
            Equation::Connect { lhs, .. } => lhs.get_location(),
            Equation::For { indices, .. } => indices.first().map(|i| &i.ident.location),
            Equation::When(blocks) => blocks.first().and_then(|b| b.cond.get_location()),
            Equation::If { cond_blocks, .. } => {
                cond_blocks.first().and_then(|b| b.cond.get_location())
            }
            Equation::FunctionCall { comp, .. } => comp.get_location(),
            Equation::Assert { condition, .. } => condition.get_location(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]
pub enum TerminalType {
    #[default]
    Empty,
    UnsignedReal,
    UnsignedInteger,
    String,
    Bool,
    End,
}

#[derive(Clone, PartialEq, Serialize, Deserialize)]

pub enum Expression {
    Empty {
        span: Span,
    },
    Range {
        start: Arc<Expression>,
        step: Option<Arc<Expression>>,
        end: Arc<Expression>,
        span: Span,
    },
    Unary {
        op: OpUnary,
        rhs: Arc<Expression>,
        span: Span,
    },
    Binary {
        op: OpBinary,
        lhs: Arc<Expression>,
        rhs: Arc<Expression>,
        span: Span,
    },
    Terminal {
        terminal_type: TerminalType,
        token: Token,
        span: Span,
    },
    ComponentReference(ComponentReference),
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
        span: Span,
    },
    /// Class modification in extends/declaration context: `i(x = 2)`
    /// Syntactically identical to FunctionCall but semantically different.
    /// This is used for modifications in extends clauses and component declarations,
    /// not for actual function calls in expressions.
    ClassModification {
        target: ComponentReference,
        modifications: Vec<Expression>,
        #[serde(default)]
        each_flags: Vec<bool>,
        #[serde(default)]
        final_flags: Vec<bool>,
        #[serde(default)]
        redeclare_flags: Vec<bool>,
        span: Span,
    },
    /// Named argument in function calls: `func(param = value)`
    /// Preserves the parameter name token with source location for better error messages.
    /// This replaces the use of `Binary { Assign }` for named function arguments.
    NamedArgument {
        /// The parameter name token (with source location)
        name: Token,
        /// The argument value expression
        value: Arc<Expression>,
        span: Span,
    },
    /// Simple modification assignment in extends/declarations: `x = 2`
    /// This is used for modifications like `extends Base(x = 2)` where `x = 2` is a simple
    /// value modification. Distinct from ClassModification which handles `i(x = 2)`.
    Modification {
        /// The target being modified (e.g., "x" or "x.start")
        target: ComponentReference,
        /// The modification value
        value: Arc<Expression>,
        span: Span,
    },
    Array {
        elements: Vec<Expression>,
        /// True if original syntax was `[a;b]` matrix notation, false for `{a,b}` array notation
        is_matrix: bool,
        span: Span,
    },
    /// Tuple expression for multi-output function calls: (a, b) = func()
    Tuple {
        elements: Vec<Expression>,
        span: Span,
    },
    /// If expression: if cond then expr elseif cond2 then expr2 else expr3
    If {
        /// List of (condition, expression) pairs for if and elseif branches
        branches: Vec<(Expression, Expression)>,
        /// The else branch expression
        else_branch: Arc<Expression>,
        span: Span,
    },
    /// Parenthesized expression to preserve explicit parentheses from source
    Parenthesized {
        inner: Arc<Expression>,
        span: Span,
    },
    /// Array comprehension: {expr for i in range if filter}
    ArrayComprehension {
        expr: Arc<Expression>,
        indices: Vec<ForIndex>,
        /// Optional filter condition
        filter: Option<Arc<Expression>>,
        span: Span,
    },
    /// Array indexing on arbitrary expressions: `(a+b)[i]`, `func()[i]`
    ///
    /// This allows indexing into expressions that are not simple component
    /// references, such as function return values or computed arrays.
    ArrayIndex {
        /// The expression to index into.
        base: Arc<Expression>,
        /// The subscript expressions.
        subscripts: Vec<Subscript>,
        span: Span,
    },
    /// Field access on arbitrary expressions: `func().field`, `(if cond then a else b).field`
    ///
    /// This allows accessing record fields from expressions that are not simple component
    /// references, such as function return values or conditional expressions.
    /// MLS §5.3.2: Component references can include field access through dot notation.
    FieldAccess {
        /// The expression to access the field from.
        base: Arc<Expression>,
        /// The field name to access.
        field: String,
        span: Span,
    },
}

impl Default for Expression {
    fn default() -> Self {
        Self::Empty { span: Span::DUMMY }
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Empty { .. } => write!(f, "Empty"),
            Expression::Range {
                start, step, end, ..
            } => f
                .debug_struct("Range")
                .field("start", start)
                .field("step", step)
                .field("end", end)
                .finish(),
            Expression::ComponentReference(comp) => write!(f, "{:?}", comp),
            Expression::FunctionCall { comp, args, .. } => f
                .debug_struct("FunctionCall")
                .field("comp", comp)
                .field("args", args)
                .finish(),
            Expression::ClassModification {
                target,
                modifications,
                ..
            } => f
                .debug_struct("ClassModification")
                .field("target", target)
                .field("modifications", modifications)
                .finish(),
            Expression::NamedArgument { name, value, .. } => f
                .debug_struct("NamedArgument")
                .field("name", &name.text)
                .field("value", value)
                .finish(),
            Expression::Modification { target, value, .. } => f
                .debug_struct("Modification")
                .field("target", target)
                .field("value", value)
                .finish(),
            Expression::Binary { op, lhs, rhs, .. } => f
                .debug_struct(&format!("{:?}", op))
                .field("lhs", lhs)
                .field("rhs", rhs)
                .finish(),
            Expression::Unary { op, rhs, .. } => f
                .debug_struct(&format!("{:?}", op))
                .field("rhs", rhs)
                .finish(),
            Expression::Terminal {
                terminal_type,
                token,
                ..
            } => {
                write!(f, "{:?}({:?})", terminal_type, token)
            }
            Expression::Array { elements, .. } => f.debug_list().entries(elements.iter()).finish(),
            Expression::Tuple { elements, .. } => write!(f, "({})", format_debug_list(elements)),
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                write!(
                    f,
                    "{} else {:?}",
                    format_debug_if_branches(branches),
                    else_branch
                )
            }
            Expression::Parenthesized { inner, .. } => write!(f, "({:?})", inner),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                let filter_str = filter
                    .as_ref()
                    .map(|filt| format!(" if {:?}", filt))
                    .unwrap_or_default();
                write!(f, "{{{{ {:?} for {:?}{} }}}}", expr, indices, filter_str)
            }
            Expression::ArrayIndex {
                base, subscripts, ..
            } => f
                .debug_struct("ArrayIndex")
                .field("base", base)
                .field("subscripts", subscripts)
                .finish(),
            Expression::FieldAccess { base, field, .. } => f
                .debug_struct("FieldAccess")
                .field("base", base)
                .field("field", field)
                .finish(),
        }
    }
}

/// Format a list of expressions as comma-separated debug strings.
fn format_debug_list(elements: &[Expression]) -> String {
    elements
        .iter()
        .map(|e| format!("{:?}", e))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format if-expression branches for debug output.
fn format_debug_if_branches(branches: &[(Expression, Expression)]) -> String {
    branches
        .iter()
        .enumerate()
        .map(|(i, (cond, expr))| {
            let prefix = if i == 0 { "if " } else { " elseif " };
            format!("{}{:?} then {:?}", prefix, cond, expr)
        })
        .collect::<String>()
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::Empty { span }
            | Expression::Range { span, .. }
            | Expression::Unary { span, .. }
            | Expression::Binary { span, .. }
            | Expression::Terminal { span, .. }
            | Expression::FunctionCall { span, .. }
            | Expression::ClassModification { span, .. }
            | Expression::NamedArgument { span, .. }
            | Expression::Modification { span, .. }
            | Expression::Array { span, .. }
            | Expression::Tuple { span, .. }
            | Expression::If { span, .. }
            | Expression::Parenthesized { span, .. }
            | Expression::ArrayComprehension { span, .. }
            | Expression::ArrayIndex { span, .. }
            | Expression::FieldAccess { span, .. } => *span,
            Expression::ComponentReference(comp) => comp.span,
        }
    }

    /// Get the source location of the first token in this expression.
    /// Returns None for Empty expressions.
    pub fn get_location(&self) -> Option<&Location> {
        match self {
            Expression::Empty { .. } => None,
            Expression::Range { start, .. } => start.get_location(),
            Expression::Unary { rhs, .. } => rhs.get_location(),
            Expression::Binary { lhs, .. } => lhs.get_location(),
            Expression::Terminal { token, .. } => Some(&token.location),
            Expression::ComponentReference(comp) => {
                comp.parts.first().map(|part| &part.ident.location)
            }
            Expression::FunctionCall { comp, .. } => {
                comp.parts.first().map(|part| &part.ident.location)
            }
            Expression::ClassModification { target, .. } => {
                target.parts.first().map(|part| &part.ident.location)
            }
            Expression::NamedArgument { name, .. } => Some(&name.location),
            Expression::Modification { target, .. } => {
                target.parts.first().map(|part| &part.ident.location)
            }
            Expression::Array { elements, .. } => elements.first().and_then(|e| e.get_location()),
            Expression::Tuple { elements, .. } => elements.first().and_then(|e| e.get_location()),
            Expression::If { branches, .. } => {
                branches.first().and_then(|(cond, _)| cond.get_location())
            }
            Expression::Parenthesized { inner, .. } => inner.get_location(),
            Expression::ArrayComprehension { expr, .. } => expr.get_location(),
            Expression::ArrayIndex { base, .. } => base.get_location(),
            Expression::FieldAccess { base, .. } => base.get_location(),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Empty { .. } => write!(f, ""),
            Expression::Range {
                start, step, end, ..
            } => format_range(f, start, step, end),
            Expression::Unary { op, rhs, .. } => {
                write!(f, "{}{}", format_unary_op(op), rhs)
            }
            Expression::Binary { op, lhs, rhs, .. } => {
                write!(f, "{} {} {}", lhs, format_binary_op(op), rhs)
            }
            Expression::Terminal {
                terminal_type,
                token,
                ..
            } => match terminal_type {
                TerminalType::String => write!(f, "\"{}\"", token.text),
                TerminalType::Bool => write!(f, "{}", token.text),
                _ => write!(f, "{}", token.text),
            },
            Expression::ComponentReference(comp) => write!(f, "{}", comp),
            Expression::FunctionCall { comp, args, .. } => {
                write!(f, "{}({})", comp, format_display_list(args))
            }
            Expression::ClassModification {
                target,
                modifications,
                ..
            } => {
                write!(f, "{}({})", target, format_display_list(modifications))
            }
            Expression::NamedArgument { name, value, .. } => write!(f, "{} = {}", name.text, value),
            Expression::Modification { target, value, .. } => write!(f, "{} = {}", target, value),
            Expression::Array { elements, .. } => {
                write!(f, "{{{}}}", format_display_list(elements))
            }
            Expression::Tuple { elements, .. } => write!(f, "({})", format_display_list(elements)),
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                write!(f, "{} else {}", format_if_branches(branches), else_branch)
            }
            Expression::Parenthesized { inner, .. } => write!(f, "({})", inner),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                let filter_str = filter
                    .as_ref()
                    .map(|filt| format!(" if {}", filt))
                    .unwrap_or_default();
                write!(
                    f,
                    "{{ {} for {}{} }}",
                    expr,
                    format_for_indices(indices),
                    filter_str
                )
            }
            Expression::ArrayIndex {
                base, subscripts, ..
            } => {
                write!(f, "{}[{}]", base, format_subscripts(subscripts))
            }
            Expression::FieldAccess { base, field, .. } => {
                write!(f, "{}.{}", base, field)
            }
        }
    }
}

fn format_unary_op(op: &OpUnary) -> &'static str {
    match op {
        OpUnary::Minus => "-",
        OpUnary::Plus => "+",
        OpUnary::DotMinus => ".-",
        OpUnary::DotPlus => ".+",
        OpUnary::Not => "not ",
        OpUnary::Empty => "",
    }
}

fn format_binary_op(op: &OpBinary) -> &'static str {
    match op {
        OpBinary::Add => "+",
        OpBinary::Sub => "-",
        OpBinary::Mul => "*",
        OpBinary::Div => "/",
        OpBinary::Eq => "==",
        OpBinary::Neq => "<>",
        OpBinary::Lt => "<",
        OpBinary::Le => "<=",
        OpBinary::Gt => ">",
        OpBinary::Ge => ">=",
        OpBinary::And => "and",
        OpBinary::Or => "or",
        OpBinary::Exp => "^",
        OpBinary::ExpElem => ".^",
        OpBinary::AddElem => ".+",
        OpBinary::SubElem => ".-",
        OpBinary::MulElem => ".*",
        OpBinary::DivElem => "./",
        OpBinary::Assign => "=",
        OpBinary::Empty => "?",
    }
}

/// Format a range expression.
fn format_range(
    f: &mut std::fmt::Formatter<'_>,
    start: &Expression,
    step: &Option<Arc<Expression>>,
    end: &Expression,
) -> std::fmt::Result {
    match step {
        Some(s) => write!(f, "{}:{}:{}", start, s, end),
        None => write!(f, "{}:{}", start, end),
    }
}

/// Format a list of displayable items as comma-separated string.
fn format_display_list<T: std::fmt::Display>(items: &[T]) -> String {
    items
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format if-expression branches for Display output.
fn format_if_branches(branches: &[(Expression, Expression)]) -> String {
    branches
        .iter()
        .enumerate()
        .map(|(i, (cond, expr))| {
            let prefix = if i == 0 { "if " } else { " elseif " };
            format!("{}{} then {}", prefix, cond, expr)
        })
        .collect::<String>()
}

/// Format for-loop indices as comma-separated "var in range" string.
fn format_for_indices(indices: &[ForIndex]) -> String {
    indices
        .iter()
        .map(|idx| format!("{} in {}", idx.ident.text, idx.range))
        .collect::<Vec<_>>()
        .join(", ")
}

impl std::fmt::Display for Equation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Equation::Empty => write!(f, ""),
            Equation::Simple { lhs, rhs } => write!(f, "{} = {}", lhs, rhs),
            Equation::Connect { lhs, rhs } => write!(f, "connect({}, {})", lhs, rhs),
            Equation::For { indices, equations } => format_for_equation(f, indices, equations),
            Equation::When(blocks) => format_when_equation(f, blocks),
            Equation::If {
                cond_blocks,
                else_block,
            } => format_if_equation(f, cond_blocks, else_block),
            Equation::FunctionCall { comp, args } => {
                write!(f, "{}({})", comp, format_display_list(args))
            }
            Equation::Assert {
                condition,
                message,
                level,
            } => {
                if let Some(lvl) = level {
                    write!(f, "assert({}, {}, {})", condition, message, lvl)
                } else {
                    write!(f, "assert({}, {})", condition, message)
                }
            }
        }
    }
}

/// Format a for-equation.
fn format_for_equation(
    f: &mut std::fmt::Formatter<'_>,
    indices: &[ForIndex],
    equations: &[Equation],
) -> std::fmt::Result {
    writeln!(f, "for {} loop", format_for_indices(indices))?;
    for eq in equations {
        writeln!(f, "  {};", eq)?;
    }
    write!(f, "end for")
}

/// Format a when-equation.
fn format_when_equation(
    f: &mut std::fmt::Formatter<'_>,
    blocks: &[EquationBlock],
) -> std::fmt::Result {
    for (i, block) in blocks.iter().enumerate() {
        if i == 0 {
            writeln!(f, "when {} then", block.cond)?;
        } else {
            writeln!(f, "elsewhen {} then", block.cond)?;
        }
        for eq in &block.eqs {
            writeln!(f, "  {};", eq)?;
        }
    }
    write!(f, "end when")
}

/// Format an if-equation.
fn format_if_equation(
    f: &mut std::fmt::Formatter<'_>,
    cond_blocks: &[EquationBlock],
    else_block: &Option<Vec<Equation>>,
) -> std::fmt::Result {
    for (i, block) in cond_blocks.iter().enumerate() {
        if i == 0 {
            writeln!(f, "if {} then", block.cond)?;
        } else {
            writeln!(f, "elseif {} then", block.cond)?;
        }
        for eq in &block.eqs {
            writeln!(f, "  {};", eq)?;
        }
    }
    if let Some(else_eqs) = else_block {
        writeln!(f, "else")?;
        for eq in else_eqs {
            writeln!(f, "  {};", eq)?;
        }
    }
    write!(f, "end if")
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub enum Statement {
    #[default]
    Empty,
    Assignment {
        comp: ComponentReference,
        value: Expression,
    },
    Return {
        token: Token,
    },
    Break {
        token: Token,
    },
    For {
        indices: Vec<ForIndex>,
        equations: Vec<Statement>,
    },
    While(StatementBlock),
    /// If statement: if cond then stmts elseif cond2 then stmts2 else stmts3
    If {
        cond_blocks: Vec<StatementBlock>,
        else_block: Option<Vec<Statement>>,
    },
    /// When statement: when cond then stmts elsewhen cond2 then stmts2
    When(Vec<StatementBlock>),
    /// Function call statement, optionally with output assignments
    /// For `(a, b) := func(x)`, outputs contains [a, b]
    FunctionCall {
        comp: ComponentReference,
        args: Vec<Expression>,
        /// Output variables being assigned (for `(a, b) := func(x)` style calls)
        outputs: Vec<Expression>,
    },
    /// MLS §8.3.6: reinit(x, expr) - reinitialize a state variable
    /// Only valid inside when-statements.
    Reinit {
        /// The continuous-time state variable to reinitialize.
        variable: ComponentReference,
        /// The new value expression.
        value: Expression,
    },
    /// MLS §8.3.7: assert(condition, message, level)
    Assert {
        /// The Boolean condition to check.
        condition: Expression,
        /// The error message string.
        message: Expression,
        /// Optional assertion level (AssertionLevel.error or .warning).
        level: Option<Box<Expression>>,
    },
}

impl Statement {
    /// Get the source location of the first token in this statement.
    /// Returns None for Empty statements.
    pub fn get_location(&self) -> Option<&Location> {
        match self {
            Statement::Empty => None,
            Statement::Assignment { comp, .. } => comp.get_location(),
            Statement::Return { token } => Some(&token.location),
            Statement::Break { token } => Some(&token.location),
            Statement::For { indices, .. } => indices.first().map(|i| &i.ident.location),
            Statement::While(block) => block.cond.get_location(),
            Statement::If { cond_blocks, .. } => {
                cond_blocks.first().and_then(|b| b.cond.get_location())
            }
            Statement::When(blocks) => blocks.first().and_then(|b| b.cond.get_location()),
            Statement::FunctionCall { comp, .. } => comp.get_location(),
            Statement::Reinit { variable, .. } => variable.get_location(),
            Statement::Assert { condition, .. } => condition.get_location(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub enum Subscript {
    #[default]
    Empty,
    Expression(Expression),
    Range {
        token: Token,
    },
}

impl Display for Subscript {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Subscript::Empty => write!(f, ""),
            Subscript::Expression(expr) => write!(f, "{}", expr),
            Subscript::Range { .. } => write!(f, ":"),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Serialize, Deserialize)]

pub enum Connection {
    #[default]
    Empty,
    Flow(Token),
    Stream(Token),
}

/// Annotation - modification list for annotations
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Annotation {
    pub modifications: Vec<Expression>,
    pub location: Location,
}

/// Modification - represents a modification in class or component declarations
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Modification {
    pub name: Token,
    pub value: Option<Expression>,
    pub sub_modifications: Vec<Modification>,
    pub each: bool,
    pub final_: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_class(name: &str, def_id: DefId) -> ClassDef {
        ClassDef {
            def_id: Some(def_id),
            name: Token {
                text: Arc::from(name),
                ..Token::default()
            },
            ..ClassDef::default()
        }
    }

    #[test]
    fn get_class_by_def_id_resolves_nested_classes() {
        let root_id = DefId::new(1);
        let leaf_id = DefId::new(2);

        let mut tree = ClassTree::new();
        let mut root = make_class("Root", root_id);
        root.classes
            .insert("Leaf".to_string(), make_class("Leaf", leaf_id));
        tree.definitions.classes.insert("Root".to_string(), root);
        tree.def_map.insert(root_id, "Root".to_string());
        tree.def_map.insert(leaf_id, "Root.Leaf".to_string());
        tree.name_map.insert("Root".to_string(), root_id);
        tree.name_map.insert("Root.Leaf".to_string(), leaf_id);

        let root_class = tree.get_class_by_def_id(root_id).expect("missing Root");
        let leaf_class = tree.get_class_by_def_id(leaf_id).expect("missing Leaf");

        assert_eq!(root_class.name.text.as_ref(), "Root");
        assert_eq!(leaf_class.name.text.as_ref(), "Leaf");
    }

    #[test]
    fn get_class_by_def_id_reflects_current_tree_shape_after_reorder() {
        let a_id = DefId::new(10);
        let b_id = DefId::new(11);

        let mut tree = ClassTree::new();
        tree.definitions
            .classes
            .insert("A".to_string(), make_class("A", a_id));
        tree.definitions
            .classes
            .insert("B".to_string(), make_class("B", b_id));
        tree.def_map.insert(a_id, "A".to_string());
        tree.def_map.insert(b_id, "B".to_string());
        tree.name_map.insert("A".to_string(), a_id);
        tree.name_map.insert("B".to_string(), b_id);

        let b_before = tree
            .get_class_by_def_id(b_id)
            .expect("missing B before reorder");
        assert_eq!(b_before.name.text.as_ref(), "B");

        // Reorder top-level class indices after cache initialization.
        let a_class = tree
            .definitions
            .classes
            .shift_remove("A")
            .expect("missing class A");
        tree.definitions.classes.insert("A".to_string(), a_class);

        let b_after = tree
            .get_class_by_def_id(b_id)
            .expect("missing B after reorder");
        assert_eq!(b_after.name.text.as_ref(), "B");
        assert_eq!(b_after.def_id, Some(b_id));
    }

    #[test]
    fn get_class_by_def_id_returns_none_for_stale_def_map_entry() {
        let root_id = DefId::new(21);
        let missing_id = DefId::new(22);

        let mut tree = ClassTree::new();
        tree.definitions
            .classes
            .insert("Root".to_string(), make_class("Root", root_id));
        tree.def_map.insert(root_id, "Root".to_string());
        tree.def_map.insert(missing_id, "Root.Missing".to_string());
        tree.name_map.insert("Root".to_string(), root_id);

        assert!(tree.get_class_by_def_id(missing_id).is_none());
    }

    #[test]
    fn class_def_index_resolves_nested_classes_without_def_map_paths() {
        let root_id = DefId::new(31);
        let leaf_id = DefId::new(32);

        let mut tree = ClassTree::new();
        let mut root = make_class("Root", root_id);
        root.classes
            .insert("Leaf".to_string(), make_class("Leaf", leaf_id));
        tree.definitions.classes.insert("Root".to_string(), root);

        let index = ClassDefIndex::from_tree(&tree);

        assert_eq!(
            index
                .get(root_id)
                .expect("missing Root from class index")
                .name
                .text
                .as_ref(),
            "Root"
        );
        assert_eq!(
            index
                .get(leaf_id)
                .expect("missing Leaf from class index")
                .name
                .text
                .as_ref(),
            "Leaf"
        );
    }

    #[test]
    fn class_def_index_resolves_qualified_name_from_resolved_name_map() {
        let root_id = DefId::new(41);
        let leaf_id = DefId::new(42);

        let mut tree = ClassTree::new();
        let mut root = make_class("Root", root_id);
        root.classes
            .insert("Leaf".to_string(), make_class("Leaf", leaf_id));
        tree.definitions.classes.insert("Root".to_string(), root);
        tree.name_map.insert("Root".to_string(), root_id);
        tree.name_map.insert("Root.Leaf".to_string(), leaf_id);
        let index = ClassDefIndex::from_tree(&tree);

        assert_eq!(
            index
                .get_by_qualified_name("Root.Leaf")
                .expect("missing Leaf from qualified class index lookup")
                .def_id,
            Some(leaf_id)
        );
    }
}
