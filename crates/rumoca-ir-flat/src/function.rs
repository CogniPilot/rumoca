use super::*;

impl Function {
    /// Create a new function definition.
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: VarName::new(name),
            inputs: Vec::new(),
            outputs: Vec::new(),
            locals: Vec::new(),
            body: Vec::new(),
            pure: true, // Default to pure
            external: None,
            derivatives: Vec::new(),
            span,
        }
    }

    /// Add an input parameter.
    pub fn add_input(&mut self, param: FunctionParam) {
        self.inputs.push(param);
    }

    /// Add an output parameter.
    pub fn add_output(&mut self, param: FunctionParam) {
        self.outputs.push(param);
    }

    /// Add a local variable.
    pub fn add_local(&mut self, local: FunctionParam) {
        self.locals.push(local);
    }
}

impl FunctionParam {
    /// Create a new function parameter.
    pub fn new(name: impl Into<String>, type_name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            type_name: type_name.into(),
            dims: Vec::new(),
            default: None,
            description: None,
        }
    }

    /// Create a new array parameter with dimensions.
    pub fn with_dims(mut self, dims: Vec<i64>) -> Self {
        self.dims = dims;
        self
    }

    /// Set a default value.
    pub fn with_default(mut self, default: Expression) -> Self {
        self.default = Some(default);
        self
    }
}
