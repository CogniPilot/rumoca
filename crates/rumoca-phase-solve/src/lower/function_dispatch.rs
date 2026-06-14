use super::*;
use rumoca_ir_solve::ExternalFunctionKind;

fn energyplus_external_function_kind(name: &str) -> Option<ExternalFunctionKind> {
    match name {
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject" => {
            Some(ExternalFunctionKind::BuildingsEnergyPlusSpawnExternalObject)
        }
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.initialize" => {
            Some(ExternalFunctionKind::BuildingsEnergyPlusInitialize)
        }
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.getParameters" => {
            Some(ExternalFunctionKind::BuildingsEnergyPlusGetParameters)
        }
        "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.exchange" => {
            Some(ExternalFunctionKind::BuildingsEnergyPlusExchange)
        }
        _ => None,
    }
}

fn energyplus_external_arg_is_native_handle(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::FunctionCall {
            name,
            is_constructor: true,
            ..
        } if name.as_str()
            == "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject"
    )
}

fn energyplus_external_arg_name_is_non_numeric_metadata(
    function_name: &str,
    arg_name: &str,
) -> bool {
    if function_name != "Buildings.ThermalZones.EnergyPlus_9_6_0.BaseClasses.SpawnExternalObject" {
        return false;
    }
    matches!(
        arg_name,
        "modelicaNameBuilding"
            | "modelicaInstanceName"
            | "spawnExe"
            | "idfVersion"
            | "idfName"
            | "epwName"
            | "epName"
            | "fmuName"
            | "buildingsRootFileLocation"
            | "jsonName"
            | "jsonKeysValues"
            | "parOutNames"
            | "parOutUnits"
            | "inpNames"
            | "inpUnits"
            | "outNames"
            | "outUnits"
    )
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if self.is_record_constructor_call(name, is_constructor) {
            let (named_args, positional_args) =
                function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
            if let Some(expr) = named_args
                .get("re")
                .copied()
                .or_else(|| positional_args.first().copied())
            {
                // Modelica.Complex and other scalar record constructors use
                // declared field order; numeric scalar contexts read the first
                // field unless a projection selects another component.
                return self.lower_expr(expr, caller_scope, call_depth + 1);
            }
            return Ok(self.emit_const(0.0));
        }

        if let Some(reg) = self.lower_runtime_string_special_intrinsic(name.as_str(), args)? {
            return Ok(reg);
        }

        if let Some(reg) = self.try_lower_qualified_standard_numeric_intrinsic(
            name,
            args,
            span,
            caller_scope,
            call_depth,
        )? {
            return Ok(reg);
        }

        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            if let Some(reg) =
                self.try_lower_intrinsic_function_call(name, args, span, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: format!("recursion depth exceeded ({MAX_FUNCTION_INLINE_DEPTH})"),
            });
        }

        let function = if let Some(function) = self.lookup_function(name).cloned() {
            function
        } else if let Some(projection) = self.lookup_function_output_projection(name) {
            return self.lower_projected_function_call(
                &projection,
                args,
                span,
                caller_scope,
                call_depth,
            );
        } else if let Some(closure) = self.lookup_function_closure(name).cloned() {
            return self.lower_function_closure_call(
                &closure,
                args,
                span,
                caller_scope,
                call_depth,
            );
        } else if let Some(reg) =
            self.try_lower_intrinsic_function_call(name, args, span, caller_scope, call_depth)?
        {
            return Ok(reg);
        } else {
            return Err(LowerError::MissingFunction {
                name: name.as_str().to_string(),
            });
        };

        if function.external.is_some() {
            if let Some(reg) =
                self.try_lower_intrinsic_function_call(name, args, span, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            if let Some(reg) =
                self.try_lower_energyplus_external_call(name, args, caller_scope, call_depth)?
            {
                return Ok(reg);
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function call `{}` cannot be inlined",
                    name.as_str()
                ),
            });
        }

        self.with_local_lower_frame(|this| {
            let bindings =
                this.bind_function_inputs(name, &function.inputs, args, caller_scope, call_depth)?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;

            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;

            if let Some(output) = function.outputs.first()
                && let Some(reg) = scope
                    .get(&ComponentPath::from_flat_path(&output.name))
                    .copied()
            {
                return Ok(reg);
            }

            Ok(this.emit_const(0.0))
        })
    }

    fn try_lower_energyplus_external_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(kind) = energyplus_external_function_kind(name.as_str()) else {
            return Ok(None);
        };
        let scalar_args =
            self.lower_energyplus_external_scalar_args(name, args, caller_scope, call_depth)?;
        self.emit_external_call(kind, &scalar_args).map(Some)
    }

    pub(super) fn try_lower_energyplus_external_call_outputs(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
        output_count: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(kind) = energyplus_external_function_kind(name.as_str()) else {
            return Ok(None);
        };
        let scalar_args =
            self.lower_energyplus_external_scalar_args(name, args, caller_scope, call_depth)?;
        let mut outputs = Vec::with_capacity(output_count);
        for output_index in 0..output_count {
            outputs.push(self.emit_external_call_output(kind, &scalar_args, output_index)?);
        }
        Ok(Some(outputs))
    }

    fn lower_energyplus_external_scalar_args(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let (named_args, positional_args) =
            function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
        let positional_arg_names = self
            .lookup_function(name)
            .map(|function| {
                function
                    .inputs
                    .iter()
                    .map(|input| input.name.as_str())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        let mut scalar_args = Vec::new();
        for (index, arg) in positional_args.into_iter().enumerate() {
            if positional_arg_names.get(index).is_some_and(|arg_name| {
                energyplus_external_arg_name_is_non_numeric_metadata(name.as_str(), arg_name)
            }) {
                continue;
            }
            if energyplus_external_arg_is_native_handle(arg) {
                continue;
            }
            scalar_args.push(self.lower_expr(arg, caller_scope, call_depth + 1)?);
        }
        for (arg_name, arg) in named_args {
            if arg_name == "adapter"
                || energyplus_external_arg_name_is_non_numeric_metadata(name.as_str(), &arg_name)
                || energyplus_external_arg_is_native_handle(arg)
            {
                continue;
            }
            scalar_args.push(self.lower_expr(arg, caller_scope, call_depth + 1)?);
        }
        Ok(scalar_args)
    }

    pub(super) fn lookup_function_closure(
        &self,
        name: &rumoca_core::Reference,
    ) -> Option<&FunctionClosure> {
        self.function_closures
            .get(&ComponentPath::from_reference(name))
    }

    pub(super) fn lower_function_closure_call(
        &mut self,
        closure: &FunctionClosure,
        args: &[rumoca_core::Expression],
        _span: rumoca_core::Span,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Err(LowerError::InvalidFunction {
                name: closure.target_name.as_str().to_string(),
                reason: format!("recursion depth exceeded ({MAX_FUNCTION_INLINE_DEPTH})"),
            });
        }
        let Some(function) = self.lookup_function(&closure.target_name).cloned() else {
            return Err(LowerError::MissingFunction {
                name: closure.target_name.as_str().to_string(),
            });
        };
        self.ensure_pure_inline_function(closure.target_name.as_str(), &function)?;
        if function.external.is_some() {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "external function call `{}` cannot be inlined",
                    closure.target_name.as_str()
                ),
            });
        }

        self.with_local_lower_frame(|this| {
            let bindings = this.bind_function_closure_inputs(
                &closure.target_name,
                &function.inputs,
                args,
                caller_scope,
                closure,
                call_depth,
            )?;
            let mut scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut scope, call_depth)?;

            let _returned = this.lower_statements(&function.body, &mut scope, call_depth + 1)?;

            if let Some(output) = function.outputs.first()
                && let Some(reg) = scope
                    .get(&ComponentPath::from_flat_path(&output.name))
                    .copied()
            {
                return Ok(reg);
            }

            Ok(this.emit_const(0.0))
        })
    }

    pub(super) fn lookup_function(
        &self,
        name: &rumoca_core::Reference,
    ) -> Option<&'a rumoca_core::Function> {
        self.lookup_function_key(name.as_str())
    }

    pub(super) fn lookup_function_key(&self, name: &str) -> Option<&'a rumoca_core::Function> {
        let lookup_name = VarName::new(name);
        if let Some(function) = self.functions.get(&lookup_name) {
            return Some(function);
        }
        self.functions
            .iter()
            .find(|(key, _)| key.as_str() == name)
            .map(|(_, value)| value)
    }

    pub(super) fn is_record_constructor_call(
        &self,
        name: &rumoca_core::Reference,
        is_constructor: bool,
    ) -> bool {
        self.is_record_constructor_call_key(name.as_str(), is_constructor)
    }

    pub(super) fn is_record_constructor_call_key(&self, name: &str, is_constructor: bool) -> bool {
        is_constructor
            || self
                .lookup_function_key(name)
                .is_some_and(|function| is_record_constructor_signature(name, function))
    }
}
