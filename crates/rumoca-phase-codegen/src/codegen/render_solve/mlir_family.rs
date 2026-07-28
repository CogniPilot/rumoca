use super::template_partition::SolveNativeFamilyValue;
use super::*;

pub(in crate::codegen) fn render_solve_native_family_mlir_function(
    family: Value,
    prefix: String,
) -> RenderResult {
    let Some(family) = family.downcast_object_ref::<SolveNativeFamilyValue>() else {
        return Err(render_err(
            "render_solve_native_family_mlir expects a native family from solve_blocks",
        ));
    };
    if prefix.is_empty()
        || !prefix
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
    {
        return Err(render_err(
            "MLIR native-family prefix must be a non-empty identifier",
        ));
    }
    MlirFamilyRenderer::new(family.family(), prefix)?.render()
}

struct MlirFamilyRenderer<'a> {
    family: &'a RenderNativeAffineFamily,
    prefix: String,
    lines: Vec<String>,
    coordinates: Vec<String>,
}

impl<'a> MlirFamilyRenderer<'a> {
    fn new(family: &'a RenderNativeAffineFamily, prefix: String) -> Result<Self, minijinja::Error> {
        validate_mlir_family(family)?;
        let rank = family.domain.binders.len();
        Ok(Self {
            family,
            prefix,
            lines: render_vec_with_capacity(
                family
                    .base_ops
                    .len()
                    .saturating_mul(4)
                    .saturating_add(rank * 4),
                "MLIR native-family instruction count",
            )?,
            coordinates: render_vec_with_capacity(rank, "MLIR native-family coordinate count")?,
        })
    }

    fn render(mut self) -> RenderResult {
        self.emit_loop_header()?;
        for (position, op) in self.family.base_ops.iter().enumerate() {
            self.emit_op(position, *op)?;
        }
        self.line("    }");
        Ok(self.lines.join("\n"))
    }

    fn emit_loop_header(&mut self) -> Result<(), minijinja::Error> {
        mlir_i64_index(self.family.count, "native-family row count")?;
        self.line(format!(
            "    %{}_begin = arith.constant 0 : index",
            self.prefix
        ));
        self.line(format!(
            "    %{}_end = arith.constant {} : index",
            self.prefix, self.family.count
        ));
        self.line(format!(
            "    %{}_step = arith.constant 1 : index",
            self.prefix
        ));
        self.line(format!(
            "    scf.for %{}_row = %{}_begin to %{}_end step %{}_step {{",
            self.prefix, self.prefix, self.prefix, self.prefix
        ));
        self.line(format!(
            "      %{}_row_i64 = arith.index_cast %{}_row : index to i64",
            self.prefix, self.prefix
        ));
        self.emit_coordinates()
    }

    fn emit_coordinates(&mut self) -> Result<(), minijinja::Error> {
        let extents = self
            .family
            .domain
            .extents()
            .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
        let strides = self
            .family
            .domain
            .ordinal_strides()
            .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
        for (dimension, (&extent, &stride)) in extents.iter().zip(&strides).enumerate() {
            mlir_i64_index(extent, "native-family binder extent")?;
            mlir_i64_index(stride, "native-family row-major stride")?;
            let coordinate = format!("%{}_coord{dimension}", self.prefix);
            if extent <= 1 {
                self.line(format!("      {coordinate} = arith.constant 0 : i64"));
            } else {
                let quotient = self.emit_row_quotient(dimension, stride);
                self.line(format!(
                    "      %{}_extent{dimension} = arith.constant {extent} : i64",
                    self.prefix
                ));
                self.line(format!(
                    "      {coordinate} = arith.remui {quotient}, %{}_extent{dimension} : i64",
                    self.prefix
                ));
            }
            self.coordinates.push(coordinate);
        }
        Ok(())
    }

    fn emit_row_quotient(&mut self, dimension: usize, stride: usize) -> String {
        if stride == 1 {
            return format!("%{}_row_i64", self.prefix);
        }
        self.line(format!(
            "      %{}_stride{dimension} = arith.constant {stride} : i64",
            self.prefix
        ));
        self.line(format!(
            "      %{}_quot{dimension} = arith.divui %{}_row_i64, %{}_stride{dimension} : i64",
            self.prefix, self.prefix, self.prefix
        ));
        format!("%{}_quot{dimension}", self.prefix)
    }

    fn emit_op(&mut self, position: usize, op: solve::LinearOp) -> Result<(), minijinja::Error> {
        match op {
            solve::LinearOp::Const { dst, value } => self.emit_const(position, dst, value),
            solve::LinearOp::LoadTime { dst } => {
                self.emit_move_from_value(dst, "%t".to_string());
                Ok(())
            }
            solve::LinearOp::LoadY { dst, index } => self.emit_load(position, dst, index, "y"),
            solve::LinearOp::LoadP { dst, index } => self.emit_load(position, dst, index, "p"),
            solve::LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => self.emit_indexed_p(dst, base, count, index),
            solve::LinearOp::Move { dst, src } => {
                self.emit_move_from_value(dst, self.reg(src));
                Ok(())
            }
            solve::LinearOp::Unary { dst, op, arg } => self.emit_unary(dst, op, arg),
            solve::LinearOp::Binary { dst, op, lhs, rhs } => self.emit_binary(dst, op, lhs, rhs),
            solve::LinearOp::Compare { dst, op, lhs, rhs } => {
                self.emit_compare(dst, op, lhs, rhs);
                Ok(())
            }
            solve::LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                self.emit_select(dst, cond, if_true, if_false);
                Ok(())
            }
            solve::LinearOp::StoreOutput { src } => self.emit_store_output(src),
            unsupported => Err(render_err(format!(
                "MLIR native family does not support `{}`",
                unsupported.kind_name()
            ))),
        }
    }

    fn emit_const(
        &mut self,
        position: usize,
        dst: solve::Reg,
        value: f64,
    ) -> Result<(), minijinja::Error> {
        let terms = self.const_terms_at(position)?;
        if terms.is_empty() {
            self.line(format!(
                "      {} = arith.constant {value:?} : f64",
                self.reg(dst)
            ));
            return Ok(());
        }
        let mut current = format!("%{}_r{dst}_base", self.prefix);
        self.line(format!("      {current} = arith.constant {value:?} : f64"));
        for (term_index, term) in terms.iter().enumerate() {
            let coordinate = self.coordinate(term.dimension)?;
            let coord_f = format!("%{}_r{dst}_coord{term_index}", self.prefix);
            let scale = format!("%{}_r{dst}_scale{term_index}", self.prefix);
            let contribution = format!("%{}_r{dst}_part{term_index}", self.prefix);
            let next = if term_index
                .checked_add(1)
                .is_some_and(|next| next == terms.len())
            {
                self.reg(dst)
            } else {
                format!("%{}_r{dst}_sum{term_index}", self.prefix)
            };
            self.line(format!(
                "      {coord_f} = arith.sitofp {coordinate} : i64 to f64"
            ));
            self.line(format!(
                "      {scale} = arith.constant {:?} : f64",
                term.stride
            ));
            self.line(format!(
                "      {contribution} = arith.mulf {coord_f}, {scale} : f64"
            ));
            self.line(format!(
                "      {next} = arith.addf {current}, {contribution} : f64"
            ));
            current = next;
        }
        Ok(())
    }

    fn emit_load(
        &mut self,
        position: usize,
        dst: solve::Reg,
        base: usize,
        source: &str,
    ) -> Result<(), minijinja::Error> {
        let terms = self.load_terms_at(position)?;
        let index_i64 = self.emit_affine_index(base, &terms, &format!("r{dst}_load"))?;
        let index = format!("%{}_r{dst}_index", self.prefix);
        self.line(format!(
            "      {index} = arith.index_cast {index_i64} : i64 to index"
        ));
        self.line(format!(
            "      {} = memref.load %{source}[{index}] : memref<?xf64>",
            self.reg(dst)
        ));
        Ok(())
    }

    fn emit_affine_index(
        &mut self,
        base: usize,
        terms: &[solve::AffineStencilIndexStrideTerm],
        stem: &str,
    ) -> Result<String, minijinja::Error> {
        let terms = combined_affine_index_terms(terms, &self.family.domain)?;
        mlir_i64_index(base, "native-family affine index base")?;
        let mut current = format!("%{}_{}_base", self.prefix, stem);
        self.line(format!("      {current} = arith.constant {base} : i64"));
        for (term_index, term) in terms.iter().enumerate() {
            let coordinate = self.coordinate(term.dimension)?;
            let scale = format!("%{}_{}_scale{term_index}", self.prefix, stem);
            let contribution = format!("%{}_{}_part{term_index}", self.prefix, stem);
            let next = format!("%{}_{}_sum{term_index}", self.prefix, stem);
            self.line(format!(
                "      {scale} = arith.constant {} : i64",
                term.stride
            ));
            self.line(format!(
                "      {contribution} = arith.muli {coordinate}, {scale} : i64"
            ));
            self.line(format!(
                "      {next} = arith.addi {current}, {contribution} : i64"
            ));
            current = next;
        }
        Ok(current)
    }

    fn load_terms_at(
        &self,
        position: usize,
    ) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, minijinja::Error> {
        let count = self
            .family
            .load_strides
            .iter()
            .filter(|stride| stride.op_position == position)
            .try_fold(0usize, |count, stride| {
                count.checked_add(stride.terms.len())
            })
            .ok_or_else(|| render_err("MLIR native-family load stride count overflows"))?;
        let mut terms = render_vec_with_capacity(count, "MLIR native-family load strides")?;
        for stride in self
            .family
            .load_strides
            .iter()
            .filter(|stride| stride.op_position == position)
        {
            terms.extend(stride.terms.iter().cloned());
        }
        Ok(terms)
    }

    fn const_terms_at(
        &self,
        position: usize,
    ) -> Result<Vec<solve::AffineStencilConstStrideTerm>, minijinja::Error> {
        let count = self
            .family
            .const_strides
            .iter()
            .filter(|stride| stride.op_position == position)
            .try_fold(0usize, |count, stride| {
                count.checked_add(stride.terms.len())
            })
            .ok_or_else(|| render_err("MLIR native-family constant stride count overflows"))?;
        let mut terms = render_vec_with_capacity(count, "MLIR native-family constant strides")?;
        for stride in self
            .family
            .const_strides
            .iter()
            .filter(|stride| stride.op_position == position)
        {
            terms.extend(stride.terms.iter().cloned());
        }
        combined_affine_const_terms(&terms, &self.family.domain)
    }

    fn emit_indexed_p(
        &mut self,
        dst: solve::Reg,
        base: usize,
        count: usize,
        index_reg: solve::Reg,
    ) -> Result<(), minijinja::Error> {
        mlir_i64_index(base, "native-family indexed parameter base")?;
        if count == 0 {
            let index = format!("%{}_r{dst}_indexed_base", self.prefix);
            self.line(format!("      {index} = arith.constant {base} : index"));
            self.line(format!(
                "      {} = memref.load %p[{index}] : memref<?xf64>",
                self.reg(dst)
            ));
            return Ok(());
        }
        let last = count
            .checked_sub(1)
            .ok_or_else(|| render_err("native-family indexed parameter count underflows"))?;
        mlir_i64_index(last, "native-family indexed parameter upper offset")?;
        let absolute_last = base.checked_add(last).ok_or_else(|| {
            render_err("native-family indexed parameter absolute index overflows host range")
        })?;
        mlir_i64_index(
            absolute_last,
            "native-family indexed parameter absolute upper index",
        )?;
        let stem = format!("{}_r{dst}_indexed", self.prefix);
        self.line(format!(
            "      %{stem}_round = math.round {} : f64",
            self.reg(index_reg)
        ));
        self.line(format!("      %{stem}_zero = arith.constant 0.0 : f64"));
        self.line(format!(
            "      %{stem}_high = arith.constant {}.0 : f64",
            last
        ));
        self.line(format!(
            "      %{stem}_low = arith.maxnumf %{stem}_round, %{stem}_zero : f64"
        ));
        self.line(format!(
            "      %{stem}_clamp = arith.minnumf %{stem}_low, %{stem}_high : f64"
        ));
        self.line(format!(
            "      %{stem}_i64 = arith.fptosi %{stem}_clamp : f64 to i64"
        ));
        self.line(format!("      %{stem}_base = arith.constant {base} : i64"));
        self.line(format!(
            "      %{stem}_absolute = arith.addi %{stem}_base, %{stem}_i64 : i64"
        ));
        self.line(format!(
            "      %{stem}_index = arith.index_cast %{stem}_absolute : i64 to index"
        ));
        self.line(format!(
            "      {} = memref.load %p[%{stem}_index] : memref<?xf64>",
            self.reg(dst)
        ));
        Ok(())
    }

    fn emit_unary(
        &mut self,
        dst: solve::Reg,
        op: solve::UnaryOp,
        arg: solve::Reg,
    ) -> Result<(), minijinja::Error> {
        if let Some(operation) = mlir_unary_operation(op) {
            self.line(format!(
                "      {} = {operation} {} : f64",
                self.reg(dst),
                self.reg(arg)
            ));
            return Ok(());
        }
        match op {
            solve::UnaryOp::Not => self.emit_not(dst, arg),
            solve::UnaryOp::Sign => self.emit_sign(dst, arg),
            _ => return Err(render_err(format!("unsupported MLIR unary op {op:?}"))),
        }
        Ok(())
    }

    fn emit_binary(
        &mut self,
        dst: solve::Reg,
        op: solve::BinaryOp,
        lhs: solve::Reg,
        rhs: solve::Reg,
    ) -> Result<(), minijinja::Error> {
        if let Some(operation) = mlir_binary_operation(op) {
            self.line(format!(
                "      {} = {operation} {}, {} : f64",
                self.reg(dst),
                self.reg(lhs),
                self.reg(rhs)
            ));
            return Ok(());
        }
        let boolean_op = match op {
            solve::BinaryOp::And => "arith.andi",
            solve::BinaryOp::Or => "arith.ori",
            _ => return Err(render_err(format!("unsupported MLIR binary op {op:?}"))),
        };
        let zero = format!("%{}_r{dst}_zero", self.prefix);
        let lhs_bool = format!("%{}_r{dst}_lhs_bool", self.prefix);
        let rhs_bool = format!("%{}_r{dst}_rhs_bool", self.prefix);
        let result_bool = format!("%{}_r{dst}_bool", self.prefix);
        self.line(format!("      {zero} = arith.constant 0.0 : f64"));
        self.line(format!(
            "      {lhs_bool} = arith.cmpf une, {}, {zero} : f64",
            self.reg(lhs)
        ));
        self.line(format!(
            "      {rhs_bool} = arith.cmpf une, {}, {zero} : f64",
            self.reg(rhs)
        ));
        self.line(format!(
            "      {result_bool} = {boolean_op} {lhs_bool}, {rhs_bool} : i1"
        ));
        self.line(format!(
            "      {} = arith.uitofp {result_bool} : i1 to f64",
            self.reg(dst)
        ));
        Ok(())
    }

    fn emit_compare(
        &mut self,
        dst: solve::Reg,
        op: solve::CompareOp,
        lhs: solve::Reg,
        rhs: solve::Reg,
    ) {
        let boolean = format!("%{}_r{dst}_bool", self.prefix);
        self.line(format!(
            "      {boolean} = arith.cmpf {}, {}, {} : f64",
            mlir_compare_predicate(op),
            self.reg(lhs),
            self.reg(rhs)
        ));
        self.line(format!(
            "      {} = arith.uitofp {boolean} : i1 to f64",
            self.reg(dst)
        ));
    }

    fn emit_select(
        &mut self,
        dst: solve::Reg,
        cond: solve::Reg,
        if_true: solve::Reg,
        if_false: solve::Reg,
    ) {
        let zero = format!("%{}_r{dst}_zero", self.prefix);
        let boolean = format!("%{}_r{dst}_bool", self.prefix);
        self.line(format!("      {zero} = arith.constant 0.0 : f64"));
        self.line(format!(
            "      {boolean} = arith.cmpf une, {}, {zero} : f64",
            self.reg(cond)
        ));
        self.line(format!(
            "      {} = arith.select {boolean}, {}, {} : f64",
            self.reg(dst),
            self.reg(if_true),
            self.reg(if_false)
        ));
    }

    fn emit_not(&mut self, dst: solve::Reg, arg: solve::Reg) {
        let zero = format!("%{}_r{dst}_zero", self.prefix);
        let boolean = format!("%{}_r{dst}_bool", self.prefix);
        self.line(format!("      {zero} = arith.constant 0.0 : f64"));
        self.line(format!(
            "      {boolean} = arith.cmpf oeq, {}, {zero} : f64",
            self.reg(arg)
        ));
        self.line(format!(
            "      {} = arith.uitofp {boolean} : i1 to f64",
            self.reg(dst)
        ));
    }

    fn emit_sign(&mut self, dst: solve::Reg, arg: solve::Reg) {
        let stem = format!("%{}_r{dst}", self.prefix);
        self.line(format!("      {stem}_zero = arith.constant 0.0 : f64"));
        self.line(format!("      {stem}_one = arith.constant 1.0 : f64"));
        self.line(format!("      {stem}_neg = arith.constant -1.0 : f64"));
        self.line(format!(
            "      {stem}_gt = arith.cmpf ogt, {}, {stem}_zero : f64",
            self.reg(arg)
        ));
        self.line(format!(
            "      {stem}_lt = arith.cmpf olt, {}, {stem}_zero : f64",
            self.reg(arg)
        ));
        self.line(format!(
            "      {stem}_lower = arith.select {stem}_lt, {stem}_neg, {stem}_zero : f64"
        ));
        self.line(format!(
            "      {} = arith.select {stem}_gt, {stem}_one, {stem}_lower : f64",
            self.reg(dst)
        ));
    }

    fn emit_move_from_value(&mut self, dst: solve::Reg, value: String) {
        let one = format!("%{}_r{dst}_move_one", self.prefix);
        self.line(format!("      {one} = arith.constant 1.0 : f64"));
        self.line(format!(
            "      {} = arith.mulf {value}, {one} : f64",
            self.reg(dst)
        ));
    }

    fn emit_store_output(&mut self, src: solve::Reg) -> Result<(), minijinja::Error> {
        let terms = self.family.output_map.strides.clone();
        let output_i64 = self.emit_affine_index(self.family.output_map.start, &terms, "output")?;
        let output = format!("%{}_output_index", self.prefix);
        self.line(format!(
            "      {output} = arith.index_cast {output_i64} : i64 to index"
        ));
        self.line(format!(
            "      memref.store {}, %out[{output}] : memref<?xf64>",
            self.reg(src)
        ));
        Ok(())
    }

    fn coordinate(&self, dimension: usize) -> Result<String, minijinja::Error> {
        self.coordinates.get(dimension).cloned().ok_or_else(|| {
            render_err(format!(
                "MLIR native-family stride dimension {dimension} is out of bounds"
            ))
        })
    }

    fn reg(&self, register: solve::Reg) -> String {
        format!("%{}_r{register}", self.prefix)
    }

    fn line(&mut self, line: impl Into<String>) {
        self.lines.push(line.into());
    }
}

fn mlir_unary_operation(op: solve::UnaryOp) -> Option<&'static str> {
    match op {
        solve::UnaryOp::Neg => Some("arith.negf"),
        solve::UnaryOp::Abs => Some("math.absf"),
        solve::UnaryOp::Sqrt => Some("math.sqrt"),
        solve::UnaryOp::Floor => Some("math.floor"),
        solve::UnaryOp::Ceil => Some("math.ceil"),
        solve::UnaryOp::Trunc => Some("math.trunc"),
        solve::UnaryOp::Sin => Some("math.sin"),
        solve::UnaryOp::Cos => Some("math.cos"),
        solve::UnaryOp::Tan => Some("math.tan"),
        solve::UnaryOp::Asin => Some("math.asin"),
        solve::UnaryOp::Acos => Some("math.acos"),
        solve::UnaryOp::Atan => Some("math.atan"),
        solve::UnaryOp::Sinh => Some("math.sinh"),
        solve::UnaryOp::Cosh => Some("math.cosh"),
        solve::UnaryOp::Tanh => Some("math.tanh"),
        solve::UnaryOp::Exp => Some("math.exp"),
        solve::UnaryOp::Log => Some("math.log"),
        solve::UnaryOp::Log10 => Some("math.log10"),
        solve::UnaryOp::Not | solve::UnaryOp::Sign => None,
    }
}

fn mlir_binary_operation(op: solve::BinaryOp) -> Option<&'static str> {
    match op {
        solve::BinaryOp::Add => Some("arith.addf"),
        solve::BinaryOp::Sub => Some("arith.subf"),
        solve::BinaryOp::Mul => Some("arith.mulf"),
        solve::BinaryOp::Div => Some("arith.divf"),
        solve::BinaryOp::Pow => Some("math.powf"),
        solve::BinaryOp::Atan2 => Some("math.atan2"),
        solve::BinaryOp::Min => Some("arith.minnumf"),
        solve::BinaryOp::Max => Some("arith.maxnumf"),
        solve::BinaryOp::And | solve::BinaryOp::Or => None,
    }
}

fn mlir_compare_predicate(op: solve::CompareOp) -> &'static str {
    match op {
        solve::CompareOp::Lt => "olt",
        solve::CompareOp::Le => "ole",
        solve::CompareOp::Gt => "ogt",
        solve::CompareOp::Ge => "oge",
        solve::CompareOp::Eq => "oeq",
        solve::CompareOp::Ne => "une",
    }
}

fn mlir_i64_index(value: usize, context: &str) -> Result<i64, minijinja::Error> {
    i64::try_from(value)
        .map_err(|_| render_err(format!("{context} {value} exceeds MLIR i64 index range")))
}

fn validate_mlir_family(family: &RenderNativeAffineFamily) -> Result<(), minijinja::Error> {
    let scalar_count = family
        .domain
        .scalar_count()
        .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
    if family.count != scalar_count {
        return Err(render_err(format!(
            "MLIR native-family row count {} does not match domain cardinality {scalar_count}",
            family.count
        )));
    }
    let extents = family
        .domain
        .extents()
        .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
    validate_mlir_output_map(family, &extents)?;
    validate_mlir_stride_metadata(family, &extents)
}

fn validate_mlir_output_map(
    family: &RenderNativeAffineFamily,
    extents: &[usize],
) -> Result<(), minijinja::Error> {
    let output_count = family
        .output_map
        .output_count(&family.domain)
        .map_err(|error| render_err(format!("invalid MLIR native-family output map: {error:?}")))?;
    if let Some(maximum) = output_count.checked_sub(1) {
        mlir_i64_index(maximum, "native-family maximum output index")?;
    }
    validate_mlir_affine_index_bounds(
        family.output_map.start,
        &family.output_map.strides,
        extents,
        "native-family output map",
    )
}

fn validate_mlir_stride_metadata(
    family: &RenderNativeAffineFamily,
    extents: &[usize],
) -> Result<(), minijinja::Error> {
    for stride in &family.load_strides {
        let Some(op) = family.base_ops.get(stride.op_position) else {
            return Err(render_err(format!(
                "MLIR native-family load stride targets missing op {} of {}",
                stride.op_position,
                family.base_ops.len()
            )));
        };
        let base = match op {
            solve::LinearOp::LoadY { index, .. } | solve::LinearOp::LoadP { index, .. } => *index,
            _ => {
                return Err(render_err(format!(
                    "MLIR native-family load stride at op {} targets `{}`, not LoadY or LoadP",
                    stride.op_position,
                    op.kind_name()
                )));
            }
        };
        validate_mlir_affine_index_bounds(
            base,
            &combined_load_terms(family, stride.op_position)?,
            extents,
            "native-family load",
        )?;
    }
    for stride in &family.const_strides {
        let Some(op) = family.base_ops.get(stride.op_position) else {
            return Err(render_err(format!(
                "MLIR native-family constant stride targets missing op {} of {}",
                stride.op_position,
                family.base_ops.len()
            )));
        };
        if !matches!(op, solve::LinearOp::Const { .. }) {
            return Err(render_err(format!(
                "MLIR native-family constant stride at op {} targets `{}`, not Const",
                stride.op_position,
                op.kind_name()
            )));
        }
        for term in &stride.terms {
            if term.dimension >= extents.len() {
                return Err(render_err(format!(
                    "MLIR native-family constant stride dimension {} exceeds domain rank {}",
                    term.dimension,
                    extents.len()
                )));
            }
            if !term.stride.is_finite() {
                return Err(render_err(
                    "MLIR native-family constant stride must be finite",
                ));
            }
        }
    }
    Ok(())
}

fn combined_load_terms(
    family: &RenderNativeAffineFamily,
    op_position: usize,
) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, minijinja::Error> {
    let count = family
        .load_strides
        .iter()
        .filter(|stride| stride.op_position == op_position)
        .try_fold(0usize, |count, stride| {
            count.checked_add(stride.terms.len())
        })
        .ok_or_else(|| render_err("MLIR native-family load stride count overflows"))?;
    let mut terms = render_vec_with_capacity(count, "MLIR native-family load strides")?;
    for stride in family
        .load_strides
        .iter()
        .filter(|stride| stride.op_position == op_position)
    {
        terms.extend(stride.terms.iter().cloned());
    }
    Ok(terms)
}

fn validate_mlir_affine_index_bounds(
    base: usize,
    terms: &[solve::AffineStencilIndexStrideTerm],
    extents: &[usize],
    context: &str,
) -> Result<(), minijinja::Error> {
    let start = i128::try_from(base)
        .map_err(|_| render_err(format!("{context} base exceeds MLIR integer range")))?;
    let mut dimension_strides =
        render_vec_with_capacity(extents.len(), "MLIR native-family affine dimension strides")?;
    dimension_strides.resize(extents.len(), 0i128);
    for term in terms {
        let Some(stride) = dimension_strides.get_mut(term.dimension) else {
            return Err(render_err(format!(
                "{context} stride dimension {} exceeds domain rank {}",
                term.dimension,
                extents.len()
            )));
        };
        *stride = stride
            .checked_add(term.stride as i128)
            .ok_or_else(|| render_err(format!("{context} stride accumulation overflows")))?;
    }
    let mut minimum = start;
    let mut maximum = start;
    for (&extent, stride) in extents.iter().zip(dimension_strides) {
        let last = i128::try_from(extent.saturating_sub(1))
            .map_err(|_| render_err(format!("{context} extent exceeds MLIR integer range")))?;
        let offset = last
            .checked_mul(stride)
            .ok_or_else(|| render_err(format!("{context} stride multiplication overflows")))?;
        if offset < 0 {
            minimum = minimum
                .checked_add(offset)
                .ok_or_else(|| render_err(format!("{context} minimum index overflows")))?;
        } else {
            maximum = maximum
                .checked_add(offset)
                .ok_or_else(|| render_err(format!("{context} maximum index overflows")))?;
        }
    }
    if minimum < 0 {
        return Err(render_err(format!(
            "{context} produces negative index {minimum}"
        )));
    }
    if maximum > i128::from(i64::MAX) {
        return Err(render_err(format!(
            "{context} maximum index {maximum} exceeds MLIR i64 index range"
        )));
    }
    Ok(())
}
