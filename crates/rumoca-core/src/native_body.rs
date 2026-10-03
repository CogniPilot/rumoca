//! Compiler-defined bodies of cataloged foreign entry points (MLS §12.9).
//!
//! An external function has the meaning of its foreign body, and the Solve
//! runtime executes only programs the compiler owns. A foreign entry point
//! therefore executes only when this closed catalog defines it: each row names
//! its exact entry point, its ordered external argument interface, and one
//! definitional evaluator. The DAE proves a declaration against the row's
//! interface, Solve issues the row as one typed operation, and every evaluator
//! and backend computes the row's value through [`NativeBody::evaluate`], so
//! there is one meaning per row (SPEC_0040 DAE-C30).

/// One cataloged foreign entry point with a compiler-defined body.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
#[serde(rename_all = "snake_case")]
pub enum NativeBody {
    /// `ModelicaRandom_xorshift64star(stateIn, stateOut, result)`.
    Xorshift64Star,
    /// `ModelicaRandom_xorshift128plus(stateIn, stateOut, result)`.
    Xorshift128Plus,
    /// `ModelicaRandom_xorshift1024star(stateIn, stateOut, result)`.
    Xorshift1024Star,
}

/// Whether the foreign body reads or writes an external argument position.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeArgumentRole {
    Input,
    Output,
}

/// The Modelica element type crossing one external argument position.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeElement {
    /// A Modelica Integer, passed to the foreign body as a C `int`.
    Integer,
    /// A Modelica Real, passed as a C `double`.
    Real,
}

/// One ordered external argument position of a cataloged interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NativeArgument {
    pub role: NativeArgumentRole,
    pub element: NativeElement,
    /// The vector extent, or `None` for a scalar.
    pub extent: Option<u32>,
}

/// One element value crossing a native body boundary.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NativeScalar {
    Integer(i64),
    Real(f64),
}

/// The operands a native body evaluation received do not match its
/// interface: the checked operation that issued it was not constructed from
/// the row's signature.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NativeOperandMismatch {
    pub body: NativeBody,
}

impl std::fmt::Display for NativeOperandMismatch {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            formatter,
            "native body `{}` received operands outside its interface",
            self.body.entry_point()
        )
    }
}

impl std::error::Error for NativeOperandMismatch {}

const fn argument(
    role: NativeArgumentRole,
    element: NativeElement,
    extent: Option<u32>,
) -> NativeArgument {
    NativeArgument {
        role,
        element,
        extent,
    }
}

const fn xorshift_interface(extent: u32) -> [NativeArgument; 3] {
    [
        argument(
            NativeArgumentRole::Input,
            NativeElement::Integer,
            Some(extent),
        ),
        argument(
            NativeArgumentRole::Output,
            NativeElement::Integer,
            Some(extent),
        ),
        argument(NativeArgumentRole::Output, NativeElement::Real, None),
    ]
}

const XORSHIFT64STAR_INTERFACE: [NativeArgument; 3] = xorshift_interface(2);
const XORSHIFT128PLUS_INTERFACE: [NativeArgument; 3] = xorshift_interface(4);
const XORSHIFT1024STAR_INTERFACE: [NativeArgument; 3] = xorshift_interface(33);

impl NativeBody {
    /// Every row of the catalog.
    pub const ALL: [Self; 3] = [
        Self::Xorshift64Star,
        Self::Xorshift128Plus,
        Self::Xorshift1024Star,
    ];

    /// The row a C entry point names, if the catalog defines it.
    pub fn from_c_entry_point(symbol: &str) -> Option<Self> {
        Self::ALL
            .into_iter()
            .find(|body| body.entry_point() == symbol)
    }

    /// The exact C entry point this row defines.
    pub const fn entry_point(self) -> &'static str {
        match self {
            Self::Xorshift64Star => "ModelicaRandom_xorshift64star",
            Self::Xorshift128Plus => "ModelicaRandom_xorshift128plus",
            Self::Xorshift1024Star => "ModelicaRandom_xorshift1024star",
        }
    }

    /// The ordered external argument interface a declaration must have.
    pub const fn interface(self) -> &'static [NativeArgument] {
        match self {
            Self::Xorshift64Star => &XORSHIFT64STAR_INTERFACE,
            Self::Xorshift128Plus => &XORSHIFT128PLUS_INTERFACE,
            Self::Xorshift1024Star => &XORSHIFT1024STAR_INTERFACE,
        }
    }

    /// The input positions of [`Self::interface`], in order.
    pub fn inputs(self) -> impl Iterator<Item = NativeArgument> {
        self.interface()
            .iter()
            .copied()
            .filter(|argument| argument.role == NativeArgumentRole::Input)
    }

    /// The output positions of [`Self::interface`], in order.
    pub fn outputs(self) -> impl Iterator<Item = NativeArgument> {
        self.interface()
            .iter()
            .copied()
            .filter(|argument| argument.role == NativeArgumentRole::Output)
    }

    /// Evaluate the body: one element slice per input position, in interface
    /// order, and one element vector per output position, in interface order.
    pub fn evaluate(
        self,
        inputs: &[&[NativeScalar]],
    ) -> Result<Vec<Vec<NativeScalar>>, NativeOperandMismatch> {
        let mismatch = NativeOperandMismatch { body: self };
        let [state] = inputs else {
            return Err(mismatch);
        };
        let extent = self
            .inputs()
            .next()
            .and_then(|input| input.extent)
            .ok_or(mismatch)? as usize;
        if state.len() != extent {
            return Err(mismatch);
        }
        let mut words = Vec::with_capacity(extent);
        for value in state.iter() {
            let NativeScalar::Integer(value) = value else {
                return Err(mismatch);
            };
            words.push(c_int(*value));
        }
        let result = match self {
            Self::Xorshift64Star => xorshift64star(&mut words),
            Self::Xorshift128Plus => xorshift128plus(&mut words),
            Self::Xorshift1024Star => xorshift1024star(&mut words),
        };
        let state_out = words
            .into_iter()
            .map(|word| NativeScalar::Integer(i64::from(word)))
            .collect();
        Ok(vec![state_out, vec![NativeScalar::Real(result)]])
    }
}

/// MLS §12.9.1.1 passes an Integer to a foreign body as a C `int`; a wider
/// value is narrowed modulo 2^32 as the C conversion does.
fn c_int(value: i64) -> i32 {
    value as i32
}

/// The 64-bit word whose low and high halves are two consecutive C `int`
/// state elements (the MSL union of `int32_t[2]` and `uint64_t`).
fn word(state: &[i32], index: usize) -> u64 {
    u64::from(state[2 * index] as u32) | (u64::from(state[2 * index + 1] as u32) << 32)
}

fn store_word(state: &mut [i32], index: usize, value: u64) {
    state[2 * index] = value as u32 as i32;
    state[2 * index + 1] = (value >> 32) as u32 as i32;
}

/// `ModelicaRandom_RAND`: the word read as a signed 64-bit integer, scaled by
/// 2^-64 and shifted by 0.5, in binary64.
fn unit_interval(value: u64) -> f64 {
    // The C source spells 2^-64 as 5.42101086242752217004e-20, which rounds
    // to exactly this binary64 value.
    const INVERSE_2_POW_64: f64 = 1.0 / 18_446_744_073_709_551_616.0;
    (value as i64) as f64 * INVERSE_2_POW_64 + 0.5
}

fn xorshift64star(state: &mut [i32]) -> f64 {
    let mut x = word(state, 0);
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    x = x.wrapping_mul(2_685_821_657_736_338_717);
    store_word(state, 0, x);
    unit_interval(x)
}

fn xorshift128plus(state: &mut [i32]) -> f64 {
    let mut s1 = word(state, 0);
    let s0 = word(state, 1);
    store_word(state, 0, s0);
    s1 ^= s1 << 23;
    let next = (s1 ^ s0 ^ (s1 >> 17) ^ (s0 >> 26)).wrapping_add(s0);
    store_word(state, 1, next);
    unit_interval(next)
}

fn xorshift1024star(state: &mut [i32]) -> f64 {
    let p = (state[32] & 15) as usize;
    let mut s0 = word(state, p);
    let p = (p + 1) & 15;
    let mut s1 = word(state, p);
    s1 ^= s1 << 31;
    s1 ^= s1 >> 11;
    s0 ^= s0 >> 30;
    let next = s0 ^ s1;
    store_word(state, p, next);
    state[32] = p as i32;
    unit_interval(next.wrapping_mul(1_181_783_497_276_652_981))
}

#[cfg(test)]
mod tests;
