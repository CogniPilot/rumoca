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
//!
//! A row whose foreign body reads or writes hidden library state names that
//! state as a [`ForeignStateCell`]. Flattening threads the cell through every
//! call as an explicit value (SPEC_0040 FLAT-C05), so the row's interface is
//! the declared one followed by the cell's input and output, and its body is
//! as pure as any other row.

mod xorshift;

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
    /// `ModelicaRandom_setInternalState_xorshift1024star(state, nState, id)`
    /// threaded through the [`ForeignStateCell::Xorshift1024Star`] cell.
    Xorshift1024StarSetState,
    /// `y = ModelicaRandom_impureRandom_xorshift1024star(id)` threaded
    /// through the [`ForeignStateCell::Xorshift1024Star`] cell.
    Xorshift1024StarImpureDraw,
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

/// Why a native body evaluation produced no result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeBodyError {
    /// The operands do not match the row's interface: the checked operation
    /// that issued the evaluation was not constructed from the row.
    OperandMismatch { body: NativeBody },
    /// The foreign body reports an error for these operands, as the foreign
    /// code does through `ModelicaError`.
    Failure {
        body: NativeBody,
        message: &'static str,
    },
}

impl std::fmt::Display for NativeBodyError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::OperandMismatch { body } => write!(
                formatter,
                "native body `{}` received operands outside its interface",
                body.entry_point()
            ),
            Self::Failure { body, message } => {
                write!(formatter, "`{}`: {message}", body.entry_point())
            }
        }
    }
}

impl std::error::Error for NativeBodyError {}

/// Hidden state a foreign library keeps between calls.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ForeignStateCell {
    /// The `ModelicaRandom.c` impure generator: 16 xorshift1024* words as 32
    /// C `int`s, the word index, and the initialization check `id`.
    Xorshift1024Star,
}

impl ForeignStateCell {
    /// The Integer vector extent that holds the cell.
    pub const fn extent(self) -> u32 {
        match self {
            Self::Xorshift1024Star => 34,
        }
    }

    /// The value the library's static storage holds before any call.
    pub fn initial_value(self) -> Vec<i64> {
        match self {
            Self::Xorshift1024Star => vec![0; self.extent() as usize],
        }
    }

    /// An identifier fragment naming the cell.
    pub const fn name(self) -> &'static str {
        match self {
            Self::Xorshift1024Star => "ModelicaRandom_xorshift1024star_state",
        }
    }
}

/// How a row's foreign body reaches hidden state, and the interface its
/// declaration has before flattening threads the cell through it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ForeignStateAccess {
    pub cell: ForeignStateCell,
    /// The declared external arguments, in order.
    pub declared: &'static [NativeArgument],
    /// The element type of the `output = symbol(...)` return form, if any.
    pub declared_return: Option<NativeElement>,
}

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

const fn input(element: NativeElement, extent: Option<u32>) -> NativeArgument {
    argument(NativeArgumentRole::Input, element, extent)
}

const fn output(element: NativeElement, extent: Option<u32>) -> NativeArgument {
    argument(NativeArgumentRole::Output, element, extent)
}

const INTEGER: NativeElement = NativeElement::Integer;
const REAL: NativeElement = NativeElement::Real;
const CELL: Option<u32> = Some(ForeignStateCell::Xorshift1024Star.extent());

const fn xorshift_interface(extent: u32) -> [NativeArgument; 3] {
    [
        input(INTEGER, Some(extent)),
        output(INTEGER, Some(extent)),
        output(REAL, None),
    ]
}

const XORSHIFT64STAR_INTERFACE: [NativeArgument; 3] = xorshift_interface(2);
const XORSHIFT128PLUS_INTERFACE: [NativeArgument; 3] = xorshift_interface(4);
const XORSHIFT1024STAR_INTERFACE: [NativeArgument; 3] = xorshift_interface(33);
const SET_STATE_DECLARED: [NativeArgument; 3] = [
    input(INTEGER, Some(33)),
    input(INTEGER, None),
    input(INTEGER, None),
];
const SET_STATE_INTERFACE: [NativeArgument; 5] = [
    input(INTEGER, Some(33)),
    input(INTEGER, None),
    input(INTEGER, None),
    input(INTEGER, CELL),
    output(INTEGER, CELL),
];
const IMPURE_DRAW_DECLARED: [NativeArgument; 1] = [input(INTEGER, None)];
const IMPURE_DRAW_INTERFACE: [NativeArgument; 4] = [
    input(INTEGER, None),
    output(REAL, None),
    input(INTEGER, CELL),
    output(INTEGER, CELL),
];

impl NativeBody {
    /// Every row of the catalog.
    pub const ALL: [Self; 5] = [
        Self::Xorshift64Star,
        Self::Xorshift128Plus,
        Self::Xorshift1024Star,
        Self::Xorshift1024StarSetState,
        Self::Xorshift1024StarImpureDraw,
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
            Self::Xorshift1024StarSetState => "ModelicaRandom_setInternalState_xorshift1024star",
            Self::Xorshift1024StarImpureDraw => "ModelicaRandom_impureRandom_xorshift1024star",
        }
    }

    /// The ordered external argument interface a checked declaration has.
    ///
    /// For a row with hidden state this is the threaded interface: the
    /// declared arguments, the return-form output as an output argument, then
    /// the cell's input and output.
    pub const fn interface(self) -> &'static [NativeArgument] {
        match self {
            Self::Xorshift64Star => &XORSHIFT64STAR_INTERFACE,
            Self::Xorshift128Plus => &XORSHIFT128PLUS_INTERFACE,
            Self::Xorshift1024Star => &XORSHIFT1024STAR_INTERFACE,
            Self::Xorshift1024StarSetState => &SET_STATE_INTERFACE,
            Self::Xorshift1024StarImpureDraw => &IMPURE_DRAW_INTERFACE,
        }
    }

    /// The hidden state this row's foreign body reaches, if any.
    pub const fn foreign_state(self) -> Option<ForeignStateAccess> {
        match self {
            Self::Xorshift64Star | Self::Xorshift128Plus | Self::Xorshift1024Star => None,
            Self::Xorshift1024StarSetState => Some(ForeignStateAccess {
                cell: ForeignStateCell::Xorshift1024Star,
                declared: &SET_STATE_DECLARED,
                declared_return: None,
            }),
            Self::Xorshift1024StarImpureDraw => Some(ForeignStateAccess {
                cell: ForeignStateCell::Xorshift1024Star,
                declared: &IMPURE_DRAW_DECLARED,
                declared_return: Some(REAL),
            }),
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
    ) -> Result<Vec<Vec<NativeScalar>>, NativeBodyError> {
        let mismatch = NativeBodyError::OperandMismatch { body: self };
        let expected = self.inputs().collect::<Vec<_>>();
        if inputs.len() != expected.len() {
            return Err(mismatch);
        }
        let mut integers = Vec::with_capacity(inputs.len());
        for (values, argument) in inputs.iter().zip(&expected) {
            let extent = argument.extent.map_or(1, |extent| extent as usize);
            if values.len() != extent || argument.element != INTEGER {
                return Err(mismatch);
            }
            let words = values
                .iter()
                .map(|value| match value {
                    NativeScalar::Integer(value) => Ok(c_int(*value)),
                    NativeScalar::Real(_) => Err(mismatch),
                })
                .collect::<Result<Vec<_>, _>>()?;
            integers.push(words);
        }
        let failure = |message| NativeBodyError::Failure {
            body: self,
            message,
        };
        Ok(match (self, integers.as_mut_slice()) {
            (Self::Xorshift64Star, [state]) => generated(xorshift::xorshift64star, state),
            (Self::Xorshift128Plus, [state]) => generated(xorshift::xorshift128plus, state),
            (Self::Xorshift1024Star, [state]) => generated(xorshift::xorshift1024star, state),
            (Self::Xorshift1024StarSetState, [state, size, id, _]) => {
                // `nState` is a C `size_t`, so a negative `int` is too large.
                if size[0] as u32 > 33 {
                    return Err(failure("External state vector is too large. Should be 33."));
                }
                let mut cell = state.clone();
                cell.push(id[0]);
                vec![integer_values(&cell)]
            }
            (Self::Xorshift1024StarImpureDraw, [id, cell]) => {
                if id[0] != cell[33] {
                    return Err(failure(
                        "Function impureRandom not initialized with function initializeImpureRandom",
                    ));
                }
                let result = xorshift::xorshift1024star(&mut cell[..33]);
                vec![vec![NativeScalar::Real(result)], integer_values(cell)]
            }
            _ => return Err(mismatch),
        })
    }
}

/// A generator row's outputs: the advanced state, then the draw.
fn generated(generator: fn(&mut [i32]) -> f64, state: &mut [i32]) -> Vec<Vec<NativeScalar>> {
    let result = generator(state);
    vec![integer_values(state), vec![NativeScalar::Real(result)]]
}

fn integer_values(words: &[i32]) -> Vec<NativeScalar> {
    words
        .iter()
        .map(|word| NativeScalar::Integer(i64::from(*word)))
        .collect()
}

/// MLS §12.9.1.1 passes an Integer to a foreign body as a C `int`; a wider
/// value is narrowed modulo 2^32 as the C conversion does.
fn c_int(value: i64) -> i32 {
    value as i32
}

#[cfg(test)]
mod tests;
