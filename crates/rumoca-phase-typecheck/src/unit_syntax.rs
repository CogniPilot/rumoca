//! MLS Chapter 19 unit-expression syntax validation.
//!
//! Validates the *structure* of `unit`/`displayUnit` strings against the
//! MLS §19.2 grammar:
//!
//! ```text
//! unit-expression  := unit-numerator [ "/" unit-denominator ]
//! unit-numerator   := "1" | unit-factors | "(" unit-expression ")"
//! unit-denominator := unit-factor | "(" unit-expression ")"
//! unit-factors     := unit-factor [ "." unit-factors ]
//! unit-factor      := unit-operand [ unit-exponent ]
//! unit-exponent    := [ "+" | "-" ] integer
//! unit-operand     := unit-symbol | unit-prefix unit-symbol
//! ```
//!
//! The symbol vocabulary is deliberately permissive (MLS leaves non-SI
//! symbols tool-specific; MSL itself uses `bar`, `degC`, `rpm`, ...), so this
//! module enforces only the structural rules: no whitespace (§19.2 allows
//! neither comments nor white-space), at most one division per group, dot
//! notation between factors, and well-formed exponents.

/// Why a unit string failed validation. The variants mirror the individual
/// MLS rules so contract tests can target them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnitSyntaxError {
    /// Whitespace is not allowed anywhere in a unit expression.
    Whitespace,
    /// More than one division operator in the same group is ambiguous and
    /// not allowed by the SI standard.
    MultipleDivisions,
    /// Structurally malformed (empty group, dangling operator, bad
    /// parenthesis, malformed exponent, ...).
    Malformed(String),
}

impl std::fmt::Display for UnitSyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Whitespace => {
                write!(
                    f,
                    "white-space is not allowed in a unit expression (MLS §19.2)"
                )
            }
            Self::MultipleDivisions => write!(
                f,
                "multiple units right of the division-symbol are ambiguous; use parentheses (MLS §19.2)"
            ),
            Self::Malformed(detail) => {
                write!(f, "malformed unit expression: {detail} (MLS §19.2)")
            }
        }
    }
}

/// Validate a unit string against the MLS §19.2 unit-expression grammar.
///
/// The empty string means "no unit declared" and is accepted.
pub fn validate_unit_expression(unit: &str) -> Result<(), UnitSyntaxError> {
    if unit.is_empty() {
        return Ok(());
    }
    if unit.chars().any(char::is_whitespace) {
        return Err(UnitSyntaxError::Whitespace);
    }
    let mut parser = UnitParser {
        chars: unit.chars().collect(),
        pos: 0,
    };
    parser.parse_expression()?;
    if parser.pos != parser.chars.len() {
        return Err(UnitSyntaxError::Malformed(format!(
            "unexpected `{}`",
            parser.rest()
        )));
    }
    Ok(())
}

struct UnitParser {
    chars: Vec<char>,
    pos: usize,
}

impl UnitParser {
    fn rest(&self) -> String {
        self.chars[self.pos..].iter().collect()
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.pos).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        Some(ch)
    }

    fn parse_expression(&mut self) -> Result<(), UnitSyntaxError> {
        self.parse_numerator()?;
        if self.peek() == Some('/') {
            self.bump();
            self.parse_denominator()?;
            // MLS §19.2 / SI: at most one division; a second `/` at this
            // level is the ambiguous form (`J/kg/K`).
            if self.peek() == Some('/') {
                return Err(UnitSyntaxError::MultipleDivisions);
            }
        }
        Ok(())
    }

    fn parse_numerator(&mut self) -> Result<(), UnitSyntaxError> {
        if self.peek() == Some('1')
            && !matches!(self.chars.get(self.pos + 1), Some(c) if c.is_ascii_digit())
        {
            // Dimensionless numerator "1" (e.g. `1/min`), unless the digit
            // starts a longer (invalid) token.
            self.bump();
            return Ok(());
        }
        if self.peek() == Some('(') {
            return self.parse_parenthesized();
        }
        self.parse_factors()
    }

    fn parse_denominator(&mut self) -> Result<(), UnitSyntaxError> {
        if self.peek() == Some('(') {
            return self.parse_parenthesized();
        }
        self.parse_factor()
    }

    fn parse_parenthesized(&mut self) -> Result<(), UnitSyntaxError> {
        self.bump(); // consume '('
        self.parse_expression()?;
        if self.bump() != Some(')') {
            return Err(UnitSyntaxError::Malformed(
                "missing closing parenthesis".to_string(),
            ));
        }
        Ok(())
    }

    fn parse_factors(&mut self) -> Result<(), UnitSyntaxError> {
        self.parse_factor()?;
        while self.peek() == Some('.') {
            self.bump();
            self.parse_factor()?;
        }
        Ok(())
    }

    fn parse_factor(&mut self) -> Result<(), UnitSyntaxError> {
        // unit-operand: one or more symbol characters (anything except the
        // structural characters and digits/signs which start the exponent).
        let start = self.pos;
        while let Some(ch) = self.peek() {
            // Unit symbols are letter-like; digits and signs start the
            // exponent, everything else is structure or invalid (`^`, `*`).
            if !(ch.is_alphabetic() || matches!(ch, '°' | 'µ' | '%' | '\'')) {
                break;
            }
            self.bump();
        }
        if self.pos == start {
            return Err(UnitSyntaxError::Malformed(
                if self.pos >= self.chars.len() {
                    "missing unit symbol".to_string()
                } else {
                    format!("expected a unit symbol before `{}`", self.rest())
                },
            ));
        }
        // Optional unit-exponent: [+|-] integer
        if matches!(self.peek(), Some('+') | Some('-')) {
            self.bump();
            if !matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
                return Err(UnitSyntaxError::Malformed(
                    "sign without exponent digits".to_string(),
                ));
            }
        }
        while matches!(self.peek(), Some(c) if c.is_ascii_digit()) {
            self.bump();
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn accepts_si_and_msl_unit_vocabulary() {
        // SI basic + derived (UNIT-007) and the MLS non-SI table (UNIT-008),
        // plus MSL-specific symbols that the open vocabulary must keep valid.
        for unit in [
            "m",
            "kg",
            "s",
            "A",
            "K",
            "mol",
            "cd",
            "rad",
            "Hz",
            "N",
            "Pa",
            "J",
            "W",
            "C",
            "V",
            "F",
            "Ohm",
            "S",
            "Wb",
            "T",
            "H",
            "lm",
            "lx",
            "Bq",
            "Gy",
            "Sv",
            "kat",
            "min",
            "h",
            "d",
            "l",
            "L",
            "eV",
            "deg",
            "degC",
            "bar",
            "rpm",
            "N.m",
            "rad/s",
            "m/s2",
            "J/(kg.K)",
            "kg.m2",
            "1/min",
            "W/m2",
            "uF",
            "mH",
            "ms",
            "km/h",
            "N.m.s",
            "(rad/s)/V",
        ] {
            assert!(
                validate_unit_expression(unit).is_ok(),
                "`{unit}` must be a valid unit expression"
            );
        }
    }

    #[test]
    fn rejects_whitespace_anywhere() {
        for unit in ["N m", " N", "m /s", "kg. m"] {
            assert_eq!(
                validate_unit_expression(unit),
                Err(UnitSyntaxError::Whitespace),
                "`{unit}` must be rejected for whitespace"
            );
        }
    }

    #[test]
    fn rejects_multiple_divisions() {
        assert_eq!(
            validate_unit_expression("J/kg/K"),
            Err(UnitSyntaxError::MultipleDivisions)
        );
        // The parenthesized forms stay valid.
        assert!(validate_unit_expression("J/(kg.K)").is_ok());
        assert!(validate_unit_expression("(J/kg)/K").is_ok());
    }

    #[test]
    fn rejects_structural_garbage() {
        for unit in ["/s", "m..s", "m^2", "m(", "()", "m)", "N.", "m-", "m2."] {
            assert!(
                matches!(
                    validate_unit_expression(unit),
                    Err(UnitSyntaxError::Malformed(_))
                ),
                "`{unit}` must be rejected as malformed"
            );
        }
    }

    #[test]
    fn empty_unit_means_undeclared_and_is_accepted() {
        assert!(validate_unit_expression("").is_ok());
    }
}
