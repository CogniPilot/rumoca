use logos::{Logos, SpannedIter};

use crate::tokens::{LexicalError, Token}; // your Token enum, as above

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream
            .next()
            .map(|(token, span)| Ok((span.start, token?, span.end)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let filename = String::from("models/integrator.mo");
        let source_code = std::fs::read_to_string(filename).unwrap();
        let mut lexer = Lexer::new(&source_code);
        for result in &mut lexer {
            println!("{:?}", result)
        }
    }
}
