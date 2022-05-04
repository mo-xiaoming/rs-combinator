type Output = String;

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<'input> {
    EarlyEOF {
        input: &'input str,
        expected: String,
    },
    Unexpected {
        input: &'input str,
        expected: String,
        got: &'input str,
    },
    Missing {
        input: &'input str,
        expected: String,
    },
}

type ParseResult<'input> = Result<(&'input str, Output), ParseError<'input>>;

pub trait Parser<'input> {
    fn parse(&self, input: &'input str) -> ParseResult<'input>;
}

impl<'input, P> Parser<'input> for P
where
    P: Fn(&'input str) -> ParseResult<'input>,
{
    fn parse(&self, input: &'input str) -> ParseResult<'input> {
        self(input)
    }
}

pub mod chars;
pub mod branch;

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
