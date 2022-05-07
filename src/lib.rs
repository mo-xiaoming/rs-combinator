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

type ParseResult<'input, Output> = Result<(&'input str, Output), ParseError<'input>>;

pub trait Parser<'input, Output> {
    fn parse(&self, input: &'input str) -> ParseResult<'input, Output>;
}

impl<'input, Output, P> Parser<'input, Output> for P
where
    P: Fn(&'input str) -> ParseResult<'input, Output>,
{
    fn parse(&self, input: &'input str) -> ParseResult<'input, Output> {
        self(input)
    }
}

pub mod branch;
pub mod chars;
pub mod sequence;

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
