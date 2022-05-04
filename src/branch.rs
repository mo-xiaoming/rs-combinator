use crate::{ParseError, ParseResult, Parser};

pub trait Alt<'input> {
    fn choice(&self, input: &'input str) -> ParseResult<'input>;
}

impl<'input, P> Alt<'input> for (P, P)
where
    P: Parser<'input>,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            r => r,
        }
    }
}

impl<'input, P> Alt<'input> for (P, P, P)
where
    P: Parser<'input>,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input> {
        match self.0.parse(input) {
            Err(_) => match self.1.parse(input) {
                Err(_) => self.2.parse(input),
                r => r,
            },
            r => r,
        }
    }
}

pub fn alt<'input>(ps: impl Alt<'input>) -> impl Parser<'input> {
    move |input: &'input str| ps.choice(input)
}
