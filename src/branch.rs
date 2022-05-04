use crate::{ParseResult, Parser};

pub trait Alt<'input> {
    fn choice(&self, input: &'input str) -> ParseResult<'input>;
}

impl<'input, P1, P2> Alt<'input> for (P1, P2)
where
    P1: Parser<'input>,
    P2: Parser<'input>,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            r => r,
        }
    }
}

impl<'input, P1, P2, P3> Alt<'input> for (P1, P2, P3)
where
    P1: Parser<'input>,
    P2: Parser<'input>,
    P3: Parser<'input>,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        chars::{alpha1, digit1},
        ParseError,
    };

    #[test]
    fn test_alt() {
        let parser = |input| alt((alpha1(), digit1())).parse(input);

        assert_eq!(parser("abc"), Ok(("", "abc".to_owned())));

        assert_eq!(parser("123456"), Ok(("", "123456".to_owned())));

        assert_eq!(
            parser(" "),
            Err(ParseError::Unexpected {
                input: " ",
                expected: "[0-9]".to_owned(),
                got: " "
            })
        );
    }
}
