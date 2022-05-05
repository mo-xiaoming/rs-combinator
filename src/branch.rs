use crate::{ParseError, ParseResult, Parser};

pub trait Alt<'input, Output> {
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output>;
}

impl<'input, Output, P1, P2> Alt<'input, Output> for (P1, P2)
where
    P1: Parser<'input, Output>,
    P2: Parser<'input, Output>,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output> {
        match self.0.parse(input) {
            Err(_) => self.1.parse(input),
            r => r,
        }
    }
}

impl<'input, Output, P1, P2, P3> Alt<'input, Output> for (P1, P2, P3)
where
    P1: Parser<'input, Output>,
    P2: Parser<'input, Output>,
    P3: Parser<'input, Output>,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output> {
        match self.0.parse(input) {
            Err(_) => match self.1.parse(input) {
                Err(_) => self.2.parse(input),
                r => r,
            },
            r => r,
        }
    }
}

pub fn alt<'input, Output>(ps: impl Alt<'input, Output>) -> impl Parser<'input, Output> {
    move |input: &'input str| ps.choice(input)
}

pub trait Permutation<'input, Output> {
    fn permutation(&self, input: &'input str) -> ParseResult<'input, Output>;
}

impl<'input, Output, P1, P2> Permutation<'input, (Output, Output)> for (P1, P2)
where
    P1: Parser<'input, Output>,
    P2: Parser<'input, Output>,
    Output: Copy,
{
    fn permutation(&self, input: &'input str) -> ParseResult<'input, (Output, Output)> {
        let mut it = input;
        let mut res: [Option<Output>; 2] = [None, None];
        let mut err: Option<ParseError> = None;
        loop {
            let mut nerr = 0;
            if res[0].is_none() {
                match self.0.parse(it) {
                    Ok((next_input, last_match)) => {
                        res[0] = Some(last_match);
                        it = next_input;
                        continue;
                    }
                    Err(e) => {
                        if res[1].is_some() {
                            return Err(e);
                        }
                        err = Some(e);
                        nerr += 1;
                    }
                }
            }
            if res[1].is_none() {
                match self.1.parse(it) {
                    Ok((next_input, last_match)) => {
                        res[1] = Some(last_match);
                        it = next_input;
                        continue;
                    }
                    Err(e) => {
                        if res[0].is_some() {
                            return Err(e);
                        }
                        err = Some(e);
                        nerr += 1;
                    }
                }
            }
            if nerr == 2 {
                return Err(err.unwrap());
            }
            if res.iter().all(Option::is_some) {
                return Ok((it, (res[0].unwrap(), res[1].unwrap())));
            }
        }
    }
}

pub fn permutation<'input, Output>(
    ps: impl Permutation<'input, Output>,
) -> impl Parser<'input, Output> {
    move |input: &'input str| ps.permutation(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        chars::{alpha1, anychar, char, digit1},
        ParseError,
    };

    #[test]
    fn test_alt() {
        let parser = |input| alt((alpha1(), digit1())).parse(input);

        assert_eq!(parser("abc"), Ok(("", "abc")));

        assert_eq!(parser("123456"), Ok(("", "123456")));

        assert_eq!(
            parser(" "),
            Err(ParseError::Unexpected {
                input: " ",
                expected: "[0-9]".to_owned(),
                got: " "
            })
        );
    }

    #[test]
    fn test_permutation() {
        let parser1 = |input| permutation((alpha1(), digit1())).parse(input);

        assert_eq!(parser1("abc123"), Ok(("", ("abc", "123"))));

        assert_eq!(parser1("123abc"), Ok(("", ("abc", "123"))));

        assert_eq!(
            parser1("abc;"),
            Err(ParseError::Unexpected {
                input: ";",
                expected: "[0-9]".to_owned(),
                got: ";"
            })
        );

        let parser2 = |input| permutation((anychar(), char('a'))).parse(input);

        assert_eq!(parser2("ba"), Ok(("", ('b', 'a'))));

        assert_eq!(
            parser2("ab"),
            Err(ParseError::Unexpected {
                input: "b",
                expected: "a".to_owned(),
                got: "b"
            })
        );
    }
}
