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
        self.0.parse(input).or_else(|_| self.1.parse(input))
    }
}

impl<'input, Output, P1, P2, P3> Alt<'input, Output> for (P1, P2, P3)
where
    P1: Parser<'input, Output>,
    P2: Parser<'input, Output>,
    P3: Parser<'input, Output>,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output> {
        self.0.parse(input).or_else(|_| self.1.parse(input).or_else(|_| self.2.parse(input)))
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
        assert_eq_parse_error,
        chars::{alpha1, anychar, char, digit1},
        Token,
    };

    #[test]
    fn test_alt() {
        let parser = |input| alt((alpha1(), digit1())).parse(input);

        assert_eq!(parser("abc"), Ok(("", Token::Alpha1("abc"))));

        assert_eq!(parser("123456"), Ok(("", Token::Digit1("123456"))));

        assert_eq_parse_error(" ", parser, Token::Digit1, None);
    }

    #[test]
    fn test_permutation() {
        let parser = |input| permutation((alpha1(), digit1())).parse(input);

        assert_eq!(
            parser("abc123"),
            Ok(("", (Token::Alpha1("abc"), Token::Digit1("123"))))
        );

        assert_eq!(
            parser("123abc"),
            Ok(("", (Token::Alpha1("abc"), Token::Digit1("123"))))
        );

        assert_eq!(parser("abc;").unwrap_err().failed_at, Token::Digit1(";"));
        assert_eq!(parser("abc;").unwrap_err().expected_length, None);

        let parser = |input| permutation((anychar(), char('a'))).parse(input);

        assert_eq!(
            parser("ba"),
            Ok(("", (Token::AnyChar("b"), Token::Char("a"))))
        );

        assert_eq!(parser("ab").unwrap_err().failed_at, Token::Char("b"));
        assert_eq!(parser("ab").unwrap_err().expected_length, Some(1));
    }
}
