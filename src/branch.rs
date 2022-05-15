use crate::{ParseError, ParseResult, Parser};

pub trait Alt<'input, Output> {
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output>;
}

impl<'input, Output, P1, P2> Alt<'input, Output> for (P1, P2)
where
    P1: Parser<'input, Output>,
    P2: Parser<'input, Output>,
    Output: std::fmt::Debug,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output> {
        let r1 = self.0.parse(input);
        if let a @ Ok(_) = r1 {
            return a;
        }
        let e1 = r1.unwrap_err();
        let r2 = self.1.parse(input);
        if let a @ Ok(_) = r2 {
            return a;
        }
        let e2 = r2.unwrap_err();
        return Err(ParseError::Multiple(vec![e1, e2]));
    }
}

impl<'input, Output, P1, P2, P3> Alt<'input, Output> for (P1, P2, P3)
where
    P1: Parser<'input, Output>,
    P2: Parser<'input, Output>,
    P3: Parser<'input, Output>,
    Output: std::fmt::Debug,
{
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output> {
        let r1 = self.0.parse(input);
        if let a @ Ok(_) = r1 {
            return a;
        }
        let e1 = r1.unwrap_err();
        let r2 = self.1.parse(input);
        if let a @ Ok(_) = r2 {
            return a;
        }
        let e2 = r2.unwrap_err();
        let r3 = self.2.parse(input);
        if let a @ Ok(_) = r3 {
            return a;
        }
        let e3 = r3.unwrap_err();
        return Err(ParseError::Multiple(vec![e1, e2, e3]));
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
        let mut errs: [Option<ParseError>; 2] = [None, None];
        loop {
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
                        errs[0] = Some(e);
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
                        errs[1] = Some(e);
                    }
                }
            }
            if errs.iter().all(Option::is_some) {
                return Err(ParseError::Multiple(vec![
                    errs[0].take().unwrap(),
                    errs[1].take().unwrap(),
                ]));
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
        tests::{
            assert_eq_parse_error_multiple, assert_eq_parse_error_single, assert_eq_single_error,
            SingleError,
        },
        Token,
    };

    #[test]
    fn test_alt() {
        let parser = |input| alt((alpha1(), digit1())).parse(input);

        assert_eq!(parser("abc"), Ok(("", Token::Alpha1("abc"))));

        assert_eq!(parser("123456"), Ok(("", Token::Digit1("123456"))));

        assert_eq_parse_error_multiple(" ", parser, |me| {
            assert_eq!(me.len(), 2);
            match &me[0] {
                a @ ParseError::Single { .. } => {
                    assert_eq_single_error(
                        " ",
                        &SingleError {
                            token_ctor: Token::Alpha1,
                            expected_length: None,
                            expected_pattern_contains: Some("more than one letter"),
                        },
                        a,
                    );
                }
                ParseError::Multiple(_) => unreachable!(),
            }
            match &me[1] {
                a @ ParseError::Single { .. } => {
                    assert_eq_single_error(
                        " ",
                        &SingleError {
                            token_ctor: Token::Digit1,
                            expected_length: None,
                            expected_pattern_contains: Some("more than one digit"),
                        },
                        a,
                    );
                }
                ParseError::Multiple(_) => unreachable!(),
            };
        });
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

        let se = SingleError {
            token_ctor: Token::Digit1(";"),
            expected_length: None,
            expected_pattern_contains: Some("more than one digit"),
        };
        assert_eq_parse_error_single("abc;", parser, &se);

        let parser = |input| permutation((anychar(), char('a'))).parse(input);

        assert_eq!(
            parser("ba"),
            Ok(("", (Token::AnyChar("b"), Token::Char("a"))))
        );

        let se = SingleError {
            token_ctor: Token::Char("b"),
            expected_length: Some(1),
            expected_pattern_contains: Some("a"),
        };
        assert_eq_parse_error_single("ab", parser, &se);

        let parser = |input| permutation((char('b'), char('a'))).parse(input);
        assert_eq_parse_error_multiple("cde", parser, |me| {
            assert_eq!(me.len(), 2);
            assert_eq_single_error(
                "cde",
                &SingleError {
                    token_ctor: Token::Char,
                    expected_length: Some(1),
                    expected_pattern_contains: Some("b"),
                },
                &me[0],
            );
            assert_eq_single_error(
                "cde",
                &SingleError {
                    token_ctor: Token::Char,
                    expected_length: Some(1),
                    expected_pattern_contains: Some("a"),
                },
                &me[1],
            );
        });
    }
}
