use crate::{ParseError, ParseResult, Parser};

pub trait Alt<'input, Output> {
    fn choice(&self, input: &'input str) -> ParseResult<'input, Output>;
}

macro_rules! one_parse {
    ($self_:ident, $input:expr, $idx:tt) => {
        match $self_.$idx.parse($input) {
            a @ Ok(_) => return a,
            Err(e) => e,
        }
    };
}

macro_rules! choice_impl {
    ($($parser:ident, $idx:tt), +) => {
        impl<'input, Output, $($parser), +> Alt<'input, Output> for ($($parser), +)
        where
            $($parser: Parser<'input, Output>), +,
            Output: std::fmt::Debug,
        {
            fn choice(&self, input: &'input str) -> ParseResult<'input, Output> {
                Err(ParseError::Multiple(vec![$(one_parse!(self, input, $idx)), +]))
            }
        }
    };
}

choice_impl!(P0, 0, P1, 1);
choice_impl!(P0, 0, P1, 1, P2, 2);

pub fn alt<'input, Output>(ps: impl Alt<'input, Output>) -> impl Parser<'input, Output> {
    move |input: &'input str| ps.choice(input)
}

pub trait Permutation<'input, Output> {
    fn permutation(&self, input: &'input str) -> ParseResult<'input, Output>;
}

macro_rules! check_one {
    ($self_:ident, $errs:ident, $res:ident, $idx:tt, $it:ident) => {
        if $res[$idx].is_none() {
            match $self_.$idx.parse($it) {
                Ok((next_input, last_match)) => {
                    $res[$idx] = Some(last_match);
                    $it = next_input;
                    continue;
                }
                Err(e) => {
                    if $res.into_iter().filter(|o| o.is_none()).count() == 1 {
                        return Err(e);
                    }
                    $errs[$idx] = Some(e);
                }
            }
        }
    };
}

macro_rules! check_all_some {
    ($it:ident, $errs:ident, $res:ident, $($idx:tt), +) => {
        if $errs.iter().all(Option::is_some) {
            return Err(ParseError::Multiple(vec![
                $($errs[$idx].take().unwrap()), +
            ]));
        }
        if $res.iter().all(Option::is_some) {
            return Ok(($it, ($($res[$idx].unwrap()), +)));
        }
    };
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
            check_one!(self, errs, res, 0, it);
            check_one!(self, errs, res, 1, it);
            check_all_some!(it, errs, res, 0, 1);
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
