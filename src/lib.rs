/// .0 pointers to the matched slice
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'input> {
    Char(&'input str),
    IsA(&'input str),
    IsNot(&'input str),
    OneOf(&'input str),
    NoneOf(&'input str),
    Tag(&'input str),
    TagNoCase(&'input str),
    Take(&'input str),
    TakeWhile(&'input str),
    TakeUntil(&'input str),
    TakeTill(&'input str),
    Alpha1(&'input str),
    Digit1(&'input str),
    AnyChar(&'input str),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<'input> {
    Single {
        failed_at: Token<'input>, // .0 pointers to the original slice
        expected_pattern: String,
        expected_length: Option<usize>,
    },
    Multiple(Vec<ParseError<'input>>),
}

impl<'input> ParseError<'input> {
    fn new(
        failed_at: Token<'input>,
        expected_pattern: String,
        expected_length: Option<usize>,
    ) -> Self {
        Self::Single {
            failed_at,
            expected_pattern,
            expected_length,
        }
    }

    fn append(self, error: ParseError<'input>) -> Self {
        match self {
            a @ Self::Single { .. } => Self::Multiple(vec![a, error]),
            Self::Multiple(mut m) => {
                m.push(error);
                Self::Multiple(m)
            }
        }
    }
}

impl<'input> std::fmt::Display for ParseError<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! f {
            ($a:expr) => {
                match self {
                    ParseError::Single {
                        expected_pattern, ..
                    } => {
                        format!($a, expected_pattern)
                    }
                    ParseError::Multiple(v) => {
                        let mut s = String::with_capacity(100);
                        for e in v {
                            s.push_str(format!("{e}\n").as_ref());
                        }
                        s
                    }
                }
            };
        }
        match self {
            ParseError::Single {
                failed_at,
                expected_length,
                ..
            } => {
                let s = match failed_at {
                    Token::Char(_) => f!("'{}'"),
                    Token::IsA(_) => f!("till not one of \"{}\""),
                    Token::IsNot(_) => f!("until one of \"{}\""),
                    Token::OneOf(_) => f!("one of \"{}\""),
                    Token::NoneOf(_) => f!("none of \"{}\""),
                    Token::Tag(_) => f!("\"{}\""),
                    Token::TagNoCase(_) => f!("case insensitive \"{}\""),
                    Token::Take(_) => format!("{} chars", expected_length.unwrap()),
                    Token::TakeWhile(_) => f!("{}"),
                    Token::TakeUntil(_) => f!("{}"),
                    Token::TakeTill(_) => f!("{}"),
                    Token::Alpha1(_) => f!("{}"),
                    Token::Digit1(_) => f!("{}"),
                    Token::AnyChar(_) => f!("{}"),
                };
                write!(f, "{s}")
            }
            a @ ParseError::Multiple(_) => write!(f, "{a}"),
        }
    }
}

impl<'input> std::error::Error for ParseError<'input> {}

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

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    pub(crate) struct SingleError<'input, TokenCtor>
    where
        TokenCtor: TokenGen<'input>,
    {
        pub(crate) token_ctor: TokenCtor,
        pub(crate) expected_length: Option<usize>,
        pub(crate) expected_pattern_contains: Option<&'input str>,
    }

    pub(crate) trait TokenGen<'input> {
        fn call(&self, input: &'input str) -> Token<'input>;
    }

    impl<'input, F> TokenGen<'input> for F
    where
        F: Fn(&'input str) -> Token<'input>,
    {
        fn call(&self, input: &'input str) -> Token<'input> {
            self(input)
        }
    }

    impl<'input> TokenGen<'input> for Token<'input> {
        fn call(&self, _: &'input str) -> Token<'input> {
            *self
        }
    }

    pub(crate) fn assert_eq_single_error<'input, TokenCtor>(
        input_failed_at: &'input str,
        expected: &SingleError<'input, TokenCtor>,
        got: &ParseError,
    ) where
        TokenCtor: TokenGen<'input>,
    {
        if let ParseError::Single {
            failed_at,
            expected_length,
            expected_pattern,
        } = got
        {
            assert_eq!(*failed_at, (expected.token_ctor).call(input_failed_at));
            assert_eq!(*expected_length, expected.expected_length);
            if let Some(pat) = expected.expected_pattern_contains {
                assert!(
                    expected_pattern.contains(pat),
                    "'{expected_pattern}' doesn't contain '{pat}'"
                )
            }
        } else {
            let e = ParseError::Single {
                failed_at: (expected.token_ctor).call(input_failed_at),
                expected_length: expected.expected_length,
                expected_pattern: format!(
                    "must contains '{:?}'",
                    expected.expected_pattern_contains
                ),
            };
            unreachable!(
                "{}",
                format!(
                    "expected a ParseError::Single({}), got a ParseError::Multiple({})",
                    e, got
                )
            );
        }
    }

    pub(crate) fn assert_eq_parse_error_single<'input, P, TokenCtor, Output>(
        input: &'input str,
        parser: P,
        error: &SingleError<'input, TokenCtor>,
    ) where
        P: Fn(&'input str) -> ParseResult<'input, Output>,
        TokenCtor: TokenGen<'input>,
        Output: std::fmt::Debug,
    {
        assert_eq_single_error(input, error, &parser(input).unwrap_err());
    }

    pub(crate) fn assert_eq_parse_error_multiple<'input, P, F, Output>(
        input: &'input str,
        parser: P,
        assert_fn: F,
    ) where
        P: Fn(&'input str) -> ParseResult<'input, Output>,
        F: Fn(&Vec<ParseError<'input>>),
        Output: std::fmt::Debug,
    {
        if let ParseError::Multiple(m) = parser(input).unwrap_err() {
            assert_fn(&m);
        } else {
            unreachable!("expected a ParseError::Multiple, got a ParseError::Single")
        }
    }
}

pub mod branch;
pub mod chars;
pub mod multi;
pub mod sequence;

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
