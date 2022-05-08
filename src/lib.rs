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
pub struct ParseError<'input> {
    pub failed_at: Token<'input>, // .0 pointers to the original slice
    pub expected_pattern: String,
    pub expected_length: Option<usize>,
}

impl<'input> ParseError<'input> {
    fn new(
        failed_at: Token<'input>,
        expected_pattern: String,
        expected_length: Option<usize>,
    ) -> Self {
        Self {
            failed_at,
            expected_pattern,
            expected_length,
        }
    }
}

impl<'input> std::fmt::Display for ParseError<'input> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        macro_rules! f {
            ($a:expr) => {
                format!($a, self.expected_pattern)
            };
        }
        let s = match self.failed_at {
            Token::Char(_) => f!("'{}'"),
            Token::IsA(_) => f!("till not one of \"{}\""),
            Token::IsNot(_) => f!("until one of \"{}\""),
            Token::OneOf(_) => f!("one of \"{}\""),
            Token::NoneOf(_) => f!("none of \"{}\""),
            Token::Tag(_) => f!("\"{}\""),
            Token::TagNoCase(_) => f!("case insensitive \"{}\""),
            Token::Take(_) => format!("{} chars", self.expected_length.unwrap()),
            Token::TakeWhile(_) => f!("{}"),
            Token::TakeUntil(_) => f!("{}"),
            Token::TakeTill(_) => f!("{}"),
            Token::Alpha1(_) => f!("{}"),
            Token::Digit1(_) => f!("{}"),
            Token::AnyChar(_) => f!("{}"),
        };
        write!(f, "{s}")
    }
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

#[cfg(test)]
pub(crate) fn assert_eq_parse_error<'a, P, TokenCtor, Output>(
    input: &'a str,
    parser: P,
    token_ctor: TokenCtor,
    expected_length: Option<usize>,
) where
    P: Fn(&'a str) -> ParseResult<'a, Output>,
    TokenCtor: Fn(&'a str) -> Token,
    Output: std::fmt::Debug,
{
    assert_eq!(parser(input).unwrap_err().failed_at, token_ctor(input));
    assert_eq!(parser(input).unwrap_err().expected_length, expected_length);
}

pub mod branch;
pub mod chars;
pub mod sequence;
pub mod multi;

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
