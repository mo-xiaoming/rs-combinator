#[derive(Debug, PartialEq, Eq)]
pub enum ParseError<'a> {
    EarlyEOF {
        input: &'a str,
        expected: String,
    },
    Unexpected {
        input: &'a str,
        expected: String,
        got: &'a str,
    },
}

type ParseResult<'a, Output> = Result<(&'a str, Output), ParseError<'a>>;

pub trait Parser<'a, Output> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output>;
}

impl<'a, P, Output> Parser<'a, Output> for P
where
    P: Fn(&'a str) -> ParseResult<'a, Output>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, Output> {
        self(input)
    }
}

pub fn char<'a>(c: char) -> impl Parser<'a, char> {
    move |input: &'a str| match input.chars().next() {
        Some(m) if m == c => Ok((&input[c.len_utf8()..], c)),
        Some(m) => Err(ParseError::Unexpected {
            input,
            expected: c.to_string(),
            got: &input[..m.len_utf8()],
        }),
        None => Err(ParseError::EarlyEOF {
            input,
            expected: c.to_string(),
        }),
    }
}

/*
pub fn is_a<'a, 'b>(s: &'b str) -> impl Parser<'a, &'a str> + 'b {
    move |input: &'a str| {
        let mut i = input;
        loop {
            match i.get(..s.len()) {
                Some(m) if m == s => i = &i[s.len()..],
                Some(m) => {
                    if i == input {
                        return Err(ParseError::Unexpected {
                            input,
                            expected: s.to_owned(),
                            got: m,
                        });
                    }
                    return Ok((i, &input[..input.len() - i.len()]));
                }
                None => {
                    return Err(ParseError::EarlyEOF {
                        input,
                        expected: s.to_owned(),
                    })
                }
            }
        }
    }
}
 */

pub fn is_a<'a, 'b>(s: &'b str) -> impl Parser<'a, &'a str> + 'b {
    move |input: &'a str| {
        let mut i = 0;
        loop {
            match input.chars().next() {
                Some(m) if s.contains(m) => i += 1,
                Some(_) => {
                    if i == 0 {
                        return Err(ParseError::Unexpected {
                            input,
                            expected: s.to_owned(),
                            got: &input[..i],
                        });
                    }
                    return Ok((&input[i..], &input[..i]));
                }
                None => {
                    return Err(ParseError::EarlyEOF {
                        input,
                        expected: s.to_owned(),
                    })
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        assert_eq!(char('a').parse("abc"), Ok(("bc", 'a')));

        assert_eq!(
            char('a').parse(" abc"),
            Err(ParseError::Unexpected {
                input: " abc",
                expected: String::from("a"),
                got: " "
            })
        );

        assert_eq!(
            char('a').parse("bc"),
            Err(ParseError::Unexpected {
                input: "bc",
                expected: String::from("a"),
                got: "b"
            })
        );
    }

    #[test]
    fn test_is_a() {
        assert_eq!(
            is_a("1234567890ABCDEF").parse("123 and voila"),
            Ok((" and voila", "123"))
        );

        assert_eq!(
            is_a("1234567890ABCDEF").parse("DEADBEEF and others"),
            Ok((" and others", "DEADBEEF"))
        );

        assert_eq!(
            is_a("1234567890ABCDEF").parse("BADBABEsomething"),
            Ok(("something", "BADBABE"))
        );

        assert_eq!(
            is_a("1234567890ABCDEF").parse("D15EA5E"),
            Ok(("", "D15EA5E"))
        );

        assert_eq!(
            is_a("1234567890ABCDEF").parse(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "1234567890ABCDEF".to_owned()
            })
        );
    }
}

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
