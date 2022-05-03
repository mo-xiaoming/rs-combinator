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
}

type ParseResult<'input, Output> = Result<(&'input str, Output), ParseError<'input>>;

pub trait Parser<'input, Output> {
    fn parse(&self, input: &'input str) -> ParseResult<'input, Output>;
}

impl<'input, P, Output> Parser<'input, Output> for P
where
    P: Fn(&'input str) -> ParseResult<'input, Output>,
{
    fn parse(&self, input: &'input str) -> ParseResult<'input, Output> {
        self(input)
    }
}

pub fn char<'input>(c: char) -> impl Parser<'input, char> {
    move |input: &'input str| match input.chars().next() {
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
pub fn is_a<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| {
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

fn is_a_not_impl<'input, 'b, P: 'b>(s: &'b str, pred: P) -> impl Parser<'input, &'input str> + 'b
where
    P: Fn(&'b str, char) -> bool,
{
    move |input: &'input str| {
        let mut it = input;
        loop {
            match it.chars().next() {
                Some(m) if pred(s, m) => it = &it[m.len_utf8()..],
                Some(m) => {
                    if it == input {
                        return Err(ParseError::Unexpected {
                            input,
                            expected: s.to_owned(),
                            got: &input[..m.len_utf8()],
                        });
                    }
                    let l = input.len() - it.len();
                    return Ok((&input[l..], &input[..l]));
                }
                None => {
                    if it == input {
                        return Err(ParseError::EarlyEOF {
                            input,
                            expected: s.to_owned(),
                        });
                    }
                    let l = input.len() - it.len();
                    return Ok((&input[l..], &input[..l]));
                }
            }
        }
    }
}

pub fn is_a<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| is_a_not_impl(s, str::contains).parse(input)
}

pub fn is_not<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| is_a_not_impl(s, |a, c| !a.contains(c)).parse(input)
}

fn one_none_of_impl<'input, 'b, P: 'b>(s: &'b str, pred: P) -> impl Parser<'input, char> + 'b
where
    P: Fn(&'b str, char) -> bool,
{
    move |input: &'input str| match input.chars().next() {
        Some(m) if pred(s, m) => Ok((&input[m.len_utf8()..], m)),
        Some(m) => Err(ParseError::Unexpected {
            input,
            expected: s.to_owned(),
            got: &input[..m.len_utf8()],
        }),
        None => Err(ParseError::EarlyEOF {
            input,
            expected: s.to_owned(),
        }),
    }
}
pub fn one_of<'input, 'b>(s: &'b str) -> impl Parser<'input, char> + 'b {
    move |input: &'input str| one_none_of_impl(s, str::contains).parse(input)
}

pub fn none_of<'input, 'b>(s: &'b str) -> impl Parser<'input, char> + 'b {
    move |input: &'input str| one_none_of_impl(s, |a, c| !a.contains(c)).parse(input)
}

pub fn tag<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| match input.get(..s.len()) {
        Some(m) if m == s => Ok((&input[m.len()..], &input[..m.len()])),
        Some(m) => Err(ParseError::Unexpected {
            input,
            expected: s.to_owned(),
            got: m,
        }),
        None => Err(ParseError::EarlyEOF {
            input,
            expected: s.to_owned(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_char() {
        let a = |input| char('a').parse(input);

        assert_eq!(a("abc"), Ok(("bc", 'a')));

        assert_eq!(
            a(" abc"),
            Err(ParseError::Unexpected {
                input: " abc",
                expected: String::from("a"),
                got: " "
            })
        );

        assert_eq!(
            a("bc"),
            Err(ParseError::Unexpected {
                input: "bc",
                expected: String::from("a"),
                got: "b"
            })
        );
    }

    #[test]
    fn test_is_a() {
        let hex = |input| is_a("1234567890ABCDEF").parse(input);

        assert_eq!(hex("123 and voila"), Ok((" and voila", "123")));

        assert_eq!(hex("DEADBEEF and others"), Ok((" and others", "DEADBEEF")));

        assert_eq!(hex("BADBABEsomething"), Ok(("something", "BADBABE")));

        assert_eq!(hex("D15EA5E"), Ok(("", "D15EA5E")));

        assert_eq!(
            hex("xD"),
            Err(ParseError::Unexpected {
                input: "xD",
                expected: "1234567890ABCDEF".to_owned(),
                got: "x"
            })
        );

        assert_eq!(
            hex(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "1234567890ABCDEF".to_owned()
            })
        );
    }

    #[test]
    fn test_is_not() {
        let not_space = |input| is_not(" \t\r\n").parse(input);

        assert_eq!(not_space("Hello, World!"), Ok((" World!", "Hello,")));

        assert_eq!(not_space("Sometimes\t"), Ok(("\t", "Sometimes")));

        assert_eq!(not_space("Nospace"), Ok(("", "Nospace")));

        assert_eq!(
            not_space(" N"),
            Err(ParseError::Unexpected {
                input: " N",
                expected: " \t\r\n".to_owned(),
                got: " "
            })
        );

        assert_eq!(
            not_space(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: " \t\r\n".to_owned()
            })
        );
    }

    #[test]
    fn test_one_of() {
        assert_eq!(one_of("abc").parse("b"), Ok(("", 'b')));

        assert_eq!(
            one_of("a").parse("bc"),
            Err(ParseError::Unexpected {
                input: "bc",
                expected: "a".to_owned(),
                got: "b"
            })
        );

        assert_eq!(
            one_of("a").parse(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "a".to_owned()
            })
        );
    }

    #[test]
    fn test_none_of() {
        assert_eq!(none_of("abc").parse("z"), Ok(("", 'z')));

        assert_eq!(
            none_of("ab").parse("a"),
            Err(ParseError::Unexpected {
                input: "a",
                expected: "ab".to_owned(),
                got: "a"
            })
        );

        assert_eq!(
            none_of("a").parse(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "a".to_owned()
            })
        );
    }

    #[test]
    fn test_tag() {
        let hello = |input| tag("Hello").parse(input);

        assert_eq!(hello("Hello, World!"), Ok((", World!", "Hello")));

        assert_eq!(
            hello("Something"),
            Err(ParseError::Unexpected {
                input: "Something",
                expected: "Hello".to_owned(),
                got: "Somet"
            })
        );

        assert_eq!(
            hello("less"),
            Err(ParseError::EarlyEOF {
                input: "less",
                expected: "Hello".to_owned()
            })
        );

        assert_eq!(
            hello("Hell"),
            Err(ParseError::EarlyEOF {
                input: "Hell",
                expected: "Hello".to_owned()
            })
        );

        assert_eq!(
            hello(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "Hello".to_owned()
            })
        );
    }
}

#[doc = include_str!("../README.md")]
#[cfg(doctest)]
pub struct ReadmeDoctests;
