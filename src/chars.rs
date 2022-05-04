use crate::{ParseError, Parser};

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

pub fn tag_no_case_impl<'input, 'b, P: 'b>(
    s: &'b str,
    cmp: P,
) -> impl Parser<'input, &'input str> + 'b
where
    P: Fn(&str, &str) -> bool,
{
    move |input: &'input str| match input.get(..s.len()) {
        Some(m) if cmp(m, s) => Ok((&input[m.len()..], &input[..m.len()])),
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

pub fn tag<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| tag_no_case_impl(s, str::eq).parse(input)
}

pub fn tag_no_case<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| {
        tag_no_case_impl(s, |a, b| a.to_lowercase() == b.to_ascii_lowercase()).parse(input)
    }
}

pub fn take<'input>(n: usize) -> impl Parser<'input, &'input str> {
    move |input: &'input str| match input.get(..n) {
        Some(m) => Ok((&input[n..], m)),
        None => Err(ParseError::EarlyEOF {
            input,
            expected: format!("{n} length string"),
        }),
    }
}

pub fn take_while<'input, P>(pred: P) -> impl Parser<'input, &'input str>
where
    P: Fn(char) -> bool,
{
    move |input: &'input str| {
        let mut i = 0;
        for c in input.chars() {
            if !pred(c) {
                break;
            }
            i += 1;
        }
        Ok((&input[i..], &input[..i]))
    }
}

pub fn take_till<'input, P>(pred: P) -> impl Parser<'input, &'input str>
where
    P: Fn(char) -> bool,
{
    move |input: &'input str| {
        let mut i = 0;
        for c in input.chars() {
            if pred(c) {
                break;
            }
            i += 1;
        }
        Ok((&input[i..], &input[..i]))
    }
}

pub fn take_until<'input, 'b>(s: &'b str) -> impl Parser<'input, &'input str> + 'b {
    move |input: &'input str| match input.find(s) {
        Some(i) => Ok((&input[i..], &input[..i])),
        None => Err(ParseError::Missing {
            input,
            expected: s.to_owned(),
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::ParseError;

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

    #[test]
    fn test_tag_no_case() {
        let hello = |input| tag_no_case("Hello").parse(input);

        assert_eq!(hello("Hello, World!"), Ok((", World!", "Hello")));

        assert_eq!(hello("hello, World!"), Ok((", World!", "hello")));

        assert_eq!(hello("HeLlo, World!"), Ok((", World!", "HeLlo")));

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

    #[test]
    fn test_take() {
        let take6 = |input| take(6usize).parse(input);

        assert_eq!(take6("1234567"), Ok(("7", "123456")));

        assert_eq!(take6("things"), Ok(("", "things")));

        assert_eq!(
            take6("short"),
            Err(ParseError::EarlyEOF {
                input: "short",
                expected: "6 length string".to_owned()
            })
        );

        assert_eq!(
            take6(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "6 length string".to_owned()
            })
        );
    }

    #[test]
    fn test_take_while() {
        let alpha = |input| take_while(char::is_alphabetic).parse(input);

        assert_eq!(alpha("latin123"), Ok(("123", "latin")));

        assert_eq!(alpha("12345"), Ok(("12345", "")));

        assert_eq!(alpha("latin"), Ok(("", "latin")));

        assert_eq!(alpha(""), Ok(("", "")));
    }

    #[test]
    fn test_take_till() {
        let till_colon = |input| take_till(|c| c == ':').parse(input);

        assert_eq!(till_colon("latin:123"), Ok((":123", "latin")));

        assert_eq!(till_colon(":empty matched"), Ok((":empty matched", "")));

        assert_eq!(till_colon("12345"), Ok(("", "12345")));

        assert_eq!(till_colon(""), Ok(("", "")));
    }

    #[test]
    fn test_take_until() {
        let until_eof = |input| take_until("eof").parse(input);

        assert_eq!(until_eof("hello, worldeof"), Ok(("eof", "hello, world")));

        assert_eq!(
            until_eof("hello, world"),
            Err(ParseError::Missing {
                input: "hello, world",
                expected: "eof".to_owned()
            })
        );

        assert_eq!(
            until_eof(""),
            Err(ParseError::Missing {
                input: "",
                expected: "eof".to_owned()
            })
        );

        assert_eq!(until_eof("1eof2eof"), Ok(("eof2eof", "1")));
    }
}
