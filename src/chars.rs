use crate::{ParseError, Parser, Token};

struct BreakAt<'input> {
    first: &'input str,
    second: &'input str,
}

fn break_at<P>(input: &str, pred: P) -> BreakAt
where
    P: FnMut(char) -> bool,
{
    match input.chars().position(pred) {
        Some(n) => {
            let (a, b) = input.split_at(n);
            BreakAt {
                first: a,
                second: b,
            }
        }
        None => BreakAt {
            first: input,
            second: "",
        },
    }
}

fn match_many1_chars<'input>(
    break_at_pred: impl Fn(char) -> bool + Copy,
    token_ctor: impl Fn(&'input str) -> Token,
    err_msg_fn: impl Fn() -> String,
) -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| {
        let BreakAt { first, second } = break_at(input, break_at_pred);
        if first.is_empty() {
            Err(ParseError::new(token_ctor(input), err_msg_fn(), None))
        } else {
            Ok((second, token_ctor(first)))
        }
    }
}

pub fn match_many0_chars<'input>(
    break_at_pred: impl Fn(char) -> bool + Copy,
    token_ctor: impl Fn(&'input str) -> Token,
) -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| {
        let BreakAt { first, second } = break_at(input, break_at_pred);
        Ok((second, token_ctor(first)))
    }
}

fn match_first_char<'input>(
    break_at_pred: impl Fn(char) -> bool + Copy,
    token_ctor: impl Fn(&'input str) -> Token,
    err_msg_fn: impl Fn() -> String,
) -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| match input.chars().next() {
        Some(m) if break_at_pred(m) => {
            Ok((&input[m.len_utf8()..], token_ctor(&input[..m.len_utf8()])))
        }
        _ => Err(ParseError::new(token_ctor(input), err_msg_fn(), Some(1))),
    }
}

fn match_char_sequence<'input, 'b>(
    seq: &'b str,
    cmp: impl Fn(&str, &str) -> bool + Copy + 'b,
    token_ctor: impl Fn(&'input str) -> Token + Copy + 'b,
) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| match input.get(..seq.len()) {
        Some(m) if cmp(m, seq) => Ok((&input[m.len()..], token_ctor(&input[..m.len()]))),
        _ => Err(ParseError::new(
            token_ctor(input),
            seq.to_owned(),
            Some(seq.len()),
        )),
    }
}

pub fn char<'input>(c: char) -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| {
        match_first_char(|m| c == m, Token::Char, || c.to_string()).parse(input)
    }
}
pub fn is_a<'input, 'b>(chars_set: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| {
        match_many1_chars(
            |c| !chars_set.contains(c),
            Token::IsA,
            || format!("chars to be any of \"{chars_set}\""),
        )
        .parse(input)
    }
}

pub fn is_not<'input, 'b>(chars_set: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| {
        match_many1_chars(
            |c| chars_set.contains(c),
            Token::IsNot,
            || format!("chars to be none of \"{chars_set}\""),
        )
        .parse(input)
    }
}

pub fn one_of<'input, 'b>(chars_set: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| {
        match_first_char(
            |c| chars_set.contains(c),
            Token::OneOf,
            || format!("char to be one of \"{chars_set}\""),
        )
        .parse(input)
    }
}

pub fn none_of<'input, 'b>(chars_set: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| {
        match_first_char(
            |c| !chars_set.contains(c),
            Token::NoneOf,
            || format!("char to be none of \"{chars_set}\""),
        )
        .parse(input)
    }
}

pub fn tag<'input, 'b>(seq: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| match_char_sequence(seq, str::eq, Token::Tag).parse(input)
}

pub fn tag_no_case<'input, 'b>(seq: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| {
        match_char_sequence(
            seq,
            |a, b| a.to_lowercase() == b.to_ascii_lowercase(),
            Token::TagNoCase,
        )
        .parse(input)
    }
}

pub fn take<'input>(n: usize) -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| match input.get(..n) {
        Some(m) => Ok((&input[n..], Token::Take(m))),
        _ => Err(ParseError::new(Token::Take(input), n.to_string(), Some(n))),
    }
}

pub fn take_while<'input, P>(pred: P) -> impl Parser<'input, Token<'input>>
where
    P: Fn(char) -> bool + Copy,
{
    move |input: &'input str| match_many0_chars(|c| !pred(c), Token::TakeWhile).parse(input)
}

pub fn take_till<'input, P>(pred: P) -> impl Parser<'input, Token<'input>>
where
    P: Fn(char) -> bool + Copy,
{
    move |input: &'input str| match_many0_chars(pred, Token::TakeTill).parse(input)
}

pub fn take_until<'input, 'b>(boundary_seq: &'b str) -> impl Parser<'input, Token<'input>> + 'b {
    move |input: &'input str| match input.find(boundary_seq) {
        Some(i) => Ok((&input[i..], Token::TakeUntil(&input[..i]))),
        None => Err(ParseError::new(Token::TakeUntil(input), boundary_seq.to_owned(), None)),
    }
}

pub fn alpha1<'input>() -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| {
        match_many1_chars(
            |c: char| !c.is_ascii_alphabetic(),
            Token::Alpha1,
            || "any letters".to_owned(),
        ).parse(input)
    }
}

pub fn digit1<'input>() -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| {
        match_many1_chars(
            |c: char| !c.is_ascii_digit(),
            Token::Digit1,
            || "any digits".to_owned(),
        )
        .parse(input)
    }
}

pub fn anychar<'input>() -> impl Parser<'input, Token<'input>> {
    move |input: &'input str| match input.chars().next() {
        Some(m) => Ok((
            &input[m.len_utf8()..],
            Token::AnyChar(&input[..m.len_utf8()]),
        )),
        None => Err(ParseError::new(
            Token::AnyChar(input),
            "a char".to_string(),
            Some(1),
        )),
    }
}

#[cfg(test)]
mod tests {
    use crate::assert_eq_parse_error;

    use super::*;

    #[test]
    fn test_char() {
        let a = |input| char('a').parse(input);

        assert_eq!(a("abc"), Ok(("bc", Token::Char("a"))));

        assert_eq_parse_error(" abc", a, Token::Char, Some(1));

        assert_eq_parse_error("bc", a, Token::Char, Some(1));
    }

    #[test]
    fn test_is_a() {
        let hex = |input| is_a("1234567890ABCDEF").parse(input);

        assert_eq!(hex("123 and voila"), Ok((" and voila", Token::IsA("123"))));

        assert_eq!(
            hex("DEADBEEF and others"),
            Ok((" and others", Token::IsA("DEADBEEF")))
        );

        assert_eq!(
            hex("BADBABEsomething"),
            Ok(("something", Token::IsA("BADBABE")))
        );

        assert_eq!(hex("D15EA5E"), Ok(("", Token::IsA("D15EA5E"))));

        assert_eq_parse_error("xD", hex, Token::IsA, None);

        assert_eq_parse_error("", hex, Token::IsA, None);
    }

    #[test]
    fn test_is_not() {
        let not_space = |input| is_not(" \t\r\n").parse(input);

        assert_eq!(
            not_space("Hello, World!"),
            Ok((" World!", Token::IsNot("Hello,")))
        );

        assert_eq!(
            not_space("Sometimes\t"),
            Ok(("\t", Token::IsNot("Sometimes")))
        );

        assert_eq!(not_space("Nospace"), Ok(("", Token::IsNot("Nospace"))));

        assert_eq_parse_error(" N", not_space, Token::IsNot, None);

        assert_eq_parse_error("", not_space, Token::IsNot, None);
    }

    #[test]
    fn test_one_of() {
        assert_eq!(one_of("abc").parse("b"), Ok(("", Token::OneOf("b"))));

        assert_eq_parse_error("bc", |s| one_of("a").parse(s), Token::OneOf, Some(1));

        assert_eq_parse_error("", |s| one_of("a").parse(s), Token::OneOf, Some(1));
    }

    #[test]
    fn test_none_of() {
        assert_eq!(none_of("abc").parse("z"), Ok(("", Token::NoneOf("z"))));

        assert_eq_parse_error("a", |s| none_of("ab").parse(s), Token::NoneOf, Some(1));

        assert_eq_parse_error("", |s| none_of("ab").parse(s), Token::NoneOf, Some(1));
    }

    #[test]
    fn test_tag() {
        let hello = |input| tag("Hello").parse(input);

        assert_eq!(
            hello("Hello, World!"),
            Ok((", World!", Token::Tag("Hello")))
        );

        assert_eq!(
            hello("Something").unwrap_err().failed_at,
            Token::Tag("Something")
        );

        assert_eq_parse_error("less", hello, Token::Tag, Some(5));

        assert_eq_parse_error("Hell", hello, Token::Tag, Some(5));

        assert_eq_parse_error("", hello, Token::Tag, Some(5));
    }

    #[test]
    fn test_tag_no_case() {
        let hello = |input| tag_no_case("Hello").parse(input);

        assert_eq!(
            hello("Hello, World!"),
            Ok((", World!", Token::TagNoCase("Hello")))
        );

        assert_eq!(
            hello("hello, World!"),
            Ok((", World!", Token::TagNoCase("hello")))
        );

        assert_eq!(
            hello("HeLlo, World!"),
            Ok((", World!", Token::TagNoCase("HeLlo")))
        );

        assert_eq_parse_error("Something", hello, Token::TagNoCase, Some(5));

        assert_eq_parse_error("less", hello, Token::TagNoCase, Some(5));

        assert_eq_parse_error("Hell", hello, Token::TagNoCase, Some(5));

        assert_eq_parse_error("", hello, Token::TagNoCase, Some(5));
    }

    #[test]
    fn test_take() {
        let take6 = |input| take(6usize).parse(input);

        assert_eq!(take6("1234567"), Ok(("7", Token::Take("123456"))));

        assert_eq!(take6("things"), Ok(("", Token::Take("things"))));

        assert_eq_parse_error("short", take6, Token::Take, Some(6));

        assert_eq_parse_error("", take6, Token::Take, Some(6));
    }

    #[test]
    fn test_take_while() {
        let alpha = |input| take_while(char::is_alphabetic).parse(input);

        assert_eq!(alpha("latin123"), Ok(("123", Token::TakeWhile("latin"))));

        assert_eq!(alpha("12345"), Ok(("12345", Token::TakeWhile(""))));

        assert_eq!(alpha("latin"), Ok(("", Token::TakeWhile("latin"))));

        assert_eq!(alpha(""), Ok(("", Token::TakeWhile(""))));
    }

    #[test]
    fn test_take_till() {
        let till_colon = |input| take_till(|c| c == ':').parse(input);

        assert_eq!(
            till_colon("latin:123"),
            Ok((":123", Token::TakeTill("latin")))
        );

        assert_eq!(
            till_colon(":empty matched"),
            Ok((":empty matched", Token::TakeTill("")))
        );

        assert_eq!(till_colon("12345"), Ok(("", Token::TakeTill("12345"))));

        assert_eq!(till_colon(""), Ok(("", Token::TakeTill(""))));
    }

    #[test]
    fn test_take_until() {
        let until_eof = |input| take_until("eof").parse(input);

        assert_eq!(
            until_eof("hello, worldeof"),
            Ok(("eof", Token::TakeUntil("hello, world")))
        );

        assert_eq_parse_error("hello, world", until_eof, Token::TakeUntil, None);

        assert_eq_parse_error("", until_eof, Token::TakeUntil, None);

        assert_eq!(
            until_eof("1eof2eof"),
            Ok(("eof2eof", Token::TakeUntil("1")))
        );
    }

    #[test]
    fn test_alpha1() {
        assert_eq!(alpha1().parse("aB1c"), Ok(("1c", Token::Alpha1("aB"))));

        assert_eq_parse_error("1c", |s| alpha1().parse(s), Token::Alpha1, None);

        assert_eq_parse_error("", |s| alpha1().parse(s), Token::Alpha1, None);
    }

    #[test]
    fn test_digit1() {
        assert_eq!(digit1().parse("21c"), Ok(("c", Token::Digit1("21"))));

        assert_eq_parse_error("c1", |s| digit1().parse(s), Token::Digit1, None);

        assert_eq_parse_error("", |s| digit1().parse(s), Token::Digit1, None);
    }

    #[test]
    fn test_anychar() {
        assert_eq!(anychar().parse("abc"), Ok(("bc", Token::AnyChar("a"))));

        assert_eq_parse_error("", |s| anychar().parse(s), Token::AnyChar, Some(1));
    }
}
