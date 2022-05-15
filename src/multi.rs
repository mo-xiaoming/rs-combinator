use crate::Parser;

pub fn count<'input, Output, P>(parser: P, n: usize) -> impl Parser<'input, Vec<Output>>
where
    P: Parser<'input, Output>,
{
    move |input: &'input str| {
        let mut v = Vec::with_capacity(n);
        let mut cur_input = input;

        for _ in 0..n {
            let (next_input, r) = parser.parse(cur_input)?;
            cur_input = next_input;
            v.push(r);
        }
        Ok((cur_input, v))
    }
}

pub fn many0<'input, Output, P>(parser: P) -> impl Parser<'input, Vec<Output>>
where
    P: Parser<'input, Output>,
{
    move |input: &'input str| {
        let mut v = Vec::with_capacity(4);
        let mut cur_input = input;

        loop {
            match parser.parse(cur_input) {
                Ok((next_input, r)) => {
                    if cur_input.len() == next_input.len() {
                        panic!("parsers accept empty inputs cannot be in many0");
                    }
                    cur_input = next_input;
                    v.push(r);
                }
                Err(_) => return Ok((cur_input, v)),
            }
        }
    }
}

pub fn many1<'input, Output, P>(parser: P) -> impl Parser<'input, Vec<Output>>
where
    P: Parser<'input, Output>,
{
    move |input: &'input str| {
        let mut v = Vec::with_capacity(4);
        let mut cur_input = input;

        loop {
            match parser.parse(cur_input) {
                Ok((next_input, r)) => {
                    if cur_input.len() == next_input.len() {
                        panic!("parsers accept empty inputs cannot be in many1");
                    }
                    cur_input = next_input;
                    v.push(r);
                }
                Err(e) => {
                    if v.is_empty() {
                        return Err(e);
                    }
                    return Ok((cur_input, v));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        chars::tag,
        tests::{assert_eq_parse_error_single, SingleError},
        Token,
    };

    #[test]
    fn test_count() {
        let parser = |input| count(tag("abc"), 2).parse(input);

        assert_eq!(
            parser("abcabc"),
            Ok(("", vec![Token::Tag("abc"), Token::Tag("abc")]))
        );

        assert_eq!(
            parser("abcabcabc"),
            Ok(("abc", vec![Token::Tag("abc"), Token::Tag("abc")]))
        );

        let se = SingleError {
            token_ctor: Token::Tag,
            expected_length: Some(3),
            expected_pattern_contains: Some("abc"),
        };

        assert_eq_parse_error_single("", parser, &se);

        assert_eq_parse_error_single("123123", parser, &se);

        let se = SingleError {
            token_ctor: Token::Tag("123"),
            expected_length: Some(3),
            expected_pattern_contains: Some("abc"),
        };

        assert_eq_parse_error_single("abc123", parser, &se);
    }

    #[test]
    fn test_many0() {
        let parser = |input| many0(tag("abc")).parse(input);

        assert_eq!(
            parser("abcabc"),
            Ok(("", vec![Token::Tag("abc"), Token::Tag("abc")]))
        );

        assert_eq!(parser("abc123"), Ok(("123", vec![Token::Tag("abc")])));

        assert_eq!(parser("123123"), Ok(("123123", vec![])));

        assert_eq!(parser(""), Ok(("", vec![])));
    }

    #[test]
    #[should_panic(expected = "empty inputs")]
    fn test_many0_panic_with_xxx0() {
        let _ = many0(tag("")).parse("abc");
    }

    #[test]
    fn test_many1() {
        let parser = |input| many1(tag("abc")).parse(input);

        assert_eq!(
            parser("abcabc"),
            Ok(("", vec![Token::Tag("abc"), Token::Tag("abc")]))
        );

        assert_eq!(parser("abc123"), Ok(("123", vec![Token::Tag("abc")])));

        let se = SingleError {
            token_ctor: Token::Tag,
            expected_length: Some(3),
            expected_pattern_contains: Some("abc"),
        };

        assert_eq_parse_error_single("123123", parser, &se);

        assert_eq_parse_error_single("", parser, &se);
    }

    #[test]
    #[should_panic(expected = "empty inputs")]
    fn test_many1_panic_with_xxx0() {
        let _ = many1(tag("")).parse("abc");
    }
}
