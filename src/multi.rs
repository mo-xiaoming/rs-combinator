use crate::{Parser, ParseError};

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
        let mut v = Vec::new();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assert_eq_parse_error, chars::tag, Token};

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

        assert_eq_parse_error("", parser, Token::Tag, Some(3));

        let e = parser("123123").unwrap_err();
        assert_eq!(e.failed_at, Token::Tag("123123"));
        assert_eq!(e.expected_pattern, "abc".to_owned());
        assert_eq!(e.expected_length, Some(3));

        let e = parser("abc123").unwrap_err();
        assert_eq!(e.failed_at, Token::Tag("123"));
        assert_eq!(e.expected_pattern, "abc".to_owned());
        assert_eq!(e.expected_length, Some(3));
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
        many0(tag("")).parse("abc");
    }
}
