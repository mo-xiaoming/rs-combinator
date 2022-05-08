use crate::Parser;

pub fn delimited<'input, Output1, Output2, Output3>(
    parser1: impl Parser<'input, Output1>,
    parser2: impl Parser<'input, Output2>,
    parser3: impl Parser<'input, Output3>,
) -> impl Parser<'input, Output2> {
    move |input: &'input str| {
        parser1.parse(input).and_then(|(next_input, _)| {
            parser2.parse(next_input).and_then(|(last_input, m)| {
                parser3
                    .parse(last_input)
                    .map(|(final_input, _)| (final_input, m))
            })
        })
    }
}

pub fn preceded<'input, Output1, Output2>(
    parser1: impl Parser<'input, Output1>,
    parser2: impl Parser<'input, Output2>,
) -> impl Parser<'input, Output2> {
    move |input: &'input str| {
        parser1
            .parse(input)
            .and_then(|(next_input, _)| parser2.parse(next_input))
    }
}

pub fn terminated<'input, Output1, Output2>(
    parser1: impl Parser<'input, Output1>,
    parser2: impl Parser<'input, Output2>,
) -> impl Parser<'input, Output1> {
    move |input: &'input str| {
        parser1.parse(input).and_then(|(next_input, r1)| {
            parser2
                .parse(next_input)
                .map(|(last_input, _)| (last_input, r1))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{assert_eq_parse_error, chars::tag, Token};

    #[test]
    fn test_delimited() {
        let parser = |input| delimited(tag("("), tag("abc"), tag(")")).parse(input);

        assert_eq!(parser("(abc)"), Ok(("", Token::Tag("abc"))));

        assert_eq!(parser("(abc)def"), Ok(("def", Token::Tag("abc"))));

        let e = parser("").unwrap_err();
        assert_eq!(e.failed_at, Token::Tag(""),);
        assert!(e.expected_pattern.contains('('));
        assert_eq!(e.expected_length, Some(1));

        let e = parser("123").unwrap_err();
        assert_eq!(e.failed_at, Token::Tag("123"),);
        assert!(e.expected_pattern.contains('('));
        assert_eq!(e.expected_length, Some(1));
    }

    #[test]
    fn test_preceded() {
        let parser = |input| preceded(tag("abc"), tag("efg")).parse(input);

        assert_eq!(parser("abcefg"), Ok(("", Token::Tag("efg"))));

        assert_eq!(parser("abcefghij"), Ok(("hij", Token::Tag("efg"))));

        assert_eq_parse_error("", parser, Token::Tag, Some(3));

        assert_eq_parse_error("123", parser, Token::Tag, Some(3));
    }

    #[test]
    fn test_terminated() {
        let parser = |input| terminated(tag("abc"), tag("efg")).parse(input);

        assert_eq!(parser("abcefg"), Ok(("", Token::Tag("abc"))));

        assert_eq!(parser("abcefghij"), Ok(("hij", Token::Tag("abc"))));

        assert_eq_parse_error("", parser, Token::Tag, Some(3));

        assert_eq_parse_error("123", parser, Token::Tag, Some(3));
    }
}
