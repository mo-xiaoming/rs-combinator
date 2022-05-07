use crate::{ParseError, Parser};

pub fn delimited<'input, P1, P2, P3, Output1, Output2, Output3>(
    parser1: P1,
    parser2: P2,
    parser3: P3,
) -> impl Parser<'input, Output2>
where
    P1: Parser<'input, Output1>,
    P2: Parser<'input, Output2>,
    P3: Parser<'input, Output3>,
{
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{chars::tag, ParseError};

    #[test]
    fn test_delimited() {
        let parser = |input| delimited(tag("("), tag("abc"), tag(")")).parse(input);

        assert_eq!(parser("(abc)"), Ok(("", "abc")));

        assert_eq!(parser("(abc)def"), Ok(("def", "abc")));

        assert_eq!(
            parser(""),
            Err(ParseError::EarlyEOF {
                input: "",
                expected: "(".to_owned()
            })
        );

        assert_eq!(
            parser("123"),
            Err(ParseError::Unexpected {
                input: "123",
                expected: "(".to_owned(),
                got: "1"
            })
        );
    }
}
