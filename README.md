# This a combinator lib written in rust

- For learning rust and combinator
- Going to have a similar interface like [nom](https://github.com/Geal/nom)`::complete`
- **Don't** care about performance

## Examples

```rust
use rs_combinator::{chars::take_until, ParseError, Parser};

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
```