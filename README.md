# This a combinator lib written in rust

- For learning rust and combinator
- Going to have a similar interface like [nom](https://github.com/Geal/nom)`::complete`
- **Don't** care about performance

## Examples

```rust
use rs_combinator::{chars::take_until, Token, Parser};

let until_eof = |input| take_until("eof").parse(input);

assert_eq!(until_eof("hello, worldeof"), Ok(("eof", Token::TakeUntil("hello, world"))));

assert_eq!(
    until_eof("hello, world").unwrap_err().failed_at,
    Token::TakeUntil("hello, world"),
);

assert_eq!(until_eof("").unwrap_err().failed_at, Token::TakeUntil(""));

assert_eq!(until_eof("1eof2eof"), Ok(("eof2eof", Token::TakeUntil("1"))));
```