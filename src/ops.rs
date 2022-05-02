pub mod add;

pub use add::add_op;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(7, add_op(3, 4));
    }
}