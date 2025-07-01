



#[macro_export(local_inner_macros)]
macro_rules! flatten_tuple {
    ($head: ident, ($($tail: ident),*)) => {
        ($head, $($tail),*)
    };
    ($head: ident, $tail: ident) => {
        ($head, $tail)
    };
}

#[macro_export]
macro_rules! tuple {
    ($head: expr) => {
        $head
    };
    ($head: expr, $($tail: expr),+ $(,)?) => {
        {
            use $crate::parser::pair;

            pair(
                $head,
                tuple!($($tail), +)
            )
            .map(|(h, t)|{
                $crate::flatten_tuple!(h, t)
            })
        }
    };
}



#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn test_tuple_macro() {
        let number = recognize(
            tuple!(
                char_range('1'..='9').or_else(left(char1('0'), not(char_range('0'..='9')))),
                option(many0(char_range('0'..='9')))
            )
        )
        .map(|s| s.parse::<i64>().unwrap());

        assert_eq!(number.run("123"), Some((123, "")));
        assert_eq!(number.run("012"), None);
    }
}