use crate::{parser::*};


pub enum JsonAST {

}

pub enum JsonNumber {
    Integer(i64),
    Real(f64)
}


pub fn parse_json(input: &str) -> Option<JsonAST> {
    let ws = many0(char1(' ').or_else(char1('\t')).or_else(char1('\n')).or_else(char1('\r')));
    let parse_true = string("true");
    let parse_false = string("false");
    let parse_null = string("null");

    let one_nine = char_range('1'..='9');
    let digit = char_range('0'..='9');
    let digits = recognize(many1(digit.clone()));
    let zero = left(
        char1('0'),
        not(digit.clone())
    ).map(|c| c.to_string());

    let number_part = zero.or_else(
        pair(
            one_nine.clone(),
            many0(digit.clone())
        )
        .map(|(c, cs)|{
            vec![c.to_string(), cs.iter().collect::<String>()].concat()
        })
    );

    let minus = option(char1('-')).map(|c| c.map(|c|c.to_string()).unwrap_or_default());
    let integer = pair(minus, number_part)
        .map(|(s1, s2)| vec![s1, s2].concat().parse::<i64>().unwrap());    

    let sign = option(char1('+').or_else(char1('-')))
        .map(|oc|oc.map(|c| c.to_string()).unwrap_or_default());
    let exponent = option(
            pair(
                char1('e').or_else(char1('E')),
                pair(
                    sign.clone(),
                    digits.clone()
                )
                .map(|(s, d)| vec![s, d].concat())
            )
            .map(|(c, s)|vec![c.to_string(), s].concat()),
        )
        .map(|oc|oc.map(|c| c.to_string()).unwrap_or_default());
    let fraction = option(
        pair(
            char1('.'),
            digits.clone()
        )
        .map(|(c, s)|vec![c.to_string(), s].concat())
    ).map(|oc|oc.map(|c| c.to_string()).unwrap_or_default());

    
    todo!()
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_integer() {
        let one_nine = char_range('1'..='9');
        let digit = char_range('0'..='9');
        let zero = left(
            char1('0'),
            not(digit.clone())
        ).map(|c| c.to_string());

        let number_part = zero.or_else(
            pair(
                one_nine.clone(),
                many0(digit.clone())
            )
            .map(|(c, cs)|{
                vec![c.to_string(), cs.iter().collect::<String>()].concat()
            })
        );

        let sign = option(char1('-')).map(|c| c.map(|c|c.to_string()).unwrap_or_default());
        let integer = pair(sign, number_part)
            .map(|(s1, s2)| vec![s1, s2].concat().parse::<i64>().unwrap());

        assert_eq!(integer.run("123  "), Some((123, "  ")));
        assert_eq!(integer.run("0    "), Some((0, "    ")));
        assert_eq!(integer.run("9    "), Some((9, "    ")));
        assert_eq!(integer.run("-123 "), Some((-123, " ")));
        assert_eq!(integer.run("-8   "), Some((-8, "   ")));
        assert_eq!(integer.run("012  "), None);
        assert_eq!(integer.run("-012 "), None);
    }
}