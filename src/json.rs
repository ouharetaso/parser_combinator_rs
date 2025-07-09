use std::collections::HashMap;
use parser_combinator::parser::*;


#[derive(Clone, Debug, PartialEq)]
pub enum JsonValue{
    Null,
    Bool(bool),
    Number(JsonNumber),
    String(String),
    Array(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>)
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum JsonNumber {
    Integer(i64),
    Float(f64)
}


fn parse_number(input: &str) -> Option<(JsonValue, &str)> {
    let digit = char_range('0'..='9');
    let digits = many1(digit.clone());
    let one_nine = char_range('1'..='9');
    let zero = char1('0');

    let integer = pair(
        option(char1('-')),
        one_nine.clone().and_then(|_| many0(digit.clone()).map(|_|()))
            .or_else(pair(
                zero.clone(),
                not(digit.clone())
            )
            .map(|_|())
        )
    );

    let fraction = option(pair(char1('.'), digits.clone()));
    let exponent = option(pair(
        char1('e').or_else(char1('E')),
        pair(
            option(char1('+').or_else(char1('-'))),
            digits.clone()
        )
    ));

    let number = recognize(
        pair(
            integer,
            pair(
                fraction,
                exponent
            )
        )
    )
    .map(|s|
        if let Ok(i) = s.parse::<i64>() {
            JsonValue::Number(JsonNumber::Integer(i))
        }
        else if let Ok(f) = s.parse::<f64>() {
            JsonValue::Number(JsonNumber::Float(f))
        }
        else {
            unreachable!()
        }
    );

    number.run(input)
}


fn parse_string(input: &str) -> Option<(String, &str)> {
    let hex = char_range('0'..='9')
        .or_else(char_range('a'..='f'))
        .or_else(char_range('A'..='F'));
    let escape = char1('"')
        .or_else(char1('\\'))
        .or_else(char1('/'))
        .or_else(char1('b').map(|_|'\x08'))
        .or_else(char1('f').map(|_|'\x0c'))
        .or_else(char1('n').map(|_|'\n'))
        .or_else(char1('r').map(|_|'\r'))
        .or_else(char1('t').map(|_|'\t'))
        .or_else(
            right(
            char1('u'),
            recognize(repeat_exact(4, hex))
            )
            .map(|s| char::from_u32(u32::from_str_radix(&s, 16).unwrap()).unwrap())
        );
    
    let character = not(char1('"').or_else(char1('\\')))
        .and_then(|_| char_range('\x20'..='\u{10FFFF}'))
        .or_else(right(char1('\\'), escape));
    let characters = many0(character)
        .map(|cs|
            cs.iter().collect::<String>()
        );
    right(
        char1('"'),
        left(
            characters,
            char1('"')
        )
    ).run(input)
}

fn parse_element(input: &str) -> Option<(JsonValue, &str)> {
    let ws = many0(char1('\x20').or_else(char1('\x0a')).or_else(char1('\x0d')).or_else(char1('\x09')));

    let element = right(
        ws.clone(),
        left(
            parse_value,
            ws.clone()
        )
    );
    element.run(input)
}

fn parse_object(input: &str) -> Option<(JsonValue, &str)> {
    let ws = many0(char1('\x20').or_else(char1('\x0a')).or_else(char1('\x0d')).or_else(char1('\x09')));

    let element = parse_element;
    let member = 
    pair(
        right(
            ws.clone(),
            left(
                parse_string,
                ws.clone().and_then(|_| char1(':'))
            )
        ),
        element.clone()
    );

    let members = pair(
        member.clone(),
        many0(
            right(
                char1(','),
                member.clone()
            )
        )
    ).map(|(m, v)|{
        let mut result = vec![m];
        result.extend(v);
        result
    });
    
    
    let object = right(
        char1('{'),
        left(
            members
            .or_else(
                ws.clone().map(|_| vec![])
            ),
            char1('}')
        )
    );

    object.map(|v| JsonValue::Object(v.into_iter().collect())).run(input)
}

fn parse_array(input: &str) -> Option<(JsonValue, &str)> {
    let ws = many0(char1('\x20').or_else(char1('\x0a')).or_else(char1('\x0d')).or_else(char1('\x09')));

    let elements = pair(
        parse_element,
        many0(
            right(
                char1(','),
                parse_element
            )
        )
    ).map(|(e, v)|{
        let mut result = vec![e];
        result.extend(v);
        result
    });

    let array = right(
        char1('['),
        left(
            elements.or_else(ws.map(|_|vec![])),
            char1(']')
        )
    );

    array.map(|v|JsonValue::Array(v)).run(input)
}

fn parse_bool(input: &str) -> Option<(JsonValue, &str)> {
    let parse_false = string("false").map(|_| JsonValue::Bool(false));
    let parse_true = string("true").map(|_| JsonValue::Bool(true));
    
    parse_false.or_else(parse_true).run(input)
}

fn parse_null(input: &str) -> Option<(JsonValue, &str)> {
    string("null").map(|_| JsonValue::Null).run(input)
}

fn parse_value(input: &str) -> Option<(JsonValue, &str)> {
    parse_object
    .or_else(parse_array)
    .or_else(parse_string.map(|s|JsonValue::String(s)))
    .or_else(parse_number)
    .or_else(parse_bool)
    .or_else(parse_null)
    .run(input)
}

pub fn parse_json(input: &str) -> Option<JsonValue> {
    match parse_element.run(input) {
        Some((json_value, rest)) => {
            if rest.is_empty() {
                Some(json_value)
            }
            else {
                None
            }
        }
        None => None
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        assert_eq!(parse_number("0"),           Some((JsonValue::Number(JsonNumber::Integer(0)), "")));
        assert_eq!(parse_number("1234"),        Some((JsonValue::Number(JsonNumber::Integer(1234)), "")));

        assert_eq!(parse_number("56.7"),        Some((JsonValue::Number(JsonNumber::Float(56.7)), "")));
        assert_eq!(parse_number("12e8"),        Some((JsonValue::Number(JsonNumber::Float(12e8)), "")));
        assert_eq!(parse_number("51e+2"),       Some((JsonValue::Number(JsonNumber::Float(51e+2)), "")));
        assert_eq!(parse_number("102e-4"),      Some((JsonValue::Number(JsonNumber::Float(102e-4)), "")));
        assert_eq!(parse_number("16.38e4"),     Some((JsonValue::Number(JsonNumber::Float(16.38e4)), "")));
        assert_eq!(parse_number("32.76e+8"),    Some((JsonValue::Number(JsonNumber::Float(32.76e+8)), "")));
        assert_eq!(parse_number("65.53e-6"),    Some((JsonValue::Number(JsonNumber::Float(65.53e-6)), "")));
        
        assert_eq!(parse_number("-0"),          Some((JsonValue::Number(JsonNumber::Integer(-0)), "")));
        assert_eq!(parse_number("-1234"),       Some((JsonValue::Number(JsonNumber::Integer(-1234)), "")));

        assert_eq!(parse_number("-56.7"),       Some((JsonValue::Number(JsonNumber::Float(-56.7)), "")));
        assert_eq!(parse_number("-12e8"),       Some((JsonValue::Number(JsonNumber::Float(-12e8)), "")));
        assert_eq!(parse_number("-51e+2"),      Some((JsonValue::Number(JsonNumber::Float(-51e+2)), "")));
        assert_eq!(parse_number("-102e-4"),     Some((JsonValue::Number(JsonNumber::Float(-102e-4)), "")));
        assert_eq!(parse_number("-16.38e4"),    Some((JsonValue::Number(JsonNumber::Float(-16.38e4)), "")));
        assert_eq!(parse_number("-32.76e+8"),   Some((JsonValue::Number(JsonNumber::Float(-32.76e+8)), "")));
        assert_eq!(parse_number("-65.53e-6"),   Some((JsonValue::Number(JsonNumber::Float(-65.53e-6)), "")));
    

        assert_eq!(parse_number("abc"), None);
        assert_eq!(parse_number("01"), None);
        assert_eq!(parse_number("+42"), None);
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(parse_string(r#""string literal""#), Some(("string literal".to_string(), "")));
        assert_eq!(parse_string(r#""""#), Some(("".to_string(), "")));
        assert_eq!(parse_string(r#""\\\/\b\f\n\r\t\u3042""#), Some(("\\/\x08\x0c\n\r\tあ".to_string(), "")));

        assert_eq!(parse_string(r#""not closed"#), None);
        assert_eq!(parse_string("\"unescaped newline\n\""), None);
        assert_eq!(parse_string(r#"invalid escape \q"#), None);
        assert_eq!(parse_string(r#"incomplete unicode escape \u123"#), None);

    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse_bool("true"), Some((JsonValue::Bool(true), "")));
        assert_eq!(parse_bool("false"), Some((JsonValue::Bool(false), "")));

        assert_eq!(parse_bool("True"), None);
        assert_eq!(parse_bool("False"), None);
    }

    #[test]
    fn test_parse_null() {
        assert_eq!(parse_null("null"), Some((JsonValue::Null, "")));
        assert_eq!(parse_null("Null"), None);
    }

    #[test]
    fn test_parse_object() {
        assert_eq!(parse_object("{}"), Some((JsonValue::Object(HashMap::new()), "")));
        assert_eq!(parse_object("{    }"), Some((JsonValue::Object(HashMap::new()), "")));

        assert_eq!(
            parse_object(
            r#"{
                "key" : "value"
            }"#),
            Some((
                JsonValue::Object(
                    vec![
                        (
                            "key".to_string(),
                            JsonValue::String("value".to_string())
                        )
                    ]
                    .into_iter()
                    .collect()
                ),
                ""
            ))
        );
        
        assert_eq!(
            parse_object(
            r#"{
                "a":
                {
                    "b": true
                },
                "c":
                [
                    null
                ]
            }"#),
            Some((
                JsonValue::Object(
                    vec![
                        (
                            "a".to_string(),
                            JsonValue::Object(
                                vec![
                                    (
                                        "b".to_string(),
                                        JsonValue::Bool(true)
                                    )
                                ]
                                .into_iter()
                                .collect()
                            )
                        ),
                        (
                            "c".to_string(),
                            JsonValue::Array(vec![
                                JsonValue::Null
                            ])
                        )
                    ]
                    .into_iter()
                    .collect()
                ),
                ""
            ))
        );

        assert_eq!(parse_object(r#"{"key": "value", }"#), None);
        assert_eq!(parse_object(r#"{key: "value" }"#), None);
        assert_eq!(parse_object(r#"{"key": }"#), None);
        assert_eq!(parse_object(r#"{"key" "value"}"#), None);
        assert_eq!(parse_object(r#"{"key": "value""#), None);
    }

    #[test]
    fn test_parse_json() {
        assert_eq!(
            parse_json(r#"{}"#),
            Some(
                JsonValue::Object(
                    HashMap::new()
                )
            )
        );

        assert_eq!(
            parse_json(r#"{"name": "ami"}"#),
            Some(
                JsonValue::Object(
                    vec![
                        (
                            "name".to_string(),
                            JsonValue::String("ami".to_string())
                        )
                    ]
                    .into_iter()
                    .collect()
                )
            )
        );

        assert_eq!(
            parse_json(r#"{"name": "ami", "age": 21}"#),
            Some(
                JsonValue::Object(
                    vec![
                        (
                            "name".to_string(),
                            JsonValue::String("ami".to_string())
                        ),
                        (
                            "age".to_string(),
                            JsonValue::Number(JsonNumber::Integer(21))
                        )
                    ]
                    .into_iter()
                    .collect()
                )
            )
        );

        assert_eq!(
            parse_json(r#"[
                {
                    "name": "ami",
                    "age": 21
                },
                {
                    "name": "puni",
                    "age": 7
                },
                {
                    "name": "piyo",
                    "age": 9
                }
            ]"#),
            Some(
                JsonValue::Array(vec![
                    JsonValue::Object(
                        vec![
                            (
                                "name".to_string(),
                                JsonValue::String("ami".to_string())
                            ),
                            (
                                "age".to_string(),
                                JsonValue::Number(JsonNumber::Integer(21))
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                    JsonValue::Object(
                        vec![
                            (
                                "name".to_string(),
                                JsonValue::String("puni".to_string())
                            ),
                            (
                                "age".to_string(),
                                JsonValue::Number(JsonNumber::Integer(7))
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                    JsonValue::Object(
                        vec![
                            (
                                "name".to_string(),
                                JsonValue::String("piyo".to_string())
                            ),
                            (
                                "age".to_string(),
                                JsonValue::Number(JsonNumber::Integer(9))
                            )
                        ]
                        .into_iter()
                        .collect()
                    ),
                ])
            )
        )
    }
}