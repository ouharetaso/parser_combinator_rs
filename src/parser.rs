pub trait Parser: Clone{
    type Output;

    fn run<'i>(&self, input: &'i str) -> Option<(Self::Output, &'i str)>;

    fn map<B, F>(self, f: F) -> impl Parser<Output = B>
    where
        Self: Sized + Clone,
        F: Fn(Self::Output) -> B + Clone
    {
        p_fn(move|input| {
            match self.run(input) {
                Some((a, rest)) => Some((f(a), rest)),
                None => None
            }
        })
    }

    fn and_then<B, P, F>(self, f: F) -> impl Parser<Output = B>
    where
        Self: Sized + Clone,
        P: Parser<Output = B>,
        F: Fn(Self::Output) -> P + Clone
    {
        let parser = self.clone();
        p_fn(move|input| {
            match parser.run(input) {
                Some((a, rest)) => f(a).run(rest),
                None => None
            }
        })
    }

    fn or_else<P>(self, p: P) -> impl Parser<Output = Self::Output>
    where
        Self: Sized + Clone,
        P: Parser<Output = Self::Output>
    {
        let parser = p.clone();
        let _self = self.clone();
        p_fn(move|input| {
            match _self.run(input) {
                res @ Some(_) => res,
                None => parser.run(input)
            }
        })
    }
}

fn p_fn<F, A>(f: F) -> FnParser<F, A>
where
    for<'i> F: Fn(&'i str) -> Option<(A, &'i str)> + Clone
{
    FnParser{
        f,
        _phantom: std::marker::PhantomData
    }
}

impl<A, F> Parser for F
where
    F: for<'i> Fn(&'i str) -> Option<(A, &'i str)> + Clone
{
    type Output = A;
    fn run<'i>(&self, input: &'i str) -> Option<(Self::Output, &'i str)> {
        (self)(input)
    }
}

#[derive(Copy)]
struct FnParser<F, A>
where
    for<'i> F: Fn(&'i str) -> Option<(A, &'i str)> + Clone
{
    f: F,
    _phantom: std::marker::PhantomData<A>
}

impl<F, A> Clone for FnParser<F, A>
where
    for<'i> F: Fn(&'i str) -> Option<(A, &'i str)> + Clone
{
    fn clone(&self) -> Self {
        FnParser { f: self.f.clone(), _phantom: std::marker::PhantomData }
    }
}

impl<F, A> Parser for FnParser<F, A>
where
    for<'i> F: Fn(&'i str) -> Option<(A, &'i str)> + Clone
{
    type Output = A;

    fn run<'i>(&self, input: &'i str) -> Option<(Self::Output, &'i str)> {
        (self.f)(input)
    }
}

pub fn option<A>(p: impl Parser<Output = A>) -> impl Parser<Output = Option<A>> {
    p_fn(move |input|{
        match p.run(input) {
            Some((a, rest)) => Some((Some(a), rest)),
            None => Some((None, input))
        }
    })
}

pub fn many0<A>(p: impl Parser<Output = A>) -> impl Parser<Output = Vec<A>> {
    p_fn(move |input| {
        let mut input = input;
        let mut vec = Vec::new();
        loop {
            match p.run(input) {
                Some((a, rest)) => {
                    vec.push(a);
                    input = rest;
                },
                None => break Some((vec, input))
            }
        }
    })
}

pub fn many1<A>(p: impl Parser<Output = A>) -> impl Parser<Output = Vec<A>> {
    p_fn(move |input| {
        let mut input = input;
        let mut vec = Vec::new();
        match p.run(input) {
                Some((a, rest)) => {
                    vec.push(a);
                    input = rest;
                },
                None => return None
        }
        loop {
            match p.run(input) {
                Some((a, rest)) => {
                    vec.push(a);
                    input = rest;
                },
                None => break Some((vec, input))
            }
        }
    })
}

pub fn pair<A, B>(p1: impl Parser<Output = A>, p2: impl Parser<Output = B>) -> impl Parser<Output = (A, B)> {
    p_fn(move|input| {
        if let Some((res1, rest1)) = p1.run(input) {
            if let Some((res2, rest2)) = p2.run(rest1) {
                return Some(((res1, res2), rest2));
            }
        }
        None
    })
}

pub fn not<A>(p: impl Parser<Output = A>) -> impl Parser<Output = ()> {
    p_fn(move |input|
        match p.run(input) {
            Some(_) => None,
            None => Some(((), input))
        }
    )
}

pub fn left<A, B>(p1: impl Parser<Output = A>, p2: impl Parser<Output = B>) -> impl Parser<Output = A> {
    pair(p1, p2).map(|(a, _b)| a)
}

pub fn right<A, B>(p1: impl Parser<Output = A>, p2: impl Parser<Output = B>) -> impl Parser<Output = B> {
    pair(p1, p2).map(|(_a, b)| b)
}

pub fn recognize<A>(p: impl Parser<Output = A>) -> impl Parser<Output = String> {
    p_fn(move|input| {
        let original_input = input;
        p.run(input).map(|(_a, rest )| {
            let consumed_len = original_input.len() - rest.len();
            (original_input[..consumed_len].to_string(), rest)
        })
    })
}

pub fn take_while1<F>(f: F) -> impl Parser<Output = String>
where
    F: Fn(char) -> bool + Clone
{
    p_fn(move|input|{
        let end_index = match input.char_indices().find(|&(_, c)| !f(c)) {
            Some((index, _)) => index,
            None => input.len()
        };

        if end_index > 0 {
            let (matched, rest) = input.split_at(end_index);
            Some((matched.to_string(), rest))
        }
        else {
            None
        }
    })
}

pub fn repeat_exact<A>(n: usize, p: impl Parser<Output = A>) -> impl Parser<Output = Vec<A>> {
    p_fn(move|input|{
        let mut input = input;
        let mut result = Vec::<A>::new();
        for _ in 0..n {
            if let Some((a, rest)) = p.run(input) {
                result.push(a);
                input = rest
            }
            else {
                return None
            }
        }

        if let Some(_) = p.run(input) {
            return None
        }
        else {
            Some((result, input))
        }
    })
}

pub fn char1(c: char) -> impl Parser<Output = char> {
    p_fn(move |input| {
        if input.starts_with(c) {
            Some((c, &input[c.len_utf8()..]))
        }
        else {
            None
        }
    })
}

pub fn any_char() -> impl Parser<Output = char> {
    p_fn(|input| {
        input
            .chars()
            .next()
            .map(|c|{
                (c, &input[c.len_utf8()..])
            })
    })
}

pub fn char_range(range: impl std::ops::RangeBounds<char> + Clone) -> impl Parser<Output = char> {
    p_fn(move|input| {
        if let Some(c) =  input.chars().next() {
            if range.contains(&c) {
                return Some((c, &input[c.len_utf8()..]));
            }
        }
        None
    })
}

pub fn string(str: &'static str) -> impl Parser<Output = String> {
    p_fn(move|input| {
        if input.starts_with(str) {
            Some((input[..str.len()].to_string(), &input[str.len()..]))
        }
        else {
            None
        }
    })
}


// maybe i won't use these functions
#[allow(dead_code)]
fn select<A, P>(parsers: Vec<P>) -> impl Parser<Output = A>
where
    P: Parser<Output = A> + Clone
{
    p_fn(move |input| {
        for p in &parsers {
            if let res @ Some(_) = p.run(input) {
                return res;
            }
        }
        None
    })
}

#[allow(dead_code)]
fn sequence<A, P>(parsers: Vec<P>) -> impl Parser<Output = Vec<A>>
where
    P: Parser<Output = A> + Clone
{
    p_fn(move |input| {
        let mut res = Vec::new();
        let mut input = input;

        for p in &parsers {
            if let Some((a, rest)) = p.run(input) {
                res.push(a);
                input = rest;
            }
            else {
                return None;
            }
        }

        Some((res, input))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_char1() {
        let parse_a = char1('a');

        assert_eq!(parse_a.run("abc"), Some(('a', "bc")));
        assert_eq!(parse_a.run("bcd"), None);
    }

    #[test]
    fn test_and_then_char() {
        let parse_a = char1('a');
        let parse_ab = parse_a.clone().and_then(|_| char1('b'));

        assert_eq!(parse_ab.run("abcd"), Some(('b', "cd")));
        assert_eq!(parse_ab.run("bcde"), None);
    }

    #[test]
    fn test_any_char() {
        let parse_any_char = any_char();
        let parse_any_2chars = parse_any_char.clone().and_then(|_| any_char());

        assert_eq!(parse_any_char.run("hello"), Some(('h', "ello")));
        assert_eq!(parse_any_2chars.run("hello"), Some(('e', "llo")));
    }

    #[test]
    fn test_or_else() {
        let parse_a = char1('a');
        let parse_b = char1('b');

        let parse_a_or_b = parse_a.or_else(parse_b);

        assert_eq!(parse_a_or_b.run("abc"), Some(('a', "bc")));
        assert_eq!(parse_a_or_b.run("bcd"), Some(('b', "cd")));
        assert_eq!(parse_a_or_b.run("cde"), None);
        assert_eq!(parse_a_or_b.run("cde"), None);
    }

    #[test]
    fn test_many0() {
        let parse_a = char1('a');
        let parse_repeat_a = many0(parse_a);

        assert_eq!(parse_repeat_a.run("abc"), Some((vec!['a'], "bc")));
        assert_eq!(parse_repeat_a.run("aaaabc"), Some((vec!['a', 'a', 'a', 'a'], "bc")));
        assert_eq!(parse_repeat_a.run("bc"), Some((vec![], "bc")));        
    }

    #[test]
    fn test_many1() {
        let parse_a = char1('a');
        let parse_repeat_one_more_a = many1(parse_a);

        assert_eq!(parse_repeat_one_more_a.run("abc"), Some((vec!['a'], "bc")));
        assert_eq!(parse_repeat_one_more_a.run("aaaabc"), Some((vec!['a', 'a', 'a', 'a'], "bc")));
        assert_eq!(parse_repeat_one_more_a.run("bc"), None);        
    }

    #[test]
    fn test_char_range() {
        let lower_alphabet = char_range('a'..='z');
        let upper_alphabet = char_range('A'..='Z');
        let alphabet = lower_alphabet.clone().or_else(upper_alphabet);
        let number = char_range('0'..='9');
        let alpha_numeral = alphabet.clone().or_else(number);
        let underscore = char1('_');

        let identifier_first = underscore.clone().or_else(alphabet);
        let identifier_rest = underscore.clone().or_else(alpha_numeral);

        let identifier = identifier_first.clone().and_then(move|first| {
            many0(identifier_rest.clone()).map(move |mut rest| {
                let mut chars = vec![first];
                chars.append(&mut rest);
                chars.into_iter().collect::<String>()
            })
        });

        assert_eq!(identifier.run("id123  "), Some(("id123".to_string(), "  ")));
        assert_eq!(identifier.run("_start;"), Some(("_start".to_string(), ";")));
        assert_eq!(identifier.run("8192u16"), None);
    }

    #[test]
    fn test_pair() {
        let alphabet = char_range('a'..='z').clone().or_else(char_range('A'..='Z'));
        let alphabets = many1(alphabet).map(|v| v.iter().collect::<String>());
        let number = char_range('0'..='9');
        let numbers = many1(number).map(|v| v.iter().collect::<String>());
        let alphabet_then_number = pair(alphabets, numbers);

        assert_eq!(alphabet_then_number.run("abc123"), Some((("abc".to_string(), "123".to_string()), "")));
        assert_eq!(alphabet_then_number.run("123abc"), None);
    }

    #[test]
    fn test_option() {
        let sign = option(char1('+').or_else(char1('-')))
            .map(|a| a.map(|c|c.to_string()).unwrap_or("".to_string()));

        assert_eq!(sign.run("123"), Some(("".to_string(), "123")));
        assert_eq!(sign.run("+123"), Some(("+".to_string(), "123")));
        assert_eq!(sign.run("-123"), Some(("-".to_string(), "123")));
    }

    #[test]
    fn test_not() {
        let not_a = not(char1('a'));
        let not_a_string = many1(
            not_a.and_then(|_| any_char())
        )
        .map(|cs| cs.iter().collect::<String>());

        assert_eq!(not_a_string.run("bcdefg"), Some(("bcdefg".to_string(), "")));
        assert_eq!(not_a_string.run("bcdefa"), Some(("bcdef".to_string(), "a")));
        assert_eq!(not_a_string.run("abcdef"), None);
    }

    #[test]
    fn test_left_right() {
        let string_inner = many0(
            not(char1('"')).and_then(|_| any_char())
        )
        .map(|cs| cs.iter().collect::<String>());

        let quoted_string = right(
            char1('"'),
            left(
                    string_inner,
                    char1('"')
            )
        );

        assert_eq!(quoted_string.run("\"quoted string\""), Some(("quoted string".to_string(), "")));
        assert_eq!(quoted_string.run("\"\""), Some(("".to_string(), "")));
        assert_eq!(quoted_string.run("\"not closed quoted string"), None);
        assert_eq!(quoted_string.run("quoted string will start\"hello\""), None);
    }

    #[test]
    fn test_recognize() {
        let digit = char_range('0'..='9');
        let digits = many1(digit.clone());
        let recognized_digits = recognize(digits);

        assert_eq!(recognized_digits.run("123abc"), Some(("123".to_string(), "abc")));
        assert_eq!(recognized_digits.run("abc123"), None);
        assert_eq!(recognized_digits.run("456xyz789"), Some(("456".to_string(), "xyz789")));
        assert_eq!(recognized_digits.run(""), None);
    }

    #[test]
    fn test_take_while1() {
        let take_digits = take_while1(|c| c.is_digit(10));

        assert_eq!(take_digits.run("123abc"), Some(("123".to_string(), "abc")));
        assert_eq!(take_digits.run("abc123"), None);
        assert_eq!(take_digits.run("456xyz789"), Some(("456".to_string(), "xyz789")));
        assert_eq!(take_digits.run(""), None);
    }

    #[test]
    fn test_select() {
        let nums = ('0'..='9').map(|c| char1(c)).collect();
        let digit = select(nums);
        let digits = many1(digit.clone());

        assert_eq!(digit.run("0123"), Some(('0', "123")));
        assert_eq!(digit.run("abcd"), None);
        assert_eq!(digits.run("1234usize"), Some((vec!['1', '2', '3', '4'], "usize")));
        assert_eq!(digits.run("hello1234"), None);        
    }

    #[test]
    fn test_repeat_exact() {
        let repeat_a_4times = repeat_exact(4, char1('a'))
            .map(|cs|cs.iter().collect::<String>());

        assert_eq!(repeat_a_4times.run("aaaab"), Some(("aaaa".to_string(), "b")));
        assert_eq!(repeat_a_4times.run("aaaa"), Some(("aaaa".to_string(), "")));
        assert_eq!(repeat_a_4times.run("aaaaa"), None);
        assert_eq!(repeat_a_4times.run("aaabb"), None);
    }

    #[test]
    fn test_sequence() {
        let abc = sequence(vec![char1('a'), char1('b'), char1('c')]);

        assert_eq!(
            abc.run("abcdef"),
            Some((vec!['a','b','c'], "def"))
        );

        assert_eq!(
            abc.run("axcdef"),
            None
        );

        assert_eq!(
            abc.run("ab"),
            None
        );

        let mut char_parser_vec = vec![char1('a')];
        char_parser_vec.clear();
        let parse_nothing = sequence(char_parser_vec);
        assert_eq!(
            parse_nothing.run("anything is ok"),
            Some((vec![], "anything is ok"))
        );
    }
}