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

pub fn zero_or_more<A>(p: impl Parser<Output = A>) -> impl Parser<Output = Vec<A>> {
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

pub fn one_or_more<A>(p: impl Parser<Output = A>) -> impl Parser<Output = Vec<A>> {
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
    fn test_zero_or_more() {
        let parse_a = char1('a');
        let parse_repeat_a = zero_or_more(parse_a);

        assert_eq!(parse_repeat_a.run("abc"), Some((vec!['a'], "bc")));
        assert_eq!(parse_repeat_a.run("aaaabc"), Some((vec!['a', 'a', 'a', 'a'], "bc")));
        assert_eq!(parse_repeat_a.run("bc"), Some((vec![], "bc")));        
    }

    #[test]
    fn test_one_or_more() {
        let parse_a = char1('a');
        let parse_repeat_one_more_a = one_or_more(parse_a);

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
            zero_or_more(identifier_rest.clone()).map(move |mut rest| {
                let mut chars = vec![first];
                chars.append(&mut rest);
                chars.into_iter().collect::<String>()
            })
        });

        assert_eq!(identifier.run("id123  "), Some(("id123".to_string(), "  ")));
        assert_eq!(identifier.run("_start;"), Some(("_start".to_string(), ";")));
        assert_eq!(identifier.run("8192u16"), None);
    }
}