
pub trait Parser{
    type Output;

    fn run<'i>(&self, input: &'i str) -> Option<(Self::Output, &'i str)>;

    fn map<B>(&self, f: impl Fn(Self::Output) -> B) -> impl Parser<Output = B> {
        p_fn(move|input| {
            match self.run(input) {
                Some((a, rest)) => Some((f(a), rest)),
                None => None
            }
        })
    }

    fn and_then<B, F>(&self, f: impl Fn(Self::Output) -> F) -> impl Parser<Output = B>
    where
        F: Parser<Output = B>
    {
        p_fn(move|input| {
            match self.run(input) {
                Some((a, rest)) => f(a).run(rest),
                None => None
            }
        })
    }
}

fn p_fn<F, A>(f: F) -> FnParser<F, A>
where
    for<'i> F: Fn(&'i str) -> Option<(A, &'i str)>
{
    FnParser{
        f,
        _phantom: std::marker::PhantomData
    }
}

impl<A, F> Parser for F
where
    F: for<'i> Fn(&'i str) -> Option<(A, &'i str)>
{
    type Output = A;
    fn run<'i>(&self, input: &'i str) -> Option<(Self::Output, &'i str)> {
        (self)(input)
    }
}

pub struct FnParser<F, A> {
    f: F,
    _phantom: std::marker::PhantomData<A>
}

impl<F, A> Parser for FnParser<F, A>
where
    for<'i> F: Fn(&'i str) -> Option<(A, &'i str)>
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
        let parse_ab = parse_a.and_then(|_| char1('b'));

        assert_eq!(parse_ab.run("abcd"), Some(('b', "cd")));
        assert_eq!(parse_ab.run("bcde"), None);
    }
}