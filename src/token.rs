use lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Token {
    ProperCase,
    Caps,
    Lowercase,
    Digits,
    Alphabets,
    Alphanumeric,
    Whitespace,
    Start,
    End,
    ProperCaseWithSpaces,
    CapsWithSpaces,
    LowercaseWithSpaces,
    AlphabetsWithSpaces,
    Literal(String),
}

use Token::*;

// 1-indexed, inclusive
#[derive(Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub const ALL_RE_TOKENS: &'static [Token] = &[
    ProperCase,
    Caps,
    Lowercase,
    Digits,
    Alphabets,
    Alphanumeric,
    Whitespace,
    ProperCaseWithSpaces,
    CapsWithSpaces,
    LowercaseWithSpaces,
    AlphabetsWithSpaces,
];

impl Token {
    pub fn all_matches(&self, s: &str) -> Vec<Span> {
        let mut matches = Vec::new();
        let mut offset = 0;
        match self {
            Start => {
                // should only be looking at this span's end, in the start position of a substring
                // op
                matches.push(Span { start: 0, end: 1 });
            }
            End => {
                // should only be looking at this span's start, in the end position of a substring
                // op
                matches.push(Span {
                    start: s.len() + 1,
                    end: s.len() + 2,
                });
            }
            Literal(tok_str) => {
                let len = tok_str.len();
                while offset < s.len() {
                    let start = &s[offset..].find(tok_str);
                    match start {
                        None => {
                            // no more matches
                            return matches;
                        }
                        Some(start) => {
                            let end = start + len;
                            matches.push(Span {
                                start: offset + start + 1,
                                end: offset + end + 1,
                            });
                            offset = offset + end;
                        }
                    }
                }
            }
            _ => {
                let re = self.to_regex();
                while offset < s.len() {
                    let mat = re.find(&s[offset..]);
                    match mat {
                        None => {
                            // no more matches
                            return matches;
                        }
                        Some(mat) => {
                            matches.push(Span {
                                start: offset + mat.start() + 1,
                                end: offset + mat.end() + 1,
                            });
                            offset = offset + mat.end();
                        }
                    }
                }
            }
        }
        matches
    }

    fn to_regex(&self) -> &Regex {
        match self {
            ProperCase => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{Lu}\p{Ll}+").unwrap();
                }
                &RE
            }
            Caps => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{Lu}+").unwrap();
                }
                &RE
            }
            Lowercase => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{Ll}+").unwrap();
                }
                &RE
            }
            Digits => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\d+").unwrap();
                }
                &RE
            }
            Alphabets => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{L}+").unwrap();
                }
                &RE
            }
            Alphanumeric => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"[\p{L}\d]+").unwrap();
                }
                &RE
            }
            Whitespace => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{Zs}+").unwrap();
                }
                &RE
            }
            ProperCaseWithSpaces => {
                lazy_static! {
                    static ref RE: Regex =
                        Regex::new(r"\p{Lu}\p{Ll}+(?:\p{Zs}+\p{Lu}\p{Ll}+)*").unwrap();
                }
                &RE
            }
            CapsWithSpaces => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{Lu}+(?:\p{Zs}+\p{Lu}+)*").unwrap();
                }
                &RE
            }
            LowercaseWithSpaces => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{Ll}+(?:\p{Zs}+\p{Ll}+)*").unwrap();
                }
                &RE
            }
            AlphabetsWithSpaces => {
                lazy_static! {
                    static ref RE: Regex = Regex::new(r"\p{L}+(?:\p{Zs}+\p{L}+)*").unwrap();
                }
                &RE
            }
            Start | End | Literal(_) => {
                panic!("Token type {:?} does not support regex", self)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_regex() {
        let re = ProperCaseWithSpaces.to_regex();
        assert!(re.is_match("Foo Bar Baz"));
        assert!(!re.is_match("foo bar"));
    }
}
