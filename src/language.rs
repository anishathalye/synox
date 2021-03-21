use crate::token::Token;
use std::fmt::Debug;

type State<'a> = &'a Vec<String>;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct ColumnIndex(pub usize);

pub trait StringProgram: Debug {
    fn run(&self, row: State) -> Option<String>;
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StringExpression(pub Vec<SubstringExpression>);

impl StringProgram for StringExpression {
    fn run(&self, row: State) -> Option<String> {
        self.0.iter().fold(Some(String::new()), |acc, e| {
            acc.and_then(|mut s| {
                e.run(row).map(|part| {
                    s.push_str(&part);
                    s
                })
            })
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum SubstringExpression {
    ConstantString(String),
    Substring(ColumnIndex, Position, Position),
}

impl SubstringExpression {
    pub fn run(&self, row: State) -> Option<String> {
        match self {
            SubstringExpression::ConstantString(s) => Some(s.clone()),
            SubstringExpression::Substring(ci, p_start, p_end) => {
                let s = row.get(ci.0)?;
                let p_start = p_start.run(s)?;
                let p_end = p_end.run(s)?;
                if p_start.0 >= p_end.0 {
                    return None;
                }
                Some(String::from(&s[p_start.0 - 1..p_end.0 - 1]))
            }
        }
    }
}

// a one-based string index
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct StringIndex(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Occurrence(pub isize);

impl Occurrence {
    pub fn weight(&self) -> isize {
        // prefer occurrences closer to ends
        -self.0.abs()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Position {
    Match(Token, Occurrence, Direction),
    ConstantPosition(Occurrence),
}

impl Position {
    fn run(&self, s: &str) -> Option<StringIndex> {
        match self {
            Position::Match(token, k, dir) => {
                let k = k.0;
                let matches = token.all_matches(s);
                let n = matches.len() as isize;
                let k = if k > 0 { k - 1 } else { n + k };
                if !(0 <= k && k < n) {
                    return None;
                }
                // now, k is a 0-based index, and we know that it's in bounds
                let r = &matches[k as usize];
                match dir {
                    Direction::Start => Some(StringIndex(r.start)),
                    Direction::End => Some(StringIndex(r.end)),
                }
            }
            Position::ConstantPosition(k) => {
                let k = k.0;
                let n = s.len() as isize;
                let k = if k > 0 { k } else { n + k + 1 };
                if !(0 < k && k <= n + 1) {
                    None
                } else {
                    Some(StringIndex(k as usize))
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Direction {
    Start,
    End,
}

#[cfg(test)]
mod tests {
    use super::*;
    use Direction::*;
    use Position::*;
    use SubstringExpression::*;

    fn assert_eval_single(p: &impl StringProgram, s: &str, expected: &str) {
        let res = p.run(&vec![String::from(s)]).unwrap();
        assert_eq!(res, String::from(expected));
    }

    #[test]
    fn extract_country() {
        let p = StringExpression(vec![Substring(
            ColumnIndex(0),
            Match(Token::Literal(String::from(", ")), Occurrence(1), End),
            Match(Token::End, Occurrence(-1), Start),
        )]);
        assert_eval_single(&p, "Mumbai, India", "India");
        assert_eval_single(
            &p,
            "Los Angeles, United States of America",
            "United States of America",
        );
        assert_eval_single(&p, "Newark, United States", "United States");
    }

    #[test]
    fn extract_initials() {
        let p = StringExpression(vec![
            Substring(
                ColumnIndex(0),
                Match(Token::CapsWithSpaces, Occurrence(1), Start),
                Match(Token::Caps, Occurrence(1), End),
            ),
            ConstantString(String::from(".")),
            Substring(
                ColumnIndex(0),
                Match(Token::Whitespace, Occurrence(-1), End),
                Match(Token::Lowercase, Occurrence(-1), Start),
            ),
            ConstantString(String::from(".")),
        ]);
        assert_eval_single(&p, "Brandon Henry Saunders", "B.S.");
        assert_eval_single(&p, "William Lee", "W.L.");
        assert_eval_single(&p, "Dafna Q. Chen", "D.C.");
        assert_eval_single(&p, "Danielle D. Saunders", "D.S.");
    }

    #[test]
    fn add_bracket() {
        let p = StringExpression(vec![
            Substring(
                ColumnIndex(0),
                Match(Token::Start, Occurrence(1), End),
                Match(Token::Digits, Occurrence(1), End),
            ),
            ConstantString(String::from("]")),
        ]);
        assert_eval_single(&p, "[CPT-00350", "[CPT-00350]");
        assert_eval_single(&p, "[CPT-00340", "[CPT-00340]");
        assert_eval_single(&p, "[CPT-11536]", "[CPT-11536]");
        assert_eval_single(&p, "[CPT-115]", "[CPT-115]");
    }

    #[test]
    fn constant_strings() {
        let p = StringExpression(vec![Substring(
            ColumnIndex(0),
            Match(
                Token::Literal(String::from("nextData ")),
                Occurrence(1),
                End,
            ),
            Match(
                Token::Literal(String::from(" moreInfo")),
                Occurrence(1),
                Start,
            ),
        )]);
        assert_eval_single(&p, "nextData 12 Street moreInfo 35", "12 Street");
        assert_eval_single(&p, "nextData Main moreInfo 36", "Main");
        assert_eval_single(&p, "nextData Albany Street moreInfo 37", "Albany Street");
        assert_eval_single(&p, "nextData Green Street moreInfo 39", "Green Street");
    }

    #[test]
    fn constant_position() {
        let p = StringExpression(vec![Substring(
            ColumnIndex(0),
            ConstantPosition(Occurrence(3)),
            Match(Token::Literal(String::from("|")), Occurrence(1), Start),
        )]);
        assert_eval_single(&p, "xzHello|asdofij", "Hello");
    }
}
