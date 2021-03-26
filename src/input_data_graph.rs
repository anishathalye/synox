use crate::graph;
use crate::language::{Occurrence, StringIndex};
use crate::token::{Token, ALL_RE_TOKENS};
use std::cmp;
use std::collections::{HashMap, HashSet};

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Id {
    pub row: usize,
    pub col: usize,
}

impl Id {
    #[cfg(test)]
    pub fn new(row: usize, col: usize) -> Id {
        Id { row, col }
    }
}

type Node = usize;
type Edge = (Node, Node);

#[derive(Debug)]
pub struct InputDataGraph {
    pub labels: HashMap<Node, HashMap<Id, StringIndex>>,
    pub tokens: HashMap<Edge, HashSet<(Token, Occurrence)>>,
}

impl InputDataGraph {
    // Requires strs to be non-jagged
    pub fn new(strs: &[Vec<String>]) -> Self {
        let rows = strs.len();
        if rows == 0 {
            return Self::empty();
        }
        let cols = strs[0].len();
        Self::union((0..cols).map(|col| {
            (0..rows)
                .map(|row| Self::from_str(strs[row][col].as_str(), Id { row, col }))
                .fold(None, |acc, x| -> Option<Self> {
                    match acc {
                        Some(acc) => Some(acc.intersection(&x)),
                        None => Some(x),
                    }
                })
                .unwrap()
        }))
    }

    fn empty() -> Self {
        Self {
            labels: HashMap::new(),
            tokens: HashMap::new(),
        }
    }

    pub fn nodes(&self) -> impl ExactSizeIterator<Item = &Node> + '_ {
        self.labels.keys()
    }

    pub fn edges(&self) -> impl ExactSizeIterator<Item = &Edge> + '_ {
        self.tokens.keys()
    }

    fn from_str(s: &str, id: Id) -> Self {
        let mut labels = HashMap::new();
        let mut tokens = HashMap::new();

        for i in 0..s.len() + 3 {
            let mut label = HashMap::new();
            label.insert(id, StringIndex(i));
            labels.insert(i, label);
        }

        let mut t_start = HashSet::new();
        t_start.insert((Token::Start, Occurrence(1)));
        // note: we are not inserting start/end with occurrence -1, because this matches exactly
        // once in any string, so it would be redundant to include the other (they would always
        // occur together)
        tokens.insert((0, 1), t_start);
        let mut t_end = HashSet::new();
        t_end.insert((Token::End, Occurrence(1)));
        tokens.insert((s.len() + 1, s.len() + 2), t_end);

        // this is in terms of adjusted indices
        for token in ALL_RE_TOKENS {
            let matches = token.all_matches(s);
            let n = matches.len() as isize;
            for (i, span) in matches.iter().enumerate() {
                let set = tokens
                    .entry((span.start, span.end))
                    .or_insert_with(HashSet::new);
                let i = i as isize;
                set.insert((token.clone(), Occurrence(i + 1)));
                set.insert((token.clone(), Occurrence(i - n)));
            }
        }

        for i in 1..s.len() + 1 {
            for j in i + 1..s.len() + 2 {
                // these indices are 0-based so we can actually index into the slice; also,
                // right_index is exclusive
                let left_index = i - 1;
                let right_index = j - 1;
                let c_s = &s[left_index..right_index];

                // literal token
                let lit_tok = Token::Literal(String::from(c_s));
                let lit_tok_matches = lit_tok.all_matches(s);
                let lit_tok_matches_n = lit_tok_matches.len() as isize;
                for (span_idx, span) in lit_tok_matches.iter().enumerate() {
                    if span.start == i && span.end == j {
                        let set = tokens.entry((i, j)).or_insert_with(HashSet::new);
                        let span_idx = span_idx as isize;
                        set.insert((lit_tok.clone(), Occurrence(span_idx + 1)));
                        set.insert((lit_tok.clone(), Occurrence(span_idx - lit_tok_matches_n)));
                        break; // don't need to look at more matches
                    }
                }
            }
        }

        Self { labels, tokens }
    }

    fn intersection(&self, other: &Self) -> Self {
        let mut renumber: HashMap<Edge, Node> = HashMap::new();
        let mut curr = 0;
        let mut number = |n1, n2| -> Node {
            *renumber.entry((n1, n2)).or_insert_with(|| {
                let v = curr;
                curr += 1;
                v
            })
        };

        let mut tokens = HashMap::new();
        let mut nodes = HashSet::new();
        for ((v1s, v1f), t1) in &self.tokens {
            for ((v2s, v2f), t2) in &other.tokens {
                let intersection: HashSet<_> = t1.intersection(&t2).cloned().collect();
                if !intersection.is_empty() {
                    let vs = number(*v1s, *v2s);
                    nodes.insert(vs);
                    let vf = number(*v1f, *v2f);
                    nodes.insert(vf);
                    let e = (vs, vf);
                    tokens.insert(e, intersection);
                }
            }
        }

        let mut labels = HashMap::new();
        for (v1, l1) in &self.labels {
            for (v2, l2) in &other.labels {
                let v = number(*v1, *v2);
                if nodes.contains(&v) {
                    let mut union = HashMap::new();
                    union.extend(l1);
                    union.extend(l2);
                    labels.insert(v, union);
                }
            }
        }

        Self { labels, tokens }
    }

    fn union(graphs: impl Iterator<Item = Self>) -> Self {
        let mut labels = HashMap::new();
        let mut tokens = HashMap::new();

        let mut curr = 0;
        for graph in graphs {
            let mut renumber: HashMap<Node, Node> = HashMap::new();
            let mut number = |n| -> Node {
                *renumber.entry(n).or_insert_with(|| {
                    let v = curr;
                    curr += 1;
                    v
                })
            };
            // copy labels, with renumbering
            for (n, l) in graph.labels {
                labels.insert(number(n), l);
            }
            // copy tokens, with renumbering
            for ((v1, v2), t) in graph.tokens {
                tokens.insert((number(v1), number(v2)), t);
            }
        }

        Self { labels, tokens }
    }

    pub fn distances(&self) -> HashMap<Edge, usize> {
        let mut result = HashMap::new();
        for v1 in self.nodes() {
            for v2 in self.nodes() {
                let dist = self.labels[v1]
                    .iter()
                    .map(|(id, index1)| {
                        // we only care about nodes corresponding to the same column, otherwise their
                        // distance is not going to be used; if labels[v2] doesn't contain id, we
                        // just use index1 (it doesn't matter what we use, the result is not going
                        // to be used
                        ((self.labels[v2].get(id).unwrap_or(index1).0 as isize)
                            - (index1.0 as isize))
                            .abs() as usize
                    })
                    .sum();
                result.insert((*v1, *v2), dist);
            }
        }
        result
    }

    pub fn rank_nodes(&self, distances: &HashMap<Edge, usize>) -> HashMap<Node, usize> {
        let mut v_out: HashMap<Node, usize> = HashMap::new();
        let mut v_in: HashMap<Node, usize> = HashMap::new();
        for node in self.nodes() {
            v_out.insert(*node, 0);
            v_in.insert(*node, 0);
        }
        let adj = graph::adjacency_map(self.edges());
        let inv = graph::invert_adjacency_map(&adj);
        let mut topo = graph::topological_sort(&adj);
        let empty: HashSet<Node> = HashSet::new();
        // NOTE this looks different from the paper, but that is because our topological sort order
        // is the reverse of the order assumed in Figure 16
        for v in &topo {
            for vi in inv.get(&v).unwrap_or(&empty) {
                v_in.insert(*v, cmp::max(v_in[&v], v_in[&vi] + distances[&(*vi, *v)]));
            }
        }
        topo.reverse();
        for v in &topo {
            for vi in adj.get(&v).unwrap_or(&empty) {
                v_out.insert(*v, cmp::max(v_out[&v], v_out[&vi] + distances[&(*v, *vi)]));
            }
        }
        // total the score in v_out (instead of allocating a separate hash map)
        for (v, score_out) in &mut v_out {
            *score_out += v_in[v];
        }
        v_out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generate_from_string_1() {
        // from Figure 6 in BlinkFill VLDB '16 paper
        let s = "1 lb";
        let graph = InputDataGraph::from_str(&s, Id::new(0, 0));
        // do some spot checks
        assert_eq!(graph.nodes().len(), 7);
        assert_eq!(graph.edges().len(), 12);
        // check a_12
        let toks: HashSet<_> = vec![
            (Token::Digits, Occurrence(1)),
            (Token::Digits, Occurrence(-1)),
            (Token::Alphanumeric, Occurrence(1)),
            (Token::Alphanumeric, Occurrence(-2)), // note that the figure is wrong about this one
            (Token::Literal(String::from("1")), Occurrence(1)),
            (Token::Literal(String::from("1")), Occurrence(-1)),
        ]
        .into_iter()
        .collect();
        assert_eq!(graph.tokens.get(&(1, 2)).unwrap(), &toks);
        // check size of a_35
        assert_eq!(graph.tokens.get(&(3, 5)).unwrap().len(), 12);
    }

    #[test]
    fn generate_from_string_2() {
        let s = "23 g";
        let graph = InputDataGraph::from_str(&s, Id::new(0, 0));
        // do some spot checks
        assert_eq!(graph.nodes().len(), 7);
        assert_eq!(graph.edges().len(), 12);
        // check a_45
        let toks: HashSet<_> = vec![
            (Token::Lowercase, Occurrence(1)),
            (Token::Lowercase, Occurrence(-1)),
            (Token::Alphabets, Occurrence(1)),
            (Token::Alphabets, Occurrence(-1)),
            (Token::Alphanumeric, Occurrence(2)),
            (Token::Alphanumeric, Occurrence(-1)),
            (Token::LowercaseWithSpaces, Occurrence(1)),
            (Token::LowercaseWithSpaces, Occurrence(-1)),
            (Token::AlphabetsWithSpaces, Occurrence(1)),
            (Token::AlphabetsWithSpaces, Occurrence(-1)),
            (Token::Literal(String::from("g")), Occurrence(1)),
            (Token::Literal(String::from("g")), Occurrence(-1)),
        ]
        .into_iter()
        .collect();
        assert_eq!(graph.tokens.get(&(4, 5)).unwrap(), &toks);
    }

    #[test]
    fn intersection() {
        // from Figure 9 in BlinkFill paper
        let g1 = InputDataGraph::from_str("1 lb", Id::new(0, 0));
        let g2 = InputDataGraph::from_str("23 g", Id::new(1, 0));
        let graph = g1.intersection(&g2);
        assert_eq!(graph.nodes().len(), 6);
        assert_eq!(graph.edges().len(), 6);
        let token_lengths: HashSet<_> = graph.tokens.values().map(|v| v.len()).collect();
        assert_eq!(token_lengths, vec![4, 1, 10, 2].iter().cloned().collect());
        let toks: HashSet<_> = vec![
            (Token::Literal(String::from(" ")), Occurrence(1)),
            (Token::Literal(String::from(" ")), Occurrence(-1)),
            (Token::Whitespace, Occurrence(1)),
            (Token::Whitespace, Occurrence(-1)),
        ]
        .into_iter()
        .collect();
        assert!(graph.tokens.values().any(|v| *v == toks));
    }

    #[test]
    fn union() {
        // our own made-up test case, a basic sanity-check
        let g1 = InputDataGraph::from_str("1 lb", Id::new(0, 0));
        let g2 = InputDataGraph::from_str("23 g", Id::new(1, 0));
        let graphs = vec![g1, g2];
        let union = InputDataGraph::union(graphs.into_iter());
        assert_eq!(union.nodes().len(), 14);
        assert_eq!(union.edges().len(), 24);
    }

    #[test]
    fn new_single_column() {
        let strs = vec![vec![String::from("1 lb")], vec![String::from("23 g")]];
        let graph = InputDataGraph::new(&strs);
        assert_eq!(graph.nodes().len(), 6);
        assert_eq!(graph.edges().len(), 6);
    }

    #[test]
    fn new_multi_column() {
        let strs = vec![
            vec![String::from("1 lb"), String::from("1 lb")],
            vec![String::from("1 lb"), String::from("23 g")],
        ];
        let graph = InputDataGraph::new(&strs);
        assert_eq!(graph.nodes().len(), 7 + 6);
        assert_eq!(graph.edges().len(), 12 + 6);
    }
}
