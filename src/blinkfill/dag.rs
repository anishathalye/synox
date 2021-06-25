use super::input_data_graph::{Id, InputDataGraph};
use super::language::{
    ColumnIndex, Direction, Occurrence, Position, StringExpression, StringIndex,
    SubstringExpression,
};
use crate::graph;
use std::collections::{BTreeMap, BTreeSet, HashMap};

const EPSILON: usize = 1;
const KAPPA: usize = 15; // BlinkFill Section 7.3

type Node = usize;
type Edge = (Node, Node);

#[derive(Debug)]
pub struct Dag {
    start: Node,
    finish: Node,
    substrings: BTreeMap<Edge, Vec<SubstringExpressionSet>>,
    num_examples: usize,
}

impl Dag {
    fn new(input: &[&str], output: &str, graph: &InputDataGraph, row: usize) -> Self {
        let mut substrings = BTreeMap::new();
        let n = output.len();

        for i in 0..n {
            for j in i + 1..n + 1 {
                let s = &output[i..j];
                // learn the constant string
                let mut exprs = vec![ConstantString(String::from(s))];
                // learn all substring expressions
                for (ci, input_str) in input.iter().enumerate() {
                    // find all instances of s (including overlapping ones) in input_str
                    let id = Id { row, col: ci };
                    let mut offset = 0;
                    while offset < input_str.len() {
                        match input_str[offset..].find(&s) {
                            None => {
                                break;
                            }
                            Some(start) => {
                                let l = offset + start;
                                let r = l + s.len();
                                let (l, r) = (StringIndex(l + 1), StringIndex(r + 1));
                                let substring_exprs =
                                    SubstringExpressionSet::generate_substring_set(
                                        id, l, r, &graph,
                                    );
                                exprs.push(substring_exprs);
                                offset = offset + start + 1;
                            }
                        }
                    }
                }
                substrings.insert((i, j), exprs);
            }
        }

        Self {
            start: 0,
            finish: n,
            substrings,
            num_examples: 1,
        }
    }

    fn intersection(&self, other: &Self) -> Self {
        // uses roughly the same renumbering technique as InputDataGraph::intersection()
        let mut renumber: HashMap<Edge, Node> = HashMap::new();
        let mut curr = 0;
        let mut number = |n1, n2| -> Node {
            *renumber.entry((n1, n2)).or_insert_with(|| {
                let v = curr;
                curr += 1;
                v
            })
        };

        let mut substrings = BTreeMap::new();
        for ((v1s, v1f), s1) in &self.substrings {
            for ((v2s, v2f), s2) in &other.substrings {
                let vs = number(*v1s, *v2s);
                let vf = number(*v1f, *v2f);
                let mut exprs = vec![];
                for e1 in s1 {
                    for e2 in s2 {
                        if let Some(e) = e1.intersection(&e2) {
                            exprs.push(e);
                        }
                    }
                }
                if !exprs.is_empty() {
                    substrings.insert((vs, vf), exprs);
                }
            }
        }

        Self {
            start: number(self.start, other.start),
            finish: number(self.finish, other.finish),
            substrings,
            num_examples: self.num_examples + other.num_examples,
        }
    }

    pub fn learn(paired: &[(Vec<&str>, &str)], graph: &InputDataGraph) -> Self {
        paired
            .iter()
            .enumerate()
            .map(|(row, (input, output))| Self::new(&input, output, &graph, row))
            .fold(None, |acc, x| -> Option<Self> {
                match acc {
                    Some(acc) => Some(acc.intersection(&x)),
                    None => Some(x),
                }
            })
            .unwrap()
    }

    pub fn top_ranked_expression(&self, graph: &InputDataGraph) -> Option<StringExpression> {
        let distances = graph.distances();
        let ranks = graph.rank_nodes(&distances);
        let mut best_by_edge: BTreeMap<Edge, (usize, SubstringExpression)> = BTreeMap::new();
        // compute distances for edges
        let idg_adj = graph::adjacency_map(graph.edges());
        let idg_inv = graph::invert_adjacency_map(&idg_adj);
        for (edge, expr_set_set) in &self.substrings {
            let mut best: Option<SubstringExpression> = None;
            let mut best_score = 0;
            for expr_set in expr_set_set {
                let expr;
                let score;
                match expr_set {
                    ConstantString(s) => {
                        expr = Some(SubstringExpression::ConstantString(s.clone()));
                        score = s.len() * s.len() * EPSILON;
                    }
                    SubstringSet(ci, p_l, p_r) => {
                        let key = |p: &'_ &PositionSet| -> usize {
                            match p {
                                ConstantPosition(_) => 0,
                                GraphNode(v) => ranks[&v],
                            }
                        };
                        let p_l = p_l.iter().max_by_key(key).unwrap();
                        let p_r = p_r.iter().max_by_key(key).unwrap();

                        // NOTE all of the programs captured in the dag are consistent with the
                        // input-output examples, but they aren't necessarily even valid for the
                        // rest of the inputs.
                        //
                        // XXX Maybe we should find all the consistent (p_l, p_r) pairs first
                        // instead of taking the max by key above, and then after that take the max
                        // by key, so we don't accidentally throw away a good example and then be
                        // left with one that doesn't work. The BlinkFill paper isn't very clear
                        // about avoiding generating invalid programs for the input strings that
                        // don't have output examples.

                        let sample = |p: &PositionSet| -> Position {
                            match p {
                                ConstantPosition(k) => Position::ConstantPosition(*k),
                                GraphNode(v) => {
                                    // check in-edges
                                    let mut best: Option<Position> = None;
                                    let mut best_weight = (0, 0);
                                    // weight is (token.weight, -abs(occurence)), to favor certain
                                    // tokens, and then matches near the start or end
                                    if let Some(vss) = idg_inv.get(v) {
                                        for vs in vss {
                                            if let Some(toks) = graph.tokens.get(&(*vs, *v)) {
                                                for (tok, occ) in toks {
                                                    let weight = (tok.weight(), occ.weight());
                                                    if best == None || weight > best_weight {
                                                        best_weight = weight;
                                                        best = Some(Position::Match(
                                                            tok.clone(),
                                                            *occ,
                                                            Direction::End,
                                                        ));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // check out-edges
                                    if let Some(vfs) = idg_adj.get(v) {
                                        for vf in vfs {
                                            if let Some(toks) = graph.tokens.get(&(*v, *vf)) {
                                                for (tok, occ) in toks {
                                                    let weight = (tok.weight(), occ.weight());
                                                    if best == None || weight > best_weight {
                                                        best_weight = weight;
                                                        best = Some(Position::Match(
                                                            tok.clone(),
                                                            *occ,
                                                            Direction::Start,
                                                        ));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    // we should never get here; if we did, it means our
                                    // PositionSet was invalid
                                    best.expect("top_ranked_expression: no tokens for graph node")
                                }
                            }
                        };

                        let len: usize;
                        let mut bad = false;
                        match (p_l, p_r) {
                            (ConstantPosition(k1), ConstantPosition(k2)) => {
                                len = k2.0 as usize - k1.0 as usize;
                            }
                            (ConstantPosition(k), GraphNode(v)) => {
                                let k = k.0 as usize;
                                let mut sum = 0;
                                for (id, si) in &graph.labels[v] {
                                    if si.0 > k {
                                        // NOTE the paper isn't super clear about how to measure
                                        // the length of token-based matches (it depends on which
                                        // string we are matching); we could do it based on all the
                                        // strings (including ones we don't have input-output
                                        // examples for), or we could do it just for the ones
                                        // included in the examples; doing the former might make more
                                        // sense, so that is what we do here
                                        if id.row < self.num_examples {
                                            sum += si.0 - k;
                                        }
                                    } else {
                                        bad = true;
                                        break;
                                    }
                                }
                                len = sum / self.num_examples;
                            }
                            (GraphNode(v), ConstantPosition(k)) => {
                                // similar to the above case
                                // note: difference direction is opposite the above case
                                let k = k.0 as usize;
                                let mut sum = 0;
                                for (id, si) in &graph.labels[v] {
                                    if k > si.0 {
                                        if id.row < self.num_examples {
                                            sum += k - si.0;
                                        }
                                    } else {
                                        bad = true;
                                        break;
                                    }
                                }
                                len = sum / self.num_examples;
                            }
                            (GraphNode(v1), GraphNode(v2)) => {
                                let mut sum = 0;
                                for (id, si1) in &graph.labels[v1] {
                                    let si2 = graph.labels[v2][id];
                                    if si2.0 > si1.0 {
                                        if id.row < self.num_examples {
                                            sum += si2.0 - si1.0;
                                        }
                                    } else {
                                        bad = true;
                                        break;
                                    }
                                }
                                len = sum / self.num_examples;
                            }
                        }
                        expr = if !bad {
                            Some(SubstringExpression::Substring(
                                *ci,
                                sample(p_l),
                                sample(p_r),
                            ))
                        } else {
                            None
                        };
                        score = len * len * KAPPA;
                    }
                }
                if let Some(expr) = expr {
                    if best == None || score > best_score {
                        best = Some(expr);
                        best_score = score;
                    }
                }
            }
            if let Some(best) = best {
                best_by_edge.insert(*edge, (best_score, best));
            }
        }
        // find shortest path
        let adj = graph::adjacency_map(best_by_edge.keys());
        let path = graph::shortest_path_dag(&self.start, &self.finish, &adj, |v1, v2| {
            // negating because graph finds lowest cost path, we want highest score
            -(best_by_edge[&(*v1, *v2)].0 as isize)
        })?;
        Some(StringExpression(
            path.iter()
                .map(|e| best_by_edge.remove(e).unwrap().1)
                .collect(),
        ))
    }
}

#[derive(Debug, PartialEq, Eq)]
enum SubstringExpressionSet {
    ConstantString(String),
    SubstringSet(ColumnIndex, BTreeSet<PositionSet>, BTreeSet<PositionSet>),
}

use SubstringExpressionSet::*;

impl SubstringExpressionSet {
    // returns a SubstringSet
    fn generate_substring_set(
        id: Id,
        l: StringIndex,
        r: StringIndex,
        graph: &InputDataGraph,
    ) -> Self {
        let mut v_l = BTreeSet::new();
        let mut v_r = BTreeSet::new();
        for (v, labels) in &graph.labels {
            if labels.get(&id) == Some(&l) {
                v_l.insert(GraphNode(*v));
            } else if labels.get(&id) == Some(&r) {
                v_r.insert(GraphNode(*v));
            }
        }
        v_l.insert(ConstantPosition(Occurrence(l.0 as isize)));
        v_r.insert(ConstantPosition(Occurrence(r.0 as isize)));
        SubstringSet(ColumnIndex(id.col), v_l, v_r)
    }

    #[cfg(test)]
    fn denote(&self, graph: &InputDataGraph) -> BTreeSet<SubstringExpression> {
        let mut set: BTreeSet<SubstringExpression> = BTreeSet::new();
        match self {
            ConstantString(s) => {
                set.insert(SubstringExpression::ConstantString(s.clone()));
            }
            SubstringSet(ci, p_l, p_r) => {
                for p_l in p_l.iter().flat_map(|p_l| p_l.denote(graph)) {
                    for p_r in p_r.iter().flat_map(|p_r| p_r.denote(graph)) {
                        set.insert(SubstringExpression::Substring(
                            *ci,
                            p_l.clone(),
                            p_r.clone(),
                        ));
                    }
                }
            }
        }
        set
    }

    fn intersection(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (ConstantString(s1), ConstantString(s2)) if s1 == s2 => {
                Some(ConstantString(s1.clone()))
            }
            (SubstringSet(c1, p1_l, p1_r), SubstringSet(c2, p2_l, p2_r)) if c1 == c2 => {
                // return None if either intersection is empty; this is not necessary for
                // correctness but it's a performance optimization
                let p_l: BTreeSet<_> = p1_l.intersection(p2_l).cloned().collect();
                if p_l.is_empty() {
                    return None;
                }
                let p_r: BTreeSet<_> = p1_r.intersection(p2_r).cloned().collect();
                if p_r.is_empty() {
                    return None;
                }
                Some(SubstringSet(*c1, p_l, p_r))
            }
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
enum PositionSet {
    ConstantPosition(Occurrence),
    GraphNode(Node),
}

use PositionSet::*;

impl PositionSet {
    #[cfg(test)]
    fn denote(&self, graph: &InputDataGraph) -> BTreeSet<Position> {
        let mut set: BTreeSet<Position> = BTreeSet::new();
        match self {
            ConstantPosition(k) => {
                set.insert(Position::ConstantPosition(*k));
            }
            GraphNode(v) => {
                // find all edges that end at v or start at v
                for ((vs, vf), tok_occs) in &graph.tokens {
                    if vs == v {
                        for (tok, occ) in tok_occs {
                            set.insert(Position::Match(tok.clone(), *occ, Direction::Start));
                        }
                    } else if vf == v {
                        for (tok, occ) in tok_occs {
                            set.insert(Position::Match(tok.clone(), *occ, Direction::End));
                        }
                    }
                }
            }
        }
        set
    }
}

#[cfg(test)]
mod tests {
    use super::super::token::Token;
    use super::*;
    use crate::StringProgram;

    #[test]
    fn generate_substring_set() {
        // generate the graph from BlinkFill Fig. 14
        let strs = vec![
            vec!["Mumbai, India"],
            vec!["Los Angeles, United States of America"],
            vec!["Newark, United States"],
            vec!["New York, United States of America"],
            vec!["Wellington, New Zealand"],
            vec!["New Delhi, India"],
        ];
        let graph = InputDataGraph::new(&strs);
        // find the substring expression set that generates "India" from the 1st string
        let sub = SubstringExpressionSet::generate_substring_set(
            Id::new(0, 0),
            StringIndex(9),
            StringIndex(14),
            &graph,
        );
        let sub_denote = sub.denote(&graph);

        // spot check a couple things
        assert!(sub_denote.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(Token::ProperCase, Occurrence(2), Direction::Start),
            Position::Match(Token::ProperCase, Occurrence(2), Direction::End)
        )));
        assert!(sub_denote.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(Token::Caps, Occurrence(2), Direction::Start),
            Position::Match(Token::ProperCase, Occurrence(2), Direction::End)
        )));
        assert!(sub_denote.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(Token::Alphabets, Occurrence(-1), Direction::Start),
            Position::Match(Token::End, Occurrence(1), Direction::Start)
        )));
        // even though this next pattern would occur if the graph were built from only the first
        // string, this pattern doesn't match in the other strings, so it does not appear
        assert!(!sub_denote.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(
                Token::Literal(String::from("Mumbai, ")),
                Occurrence(1),
                Direction::End
            ),
            Position::Match(Token::End, Occurrence(1), Direction::Start)
        )));
        assert!(sub_denote.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(
                Token::Literal(String::from(", ")),
                Occurrence(1),
                Direction::End
            ),
            Position::Match(Token::End, Occurrence(1), Direction::Start)
        )));

        // make sure all the string programs generate the right string
        for prog in sub_denote {
            let output = prog.run(&vec!["Mumbai, India"]);
            assert_eq!(output.unwrap(), "India");
        }
    }

    #[test]
    fn generate_substring_set_single() {
        // similar to the negated case from above, with a different graph, should appear
        let strs = vec![vec!["Shrewsbury, MA"], vec!["Shrewsbury, United Kingdom"]];
        let graph = InputDataGraph::new(&strs);
        // find the substring expression set that generates "MA" from the 1st string
        let sub = SubstringExpressionSet::generate_substring_set(
            Id::new(0, 0),
            StringIndex(13),
            StringIndex(15),
            &graph,
        );
        let sub_denote = sub.denote(&graph);
        assert!(sub_denote.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(
                Token::Literal(String::from("Shrewsbury, ")),
                Occurrence(1),
                Direction::End
            ),
            Position::Match(Token::End, Occurrence(1), Direction::Start)
        )));
    }

    fn all_for(
        dag: &Dag,
        graph: &InputDataGraph,
        n1: Node,
        n2: Node,
    ) -> BTreeSet<SubstringExpression> {
        dag.substrings
            .get(&(n1, n2))
            .unwrap()
            .iter()
            .flat_map(|e| e.denote(&graph))
            .collect()
    }

    #[test]
    fn generate_dag() {
        let strs = vec![
            vec!["Mumbai, India"],
            vec!["Los Angeles, United States of America"],
            vec!["Newark, United States"],
            vec!["New York, United States of America"],
            vec!["Wellington, New Zealand"],
            vec!["New Delhi, India"],
        ];
        let graph = InputDataGraph::new(&strs);
        let dag = Dag::new(&strs[0], "India", &graph, 0);
        // some spot checks
        assert!(all_for(&dag, &graph, 0, 3)
            .contains(&SubstringExpression::ConstantString(String::from("Ind"))));
        assert!(
            all_for(&dag, &graph, 0, 1).contains(&SubstringExpression::Substring(
                ColumnIndex(0),
                Position::Match(Token::Caps, Occurrence(2), Direction::Start),
                Position::ConstantPosition(Occurrence(10))
            ))
        );
        assert!(
            all_for(&dag, &graph, 0, 5).contains(&SubstringExpression::Substring(
                ColumnIndex(0),
                Position::Match(Token::ProperCase, Occurrence(-1), Direction::Start),
                Position::Match(Token::End, Occurrence(1), Direction::Start),
            ))
        );
    }

    #[test]
    fn learn() {
        let strs = vec![
            vec!["Mumbai, India"],
            vec!["Los Angeles, United States of America"],
            vec!["Newark, United States"],
            vec!["New York, United States of America"],
            vec!["Wellington, New Zealand"],
            vec!["New Delhi, India"],
        ];
        let graph = InputDataGraph::new(&strs);
        let examples = vec![
            (strs[0].clone(), "India"),
            (strs[1].clone(), "United States of America"),
        ];
        let dag = Dag::learn(&examples, &graph);
        // check all expressions that extract output in one go
        let exprs = all_for(&dag, &graph, dag.start, dag.finish);
        for e in &exprs {
            for ex in &examples {
                assert_eq!(e.run(&ex.0).unwrap(), ex.1);
            }
        }
        // spot-check
        assert!(!exprs.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(Token::ProperCase, Occurrence(-1), Direction::Start),
            Position::Match(Token::End, Occurrence(1), Direction::Start),
        )));
        assert!(exprs.contains(&SubstringExpression::Substring(
            ColumnIndex(0),
            Position::Match(
                Token::Literal(String::from(", ")),
                Occurrence(1),
                Direction::End
            ),
            Position::Match(Token::End, Occurrence(1), Direction::Start),
        )));
        // check final program
        let best = dag.top_ranked_expression(&graph).unwrap();
        let expected = vec![
            "United States",
            "United States of America",
            "New Zealand",
            "India",
        ];
        for (i, s) in strs[2..].iter().enumerate() {
            assert_eq!(best.run(s).unwrap(), expected[i]);
        }
    }

    #[test]
    fn learn_2() {
        let strs = vec![
            vec!["323-708-7700"],
            vec!["(425).706.7709"],
            vec!["510.220.5586"],
            vec!["(471)-378-3829"],
        ];
        let graph = InputDataGraph::new(&strs);
        let examples = vec![
            (strs[0].clone(), "323-708-7700"),
            (strs[1].clone(), "425-706-7709"),
        ];
        let dag = Dag::learn(&examples, &graph);
        let best = dag.top_ranked_expression(&graph).unwrap();
        let expected = vec!["510-220-5586", "471-378-3829"];
        for (i, s) in strs[2..].iter().enumerate() {
            assert_eq!(best.run(s).unwrap(), expected[i]);
        }
    }

    #[test]
    fn learn_3() {
        let strs = vec![
            vec!["Brandon Henry Saunders"],
            vec!["Dafna Q. Chen"],
            vec!["William Lee"],
            vec!["Danelle D. Saunders"],
            vec!["Emilio William Conception"],
        ];
        let graph = InputDataGraph::new(&strs);
        let examples = vec![(strs[0].clone(), "B.S."), (strs[1].clone(), "D.C.")];
        let dag = Dag::learn(&examples, &graph);
        let best = dag.top_ranked_expression(&graph).unwrap();
        let expected = vec!["W.L.", "D.S.", "E.C."];
        for (i, s) in strs[2..].iter().enumerate() {
            assert_eq!(best.run(s).unwrap(), expected[i]);
        }
    }

    #[test]
    fn learn_4() {
        let strs = vec![
            vec!["GOPR0365.MP4.mp4"],
            vec!["GOPR0411.MP4.mp4"],
            vec!["GOPR0329.MP4.mp4"],
        ];
        let graph = InputDataGraph::new(&strs);
        let examples = vec![(strs[0].clone(), "GOPR0365.mp4")];
        let dag = Dag::learn(&examples, &graph);
        let best = dag.top_ranked_expression(&graph).unwrap();
        let expected = vec!["GOPR0411.mp4", "GOPR0329.mp4"];
        for (i, s) in strs[1..].iter().enumerate() {
            assert_eq!(best.run(s).unwrap(), expected[i]);
        }
    }

    #[test]
    fn learn_5() {
        let strs = vec![
            vec!["IMG_3246.JPG"],
            vec!["GOPR0411.MP4"],
            vec!["DSC_0324.jpg"],
            vec!["DSC0324.jpg"],
            vec!["RD392.HEIC"],
        ];
        let graph = InputDataGraph::new(&strs);
        let examples = vec![(strs[0].clone(), "IMG_3246")];
        let dag = Dag::learn(&examples, &graph);
        let best = dag.top_ranked_expression(&graph).unwrap();
        let expected = vec!["GOPR0411", "DSC_0324", "DSC0324", "RD392"];
        for (i, s) in strs[1..].iter().enumerate() {
            assert_eq!(best.run(s).unwrap(), expected[i]);
        }
    }

    #[test]
    fn learn_multi_column() {
        let strs = vec![
            vec!["1", "IMG_3246.JPG"],
            vec!["2", "GOPR0411.MP4"],
            vec!["3", "DSC_0324.jpg"],
            vec!["4", "DSC0324.jpg"],
            vec!["5", "RD392.HEIC"],
        ];
        let graph = InputDataGraph::new(&strs);
        let examples = vec![
            (strs[0].clone(), "1_IMG_3246"),
            (strs[1].clone(), "2_GOPR0411"),
        ];
        let dag = Dag::learn(&examples, &graph);
        let best = dag.top_ranked_expression(&graph).unwrap();
        let expected = vec!["3_DSC_0324", "4_DSC0324", "5_RD392"];
        for (i, s) in strs[2..].iter().enumerate() {
            assert_eq!(best.run(s).unwrap(), expected[i]);
        }
    }
}
