use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;
use std::ops::Add;

type Edge<N> = (N, N);
type AdjacencyMap<N> = HashMap<N, HashSet<N>>;

pub fn adjacency_map<'a, N>(edges: impl Iterator<Item = &'a Edge<N>>) -> AdjacencyMap<N>
where
    N: Eq + Copy + Hash + 'a,
{
    let mut adj = HashMap::new();
    for (v1, v2) in edges {
        adj.entry(*v1).or_insert(HashSet::new()).insert(*v2);
    }
    adj
}

pub fn invert_adjacency_map<N>(adj: &AdjacencyMap<N>) -> AdjacencyMap<N>
where
    N: Eq + Copy + Hash,
{
    let mut inv = HashMap::new();
    for (v1, vs) in adj {
        for v2 in vs {
            inv.entry(*v2).or_insert(HashSet::new()).insert(*v1);
        }
    }
    inv
}

pub fn topological_sort<N>(adj: &AdjacencyMap<N>) -> Vec<N>
where
    N: Eq + Copy + Hash,
{
    let mut in_degree: HashMap<&N, usize> = HashMap::new();
    for (n1, ns) in adj {
        in_degree.entry(&n1).or_insert(0);
        for n2 in ns {
            *in_degree.entry(&n2).or_insert(0) += 1;
        }
    }
    let mut pending: VecDeque<&N> = in_degree
        .iter()
        .filter(|(_, &deg)| deg == 0)
        .map(|e| e.0)
        .cloned()
        .collect();
    let mut results: Vec<N> = Vec::with_capacity(adj.len());
    let empty = HashSet::new();
    while let Some(n) = pending.pop_front() {
        results.push(*n);
        for n2 in adj.get(n).unwrap_or(&empty) {
            let deg = in_degree.get_mut(&n2).unwrap();
            if *deg == 1 {
                pending.push_front(n2);
            }
            *deg -= 1;
        }
    }
    results
}

pub fn shortest_path_dag<N, F, C>(
    start: &N,
    end: &N,
    adj: &AdjacencyMap<N>,
    mut cost: F,
) -> Option<Vec<Edge<N>>>
where
    N: Eq + Copy + Hash,
    F: FnMut(&N, &N) -> C,
    C: Copy + Ord + Add<Output = C>,
{
    let inv = invert_adjacency_map(adj);
    // single-source-shortest-path, DP solution, go in topological sort order
    let topo = topological_sort(adj);
    let index: HashMap<_, _> = topo.iter().enumerate().map(|(i, v)| (v, i)).collect();
    if !index.contains_key(start) || !index.contains_key(end) {
        return None;
    }
    let mut best: Vec<Option<(C, &N)>> = Vec::with_capacity(index[end] - index[start]);
    let start_i = index[start];
    let end_i = index[end];
    let empty = HashSet::new();
    for i in start_i + 1..end_i + 1 {
        let here = &topo[i];
        let mut best_here = None;
        // check to see if start node connects here
        if inv.get(here).unwrap_or(&empty).contains(start) {
            best_here = Some((cost(start, here), start));
        }
        // check all other predecessors in topological order
        for j in start_i + 1..i {
            let intermediate = &topo[j];
            if inv.get(here).unwrap_or(&empty).contains(&intermediate) {
                if let Some((c, _)) = best[j - start_i - 1] {
                    let jump_cost = c + cost(intermediate, here);
                    match best_here {
                        None => {
                            best_here = Some((jump_cost, intermediate));
                        }
                        Some((c, _)) => {
                            if c > jump_cost {
                                best_here = Some((jump_cost, intermediate));
                            }
                        }
                    }
                }
            }
        }
        best.push(best_here);
    }
    // reconstruct the actual path
    let mut path = Vec::new();
    if best[best.len() - 1] == None {
        return None;
    }
    let mut curr_i = end_i;
    while curr_i != start_i {
        let prev = best[curr_i - start_i - 1].unwrap().1;
        path.push((*prev, topo[curr_i]));
        curr_i = index[prev];
    }
    path.reverse();
    Some(path)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_topo<N: Eq + Hash>(edges: &[(N, N)], sort: &[N]) -> bool {
        let index: HashMap<_, _> = sort.iter().enumerate().map(|(i, v)| (v, i)).collect();
        for (v1, v2) in edges {
            if index[v1] > index[v2] {
                return false;
            }
        }
        true
    }

    #[test]
    fn test_topological_sort() {
        let edges_list = vec![
            vec![(5, 2), (5, 0), (4, 0), (4, 1), (2, 3), (3, 1)],
            vec![(0, 1), (1, 2), (2, 3), (3, 4)],
            vec![
                (0, 1),
                (0, 2),
                (0, 3),
                (0, 4),
                (0, 5),
                (1, 2),
                (1, 3),
                (1, 4),
                (1, 5),
                (2, 3),
                (2, 4),
                (2, 5),
                (3, 4),
                (3, 5),
                (4, 5),
            ],
        ];
        for edges in edges_list {
            let adj = adjacency_map(edges.iter());
            let sort = topological_sort(&adj);
            assert!(check_topo(&edges, &sort));
        }
    }

    #[test]
    fn test_shortest_path_dag() {
        let edges = vec![(5, 2), (5, 0), (4, 0), (4, 1), (2, 3), (3, 1), (5, 3)];
        let adj = adjacency_map(edges.iter());
        let path = shortest_path_dag(&5, &1, &adj, |_, _| 1);
        assert_eq!(path.unwrap(), vec![(5, 3), (3, 1)]);
    }

    #[test]
    fn test_shortest_path_dag_weights() {
        let edges = vec![(5, 2), (5, 0), (4, 0), (4, 1), (2, 3), (3, 1), (5, 3)];
        let adj = adjacency_map(edges.iter());
        let path = shortest_path_dag(&5, &1, &adj, |n1, n2| {
            // make even-numbered paths expensive
            if (n1 - n2) % 2 == 0 {
                100
            } else {
                1
            }
        });
        assert_eq!(path.unwrap(), vec![(5, 2), (2, 3), (3, 1)]);
    }
}
