mod dag;
mod graph;
mod input_data_graph;
mod language;
mod token;

use dag::Dag;
use input_data_graph::InputDataGraph;
pub use language::StringProgram;

pub fn learn(
    unpaired: &[Vec<String>],
    examples: &[(Vec<String>, String)],
) -> Option<impl StringProgram> {
    // NOTE we construct all_unpaired here rather than having the caller pass it in, so we can
    // enforce the precondition of Dag::learn that the indices of the examples correspond to the
    // indices used in constructing the graph (so all unpaired has to be examples concatenated with
    // the unpaired, in that order)
    let all_unpaired: Vec<_> = examples
        .iter()
        .map(|(row, _)| row)
        .chain(unpaired.iter())
        .cloned()
        .collect();
    let graph = InputDataGraph::new(&all_unpaired);
    let dag = Dag::learn(examples, &graph);
    dag.top_ranked_expression(&graph)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn end_to_end() {
        let data = vec![
            vec![
                "1",
                "The Expanse S01E01 1080p HDTV x264-BRISK",
                "The Expanse S01E01 (1080p)",
            ],
            vec!["2", "The Expanse S03E02 720p HDTV x264-SVA", ""],
            vec!["3", "The Expanse S03E03 720p HDTV x264-FLEET", ""],
            vec!["4", "The Expanse S01E04 1080p HDTV x264-BRISK", ""],
            vec!["5", "The Expanse S02E04 720p HDTV x264-SVA", ""],
            vec!["6", "The Expanse S01E05 720p HDTV x264-KILLERS", ""],
            vec![
                "7",
                "The Expanse S01E06 INTERNAL 720p HDTV x264-KILLERS",
                "The Expanse S01E06 (720p)",
            ],
            vec!["8", "The Expanse S03E07 PROPER 2160p HDTV x264-AVS", ""],
            vec!["9", "The Expanse S03E08 REPACK 720p HDTV x264-LucidTV", ""],
            vec!["10", "The Expanse S03E09 REPACK 720p HDTV x264-LucidTV", ""],
            vec!["11", "The Expanse S03E10 REPACK 720p HDTV x264-LucidTV", ""],
            vec!["12", "The Expanse S03E11 REPACK 720p HDTV x264-LucidTV", ""],
            vec!["13", "The Expanse S03E12 REPACK 720p HDTV x264-LucidTV", ""],
            vec!["14", "The Expanse S03E13 REPACK 720p HDTV x264-LucidTV", ""],
        ];
        let unpaired: Vec<Vec<String>> = data
            .iter()
            .filter(|row| row[row.len() - 1] == "")
            .map(|row| {
                let mut row: Vec<String> = row.iter().map(|s| String::from(*s)).collect();
                row.pop();
                row
            })
            .collect();
        let examples: Vec<(Vec<String>, String)> = data
            .iter()
            .filter(|row| row[row.len() - 1] != "")
            .map(|row| {
                let mut row: Vec<String> = row.iter().map(|s| String::from(*s)).collect();
                let last = row.pop().unwrap();
                (row, last)
            })
            .collect();
        let prog = learn(&unpaired, &examples).unwrap();
        let results: Vec<_> = unpaired.iter().map(|row| prog.run(row).unwrap()).collect();
        assert_eq!(results[0], "The Expanse S03E02 (720p)");
        assert_eq!(results[5], "The Expanse S03E07 (2160p)");
    }
}
