//! Implements the [BlinkFill][blinkfill-paper] algorithm for learning syntactic string
//! transformations.
//!
//! This crate implements [BlinkFill (Singh '16, in Proc. VLDB)][blinkfill-paper], an algorithm to
//! learn syntactic string transformations from input-output examples along with unpaired examples.
//! The learned programs belong to a fairly restricted language. The language includes features
//! like extracting substrings based on token matches, but it does not include features like loops.
//! For this reason, there are certain transformations on strings that cannot be learned by
//! BlinkFill, such as "remove all the spaces from a string". Furthermore, BlinkFill programs are
//! purely syntactic transformations, so semantic transformations like mapping "3" to "March"
//! cannot be learned. Nevertheless, BlinkFill works well on many practical string transformation
//! tasks.
//!
//! [blinkfill-paper]: http://www.vldb.org/pvldb/vol9/p816-singh.pdf

use crate::StringProgram;

mod dag;
mod input_data_graph;
mod language;
mod token;

use dag::Dag;
use input_data_graph::InputDataGraph;

/// Learns a string program using the BlinkFill algorithm.
///
/// BlinkFill learns from input-output examples as well as unpaired examples, where only the input
/// is available and the output is unknown. Performance decreases as the number of unpaired
/// examples (as well as input-output examples) grows, so if a large number of unpaired examples
/// are available, it is recommended to use a random subset of them for inference.
///
/// Duplication should be avoided between the unpaired inputs and paired examples. All of the
/// examples should have the same number of columns.
///
/// This function returns `None` if no string program satisfying all of the input-output examples
/// can be found.
///
/// # Example: extracting area code from phone numbers in mixed formats
///
/// ```
/// use synox::StringProgram;
/// use synox::blinkfill;
///
/// # fn main() -> Result<(), ()> {
/// #   assert_eq!(example(), Some(()));
/// #   Ok(())
/// # }
/// # fn example() -> Option<()> {
/// // lots of unpaired data might be available
/// let unpaired = &[vec!["508-243-5835"],
///                  vec!["(204) 447-3924"],
///                  vec!["(617)-253-1234"],
///                  vec!["+1 212-456-7890"],
///                  vec!["1-212-567-3828"]];
/// // and less paired data might be available
/// let examples = &[(vec!["(123)-456-7890"],  "123"),
///                  (vec!["+1 234-567-8901"], "234")];
///
/// let prog = blinkfill::learn(unpaired, examples)?;
///
/// let result = prog.run(&["(617) 253-1337"])?;
/// assert_eq!(result, "617");
/// # Some(())
/// # }
/// ```
///
/// # Panics
///
/// Panics if `examples` is empty or if the `examples` and `unpaired` data do not all have the same
/// number of columns.
pub fn learn<S0, S1, S2>(
    unpaired: &[Vec<S0>],
    examples: &[(Vec<S1>, S2)],
) -> Option<impl StringProgram>
where
    S0: AsRef<str>,
    S1: AsRef<str>,
    S2: AsRef<str>,
{
    // check length of all examples for consistency
    if examples.is_empty() {
        panic!("learn given no input-output examples");
    }
    let cols = examples[0].0.len();
    for (i, (ex, _)) in examples.iter().enumerate() {
        if ex.len() != cols {
            panic!("incorrect column count in input-output example {}", i);
        }
    }
    for (i, ex) in unpaired.iter().enumerate() {
        if ex.len() != cols {
            panic!("incorrect column count in unpaired example {}", i);
        }
    }

    // NOTE we construct all_unpaired here rather than having the caller pass it in, so we can
    // enforce the precondition of Dag::learn that the indices of the examples correspond to the
    // indices used in constructing the graph (so all unpaired has to be examples concatenated with
    // the unpaired, in that order)
    let unpaired = unpaired
        .iter()
        .map(|row| row.iter().map(|x| x.as_ref()).collect());
    let examples: Vec<(Vec<&str>, &str)> = examples
        .iter()
        .map(|(row, output)| (row.iter().map(|x| x.as_ref()).collect(), output.as_ref()))
        .collect();
    let all_unpaired: Vec<Vec<&str>> = examples
        .clone()
        .into_iter()
        .map(|(row, _)| row)
        .chain(unpaired.into_iter())
        .collect();
    let graph = InputDataGraph::new(&all_unpaired);
    let dag = Dag::learn(&examples, &graph);
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

    #[test]
    fn end_to_end_2() {
        let examples = vec![
            (
                vec!["Barcelona_January_2016-DSC02368.JPG"],
                "2016/January/Barcelona/DSC02368.JPG",
            ),
            (
                vec!["Smuggler's Notch_February_2017-GOPR0238.mp4"],
                "2017/February/Smuggler's Notch/GOPR0238.mp4",
            ),
        ];
        let unpaired = vec![
            vec!["Barcelona_January_2016-DSC04239.JPG"],
            vec!["Barcelona_January_2016-IMG_4728.MOV"],
            vec!["Barcelona_January_2016-IMG_4669.JPG"],
            vec!["Smuggler's Notch_February_2017-DSC02558.JPG"],
            vec!["Jay Peak_January_2019-GOPR0328.MP4"],
            vec!["Jay Peak_January_2019-IMG_3669.mov"],
            vec!["Jay Peak_January_2019-DSC0268.jpg"],
            vec!["Inyo National Forest_August_2019-DSC3583.jpg"],
            vec!["Inyo National Forest_August_2019-IMG_2456.HEIC"],
            vec!["Inyo National Forest_August_2019-IMG_2458.MOV"],
            vec!["Prague_August_2021-IMG_2618.JPG"],
            vec!["Prague_August_2021-DSC01809.JPG"],
            vec!["Prague_August_2021-DSC01952.JPG"],
            vec!["Prague_August_2021-IMG_2618.JPG"],
        ];
        let prog = learn(&unpaired, &examples).unwrap();
        let expected = vec![
            "2016/January/Barcelona/DSC04239.JPG",
            "2016/January/Barcelona/IMG_4728.MOV",
            "2016/January/Barcelona/IMG_4669.JPG",
            "2017/February/Smuggler's Notch/DSC02558.JPG",
            "2019/January/Jay Peak/GOPR0328.MP4",
            "2019/January/Jay Peak/IMG_3669.mov",
            "2019/January/Jay Peak/DSC0268.jpg",
            "2019/August/Inyo National Forest/DSC3583.jpg",
            "2019/August/Inyo National Forest/IMG_2456.HEIC",
            "2019/August/Inyo National Forest/IMG_2458.MOV",
            "2021/August/Prague/IMG_2618.JPG",
            "2021/August/Prague/DSC01809.JPG",
            "2021/August/Prague/DSC01952.JPG",
            "2021/August/Prague/IMG_2618.JPG",
        ];
        for (i, input) in unpaired.iter().enumerate() {
            assert_eq!(prog.run(input).unwrap(), expected[i]);
        }
    }
}
