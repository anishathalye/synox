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
