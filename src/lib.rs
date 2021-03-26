mod dag;
mod graph;
mod input_data_graph;
mod language;
mod token;

use dag::Dag;
use input_data_graph::InputDataGraph;
pub use language::StringProgram;

pub fn learn(
    all_unpaired: &[Vec<String>],
    examples: &[(Vec<String>, String)],
) -> Option<impl StringProgram> {
    let graph = InputDataGraph::new(all_unpaired);
    let dag = Dag::learn(examples, &graph);
    dag.top_ranked_expression(&graph)
}
