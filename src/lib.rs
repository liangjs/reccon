pub mod ast;
pub mod graph;

mod loop_utils;

mod rec_loop;
use std::collections::HashMap;

use petgraph::graph::NodeIndex;
use petgraph::visit::IntoNodeReferences;
pub use rec_loop::*;

mod rec_ast;
pub use rec_ast::*;

pub struct RecResult {
    pub stmt: ast::Statement,
    pub new_vars: Vec<String>,
}

pub fn reconstruct<G: graph::CFG>(graph: G, entry: NodeIndex) -> Option<RecResult> {
    let loop_result = loop_structure(graph, entry)?;
    let ast_result = ast_structure(&loop_result.graph, loop_result.entry)?;

    let mut ast_map = HashMap::new();
    for (node, ast) in loop_result.graph.node_references() {
        ast_map.insert(node, ast);
    }

    let mut new_vars = Vec::new();
    new_vars.extend(loop_result.new_vars.clone());
    new_vars.extend(ast_result.new_vars.clone());

    Some(RecResult {
        stmt: ast_result.stmt.unfold(&ast_map),
        new_vars,
    })
}

#[cfg(test)]
mod test;
