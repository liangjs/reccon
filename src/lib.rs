pub mod graph;
pub mod ast;

mod loop_utils;

mod rec_loop;
use std::collections::HashMap;

use graph::ControlFlowGraph;
use petgraph::{graph::NodeIndex, visit::IntoNodeReferences};
pub use rec_loop::*;

mod rec_ast;
pub use rec_ast::*;

pub struct RecResult {
    pub stmt: ast::Statement,
    pub new_vars: Vec<String>,
}

pub fn reconstruct<N>(graph: &ControlFlowGraph<N>, entry: NodeIndex) -> Option<RecResult> {
    let loop_result = loop_structure(graph, entry)?;
    let ast_result = ast_structure(&loop_result.graph, loop_result.entry)?;

    let mut map = HashMap::new();
    for (node, ast) in loop_result.graph.node_references() {
        map.insert(node, ast);
    }

    let mut new_vars = Vec::new();
    new_vars.extend(loop_result.new_vars.clone());
    new_vars.extend(ast_result.new_vars.clone());

    Some(RecResult {
        stmt: ast_result.stmt.unfold(&map),
        new_vars
    })
}

#[cfg(test)]
mod test;