use std::fmt::Debug;

use petgraph::dot;
pub use petgraph::graph::NodeIndex;
use petgraph::visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences};

#[derive(Debug, Clone, Copy)]
pub enum ControlFlowEdge {
    NotBranch,
    Branch(bool),
}

pub type ControlFlowGraph<N> = petgraph::stable_graph::StableGraph<
    N,
    ControlFlowEdge,
    petgraph::Directed,
    petgraph::graph::DefaultIx,
>;

pub fn debug_print<N: Debug>(graph: &ControlFlowGraph<N>) {
    println!("node num: {}", graph.node_count());
    for (i, weight) in graph.node_references() {
        println!("node {}: {:?}", i.index(), weight);
    }
    for e in graph.edge_references() {
        println!("{} -> {}", e.source().index(), e.target().index());
    }
}

pub fn dot_view<'a, N>(graph: &'a ControlFlowGraph<N>, entry: NodeIndex) -> String
where
    N: ToString + Debug,
{
    let config = [dot::Config::EdgeNoLabel, dot::Config::NodeNoLabel];
    let edge_attr = |_graph: &'a ControlFlowGraph<N>,
                     edge: <&'a ControlFlowGraph<N> as IntoEdgeReferences>::EdgeRef|
     -> String {
        let color = match edge.weight() {
            ControlFlowEdge::NotBranch => "color=black",
            ControlFlowEdge::Branch(br_type) => {
                if *br_type {
                    "color=blue"
                } else {
                    "color=red"
                }
            }
        };
        color.to_string()
    };
    let node_attr = |graph: &'a ControlFlowGraph<N>,
                     node: <&'a ControlFlowGraph<N> as IntoNodeReferences>::NodeRef|
     -> String {
        let shape = if node.0 == entry {
            "shape=circle"
        } else if graph.edges(node.0).count() == 2 {
            "shape=diamond"
        } else {
            "shape=box"
        };
        /*
        let bgcolor = if node.0 == entry {
            "stype=filled,fillcolor=aqua"
        } else {
            "sytle=filled,fillcolor=white"
        };
        */
        let label: &str = &format!("label=\"{}\"", node.1.to_string());
        [shape, label].join(",")
    };
    let view = dot::Dot::with_attr_getters(graph, &config, &edge_attr, &node_attr);
    format!("{:?}", view)
}

pub fn ordered_neighbors<N>(graph: &ControlFlowGraph<N>, source: NodeIndex) -> Vec<NodeIndex> {
    let mut edge_iter = graph.edges(source);
    match edge_iter.next() {
        None => vec![],
        Some(e1) => match edge_iter.next() {
            None => vec![e1.target()],
            Some(e2) => match *e1.weight() {
                ControlFlowEdge::NotBranch => panic!("graph branch edge error"),
                ControlFlowEdge::Branch(br_type) => match br_type {
                    false => vec![e1.target(), e2.target()],
                    true => vec![e2.target(), e1.target()],
                },
            },
        },
    }
}
