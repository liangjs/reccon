use crate::graph::*;
use crate::*;

#[test]
fn test_overlap_loop() {
    let graph: StaticGraph<String> =
        StaticGraph::new(5, vec![(0, 1), (1, 2), (2, 0), (2, 3), (3, 1), (3, 4)]);
    let results = loop_structure(&graph, 0).unwrap();
    assert_eq!(results.graph.node_num(), 10);
}

#[test]
fn test_exch_loop() {
    let graph: StaticGraph<String> =
        StaticGraph::new(5, vec![(0, 1), (0, 3), (1, 2), (1, 4), (2, 1), (2, 3), (3, 2)]);
    let results = loop_structure(&graph, 0).unwrap();
    assert_eq!(results.graph.node_num(), 8);
}