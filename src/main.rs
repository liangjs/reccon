use reccon::graph::StaticGraph;

extern crate reccon;

fn main() {
    let mut graph: StaticGraph<String> = StaticGraph::new_from_default(7);
    graph.add_edge(0, 1);
    graph.add_edge(0, 2);
    graph.add_edge(1, 3);
    graph.add_edge(1, 4);
    graph.add_edge(2, 4);
    graph.add_edge(2, 5);
    graph.add_edge(3, 6);
    graph.add_edge(4, 6);
    graph.add_edge(4, 0);
    graph.add_edge(6, 1);
    graph.add_edge(6, 5);
    /*
    let mut graph: StaticGraph<String> = StaticGraph::new_from_default(5);
    graph.add_edge(0, 1);
    graph.add_edge(0, 3);
    graph.add_edge(1, 2);
    graph.add_edge(2, 4);
    graph.add_edge(2, 0);
    graph.add_edge(3, 4);
    */
    //let graph: StaticGraph<String> =
    //    StaticGraph::new(5, vec![(0, 1), (1, 2), (2, 0), (2, 3), (3, 1), (3, 4)]);
    //let graph: StaticGraph<String> =
    //    StaticGraph::new(5, vec![(0, 1), (0, 3), (1, 2), (1, 4), (2, 1), (2, 3), (3, 2)]);
    /*
    let graph: StaticGraph<String> = StaticGraph::new(
        6,
        vec![
            (0, 1),
            (1, 2),
            (2, 3),
            (2, 0),
            (3, 1),
            (3, 4),
            (4, 0),
            (4, 5),
        ],
    );
    */
    /*
    let graph: StaticGraph<String> = StaticGraph::new(
        7,
        vec![
            (0, 6),
            (6, 1),
            (6, 2),
            (1, 2),
            (2, 3),
            (3, 1),
            (3, 4),
            (4, 0),
            (4, 5),
        ],
    );
    */

    //let result = reccon::loop_structure(&graph, 0).unwrap();
    //println!("entry: {}", result.entry);
    //println!("vars: {:?}", result.new_vars);
    //reccon::graph::debug_print(&result.graph);
    //reccon::graph::dot_graph(&result.graph, result.entry);

    let result = reccon::reconstruct(&graph, 0).unwrap();
    println!("vars: {:?}", result.new_vars);
    println!("{}", result.stmt.to_string());
}
