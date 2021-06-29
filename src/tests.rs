use std::iter::FromIterator;

use itertools::Itertools;
use rand::Rng;
use rand::prelude::SliceRandom;

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

#[test]
fn test_double_edge() {
    let graph: StaticGraph<String> =
        StaticGraph::new(2, vec![(0, 1), (0, 1)]);
    reconstruct(&graph, 0).unwrap();
}

fn random_graph(node_num: usize, density: f64) -> (StaticGraph<String>, usize) {
    let mut rng = rand::thread_rng();
    let mut edges: Vec<Vec<usize>> = Vec::with_capacity(node_num);
    edges.resize(node_num, Default::default());

    /* first, generate a random tree */
    for i in 1..node_num {
        loop {
            let fr = rng.gen_range(0..i);
            if edges[fr].len() < 2 {
                edges[fr].push(i);
                break;
            }
        }
    }

    /* add random edges */
    let add_edge_num = (node_num as f64 * density).ceil() as usize;
    for _ in 0..add_edge_num {
        let mut x = usize::MAX;
        let mut fail_cnt: usize = 0;
        while fail_cnt < node_num {
            x = rng.gen_range(0..node_num);
            if edges[x].len() < 2 {
                break;
            }
            fail_cnt += 1;
        }
        if x == usize::MAX {
            break;
        }
        let y = rng.gen_range(0..node_num);
        edges[x].push(y);
    }

    /* randomize node number */
    let mut order = Vec::from_iter(0..node_num);
    order.shuffle(&mut rng);

    /* collect edges */
    let mut edges_vec: Vec<(usize, usize)> = Vec::new();
    for x in 0..node_num {
        for y in edges[x].iter() {
            let y = *y;
            edges_vec.push((order[x], order[y]));
        }
    }
    let mut graph: StaticGraph<String> = StaticGraph::new(node_num, edges_vec);
    for i in graph.node_iter().collect_vec() {
        let note = graph.get_note_mut(i);
        *note = i.to_string();
    }
    let entry = order[0];
    (graph, entry)
}

#[test]
fn random_test()
{
    let node_num = 5;
    let density = 0.4;
    let (graph, entry) = random_graph(node_num, density);
    let result = reconstruct(&graph, entry);
    if result.is_none() {
        dot_graph(&graph, entry);
        panic!("reconstruct failed");
    }
}