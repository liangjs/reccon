use rand::prelude::*;
use rand::Rng;
use rand_chacha::ChaCha8Rng;

use crate::*;

use super::utils::*;

#[test]
fn test_overlap_loop() {
    let (graph, entry) = build_graph(5, 0, vec![(0, 1), (1, 2), (2, 0), (2, 3), (3, 1), (3, 4)]);
    let result = loop_structure(&graph, entry).unwrap();
    assert_eq!(result.graph.node_count(), 10);
}

#[test]
fn test_exch_loop() {
    let (graph, entry) = build_graph(
        5,
        0,
        vec![(0, 1), (0, 3), (1, 2), (1, 4), (2, 1), (2, 3), (3, 2)],
    );
    let result = loop_structure(&graph, entry).unwrap();
    assert_eq!(result.graph.node_count(), 8);
}

#[test]
fn test_double_edge() {
    test_graph(2, 0, vec![(0, 1), (0, 1)]);
}

#[test]
fn test_abnormal_selection() {
    test_graph(5, 0, vec![(0, 1), (0, 2), (1, 3), (1, 4), (2, 3), (2, 4)]);
}

#[test]
fn test_same_loop_exit() {
    test_graph(
        10,
        0,
        vec![
            (0, 1),
            (0, 7),
            (1, 2),
            (2, 3),
            (2, 7),
            (3, 4),
            (3, 7),
            (4, 5),
            (4, 6),
            (5, 6),
            (6, 2),
            (7, 8),
            (7, 9),
            (8, 9),
        ],
    );
}

#[test]
fn test_complex1() {
    test_graph(
        9,
        0,
        vec![
            (0, 1),
            (0, 2),
            (1, 3),
            (1, 4),
            (2, 4),
            (2, 5),
            (3, 6),
            (4, 6),
            (4, 0),
            (5, 7),
            (6, 1),
            (6, 8),
            (7, 8),
        ],
    );
}

#[test]
fn test_mixed_abnormal_selection() {
    test_graph(
        6,
        0,
        vec![
            (0, 5),
            (0, 1),
            (5, 3),
            (5, 2),
            (1, 4),
            (1, 2),
            (2, 3),
            (2, 0),
        ],
    );
}

#[test]
fn random_test() {
    let tests = [(6, 0.4, 100)];
    for (node_num, density, times) in tests.iter() {
        for _ in 0..*times {
            let mut seed: <ChaCha8Rng as SeedableRng>::Seed = Default::default();
            thread_rng().fill(&mut seed);
            println!("random_test_seeded({:?}, {}, {})", seed, node_num, density);
            random_test_seeded(seed, *node_num, *density);
        }
    }
}