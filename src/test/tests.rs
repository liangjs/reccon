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
fn test_loop_exit() {
    test_graph(
        10,
        5,
        vec![
            (5, 4),
            (5, 9),
            (4, 3),
            (4, 1),
            (3, 6),
            (3, 6),
            (1, 2),
            (6, 0),
            (6, 8),
            (2, 5),
            (2, 4),
            (8, 7),
            (8, 4),
        ],
    );
    test_graph(
        10,
        6,
        vec![
            (6, 7),
            (6, 9),
            (7, 3),
            (3, 0),
            (3, 0),
            (0, 4),
            (0, 1),
            (4, 2),
            (4, 6),
            (1, 8),
            (1, 5),
            (5, 0),
            (5, 3),
        ],
    )
}

#[test]
fn test_loop_condition() {
    test_graph(
        10,
        0,
        vec![
            (0, 2),
            (0, 7),
            (2, 8),
            (2, 9),
            (7, 4),
            (7, 6),
            (8, 1),
            (1, 3),
            (1, 2),
            (3, 0),
            (3, 3),
            (4, 5),
            (4, 4),
        ],
    );
}

#[test]
fn test_dfs_order() {
    test_graph(
        9,
        7,
        vec![
            (7, 3),
            (7, 6),
            (3, 0),
            (3, 8),
            (6, 5),
            (6, 4),
            (0, 2),
            (5, 1),
            (5, 2),
            (1, 0),
            (2, 6),
            (8, 7),
        ],
    );
    test_graph(
        10,
        6,
        vec![
            (6, 0),
            (6, 7),
            (0, 3),
            (0, 9),
            (7, 5),
            (7, 2),
            (3, 1),
            (3, 8),
            (5, 6),
            (8, 4),
            (8, 4),
            (2, 3),
            (4, 0),
        ],
    );
    test_graph(
        10,
        3,
        vec![
            (3, 9),
            (3, 2),
            (9, 6),
            (9, 5),
            (6, 1),
            (6, 8),
            (5, 7),
            (5, 9),
            (7, 8),
            (1, 4),
            (8, 4),
            (4, 0),
            (0, 7),
            (0, 6),
        ],
    );
}

#[test]
fn random_test() {
    let tests = [(10, 0.5, 100)];
    for (node_num, density, times) in tests.iter() {
        for _ in 0..*times {
            let mut seed: <ChaCha8Rng as SeedableRng>::Seed = Default::default();
            thread_rng().fill(&mut seed);
            println!("random_test_seeded({:?}, {}, {})", seed, node_num, density);
            random_test_seeded(seed, *node_num, *density);
        }
    }
}
