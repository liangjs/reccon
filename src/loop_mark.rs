use std::collections::{HashMap, HashSet, VecDeque};
use std::marker::PhantomData;

use itertools::enumerate;
use petgraph::EdgeDirection;

use crate::graph::*;

#[derive(Debug, Clone)]
pub struct LoopAttr {
    pub is_head: bool,
    pub level: usize,
    pub inner: NodeIndex,
    pub outer: NodeIndex,
}

pub trait GetLoopAttr {
    fn loop_attr_ref(&self) -> &LoopAttr;
    fn loop_attr_mut(&mut self) -> &mut LoopAttr;
}

struct LoopMarker<N> {
    visited: Vec<usize>,
    in_stack: Vec<bool>,
    node_map: HashMap<NodeIndex, usize>,
    dfn: usize,
    entry: NodeIndex,
    _phantom: PhantomData<N>,
}

type LoopGraph<N> = ControlFlowGraph<N>;

impl<N: GetLoopAttr> LoopMarker<N> {
    pub fn mark(graph: &mut LoopGraph<N>, entry: NodeIndex) {
        let n = graph.node_count();
        let mut state = LoopMarker {
            visited: vec![0; n],
            in_stack: vec![false; n],
            node_map: HashMap::new(),
            dfn: 0,
            entry,
            _phantom: PhantomData,
        };
        for (i, node) in enumerate(graph.node_indices()) {
            state.node_map.insert(node, i);
        }
        LoopMarker::clean_loop_attr(graph);
        state.dfs(graph, entry);
        state.visited = vec![0; n];
        state.in_stack = vec![false; n];
        LoopMarker::mark_range(&mut state, graph, entry);
    }

    fn clean_loop_attr(graph: &mut LoopGraph<N>) {
        for attr in graph.node_weights_mut() {
            *attr.loop_attr_mut() = LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: NodeIndex::end(),
                outer: NodeIndex::end(),
            };
        }
    }

    fn dfs(&mut self, graph: &mut LoopGraph<N>, current: NodeIndex) {
        let mapped_current = *self.node_map.get(&current).unwrap();
        self.visited[mapped_current] = 1;
        self.in_stack[mapped_current] = true;
        let nexts = ordered_neighbors(graph, current);
        for next in nexts {
            let mapped_next = *self.node_map.get(&next).unwrap();
            if self.visited[mapped_next] != 0 {
                if self.in_stack[mapped_next] {
                    /* loop-back edge */
                    graph.node_weight_mut(next).unwrap().loop_attr_mut().is_head = true;
                } else {
                    /* cross edge */
                }
            } else {
                self.dfs(graph, next);
            }
        }
        self.in_stack[mapped_current] = false;
        graph
            .node_weight_mut(current)
            .unwrap()
            .loop_attr_mut()
            .level = self.dfn;
        self.dfn += 1;
    }

    fn mark_range(&mut self, graph: &mut LoopGraph<N>, current: NodeIndex) {
        let mapped_current = *self.node_map.get(&current).unwrap();
        self.in_stack[mapped_current] = true;
        if graph.node_weight(current).unwrap().loop_attr_ref().is_head {
            self.mark_range_helper(graph, current);
        }
        let nexts = ordered_neighbors(graph, current);
        for next in nexts {
            let mapped_next = *self.node_map.get(&next).unwrap();
            self.visited[mapped_next] += 1;
            if self.visited[mapped_next] == 1 && next != self.entry {
                self.mark_range(graph, next);
            }
        }
        self.in_stack[mapped_current] = false;
    }

    fn mark_range_helper(&mut self, graph: &mut LoopGraph<N>, head: NodeIndex) {
        let flood_fill = |foward| {
            let mut queued = HashSet::new();
            let mut queue = VecDeque::new();
            queue.push_back(head);
            queued.insert(head);
            while let Some(node) = queue.pop_front() {
                let direction = if foward {
                    EdgeDirection::Outgoing
                } else {
                    EdgeDirection::Incoming
                };
                let nexts: Vec<NodeIndex> = graph.neighbors_directed(node, direction).collect();
                for next in nexts {
                    let mapped_next = *self.node_map.get(&next).unwrap();
                    if queued.contains(&next) {
                        continue;
                    }
                    if self.in_stack[mapped_next] {
                        continue;
                    }
                    queue.push_back(next);
                    queued.insert(next);
                }
            }
            queued
        };
        let fill_backward = flood_fill(false);
        let fill_forward = flood_fill(true);
        let intersect = fill_backward.intersection(&fill_forward);
        for node in intersect {
            LoopMarker::set_loop(graph, *node, head);
        }
    }

    fn set_loop(graph: &mut LoopGraph<N>, node: NodeIndex, head: NodeIndex) {
        let attr = graph.node_weight_mut(node).unwrap().loop_attr_mut();
        attr.outer = attr.inner;
        attr.inner = head;
    }
}

pub fn loop_mark<N: GetLoopAttr>(graph: &mut LoopGraph<N>, entry: NodeIndex) {
    LoopMarker::mark(graph, entry);
}
