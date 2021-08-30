use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt::Debug;
use std::marker::PhantomData;

use itertools::{enumerate, sorted};
use petgraph::visit::IntoNodeReferences;
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

impl<N: GetLoopAttr> LoopMarker<N> {
    pub fn mark(graph: &mut ControlFlowGraph<N>, entry: NodeIndex) {
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

    fn clean_loop_attr(graph: &mut ControlFlowGraph<N>) {
        for attr in graph.node_weights_mut() {
            *attr.loop_attr_mut() = LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: NodeIndex::end(),
                outer: NodeIndex::end(),
            };
        }
    }

    fn dfs(&mut self, graph: &mut ControlFlowGraph<N>, current: NodeIndex) {
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

    fn mark_range(&mut self, graph: &mut ControlFlowGraph<N>, current: NodeIndex) {
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

    fn mark_range_helper(&mut self, graph: &mut ControlFlowGraph<N>, head: NodeIndex) {
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

    fn set_loop(graph: &mut ControlFlowGraph<N>, node: NodeIndex, head: NodeIndex) {
        let attr = graph.node_weight_mut(node).unwrap().loop_attr_mut();
        attr.outer = attr.inner;
        attr.inner = head;
    }
}

pub fn loop_mark<N: GetLoopAttr + Debug>(graph: &mut ControlFlowGraph<N>, entry: NodeIndex) {
    LoopMarker::mark(graph, entry);
    println!("==== loop mark");
    debug_print(graph);
}

pub struct LoopNodes<N> {
    loop_nodes: HashMap<NodeIndex, HashSet<NodeIndex>>,
    _phantom: PhantomData<N>,
}

impl<N: GetLoopAttr> LoopNodes<N> {
    pub fn new(graph: &ControlFlowGraph<N>) -> LoopNodes<N> {
        let mut loop_nodes = HashMap::new();
        for (x, attr) in graph.node_references() {
            if attr.loop_attr_ref().is_head {
                loop_nodes.insert(x, HashSet::new());
            }
        }
        for x in graph.node_indices() {
            for head in LoopNodes::nested_loops(graph, x).iter() {
                loop_nodes.get_mut(head).unwrap().insert(x);
            }
        }
        LoopNodes {
            loop_nodes,
            _phantom: PhantomData,
        }
    }

    pub fn nested_loops(graph: &ControlFlowGraph<N>, node: NodeIndex) -> Vec<NodeIndex> {
        let mut x = node;
        let mut loops = Vec::new();
        while x != NodeIndex::end() {
            let attr = &graph.node_weight(x).unwrap().loop_attr_ref();
            if attr.inner == NodeIndex::end() {
                break;
            }
            x = attr.inner;
            loops.push(x);
            x = graph.node_weight(x).unwrap().loop_attr_ref().outer;
        }
        loops
    }

    pub fn add_node(&mut self, graph: &ControlFlowGraph<N>, node: NodeIndex) {
        for head in LoopNodes::nested_loops(graph, node) {
            self.loop_nodes.get_mut(&head).unwrap().insert(node);
        }
    }

    pub fn common_loop(
        graph: &ControlFlowGraph<N>,
        node1: NodeIndex,
        node2: NodeIndex,
    ) -> NodeIndex {
        let loops1 = LoopNodes::nested_loops(graph, node1);
        let loops2 = LoopNodes::nested_loops(graph, node2);
        for x in loops1.iter().rev() {
            if loops2.contains(x) {
                return *x;
            }
        }
        NodeIndex::end()
    }

    pub fn get(&self, head: NodeIndex) -> &HashSet<NodeIndex> {
        self.loop_nodes.get(&head).unwrap()
    }

    pub fn inside_loop(&self, head: NodeIndex, node: NodeIndex) -> bool {
        self.get(head).contains(&node)
    }

    pub fn loop_exits(
        &self,
        graph: &ControlFlowGraph<N>,
        head: NodeIndex,
    ) -> Vec<(NodeIndex, NodeIndex)> {
        let mut exits = Vec::new();
        let nodes = self.get(head);
        for x in sorted(nodes) {
            let x = *x;
            for y in graph.neighbors_directed(x, EdgeDirection::Outgoing) {
                if !self.inside_loop(head, y) {
                    exits.push((x, y));
                    break;
                }
            }
        }
        exits
    }

    pub fn loop_entries(
        &self,
        graph: &ControlFlowGraph<N>,
        head: NodeIndex,
    ) -> Vec<(NodeIndex, NodeIndex)> {
        let mut entries = Vec::new();
        let nodes = self.get(head);
        for x in sorted(nodes) {
            let x = *x;
            for y in graph.neighbors_directed(x, EdgeDirection::Incoming) {
                if !self.inside_loop(head, y) {
                    entries.push((y, x));
                }
            }
        }
        entries
    }

    pub fn abnormal_entries(
        &self,
        graph: &ControlFlowGraph<N>,
        head: NodeIndex,
    ) -> Vec<(NodeIndex, NodeIndex)> {
        let mut entries = Vec::new();
        for e in self.loop_entries(graph, head) {
            if e.1 != head {
                entries.push(e);
            }
        }
        entries
    }

    pub fn node_order(graph: &ControlFlowGraph<N>) -> Vec<NodeIndex> {
        let mut order: Vec<(NodeIndex, usize)> = graph
            .node_references()
            .map(|x| (x.0, x.1.loop_attr_ref().level))
            .collect();
        order.sort_by(|a, b| a.1.cmp(&b.1));
        order.iter().map(|x| x.0).collect()
    }
}
