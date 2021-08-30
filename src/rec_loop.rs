use std::collections::{HashMap, HashSet, VecDeque};
use std::usize;

use itertools::sorted;
use itertools::Itertools;
use petgraph::visit::EdgeRef;
use petgraph::visit::{IntoEdgeReferences, IntoNodeReferences};
use petgraph::EdgeDirection;

use crate::ast::*;
use crate::graph::*;
use crate::loop_mark::*;

const VAR_PREFIX: &str = "break_";

pub struct LoopRecOutput {
    pub graph: ControlFlowGraph<AST>,
    pub entry: NodeIndex,
    pub new_vars: Vec<String>,
}

pub fn loop_structure<N>(graph: &ControlFlowGraph<N>, entry: NodeIndex) -> Option<LoopRecOutput> {
    let (mut loop_graph, entry) = construct_loop_graph(graph, entry)?;

    loop_mark(&mut loop_graph, entry);
    println!("===== mark1");
    debug_print(&loop_graph);

    let NormalizeResult {
        entry,
        new_vars: new_vars1,
    } = LoopNormalizer::normalize_exit(&mut loop_graph, entry);
    println!("===== normal exit");
    debug_print(&loop_graph);

    loop_mark(&mut loop_graph, entry);
    println!("===== mark2");
    debug_print(&loop_graph);
    println!("{}", dot_view(&loop_graph, entry));

    let NormalizeResult {
        entry,
        new_vars: mut new_vars2,
    } = LoopNormalizer::normalize_entry(&mut loop_graph, entry);
    println!("===== normal entry");
    debug_print(&loop_graph);
    println!("{}", dot_view(&loop_graph, entry));

    let out_graph = construct_out_graph(&loop_graph);
    let mut new_vars = new_vars1;
    new_vars.append(&mut new_vars2);
    Some(LoopRecOutput {
        graph: out_graph,
        entry,
        new_vars,
    })
}

fn construct_loop_graph<N>(
    graph: &ControlFlowGraph<N>,
    entry: NodeIndex,
) -> Option<(LoopGraph, NodeIndex)> {
    let mut loop_graph = LoopGraph::new();
    let mut node_map = HashMap::new();
    for x in graph.node_indices() {
        let is_branch = graph.edges(x).count() == 2;
        let node = loop_graph.add_node(NodeAttr::new(x, is_branch));
        node_map.insert(x, node);
    }
    for e in graph.edge_references() {
        let x = e.source();
        let y = e.target();
        let node_x = node_map.get(&x).unwrap();
        let node_y = node_map.get(&y).unwrap();
        loop_graph.add_edge(*node_x, *node_y, *e.weight());
    }
    let new_entry = node_map.get(&entry).unwrap();
    Some((loop_graph, *new_entry))
}

fn construct_out_graph(graph: &LoopGraph) -> ControlFlowGraph<AST> {
    let mut out_graph = ControlFlowGraph::new();
    let mut node_map = HashMap::new();
    for x in graph.node_indices() {
        let ast = &graph.node_weight(x).unwrap().ast;
        let map_x = out_graph.add_node(ast.clone());
        node_map.insert(x, map_x);
    }
    for x in graph.node_indices() {
        let map_x = node_map.get(&x).unwrap();
        for e in graph.edges(x) {
            let y = e.target();
            let map_y = node_map.get(&y).unwrap();
            out_graph.add_edge(*map_x, *map_y, *e.weight());
        }
    }
    out_graph
}

#[derive(Debug, Clone)]
struct NodeAttr {
    loop_attr: LoopAttr,
    ast: AST,
}

impl ToString for NodeAttr {
    fn to_string(&self) -> String {
        self.ast.to_string()
    }
}

impl GetLoopAttr for NodeAttr {
    fn loop_attr_ref(&self) -> &LoopAttr {
        &self.loop_attr
    }

    fn loop_attr_mut(&mut self) -> &mut LoopAttr {
        &mut self.loop_attr
    }
}

impl NodeAttr {
    pub fn new(origin: NodeIndex, is_branch: bool) -> NodeAttr {
        NodeAttr {
            loop_attr: LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: NodeIndex::end(),
                outer: NodeIndex::end(),
            },
            ast: match is_branch {
                false => AST::AState(Statement::Original { node_idx: origin }),
                true => AST::ABool(BoolExpr::Original { node_idx: origin }),
            },
        }
    }

    pub fn new_node(inner_loop: NodeIndex, ast: AST) -> NodeAttr {
        NodeAttr {
            loop_attr: LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: inner_loop,
                outer: NodeIndex::end(),
            }, // not used
            ast,
        }
    }
}

type LoopGraph = ControlFlowGraph<NodeAttr>;

struct LoopNormalizer {
    order: Vec<NodeIndex>,
    loop_nodes: HashMap<NodeIndex, HashSet<NodeIndex>>,
    entry: NodeIndex,
    new_vars: Vec<String>,
}

struct NormalizeResult {
    entry: NodeIndex,
    new_vars: Vec<String>,
}

impl LoopNormalizer {
    pub fn normalize_exit(graph: &mut LoopGraph, entry: NodeIndex) -> NormalizeResult {
        let mut state = LoopNormalizer::init(graph, entry);
        state.normalize_exit_all(graph);
        NormalizeResult {
            entry: state.entry,
            new_vars: state.new_vars,
        }
    }

    pub fn normalize_entry(graph: &mut LoopGraph, entry: NodeIndex) -> NormalizeResult {
        let mut state = LoopNormalizer::init(graph, entry);
        state.normalize_entry_all(graph);
        NormalizeResult {
            entry: state.entry,
            new_vars: state.new_vars,
        }
    }

    fn init(graph: &LoopGraph, entry: NodeIndex) -> LoopNormalizer {
        LoopNormalizer {
            order: LoopNormalizer::get_order(graph),
            loop_nodes: LoopNormalizer::get_loop_nodes(graph),
            entry,
            new_vars: Vec::new(),
        }
    }

    fn get_order(graph: &LoopGraph) -> Vec<NodeIndex> {
        let mut order: Vec<(NodeIndex, usize)> = graph
            .node_references()
            .map(|x| (x.0, x.1.loop_attr.level))
            .collect();
        order.sort_by(|a, b| a.1.cmp(&b.1));
        order.iter().map(|x| x.0).collect()
    }

    fn get_loop_nodes(graph: &LoopGraph) -> HashMap<NodeIndex, HashSet<NodeIndex>> {
        let mut loop_nodes = HashMap::new();
        for (x, attr) in graph.node_references() {
            if attr.loop_attr.is_head {
                loop_nodes.insert(x, HashSet::new());
            }
        }
        for x in graph.node_indices() {
            for head in LoopNormalizer::nested_loops(graph, x).iter() {
                loop_nodes.get_mut(head).unwrap().insert(x);
            }
        }
        loop_nodes
    }

    fn loop_add_node(&mut self, graph: &LoopGraph, node: NodeIndex) {
        for head in LoopNormalizer::nested_loops(graph, node) {
            self.loop_nodes.get_mut(&head).unwrap().insert(node);
        }
    }

    fn nested_loops(graph: &LoopGraph, node: NodeIndex) -> Vec<NodeIndex> {
        let mut x = node;
        let mut loops = Vec::new();
        while x != NodeIndex::end() {
            let attr = &graph.node_weight(x).unwrap().loop_attr;
            if attr.inner == NodeIndex::end() {
                break;
            }
            x = attr.inner;
            loops.push(x);
            x = graph.node_weight(x).unwrap().loop_attr.outer;
        }
        loops
    }

    fn common_loop(graph: &LoopGraph, node1: NodeIndex, node2: NodeIndex) -> NodeIndex {
        let loops1 = LoopNormalizer::nested_loops(graph, node1);
        let loops2 = LoopNormalizer::nested_loops(graph, node2);
        for x in loops1.iter().rev() {
            if loops2.contains(x) {
                return *x;
            }
        }
        NodeIndex::end()
    }

    fn abnormal_exits(&self, graph: &LoopGraph, head: NodeIndex) -> Vec<(NodeIndex, NodeIndex)> {
        let mut exits = Vec::new();
        let nodes = self.loop_nodes.get(&head).unwrap();
        for x in sorted(nodes) {
            let x = *x;
            for y in graph.neighbors_directed(x, EdgeDirection::Outgoing) {
                if !self.loop_nodes.get(&head).unwrap().contains(&y) {
                    exits.push((x, y));
                    break;
                }
            }
        }
        exits
    }

    fn abnormal_entries(&self, graph: &LoopGraph, head: NodeIndex) -> Vec<(NodeIndex, NodeIndex)> {
        let mut entries = Vec::new();
        let nodes = self.loop_nodes.get(&head).unwrap();
        for x in sorted(nodes) {
            let x = *x;
            if x == head {
                continue;
            }
            for y in graph.neighbors_directed(x, EdgeDirection::Incoming) {
                if !self.loop_nodes.get(&head).unwrap().contains(&y) {
                    entries.push((y, x));
                }
            }
        }
        entries
    }

    fn normalize_exit_all(&mut self, graph: &mut LoopGraph) {
        let saved_order = self.order.clone();
        for head in saved_order.iter() {
            let head = *head;
            if !graph.node_weight(head).unwrap().loop_attr.is_head {
                continue;
            }
            self.normalize_exit_one(graph, head);
        }
    }

    fn normalize_exit_one(&mut self, graph: &mut LoopGraph, head: NodeIndex) {
        let exits = self.abnormal_exits(graph, head);
        let entries = self.abnormal_entries(graph, head);
        let n = exits.len();

        /* already normalized */
        if exits.len() <= 1 {
            return;
        }
        let exits_num = exits.iter().map(|x| x.1).unique().count();

        let outter_loop = graph.node_weight(head).unwrap().loop_attr.outer;

        /* create new vars */
        let c_var = format!("{}{}", VAR_PREFIX, head.index());
        self.new_vars.push(c_var.clone());

        /* node assgin c=-1 */
        let c_assign_init = graph.add_node(NodeAttr::new_node(
            outter_loop,
            AST::AState(Statement::Assign {
                var: c_var.clone(),
                value: Box::new(Expr::Int(-1)),
            }),
        ));
        self.loop_add_node(graph, c_assign_init);
        if self.entry == head {
            self.entry = c_assign_init;
        }

        /* the branches after loop */
        let mut out_node = exits[n - 1].1;
        if exits_num > 1 {
            for i in (0..n - 1).rev() {
                let out_i = exits[i].1;
                /* if (c==i) out_i else out_node */
                let c_cond = graph.add_node(NodeAttr::new_node(
                    outter_loop,
                    AST::ABool(BoolExpr::Eq {
                        var: c_var.clone(),
                        value: Box::new(Expr::Int(i as i32)),
                    }),
                ));
                self.loop_add_node(graph, c_cond);
                graph.add_edge(c_cond, out_node, ControlFlowEdge::Branch(false));
                graph.add_edge(c_cond, out_i, ControlFlowEdge::Branch(true));
                out_node = c_cond;
            }
        }

        /* loop condition */
        let c_cond = graph.add_node(NodeAttr::new_node(
            head,
            AST::ABool(BoolExpr::Eq {
                var: c_var.clone(),
                value: Box::new(Expr::Int(-1)),
            }),
        ));
        self.loop_add_node(graph, c_cond);
        /* edge c_assign_init->c_cond */
        graph.add_edge(c_assign_init, c_cond, ControlFlowEdge::NotBranch);
        /* if (c == -1) head else out_node */
        graph.add_edge(c_cond, out_node, ControlFlowEdge::Branch(false));
        graph.add_edge(c_cond, head, ControlFlowEdge::Branch(true));

        /* remove abnormal exits */
        for i in 0..n {
            /* node assgin c=i */
            let c_assign = graph.add_node(NodeAttr::new_node(
                head,
                AST::AState(Statement::Assign {
                    var: c_var.clone(),
                    value: Box::new(Expr::Int(i as i32)),
                }),
            ));
            self.loop_add_node(graph, c_assign);
            /* exit_node->c_assign->c_cond */
            replace_edge_dest(graph, exits[i].0, exits[i].1, c_assign);
            graph.add_edge(c_assign, c_cond, ControlFlowEdge::NotBranch);
        }

        /* change edges prev->head to prev->c_assign_init/c_cond */
        let prevs: Vec<NodeIndex> = graph
            .neighbors_directed(head, EdgeDirection::Incoming)
            .collect();
        for prev in prevs {
            if prev == c_cond {
                continue;
            }
            let new_dest;
            if graph.node_weight(prev).unwrap().loop_attr.inner == head {
                new_dest = c_cond;
            } else {
                new_dest = c_assign_init;
            }
            replace_edge_dest(graph, prev, head, new_dest);
        }

        /* edit abnormal entries */
        for (prev, ent) in entries {
            /* node assgin c=-1 */
            let c_assign_init = graph.add_node(NodeAttr::new_node(
                LoopNormalizer::common_loop(graph, head, prev),
                AST::AState(Statement::Assign {
                    var: c_var.clone(),
                    value: Box::new(Expr::Int(-1)),
                }),
            ));
            self.loop_add_node(graph, c_assign_init);
            /* change edges prev->ent to prev->b_assign_true->ent */
            replace_edge_dest(graph, prev, ent, c_assign_init);
            graph.add_edge(c_assign_init, ent, ControlFlowEdge::NotBranch);
        }
    }

    fn normalize_entry_all(&mut self, graph: &mut LoopGraph) {
        let saved_order = self.order.clone();
        for head in saved_order.iter() {
            let head = *head;
            if !graph.node_weight(head).unwrap().loop_attr.is_head {
                continue;
            }
            self.normalize_entry_one(graph, head);
        }
    }

    fn normalize_entry_one(&mut self, graph: &mut LoopGraph, head: NodeIndex) {
        let entries = self.abnormal_entries(graph, head);
        let mut dup_nodes = HashMap::new();

        for (prev, ent) in entries.iter() {
            let prev = *prev;
            let ent = *ent;
            let dup_ent = self.duplicate_nodes(graph, head, &mut dup_nodes, ent);
            /* change prev->ent to prev->dup_ent */
            replace_edge_dest(graph, prev, ent, dup_ent);
            clean_orphan(graph, ent);
        }
    }

    fn duplicate_nodes(
        &mut self,
        graph: &mut LoopGraph,
        head: NodeIndex,
        dup_nodes: &mut HashMap<NodeIndex, NodeIndex>,
        node: NodeIndex,
    ) -> NodeIndex {
        /* do not dup head */
        if node == head {
            return head;
        }

        /* do not dup exit */
        if !self.loop_nodes.get(&head).unwrap().contains(&node) {
            return node;
        }

        /* dup node existing */
        if let Some(dup) = dup_nodes.get(&node) {
            return *dup;
        }

        /* duplicate node */
        let dup = graph.add_node(graph.node_weight(node).unwrap().clone());
        dup_nodes.insert(node, dup);
        self.loop_add_node(graph, dup);

        /* duplicate edges */
        let edges: Vec<(NodeIndex, ControlFlowEdge)> = graph
            .edges(node)
            .map(|e| (e.target(), *e.weight()))
            .collect();
        for e in edges {
            let next = e.0;
            let dup_next = self.duplicate_nodes(graph, head, dup_nodes, next);
            graph.add_edge(dup, dup_next, e.1);
        }

        dup
    }
}

fn replace_edge_dest(
    graph: &mut LoopGraph,
    id_from: NodeIndex,
    id_to_old: NodeIndex,
    id_to_new: NodeIndex,
) {
    while let Some(edge) = graph.find_edge(id_from, id_to_old) {
        let weight = *graph.edge_weight(edge).unwrap();
        graph.remove_edge(edge);
        graph.add_edge(id_from, id_to_new, weight);
    }
}

fn clean_orphan(graph: &mut LoopGraph, start: NodeIndex) {
    let mut que = VecDeque::new();
    que.push_back(start);
    while let Some(node) = que.pop_front() {
        let deg = graph
            .neighbors_directed(node, EdgeDirection::Incoming)
            .count();
        if deg > 0 {
            continue;
        }
        for next in graph.neighbors(node) {
            que.push_back(next);
        }
        graph.remove_node(node);
    }
}
