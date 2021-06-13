use itertools::Itertools;
use std::collections::HashMap;
use std::collections::HashSet;

use crate::ast::*;
use crate::graph::*;

const VAR_PREFIX: &str = "break_";

pub struct LoopRecOutput {
    pub graph: StaticGraph<AST>,
    pub entry: usize,
    pub new_vars: Vec<String>,
}

pub fn loop_structure(graph: &dyn Graph, entry: usize) -> Option<LoopRecOutput> {
    let mut loop_graph = construct_loop_graph(graph)?;

    LoopMarker::mark(&mut loop_graph, entry);
    //println!("===== mark1");
    //debug_print(&loop_graph);

    let NormalizeResult {
        entry,
        new_vars: new_vars1,
    } = LoopNormalizer::normalize_exit(&mut loop_graph, entry);
    //println!("===== normal exit");
    //debug_print(&loop_graph);

    LoopMarker::mark(&mut loop_graph, entry);
    //println!("===== mark2");
    //debug_print(&loop_graph);

    let NormalizeResult {
        entry,
        new_vars: mut new_vars2,
    } = LoopNormalizer::normalize_entry(&mut loop_graph, entry);
    //println!("===== normal entry");
    //debug_print(&loop_graph);

    let out_graph = construct_out_graph(&loop_graph);
    let mut new_vars = new_vars1;
    new_vars.append(&mut new_vars2);
    Some(LoopRecOutput {
        graph: out_graph,
        entry,
        new_vars,
    })
}

fn construct_loop_graph(graph: &dyn Graph) -> Option<LoopGraph> {
    let mut loop_graph = LoopGraph::new();
    for x in graph.node_iter() {
        let branch_attr = construct_branch_attr(graph, x)?;
        loop_graph.add_node(NodeAttr::new(x, branch_attr));
    }
    for x in graph.node_iter() {
        for y in graph.edge_iter(x) {
            loop_graph.add_edge(x, y);
        }
    }
    Some(loop_graph)
}

fn construct_branch_attr(graph: &dyn Graph, id: usize) -> Option<BranchAttr> {
    let edges: Vec<usize> = graph.edge_iter(id).collect();
    let edge_count = edges.len();
    if edge_count > 2 {
        None
    } else if edge_count == 2 {
        Some(BranchAttr::Branch(edges[0], edges[1]))
    } else {
        Some(BranchAttr::NotBranch)
    }
}

fn construct_out_graph(graph: &LoopGraph) -> StaticGraph<AST> {
    let mut out_graph = StaticGraph::new_empty();
    for x in graph.node_iter() {
        out_graph.add_node(graph.read_note(x).ast.clone());
    }
    for x in graph.node_iter() {
        let mut edges: Vec<usize> = graph.edge_iter(x).collect();
        if let BranchAttr::Branch(br_false, br_true) = graph.read_note(x).branch_attr {
            edges = vec![br_false, br_true];
        }
        for y in edges.iter() {
            let y = *y;
            out_graph.add_edge(x, y);
        }
    }
    out_graph
}

#[derive(Debug, Clone)]
struct NodeAttr {
    loop_attr: LoopAttr,
    branch_attr: BranchAttr,
    ast: AST,
}

#[derive(Debug, Clone)]
struct LoopAttr {
    is_head: bool,
    level: usize,
    inner: usize,
    outer: usize,
}

#[derive(Debug, Clone)]
enum BranchAttr {
    NotBranch,
    Branch(usize, usize), // (false_branch, true_branch)
}

impl NodeAttr {
    pub fn new(origin: usize, branch_attr: BranchAttr) -> NodeAttr {
        NodeAttr {
            loop_attr: LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: usize::MAX,
                outer: usize::MAX,
            },
            branch_attr: branch_attr.clone(),
            ast: match branch_attr {
                BranchAttr::NotBranch => AST::AState(Statement::Original { node_num: origin }),
                BranchAttr::Branch(_, _) => AST::ABool(BoolExpr::Original { node_num: origin }),
            },
        }
    }

    pub fn new_node(inner_loop: usize, branch_attr: BranchAttr, ast: AST) -> NodeAttr {
        NodeAttr {
            loop_attr: LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: inner_loop,
                outer: usize::MAX,
            }, // not used
            branch_attr,
            ast,
        }
    }
}

type LoopGraph = EditableGraph<NodeAttr>;

struct LoopMarker {
    visited: Vec<usize>,
    processed: Vec<usize>,
    in_stack: Vec<bool>,
    dfn: usize,
    entry: usize,
}

impl LoopMarker {
    pub fn mark(graph: &mut LoopGraph, entry: usize) {
        let n = graph.node_num();
        let mut state = LoopMarker {
            visited: vec![0; n],
            in_stack: vec![false; n],
            processed: vec![usize::MAX; n],
            dfn: 0,
            entry,
        };
        LoopMarker::clean_loop_attr(graph);
        state.dfs(graph, entry);
        state.visited = vec![0; n];
        LoopMarker::mark_range(&mut state, graph, entry);
    }

    fn clean_loop_attr(graph: &mut LoopGraph) {
        let nodes: Vec<usize> = graph.node_iter().collect();
        for x in nodes {
            graph.get_note_mut(x).loop_attr = LoopAttr {
                is_head: false,
                level: usize::MAX,
                inner: usize::MAX,
                outer: usize::MAX,
            };
        }
    }

    fn dfs(&mut self, graph: &mut LoopGraph, current: usize) {
        self.visited[current] = 1;
        self.in_stack[current] = true;
        let nexts: Vec<usize> = graph.edge_iter(current).collect();
        for next in nexts {
            if self.visited[next] != 0 {
                if self.in_stack[next] {
                    /* loop-back edge */
                    graph.get_note_mut(next).loop_attr.is_head = true;
                } else {
                    /* cross edge */
                }
            } else {
                self.dfs(graph, next);
            }
        }
        self.in_stack[current] = false;
        graph.get_note_mut(current).loop_attr.level = self.dfn;
        self.dfn += 1;
    }

    fn mark_range(&mut self, graph: &mut LoopGraph, current: usize) {
        if graph.read_note(current).loop_attr.is_head {
            self.mark_range_helper(graph, current, current);
        }
        let nexts: Vec<usize> = graph.edge_iter(current).collect();
        for next in nexts {
            self.visited[next] += 1;
            if self.visited[next] == 1 {
                self.mark_range(graph, next);
            }
        }
    }

    fn mark_range_helper(&mut self, graph: &mut LoopGraph, current: usize, head: usize) {
        if graph.read_note(current).loop_attr.inner == head {
            return;
        }

        let mut mark_current = false;
        let mut decide_next = false;

        if graph.read_note(current).loop_attr.is_head {
            if current == head {
                mark_current = true;
                LoopMarker::set_loop(graph, current, head);
                decide_next = true;
            } else {
                if (self.visited[current] == 0 && current != self.entry)
                    || self.visited[current] == graph.reverse_edge_iter(current).count()
                {
                    if self.processed[current] != head {
                        self.processed[current] = head;
                        decide_next = true;
                    }
                }
            }
        } else {
            decide_next = true;
        }

        if decide_next {
            let nexts: Vec<usize> = graph.edge_iter(current).collect();
            for next in nexts {
                self.mark_range_helper(graph, next, head);
                if graph.read_note(next).loop_attr.inner == head {
                    mark_current = true;
                }
            }
        }

        if mark_current && current != head {
            LoopMarker::set_loop(graph, current, head);
        }
    }

    fn set_loop(graph: &mut LoopGraph, node: usize, head: usize) {
        let attr = graph.get_note_mut(node);
        attr.loop_attr.outer = attr.loop_attr.inner;
        attr.loop_attr.inner = head;
    }
}

struct LoopNormalizer {
    order: Vec<usize>,
    loop_nodes: HashMap<usize, HashSet<usize>>,
    entry: usize,
    new_vars: Vec<String>,
}

struct NormalizeResult {
    entry: usize,
    new_vars: Vec<String>,
}

impl LoopNormalizer {
    pub fn normalize_exit(graph: &mut LoopGraph, entry: usize) -> NormalizeResult {
        let mut state = LoopNormalizer::init(graph, entry);
        state.normalize_exit_all(graph);
        NormalizeResult {
            entry: state.entry,
            new_vars: state.new_vars,
        }
    }

    pub fn normalize_entry(graph: &mut LoopGraph, entry: usize) -> NormalizeResult {
        let mut state = LoopNormalizer::init(graph, entry);
        state.normalize_entry_all(graph);
        NormalizeResult {
            entry: state.entry,
            new_vars: state.new_vars,
        }
    }

    fn init(graph: &LoopGraph, entry: usize) -> LoopNormalizer {
        LoopNormalizer {
            order: LoopNormalizer::get_order(graph),
            loop_nodes: LoopNormalizer::get_loop_nodes(graph),
            entry,
            new_vars: Vec::new(),
        }
    }

    fn get_order(graph: &LoopGraph) -> Vec<usize> {
        let mut order: Vec<(usize, usize)> = graph
            .node_iter()
            .map(|x| (x, graph.read_note(x).loop_attr.level))
            .collect();
        order.sort_by(|a, b| a.1.cmp(&b.1));
        order.iter().map(|x| x.0).collect()
    }

    fn get_loop_nodes(graph: &LoopGraph) -> HashMap<usize, HashSet<usize>> {
        let mut loop_nodes = HashMap::new();
        for x in graph.node_iter() {
            if graph.read_note(x).loop_attr.is_head {
                loop_nodes.insert(x, HashSet::new());
            }
        }
        for x in graph.node_iter() {
            for head in LoopNormalizer::nested_loops(graph, x).iter() {
                loop_nodes.get_mut(&head).unwrap().insert(x);
            }
        }
        loop_nodes
    }

    fn loop_add_node(&mut self, graph: &LoopGraph, node: usize) {
        for head in LoopNormalizer::nested_loops(graph, node) {
            self.loop_nodes.get_mut(&head).unwrap().insert(node);
        }
    }

    fn nested_loops(graph: &LoopGraph, node: usize) -> Vec<usize> {
        let mut x = node;
        let mut loops = Vec::new();
        while x != usize::MAX {
            let attr = &graph.read_note(x).loop_attr;
            if attr.inner == usize::MAX {
                break;
            }
            x = attr.inner;
            loops.push(x);
            x = graph.read_note(x).loop_attr.outer;
        }
        loops
    }

    fn common_loop(graph: &LoopGraph, node1: usize, node2: usize) -> usize {
        let loops1 = LoopNormalizer::nested_loops(graph, node1);
        let loops2 = LoopNormalizer::nested_loops(graph, node2);
        for x in loops1.iter().rev() {
            if loops2.contains(x) {
                return *x;
            }
        }
        usize::MAX
    }

    fn abnormal_exits(&self, graph: &LoopGraph, head: usize) -> Vec<(usize, usize)> {
        let mut exits = Vec::new();
        let nodes = self.loop_nodes.get(&head).unwrap();
        for x in nodes.iter() {
            let x = *x;
            for y in graph.edge_iter(x) {
                if !self.loop_nodes.get(&head).unwrap().contains(&y) {
                    exits.push((x, y));
                    break;
                }
            }
        }
        exits
    }

    fn abnormal_entries(&self, graph: &LoopGraph, head: usize) -> Vec<(usize, usize)> {
        let mut entries = Vec::new();
        let nodes = self.loop_nodes.get(&head).unwrap();
        for x in nodes.iter() {
            let x = *x;
            if x == head {
                continue;
            }
            for y in graph.reverse_edge_iter(x) {
                if !self.loop_nodes.get(&head).unwrap().contains(&y) {
                    entries.push((y, x));
                    break;
                }
            }
        }
        entries
    }

    fn normalize_exit_all(&mut self, graph: &mut LoopGraph) {
        let saved_order = self.order.clone();
        for head in saved_order.iter() {
            let head = *head;
            if !graph.read_note(head).loop_attr.is_head {
                continue;
            }
            self.normalize_exit_one(graph, head);
        }
    }

    fn normalize_exit_one(&mut self, graph: &mut LoopGraph, head: usize) {
        let exits = self.abnormal_exits(graph, head);
        let entries = self.abnormal_entries(graph, head);
        let n = exits.len();

        /* already normalized */
        let exits_num = exits.iter().map(|x| x.1).unique().count();
        if exits_num <= 1 {
            return;
        }

        let outter_loop = graph.get_note_mut(head).loop_attr.outer;

        /* create new vars */
        let c_var = format!("{}{}", VAR_PREFIX, head);
        self.new_vars.push(c_var.clone());

        /* node assgin c=-1 */
        let c_assign_init = graph.add_node(NodeAttr::new_node(
            outter_loop,
            BranchAttr::NotBranch,
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
        for i in (0..n - 1).rev() {
            let out_i = exits[i].1;
            /* if (c==i) out_i else out_node */
            let c_cond = graph.add_node(NodeAttr::new_node(
                outter_loop,
                BranchAttr::Branch(out_node, out_i),
                AST::ABool(BoolExpr::Eq {
                    var: c_var.clone(),
                    value: Box::new(Expr::Int(i as i32)),
                }),
            ));
            self.loop_add_node(graph, c_cond);
            graph.add_edge(c_cond, out_node);
            graph.add_edge(c_cond, out_i);
            out_node = c_cond;
        }

        /* loop condition */
        let c_cond = graph.add_node(NodeAttr::new_node(
            head,
            BranchAttr::Branch(out_node, head),
            AST::ABool(BoolExpr::Eq {
                var: c_var.clone(),
                value: Box::new(Expr::Int(-1)),
            }),
        ));
        self.loop_add_node(graph, c_cond);
        /* edge c_assign_init->c_cond */
        graph.add_edge(c_assign_init, c_cond);
        /* if (c == -1) head else out_node */
        graph.add_edge(c_cond, out_node);
        graph.add_edge(c_cond, head);

        /* remove abnormal exits */
        for i in 0..n {
            /* node assgin c=i */
            let c_assign = graph.add_node(NodeAttr::new_node(
                head,
                BranchAttr::NotBranch,
                AST::AState(Statement::Assign {
                    var: c_var.clone(),
                    value: Box::new(Expr::Int(i as i32)),
                }),
            ));
            self.loop_add_node(graph, c_assign);
            /* exit_node->c_assign->c_cond */
            replace_edge_dest(graph, exits[i].0, exits[i].1, c_assign);
            graph.add_edge(c_assign, c_cond);
        }

        /* change edges prev->head to prev->c_assign_init/c_cond */
        let prevs: Vec<usize> = graph.reverse_edge_iter(head).collect();
        for prev in prevs {
            if prev == c_cond {
                continue;
            }
            let new_dest;
            if graph.read_note(prev).loop_attr.inner == head {
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
                BranchAttr::NotBranch,
                AST::AState(Statement::Assign {
                    var: c_var.clone(),
                    value: Box::new(Expr::Int(-1)),
                }),
            ));
            self.loop_add_node(graph, c_assign_init);
            /* change edges prev->ent to prev->b_assign_true->ent */
            replace_edge_dest(graph, prev, ent, c_assign_init);
            graph.add_edge(c_assign_init, ent);
        }
    }

    fn normalize_entry_all(&mut self, graph: &mut LoopGraph) {
        let saved_order = self.order.clone();
        for head in saved_order.iter() {
            let head = *head;
            if !graph.read_note(head).loop_attr.is_head {
                continue;
            }
            self.normalize_entry_one(graph, head);
        }
    }

    fn normalize_entry_one(&mut self, graph: &mut LoopGraph, head: usize) {
        let entries = self.abnormal_entries(graph, head);
        let mut dup_nodes: HashMap<usize, usize> = HashMap::new();

        for (prev, ent) in entries.iter() {
            let prev = *prev;
            let ent = *ent;
            let dup_ent = self.duplicate_nodes(graph, head, &mut dup_nodes, ent);
            /* change prev->ent to prev->dup_ent */
            replace_edge_dest(graph, prev, ent, dup_ent);
        }
    }

    fn duplicate_nodes(
        &self,
        graph: &mut LoopGraph,
        head: usize,
        dup_nodes: &mut HashMap<usize, usize>,
        node: usize,
    ) -> usize {
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
        let dup = graph.add_node(graph.read_note(node).clone());
        dup_nodes.insert(node, dup);

        /* duplicate edges */
        let nexts: Vec<usize> = graph.edge_iter(node).collect();
        let mut nexts_new: Vec<usize> = Vec::new();
        for next in nexts.iter() {
            let next = *next;
            let dup_next = self.duplicate_nodes(graph, head, dup_nodes, next);
            graph.add_edge(dup, dup_next);
            nexts_new.push(dup_next);
        }

        /* fix branch relation */
        if let BranchAttr::Branch(br_false, _) = graph.read_note(node).branch_attr {
            let attr = if br_false == nexts[0] {
                BranchAttr::Branch(nexts_new[0], nexts_new[1])
            } else {
                BranchAttr::Branch(nexts_new[1], nexts_new[0])
            };
            graph.get_note_mut(dup).branch_attr = attr;
        }

        dup
    }
}

fn replace_edge_dest(graph: &mut LoopGraph, id_from: usize, id_to_old: usize, id_to_new: usize) {
    graph.del_edge(id_from, id_to_old);
    graph.add_edge(id_from, id_to_new);
    let attr = graph.get_note_mut(id_from);
    if let BranchAttr::Branch(br_false, br_true) = attr.branch_attr {
        if br_true == id_to_old {
            attr.branch_attr = BranchAttr::Branch(br_false, id_to_new);
        }
        if br_false == id_to_old {
            attr.branch_attr = BranchAttr::Branch(id_to_new, br_true);
        }
    }
}
