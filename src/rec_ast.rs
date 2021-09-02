use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::iter::FromIterator;

use itertools::Itertools;
use petgraph::algo::{dominators, toposort};
use petgraph::data::DataMap;
use petgraph::graphmap::DiGraphMap;
use petgraph::visit::{EdgeRef, IntoNodeReferences, IntoEdgeReferences, IntoNodeIdentifiers};
use petgraph::Direction;

use crate::ast::*;
use crate::graph::*;
use crate::loop_utils::*;

const VAR_PREFIX: &str = "pred_";

pub struct ASTRecResult {
    pub stmt: Statement,
    pub new_vars: Vec<String>,
}

pub fn ast_structure<N>(graph: &ControlFlowGraph<N>, entry: NodeIndex) -> Option<ASTRecResult> {
    let (mut graph, mut entry) = construct_ast_graph(graph, entry)?;
    add_halt(&mut graph, true);

    loop_mark(&mut graph, entry);
    let mut new_vars = Vec::new();
    let mut loops = LoopNodes::new(&graph);
    let order = LoopNodes::node_order(&graph);

    for head in order {
        let attr = graph.node_weight(head).unwrap();
        if !attr.loop_attr_ref().is_head {
            continue;
        }
        let entry_in_loop = loops.inside_loop(head, entry);
        //println!("{}", dot_view(&graph, entry));
        let (new_node, vars) = ast_structure_loop(&mut graph, &mut loops, head)?;
        new_vars.extend(vars);
        //debug_print(&graph);
        if entry_in_loop {
            entry = new_node;
        }
    }

    //println!("{}", dot_view(&graph, entry));
    let mut result = ast_structure_acyclic(&mut graph, entry)?;
    result.new_vars.extend(new_vars);
    Some(result)
}

fn ast_structure_loop(
    graph: &mut ASTGraph,
    loops: &mut LoopNodes<NodeAttr>,
    head: NodeIndex,
) -> Option<(NodeIndex, Vec<String>)> {
    let nodes = loops.get_exist(graph, head);
    let mut ast_map = HashMap::new();
    for node in nodes.iter() {
        let ast = &graph.node_weight(*node).unwrap().ast;
        ast_map.insert(*node, ast);
    }

    let (mut subgraph, entry) = construct_loop_subgraph(graph, loops, head);
    //println!("{}", dot_view(&subgraph, entry));
    let result = ast_structure_acyclic(&mut subgraph, entry)?;
    //println!("{}", result.stmt.to_string());
    let ast = AST::AState(Statement::While {
        cond: Box::new(BoolExpr::True),
        body: Box::new(result.stmt.unfold(&ast_map)),
    });

    let outer_loop = graph.node_weight(head).unwrap().loop_attr.outer;
    let new_node = replace_block(graph, &nodes, ast);
    graph.node_weight_mut(new_node).unwrap().loop_attr.inner = outer_loop;
    loops.remove_nodes(&nodes);
    loops.add_node(&graph, new_node);

    Some((new_node, result.new_vars))
}

fn construct_loop_subgraph(
    graph: &ASTGraph,
    loops: &LoopNodes<NodeAttr>,
    head: NodeIndex,
) -> (ASTGraph, NodeIndex) {
    let nodes: HashSet<NodeIndex> = HashSet::from_iter(loops.get_exist(graph, head));
    let exit_edges = loops.loop_exits(graph, head);

    let exits_num = exit_edges.iter().map(|x| x.1).unique().count();
    assert!(exits_num <= 1, "exit_edges {:?}", exit_edges);
    let exit = exit_edges.first().and_then(|x| Some(x.1));

    let mut new_graph = ASTGraph::new();
    let mut node_map = HashMap::new();
    for x in nodes.iter() {
        let x = *x;
        let is_branch = graph.edges(x).count() == 2;
        let node = new_graph.add_node(NodeAttr::new(x, is_branch));
        node_map.insert(x, node);
    }

    let node_continue = new_graph.add_node(NodeAttr::new_node(AST::AState(Statement::Continue)));
    let node_break = new_graph.add_node(NodeAttr::new_node(AST::AState(Statement::Break)));

    for x in nodes.iter() {
        let node_x = *node_map.get(x).unwrap();
        let x = *x;
        for e in graph.edges(x) {
            let y = e.target();
            if y == head {
                new_graph.add_edge(node_x, node_continue, *e.weight());
            } else if Some(y) == exit {
                new_graph.add_edge(node_x, node_break, *e.weight());
            } else {
                match node_map.get(&y) {
                    Some(node_y) => {
                        new_graph.add_edge(node_x, *node_y, *e.weight());
                    }
                    None => panic!("error when building loop subgraph"),
                }
            }
        }
    }

    let entry = *node_map.get(&head).unwrap();
    (new_graph, entry)
}

fn ast_structure_acyclic(graph: &mut ASTGraph, entry: NodeIndex) -> Option<ASTRecResult> {
    let new_entry = graph.add_node(NodeAttr::new_node(AST::AState(Statement::Nop)));
    graph.add_edge(new_entry, entry, ControlFlowEdge::NotBranch);
    let entry = new_entry;
    strip_graph(graph, entry);
    let mut new_vars = Vec::new();
    loop {
        let result = Simplifier::simplify(graph, entry);
        new_vars.extend(result.new_vars);
        //println!("{}", dot_view(&graph, entry));
        if graph.node_count() == 1 {
            break;
        }
        if !result.updated {
            Splitter::split(graph, entry)?;
        }
    }
    let ast = &graph.node_weights().next()?.ast;
    Some(ASTRecResult {
        stmt: ast.clone_state(),
        new_vars,
    })
}

fn strip_graph(graph: &mut ASTGraph, entry: NodeIndex) {
    let mut bfs = petgraph::visit::Bfs::new(graph as &ASTGraph, entry);
    let mut reachable: HashSet<NodeIndex> = HashSet::new();
    while let Some(node) = bfs.next(graph as &ASTGraph) {
        reachable.insert(node);
    }
    let unreachable: Vec<NodeIndex> = graph
        .node_references()
        .filter_map(|x| {
            if reachable.contains(&x.0) {
                None
            } else {
                Some(x.0)
            }
        })
        .collect();
    for node in unreachable {
        graph.remove_node(node);
    }
}

fn add_halt(graph: &mut ASTGraph, single: bool) -> Vec<NodeIndex> {
    let nodes: Vec<NodeIndex> = graph.node_identifiers().collect();
    let mut halts = Vec::new();
    let mut halt = NodeIndex::end();
    if single {
        halt = graph.add_node(NodeAttr::new_node(AST::AState(Statement::Halt)));
        halts.push(halt);
    }
    for node in nodes.iter() {
        let node = *node;
        if graph.edges(node).count() == 0 {
            if !single {
                halt = graph.add_node(NodeAttr::new_node(AST::AState(Statement::Halt)));
                halts.push(halt);
            }
            graph.add_edge(node, halt, ControlFlowEdge::NotBranch);
        }
    }
    halts
}

fn construct_ast_graph<N>(
    graph: &ControlFlowGraph<N>,
    entry: NodeIndex,
) -> Option<(ASTGraph, NodeIndex)> {
    let mut ast_graph = ASTGraph::new();
    let mut node_map = HashMap::new();
    for x in graph.node_indices() {
        let is_branch = graph.edges(x).count() == 2;
        let node = ast_graph.add_node(NodeAttr::new(x, is_branch));
        node_map.insert(x, node);
    }
    for e in graph.edge_references() {
        let x = e.source();
        let y = e.target();
        let node_x = node_map.get(&x).unwrap();
        let node_y = node_map.get(&y).unwrap();
        ast_graph.add_edge(*node_x, *node_y, *e.weight());
    }
    let entry = *node_map.get(&entry).unwrap();
    Some((ast_graph, entry))
}

type ASTGraph = ControlFlowGraph<NodeAttr>;

#[derive(Debug)]
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
            loop_attr: LoopAttr::default(),
            ast: match is_branch {
                false => AST::AState(Statement::Original { node_idx: origin }),
                true => AST::ABool(BoolExpr::Original { node_idx: origin }),
            },
        }
    }

    pub fn new_node(ast: AST) -> NodeAttr {
        NodeAttr {
            loop_attr: LoopAttr::default(),
            ast,
        }
    }
}

fn is_branch(graph: &ASTGraph, node: NodeIndex) -> bool {
    graph.edges(node).count() == 2
}

fn branch_attr(graph: &ASTGraph, node: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
    let nexts = ordered_neighbors(graph, node);
    if nexts.len() != 2 {
        None
    } else {
        Some((nexts[0], nexts[1]))
    }
}

fn single_income(graph: &ASTGraph, node: NodeIndex) -> bool {
    graph.edges_directed(node, Direction::Incoming).count() == 1
}

struct SimplifyResult {
    updated: bool,
    new_vars: Vec<String>,
}

struct Simplifier {
    queue: VecDeque<NodeIndex>,
    visited: HashSet<NodeIndex>,
    entry: NodeIndex,
    new_vars: Vec<String>,
}

impl Simplifier {
    pub fn simplify(graph: &mut ASTGraph, entry: NodeIndex) -> SimplifyResult {
        let mut state = Simplifier {
            queue: VecDeque::new(),
            visited: HashSet::new(),
            entry,
            new_vars: Vec::new(),
        };
        state.dfs(graph, entry);
        state.queue_reverse();
        let updated = state.simplify_all(graph);
        SimplifyResult {
            updated,
            new_vars: state.new_vars,
        }
    }

    fn dfs(&mut self, graph: &mut ASTGraph, current: NodeIndex) {
        self.visited.insert(current);
        let nexts: Vec<NodeIndex> = graph.neighbors(current).collect();
        for next in nexts.iter() {
            if !self.visited.contains(next) {
                self.dfs(graph, *next);
            }
        }
        self.queue_add(current);
    }

    fn queue_add(&mut self, node: NodeIndex) {
        self.queue.push_front(node);
    }

    fn queue_reverse(&mut self) {
        self.queue = self.queue.iter().rev().map(|x| *x).collect();
    }

    fn simplify_all(&mut self, graph: &mut ASTGraph) -> bool {
        let mut updated = false;
        while let Some(node) = self.queue.pop_front() {
            if !graph.contains_node(node) {
                continue;
            }
            if self.simplify_one(graph, node) {
                updated = true;
            }
        }
        updated
    }

    fn simplify_one(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        if self.try_dump_if(graph, handle) {
            return true;
        }
        if self.try_seq(graph, handle) {
            return true;
        }
        if self.try_if_then(graph, handle) {
            return true;
        }
        if self.try_if_then_else(graph, handle) {
            return true;
        }
        if self.try_branch_or(graph, handle) {
            return true;
        }
        if self.try_short_circuit(graph, handle) {
            return true;
        }
        false
    }

    fn try_seq(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        if graph.edges(handle).count() != 1 {
            return false;
        }
        let next = match graph.neighbors(handle).next() {
            None => return false,
            Some(x) => x,
        };
        if is_branch(graph, next) {
            return false;
        }
        if !single_income(graph, next) {
            return false;
        }

        let ast_handle = graph.node_weight(handle).unwrap().ast.clone();
        let ast_next = graph.node_weight(next).unwrap().ast.clone();

        if handle == self.entry {
            if graph.edges(next).count() != 0 {
                return false;
            }
            let _new_node = replace_block(graph, &vec![handle, next], ast_next);
            true
        } else {
            let loop_back = graph.contains_edge(next, handle);
            let new_node = replace_block(
                graph,
                &vec![handle, next],
                AST::AState(Statement::Compound {
                    first: Box::new(ast_handle.clone_state()),
                    next: Box::new(ast_next.clone_state()),
                }),
            );
            if loop_back {
                graph.add_edge(new_node, new_node, ControlFlowEdge::NotBranch);
            }
            self.queue_add(new_node);
            true
        }
    }

    fn try_if_then(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        let cond = handle;
        let (br_false, br_true) = match branch_attr(graph, cond) {
            None => return false,
            Some(br) => (br.0, br.1),
        };
        let cond_expr = graph.node_weight(cond).unwrap().ast.clone_bool();

        let is_part = |part, next| {
            if !single_income(graph, part) {
                return false;
            }
            let cnt = graph.edges(part).count();
            if cnt == 0 {
                return true;
            }
            cnt == 1 && graph.contains_edge(part, next)
        };

        let part;
        let next;
        if is_part(br_false, br_true) {
            part = br_false;
            next = br_true;
        } else if is_part(br_true, br_false) {
            part = br_true;
            next = br_false;
        } else {
            return false;
        }
        let refresh_next = graph.edges(part).count() > 0;

        let new_node = replace_block(
            graph,
            &vec![part, cond],
            AST::AState(Statement::IfThen {
                cond: Box::new(if br_true == part {
                    cond_expr
                } else {
                    BoolExpr::Not {
                        value: Box::new(cond_expr),
                    }
                }),
                body_then: Box::new(graph.node_weight(part).unwrap().ast.clone_state()),
            }),
        );

        if next == cond {
            graph.add_edge(new_node, new_node, ControlFlowEdge::NotBranch);
        }

        self.queue_add(new_node);
        if refresh_next {
            self.queue_add(next);
        }
        true
    }

    fn try_if_then_else(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        let cond = handle;
        let (br_false, br_true) = match branch_attr(graph, cond) {
            None => return false,
            Some(br) => (br.0, br.1),
        };

        if !single_income(graph, br_false) || !single_income(graph, br_true) {
            return false;
        }

        let next_false: Vec<NodeIndex> = graph.neighbors(br_false).collect();
        let next_true: Vec<NodeIndex> = graph.neighbors(br_true).collect();
        let next: Option<NodeIndex>;

        if next_false.len() == 0 && next_true.len() == 0 {
            next = None;
        } else if next_false.len() == 1
            && next_true.len() == 1
            && next_false.first() == next_true.first()
        {
            next = Some(*next_false.first().unwrap());
        } else {
            return false;
        }

        let old_nodes = vec![cond, br_false, br_true];
        let new_node = replace_block(
            graph,
            &old_nodes,
            AST::AState(Statement::IfThenElse {
                cond: Box::new(graph.node_weight(cond).unwrap().ast.clone_bool()),
                body_then: Box::new(graph.node_weight(br_true).unwrap().ast.clone_state()),
                body_else: Box::new(graph.node_weight(br_false).unwrap().ast.clone_state()),
            }),
        );

        if next == Some(cond) {
            graph.add_edge(new_node, new_node, ControlFlowEdge::NotBranch);
        }

        self.queue_add(new_node);
        if let Some(next) = next {
            self.queue_add(next);
        }
        true
    }

    fn try_dump_if(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        let cond = handle;
        let next = match branch_attr(graph, cond) {
            None => return false,
            Some(br) => {
                if br.0 != br.1 {
                    return false;
                }
                br.1
            }
        };

        let new_node = replace_block(
            graph,
            &vec![cond],
            AST::AState(Statement::IfThen {
                cond: Box::new(graph.node_weight(cond).unwrap().ast.clone_bool()),
                body_then: Box::new(Statement::Nop),
            }),
        );

        if next == handle {
            graph.add_edge(new_node, new_node, ControlFlowEdge::NotBranch);
        }

        self.queue_add(new_node);
        true
    }

    fn try_branch_or(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        let (handle_false, handle_true) = match branch_attr(graph, handle) {
            None => return false,
            Some(br) => (br.0, br.1),
        };
        if !single_income(graph, handle) {
            return false;
        }

        let prev = graph
            .neighbors_directed(handle, Direction::Incoming)
            .next()
            .unwrap();
        if !is_branch(graph, prev) {
            return false;
        }
        let cur_branch = handle;
        let prev_cond = prev;
        let (_cond_false, cond_true) = match branch_attr(graph, prev_cond) {
            None => return false,
            Some(br) => (br.0, br.1),
        };
        let other = graph
            .neighbors(prev_cond)
            .filter(|x| *x != cur_branch)
            .next()
            .unwrap();
        if other != handle_true && other != handle_false {
            return false;
        }
        let branch_merge = other;

        let handle_expr = match &graph.node_weight(handle).unwrap().ast {
            AST::AState(_) => panic!("not a branch"),
            AST::ABool(expr) => expr.clone(),
        };
        let cond_expr = match &graph.node_weight(prev_cond).unwrap().ast {
            AST::AState(_) => panic!("not a branch"),
            AST::ABool(expr) => expr.clone(),
        };

        let branches = if branch_merge == handle_true {
            (handle_false, handle_true)
        } else {
            (handle_true, handle_false)
        };

        /* build an OR condition */
        let handle_part = if branch_merge == handle_true {
            handle_expr
        } else {
            BoolExpr::Not {
                value: Box::new(handle_expr),
            }
        };
        let cond_part = if other == cond_true {
            cond_expr
        } else {
            BoolExpr::Not {
                value: Box::new(cond_expr),
            }
        };
        let ast = AST::ABool(BoolExpr::Or {
            value1: Box::new(cond_part),
            value2: Box::new(handle_part),
        });
        let new_node = replace_condition(graph, vec![handle, prev_cond], branches, ast);

        self.queue_add(new_node);
        self.queue_add(branch_merge);
        true
    }

    fn try_short_circuit(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        let prev_cond = handle;
        if !is_branch(graph, handle) {
            return false;
        }

        let check_mid = |m| match m {
            None => true,
            Some(m) => single_income(graph, m) && !is_branch(graph, m),
        };

        let pnexts: Vec<NodeIndex> = graph.neighbors(prev_cond).collect();
        for next_place in 0..2 {
            let merge_place = next_place ^ 1;
            for has_mids in 0..4 {
                let mid1 = if (has_mids & 1) == 0 {
                    None
                } else {
                    Some(pnexts[merge_place])
                };
                let mid2 = if (has_mids & 2) == 0 {
                    None
                } else {
                    Some(pnexts[next_place])
                };
                if !check_mid(mid1) || !check_mid(mid2) {
                    continue;
                }

                let next_cond = match mid2 {
                    None => pnexts[next_place],
                    Some(m) => graph.neighbors(m).next().unwrap(),
                };
                if !single_income(graph, next_cond) {
                    continue;
                }
                if !is_branch(graph, next_cond) {
                    continue;
                }

                let br_merge = match mid1 {
                    None => pnexts[merge_place],
                    Some(m) => graph.neighbors(m).next().unwrap(),
                };

                let mid3 = match Simplifier::is_shortcut(graph, next_cond, br_merge) {
                    None => continue,
                    Some(m) => m,
                };
                if !check_mid(mid3) {
                    continue;
                }

                self.do_short_circuit(graph, prev_cond, next_cond, br_merge, mid1, mid2, mid3);
                return true;
            }
        }

        false
    }

    fn do_short_circuit(
        &mut self,
        graph: &mut ASTGraph,
        prev_cond: NodeIndex,
        next_cond: NodeIndex,
        br_merge: NodeIndex,
        mid1: Option<NodeIndex>,
        mid2: Option<NodeIndex>,
        mid3: Option<NodeIndex>,
    ) {
        let (_br_prev_false, br_prev_true) = match branch_attr(graph, prev_cond) {
            None => panic!("prev_cond is not branch"),
            Some(br) => (br.0, br.1),
        };
        let (br_next_false, br_next_true) = match branch_attr(graph, next_cond) {
            None => panic!("next_cond is not branch"),
            Some(br) => (br.0, br.1),
        };
        let br_merge_prev = match mid1 {
            None => br_merge,
            Some(x) => x,
        };
        let br_merge_next = match mid3 {
            None => br_merge,
            Some(x) => x,
        };

        let branches = if br_merge_next == br_next_true {
            (br_next_false, br_merge)
        } else {
            (br_next_true, br_merge)
        };

        let mut old_nodes = vec![prev_cond, next_cond];
        for mid in [mid1, mid2, mid3].iter() {
            if let Some(x) = mid {
                old_nodes.push(*x);
            }
        }

        let prev_cond_expr = graph.node_weight(prev_cond).unwrap().ast.clone_bool();
        let next_cond_expr = graph.node_weight(next_cond).unwrap().ast.clone_bool();

        let p_var = format!("{}{}", VAR_PREFIX, prev_cond.index());
        let p_assign_true = Statement::Assign {
            var: p_var.clone(),
            value: Box::new(Expr::Bool(BoolExpr::True)),
        };
        let p_assign = Statement::Assign {
            var: p_var.clone(),
            value: Box::new(if br_merge_next == br_next_true {
                Expr::Bool(next_cond_expr)
            } else {
                Expr::Bool(BoolExpr::Not {
                    value: Box::new(next_cond_expr),
                })
            }),
        };
        let prev_pass_stmt = match mid1 {
            None => p_assign_true,
            Some(mid1) => Statement::Compound {
                first: Box::new(graph.node_weight(mid1).unwrap().ast.clone_state()),
                next: Box::new(p_assign_true),
            },
        };
        let mut prev_unpass_stmt = match mid2 {
            None => p_assign,
            Some(mid2) => Statement::Compound {
                first: Box::new(graph.node_weight(mid2).unwrap().ast.clone_state()),
                next: Box::new(p_assign),
            },
        };
        if let Some(mid3) = mid3 {
            prev_unpass_stmt = Statement::Compound {
                first: Box::new(prev_unpass_stmt),
                next: Box::new(Statement::IfThen {
                    cond: Box::new(BoolExpr::Var {
                        name: p_var.clone(),
                    }),
                    body_then: Box::new(graph.node_weight(mid3).unwrap().ast.clone_state()),
                }),
            };
        }
        let prev_pass_cond = if br_merge_prev == br_prev_true {
            prev_cond_expr
        } else {
            BoolExpr::Not {
                value: Box::new(prev_cond_expr),
            }
        };

        /*
         *  if (prev) { mid1; p = true; }
         *  else { mid2; p = next; if (p) mid3; }
         *  if (p) { ... } else { ... }
         */
        let ast1 = AST::AState(Statement::IfThenElse {
            cond: Box::new(prev_pass_cond),
            body_then: Box::new(prev_pass_stmt),
            body_else: Box::new(prev_unpass_stmt),
        });
        let ast2 = AST::ABool(BoolExpr::Var {
            name: p_var.clone(),
        });

        let (new_node1, new_node2) = replace_short_circuit(graph, old_nodes, branches, ast1, ast2);

        self.queue_add(new_node1);
        self.queue_add(new_node2);
        self.queue_add(br_merge);
        self.new_vars.push(p_var);
    }

    fn is_shortcut(
        graph: &ASTGraph,
        branch: NodeIndex,
        merge: NodeIndex,
    ) -> Option<Option<NodeIndex>> {
        if !is_branch(graph, branch) {
            return None;
        }
        if graph.contains_edge(branch, merge) {
            return Some(None);
        }
        for mid in graph.neighbors(branch) {
            if !single_income(graph, mid) {
                continue;
            }
            if is_branch(graph, mid) {
                continue;
            }
            if graph.contains_edge(mid, merge) {
                return Some(Some(mid));
            }
        }
        None
    }
}

fn replace_in_edges(graph: &mut ASTGraph, old_nodes: &Vec<NodeIndex>, node: NodeIndex) {
    let old_nodes: HashSet<NodeIndex> = HashSet::from_iter(old_nodes.clone());
    for old in old_nodes.iter() {
        let old = *old;
        let prevs: Vec<NodeIndex> = graph.neighbors_directed(old, Direction::Incoming).collect();
        /* replace in edges */
        for prev in prevs.iter() {
            if old_nodes.contains(prev) {
                continue;
            }
            replace_edge_dest(graph, *prev, old, node);
        }
    }
}

fn replace_out_edges_block(graph: &mut ASTGraph, old_nodes: &Vec<NodeIndex>, node: NodeIndex) {
    let old_nodes: HashSet<NodeIndex> = HashSet::from_iter(old_nodes.clone());
    /* record out edges */
    let mut all_nexts: HashSet<NodeIndex> = HashSet::new();
    for old in old_nodes.iter() {
        let old = *old;
        for next in graph.neighbors(old) {
            if old_nodes.contains(&next) {
                continue;
            }
            all_nexts.insert(next);
        }
    }
    /* add out edges */
    if all_nexts.len() == 0 {
        return;
    }
    if all_nexts.len() != 1 {
        panic!("Block has many outgoing edges.");
    }
    let next = all_nexts.iter().next().unwrap();
    graph.add_edge(node, *next, ControlFlowEdge::NotBranch);
}

fn replace_out_edges_cond(
    graph: &mut ASTGraph,
    old_nodes: &Vec<NodeIndex>,
    node: NodeIndex,
    branches: (NodeIndex, NodeIndex),
) {
    let old_nodes: HashSet<NodeIndex> = HashSet::from_iter(old_nodes.clone());
    let (mut br_false, mut br_true) = branches;
    if old_nodes.contains(&br_false) {
        br_false = node;
    }
    if old_nodes.contains(&br_true) {
        br_true = node;
    }
    graph.add_edge(node, br_false, ControlFlowEdge::Branch(false));
    graph.add_edge(node, br_true, ControlFlowEdge::Branch(true));
}

fn remove_old_nodes(graph: &mut ASTGraph, old_nodes: &Vec<NodeIndex>) {
    for old in old_nodes.iter() {
        graph.remove_node(*old).unwrap();
    }
}

fn replace_block(graph: &mut ASTGraph, old_nodes: &Vec<NodeIndex>, ast: AST) -> NodeIndex {
    let node = graph.add_node(NodeAttr::new_node(ast));
    replace_in_edges(graph, old_nodes, node);
    replace_out_edges_block(graph, old_nodes, node);
    remove_old_nodes(graph, old_nodes);
    node
}

fn replace_condition(
    graph: &mut ASTGraph,
    old_nodes: Vec<NodeIndex>,
    branches: (NodeIndex, NodeIndex),
    ast: AST,
) -> NodeIndex {
    let node = graph.add_node(NodeAttr::new_node(ast));
    replace_in_edges(graph, &old_nodes, node);
    replace_out_edges_cond(graph, &old_nodes, node, branches);
    remove_old_nodes(graph, &old_nodes);
    node
}

fn replace_short_circuit(
    graph: &mut ASTGraph,
    old_nodes: Vec<NodeIndex>,
    branches: (NodeIndex, NodeIndex),
    ast1: AST,
    ast2: AST,
) -> (NodeIndex, NodeIndex) {
    let node1 = graph.add_node(NodeAttr::new_node(ast1));
    let node2 = graph.add_node(NodeAttr::new_node(ast2));
    replace_in_edges(graph, &old_nodes, node1);
    replace_out_edges_cond(graph, &old_nodes, node2, branches);
    graph.add_edge(node1, node2, ControlFlowEdge::NotBranch);
    remove_old_nodes(graph, &old_nodes);
    (node1, node2)
}

fn replace_edge_dest(
    graph: &mut ASTGraph,
    id_from: NodeIndex,
    id_to_old: NodeIndex,
    id_to_new: NodeIndex,
) {
    let edge = graph.find_edge(id_from, id_to_old).unwrap();
    let edge_attr = graph.remove_edge(edge).unwrap();
    graph.add_edge(id_from, id_to_new, edge_attr);
}

struct Splitter<'a> {
    graph: &'a mut ASTGraph,
    sink: NodeIndex,
    doms: dominators::Dominators<NodeIndex>,
    rdoms: dominators::Dominators<NodeIndex>,
    visited: HashSet<NodeIndex>,
}

impl<'a> Drop for Splitter<'a> {
    fn drop(&mut self) {
        self.graph.remove_node(self.sink);
    }
}

impl<'a> Splitter<'a> {
    pub fn split(graph: &'a mut ASTGraph, entry: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
        let mut splitter = Splitter::new(graph, entry);

        let mut order: Vec<NodeIndex> = match toposort::<&ASTGraph>(splitter.graph, None) {
            Ok(ord) => ord,
            Err(cyc) => panic!("cycle is found when splitting: {:?}", cyc),
        };
        order.reverse();
        for cond in order {
            if !is_branch(splitter.graph, cond) {
                continue;
            }
            match splitter.try_split_under(cond) {
                None => continue,
                Some(ans) => return Some(ans),
            };
        }
        None
    }

    fn new(graph: &'a mut ASTGraph, entry: NodeIndex) -> Splitter {
        let sink = add_halt(graph, true)[0];
        let rgraph = Splitter::build_reverse_graph(graph);
        let doms = dominators::simple_fast::<&ASTGraph>(graph, entry);
        let rdoms = dominators::simple_fast(&rgraph, sink);
        Splitter {
            graph,
            sink,
            doms,
            rdoms,
            visited: HashSet::new(),
        }
    }

    fn build_reverse_graph(graph: &ASTGraph) -> DiGraphMap<NodeIndex, usize> {
        let mut rgraph = DiGraphMap::new();
        for node in graph.node_indices() {
            rgraph.add_node(node);
        }
        for edge in graph.edge_references() {
            rgraph.add_edge(edge.target(), edge.source(), 0);
        }
        rgraph
    }

    fn try_split_under(&mut self, head: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
        let imm_post_dom = match self.rdoms.immediate_dominator(head) {
            None => return None,
            Some(x) => x,
        };
        self.visited = HashSet::new();
        self.dfs(head, head, imm_post_dom)
    }

    fn dfs(
        &mut self,
        head: NodeIndex,
        current: NodeIndex,
        imm_post_dom: NodeIndex,
    ) -> Option<(NodeIndex, NodeIndex)> {
        self.visited.insert(current);
        if current == imm_post_dom {
            return None;
        }
        let nexts: Vec<NodeIndex> = self.graph.neighbors(current).collect();
        for next in nexts {
            if self.visited.contains(&next) {
                continue;
            }
            let ans = self.dfs(head, next, imm_post_dom);
            if ans.is_some() {
                return ans;
            }
        }
        let mut dom_iter = self.doms.dominators(current).unwrap();
        if !dom_iter.contains(&head) {
            return Some((current, self.split_node(current)));
        }
        None
    }

    fn split_node(&mut self, node: NodeIndex) -> NodeIndex {
        let ast = self.graph.node_weight(node).unwrap().ast.clone();
        let new_node = self.graph.add_node(NodeAttr::new_node(ast));

        let nexts: Vec<(NodeIndex, ControlFlowEdge)> = self
            .graph
            .edges_directed(node, Direction::Outgoing)
            .map(|e| (e.target(), *e.weight()))
            .collect();
        for (next, weight) in nexts {
            self.graph.add_edge(new_node, next, weight);
        }

        let prevs: Vec<NodeIndex> = self
            .graph
            .neighbors_directed(node, Direction::Incoming)
            .collect();
        for prev in prevs {
            if self.visited.contains(&prev) {
                continue;
            }
            replace_edge_dest(self.graph, prev, node, new_node);
        }

        new_node
    }
}
