use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::iter::FromIterator;

use petgraph::data::DataMap;
use petgraph::visit::{EdgeRef, IntoEdgeReferences, IntoNodeIdentifiers};
use petgraph::Direction;

use crate::ast::*;
use crate::graph::*;

const VAR_PREFIX: &str = "pred_";

pub struct ASTRecResult {
    pub stmt: Statement,
    pub new_vars: Vec<String>,
}

pub fn ast_structure<N>(graph: &ControlFlowGraph<N>, entry: NodeIndex) -> Option<ASTRecResult> {
    let (mut ast_graph, entry) = construct_ast_graph(graph, entry)?;
    add_halt(&mut ast_graph);
    //dot_graph(&ast_graph, entry);
    let mut new_vars = Vec::new();
    loop {
        let result = Simplifier::simplify(&mut ast_graph, entry);
        new_vars.extend(result.new_vars);
        //dot_graph(&ast_graph, entry);
        if ast_graph.node_count() == 1 {
            break;
        }
        if !result.updated {
            return None;
        }
    }
    let ast = &ast_graph.node_weights().next()?.ast;
    Some(ASTRecResult {
        stmt: ast.clone_state(),
        new_vars,
    })
}

fn add_halt(graph: &mut ASTGraph) {
    let nodes: Vec<NodeIndex> = graph.node_identifiers().collect();
    for node in nodes.iter() {
        let node = *node;
        if graph.edges(node).count() == 0 {
            let halt = graph.add_node(NodeAttr::new_node(AST::AState(Statement::Halt)));
            graph.add_edge(node, halt, ControlFlowEdge::NotBranch);
        }
    }
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
    let new_entry = ast_graph.add_node(NodeAttr::new_node(AST::AState(Statement::Nop)));
    ast_graph.add_edge(new_entry, entry, ControlFlowEdge::NotBranch);
    Some((ast_graph, new_entry))
}

type ASTGraph = ControlFlowGraph<NodeAttr>;

#[derive(Debug)]
struct NodeAttr {
    ast: AST,
}

impl ToString for NodeAttr {
    fn to_string(&self) -> String {
        self.ast.to_string()
    }
}

impl NodeAttr {
    pub fn new(origin: NodeIndex, is_branch: bool) -> NodeAttr {
        NodeAttr {
            ast: match is_branch {
                false => AST::AState(Statement::Original { node_idx: origin }),
                true => AST::ABool(BoolExpr::Original { node_idx: origin }),
            },
        }
    }

    pub fn new_node(ast: AST) -> NodeAttr {
        NodeAttr { ast }
    }
}

fn is_branch(graph: &ASTGraph, node: NodeIndex) -> bool {
    graph.edges(node).count() == 2
}

fn branch_attr(graph: &ASTGraph, node: NodeIndex) -> Option<(NodeIndex, NodeIndex)> {
    let mut it = graph.edges(node);
    let e1 = match it.next() {
        None => return None,
        Some(e) => e,
    };
    let e2 = match it.next() {
        None => return None,
        Some(e) => e,
    };
    assert!(it.next().is_none());
    let br_e1 = match e1.weight() {
        ControlFlowEdge::NotBranch => return None,
        ControlFlowEdge::Branch(br) => *br,
    };
    let br_e2 = match e2.weight() {
        ControlFlowEdge::NotBranch => return None,
        ControlFlowEdge::Branch(br) => *br,
    };
    assert!(br_e1 != br_e2);
    match br_e1 {
        false => Some((e1.target(), e2.target())),
        true => Some((e2.target(), e1.target())),
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
        if self.try_while(graph, handle) {
            return true;
        }
        if self.try_do_while(graph, handle) {
            return true;
        }
        if self.try_dumb_loop(graph, handle) {
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
            let _new_node = Simplifier::replace_block(graph, vec![handle, next], ast_next);
            true
        } else {
            let loop_back = graph.contains_edge(next, handle);
            let new_node = Simplifier::replace_block(
                graph,
                vec![handle, next],
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

        let new_node = Simplifier::replace_block(
            graph,
            vec![part, cond],
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
        let new_node = Simplifier::replace_block(
            graph,
            old_nodes,
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

        let new_node = Simplifier::replace_block(
            graph,
            vec![cond],
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
        let new_node = Simplifier::replace_condition(graph, vec![handle, prev_cond], branches, ast);

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

        let (new_node1, new_node2) =
            Simplifier::replace_short_circuit(graph, old_nodes, branches, ast1, ast2);

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

    fn get_loop_branch(
        graph: &ASTGraph,
        head: NodeIndex,
        branch: (NodeIndex, NodeIndex),
    ) -> Option<((NodeIndex, NodeIndex), Option<NodeIndex>)> {
        let test_next = |node| {
            if graph.edges(node).count() != 1 {
                return false;
            }
            if !single_income(graph, node) {
                return false;
            }
            graph.neighbors(node).next().unwrap() == head
        };
        let (br_false, br_true) = branch;
        let br_loop;
        let br_exit;
        let mut next: Option<NodeIndex> = None;
        if br_false == head {
            br_loop = br_false;
            br_exit = br_true;
        } else if br_true == head {
            br_loop = br_true;
            br_exit = br_false;
        } else if test_next(br_false) {
            br_loop = br_false;
            br_exit = br_true;
            next = Some(br_false);
        } else if test_next(br_true) {
            br_loop = br_true;
            br_exit = br_false;
            next = Some(br_true);
        } else {
            return None;
        }
        Some(((br_loop, br_exit), next))
    }

    fn try_while(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        let (br_false, br_true) = match branch_attr(graph, handle) {
            None => return false,
            Some(br) => (br.0, br.1),
        };

        let br_loop;
        //let br_exit;
        let next;
        match Simplifier::get_loop_branch(graph, handle, (br_false, br_true)) {
            None => return false,
            Some(res) => {
                br_loop = res.0 .0;
                //br_exit = res.0 .1;
                next = res.1;
            }
        };

        if let Some(next) = next {
            if !single_income(graph, next) {
                return false;
            }
        }

        let mut old_nodes = vec![handle];
        if let Some(x) = next {
            old_nodes.push(x);
        }

        let loop_cond = graph.node_weight(handle).unwrap().ast.clone_bool();
        let loop_cond = if br_loop == br_true {
            loop_cond
        } else {
            BoolExpr::Not {
                value: Box::new(loop_cond),
            }
        };

        let body_stmt = match next {
            None => Statement::Nop,
            Some(next) => graph.node_weight(next).unwrap().ast.clone_state(),
        };

        /* while (cond) { handle; } */
        let ast = AST::AState(Statement::While {
            cond: Box::new(loop_cond),
            body: Box::new(body_stmt),
        });

        let new_node = Simplifier::replace_block(graph, old_nodes, ast);
        self.queue_add(new_node);
        true
    }

    fn try_do_while(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        let cond = match graph.neighbors(handle).next() {
            None => return false,
            Some(x) => x,
        };
        if !single_income(graph, cond) {
            return false;
        }
        let (br_false, br_true) = match branch_attr(graph, cond) {
            None => return false,
            Some(br) => (br.0, br.1),
        };

        let br_loop;
        //let br_exit;
        let next;
        match Simplifier::get_loop_branch(graph, handle, (br_false, br_true)) {
            None => return false,
            Some(res) => {
                br_loop = res.0 .0;
                //br_exit = res.0 .1;
                next = res.1;
            }
        };

        if let Some(next) = next {
            if !single_income(graph, next) {
                return false;
            }
        }

        let mut old_nodes = vec![handle, cond];
        if let Some(x) = next {
            old_nodes.push(x);
        }

        let cond_expr = graph.node_weight(cond).unwrap().ast.clone_bool();
        let not_cond_expr = BoolExpr::Not {
            value: Box::new(cond_expr.clone()),
        };
        let loop_cond;
        let break_cond;
        if br_loop == br_false {
            loop_cond = not_cond_expr;
            break_cond = cond_expr;
        } else {
            loop_cond = cond_expr;
            break_cond = not_cond_expr;
        };

        let handle_stmt = graph.node_weight(handle).unwrap().ast.clone_state();
        let ast = match next {
            /* do { handle; } while (cond); */
            None => AST::AState(Statement::DoWhile {
                cond: Box::new(loop_cond),
                body: Box::new(handle_stmt),
            }),
            /* while (true) { handle; if (cond) break; next; } */
            Some(next) => AST::AState(Statement::While {
                cond: Box::new(BoolExpr::True),
                body: Box::new(Statement::Compound {
                    first: Box::new(handle_stmt),
                    next: Box::new(Statement::Compound {
                        first: Box::new(Statement::IfThen {
                            cond: Box::new(break_cond),
                            body_then: Box::new(Statement::Break),
                        }),
                        next: Box::new(graph.node_weight(next).unwrap().ast.clone_state()),
                    }),
                }),
            }),
        };

        let new_node = Simplifier::replace_block(graph, old_nodes, ast);
        self.queue_add(new_node);
        true
    }

    fn try_dumb_loop(&mut self, graph: &mut ASTGraph, handle: NodeIndex) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        let next_tmp = match graph.neighbors(handle).next() {
            None => return false,
            Some(x) => x,
        };
        let next = if next_tmp == handle {
            None
        } else {
            if !single_income(graph, next_tmp) {
                return false;
            }
            if graph.edges(next_tmp).count() != 1 {
                return false;
            }
            if graph.neighbors(next_tmp).next() != Some(handle) {
                return false;
            }
            Some(next_tmp)
        };

        let mut old_nodes = vec![handle];
        if let Some(x) = next {
            old_nodes.push(x);
        }

        let handle_stmt = graph.node_weight(handle).unwrap().ast.clone_state();
        let ast = AST::AState(Statement::While {
            cond: Box::new(BoolExpr::True),
            body: Box::new(match next {
                None => handle_stmt,
                Some(next) => Statement::Compound {
                    first: Box::new(handle_stmt),
                    next: Box::new(graph.node_weight(next).unwrap().ast.clone_state()),
                },
            }),
        });

        let new_node = Simplifier::replace_block(graph, old_nodes, ast);
        self.queue_add(new_node);
        true
    }

    fn replace_in_edges(graph: &mut ASTGraph, old_nodes: &Vec<NodeIndex>, node: NodeIndex) {
        let old_nodes: HashSet<NodeIndex> = HashSet::from_iter(old_nodes.clone());
        for old in old_nodes.iter() {
            let old = *old;
            let prevs: Vec<NodeIndex> =
                graph.neighbors_directed(old, Direction::Incoming).collect();
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

    fn replace_block(graph: &mut ASTGraph, old_nodes: Vec<NodeIndex>, ast: AST) -> NodeIndex {
        let node = graph.add_node(NodeAttr::new_node(ast));
        Simplifier::replace_in_edges(graph, &old_nodes, node);
        Simplifier::replace_out_edges_block(graph, &old_nodes, node);
        Simplifier::remove_old_nodes(graph, &old_nodes);
        node
    }

    fn replace_condition(
        graph: &mut ASTGraph,
        old_nodes: Vec<NodeIndex>,
        branches: (NodeIndex, NodeIndex),
        ast: AST,
    ) -> NodeIndex {
        let node = graph.add_node(NodeAttr::new_node(ast));
        Simplifier::replace_in_edges(graph, &old_nodes, node);
        Simplifier::replace_out_edges_cond(graph, &old_nodes, node, branches);
        Simplifier::remove_old_nodes(graph, &old_nodes);
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
        Simplifier::replace_in_edges(graph, &old_nodes, node1);
        Simplifier::replace_out_edges_cond(graph, &old_nodes, node2, branches);
        graph.add_edge(node1, node2, ControlFlowEdge::NotBranch);
        Simplifier::remove_old_nodes(graph, &old_nodes);
        (node1, node2)
    }
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
