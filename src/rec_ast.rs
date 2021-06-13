use std::collections::HashSet;
use std::collections::VecDeque;
use std::iter::FromIterator;

use itertools::Itertools;

use crate::ast::*;
use crate::graph::*;

const VAR_PREFIX: &str = "pred_";

pub struct ASTRecResult {
    pub stmt: Statement,
    pub new_vars: Vec<String>,
}

pub fn ast_structure(graph: &dyn Graph, entry: usize) -> Option<ASTRecResult> {
    let mut ast_graph = construct_ast_graph(graph)?;
    let entry = add_entry(&mut ast_graph, entry);
    add_halt(&mut ast_graph);
    let result = Simplifier::simplify(&mut ast_graph, entry)?;
    let ast = &ast_graph.read_note(result.node).ast;
    Some(ASTRecResult {
        stmt: ast.clone_state(),
        new_vars: result.new_vars,
    })
}

fn add_entry(graph: &mut ASTGraph, entry: usize) -> usize {
    let new_entry = graph.add_node(NodeAttr::new_node(
        BranchAttr::NotBranch,
        AST::AState(Statement::Nop),
    ));
    graph.add_edge(new_entry, entry);
    new_entry
}

fn add_halt(graph: &mut ASTGraph) {
    let nodes: Vec<usize> = graph.node_iter().collect();
    for node in nodes.iter() {
        let node = *node;
        if graph.edge_iter(node).count() == 0 {
            let halt = graph.add_node(NodeAttr::new_node(BranchAttr::NotBranch, AST::AState(Statement::Halt)));
            graph.add_edge(node, halt);
        }
    }
}

fn construct_ast_graph(graph: &dyn Graph) -> Option<ASTGraph> {
    let mut loop_graph = ASTGraph::new();
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

type ASTGraph = EditableGraph<NodeAttr>;

#[derive(Debug, Clone)]
enum BranchAttr {
    NotBranch,
    Branch(usize, usize), // (false_branch, true_branch)
}

struct NodeAttr {
    branch_attr: BranchAttr,
    ast: AST,
}

impl NodeAttr {
    pub fn new(origin: usize, branch_attr: BranchAttr) -> NodeAttr {
        NodeAttr {
            branch_attr: branch_attr.clone(),
            ast: match branch_attr {
                BranchAttr::NotBranch => AST::AState(Statement::Original { node_num: origin }),
                BranchAttr::Branch(_, _) => AST::ABool(BoolExpr::Original { node_num: origin }),
            },
        }
    }

    pub fn new_node(branch_attr: BranchAttr, ast: AST) -> NodeAttr {
        NodeAttr { branch_attr, ast }
    }
}

fn is_branch(graph: &ASTGraph, node: usize) -> bool {
    match graph.read_note(node).branch_attr {
        BranchAttr::NotBranch => false,
        BranchAttr::Branch(_, _) => true,
    }
}

struct SimplifyResult {
    node: usize,
    new_vars: Vec<String>,
}

struct Simplifier {
    queue_1: VecDeque<usize>,
    queue_2: VecDeque<usize>,
    visited: Vec<bool>,
    entry: usize,
    new_vars: Vec<String>,
}

impl Simplifier {
    pub fn simplify(graph: &mut ASTGraph, entry: usize) -> Option<SimplifyResult> {
        let n = graph.node_num();
        let mut state = Simplifier {
            queue_1: VecDeque::new(),
            queue_2: VecDeque::new(),
            visited: vec![false; n],
            entry,
            new_vars: Vec::new(),
        };
        state.dfs(graph, entry);
        state.queue_reverse();
        state.simplify_all(graph);
        if graph.node_num() != 1 {
            return None;
        }
        let node = graph.node_iter().next()?;
        Some(SimplifyResult {
            node,
            new_vars: state.new_vars,
        })
    }

    fn dfs(&mut self, graph: &mut ASTGraph, current: usize) {
        self.visited[current] = true;
        let nexts: Vec<usize> = graph.edge_iter(current).collect();
        for next in nexts.iter() {
            let next = *next;
            if !self.visited[next] {
                self.dfs(graph, next);
            }
        }
        self.queue_add(graph, current);
    }

    fn queue_add(&mut self, graph: &ASTGraph, node: usize) {
        let rcount = graph.reverse_edge_iter(node).count();
        if rcount == 1 {
            self.queue_1.push_front(node);
        } else if rcount == 2 {
            self.queue_2.push_front(node);
        }
    }

    fn queue_reverse(&mut self) {
        self.queue_1 = self.queue_1.iter().rev().map(|x| *x).collect();
        self.queue_2 = self.queue_2.iter().rev().map(|x| *x).collect();
    }

    fn simplify_all(&mut self, graph: &mut ASTGraph) {
        loop {
            if let Some(node) = self.queue_1.pop_front() {
                if graph.contain_node(node) {
                    self.simplify_type_1(graph, node);
                }
            } else if let Some(node) = self.queue_2.pop_front() {
                if graph.contain_node(node) {
                    self.simplify_type_2(graph, node);
                }
            } else {
                break;
            }
        }
    }

    fn simplify_type_1(&mut self, graph: &mut ASTGraph, handle: usize) {
        if self.try_seq(graph, handle) {
            return;
        }
        if self.try_if_then(graph, handle) {
            return;
        }
        if self.try_if_then_else(graph, handle) {
            return;
        }
        if self.try_branch_or(graph, handle) {
            return;
        }
        if self.try_short_circuit(graph, handle) {
            return;
        }
    }

    fn try_seq(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        if graph.reverse_edge_iter(handle).count() != 1 {
            return false;
        }
        let prev = match graph.reverse_edge_iter(handle).next() {
            None => return false,
            Some(x) => x,
        };
        if is_branch(graph, prev) {
            return false;
        }

        if prev == self.entry {
            if graph.edge_iter(handle).count() != 0 {
                return false;
            }
            let _new_node = Simplifier::replace_block(
                graph,
                vec![handle, prev],
                graph.read_note(handle).ast.clone(),
            );
            true
        } else {
            let next = graph.edge_iter(handle).next();
            let new_node = Simplifier::replace_block(
                graph,
                vec![handle, prev],
                AST::AState(Statement::Compound {
                    first: Box::new(graph.read_note(prev).ast.clone_state()),
                    next: Box::new(graph.read_note(handle).ast.clone_state()),
                }),
            );
            if next == Some(prev) {
                graph.add_edge(new_node, new_node);
            }
            self.queue_add(graph, new_node);
            true
        }
    }

    fn try_if_then(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        if graph.reverse_edge_iter(handle).count() != 1 {
            return false;
        }
        let cond = graph.reverse_edge_iter(handle).next().unwrap();
        let next = match graph.edge_iter(cond).filter(|x| *x != handle).next() {
            None => return false,
            Some(x) => x,
        };
        let mut refresh_next = false;
        if let Some(next2) = graph.edge_iter(handle).next() {
            if next2 != next {
                return false;
            }
            refresh_next = true;
        }

        let cond_expr = graph.read_note(cond).ast.clone_bool();
        let (_br_false, br_true) = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };

        let new_node = Simplifier::replace_block(
            graph,
            vec![handle, cond],
            AST::AState(Statement::IfThen {
                cond: Box::new(if br_true == handle {
                    cond_expr
                } else {
                    BoolExpr::Not {
                        value: Box::new(cond_expr),
                    }
                }),
                body_then: Box::new(graph.read_note(handle).ast.clone_state()),
            }),
        );

        if next == cond {
            graph.add_edge(new_node, new_node);
        }

        self.queue_add(graph, new_node);
        if refresh_next {
            self.queue_add(graph, next);
        }
        true
    }

    fn try_if_then_else(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        if graph.reverse_edge_iter(handle).count() != 1 {
            return false;
        }
        let cond = graph.reverse_edge_iter(handle).next().unwrap();
        let other = match graph.edge_iter(cond).filter(|x| *x != handle).next() {
            None => return false,
            Some(x) => x,
        };
        if is_branch(graph, other) {
            return false;
        }
        if graph.reverse_edge_iter(other).count() != 1 {
            return false;
        }
        let next = graph.edge_iter(other).next();
        let next2 = graph.edge_iter(handle).next();
        if next2 != next {
            return false;
        }

        let old_nodes = vec![cond, handle, other];
        let (br_false, br_true) = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };

        let new_node = Simplifier::replace_block(
            graph,
            old_nodes,
            AST::AState(Statement::IfThenElse {
                cond: Box::new(graph.read_note(cond).ast.clone_bool()),
                body_then: Box::new(graph.read_note(br_true).ast.clone_state()),
                body_else: Box::new(graph.read_note(br_false).ast.clone_state()),
            }),
        );

        if next == Some(cond) {
            graph.add_edge(new_node, new_node);
        }

        self.queue_add(graph, new_node);
        if let Some(next) = next {
            self.queue_add(graph, next);
        }
        true
    }

    fn try_branch_or(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let (handle_false, handle_true) = match graph.read_note(handle).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };
        if graph.reverse_edge_iter(handle).count() != 1 {
            return false;
        }

        let prev = graph.reverse_edge_iter(handle).next().unwrap();
        if !is_branch(graph, prev) {
            return false;
        }
        let cur_branch = handle;
        let prev_cond = prev;
        let (_cond_false, cond_true) = match graph.read_note(prev_cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };
        let other = graph
            .edge_iter(prev_cond)
            .filter(|x| *x != cur_branch)
            .next()
            .unwrap();
        if other != handle_true && other != handle_false {
            return false;
        }
        let branch_merge = other;

        let handle_expr = match &graph.read_note(handle).ast {
            AST::AState(_) => panic!("not a branch"),
            AST::ABool(expr) => expr.clone(),
        };
        let cond_expr = match &graph.read_note(prev_cond).ast {
            AST::AState(_) => panic!("not a branch"),
            AST::ABool(expr) => expr.clone(),
        };

        let branch_attr = if branch_merge == handle_true {
            BranchAttr::Branch(handle_false, handle_true)
        } else {
            BranchAttr::Branch(handle_true, handle_false)
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
        let new_node =
            Simplifier::replace_condition(graph, vec![handle, prev_cond], branch_attr, ast);

        if handle_false == prev_cond || handle_true == prev_cond {
            graph.add_edge(new_node, new_node);
        }

        self.queue_add(graph, new_node);
        self.queue_add(graph, branch_merge);
        true
    }

    fn try_short_circuit(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        let cur_next = match graph.edge_iter(handle).next() {
            None => return false,
            Some(x) => x,
        };
        let prev_cond = graph.reverse_edge_iter(handle).next().unwrap();
        let (_br_prev_false, br_prev_true) = match graph.read_note(prev_cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };

        let other_tmp = graph
            .edge_iter(prev_cond)
            .filter(|x| *x != handle)
            .next()
            .unwrap();
        let other;
        let other_next;
        if is_branch(graph, other_tmp) {
            other = None;
            other_next = other_tmp;
        } else {
            other = Some(other_tmp);
            other_next = match graph.edge_iter(other_tmp).next() {
                None => return false,
                Some(x) => x,
            };
        }

        let mid1: Option<usize>;
        let mid2: Option<usize>;
        let next_cond: usize;
        let br_merge: usize;
        if is_branch(graph, cur_next) && graph.edge_iter(cur_next).contains(&other_next) {
            mid1 = other;
            mid2 = Some(handle);
            next_cond = cur_next;
            br_merge = other_next;
        } else if is_branch(graph, other_next) && graph.edge_iter(other_next).contains(&cur_next) {
            mid1 = Some(handle);
            mid2 = other;
            next_cond = other_next;
            br_merge = cur_next;
        } else {
            return false;
        }

        if graph.reverse_edge_iter(next_cond).count() != 1 {
            return false;
        }
        for mid in [mid1, mid2].iter() {
            if let Some(x) = *mid {
                if graph.reverse_edge_iter(x).count() != 1 {
                    return false;
                }
                if is_branch(graph, x) {
                    return false;
                }
            }
        }

        let (br_next_false, br_next_true) = match graph.read_note(next_cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };
        let br_merge1 = match mid1 {
            None => br_merge,
            Some(x) => x,
        };

        let branch_attr = if br_merge == br_next_true {
            BranchAttr::Branch(br_next_false, br_next_true)
        } else {
            BranchAttr::Branch(br_next_true, br_next_false)
        };

        let mut old_nodes = vec![prev_cond, next_cond];
        for mid in [mid1, mid2].iter() {
            if let Some(x) = mid {
                old_nodes.push(*x);
            }
        }

        let prev_cond_expr = graph.read_note(prev_cond).ast.clone_bool();
        let next_cond_expr = graph.read_note(next_cond).ast.clone_bool();

        let p_var = format!("{}{}", VAR_PREFIX, prev_cond);
        let p_assign_true = Statement::Assign {
            var: p_var.clone(),
            value: Box::new(Expr::Bool(BoolExpr::True)),
        };
        let p_assign = Statement::Assign {
            var: p_var.clone(),
            value: Box::new(if br_merge == br_next_true {
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
                first: Box::new(graph.read_note(mid1).ast.clone_state()),
                next: Box::new(p_assign_true),
            },
        };
        let prev_unpass_stmt = match mid2 {
            None => p_assign,
            Some(mid2) => Statement::Compound {
                first: Box::new(graph.read_note(mid2).ast.clone_state()),
                next: Box::new(p_assign),
            },
        };
        let prev_pass_cond = if br_merge1 == br_prev_true {
            prev_cond_expr
        } else {
            BoolExpr::Not {
                value: Box::new(prev_cond_expr),
            }
        };

        /*
         *  if (prev) { mid1; p = true; }
         *  else { mid2; p = next; }
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
            Simplifier::replace_short_circuit(graph, old_nodes, branch_attr, ast1, ast2);

        if br_next_false == prev_cond || br_next_true == prev_cond {
            graph.add_edge(new_node2, new_node1);
        }

        self.queue_add(graph, new_node1);
        self.queue_add(graph, new_node2);
        self.queue_add(graph, br_merge);
        self.new_vars.push(p_var);
        true
    }

    fn simplify_type_2(&mut self, graph: &mut ASTGraph, handle: usize) {
        if self.try_while(graph, handle) {
            return;
        }
        if self.try_do_while(graph, handle) {
            return;
        }
        if self.try_dumb_loop(graph, handle) {
            return;
        }
    }

    fn get_loop_branch(
        graph: &ASTGraph,
        head: usize,
        branches: (usize, usize),
    ) -> Option<(usize, Option<usize>)> {
        let test_next = |node| {
            let nexts: Vec<usize> = graph.edge_iter(node).collect();
            if nexts.len() != 1 {
                return false;
            }
            if graph.reverse_edge_iter(node).count() != 1 {
                return false;
            }
            *nexts.first().unwrap() == head
        };
        let (br_false, br_true) = branches;
        let br_loop;
        let mut next: Option<usize> = None;
        if br_false == head {
            br_loop = br_false;
        } else if br_true == head {
            br_loop = br_true;
        } else if test_next(br_false) {
            br_loop = br_false;
            next = Some(br_false);
        } else if test_next(br_true) {
            br_loop = br_true;
            next = Some(br_true);
        } else {
            return None;
        }
        Some((br_loop, next))
    }

    fn try_while(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let (br_false, br_true) = match graph.read_note(handle).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };

        let br_loop;
        let next;
        match Simplifier::get_loop_branch(graph, handle, (br_false, br_true)) {
            None => return false,
            Some(res) => {
                br_loop = res.0;
                next = res.1;
            }
        };

        if let Some(next) = next {
            if graph.reverse_edge_iter(next).count() != 1 {
                return false;
            }
        }

        let mut old_nodes = vec![handle];
        if let Some(x) = next {
            old_nodes.push(x);
        }

        let loop_cond = graph.read_note(handle).ast.clone_bool();
        let loop_cond = if br_loop == br_true {
            loop_cond
        } else {
            BoolExpr::Not {
                value: Box::new(loop_cond),
            }
        };

        let body_stmt = match next {
            None => Statement::Nop,
            Some(next) => graph.read_note(next).ast.clone_state(),
        };

        /* while (cond) { handle; } */
        let ast = AST::AState(Statement::While {
            cond: Box::new(loop_cond),
            body: Box::new(body_stmt),
        });

        let new_node = Simplifier::replace_block(graph, old_nodes, ast);
        self.queue_add(graph, new_node);
        true
    }

    fn try_do_while(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        let cond = match graph.edge_iter(handle).next() {
            None => return false,
            Some(x) => x,
        };
        let (br_false, br_true) = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };

        let br_loop;
        let next;
        match Simplifier::get_loop_branch(graph, handle, (br_false, br_true)) {
            None => return false,
            Some(res) => {
                br_loop = res.0;
                next = res.1;
            }
        };

        if let Some(next) = next {
            if graph.reverse_edge_iter(next).count() != 1 {
                return false;
            }
        }

        let mut old_nodes = vec![handle, cond];
        if let Some(x) = next {
            old_nodes.push(x);
        }

        let cond_expr = graph.read_note(cond).ast.clone_bool();
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

        let handle_stmt = graph.read_note(handle).ast.clone_state();
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
                        next: Box::new(graph.read_note(next).ast.clone_state()),
                    }),
                }),
            }),
        };

        let new_node = Simplifier::replace_block(graph, old_nodes, ast);
        self.queue_add(graph, new_node);
        true
    }

    fn try_dumb_loop(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if is_branch(graph, handle) {
            return false;
        }
        let next_tmp = match graph.edge_iter(handle).next() {
            None => return false,
            Some(x) => x,
        };
        let next = if next_tmp == handle {
            None
        } else {
            if graph.reverse_edge_iter(next_tmp).count() != 1 {
                return false;
            }
            if graph.edge_iter(next_tmp).count() != 1 {
                return false;
            }
            if graph.edge_iter(next_tmp).next() != Some(handle) {
                return false;
            }
            Some(next_tmp)
        };

        let mut old_nodes = vec![handle];
        if let Some(x) = next {
            old_nodes.push(x);
        }

        let handle_stmt = graph.read_note(handle).ast.clone_state();
        let ast = AST::AState(Statement::While {
            cond: Box::new(BoolExpr::True),
            body: Box::new(match next {
                None => handle_stmt,
                Some(next) => Statement::Compound {
                    first: Box::new(handle_stmt),
                    next: Box::new(graph.read_note(next).ast.clone_state()),
                },
            }),
        });

        let new_node = Simplifier::replace_block(graph, old_nodes, ast);
        self.queue_add(graph, new_node);
        true
    }

    fn replace_in_edges(graph: &mut ASTGraph, old_nodes: &Vec<usize>, node: usize) {
        let old_nodes: HashSet<usize> = HashSet::from_iter(old_nodes.clone());
        for old in old_nodes.iter() {
            let old = *old;
            let prevs: Vec<usize> = graph.reverse_edge_iter(old).collect();
            /* replace in edges */
            for prev in prevs.iter() {
                if old_nodes.contains(prev) {
                    continue;
                }
                replace_edge_dest(graph, *prev, old, node);
            }
        }
    }

    fn replace_out_edges_block(graph: &mut ASTGraph, old_nodes: &Vec<usize>, node: usize) {
        let old_nodes: HashSet<usize> = HashSet::from_iter(old_nodes.clone());
        /* record out edges */
        let mut all_nexts: HashSet<usize> = HashSet::new();
        for old in old_nodes.iter() {
            let old = *old;
            let this_nexts: Vec<usize> = graph.edge_iter(old).collect();
            for next in this_nexts.iter() {
                if old_nodes.contains(next) {
                    continue;
                }
                all_nexts.insert(*next);
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
        graph.add_edge(node, *next);
    }

    fn replace_out_edges_cond(
        graph: &mut ASTGraph,
        old_nodes: &Vec<usize>,
        node: usize,
        branch_attr: BranchAttr,
    ) {
        let old_nodes: HashSet<usize> = HashSet::from_iter(old_nodes.clone());
        if let BranchAttr::Branch(mut br_false, mut br_true) = branch_attr {
            if old_nodes.contains(&br_false) {
                br_false = node;
            }
            if old_nodes.contains(&br_true) {
                br_true = node;
            }
            graph.get_note_mut(node).branch_attr = BranchAttr::Branch(br_false, br_true);
            graph.add_edge(node, br_false);
            graph.add_edge(node, br_true);
        } else {
            panic!("not a branch");
        }
    }

    fn remove_old_nodes(graph: &mut ASTGraph, old_nodes: &Vec<usize>) {
        for old in old_nodes.iter() {
            graph.del_node(*old);
        }
    }

    fn replace_block(graph: &mut ASTGraph, old_nodes: Vec<usize>, ast: AST) -> usize {
        let node = graph.add_node(NodeAttr::new_node(BranchAttr::NotBranch, ast));
        Simplifier::replace_in_edges(graph, &old_nodes, node);
        Simplifier::replace_out_edges_block(graph, &old_nodes, node);
        Simplifier::remove_old_nodes(graph, &old_nodes);
        node
    }

    fn replace_condition(
        graph: &mut ASTGraph,
        old_nodes: Vec<usize>,
        branch_attr: BranchAttr,
        ast: AST,
    ) -> usize {
        let node = graph.add_node(NodeAttr::new_node(
            BranchAttr::NotBranch, /* edit later */
            ast,
        ));
        Simplifier::replace_in_edges(graph, &old_nodes, node);
        Simplifier::replace_out_edges_cond(graph, &old_nodes, node, branch_attr);
        Simplifier::remove_old_nodes(graph, &old_nodes);
        node
    }

    fn replace_short_circuit(
        graph: &mut ASTGraph,
        old_nodes: Vec<usize>,
        branch_attr: BranchAttr,
        ast1: AST,
        ast2: AST,
    ) -> (usize, usize) {
        let node1 = graph.add_node(NodeAttr::new_node(BranchAttr::NotBranch, ast1));
        let node2 = graph.add_node(NodeAttr::new_node(
            BranchAttr::NotBranch, /* edit later */
            ast2,
        ));
        Simplifier::replace_in_edges(graph, &old_nodes, node1);
        Simplifier::replace_out_edges_cond(graph, &old_nodes, node2, branch_attr);
        graph.add_edge(node1, node2);
        Simplifier::remove_old_nodes(graph, &old_nodes);
        (node1, node2)
    }
}

fn replace_edge_dest(graph: &mut ASTGraph, id_from: usize, id_to_old: usize, id_to_new: usize) {
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
