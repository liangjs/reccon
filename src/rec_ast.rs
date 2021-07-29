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
    //dot_graph(&ast_graph, entry);
    let mut new_vars = Vec::new();
    loop {
        let result = Simplifier::simplify(&mut ast_graph, entry);
        new_vars.extend(result.new_vars);
        //dot_graph(&ast_graph, entry);
        if ast_graph.node_num() == 1 {
            break;
        }
        if !result.updated {
            return None;
        }
    }
    let node = ast_graph.node_iter().next()?;
    let ast = &ast_graph.read_note(node).ast;
    Some(ASTRecResult {
        stmt: ast.clone_state(),
        new_vars,
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
            let halt = graph.add_node(NodeAttr::new_node(
                BranchAttr::NotBranch,
                AST::AState(Statement::Halt),
            ));
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

#[derive(Debug)]
struct NodeAttr {
    branch_attr: BranchAttr,
    ast: AST,
}

impl ToString for NodeAttr {
    fn to_string(&self) -> String {
        self.ast.to_string()
    }
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
    updated: bool,
    new_vars: Vec<String>,
}

struct Simplifier {
    queue: VecDeque<usize>,
    visited: HashSet<usize>,
    entry: usize,
    new_vars: Vec<String>,
}

impl Simplifier {
    pub fn simplify(graph: &mut ASTGraph, entry: usize) -> SimplifyResult {
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

    fn dfs(&mut self, graph: &mut ASTGraph, current: usize) {
        self.visited.insert(current);
        let nexts: Vec<usize> = graph.edge_iter(current).collect();
        for next in nexts.iter() {
            let next = *next;
            if !self.visited.contains(&next) {
                self.dfs(graph, next);
            }
        }
        self.queue_add(current);
    }

    fn queue_add(&mut self, node: usize) {
        self.queue.push_front(node);
    }

    fn queue_reverse(&mut self) {
        self.queue = self.queue.iter().rev().map(|x| *x).collect();
    }

    fn simplify_all(&mut self, graph: &mut ASTGraph) -> bool {
        let mut updated = false;
        while let Some(node) = self.queue.pop_front() {
            if !graph.contain_node(node) {
                continue;
            }
            if self.simplify_one(graph, node) {
                updated = true;
            }
        }
        updated
    }

    fn simplify_one(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
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

    fn try_seq(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        if graph.edge_iter(handle).count() != 1 {
            return false;
        }
        let next = match graph.edge_iter(handle).next() {
            None => return false,
            Some(x) => x,
        };
        if is_branch(graph, next) {
            return false;
        }
        if graph.reverse_edge_iter(next).count() != 1 {
            return false;
        }

        if handle == self.entry {
            if graph.edge_iter(next).count() != 0 {
                return false;
            }
            let _new_node = Simplifier::replace_block(
                graph,
                vec![handle, next],
                graph.read_note(next).ast.clone(),
            );
            true
        } else {
            let loop_back = graph.edge_iter(next).contains(&handle);
            let new_node = Simplifier::replace_block(
                graph,
                vec![handle, next],
                AST::AState(Statement::Compound {
                    first: Box::new(graph.read_note(handle).ast.clone_state()),
                    next: Box::new(graph.read_note(next).ast.clone_state()),
                }),
            );
            if loop_back {
                graph.add_edge(new_node, new_node);
            }
            self.queue_add(new_node);
            true
        }
    }

    fn try_if_then(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let cond = handle;
        let (br_false, br_true) = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };
        let cond_expr = graph.read_note(cond).ast.clone_bool();

        let is_part = |part, next| {
            if graph.reverse_edge_iter(part).count() != 1 {
                return false;
            }
            let cnt = graph.edge_iter(part).count();
            if cnt == 0 {
                return true;
            }
            cnt == 1 && graph.edge_iter(part).contains(&next)
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
        let refresh_next = graph.edge_iter(part).count() > 0;

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
                body_then: Box::new(graph.read_note(part).ast.clone_state()),
            }),
        );

        if next == cond {
            graph.add_edge(new_node, new_node);
        }

        self.queue_add(new_node);
        if refresh_next {
            self.queue_add(next);
        }
        true
    }

    fn try_if_then_else(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let cond = handle;
        let (br_false, br_true) = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };

        if graph.reverse_edge_iter(br_false).count() != 1
            || graph.reverse_edge_iter(br_true).count() != 1
        {
            return false;
        }

        let next_false: Vec<usize> = graph.edge_iter(br_false).collect();
        let next_true: Vec<usize> = graph.edge_iter(br_true).collect();
        let next: Option<usize>;

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
                cond: Box::new(graph.read_note(cond).ast.clone_bool()),
                body_then: Box::new(graph.read_note(br_true).ast.clone_state()),
                body_else: Box::new(graph.read_note(br_false).ast.clone_state()),
            }),
        );

        if next == Some(cond) {
            graph.add_edge(new_node, new_node);
        }

        self.queue_add(new_node);
        if let Some(next) = next {
            self.queue_add(next);
        }
        true
    }

    fn try_dump_if(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let cond = handle;
        let next = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => {
                if br_false != br_true {
                    return false;
                }
                br_true
            }
        };

        let new_node = Simplifier::replace_block(
            graph,
            vec![cond],
            AST::AState(Statement::IfThen {
                cond: Box::new(graph.read_note(cond).ast.clone_bool()),
                body_then: Box::new(Statement::Nop),
            }),
        );

        if next == handle {
            graph.add_edge(new_node, new_node);
        }

        self.queue_add(new_node);
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

        self.queue_add(new_node);
        self.queue_add(branch_merge);
        true
    }

    fn try_short_circuit(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let prev_cond = handle;
        if !is_branch(graph, handle) {
            return false;
        }

        let check_mid = |m| match m {
            None => true,
            Some(m) => graph.reverse_edge_iter(m).count() == 1 && !is_branch(graph, m),
        };

        let pnexts: Vec<usize> = graph.edge_iter(prev_cond).collect();
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
                    Some(m) => graph.edge_iter(m).next().unwrap(),
                };
                if graph.reverse_edge_iter(next_cond).count() != 1 {
                    continue;
                }
                if !is_branch(graph, next_cond) {
                    continue;
                }

                let br_merge = match mid1 {
                    None => pnexts[merge_place],
                    Some(m) => graph.edge_iter(m).next().unwrap(),
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
        prev_cond: usize,
        next_cond: usize,
        br_merge: usize,
        mid1: Option<usize>,
        mid2: Option<usize>,
        mid3: Option<usize>,
    ) {
        let (_br_prev_false, br_prev_true) = match graph.read_note(prev_cond).branch_attr {
            BranchAttr::NotBranch => panic!("prev_cond is not branch"),
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };
        let (br_next_false, br_next_true) = match graph.read_note(next_cond).branch_attr {
            BranchAttr::NotBranch => panic!("next_cond is not branch"),
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
        };
        let br_merge_prev = match mid1 {
            None => br_merge,
            Some(x) => x,
        };
        let br_merge_next = match mid3 {
            None => br_merge,
            Some(x) => x,
        };

        let branch_attr = if br_merge_next == br_next_true {
            BranchAttr::Branch(br_next_false, br_merge)
        } else {
            BranchAttr::Branch(br_next_true, br_merge)
        };

        let mut old_nodes = vec![prev_cond, next_cond];
        for mid in [mid1, mid2, mid3].iter() {
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
                first: Box::new(graph.read_note(mid1).ast.clone_state()),
                next: Box::new(p_assign_true),
            },
        };
        let mut prev_unpass_stmt = match mid2 {
            None => p_assign,
            Some(mid2) => Statement::Compound {
                first: Box::new(graph.read_note(mid2).ast.clone_state()),
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
                    body_then: Box::new(graph.read_note(mid3).ast.clone_state()),
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
            Simplifier::replace_short_circuit(graph, old_nodes, branch_attr, ast1, ast2);

        self.queue_add(new_node1);
        self.queue_add(new_node2);
        self.queue_add(br_merge);
        self.new_vars.push(p_var);
    }

    fn is_shortcut(graph: &ASTGraph, branch: usize, merge: usize) -> Option<Option<usize>> {
        if !is_branch(graph, branch) {
            return None;
        }
        let nexts: Vec<usize> = graph.edge_iter(branch).collect();
        if nexts.contains(&merge) {
            return Some(None);
        }
        for mid in nexts.iter() {
            let mid = *mid;
            if graph.reverse_edge_iter(mid).count() != 1 {
                continue;
            }
            if is_branch(graph, mid) {
                continue;
            }
            if graph.edge_iter(mid).contains(&merge) {
                return Some(Some(mid));
            }
        }
        None
    }

    fn get_loop_branch(
        graph: &ASTGraph,
        head: usize,
        branches: (usize, usize),
    ) -> Option<((usize, usize), Option<usize>)> {
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
        let br_exit;
        let mut next: Option<usize> = None;
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

    fn try_while(&mut self, graph: &mut ASTGraph, handle: usize) -> bool {
        let (br_false, br_true) = match graph.read_note(handle).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
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
        self.queue_add(new_node);
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
        if graph.reverse_edge_iter(cond).count() != 1 {
            return false;
        }
        let (br_false, br_true) = match graph.read_note(cond).branch_attr {
            BranchAttr::NotBranch => return false,
            BranchAttr::Branch(br_false, br_true) => (br_false, br_true),
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
        self.queue_add(new_node);
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
        self.queue_add(new_node);
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
