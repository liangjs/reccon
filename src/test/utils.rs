use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
use std::iter::FromIterator;

use itertools::Itertools;
use rand::prelude::*;
use rand::Rng;
use rand_chacha::ChaCha8Rng;

use crate::ast::*;
use crate::graph::*;
use crate::*;


pub fn build_graph(
    node_num: usize,
    entry: usize,
    edges: Vec<(usize, usize)>,
) -> (ControlFlowGraph<usize>, NodeIndex) {
    let mut graph = ControlFlowGraph::with_capacity(node_num, edges.len());
    let mut node_map = HashMap::new();
    let mut out_degree = vec![0; node_num];
    for i in 0..node_num {
        let node = graph.add_node(i);
        node_map.insert(i, node);
    }
    for e in edges.iter() {
        out_degree[e.0] += 1;
    }
    let mut out_visit = vec![false; node_num];
    for e in edges.iter() {
        let e0 = node_map.get(&e.0).unwrap();
        let e1 = node_map.get(&e.1).unwrap();
        let weight = if out_degree[e.0] < 2 {
            ControlFlowEdge::NotBranch
        } else {
            let w = ControlFlowEdge::Branch(out_visit[e.0]);
            out_visit[e.0] = true;
            w
        };
        graph.add_edge(*e0, *e1, weight);
    }
    let entry = node_map.get(&entry).unwrap();
    (graph, *entry)
}

pub fn test_graph(node_num: usize, entry: usize, edges: Vec<(usize, usize)>) {
    let (graph, entry) = build_graph(node_num, entry, edges);
    let result = reconstruct(&graph, entry).unwrap();
    println!("{}", result.stmt.to_string());
    Walker::exhaustive_walk(&graph, entry, &result);
}

pub fn random_graph<T: Rng>(
    rng: &mut T,
    node_num: usize,
    density: f64,
) -> (ControlFlowGraph<usize>, NodeIndex) {
    //let mut rng = rand::thread_rng();
    let mut edges: Vec<Vec<usize>> = Vec::with_capacity(node_num);
    edges.resize(node_num, Default::default());

    /* first, generate a random tree */
    for i in 1..node_num {
        loop {
            let fr = rng.gen_range(0..i);
            if edges[fr].len() < 2 {
                edges[fr].push(i);
                break;
            }
        }
    }

    /* add random edges */
    let add_edge_num = (node_num as f64 * density).ceil() as usize;
    for _ in 0..add_edge_num {
        let mut x = usize::MAX;
        let mut fail_cnt: usize = 0;
        while fail_cnt < node_num {
            x = rng.gen_range(0..node_num);
            if edges[x].len() < 2 {
                break;
            }
            fail_cnt += 1;
        }
        if fail_cnt == node_num {
            break;
        }
        let y = rng.gen_range(0..node_num);
        edges[x].push(y);
    }

    /* randomize node number */
    let mut order = Vec::from_iter(0..node_num);
    order.shuffle(rng);

    /* collect edges */
    let mut edges_vec: Vec<(usize, usize)> = Vec::new();
    for x in 0..node_num {
        for y in edges[x].iter() {
            let y = *y;
            edges_vec.push((order[x], order[y]));
        }
    }
    let (graph, entry) = build_graph(node_num, order[0], edges_vec);
    (graph, entry)
}

pub fn random_test_seeded(seed: <ChaCha8Rng as SeedableRng>::Seed, node_num: usize, density: f64) {
    let mut rng = ChaCha8Rng::from_seed(seed);
    let (graph, entry) = random_graph(&mut rng, node_num, density);
    //println!("{}", dot_view(&graph, entry));
    let result = match reconstruct(&graph, entry) {
        None => {
            panic!("reconstruct failed");
        }
        Some(r) => r,
    };
    //println!("{}", result.stmt.to_string());
    Walker::exhaustive_walk(&graph, entry, &result);
}

#[derive(PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy, Debug)]
enum Value {
    Bool(bool),
    Int(i32),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
enum EvalAST {
    AState(Box<Statement>),
    AExpr(Box<Expr>),
}

#[derive(Clone, Debug)]
struct WalkPath {
    node: Option<NodeIndex>,
    vars: HashMap<String, Option<Value>>,
    ast_path: Vec<EvalAST>,
}

impl WalkPath {
    pub fn new_empty(entry: NodeIndex, ast: EvalAST, vars: &Vec<String>) -> WalkPath {
        let mut vars_empty: HashMap<String, Option<Value>> = HashMap::new();
        for var in vars {
            vars_empty.insert(var.clone(), None);
        }
        WalkPath {
            node: Some(entry),
            vars: vars_empty,
            ast_path: vec![ast],
        }
    }
}

impl PartialEq for WalkPath {
    fn eq(&self, other: &Self) -> bool {
        if self.node != other.node {
            return false;
        }
        if self.vars != other.vars {
            return false;
        }
        if self.ast_path.len() != other.ast_path.len() {
            return false;
        }
        for i in 0..self.ast_path.len() {
            if self.ast_path[i] != other.ast_path[i] {
                return false;
            }
        }
        true
    }
}

impl Eq for WalkPath {}

impl Hash for WalkPath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.node.hash(state);
        for (k, v) in self.vars.iter().sorted() {
            k.hash(state);
            v.hash(state);
        }
        for x in self.ast_path.iter() {
            x.hash(state);
        }
    }
}

struct Walker {
    queue: VecDeque<Box<WalkPath>>,
    visited: HashSet<WalkPath>,
}

impl Walker {
    pub fn exhaustive_walk<N>(graph: &ControlFlowGraph<N>, entry: NodeIndex, result: &RecResult) {
        let init_path = WalkPath::new_empty(
            entry,
            EvalAST::AState(Box::new(result.stmt.clone())),
            &result.new_vars,
        );
        let mut walker = Walker {
            queue: VecDeque::new(),
            visited: HashSet::new(),
        };
        walker.add_state(Box::new(init_path));
        walker.eval(graph);
    }

    fn add_state(&mut self, state: Box<WalkPath>) {
        if self.visited.contains(&state) {
            return;
        }
        // println!("push {:?}", state);
        self.visited.insert(state.as_ref().clone());
        self.queue.push_back(state);
    }

    fn eval<N>(&mut self, graph: &ControlFlowGraph<N>) {
        while let Some(mut state) = self.queue.pop_front() {
            // println!("pop {:?}", state);
            let last_ast = state.ast_path.pop();
            match last_ast {
                None => continue,
                Some(ast) => match ast {
                    EvalAST::AState(stmt) => self.eval_stmt(graph, state, stmt),
                    EvalAST::AExpr(expr) => self.eval_expr(graph, state, expr),
                },
            }
        }
    }

    fn push_new_ast(
        &mut self,
        mut state: Box<WalkPath>,
        old_ast: Option<EvalAST>,
        new_ast: EvalAST,
    ) {
        if let Some(old_ast) = old_ast {
            state.ast_path.push(old_ast);
        }
        state.ast_path.push(new_ast);
        self.add_state(state);
    }

    fn eval_stmt<N>(
        &mut self,
        graph: &ControlFlowGraph<N>,
        mut state: Box<WalkPath>,
        stmt: Box<Statement>,
    ) {
        match stmt.as_ref() {
            Statement::Compound { first, next } => {
                if let Statement::Nop = first.as_ref() {
                    state.ast_path.push(EvalAST::AState(next.clone()));
                    self.add_state(state);
                } else {
                    let new_ast = EvalAST::AState(first.clone());
                    let old_ast = EvalAST::AState(Box::new(Statement::Compound {
                        first: Box::new(Statement::Nop),
                        next: next.clone(),
                    }));
                    self.push_new_ast(state, Some(old_ast), new_ast);
                }
            }
            Statement::Original { node_idx } => {
                assert_eq!(Some(*node_idx), state.node);
                let nexts: Vec<NodeIndex> = graph.neighbors(state.node.unwrap()).collect();
                state.node = if nexts.len() == 0 {
                    None
                } else if nexts.len() == 1 {
                    Some(*nexts.first().unwrap())
                } else {
                    panic!("node type error");
                };
                self.add_state(state);
            }
            Statement::Assign { var: _, value } => {
                let new_ast = EvalAST::AExpr(value.clone());
                self.push_new_ast(state, Some(EvalAST::AState(stmt)), new_ast);
            }
            Statement::IfThen { cond, body_then: _ } => {
                let bool_expr = Expr::Bool(cond.as_ref().clone());
                let new_ast = EvalAST::AExpr(Box::new(bool_expr));
                self.push_new_ast(state, Some(EvalAST::AState(stmt)), new_ast);
            }
            Statement::IfThenElse {
                cond,
                body_then: _,
                body_else: _,
            } => {
                let bool_expr = Expr::Bool(cond.as_ref().clone());
                let new_ast = EvalAST::AExpr(Box::new(bool_expr));
                self.push_new_ast(state, Some(EvalAST::AState(stmt)), new_ast);
            }
            Statement::While { cond, body: _ } => {
                let bool_expr = Expr::Bool(cond.as_ref().clone());
                let new_ast = EvalAST::AExpr(Box::new(bool_expr));
                self.push_new_ast(state, Some(EvalAST::AState(stmt)), new_ast);
            }
            Statement::DoWhile { cond, body } => {
                let new_stmt = Statement::While {
                    cond: cond.clone(),
                    body: body.clone(),
                };
                let new_ast = EvalAST::AState(Box::new(new_stmt));
                state.ast_path.push(new_ast);
                state.ast_path.push(EvalAST::AState(body.clone()));
                self.add_state(state);
            }
            Statement::Break => {
                /* pop until "While" (included) */
                loop {
                    let s = state.ast_path.pop().expect("break without loop");
                    if let EvalAST::AState(s) = s {
                        if let Statement::While { .. } = s.as_ref() {
                            break;
                        }
                    }
                }
                self.add_state(state);
            }
            Statement::Continue => {
                /* pop until "While" (not included) */
                loop {
                    let s = state.ast_path.last().expect("continue without loop");
                    if let EvalAST::AState(s) = s {
                        if let Statement::While { .. } = s.as_ref() {
                            break;
                        }
                    }
                    state.ast_path.pop();
                }
                self.add_state(state);
            }
            Statement::Nop => {
                self.add_state(state);
            }
            Statement::Halt => {
                assert_eq!(state.node, None);
            }
        }
    }

    fn get_var(state: &Box<WalkPath>, var_name: &String) -> Value {
        let val = state.vars.get(var_name);
        let val = val.expect("no such variable");
        let val = val.expect("variable not defined");
        val
    }

    fn eval_expr<N>(
        &mut self,
        graph: &ControlFlowGraph<N>,
        mut state: Box<WalkPath>,
        expr: Box<Expr>,
    ) {
        match expr.as_ref() {
            Expr::Bool(bool_expr) => match bool_expr {
                BoolExpr::Original { node_idx } => {
                    assert_eq!(Some(*node_idx), state.node);
                    let nexts: Vec<NodeIndex> = ordered_neighbors(graph, *node_idx);
                    assert_eq!(nexts.len(), 2);
                    let mut state_new = state.clone();
                    state
                        .ast_path
                        .push(EvalAST::AExpr(Box::new(Expr::Bool(BoolExpr::True))));
                    state.node = Some(nexts[1]);
                    state_new
                        .ast_path
                        .push(EvalAST::AExpr(Box::new(Expr::Bool(BoolExpr::False))));
                    state_new.node = Some(nexts[0]);
                    self.add_state(state);
                    self.add_state(state_new);
                }
                BoolExpr::Var { name } => {
                    let val = Walker::get_var(&state, name);
                    match val {
                        Value::Bool(bool_val) => {
                            state.ast_path.push(EvalAST::AExpr(Box::new(Expr::Bool(
                                BoolExpr::from_bool(bool_val),
                            ))));
                            self.add_state(state);
                        }
                        Value::Int(_) => panic!("expect Bool, but found Int"),
                    };
                }
                BoolExpr::True => self.eval_bool(graph, state, true),
                BoolExpr::False => self.eval_bool(graph, state, false),
                BoolExpr::Not { value } => {
                    let bool_expr = Expr::Bool(value.as_ref().clone());
                    let new_ast = EvalAST::AExpr(Box::new(bool_expr));
                    self.push_new_ast(state, Some(EvalAST::AExpr(expr)), new_ast);
                }
                BoolExpr::Or { value1, value2: _ } => {
                    let bool_expr = Expr::Bool(value1.as_ref().clone());
                    let new_ast = EvalAST::AExpr(Box::new(bool_expr));
                    self.push_new_ast(state, Some(EvalAST::AExpr(expr)), new_ast);
                }
                BoolExpr::And { value1, value2: _ } => {
                    let bool_expr = Expr::Bool(value1.as_ref().clone());
                    let new_ast = EvalAST::AExpr(Box::new(bool_expr));
                    self.push_new_ast(state, Some(EvalAST::AExpr(expr)), new_ast);
                }
                BoolExpr::Eq { var: _, value } => {
                    let new_ast = EvalAST::AExpr(value.clone());
                    self.push_new_ast(state, Some(EvalAST::AExpr(expr)), new_ast);
                }
            },
            Expr::Int(int_val) => self.eval_int(graph, state, *int_val),
        }
    }

    fn eval_bool<N>(&mut self, _graph: &ControlFlowGraph<N>, mut state: Box<WalkPath>, res: bool) {
        let last_ast = state
            .ast_path
            .pop()
            .expect("expression should have a parent");
        match last_ast {
            EvalAST::AState(stmt) => match *stmt {
                Statement::Compound { .. } => panic!("ast error"),
                Statement::Original { .. } => panic!("ast error"),
                Statement::Assign { var, value: _ } => {
                    state.vars.insert(var.clone(), Some(Value::Bool(res)));
                    self.add_state(state);
                }
                Statement::IfThen { cond: _, body_then } => {
                    if res {
                        state.ast_path.push(EvalAST::AState(body_then));
                    }
                    self.add_state(state);
                }
                Statement::IfThenElse {
                    cond: _,
                    body_then,
                    body_else,
                } => {
                    if res {
                        state.ast_path.push(EvalAST::AState(body_then));
                    } else {
                        state.ast_path.push(EvalAST::AState(body_else));
                    }
                    self.add_state(state);
                }
                Statement::While { cond, body } => {
                    if res {
                        state
                            .ast_path
                            .push(EvalAST::AState(Box::new(Statement::While {
                                cond,
                                body: body.clone(),
                            })));
                        state.ast_path.push(EvalAST::AState(body));
                    }
                    self.add_state(state);
                }
                Statement::DoWhile { .. } => panic!("ast error"),
                Statement::Break => panic!("ast error"),
                Statement::Continue => panic!("ast error"),
                Statement::Nop => panic!("ast error"),
                Statement::Halt => panic!("ast error"),
            },
            EvalAST::AExpr(expr) => match *expr {
                Expr::Bool(bool_expr) => match bool_expr {
                    BoolExpr::Original { .. } => panic!("ast error"),
                    BoolExpr::Var { .. } => panic!("ast error"),
                    BoolExpr::True => panic!("ast error"),
                    BoolExpr::False => panic!("ast error"),
                    BoolExpr::Not { .. } => {
                        state.ast_path.push(EvalAST::AExpr(Box::new(Expr::Bool(
                            BoolExpr::from_bool(!res),
                        ))));
                        self.add_state(state);
                    }
                    BoolExpr::Or { value1: _, value2 } => {
                        if res {
                            state
                                .ast_path
                                .push(EvalAST::AExpr(Box::new(Expr::Bool(BoolExpr::True))));
                        } else {
                            state
                                .ast_path
                                .push(EvalAST::AExpr(Box::new(Expr::Bool(*value2))));
                        }
                        self.add_state(state);
                    }
                    BoolExpr::And { value1: _, value2 } => {
                        if !res {
                            state
                                .ast_path
                                .push(EvalAST::AExpr(Box::new(Expr::Bool(BoolExpr::False))));
                        } else {
                            state
                                .ast_path
                                .push(EvalAST::AExpr(Box::new(Expr::Bool(*value2))));
                        }
                        self.add_state(state);
                    }
                    BoolExpr::Eq { var, value: _ } => {
                        let val = Walker::get_var(&state, &var);
                        match val {
                            Value::Bool(bool_val) => {
                                state.ast_path.push(EvalAST::AExpr(Box::new(Expr::Bool(
                                    BoolExpr::from_bool(bool_val == res),
                                ))));
                                self.add_state(state);
                            }
                            Value::Int(_) => panic!("expect Bool, but found Int"),
                        };
                    }
                },
                Expr::Int(_) => panic!("ast error"),
            },
        }
    }

    fn eval_int<N>(&mut self, _graph: &ControlFlowGraph<N>, mut state: Box<WalkPath>, res: i32) {
        let last_ast = state
            .ast_path
            .pop()
            .expect("expression should have a parent");
        match last_ast {
            EvalAST::AState(stmt) => match *stmt {
                Statement::Assign { var, value: _ } => {
                    state.vars.insert(var.clone(), Some(Value::Int(res)));
                    self.add_state(state);
                }
                _ => panic!("ast error"),
            },
            EvalAST::AExpr(expr) => match *expr {
                Expr::Bool(bool_expr) => match bool_expr {
                    BoolExpr::Eq { var, value: _ } => {
                        let val = Walker::get_var(&state, &var);
                        match val {
                            Value::Bool(_) => panic!("expect Int, but found Bool"),
                            Value::Int(int_val) => {
                                state.ast_path.push(EvalAST::AExpr(Box::new(Expr::Bool(
                                    BoolExpr::from_bool(int_val == res),
                                ))));
                                self.add_state(state);
                            }
                        }
                    }
                    _ => panic!("ast error"),
                },
                Expr::Int(_) => panic!("ast error"),
            },
        }
    }
}