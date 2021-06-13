use std::collections::HashMap;

use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum Statement {
    Compound {
        first: Box<Statement>,
        next: Box<Statement>,
    },
    Original {
        node_num: usize,
    },
    Assign {
        var: String,
        value: Box<Expr>,
    },
    IfThen {
        cond: Box<BoolExpr>,
        body_then: Box<Statement>,
    },
    IfThenElse {
        cond: Box<BoolExpr>,
        body_then: Box<Statement>,
        body_else: Box<Statement>,
    },
    While {
        cond: Box<BoolExpr>,
        body: Box<Statement>,
    },
    DoWhile {
        cond: Box<BoolExpr>,
        body: Box<Statement>,
    },
    Break,
    Continue,
    Nop,
}

impl ToString for Statement {
    fn to_string(&self) -> String {
        match self {
            Statement::Compound { first, next } => {
                format!("{}\n{}", first.to_string(), next.to_string())
            }
            Statement::Original { node_num } => format!("node {};", node_num),
            Statement::Assign { var, value } => format!("{} = {};", var, value.to_string()),
            Statement::IfThen { cond, body_then } => format!(
                "if ({}) {{\n{}\n}}",
                cond.to_string(),
                indent(&body_then.to_string())
            ),
            Statement::IfThenElse {
                cond,
                body_then,
                body_else,
            } => format!(
                "if ({}) {{\n{}\n}} else {{\n{}\n}}",
                cond.to_string(),
                indent(&body_then.to_string()),
                indent(&body_else.to_string())
            ),
            Statement::While { cond, body } => format!(
                "while ({}) {{\n{}\n}}",
                cond.to_string(),
                indent(&body.to_string())
            ),
            Statement::DoWhile { cond, body } => format!(
                "do {{\n{}\n}} while ({});",
                indent(&body.to_string()),
                cond.to_string()
            ),
            Statement::Break => format!("break;"),
            Statement::Continue => format!("continue;"),
            Statement::Nop => format!(";"),
        }
    }
}

fn indent(stmts: &str) -> String {
    stmts.split("\n").map(|s| format!("\t{}", s)).join("\n")
}

#[derive(Debug, Clone)]
pub enum BoolExpr {
    Original {
        node_num: usize,
    },
    Var {
        name: String,
    },
    True,
    False,
    Not {
        value: Box<BoolExpr>,
    },
    Or {
        value1: Box<BoolExpr>,
        value2: Box<BoolExpr>,
    },
    And {
        value1: Box<BoolExpr>,
        value2: Box<BoolExpr>,
    },
    Eq {
        var: String,
        value: Box<Expr>,
    },
}

impl ToString for BoolExpr {
    fn to_string(&self) -> String {
        match self {
            BoolExpr::Original { node_num } => format!("node {}", node_num),
            BoolExpr::Var { name } => name.clone(),
            BoolExpr::True => String::from("true"),
            BoolExpr::False => String::from("false"),
            BoolExpr::Not { value } => format!("(not {})", value.to_string()),
            BoolExpr::Or { value1, value2 } => {
                format!("(or {} {})", value1.to_string(), value2.to_string())
            }
            BoolExpr::And { value1, value2 } => {
                format!("(and {} {})", value1.to_string(), value2.to_string())
            }
            BoolExpr::Eq { var, value } => format!("{} == {}", var, value.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(BoolExpr),
    Int(i32),
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::Bool(bool_expr) => bool_expr.to_string(),
            Expr::Int(int_val) => int_val.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AST {
    AState(Statement),
    ABool(BoolExpr),
}

impl AST {
    pub fn clone_state(&self) -> Statement {
        match self {
            AST::AState(x) => x.clone(),
            AST::ABool(_) => panic!("AST is not statement."),
        }
    }

    pub fn clone_bool(&self) -> BoolExpr {
        match self {
            AST::AState(_) => panic!("AST is not bool expression."),
            AST::ABool(x) => x.clone(),
        }
    }
}

impl ToString for AST {
    fn to_string(&self) -> String {
        match self {
            AST::AState(stmt) => stmt.to_string(),
            AST::ABool(expr) => expr.to_string(),
        }
    }
}

impl Statement {
    pub fn unfold(&self, map: &HashMap<usize, &AST>) -> Statement {
        match self {
            Statement::Compound { first, next } => Statement::Compound {
                first: Box::new(first.unfold(map)),
                next: Box::new(next.unfold(map)),
            },
            Statement::Original { node_num } => match map.get(node_num) {
                Some(ast) => ast.clone_state(),
                None => panic!("origin not in map"),
            },
            Statement::Assign { var, value } => Statement::Assign {
                var: var.clone(),
                value: Box::new(value.unfold(map)),
            },
            Statement::IfThen { cond, body_then } => Statement::IfThen {
                cond: Box::new(cond.unfold(map)),
                body_then: Box::new(body_then.unfold(map)),
            },
            Statement::IfThenElse {
                cond,
                body_then,
                body_else,
            } => Statement::IfThenElse {
                cond: Box::new(cond.unfold(map)),
                body_then: Box::new(body_then.unfold(map)),
                body_else: Box::new(body_else.unfold(map)),
            },
            Statement::While { cond, body } => Statement::While {
                cond: Box::new(cond.unfold(map)),
                body: Box::new(body.unfold(map)),
            },
            Statement::DoWhile { cond, body } => Statement::DoWhile {
                cond: Box::new(cond.unfold(map)),
                body: Box::new(body.unfold(map)),
            },
            Statement::Break => self.clone(),
            Statement::Continue => self.clone(),
            Statement::Nop => self.clone(),
        }
    }
}

impl BoolExpr {
    pub fn unfold(&self, map: &HashMap<usize, &AST>) -> BoolExpr {
        match self {
            BoolExpr::Original { node_num } => match map.get(node_num) {
                Some(ast) => ast.clone_bool(),
                None => panic!("origin not in map"),
            },
            BoolExpr::Var { name } => BoolExpr::Var { name: name.clone() },
            BoolExpr::True => self.clone(),
            BoolExpr::False => self.clone(),
            BoolExpr::Not { value } => BoolExpr::Not {
                value: Box::new(value.unfold(map)),
            },
            BoolExpr::Or { value1, value2 } => BoolExpr::Or {
                value1: Box::new(value1.unfold(map)),
                value2: Box::new(value2.unfold(map)),
            },
            BoolExpr::And { value1, value2 } => BoolExpr::And {
                value1: Box::new(value1.unfold(map)),
                value2: Box::new(value2.unfold(map)),
            },
            BoolExpr::Eq { var, value } => BoolExpr::Eq {
                var: var.clone(),
                value: Box::new(value.unfold(map)),
            },
        }
    }
}

impl Expr {
    pub fn unfold(&self, map: &HashMap<usize, &AST>) -> Expr {
        match self {
            Expr::Bool(expr) => Expr::Bool(expr.unfold(map)),
            Expr::Int(x) => Expr::Int(*x),
        }
    }
}
