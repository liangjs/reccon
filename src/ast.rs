use itertools::Itertools;

#[derive(Debug, Clone)]
pub enum Single {
    Original { node_num: usize },
    Assign { var: String, value: Box<Expr> },
    Condition(Box<BoolExpr>),
    Nop,
}

impl ToString for Single {
    fn to_string(&self) -> String {
        match self {
            Single::Original { node_num } => format!("node {}", node_num),
            Single::Assign { var, value } => format!("{} = {}", var, value.to_string()),
            Single::Condition(bool_expr) => bool_expr.to_string(),
            Single::Nop => String::from("nop"),
        }
    }
}

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
        value: Box<BoolExpr>,
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
            Statement::Compound { first, next } => format!("{}\n{}", first.to_string(), next.to_string()),
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