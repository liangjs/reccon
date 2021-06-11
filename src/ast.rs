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
pub enum AST {
    Compound {
        first: Box<AST>,
        next: Box<AST>,
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
        body_then: Box<AST>,
    },
    IfThenElse {
        cond: Box<BoolExpr>,
        body_then: Box<AST>,
        body_else: Box<AST>,
    },
    While {
        cond: Box<BoolExpr>,
        body: Box<AST>,
    },
    DoWhile {
        cond: Box<BoolExpr>,
        body: Box<AST>,
    },
    Break,
    Continue,
    Nop,
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
            BoolExpr::Or { value1, value2 } => format!("(or {} {})", value1.to_string(), value2.to_string()),
            BoolExpr::And { value1, value2 } => format!("(and {} {})", value1.to_string(), value2.to_string()),
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