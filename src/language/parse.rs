use pest::error::*;
use pest::iterators::Pair;
use pest::*;
use std::collections::HashMap;
use std::ops::*;
use z3::ast::{Ast, Bool, Int};
use z3::{Config, Context, Solver};

#[derive(Parser)]
#[grammar = "./akl.pest"]
pub struct AklParser<'a> {
    ctx: Context,
    pub funcs: Vec<Box<AstNode>>,
    variables: Vec<Vec<String>>,
    basis_t: Vec<Vec<Box<Bool<'a>>>>,
    basis_f: Vec<Vec<Box<Bool<'a>>>>,
    func_map: HashMap<Box<AstNode>, usize>,
    stack: Vec<Box<AstNode>>,
}

pub fn numeric_eval(expr: AstNode, value: HashMap<String, i64>) -> i64 {
    match expr {
        AstNode::NumericTerm(lhs, rhs, op) => {
            let lhs_eval = numeric_eval(*lhs, value.clone());
            let rhs_eval = numeric_eval(*rhs, value.clone());
            match op {
                Ops::Add => lhs_eval + rhs_eval,
                Ops::Sub => lhs_eval - rhs_eval,
                Ops::Multply => lhs_eval * rhs_eval,
                Ops::Divide => lhs_eval / rhs_eval,
                _ => {
                    // TODO
                    0
                }
            }
        }
        AstNode::Constant(c) => {
            match c {
                Constant::Number(x) => x,
                _ => {
                    // TODO
                    0
                }
            }
        }
        AstNode::Ident(x) => value[&x],
        _ => {
            // TODO
            0
        }
    }
}

impl<'a> AklParser<'a> {
    pub fn new() -> Self {
        let cfg = Config::new();
        let ctx = Context::new(&cfg);
        Self {
            ctx: ctx,
            variables: vec![],
            funcs: vec![],
            func_map: HashMap::new(),
            basis_f: vec![],
            basis_t: vec![],
            stack: vec![],
        }
    }

    pub fn numeric_convert(&self, expr: AstNode) -> Int<'_> {
        match expr {
            AstNode::NumericTerm(lhs, rhs, op) => {
                let lhs_eval = self.numeric_convert(*lhs);
                let rhs_eval = self.numeric_convert(*rhs);
                match op {
                    Ops::Add => lhs_eval + rhs_eval,
                    Ops::Sub => lhs_eval - rhs_eval,
                    Ops::Multply => lhs_eval * rhs_eval,
                    Ops::Divide => lhs_eval / rhs_eval,
                    _ => {
                        // TODO
                        Int::from_i64(&self.ctx, 0)
                    }
                }
            }
            AstNode::Constant(c) => {
                match c {
                    Constant::Number(x) => Int::from_i64(&self.ctx, x),
                    _ => {
                        // TODO
                        Int::from_i64(&self.ctx, 0)
                    }
                }
            }
            AstNode::Ident(x) => Int::new_const(&self.ctx, x.as_str()),
            _ => {
                // TODO
                Int::from_i64(&self.ctx, 0)
            }
        }
    }

    pub fn boolean_convert(&self, expr: AstNode) -> Bool<'_> {
        match expr {
            AstNode::BooleanTerm(lhs, rhs, op) => {
                let l = lhs.clone();
                let r = rhs.clone();
                let numeric = || -> Bool<'_> {
                    let lhs_eval = self.numeric_convert(*lhs);
                    let rhs_eval = self.numeric_convert(*rhs);
                    match op {
                        Ops::Equal => lhs_eval._eq(&rhs_eval),
                        _ => {
                            // TODO
                            Bool::from_bool(&self.ctx, false)
                        }
                    }
                };
                let lhs = l.clone();
                let rhs = r.clone();
                let boolean = || -> Bool<'_> {
                    let lhs_eval = self.boolean_convert(*lhs);
                    let rhs_eval = self.boolean_convert(*rhs);
                    match op {
                        Ops::NotEqual => (lhs_eval._eq(&rhs_eval)).not(),
                        Ops::Equal => lhs_eval._eq(&rhs_eval),
                        Ops::And => lhs_eval & rhs_eval,
                        Ops::Or => lhs_eval | rhs_eval,
                        _ => {
                            // TODO
                            Bool::from_bool(&self.ctx, false)
                        }
                    }
                };
                match *l {
                    AstNode::NumericTerm(_, _, _) => numeric(),
                    AstNode::BooleanTerm(_, _, _) => boolean(),
                    AstNode::Constant(x) => {
                        if x.is_numeric() {
                            numeric()
                        } else {
                            boolean()
                        }
                    }
                    _ => Bool::from_bool(&self.ctx, false),
                }
            }
            _ => {
                // TODO
                Bool::from_bool(&self.ctx, false)
            }
        }
    }

    pub fn parse_boolean(&mut self, expr: Pair<Rule>) -> AstNode {
        let pairs: Vec<Pair<Rule>> = expr.into_inner().collect();
        if pairs.len() == 1 {
            match pairs[0].as_rule() {
                Rule::ident => AstNode::Ident(pairs[0].as_str().to_string()),
                Rule::constants => AstNode::Constant(Constant::parse_bool(pairs[0].as_str())),
                Rule::boolean_term => self.parse_boolean(pairs[0].clone()),
                _ => AstNode::Todo,
            }
        } else {
            let mut i = 0;
            let mut res = AstNode::Todo;
            while pairs.len() > i {
                if i == 0 {
                    match pairs[i].as_rule() {
                        Rule::boolean_term => {
                            // TODO
                            res = self.parse_boolean(pairs[i].clone());
                        }
                        Rule::numeric_term => {
                            res = self.parse_numeric(pairs[i].clone());
                        }
                        _ => {}
                    }
                } else {
                    match pairs[i].as_rule() {
                        Rule::boolean_term => {
                            // TODO
                            let op = Ops::from_str(pairs[i - 1].as_str());
                            let rhs = self.parse_boolean(pairs[i].clone());
                            res = AstNode::BooleanTerm(Box::new(res.clone()), Box::new(rhs), op);
                        }
                        Rule::numeric_term => {
                            let op = Ops::from_str(pairs[i - 1].as_str());
                            let rhs = self.parse_numeric(pairs[i].clone());
                            res = AstNode::BooleanTerm(Box::new(res.clone()), Box::new(rhs), op);
                        }
                        _ => {}
                    }
                }
                i += 2;
            }
            res
        }
    }

    pub fn parse_numeric(&mut self, expr: Pair<Rule>) -> AstNode {
        let pairs: Vec<Pair<Rule>> = expr.into_inner().collect();
        if pairs.len() == 1 {
            match pairs[0].as_rule() {
                Rule::ident => AstNode::Ident(pairs[0].as_str().to_string()),
                Rule::constants => AstNode::Constant(Constant::parse_numeric(pairs[0].as_str())),
                Rule::numeric_term => self.parse_numeric(pairs[0].clone()),
                _ => AstNode::Todo,
            }
        } else {
            match pairs[0].as_rule() {
                Rule::numeric_term => {
                    assert_eq!(pairs[2].as_rule(), Rule::numeric_term);
                    let op = Ops::from_str(pairs[1].as_str());
                    let lhs = self.parse_numeric(pairs[0].clone());
                    let rhs = self.parse_numeric(pairs[2].clone());
                    AstNode::NumericTerm(Box::new(lhs), Box::new(rhs), op)
                }
                _ => AstNode::Todo,
            }
        }
    }

    pub fn parse_loop(&mut self, expr: Pair<Rule>) -> AstNode {
        let pairs: Vec<Pair<Rule>> = expr.into_inner().collect();
        let expr = self.parse_boolean(pairs[0].clone());
        let block = self.parse_block(pairs[1].clone());
        AstNode::Loop(Box::new(expr), Box::new(block))
    }

    pub fn parse_expr(&mut self, expr: Pair<Rule>) -> AstNode {
        let pair = expr.into_inner().peek().unwrap();
        match pair.as_rule() {
            Rule::akl_if => self.parse_if(pair),
            Rule::akl_loop => self.parse_loop(pair),
            Rule::akl_call => self.parse_call(pair),
            Rule::akl_return => self.parse_return(pair),
            _ => AstNode::Todo,
        }
    }

    pub fn parse_if(&mut self, expr: Pair<Rule>) -> AstNode {
        let mut pair = expr.into_inner();
        let term = self.parse_boolean(pair.next().unwrap());
        let block = self.parse_block(pair.next().unwrap());
        AstNode::If(Box::new(term), Box::new(block))
    }

    pub fn parse_block(&mut self, block: Pair<Rule>) -> AstNode {
        let mut nodes = vec![];
        let pairs = block.into_inner();
        for pair in pairs {
            nodes.push(Box::new(self.parse_expr(pair)));
        }
        AstNode::Block(nodes)
    }

    pub fn parse_func_parm(&mut self, parm: Pair<Rule>) -> Variable {
        let mut pair = parm.into_inner();
        let var_name = pair.next().unwrap().as_str();
        let type_name = pair.next().unwrap().as_str();
        Variable::new(
            Box::new(AstNode::Ident(var_name.to_string())),
            VType::from_str(type_name),
        )
    }

    pub fn parse_parm(&mut self, parm: Pair<Rule>) -> AstNode {
        let mut pairs = parm.into_inner();
        let pair = pairs.next().unwrap();
        match pair.as_rule() {
            Rule::boolean_term => self.parse_boolean(pair),
            Rule::numeric_term => self.parse_numeric(pair),
            _ => AstNode::Todo,
        }
    }

    pub fn parse_call(&mut self, call: Pair<Rule>) -> AstNode {
        // TODO
        let mut call = call.into_inner();
        let name = AstNode::Ident(call.next().unwrap().as_str().to_string());
        let mut variables = vec![];
        while call.peek().is_some() && call.peek().unwrap().as_rule() == Rule::parm {
            variables.push(Box::new(self.parse_parm(call.next().unwrap())));
        }
        AstNode::Call(Box::new(name), variables)
    }

    pub fn parse_return(&mut self, _: Pair<Rule>) -> AstNode {
        // TODO
        AstNode::Return
    }

    pub fn register_func(&mut self, func: Pair<Rule>) {
        // TODO
        let mut func = func.into_inner();
        let name = AstNode::Ident(func.next().unwrap().as_str().to_string());
        let mut variables = vec![];
        while func.peek().unwrap().as_rule() == Rule::parm_with_type {
            variables.push(self.parse_func_parm(func.next().unwrap()));
        }
        let block = self.parse_block(func.next().unwrap());
        self.basis_f.push(vec![]);
        self.basis_t.push(vec![]);
        self.variables.push(vec![]);
        for var in variables.clone() {
            if let AstNode::Ident(name) = *var.name {
                self.variables[self.funcs.len()].push(name);
            }
        }
        self.funcs.push(Box::new(AstNode::Func(
            Box::new(name),
            variables,
            Box::new(block),
        )));
    }

    pub fn parse_top_command(&mut self, expr: Pair<Rule>) {
        match expr.as_rule() {
            Rule::func => {
                self.register_func(expr);
            }
            _ => {}
        }
    }

    /*pub fn compress(&mut self, node: AstNode, variables: Vec<Variable>) -> AstNode {
        match node {
            AstNode::Func(name, parms, block) => {
                AstNode::Func(name, parms, Box::new(self.compress(*block, parms)))
            },
            AstNode::Loop(expr, block) => {
                let uuid = Uuid::from_u128(self.funcs.len() as u128);
                let name = AstNode::Ident(uuid.to_simple().encode_lower(&mut Uuid::encode_buffer()).to_string());
                self.func_map.insert(name, self.funcs.len());
                let mut v = vec![];
                let mut v1 = vec![Box::new(self.compress(*block, variables)), Box::new(AstNode::Call(Box::new(AstNode::Return)];
                let block1 = AstNode::Block(v1);
                v.push(Box::new(AstNode::If(expr, block1)));
                v.push(Box::new(AstNode::Return));
                let block2 = AstNode::Block(v);
                let func = AstNode::Func(Box::new(name),variables,block2);
            },
            AstNode::If(expr, block) => {

            },
            AstNode::Block(nodes) => {
                let mut v = vec![];
                for node in nodes {
                    v.push(Box::new(self.compress(f_num, *node)));
                }
                AstNode::Block(v)
            },
        }
    }*/

    pub fn dfs(&mut self, f_num: usize, node: AstNode) {
        self.stack.push(Box::new(node.clone()));
        match node {
            AstNode::Func(_, _, block) => {
                self.dfs(f_num, *block);
            }
            AstNode::Block(nodes) => {
                for node in nodes.clone() {
                    self.dfs(f_num, *node);
                }
            }
            AstNode::If(_, block) => {
                self.dfs(f_num, *block);
            }
            AstNode::Return => {
                let mut cond = None;
                for node in self.stack.clone() {
                    if let AstNode::If(expr, _) = *node {
                        match *expr {
                            AstNode::BooleanTerm(_, _, _) | AstNode::Constant(_) => {
                                if let Some(c) = cond {
                                    cond = Some(
                                        c & unsafe {
                                            std::mem::transmute::<Bool<'_>, Bool<'a>>(
                                                self.boolean_convert(*expr),
                                            )
                                        },
                                    );
                                } else {
                                    cond = Some(unsafe {
                                        std::mem::transmute::<Bool<'_>, Bool<'a>>(
                                            self.boolean_convert(*expr),
                                        )
                                    });
                                }
                            }
                            _ => {}
                        }
                    }
                }
                if let Some(c) = cond {
                    self.basis_f[f_num].push(Box::new(c));
                }
            }
            _ => {}
        }
        self.stack.pop();
    }

    pub fn run(&mut self, source: &str) -> Result<(), Error<Rule>> {
        let pairs = Self::parse(Rule::program, source)?;
        for pair in pairs {
            match pair.as_rule() {
                Rule::top_command => {
                    let mut pair = pair.into_inner();
                    self.parse_top_command(pair.next().unwrap());
                }
                _ => {
                    // TODO: unexpected
                }
            }
        }
        for i in 0..self.funcs.len() {
            self.dfs(i, *self.funcs[i].clone());
            for cond in self.basis_f[i].clone() {
                let solver = Solver::new(&self.ctx);
                solver.assert(&cond);
                solver.check();
                if let Some(model) = solver.get_model() {
                    for var in self.variables[i].clone() {
                        dbg!(model.eval(&Int::new_const(&self.ctx, var.as_str()), true));
                    }
                }
            }
        }
        /*let f_num = self.funcs.len();
        for i in 0..f_num {
            self.funcs[i] = Box::new(self.compress(i, *self.funcs[i]));
        }*/

        return Ok(());
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum VType {
    Unknown,
    Number,
    String,
    Set,
    Boolean,
    Queue,
    Stack,
    Array,
    LinkedList,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Ops {
    Unknown,
    NotEqual,
    Equal,
    Leq,
    Geq,
    Less,
    Greater,
    And,
    Or,
    Add,
    Sub,
    Multply,
    Divide,
}

impl Ops {
    fn from_str(s: &str) -> Self {
        match s {
            "!=" => Self::NotEqual,
            "==" => Self::Equal,
            "<=" => Self::Leq,
            ">=" => Self::Geq,
            "<" => Self::Less,
            ">" => Self::Greater,
            "&&" => Self::And,
            "||" => Self::Or,
            "+" => Self::Add,
            "-" => Self::Sub,
            "/" => Self::Divide,
            "*" => Self::Multply,
            _ => Self::Unknown,
        }
    }
}

impl VType {
    fn from_str(s: &str) -> Self {
        match s {
            "number" => VType::Number,
            "string" => VType::String,
            "boolean" => VType::Boolean,
            _ => VType::Unknown,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Variable {
    name: Box<AstNode>,
    vtype: VType,
}

impl Variable {
    fn new(name: Box<AstNode>, vtype: VType) -> Self {
        Self { name, vtype }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Constant {
    Unknown,
    Boolean(bool),
    Number(i64),
}

impl Constant {
    fn parse_bool(source: &str) -> Self {
        match source {
            "true" => Self::Boolean(true),
            "false" => Self::Boolean(false),
            _ => Self::Unknown,
        }
    }

    fn parse_numeric(source: &str) -> Self {
        Self::Number(source.parse::<i64>().unwrap())
    }

    fn is_numeric(&self) -> bool {
        match *self {
            Self::Number(_) => true,
            _ => false,
        }
    }

    fn is_boolean(&self) -> bool {
        match *self {
            Self::Boolean(_) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum AstNode {
    Todo,
    Constant(Constant),
    NumericTerm(Box<AstNode>, Box<AstNode>, Ops),
    BooleanTerm(Box<AstNode>, Box<AstNode>, Ops),
    Block(Vec<Box<AstNode>>),
    Func(Box<AstNode>, Vec<Variable>, Box<AstNode>),
    If(Box<AstNode>, Box<AstNode>),
    Loop(Box<AstNode>, Box<AstNode>),
    Ident(String),
    Call(Box<AstNode>, Vec<Box<AstNode>>),
    Assign(Box<AstNode>, Box<AstNode>),
    Return,
}
