use pest::error::*;
use pest::iterators::Pair;
use pest::*;
use std::collections::{HashMap, VecDeque, BTreeSet};
use std::ops::*;
use z3::ast::{Ast, Bool, BV};
use z3::{Config, Context, Solver, SatResult};
use uuid::Uuid;

use crate::language::topological_sort::do_topological_sort;

const BV_SIZE: u32 = 63;
const INF: i64 = 1_000_000_000_000_000_000;

#[derive(Parser)]
#[grammar = "./akl.pest"]
pub struct AklParser<'a> {
    ctx: Context,
    pub funcs: Vec<Box<AstNode>>,
    variables: Vec<Vec<String>>,
    basis_t: Vec<Vec<(Box<Bool<'a>>, Vec<Box<BV<'a>>>)>>, // Supported arithmetic representation yet
    basis_f: Vec<Vec<Box<Bool<'a>>>>,
    func_map: HashMap<String, usize>,
    stack: Vec<Box<AstNode>>,
    var_stack: Vec<Variable>,
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
                Ops::And => lhs_eval & rhs_eval,
                Ops::Or => lhs_eval | rhs_eval,
                Ops::Xor => lhs_eval ^ rhs_eval,
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
            var_stack: vec![],
        }
    }

    pub fn numeric_convert(&self, expr: AstNode) -> BV<'_> {
        match expr {
            AstNode::NumericTerm(lhs, rhs, op) => {
                let lhs_eval = self.numeric_convert(*lhs);
                let rhs_eval = self.numeric_convert(*rhs);
                match op {
                    Ops::Add => lhs_eval + rhs_eval,
                    Ops::Sub => lhs_eval - rhs_eval,
                    Ops::Multply => lhs_eval * rhs_eval,
                    Ops::Divide => BV::from_int(&(lhs_eval.to_int(true) / rhs_eval.to_int(true)), BV_SIZE),
                    Ops::And => lhs_eval & rhs_eval,
                    Ops::Or => lhs_eval | rhs_eval,
                    Ops::Xor => lhs_eval ^ rhs_eval,
                    _ => {
                        // TODO
                        BV::from_i64(&self.ctx, 0, BV_SIZE)
                    }
                }
            }
            AstNode::Constant(c) => {
                match c {
                    Constant::Number(x) => BV::from_i64(&self.ctx, x, BV_SIZE),
                    _ => {
                        // TODO
                        BV::from_i64(&self.ctx, 0, BV_SIZE)
                    }
                }
            }
            AstNode::Ident(x) => BV::new_const(&self.ctx, x.as_str(), BV_SIZE),
            _ => {
                // TODO
                BV::from_i64(&self.ctx, 0, BV_SIZE)
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
                    },
                    AstNode::Ident(_) => {
                        match *r {
                            AstNode::NumericTerm(_, _, _) => numeric(),
                            AstNode::BooleanTerm(_, _, _) => boolean(),
                            AstNode::Constant(x) => {
                                if x.is_numeric() {
                                    numeric()
                                } else {
                                    boolean()
                                }
                            },
                            _ => Bool::from_bool(&self.ctx, false),
                        }
                    },
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
        let uuid = Uuid::from_u128(self.funcs.len() as u128);
        let name = uuid.to_simple().encode_lower(&mut Uuid::encode_buffer()).to_string();
        self.func_map.insert(name.clone(), self.funcs.len());
        let mut v = vec![];
        let v1 = vec![Box::new(block)];
        let block1 = AstNode::Block(v1);
        v.push(Box::new(AstNode::If(Box::new(expr), Box::new(block1))));
        v.push(Box::new(AstNode::Return));
        let block2 = AstNode::Block(v);
        let variables = self.var_stack.clone(); // TODO
        let mut variables2 = vec![]; // TODO
        for var in variables.clone() {
            variables2.push(var.name);
        }
        self.funcs.push(Box::new(AstNode::Func(Box::new(AstNode::Ident(name.clone())),variables,Box::new(block2))));
        AstNode::Call(Box::new(AstNode::Ident(name)), variables2)
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
        for var in variables.clone() {
            self.var_stack.push(var);
        }
        let block = self.parse_block(func.next().unwrap());
        self.basis_f.push(vec![]);
        self.basis_t.push(vec![]);
        self.variables.push(vec![]);
        for var in variables.clone() {
            self.var_stack.pop();
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
            AstNode::Call(name, vars) => {
                if let AstNode::Ident(x) = *name {
                    if self.func_map.get(&x).is_some() && f_num == self.func_map[&x] {
                        let mut cond = None;
                        let mut vars_ret = vec![];
                        for v in vars {
                            vars_ret.push(Box::new(unsafe {
                                std::mem::transmute::<BV<'_>, BV<'a>>(self.numeric_convert(*v))
                            }));
                        }
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
                            self.basis_t[f_num].push((Box::new(c), vars_ret));
                        } else {
                            self.basis_t[f_num].push((Box::new(unsafe {
                                std::mem::transmute::<Bool<'_>, Bool<'a>>(Bool::from_bool(&self.ctx, true)) }), vars_ret));
                        }
                    }
                }
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
            dbg!(self.funcs[i].clone());
            if let AstNode::Func(first, _, _) = *self.funcs[i].clone() {
                if let AstNode::Ident(name) = *first {
                    self.func_map.insert(name, i);
                }
            }
            self.dfs(i, *self.funcs[i].clone());
        }
        return Ok(());
    }

    pub fn eval(&mut self, max_depth: usize, deg: usize, i: usize) -> Vec<(Vec<i64>, i64)> {
        let mut res = vec![];
        // TODO: Currently, parameters must be BV - must be fix
        let mut q: VecDeque<Vec<i64>> = VecDeque::new();
        let mut dp: Vec<i64> = vec![];
        let mut map: HashMap<Vec<i64>, usize> = HashMap::new();
        let mut map_inv: Vec<Vec<i64>> = vec![];
        let mut adj: Vec<Vec<usize>> = vec![];
        let mut cnt = 0;
        for cond in self.basis_f[i].clone() {
            let solver = Solver::new(&self.ctx);
            solver.assert(&cond);
            if solver.check() == SatResult::Sat {
            if let Some(model) = solver.get_model() {
                let mut list: Vec<i64> = vec![];
                let mut failed = false;
                for var in self.variables[i].clone() {
                    if let Some(x) = model.eval(&BV::new_const(&self.ctx, var.as_str(), BV_SIZE), true) {
                        list.push(x.as_i64().unwrap()); // Unwrap must not be fail [PROOF]
                    } else {
                        failed = true;
                        break;
                    }
                }
                if !failed && map.get(&list.clone()).is_none() {
                    q.push_back(list.clone());
                    map.insert(list.clone(), cnt);
                    dp.push(1);
                    adj.push(vec![]);
                    map_inv.push(list.clone());
                    cnt += 1;
                }
            }
            }
        }
        while let Some(state) = q.pop_front() {
            if cnt == max_depth {
                // END
                break;
            }
            let idx = map[&state];
            //let cost = dp[&state];
            // Find adjacencies
            for (cond1, conds) in self.basis_t[i].clone() {
                let solver = Solver::new(&self.ctx);
                solver.assert(&cond1);
                for i in 0..conds.len() {
                    solver.assert(&conds[i]._eq(&BV::from_i64(&self.ctx, state[i], BV_SIZE)));
                }
                for _d in 0..deg {
                    if solver.check() != SatResult::Sat {
                        break;
                    }
                    if let Some(model) = solver.get_model() {
                        let mut list: Vec<i64> = vec![];
                        let mut failed = false;
                        for var in self.variables[i].clone() {
                            if let Some(x) =
                                model.eval(&BV::new_const(&self.ctx, var.as_str(), BV_SIZE), true)
                            {
                                list.push(x.as_i64().unwrap()); // Unwrap must not be fail [PROOF]
                                solver.assert(&!(BV::new_const(&self.ctx, var.as_str(), BV_SIZE)._eq(&x)));
                            } else {
                                failed = true;
                                break;
                            }
                        }
                        if !failed {
                            /*if let Some(x) = dp.get_mut(&list.clone()) {
                                if (*x) + cost < INF {
                                    *x += cost;
                                } else {
                                    *x = INF;
                                }
                            } else {
                                q.push_back(list.clone());
                                dp.insert(list.clone(), cost);
                            }*/
                            if let None = map.get(&list.clone()) {
                                q.push_back(list.clone());
                                map.insert(list.clone(), cnt);
                                map_inv.push(list.clone());
                                cnt += 1;
                                adj.push(vec![]);
                                dp.push(0);
                            }
                            adj[map[&list]].push(idx);
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        let ord = do_topological_sort(adj.clone());
        for u in ord {
            for v in adj[u].clone() {
                if dp[u] + dp[v]+1 >= INF {
                    dp[u] = INF;
                } else {
                    dp[u] += dp[v]+1;
                }
            }
        }
        for i in 0..cnt {
            res.push((map_inv[i].clone(), dp[i]));
        }
        res
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
    Xor,
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
            "&&" | "&" => Self::And,
            "||" | "|" => Self::Or,
            "^" => Self::Xor,
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
            _ => Self::parse_numeric(source), // Failed
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
    Ident(String),
    Call(Box<AstNode>, Vec<Box<AstNode>>),
    Assign(Box<AstNode>, Box<AstNode>),
    Return,
}
