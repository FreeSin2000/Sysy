use koopa::ir::*;
use koopa::ir::builder_traits::*;
use koopa::ir::entities::*;

use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl Visitable for CompUnit {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        *ast_trans = AstTrans::new();
        ast_trans.enter_scope();
        self.func_def.accept(ast_trans);
        ast_trans.exit_scope();
        None
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl Visitable for FuncDef {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        let name = String::from("@") + &self.ident;
        let params_ty = vec![];
        let ret_ty = match &self.func_type {
            FuncType::Int => Type::get_i32(),
        };       
        let func = ast_trans.new_func(name.into(), params_ty, ret_ty);
        ast_trans.enter_func(func);
        // ast_trans.enter_scope();
        let func_data = ast_trans.get_func_data_mut();
        let entry_bb = ast_trans.new_basic_block(Some("%entry".into()));
        ast_trans.extend_bb(entry_bb);
        // let entry_bb = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        ast_trans.enter_bb(entry_bb);
        self.block.accept(ast_trans);
        ast_trans.exit_bb();
        ast_trans.exit_func();
        None
    }
}
#[derive(Debug)]
pub enum FuncType {
    Int,
}

// Block         ::= "{" {BlockItem} "}";
#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

impl Visitable for Block {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {

        // let entry_bb = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));

        // let cur_uid = ast_trans.next_uid();
        // let entry_bb = ast_trans.new_basic_block(Some("%entry_".to_string() + &cur_uid.to_string()));

        // let entry_bb = ast_trans.new_basic_block(Some("%entry".to_string()));

        ast_trans.enter_scope();
        for block_item in &self.block_items {
            match block_item {
                BlockItem::Decl(decl) => {
                    match decl {
                        Decl::ConstDecl(const_decl) => {
                            const_decl.build_bindings(ast_trans); 
                        },
                        Decl::VarDecl(var_decl) => {
                            var_decl.build_bindngs(ast_trans);
                            // func_data.layout_mut().bb_mut(entry_bb).insts_mut().extend(insts);
                        }
                        _ => unimplemented!("Not implement other decl."),
                    }
                },
                BlockItem::Stmt(stmt) => {
                    // let (_, insts) = stmt.to_value(func_data, ast_trans);
                    stmt.accept(ast_trans);
                    // func_data.layout_mut().bb_mut(entry_bb).insts_mut().extend(insts);
                },
            };
        }
        ast_trans.exit_scope();
        None
    }
}

// Stmt          ::= LVal "=" Exp ";"
//                 | "return" Exp ";";
#[derive(Debug)]
pub enum Stmt {
    LValExp(LVal, Exp),
    OptExp(Option<Exp>),
    Block(Block),
    RetExp(Exp),
    IfStmt(Exp, Box<Stmt>, Option<Box<Stmt>>),
}

pub trait Visitable {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value>;
}
impl Visitable for Stmt {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            Self::LValExp(lval, exp) => {
                // let (l_val, mut l_insts) = lval.to_value(func_data, ast_trans);
                let l_val = lval.accept(ast_trans).unwrap();
                // let (r_val, r_insts) = exp.to_value(func_data, ast_trans);
                let r_val = exp.accept(ast_trans).unwrap();
                // let (s_val, s_insts) = AstTrans::build_store_op(func_data, l_val, r_val);
                let s_val = ast_trans.new_store(r_val, l_val);
                // l_insts.extend(r_insts);
                // l_insts.extend(s_insts);
                // (l_val, l_insts)
                Some(l_val)
            },
            Self::OptExp(opt_exp) => {
                match opt_exp {
                    Some(exp) => exp.accept(ast_trans),
                    None => None,
                }
            },
            Self::Block(block) => {
                block.accept(ast_trans);
                None
            },
            Self::RetExp(exp) => {
                // let (res, mut insts) = exp.to_value(func_data, ast_trans);
                let res = exp.accept(ast_trans).unwrap();
                // let (ret, ret_insts) = AstTrans::build_ret_op(func_data, Some(res));
                let ret = ast_trans.new_ret(Some(res));
                // insts.extend(ret_insts);
                Some(ret)
            },
            Self::IfStmt(exp, then_stmt, opt_else_stmt) => {
                let cond_val = exp.accept(ast_trans).unwrap();
                // let zero = ast_trans.new_integer(0);
                // let cond_val = ast_trans.new_binary(BinaryOp::NotEq, cond_exp, zero);


                let then_uid = ast_trans.next_uid();
                let then_block = ast_trans.new_basic_block(Some(format!("%then_{}", then_uid.to_string())));

                ast_trans.extend_bb(then_block);

                match opt_else_stmt {
                    Some(else_stmt) => {
                        let else_uid = ast_trans.next_uid();
                        let else_block = ast_trans.new_basic_block(Some(format!("%else_{}", else_uid.to_string())));

                        let end_uid = ast_trans.next_uid();
                        let end_block = ast_trans.new_basic_block(Some(format!("%end_{}", end_uid.to_string())));

                        ast_trans.extend_bb(else_block);
                        ast_trans.extend_bb(end_block);

                        let br_inst = ast_trans.new_branch(cond_val, then_block, else_block);

                        ast_trans.exit_bb();
                        ast_trans.enter_bb(then_block);
                        then_stmt.accept(ast_trans);
                        ast_trans.new_jump(end_block);

                        ast_trans.exit_bb();
                        ast_trans.enter_bb(else_block);
                        else_stmt.accept(ast_trans);
                        ast_trans.new_jump(end_block);
                        ast_trans.extend_bb(else_block);

                        ast_trans.exit_bb();
                        ast_trans.enter_bb(end_block);
                    },
                    None => {
                        let end_uid = ast_trans.next_uid();
                        let end_block = ast_trans.new_basic_block(Some(format!("%end_{}", end_uid.to_string())));
                        let br_inst = ast_trans.new_branch(cond_val, then_block, end_block);
                        ast_trans.extend_bb(end_block);

                        ast_trans.exit_bb();
                        ast_trans.enter_bb(then_block);
                        then_stmt.accept(ast_trans);
                        ast_trans.new_jump(end_block);

                        ast_trans.exit_bb();
                        ast_trans.enter_bb(end_block);
                    },
                };
                None
            },
        }
    }
}

#[derive(Debug)]
pub enum Number {
    INT_CONST(i32),
}

impl Visitable for Number {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            Number::INT_CONST(num) => {
                // let val = func_data.dfg_mut().new_value().integer(*num);
                let val = ast_trans.new_integer(*num);
                Some(val)
            },
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}


#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOpExp(UnaryOp, Box<UnaryExp>),
}

impl Visitable for UnaryExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.accept(ast_trans),
            UnaryExp::UnaryOpExp(unary_op, unary_exp) => {
                match unary_op {
                    UnaryOp::Plus => unary_exp.accept(ast_trans),
                    UnaryOp::Minus => {
                        // let lhs = func_data.dfg_mut().new_value().integer(0);
                        let lhs = ast_trans.new_integer(0);
                        // let (rhs, mut insts) = unary_exp.to_value(func_data, ast_trans);
                        let rhs = unary_exp.accept(ast_trans).unwrap();
                        // let (val, val_insts) = AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Sub);
                        let val = ast_trans.new_binary(BinaryOp::Sub, lhs, rhs);
                        // insts.extend(val_insts);
                        Some(val)
                    },
                    UnaryOp::Not => {
                        // let rhs  = func_data.dfg_mut().new_value().integer(0);
                        let rhs = ast_trans.new_integer(0);
                        // let (lhs, mut insts) = unary_exp.to_value(func_data, ast_trans);
                        let lhs = unary_exp.accept(ast_trans).unwrap();
                        // let (val, val_insts) = AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Eq); 
                        let val = ast_trans.new_binary(BinaryOp::Eq, lhs, rhs);
                        // insts.extend(val_insts);
                        // (val, insts)
                        Some(val)
                    },
                }
            }
        }
    }
}

// PrimaryExp    ::= "(" Exp ")" | LVal | Number;
#[derive(Debug)]
pub enum PrimaryExp {
    ParenthesizedExp(Exp),
    LVal(LVal),
    Number(Number),
}

impl Visitable for PrimaryExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            PrimaryExp::ParenthesizedExp(exp) => exp.accept(ast_trans),
            PrimaryExp::LVal(lval) => {
                // let (val, mut val_insts) = lval.to_value(func_data, ast_trans);
                let val = lval.accept(ast_trans).unwrap(); 
                // let val_data = func_data.dfg().value(val);
                match ast_trans.get_value_kind(val) {
                    ValueKind::Integer(int) => Some(val),
                    ValueKind::Binary(binary) => Some(val),
                    ValueKind::Alloc(alloc) => {
                        // let (load_val, load_insts) = AstTrans::build_load_op(func_data, val);
                        let load_val = ast_trans.new_load(val);
                        // val_insts.extend(load_insts);
                        // (load_val, val_insts)
                        Some(load_val)
                    }
                    _ => unimplemented!("Unimplement LVal kind."),
                }
            },
            PrimaryExp::Number(num) => num.accept(ast_trans),
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

impl Visitable for Exp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        self.lor_exp.accept(ast_trans)
    }
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
} 

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulOpExp(Box<MulExp>, MulOp, UnaryExp),
}

impl Visitable for MulExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.accept(ast_trans),
            MulExp::MulOpExp(mul_exp, mul_op, unary_exp) => {
                // let (lhs, mut l_insts) = mul_exp.to_value(func_data, ast_trans);
                let lhs = mul_exp.accept(ast_trans).unwrap();
                // let (rhs, r_insts) = unary_exp.to_value(func_data, ast_trans);
                let rhs = unary_exp.accept(ast_trans).unwrap();
                let val = match mul_op {
                    MulOp::Mul => ast_trans.new_binary(BinaryOp::Mul, lhs, rhs),
                    MulOp::Div => ast_trans.new_binary(BinaryOp::Div, lhs, rhs),
                    MulOp::Mod => ast_trans.new_binary(BinaryOp::Mod, lhs, rhs),
                };
                // l_insts.extend(r_insts);
                // l_insts.extend(val_insts);
                // (val, l_insts)
                Some(val)
            }
        }
    }
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
} 
#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    AddOpExp(Box<AddExp>, AddOp, MulExp),
}
impl Visitable for AddExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.accept(ast_trans),
            AddExp::AddOpExp(add_exp, add_op, mul_exp) => {
                let lhs = add_exp.accept(ast_trans).unwrap();
                let rhs = mul_exp.accept(ast_trans).unwrap();
                let val = match add_op {
                    AddOp::Add => ast_trans.new_binary(BinaryOp::Add, lhs, rhs),
                    AddOp::Sub => ast_trans.new_binary(BinaryOp::Sub, lhs, rhs),
                };
                // l_insts.extend(r_insts);
                // l_insts.extend(val_insts);
                // (val, l_insts)
                Some(val)
            }
        }
    }
}

// RelExp      ::= AddExp | RelExp ("<" | ">" | "<=" | ">=") AddExp;
#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    RelOpExp(Box<RelExp>, RelOp, AddExp),
}

impl Visitable for RelExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            RelExp::AddExp(add_exp) => add_exp.accept(ast_trans),
            RelExp::RelOpExp(rel_exp, rel_op, add_exp) => {
                let lhs = rel_exp.accept(ast_trans).unwrap();
                let rhs = add_exp.accept(ast_trans).unwrap();
                let val = match rel_op {
                    RelOp::Gt => ast_trans.new_binary(BinaryOp::Gt, lhs, rhs),
                    RelOp::Lt => ast_trans.new_binary(BinaryOp::Lt, lhs, rhs),
                    RelOp::Ge => ast_trans.new_binary(BinaryOp::Ge, lhs, rhs),
                    RelOp::Le => ast_trans.new_binary(BinaryOp::Le, lhs, rhs),
                };
                // l_insts.extend(r_insts);
                // l_insts.extend(val_insts);
                // (val, l_insts)
                Some(val)
            }
        }
    }
}

#[derive(Debug)]
pub enum RelOp {
    Lt,
    Le,
    Gt,
    Ge,
}

// EqExp       ::= RelExp | EqExp ("==" | "!=") RelExp;

#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    EqOpExp(Box<EqExp>, EqOp, RelExp),
}

impl Visitable for EqExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.accept(ast_trans),
            EqExp::EqOpExp(eq_exp, eq_op, rel_exp) => {
                let lhs = eq_exp.accept(ast_trans).unwrap();
                let rhs = rel_exp.accept(ast_trans).unwrap();
                let val = match eq_op {
                    EqOp::Eq => ast_trans.new_binary(BinaryOp::Eq, lhs, rhs),
                    EqOp::NotEq => ast_trans.new_binary(BinaryOp::NotEq, lhs, rhs),
                };
                // l_insts.extend(r_insts);
                // l_insts.extend(val_insts);
                // (val, l_insts)
                Some(val)
            }
        }
    }
}
#[derive(Debug)]
pub enum EqOp {
    Eq,
    NotEq,
} 

// LAndExp     ::= EqExp | LAndExp "&&" EqExp;

#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndOpExp(Box<LAndExp>, EqExp),
}

impl Visitable for LAndExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.accept(ast_trans),
            LAndExp::LAndOpExp(land_exp, eq_exp) => {
                let lhs = land_exp.accept(ast_trans).unwrap();
                let rhs = eq_exp.accept(ast_trans).unwrap();
                let zero = ast_trans.new_integer(0);
                let logic_lhs = ast_trans.new_binary(BinaryOp::NotEq, lhs, zero);
                let logic_rhs = ast_trans.new_binary(BinaryOp::NotEq, rhs, zero);
                let val = ast_trans.new_binary(BinaryOp::And, logic_lhs, logic_rhs);
                // l_insts.extend(r_insts);
                // l_insts.extend(ll_insts);
                // l_insts.extend(lr_insts);
                // l_insts.extend(val_insts);
                // (val, l_insts)
                // ast_trans.extend_insts(vec![logic_lhs, logic_rhs, val]);
                Some(val)
            }
        }
    }
}
#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOpExp(Box<LOrExp>, LAndExp),
}
impl Visitable for LOrExp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.accept(ast_trans),
            LOrExp::LOrOpExp(lor_exp, land_exp) => {
                let lhs = lor_exp.accept(ast_trans).unwrap();
                let rhs = land_exp.accept(ast_trans).unwrap();
                let val = ast_trans.new_binary(BinaryOp::Or, lhs, rhs);
                // let zero = func_data.dfg_mut().new_value().integer(0);
                let zero = ast_trans.new_integer(0);
                let logic_val = ast_trans.new_binary(BinaryOp::NotEq, zero, val);
                // l_insts.extend(r_insts);
                // l_insts.extend(val_insts);
                // l_insts.extend(lv_insts);
                // (l_val, l_insts)
                Some(logic_val)
            }
        }
    }
}

// Decl          ::= ConstDecl | VarDecl;
#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

// ConstDecl     ::= "const" BType ConstDef {"," ConstDef} ";";
#[derive(Debug)]
pub struct ConstDecl {
    pub btype: BType,
    pub const_defs: Vec<ConstDef>,
}

impl ConstDecl {
    fn build_bindings(&self, ast_trans: &mut AstTrans) -> (){
        for const_def in &self.const_defs {
            let (ident, val) = const_def.to_binding(ast_trans);
            match self.btype {
                BType::Int => {
                    // let val_data = ast_trans.get_value_data(val);
                    // let num = AstTrans::const_num(val_data);
                    let num = ast_trans.get_const(val);
                    ast_trans.bind(ident.clone(), BindingItem::ConstInt(val, num));
                },
                _ => unimplemented!("Not implement other type bindings."),
            }
        }
    }
}

// BType         ::= "int";
#[derive(Debug)]
pub enum BType {
    Int,
}

// ConstDef      ::= IDENT "=" ConstInitVal;
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

impl ConstDef {
    fn to_binding(&self, ast_trans: &mut AstTrans) -> (String, Value) {
        let val = self.const_init_val.const_exp.exp.accept(ast_trans).unwrap();
        assert!(ast_trans.get_value_kind(val).is_const());
        (self.ident.clone(), val)
    }
}

// ConstInitVal  ::= ConstExp;
#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}


// BlockItem     ::= Decl | Stmt;
#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}


// LVal          ::= IDENT;
#[derive(Debug)]
pub enum LVal {
    IDENT(String),
}
impl Visitable for LVal {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            LVal::IDENT(ident) => { 
                if let Some(binding) = ast_trans.lookup(ident) {
                    match binding {
                        BindingItem::ConstInt(val, num) => {
                            Some(*val)
                        },
                        BindingItem::VarInt(val) => {
                            Some(*val)
                        },
                        _ => unimplemented!("Unimplement BindingItem type."),
                    }
                } else {
                    panic!("undefined name.");
                }
            },
            _ => unimplemented!("Unimplement LVal."),
        }
    }
}
// ConstExp      ::= Exp;
#[derive(Debug)]
pub struct ConstExp {
    pub exp: Exp,
}

// InitVal       ::= Exp;
#[derive(Debug)]
pub struct InitVal {
    pub exp: Exp,
}
impl Visitable for InitVal {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        self.exp.accept(ast_trans)
    }
}

// VarDef        ::= IDENT | IDENT "=" InitVal;
#[derive(Debug)]
pub enum VarDef {
    IDENT(String),
    IDENTInitVal(String, InitVal),
}

// VarDecl       ::= BType VarDef {"," VarDef} ";";
#[derive(Debug)]
pub struct VarDecl {
    pub btype: BType,
    pub var_defs: Vec<VarDef>,
}

impl VarDecl {
    pub fn build_bindngs(&self, ast_trans: &mut AstTrans) {
        for var_def in &self.var_defs {
            match var_def {
                VarDef::IDENT(ident) => {
                    // let (l_val, l_insts) = AstTrans::build_alloc_op(func_data, Some(String::from("@") + ident), TypeKind::Int32);
                    let l_val = ast_trans.new_alloc(Type::get(TypeKind::Int32));
                    let cur_uid = ast_trans.next_uid();
                    ast_trans.set_value_name(l_val, Some(String::from("@") + ident + &format!("_{}", cur_uid.to_string())));
                    let binding = BindingItem::VarInt(l_val);
                    ast_trans.bind(ident.to_string(), binding);
                    // decl_insts.extend(l_insts);
                },
                VarDef::IDENTInitVal(ident, init_val) => {
                    // let (l_val, mut l_insts) = AstTrans::build_alloc_op(func_data, Some(String::from("@") + ident), TypeKind::Int32);
                    let l_val = ast_trans.new_alloc(Type::get(TypeKind::Int32));
                    let cur_uid = ast_trans.next_uid();
                    ast_trans.set_value_name(l_val, Some(String::from("@") + ident + &format!("_{}", cur_uid.to_string())));
                    let binding = BindingItem::VarInt(l_val);
                    ast_trans.bind(ident.to_string(), binding);

                    let r_val = init_val.accept(ast_trans).unwrap();
                    // let (s_val, s_insts) = AstTrans::build_store_op(func_data, l_val, r_val);
                    let s_val = ast_trans.new_store(r_val, l_val);
                    // l_insts.extend(r_insts);
                    // l_insts.extend(s_insts);

                }
            }
        }
    }
}

pub enum BindingItem {
    ConstInt(Value, i32),
    VarInt(Value),
}
pub struct AstContext {
    pub funcs: Vec<Function>,
    pub bbs: Vec<BasicBlock>,
}
impl AstContext {
    pub fn new() -> AstContext {
        AstContext {
            funcs: Vec::new(),
            bbs: Vec::new(),
        }
    }
    pub fn push_bb(&mut self, bb: BasicBlock) {
        self.bbs.push(bb);
    }
    pub fn push_func(&mut self, func: Function) {
        self.funcs.push(func);
    }
    pub fn pop_bb(&mut self) {
        self.bbs.pop();
    }
    pub fn pop_func(&mut self) {
        self.funcs.pop();
    }
    pub fn get_bb(&mut self) -> BasicBlock {
        *self.bbs.last().unwrap()
    }
    pub fn get_func(&mut self) -> Function {
        *self.funcs.last().unwrap()
    }
}

pub struct ScopeStack {
    pub stack: Vec<HashMap<String, BindingItem>>,
    pub global_uid: i32,
}
impl ScopeStack {
    pub fn new() -> ScopeStack {
        ScopeStack {
            stack: Vec::new(),
            global_uid: -1,
        }
    }
    pub fn push_scope(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<HashMap<String, BindingItem>> {
        self.stack.pop()
    }

    pub fn new_uid(&mut self) -> i32 {
        self.global_uid = self.global_uid + 1;
        self.global_uid
    }
    pub fn cur_scope(&mut self) -> &HashMap<String, BindingItem> {
        self.stack.last().unwrap()
    }

    pub fn cur_scope_mut(&mut self) -> &mut HashMap<String, BindingItem> {
        self.stack.last_mut().unwrap()
    }
    pub fn get_binding(&mut self, name: &String) -> Option<&BindingItem> {
        let cur_scope = self.cur_scope();
        // cur_scope.get(name)
        self.stack
            .iter()
            .rev()
            .find_map(|scope| scope.get(name))
    }
    pub fn insert_binding(&mut self, name: &String, binding: BindingItem) {
        let cur_scope = self.cur_scope_mut();
        cur_scope.insert(name.to_string(), binding);
    }
}

pub struct AstTrans {
    pub local_sym_tab: HashMap<String, BindingItem>,
    pub scope_manager: ScopeStack,
    pub koopa_program: Program,
    pub cur_ctx: AstContext,
}

impl AstTrans {
    pub fn new() -> AstTrans {
        AstTrans {
            local_sym_tab: HashMap::new(),
            koopa_program: Program::new(),
            cur_ctx: AstContext::new(),
            scope_manager: ScopeStack::new(),
        }
    }

    pub fn enter_func(&mut self, func: Function) {
        self.cur_ctx.push_func(func);
    }
    pub fn enter_bb(&mut self, bb: BasicBlock) {
        self.cur_ctx.push_bb(bb);
    }

    pub fn exit_func(&mut self) {
        self.cur_ctx.pop_func();
    }
    pub fn exit_bb(&mut self) {
        self.cur_ctx.pop_bb();
    }

    pub fn get_cur_func(&mut self) -> Function {
        self.cur_ctx.get_func()
    }

    pub fn get_cur_bb(&mut self) -> BasicBlock {
        self.cur_ctx.get_bb()
    }

    pub fn extend_bb(&mut self, bb: BasicBlock) {
        let func_data = self.get_func_data_mut();
        func_data.layout_mut().bbs_mut().extend(vec![bb]);
    }

    pub fn set_value_name(&mut self, val: Value, name: Option<String>) {
        let func_data = self.get_func_data_mut();
        func_data.dfg_mut().set_value_name(val, name);
    }

    pub fn get_func_data(&mut self) -> &FunctionData {
        let func = self.get_cur_func();
        self.koopa_program.func(func)
    }
    pub fn get_func_data_mut(&mut self) -> &mut FunctionData{
        let func = self.get_cur_func();
        self.koopa_program.func_mut(func)
    }

    pub fn get_value_data(&mut self, val: Value) -> &ValueData {
        let func_data = self.get_func_data();
        func_data.dfg().value(val)
    }
    pub fn get_value_kind(&mut self, val: Value) -> &ValueKind {
        let val_data = self.get_value_data(val);
        val_data.kind()
    }
    pub fn get_const(&mut self, val: Value) -> i32 {
        let val_data = self.get_value_data(val);
        Self::const_num(val_data)
    }
    pub fn new_func(&mut self, func_name: String, func_params_ty: Vec<(Option<String>, Type)>, func_ret_ty: Type) -> Function {
        self.koopa_program.new_func(FunctionData::with_param_names(func_name, func_params_ty, func_ret_ty))
    }
    pub fn new_basic_block(&mut self, bb_name: Option<String>) -> BasicBlock {
        let func_data = self.get_func_data_mut();
        func_data.dfg_mut().new_bb().basic_block(bb_name)
    }

    pub fn new_integer(&mut self, num: i32) -> Value {
        let func_data = self.get_func_data_mut();
        func_data.dfg_mut().new_value().integer(num)
    }
    pub fn new_ret(&mut self, res: Option<Value>) -> Value {
        let func_data = self.get_func_data_mut();
        let ret_val = func_data.dfg_mut().new_value().ret(res);
        self.extend_inst(ret_val);
        ret_val
    }
    pub fn new_jump(&mut self, dest_block: BasicBlock) -> Value {
        let func_data = self.get_func_data_mut();
        let jump_val = func_data.dfg_mut().new_value().jump(dest_block);
        self.extend_inst(jump_val);
        jump_val
    }
    pub fn new_branch(&mut self, cond_val: Value, then_block: BasicBlock, else_block: BasicBlock) -> Value {
        let func_data = self.get_func_data_mut();
        let branch_val = func_data.dfg_mut().new_value().branch(cond_val, then_block, else_block);
        self.extend_inst(branch_val);
        branch_val
    }
    pub fn new_binary(&mut self, bin_op: BinaryOp, lhs: Value, rhs: Value) -> Value {
        if self.get_value_kind(lhs).is_const() && self.get_value_kind(rhs).is_const() {
            let lhs_num = self.get_const(lhs);
            let rhs_num = self.get_const(rhs);
            let val_num = match bin_op {
                BinaryOp::NotEq => (lhs_num != rhs_num) as i32,
                BinaryOp::Eq => (lhs_num == rhs_num) as i32,
                BinaryOp::Gt => (lhs_num > rhs_num) as i32,
                BinaryOp::Lt => (lhs_num < rhs_num) as i32,
                BinaryOp::Ge => (lhs_num >= rhs_num) as i32,
                BinaryOp::Le => (lhs_num <= rhs_num) as i32,
                BinaryOp::Add => lhs_num + rhs_num,
                BinaryOp::Sub => lhs_num - rhs_num,
                BinaryOp::Mul => lhs_num * rhs_num,
                BinaryOp::Div => lhs_num / rhs_num,
                BinaryOp::Mod => lhs_num % rhs_num,
                BinaryOp::Or => (lhs_num !=0 || rhs_num != 0) as i32,
                BinaryOp::And => (lhs_num != 0 && rhs_num != 0) as i32,
                _ => unimplemented!("Binary op not implement!"), 
            };
            self.new_integer(val_num)
        } else {
            let func_data = self.get_func_data_mut();
            let bin_val = func_data.dfg_mut().new_value().binary(bin_op, lhs, rhs);
            self.extend_inst(bin_val);
            bin_val
        }
    }

    pub fn new_load(&mut self, val: Value) -> Value {
        let func_data = self.get_func_data_mut();
        let load_val = func_data.dfg_mut().new_value().load(val);
        self.extend_inst(load_val);
        load_val
    }

    pub fn new_store(&mut self, val: Value, dest: Value) -> Value {
        let func_data = self.get_func_data_mut();
        let store_val = func_data.dfg_mut().new_value().store(val, dest);
        self.extend_inst(store_val); 
        store_val
    }

    pub fn new_alloc(&mut self, ty: Type) -> Value {
        let func_data = self.get_func_data_mut();
        let alloc_val = func_data.dfg_mut().new_value().alloc(ty);
        self.extend_inst(alloc_val);
        alloc_val
    }

    // pub fn extend_bb(&mut self, bb: BasicBlock) {
    //     let func_data = self.get_func_data_mut();
    //     func_data.layout_mut().bbs_mut().extend(vec![bb]);
    //     self.cur_ctx.push_bb(bb);
    // }

    pub fn extend_inst(&mut self, inst: Value) {
        let bb = self.get_cur_bb();
        let func_data = self.get_func_data_mut();
        func_data.layout_mut().bb_mut(bb).insts_mut().extend(vec![inst]);
    }
    pub fn extend_insts(&mut self, insts: Vec<Value>) {
        let bb = self.get_cur_bb();
        let func_data = self.get_func_data_mut();
        func_data.layout_mut().bb_mut(bb).insts_mut().extend(insts);
    }

    pub fn enter_scope(&mut self) {
        self.scope_manager.push_scope();
    }
    pub fn exit_scope(&mut self) {
        self.scope_manager.pop_scope();
    }
    pub fn bind(&mut self, name: String, binding: BindingItem) {
        // self.local_sym_tab.insert(name, binding);
        self.scope_manager.insert_binding(&name, binding);
    }
    pub fn lookup(&mut self, name: &String) -> Option<&BindingItem> {
        // self.local_sym_tab.get(name)
        self.scope_manager.get_binding(name)
    }

    pub fn next_uid(&mut self) -> i32 {
        self.scope_manager.new_uid()
    }
    // pub fn build_binary_op(func_data: &mut FunctionData, lhs: Value, rhs: Value, op: BinaryOp) -> (Value, Vec<Value>) {
    //     let lhs_data = func_data.dfg().value(lhs);
    //     let rhs_data = func_data.dfg().value(rhs);
    //     if lhs_data.kind().is_const() && rhs_data.kind().is_const() {
    //         let lhs_num = match lhs_data.kind() {
    //             ValueKind::Integer(num) => num.value(),
    //             _ => panic!("Not a number."), 
    //         };
    //         let rhs_num = match rhs_data.kind() {
    //             ValueKind::Integer(num) => num.value(),
    //             _ => panic!("Not a number!"), 
    //         };
    //         let val_num = match op {
    //             BinaryOp::NotEq => (lhs_num != rhs_num) as i32,
    //             BinaryOp::Eq => (lhs_num == rhs_num) as i32,
    //             BinaryOp::Gt => (lhs_num > rhs_num) as i32,
    //             BinaryOp::Lt => (lhs_num < rhs_num) as i32,
    //             BinaryOp::Ge => (lhs_num >= rhs_num) as i32,
    //             BinaryOp::Le => (lhs_num <= rhs_num) as i32,
    //             BinaryOp::Add => lhs_num + rhs_num,
    //             BinaryOp::Sub => lhs_num - rhs_num,
    //             BinaryOp::Mul => lhs_num * rhs_num,
    //             BinaryOp::Div => lhs_num / rhs_num,
    //             BinaryOp::Mod => lhs_num % rhs_num,
    //             BinaryOp::Or => (lhs_num !=0 || rhs_num != 0) as i32,
    //             BinaryOp::And => (lhs_num != 0 && rhs_num != 0) as i32,
    //             _ => unimplemented!("Binary op not implement!"), 
    //         };
    //         let val = func_data.dfg_mut().new_value().integer(val_num);
    //         (val, vec![])
    //     } else {
    //         let val = func_data.dfg_mut().new_value().binary(op, lhs, rhs);
    //         (val, vec![val])
    //     }
    // }
    // pub fn build_alloc_op(func_data: &mut FunctionData, ident: Option<String>, tk: TypeKind) -> (Value, Vec<Value>) {
    //     let a_val = func_data.dfg_mut().new_value().alloc(Type::get(tk));
    //     func_data.dfg_mut().set_value_name(a_val, ident);
    //     (a_val, vec![a_val])
    // }
    // pub fn build_store_op(func_data: &mut FunctionData, l_val: Value, r_val: Value) -> (Value, Vec<Value>) {
    //     let s_val = func_data.dfg_mut().new_value().store(r_val, l_val);
    //     (s_val, vec![s_val])
    // }
    // pub fn build_load_op(func_data: &mut FunctionData, val: Value) -> (Value, Vec<Value>) {
    //     let l_val = func_data.dfg_mut().new_value().load(val);
    //     (l_val, vec![l_val])
    // }
    // pub fn build_ret_op(func_data: &mut FunctionData, res: Option<Value>) -> (Value, Vec<Value>) {
    //     let ret_val = func_data.dfg_mut().new_value().ret(res);
    //     (ret_val, vec![ret_val])
    // }
    pub fn const_num(val_data: &ValueData) -> i32 {
        match val_data.kind() {
            ValueKind::Integer(int) => int.value(),
            _ => panic!("Not a Integer, maybe not implement."),
        }
    }
}
use koopa::back::KoopaGenerator;
pub fn koopa_to_string(program: & Program) -> String {
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(program).unwrap();
    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
    return text_form_ir;
}