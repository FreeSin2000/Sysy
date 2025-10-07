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
        let entry_bb = ast_trans.new_basic_block(Some("%entry".into()));
        ast_trans.extend_bb(entry_bb);

        self.block.accept(ast_trans);

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
                        }
                        _ => unimplemented!("Not implement other decl."),
                    }
                },
                BlockItem::Stmt(stmt) => {
                    stmt.accept(ast_trans);
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
    WhileStmt(Exp, Box<Stmt>),
}

pub trait Visitable {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value>;
}
impl Visitable for Stmt {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        if ast_trans.is_cur_bb_terminate() {
            None
        } else {
            match self {
                Self::LValExp(lval, exp) => {
                    let l_val = lval.accept(ast_trans).unwrap();
                    let r_val = exp.accept(ast_trans).unwrap();
                    let s_val = ast_trans.new_store(r_val, l_val);
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
                    let res = exp.accept(ast_trans).unwrap();
                    let ret = ast_trans.new_ret(Some(res));
                    Some(ret)
                },
                Self::IfStmt(exp, then_stmt, opt_else_stmt) => {

                    let cond_val = exp.accept(ast_trans).unwrap();

                    let then_uid = ast_trans.next_uid();
                    let then_block = ast_trans.new_basic_block(Some(format!("%if_then_{}", then_uid.to_string())));

                    match opt_else_stmt {
                        Some(else_stmt) => {
                            let else_uid = ast_trans.next_uid();
                            let else_block = ast_trans.new_basic_block(Some(format!("%if_else_{}", else_uid.to_string())));

                            let end_uid = ast_trans.next_uid();
                            let end_block = ast_trans.new_basic_block(Some(format!("%if_end_{}", end_uid.to_string())));

                            
                            let br_inst = ast_trans.new_branch(cond_val, then_block, else_block);

                            ast_trans.extend_bb(then_block);
                            ast_trans.enter_scope();
                            then_stmt.accept(ast_trans);
                            ast_trans.exit_scope();
                            if !ast_trans.is_cur_bb_terminate() {
                                ast_trans.new_jump(end_block);
                            }
                            
                            ast_trans.extend_bb(else_block);
                            ast_trans.enter_scope();
                            else_stmt.accept(ast_trans);
                            ast_trans.exit_scope();

                            if !ast_trans.is_cur_bb_terminate() {
                                ast_trans.new_jump(end_block);
                            }
                            ast_trans.extend_bb(end_block);
                        },
                        None => {
                            let end_uid = ast_trans.next_uid();
                            let end_block = ast_trans.new_basic_block(Some(format!("%if_end_{}", end_uid.to_string())));
                            let br_inst = ast_trans.new_branch(cond_val, then_block, end_block);

                            ast_trans.extend_bb(then_block);
                            ast_trans.enter_scope();
                            then_stmt.accept(ast_trans);
                            ast_trans.exit_scope();

                            if !ast_trans.is_cur_bb_terminate() {
                                ast_trans.new_jump(end_block);
                            }
                            ast_trans.extend_bb(end_block);
                        },
                    };
                    None
                },
                Self::WhileStmt(exp, stmt) => {
                    let entry_uid = ast_trans.next_uid();
                    let entry_bb = ast_trans.new_basic_block(Some(format!("%while_entry_{}", entry_uid.to_string())));
                    let body_uid = ast_trans.next_uid();
                    let body_bb = ast_trans.new_basic_block(Some(format!("%while_body_{}", body_uid.to_string())));
                    let end_uid = ast_trans.next_uid();
                    let end_bb = ast_trans.new_basic_block(Some(format!("%while_end_{}", end_uid.to_string())));

                    ast_trans.new_jump(entry_bb);

                    ast_trans.extend_bb(entry_bb);
                    let cond_val = exp.accept(ast_trans).unwrap();
                    ast_trans.new_branch(cond_val, body_bb, end_bb);

                    ast_trans.extend_bb(body_bb);
                    ast_trans.enter_scope();
                    stmt.accept(ast_trans);
                    if !ast_trans.is_cur_bb_terminate() {
                        ast_trans.new_jump(entry_bb);
                    }
                    ast_trans.exit_scope();

                    ast_trans.extend_bb(end_bb);
                    None
                },
            }
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
                        let lhs = ast_trans.new_integer(0);
                        let rhs = unary_exp.accept(ast_trans).unwrap();
                        let val = ast_trans.new_binary(BinaryOp::Sub, lhs, rhs);
                        Some(val)
                    },
                    UnaryOp::Not => {
                        let rhs = ast_trans.new_integer(0);
                        let lhs = unary_exp.accept(ast_trans).unwrap();
                        let val = ast_trans.new_binary(BinaryOp::Eq, lhs, rhs);
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
                let val = lval.accept(ast_trans).unwrap(); 
                match ast_trans.get_value_kind(val) {
                    ValueKind::Integer(int) => Some(val),
                    ValueKind::Binary(binary) => Some(val),
                    ValueKind::Alloc(alloc) => {
                        let load_val = ast_trans.new_load(val);
                        Some(load_val)
                    },
                    ValueKind::Load(load) => Some(val),
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
                let lhs = mul_exp.accept(ast_trans).unwrap();
                let rhs = unary_exp.accept(ast_trans).unwrap();
                let val = match mul_op {
                    MulOp::Mul => ast_trans.new_binary(BinaryOp::Mul, lhs, rhs),
                    MulOp::Div => ast_trans.new_binary(BinaryOp::Div, lhs, rhs),
                    MulOp::Mod => ast_trans.new_binary(BinaryOp::Mod, lhs, rhs),
                };
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
                
                let then_uid = ast_trans.next_uid();
                let then_bb = ast_trans.new_basic_block(Some(format!("%land_then_{}", then_uid.to_string())));

                let else_uid = ast_trans.next_uid();
                let else_bb = ast_trans.new_basic_block(Some(format!("%land_else_{}", else_uid.to_string())));

                let end_uid = ast_trans.next_uid();
                let end_bb = ast_trans.new_basic_block(Some(format!("%land_end_{}", end_uid.to_string())));

                let land_val = ast_trans.new_alloc(Type::get(TypeKind::Int32));

                let lhs = land_exp.accept(ast_trans).unwrap();

                ast_trans.new_branch(lhs, then_bb, else_bb);

                ast_trans.extend_bb(then_bb);

                let rhs = eq_exp.accept(ast_trans).unwrap();
                let zero = ast_trans.new_integer(0);
                let logic_rhs = ast_trans.new_binary(BinaryOp::NotEq, rhs, zero);

                ast_trans.new_store(logic_rhs, land_val);
                ast_trans.new_jump(end_bb); 

                ast_trans.extend_bb(else_bb);
                ast_trans.new_store(zero, land_val);
                ast_trans.new_jump(end_bb);

                ast_trans.extend_bb(end_bb);
                let result_val = ast_trans.new_load(land_val);
                Some(result_val)
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
                let then_uid = ast_trans.next_uid();
                let then_bb = ast_trans.new_basic_block(Some(format!("%lor_then_{}", then_uid.to_string())));

                let else_uid = ast_trans.next_uid();
                let else_bb = ast_trans.new_basic_block(Some(format!("%lor_else_{}", else_uid.to_string())));

                let end_uid = ast_trans.next_uid();
                let end_bb = ast_trans.new_basic_block(Some(format!("%lor_end_{}", end_uid.to_string())));

                let lor_val = ast_trans.new_alloc(Type::get(TypeKind::Int32));

                let lhs = lor_exp.accept(ast_trans).unwrap();
                let zero = ast_trans.new_integer(0);
                let logic_lhs = ast_trans.new_binary(BinaryOp::NotEq, lhs, zero);

                ast_trans.new_branch(lhs, then_bb, else_bb);
                ast_trans.extend_bb(then_bb);
                ast_trans.new_store(logic_lhs, lor_val);
                ast_trans.new_jump(end_bb);

                ast_trans.extend_bb(else_bb);
                let rhs = land_exp.accept(ast_trans).unwrap();
                let logic_rhs = ast_trans.new_binary(BinaryOp::NotEq, rhs, zero);

                ast_trans.new_store(logic_rhs, lor_val);
                ast_trans.new_jump(end_bb); 

                ast_trans.extend_bb(end_bb);
                let result_val = ast_trans.new_load(lor_val);
                Some(result_val)
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
                    // TODO: 
                    ast_trans.bind(ident.clone(), BindingItem::ConstInt(val));
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
        // assert!(ast_trans.get_value_kind(val).is_const());
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
                        BindingItem::ConstInt(val) => {
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
                    let l_val = ast_trans.new_alloc(Type::get(TypeKind::Int32));
                    let cur_uid = ast_trans.next_uid();
                    ast_trans.set_value_name(l_val, Some(String::from("@") + ident + &format!("_{}", cur_uid.to_string())));
                    let binding = BindingItem::VarInt(l_val);
                    ast_trans.bind(ident.to_string(), binding);
                },
                VarDef::IDENTInitVal(ident, init_val) => {
                    let l_val = ast_trans.new_alloc(Type::get(TypeKind::Int32));
                    let cur_uid = ast_trans.next_uid();
                    ast_trans.set_value_name(l_val, Some(String::from("@") + ident + &format!("_{}", cur_uid.to_string())));
                    let binding = BindingItem::VarInt(l_val);
                    ast_trans.bind(ident.to_string(), binding);

                    let r_val = init_val.accept(ast_trans).unwrap();
                    let s_val = ast_trans.new_store(r_val, l_val);
                }
            }
        }
    }
}

pub enum BindingItem {
    ConstInt(Value),
    VarInt(Value),
}
pub struct AstContext {
    pub func: Option<Function>,
    pub bb: Option<BasicBlock>,
    pub bb_terminate: bool,
}
impl AstContext {
    pub fn new() -> AstContext {
        AstContext {
            func: None,
            bb: None,
            bb_terminate: false,
        }
    }
    pub fn set_bb(&mut self, bb: BasicBlock) {
        self.bb = Some(bb);
    }
    pub fn clear_bb_terminate(&mut self) {
        self.bb_terminate = false;
    }
    pub fn set_bb_terminate(&mut self) {
        self.bb_terminate = true;
    }
    pub fn get_bb_terminate(&mut self) -> bool {
        self.bb_terminate
    }

    pub fn set_func(&mut self, func: Function) {
        self.func = Some(func);
    }
    pub fn get_bb(&mut self) -> BasicBlock {
        self.bb.unwrap()
    }
    pub fn get_func(&mut self) -> Function {
        self.func.unwrap()
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

    pub fn get_cur_func(&mut self) -> Function {
        self.cur_ctx.get_func()
    }

    pub fn get_cur_bb(&mut self) -> BasicBlock {
        self.cur_ctx.get_bb()
    }

    pub fn is_cur_bb_terminate(&mut self) -> bool {
        self.cur_ctx.get_bb_terminate()
    }

    pub fn extend_bb(&mut self, bb: BasicBlock) {
        let func_data = self.get_func_data_mut();
        func_data.layout_mut().bbs_mut().extend(vec![bb]);
        self.cur_ctx.set_bb(bb);
        self.cur_ctx.clear_bb_terminate();
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
        let func = self.koopa_program.new_func(FunctionData::with_param_names(func_name, func_params_ty, func_ret_ty));
        self.cur_ctx.set_func(func);
        func
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
        self.cur_ctx.set_bb_terminate();
        ret_val
    }
    pub fn new_jump(&mut self, dest_block: BasicBlock) -> Value {
        let func_data = self.get_func_data_mut();
        let jump_val = func_data.dfg_mut().new_value().jump(dest_block);
        self.extend_inst(jump_val);
        self.cur_ctx.set_bb_terminate();
        jump_val
    }
    pub fn new_branch(&mut self, cond_val: Value, then_block: BasicBlock, else_block: BasicBlock) -> Value {
        let func_data = self.get_func_data_mut();
        let branch_val = func_data.dfg_mut().new_value().branch(cond_val, then_block, else_block);
        self.extend_inst(branch_val);
        self.cur_ctx.set_bb_terminate();
        branch_val
    }
    pub fn new_binary(&mut self, bin_op: BinaryOp, lhs: Value, rhs: Value) -> Value {
        let func_data = self.get_func_data_mut();
        let bin_val = func_data.dfg_mut().new_value().binary(bin_op, lhs, rhs);
        self.extend_inst(bin_val);
        bin_val
        // if self.get_value_kind(lhs).is_const() && self.get_value_kind(rhs).is_const() {
        //     let lhs_num = self.get_const(lhs);
        //     let rhs_num = self.get_const(rhs);
        //     let val_num = match bin_op {
        //         BinaryOp::NotEq => (lhs_num != rhs_num) as i32,
        //         BinaryOp::Eq => (lhs_num == rhs_num) as i32,
        //         BinaryOp::Gt => (lhs_num > rhs_num) as i32,
        //         BinaryOp::Lt => (lhs_num < rhs_num) as i32,
        //         BinaryOp::Ge => (lhs_num >= rhs_num) as i32,
        //         BinaryOp::Le => (lhs_num <= rhs_num) as i32,
        //         BinaryOp::Add => lhs_num + rhs_num,
        //         BinaryOp::Sub => lhs_num - rhs_num,
        //         BinaryOp::Mul => lhs_num * rhs_num,
        //         BinaryOp::Div => lhs_num / rhs_num,
        //         BinaryOp::Mod => lhs_num % rhs_num,
        //         BinaryOp::Or => (lhs_num !=0 || rhs_num != 0) as i32,
        //         BinaryOp::And => (lhs_num != 0 && rhs_num != 0) as i32,
        //         _ => unimplemented!("Binary op not implement!"), 
        //     };
        //     self.new_integer(val_num)
        // } else {
        //     let func_data = self.get_func_data_mut();
        //     let bin_val = func_data.dfg_mut().new_value().binary(bin_op, lhs, rhs);
        //     self.extend_inst(bin_val);
        //     bin_val
        // }
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