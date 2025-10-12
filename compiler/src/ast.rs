use koopa::ir::*;
use koopa::ir::builder_traits::*;
use koopa::ir::entities::*;

use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug,Clone)]
pub enum CompType {
    Int,
    Void,
}

#[derive(Debug,Clone)]
pub enum GlobalUnit {
    FuncDef(FuncDef),
    Decl(Decl),
}
// CompUnit    ::= [CompUnit] FuncDef;
#[derive(Debug,Clone)]
pub struct CompUnit {
    pub comp_unit: Box<Option<CompUnit>>,
    pub global_unit: GlobalUnit,
}


impl CompUnit {
    pub fn collect_global(&self, ast_trans: &mut AstTrans) {

        if let Some(comp_unit) = &*self.comp_unit {
            comp_unit.collect_global(ast_trans);
        } else {
            let ty_i32 = Type::get(TypeKind::Int32);
            let ty_ptr_i32 = Type::get(TypeKind::Pointer(ty_i32.clone()));
            let ty_unit = Type::get(TypeKind::Unit);
            ast_trans.new_func_decl(String::from("@getint"),vec![], ty_i32.clone());
            ast_trans.new_func_decl(String::from("@getch"),vec![], ty_i32.clone());
            ast_trans.new_func_decl(String::from("@getarray"),vec![ty_ptr_i32.clone()], ty_i32.clone());
            ast_trans.new_func_decl(String::from("@putint"),vec![ty_i32.clone()], ty_unit.clone());
            ast_trans.new_func_decl(String::from("@putch"),vec![ty_i32.clone()], ty_unit.clone());
            ast_trans.new_func_decl(String::from("@putarray"),vec![ty_i32.clone(), ty_ptr_i32.clone()], ty_unit.clone());
            ast_trans.new_func_decl(String::from("@starttime"),vec![], ty_unit.clone());
            ast_trans.new_func_decl(String::from("@stoptime"),vec![], ty_unit.clone());
        }
        match &self.global_unit {
            GlobalUnit::FuncDef(func_def) => {
                let func_name = String::from("@") + &func_def.ident;
                // let mut params_ty = vec![];
                // if let Some(func_params) = &func_def.params {
                //     for param in &func_params.params {
                //         let param_type = match param.param_type {
                //             BType::Int => Type::get(TypeKind::Int32),
                //         };
                //         let param_name = param.param_name.clone();
                //         params_ty.push((Some(format!("@{}", param_name)), param_type));
                //     }
                // }
                let params_ty: Vec<(Option<String>, Type)> = 
                    if let Some(func_params) = &func_def.params {
                        func_params.params
                            .clone()
                            .into_iter()
                            .map(|param| {
                                let param_type = match param.param_type {
                                    BType::Int => Type::get(TypeKind::Int32),
                                };
                                let param_name = param.param_name.clone();
                                (Some(format!("@{}", param_name)), param_type)
                            })
                            .collect()
                    } else {
                        vec![]
                    };
                let ret_ty = match &func_def.func_type {
                    FuncType::Int => Type::get_i32(),
                    FuncType::Void => Type::get_unit(),
                };       
                let func = ast_trans.new_func(func_name, params_ty.clone(), ret_ty);
                ast_trans.bind(func_def.ident.clone(), BindingItem::Func(func));
            },
            GlobalUnit::Decl(decl) => {
                match decl {
                    Decl::ConstDecl(const_decl) => {
                        const_decl.build_bindings(ast_trans); 
                    },
                    Decl::VarDecl(var_decl) => {
                        for var_def in &var_decl.var_defs {
                            // TODO
                            let l_ty = Type::get(TypeKind::Int32);
                            let (ident, l_init) = match var_def {
                                VarDef::IDENT(ident) => {
                                    (ident.clone(), ast_trans.koopa_program.new_value().zero_init(l_ty))
                                },
                                VarDef::IDENTInitVal(ident, init_val) => {
                                    let init_num = init_val.exp.const_eval(ast_trans);
                                    (ident.clone(), ast_trans.koopa_program.new_value().integer(init_num))
                                }
                            };
                            let l_val = ast_trans.new_global_alloc(l_init);
                            ast_trans.set_global_value_name(l_val, Some(String::from("@") + &ident));
                            let binding = BindingItem::VarInt(l_val);
                            ast_trans.bind(ident.to_string(), binding);
                        }
                    },
                    // _ => unimplemented!("Not implement other decl."),
                }
            },
        }
    }
}

impl Visitable for CompUnit {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        if let Some(comp_unit) = &*self.comp_unit {
            comp_unit.accept(ast_trans);
        }
        match &self.global_unit {
            GlobalUnit::FuncDef(func_def) => {
                ast_trans.enter_scope();
                func_def.accept(ast_trans);
                ast_trans.exit_scope();
            },
            GlobalUnit::Decl(_) => (),
            _ => todo!("Unimplement Global Decl."),
        }
        None
    }
}

#[derive(Debug,Clone)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub params: Option<FuncFParams>,
    pub block: Block,
}

impl Visitable for FuncDef {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        let func_binding = ast_trans.lookup(&self.ident).unwrap();
        let func = match func_binding {
            BindingItem::Func(f) => *f,
            _ => panic!("Not a func."),
        };

        ast_trans.set_func(func);

        // let mut fparams_name = vec![];
        let fparams_name: Vec<String> = 
            if let Some(func_params) = &self.params {
                // for param in &func_params.params {
                //     let param_name = param.param_name.clone();
                //     fparams_name.push(param_name.clone());
                // }
                func_params.params
                    .iter()
                    .map(|param| param.param_name.clone())
                    .collect()
            } else {
                vec![]
            };

        let entry_bb = ast_trans.new_basic_block(Some(format!("%entry_{}", self.ident)));
        ast_trans.extend_bb(entry_bb);
        let func_data = ast_trans.get_func_data();

        let fparams_val: Vec<Value> = func_data.params()
            .to_vec();

        // for (param_name, param_val) in fparams_name.into_iter().zip(fparams_val.into_iter()) {
        //     let temp_param = ast_trans.new_alloc(Type::get(TypeKind::Int32));
        //     ast_trans.set_value_name(temp_param, Some(format!("%{}", &param_name)));
        //     ast_trans.new_store(param_val, temp_param);
        //     ast_trans.bind(param_name, BindingItem::VarInt(temp_param));
        // }

        fparams_name.into_iter()
            .zip(fparams_val.into_iter())
            .for_each( |(param_name, param_val)| {
                let temp_param = ast_trans.new_alloc(Type::get(TypeKind::Int32));
                ast_trans.set_value_name(temp_param, Some(format!("%{}", &param_name)));
                ast_trans.new_store(param_val, temp_param);
                ast_trans.bind(param_name, BindingItem::VarInt(temp_param));
            });
        
        ast_trans.enter_scope();
        self.block.accept(ast_trans);
        if !ast_trans.is_cur_bb_terminate() {
            ast_trans.new_ret(None);
        }
        ast_trans.exit_scope();
        None
    }
}
#[derive(Debug,Clone)]
pub enum FuncType {
    Int,
    Void,
}

impl std::convert::From<CompType> for FuncType {
    fn from(comp_type: CompType) -> Self {
        match comp_type {
            CompType::Int => FuncType::Int,
            CompType::Void => FuncType::Void,
        }
    }
}

// FuncFParams ::= FuncFParam {"," FuncFParam};
#[derive(Debug,Clone)]
pub struct FuncFParams {
    pub params: Vec<FuncFParam>,
}

// FuncFParam  ::= BType IDENT;
#[derive(Debug,Clone)]
pub struct FuncFParam {
    pub param_type: BType,
    pub param_name: String,
}

// FuncRParams ::= Exp {"," Exp};
#[derive(Debug,Clone)]
pub struct FuncRParams {
    pub params: Vec<Exp>,
}
impl Visitable for FuncRParams {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        None
    }
}

// Block         ::= "{" {BlockItem} "}";
#[derive(Debug,Clone)]
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
                        },
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
#[derive(Debug,Clone)]
pub enum Stmt {
    LValExp(LVal, Exp),
    OptExp(Option<Exp>),
    Block(Block),
    RetExp(Option<Exp>),
    IfStmt(Exp, Box<Stmt>, Option<Box<Stmt>>),
    WhileStmt(Exp, Box<Stmt>),
    ContStmt,
    BreakStmt,
}

pub trait Visitable {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value>;
}
pub trait ConstValue {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32;
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
                Self::RetExp(ret_exp) => {
                    let res = match ret_exp {
                        None => None,
                        Some(exp) => exp.accept(ast_trans),
                    };
                    let ret = ast_trans.new_ret(res);
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

                    ast_trans.enter_loop(entry_bb, end_bb);
                    stmt.accept(ast_trans);
                    if !ast_trans.is_cur_bb_terminate() {
                        ast_trans.new_jump(entry_bb);
                    }
                    ast_trans.exit_loop();

                    ast_trans.exit_scope();

                    ast_trans.extend_bb(end_bb);
                    None
                },
                Self::ContStmt => {
                    let entry_bb = ast_trans.cur_loop().entry_bb;
                    ast_trans.new_jump(entry_bb);
                    None
                },
                Self::BreakStmt => {
                    let end_bb = ast_trans.cur_loop().end_bb;
                    if !ast_trans.is_cur_bb_terminate() {
                        ast_trans.new_jump(end_bb);
                    }
                    None
                },
            }
        }
    }
}

#[derive(Debug,Clone)]
pub enum Number {
    INT_CONST(i32),
}

impl ConstValue for Number {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            Number::INT_CONST(num) => {
                *num
            },
        }
    }
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

#[derive(Debug,Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}


#[derive(Debug,Clone)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    UnaryOpExp(UnaryOp, Box<UnaryExp>),
    FuncCall(String, Option<FuncRParams>),
}

impl ConstValue for UnaryExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.const_eval(ast_trans),
            UnaryExp::UnaryOpExp(unary_op, unary_exp) => {
                match unary_op {
                    UnaryOp::Plus => unary_exp.const_eval(ast_trans),
                    UnaryOp::Minus => {
                        let lhs = 0;
                        let rhs = unary_exp.const_eval(ast_trans);
                        ast_trans.const_binary(BinaryOp::Sub, lhs, rhs)
                    },
                    UnaryOp::Not => {
                        let rhs = 0;
                        let lhs = unary_exp.const_eval(ast_trans);
                        ast_trans.const_binary(BinaryOp::Eq, lhs, rhs)
                    },
                }
            },
            UnaryExp::FuncCall(ident, opt_params) => {
                panic!("Const can't be assigned with func call.");
                0
            },
        }
    }
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
            UnaryExp::FuncCall(ident, opt_params) => {
                let func_bind = ast_trans.lookup(ident).unwrap().clone();
                // let mut real_params: Vec<Value> = Vec::new();
                // if let Some(params) = opt_params {
                //     for param_ref in params.params.iter() {
                //         let param = param_ref.clone();
                //         let param_val = param.accept(ast_trans).unwrap();
                //         real_params.push(param_val);
                //     }
                // }
                let real_params: Vec<Value> = 
                if let Some(params) = opt_params {
                    params.params
                        .clone()
                        .into_iter()
                        .map(|param| {
                            param.accept(ast_trans).unwrap()
                        })
                        .collect()
                } else { vec![]};
                let func = match func_bind {
                    BindingItem::Func(f) => f,
                    _ => panic!("{} is not a func.", ident),
                };
                let call_val = ast_trans.new_call(func, real_params);
                Some(call_val)
            },
        }
    }
}

// PrimaryExp    ::= "(" Exp ")" | LVal | Number;
#[derive(Debug,Clone)]
pub enum PrimaryExp {
    ParenthesizedExp(Exp),
    LVal(LVal),
    Number(Number),
}

impl ConstValue for PrimaryExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            PrimaryExp::ParenthesizedExp(exp) => exp.const_eval(ast_trans),
            PrimaryExp::LVal(lval) => {
                lval.const_eval(ast_trans)
            },
            PrimaryExp::Number(num) => num.const_eval(ast_trans),
        }
    }
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
                    ValueKind::FuncArgRef(arg) => Some(val),
                    ValueKind::GlobalAlloc(global_alloc) => {
                        let load_val = ast_trans.new_load(val);
                        Some(load_val)
                    },
                    _ => unimplemented!("Unimplement LVal kind."),
                }
            },
            PrimaryExp::Number(num) => num.accept(ast_trans),
        }
    }
}

#[derive(Debug,Clone)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

impl Visitable for Exp {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        self.lor_exp.accept(ast_trans)
    }
}

impl ConstValue for Exp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        self.lor_exp.const_eval(ast_trans)
    }
}

#[derive(Debug,Clone)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
} 

#[derive(Debug,Clone)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulOpExp(Box<MulExp>, MulOp, UnaryExp),
}

impl ConstValue for MulExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.const_eval(ast_trans),
            MulExp::MulOpExp(mul_exp, mul_op, unary_exp) => {
                let lhs = mul_exp.const_eval(ast_trans);
                let rhs = unary_exp.const_eval(ast_trans);
                match mul_op {
                    MulOp::Mul => ast_trans.const_binary(BinaryOp::Mul, lhs, rhs),
                    MulOp::Div => ast_trans.const_binary(BinaryOp::Div, lhs, rhs),
                    MulOp::Mod => ast_trans.const_binary(BinaryOp::Mod, lhs, rhs),
                }
            }
        }
    }
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

#[derive(Debug,Clone)]
pub enum AddOp {
    Add,
    Sub,
} 
#[derive(Debug,Clone)]
pub enum AddExp {
    MulExp(MulExp),
    AddOpExp(Box<AddExp>, AddOp, MulExp),
}

impl ConstValue for AddExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.const_eval(ast_trans),
            AddExp::AddOpExp(add_exp, add_op, mul_exp) => {
                let lhs = add_exp.const_eval(ast_trans);
                let rhs = mul_exp.const_eval(ast_trans);
                match add_op {
                    AddOp::Add => ast_trans.const_binary(BinaryOp::Add, lhs, rhs),
                    AddOp::Sub => ast_trans.const_binary(BinaryOp::Sub, lhs, rhs),
                }
            }
        }
    }
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
#[derive(Debug,Clone)]
pub enum RelExp {
    AddExp(AddExp),
    RelOpExp(Box<RelExp>, RelOp, AddExp),
}

impl ConstValue for RelExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            RelExp::AddExp(add_exp) => add_exp.const_eval(ast_trans),
            RelExp::RelOpExp(rel_exp, rel_op, add_exp) => {
                let lhs_num = rel_exp.const_eval(ast_trans);
                let rhs_num = add_exp.const_eval(ast_trans);

                let val_num = match rel_op {
                    RelOp::Gt => (lhs_num > rhs_num) as i32,
                    RelOp::Lt => (lhs_num < rhs_num) as i32,
                    RelOp::Ge => (lhs_num >= rhs_num) as i32,
                    RelOp::Le => (lhs_num <= rhs_num) as i32,
                };
                val_num
            }
        }
    }
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

#[derive(Debug,Clone)]
pub enum RelOp {
    Lt,
    Le,
    Gt,
    Ge,
}

// EqExp       ::= RelExp | EqExp ("==" | "!=") RelExp;

#[derive(Debug,Clone)]
pub enum EqExp {
    RelExp(RelExp),
    EqOpExp(Box<EqExp>, EqOp, RelExp),
}

impl ConstValue for EqExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.const_eval(ast_trans),
            EqExp::EqOpExp(eq_exp, eq_op, rel_exp) => {
                let lhs_num = eq_exp.const_eval(ast_trans);
                let rhs_num = rel_exp.const_eval(ast_trans);
                let val_num = match eq_op {
                    EqOp::Eq => (lhs_num == rhs_num) as i32,
                    EqOp::NotEq => (lhs_num != rhs_num) as i32,
                };
                val_num
            }
        }
    }
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
#[derive(Debug,Clone)]
pub enum EqOp {
    Eq,
    NotEq,
} 

// LAndExp     ::= EqExp | LAndExp "&&" EqExp;

#[derive(Debug,Clone)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndOpExp(Box<LAndExp>, EqExp),
}

impl ConstValue for LAndExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.const_eval(ast_trans),
            LAndExp::LAndOpExp(land_exp, eq_exp) => {
                let lhs_num = land_exp.const_eval(ast_trans);
                let rhs_num = eq_exp.const_eval(ast_trans);
                (lhs_num != 0 && rhs_num != 0) as i32
            },
        }
    }
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
#[derive(Debug,Clone)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOpExp(Box<LOrExp>, LAndExp),
}

impl ConstValue for LOrExp {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.const_eval(ast_trans),
            LOrExp::LOrOpExp(lor_exp, land_exp) => {
                let lhs_num = lor_exp.const_eval(ast_trans);
                let rhs_num = land_exp.const_eval(ast_trans);
                (lhs_num != 0 || rhs_num != 0) as i32
            },
        }
    }
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
#[derive(Debug,Clone)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

// ConstDecl     ::= "const" BType ConstDef {"," ConstDef} ";";
#[derive(Debug,Clone)]
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
#[derive(Debug,Clone)]
pub enum BType {
    Int,
}
impl std::convert::From<CompType> for BType {
    fn from(comp_type: CompType) -> Self {
        match comp_type {
            CompType::Int => BType::Int,
            _ => panic!("Type conversion failed: BType only supports 'Int'"),
        }
    }
}

// ConstDef      ::= IDENT "=" ConstInitVal;
#[derive(Debug,Clone)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

impl ConstDef {
    fn to_binding(&self, ast_trans: &mut AstTrans) -> (String, i32) {
        // let val = self.const_init_val.const_exp.exp.accept(ast_trans).unwrap();
        let val = self.const_init_val.const_exp.exp.const_eval(ast_trans);
        // assert!(ast_trans.get_value_kind(val).is_const());
        (self.ident.clone(), val)
    }
}

// ConstInitVal  ::= ConstExp;
#[derive(Debug,Clone)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}


// BlockItem     ::= Decl | Stmt;
#[derive(Debug,Clone)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}


// LVal          ::= IDENT;
#[derive(Debug,Clone)]
pub enum LVal {
    IDENT(String),
}
impl ConstValue for LVal {
    fn const_eval(&self, ast_trans: &mut AstTrans) -> i32 {
        match self {
            LVal::IDENT(ident) => { 
                if let Some(binding) = ast_trans.lookup(ident) {
                    match binding {
                        BindingItem::ConstInt(val) => {
                            *val
                        },
                        _ => panic!("Not a Const Value Binding!"),
                    }
                } else {
                    panic!("undefined name.");
                }
            },
            _ => unimplemented!("Unimplement LVal."),
        }
    }
}
impl Visitable for LVal {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        match self {
            LVal::IDENT(ident) => { 
                let binding = ast_trans.lookup(ident).unwrap().clone();
                    match binding {
                        BindingItem::ConstInt(val) => {
                            Some(ast_trans.new_integer(val))
                        },
                        BindingItem::VarInt(val) => {
                            Some(val)
                        },
                        _ => unimplemented!("Unimplement BindingItem type."),
                    }
            },
            _ => unimplemented!("Unimplement LVal."),
        }
    }
}
// ConstExp      ::= Exp;
#[derive(Debug,Clone)]
pub struct ConstExp {
    pub exp: Exp,
}

// InitVal       ::= Exp;
#[derive(Debug,Clone)]
pub struct InitVal {
    pub exp: Exp,
}
impl Visitable for InitVal {
    fn accept(&self, ast_trans: &mut AstTrans) -> Option<Value> {
        self.exp.accept(ast_trans)
    }
}

// VarDef        ::= IDENT | IDENT "=" InitVal;
#[derive(Debug,Clone)]
pub enum VarDef {
    IDENT(String),
    IDENTInitVal(String, InitVal),
}

// VarDecl       ::= BType VarDef {"," VarDef} ";";
#[derive(Debug,Clone)]
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
#[derive(Debug,Clone)]
pub enum BindingItem {
    ConstInt(i32),
    VarInt(Value),
    Func(Function),
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
pub struct LoopTarget {
    pub entry_bb: BasicBlock,
    pub end_bb: BasicBlock,
}

impl LoopTarget {
    pub fn new(entry_bb: BasicBlock, end_bb: BasicBlock) -> LoopTarget {
        LoopTarget {
            entry_bb,
            end_bb,
        }
    }
}
pub struct LoopStack {
    pub stack: Vec<LoopTarget>,
}
impl LoopStack {
    pub fn new() -> LoopStack {
        LoopStack {
            stack: Vec::new(),
        }
    }
    pub fn push_loop(&mut self, entry_bb: BasicBlock, exit_bb: BasicBlock) {
        self.stack.push(LoopTarget::new(entry_bb, exit_bb));
    }
    pub fn pop_loop(&mut self) {
        self.stack.pop();
    }
    pub fn top_loop(&mut self) -> &LoopTarget {
        self.stack.last().unwrap()
    }
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
    pub loop_manager: LoopStack,
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
            loop_manager: LoopStack::new(),
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
    pub fn set_global_value_name(&mut self, val: Value, name: Option<String>) {
        self.koopa_program.set_value_name(val, name);
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
    // Ungly implement QAQ.
    // TODO: How to refine?
    pub fn get_value_kind(&mut self, val: Value) -> ValueKind {
        if val.is_global() {
            self.koopa_program.borrow_value(val).clone().kind().clone()
        } else {
            let val_data = self.get_value_data(val);
            val_data.kind().clone()
        }
    }
    // pub fn get_const(&mut self, val: Value) -> i32 {
    //     if val.is_global() {
    //         match self.koopa_program.borrow_value(val).kind() {
    //             ValueKind::Integer(int) => int.value(),
    //             _ => panic!("Not a Integer, maybe not implement."),
    //         }
    //     } else {
    //         match self.get_value_data(val).kind() {
    //             ValueKind::Integer(int) => int.value(),
    //             _ => panic!("Not a Integer, maybe not implement."),
    //         }
    //     }
    // }
    pub fn new_func(&mut self, func_name: String, func_params_ty: Vec<(Option<String>, Type)>, func_ret_ty: Type) -> Function {
        let func = self.koopa_program.new_func(FunctionData::with_param_names(func_name, func_params_ty, func_ret_ty));
        // self.cur_ctx.set_func(func);
        func
    }
    pub fn new_func_decl(&mut self, func_name: String, func_params_ty: Vec<Type>, func_ret_ty: Type) -> Function {
        let bind_name = func_name[1..].to_string();
        let func = self.koopa_program.new_func(FunctionData::new_decl(func_name, func_params_ty, func_ret_ty));
        self.bind(bind_name, BindingItem::Func(func));
        // self.cur_ctx.set_func(func);
        func
    }
    pub fn set_func(&mut self, func: Function) {
        self.cur_ctx.set_func(func);
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
    pub fn const_binary(&mut self, bin_op: BinaryOp, lhs_num: i32, rhs_num: i32) -> i32 {
        // assert!(self.get_value_kind(lhs).is_const() && self.get_value_kind(rhs).is_const());
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
        val_num
    }
    pub fn new_binary(&mut self, bin_op: BinaryOp, lhs: Value, rhs: Value) -> Value {
        let func_data = self.get_func_data_mut();
        let bin_val = func_data.dfg_mut().new_value().binary(bin_op, lhs, rhs);
        self.extend_inst(bin_val);
        bin_val
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
    pub fn new_global_alloc(&mut self, init_val: Value) -> Value {
        self.koopa_program.new_value().global_alloc(init_val)
    }
    pub fn new_call(&mut self, func: Function, params: Vec<Value>) -> Value {
        let func_data = self.get_func_data_mut();
        let call_val = func_data.dfg_mut().new_value().call(func, params);
        self.extend_inst(call_val);
        call_val
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
        self.scope_manager.insert_binding(&name, binding);
    }
    pub fn lookup(&mut self, name: &String) -> Option<&BindingItem> {
        self.scope_manager.get_binding(name)
    }
    pub fn next_uid(&mut self) -> i32 {
        self.scope_manager.new_uid()
    }
    pub fn enter_loop(&mut self, loop_enter: BasicBlock, loop_end: BasicBlock)  {
        self.loop_manager.push_loop(loop_enter, loop_end);
    }
    pub fn exit_loop(&mut self)  {
        self.loop_manager.pop_loop();
    }
    pub fn cur_loop(&mut self) -> &LoopTarget {
        self.loop_manager.top_loop()
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