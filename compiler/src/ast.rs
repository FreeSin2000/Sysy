use koopa::ir::*;
use koopa::ir::builder_traits::*;
use koopa::ir::entities::*;

use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}
impl CompUnit {
    pub fn to_program(&self, ast_trans: &AstTrans) -> Program {
        let mut program = Program::new();
        self.func_def.build_func(&mut program, ast_trans);
        program
    }
}


#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    pub fn build_func(&self, program: &mut Program, ast_trans: &AstTrans) {
        let name = String::from("@") + &self.ident;
        let params_ty = vec![];
        let ret_ty = match &self.func_type {
            FuncType::Int => Type::get_i32(),
        };       
        let main_func = program.new_func(FunctionData::with_param_names(name.into(), params_ty, ret_ty));
        self.block.build_bbs(program.func_mut(main_func), ast_trans);
        let mut st = ast_trans.local_sym_tab.borrow_mut();
        st.clear();
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

impl Block {
    pub fn build_bbs(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) {
        let entry_bb = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry_bb]);
        for block_item in &self.block_items {
            match block_item {
                BlockItem::Decl(decl) => {
                    match decl {
                        Decl::ConstDecl(const_decl) => {
                            const_decl.build_bindings(func_data, ast_trans);
                        },
                        Decl::VarDecl(var_decl) => {
                            let insts = var_decl.to_bindngs(func_data, ast_trans);
                            func_data.layout_mut().bb_mut(entry_bb).insts_mut().extend(insts);
                        }
                        _ => unimplemented!("Not implement other decl."),
                    }
                },
                BlockItem::Stmt(stmt) => {
                    let (_, insts) = stmt.to_value(func_data, ast_trans);
                    func_data.layout_mut().bb_mut(entry_bb).insts_mut().extend(insts);
                },
            };
        }
    }
}

// Stmt          ::= LVal "=" Exp ";"
//                 | "return" Exp ";";
#[derive(Debug)]
pub enum Stmt {
    LValExp(LVal, Exp),
    RetExp(Exp),
}

pub trait DeriveValue {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>);
}
impl DeriveValue for Stmt {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            Self::LValExp(lval, exp) => {
                let (l_val, mut l_insts) = lval.to_value(func_data, ast_trans);
                let (r_val, r_insts) = exp.to_value(func_data, ast_trans);
                let (s_val, s_insts) = AstTrans::build_store_op(func_data, l_val, r_val);
                l_insts.extend(r_insts);
                l_insts.extend(s_insts);
                (l_val, l_insts)
            },
            Self::RetExp(exp) => {
                let (res, mut insts) = exp.to_value(func_data, ast_trans);
                let (ret, ret_insts) = AstTrans::build_ret_op(func_data, Some(res));
                // let ret = func_data.dfg_mut().new_value().ret(Some(res));
                insts.extend(ret_insts);
                (ret, insts)
            }
        }
    }
}

#[derive(Debug)]
pub enum Number {
    INT_CONST(i32),
}

impl DeriveValue for Number {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            Number::INT_CONST(num) => {
                let val = func_data.dfg_mut().new_value().integer(*num);
                (val, vec![])
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

impl DeriveValue for UnaryExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.to_value(func_data, ast_trans),
            UnaryExp::UnaryOpExp(unary_op, unary_exp) => {
                match unary_op {
                    UnaryOp::Plus => unary_exp.to_value(func_data, ast_trans),
                    UnaryOp::Minus => {
                        let lhs = func_data.dfg_mut().new_value().integer(0);
                        let (rhs, mut insts) = unary_exp.to_value(func_data, ast_trans);
                        let (val, val_insts) = AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Sub);
                        insts.extend(val_insts);
                        (val, insts)
                    },
                    UnaryOp::Not => {
                        let rhs  = func_data.dfg_mut().new_value().integer(0);
                        let (lhs, mut insts) = unary_exp.to_value(func_data, ast_trans);
                        let (val, val_insts) = AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Eq); 
                        insts.extend(val_insts);
                        (val, insts)
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

impl DeriveValue for PrimaryExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>){
        match self {
            PrimaryExp::ParenthesizedExp(exp) => exp.to_value(func_data, ast_trans),
            PrimaryExp::LVal(lval) => {
                let (val, mut val_insts) = lval.to_value(func_data, ast_trans);
                let val_data = func_data.dfg().value(val);
                match val_data.kind() {
                    ValueKind::Integer(int) => (val, val_insts),
                    ValueKind::Binary(binary) =>(val, val_insts),
                    ValueKind::Alloc(alloc) => {
                        let (load_val, load_insts) = AstTrans::build_load_op(func_data, val);
                        val_insts.extend(load_insts);
                        (load_val, val_insts)
                    }
                    _ => unimplemented!("Unimplement LVal kind."),
                }
            },
            PrimaryExp::Number(num) => num.to_value(func_data, ast_trans),
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

impl DeriveValue for Exp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        return self.lor_exp.to_value(func_data, ast_trans);
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

impl DeriveValue for MulExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.to_value(func_data, ast_trans),
            MulExp::MulOpExp(mul_exp, mul_op, unary_exp) => {
                let (lhs, mut l_insts) = mul_exp.to_value(func_data, ast_trans);
                let (rhs, r_insts) = unary_exp.to_value(func_data, ast_trans);
                let (val, val_insts) = match mul_op {
                    MulOp::Mul => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Mul),
                    MulOp::Div => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Div),
                    MulOp::Mod => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Mod),
                };
                l_insts.extend(r_insts);
                l_insts.extend(val_insts);
                (val, l_insts)
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
impl DeriveValue for AddExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.to_value(func_data, ast_trans),
            AddExp::AddOpExp(add_exp, add_op, mul_exp) => {
                let (lhs, mut l_insts) = add_exp.to_value(func_data, ast_trans);
                let (rhs, r_insts) = mul_exp.to_value(func_data, ast_trans);
                let (val, val_insts) = match add_op {
                    AddOp::Add => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Add),
                    AddOp::Sub => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Sub),
                };
                l_insts.extend(r_insts);
                l_insts.extend(val_insts);
                (val, l_insts)
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

impl DeriveValue for RelExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            RelExp::AddExp(add_exp) => add_exp.to_value(func_data, ast_trans),
            RelExp::RelOpExp(rel_exp, rel_op, add_exp) => {
                let (lhs, mut l_insts) = rel_exp.to_value(func_data, ast_trans);
                let (rhs, r_insts) = add_exp.to_value(func_data, ast_trans);
                let (val, val_insts) = match rel_op {
                    RelOp::Gt => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Gt),
                    RelOp::Lt => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Lt),
                    RelOp::Ge => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Ge),
                    RelOp::Le => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Le),
                };
                l_insts.extend(r_insts);
                l_insts.extend(val_insts);
                (val, l_insts)
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

impl DeriveValue for EqExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.to_value(func_data, ast_trans),
            EqExp::EqOpExp(eq_exp, eq_op, rel_exp) => {
                let (lhs, mut l_insts) = eq_exp.to_value(func_data, ast_trans);
                let (rhs, r_insts) = rel_exp.to_value(func_data, ast_trans);
                let (val, val_insts) = match eq_op {
                    EqOp::Eq => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Eq),
                    EqOp::NotEq => AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::NotEq),
                };
                l_insts.extend(r_insts);
                l_insts.extend(val_insts);
                (val, l_insts)
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

impl DeriveValue for LAndExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.to_value(func_data, ast_trans),
            LAndExp::LAndOpExp(land_exp, eq_exp) => {
                let (lhs, mut l_insts) = land_exp.to_value(func_data, ast_trans);
                let (rhs, r_insts) = eq_exp.to_value(func_data, ast_trans);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let (l_lhs, ll_insts) = AstTrans::build_binary_op(func_data, lhs, zero, BinaryOp::NotEq);
                // let l_lhs = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, lhs, zero); 
                let (l_rhs, lr_insts) = AstTrans::build_binary_op(func_data, rhs, zero, BinaryOp::NotEq);
                // let l_rhs = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, rhs, zero); 
                let (val, val_insts) = AstTrans::build_binary_op(func_data, l_lhs, l_rhs, BinaryOp::And);
                // let val = func_data.dfg_mut().new_value().binary(BinaryOp::And, l_lhs, l_rhs);
                l_insts.extend(r_insts);
                l_insts.extend(ll_insts);
                l_insts.extend(lr_insts);
                l_insts.extend(val_insts);
                (val, l_insts)
            }
        }
    }
}
#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrOpExp(Box<LOrExp>, LAndExp),
}
impl DeriveValue for LOrExp {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.to_value(func_data, ast_trans),
            LOrExp::LOrOpExp(lor_exp, land_exp) => {
                let (lhs, mut l_insts) = lor_exp.to_value(func_data, ast_trans);
                let (rhs, r_insts) = land_exp.to_value(func_data, ast_trans);
                let (val, val_insts) = AstTrans::build_binary_op(func_data, lhs, rhs, BinaryOp::Or);
                // let val = func_data.dfg_mut().new_value().binary(BinaryOp::Or, lhs, rhs);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let (l_val, lv_insts) = AstTrans::build_binary_op(func_data, val, zero, BinaryOp::NotEq);
                // let l_val = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, val, zero); 
                l_insts.extend(r_insts);
                l_insts.extend(val_insts);
                l_insts.extend(lv_insts);
                (l_val, l_insts)
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
    fn build_bindings(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> () {
        for const_def in &self.const_defs {
            let (ident, val) = const_def.to_binding(func_data, ast_trans);
            match self.btype {
                BType::Int => {
                    let val_data = func_data.dfg().value(val);
                    let num = AstTrans::const_num(val_data);
                    let mut st = ast_trans.local_sym_tab.borrow_mut();
                    st.insert(ident.clone(), BindingItem::ConstInt(val, num));
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
    fn to_binding(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (String, Value) {
        let (val, _) = self.const_init_val.const_exp.exp.to_value(func_data, ast_trans);
        assert!(func_data.dfg().value(val).kind().is_const());
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
impl DeriveValue for LVal {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        match self {
            LVal::IDENT(ident) => { 
                let st = ast_trans.local_sym_tab.borrow();
                if let Some(binding) = st.get(ident) {
                    match binding {
                        BindingItem::ConstInt(val, num) => {
                            (*val, vec![])
                        },
                        BindingItem::VarInt(val) => {
                            (*val, vec![])
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
impl DeriveValue for InitVal {
    fn to_value(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> (Value, Vec<Value>) {
        self.exp.to_value(func_data, ast_trans)
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
    pub fn to_bindngs(&self, func_data: &mut FunctionData, ast_trans: &AstTrans) -> Vec<Value> {
        let mut decl_insts = Vec::new();
        for var_def in &self.var_defs {
            match var_def {
                VarDef::IDENT(ident) => {
                    let (l_val, l_insts) = AstTrans::build_alloc_op(func_data, Some(String::from("@") + ident), TypeKind::Int32);
                    let binding = BindingItem::VarInt(l_val);
                    ast_trans.insert_sym(ident, binding);
                    decl_insts.extend(l_insts);
                },
                VarDef::IDENTInitVal(ident, init_val) => {
                    let (l_val, mut l_insts) = AstTrans::build_alloc_op(func_data, Some(String::from("@") + ident), TypeKind::Int32);
                    let (r_val, r_insts) = init_val.to_value(func_data, ast_trans);
                    let (s_val, s_insts) = AstTrans::build_store_op(func_data, l_val, r_val);
                    l_insts.extend(r_insts);
                    l_insts.extend(s_insts);
                    let binding = BindingItem::VarInt(l_val);
                    ast_trans.insert_sym(ident, binding);
                    decl_insts.extend(l_insts);
                }
            }
        }
        decl_insts
    }
}

pub enum BindingItem {
    ConstInt(Value, i32),
    VarInt(Value),
}

pub struct AstTrans {
    pub local_sym_tab: RefCell<HashMap<String, BindingItem>>,
}

impl AstTrans {
    pub fn new() -> AstTrans {
        AstTrans {local_sym_tab: RefCell::new(HashMap::new())}
    }
    pub fn to_koopa(&self, comp_unit: &CompUnit) -> Program {
        comp_unit.to_program(self)
    }
    pub fn build_binary_op(func_data: &mut FunctionData, lhs: Value, rhs: Value, op: BinaryOp) -> (Value, Vec<Value>) {
        let lhs_data = func_data.dfg().value(lhs);
        let rhs_data = func_data.dfg().value(rhs);
        if lhs_data.kind().is_const() && rhs_data.kind().is_const() {
            let lhs_num = match lhs_data.kind() {
                ValueKind::Integer(num) => num.value(),
                _ => panic!("Not a number."), 
            };
            let rhs_num = match rhs_data.kind() {
                ValueKind::Integer(num) => num.value(),
                _ => panic!("Not a number!"), 
            };
            let val_num = match op {
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
            let val = func_data.dfg_mut().new_value().integer(val_num);
            (val, vec![])
        } else {
            let val = func_data.dfg_mut().new_value().binary(op, lhs, rhs);
            (val, vec![val])
        }
    }
    pub fn build_alloc_op(func_data: &mut FunctionData, ident: Option<String>, tk: TypeKind) -> (Value, Vec<Value>) {
        let a_val = func_data.dfg_mut().new_value().alloc(Type::get(tk));
        func_data.dfg_mut().set_value_name(a_val, ident);
        (a_val, vec![a_val])
    }
    pub fn build_store_op(func_data: &mut FunctionData, l_val: Value, r_val: Value) -> (Value, Vec<Value>) {
        let s_val = func_data.dfg_mut().new_value().store(r_val, l_val);
        (s_val, vec![s_val])
    }
    pub fn build_load_op(func_data: &mut FunctionData, val: Value) -> (Value, Vec<Value>) {
        let l_val = func_data.dfg_mut().new_value().load(val);
        (l_val, vec![l_val])
    }
    pub fn build_ret_op(func_data: &mut FunctionData, res: Option<Value>) -> (Value, Vec<Value>) {
        let ret_val = func_data.dfg_mut().new_value().ret(res);
        (ret_val, vec![ret_val])
    }
    pub fn const_num(val_data: &ValueData) -> i32 {
        match val_data.kind() {
            ValueKind::Integer(int) => int.value(),
            _ => panic!("Not a Integer, maybe not implement."),
        }
    }
    pub fn insert_sym(&self, name: &String, binding: BindingItem) {
        let mut st = self.local_sym_tab.borrow_mut();
        st.insert(name.clone(), binding);
    }
}
use koopa::back::KoopaGenerator;
pub fn koopa_to_string(program: & Program) -> String {
    let mut gen = KoopaGenerator::new(Vec::new());
    gen.generate_on(program).unwrap();
    let text_form_ir = std::str::from_utf8(&gen.writer()).unwrap().to_string();
    return text_form_ir;
}