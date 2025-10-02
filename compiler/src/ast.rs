use koopa::ir::*;
use koopa::ir::builder_traits::*;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}
impl CompUnit {
    pub fn to_program(&self) -> Program {
        let mut program = Program::new();
        self.func_def.build_func(&mut program);
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
    pub fn build_func(&self, program: &mut Program) {
        let name = String::from("@") + &self.ident;
        let params_ty = vec![];
        let ret_ty = match &self.func_type {
            FuncType::Int => Type::get_i32(),
        };       
        let main_func = program.new_func(FunctionData::with_param_names(name.into(), params_ty, ret_ty));
        self.block.build_bbs(program.func_mut(main_func));
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}


#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

impl Block {
    pub fn build_bbs(&self, func_data: &mut FunctionData) {
        let entry_bb = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry_bb]);
        let (_, insts) = self.stmt.to_value(func_data);
        func_data.layout_mut().bb_mut(entry_bb).insts_mut().extend(insts);
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

pub trait DeriveValue {
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>);
}
impl DeriveValue for Stmt {
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        let (res, mut insts) = self.exp.to_value(func_data);
        let ret = func_data.dfg_mut().new_value().ret(Some(res));
        insts.push(ret);
        (ret, insts)
    }
}

#[derive(Debug)]
pub enum Number {
    INT_CONST(i32),
}

impl DeriveValue for Number {
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            UnaryExp::PrimaryExp(primary_exp) => primary_exp.to_value(func_data),
            UnaryExp::UnaryOpExp(unary_op, unary_exp) => {
                match unary_op {
                    UnaryOp::Plus => unary_exp.to_value(func_data),
                    UnaryOp::Minus => {
                        let lhs = func_data.dfg_mut().new_value().integer(0);
                        let (rhs, mut insts) = unary_exp.to_value(func_data);
                        let val = func_data.dfg_mut().new_value().binary(BinaryOp::Sub, lhs, rhs);
                        insts.push(val);
                        (val, insts)
                    },
                    UnaryOp::Not => {
                        let rhs  = func_data.dfg_mut().new_value().integer(0);
                        let (lhs, mut insts) = unary_exp.to_value(func_data);
                        let val = func_data.dfg_mut().new_value().binary(BinaryOp::Eq, lhs, rhs);
                        let mut res_insts: Vec<Value> = Vec::new();
                        insts.push(val);
                        (val, insts)
                    },
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum PrimaryExp {
    ParenthesizedExp(Exp),
    Number(Number),
}

impl DeriveValue for PrimaryExp {
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>){
        match self {
            PrimaryExp::ParenthesizedExp(exp) => exp.to_value(func_data),
            PrimaryExp::Number(num) => num.to_value(func_data),
        }
    }
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: Box<LOrExp>,
}

impl DeriveValue for Exp {
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        return self.lor_exp.to_value(func_data);
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.to_value(func_data),
            MulExp::MulOpExp(mul_exp, mul_op, unary_exp) => {
                let (lhs, mut l_insts) = mul_exp.to_value(func_data);
                let (rhs, r_insts) = unary_exp.to_value(func_data);
                let val = match mul_op {
                    MulOp::Mul => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Mul, lhs, rhs)
                    },
                    MulOp::Div => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Div, lhs, rhs)
                    },
                    MulOp::Mod => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Mod, lhs, rhs)
                    },
                };
                l_insts.extend(r_insts);
                l_insts.push(val);
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.to_value(func_data),
            AddExp::AddOpExp(add_exp, add_op, mul_exp) => {
                let (lhs, mut l_insts) = add_exp.to_value(func_data);
                let (rhs, r_insts) = mul_exp.to_value(func_data);
                let val = match add_op {
                    AddOp::Add => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Add, lhs, rhs)
                    },
                    AddOp::Sub => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Sub, lhs, rhs)
                    },
                };
                l_insts.extend(r_insts);
                l_insts.push(val);
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            RelExp::AddExp(add_exp) => add_exp.to_value(func_data),
            RelExp::RelOpExp(rel_exp, rel_op, add_exp) => {
                let (lhs, mut l_insts) = rel_exp.to_value(func_data);
                let (rhs, r_insts) = add_exp.to_value(func_data);
                let val = match rel_op {
                    RelOp::Lt => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Lt, lhs, rhs)
                    },
                    RelOp::Gt => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Gt, lhs, rhs)
                    },
                    RelOp::Le => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Le, lhs, rhs)
                    },
                    RelOp::Ge => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Ge, lhs, rhs)
                    },
                };
                l_insts.extend(r_insts);
                l_insts.push(val);
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.to_value(func_data),
            EqExp::EqOpExp(eq_exp, eq_op, rel_exp) => {
                let (lhs, mut l_insts) = eq_exp.to_value(func_data);
                let (rhs, r_insts) = rel_exp.to_value(func_data);
                let val = match eq_op {
                    EqOp::Eq => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::Eq, lhs, rhs)
                    },
                    EqOp::NotEq => {
                        func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, lhs, rhs)
                    },
                };
                l_insts.extend(r_insts);
                l_insts.push(val);
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.to_value(func_data),
            LAndExp::LAndOpExp(land_exp, eq_exp) => {
                let (lhs, mut l_insts) = land_exp.to_value(func_data);
                let (rhs, r_insts) = eq_exp.to_value(func_data);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let l_lhs = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, lhs, zero); 
                let l_rhs = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, rhs, zero); 
                let val = func_data.dfg_mut().new_value().binary(BinaryOp::And, l_lhs, l_rhs);
                l_insts.extend(r_insts);
                l_insts.push(l_lhs);
                l_insts.push(l_rhs);
                l_insts.push(val);
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
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        match self {
            LOrExp::LAndExp(land_exp) => land_exp.to_value(func_data),
            LOrExp::LOrOpExp(lor_exp, land_exp) => {
                let (lhs, mut l_insts) = lor_exp.to_value(func_data);
                let (rhs, r_insts) = land_exp.to_value(func_data);
                let val = func_data.dfg_mut().new_value().binary(BinaryOp::Or, lhs, rhs);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let l_val = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, val, zero); 
                l_insts.extend(r_insts);
                l_insts.push(val);
                l_insts.push(l_val);
                (l_val, l_insts)
            }
        }
    }
}

pub struct AstTrans;

impl AstTrans {
    pub fn to_koopa(&self, comp_unit: &CompUnit) -> Program {
        comp_unit.to_program()
    }
}