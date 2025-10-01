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
    pub unary_exp: Box<UnaryExp>,
}

impl DeriveValue for Exp {
    fn to_value(&self, func_data: &mut FunctionData) -> (Value, Vec<Value>) {
        return self.unary_exp.to_value(func_data);
    }
}