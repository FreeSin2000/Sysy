use koopa::ir::*;
use koopa::ir::builder_traits::*;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}
impl CompUnit {
    pub fn to_program(&self) -> Program {
        let mut program = Program::new();
        let _ = program.new_func(func_def.to_func_data());
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
    pub fn to_func_data(&self) -> FunctionData {
        let name = self.ident.clone();
        let params_ty = vec![];
        let ret_ty = match &self.func_type {
            Int => Type::get_i32(),
        };       
        let mut func_data = FunctionData::new(name, params_ty, ret_ty); 
        func_data.layout_mut().bbs_mut().extend(self.block.to_bbs(func_data));
        // let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        // func_data.layout_mut().bbs_mut().extend([entry]);(
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
    pub fn to_bbs(&self, func_data: &mut FunctionData) -> Vec<BasicBlock> {
        let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        self.stmt.to_stmts(func_data);
        func_data.layout_mut().bb_mut(entry).insts_mut().extend(self.stmt.to_values(func_data));
        [entry]
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}

impl Stmt {
    pub fn to_values(&self, func_data: &mut FunctionData) -> Vec<Value> {
        let zero = fib_data.dfg_mut().new_value().integer(0);
        let ret = fib_data.dfg_mut().new_value().ret(Some(zero));
        [ret];
    }
}
