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
        let values = self.stmt.to_values(func_data);
        func_data.layout_mut().bb_mut(entry_bb).insts_mut().extend(values);
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}

impl Stmt {
    pub fn to_values(&self, func_data: &mut FunctionData) -> Vec<Value> {
        let res = func_data.dfg_mut().new_value().integer(self.num);
        let ret = func_data.dfg_mut().new_value().ret(Some(res));
        vec![ret]
    }
}
