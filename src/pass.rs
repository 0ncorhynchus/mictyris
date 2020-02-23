use crate::parser::*;

#[derive(Clone, Debug, PartialEq)]
pub enum AST {
    Const(Lit),
    Var(String),
    Call(Box<AST>, Vec<AST>),
    Lambda(Vec<String>, Vec<AST>, Box<AST>),
    Cond(Box<AST>, Box<AST>, Option<Box<AST>>),
    Assign(String, Box<AST>),
}

pub fn pass(expr: &Expr) -> Option<AST> {
    match expr {
        Expr::Variable(variable) => Some(AST::Var(variable.clone())),
        Expr::Literal(literal) => Some(AST::Const(literal.clone())),
        Expr::ProcCall(operator, operands) => {
            let operator = Box::new(pass(operator)?);
            let operands = operands.iter().map(pass).collect::<Option<Vec<_>>>()?;
            Some(AST::Call(operator, operands))
        }
        Expr::Lambda(formals, body) => {
            let mut commands = body.iter().map(pass).collect::<Option<Vec<_>>>()?;
            let expr = Box::new(commands.pop()?);
            Some(AST::Lambda(formals.clone(), commands, expr))
        }
        Expr::Cond(test, consequent, alternate) => {
            let test = Box::new(pass(test)?);
            let consequent = Box::new(pass(consequent)?);
            let alternate = match alternate {
                Some(alt) => Some(Box::new(pass(alt)?)),
                None => None,
            };
            Some(AST::Cond(test, consequent, alternate))
        }
        Expr::Assignment(var, expr) => {
            let expr = Box::new(pass(expr)?);
            Some(AST::Assign(var.clone(), expr))
        }
        _ => None,
    }
}
