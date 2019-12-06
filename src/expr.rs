use fnv::FnvHashMap;
use std::f64::consts;
use std::ops::Deref;
use std::rc::Rc;
use std::str::FromStr;

type ContextHashMap<K, V> = FnvHashMap<K, V>;

use shunting_yard::to_rpn;
use std;
use std::fmt;
use tokenizer::{tokenize, Token};
use Error;

/// Representation of a parsed expression.
///
/// The expression is internally stored in the [reverse Polish notation (RPN)][RPN] as a sequence
/// of `Token`s.
///
/// Methods `bind`, `bind_with_context`, `bind2`, ... can be used to create  closures from
/// the expression that then can be passed around and used as any other `Fn` closures.
///
/// ```rust
/// let func = "x^2".parse::<meval::Expr>().unwrap().bind("x").unwrap();
/// let r = Some(2.).map(func);
/// assert_eq!(r, Some(4.));
/// ```
///
/// [RPN]: https://en.wikipedia.org/wiki/Reverse_Polish_notation
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    rpn: Vec<Token>,
}

impl Expr {
    /// Evaluates the expression.
    pub fn eval(&self) -> Result<f64, Error> {
        self.eval_with_context(builtin())
    }

    /// Evaluates the expression with variables given by the argument.
    pub fn eval_with_context(&self, ctx: Context) -> Result<f64, Error> {
        use tokenizer::Operation::*;
        use tokenizer::Token::*;

        let mut stack : Vec<f64> = Vec::with_capacity(16);

        for token in &self.rpn {
            match *token {
                Var(ref n) => {
                    if let Some(v) = ctx.get_var(n) {
                        stack.push(v);
                    } else {
                        return Err(Error::UnknownVariable(n.clone()));
                    }
                }
                // Alias(ref n) => {
                //     if let Some(v) = ctx.get_aliases_values(*n) {
                //         stack.push(v);
                //     } else {
                //         return Err(Error::UnknownVariable(n.clone()));
                //     }
                // }
                Number(f) => stack.push(f),
                Binary(op) => {
                    let right = stack.pop().unwrap();
                    let left = stack.pop().unwrap();
                    let r = match op {
                        Plus => left + right,
                        Minus => left - right,
                        Times => left * right,
                        Div => left / right,
                        //Rem => left % right,
                        //Pow => left.powf(right),
                        _ => {
                            return Err(Error::EvalError(format!(
                                "Unimplemented binary operation: {:?}",
                                op
                            )));
                        }
                    };
                    stack.push(r);
                }
                Unary(op) => {
                    let x = stack.pop().unwrap();
                    let r = match op {
                        Plus => x,
                        Minus => -x,
                        // Fact => {
                        //     // Check to make sure x has no fractional component (can be converted to int without loss)
                        //     match factorial(x) {
                        //         Ok(res) => res,
                        //         Err(e) => return Err(Error::EvalError(String::from(e))),
                        //     }
                        // }
                        _ => {
                            return Err(Error::EvalError(format!(
                                "Unimplemented unary operation: {:?}",
                                op
                            )));
                        }
                    };
                    stack.push(r);
                }
                Func(ref n, Some(i)) => {
                    if stack.len() < i {
                        return Err(Error::EvalError(format!(
                            "eval: stack does not have enough arguments for function token \
                             {:?}",
                            token
                        )));
                    }
                    match ctx.eval_func(n, &stack[stack.len() - i..]) {
                        Ok(r) => {
                            let nl = stack.len() - i;
                            stack.truncate(nl);
                            stack.push(r);
                        }
                        Err(e) => return Err(Error::Function(n.to_owned(), e)),
                    }
                }
                _ => return Err(Error::EvalError(format!("Unrecognized token: {:?}", token))),
            }
        }

        let r = stack.pop().expect("Stack is empty, this is impossible.");
        if !stack.is_empty() {
            return Err(Error::EvalError(format!(
                "There are still {} items on the stack.",
                stack.len()
            )));
        }
        Ok(r)
    }

    // /// Checks that the value of every variable in the expression is specified by the context `ctx`.
    // ///
    // /// # Failure
    // ///
    // /// Returns `Err` if a missing variable is detected.
    // fn check_context(&self, ctx: Context) -> Result<(), Error> {
    //     for t in &self.rpn {
    //         match *t {
    //             Token::Var(ref name) => {
    //                 if ctx.get_var(name).is_none() {
    //                     return Err(Error::UnknownVariable(name.clone()));
    //                 }
    //             }
    //             Token::Alias(ref id) => {
    //                 if ctx.get_aliases_values(*id).is_none() {
    //                     return Err(Error::UnknownAlias(id.to_string()));
    //                 }
    //             }
    //             Token::Func(ref name, Some(i)) => {
    //                 let v = vec![0.; i];
    //                 if let Err(e) = ctx.eval_func(name, &v) {
    //                     return Err(Error::Function(name.to_owned(), e));
    //                 }
    //             }
    //             Token::Func(_, None) => {
    //                 return Err(Error::EvalError(format!(
    //                     "expr::check_context: Unexpected token: {:?}",
    //                     *t
    //                 )));
    //             }
    //             Token::LParen
    //             | Token::RParen
    //             | Token::Binary(_)
    //             | Token::Unary(_)
    //             | Token::Comma
    //             | Token::Number(_) => {}
    //         }
    //     }
    //     Ok(())
    // }
    
}

/// Evaluates a string with built-in constants and functions.
pub fn eval_str<S: AsRef<str>>(expr: S) -> Result<f64, Error> {
    let expr = Expr::from_str(expr.as_ref())?;
    expr.eval_with_context(builtin())
}

impl FromStr for Expr {
    type Err = Error;
    /// Constructs an expression by parsing a string.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tokens = tokenize(s)?;
        let rpn = to_rpn(&tokens)?;

        Ok(Expr { rpn: rpn })
    }
}

impl Deref for Expr {
    type Target = [Token];

    fn deref(&self) -> &[Token] {
        &self.rpn
    }
}

/// Function evaluation error.
#[derive(Debug, Clone, PartialEq)]
pub enum FuncEvalError {
    TooFewArguments,
    TooManyArguments,
    NumberArgs(usize),
    UnknownFunction,
}

impl fmt::Display for FuncEvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            FuncEvalError::UnknownFunction => write!(f, "Unknown function"),
            FuncEvalError::NumberArgs(i) => write!(f, "Expected {} arguments", i),
            FuncEvalError::TooFewArguments => write!(f, "Too few arguments"),
            FuncEvalError::TooManyArguments => write!(f, "Too many arguments"),
        }
    }
}

impl std::error::Error for FuncEvalError {
    fn description(&self) -> &str {
        match *self {
            FuncEvalError::UnknownFunction => "unknown function",
            FuncEvalError::NumberArgs(_) => "wrong number of function arguments",
            FuncEvalError::TooFewArguments => "too few function arguments",
            FuncEvalError::TooManyArguments => "too many function arguments",
        }
    }
}

#[doc(hidden)]
pub fn max_array(xs: &[f64]) -> f64 {
    xs.iter().fold(::std::f64::NEG_INFINITY, |m, &x| m.max(x))
}

#[doc(hidden)]
pub fn min_array(xs: &[f64]) -> f64 {
    xs.iter().fold(::std::f64::INFINITY, |m, &x| m.min(x))
}

/// Returns the built-in constants and functions in a form that can be used as a `ContextProvider`.
#[doc(hidden)]
pub fn builtin<'a>() -> Context<'a> {
    // TODO: cache this (lazy_static)
    Context::new()
}

/// A structure for storing variables/constants and functions to be used in an expression.
///
/// # Example
///
/// ```rust
/// use meval::{eval_str_with_context, Context};
///
/// let mut ctx = Context::new(); // builtins
/// ctx.var("x", 3.)
///    .func("f", |x| 2. * x)
///    .funcn("sum", |xs| xs.iter().sum(), ..);
///
/// assert_eq!(eval_str_with_context("pi + sum(1., 2.) + f(x)", &ctx),
///            Ok(std::f64::consts::PI + 1. + 2. + 2. * 3.));
/// ```
#[derive(Clone)]
pub struct Context<'a> {
    vars: ContextHashMap<String, f64>,
    aliases: ContextHashMap<String, Vec<f64>>,
    funcs: ContextHashMap<String, GuardedFunc<'a>>,
}

impl<'a> Context<'a> {
    /// Creates a context with built-in constants and functions.
    pub fn new() -> Context<'a> {
        thread_local!(static DEFAULT_CONTEXT: Context<'static> = {
            let mut ctx = Context::empty();
            ctx.var("pi", consts::PI);
            ctx.var("e", consts::E);

            ctx.func("sqrt", f64::sqrt);
            ctx.func("exp", f64::exp);
            ctx.func("ln", f64::ln);
            ctx.func("log10", f64::log10);
            ctx.func("abs", f64::abs);
            ctx.func("sin", f64::sin);
            ctx.func("cos", f64::cos);
            ctx.func("tan", f64::tan);
            ctx.func("asin", f64::asin);
            ctx.func("acos", f64::acos);
            ctx.func("atan", f64::atan);
            ctx.func("sinh", f64::sinh);
            ctx.func("cosh", f64::cosh);
            ctx.func("tanh", f64::tanh);
            ctx.func("asinh", f64::asinh);
            ctx.func("acosh", f64::acosh);
            ctx.func("atanh", f64::atanh);
            ctx.func("floor", f64::floor);
            ctx.func("ceil", f64::ceil);
            ctx.func("round", f64::round);
            ctx.func("signum", f64::signum);
            ctx.func2("atan2", f64::atan2);
            ctx
        });

        DEFAULT_CONTEXT.with(|ctx| ctx.clone())
    }

    /// Creates an empty contexts.
    pub fn empty() -> Context<'a> {
        Context {
            vars: ContextHashMap::default(),
            aliases: ContextHashMap::default(),
            funcs: ContextHashMap::default(),
        }
    }

    fn get_var(&self, name: &str) -> Option<f64> {
        self.vars.get(name).cloned()
    }

    fn get_aliases_values(&self, id: i32) -> Option<Vec<f64>> {
        self.aliases.get(&id.to_string()).cloned()
    }

    fn eval_func(&self, name: &str, args: &[f64]) -> Result<f64, FuncEvalError> {
        self.funcs
            .get(name)
            .map_or(Err(FuncEvalError::UnknownFunction), |f| f(args))
    }

    /// Adds a new variable/constant.
    pub fn var<S: Into<String>>(&mut self, var: S, value: f64) -> &mut Self {
        self.vars.insert(var.into(), value);
        self
    }

    /// Adds a new function of one argument.
    pub fn func<S, F>(&mut self, name: S, func: F) -> &mut Self
    where
        S: Into<String>,
        F: Fn(f64) -> f64 + 'a,
    {
        self.funcs.insert(
            name.into(),
            Rc::new(move |args: &[f64]| {
                if args.len() == 1 {
                    Ok(func(args[0]))
                } else {
                    Err(FuncEvalError::NumberArgs(1))
                }
            }),
        );
        self
    }

    /// Adds a new function of two arguments.
    pub fn func2<S, F>(&mut self, name: S, func: F) -> &mut Self
    where
        S: Into<String>,
        F: Fn(f64, f64) -> f64 + 'a,
    {
        self.funcs.insert(
            name.into(),
            Rc::new(move |args: &[f64]| {
                if args.len() == 2 {
                    Ok(func(args[0], args[1]))
                } else {
                    Err(FuncEvalError::NumberArgs(2))
                }
            }),
        );
        self
    }

    /// Adds a new function of three arguments.
    pub fn func3<S, F>(&mut self, name: S, func: F) -> &mut Self
    where
        S: Into<String>,
        F: Fn(f64, f64, f64) -> f64 + 'a,
    {
        self.funcs.insert(
            name.into(),
            Rc::new(move |args: &[f64]| {
                if args.len() == 3 {
                    Ok(func(args[0], args[1], args[2]))
                } else {
                    Err(FuncEvalError::NumberArgs(3))
                }
            }),
        );
        self
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        Context::new()
    }
}

type GuardedFunc<'a> = Rc<dyn Fn(&[f64]) -> Result<f64, FuncEvalError> + 'a>;


#[cfg(test)]
mod tests {
    use super::*;
    use Error;

    #[test]
    fn test_eval() {
        assert_eq!(eval_str("2 + 3"), Ok(5.));
        assert_eq!(eval_str("2 + (3 + 4)"), Ok(9.));
        assert_eq!(eval_str("-2^(4 - 3) * (3 + 4)"), Ok(-14.));
        assert_eq!(eval_str("-2*3! + 1"), Ok(-11.));
        assert_eq!(eval_str("-171!"), Ok(std::f64::NEG_INFINITY));
        assert_eq!(eval_str("150!/148!"), Ok(22350.));
        assert_eq!(eval_str("a + 3"), Err(Error::UnknownVariable("a".into())));
        assert_eq!(eval_str("round(sin (pi) * cos(0))"), Ok(0.));
        assert_eq!(eval_str("round( sqrt(3^2 + 4^2)) "), Ok(5.));
        assert_eq!(eval_str("max(1.)"), Ok(1.));
        assert_eq!(eval_str("max(1., 2., -1)"), Ok(2.));
        assert_eq!(eval_str("min(1., 2., -1)"), Ok(-1.));
        assert_eq!(
            eval_str("sin(1.) + cos(2.)"),
            Ok((1f64).sin() + (2f64).cos())
        );
        assert_eq!(eval_str("10 % 9"), Ok(10f64 % 9f64));

        match eval_str("0.5!") {
            Err(Error::EvalError(_)) => {}
            _ => panic!("Cannot evaluate factorial of non-integer"),
        }
    }

    #[test]
    fn test_builtins() {
        assert_eq!(eval_str("atan2(1.,2.)"), Ok((1f64).atan2(2.)));
    }
}
