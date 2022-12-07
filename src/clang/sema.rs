
use super::parse::{Node, NodeType};
use super::roundup;
use super::{Ctype, Scope, TokenType, Type, Var};

use std::collections::HashMap;
use std::mem;
use std::sync::Mutex;
use lazy_static::*;
use std::cell::{RefCell, Cell};
use std::ops::DerefMut;

macro_rules! matches(
    ($e:expr, $p:pat) => (
        match $e {
            $p => true,
            _ => false
        }
    )
);

// Quoted from 9cc
// > Semantics analyzer. This pass plays a few important roles as shown
// > below:
// >
// > - Add types to nodes. For example, a tree that represents "1+2" is
// >   typed as INT because the result type of an addition of two
// >   integers is integer.
// >
// > - Resolve variable names based on the C scope rules.
// >   Local variables are resolved to offsets from the base pointer.
// >   Global variables are resolved to their names.
// >
// > - Insert nodes to make array-to-pointer conversion explicit.
// >   Recall that, in C, "array of T" is automatically converted to
// >   "pointer to T" in most contexts.
// >
// > - Scales operands for pointer arithmetic. E.g. ptr+1 becomes ptr+4
// >   for integer and becomes ptr+8 for pointer.
// >
// > - Reject bad assignments, such as `1=2+3`.


struct SemaStates{
    globals: RefCell<Vec<Var>>,
    env: RefCell<Env>,
    strlabel: Cell<usize>,
    stacksize: Cell<usize>,
}

#[derive(Debug, Clone)]
struct Env {
    vars: HashMap<String, Var>,
    next: Option<Box<Env>>,
}

impl Env {
    pub fn new(next: Option<Box<Env>>) -> Self {
        Env {
            vars: HashMap::new(),
            next,
        }
    }
}

fn into_new_range<T: Sized>(param: T, f: impl Fn(T) -> T,states: &SemaStates) -> T {
    let env = states.env.replace(Env::new(None));
    *states.env.borrow_mut() = Env::new(Some(Box::new(env)));
    let ret = f(param);
    // Rollback
    states.env.replace_with(|env|*env.next.take().unwrap());
    ret
}

fn find_var(name: &str,states: &SemaStates) -> Option<Var> {
    let env = states.env.borrow().clone();
    let mut next: &Option<Box<Env>> = &Some(Box::new(env));
    loop {
        if let Some(ref e) = next {
            let var = e.vars.get(name);
            if var.is_some() {
                return var.cloned();
            }
            next = &e.next;
        } else {
            return None;
        }
    }
}

fn maybe_decay(base: Node, decay: bool) -> Node {
    if !decay {
        return base;
    }

    if let Ctype::Ary(ary_of, _) = base.ty.ty.clone() {
        let mut node = Node::new(NodeType::Addr(Box::new(base)));
        node.ty = Box::new(Type::ptr_to(ary_of.clone()));
        node
    } else {
        base
    }
}

fn check_lval(node: &Node) {
    let op = &node.op;
    if !matches!(op, NodeType::Lvar(_))
        && !matches!(op, NodeType::Gvar(_, _, _))
        && !matches!(op, NodeType::Deref(_))
        && !matches!(op, NodeType::Dot(_, _, _))
    {
        panic!("not an lvalue: {:?}", node.op);
    }
}

fn walk(mut node: Node, decay: bool,states: &SemaStates) -> Node {
    use self::NodeType::*;
    let op = node.op.clone();
    match op {
        Num(_) | Null | Break => (),
        Str(data, len) => {
            // Quoted from 9cc
            // > A string literal is converted to a reference to an anonymous
            // > global variable of type char array.
            let name = format!(".L.str{}", states.strlabel.get());
            states.strlabel.set(states.strlabel.get() + 1);
            let var = Var::new_global(node.ty.clone(), name, data, len, false);
            let name = var.name.clone();
            states.globals.borrow_mut().push(var);

            let mut ret = Node::new(NodeType::Gvar(name, "".into(), len));
            ret.ty = node.ty;
            return maybe_decay(ret, decay);
        }
        Ident(ref name) => {
            if let Some(var) = find_var(name,states) {
                match var.scope {
                    Scope::Local(offset) => {
                        let mut ret = Node::new(NodeType::Lvar(Scope::Local(offset)));
                        ret.ty = var.ty.clone();
                        return maybe_decay(ret, decay);
                    }
                    Scope::Global(ref data, len, _) => {
                        let mut ret =
                            Node::new(NodeType::Gvar(var.name.clone(), data.clone(), len));
                        ret.ty = var.ty.clone();
                        return maybe_decay(ret, decay);
                    }
                }
            } else {
                panic!("undefined variable: {}", name);
            }
        }
        Vardef(name, init_may, _) => {
            let stacksize = states.stacksize.get();
            states.stacksize.set(roundup(stacksize, node.ty.align) + node.ty.size);
            let offset = states.stacksize.get();

            states.env.borrow_mut().vars.insert(
                name.clone(),
                Var::new(node.ty.clone(), name.clone(), Scope::Local(offset)),
            );

            let mut init = None;
            if let Some(init2) = init_may {
                init = Some(Box::new(walk(*init2, true,states)));
            }
            node.op = Vardef(name, init, Scope::Local(offset));
        }
        If(mut cond, mut then, els_may) => {
            cond = Box::new(walk(*cond, true,states));
            then = Box::new(walk(*then, true,states));
            let mut new_els = None;
            if let Some(els) = els_may {
                new_els = Some(Box::new(walk(*els, true,states)));
            }
            node.op = If(cond, then, new_els);
        }
        Ternary(mut cond, mut then, mut els) => {
            cond = Box::new(walk(*cond, true,states));
            then = Box::new(walk(*then, true,states));
            els = Box::new(walk(*els, true,states));
            node.ty = then.ty.clone();
            node.op = Ternary(cond, then, els);
        }
        For(init, cond, inc, body) => {
            let f = |(init, cond, inc, body)| -> (Node, Node, Node, Node) {
                (
                    walk(init, true,states),
                    walk(cond, true,states),
                    walk(inc, true,states),
                    walk(body, true,states),
                )
            };
            let (init, cond, inc, body) = into_new_range((*init, *cond, *inc, *body),f,states);
            node.op = For(
                Box::new(init),
                Box::new(cond),
                Box::new(inc),
                Box::new(body),
            );
        }
        DoWhile(body, cond) => {
            node.op = DoWhile(Box::new(walk(*body, true,states)), Box::new(walk(*cond, true,states)));
        }
        Dot(mut expr, name, _) => {
            expr = Box::new(walk(*expr, true,states));
            let offset;
            if let Ctype::Struct(ref members) = expr.ty.ty {
                if members.is_empty() {
                    panic!("incomplete type");
                }
                let m_may = members.iter().find(|m| {
                    if let NodeType::Vardef(ref m_name, _, _) = m.op {
                        if m_name != &name {
                            return false;
                        }
                        return true;
                    }
                    false
                });

                if let Some(m) = m_may {
                    if let NodeType::Vardef(_, _, Scope::Local(offset2)) = m.op {
                        node.ty = m.ty.clone();
                        offset = offset2;
                    } else {
                        unreachable!()
                    }
                } else {
                    panic!("member missing: {}", name);
                }
            } else {
                panic!("struct expected before '.'");
            }

            node.op = NodeType::Dot(expr, name, offset);
            return maybe_decay(node, decay);
        }
        BinOp(token_type, mut lhs, mut rhs) => {
            use self::TokenType::*;
            match token_type {
                Plus | Minus => {
                    lhs = Box::new(walk(*lhs, true,states));
                    rhs = Box::new(walk(*rhs, true,states));

                    if matches!(rhs.ty.ty, Ctype::Ptr(_)) {
                        mem::swap(lhs.deref_mut(),rhs.deref_mut());
                    }
                    if matches!(rhs.ty.ty, Ctype::Ptr(_)) {
                        panic!("'pointer {:?} pointer' is not defined", node.op)
                    }

                    if matches!(lhs.ty.ty, Ctype::Ptr(_)) {
                        rhs = Box::new(Node::scale_ptr(rhs, &lhs.ty));
                    }

                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
                AddEQ | SubEQ => {
                    lhs = Box::new(walk(*lhs, false,states));
                    check_lval(&*lhs);
                    rhs = Box::new(walk(*rhs, true,states));

                    if matches!(lhs.ty.ty, Ctype::Ptr(_)) {
                        rhs = Box::new(Node::scale_ptr(rhs, &lhs.ty));
                    }
                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
                Equal | MulEQ | DivEQ | ModEQ | ShlEQ | ShrEQ | BitandEQ | XorEQ | BitorEQ => {
                    lhs = Box::new(walk(*lhs, false,states));
                    check_lval(&*lhs);
                    node.op = BinOp(token_type, lhs.clone(), Box::new(walk(*rhs, true,states)));
                    node.ty = lhs.ty;
                }
                _ => {
                    lhs = Box::new(walk(*lhs, true,states));
                    rhs = Box::new(walk(*rhs, true,states));
                    node.op = BinOp(token_type, lhs.clone(), rhs);
                    node.ty = lhs.ty;
                }
            }
        }
        PostInc(mut expr) => {
            expr = Box::new(walk(*expr, true,states));
            node.ty = expr.ty.clone();
            node.op = PostInc(expr);
        }
        PostDec(mut expr) => {
            expr = Box::new(walk(*expr, true,states));
            node.ty = expr.ty.clone();
            node.op = PostDec(expr);
        }
        Neg(mut expr) => {
            expr = Box::new(walk(*expr, true,states));
            node.ty = expr.ty.clone();
            node.op = Neg(expr);
        }
        Exclamation(mut expr) => {
            expr = Box::new(walk(*expr, true,states));
            node.ty = expr.ty.clone();
            node.op = Exclamation(expr);
        }
        Addr(mut expr) => {
            expr = Box::new(walk(*expr, true,states));
            check_lval(&*expr);
            node.ty = Box::new(Type::ptr_to(expr.ty.clone()));
            node.op = Addr(expr);
        }
        Deref(mut expr) => {
            expr = Box::new(walk(*expr, true,states));
            match expr.ty.ty {
                Ctype::Ptr(ref ptr_to) => node.ty = ptr_to.clone(),
                Ctype::Void => panic!("cannot dereference void pointer"),
                _ => panic!("operand must be a pointer"),
            }
            node.op = Deref(expr);
            return maybe_decay(node, decay);
        }
        Return(expr) => node.op = Return(Box::new(walk(*expr, true,states))),
        ExprStmt(expr) => node.op = ExprStmt(Box::new(walk(*expr, true,states))),
        Sizeof(mut expr) => {
            expr = Box::new(walk(*expr, false,states));
            node = Node::new_int(expr.ty.size as i32)
        }
        Alignof(mut expr) => {
            expr = Box::new(walk(*expr, false,states));
            node = Node::new_int(expr.ty.align as i32)
        }
        Call(name, mut args) => {
            if let Some(var) = find_var(&name,states) {
                if let Ctype::Func(returning) = var.ty.ty {
                    node.ty = returning;
                } else {
                    eprint!("bad function: {}", name);
                }
            } else {
                eprint!("bad function: {}", name);
            }

            args = args.into_iter().map(|arg| walk(arg, true,states)).collect();
            node.op = Call(name, args);
        }
        CompStmt(mut stmts) => {
            let f = |stmts: Vec<Node>| -> Vec<Node> {
                stmts.into_iter().map(|stmt| walk(stmt, true,states)).collect()
            };
            stmts = into_new_range(stmts,f,states);
            node.op = CompStmt(stmts);
        }
        VecStmt(mut stmts) => {
            stmts = stmts.into_iter().map(|stmt| walk(stmt, true,states)).collect();
            node.op = VecStmt(stmts);
        }
        StmtExpr(body) => {
            node.op = StmtExpr(Box::new(walk(*body, true,states)));
            node.ty = Box::new(Type::int_ty())
        }
        _ => panic!("unknown node type"),
    };
    node
}

pub fn sema(nodes: Vec<Node>) -> (Vec<Node>, Vec<Var>) {
    let mut new_nodes = vec![];

    let states = SemaStates{
        globals: RefCell::new(vec![]),
        env: RefCell::new(Env::new(None)),
        stacksize: Cell::new(0),
        strlabel: Cell::new(0),
    };

    for mut node in nodes {
        if let NodeType::Vardef(name, _, Scope::Global(data, len, is_extern)) = node.op {
            let var = Var::new_global(node.ty, name.clone(), data, len, is_extern);
            states.globals.borrow_mut().push(var.clone());
            states.env.borrow_mut().vars.insert(name, var);
            continue;
        }

        let var;
        match &node.op {
            NodeType::Func(name, _, _, _) | NodeType::Decl(name) => {
                var = Var::new_global(node.ty.clone(), name.clone(), "".into(), 0, false);
                states.env.borrow_mut().vars.insert(name.clone(), var);
            }
            _ => unreachable!(),
        }

        if matches!(node.op, NodeType::Decl(_)) {
            continue;
        }

        if let NodeType::Func(name, args, body, _) = node.op {
            let mut args2 = vec![];
            for arg in args {
                args2.push(walk(arg, true,&states));
            }
            let body2 = walk(*body, true,&states);
            node.op = NodeType::Func(
                name.clone(),
                args2,
                Box::new(body2),
                states.stacksize.get(),
            );
            states.stacksize.set(0);
            new_nodes.push(node);
        }
    }
    (new_nodes, states.globals.take())
}
