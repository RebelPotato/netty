use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct Addr(pub usize);

#[derive(Debug)]
pub enum Node {
    Ref(String),
    Lam(String, Addr),
    Let(String, Addr, Addr),
    App(Addr, Addr),
    Num(u8),
    Prim(String),
}

#[derive(Debug)]
pub struct DefNode {
    pub name: String,
    pub root: Addr,
    pub safe: bool,
    pub store: Vec<Node>,
}

impl DefNode {
    pub fn get(&self, addr: Addr) -> &Node {
        &self.store[addr.0]
    }
    pub fn get_mut(&mut self, addr: Addr) -> &mut Node {
        &mut self.store[addr.0]
    }
}

#[derive(Debug)]
pub struct Program {
    pub main: DefNode,
    pub defs: Vec<DefNode>,
}

impl Program {
    pub fn new(main: DefNode, defs: Vec<DefNode>) -> Self {
        Program { main, defs }
    }
}

// Visualization

fn fmt_node(def: &DefNode, f: &mut Formatter, node: &Node, level: u8, at_body: bool) -> Result {
    match node {
        Node::Ref(name) => write!(f, "{}", name),
        Node::Lam(name, body) => {
            write!(f, "\\{}. ", name)?;
            fmt_node(def, f, def.get(*body), level + 1, false)
        }
        Node::Let(name, value, body) => {
            write!(f, "\n{}let {} = ", " ".repeat(level as usize * 2), name)?;
            fmt_node(def, f, def.get(*value), level, false)?;
            write!(f, " in ")?;
            fmt_node(def, f, def.get(*body), level, false)
        }
        Node::App(a, b) => {
            if at_body {
                write!(f, "(")?;
            }
            fmt_node(def, f, def.get(*a), level, false)?;
            write!(f, " ")?;
            fmt_node(def, f, def.get(*b), level, true)?;
            if at_body {
                write!(f, ")")
            } else {
                Ok(())
            }
        }
        Node::Num(n) => write!(f, "{}", n),
        Node::Prim(name) => write!(f, "({})", name),
    }
}

impl Display for DefNode {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if !self.safe {
            write!(f, "== unsafe ==\n")?;
        }
        write!(f, "{} = ", self.name)?;
        fmt_node(self, f, self.get(self.root), 1, false)?;
        Ok(())
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter) -> Result {
        for def in &self.defs {
            write!(f, "{}\n\n", def)?;
        }
        write!(f, "{}", self.main)?;
        Ok(())
    }
}