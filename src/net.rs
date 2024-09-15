use core::panic;
use std::{
    cell::RefCell, fmt::{self, Display, Formatter}, rc::Rc
};
use rand::Rng;

#[derive(Debug)]
pub struct Name {
    id: u64,
    content: RefCell<Option<Wire>>,
}
impl Name {
    pub fn new(id: u64) -> Name {
        Name {
            id,
            content: RefCell::new(None),
        }
    }
    pub fn tmp() -> Name {
        let mut rng = rand::thread_rng();
        Name::new(rng.gen())
    }
    pub fn set(&self, wire: Wire) -> Option<Equation> {
        let content = self.content.take();
        match content {
            Some(ind) => Some(Equation::new(ind, wire)),
            None => {
                *self.content.borrow_mut() = Some(wire);
                None
            }
        }
    }
}
impl Display for Name {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match &*self.content.borrow() {
            Some(term) => write!(f, "{}", term),
            None => write!(f, "[#{}]", self.id),
        }
    }
}

#[derive(Debug)]
pub enum Wire {
    T(Box<Term>),
    N(Rc<Name>),
}
impl Display for Wire {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Wire::T(term) => write!(f, "{}", term),
            Wire::N(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Debug)]
pub enum Term {
    // lambda calculus
    Con(Wire, Wire),
    Dup(Wire, Wire),
    Erase,

    // primitives: integer
    Z,
    S(Wire),
    Add(Wire, Wire),
}
impl Display for Term {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Term::Con(x, y) => write!(f, "con({}, {})", x, y),
            Term::Dup(x, y) => write!(f, "dup({}, {})", x, y),
            Term::Erase => write!(f, "*"),

            Term::Z => write!(f, "Z"),
            Term::S(x) => write!(f, "S{}", x),
            Term::Add(y, sum) => write!(f, "add({}, {})", y, sum),
        }
    }
}

#[derive(Debug)]
pub struct Equation {
    pub left: Wire,
    pub right: Wire,
}
impl Equation {
    pub fn new(left: Wire, right: Wire) -> Equation {
        Equation { left, right }
    }
}

pub fn step(eqs: &mut Vec<Equation>) {
    let eq = eqs.pop().unwrap();
    match (eq.left, eq.right) {
        (Wire::T(left), Wire::T(right)) => rule(eqs, left, right),

        (Wire::T(term), Wire::N(name)) => attach(eqs, name, Wire::T(term)),
        (Wire::N(name), Wire::T(term)) => attach(eqs, name, Wire::T(term)),
        (Wire::N(name0), Wire::N(name1)) => attach(eqs, name0, Wire::N(name1)),
    }
}

fn attach(eqs: &mut Vec<Equation>, name: Rc<Name>, wire: Wire) {
    match name.set(wire) {
        Some(eq) => eqs.push(eq),
        None => (),
    }
}

fn rule(eqs: &mut Vec<Equation>, left: Box<Term>, right: Box<Term>) {
    match (*left, *right) {
        (Term::Erase, x) | (x, Term::Erase) => rule_erase(eqs, x),
        (Term::Con(y, z), x) | (x, Term::Con(y, z)) => rule_con(eqs, x, y, z),
        (Term::Dup(y, z), x) | (x, Term::Dup(y, z)) => rule_dup(eqs, x, y, z),

        // primitives
        (Term::Add(y, sum), x) | (x, Term::Add(y, sum)) => rule_add(eqs, x, y, sum),

        (left, right) => panic!("No rule applies to {} = {}", left, right),
    }
}

fn rule_erase(eqs: &mut Vec<Equation>, x: Term) {
    match x {
        Term::Add(a, b) | Term::Con(a, b) | Term::Dup(a, b) => {
            eqs.push(Equation::new(a, Wire::T(Box::new(Term::Erase))));
            eqs.push(Equation::new(b, Wire::T(Box::new(Term::Erase))));
        }
        Term::S(x) => {
            eqs.push(Equation::new(x, Wire::T(Box::new(Term::Erase))));
        }
        Term::Z | Term::Erase => {}
    }
}

fn rule_con(eqs: &mut Vec<Equation>, x: Term, y: Wire, z: Wire) {
    match x {
        Term::Con(y0, z0) => {
            eqs.push(Equation::new(y, y0));
            eqs.push(Equation::new(z, z0));
        }
        Term::Dup(y0, z0) => con_dup(eqs, y, z, y0, z0),
        Term::Add(_, _) | Term::S(_) | Term::Z => panic!("Con does not interact with {}", x),
        Term::Erase => unreachable!(),
    }
}
fn rule_dup(eqs: &mut Vec<Equation>, x: Term, y: Wire, z: Wire) {
    match x {
        Term::Dup(y0, z0) => {
            eqs.push(Equation::new(y, y0));
            eqs.push(Equation::new(z, z0));
        }
        Term::Add(_, _) | Term::S(_) | Term::Z => panic!("Dup does not interact with {}", x),
        Term::Con(_, _) | Term::Erase => unreachable!(),
    }
}

fn con_dup(eqs: &mut Vec<Equation>, yc: Wire, zc: Wire, yd: Wire, zd: Wire) {
    let v0 = Rc::new(Name::tmp());
    let v1 = Rc::new(Name::tmp());
    let v2 = Rc::new(Name::tmp());
    let v3 = Rc::new(Name::tmp());

    let cz = Term::Dup(Wire::N(v0.clone()), Wire::N(v2.clone()));
    let cy = Term::Dup(Wire::N(v1.clone()), Wire::N(v3.clone()));
    let dy = Term::Con(Wire::N(v1), Wire::N(v0));
    let dz = Term::Con(Wire::N(v3), Wire::N(v2));

    eqs.push(Equation::new(yc, Wire::T(Box::new(cy))));
    eqs.push(Equation::new(zc, Wire::T(Box::new(cz))));
    eqs.push(Equation::new(yd, Wire::T(Box::new(dy))));
    eqs.push(Equation::new(zd, Wire::T(Box::new(dz))));
}

fn rule_add(eqs: &mut Vec<Equation>, x: Term, y: Wire, sum: Wire) {
    match x {
        Term::Z => eqs.push(Equation::new(y, sum)),
        Term::S(x) => {
            let v = Rc::new(Name::tmp());
            let s = Term::S(Wire::N(v.clone()));
            let add = Term::Add(y, Wire::N(v));
            eqs.push(Equation::new(sum, Wire::T(Box::new(s))));
            eqs.push(Equation::new(x, Wire::T(Box::new(add))));
        }
        Term::Add(_, _) => panic!("Add does not interact with {}", x),
        Term::Con(_, _) | Term::Dup(_, _) | Term::Erase => unreachable!(),
    }
}

pub fn dump(eqs: &Vec<Equation>) {
    for eq in eqs.iter() {
        println!("{} = {}", eq.left, eq.right);
    }
    println!("---");
}