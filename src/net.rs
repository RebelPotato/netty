use core::panic;
use std::{
    cell::RefCell, collections::HashMap, fmt::{self, Display, Formatter}, rc::Rc
};
use pest::Parser;
use pest_derive::Parser;
use pest::iterators::Pair;
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
fn rule_dup(_eqs: &mut Vec<Equation>, x: Term, _y: Wire, _z: Wire) {
    match x {
        // Term::Dup(y0, z0) => {
        //     eqs.push(Equation::new(y, y0));
        //     eqs.push(Equation::new(z, z0));
        // }
        Term::Dup(_, _) | Term::Add(_, _) | Term::S(_) | Term::Z => panic!("Dup does not interact with {}", x),
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

// parser

#[derive(Parser)]
#[grammar = "wonton.pest"]
struct Wonton;

struct Names {
    map: HashMap<String, Rc<Name>>,
    count: HashMap<String, u8>,
    counter: u64,
}
impl Names {
    fn new() -> Names {
        Names {
            map: HashMap::new(),
            count: HashMap::new(),
            counter: 0,
        }
    }
    fn get(&mut self, name: &str) -> Rc<Name> {
        match self.map.get(name) {
            Some(id) => {
                let count = self.count.get_mut(name).unwrap();
                *count += 1;
                if *count > 2 {
                    panic!("Variable {} should be used two times only", name);
                }
                id.clone()
            },
            None => {
                self.counter += 1;
                let new_name = Rc::new(Name::new(self.counter));
                self.map.insert(name.to_string(), new_name.clone());
                self.count.insert(name.to_string(), 1);
                new_name
            }
        }
    }
}

fn parse_name(pair: Pair<Rule>, names: &mut Names) -> Wire {
    let name = pair.as_str();
    if name == "*" {
        Wire::T(Box::new(Term::Erase))
    } else {
        Wire::N(names.get(name))
    }
}

fn parse_lhs(pair: Pair<Rule>, names: &mut Names) -> Wire {
    let item = pair.into_inner().next().unwrap();
    match item.as_rule() {
        Rule::name => parse_name(item, names),
        Rule::dup => {
            let mut pair = item.into_inner();
            let x = parse_name(pair.next().unwrap(), names);
            let y = parse_name(pair.next().unwrap(), names);
            Wire::T(Box::new(Term::Dup(x, y)))
        }
        _ => unreachable!()
    }
}

fn parse_aexp(pair: Pair<Rule>, eqs: &mut Vec<Equation>,  names: &mut Names) -> Wire {
    let item = pair.into_inner().next().unwrap();
    match item.as_rule() {
        Rule::bra => parse_exp(item.into_inner().next().unwrap(), eqs, names),
        Rule::lam => {
            let mut pair = item.into_inner();
            let x = parse_name(pair.next().unwrap(), names);
            let body = parse_exp(pair.next().unwrap(), eqs, names);
            Wire::T(Box::new(Term::Con(x, body)))
        }
        Rule::name => parse_name(item, names),
        _ => unreachable!()
    }
}

fn parse_exp(pair: Pair<Rule>, eqs: &mut Vec<Equation>, names: &mut Names) -> Wire {
    let mut aexps = pair.into_inner();
    let mut f = parse_aexp(aexps.next().unwrap(), eqs, names);
    for xp in aexps {
        let x = parse_aexp(xp, eqs, names);
        let result = Rc::new(Name::tmp());
        let term = Term::Con(x, Wire::N(result.clone()));
        eqs.push(Equation::new(f, Wire::T(Box::new(term))));
        f = Wire::N(result);
    }
    f
}

fn parse_stmt(pair: Pair<Rule>, eqs: &mut Vec<Equation>, names: &mut Names){
    let mut pair = pair.into_inner();
    let left = parse_lhs(pair.next().unwrap(), names);
    let right = parse_exp(pair.next().unwrap(), eqs, names);
    eqs.push(Equation::new(left, right));
}

pub fn parse_eqs(s: &str, result: Rc<Name>) -> Result<Vec<Equation>, pest::error::Error<Rule>> {
    let parsed = Wonton::parse(Rule::main, s)?
        .next()
        .unwrap();
    let pair = parsed.into_inner();

    let mut eqs = vec![];
    let mut names = Names::new();
    for term in pair {
        match term.as_rule() {
            Rule::stmt => parse_stmt(term, &mut eqs, &mut names),
            Rule::exp => {
                let wire = parse_exp(term, &mut eqs, &mut names);
                eqs.push(Equation::new(Wire::N(result.clone()), wire));
                break;
            },
            _ => unreachable!()
        }
    }
    Ok(eqs)
}