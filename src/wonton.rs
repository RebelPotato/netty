use pest::Parser;
use pest_derive::Parser;
use pest::iterators::Pair;
use std::collections::HashMap;
use std::rc::Rc;
use crate::net::{Equation, Name, Term, Wire};

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