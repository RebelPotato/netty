use super::{Addr, Def, Num, NumValue, Pair as NodePair, Port, ROM};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;

#[derive(Debug)]
struct Names {
    map: HashMap<String, Addr>,
    count: HashMap<String, u8>,
    counter: Addr,
}
impl Names {
    fn new() -> Names {
        Names {
            map: HashMap::new(),
            count: HashMap::new(),
            counter: 0,
        }
    }
    fn has(&mut self, name: &str) -> bool {
        self.map.contains_key(name)
    }
    fn get(&mut self, name: &str) -> Addr {
        let count = self.count.get_mut(name).unwrap();
        *count += 1;
        if *count > 2 {
            panic!("Variable {} should be used two times only", name);
        }
        *self.map.get(name).unwrap()
    }
    fn put(&mut self, name: &str) -> Addr {
        self.counter += 1;
        self.map.insert(name.to_string(), self.counter);
        self.count.insert(name.to_string(), 1);
        self.counter
    }
}

#[derive(Parser)]
#[grammar = "filling/grammar.pest"]
struct Filling;

pub struct State {
    names: Names,
    def_names: HashMap<String, Addr>,
    node: Vec<NodePair>,
    rbag: Vec<NodePair>,
}
impl State {
    pub fn new() -> State {
        State {
            names: Names::new(),
            def_names: HashMap::new(),
            node: Vec::new(),
            rbag: Vec::new(),
        }
    }
    fn clear_buffer(&mut self) {
        self.rbag.clear();
        self.node.clear();
    }
    fn add_pair(&mut self, left: Port, right: Port) -> Addr {
        let addr = self.node.len() as Addr;
        self.node.push(NodePair::new(left, right));
        addr
    }
    fn parse_pair(&mut self, pair: Pair<Rule>) -> (Addr, bool) {
        let mut pair = pair.into_inner();
        let left = self.parse_tree(pair.next().unwrap());
        let right = self.parse_tree(pair.next().unwrap());
        let node = self.add_pair(left.0, right.0);
        (node, left.1 && right.1)
    }
    // returns the node and if it is safe
    fn parse_node(&mut self, pair: Pair<Rule>) -> (Port, bool) {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::era => (Port::new(super::ERA, 0), true),
            Rule::r#ref => {
                let name = pair.into_inner().next().unwrap().as_str();
                let addr = self.def_names.get(name).unwrap();
                (Port::new(super::REF, *addr), true)
            }
            Rule::num => {
                let mut pair = pair.into_inner();
                let value: NumValue = pair.next().unwrap().as_str().parse().unwrap();
                (Num::new(super::U8, value).as_port(), true)
            }
            Rule::con => {
                let (node, safe) = self.parse_pair(pair);
                (Port::new(super::CON, node), safe)
            }
            Rule::dup => {
                let (node, _) = self.parse_pair(pair);
                (Port::new(super::DUP, node), false)
            }
            Rule::opr => {
                let (node, safe) = self.parse_pair(pair);
                (Port::new(super::OPR, node), safe)
            }
            Rule::swi => {
                let (node, safe) = self.parse_pair(pair);
                (Port::new(super::SWI, node), safe)
            }
            _ => unreachable!(),
        }
    }

    pub fn parse_tree(&mut self, pair: Pair<Rule>) -> (Port, bool) {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::name => {
                let name = pair.as_str();
                let addr = if self.names.has(name) {
                    self.names.get(name)
                } else {
                    self.names.put(name)
                };
                (Port::new(super::VAR, addr), true)
            }
            Rule::node => self.parse_node(pair),
            _ => unreachable!(),
        }
    }

    pub fn parse_exp(&mut self, pair: Pair<Rule>) -> (Port, bool) {
        let mut pair = pair.into_inner();
        let (root, safe) = self.parse_tree(pair.next().unwrap());
        let mut safe = safe;
        for conn in pair {
            let mut conn = conn.into_inner();
            let (left, lsafe) = self.parse_tree(conn.next().unwrap());
            let (right, rsafe) = self.parse_tree(conn.next().unwrap());
            self.rbag.push(NodePair::new(left, right));
            safe = safe && lsafe && rsafe;
        }
        (root, safe)
    }

    pub fn parse_def(&mut self, pair: Pair<Rule>) -> Def {
        let mut pair = pair.into_inner();
        let name = pair.next().unwrap().as_str().to_string();

        self.clear_buffer();
        let (root, safe) = self.parse_exp(pair.next().unwrap());

        Def {
            name,
            safe,
            root,
            rbag: self.rbag.clone(),
            node: self.node.clone(),
        }
    }
    pub fn parse_rom(&mut self, s: &str) -> Result<ROM, pest::error::Error<Rule>> {
        let parsed = Filling::parse(Rule::main, s)?.next().unwrap();
        let inner = parsed.into_inner();

        let mut counter: Addr = 0;
        for def in inner.clone() {
            if let Rule::def = def.as_rule() {
                let name = def.into_inner().next().unwrap().as_str().to_string();
                self.def_names.insert(name, counter);
                counter += 1;
            }
        }
        let mut defs = Vec::new();
        for def in inner {
            if let Rule::def = def.as_rule() {
                defs.push(self.parse_def(def));
            }
        }
        Ok(ROM { defs })
    }
}

pub fn parse_rom(s: &str) -> Result<ROM, pest::error::Error<Rule>> {
    let mut state = State::new();
    state.parse_rom(s)
}
