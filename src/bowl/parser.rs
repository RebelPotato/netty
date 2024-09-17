use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;

use super::ast::{DefNode, Node, Program, Addr};

#[derive(Parser)]
#[grammar = "bowl/grammar.pest"]
struct Bowl;

fn push_node(store: &mut Vec<Node>, node: Node) -> Addr {
    let addr = Addr(store.len());
    store.push(node);
    addr
}

fn parse_aexp(pair: Pair<Rule>, store: &mut Vec<Node>) -> Addr {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::primop => {
            let op = pair.into_inner().next().unwrap().as_str().to_string();
            push_node(store, Node::Prim(op))
        }
        Rule::u8 => push_node(store, Node::Num(pair.as_str().parse().unwrap())),
        Rule::name => push_node(store, Node::Ref(pair.as_str().to_string())),
        Rule::lam => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let body = parse_exp(inner.next().unwrap(), store);
            push_node(store, Node::Lam(name, body))
        }
        _ => unreachable!()
    }
}

fn parse_cexp(pair: Pair<Rule>, store: &mut Vec<Node>) -> Addr {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::bra => parse_cexp(pair.into_inner().next().unwrap(), store),
        Rule::app => {
            let mut inner = pair.into_inner();
            let mut a = parse_aexp(inner.next().unwrap(), store);
            for b in inner {
                let b = parse_aexp(b, store);
                a = push_node(store, Node::App(a, b));
            }
            a
        }
        Rule::aexp => parse_aexp(pair, store),
        _ => unreachable!()
    }
}

fn parse_exp(pair: Pair<Rule>, store: &mut Vec<Node>) -> Addr {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::r#let => {
            let mut inner = pair.into_inner();
            let name = inner.next().unwrap().as_str().to_string();
            let value = parse_cexp(inner.next().unwrap(), store);
            let body = parse_exp(inner.next().unwrap(), store);
            push_node(store, Node::Let(name, value, body))
        }
        Rule::cexp => parse_cexp(pair, store),
        _ => unreachable!()
    }
}

fn parse_def(pair: Pair<Rule>) -> DefNode {
    let mut inner = pair.into_inner();
    let name = inner.next().unwrap().as_str().to_string();
    let safe = false;
    let mut store = Vec::new();
    let root = parse_exp(inner.next().unwrap(), &mut store);
    DefNode {
        name,
        safe,
        root,
        store,
    }
}

pub fn parse(s: &str) -> Result<Program, pest::error::Error<Rule>> {
    let parsed = Bowl::parse(Rule::main, s)?.next().unwrap();
    let inner = parsed.into_inner();

    let mut main = None;
    let mut defs = Vec::new();
    for def in inner {
        if let Rule::def = def.as_rule() {
            let def = parse_def(def);
            if def.name == "main" {
                main = Some(def);
            } else {
                defs.push(def);
            }
        }
    }
    if let Some(main) = main {
        Ok(Program::new(main, defs))
    } else {
        panic!("No main definition found");
    }
}
